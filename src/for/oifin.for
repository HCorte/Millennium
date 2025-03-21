C OIFIN.FOR
C
C V39 08-JUN-2005 FRP Modify for IPS Distribution.
C V38 06-OCT-2000 UXN AlphaIPS release.
C V37 07-MAR-1997 RXK More dates unrevbyted 
C V36 07-MAR-1997 RXK Some things unrevbyted 
C V35 18-FEB-1997 HXK Cleaned up hack for AGTXFR
C V34 07-FEB-1997 HXK Hack for AGTXFR (temporarily using AGTHCH)
C V33 05-FEB-1997 RXK Some things revbyted
C V32 28-JAN-1997 HXK LOTGEN changes for IPS
C V31 05-DEC-1996 HXK Updated for Finland IPS pre-release
C V30 02-FEB-1995 JXJ UPDATED CALCULATION OF COMMISIONS
C V29 01-FEB-1995 JXJ UPDATED TO MATCH NEW ERROR MESSAGE MAPPING
C V28 31-JAN-1995 JXJ CORRECTED USE OF SCITY
C V27 28-JAN-1995 JXJ UPDATED TO SEND TOTALS ON ALL TERMINALS
C V26 26-JAN-1995 JXJ IPS HANDLES SETTING THERE DATA TO NEGATIVE THEY ALSO 
C                     ADD IT TO TOTALS
C V25 12-JAN-1995 DJO CHANGED IINR TO IINVR AS DEFINED IN GLOBAL.DEF.
C V24 11-JAN-1995 ITD DO NOT CHANGE AMOUNTS RETURNED FROM INSTANT SYSTEM TO 
C                     NEGATIVE
C V23 11-JAN-1995 ITD CHECK IF INVOICE REPORT BEFORE CLEARING INVOICE FLAG
C V22 10-JAN-1995 ITD UPDATED TO MATCH NEW MESSAGE FORMATS FOR INSTANT REPORTS
C V21 22-OCT-1994 DJO Changed to give correct totals for logical record 
C                     invoices.
C V20 21-OCT-1994 DJO Added support for both BACS & CHAPS sweep days.
C V19 21-OCT-1994 DJO Added code to support both BACS & CAHPS sweep days.
C V18 18-OCT-1994 DJO Changed (again!!) to only reset the auto-invoice flag 
C                     on good transactions.
C V17 08-OCT-1994 DJO Changed to only do an auto invoice 1 time, regardless of 
C                     outcome.
C V16 04-OCT-1994 MCM CLEAR AUTO INVOICE REQUEST BIT ONLY IF GOOD RESPONSE 
C                     FROM LMS
C V15 02-OCT-1994 DJO Changed the sense of the AGTPAY flag as per design spec.
C V14 30-SEP-1994 MCM CORRECTED DISPLAY OF INVOICE/ORIGINAL
C V13 24-SEP-1994 MCM CHAPS BIT IS OFF/BACS BIT IS ON
C V12 11-SEP-1994 MCM DO NOT SEND COMMISSION FOR POST OFFICE SUBORDINATES
C V11 05-SEP-1994 MCM SET THE PENNIES TO NEGATIVE FOR SALES COMMISSION
C V10 30-AUG-1994 MCM MODIFIED PER LMS REQUIREMENTS
C V09 22-AUG-1994 MCM SEND NEGATIVE VALUES TO THE TERMINAL/GVT
C V08 19-JUL-1994 MCM ADDED PACK SETTLED, ADJUSTMENT TEXT, REPORT AND COMBINED 
C                     INVOICE REPORT
C V07 05-MAY-1993 MCM RELEASED FOR GEORGIA
C V06 01-JUN-1992 TKO CHANGED TRCLS TO TRTYP
C V05 26-MAR-1992 NJA ADDED (DATE MMDDYY)
C V04 10-FEB-1992 JPJ ADDED (GVT)
C V03 05-FEB-1992 NJA ADDED (GVT REVBYT)
C V02 04-FEB-1992 NJA ADDED (2 BYTE CHECKSUM)
C V01 13-NOV-1991 JPJ RELEASED FOR VAX (INSTANTS)
C
C SUBROUTINE TO BUILD INSTANT FINANCIAL REPORT OUTPUT MESSAGES.
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE OIFIN(TRABUF,OUTTAB,OUTLEN,CRSBUF)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CRSBUF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:INSCOM.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
C
        INTEGER*4 MYCHKSUM, CHKLEN, IND
        INTEGER*4 ERRTYP, I, RTER, RAGT, ST
        INTEGER*4 CHKDIG, MESS(EDLEN), GNUM, GTYP
        INTEGER*4 SALCNT, SALAMT, CANCNT, CANAMT, VALCNT, VALAMT
        INTEGER*4 SALCOM(2), VALCOM(2), NETDUE(2)
        INTEGER*4 NUMGAM
        INTEGER*4 CLRKNUM
C
        BYTE      OUTTAB(*)
        INTEGER*2 OUTLEN, DATE(12)
C
        INTEGER*4   I4TEMP
        INTEGER*2   I2TEMP(2)
        BYTE        I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
        DATA ERRTYP/Z90/
C
        CLRKNUM = 0
        SALCNT=0
        SALAMT=0
        CANCNT=0
        CANAMT=0
        VALCNT=0
        VALAMT=0
        SALCOM(1)=0
        SALCOM(2)=0
        VALCOM(1)=0
        VALCOM(2)=0
        NETDUE(1)=0
        NETDUE(2)=0
C
C CONTROL AND SEQUENCE NUMBER
C
        OUTTAB(1) = '20'X+TRABUF(TTRN)
C
C IF TRANSACTION STATUS IS NOT GOOD
C BUILD ERROR MESSAGE.
C
        IF(TRABUF(TSTAT).NE.GOOD.AND.TRABUF(TERR).NE.BCRS) THEN
          OUTTAB(2) = ERRTYP
          OUTTAB(5) = TRABUF(TERR)
          OUTLEN=5
          GOTO 1000
        ENDIF
C
C TYPE AND SUBTYPE
C
        OUTTAB(2) = 'D5'X
C
C TIME
C
        IND=5
	CALL HOST_TO_TERM(OUTTAB(IND), TRABUF(TTIM), 3)
        IND=IND+3
C
C JULIAN DATE
C
	CALL HOST_TO_TERM(OUTTAB(IND), DAYJUL, 2)
        IND=IND+2
C
C SERIAL NUMBER AND CHECK DIGITS
C
        CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),I4TEMP,CHKDIG)
	CALL HOST_TO_TERM(OUTTAB(IND), I4TEMP, 3)
        OUTTAB(IND+3) = CHKDIG
        IND=IND+4
C
C INSTANT RESULT CODE ONE
C
        OUTTAB(IND+0) = 0
        IF(TRABUF(TIERR).NE.INOER) OUTTAB(IND+0) = 1
        IND=IND+1
C
C INSTANT RESULT CODE TWO (NUMBER OF ENTRIES)
C
        IF(TRABUF(TIERR).EQ.INOER)THEN
           IF(TRABUF(TRTYP).EQ.IPSET.OR.TRABUF(TRTYP).EQ.IADJR) THEN
              OUTTAB(IND+0) = CRSBUF(NBRENT)
           ELSE
              OUTTAB(IND+0) = 0
           ENDIF
        ELSE
           OUTTAB(IND+0) = TRABUF(TIERR)
        ENDIF
        IND=IND+1
C
C INSTANT REPORT CLASS
C
        OUTTAB(IND+0) = TRABUF(TRTYP)
        IND=IND+1
C
C PUT CLERK NUMBER IN UPPER NIBBLE OF SUB CLASS.
C
        CLRKNUM=AGTHTB(AGTPASOFF,TRABUF(TTER))
C
        OUTTAB(IND+0) = IOR(ISHFT(CLRKNUM,4),TRABUF(TRSUB))
        IND=IND+1
C
C PACK SETTLED REPORT
C
        IF(TRABUF(TRTYP).EQ.IPSET) THEN
C
C MORE FLAG
C
          IF(CRSBUF(NXTGAM).NE.0) THEN
            OUTTAB(IND+0) = '01'X
          ELSE
            OUTTAB(IND+0) = '00'X
          ENDIF
          IND=IND+1
C
C SET NEXT GAME AND PACK
C
	  CALL HOST_TO_TERM(OUTTAB(IND), CRSBUF(NXTGAM), 2)
	  IND = IND + 2
	  CALL HOST_TO_TERM(OUTTAB(IND), CRSBUF(NXTPCK), 4)
	  IND = IND + 4
C
C SET NEXT RETAILER NUMBER
C
	  CALL HOST_TO_TERM(OUTTAB(IND), CRSBUF(NXTRET), 4)
	  IND = IND + 4
C
C BEGIN INVOICE CDC
C
          DATE(VMON)=CRSBUF(BEGMON)
          DATE(VDAY)=CRSBUF(BEGDAY)
          DATE(VYEAR)=CRSBUF(BEGYEAR)
          CALL BDATE(DATE)
          I4TEMP=DATE(VCDC)
C
	  CALL HOST_TO_TERM(OUTTAB(IND), I4TEMP, 2)
          IND=IND+2
C
C END INVOICE CDC
C
          DATE(VMON)=CRSBUF(ENDMON)
          DATE(VDAY)=CRSBUF(ENDDAY)
          DATE(VYEAR)=CRSBUF(ENDYEAR)
          CALL BDATE(DATE)
          I4TEMP=DATE(VCDC)
C
	  CALL HOST_TO_TERM(OUTTAB(IND), I4TEMP, 2)
          IND=IND+2
C
C TOTAL PACK SETTLED AMOUNT
C
	  CALL HOST_TO_TERM(OUTTAB(IND), CRSBUF(PCKAMT), 4)
          IND=IND+4
C
C PACK SETTLED INFORMATION
C
          DO 100 I=0,CRSBUF(NBRENT)-1
C
C GAME NUMBER
C
	     CALL HOST_TO_TERM(OUTTAB(IND), CRSBUF(GAMNUM+I), 2)
             IND=IND+2
C
C PACK NUMBER
C
	     CALL HOST_TO_TERM(OUTTAB(IND), CRSBUF(PCKNUM+I), 4)
             IND=IND+4
C
C ORDER NUMBER (FIRST 9 DIGITS)
C
	     CALL HOST_TO_TERM(OUTTAB(IND), CRSBUF(ORDNUM1+I), 4)
             IND=IND+4
C
C ORDER NUMBER (LAST 4 DIGITS)
C
	     CALL HOST_TO_TERM(OUTTAB(IND), CRSBUF(ORDNUM2+I), 2)
             IND=IND+2
C
C CDC DATE
C
             DATE(VMON)=CRSBUF(PCKMON+I)
             DATE(VDAY)=CRSBUF(PCKDAY+I)
             DATE(VYEAR)=CRSBUF(PCKYEAR+I)
             CALL BDATE(DATE)
             I4TEMP=DATE(VCDC)
C
	     CALL HOST_TO_TERM(OUTTAB(IND), I4TEMP, 2)
             IND=IND+2
C
C AUTO SETTLED FLAG
C
             OUTTAB(IND+0)=CRSBUF(AUTFLG+I)
             IND=IND+1
100       CONTINUE
C
C ADJUSTMENT REPORT
C
        ELSEIF(TRABUF(TRTYP).EQ.IADJR) THEN
C
C MORE FLAG
C
           OUTTAB(IND+0) = CRSBUF(MFLAG)
           IND = IND + 1
C
C SET ADJUSTMENT TYPE AND DATE
C
	  CALL HOST_TO_TERM(OUTTAB(IND), CRSBUF(NXTDAT), 4)
	  IND = IND + 4
	  CALL HOST_TO_TERM(OUTTAB(IND), CRSBUF(NXTTIM), 4)
	  IND = IND + 4
	  CALL HOST_TO_TERM(OUTTAB(IND), CRSBUF(NXTTYP), 2)
	  IND = IND + 2
C
C BEGIN INVOICE CDC
C
	  CALL HOST_TO_TERM(OUTTAB(IND), CRSBUF(BEGCDC), 2)
          IND=IND+2
C
C END INVOICE CDC
C
	  CALL HOST_TO_TERM(OUTTAB(IND),CRSBUF(ENDCDC), 2)
          IND=IND+2
C
C TOTAL ADJUSTMENT AMOUNT
C
	  CALL HOST_TO_TERM(OUTTAB(IND),CRSBUF(ADJTOT),4)
          IND=IND+4
C
C ADJUSTMENT INFORMATION
C
          DO 200 I=0,CRSBUF(NBRENT)-1
C
C CDC OF ADJUSTMENT
C
	     CALL HOST_TO_TERM(OUTTAB(IND),CRSBUF(ADJCDC+I),2)
             IND=IND+2
C
C ADJUSTMENT TEXT
C
	     CALL FASTMOV(CRSBUF(ADJTXT1+I), OUTTAB(IND), 1)
	     IND = IND + 4
	     CALL FASTMOV(CRSBUF(ADJTXT2+I), OUTTAB(IND), 1)
	     IND = IND + 4
	     CALL FASTMOV(CRSBUF(ADJTXT3+I), OUTTAB(IND), 1)
	     IND = IND + 4
	     CALL FASTMOV(CRSBUF(ADJTXT4+I), OUTTAB(IND), 1)
	     IND = IND + 4
C
C ADJUSTMENT AMOUNT
C
	     CALL HOST_TO_TERM(OUTTAB(IND),CRSBUF(ADJAMT+I),4)
             IND=IND+4
200       CONTINUE
C
C INVOICE HEADER
C
        ELSEIF(TRABUF(TRTYP).EQ.IINVF) THEN
C
C REGISTERED OFFICE ADDRESS 1
C
          CALL MOVBYT(REGSTR1,1,OUTTAB,IND,30)
          IND=IND+30
C
C REGISTERED OFFICE ADDRESS 2
C
          CALL MOVBYT(REGSTR2,1,OUTTAB,IND,30)
          IND=IND+30
C
C REGISTERED OFFICE TOWN
C
          CALL MOVBYT(REGTWN,1,OUTTAB,IND,20)
          IND=IND+20
C
C REGISTERED OFFICE COUNTY
C
          CALL MOVBYT(REGCTY,1,OUTTAB,IND,20)
          IND=IND+20
C
C REGISTERED OFFICE POST CODE
C
          CALL MOVBYT(REGPST,1,OUTTAB,IND,7)
          IND=IND+7
C
C REGISTRATION NUMBER
C
	  CALL HOST_TO_TERM(OUTTAB(IND),REGNUM,4)
          IND=IND+4
C
C VAT REGISTRATION NUMBER
C
	  CALL HOST_TO_TERM(OUTTAB(IND),VATNUM,4)
          IND=IND+4
C
C INVOICE FOOTER
C
        ELSEIF(TRABUF(TRTYP).EQ.IINVH) THEN
          IF(P(SUPFIL).EQ.1) THEN
            TRABUF(TERR)=SUPR
            OUTTAB(2) = ERRTYP
            OUTTAB(5) = TRABUF(TERR)
            OUTLEN=5
            GOTO 1000
          ENDIF
C
C GET REQUESTED AGENT NUMBER
C
          IF(TRABUF(TRSUB).EQ.4.OR.TRABUF(TRSUB).EQ.5)THEN !INDIVIDUAL
            RAGT=TRABUF(TRCON)
          ELSEIF(TRABUF(TRSUB).EQ.2.OR.TRABUF(TRSUB).EQ.6) THEN
            RAGT=TRABUF(TRCHN)         
          ELSE
            RAGT=AGTTAB(AGTNUM,TRABUF(TTER))
          ENDIF
          DO 400 I=1,NUMAGT
             IF(RAGT.EQ.AGTTAB(AGTNUM,I)) RTER=I
400       CONTINUE
          IF(RTER.LT.1.OR.RTER.GT.NUMAGT) THEN
            TRABUF(TERR)=INVL
            OUTTAB(2) = ERRTYP
            OUTTAB(5) = TRABUF(TERR)
            OUTLEN=5
            GOTO 1000
          ENDIF
C
C READ AGENT RECORD
C
          CALL READW(ASFFDB,RTER,ASFREC,ST)
          IF(ST.NE.0) THEN
            MESS(1)=INO
            MESS(2)=TEGEN
            MESS(3)=4
            CALL FASTMOV(SFNAMES(1,ASF),MESS(4),5)
            MESS(9)=RTER
            CALL QUEMES(MESS)
            OUTTAB(2) = ERRTYP
            OUTTAB(5) = TRABUF(TERR)
            OUTLEN=5
            GOTO 1000
          ENDIF
C
C AGENT NAME
C
          CALL MOVBYT(ASFINF,SNAME,OUTTAB,IND,LNAME)
          IND=IND+LNAME
C
C AGENT STREET ADDRESS 1
C
          CALL MOVBYT(ASFINF,SSTRT,OUTTAB,IND,LSTRT)
          IND=IND+LSTRT
C
C AGENT STREET ADDRESS 2
C
CPXN      CALL MOVBYT(ASFINF,SSTR2,OUTTAB,IND,LSTR2)
CPXN      IND=IND+LSTR2
C
C AGENT TOWN
C
          CALL MOVBYT(ASFINF,SCITY,OUTTAB,IND,LCITY)
          IND=IND+LCITY
C
C AGENT COUNTY
C
          CALL MOVBYT(ASFINF,SSTTE,OUTTAB,IND,LSTTE)
          IND=IND+LSTTE
C
C AGENT POST CODE
C
          CALL MOVBYT(ASFINF,SZIPC,OUTTAB,IND,LZIPC)
          IND=IND+LZIPC
C
C VALIDATIONS PER CLERK
C
        ELSEIF(TRABUF(TRTYP).EQ.IVCLRK) THEN

           I4TEMP=0                                       ! filler
	   CALL HOST_TO_TERM(OUTTAB(IND),I4TEMP,2)
           IND=IND+2
C
	   I4TEMP = AGTMIS(CLRKNUM,1,TRABUF(TTER))
	   CALL HOST_TO_TERM(OUTTAB(IND),I4TEMP,4)             !validation cnt
           IND=IND+4

           I4TEMP = AGTMIS(CLRKNUM,2,TRABUF(TTER))*DYN_BETUNIT !validations amt
	   CALL HOST_TO_TERM(OUTTAB(IND), I4TEMP, 4) 
           IND=IND+4

        ELSE                                         
C
C GET REQUESTED AGENT NUMBER
C
c         IF(TRABUF(TRSUB).EQ.4.OR.TRABUF(TRSUB).EQ.5)THEN !INDIVIDUAL
c           RAGT=TRABUF(TRCON)
c         ELSEIF(TRABUF(TRSUB).EQ.2.OR.TRABUF(TRSUB).EQ.6) THEN
c           RAGT=TRABUF(TRCHN)         
c         ELSE
            RAGT=AGTTAB(AGTNUM,TRABUF(TTER))
c         ENDIF
          DO 500 I=1,NUMAGT
             IF(RAGT.EQ.AGTTAB(AGTNUM,I)) RTER=I
500       CONTINUE
          IF(RTER.LT.1.OR.RTER.GT.NUMAGT) THEN
            TRABUF(TERR)=INVL
            OUTTAB(2) = ERRTYP
            OUTTAB(5) = TRABUF(TERR)
            OUTLEN=5
            GOTO 1000
          ENDIF
C
C REPORT DATE
C
          DATE(VMON)=CRSBUF(STRSAL+9)
          DATE(VDAY)=CRSBUF(STRSAL+10)
          DATE(VYEAR)=CRSBUF(STRSAL+11)
          CALL BDATE(DATE)
          I4TEMP=DATE(VCDC)
C
	  CALL HOST_TO_TERM(OUTTAB(IND),I4TEMP,2)
          IND=IND+2
C
C TOTAL LOTS SOLD (CNT)
C
	  CALL HOST_TO_TERM(OUTTAB(IND),CRSBUF(STRSAL),4)
          IND=IND+4
C
C TOTAL LOTS SOLD (AMT)
C
	  CALL HOST_TO_TERM(OUTTAB(IND),CRSBUF(STRSAL+1),4)
          IND=IND+4
C
C TOTAL RETURN (AMT)
C
	  CALL HOST_TO_TERM(OUTTAB(IND),CRSBUF(STRSAL+4),4)
          IND=IND+4
C
C TOTAL CASHES (CNT)
C
	  CALL HOST_TO_TERM(OUTTAB(IND),CRSBUF(STRSAL+2),4)
          IND=IND+4
C
C TOTAL CASHES (AMT)
C
	  CALL HOST_TO_TERM(OUTTAB(IND),CRSBUF(STRSAL+3),4)
          IND=IND+4
C
C TOTAL CRSBUF SALES COMMISION
C DO NOT DISPLAY POST OFFICE SUBORDINATES COMMISSIONS
C
C          IF((TSBIT(AGTTAB(AGTTYP,RTER),AGTPOS).OR.
C     *       TSBIT(AGTTAB(AGTTYP,RTER),AGTSUB)).AND.
C     *       (TRABUF(TRSUB).EQ.1)) THEN
C           I4TEMP = 0
C          ELSE
C           I4TEMP = CRSBUF(STRSAL+5)
C          ENDIF
C
	  CALL HOST_TO_TERM(OUTTAB(IND),CRSBUF(STRSAL+5),4)
          IND=IND+4
C
C TOTAL CRSBUF PRIZE COMMISION
C
C DO NOT DISPLAY POST OFFICE SUBORDINATES COMMISSIONS
C
C          IF((TSBIT(AGTTAB(AGTTYP,RTER),AGTPOS).OR.
C     *       TSBIT(AGTTAB(AGTTYP,RTER),AGTSUB)).AND.
C     *       (TRABUF(TRSUB).EQ.1)) THEN
C           I4TEMP = 0
C          ELSE
C           I4TEMP = CRSBUF(STRSAL+6)
C          ENDIF
C
	  CALL HOST_TO_TERM(OUTTAB(IND),CRSBUF(STRSAL+6),4)
          IND=IND+4
C
C TOTAL ADJUSTMENT
C
	  CALL HOST_TO_TERM(OUTTAB(IND),CRSBUF(STRSAL+7),4)
          IND=IND+4
C
C ADD COMMISSIONS TO THE AMOUNT DUE FOR POST OFFICE SUBORDINATES
C
C          IF((TSBIT(AGTTAB(AGTTYP,RTER),AGTPOS).OR.
C     *       TSBIT(AGTTAB(AGTTYP,RTER),AGTSUB)).AND.
C     *       (TRABUF(TRSUB).EQ.1)) THEN
C           I4TEMP = CRSBUF(STRSAL+8)+CRSBUF(STRSAL+5)+CRSBUF(STRSAL+6)
C          ELSE
C           I4TEMP = CRSBUF(STRSAL+8)
C          ENDIF
C
C TOTAL DUE (AMT)
C
	  CALL HOST_TO_TERM(OUTTAB(IND),CRSBUF(STRSAL+8),4)
          IND=IND+4
C
C SET OPTIONS FLAG
C
          I4TEMP=0
C
C ORIGINAL COPY
C
          IF(TSBIT(AGTBTB(AGTPAR,TRABUF(TTER)),AGTRINV).AND.
     *      TRABUF(TRTYP).EQ.IINVR) THEN
            IF(TRABUF(TRSUB).EQ.1) THEN
              I4TEMP = I4TEMP + '01'X
              IF(TRABUF(TSTAT).EQ.GOOD.AND.
     *           TRABUF(TIERR).EQ.INOER) THEN
                CALL BCLR(AGTBTB(AGTPAR,TRABUF(TTER)),AGTRINV)
              ENDIF
            ELSE
              IF(RTER.EQ.AGTHTB(AGTCHN,TRABUF(TTER)))  THEN
                I4TEMP = I4TEMP + '01'X
                IF(TRABUF(TSTAT).EQ.GOOD.AND.
     *             TRABUF(TIERR).EQ.INOER) THEN
                  CALL BCLR(AGTBTB(AGTPAR,TRABUF(TTER)),AGTRINV)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
C
C CHAPS AGENT
C
          IF(TSBIT(AGTTAB(AGTTYP,RTER),AGTPAY)) 
     *       I4TEMP = I4TEMP + '02'X
C
C INVOICE TERMINAL
C
          IF(TSBIT(AGTTAB(AGTTYP,RTER),AGTINV).AND.                 !INDEPENDENT
     *       AGTHTB(AGTCHN,RTER).EQ.0) I4TEMP = I4TEMP + '04'X
          IF(TSBIT(AGTTAB(AGTTYP,RTER),AGTINV)) THEN             !FRANCHISE SUB
             IF(AGTHTB(AGTCHN,RTER).NE.0) THEN
               IF(TSBIT(AGTBTB(AGTPAR,AGTHTB(AGTCHN,RTER)),AGTFCH))
     *           I4TEMP = I4TEMP + '04'X          
             ENDIF
          ENDIF
          OUTTAB(IND+0) = I4TEMP
          IND=IND+1         
C
C SWEEP CDC
C
          DATE(VMON)=CRSBUF(STRSAL+15)
          DATE(VDAY)=CRSBUF(STRSAL+16)
          DATE(VYEAR)=CRSBUF(STRSAL+17)
          CALL BDATE(DATE)
C         IF(TSBIT(AGTTAB(AGTTYP,RTER),AGTPAY)) THEN
C           I4TEMP=DATE(VCDC)+SWEEPDAY
C         ELSE
C           I4TEMP=DATE(VCDC)+SWEEPDAY-2
C         ENDIF
          I4TEMP=DATE(VCDC)

	  CALL HOST_TO_TERM(OUTTAB(IND),I4TEMP,2)
          IND=IND+2
C
C BEGIN INVOICE CDC
C
          DATE(VMON)=CRSBUF(STRSAL+12)
          DATE(VDAY)=CRSBUF(STRSAL+13)
          DATE(VYEAR)=CRSBUF(STRSAL+14)
          CALL BDATE(DATE)
          I4TEMP=DATE(VCDC)

	  CALL HOST_TO_TERM(OUTTAB(IND),I4TEMP,2)
          IND=IND+2
C
C END INVOICE CDC
C
          DATE(VMON)=CRSBUF(STRSAL+15)
          DATE(VDAY)=CRSBUF(STRSAL+16)
          DATE(VYEAR)=CRSBUF(STRSAL+17)
          CALL BDATE(DATE)
          I4TEMP=DATE(VCDC)

	  CALL HOST_TO_TERM(OUTTAB(IND),I4TEMP,2)
          IND=IND+2
C
C DOCUMENT NUMBER
C
	  CALL HOST_TO_TERM(OUTTAB(IND),CRSBUF(STRSAL+18),4)
          IND=IND+4
C
C ONLINE INFORMATION
C
          IF(P(SUPFIL).EQ.1) THEN
            TRABUF(TERR)=SUPR
            OUTTAB(2) = ERRTYP
            OUTTAB(5) = TRABUF(TERR)
            OUTLEN=5
            GOTO 1000
          ENDIF
C
C READ AGENT RECORD
C
          CALL READW(ASFFDB,RTER,ASFREC,ST)
          IF(ST.NE.0) THEN
            MESS(1)=INO
            MESS(2)=TEGEN
            MESS(3)=4
            CALL FASTMOV(SFNAMES(1,ASF),MESS(4),5)
            MESS(9)=RTER
            CALL QUEMES(MESS)
            OUTTAB(2) = ERRTYP
            OUTTAB(5) = TRABUF(TERR)
            OUTLEN=5
            GOTO 1000
          ENDIF
C
C CHAIN SUMMARY
C
c         IF(TRABUF(TRSUB).EQ.2.OR.TRABUF(TRSUB).EQ.6) THEN 
CPXN        DO 600 GNUM=1,MAXGAM
CPXN           GTYP=GNTTAB(GAMTYP,GNUM)
CPXN           IF(GTYP.LE.0) GOTO 600
CPXN           SALCNT=SALCNT+CHNBIL(GSCNT,GNUM,1)
CPXN           SALAMT=SALAMT+CHNBIL(GSAMT,GNUM,1)
CPXN           VALCNT=VALCNT+CHNBIL(GVCNT,GNUM,1)
CPXN           VALAMT=VALAMT+CHNBIL(GVAMT,GNUM,1)
CPXN           CANCNT=CANCNT+CHNBIL(GCCNT,GNUM,1)
CPXN           CANAMT=CANAMT+CHNBIL(GCAMT,GNUM,1)
600         CONTINUE
CPXN        CALL ADDI8I8(SALCOM,CHNINV(ASFSCMU,1),BETUNIT)
CPXN        CALL ADDI8I8(VALCOM,CHNINV(ASFVCMU,1),BETUNIT)
CPXN        CALL ADDI8I8(NETDUE,CHNINV(ASFDUEU,1),BETUNIT)
c         ELSE
            DO 700 GNUM=1,MAXGAM
               GTYP=GNTTAB(GAMTYP,GNUM)
               IF(GTYP.LE.0)GOTO 700
               SALCNT=SALCNT+ASFBIL(GSCNT,GNUM,1)
               SALAMT=SALAMT+ASFBIL(GSAMT,GNUM,1)
               VALCNT=VALCNT+ASFBIL(GVCNT,GNUM,1)
               VALAMT=VALAMT+ASFBIL(GVAMT,GNUM,1)
               CANCNT=CANCNT+ASFBIL(GCCNT,GNUM,1)
               CANAMT=CANAMT+ASFBIL(GCAMT,GNUM,1)
700         CONTINUE
            CALL ADDI8I8(SALCOM,ASFINV(ASFSCMU,1),BETUNIT)
            CALL ADDI8I8(VALCOM,ASFINV(ASFVCMU,1),BETUNIT)
            CALL ADDI8I8(NETDUE,ASFINV(ASFDUEU,1),BETUNIT)
c         ENDIF
C
C DO NOT DISPLAY POST OFFICE SUBORDINATES COMMISSIONS
C
          IF((TSBIT(AGTTAB(AGTTYP,RTER),AGTPOS).OR.
     *       TSBIT(AGTTAB(AGTTYP,RTER),AGTSUB)).AND.
     *       (TRABUF(TRSUB).EQ.1)) THEN
            SALCOM(1)=0
            SALCOM(2)=0
            VALCOM(1)=0
            VALCOM(2)=0
            CALL ADDI8I8(NETDUE,ASFINV(ASFSCMU,1),BETUNIT)
            CALL ADDI8I8(NETDUE,ASFINV(ASFVCMU,1),BETUNIT)
          ENDIF
C
C ONLINE SALES (CNT)
C
	  CALL HOST_TO_TERM(OUTTAB(IND),SALCNT,4)
          IND=IND+4
C
C ONLINE SALES (AMT)
C
	  CALL HOST_TO_TERM(OUTTAB(IND),SALAMT,4)
          IND=IND+4
C
C ONLINE CANCEL (CNT)
C
	  CALL HOST_TO_TERM(OUTTAB(IND),CANCNT,4)
          IND=IND+4
C
C ONLINE CANCEL (AMT)
C
          I4TEMP = CANAMT*-1
	  CALL HOST_TO_TERM(OUTTAB(IND),I4TEMP,4)
          IND=IND+4
C
C ONLINE VALIDATION (CNT)
C
	  CALL HOST_TO_TERM(OUTTAB(IND),VALCNT,4)
          IND=IND+4
C
C ONLINE VALIDATION (AMT)
C
          I4TEMP = VALAMT*-1
	  CALL HOST_TO_TERM(OUTTAB(IND),I4TEMP,4)
          IND=IND+4
C
C TOTAL ONLINE AND INSTANT SALES COMMISSION
C
          SALCOM(1)=SALCOM(1)*-1
          SALCOM(2)=SALCOM(2)*-1
CPXN      CALL ADDI8I4(SALCOM(1),CRSBUF(STRSAL+5),PENUNIT)
          I4TEMP = SALCOM(2)

	  CALL HOST_TO_TERM(OUTTAB(IND),I4TEMP,4)
          IND=IND+4
C
          I4TEMP = SALCOM(1)
	  CALL HOST_TO_TERM(OUTTAB(IND),I4TEMP,4)
          IND=IND+4
C
C TOTAL ONLINE AND INSTANT VALIDATION COMMISSION
C
          VALCOM(1)=VALCOM(1)*-1
          VALCOM(2)=VALCOM(2)*-1
CPXN      CALL ADDI8I4(VALCOM(1),CRSBUF(STRSAL+6),PENUNIT)
          I4TEMP = VALCOM(2)

	  CALL HOST_TO_TERM(OUTTAB(IND),I4TEMP,4)
          IND=IND+4
C
          I4TEMP = VALCOM(1)
	  CALL HOST_TO_TERM(OUTTAB(IND),I4TEMP,4)
          IND=IND+4
C
C TOTAL ONLINE AND INSTANT AMOUNT DUE
C
CPXN      CALL ADDI8I4(NETDUE(1),CRSBUF(STRSAL+8),PENUNIT)
          I4TEMP = NETDUE(2)
	  CALL HOST_TO_TERM(OUTTAB(IND),I4TEMP,4)
          IND=IND+4
C
          I4TEMP = NETDUE(1)
	  CALL HOST_TO_TERM(OUTTAB(IND),I4TEMP,4)
          IND=IND+4
C
C VOUCHER COUNT
C
          I4TEMP = 0
	  CALL HOST_TO_TERM(OUTTAB(IND),I4TEMP,4)
          IND=IND+4
C
C VOUCHER AMOUNT
C
          I4TEMP = ASFINV(ASFSRV,1)
	  CALL HOST_TO_TERM(OUTTAB(IND),I4TEMP,4)
          IND=IND+4
C
C NUMBER OF INSTANT GAMES
C
          NUMGAM = 0
          DO I=1,AITGAM
             IF(ASFITGSAL(AITGNUM,I).NE.0) NUMGAM = NUMGAM + 1
          ENDDO
          OUTTAB(IND+0) = NUMGAM
          IND=IND+1
C
C INSTANT GAME NUMBER AND SALES AMOUNT
C
          DO I=1,NUMGAM
	     CALL HOST_TO_TERM(OUTTAB(IND),ASFITGSAL(AITGNUM,I),2)
             IND=IND+2

             I4TEMP = ASFITGSAL(AITGAMT,I)*DYN_BETUNIT
  	     CALL HOST_TO_TERM(OUTTAB(IND),I4TEMP,4)
             IND=IND+4
          ENDDO

        ENDIF
        OUTLEN=IND-1
C
C CALCULATE CHECKSUM
C
1000    CONTINUE
        I4CCITT = TRABUF(TCHK)
	CALL HOST_TO_TERM(OUTTAB(3),I4CCITT,2)
        CHKLEN=OUTLEN-1
        CALL GETCCITT(OUTTAB,1,CHKLEN,MYCHKSUM)
        I4CCITT = MYCHKSUM
	CALL HOST_TO_TERM(OUTTAB(3),I4CCITT,2)
        RETURN
C
        END
