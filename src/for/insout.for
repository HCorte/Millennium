C INSOUT.FOR
C
C V28 09-DEZ-2020 SCML New Terminals Project - Olimpo
C V27 07-JUN-2005 FRP Modify for IPS Distribution.
C V26 06-OCT-2000 UXN AlphaIPS release. FSE sign-on added.
C V25 20-JUN-2000 OXK GETIGNAM as a separate module, Cleanup w/ WARNINGS=ALL
C V24 30-NOV-1999 UXN Unused parameter removed from OIFIN()
C V23 22-MAY-1997 WPW Fix from Rita for retries.
C V22 06-MAR-1997 RXK Cross counters counted only on livsys
C V21 13-FEB-1997 RXK IMNU=instant supply message, IORD=instant games names 
C                     request message
C V20 03-FEB-1997 WPW Changes for downloading GVTs.
C V19 28-JAN-1997 HXK Crscnt calculation logic and message numbers for 
C                     ERRLOG changed
C V18 19-JAN-1997 HXK Check for range on IGNBR
C V17 18-DEC-1996 HXK Modified call of DCORD 
C V16 11-DEC-1996 HXK Fix for ommission of sys*.def's ... GPK
C V15 11-DEC-1996 HXK Added code to load instant game names
C V14 05-DEC-1996 HXK Updated for Finland IPS pre-release
C V13 20-JAN-1995 JXJ ADDED PERFORMANCE COUNTER FOR INSTANTS
C V12 11-JAN-1995 DJO Changed to use APUBUF to get the TRABUF from the 
C                     INSPRO task.
C V11 10-JAN-1995 ITD UPDATED LOGIC THAT INCREAMENTS CRSCNT(2)
C V10 22-DEC-1994 DJO Updates made for the install system.
C V09 16-DEC-1994 DJO Changes made to support install request/confirm 
C                     messages from GVT's.
C V08 22-AUG-1994 MCM SET THE ERROR TO ZERO FOR FINANCIAL REPORTS
C V07 06-JUL-1994 MCM ADDED OPENING OF THE ASF FOR THE COMBINED REPORT
C V06 25-JAN-1994 JPJ Moved setting of tstat=rejt/terr=synt to correct location
C V05 04-MAY-1993 MCM RELEASED FOR GEORGIA
C V04 24-APR-1992 NJA ADDED (INVALID VALIDATION COUNT)
C V03 26-MAR-1992 NJA ADDED (CALL TO CDATE)
C V02 10-FEB-1992 JPJ ADDED (GVT)
C V01 13-NOV-1991 JPJ RELEASED FOR VAX (INSTANTS)
C
C
C INSTANT TICKET OUTPUT PROCESSING TASK
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM INSOUT
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CRSBUF.DEF'
        INCLUDE 'INCLIB:INSCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        INCLUDE 'INCLIB:APUCOM.DEF'
        INCLUDE 'INCLIB:INSNAM.DEF'
C
        INTEGER*4 MESS(EDLEN),LOGREC(LREC*3),WRKBUF(TRALEN)
        INTEGER*4 I
        INTEGER*4 TER, BUF, STATUS
        INTEGER*4 TASK, LSTSUP/1/
C
        CHARACTER*10 ALLBLANK
C
        LOGICAL  FIRSTCALL
        DATA FIRSTCALL/.TRUE./
        DATA ALLBLANK/'          '/
C
C
        CALL COPYRITE
        CALL SNIF_AND_WRKSET
        TASK=INO
        MESS(1)=TASK
C
C       CLEAR GAME NAMES ARRAY
C
        DO I=1,INSNBR
           INSSNAM(I) = ALLBLANK
        ENDDO

        IF (FIRSTCALL) THEN
             FIRSTCALL = .FALSE.
             CALL GETIGNAM(7)
        ENDIF
5       CONTINUE
C
C WAIT FOR SOMETHING TO DO
C IF END OF DAY THEN CALL GSTOP(GEXIT_SUCCESS)
C
10      CONTINUE
        IF(DAYSTS.EQ.DSCLOS) CALL GSTOP(GEXIT_SUCCESS)
        IF(DAYSTS.EQ.DSSUSP) THEN
	  CALL USRCLOS1(1)
      	  CALL USRCLOS1(3)
          LSTSUP=1
30        CONTINUE
          CALL HOLD(0,STATUS)
          IF(DAYSTS.EQ.DSOPEN) GOTO 5
          IF(DAYSTS.EQ.DSCLOS) CALL GSTOP(GEXIT_SUCCESS)
          GOTO 30
        ENDIF
        CALL HOLD(0,STATUS)
C
C
        IF(LSTSUP.NE.P(SUPFIL)) THEN
          LSTSUP=P(SUPFIL)
          IF(LSTSUP.EQ.0) THEN
            CALL OPNFIL
          ELSE
	    CALL USRCLOS1(1)
      	    CALL USRCLOS1(3)
          ENDIF
        ENDIF
C
C GET BUFFER NUMBER FROM TOP OF QUEUE.
C IF NO WAGERS QUEUED, GO BACK TO WAIT STATE.
C
20      CONTINUE
        CALL TOPQUE(TASK,BUF)
        IF(BUF.EQ.0) GOTO 10
C
C GET TRABUF OUT OF PROCOM BUFFER
C
        CALL LOGTRA(TRABUF,APUBUF(2,BUF))
C
C SET UP TRABUF
C 
        TRABUF(TSER)=PRO(SERIAL,BUF)
        TRABUF(TTIM)=PRO(TSTAMP,BUF)
        TRABUF(TSIZE)=HPRO(NUMLRC,BUF)
C SET UP TRABUF Olimpo serial Number  
C----+------------------------------------------------------------------
C V28| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
C        TRABUF(TVOLMSERL_IL)=PRO(SEROLM_OLM,BUF) !SEROLM=49
C        TRABUF(TVOLMSERM_IL)=PRO(SEROLM_OLM+1,BUF)
C        TRABUF(TVOLMSERH_IL)=BPRO(SEROLM_OLM+8,BUF)
C        TRABUF(TVOLMCOMF_IL)=BPRO(CHOLM_OLM)
C        TRABUF(TGOLMSERL_IL)=PRO(SEROLM_OLM,BUF)
C        TRABUF(TGOLMSERM_IL)=PRO(SEROLM_OLM+1,BUF)
C        TRABUF(TGOLMSERH_IL)=BPRO(SEROLM_OLM+8,BUF)
C        TRABUF(TGOLMMIDL_IL)=PRO(MESSID_OLM)
C        TRABUF(TGOLMMIDH_IL)=PRO(MESSID_OLM+1)
C        TRABUF(TGOLMCOMF_IL)=BPRO(CHOLM_OLM)
C----+------------------------------------------------------------------
C V28| New Terminals Project - Olimpo
C----+------------------------------------------------------------------        
C
C UPDATE NXTIXRF NUMBER ON BACKUP SYSTEMS
C
        IF(TRABUF(TITYP).EQ.IFIN) THEN
           IF(TRABUF(TRTYP).EQ.IINVF.OR.
     *        TRABUF(TRTYP).EQ.IINVH.OR.
     *        TRABUF(TRTYP).EQ.IVCLRK) GOTO 80
        ENDIF
C
        IF(TRABUF(TITYP).EQ.IFSESON.AND.TRABUF(TIFSETYP).NE.0) GOTO 80 !NO FSE 
C
        IF(TRABUF(TITYP).EQ.ISOF.OR.
     *     TRABUF(TITYP).EQ.IEST.OR.
     *     TRABUF(TITYP).EQ.ISON.OR.
     *     TRABUF(TITYP).EQ.IIMSG) GOTO 80
C
        IF(TRABUF(TSTAT).EQ.GOOD) THEN
           IF(P(SYSTYP).EQ.LIVSYS    .AND.
     *       .NOT.(TRABUF(TITYP).EQ.ISOF .OR.
     *       TRABUF(TITYP).EQ.IEST   .OR.
     *       TRABUF(TITYP).EQ.ISON   .OR.
     *       TRABUF(TITYP).EQ.IIMSG  .OR.
     *       (TRABUF(TITYP).EQ.IFIN  .AND.
     *       (TRABUF(TRTYP).EQ.IINVF.OR.TRABUF(TRTYP).EQ.IINVH.OR.
     *        TRABUF(TRTYP).EQ.IVCLRK))))
C     *        TRABUF(TRTYP).EQ.IVCLRK)).OR.
C     *       TRABUF(TITYP).EQ.IORD))
     *       CRSCNT(2)=CRSCNT(2)+1  
           IF(P(SYSTYP).NE.LIVSYS) CALL UPDXRF(TRABUF(TIXRF))
        ENDIF
C
C DECODE CROSS SYSTEM BUFFER
C 
80      CONTINUE
        IF(TRABUF(TSTAT).EQ.GOOD) THEN
          CALL FASTSET(0,CRSBUF,CRSLEN)
          IF(TRABUF(TITYP).EQ.IVAL) THEN
            CALL DCVAL(TRABUF,PRO(INSTAB,BUF))
          ELSE IF(TRABUF(TITYP).EQ.ILOT) THEN
            CALL DCLOT(TRABUF,PRO(INSTAB,BUF))
          ELSE IF(TRABUF(TITYP).EQ.ICAR) THEN
            CALL DCCAR(TRABUF,PRO(INSTAB,BUF))
          ELSE IF(TRABUF(TITYP).EQ.IQTA) THEN
            CALL DCQTA(TRABUF,PRO(INSTAB,BUF),CRSBUF)
          ELSE IF(TRABUF(TITYP).EQ.IINV) THEN
            CALL DCINV(TRABUF,PRO(INSTAB,BUF),CRSBUF)
          ELSE IF(TRABUF(TITYP).EQ.IFIN) THEN
            TRABUF(TIERR)=INOER
            IF(TRABUF(TRTYP).EQ.IINVF.OR.TRABUF(TRTYP).EQ.IINVH.OR.
     *         TRABUF(TRTYP).EQ.IVCLRK) GOTO 90
            CALL DCFIN(TRABUF,PRO(INSTAB,BUF),CRSBUF)
          ELSE IF(TRABUF(TITYP).EQ.IMNU) THEN
            CALL DCMNU(TRABUF,PRO(INSTAB,BUF))
          ELSE IF(TRABUF(TITYP).EQ.IORD) THEN
            CALL DCORD(TRABUF,PRO(INSTAB,BUF),CRSBUF)
          ELSE IF(TRABUF(TITYP).EQ.ISON) THEN
             TRABUF(TIERR)=INOER
          ELSE IF(TRABUF(TITYP).EQ.ISOF) THEN
             TRABUF(TIERR)=INOER
          ELSE IF(TRABUF(TITYP).EQ.ICNF) THEN
            CALL DCCNF(TRABUF,PRO(INSTAB,BUF))
          ELSE IF(TRABUF(TITYP).EQ.IGTB) THEN
            CALL DCGTB(TRABUF,PRO(INSTAB,BUF),CRSBUF)
          ELSE IF(TRABUF(TITYP).EQ.IEST) THEN
             TRABUF(TIERR)=INOER
          ELSE IF (TRABUF(TITYP).EQ.IFSESON) THEN
            CALL DCFSESON(TRABUF,PRO(INSTAB,BUF))
          ELSE IF(TRABUF(TITYP).EQ.IISS) THEN
            CALL DCISS(TRABUF,PRO(INSTAB,BUF))
          ELSE IF(TRABUF(TITYP).EQ.IOACT) THEN
            CALL DCOACT(TRABUF,PRO(INSTAB,BUF))
          ELSE
             TRABUF(TSTAT)=REJT
             TRABUF(TERR)=SYNT
          ENDIF
        ENDIF
C
90      CONTINUE
C
C SET ERROR AND STATUS IN TRABUF
C
        IF(TRABUF(TIERR).NE.INOER.AND.TRABUF(TSTAT).EQ.GOOD) THEN
          TRABUF(TERR) =BCRS
          TRABUF(TSTAT)=REJT
C          CALL FASTSET(0,CRSBUF,CRSLEN)
        ENDIF
C
C SEND ERROR MESSAGE FOR TIMED OUT TRANSACTIONS
C
        IF(TRABUF(TIERR).EQ.INTIM) THEN
          MESS(2) = TEGEN
          MESS(3) = 33
          MESS(4) = TRABUF(TIXRF)
          MESS(5) = BUF
          CALL QUEMES(MESS)
        ENDIF
C
        TER=TRABUF(TTER)
C
C MAKE SURE THAT THE TERMINAL IS SIGNED ON.
C
	IF((AGTHTB(AGTPASOFF,TER).LE.0.OR.
     *     AGTHTB(AGTPASOFF,TER).GT.NUMCLERK).AND.
     *     TRABUF(TITYP).EQ.IVAL) THEN
	    TRABUF(TSTAT) = REJT
	    TRABUF(TERR)  = NOTON
	ENDIF
C
C UPDATE FINACAL TOTALS
C
        IF(TRABUF(TSTAT).EQ.GOOD) THEN
          CALL UPDSUB(TRABUF)
          PERFRM(1,PERINS)=PERFRM(1,PERINS)+1
        ENDIF
C
C UPDATE INVALID VALIDATION COUNTER
C
        IF(TRABUF(TITYP).EQ.IVAL) THEN
          IF(TRABUF(TSTAT).EQ.GOOD) THEN
            DO 100 I=0,TRABUF(TIBCH)-1
               IF(TRABUF(TISTS1+I).EQ.04.OR.
     *            TRABUF(TISTS1+I).EQ.06)
     *           AGTBTB(AGTIVL,TER)=AGTBTB(AGTIVL,TER)+1
100         CONTINUE
          ELSE
            AGTBTB(AGTIVL,TER)=0
          ENDIF
C
C SEND ERROR MESSAGE IF INVALID COUNT EXCEEDS THE MAXIMUM
C
          IF(AGTBTB(AGTIVL,TER).GE.P(GVTIVL)) THEN
            MESS(2) = TEGEN
            MESS(3) = 38
            MESS(4) = TER
            CALL QUEMES(MESS)
            AGTBTB(AGTIVL,TER)=0
          ENDIF
        ENDIF
C
C IF RETRY SEND GOOD TRANSACTION STORED IN PROCOM
C
        IF(TRABUF(TERR).EQ.RETY.AND.TRABUF(TSTAT).EQ.REJT) THEN
          CALL LOGTRA(WRKBUF,PRO(INSTAB,BUF))
          IF(WRKBUF(TITYP).EQ.IVAL) THEN
            CALL OIVAL(WRKBUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ELSE IF (WRKBUF(TITYP).EQ.ILOT) THEN
            CALL OILOT(WRKBUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ELSE IF (WRKBUF(TITYP).EQ.ICAR) THEN
            CALL OICAR(WRKBUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ELSE IF (WRKBUF(TITYP).EQ.ICNF) THEN
            CALL OICNF(WRKBUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ELSE IF (WRKBUF(TITYP).EQ.IMNU) THEN
            CALL OIMNU(WRKBUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ELSE IF (WRKBUF(TITYP).EQ.IISS) THEN
            CALL OIISS(WRKBUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ELSE IF (WRKBUF(TITYP).EQ.IOACT) THEN
            CALL OIOACT(WRKBUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ENDIF
        ELSE        
C
C SET UP OUTPUT BUFFER
C
          IF(TRABUF(TITYP).EQ.IVAL) THEN
            CALL OIVAL(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ELSE IF (TRABUF(TITYP).EQ.ILOT) THEN
            CALL OILOT(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ELSE IF (TRABUF(TITYP).EQ.ICAR) THEN
            CALL OICAR(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ELSE IF (TRABUF(TITYP).EQ.IQTA) THEN
            CALL OIQTA(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),CRSBUF)
          ELSE IF (TRABUF(TITYP).EQ.IINV) THEN
            CALL OIINV(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),CRSBUF)
          ELSE IF (TRABUF(TITYP).EQ.IFIN) THEN
            CALL OIFIN(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),CRSBUF)
          ELSE IF (TRABUF(TITYP).EQ.IMNU) THEN
            CALL OIMNU(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ELSE IF (TRABUF(TITYP).EQ.IORD) THEN
            CALL OIORD(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),CRSBUF)
          ELSE IF (TRABUF(TITYP).EQ.ISON) THEN
            CALL OISON(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ELSE IF (TRABUF(TITYP).EQ.ISOF) THEN
            CALL OISOF(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ELSE IF (TRABUF(TITYP).EQ.ICNF) THEN
            CALL OICNF(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ELSE IF (TRABUF(TITYP).EQ.IEST) THEN
            CALL OIEST(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ELSE IF (TRABUF(TITYP).EQ.IGTB) THEN
            CALL OIGTB(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),CRSBUF)
          ELSE IF (TRABUF(TITYP).EQ.IFSESON) THEN
            CALL OIFSESON(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ELSE IF (TRABUF(TITYP).EQ.IISS) THEN
            CALL OIISS(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ELSE IF (TRABUF(TITYP).EQ.IOACT) THEN
            CALL OIOACT(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ENDIF
        ENDIF        
C
C PUT GOOD TRANSACTION INTO WRKBUF
C       
        CALL TRALOG(TRABUF,LOGREC)
        CALL WLOG(TRABUF(TSER),LOGREC,TASK)
C
C QUEUE TRANSACTION TO LOGGER OUTPUT QUEUE
C
        TER=TRABUF(TTER)
        AGTHTB(ATRNUM,TER)=TRABUF(TTRN)
        IF(TRABUF(TERR).EQ.TBAD) HPRO(ENCOVR,BUF)=-1
        IF(TRABUF(TSTAT).NE.GOOD.AND.TRABUF(TERR).NE.RETY) THEN
          AGTHTB(ACHKSM,TER)=-1
        ELSE
          AGTHTB(ACHKSM,TER)=TRABUF(TCHK)
        ENDIF
        HPRO(TRCODE,BUF)=TYPREG              
        CALL QUETRA(LOG, BUF)
        CALL DQUTRA(TASK,BUF)
        GOTO 20
        END
C
C SUBROUTINE TO OPEN ASF AND CLERK FILE
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE OPNFIL
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:INSCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
C
        INTEGER*4 MESS(EDLEN)
        INTEGER*4 ST
C
C OPEN ASF FILE
C
        MESS(1)=INO
        CALL OPENW(1,SFNAMES(1,ASF),4,0,0,ST)
        CALL IOINIT(ASFFDB,1,ASFSEC*256)
        IF(ST.NE.0) THEN
          MESS(2)=TEGEN
          MESS(3)=3
          CALL FASTMOV(SFNAMES(1,ASF),MESS(4),5)
          MESS(9)=ST
          CALL QUEMES(MESS)
        ENDIF
C
C OPEN CLERK ACCOUNTING FILE
C
        IF(P(CLRKACT).EQ.0) THEN
          CALL OPENW(3,SFNAMES(1,CLK),4,0,0,ST)
          CALL IOINIT(CLRKFDB,3,CLRKSEC*256)
          IF(ST.NE.0) THEN
            MESS(2)=TEGEN
            MESS(3)=3
            CALL FASTMOV(SFNAMES(1,CLK),MESS(4),5)
            MESS(9)=ST
            CALL QUEMES(MESS)
          ENDIF
        ENDIF
        RETURN
        END
