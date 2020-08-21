C
C PROGRAM PURWIN
C $Log:   GXAFXT:[GOLS]PURWIN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:34:42   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.11   23 Nov 1995 13:12:36   HXK
C  Merge of post 65 stuff; changes for Double/Couple
C  
C     Rev 1.11   30 Oct 1995 11:03:04   RXK
C  RfSS 178: fix for kicker amounts
C  
C     Rev 1.10   24 Mar 1995 14:25:52   HXK
C  Fix for refund
C  
C     Rev 1.9   02 Sep 1994 18:10:52   HXK
C  Merge of May,June RFSS batch 
C  
C     Rev 1.11   29 Apr 1994 17:05:46   JXP
C  COPY=0
C  
C     Rev 1.10   29 Apr 1994 12:48:26   HXK
C  USE STEP IN DO LOOP FOR SORT.
C  
C     Rev 1.9   19 Apr 1994 15:35:02   HXK
C  SORT BY AMOUNT IN DESCENDING OREDER IF LIMIT SPECIFIED
C  
C     Rev 1.8   17 Dec 1993 12:36:44   JXP
C  Include cash and refund limits in report
C  
C     Rev 1.7   28 Nov 1993 19:10:28   SXH
C  ONLY UPDATE JOKER CSHAMT IF THERE IS A JOKER GAME ASSOCIATED WITH MAIN
C  GAME
C  
C     Rev 1.6   18 Nov 1993 11:22:22   SXH
C  Claim and tax fields removed from print-out
C  Tickets with zero amounts not shown
C  Fixed bug where a LOTTO/VAKIO/RAVI and JOKER pay amount
C  is all shown as LOTTO/VAKIO/RAVI
C  
C     Rev 1.5   06 Nov 1993 22:13:16   HXK
C  ENLARGED MEMORY TABLE.
C  
C     Rev 1.4   05 Nov 1993 11:49:12   HXK
C  GRAND TOTALS FOR PURWIN  -SXH.
C  
C     Rev 1.3   08 Sep 1993 17:23:52   SXH
C  Hard-coded COPY=1
C  
C     Rev 1.2   07 Sep 1993 11:57:48   SXH
C  Agent number 8 chars
C  
C     Rev 1.1   20 Aug 1993 11:49:14   SXH
C  Released for Finland
C  
C     Rev 1.0   21 Jan 1993 17:24:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - purwin.for **
C
C PURWIN.FOR
C
C V03 25-MAY-92 GCAN INITIALIZE CLMAMT TO ZERO.
C V02 12-NOV-91 MTK  INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C
C UCASHED TICKET PURGE REPORT
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM PURWIN
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:PRMVLF.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:HSHCOM.DEF'
	INCLUDE 'INCLIB:TNAMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:PRGREC.DEF'
C
C MAXIMUM SORT TABLE ( MAXIMUN NUMBER OF WAGERS TO PURGE )
C
        INTEGER * 4 MAX_SORT
C
        PARAMETER(MAX_SORT = 130000)
C
	INTEGER*4    K                       !
	INTEGER*4    CSHAMT                  !
	INTEGER*4    REFAMT                  !
	INTEGER*4    CLMAMT                  !
	INTEGER*4    TAXAMT                  !
	INTEGER*4    GAM                     !
	INTEGER*4    SAGT                    !
	INTEGER*4    SCHK                    !
	INTEGER*4    SSER                    !
	INTEGER*4    SCDC                    !
	INTEGER*4    SCNT                    !
	INTEGER*4    START                   !
	INTEGER*4    CHK                     !
	INTEGER*4    SER                     !
	INTEGER*4    JUL                     !
	INTEGER*4    PAGE                    !
	INTEGER*4    I                       !
	INTEGER*4    EXT                     !
	INTEGER*4    COPY                    !
	INTEGER*4    ST                      !
	INTEGER*4    LENGTH                  !
	INTEGER*4    VS                      !
	INTEGER*4    COUNT                   !
	INTEGER*4    BLK                     !
	INTEGER*4    IND                     !
	INTEGER*4    REPLU                   !
	INTEGER*4    LINCNT                  !
	INTEGER*4    OLDBLK                  !
	INTEGER*4    FDB(7)                  !
	INTEGER*4    VLEN(0:3)               !
	INTEGER*4    SORT(5, MAX_SORT)       !
        INTEGER*4    TOTCSH/0/               !
        INTEGER*4    TOTREF/0/               !
        INTEGER*4    TOTTAX/0/               !
        INTEGER*4    TOTCLM/0/               !
        INTEGER*4    PCOUNT/0/               !
        INTEGER*4    GAMTOTCSH(MAXGAM)       !
        INTEGER*4    GAMTOTREF(MAXGAM)       !
        INTEGER*4    GAMTOTTAX(MAXGAM)       !
        INTEGER*4    GAMTOTCLM(MAXGAM)       !
        INTEGER*4    GAMCOUNT(MAXGAM)        !
        INTEGER*4    MIN_AMT_CSH	     !
        INTEGER*4    MIN_AMT_REF	     !
        INTEGER*4    START_LOOP              !
        INTEGER*4    END_LOOP                !                  
        INTEGER*4    STEP                    !
        INTEGER*4    JOK_WITH_OTHER_CNT      !
        INTEGER*4    JOK_WITH_OTHER_AMT      !
C
C TOTAL PURGE WITH NO PAY ORDER ( NUMTOT: TRACNT IS COUNTER, DOLAMT IS AMOUNT )
C
        INTEGER*4    TOTPGNOP(MAXGAM, NUMTOT) 

	INTEGER*2    DATE(LDATE_LEN)         !
	INTEGER*2    I2SORT(10, MAX_SORT)    !

	EQUIVALENCE (SORT,I2SORT)

	DATA VLEN/1,2,3,4/
C
	CALL COPYRITE
C
C
	REPLU = 7
	COUNT = 0
	OLDBLK= 0
	BLK   = 0
        CALL FASTSET(0,GAMTOTCSH,MAXGAM)
        CALL FASTSET(0,GAMTOTREF,MAXGAM)
        CALL FASTSET(0,GAMTOTTAX,MAXGAM)
        CALL FASTSET(0,GAMTOTCLM,MAXGAM)
        CALL FASTSET(0,GAMCOUNT,MAXGAM)
        CALL FASTSET(0, TOTPGNOP, MAXGAM * NUMTOT)
C
C DISPLAY MESSAGE TO USER
C
	TYPE*,IAM(),' Generating purged ticket report'
	IF(DAYSTS.NE.DSCLOS) THEN
	    TYPE*,IAM(),' Invalid day status ',DAYSTS
	    CALL GPAUSE
	ENDIF
C	CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)
C	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
        COPY=0
	CAll PRMMONY('Enter minimum cash amount for report',MIN_AMT_CSH,
     *                VALUNIT,EXT)
	CAll PRMMONY('Enter minimum refund amount for report',MIN_AMT_REF,
     *                BETUNIT,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
C OPEN UNCASHED TICKETS FILE
C
	CALL OPENW(2,SFNAMES(1,UTP),4,0,0,ST)
	CALL IOINIT(FDB,2,PRGSEC*256)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,UTP),1,ST,0)
C
C OPEN REPORT FILE
C
	CALL ROPEN('PURWIN.REP',REPLU,ST)
	IF(ST.NE.0) THEN
	    TYPE*,'PURWIN.REP open error - ',ST
	    CALL GPAUSE
	ENDIF
C
	CALL TITLE('UNCASHED TICKET PURGE REPORT','PURWIN ',1,
     *	            REPLU,PAGE,DAYCDC)
	WRITE(REPLU,9006)
     *	                 CMONY(MIN_AMT_CSH,12,VALUNIT),
     *                   CMONY(MIN_AMT_REF,12,BETUNIT)
	WRITE(REPLU,9000)
	LINCNT = 8
C
C READ UNCASHED TICKET FILE
C
100	CONTINUE
	IND=1
	BLK=BLK+1
	CALL READW(FDB,BLK,UPREC,ST)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,UTP),1,ST,0)
C
110	CONTINUE
	LENGTH=ISHFT(UPBUF(VFSSER,IND),-30)
	LENGTH=VLEN(LENGTH)
	CALL LOGVAL(VALREC,UPBUF(1,IND))
	IF(VALREC(VSSER).EQ.0) GOTO 1000
	IF(VALREC(VSTAT).EQ.VCXL.OR.VALREC(VSTAT).EQ.VDEL) GOTO 900
        IF(VALREC(VPAMT).EQ.0.AND.VALREC(VKPAMT).EQ.0.AND.
     *     VALREC(VRAMT).EQ.0) GOTO 900       ! DON'T PRINT ZERO PURGES !!
C
C WRITE RECORD TO MEMORY SORT TABLE
C
	COUNT = COUNT + 1
	IF(COUNT .GT. MAX_SORT) THEN
	    TYPE*,'Memory sort table overflow '
	    CALL GPAUSE
	ENDIF
	DATE(VCDC)=VALREC(VSCDC)
	CALL LCDATE(DATE)
	JUL=DATE(VJUL)
	CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),SER,CHK)
      	SER=SER*100+CHK
	IF(MIN_AMT_CSH.EQ.0.AND.MIN_AMT_REF.EQ.0) THEN
	   SORT(1,COUNT)=JUL
	   SORT(2,COUNT)=SER
	   SORT(3,COUNT)=VALREC(VGAM)
        ELSE
           SORT(1,COUNT)=VALREC(VPAMT)+VALREC(VKPAMT)
        ENDIF
	I2SORT(7,COUNT)=BLK
	I2SORT(8,COUNT)=IND

900	CONTINUE
	IND=IND+LENGTH
	IF(IND.GT.PRGBLC) GOTO 100
	GOTO 110
C
C GENERATE REPORT SORTED BY GAME AND TICKET NUMBER
C
1000	CONTINUE
        IF(MIN_AMT_CSH.EQ.0.AND.MIN_AMT_REF.EQ.0) THEN
	   TYPE*,IAM(),' Generating report sorted by game and ticket number'
	   CALL ISORT5(SORT,COUNT,3)
	   START=1
	   SCNT=1
	   DO I=2,COUNT+1
	      IF(SORT(3,I).NE.SORT(3,I-1)) THEN
	         CALL ISORT5(SORT(1,START),SCNT,1)
	         SCNT=0
	         START=I
	      ENDIF
	      SCNT=SCNT+1
           END DO

	   START=1
	   SCNT=1
	   DO I=2,COUNT+1
	      IF(SORT(1,I).NE.SORT(1,I-1)  .OR.
     *	         SORT(3,I).NE.SORT(3,I-1)) THEN
	         CALL ISORT5(SORT(1,START),SCNT,2)
	         SCNT=0
	         START=I
	      ENDIF
	      SCNT=SCNT+1
           END DO
        ELSE
           CALL ISORT5(SORT,COUNT,1)
        ENDIF
C
C
        IF(MIN_AMT_CSH.EQ.0.AND.MIN_AMT_REF.EQ.0) THEN
           START_LOOP=1
           END_LOOP=COUNT
           STEP=1
        ELSE
           START_LOOP=COUNT
           END_LOOP=1
           STEP=-1
        ENDIF
	DO I=START_LOOP,END_LOOP,STEP
	    BLK=I2SORT(7,I)
	    IND=I2SORT(8,I)
	    IF(BLK.NE.OLDBLK) THEN
	       CALL READW(FDB,BLK,UPREC,ST)
	       IF(ST.NE.0) CALL FILERR(SFNAMES(1,UTP),1,ST,0)
	       OLDBLK=BLK
	    ENDIF
	    CALL LOGVAL(VALREC,UPBUF(1,IND))
	    CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),SER,CHK)
	    DATE(VCDC)=VALREC(VSCDC)
	    CALL LCDATE(DATE)
	    SCDC=DATE(VJUL)

	    SSER=SER
	    SCHK=CHK
	    SAGT=AGTTAB(AGTNUM,VALREC(VSTER))
	    GAM=VALREC(VGAM)
	    VS=VALST(VALREC(VSTAT))
	    CSHAMT=VALREC(VPAMT)+VALREC(VKPAMT)-VALREC(VRAMT)
	    REFAMT=VALREC(VRAMT)
	    TAXAMT=VALREC(VTAMT)
	    CLMAMT=0
	    IF(VALREC(VSTAT).EQ.VCLAM.OR.VALREC(VSTAT).EQ.VCLAMX) THEN
               CLMAMT=CSHAMT+REFAMT
	       CSHAMT=0
	       REFAMT=0
	    ENDIF
C
C SET TOTAL PURGED WITH NO PAYMENT ORDES FOR TOTAL PURGED REPORT
C
            CALL PURGED_WITH_NO_OP(VALREC, TOTPGNOP)
C
C UPDATE TOTALS, AND WRITE INFORMATION IN PURGED REPORT
C
	    IF(LINCNT.GT.LINSPP) THEN
	        CALL TITLE('PURGED UNCASHED TICKETS','PURWIN  ',1,
     *	                    REPLU,PAGE,DAYCDC)
		WRITE(REPLU,9006)
     *	                 CMONY(MIN_AMT_CSH,12,VALUNIT),
     *                   CMONY(MIN_AMT_REF,12,BETUNIT)
	        WRITE(REPLU,9000)
	        LINCNT = 8   
	    ENDIF
	    IF(CSHAMT.GE.MIN_AMT_CSH .OR. REFAMT.GE.MIN_AMT_REF) THEN
		WRITE(REPLU,9002) SAGT,
     *                        VALREC(VSTER),
     *	                     (GLNAMES(K,GAM),K=1,4),
     *	                      SCDC,
     *                        SSER,
     *                        SCHK,
     *                        VS,
     *	                      CMONY(CSHAMT,12,VALUNIT),
     *                        CMONY(REFAMT,12,BETUNIT)
		LINCNT=LINCNT+1

		! CALCULATE RUNNING TOTALS
		TOTCSH = TOTCSH + CSHAMT
		TOTREF = TOTREF + REFAMT
		TOTTAX = TOTTAX + TAXAMT
		TOTCLM = TOTCLM + CLMAMT
		PCOUNT = PCOUNT+1
	    
		GAMTOTCSH(GAM) = GAMTOTCSH(GAM) + CSHAMT - VALREC(VKPAMT)
                IF(GNTTAB(GAMTYP,GAM).EQ.TKIK)
     *                  GAMTOTCSH(GAM) = GAMTOTCSH(GAM) + VALREC(VKPAMT)
                IF(GNTTAB(GAMTYP,GAM).NE.TKIK .AND. VALREC(VKPAMT).NE.0) THEN
			JOK_WITH_OTHER_AMT = JOK_WITH_OTHER_AMT +
     *                                     VALREC(VKPAMT)
			JOK_WITH_OTHER_CNT = JOK_WITH_OTHER_CNT + 1
		END IF

		GAMTOTREF(GAM) = GAMTOTREF(GAM) + REFAMT
		GAMTOTTAX(GAM) = GAMTOTTAX(GAM) + TAXAMT
		GAMTOTCLM(GAM) = GAMTOTCLM(GAM) + CLMAMT
		GAMCOUNT(GAM) = GAMCOUNT(GAM) + 1
	    END IF
        END DO
C
C

        ! REPORT TOTALS
	IF(LINCNT.GT.LINSPP) THEN
	    CALL TITLE('PURGED UNCASHED TICKETS','PURWIN  ',1,
     *	                REPLU,PAGE,DAYCDC) 
	    WRITE(REPLU,9000)
	    WRITE(REPLU,9006)
     *	                 CMONY(MIN_AMT_CSH,12,VALUNIT),
     *                   CMONY(MIN_AMT_REF,12,BETUNIT)
	    LINCNT = 8   
	ENDIF

        WRITE(REPLU,9003)PCOUNT,
     *	                 CMONY(TOTCSH,12,VALUNIT),
     *                   CMONY(TOTREF,12,BETUNIT)

        CALL TITLE('PURGED UNCASHED TICKETS','PURWIN  ',1,
     *	                REPLU,PAGE,DAYCDC) 

        WRITE(REPLU,9004)
        DO 4000 I = 1, MAXGAM
            IF(GAMCOUNT(I).LE.0)GOTO 4000
            WRITE(REPLU,9005)(GLNAMES(K,I),K=1,4),
     *                        GAMCOUNT(I),
     *	                      CMONY(GAMTOTCSH(I),12,VALUNIT),
     *                        CMONY(GAMTOTREF(I),12,BETUNIT)
            IF(GNTTAB(GAMTYP,I).EQ.TKIK)
     *      WRITE(REPLU,9007) (GLNAMES(K,I),K=1,4),
     *                        JOK_WITH_OTHER_CNT,
     *	                      CMONY(JOK_WITH_OTHER_AMT,12,VALUNIT)

4000    CONTINUE
C
C GENERATE PURGED REPORT WITH NO PAYMENT ORDERS INCLUDED
C
        CALL REPORT_PURGED_WITH_NO_OP(TOTPGNOP)
C
C CLOSE FILES AND SEND THE REPORTS TO THE PRINTER
C
	CALL CLOSEFIL(FDB)
	CALL USRCLOS1(     REPLU)
	CALL SPOOL('PURWIN.REP',COPY,ST)
        CALL SPOOL('PRGNOTOP.REP',COPY,ST)
C
C     =================== FORMAT STATEMENTS =====================
C
9000	FORMAT(5X,'AGENT    TERMINAL  ------GAME------',5X,
     *         'SERIAL NUMBER  STATUS',2X,'   PRIZE AMT  REFUND AMT')
C     *         '   CLAIM AMT     TAX AMT',/)
9002	FORMAT(4X,I8.8,5X,I5,2X,4A4,2X,I3.3,'-',I8.8,'-',I3.3,
     *	       4X,A4,2X,2A12)

9003    FORMAT(/,5X,'TOTAL TICKETS',28X,I8,14X,2A12)
9004    FORMAT(1X,131('='),///,
     *         20X,'GAME TOTALS',18X,'COUNT',21X,'PRIZE',6X,'REFUND')
C     *         'CLAIM',7X,'TAX')
9005    FORMAT(/,20X,4A4,10X,I8,14X,2A12)
9006    FORMAT(25X,'Minimum prize amount = ',A12,10x,
     *		  'Minimum refund amount = ',A12)
9007    FORMAT(/,20X,4A4,'with other',I8,14X,A12)

	END
