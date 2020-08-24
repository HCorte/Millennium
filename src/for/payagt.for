C
C SUBROUTINE PAYAGT
C $Log:   GXAFXT:[GOLS]PAYAGT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:22:10   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.4   13 Dec 1994 11:46:46   HXK
C  Changed format of game number
C  
C     Rev 1.3   17 Oct 1993 20:10:36   GXA
C  Corrected setting of INFOTAB after data manipulation.
C  
C     Rev 1.2   24 Sep 1993 23:18:08   GXA
C  Added Veikkaus specific parameters.
C  
C     Rev 1.1   10 Jun 1993 14:41:52   HXN
C  Move HASF.DEF, which contains AGTINF.DEF, before RECAGT.DEF.
C  
C     Rev 1.0   21 Jan 1993 17:15:54   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - hsf_payagt.for **
C
C HASF3.FOR
C
C V04 27-MAY-01 EPH DEAL WITH 2 MORE TRANSACTION TYPES (T/R)
C V03 12-MAR-91 JPJ INITIAL RELEASE FOR MARYLAND
C V02 15-FEB-91 WOL USES NEW CHARACTER CMONY ROUTINES
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C PAYAGT - ENTER AGENTS FINANCIAL TRANSACTIONS.
C
C TRANSACTION CODE FORMAT
C
C     PAYMENT   (P) =     1 -> 10000
C     CREDIT    (C) = 10001 -> 20000
C     DEBIT     (D) = 20001 -> 30000
C     PENALTY   (X) = 30001 -> 40000
C     TRANSFER  (T) = 40001 -> 50000
C     C.RECEPCAO(R) = 50001 -> 60000
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret \information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE PAYAGT(FDB)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:HASF.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

	INTEGER*2 DATE(LDATE_LEN)
	INTEGER*4 FDB(7),INP(5),SUM(2,6),BLCODE(6),HSFBUF(6)
	INTEGER*4 CDC, NUM, TNUM, CDNUM, NETAMT(2), CURBAL(2)
	INTEGER*4 LSTBAL(2), J, K, I, AMT(2), ST, EXT, AGT
C
	INTEGER*4 WEEK			!Week Number for additional ledger info.
	INTEGER*4 YEAR			!Year Number for additional ledger info.
	INTEGER*4 CHNG			!Change Code for additional ledger info.
	INTEGER*4 GAME			!Game Number for additional ledger info.
	INTEGER*4 INFOTAB		!Table Containing additional ledger inf.
C
	CHARACTER CODE(6),INPUT(20)
	EQUIVALENCE (INP,INPUT)
	DATA BLCODE/-1,-1,1,1,1,1/
C***	DATA CODE/'P','C','D','X','T','R'/
	DATA CODE/'P','-','+','X','T','R'/
C
	BYTE	  I1TEMP(4)
	INTEGER*2 I2TEMP(2)
	INTEGER*4 I4TEMP
	EQUIVALENCE(I1TEMP,I2TEMP,I4TEMP)
C
C
10	CONTINUE
	CALL CLRSCR(5)
	CALL INPNUM('Enter terminal or #agent number (E - Exit) ',
     *	            AGT,1,NUMAGT,EXT)
	IF(EXT.LT.0) RETURN
C
C READ AGENTS RECORD
C
15	CONTINUE
	CALL CLRSCR(5)
	CALL READW(FDB,AGT,ASFREC,ST)
	IF(ST.NE.0) THEN
	  CALL CLRSCR(5)
	  CALL FILERR(SFNAMES(1,ASF),2,ST,AGT)
	  GOTO 10
	ENDIF
C
C DISPLAY AGENTS CURRENT FINANCIAL INFORMATION
C
20	CONTINUE
	AMT(1)=0
	AMT(2)=0
	CALL CLRSCR(5)
	DO 30 I=1,4
	SUM(1,I)=0
	SUM(2,I)=0
30	CONTINUE
C
	CALL CLRSCR(5)
	WRITE(5,901) AGT,(ASFBYT(K),K=IDBEG(1),IDEND(1)),
     *	             (ASFBYT(K),K=IDBEG(2),IDEND(2))
C
C SCAN LEDGER TABLE FOR CURRENT INVOICE PEROID TRANSACTIONS
C
	LSTBAL(1)=ASFINV(ASFDUEU,1)
	LSTBAL(2)=ASFINV(ASFDUEP,1)
	CURBAL(1)=LSTBAL(1)
	CURBAL(2)=LSTBAL(2)
	NETAMT(1)=0
	NETAMT(2)=0
	DO 40 I=15,1,-1
	IF(ASFLGR(LGRCDC,I).LE.ASFINV(ASFEND,1)) GOTO 40
	CDNUM=ASFLGR(LGRCOD,I)/10000+1
	TNUM=MOD(ASFLGR(LGRCOD,I),10000)
	AMT(1)=ASFLGR(LGRAMTU,I)
	AMT(2)=ASFLGR(LGRAMTP,I)
	CALL ADDI8I8(CURBAL,AMT,BETUNIT)
	CALL ADDI8I8(NETAMT,AMT,BETUNIT)
	CALL ADDI8I8(SUM(1,CDNUM),AMT(1),BETUNIT)
	DATE(5)=ASFLGR(LGRCDC,I)
	CALL LCDATE(DATE)
	INFOTAB = ASFLGR(LGRINF,I)
	I4TEMP = INFOTAB
	WEEK = I1TEMP(4)
	YEAR = I1TEMP(3)
	CHNG = I1TEMP(2)
	GAME = I1TEMP(1)
	IF(YEAR.LT.77) THEN
	    YEAR = YEAR + 2000
        ELSE
	    YEAR = YEAR + 1900
	ENDIF
C
	IF(CDNUM.EQ.1) THEN
	     WRITE(5,902) (DATE(K),K=7,13),CSMONYI8(LSTBAL,12,BETUNIT),
     *	              CODE(CDNUM),TNUM,CSMONYI8(AMT,12,BETUNIT),
     *                CSMONYI8(CURBAL,12,BETUNIT)
	     WRITE(5,920) WEEK,YEAR,CHNG,GAME
C
	ELSE IF(CDNUM.EQ.2) THEN
	     WRITE(5,903) (DATE(K),K=7,13),CSMONYI8(LSTBAL,12,BETUNIT),
     *	              CODE(CDNUM),TNUM,CSMONYI8(AMT,12,BETUNIT),
     *                CSMONYI8(CURBAL,12,BETUNIT)
	     WRITE(5,920) WEEK,YEAR,CHNG,GAME
C
	ELSE IF(CDNUM.EQ.3) THEN
	     WRITE(5,904) (DATE(K),K=7,13),CSMONYI8(LSTBAL,12,BETUNIT),
     *	              CODE(CDNUM),TNUM,CSMONYI8(AMT,12,BETUNIT),
     *                CSMONYI8(CURBAL,12,BETUNIT)
	     WRITE(5,920) WEEK,YEAR,CHNG,GAME
	ELSE IF(CDNUM.EQ.4) THEN
	     WRITE(5,917) (DATE(K),K=7,13),CSMONYI8(LSTBAL,12,BETUNIT),
     *	              CODE(CDNUM),TNUM,CSMONYI8(AMT,12,BETUNIT),
     *                CSMONYI8(CURBAL,12,BETUNIT)
	     WRITE(5,920) WEEK,YEAR,CHNG,GAME
	ELSE IF(CDNUM.EQ.5) THEN
	     WRITE(5,917) (DATE(K),K=7,13),CSMONYI8(LSTBAL,12,BETUNIT),
     *	              CODE(CDNUM),TNUM,CSMONYI8(AMT,12,BETUNIT),
     *                CSMONYI8(CURBAL,12,BETUNIT)
	     WRITE(5,920) WEEK,YEAR,CHNG,GAME
	ELSE IF(CDNUM.EQ.6) THEN
	     WRITE(5,917) (DATE(K),K=7,13),CSMONYI8(LSTBAL,12,BETUNIT),
     *	              CODE(CDNUM),TNUM,CSMONYI8(AMT,12,BETUNIT),
     *                CSMONYI8(CURBAL,12,BETUNIT)
	     WRITE(5,920) WEEK,YEAR,CHNG,GAME
	ELSE
	     WRITE(5,918) CDNUM,AGT
	ENDIF
40	CONTINUE
C
C DISPLAY SUMMARY LINE
C
50	CONTINUE
	WRITE(5,919)
	WRITE(5,905) CSMONYI8(LSTBAL,11,BETUNIT),
     *              (CSMONYI8(SUM(1,K),10,BETUNIT),K=1,4),
     *	             CSMONYI8(NETAMT,11,BETUNIT),
     *               CSMONYI8(CURBAL,11,BETUNIT)
C
C PROMPT FOR TRANSACTION
C
C
	WRITE(5,906)
	CALL WIMG(5,'Enter transaction (P/-/+/X/T/R) (E-Exit) ')
	READ (5,907) INPUT
	IF(INPUT(1).EQ.'E') GOTO 10
	TNUM=-1
	IF(INPUT(1).EQ.'P') TNUM=0
	IF(INPUT(1).EQ.'-') TNUM=10000
	IF(INPUT(1).EQ.'+') TNUM=20000
	IF(INPUT(1).EQ.'X') TNUM=30000
	IF(INPUT(1).EQ.'T') TNUM=40000
	IF(INPUT(1).EQ.'R') TNUM=50000
C***
C***	CALL ASCBIN(INP,2,4,NUM,ST)
C***
	CALL INPNUM('Enter voucher number (1-9999), E-Exit: ',
     *              NUM,1,9999,EXT)
	IF(NUM.EQ.0.OR.ST.LT.0.OR.TNUM.LT.0) THEN
	  CALL CLRSCR(5)
	  WRITE(5,909)
	  CALL BELLS(1)
	  CALL XWAIT(2,2,ST)
	  GOTO 20
	ENDIF
	TNUM=TNUM+NUM
C
C INPUT AMOUNT
C
	CALL WIMG(5,'Only for transaction equals T/R use +/- sign')
	CALL INPMONYI8('Enter Amount (MMM.PP)',AMT(1),BETUNIT,EXT)
	IF(AMT(1).EQ.0.AND.AMT(2).EQ.0) GOTO 120
        CDNUM=TNUM/10000+1
	AMT(1)=AMT(1)*BLCODE(CDNUM)
	AMT(2)=AMT(2)*BLCODE(CDNUM)
C
C GET DATE (CONCOM MUST BE INITIALIZED)
C
90	CONTINUE
	CDC=DAYCDC
	IF(CDC.LE.0) THEN
	  CALL CLRSCR(5)
	  WRITE(5,911)
	  CALL BELLS(1)
	  CALL XWAIT(2,2,ST)
	  GOTO 20
	ENDIF
C
C
	IF(P(SUPFIL).NE.0) THEN
	  CALL CLRSCR(5)
	  WRITE(5,908)
	  CALL BELLS(1)
	  CALL XWAIT(2,2,ST)
	  GOTO 20
	ENDIF
C
C CHECK FOR DUPLICATE ENTRY IN LEDGER TABLE
C
	DO 100,I=1,15
	IF(ASFLGR(LGRCDC,I).LE.ASFINV(ASFEND,1)) GOTO 110
	IF(TNUM.EQ.ASFLGR(LGRCOD,I)) THEN
	  CALL CLRSCR(5)
	  WRITE(5,915) (INPUT(K),K=1,5)
	  CALL BELLS(1)
	  CALL XWAIT(2,2,ST)
	  GOTO 20
	ENDIF
100	CONTINUE
C
C FIND SLOT IN LEDGER TABLE
C
110	CONTINUE
	IF(ASFLGR(LGRCDC,15).GT.ASFINV(ASFEND,1)) THEN
	  CALL CLRSCR(5)
	  WRITE(5,912)
	  CALL BELLS(1)
	  CALL XWAIT(2,2,ST)
	  GOTO 20
	ENDIF
C
C PROMPT FOR ADDITIONAL LEDGER INFORMATION
C
	DO I = 1,20
	   INPUT(I) = ' '
	END DO
C
C GET WEEK AND YEAR NUMBER
C
	WRITE(5,926) IAM()
	CALL WIMG(5,'Enter week and year (WW/YYYY, E-Exit): ')
	READ(5,907) INPUT
	IF(INPUT(1).EQ.'E') GOTO 10
C
	CALL ASCBIN(INP,1,2,WEEK,ST)
	IF(ST.LT.0.OR.WEEK.LT.1.OR.WEEK.GT.53) THEN
	   CALL CLRSCR(5)
	   WRITE(5,929) IAM(),WEEK
	   CALL BELLS(1)
	   CALL XWAIT(2,2,ST)
	   GOTO 20
	ENDIF
C
	IF(INPUT(3).NE.'/') THEN
	   CALL CLRSCR(5)
	   WRITE(5,930) IAM()
	   CALL BELLS(1)
	   CALL XWAIT(2,2,ST)
	   GOTO 20
	ENDIF
C
	CALL ASCBIN(INP,4,4,YEAR,ST)
	IF(ST.LT.0.OR.YEAR.LT.1989.OR.YEAR.GT.2100) THEN
	   CALL CLRSCR(5)
	   WRITE(5,939) IAM(),YEAR
	   CALL BELLS(1)
	   CALL XWAIT(2,2,ST)
	   GOTO 20
	ENDIF
C
C GET CHANGE TYPE
C
	WRITE(5,936)
	CALL INPNUM('Enter Change Type (0-3, E-Exit): ',CHNG,0,3,EXT)
	IF(EXT.NE.0) GOTO 10 
C
C GET GAME NUMBER
C
	IF(CHNG.EQ.0.OR.CHNG.EQ.3) THEN
	   GAME = 0				!No Game# for these changes.
	ELSE 
	   WRITE(5,946) IAM()
	   DO I = 1,MAXGAM
	      IF(GNTTAB(GAMTYP,I).NE.0.AND.GNTTAB(GAMIDX,I).NE.0) THEN
	         WRITE(5,947) (GLNAMES(J,I),J=1,4),I
	      ENDIF
	   END DO
	   CALL INPNUM('Enter Game Number, E-Exit: ',GAME,1,MAXGAM,EXT)
	   IF(EXT.NE.0) GOTO 10
	ENDIF
C
        WRITE(5,920) WEEK,YEAR,CHNG,GAME
        CALL XWAIT(2,2,ST)
C
	YEAR = MOD(YEAR,100)
	INFOTAB = 0
	I1TEMP(4) = WEEK
	I1TEMP(3) = YEAR
	I1TEMP(2) = CHNG
	I1TEMP(1) = GAME
	INFOTAB = I4TEMP
C
C QUEUE TRANSACTION TO SYSTEM FOR PROCESSING
C
	HSFBUF(1)=AGT
	HSFBUF(2)=TNUM
	HSFBUF(3)=AMT(1)
	HSFBUF(4)=AMT(2)
	HSFBUF(5)=0
	HSFBUF(6)=INFOTAB
	CALL QUEHSF(HSFBUF,ST)
	IF(ST.NE.0) THEN
	  WRITE(5,916) ST
	  CALL BELLS(1)
	  CALL XWAIT(2,2,ST)
	ENDIF
	CALL XWAIT(2,2,ST)
	GOTO 15
C
C ERASE ENTRY FROM LEDGER TABLE
C
120	CONTINUE
	DO 130 I=1,15
	IF(ASFLGR(LGRCOD,I).EQ.TNUM) GOTO 140
130	CONTINUE
	CALL CLRSCR(5)
	WRITE(5,913) (INPUT(K),K=1,5)
	CALL BELLS(1)
	CALL XWAIT(2,2,ST)
	GOTO 20
C
C REMOVE ENTRY FROM LEDGER TABLE
C
140	CONTINUE
	HSFBUF(1)=AGT
	HSFBUF(2)=TNUM
	HSFBUF(3)=0
	HSFBUF(4)=0
	HSFBUF(5)=1
	HSFBUF(6)=INFOTAB
	CALL QUEHSF(HSFBUF,ST)
	IF(ST.NE.0) THEN
	  WRITE(5,916) ST
	  CALL BELLS(1)
	  CALL XWAIT(2,2,ST)
	ENDIF
	CALL XWAIT(2,2,ST)
	GOTO 15
C
C
901	FORMAT(' Financial update for terminal - ',I4,/,
     *	       22X,'Agent',<LAGNO>A1,4X,<LNAME>A1,/,
     *	       4X,'DATE',7X,'   LAST BAL',1X,'PYMTS',
     *	       1X,'CREDT',1X,'DEBIT',1X,'PNLTY',6X,'AMOUNT',
     *	       6X,'CRT BAL')
902	FORMAT(1X,7A2,1X,A12,1X,A1,I4.4,18X,A12,1X,A12)
903	FORMAT(1X,7A2,1X,A12,7X,A1,I4.4,12X,A12,1X,A12)
904	FORMAT(1X,7A2,1X,A12,13X,A1,I4.4,6X,A12,1X,A12)
905	FORMAT(1X,A11,1X,4(A10,1X),A11,1X,A11)
906	FORMAT(' ',/,' Payment = (P) ',/,
     *	             ' Credit  = (-) ',/,
     *	             ' Debit   = (+) ',/,
     *	             ' Penalty = (X) ',/,
     *	       ' Transaction format:        TNNNN')
907	FORMAT(20A1)
908	FORMAT(' System file access (SUPFIL) not enabled   ')
909	FORMAT('  Invalid transaction number-transaction not accepted')
910	FORMAT('  Invalid amount - Transaction not accepted')
911	FORMAT('  Sys date not initialized - transaction not accepted')
912	FORMAT('  Ledger table full - transaction not accepted')
913	FORMAT('  Entry ',5A1,' not found in ledger table ')
915	FORMAT('  Duplicate entry ',5A1,' transaction not accepted')
916	FORMAT(' Queue error> ',I2,' transaction not processed  ')
917	FORMAT(1X,7A2,1X,A12,19X,A1,I4.4,A12,1X,A12)
918     FORMAT(1X,' invalid code number  >>> ',I4,' terminal ',I4)
919	FORMAT(//,4X,'LAST BAL',3X,'PAYMENTS',
     *	       4X,'CREDITS',5X,'DEBITS',4X,'PENALTY',6X,'AMOUNT',
     *	       5X,'CRT BAL')
920	FORMAT(1X,' Week ',I2.2,' Year ',I4.4,' Change ',I1.1,
     *         ' Game ',I2.2)
926	FORMAT(1X,A,'Reference round for adjustments as WW/YYYY,',
     *         ' (01-53/1989-2100)')
929	FORMAT(1X,A,'Invalid Week Number ',I4,
     *         ' 01-53 are valid - Transaction not accepted')
930	FORMAT(1X,A,'Invalid format, follow WW with / - ',
     *         ' Transaction not accepted')
936	FORMAT(' Change type for adjustment where :',/,
     *         ' Saldo adjustment      = 0',/,
     *         ' Sale adjustment       = 1',/,
     *         ' Validation adjustment = 2',/,
     *         ' Rent adjustment       = 3')
939	FORMAT(1X,A,'Invalid Year Number ',I4,
     *         ' 1989-2100 are valid - Transaction not accepted')
946	FORMAT(1X,A,'Game Numbers for adjustments :',/)
947	FORMAT(1X,4A4,' = ',I2)
	END
