C
C SUBROUTINE SPTANL
C $Log:   GXAFXT:[GOLS]SPTANL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:14:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:41:12   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - sptanl.for **
C
C SPTANL.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-APR-90  TDM INITIAL RELEASE FOR DENMARK
C
C     THIS ROUTINE WILL DISPLAY/CALCULATE DISTRIBUTION
C     OF REDUCED SYSTEM BETS
C
C     USER CAN ASSIGN LUN 6 TO LOG DISTRIBUTION
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SPTANL(FILE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:RECSSF.DEF'
	INTEGER*4 SYSNR/0/
	INTEGER*4 UBET
	INTEGER*4 FDB(7)
	INTEGER*4 DUBLEREQ, COMB, DBL, TRI, GIDX, EXT, ANS, CMD, ST
C
	CHARACTER*4 STATUS(0:1)
	DATA STATUS /'INVL','GOOD'/
	INTEGER*4 FILE(5)
	INTEGER*4 FIRST/1/,LAST/1/,NUM_SHARES/1/
C
C OPEN SPORTS SYSTEM FILE
C
	CALL CLRSCR(5)
	CALL OPENQW(2,FILE,4,0,0,ST)
	CALL IOQINIT(FDB,2,SSFSEC*256)
	IF(ST.NE.0) CALL FILERR(FILE,1,ST,0)
	CALL READQW(FDB,1,SPSATR,ST)
	IF (ST.NE.0) THEN
	   TYPE *,' cannot read SSP.FIL ',ST
	   CALL GPAUSE
	   CALL CLOSEQFIL(FDB)
	   RETURN
	ENDIF
C
	SPSTST=1
10	CONTINUE
	TYPE *,' 1 - to display system bet'
	TYPE *,' 2 - analyze distribution of winnings'
	TYPE *,' 3 - change first ',FIRST,' system or last ',LAST
	TYPE *,' 4 - number of different shares to display ',NUM_SHARES
	CALL INPNUM('Enter command ',CMD,1,4,ST)
	IF (ST.LT.0) CALL CLOSEQFIL(FDB)
	IF (ST.LT.0) RETURN
	GOTO (100,200,300,400) CMD
	GOTO 10
C
100	CONTINUE
	TYPE*,'(1)-FULL SYSTEM '
	TYPE*,'(2)-REDUCED OR U SYSTEM '
	CALL INPNUM('Enter 1 or 2',ANS,1,2,EXT)
	IF(EXT.LT.0) GOTO 10
	IF(ANS.EQ.1) GOTO 120
C
C PRINT REDUCED SYSTEMS AND U_SYSTEMS
C
	DO 110, SYSNR=FIRST,LAST
	CALL SPTDSP(SYSNR)
110	CONTINUE
	GOTO 10
C
C PRINT VALID FULL SYSTEMS OUT
C
120	CONTINUE
	CALL INPNUM('Enter game index ',GIDX,1,NUMSPT,EXT)
	IF(EXT.LT.0) GOTO 10
	DO 130 TRI=0,15
	DO 130 DBL=0,15
	COMB=(3**TRI)*(2**DBL)
	IF(SPSFSF(TRI,DBL,GIDX).EQ.1) THEN
	   WRITE(6,140) TRI,DBL,COMB,
     *	                STATUS(SPSFSF(TRI,DBL,GIDX))
	ENDIF
140	FORMAT(1X,'Triples =',I2,'  Doubles =',I2,
     *	       '   Combinations =',I7,'  Status = ',A4)
130	CONTINUE
	GOTO 10
C
200	CONTINUE
	UBET=0
	DO 210, SYSNR=FIRST,LAST
	DUBLEREQ=0
	IF (SPSATR(SYSNR).NE.USYS) DUBLEREQ=1
	DUBLEREQ=1    !to limit # of checked combinations
	CALL SPTODDS(SYSNR,UBET,DUBLEREQ,NUM_SHARES)
210	CONTINUE
	GOTO 10
C
300	CONTINUE
	TYPE*,'Enter first system '
	CALL INPNUM('Enter system number ',FIRST,1,SPGSYS,EXT)
	IF(EXT.LT.0) GOTO 10
C
C
	TYPE*,'Enter last system number'
	CALL INPNUM('Enter system number ',LAST,FIRST,SPGSYS,EXT)
	IF(EXT.LT.0) GOTO 10
C
C
400	CONTINUE
	CALL INPNUM('Enter # of different shares to display ',
     *	             NUM_SHARES,1,SPGNBR,ST)
	GOTO 10
	END
