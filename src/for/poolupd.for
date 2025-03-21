C
C SUBROUTINE POOLUPD
C $Log:   GXAFXT:[GOLS]POOLUPD.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:26:36   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:20:00   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - poolupd.for **
C
C POOLUPD.FOR
C
C V02 24-APR-91 MP  CLEANED SOME EXTRA PAUSES,
C		    MADE SINGLE RETURN IN FEW ROUTINES,
C		    MADE A CALL TO 'CLOSEQFILE' INSTEAD OF 'CLOSEFILE'
C		    IN 'POOLCLR'.
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V02 15-JUL-89 WS RELEASED FOR SWEDEN
C V01 01-JUN-88 XXX RELEASED FOR MICHIGAN
C
C     POOLUPD.FTN
C     ___________
C    THIS FILE CONTAINS SUBROUTINES THAT ARE USED IN LOTTO
C    POOL MAINTENANCE AND VIEWING.THE FOLLOWING SUBROUTINE
C    ARE:
C            1.POOLUPD-LOTTO POOL MAINTENANCE
C            3.GETOVR -GET # OF REPEATS FOR OVERFLOWS
C            4.INCOVR -INCREMENT OVERFLOWS
C            5.DECOVR -DECREMENT OVERFLOWS
C            6.FLUOVR(GAM) -FLUSH OVERFLOW TABLE
C            7.POOLCLR(GAM) - WILL CLEAR SELECTIVELY 1 GAM
C
C   ======================
C
C    SUBROUTINE POOLUPD-THIS SUBROUTINE WILL UPDATE THE LOTTO POOLS
C
C     CALL SEQUENCE:    CALL POOLUPD(OFFSET0,INDEX,STATUS)
C     IN-OFFSET0-LOTTO WAGER OFFSET FOR LOTTO POOLS.
C        INDEX  - CURRENT INDEX USED (FOR OVERFLOWS)
C     OUT-STATUS-0 IF OPERATION WAS SUCCESSFULL
C
C
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
	SUBROUTINE POOLUPD(OFFSET0,INDX,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
C
	INTEGER*4 ST, POOL, OFFSET2, OFFSET1, OFFSET, GAM, TAB_OFF
	INTEGER*4 INWORD, STATUS, INDX, OFFSET0
C
	INTEGER*4 UPDTAB(0:7,3)
	DATA UPDTAB/Z10000000,Z01000000,Z00100000,Z00010000
     *	           ,Z00001000,Z00000100,Z00000010,Z00000001
     *	           ,Z01000000,Z00010000,Z00000100,Z00000001,4*0
     *	           ,Z00010000,Z00000001,6*0/
C
C     ANDTAB-WILL LEAVE ONLY BITS TO CORRSPONDING OFFSET(MOD8)
C
	INTEGER*4 ANDTAB(0:7,3)
	DATA ANDTAB/ZF0000000,Z0F000000,Z00F00000,Z000F0000
     *	           ,Z0000F000,Z00000F00,Z000000F0,Z0000000F
     *	           ,ZFF000000,Z00FF0000,Z0000FF00,Z000000FF,4*0
     *	           ,ZFFFF0000,Z0000FFFF,6*0/
C
C
C     RETRIEVE SHARE OFFSET AND UPDATE IT
C
C***  TYPE *,IAM(),'OFF0, LTCURPAGE ',OFFSET0,LTCURPAG
	INWORD=LTPOOL_INWORD(LTCURPAG)
	TAB_OFF=1
	IF (INWORD.EQ.4) THEN
	   TAB_OFF=2
	ELSEIF (INWORD.EQ.2) THEN
	   TAB_OFF=3
	ENDIF
	GAM=LTPOOL_PAGGAM(LTCURPAG)
	OFFSET=ABS(OFFSET0)
	OFFSET=OFFSET-1
	OFFSET=OFFSET-LTPOOL_PAGESTART(LTCURPAG)
	OFFSET1=OFFSET/INWORD      !OFFSET IN TABLE
	OFFSET2=OFFSET-INWORD*OFFSET1    !NIBBLE OFFSET IN TABLE
	OFFSET1=OFFSET1+1
C
	POOL=IAND(LTPAGE(OFFSET1),ANDTAB(OFFSET2,TAB_OFF))
C
C     CHECK IF # OF COMBINATION OVERFLOW
C
	IF (OFFSET0.GE.0) THEN     !FOR WAGERS
	   LTPOOL_TOT(GAM)=LTPOOL_TOT(GAM)+1
C***     TYPE *,IAM(),'pool ',POOL,' offset1,2 ',OFFSET1,OFFSET2,TAB_OFF
	   IF (POOL.NE.ANDTAB(OFFSET2,TAB_OFF)) THEN
	     LTPAGE(OFFSET1)=LTPAGE(OFFSET1)+UPDTAB(OFFSET2,TAB_OFF)
	   ELSE
30	     CONTINUE
	     CALL ABL(GAM,ADDOVR(1,INDX),ST)  !UPDATE OVERFLOW
	     IF (ST.NE.0) THEN
	      CALL XWAIT(100,1,ST)          !WAIT A WHILE
	      GOTO 30
	     ENDIF
35	     CONTINUE
	     OFFSET=OFFSET0-LTPOOL_BASEOFF(GAM)
	     CALL ABL(OFFSET,ADDOVR(1,INDX),ST)  !UPDATE OVERFLOW
	     IF (ST.NE.0) THEN
	      CALL XWAIT(100,1,ST)          !WAIT A WHILE
	      GOTO 35
	     ENDIF
C***       TYPE *,IAM(),'GAM, OFF, ',GAM,OFFSET,LTCURPAG
	     LTPAGE(OFFSET1)=IEOR(LTPAGE(OFFSET1),POOL)
	   ENDIF
	ELSE                           ! FOR CANCELATIONS/DELETIONS
	   LTPOOL_TOT(GAM)=LTPOOL_TOT(GAM)-1
	   IF (POOL.NE.0) THEN
	     LTPAGE(OFFSET1)=LTPAGE(OFFSET1)-UPDTAB(OFFSET2,TAB_OFF)
	   ELSE
	     OFFSET0=ABS(OFFSET0)
40	     CONTINUE
	     CALL ABL(GAM,REMOVR(1,INDX),ST)  !UPDATE OVERFLOW
	     IF (ST.NE.0) THEN
	      CALL XWAIT(100,1,ST)          !WAIT A WHILE
	      GOTO 40
	     ENDIF
45	     CONTINUE
	     OFFSET=OFFSET0-LTPOOL_BASEOFF(GAM)
	     CALL ABL(OFFSET,REMOVR(1,INDX),ST)  !UPDATE OVERFLOW
	     IF (ST.NE.0) THEN
	      CALL XWAIT(100,1,ST)          !WAIT A WHILE
	      GOTO 45
	     ENDIF
C
	   LTPAGE(OFFSET1)=IOR(LTPAGE(OFFSET1),ANDTAB(OFFSET2,TAB_OFF))
	   ENDIF
	ENDIF
C
	RETURN
	END
