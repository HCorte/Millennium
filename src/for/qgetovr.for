C
C SUBROUTINE QGETOVR
C
C V01 11-Sep-1997 UXN Produced from GETOVR.
C
C====================================================================
C    SUBROUTINE QGETOVR-GET THE NUMBER OF REPEATS FOR OVERFLOWS
C
C    CALL SEQUENCE:   CALL QGETOVR(OFFSET,TIMES,GAM)
C    IN-OFFSET=OFFSET OF NUMBER SEQUENCE OF LOTTO BOARD
C        GAM - GAM OFFSET BELONGS TO
C    OUT-TIMES=NUMBER OF TIMES OFFSET WAS BET ON.
C
C    Call QGETOVR_CLOSE to close the OVRPOOL.FIL
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
	SUBROUTINE QGETOVR(OFFSETS,GAM,MAXOFF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INCLUDE 'INCLIB:POOLLTO.DEF'
C
	INTEGER*2 OFFSETS(*)
	INTEGER*4 OFF1, BLOCK, ST, OFF, GAM
	INTEGER*4 FDB(7),MAXOFF            !FDB BLOCK FOR WRITE
	INTEGER*4 OVERFLOWS(2,MAXOVR)!TABLE TO READ OVERFLOWS FROM FILE
C
	DO 100, OFF=1,MAXOVR-1
	IF (LTOVR(2,OFF,GAM).EQ.0) GOTO 100
	OFFSETS(LTOVR(1,OFF,GAM)) = OFFSETS(LTOVR(1,OFF,GAM)) + 
     *                              LTOVR(2,OFF,GAM)
	IF(MAXOFF.LT.LTOVR(1,OFF,GAM)) MAXOFF = LTOVR(1,OFF,GAM)
100	CONTINUE
C
	IF (OVRBLK.EQ.0) RETURN
C
	  CALL OPENW(1,SFNAMES(1,LO1),4,0,0,ST)   !OPEN OVERFLOW FILE
	  IF (ST.NE.0) THEN
	    CALL FILERR(SFNAMES(1,LO1),1,ST,0)
	    CALL GSTOP(GEXIT_FATAL)
	  ENDIF
	  CALL IOINIT(FDB,1,MAXOVR*8/256*256)     !INITIALISE FOR I/O
C
C     READ FROM POOLOVR.FIL      ;READ OVERFLOWS FROM FILE
C
	DO 230, BLOCK=1,OVRBLK
	  CALL READW(FDB,BLOCK,OVERFLOWS(1,1),ST)
	  IF (ST.NE.0) THEN
	    CALL FILERR(SFNAMES(1,LO1),2,ST,BLOCK)
	    CALL GSTOP(GEXIT_FATAL)
	  ENDIF
	  IF (OVERFLOWS(1,MAXOVR).NE.GAM) GOTO 230
	  DO 220, OFF1=1,MAXOVR-1
	   IF (OVERFLOWS(2,OFF1).EQ.0) GOTO 230
	   OFFSETS(OVERFLOWS(1,OFF1)) = OFFSETS(OVERFLOWS(1,OFF1)) +
     *                                  OVERFLOWS(2,OFF1)
	   IF(MAXOFF.LT.OVERFLOWS(1,OFF1)) MAXOFF = OVERFLOWS(1,OFF1)
220	CONTINUE
230	CONTINUE
	CALL CLOSEFIL(FDB)
	END
