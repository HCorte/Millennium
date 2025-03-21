C SUBROUTINE POOLSYS
C
C V05 15-JUN-2000 OXK CLEANUP W/ WARNINGS=ALL
C V04 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C			Instant Pass Thru Phase 1
C V03 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C V02 01-AUG-1990 XXX RELEASED FOR VAX
C V01 19-JUL-1989 WS  INITIAL RELEASE FOR SWEDEN
C
C     SET OF SUBROUTINES TO UPDATE POOLS FOR SYSTEM BETS,
C     DESIGNED TO BE CALLED BY POOLPRO
C
C     ENTRIES:
C     POOLSYS - UPDATE POOLS WITH SYSTEM BET DATA
C
C     UPDATSYS - UPDATE POOLS FOR FULL SYSTEM
C     BITBOARD - CONVERT BITMASK TO BOARD
C     POOLUPD2 - UPDATE POOLS WITH SINGLE COMBINATION
C
C     POOLSYS(SYSNR,INTRVL_BOARD,ADD)
C     IN:
C     SYSNR  - SYETEM NUMBER
C     INTRVL_BOARD - SYSTEM BET, IN INTERVAL FORMAT
C     ADD    - 0 IF POOLS SHOULD BE INCREMENTED
C              -1 IF DECREMENTED
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
	SUBROUTINE POOLSYS(SYSNR,INTRVL_BOARD,ADD)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LSYSCOM.DEF'
	INTEGER*4 FROM_CHOSE, OFF, PTR, ST, MAXMRK, NUMBET
	INTEGER*4 NUMMRK, TO_CHOSE, GAME, ADD, SYSNR
	INTEGER*4 BOARD(32)
	INTEGER*4 OFFSETS(10)
	INTEGER*4 INTRVL_BOARD(*)
	INTEGER*4 BITMSK
	INTEGER*4 I4
	INTEGER*2 I2(2)
	EQUIVALENCE (I2,I4)
C
C     GET AND CHECK SYSTEM BET INFORMATION
C
	IF (SYSNR.LE.0) RETURN
	IF (SYSNR.GT.LSYSMAX) RETURN
	GAME=LSYS_GAME(SYSNR)
	IF (GAME.LE.0) RETURN
	IF (GAME.GT.LSYS_MAXGAM) RETURN
	TO_CHOSE=LSYS_GAMCHOSE(GAME)
	NUMMRK=LSYS_NUMMRK(SYSNR)
	NUMBET=LSYS_NUMBET(SYSNR)
	MAXMRK=LSYS_GAMFROM(GAME)    !HIGHEST MARK USED
C
C     CONVERT SYSTEM BET TO BOARD REPRESANTATION
C
	CALL INTOFF(1,NUMMRK,MAXMRK,INTRVL_BOARD,ST,BOARD,OFFSETS)
C
	IF(ST.NE.0) RETURN
C
C     TRY TO EXPAND THE BET
C
C     POINT TO ALL "SUBBETS" AREA
C
	PTR=LSYS_PTR(SYSNR)
C
C     AND PROCESS ALL BETS
C
	DO 10, OFF=1,NUMBET
	   FROM_CHOSE=LSYS_TAB(PTR)        !# OF MARKS ON SUBBET
	   I4=LSYS_TAB(PTR+1)          !BET MASK
	   IF (L_SYSBYTES.EQ.2) THEN
	      I2(1)=I2(2)
	      I2(2)=0
	   ENDIF
	   BITMSK=I4
C
C        UPDATE POOLS WITH THUS FOUND BET
C
	   CALL UPDATSYS(BOARD,BITMSK,FROM_CHOSE,TO_CHOSE,GAME,ADD)
	   PTR=PTR+2
10	CONTINUE
C
	RETURN
	END
