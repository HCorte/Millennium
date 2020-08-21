C SUBROUTINE POOLSPT
C
C V09 10-MAR-2000 OXK SPGNBR used in filling LOCAL_TAB & INDEX_TAB, NOT NUMROWS!
C V08 14-FEB-2000 OXK NUMROWS received as a parameter (Vakio changes)
C V07 24-FEB-1999 UXN Reduced systems statictics calculation moved to UPDSTA
C V06 26-JAN-1996 RXK Rfss 94166. Fix for calculation of statistics.
C                     For reduced systems statistics are now calculated here.
C V05 29-SEP-1993 GXA Do not force system number to one here, 
C                     (It is done in DSPORT) for FULSYS.
C V04 11-AUG-1993 HXN To remain compatible with SPTSYS.FIL, 
C                     force SYSNR to 1 if it is a Full System Bet.
C V03 10-AUG-1993 HXN To remain compatible with SPTSYS.FIL, 
C                     force SYSNR to 1 if it is a Full System Bet.
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C     UPDATE POOLS WITH SPORT SYSTEM BET
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW

	SUBROUTINE POOLSPT(SYSNR,BOARD,GAME,ADD,NUMROWS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:STACOM.DEF'
C
	INTEGER*4 CNT, INDEX, NEXT_BOARD, NUM_BOARDS, PTR
	INTEGER*4 FIRST_U, OFF, NUMROWS, ADD, GAME, SYSNR, ONE
	INTEGER*4 TAB(16)
	INTEGER*4 UTAB(16)
	INTEGER*4 BOARD(*)
	INTEGER*4 INDEX_TAB(16)
	INTEGER*4 LOCAL_TAB(16)
	INTEGER*4 IND(3)
	INTEGER*4 BITCNT(0:15) /0,1,1,2,1,2,2,3, 0,1,1,1,1,1,1,1/
	INTEGER*4 CNV_TAB(0:7,7) !TABLE FOR NEW WINNER TRANSFORMATION
	DATA CNV_TAB/0,0,0,1, 0,1,2,0
     *	            ,0,0,0,2, 0,4,4,0
     *	            ,0,0,0,3, 0,5,6,0
     *	            ,0,0,0,4, 0,2,1,0
     *	            ,0,0,0,5, 0,3,3,0
     *	            ,0,0,0,6, 0,6,5,0
     *	            ,0,0,0,7, 0,7,7,0/
	INTEGER*4 XFORM(0:7,4)
	DATA XFORM/ 0,1,2,3,4,5,6,7,
     *	            0,2,1,3,4,6,5,7,
     *	            0,0,0,0,0,0,0,0,
     *	            0,4,1,5,2,6,3,7/
C     U BETS WITH 2 MARKS XFORMATION
C
C     BET   U-MARK   DEFINED   SUBSTITUTE
C       3   1        1         1
C       3   2        1         2
C       3   1        2         2
C       3   2        2         1
C       3   1        3         3
C       3   2        3         3
C       3   1        4         4
C       3   2        4         4
C       3   1        5         5
C       3   2        5         6
C       3   1        6         6
C       3   2        6         5
C       3   1        7         7
C       3   2        7         7
C
C       5   1        1         1
C       5   4        1         4
C       5   1        2         4
C       5   4        2         1
C       5   1        3         5
C       5   4        3         5
C       5   1        4         2
C       5   4        4         2
C       5   1        5         3
C       5   4        5         6
C       5   1        6         6
C       5   4        6         3
C       5   1        7         7
C       5   4        7         7
C
C       6   2        1         2
C       6   4        1         4
C       6   2        2         4
C       6   4        2         2
C       6   2        3         6
C       6   4        3         6
C       6   2        4         1
C       6   4        4         1
C       6   2        5         3
C       6   4        5         5
C       6   2        6         5
C       6   4        6         3
C       6   2        7         7
C       6   4        7         7
C
	INTEGER*4 XFORM2(1:4,1:7,3:6)  !U-MARK/DEFINED/BET
	DATA XFORM2/
     *	 1,2,0,0, 2,1,0,0, 3,3,0,0, 4,4,0,0, 5,6,0,0, 6,5,0,0, 7,7,0,0,
     *	 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
     *	 1,0,0,4, 4,0,0,1, 5,0,0,5, 2,0,0,2, 3,0,0,6, 6,0,0,3, 7,0,0,7,
     *	 0,2,0,4, 0,4,0,2, 0,6,0,6, 0,1,0,1, 0,3,0,5, 0,5,0,3, 0,7,0,7/
	LOGICAL U_SYSTEM

C
	IF (GAME.LE.0) RETURN
C
C     NO U SYSTEM LOGIC IMPLEMENTED AT THIS TIME
C
C
	IF(SYSNR.LT.1.OR.SYSNR.GT.SPGSYS) RETURN
C
C	TYPE*,' Poolspt. SYSNR,NUMROWS =',SYSNR,NUMROWS
C
	DO 20, OFF=1,NUMROWS           !GET BOARD IN I*4 FORMAT
	   CALL GETNIBLE(TAB(OFF),BOARD,OFF)
20	CONTINUE
C
	U_SYSTEM=.FALSE.
	IF (SPSATR(SYSNR).EQ.USYS) THEN
	   U_SYSTEM=.TRUE.
	   FIRST_U=((NUMROWS+1)/2)*2+1
	   DO 25, OFF=1,NUMROWS           !GET BOARD IN I*4 FORMAT
	      CALL GETNIBLE(UTAB(OFF),BOARD,FIRST_U+OFF-1)
25	   CONTINUE
	ENDIF
C


C	TYPE*,' Poolspt. SPSATR(SYSNR) =',SPSATR(SYSNR)
	IF (SPSATR(SYSNR).EQ.FULSYS) THEN  !PROCESS FULL SYSTEM
	   CALL POOLSPFL(TAB,NUMROWS,GAME,ADD)
	   RETURN
	ENDIF
C
C     REDUCED SYSTEM BET
C

C	TYPE*,' REDUCED SYSTEM !!!'
C
	IND(1)=SPSNUM(3,SYSNR)
	IND(2)=SPSNUM(2,SYSNR)
	IND(3)=1
	DO 30, OFF=1,NUMROWS  !RELATION BETWEEN BET AND BET DEFINITION
	   INDEX_TAB(IND(BITCNT(TAB(OFF))))=OFF
	   IND(BITCNT(TAB(OFF)))=IND(BITCNT(TAB(OFF)))+1
30	CONTINUE
	PTR=SPSPTR(SYSNR)
	NUM_BOARDS=SPSTAB(PTR)
C
	IF (.NOT.U_SYSTEM) THEN
	   DO 50, NEXT_BOARD=1,NUM_BOARDS    !PROCESS ALL BOARDS
	      DO 40, OFF=1,SPGNBR            !NEXT BOARD
	         PTR=PTR+1
		 IF (OFF.GT.NUMROWS) GOTO 40
	         INDEX=INDEX_TAB(OFF)
	         CNT=BITCNT(TAB(INDEX))
	         IF (CNT.EQ.3) THEN
	            LOCAL_TAB(INDEX)=SPSTAB(PTR)
	         ELSEIF (CNT.EQ.2) THEN
	            LOCAL_TAB(INDEX)=CNV_TAB(TAB(INDEX),SPSTAB(PTR))
	         ELSEIF (CNT.EQ.1) THEN
	            LOCAL_TAB(INDEX)=IAND(TAB(INDEX),7)
	         ENDIF
40	      CONTINUE
C
C UPDATE STATISTICS TABLE
C
              ONE=1
              IF(ADD.EQ.-1) ONE=-1
C
	      CALL POOLSPFL(LOCAL_TAB,NUMROWS,GAME,ADD)  !UPDATE POOLS
C
50	   CONTINUE
	ELSE
	   DO 80, NEXT_BOARD=1,NUM_BOARDS    !PROCESS ALL BOARDS
	      DO 60, OFF=1,NUMROWS           !NEXT BOARD
	         PTR=PTR+1
	         INDEX=INDEX_TAB(OFF)
	         CNT=BITCNT(TAB(INDEX))
	         IF (UTAB(INDEX).NE.0) THEN
	            IF (CNT.EQ.3) THEN
	               LOCAL_TAB(INDEX)=XFORM(SPSTAB(PTR),UTAB(INDEX))
	            ELSE
	               LOCAL_TAB(INDEX)=
     *	                 XFORM2(UTAB(INDEX),SPSTAB(PTR),TAB(INDEX))
	            ENDIF
	         ELSE
	            LOCAL_TAB(INDEX)=IAND(TAB(INDEX),7)
	         ENDIF
60	      CONTINUE
C
	      CALL POOLSPFL(LOCAL_TAB,NUMROWS,GAME,ADD)  !UPDATE POOLS
80	   CONTINUE
	ENDIF
	RETURN
	END
