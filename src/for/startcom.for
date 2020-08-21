C
C SUBROUTINE STARTCOM
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]STARTCOM.FOV                                 $
C  $Date::   17 Apr 1996 15:18:48                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - net_netsub3.for;1 **
C
C NETSUB3.FOR
C
C V03 13-NOV-92 DAS REMOVED EVERTHING POSSIBLE (NOT MUCH LEFT HERE)
C V02 22-JAN-91 KWP REMOVED LINCOM/TERCOM/OUTQU*'S
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
C
C     START COMMUNICATION
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE STARTCOM
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*4 ST
C
C       WAIT A WHILE BEFORE ATTEMPTING TO START LANPRO
C
	CALL XWAIT(5,2,ST)
C
	IF (LANGO.NE.LANPROUP) THEN
	 CALL OPS(CHAR(7)//'**** LANPRO NOT OPERATIONAL ****',LANGO,
     *	          X2X_GAME_STATE)
C
	 LANGO=LANPROSTART
	ENDIF
C
	IF (X2X_GAME_STATE.LT.X2X_GAMES_IDLE) THEN
	 CALL OPS(CHAR(7)//'**** X2X GAME STATE INCORRECT ****',LANGO,
     *	          X2X_GAME_STATE)
	ENDIF
C
	X2X_GAME_STATE=X2X_GAMES_REQUP
C
	CALL OPS(CHAR(7)//'**** X2X COMMUNICATIONS STARTED ****',LANGO,
     *	         X2X_GAME_STATE)
C
	RETURN
	END
