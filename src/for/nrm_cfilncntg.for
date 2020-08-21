C FUNCTION CFILNCNTG
C
C V02 04-JUL-2000 OXK RAB -> RAB64
C V01             XXX INITIAL RELEASE
C
C This is a useropen routine that will set 'contiguous best try' bit.
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

	INTEGER*4 FUNCTION CFILNCNTG(FAB, RAB, LUN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INCLUDE	'($SYSSRVNAM)'
	INCLUDE '($FABDEF)'
	INCLUDE '($RABDEF)'
	INCLUDE '($RMSDEF)'
C
	INTEGER*4 LUN
C
	RECORD /FABDEF/ FAB
	RECORD /RABDEF/ RAB
C
C Set the 'contiguous best try' bit
C
	FAB.FAB$L_FOP = FAB.FAB$L_FOP .OR. FAB$M_CBT
C
C Now create the file and connect to the record stream
C
	CFILNCNTG = SYS$CREATE(FAB)
	IF(.NOT.CFILNCNTG) THEN
	  CALL LIB$SIGNAL(%VAL(CFILNCNTG))
	  GOTO 9000
	ENDIF
C
	CFILNCNTG = SYS$CONNECT(RAB)
	IF(.NOT.CFILNCNTG) THEN
	  CALL LIB$SIGNAL(%VAL(CFILNCNTG))
	  GOTO 9000
	ENDIF
C
9000	CONTINUE
	RETURN
	END
