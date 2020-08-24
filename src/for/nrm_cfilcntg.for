C FUNCTION CFILCNTG
C
C V03 04-JUL-2000 OXK RAB -> RAB64
C V02 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C			    Instant Pass Thru Phase 1
C V01 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C
C This is a useropen routine that will get rid of the 'contiguous best try'
C bit and set the 'contiguous' bit.
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
	INTEGER*4 FUNCTION CFILCNTG(FAB, RAB, LUN)
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
C Clear the 'best try' bit and set the 'contiguous' bit
C
	FAB.FAB$L_FOP = FAB.FAB$L_FOP .AND. .NOT.FAB$M_CBT
	FAB.FAB$L_FOP = FAB.FAB$L_FOP .OR.       FAB$M_CTG
C
C Now create the file and connect to the record stream
C
	CFILCNTG = SYS$CREATE(FAB)
	IF(.NOT.CFILCNTG) THEN
	  CALL LIB$SIGNAL(%VAL(CFILCNTG))
	  GOTO 9000
	ENDIF
C
	CFILCNTG = SYS$CONNECT(RAB)
	IF(.NOT.CFILCNTG) THEN
	  CALL LIB$SIGNAL(%VAL(CFILCNTG))
	  GOTO 9000
	ENDIF
C
9000	CONTINUE
	RETURN
	END
