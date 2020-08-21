C
C SUBROUTINE ABL
C
C V02 01-AUG-1996 WS  Force a memory barrier by call to MB().  Split up
C                     LISTLOCK() and LISTRLSE() into separate routines for
C                     locking/releasing top and bottom of list array.
C
C
C ABL +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Add to the Bottom of the GLIST
C
C ABL ----------------------------------------------------------
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE ABL(LIST_MEMBER, LIST_ARRAY, STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE	'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLIST.DEF'
C
	INTEGER*4	LIST_MEMBER
	INTEGER*4	LIST_ARRAY(*)
	INTEGER*4	STATUS
C
C DECLARE LOCAL VARIABLES
C
	INTEGER*4	LIST_OFFSET
C
	VOLATILE LIST_ARRAY
C
C CHECK IF LOCK IS NEEDED
C
	CALL LISTLOCK_BOT(LIST_ARRAY)
C
C ADVANCE GLIST BOTTOM
C
	LIST_OFFSET = LIST_ARRAY (GLIST_BOT) + 1
C
C CHECK THE NEED TO WRAP AROUND
C
	IF (LIST_OFFSET .GT. LIST_ARRAY (GLIST_MAX_OFFSET)) THEN
	  LIST_OFFSET = GLIST_START
	ENDIF
C
C CHECK GLIST FULL
C
	IF (LIST_OFFSET .EQ. LIST_ARRAY (GLIST_TOP)) THEN
	  LIST_ARRAY (GLIST_FULL_CNT) = LIST_ARRAY (GLIST_FULL_CNT)+1
	  STATUS = GLIST_STAT_FULL
	  GOTO 9000
	ENDIF
C
C ADD TO THE GLIST
C
	LIST_ARRAY (LIST_OFFSET)   = LIST_MEMBER
	CALL MB()
	LIST_ARRAY (GLIST_BOT)     = LIST_OFFSET
	LIST_ARRAY (GLIST_ABL_CNT) = LIST_ARRAY (GLIST_ABL_CNT) + 1
C
C SET STATUS 'GOOD'
C
	STATUS = GLIST_STAT_GOOD
C
9000	CONTINUE
C
C Unlock the list
C
	CALL LISTRLSE_BOT(LIST_ARRAY)
C
	RETURN
	END
