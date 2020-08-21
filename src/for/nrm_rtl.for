C
C SUBROUTINE RTL
C
C V02 01-AUG-1996 WS  Force a memory barrier by call to MB().  Split up
C                     LISTLOCK() and LISTRLSE() into separate routines for
C                     locking/releasing top and bottom of list array.
C
C
C RTL +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Remove from the Top of the List
C
C
C RTL ----------------------------------------------------------
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
	SUBROUTINE RTL (LIST_MEMBER, LIST_ARRAY, STATUS)
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
	CALL LISTLOCK_TOP(LIST_ARRAY)
C
C CHECK TO SEE IF LIST EMPTY
C
	IF ( LIST_ARRAY (GLIST_TOP) .EQ.
     *	     LIST_ARRAY (GLIST_BOT) ) THEN
	  LIST_ARRAY(GLIST_EMPTY_CNT) = LIST_ARRAY(GLIST_EMPTY_CNT)+1
	  STATUS = GLIST_STAT_EMPTY
	  GOTO 9000
	ENDIF
C
C ADVANCE LIST TOP
C
	LIST_OFFSET = LIST_ARRAY (GLIST_TOP) + 1
C
C CHECK IF NEED TO WRAP AROUND IN THE LIST
C
	IF (LIST_OFFSET .GT. LIST_ARRAY (GLIST_MAX_OFFSET)) THEN
	  LIST_OFFSET = GLIST_START
	ENDIF
C
C REMOVE FROM THE LIST
C
	LIST_MEMBER = LIST_ARRAY (LIST_OFFSET)
C
C UPDATE LIST HEADER INFO
C
	LIST_ARRAY (GLIST_TOP)     = LIST_OFFSET
	LIST_ARRAY (GLIST_RTL_CNT) = LIST_ARRAY (GLIST_RTL_CNT) + 1
C
C
C IF THIS WAS THE LAST ONE, INDICATE SO
C
	IF ( LIST_ARRAY( GLIST_TOP ) .EQ.
     *       LIST_ARRAY( GLIST_BOT ) ) THEN
	  STATUS = GLIST_STAT_LASTONE
	ELSE
	  STATUS = GLIST_STAT_GOOD
	ENDIF
C
9000	CONTINUE
C
C Unlock the list
C
	CALL LISTRLSE_TOP(LIST_ARRAY)
C
	RETURN
	END
