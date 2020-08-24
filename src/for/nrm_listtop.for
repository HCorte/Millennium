C
C SUBROUTINE LISTTOP
C V02 01-AUG-1996 WS  Force a memory barrier by call to MB().  Split up
C                     LISTLOCK() and LISTRLSE() into separate routines for
C                     locking/releasing top and bottom of list array.
C
C LISTTOP +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C	GET THE ELEMENT FROM THE TOP OF THE QUEUE WITHOUT REMOVING
C
C **** NOTE THAT THIS DOES NOT LOCK THE QUEUE
C
C LISTTOP ----------------------------------------------------------
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
	SUBROUTINE LISTTOP (LIST_MEMBER, LIST_ARRAY, STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
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
C
C CHECK TO SEE IF LIST EMPTY
C
	IF ( LIST_ARRAY (GLIST_TOP) .EQ. !PARAMETER	(GLIST_TOP = 2)
     *	     LIST_ARRAY (GLIST_BOT) ) THEN !PARAMETER	(GLIST_BOT = 5)
	  STATUS = GLIST_STAT_EMPTY
	  GOTO 9000
	ENDIF
C
C ADVANCE LIST TOP
C
	LIST_OFFSET = LIST_ARRAY (GLIST_TOP) + 1 !sobe um nível no index
C
C CHECK IF NEED TO WRAP AROUND IN THE LIST
C
	IF (LIST_OFFSET .GT. LIST_ARRAY (GLIST_MAX_OFFSET)) THEN
	  LIST_OFFSET = GLIST_START !so points to the start of the list/array???
	ENDIF
C
C REMOVE FROM THE LIST
C
	LIST_MEMBER = LIST_ARRAY (LIST_OFFSET)
C
C IF THIS WAS THE LAST ONE, INDICATE SO
C
	IF ( LIST_OFFSET .EQ.
     *       LIST_ARRAY( GLIST_BOT ) ) THEN
	  STATUS = GLIST_STAT_LASTONE
	ELSE
	  STATUS = GLIST_STAT_GOOD
	ENDIF
C
9000	CONTINUE
C
	RETURN
	END
