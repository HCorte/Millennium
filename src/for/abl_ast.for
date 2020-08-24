C
C SUBROUTINE ABL_AST
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]ABL_AST.FOV                                  $
C  $Date::   17 Apr 1996 12:07:44                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1993 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose: Add to the Bottom of the GLIST
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE ABL_AST(LIST_MEMBER, LIST_ARRAY, STATUS)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE	'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLIST.DEF'
C
	INCLUDE '(LIB$ROUTINES)'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	K,
     *			LIST_ARRAY(*),
     *			LIST_MEMBER,
     *			LIST_OFFSET,
     *			ST,
     *			STATUS
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CHECK IF LOCK IS NEEDED
C
	IF (IAND(LIST_ARRAY(GLIST_BOT_LOCK), GLIST_LOCK_FLAG) .NE. 0) THEN
100	  CONTINUE
	  ST = LIB$BBSSI(GLIST_LOCK_BIT, LIST_ARRAY(GLIST_BOT_LOCK))
          IF (ST) THEN
	    DO 200 K = 1, GLIST_LOCK_DELAY_COUNT
	      LIST_ARRAY(GLIST_BOT_LOCK_CNT) =
     *        LIST_ARRAY(GLIST_BOT_LOCK_CNT) + 1
C
	      CALL XWAIT(GLIST_LOCK_DELAY_VALUE, 1, ST)
C
	      IF (.NOT. BTEST(LIST_ARRAY(GLIST_BOT_LOCK),
     *                        GLIST_LOCK_BIT)) GOTO 100
200	    CONTINUE
C
	    TYPE *, IAM(), 'ABL_AST LOCK AT ',
     *              %LOC(LIST_ARRAY(GLIST_BOT_LOCK)),
     *              ' ... LOCKED TOO LONG'
	    GOTO 100
	  ENDIF
	ENDIF
C
C ADVANCE GLIST BOTTOM
C
	LIST_OFFSET = LIST_ARRAY(GLIST_BOT) + 1
C
C CHECK THE NEED TO WRAP AROUND
C
	IF (LIST_OFFSET .GT. LIST_ARRAY(GLIST_MAX_OFFSET))
     *    LIST_OFFSET = GLIST_START
C
C CHECK GLIST FULL
C
	IF (LIST_OFFSET .EQ. LIST_ARRAY(GLIST_TOP)) THEN
	  LIST_ARRAY(GLIST_FULL_CNT) = LIST_ARRAY(GLIST_FULL_CNT) + 1
	  STATUS = GLIST_STAT_FULL
	  GOTO 300
	ENDIF
C
C ADD TO THE GLIST
C
	LIST_ARRAY(LIST_OFFSET)   = LIST_MEMBER
	LIST_ARRAY(GLIST_BOT)     = LIST_OFFSET
	LIST_ARRAY(GLIST_ABL_CNT) = LIST_ARRAY(GLIST_ABL_CNT) + 1
C
C SET STATUS 'GOOD'
C
	STATUS = GLIST_STAT_GOOD
C
C UNLOCK THE LIST
C
300	CONTINUE
	IF (IAND(LIST_ARRAY(GLIST_BOT_LOCK), GLIST_LOCK_FLAG) .NE. 0)
     *    ST = LIB$BBCCI(GLIST_LOCK_BIT, LIST_ARRAY(GLIST_BOT_LOCK))
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END
C
	RETURN
	END
