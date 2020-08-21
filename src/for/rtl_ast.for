C
C SUBROUTINE RTL_AST
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]RTL_AST.FOV                                  $
C  $Date::   17 Apr 1996 14:46:18                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - rtl_ast.for ***
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	REMOVE FROM THE TOP OF THE LIST (ASYNCHRONOUS).
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE RTL_AST(LIST_MEMBER, LIST_ARRAY, STATUS)
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
     *			STATUS,
     *			ST
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CHECK IF LOCK IS NEEDED.
C
	IF (IAND(LIST_ARRAY(GLIST_TOP_LOCK),
     *           GLIST_LOCK_FLAG) .NE. 0) THEN
100	  CONTINUE
	  ST = LIB$BBSSI(GLIST_LOCK_BIT, LIST_ARRAY(GLIST_TOP_LOCK))
	  IF (ST) THEN
	    DO 200 K = 1, GLIST_LOCK_DELAY_COUNT
	      LIST_ARRAY( GLIST_TOP_LOCK_CNT) =
     *        LIST_ARRAY( GLIST_TOP_LOCK_CNT) + 1
C
	      CALL XWAIT_AST(GLIST_LOCK_DELAY_VALUE, 1, ST)
C
	      IF (.NOT. BTEST(LIST_ARRAY(GLIST_TOP_LOCK),
     *                        GLIST_LOCK_BIT)) GOTO 100
200	    CONTINUE
C
	    TYPE *, IAM(), 'RTL_AST LOCKED TOO LONG AT ',
     *              %LOC(LIST_ARRAY(GLIST_TOP_LOCK))
	    GOTO 100
	  ENDIF
	ENDIF
C
C CHECK TO SEE IF LIST EMPTY
C
	IF (LIST_ARRAY(GLIST_TOP) .EQ. LIST_ARRAY(GLIST_BOT)) THEN
	  LIST_ARRAY(GLIST_EMPTY_CNT) = LIST_ARRAY(GLIST_EMPTY_CNT) + 1
	  STATUS = GLIST_STAT_EMPTY
	  GOTO 300
	ENDIF
C
C ADVANCE LIST TOP.
C
	LIST_OFFSET = LIST_ARRAY(GLIST_TOP) + 1
C
C CHECK IF NEED TO WRAP AROUND IN THE LIST.
C
	IF (LIST_OFFSET .GT. LIST_ARRAY(GLIST_MAX_OFFSET))
     *    LIST_OFFSET = GLIST_START
C
C REMOVE FROM THE LIST.
C
	LIST_MEMBER = LIST_ARRAY(LIST_OFFSET)
C
C UPDATE LIST HEADER INFO.
C
	LIST_ARRAY(GLIST_TOP)     = LIST_OFFSET
	LIST_ARRAY(GLIST_RTL_CNT) = LIST_ARRAY(GLIST_RTL_CNT) + 1
C
C
C IF THIS WAS THE LAST ONE, INDICATE SO
C
	IF (LIST_ARRAY(GLIST_TOP) .EQ. LIST_ARRAY(GLIST_BOT)) THEN
	  STATUS = GLIST_STAT_LASTONE
	ELSE
	  STATUS = GLIST_STAT_GOOD
	ENDIF
C
C UNLOCK THE LIST.
C
300	CONTINUE
	IF (IAND(LIST_ARRAY(GLIST_TOP_LOCK),
     *           GLIST_LOCK_FLAG) .NE. 0)
     *    ST = LIB$BBCCI(GLIST_LOCK_BIT, LIST_ARRAY(GLIST_TOP_LOCK))
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
	RETURN
	END
