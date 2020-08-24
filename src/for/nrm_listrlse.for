C
C SUBROUTINE LISTRLSE
C $Log:   GXAFXT:[GOLS]LISTRLSE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:49:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:51:14   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_glist.for **
C
C
C
C LISTRLSE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Release lock entire list (top and bottom)
C
C LISTRLSE ----------------------------------------------------------
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE LISTRLSE ( LIST_ARRAY )
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:GLIST.DEF'
C
	INTEGER*4	LIST_ARRAY(*)
C
C Unlock the list
C
	CALL LIB$BBCCI(GLIST_LOCK_BIT, LIST_ARRAY(GLIST_BOT_LOCK))
	CALL LIB$BBCCI(GLIST_LOCK_BIT, LIST_ARRAY(GLIST_TOP_LOCK))
C
	RETURN
	END
