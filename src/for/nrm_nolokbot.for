C
C SUBROUTINE NOLOKBOT
C $Log:   GXAFXT:[GOLS]NOLOKBOT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:13:10   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:07:56   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_glist.for **
C
C
C
C NOLOKBOT +++++++++++++++++++++++++++++++++++++++++++++++++++++
C Removes lock flag from bottom of list
C
C NOLOKBOT ----------------------------------------------------
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE NOLOKBOT(LIST_ARRAY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:GLIST.DEF'
C
	INTEGER*4	LIST_ARRAY(*)
C
C
C
C
	LIST_ARRAY (GLIST_BOT_LOCK)     =
     *        IAND(LIST_ARRAY(GLIST_BOT_LOCK), .NOT.GLIST_LOCK_FLAG )
C
	RETURN
	END
