C
C SUBROUTINE NOLOKTOP
C $Log:   GXAFXT:[GOLS]NOLOKTOP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:13:14   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:08:02   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_glist.for **
C
C
C
C NOLOKTOP +++++++++++++++++++++++++++++++++++++++++++++++++++++
C Removes lock flag from top of list
C
C NOLOKTOP ----------------------------------------------------
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE NOLOKTOP(LIST_ARRAY)
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
	LIST_ARRAY (GLIST_TOP_LOCK)     =
     *        IAND(LIST_ARRAY(GLIST_TOP_LOCK), .NOT.GLIST_LOCK_FLAG )
C
	RETURN
	END
