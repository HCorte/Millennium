C
C SUBROUTINE DQUINP
C $Log:   GXAFXT:[GOLS]DQUINP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:00:02   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:09:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_quemgr.for **
C
C
C************************************************************
C
C	DQUINP - DEQUEUES TRANSACTION FROM INPUT QUEUE
C
C***********************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DQUINP (BUFNUM)
	IMPLICIT NONE
C
	INTEGER*4	BUFNUM
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:GLIST.DEF'
C
	INTEGER*4	STAT
C
	CALL RTL (BUFNUM, INQUE, STAT)
C
	IF (STAT .EQ. GLIST_STAT_EMPTY) THEN
		BUFNUM = 0
	ENDIF
C
	END
