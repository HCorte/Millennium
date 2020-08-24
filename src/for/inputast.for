C
C SUBROUTINE INPUTAST
C $Log:   GXAFXT:[GOLS]INPUTAST.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:38:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:40:12   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_screen.for **
C
C
C
C **** INPUTAST
C
	SUBROUTINE INPUTAST(PASTEBOARD_ID, EVFLAG)
	IMPLICIT NONE
C
	INTEGER*4   PASTEBOARD_ID
	INTEGER*2   EVFLAG
C
C
	CALL SYS$SETEF(%VAL(EVFLAG))
C
	RETURN
	END
