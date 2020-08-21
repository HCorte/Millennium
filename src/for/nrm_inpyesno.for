C
C SUBROUTINE INPYESNO
C $Log:   GXAFXT:[GOLS]INPYESNO.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:38:08   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:40:22   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_inpyesno.for **
C
C
C
C
	SUBROUTINE INPYESNO(OUTPUTSTRING, YESNOFLAG)
	IMPLICIT NONE
C
	CHARACTER   OUTPUTSTRING*(*)
	INTEGER*4   YESNOFLAG
C
	CALL XXXYESNO(.FALSE., OUTPUTSTRING, YESNOFLAG)
	RETURN
	END
