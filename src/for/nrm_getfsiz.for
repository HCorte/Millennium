C
C SUBROUTINE GETFSIZ
C $Log:   GXAFXT:[GOLS]GETFSIZ.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:20:10   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:25:44   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_getfinfo.for **
C
C
C
C *** GETFSIZ
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETFSIZ(LUN, SIZE)
	IMPLICIT NONE
C
	INTEGER*4   LUN
	INTEGER*4   SIZE
C
	CALL VAXGETFSIZ(LUN,SIZE)
	SIZE = SIZE * 2
C
	END
