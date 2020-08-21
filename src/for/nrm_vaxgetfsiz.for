C
C SUBROUTINE VAXGETFSIZ
C $Log:   GXAFXT:[GOLS]VAXGETFSIZ.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:49:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 18:01:00   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_getfinfo.for **
C
C
C
C *** VAXGETFSIZ
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE VAXGETFSIZ(LUN, SIZE)
	IMPLICIT NONE
C
	INTEGER*4   LUN
	INTEGER*4   SIZE
C
	INTEGER*4   FOR$RAB
C
	CALL GETFSIZX1(%VAL(FOR$RAB(LUN)), SIZE)
C
C The first block is used for something else, so just subtract 1
C
	SIZE = SIZE - 1
	RETURN
	END
