C
C SUBROUTINE INPMONYI8
C $Log:   GXAFXT:[GOLS]INPMONYI8.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:37:20   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:39:42   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_inpmod.for **
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE INPMONYI8(STRING,NUM,FACTOR,EXT)
	IMPLICIT NONE
C
	CHARACTER   STRING*(*)
	INTEGER*4   NUM(2),FACTOR,EXT
C
	CALL XXXMONYI8(.FALSE., STRING,NUM,FACTOR,EXT)
	RETURN
	END
