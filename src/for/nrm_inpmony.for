C
C SUBROUTINE INPMONY
C $Log:   GXAFXT:[GOLS]INPMONY.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:37:18   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:39:38   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_inpmod.for **
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE INPMONY(STRING,NUM,FACTOR,EXT)
	IMPLICIT NONE
C
	CHARACTER   STRING*(*)
	INTEGER*4   NUM,FACTOR,EXT
C
	CALL XXXMONY(.FALSE., STRING,NUM,FACTOR,EXT)
	RETURN
	END
