C
C SUBROUTINE INPDAT
C $Log:   GXAFXT:[GOLS]INPDAT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:37:06   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:39:32   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_inpmod.for **
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE INPDAT(CDC,EXT)
	IMPLICIT NONE
C
	INTEGER*4   CDC,EXT
C
	CALL XXXDAT(.FALSE., CDC,EXT)
	RETURN
	END
