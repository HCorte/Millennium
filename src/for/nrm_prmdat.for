C
C SUBROUTINE PRMDAT
C $Log:   GXAFXT:[GOLS]PRMDAT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:30:14   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:21:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_inpmod.for **
C
C
C
C
C
C **** INPDAT ENTRY
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE PRMDAT(CDC,EXT)
	IMPLICIT NONE
C
	INTEGER*4   CDC,EXT
C
	CALL XXXDAT(.TRUE., CDC,EXT)
	RETURN
	END
