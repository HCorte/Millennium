C
C SUBROUTINE PRMPER
C $Log:   GXAFXT:[GOLS]PRMPER.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:31:24   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:22:02   DAB
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
C
C SUBROUTINE TO ENTER PERCENTAGE AMOUNTS
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE PRMPER(STRING,NUM,EXT)
	IMPLICIT NONE
C
	CHARACTER   STRING*(*)
	INTEGER*4   NUM,EXT
C
	CALL XXXPER(.TRUE., STRING,NUM,EXT)
	RETURN
	END
