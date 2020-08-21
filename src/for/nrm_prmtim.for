C
C SUBROUTINE PRMTIM
C $Log:   GXAFXT:[GOLS]PRMTIM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:31:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:22:12   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_inpmod.for **
C
C
C SUBROUTINE TO ENTER TIME
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE PRMTIM(STRING,TIME,EXT)
	IMPLICIT NONE
C
	CHARACTER   STRING*(*)
	INTEGER*4   TIME,EXT
C
	CALL XXXTIM(.TRUE., STRING,TIME,EXT)
	RETURN
	END
