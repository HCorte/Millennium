C
C FUNCTION CALPER
C $Log:   GXAFXT:[GOLS]CALPER.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:23:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:46:32   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_percnt.for **
C
C
C
C SUBROUTINE TO CONVERT INTEGER TO REAL PERCENTAGE AMOUNT
C
C
	DOUBLE PRECISION FUNCTION CALPER(NUM)
	IMPLICIT NONE
C
	INTEGER*4 NUM
	CALPER=DFLOAT(NUM)/100000.0D0
	RETURN
	END
