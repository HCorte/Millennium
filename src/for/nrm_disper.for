C
C FUNCTION DISPER
C $Log:   GXAFXT:[GOLS]DISPER.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:54:48   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:05:36   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_percnt.for **
C
C
C
C
C SUBROUTINE TO CONVERT INTEGER TO DISPLAY PERCENTAGE AMOUNT
C
C
	DOUBLE PRECISION FUNCTION DISPER(NUM)
	IMPLICIT NONE
C
	INTEGER*4 NUM
	DISPER=DFLOAT(NUM)/1000.0D0
	RETURN
	END
