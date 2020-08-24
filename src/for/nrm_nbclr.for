C
C SUBROUTINE NBCLR
C $Log:   GXAFXT:[GOLS]NBCLR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:08:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:04:48   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_bitsubs.for **
C
C
C
C **** NBCLR - CLEAR A BIT COUNTING FROM THE LEFT IN INTEGER*4 ARRAY
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE NBCLR(I4ARY, BITNUM)
	IMPLICIT NONE
C
C
	INTEGER*4   I4ARY(0:*)
	INTEGER*4   BITNUM
C
	INTEGER*4   WRD
	INTEGER*4   NDX
C
C
	WRD = ISHFT(BITNUM, -8)
	NDX = IAND (BITNUM, '0000001F'X)
	I4ARY(WRD) = IBCLR(I4ARY(WRD), 31-NDX)
	RETURN
	END
