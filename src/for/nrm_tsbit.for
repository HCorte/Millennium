C
C FUNCTION TSBIT
C $Log:   GXAFXT:[GOLS]TSBIT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:37:28   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:53:32   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_bitsubs.for **
C
C
C
C **** TSBIT - TEST A BIT *** THIS IS A FUNCTION
C
	LOGICAL*1 FUNCTION TSBIT(I2ARY, BITNUM)
	IMPLICIT NONE
C
	INTEGER*2   I2ARY(0:*)
	INTEGER*4   BITNUM
C
	INTEGER*4   WRD
	INTEGER*4   NDX
C
	LOGICAL BTEST
C
	WRD = ISHFT(BITNUM, -4)
	NDX = IAND (BITNUM, '0000000F'X)
	TSBIT = BTEST(I2ARY(WRD),NDX)
	RETURN
	END
