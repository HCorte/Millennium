C
C SUBROUTINE BSET
C $Log:   GXAFXT:[GOLS]BSET.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:22:36   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:45:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_bitsubs.for **
C
C**************************************************************************
C
C SECOND GROUP ROUTINES - COUNT BITS FROM RIGHT TO LEFT
C**************************************************************************
C
C **** BSET - SET A BIT
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE BSET(I2ARY, BITNUM)
	IMPLICIT NONE
C
C
	INTEGER*2   I2ARY(0:*)
	INTEGER*4   BITNUM
C
	INTEGER*4   WRD
	INTEGER*4   NDX
C
C
	WRD = ISHFT(BITNUM, -4)
	NDX = IAND (BITNUM, '0000000F'X)
	I2ARY(WRD) = IBSET(I2ARY(WRD), NDX)
	RETURN
	END
