C
C FUNCTION NTSBIT
C $Log:   GXAFXT:[GOLS]NTSBIT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:15:02   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:10:12   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_bitsubs.for **
C
C
C
C **** NTSBIT - TEST A BIT *** THIS IS A FUNCTION
C
	LOGICAL*1 FUNCTION NTSBIT(I4ARY, BITNUM)
	IMPLICIT NONE
C
	INTEGER*4   I4ARY(0:*)
	INTEGER*4   BITNUM
C
	INTEGER*4   WRD
	INTEGER*4   NDX
C
	LOGICAL BTEST
C
	WRD = ISHFT(BITNUM, -8)
	NDX = IAND (BITNUM, '0000001F'X)
	NTSBIT = BTEST(I4ARY(WRD),31-NDX)
	RETURN
	END
