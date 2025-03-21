C
C FUNCTION TSTBIT_BSTRNG
C $Log:   GXAFXT:[GOLS]TSTBIT_BSTRNG.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:38:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:54:42   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_bitsubs.for **
C
C
C
C **** TSTBIT_BSTRNG - TEST A BIT COUNTING FROM LEFT IN A BYTE ARRAY.
C
	LOGICAL*1 FUNCTION TSTBIT_BSTRNG(BYTARY, BITNUM)
	IMPLICIT NONE
C
C
	BYTE	    BYTARY(0:*)
C
	INTEGER*4   BITTAB(0:7)
	INTEGER*4   BITNUM
	INTEGER*4   BYT
	INTEGER*4   NDX
	INTEGER*4   I4TEMP
	DATA	    BITTAB/'80'X,'40'X,'20'X,'10'X,'08'X,'04'X,'02'X,'01'X/
C
	TSTBIT_BSTRNG = .FALSE.
C
	BYT = ISHFT(BITNUM,-3)
	NDX = IAND(BITNUM,'07'X)
	I4TEMP = ZEXT(BYTARY(BYT))
	IF(IAND(I4TEMP,BITTAB(NDX)).NE.0) TSTBIT_BSTRNG = .TRUE.
	RETURN
	END
