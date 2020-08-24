C
C SUBROUTINE BINASC
C $Log:   GXAFXT:[GOLS]BINASC.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:16:20   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:41:52   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_ascbin.for **
C
C
C
C *** BINASC
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE BINASC(BYTARY,OFF,LEN,NUM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	BYTE		BYTARY(*)
	INTEGER*4	OFF
	INTEGER*4	LEN
	INTEGER*4	NUM
C
	INTEGER*4       TEMP
	INTEGER*4       K
C
C
	TEMP = NUM
	DO 1100 K = OFF+LEN-1, OFF, -1
	  BYTARY(K) = MOD(TEMP,10) + ICHAR('0')
	  TEMP = TEMP/10
1100	CONTINUE
C
	RETURN
	END
