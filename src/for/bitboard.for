C
C SUBROUTINE BITBOARD
C $Log:   GXAFXT:[GOLS]BITBOARD.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:17:02   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:42:20   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - poolsys.for **
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C     SHOULD BE REWRITTEN INTO MORE EFFICIENT ASSEMBLY CODE
C
C     BITBOARD(MASK,BOARD,MARKS)
C     IN:
C     MASK - BIT MASK OF BET
C     TIMES- NUMBER OF MARKS
C     OUT:
C     BOARD- BOARD
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE BITBOARD(MASK,BOARD,MARKS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INTEGER*4 BOARD(*)
	INTEGER*4 MASK
	INTEGER*4 MARKS
	INTEGER*4 OFF, SET
C
	INTEGER*4  MAXIMUM_BITS
	PARAMETER (MAXIMUM_BITS=32)  !MAXIMUM # OF BITS TO CHECK
C
	SET=0
	DO 10, OFF=0,MAXIMUM_BITS-1           !CHECK 2 FULL WORDS
	   IF (.NOT.TSBIT(MASK,OFF)) GOTO 10
	   SET=SET+1
	   BOARD(SET)=OFF+1
C***     TYPE *,IAM(),'OFF,SET,MARKS ',OFF,SET,MARKS
	   IF (MARKS.EQ.SET) RETURN
10	CONTINUE
	RETURN
	END
