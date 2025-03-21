C
C SUBROUTINE SUBI8I8
C $Log:   GXAFXT:[GOLS]SUBI8I8.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:21:54   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:45:34   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_mathi8.for **
C
C
C Calling sequence:
C
C     CALL SUBI8I8(AMOUNT1,AMOUNT2,FACTOR)
C
C *** RESULT = AMOUNT1 - AMOUNT2
C
C Input parametrs:
C
C     AMOUNT2(2) Int*4         !THE AMOUNT OF MONEY IN UNITS WE USE
C     FACTOR     Int*4         !SHOWS WHICH VALUE OF ONE UNIT TO USE
C
C Output parameters:
C
C     AMOUNT1(2) Int*4         !THE RESULT IN BET UNITS
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SUBI8I8(AMOUNT1,AMOUNT2,FACTOR)
	IMPLICIT NONE
    	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
C
C
	INTEGER*4	AMOUNT1(2)	!RESULT IN BET UNITS
	INTEGER*4	AMOUNT2(2)	!AMOUNT IN UNITS
	INTEGER*4	RESULT(2)       !RESULT OF SUBTRACTION
        INTEGER*4       FACTOR          !SHOWS WHICH UNIT OF MONEY TO USE
C
C
C
	RESULT(1) = -AMOUNT2(1)
	RESULT(2) = -AMOUNT2(2)
	CALL ADDI8I8(AMOUNT1,RESULT,FACTOR)
	RETURN
	END
