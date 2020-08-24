C
C SUBROUTINE ADDI8I8
C $Log:   GXAFXT:[GOLS]ADDI8I8.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:08:16   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:35:20   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_mathi8.for **
C
C MATHI8.FOR
C
C V01 11-MAR-91 JPJ INITIAL RELEASE FOR MARYLAND
C
C Calling sequence:
C
C     CALL ADDI8I8(AMOUNT1,AMOUNT2,FACTOR)
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
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1991 GTECH Corporation. All rights reserved.
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE ADDI8I8(AMOUNT1,AMOUNT2,FACTOR)
	IMPLICIT NONE
    	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
C
C
	INTEGER*4	AMOUNT1(2)	!RESULT AMOUNT IN BET UNITS UNITS
	INTEGER*4	AMOUNT2(2)	!AMOUNT IN UNITS
        INTEGER*4       FACTOR          !SHOWS WHICH UNIT OF MONEY TO USE
        INTEGER*4       MON_UNIT
	INTEGER*4       RESULT(2)
	REAL*8		TMPAMT1, TMPAMT2
C
C ASSIGNING VALUE TO THE UNIT OF MONEY
C
        IF (FACTOR .EQ. VALUNIT) THEN
          MON_UNIT=DYN_VALUNIT
        ELSE IF (FACTOR .EQ. BETUNIT) THEN
          MON_UNIT=DYN_BETUNIT
        ELSE
          MON_UNIT=FACTOR
        END IF
C
C
C
	TMPAMT1   = DFLOAT(AMOUNT1(1)) * DFLOAT(DYN_BETUNIT) +
     *		    DFLOAT(AMOUNT1(2))
	TMPAMT2   = DFLOAT(AMOUNT2(1)) * DFLOAT(MON_UNIT) +
     *		    DFLOAT(AMOUNT2(2))
	TMPAMT1   = TMPAMT1 + TMPAMT2
	RESULT(1) = TMPAMT1 / DFLOAT(DYN_BETUNIT)
	TMPAMT2   = TMPAMT1 - DFLOAT(RESULT(1)) * DFLOAT(DYN_BETUNIT)
	RESULT(2) = MOD(IDINT(TMPAMT2),DYN_BETUNIT)
	AMOUNT1(1) = RESULT(1)
	AMOUNT1(2) = RESULT(2)
	RETURN
	END
