C
C SUBROUTINE ICLOCK
C ** Source - nrm_iclock.for **
C
C V02 24-JAN-2011 RXK Fix for getting value of XTIM
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C ICLOCK.FOR
C
C V01 06-JUL-90 TKO  VAX VERSION OF ICLOCK
C
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
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	SUBROUTINE ICLOCK(UNITS, XTIM)
	IMPLICIT NONE
C
	INTEGER*4	UNITS
	INTEGER*4	XTIM(*)
C
        INTEGER*4       TIMI4(2)
        CHARACTER*8     CTIM
        EQUIVALENCE     (TIMI4,CTIM)
C
	REAL*4		SECNDS
	INTEGER*4	TEMP
C
	IF(UNITS.EQ.2)GOTO 2000
	IF(UNITS.EQ.1)GOTO 1000
	IF(UNITS.NE.0)GOTO 9000
C
C UNITS = 0 -> RETURN 3 INTEGERS: HOURS, MINUTES, SECONDS
C
	TEMP = SECNDS(0.0)
	XTIM(1) = TEMP/3600
	XTIM(2) = MOD(TEMP,3600)/60
	XTIM(3) = MOD(TEMP,60)
	GOTO 9000
C
C UNITS = 1 -> RETURN ASCII HH:MM:SS
C
1000	CONTINUE
	CALL TIME(CTIM)
        CALL FASTMOV(TIMI4,XTIM,2) 
	GOTO 9000
C
C UNITS = 2 -> RETURN # OF SECONDS SINCE MIDNIGHT
C
2000	CONTINUE
	TEMP = SECNDS(0.0)
	XTIM(1) = TEMP
	GOTO 9000
C
C
9000	CONTINUE
	RETURN
	END
