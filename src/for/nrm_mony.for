C
C FUNCTION MONY
C $Log:   GXAFXT:[GOLS]MONY.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:03:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:01:08   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_bigamt.for **
C
C BIGAMT.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 MTK INITIAL RELEASE FOR SWEDEN
C
C SUBROUTINE TO CONVERT 4 BYTE UNSIGNED INTEGER TO BASE UNIT
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
	DOUBLE PRECISION FUNCTION MONY(NUM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	DOUBLE PRECISION BIG,SBIT
C
	INTEGER*4 TEMP, HMASK, LMASK, NUM
C
	DATA LMASK/Z7FFFFFFF/,HMASK/Z80000000/
	DATA SBIT/2147483648.0D0/
C
C
	TEMP=IAND(NUM,LMASK)
	BIG=DFLOAT(TEMP)
	IF(IAND(NUM,HMASK).NE.0) BIG=BIG+SBIT
	MONY=BIG/2.0D0
	RETURN
	END
