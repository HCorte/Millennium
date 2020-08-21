C $Log:   GXAFXT:[GOLS]X2ASCCDC.FOV  
C  
C     Rev 1.0   17 Apr 1996 16:07:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   24 Sep 1993  1:59:16   JWE
C  Add $Log: for PVCS
C
C X2ASCCDC.FCC
C
C This function returns the CDC day which correspnds to the ASCII
C date which we store in the ASF for start & end dates.  The format
C of the date is defined in X2ASCDAT.DCC
C
C Input the ASCII date.  A CDC number is returned.  0 is
C returned for any invalid date.
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
        INTEGER*4 FUNCTION X2ASCCDC(ASCII_DATE)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:X2ASCDAT.DEF'
C
	CHARACTER   ASCII_DATE*(*)
C
	INTEGER*2   DAY
	INTEGER*2   MONTH
	INTEGER*2   YEAR
	INTEGER*2   DATBUF(12)
C
	READ(UNIT = ASCII_DATE(DAY_OFFSET:DAY_OFFSET + 1),
	1   FMT = '(I)', ERR=8000) DAY
	IF(DAY .LT. 1 .OR. DAY .GT. 31) GOTO 8000
	READ(UNIT = ASCII_DATE(MONTH_OFFSET:MONTH_OFFSET + 1),
	1   FMT = '(I)', ERR=8000) MONTH
	IF(MONTH .LT. 1 .OR. MONTH .GT. 12) GOTO 8000
	READ(UNIT = ASCII_DATE(YEAR_OFFSET:YEAR_OFFSET + 1),
	1   FMT = '(I)', ERR=8000) YEAR
	IF(YEAR .LT. 0 .OR. YEAR .GT. 99) GOTO 8000
C
	DATBUF(VDAY) = DAY
	DATBUF(VMON) = MONTH
	DATBUF(VYEAR) = YEAR
	CALL BDATE(DATBUF)
	X2ASCCDC = DATBUF(VCDC)
C
C Normal ending, X2ASCCDC should be set before you exit
C
	GOTO 9000
C
C Come here on error
C
8000	CONTINUE
	X2ASCCDC = 0
C
C Normal exit
C
9000	CONTINUE
	RETURN
	END
