C
C FUNCTION CTOI
C $Log:   GXAFXT:[GOLS]CTOI.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:46:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:01:38   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_ctoi.for **
C
C VAX_CTOI.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C CTOI.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
	INTEGER*4 FUNCTION CTOI(STRING, XLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	CHARACTER   STRING*(*)
	INTEGER*4   XLEN
C
	INTEGER*4   LEN
C
	INTEGER*4   NUM
	INTEGER*4   STRLEN
	INTEGER*4   K
C
	CALL OTS$CVT_TI_L(STRING, NUM, %VAL(4), %VAL(1))
	STRLEN = LEN(STRING)
C
C set xlen = # of digits converted
C
	XLEN=0
	DO 1100 K = 1,STRLEN
	  IF(STRING(K:K).EQ.' ')THEN
	    IF(XLEN.NE.0)GOTO 1200
	    GOTO 1100
	  ENDIF
	  XLEN = XLEN + 1
1100	CONTINUE
1200	CONTINUE
	CTOI = NUM
C
	RETURN
	END
