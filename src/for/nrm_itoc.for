C
C FUNCTION ITOC
C $Log:   GXAFXT:[GOLS]ITOC.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:41:38   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:43:22   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_itoc.for **
C
C VAX_ITOC.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C ITOC.FOR
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
	CHARACTER*12 FUNCTION ITOC(NUM, LEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INTEGER*4   NUM
	INTEGER*4   LEN
C
	INTEGER*4 K
	CHARACTER*12 STRING
C
	CALL OTS$CVT_L_TI(NUM, STRING, %VAL(1), %VAL(4), %VAL(0))
	DO 1100 K = 1,12
	  IF(STRING(K:K).NE.' ')GOTO 1200
1100	CONTINUE
	K = 12
1200	CONTINUE
	ITOC = STRING(K:12)
	LEN  = 12-K+1
C
	RETURN
	END
