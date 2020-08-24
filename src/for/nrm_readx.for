C
C SUBROUTINE READX
C $Log:   GXAFXT:[GOLS]READX.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:39:16   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:27:52   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_readx.for **
C
C VAX_READX.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C READX.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C THIS ROUTINE WILL DO A READ & RETURN THE ACTUAL # OF
C CHARACTERS INPUT
C
C CALL READX(LUN,STRING,INLEN)
C
C    LUN = LOGICAL UNIT #
C STRING = CHARACTER STRING TO CONTAIN INPUT
C  INLEN = LENGTH OF INPUT
C
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE READX(LUN,STRING,INLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INTEGER*4 LUN
	CHARACTER STRING*(*)
	INTEGER*4 INLEN
C
C
	READ(LUN, 101) INLEN, STRING
101	FORMAT(Q,A)
C
	RETURN
	END
