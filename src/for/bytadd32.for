C
C SUBROUTINE BYTADD32
C $Log:   GXAFXT:[GOLS]BYTADD32.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:23:42   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:46:18   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - bytadd32.for **
C
C BYTADD32.FOR
C
C V01 27-AUG-90 TKO  RELEASED FOR VAX
C
C This routine adds 32 consecutive bytes
C
C CALL BYTADD32(BYTARY, RESULT)
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
	SUBROUTINE BYTADD32(BYTARY, RESULT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	BYTE	    BYTARY(32)
	INTEGER*4   RESULT
C
	INTEGER*4   K
	INTEGER*4   X
C
C
	RESULT = 0
	DO 1900 K = 1, 32
	  X = ZEXT(BYTARY(K))
	  RESULT = RESULT + X
1900	CONTINUE
C
	RETURN
	END
