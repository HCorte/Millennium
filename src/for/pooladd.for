C
C SUBROUTINE POOLADD
C $Log:   GXAFXT:[GOLS]POOLADD.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:24:28   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:17:52   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pooladd.for **
C
C POOLADD.FOR
C
C V01 27-AUG-90 TKO  RELEASED FOR VAX
C
C This routine adds all the nibbles in as many words as requested.
C
C CALL POOLADD(BYTARY, # OF FULLWORDS, RESULT)
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
	SUBROUTINE POOLADD(BYTARY, NUMWRDS, RESULT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	BYTE	    BYTARY(*)
	INTEGER*4   RESULT
C
	INTEGER*4   NUMBYTES
	INTEGER*4   NUMWRDS
	INTEGER*4   RESULT2
	INTEGER*4   K
	INTEGER*4   X
C
C
	RESULT  = 0
	RESULT2 = 0
	NUMBYTES = ISHFT(NUMWRDS, 2)	    !NUMBYTES = NUMWRDS * 4
	DO 1900 K = 1, NUMBYTES
	  X = ZEXT(BYTARY(K))
	  RESULT = RESULT + IAND(X,'0F'X)
	  RESULT2= RESULT2+ IAND(X,'F0'X)
1900	CONTINUE
	RESULT = RESULT + ISHFT(RESULT2, -4)
C
	RETURN
	END
