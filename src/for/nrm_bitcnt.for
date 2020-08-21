C
C SUBROUTINE BITCNT
C $Log:   GXAFXT:[GOLS]BITCNT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:17:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:42:24   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_bitcnt.for **
C
C VAX_BITCNT.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C BITAND.FOR
C
C V01 01-AUG-90 TKO  RELEASED FOR VAX
C
C This will count the number of bits set in a byte array
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
	SUBROUTINE BITCNT(BARY, NUMBYT, COUNT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	BYTE		BARY(*)
	INTEGER*4	NUMBYT
	INTEGER*4	COUNT
C
	INTEGER*4	K
	INTEGER*4	TEMP
C
	INTEGER*4	BITSET(0:255)
C                     0 1 2 3 4 5 6 7 8 9 A B C D E F
C                     - - - - - - - - - - - - - - - -
	DATA BITSET/  0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,  !0-
     *	              1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,  !1-
     *	              1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,  !2-
     *	              2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,  !3-
     *	              1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,  !4-
     *	              2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,  !5-
     *	              2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,  !6-
     *	              3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,  !7-
     *	              1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,  !8-
     *	              2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,  !9-
     *	              2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,  !A-
     *	              3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,  !B-
     *	              2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,  !C-
     *	              3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,  !D-
     *	              3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,  !E-
     *	              4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8/  !F-
C
C
C
C
	COUNT = 0
	DO 1100 K = 1, NUMBYT
	  TEMP = BARY(K)
	  TEMP = IAND(TEMP, '000000FF'X)
	  COUNT = COUNT + BITSET(TEMP)
1100	CONTINUE
C
	RETURN
	END
