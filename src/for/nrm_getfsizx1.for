C SUBROUTINE GETFSIZX1
C  
C V03 04-JUL-2000 OXK RAB -> RAB64
C V02 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C V01 21 Jan 1993 DAB Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETFSIZX1(RAB, SIZE)
	IMPLICIT NONE
C
	INCLUDE	    '($RABDEF)'
C
	RECORD	    /RABDEF/ RAB
	INTEGER*4   SIZE
	INTEGER*4   FABADR
C
	FABADR = RAB.RAB$L_FAB
C
	CALL GETFSIZX2(%VAL(FABADR), SIZE)
	RETURN
	END
