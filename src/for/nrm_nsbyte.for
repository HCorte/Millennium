C
C SUBROUTINE NSBYTE
C $Log:   GXAFXT:[GOLS]NSBYTE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:14:46   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:09:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_nsbyte.for **
C
C VAX_NSBYTE.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C NSBYTE.FOR
C
C V01 06-JUL-90 TKO  VAX VERSION
C
C CALL NSBYTE(I4VAR, ARRAY, BYTENUM)
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
	SUBROUTINE NSBYTE(I4VAR, ARRAY, BYTENUM)
	IMPLICIT NONE
C
	BYTE		I4VAR(0:3)
	BYTE		ARRAY(0:*)
	INTEGER*4	BYTENUM
C
	INTEGER*4	TEMP
	INTEGER*4	BOFF
C
C On concurrent, in an I4 value the most significant byte is 0, the least
C significant byte is 3.  On dec, the most significant byte is 3, the least
C is byte 0.  Thus, the following code will find what word the referenced
C byte is in, then will treat them as reversed.
C
	TEMP = ISHFT(BYTENUM,-2)
	TEMP = ISHFT(TEMP,+2)		!TEMP = (BYTENUM/4)*4
	BOFF = IAND(BYTENUM,'03'X)	!BOFF = MOD(BYTENUM,4)
	BOFF = (3-BOFF) + TEMP		!COMPLEMENT I4 INDEX
	ARRAY(BOFF) = I4VAR(0)
	RETURN
	END
