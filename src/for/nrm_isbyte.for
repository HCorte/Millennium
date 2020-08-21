C
C SUBROUTINE ISBYTE
C $Log:   GXAFXT:[GOLS]ISBYTE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:40:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:42:48   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_isbyte.for **
C
C VAX_ISBYTE.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C ISBYTE.FOR
C
C V01 06-JUL-90 TKO  VAX VERSION
C
C CALL ISBYTE(I4VAR, ARRAY, BYTENUM)
C
C This is used to store a byte into a string (see also NLBYTE)
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
	SUBROUTINE ISBYTE(I4VAR, ARRAY, BYTENUM)
	IMPLICIT NONE
C
	BYTE		I4VAR(0:3)
	BYTE		ARRAY(0:*)
	INTEGER*4	BYTENUM
C
	ARRAY(BYTENUM) = I4VAR(0)
	RETURN
	END
