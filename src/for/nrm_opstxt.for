C
C SUBROUTINE OPSTXT
C
C V01 25-JUL-2000 UXN Initial realease (produced from NRM_OPS.FOR)
C
C CALL OPSTXT("STRING")
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
	SUBROUTINE OPSTXT(STRING)
	IMPLICIT NONE
C
	CHARACTER*(*) STRING
	INTEGER*4     ST
C
	CHARACTER*132 BUFFER
C
	BUFFER=STRING
	CALL MLOG(BUFFER,ST)
C
	RETURN
	END
