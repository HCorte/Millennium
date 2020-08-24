C
C SUBROUTINE ILBYTE
C $Log:   GXAFXT:[GOLS]ILBYTE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:35:38   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:38:18   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_ilbyte.for **
C
C VAX_ILBYTE.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C ILBYTE.FOR
C
C V01 06-JUL-90 TKO  VAX VERSION
C
C CALL ILBYTE(I4VAR, ARRAY, BYTENUM)
C
C This is used to load a byte from a string (see also NLBYTE)
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
	SUBROUTINE ILBYTE(I4VAR, ARRAY, BYTENUM)
	IMPLICIT NONE
C
	INTEGER*4	I4VAR
	BYTE		ARRAY(0:*)
	INTEGER*4	BYTENUM
C
	INTEGER*4	I4TEMP/0/
	BYTE		I1TEMP(0:3)
	EQUIVALENCE	(I4TEMP,I1TEMP)
C
	I1TEMP(0) = ARRAY(BYTENUM)
	I4VAR     = I4TEMP
	RETURN
	END
