C
C V01 15-OCT-2000 UXN Initial release.
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
C=======OPTIONS/CHECK=NOOVERFLOW
	SUBROUTINE HOST_TO_TERM(MSG, VAL, NR_BYTES)
	IMPLICIT NONE
	BYTE MSG(*)
	INTEGER*4 VAL, NR_BYTES
	INTEGER*4 I4TEMP
	BYTE      I1TEMP(4)
	EQUIVALENCE (I4TEMP,I1TEMP)
	
	I4TEMP = VAL
	IF(NR_BYTES.EQ.4) THEN
  	   MSG(1) = I1TEMP(4)
	   MSG(2) = I1TEMP(3)
	   MSG(3) = I1TEMP(2)
	   MSG(4) = I1TEMP(1)
	ELSEIF(NR_BYTES.EQ.3) THEN
  	   MSG(1) = I1TEMP(3)
	   MSG(2) = I1TEMP(2)
	   MSG(3) = I1TEMP(1)
	ELSEIF(NR_BYTES.EQ.2) THEN
  	   MSG(1) = I1TEMP(2)
	   MSG(2) = I1TEMP(1)
	ELSEIF(NR_BYTES.EQ.1) THEN
  	   MSG(1) = I1TEMP(1)
	ELSE
	   CALL OPS('HOST_TO_TERM: invalid NR_BYTES', NR_BYTES, 0)
	ENDIF
	END
