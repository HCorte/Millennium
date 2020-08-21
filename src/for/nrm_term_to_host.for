C
C V01 10-OCT-2000 UXN Initial release.
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
	SUBROUTINE TERM_TO_HOST(MSG, VAL, NR_BYTES)
	IMPLICIT NONE
	BYTE MSG(*)
	INTEGER*4 VAL, NR_BYTES
	INTEGER*4 I4TEMP
	BYTE      I1TEMP(4)
	EQUIVALENCE (I4TEMP,I1TEMP)
	
	I4TEMP    = 0
	IF(NR_BYTES.EQ.4) THEN
	   I1TEMP(4) = MSG(1)
	   I1TEMP(3) = MSG(2)
	   I1TEMP(2) = MSG(3)  
	   I1TEMP(1) = MSG(4)
	ELSEIF(NR_BYTES.EQ.3) THEN
	   I1TEMP(3) = MSG(1)
	   I1TEMP(2) = MSG(2)  
	   I1TEMP(1) = MSG(3)
	ELSEIF(NR_BYTES.EQ.2) THEN
	   I1TEMP(2) = MSG(1)  
	   I1TEMP(1) = MSG(2)
	ELSEIF(NR_BYTES.EQ.1) THEN
	   I1TEMP(1) = MSG(1)
	ELSE
	   CALL OPS('TERM_TO_HOST: invalid NR_BYTES', NR_BYTES, 0)
	ENDIF	
	VAL = I4TEMP
	END 
