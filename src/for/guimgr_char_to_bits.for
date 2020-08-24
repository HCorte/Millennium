C GUIMGR_CHAR_TO_BITS.FOR
C
C V02 31-OCT-2000 UXN GUI prefix added.
C V01 08-JUL-1993 MP  RELEASE FOR VAX 
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C This routine converts string of '1' and '0' into bit map.
C INPUT:
C	STRING - 32-character string
C OUTPUT:
C	BITS   - INTEGER*4 with bits (right to left)
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUIMGR_CHAR_TO_BITS(STRING,BITS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
C
	CHARACTER*(*)	STRING
	INTEGER*4	BITS
C
	INTEGER*4   I			!Work variables
C
	BITS=0				!Initialize
C
	DO 1000 I=32,1,-1
	    IF(STRING(I:I).EQ.'1') THEN
		BITS = IBSET(BITS, 32-I)
	    ELSE IF(STRING(I:I).NE.'0') THEN
		TYPE *,IAM(),'CHAR_TO_BITS: bad string: ', STRING
	        RETURN
	    ENDIF
1000	CONTINUE
C
	RETURN
	END
