C GUI_041.FOR
C
C V01 16-FEB-2001 UXN INITIAL RELEASE.
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
C Copyright 2001 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C This subroutine returns GUI FUNCTION.
C
C Input parameters:
C	NONE               
C
C Output parameters:
C
C	BYTE		OUTBUF(*)    OUTPUT MESSAGE
C	INTEGER*4	MES_LEN	     MESSAGE LENGTH
C	INTEGER*4	RET_CODE:
C		0		-  no error, message accepted;
C		value >= 11	-  error number to be sent to Client.
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUI_041(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	CALL GUI_AGTINF(OUTBUF,MES_LEN,RET_CODE,0)	
C
	END
