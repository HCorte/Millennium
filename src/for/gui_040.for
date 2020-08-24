C GUI_040.FOR
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
	SUBROUTINE GUI_040(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:HASF.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 I,J

	INTEGER*4 MENU_CNT
	PARAMETER(MENU_CNT = 7)
	INTEGER*4 COLS(MENU_CNT)
	LOGICAL*4 FIRST/.TRUE./
C
	RET_CODE = 0
C
	IF(FIRST) THEN
	   FIRST = .FALSE.
	   DO I=1,MENU_CNT
	      COLS(I) = 0
	      DO J=1,29
	         IF(FLDBEG((I-1)*29+J).NE.0) COLS(I) = COLS(I) + 1
	      ENDDO
	   ENDDO
	ENDIF
C
C Build GUI message
C
	CALL GUIARG_INIT()

	DO I = 1, MENU_CNT

	   CALL GUIARG_NEXT_SET(OUTBUF,COLS(I))
	   DO J = 1, COLS(I)
	      CALL GUIARG_CHAR(OUTBUF,FLDNAM( (I-1)*29+J ),8)
	   ENDDO

	ENDDO
C
C Set the length of the message
C
	CALL GUIARG_SET_MESLEN(MES_LEN)
	RETURN
C
	END
