C GUI_010.FOR
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
	SUBROUTINE GUI_010(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 GTYP
	INTEGER*4 NUM_COLS,NUM_ROWS
C
	RET_CODE = 0
C
C Build GUI message
C
	CALL GUIARG_INIT()
C
	NUM_COLS = 2
	NUM_ROWS = MAXTYP 

	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
	DO 100 GTYP=1,MAXTYP
	   IF(GTNAMES(GTYP).EQ.'        ') GOTO 100
	   CALL GUIARG_BYTE(OUTBUF,GTYP)
	   CALL GUIARG_CHAR(OUTBUF,GTNAMES(GTYP),8)
100	CONTINUE
C
	CALL GUIARG_SET_MESLEN(MES_LEN)
C
	END
