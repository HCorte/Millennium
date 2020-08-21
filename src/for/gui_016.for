C GUI_016.FOR
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
C Copyright 1999 GTECH Corporation. All rights reserved.
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
	SUBROUTINE GUI_016(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GUIMPRM.DEF'
C
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 NUM_COLS,NUM_ROWS
C
        INTEGER*4  I
C
	RET_CODE = 0
C
C Build GUI message
C
	CALL GUIARG_INIT()
C
	NUM_ROWS = 1
	NUM_COLS = 21+NUMCRS
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
        CALL GUIARG_INT2(OUTBUF,P(GVTGPL))
        CALL GUIARG_INT2(OUTBUF,P(MAXDWN))
        CALL GUIARG_BYTE(OUTBUF,P(GVTSUP))
        CALL GUIARG_BYTE(OUTBUF,P(GVTDFL))

        CALL GUIARG_BYTE(OUTBUF,P(GVTIVL))
        CALL GUIARG_BYTE(OUTBUF,P(BCHSIZ))
        CALL GUIARG_BYTE(OUTBUF,P(VALPRNT))
        CALL GUIARG_BYTE(OUTBUF,P(ACTPRNT))

        CALL GUIARG_BYTE(OUTBUF,P(SETPRNT))
        CALL GUIARG_BYTE(OUTBUF,P(RETPRNT))
        CALL GUIARG_BYTE(OUTBUF,P(DELPRNT))
        CALL GUIARG_BYTE(OUTBUF,P(FWDCNT))

        CALL GUIARG_BYTE(OUTBUF,P(GVTBMP))
        CALL GUIARG_BYTE(OUTBUF,P(GVTAUTH))
        CALL GUIARG_BYTE(OUTBUF,P(ISSPRNT))
        CALL GUIARG_BYTE(OUTBUF,P(ESTFLG))

        CALL GUIARG_TIME(OUTBUF,P(STARTIM))
        CALL GUIARG_TIME(OUTBUF,P(ENDTIM))
        CALL GUIARG_MONY(OUTBUF,P(VALPAMT))
        CALL GUIARG_MONY(OUTBUF,P(GVTSUP))

	CALL GUIARG_MONY(OUTBUF,DAYIVAL)
        DO I=1,NUMCRS
           CALL GUIARG_INT4(OUTBUF,DAYCRS(I))
	ENDDO
C
C Set message length
C
        CALL GUIARG_SET_MESLEN(MES_LEN)
C
	RETURN
C
	END
