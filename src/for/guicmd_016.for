C GUICMD_016.FOR
C 
C V02 30-JUL-2003 FRP Fix bug described in Glb00275
C V01 06-FEB-2001 HXK Initial release for AlphaGOLS
C
C TERMINAL TYPE UPDATE COMMAND
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
	SUBROUTINE GUICMD_016(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
C
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 ST
	INTEGER*4 NUM_COLS, NUM_ROWS
	INTEGER*4 BUF(CDLEN)
	INTEGER*4 STATUS
	CHARACTER*40 STATUS_STR
C
C
        INTEGER*4 GNUM,BITMAP,TER
C
	RET_CODE = 0
	STATUS   = 0
	STATUS_STR = ' '
C
	CALL GUI_GETPARAMS(OUTBUF,ST)
	IF(ST.NE.0) THEN
	   RET_CODE = 11
	   RETURN
	ENDIF

	GNUM = GUI_ARGVAL(1)
	IF(GNUM.LT.0.OR.GNUM.GT.MAXGAM) THEN
	    RET_CODE = 11
	    RETURN
	ENDIF

	BITMAP = GUI_ARGVAL(2)

	TER = GUI_ARGVAL(3)
	IF(TER.LT.1.OR.TER.GT.NUMAGT) THEN
	    RET_CODE = 11
	    RETURN
	ENDIF

	IF(GNUM.EQ.0) THEN           ! GLOBAL PARAMETERS
            BUF(1) = 3
            BUF(2) = BITMAP
            BUF(3) = TCAGT
        ELSE                       ! GAME SPECIFIC
            BUF(1) = 4
            BUF(2) = BITMAP
            BUF(3) = TCAGT
	    BUF(8) = GNUM
	ENDIF
C
C Build the command
C
	BUF(5)=TER
	BUF(6)='GUI '
	CALL QUECMD(BUF,ST)
	IF(ST.NE.0) THEN
	    RET_CODE = 11
	    RETURN
	ENDIF
C
C SEND DATA TO GUI
C
100	CONTINUE
	CALL GUIARG_INIT()
C
	NUM_COLS = 2
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
C STATUS BACK
C
	CALL GUIARG_INT4(OUTBUF,STATUS)	
	CALL GUIARG_CHAR(OUTBUF,%REF(STATUS_STR),40)	
C
	CALL GUIARG_SET_MESLEN(MES_LEN)
C
	RETURN
900	FORMAT('Invalid value: <',I8,' or >',I8)
	END
