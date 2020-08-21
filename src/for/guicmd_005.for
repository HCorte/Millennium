C GUICMD_005.FOR
C
C V01 06-FEB-2001 UXN INITIAL RELEASE FOR PORTUGAL
C
C GAMST status change to 2
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
	SUBROUTINE GUICMD_005(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:X2XGBL.DEF'
C
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 ST
	INTEGER*4 NUM_COLS, NUM_ROWS, FIELD, PAR,VAL
	INTEGER*4 CBUF(CDLEN)
C
	INTEGER*4 STATUS
	CHARACTER*40 STATUS_STR
        INTEGER*4   BYTES_PER_FIELD     !Bytes in a field              
        PARAMETER   (BYTES_PER_FIELD=4)
C
	RET_CODE = 0
	STATUS = 0
	STATUS_STR = ' '
C
C GENERAL COMMAND parameter
C
	CALL GUI_GETPARAMS(OUTBUF,ST)
	IF(ST.NE.0) THEN
	   RET_CODE = 11
	   RETURN
	ENDIF

	PAR= GUI_ARGVAL(1)
	IF(PAR.NE.1) THEN
	    STATUS_STR = 'Invalid parameter value'
	    STATUS = 1
	    GOTO 100
	ENDIF
	VAL = GUI_ARGVAL(2)
	IF(VAL.NE.2) THEN
	    STATUS_STR = 'Invalid GAMST value'
	    STATUS = 1
	    GOTO 100
	ENDIF

	IF(X2X_GAME_STATE.NE.1) THEN
	   STATUS_STR = 'Invalid game status'
	   STATUS = 1	
	   GOTO 100
	ENDIF

	FIELD = ((%LOC(X2XGBL_GAMSTAT)-%LOC(X2XGBL_SAP))/BYTES_PER_FIELD)+1  

        CBUF(1)  = 1
        CBUF(2)  = 1
        CBUF(3)  = TCX2X
        CBUF(4)  = 0
        CBUF(5)  = 0
	CBUF(6)  = 'GUI '
        CBUF(7)  = 1
        CBUF(8)  = XGBL
        CBUF(9)  = FIELD
        CBUF(10) = VAL
        CBUF(11) = 0
        CBUF(12) = 1

        CALL QUECMD(CBUF,ST)
        IF(ST.NE.0) THEN  
	    STATUS_STR = 'Command failed'
	    STATUS = 1
	    GOTO 100
	ENDIF
100     CONTINUE
C
C SEND DATA TO GUI
C
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
	END
