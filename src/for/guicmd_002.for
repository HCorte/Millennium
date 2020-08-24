C GUICMD_002.FOR
C
C V01 14-NOV-2000 UXN Initial release.
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
	SUBROUTINE GUICMD_002(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
C
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 ST
	INTEGER*4 NUM_COLS, NUM_ROWS, PAR, VAL
	INTEGER*4 BUF(CDLEN)
C
	INTEGER*4 STATUS
	CHARACTER*40 STATUS_STR
C
        INTEGER*4 PARAM_CNT
        PARAMETER(PARAM_CNT=1)
        INTEGER*4 CMD_TYP,PAR_MIN,PAR_MAX
        INTEGER*4 PARM(3,PARAM_CNT)
            PARAMETER(CMD_TYP=1)
            PARAMETER(PAR_MIN=2)
            PARAMETER(PAR_MAX=3)
C
        DATA PARM/ 1, 0, 1/           ! CHECKPOINT

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

	PAR = GUI_ARGVAL(1)
	IF(PAR.LT.1.OR.PAR.GT.PARAM_CNT) THEN ! should be the same as in CMDGEN
	    RET_CODE = 11
	    RETURN
	ENDIF
	VAL = GUI_ARGVAL(2)          
	IF(VAL.LT.PARM(PAR_MIN,PAR).OR.VAL.GT.PARM(PAR_MAX,PAR)) THEN
	    STATUS = 1
	    WRITE(STATUS_STR,900) PARM(PAR_MIN,PAR),PARM(PAR_MAX,PAR)
	    GOTO 100
	ENDIF
C
C Build the command
C
        BUF(1)=PARM(CMD_TYP,PAR)
        BUF(2)=VAL
        BUF(3)=TCGEN
	BUF(6)='GUI '
	CALL QUECMD(BUF,ST)
	IF(ST.NE.0) THEN
	    RET_CODE = 11
	    RETURN
	ENDIF
100	CONTINUE
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
900     FORMAT('Invalid value: <',I8,' or >',I8)
	END
