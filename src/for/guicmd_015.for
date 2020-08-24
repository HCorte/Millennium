C GUICMD_015.FOR
C
C V01 01-FEB-2001 HXK INITIAL RELEASE
C
C HOTLINE SCREEN COMMANDS
C
C 1. PASS NUMBER CHANGE
C 2. BRO
C 3. OPSTAT CHANGE
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
	SUBROUTINE GUICMD_015(OUTBUF,MES_LEN,RET_CODE)
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
	INTEGER*4 NUM_COLS, NUM_ROWS, PAR, VAL
	INTEGER*4 TER
	INTEGER*4 BUF(CDLEN)
	INTEGER*4 STATUS
	CHARACTER*40 STATUS_STR
C
	INTEGER*4 PARAM_CNT 
	PARAMETER(PARAM_CNT=3)
	INTEGER*4 PAR_ID,PAR_MIN,PAR_MAX,PAR_CMD,PAR_PRM
	INTEGER*4 PARM(5,PARAM_CNT)
	    PARAMETER(PAR_ID=1)
	    PARAMETER(PAR_MIN=2)
	    PARAMETER(PAR_MAX=3)
	    PARAMETER(PAR_CMD=4)
	    PARAMETER(PAR_PRM=5)
C
	INTEGER*4 PASNUM,BRONUM,OPSTAT
	    PARAMETER(PASNUM=2)
	    PARAMETER(BRONUM=5)
	    PARAMETER(OPSTAT=1)
C
	DATA PARM/ PASNUM, 0, 99999, TCAGT, 1,
     *             BRONUM, 0, 4,     TCSPE, 0,
     *             OPSTAT, 0, 2,     TCAGT, 0/
C
	RET_CODE = 0
	STATUS   = 0
	STATUS_STR = ' '
C
C  P() parameter
C
	CALL GUI_GETPARAMS(OUTBUF,ST)
	IF(ST.NE.0) THEN
	   RET_CODE = 11
	   RETURN
	ENDIF

	PAR = GUI_ARGVAL(1)
	IF(PAR.LT.1.OR.PAR.GT.PARAM_CNT) THEN
	    RET_CODE = 11
	    RETURN
	ENDIF
	VAL = GUI_ARGVAL(2)
	IF(VAL .LT. PARM(PAR_MIN,PAR) .OR. 
     *	    VAL .GT. PARM(PAR_MAX,PAR)) THEN
	    STATUS = 1
	    WRITE(STATUS_STR,900) PARM(PAR_MIN,PAR),PARM(PAR_MAX,PAR)
	    GOTO 100
	 ENDIF
	TER = GUI_ARGVAL(3)
	IF(TER.LT.1 .OR. TER.GT.NUMAGT) THEN
	    STATUS = 2
	    WRITE(STATUS_STR,901) TER
	    GOTO 100
	 ENDIF
C
C Build the command
C
        BUF(1)=PARM(PAR_ID,PAR)
        BUF(2)=VAL
        BUF(3)=PARM(PAR_CMD,PAR)
        BUF(4)=PARM(PAR_PRM,PAR)
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
901	FORMAT('Invalid terminal#: ',I8)
	END
