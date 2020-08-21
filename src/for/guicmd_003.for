C GUICMD_003.FOR
C 
C V02 30-MAR-2010 RXK SUPRET,SUPGRE added.
C V01 14-NOV-2000 UXN Initial release.
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
	SUBROUTINE GUICMD_003(OUTBUF,MES_LEN,RET_CODE)
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
	INTEGER*4 STATUS
	CHARACTER*40 STATUS_STR
C
	INTEGER*4 PARAM_CNT 
	PARAMETER(PARAM_CNT=6)
	INTEGER*4 PAR_ID,PAR_MIN,PAR_MAX	
	INTEGER*4 GLOB_PARAM(3,PARAM_CNT)
	    PARAMETER(PAR_ID=1)
	    PARAMETER(PAR_MIN=2)
	    PARAMETER(PAR_MAX=3)
C
	DATA GLOB_PARAM/ SUPWAG, 0, 1,
     *                   SUPCAN, 0, 1,
     *                   SUPVAL, 0, 1,     
     *                   SUPRET, 0, 1,     
     *                   REDIMN, 0, 99999999,    
     *                   REDDEF, 0, 99999999/

	INTEGER*4 GAM_PARAM(3,PARAM_CNT) ! GAME SPECIFIC PARAMS
        DATA GAM_PARAM/  SUPGWA, 0, 1,
     *                   SUPGCA, 0, 1,
     *                   SUPGVA, 0, 1,     
     *                   SUPGRE, 0, 1,     
     *                        4, 0, 99999999,   ! REDMIN IS NOT IN P()    
     *                        3, 0, 99999999/   ! REDMAX IS NOT IN P()
        INTEGER*4 GNUM   
	INTEGER*4 BITMAP(2)
C
	INTEGER*4 DOLVAL
	EXTERNAL  DOLVAL
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

	PAR = GUI_ARGVAL(2)
	IF(PAR.LT.1.OR.PAR.GT.PARAM_CNT) THEN
	    RET_CODE = 11
	    RETURN
	ENDIF
	VAL = GUI_ARGVAL(3)
	IF(GNUM.EQ.0) THEN           ! GLOBAL PARAMETERS
      	    IF(VAL .LT. GLOB_PARAM(PAR_MIN,PAR) .OR. 
     *	       VAL .GT. GLOB_PARAM(PAR_MAX,PAR)) THEN
	       STATUS = 1
	       WRITE(STATUS_STR,900) GLOB_PARAM(PAR_MIN,PAR),
     *                               GLOB_PARAM(PAR_MAX,PAR)
	       GOTO 100
	    ENDIF
            BUF(1) = GLOB_PARAM(PAR_ID,PAR)
            IF(PAR.EQ.5.OR.PAR.EQ.6) THEN
               BUF(2) = DOLVAL(VAL)
            ELSE 
               BUF(2) = VAL
            ENDIF
            BUF(3) = TCPAR
        ELSE                       ! GAME SPECIFIC
      	    IF(VAL .LT. GAM_PARAM(PAR_MIN,PAR) .OR. 
     *	       VAL .GT. GAM_PARAM(PAR_MAX,PAR)) THEN
	       STATUS = 1
	       WRITE(STATUS_STR,900) GAM_PARAM(PAR_MIN,PAR),
     *                               GAM_PARAM(PAR_MAX,PAR)
	       GOTO 100
	    ENDIF
	    IF(PAR.EQ.5.OR.PAR.EQ.6) THEN
               BUF(1) = GAM_PARAM(PAR_ID,PAR)
               BUF(2) = DOLVAL(VAL)
               BUF(3) = TCGEN
	       BUF(8) = GNUM
	    ELSE
               BITMAP(1) = P( GAM_PARAM(PAR_ID,PAR) )
               BITMAP(2) = P( GAM_PARAM(PAR_ID,PAR)+1 )
               IF(VAL.EQ.0) THEN
                  CALL BCLR(BITMAP,GNUM)
               ELSE
                  CALL BSET(BITMAP,GNUM)
               ENDIF
               BUF(1) = GAM_PARAM(PAR_ID,PAR)
               BUF(2) = BITMAP(1)
               BUF(3) = TCPAR
               BUF(9) = BITMAP(2)
	    ENDIF
	ENDIF
C
C Build the command
C
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
