C GUICMD_004.FOR
C
C V01 02-FEB-2001 HXK INITIAL RELEASE FOR PORTUGAL
C
C BACKid COMMAND FOR N-PLEX
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
	SUBROUTINE GUICMD_004(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DN_LINK.DEF'
	INCLUDE 'INCLIB:DN_BLOCK.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
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
	CHARACTER*1 SYSTEMS(0:6)/' ','A','B','C','D','E','F'/
	CHARACTER*1 C_SYSID
	BYTE        B_SYSID
	EQUIVALENCE(C_SYSID,B_SYSID)
	CHARACTER*1 SPACE/' '/
	INTEGER*4 CURWAY/0/
	INTEGER*4 LEN
C
        DATA PARM/ 3, 1, 6/

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
	LEN = GUI_ARGLEN(2)
	IF(LEN.NE.1) THEN
	    RET_CODE = 11
	    RETURN
	ENDIF
	B_SYSID = B_GUI_ARGCHAR(1,2)


	IF(C_SYSID.EQ.SPACE .OR. B_SYSID.EQ.0) THEN
	   VAL = 0
	ELSE 
	   IF(C_SYSID.LT.SYSTEMS(PARM(PAR_MIN,PAR)) .OR.
     *        C_SYSID.GT.SYSTEMS(PARM(PAR_MAX,PAR))) THEN
	      STATUS = 1
	      WRITE(STATUS_STR,900) C_SYSID
	      GOTO 100
	   ENDIF
	   VAL = B_SYSID - 64  !converts system letters to numeric
	ENDIF
C
C Check the network status
C
	IF(CURWAY.NE.WAYINP) THEN
	      STATUS = 2
	      WRITE(STATUS_STR,902) C_SYSID
	      GOTO 100
	ENDIF
	IF(VAL.LT.0 .OR. VAL.GT.NETSYS .OR.
     *     VAL.EQ.NETMASTER(WAYINP)) THEN
	      STATUS = 3
	      WRITE(STATUS_STR,903) C_SYSID
	      GOTO 100
	ENDIF
	IF(X2X_GAME_STATE.NE.X2X_GAMES_UP) THEN
	      STATUS = 4
	      WRITE(STATUS_STR,904)
	      GOTO 100
	ENDIF
	IF(VAL.NE.0) THEN
	   IF(NETSITE(VAL).NE.NETSITE(NODEID)) THEN
	      STATUS = 5
	      WRITE(STATUS_STR,903) C_SYSID
	      GOTO 100
	   ENDIF
	   IF(NETROUT(VAL,WAYINP).NE.ROUACT .AND.
     *        DN_LINK(VAL).STATE .NE. STATE_RUNNING) THEN
	      STATUS = 6
	      WRITE(STATUS_STR,906)
	      GOTO 100
	   ENDIF
	   IF(NETMODE(VAL,WAYINP).EQ.FILMD) THEN
	      STATUS = 7
	      WRITE(STATUS_STR,907)
	      GOTO 100
	   ENDIF
	ENDIF
C
C Build the command
C
        BUF(1)=PARM(CMD_TYP,PAR)
        BUF(2)=VAL
        BUF(3)=TCNET
	BUF(6)='GUI '
	BUF(9)=WAYINP
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
900     FORMAT('Invalid value: <',A1,'>')
902     FORMAT('GOLS Network Path Error for: <',A1,'>')
903     FORMAT('GOLS Value Error for: <',A1,'>')
904     FORMAT('GOLS Gamestate not set')
906     FORMAT('GOLS Link not up yet')
907     FORMAT('GOLS System still in recovery mode')
	END
