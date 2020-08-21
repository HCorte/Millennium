C
C SUBROUTINE X2WAKSTN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2WAKSTN.FOV                                 $
C  $Date::   17 Apr 1996 16:41:30                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2wakstn.for;1 **
C
C X2WAKSTN.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C Calling Sequence:
C
C     CALL X2WAKSTN(TRABUF,MESS,ORGMESS,LEN)
C
C Input parameters:
C
C     TRABUF      Int*4(TRALEN)   Transaction buffer
C     ORGMESS     Int*4(*)        Message from station
C
C Output parameters:
C
C     MESS        Int*4(*)        Message to be sent to station.
C     MESLEN      Int*2           Length of output message (bytes)
C
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2WAKSTN(TRABUF,MESS,ORGMESS,MESLEN)
	IMPLICIT NONE 
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*2   MESLEN          !Output message length
	INTEGER*4   ORGMESS(*)      !Station input message
	INTEGER*4   MESS(*)         !Station output message
	INTEGER*4   TEMP            !Work variable
	INTEGER*4   FLAGS           !Station sending flags
	INTEGER*4   OFFMES          !Offset to station message
	INTEGER*4   OUTBYT          !Byte offset for output message
        INTEGER*4   I, RESTYP
C
C DETERMINE THE STARTING OFFSET OF THE STATION
C MESSAGE IN THE MESSAGE FROM X2XMGR.
C
	CALL ILBYTE(OFFMES,ORGMESS,X2PRO_OFFSET-1)
	OUTBYT=0
	TEMP=0
	MESLEN=0
C
C COPY THE INPUT MESSAGE HEADER TO THE OUTPUT MESSAGE.
C
	DO 100 I=OFFMES,OFFMES+X2STMES_HDRLEN
	  OUTBYT=OUTBYT+1
	  CALL ILBYTE(TEMP,ORGMESS,I-1)
	  CALL ISBYTE(TEMP,MESS,OUTBYT-1)
100	CONTINUE
C
C SET THE STATION DATA UNIT TYPE TO DOWNLINE MESSAGE AND
C SET THE COMMAND CODE TO RESET.
C
	CALL ISBYTE(X2STMES_DATATYPE_CMD_DOWN,MESS,
     *	            X2STMES_DATATYPE-1)
C
	CALL ILBYTE(RESTYP,MESS,X2STMES_CODE-1)
	IF  (RESTYP.EQ.X2STMES_WAKE_UP) THEN
	  CALL ISBYTE(X2STMES_WAKE_UP,MESS,X2STMES_CODE-1)
	ELSE
	  TYPE *,' X2WAKSTN : Invalid Message Code ',RESTYP
	ENDIF
C
C ZERO OUT THE STATION NUMBER, CONFIGURATION, AND
C PORT NUMBER.
C
	TEMP=TRABUF(TXSTN)
	IF(TEMP.LT.0 .OR. TEMP.GT.X2X_STATIONS) TEMP=0
	CALL I4TOBUF2(TEMP,MESS,X2STMES_STATION_NO-1)
	CALL ISBYTE(0,MESS,X2STMES_CONFCHK-1)
	CALL ISBYTE(0,MESS,X2STMES_STN_PORT-1)
C
C SET THE OUTGOING FLAGS. NOTE: DISCONNECT FLAG SET
C TO UNCONDITIONAL DISCONNECT.
C
	FLAGS=X2STMES_RE+X2STMES_DIS_UNC
	CALL ISBYTE(FLAGS,MESS,X2STMES_FLAGS-1)
C
C SET THE MESSAGE LENGTH AND DETERMINE THE OUTPUT MESLEN
C FOR THE ROUTINE.
C
	MESLEN=X2STMES_HDRLEN
	CALL I2TOBUF2(MESLEN,MESS,X2STMES_MESLEN-1)
C
C PROGRAM EXIT.
C
	RETURN
	END
