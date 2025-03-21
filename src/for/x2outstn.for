C
C SUBROUTINE X2OUTSTN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2OUTSTN.FOV                                 $
C  $Date::   17 Apr 1996 16:26:06                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2outstn.for;1 **
C
C X2OUTSTN.FOR
C
C V03 18-JUL-94 WS MULTINETWORK CHANGES - Integrate UK changes into 
C		   X2X Baseline
C V02 22-APR-94 GPR CHECK FOR X25 CONN TYPE BEFORE DOING CHECKSUMS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This subroutine will create the output message to be
C sent to a station in response to a request.  This
C routine will be called only for station messages which
C contain a Station Data Unit Type (SDUT) equal to 4 (Station
C Control/Status Data), or SDUT equal to 255 (Upline Help
C Request).  This routine will fill in all required information
C into the Station Data Header, and any other control message
C information is updated by external routines.
C
C Calling Sequence:
C
C     CALL X2OUTSTN(TRABUF,MESS,ORGMESS,LEN)
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
	SUBROUTINE X2OUTSTN(TRABUF,MESS,ORGMESS,MESLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
C
	INTEGER*2   MESLEN          !Output message length
	INTEGER*2   TTNCHK1         !Checksum of TITN line 1 def param
	INTEGER*2   TTNCHK2         !Checksum of TITN line 2 def param
	INTEGER*2   STNCHK          !Checksum of station def param
	INTEGER*2   CALLCHK         !Checksum of initial call def param
	INTEGER*4   ORGMESS(*)      !Station input message
	INTEGER*4   MESS(*)         !Station output message
	INTEGER*4   TEMP            !Work variable
	INTEGER*4   STN             !Station number
	INTEGER*4   FLAGS           !Disconnect/connect flags
	INTEGER*4   OFFMES          !Offset of station message
	INTEGER*4   OUTBYT          !Byte index of output message
	INTEGER*4   CONN_TYPE       !Connection type
        INTEGER*4   I
	INTEGER*4   CHKVAL          !Function
	CHARACTER   C1TEMP(4)*1     !Work variable
	EQUIVALENCE (TEMP,C1TEMP)
C
C DETERMINE THE STARTING OFFSET OF THE STATION
C MESSAGE IN THE MESSAGE FROM X2XMGR.
C
	CALL ILBYTE(OFFMES,ORGMESS,X2PRO_OFFSET-1)
	STN=TRABUF(TXSTN)

C CHECK FOR VALID STATION NUMBER.
C
        IF(CHKVAL(STN,1,X2X_STATIONS,' STATION NUMBER ').NE.0) THEN
          MESLEN=-1
          GOTO 7000
        ENDIF
C
	TEMP=0
	MESLEN=0
	OUTBYT=0
C
C COPY THE INPUT MESSAGE HEADER TO THE OUTPUT MESSAGE.
C
	DO 100 I=OFFMES,OFFMES+X2STMES_HDRLEN
	  OUTBYT=OUTBYT+1
	  CALL ILBYTE(TEMP,ORGMESS,I-1)
	  CALL ISBYTE(TEMP,MESS,OUTBYT-1)
100	CONTINUE
C
C SET THE STATION CONFIGURATION CHECKSUM.
C
	TEMP=0
	C1TEMP(1)=X2XS_CONF(STN)
	IF (X2XS_TYPE(STN).EQ.X2XST_BCST) TEMP=0 		!V03

	CALL ISBYTE(TEMP,MESS,X2STMES_CONFCHK-1)
C
C SET THE OUTGOING FLAGS TO RESPONSE MODE AND
C SET DISCONNECT CONTROL TO DISCONNECT DEFAULT.
C NOTE: THIS CAN BE CHANGED BASED ON MESSAGE TYPE.
C
	FLAGS=X2STMES_RE+X2STMES_DIS_DEFAULT
	CALL ISBYTE(FLAGS,MESS,X2STMES_FLAGS-1)
C
C STATION DATA UNIT TYPE IS UPLINE HELP OR UPLINE
C HELP REQUEST FOR DEFAULT CONFIGURATION.
C
        IF((TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_RESET)  .OR.
     *     (TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_RESET2) .OR.
     *     (TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_DEFAULT_CONF2) .OR.
     *     (TRABUF(TXSSDTU).EQ.X2STMES_DEF_CONF_REQ)) THEN
C
C IF DEFAULT REQUEST OR UPLINE HELP AND CHECKSUM ENABLED,
C VERIFY THAT THE STATION HAS CORRECT DEFAULT PARAMETERS.
C
          CALL MOV2TOI2(X2XS_REVISION(STN),ORGMESS,
     *                OFFMES+X2STMES_REVISION-2)
C
	  IF(((X2X_DEF_CHKSUM.EQ.0) .OR.
     *       (TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_DEFAULT_CONF2) .OR.
     *	     (TRABUF(TXSSDTU).EQ.X2STMES_DEF_CONF_REQ)) .AND.
     *       ((BX2XS_CONN_TYPE(STN).EQ.X2XSCT_X25PVC) .OR. 
     *        (BX2XS_CONN_TYPE(STN).EQ.X2XSCT_X25SVC))) THEN		! V02
	    CALL MOV2TOI2(TTNCHK1,ORGMESS,OFFMES+X2STMES_TTN1CHK-2)
	    CALL MOV2TOI2(TTNCHK2,ORGMESS,OFFMES+X2STMES_TTN2CHK-2)
	    CALL MOV2TOI2(STNCHK,ORGMESS,OFFMES+X2STMES_STNCHK-2)
	    CALL MOV2TOI2(CALLCHK,ORGMESS,OFFMES+X2STMES_CALLCHK-2)
C
C IF CHECKSUMS DO NOT MATCH, READ THE STATION RECORD AND
C RECALCULATE THEM TO GUARANTEE CONSISTENCY.
C
          IF(X2XS_TTN1_CHKSUM(STN).NE.TTNCHK1 .OR.
     *       X2XS_TTN2_CHKSUM(STN).NE.TTNCHK2 .OR.
     *       X2XS_STN_CHKSUM(STN) .NE.STNCHK  .OR.
     *       X2XS_CALL_CHKSUM(STN).NE.CALLCHK) THEN
CV03            CALL READW(X2XSTN_FDB,STN,X2XSTN_REC,ST)
CV03            IF(ST.NE.0) THEN
CV03              CALL OPS('X2STCONF: Error reading X2XSTN.FIL ',STN,ST)
CV03              MESLEN=-1
CV03              GOTO 7000
CV03            ENDIF
CV03            CALL X2LODTTN
            CALL X2STNCHK(STN,TRABUF)
          ENDIF
C
C UPDATE BASED ON APPROPRIATE CHECKSUMS.
C
	    IF(X2XS_TTN1_CHKSUM(STN).NE.TTNCHK1) THEN
	      CALL X2DEFTTN(TRABUF,MESS,MESLEN,1)
	      GOTO 7000
	    ELSE IF(X2XS_TTN2_CHKSUM(STN).NE.TTNCHK2) THEN
	      CALL X2DEFTTN(TRABUF,MESS,MESLEN,2)
	      GOTO 7000
	    ELSE IF(X2XS_STN_CHKSUM(STN).NE.STNCHK) THEN
	      CALL X2DEFSTN(TRABUF,MESS,MESLEN)
	      GOTO 7000
	    ELSE IF(X2XS_CALL_CHKSUM(STN).NE.CALLCHK) THEN
	      CALL X2DEFCAL(TRABUF,MESS,MESLEN)
	      GOTO 7000
	    ENDIF
	  ENDIF
C
C UPDATE THE STATION DATA HEADER AS THIS INFO IS
C NOT INCLUDED ON UPLINE HELP REQUEST, THEN
C CREATE THE OUTPUT STATION CONFIGURATION MESSAGE.
C
	  CALL I4TOBUF2(STN,MESS,X2STMES_STATION_NO-1)
	  CALL ISBYTE(X2STMES_DATATYPE_CMD_DOWN,MESS,
     *	              X2STMES_DATATYPE-1)
	  CALL ISBYTE(X2STMES_CONF,MESS,X2STMES_CODE-1)
	  TEMP=0
	  CALL ISBYTE(TEMP,MESS,X2STMES_STN_PORT-1)
	  CALL X2STCONF(TRABUF,MESS,MESLEN)
C
C STATION DATA UNIT TYPE IS STATION/COMMAND MESSAGE.
C
	ELSE IF(TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_CMD_UP) THEN
C
C STATION STATUS REPORT (NO RESPONSE REQUIRED).
C
	  IF(TRABUF(TXSCC).EQ.X2STMES_STAT_REQ) THEN
C
C GENERAL STATION CONFIGURATION REQUEST.
C
	  ELSE IF(TRABUF(TXSCC).EQ.X2STMES_CONF_REQ) THEN
	    CALL ISBYTE(X2STMES_DATATYPE_CMD_DOWN,MESS,
     *	                X2STMES_DATATYPE-1)
	    CALL ISBYTE(X2STMES_CONF,MESS,X2STMES_CODE-1)
	    CALL X2STCONF(TRABUF,MESS,MESLEN)
C
C PORT CONFIGURATION REQUEST.
C
	  ELSE IF(TRABUF(TXSCC).EQ.X2STMES_PORT_REQ) THEN
	    CALL ISBYTE(X2STMES_DATATYPE_CMD_DOWN,MESS,
     *	                X2STMES_DATATYPE-1)
	    CALL ISBYTE(X2STMES_PORT_CONF,MESS,X2STMES_CODE-1)
C
            FLAGS=X2STMES_RE+X2STMES_DIS_DEFAULT  
	    CALL ISBYTE(FLAGS,MESS,X2STMES_FLAGS-1)
	    CALL X2PTCONF(TRABUF,MESS,MESLEN)
C
C CONNECTION VALIDITY REQUEST.
C
	  ELSE IF(TRABUF(TXSCC).EQ.X2STMES_VAL_REQ) THEN
C
C           X28 GVT REQUIRE UNCONDITIONAL DISCONNECT 
C
            CALL ILBYTE(CONN_TYPE,IX2XS_CONN_TYPE,STN-1)
            IF (CONN_TYPE.EQ.X2XSCT_X28PAD) THEN
   	       FLAGS=X2STMES_RE+X2STMES_DIS_UNC
	       CALL ISBYTE(FLAGS,MESS,X2STMES_FLAGS-1)
            ENDIF
	    CALL ISBYTE(X2STMES_DATATYPE_CMD_DOWN,MESS,
     *	                X2STMES_DATATYPE-1)
	    CALL ISBYTE(X2STMES_VAL_RESP,MESS,X2STMES_CODE-1)
	    MESLEN=0
C
C BROADCAST CONFIGURATION REQUEST.
C
	  ELSE IF(TRABUF(TXSCC).EQ.X2STMES_RELAY_REQ) THEN
	    CALL ISBYTE(X2STMES_DATATYPE_CMD_DOWN,MESS,
     *	                X2STMES_DATATYPE-1)
	    CALL ISBYTE(X2STMES_RELAY_RESP,MESS,X2STMES_CODE-1)
            FLAGS=X2STMES_RE+X2STMES_DIS_UNC
            CALL ISBYTE(FLAGS,MESS,X2STMES_FLAGS-1)
	    CALL X2BRCONF(TRABUF,MESS,MESLEN)
C
C BROADCAST SERVER (BCST) CONFIGURATION REQUEST.                    V02
C
    	  ELSE IF(TRABUF(TXSCC).EQ.X2STMES_BCST_REQ) THEN
      	      CALL ISBYTE(X2STMES_DATATYPE_CMD_DOWN,MESS,
     *			  X2STMES_DATATYPE-1)
      	      CALL ISBYTE(X2STMES_BCST_RESP,MESS,X2STMES_CODE-1)
      	      FLAGS=X2STMES_RE+X2STMES_DIS_UNC
      	      CALL ISBYTE(FLAGS,MESS,X2STMES_FLAGS-1)
      	      CALL X2BSCONF(TRABUF,MESS,MESLEN)
	  ENDIF
	ENDIF
C
C STORE THE OUTPUT MESSAGE LENGTH INTO THE STATION HEADER,
C AND INCREMENT THE OUTPUT ROUTINE MESSAGE LENGTH TO INCLUDE
C THE HEADER PORTION.
C
7000	CONTINUE
	IF(MESLEN.NE.-1) THEN
	  CALL I2TOBUF2(MESLEN,MESS,X2STMES_MESLEN-1)
	  MESLEN=MESLEN+X2STMES_HDRLEN
C
C IF MESLEN IS EQUAL TO -1 AN ERROR HAS OCCURED
C WHILE ATTEMPTING TO OBTAIN SOME INFORMATION, SO SEND A
C STATION RESET.
C
	ELSE
	  CALL X2RESSTN(TRABUF,MESS,ORGMESS,MESLEN)
	ENDIF
C
	RETURN
	END
