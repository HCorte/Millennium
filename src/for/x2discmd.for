C
C SUBROUTINE X2DISCMD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2DISCMD.FOV                                 $
C  $Date::   17 Apr 1996 16:15:28                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2discmd.for **
C
C X2DISCMD.FOR
C
C V02 14-APR-94 SCD ADDED CHECK OF MESSAGE DATA UNIT TYPE TO CALL
C		    A SEPARATE ROUTINE FOR COMMAND/STATUS DATA UNITS.
C V01 15-DEC-92  MF Fix for GVT
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine is used to tell the Front End processor
C to disable a station.  This routine is only used
C for Front End error conditions involing station data
C that contains invalid protocol.
C
C Calling Sequence:
C
C     CALL X2DISCMD(TRABUF,MESS,ORGMESS,LEN)
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
	SUBROUTINE X2DISCMD(TRABUF,MESS,ORGMESS,MESLEN,TXHDRLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2FEMES.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*2   MESLEN          !Output message length
	INTEGER*4   ORGMESS(*)      !Station input message
	INTEGER*4   MESS(*)         !Station output message
	INTEGER*4   LEN             !Entire FE+STN message length
	INTEGER*4   TEMP            !Work variable
	INTEGER*4   OUTBYT          !Byte index for output message
	INTEGER*4   ADRLEN          !Address length
	INTEGER*4   LSTBYT          !Byte index into message
	INTEGER*4   J, LOOPCNT, DATALEN, I, TXHDRLEN
	INTEGER*4   CONN_TYPE,FLAGS,NET_TYPE,OPTIONS
	INTEGER*4   O_FEHDRLEN      !FE Header Length from ORGMESS
	INTEGER*4   M_FEHDRLEN      !FE Header Length of MESS
C
	O_FEHDRLEN = X2FEMES_HEADLEN-1   !command/status header EXCLUDES
                                         !header length
	M_FEHDRLEN = X2FEMES_HEADLEN     !Downline station data header
                                         !INCLUDES header length

C GET THE BAD ADDRESS LENGTH.
C
	CALL ILBYTE(ADRLEN,ORGMESS,X2FEMES_CONSTS_ADRLEN-1+TXHDRLEN)
C
	OUTBYT=0
	TEMP=0
	MESLEN=0
C
C COPY THE INPUT MESSAGE HEADER TO THE OUTPUT MESSAGE.
C NOTE: THE TRANSPORT HEADER AND FE HEADER ARE COPIED.
C
	DO 100 I=TXHDRLEN+1,(TXHDRLEN+1)+O_FEHDRLEN-1
	   OUTBYT=OUTBYT+1
	   CALL ILBYTE(TEMP,ORGMESS,I-1)
	   CALL ISBYTE(TEMP,MESS,OUTBYT-1)
100	CONTINUE
C
C CREATE THE OUTPUT FE MESSAGE. NOTE: ALL OUTPUT INFORMATION
C (FE CONNECT ID, ADDRESS LEN, DESTINATION ADDRESS, ETC) ARE
C CONTAINED IN THE ORGINAL INPUT MESSAGE AND HAVE BEEN COPIED
C TO THE OUTPUT MESSAGE.  ONLY THE MESSAGE TYPE, FLAGS, AND
C CONNECT/DISCONNECT CONTROL FIELDS ARE CHANGED.
C
	CALL ISBYTE(0,MESS,X2FEMES_HOST_ID-1)
	CALL ISBYTE(X2FEMES_MESTYP_DOWN,MESS,X2FEMES_MESTYP-1)
C
C MAP UPLINE FE FLAGS and CONN_TYPE INTO DOWNLINE OPTION FIELD
C
	CALL ILBYTE(FLAGS,MESS,X2FEMES_FLAGS-1)           !UP: FLAGS
        CALL ILBYTE(CONN_TYPE,MESS,X2FEMES_CONNTYP-1)
        CONN_TYPE=IAND(CONN_TYPE,'7F'X)                   !UP: CONN. TYPE
C 
        IF ( IAND(FLAGS,X2FEMES_FLAGS_VS) .NE.0 .OR.      !UP: VSP OR EVSP
     *       IAND(FLAGS,X2FEMES_FLAGS_EVS).NE.0) THEN 
             FLAGS= X2FEMES_FLAGS_DVS                     !DOWN: VSP
        ELSE
             FLAGS= 0 
        ENDIF
C
        IF(CONN_TYPE.LT.0.OR.CONN_TYPE.GT.X2FEMES_MAX_CONNTYPE)THEN
            NET_TYPE = X2FEMES_NET_TYPE_UNDEF        ! or CONTYP_NONE=0
        ELSE
            NET_TYPE = IAND(X2FEMES_NET_TYPE_XREF(CONN_TYPE),'07'X) !NET TYPE
        ENDIF
C
        OPTIONS = IOR(FLAGS,NET_TYPE)                     !DOWN: OPTIONS
C
	CALL ISBYTE(OPTIONS,MESS,X2FEMES_NET_TYPE-1)      !DOWN: OPTIONS
	CALL ISBYTE(X2FEMES_UNCDDISC,MESS,X2FEMES_CONNCTL-1)
C
C FE MESSAGE COMPLETE, NOW BUILD A STATION MESSAGE TO
C INFORM THE STATION TO DISCONNECT. NOTE: STATION NUMBER
C AND CONFIGURATION NUMBER ARE CLEARED.
C
	TEMP=0
	CALL ISBYTE(X2STMES_PROTID_VAL,MESS,
     *	            X2STMES_PROTID-1+M_FEHDRLEN)
	CALL ISBYTE(X2STMES_DATATYPE_CMD_DOWN,MESS,
     *	            X2STMES_DATATYPE-1+M_FEHDRLEN)
	CALL ISBYTE(0,MESS,X2STMES_CONFCHK-1+M_FEHDRLEN)
	CALL I4TOBUF2(TEMP,MESS,X2STMES_STATION_NO-1+M_FEHDRLEN)
    	CALL ISBYTE(X2STMES_BAD_ADRESS,MESS,
     *	            X2STMES_CODE-1+M_FEHDRLEN)
	CALL ISBYTE(0,MESS,X2STMES_OPT_DATA-1+M_FEHDRLEN)
	CALL ISBYTE(X2STMES_DIS_UNC,MESS,X2STMES_FLAGS-1+M_FEHDRLEN)
C
C STORE THE BAD ADDRESS MESSAGE PORTION.
C
	CALL ISBYTE(X2STMES_BAD_ADRESS,MESS,X2STMES_HDRLEN+M_FEHDRLEN+
     *	                                    X2STMES_CMD-1)
	CALL ISBYTE(0,MESS,X2STMES_HDRLEN+M_FEHDRLEN+
     *	                                    X2STMES_BADADR_CNTNO-1)
        CALL I4TOBUF2(TRABUF(TXFVS),MESS,X2STMES_HDRLEN+M_FEHDRLEN+
     *                                      X2STMES_VS_SENT-1)
	CALL ISBYTE(ADRLEN,MESS,X2STMES_HDRLEN+M_FEHDRLEN+
     *	                                    X2STMES_BADADR_ADRLEN-1)
	LSTBYT=X2STMES_HDRLEN+M_FEHDRLEN+X2STMES_BADADR_ADDRESS
	DATALEN=5
	LOOPCNT=ADRLEN/2
	IF(MOD(ADRLEN,2).NE.0) LOOPCNT=LOOPCNT+1
	DO 200 J=1,MIN0(X2X_ADRESS_MAXLEN*4,LOOPCNT)
	   CALL ILBYTE(TEMP,MESS,X2FEMES_ADR-1+(J-1)) !Copy from FE header    
	   CALL ISBYTE(TEMP,MESS,LSTBYT-1)
	   LSTBYT=LSTBYT+1
	   DATALEN=DATALEN+1
200 	CONTINUE
C
C SET THE STATION MESSAGE LENGTH.
C
	CALL I4TOBUF2(DATALEN,MESS,X2STMES_MESLEN-1+M_FEHDRLEN)
C
C SET THE ENTIRE MESSAGE LENGTH INTO THE FE MESSAGE.
C
	LEN=M_FEHDRLEN+X2STMES_HDRLEN+DATALEN
	CALL I4TOBUF2(LEN,MESS,X2FEMES_MESLEN-1)
C
C PROGRAM EXIT.
C
	MESLEN=LEN
	RETURN
	END
