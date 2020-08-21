
C
C SUBROUTINE X2DEFTTN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2DEFTTN.FOV                                 $
C  $Date::   17 Apr 1996 16:14:52                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2defttn.for;1 **
C
C X2DEFTTN.FOR
C
C V03 05-DEC-94 SCD GET STATION AND STATION CLASS INFO FROM MEMORY INSTEAD
C		    OF READING FILES - Integrate UK changes into X2X Baseline
C V02 28-APR-94 XXX GET STATION CLASS INFO FROM MEMORY
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C
C This subroutine will create the TTN default parameters
C message for the input HDLC line number.
C
C Calling sequence:
C
C     CALL X2DEFTTN(TRABUF,MESS,MESLEN,LINE)
C
C Input parameters:
C
C     TRABUF      Int*4(TRALEN)       Transaction buffer
C     LINE        Int*4               HDLC line to configure
C
C Output parameters:
C
C     MESS        Int*4(*)            Output message
C     MESLEN      Int*2               Length of message buffer
C                                     Read error MESLEN=-1
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
	SUBROUTINE X2DEFTTN(TRABUF,MESS,MESLEN,LINE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
CV03	INCLUDE 'INCLIB:X2XSTN.DEF'
CV03	INCLUDE 'INCLIB:X2XSCL.DEF'
	INCLUDE 'INCLIB:X2XTTN.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*2   MESLEN                          !Output message length
	INTEGER*4   LINE                            !HDLC line number
	INTEGER*4   MESS(*)                         !Output message
	INTEGER*4   OFF                             !Data offset relative to hdr
	INTEGER*4   LSTBYT                          !Byte offset into buffer
	INTEGER*4   STN                             !Station number
	INTEGER*4   TTNREC                          !Record number to read
	INTEGER*4   I, NUMTTN, CHKVAL
	INTEGER*4   CLASS			    !V03
C
C INITIALIZE VARIABLES.
C
	STN=TRABUF(TXSTN)
	MESLEN=0
	OFF=X2STMES_DATA-1
	LSTBYT=0
C
C CHECK FOR VALID STATION NUMBER.
C
	IF(CHKVAL(STN,1,X2X_STATIONS,' STATION NUMBER ').NE.0) THEN
	  MESLEN=-1
	  GOTO 8000
	ENDIF
C
C READ THE STATION RECORD.
C
CV03	CALL READW(X2XSTN_FDB,STN,X2XSTN_REC,ST)
CV03	IF(ST.NE.0) THEN
CV03	  CALL OPS('X2STCONF: Error reading X2XSTN.FIL ',STN,ST)
CV03	  MESLEN=-1
CV03	  GOTO 8000
CV03	ENDIF
C
C CHECK TO ENSURE THE STATION INFORMATION
C EXISTS.
C
CV03	IF(X2XSTN_REC(1).LE.0) THEN
CV03	  CALL OPS('X2STCONF:Station does not exist',STN,TRABUF(TSER))
CV03	  MESLEN=-1
CV03	  GOTO 8000
CV03	ENDIF
C
C READ THE STATION CLASS RECORD FROM MEMORY
C
CV03        DO 600 JJ = 1, X2XSC_REC_LEN
CV03            X2XSCL_REC(JJ) = X2XSC_MEM_STORE(JJ,X2XSTN_STNCLS)
CV03600     CONTINUE
C
C CHECK TO ENSURE THE STATION CLASS INFORMATION
C EXISTS.
C
	CLASS=X2XS_STNCLS(STN)					!V03
	IF(CLASS.LE.0) THEN					!V03
	  CALL OPS('X2DEFTTN:Station class does not exist ',	!V03
     *	            STN,CLASS)					!V03
	  MESLEN=-1
	  GOTO 8000
	ENDIF
C
C UPDATE THE STATION DATA HEADER.
C
	CALL ISBYTE(X2STMES_PROTID_VAL,MESS,X2STMES_PROTID-1)
	CALL ISBYTE(X2STMES_DATATYPE_CMD_DOWN,MESS,
     *	            X2STMES_DATATYPE-1)
	CALL ISBYTE(0,MESS,X2STMES_CONFCHK-1)
	CALL I4TOBUF2(STN,MESS,X2STMES_STATION_NO-1)
	CALL ISBYTE(X2STMES_DEF_CONF,MESS,X2STMES_CODE-1)
	IF(LINE.EQ.1) THEN
	  CALL ISBYTE(X2STMES_DEF_TTN1,MESS,X2STMES_DEF_TYPE-1)
	  TTNREC=X2XC_TTN_PORT1(CLASS)					!V03
	ELSE
	  CALL ISBYTE(X2STMES_DEF_TTN2,MESS,X2STMES_DEF_TYPE-1)
	  TTNREC=X2XC_TTN_PORT2(CLASS)					!V03
	ENDIF
C
C READ THE TTN RECORD.
CV03	TTNREC=X2XSCL_TTN(LINE)
CV03	IF(X2XSTN_TTN(LINE).NE.0) TTNREC=X2XSTN_TTN(LINE)
	IF(TTNREC.LE.0) THEN						!V03
	  CALL OPS('X2DEFTTN:TTN class does not exist ',
     *	            STN,LINE)
	  MESLEN=-1
	  GOTO 8000
	ENDIF
C
	CALL FASTMOV(X2XD_TTN_RECORD_BACKUP(1,TTNREC),			!V03
     *		      X2XTTN_REC,X2XTTN_REC_LEN)			!V03
CV03	CALL READW(X2XTTN_FDB,TTNREC,X2XTTN_REC,ST)
CV03	IF(ST.NE.0) THEN
CV03	  CALL OPS('X2DEFTTN:Error reading X2XTTN.FIL ',
CV03     *	            ST,ST)
CV03	  MESLEN=-1
CV03	  GOTO 8000
CV03	ENDIF
C
C CHECK TO ENSURE THE TTN PARAMETERS EXIST.
C
	IF(X2XTTN_REC(1).LE.0 .OR. LINE.EQ.0) THEN
	  CALL OPS('X2DEFTTN:TTN class does not exist ',
     *	            TTNREC,0)
	  MESLEN=-1
	  GOTO 8000
	ENDIF
C
C DETERMINE OFFSET
C
	LSTBYT=X2STMES_TTN_CNT+2
	NUMTTN=X2XTTN_ENTRIES-2         !EXCLUDE CLASS # AND UPDATE DATE
C
C STORE TTN CONFIGURATION INTO MESSAGE.
C
	CALL ISBYTE(X2STMES_DEF_CONF,MESS,OFF+X2STMES_TTN_CMD-1)
	CALL ISBYTE(0,MESS,OFF+X2STMES_TTN_CNTNO-1)
	CALL I4TOBUF2(STN,MESS,OFF+X2STMES_TTN_STNO-1)
	CALL I4TOBUF2(LSTBYT-1,MESS,OFF+X2STMES_TTN_OFFSET-1)
	CALL I4TOBUF2(NUMTTN,MESS,OFF+X2STMES_TTN_CNT-1)
C
C STORE TTN PARAMETERS.
C
	DO 100 I=2,2+NUMTTN-1
	  CALL I4TOBUF2(X2XTTN_REC(I),MESS,OFF+LSTBYT-1)
	  LSTBYT=LSTBYT+X2STMES_TTN_LEN
100	CONTINUE
	MESLEN=LSTBYT-1
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
	END
