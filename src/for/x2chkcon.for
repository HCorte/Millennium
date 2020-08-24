C
C X2CHKCON.FOR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CHKCON.FOV                                 $
C  $Date::   18 Dec 1996 12:04:18                                         $
C  $Revision::   1.1                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C V04 12-DEC-95 DAS Changed MASK from I*2 to I*4 aka UK
C V03 12-DEC-94 GPR Integrate UK changes into X2X Baseline
C V02 03-MAR-93 LMK UPDATED TO HANDLE DIAL
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will check the connection status of SAP
C after decoding  front - end command
C
C Calling sequence:
C
C     CALL X2CHKCON(BUFF, STN_NO, FE_ID, STATUS)
C
C Input parameters:
C
C     BUFF        Chr*1(LEN)      Message buffer
C
C Output parameters:
C
C     STN_NO      Int*4           Station number
C     FE_ID     Int*4           SAP identifier
C     STATUS      Int*4           Message status
C                                 0 - connection up
C                                 1 - connection down
C                                -1 - other kind of message
C                                -2 - not a valid terminal
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
	SUBROUTINE X2CHKCON(BUFF, STN_NO, FE_ID, STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2FEMES.DEF'
	INTEGER*4   PROT_ID                 !PROTOCOL ID
	INTEGER*4   MES_TYP                 !MESSAGE TYPE
	INTEGER*4   FORM_X2X                !FORMAT X2X
	INTEGER*4   ADR_LEN                 !ADRESS LENGTH
	INTEGER*4   STN_NO                  !Station number
	INTEGER*4   FE_ID                   !SAP identifier
	INTEGER*4   STATUS                  !Message status
	INTEGER*4   STNADDRESS(X2X_ADRESS_MAXLEN)
	INTEGER*4   TERCOD, MES_COD
C
        INTEGER*4   X32LEN
        INTEGER*4   CONTYPE
        INTEGER*4   CONTYPE_OFFSET
        INTEGER*4   VSLEN_OFFSET
        INTEGER*4   VS_OFFSET
        INTEGER*4   EVSNLEN
        INTEGER*4   EVSNNUM(X2X_EVSN_MAXLEN)
C
	LOGICAL     CMD_RESPONSE            !command responce
	INTEGER*4   SRCSTAT
	INTEGER*4   OFF, OFF1
	INTEGER*4   MASK /Z00007FFF/
	INTEGER*2   BUFF(*)
D	CHARACTER*1 ASCADDRESS(20)
C
C UNPACK MESSAGE BUFFER
C
	CALL ILBYTE(PROT_ID, BUFF, X2FEMES_PROTID - 1)
	CALL ILBYTE(MES_TYP, BUFF, X2FEMES_MESTYP - 1)
	CALL ILBYTE(FORM_X2X, BUFF, X2FEMES_FORMAT - 1)
	CALL MOV2TOI4(MES_COD, BUFF, X2FEMES_MESCOD - 1)
	CMD_RESPONSE=.FALSE.
	IF (IAND(MES_COD,X2FEMES_CR).NE.0) CMD_RESPONSE=.TRUE.
	MES_COD = IAND(MES_COD, MASK)
	IF (PROT_ID .NE. X2FEMES_PROTID_X2X .OR.
     *	    MES_TYP .NE. X2FEMES_MESTYP_CMD .OR.
     *	    FORM_X2X .NE. X2FEMES_FORM_X2X   .OR.
     *	    MES_COD .NE. X2FEMES_MESCOD_CONSTS   ) THEN
	  STATUS = -1
	  RETURN
	ENDIF
	CALL MOV4TOI4(FE_ID, BUFF, X2FEMES_CONSTS_CONID - 1)
        FE_ID = IAND(FE_ID,'FFFFFF00'X)
	CALL ILBYTE(TERCOD,BUFF,X2FEMES_CONSTS_TERCOD-1) !TERMINATION
	TERCOD=IAND(TERCOD,X2FEMES_CONSTS_TERCOD_UP)
	CALL ILBYTE(ADR_LEN, BUFF, X2FEMES_CONSTS_ADRLEN - 1)
	DO 50 OFF=1,X2X_ADRESS_MAXLEN
	    STNADDRESS(OFF)=0
50	CONTINUE
	CALL FASTBYT(BUFF, X2FEMES_CONSTS_STNADD, STNADDRESS, 1,ADR_LEN)
        CALL X2QSHFT(STNADDRESS,-(64-ADR_LEN*4))
	IF (IAND(X2X_DEBUG,512).NE.0)					!V03
     *	   TYPE 9000,' addr  ',STNADDRESS(1),STNADDRESS(2)		!V03
C
        CALL ILBYTE(X32LEN, BUFF, X2FEMES_CONSTS_X32ADRLEN - 1)
        CONTYPE_OFFSET = X2FEMES_CONSTS_X32ADRLEN + X32LEN
        VSLEN_OFFSET   = CONTYPE_OFFSET + X2FEMES_CONSTS_CONTYPE
        VS_OFFSET      = VSLEN_OFFSET   + X2FEMES_CONSTS_EVS
C
        CALL ILBYTE (CONTYPE, BUFF, CONTYPE_OFFSET)
        CALL ILBYTE (EVSNLEN,   BUFF, VSLEN_OFFSET)
C
        IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS) .NE. 0) THEN
           TYPE *,'CONTYPE = ',CONTYPE
           TYPE *,'EVSNLEN = ',EVSNLEN
        ENDIF
C
        IF (EVSNLEN .GT. 0) THEN
           EVSNNUM(1) = 0
           EVSNNUM(2) = 0
           CALL FASTBYT(BUFF, VS_OFFSET+1, EVSNNUM, 1, EVSNLEN)
           CALL X2QSHFT(EVSNNUM,-(64-EVSNLEN*4*2))
	   IF (IAND(X2X_DEBUG,512).NE.0)				!V03 
     *	    TYPE 9000,' evsn ',EVSNNUM(1),EVSNNUM(2)		        !V03
        ENDIF 
C
C CHECK TO SEE CONNECTION TYPE, AND IF DIAL DO
C AND SEARCH ON VERIFICATION SEQUENCE
C OTHERWISE CHECK ON THE ADDRESS
C
        SRCSTAT = -1 
        IF ((CONTYPE .EQ. X2XSCT_GTECH_DIAL .OR.
     *       CONTYPE .EQ. X2XSCT_X28PAD)    .AND.
     *       EVSNLEN .GT. 0) THEN
             IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS) .NE. 0) THEN
                TYPE *,'CALLING X2BINVSN EVSNLEN = ',EVSNLEN
                TYPE 9999,'ENVS=',EVSNNUM(1),' ',EVSNNUM(2)
9999            FORMAT(A,Z8.8,A,Z8.8) 
             ENDIF
             CALL X2BINVSN(EVSNLEN*2, EVSNNUM, STN_NO, SRCSTAT)
             IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS) .NE. 0) THEN
                TYPE *,'STN & STATUS IN CHKCON ',STN_NO,' ',SRCSTAT
             ENDIF        
C
C ***** Start V03 changes *****
C
C BINARY SEARCH THE SORTED ADDRESS TABLE TO OBTAIN THE STATION
C NUMBER.  IF IT IS NOT FOUND, OR THE COMMON IS LOCKED,
C SEQUENTIALY SEARCH FOR IT.
C
	ELSE
	    CALL X2BINSRC(ADR_LEN,STNADDRESS,STN_NO,SRCSTAT)
	    IF (SRCSTAT.EQ.0) THEN
		GO TO 130
	    ENDIF
	    IF (X2X_CHK_NEW_STATIONS_CONNECT.EQ.0) THEN
	      DO 120 OFF=1,X2X_STATIONS
		IF(ADR_LEN.NE.X2XS_ADRESS_LEN(OFF)) GOTO 120
		IF(X2XS_ADRESS(1,OFF).EQ.0 .AND.
     *		  X2XS_ADRESS(2,OFF).EQ.0) GOTO 120
		DO 110 OFF1=1,X2X_ADRESS_MAXLEN
		    IF(X2XS_ADRESS(OFF1,OFF).NE.STNADDRESS(OFF1))GO TO 120
110		CONTINUE
		STN_NO=OFF
		GO TO 130
120	      CONTINUE
	    ENDIF
C
C ***** End V03 changes *****
C
        ENDIF
        IF (SRCSTAT .EQ. 0) GOTO 130
C
	STN_NO = -1
	STATUS=-2
	RETURN

130	CONTINUE
	IF (.NOT.CMD_RESPONSE) THEN
	   STATUS = 1
	   IF (TERCOD.NE.0) STATUS=0
	ELSE
	   STATUS=-1
	ENDIF
	RETURN
9000	FORMAT(1H ,A,2(1X,Z8.8))					!V03
	END
