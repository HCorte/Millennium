C
C SUBROUTINE X2DEFCAL
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2DEFCAL.FOV                                 $
C  $Date::   17 Apr 1996 16:14:40                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2defcal.for;1 **
C
C X2DEFCAL.FOR
C
C V03 22-AUG-94 GPR USE DIAL ENABLE AND DIAL PORTS FROM STATION CLASS
C V02 28-APR-94 XXX GET STATION CLASS INFO FROM MEMORY
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will create the initial call default
C message for the station to store into EEPROM.
C
C Calling sequence:
C
C     CALL X2DEFCAL(TRABUF,MESS,MESLEN)
C
C Input parameters:
C
C     TRABUF      Int*4(TRALEN)       Transaction buffer
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
	SUBROUTINE X2DEFCAL(TRABUF,MESS,MESLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INTEGER*2   MESLEN                          !Output message length
	INTEGER*2   RESULT3
	INTEGER*4   MESS(*)                         !Output message
	INTEGER*4   OFF                             !Data offset relative to hdr
	INTEGER*4   STN                             !Station number
	INTEGER*4   RESULT2, RESULT1, TEMP, OUTCALL, I, CHKBYT
	INTEGER*4   CHKVAL, LSTBYT
        INTEGER*4   DISMODE                         !Real disconnection mode
        INTEGER*4   PORT_COUNT                      !Number of ports
        INTEGER*4   REC                             !Record number
        INTEGER*4   TEMP_ADR(2)                     !Work variable 
C
	LOGICAL     DIALPRT                         !Dialup port flag
        LOGICAL     OLD_PROTO               !STATION PROTOCOL REVISION FLAG
	INTEGER*4   CLASS			    !STATION CLASS	!V03

C
C INITIALIZE VARIABLES.
C
	STN=TRABUF(TXSTN)
	MESLEN=0
	OFF=X2STMES_DATA-1
	LSTBYT=0
        REC=0 !eliminate warnings
C
C DETERMINE IF THIS IS THE OLD STATION PROTOCOL OR NEW
C
        OLD_PROTO = .TRUE.
        IF((TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_DEFAULT_CONF2)
     *    .OR. (TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_RESET2))
     *                     OLD_PROTO = .FALSE.
C
C UPDATE THE STATION DATA HEADER.
C
	CALL ISBYTE(X2STMES_PROTID_VAL,MESS,X2STMES_PROTID-1)
	CALL ISBYTE(X2STMES_DATATYPE_CMD_DOWN,MESS,
     *	            X2STMES_DATATYPE-1)
	CALL ISBYTE(0,MESS,X2STMES_CONFCHK-1)
	CALL I4TOBUF2(STN,MESS,X2STMES_STATION_NO-1)
	CALL ISBYTE(X2STMES_DEF_CONF,MESS,X2STMES_CODE-1)
	CALL ISBYTE(X2STMES_DEF_CALL,MESS,X2STMES_DEF_TYPE-1)
C
C CHECK FOR VALID STATION NUMBER.
C
	IF(CHKVAL(STN,1,X2X_STATIONS,' STATION NUMBER ').NE.0) THEN
	  MESLEN=-1
	  GOTO 8000
	ENDIF
C
C CHECK TO ENSURE THE STATION CLASS INFORMATION
C EXISTS.
C
	CLASS=X2XS_STNCLS(STN)
	IF (CLASS.LE.0) THEN
          CALL OPS('X2DEFCAL:Station class does not exist ',
     *              STN,CLASS)
          MESLEN=-1
          GOTO 8000
        ENDIF
C
C STORE DEFAULT INITIAL CALL PARAMETERS INTO MESSAGE.
C
	CALL ISBYTE(X2STMES_DEF_CONF,MESS,OFF+X2STMES_CALL_CMD-1)
	CALL ISBYTE(0,MESS,OFF+X2STMES_CALL_CNTNO-1)
	CALL I4TOBUF2(STN,MESS,OFF+X2STMES_CALL_STNO-1)
	LSTBYT=X2STMES_CALL_STNO+2
	CHKBYT=OFF+LSTBYT-1
C
C STORE ASSIGNED DEFAULT NETWORK PORTS.
C IF NO PORT ASSIGNED SET TO ZERO.
C
C IF DIALUP IS ENABLED THEN TWO ADDITIONAL X32 PORTS MUST BE IDENTIFIED
C
        IF(.NOT.OLD_PROTO) THEN
          PORT_COUNT = X2XS_MAXDEF + X2X_MAXX32_ASSIGN		      !V03
        ELSE
          PORT_COUNT = X2XS_MAXDEF				      !V03
        ENDIF
C

        DO 100 I=1,PORT_COUNT
C
C UP TO SIX (6) SLOTS ARE AVAILABLE - BUT IF
C DIALUP PORTS ARE ASSIGNED AND DIAL ENABLE IS ON, THEN
C USE X2X_MAXDIAL_ASSIGN (2) OF THE SLOTS FOR DIALUP.
C TWO MORE DEFAULT PORTS WILL BE ASSIGNED FROM THE X.32 PORTS
C
          DIALPRT=.FALSE.
C
C
C IN ADDITION TO THE DIALUP PORTS ASSIGNED TWO X32 PORTS MUST ALSO
C BE ASSIGNED IF DIALUP IS ENABLED
C
C READ THE APPROPRIATE NETWORK PORT (NORMAL OR DIALUP OR X32).
C
C	  ***** Start V03 changes *****
C	
          IF((I.GE.1).AND.(I.LE.X2XS_MAXDEF)) THEN
	    REC=X2XS_DEF_PORT(I,STN)
	    IF (X2XC_DEF_PORT_OVERRIDE(CLASS).NE.0) THEN
		REC=X2XC_DEF_PORT_OVERRIDE(CLASS)
		IF (I.GT.1) REC=0
	    ENDIF
	  ENDIF		  
C
C	  ***** End V03 changes *****
C	
C IF DIAL IS ENABLED THEN THIS WILL OVERRIDE SLOT 3 & 4 ASSIGNED ABOVE
C
          IF(BX2XC_DIAL_ENABLE(CLASS).EQ.0) THEN		      !V03
            IF((I.GT.2).AND.(I.LE.X2XS_MAXDEF)) THEN
               DIALPRT = .TRUE.
               REC = X2XS_DIAL_PORT(I-2,STN)
	       IF(REC.EQ.0) THEN				      !V03
	         REC = X2XC_DIAL_PORT(I-2,CLASS)		      !V03
	       ENDIF						      !V03
            ENDIF
          ENDIF
C
          IF(I.GT.X2XS_MAXDEF) REC = X2XS_X32PORT(I-4,STN)	      !V03
C
C IF RECORD DOES NOT EXIST, CLEAR IT OUT, OTHERWISE SET
C OUTCALL TO VALID.
C
	  OUTCALL=0
	  IF (REC.GT.0) THEN						    !V03
	    IF (X2XPN_OUTCALL(REC).EQ.0.AND.X2XPN_TYPE(REC).NE.0) OUTCALL=1 !V03
	  ENDIF								    !V03
C
C STORE INFORMATION INTO OUTPUT BUFFER.
C
	  CALL I4TOBUF2(OUTCALL,MESS,OFF+LSTBYT-1)
	  LSTBYT=LSTBYT+X2STMES_CALL_OUTVALID
	  TEMP=0
	  IF(BX2XPN_FAST(REC).EQ.0) TEMP=TEMP+X2STMES_CALL_FS		!V03
	  IF(BX2XPN_REVCHRG(REC).EQ.0) TEMP=TEMP+X2STMES_CALL_RC	!V03
C
          IF(DIALPRT) THEN
            TEMP=TEMP+X2STMES_CALL_DIAL
            IF(X2XC_DISDUPROM(CLASS).GT.0) THEN				!V03
              DISMODE=X2XC_DISDUPROM(CLASS)				!V03
            ELSE
              DISMODE=X2XPN_DDIS(REC)					!V03
            ENDIF
          ELSE
            IF(X2XC_DISX25ROM(CLASS).GT.0) THEN				!V03
              DISMODE=X2XC_DISX25ROM(CLASS)				!V03
            ELSE
              DISMODE=X2XPN_DDIS(REC)					!V03
            ENDIF
          ENDIF
C
	  CALL I4TOBUF2(    TEMP,MESS,OFF+LSTBYT-1)
	  LSTBYT=LSTBYT+X2STMES_CALL_FLAGS

          CALL I4TOBUF2(DISMODE,MESS,OFF+LSTBYT-1)
	  LSTBYT=LSTBYT+X2STMES_CALL_DEFDIS
C
C	  ***** Start V03 Changes *****
C
	  CALL I4TOBUF2(X2XPN_RETCNT(REC),MESS,OFF+LSTBYT-1)
	  LSTBYT=LSTBYT+X2STMES_CALL_RETCNT
	  CALL I4TOBUF2(X2XPN_RETTIM(REC),MESS,OFF+LSTBYT-1)
	  LSTBYT=LSTBYT+X2STMES_CALL_RETINT
	  CALL ISBYTE(X2XPN_ADDLEN(REC),MESS,OFF+LSTBYT-1)
	  LSTBYT=LSTBYT+X2STMES_CALL_ADRLEN
	  IF((X2XPN_HUNT_ADR(1,REC).NE.0) .AND.
     *       (X2XPN_HUNT_ADR(2,REC).NE.0) )THEN
            TEMP_ADR(1) = X2XPN_HUNT_ADR(1,REC)
            TEMP_ADR(2) = X2XPN_HUNT_ADR(2,REC)
          ELSE
            TEMP_ADR(1) = X2XPN_ADRESS(1,REC)
            TEMP_ADR(2) = X2XPN_ADRESS(2,REC)
          ENDIF
          CALL X2QSHFT(TEMP_ADR,64-(X2XPN_ADDLEN(REC)*4)) 
C
C	  ***** End V03 Changes *****
C
          CALL I4TOBUF4(TEMP_ADR(1),MESS,OFF+LSTBYT-1)
	  CALL I4TOBUF4(TEMP_ADR(2),MESS,OFF+LSTBYT-1+4)
	  LSTBYT=LSTBYT+X2STMES_CALL_ADR
100	CONTINUE
	CALL X2CHKSUM(MESS,CHKBYT,76,RESULT1,RESULT2)
  	CALL ISBYTE(RESULT1,RESULT3,0)
	CALL ISBYTE(RESULT2,RESULT3,1)
C
C PROGRAM EXIT.
C
8000	CONTINUE
	MESLEN=LSTBYT-1
	RETURN
	END
