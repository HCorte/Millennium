C
C SUBROUTINE X2R_START_STATION.FOF  
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2R_START_STATION.FOV                        $
C  $Date::   17 Apr 1996 16:33:00                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C This routine is called by X2RCMD when it needs to set various station
C states to indicate that a station in involvoed in a relay
C
C V01 19-JUL-94 WS MULTINETWORK CHANGES - Integrate UK changes into 
C		   X2X Baseline
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
C=======OPTIONS	/CHECK=NOOVERFLOW
	SUBROUTINE X2R_START_STATION(STATION, POLL_STATUS, PROCESS,
	1   ACTIVATE)
C
	IMPLICIT NONE
C
	INTEGER*4   STATION
	INTEGER*4   POLL_STATUS	!Disable polling on this station
	INTEGER*4   PROCESS	!Relay process running
	LOGICAL	    ACTIVATE	!Success flag for X2RCMD
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE	'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
	INCLUDE 'INCLIB:X2RCMD.DEF'
C
	INTEGER*4   TEMP
	INTEGER*4   CONF
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)			!V01
     *	  	TYPE *,'Entering X2R_START_STATION ',station,process	!V01
	IF(TSBIT(X2XR_STATION_REQUEST(STATION),PROCESS-1))
	1   GOTO 8000       !SKIP IF ALREADY ACTIVATED
	IF (TSBIT(X2XR_STATION_ACTIVE(STATION),PROCESS-1)
	1   .AND. X2XR_NO_ACTIVE(PROCESS).GE.1)           
	1   X2XR_NO_ACTIVE(PROCESS)=X2XR_NO_ACTIVE(PROCESS)-1
C 
C     IF POLL DISABLE IS SET THEN SET NOPOLL BIT IN
C     STATION PARAMETERS. SET SAVE VALUE TO -1 SO WE
C     KNOW THAT IT'S BEEN CHANGED.
C
	IF(POLL_STATUS.EQ.X2RNOPOLL) THEN
	    IF(.NOT.TSBIT(BX2XS_PARAM(STATION),7)) THEN
		X2XR_SAVE_STATION_POLL_FLAG(STATION,PROCESS) = -1
		CALL BSET(BX2XS_PARAM(STATION),7)
C
C     	 UPDATE STATION CONFIGURATION
C
		TEMP=ZEXT (BX2XS_CONF(STATION))
		CONF=ISHFT(IAND(TEMP,'E0'X),-5)
		CONF=CONF+1
		IF(MOD(CONF,8).EQ.0) CONF=0
		CONF=ISHFT(CONF,5)
		TEMP=IAND(TEMP,'1F'X)+CONF
		IF (X2XS_TYPE(STATION).EQ.X2XST_BCST) 	!V01
     *		     TEMP=0				!V01
		BX2XS_CONF(STATION)=TEMP
	    ENDIF
	ENDIF
C
	CALL BSET(X2XR_STATION_REQUEST(STATION),PROCESS-1)
	CALL BCLR(X2XR_STATION_ACTIVE(STATION),PROCESS-1)
	X2XR_NO_REQUEST(PROCESS) = 
	1   X2XR_NO_REQUEST(PROCESS) + 1
	X2XR_STATION_ID(1,STATION,PROCESS) = 0
	X2XR_STATION_ID(2,STATION,PROCESS) = 0
C
C     IF SENDING A SPECIFIC RESET MESSAGE FROM X2BRO
C     PASS THE  RESET NUMBER IN THE LOAD # SECTION
C     IN THE STATION_ID TO X2XRAPP.
C
	IF(X2XR_APP_DATA_MSGNUM(PROCESS).LT.0) THEN
	    X2XR_STATION_ID(2,STATION,PROCESS)=
	1	X2XR_APP_DATA_MSGNUM(PROCESS)
	ENDIF
	ACTIVATE = .TRUE.
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
	1   TYPE *,'STARTING STATION,PROCESS ',
	2   STATION,PROCESS
C
	IF(TSBIT(X2XR_STATION_REQUEST(STATION),PROCESS-1))
	1   GOTO 8000       !SKIP IF ALREADY ACTIVATED
	IF (TSBIT(X2XR_STATION_ACTIVE(STATION),PROCESS-1)
	1   .AND. X2XR_NO_ACTIVE(PROCESS).GE.1)           
	2   X2XR_NO_ACTIVE(PROCESS)=X2XR_NO_ACTIVE(PROCESS)-1
C 
C
C     IF POLL DISABLE IS SET THEN SET NOPOLL BIT IN
C     STATION PARAMETERS. SET SAVE VALUE TO -1 SO WE
C     KNOW THAT IT'S BEEN CHANGED.
C
	IF(POLL_STATUS.EQ.X2RNOPOLL) THEN
	    IF(.NOT.TSBIT(BX2XS_PARAM(STATION),7)) THEN
		X2XR_SAVE_STATION_POLL_FLAG(STATION,PROCESS)
	1	    = -1
		CALL BSET(BX2XS_PARAM(STATION),7)
C
C     	     UPDATE STATION CONFIGURATION
C
		TEMP=ZEXT (BX2XS_CONF(STATION))
		CONF=ISHFT(IAND(TEMP,'E0'X),-5)
		CONF=CONF+1
		IF(MOD(CONF,8).EQ.0) CONF=0
		CONF=ISHFT(CONF,5)
		TEMP=IAND(TEMP,'1F'X)+CONF
		BX2XS_CONF(STATION)=TEMP
	    ENDIF
	ENDIF
C
	CALL BSET(X2XR_STATION_REQUEST(STATION),PROCESS-1)
	CALL BCLR(X2XR_STATION_ACTIVE(STATION),PROCESS-1)
	X2XR_NO_REQUEST(PROCESS) = 
	1	     X2XR_NO_REQUEST(PROCESS) + 1
	X2XR_STATION_ID(1,STATION,PROCESS) = 0
	X2XR_STATION_ID(2,STATION,PROCESS) = 0
C
C     IF SENDING A SPECIFIC RESET MESSAGE FROM X2BRO
C     PASS THE  RESET NUMBER IN THE LOAD # SECTION
C     IN THE STATION_ID TO X2XRAPP.
C
	IF(X2XR_APP_DATA_MSGNUM(PROCESS).LT.0) THEN
	    X2XR_STATION_ID(2,STATION,PROCESS)=
	1	     X2XR_APP_DATA_MSGNUM(PROCESS)
	ENDIF
	ACTIVATE = .TRUE.
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
	1   TYPE *,'STARTING STATION,PROCESS ',
	2   STATION,PROCESS
C
8000	CONTINUE
	RETURN
	END
