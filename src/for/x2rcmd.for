C
C SUBROUTINE X2RCMD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RCMD.FOV                                   $
C  $Date::   30 May 1996  9:28:12                                         $
C  $Revision::   1.4                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2xrel.for **
C
C 
C V06 06-FEB-96 DAS Removed Finland specific code
C V05 14-SEP-95 DAS REMOVED GLOBAL.DEF. NOW INCLUDED IN MSGCOM
C V04 12-DEC-94 GPR Integrate UK changes into X2X Baseline
C V03 19-JUL-94 WS MULTINETWORK CHANGES
C V02  7-MAR-94 JWE Put in some of the Finland changes to plug some
C		    relay holes...
C 
C X2RCMD.FOR
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C     X2RCMD(BUFFER,PROCESS,STATUS)
C       PURPOSE:
C         INITIATE STOP OR START OF PROCESS
C       INPUT:
C         BUFFER   -     BUFFER WITH DATA INFORMATION, FORMAT:
C              CMD - COMMAND CODE 2 - STOP, 3 - START
C              RN  - RELAY PROCESS #                    - 1 BYTE
C              PS  _ POLL/NOPOLL STATUS                 - 1 BYTE
C              S/G - STATION # OR GROUP # (-1) FOR ALL  - 4 BYTES
C	       SUBNETWORK - SUBNETWORK NO		- 1 BYTE - V03
C        OUTPUT:
C          PROCESS      - PROCESS NUMBER
C	   SUBNETWORK   - SUBNETWORK NO FOR THE PROCESS - V03
C          STATUS       -  0 IF OK
C                       - -1 IF RELEASE BUFFER
C                       - -2 IF PUT ON WAIT QUEUE(NO BUFFER RELEASE0
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
C
	SUBROUTINE X2RCMD(BUFFER,PROCESS,SUBNETWORK,STATUS)	!V03
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:MSGCOM.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
	INCLUDE 'INCLIB:X2RCMD.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:X2FEMES.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*2 BUFFER(*),TEMP2
	INTEGER*4 STATE, OFFSET
	INTEGER*4 HEAD, GROUP	!V03
	INTEGER*4 SAVE_STATION_ATRIBUTES
        INTEGER*4 STATION_ATRIBUTES
	INTEGER*4 STATION, LAST, FIRST, MAX, ATRIBUTE, DESTINATION
	INTEGER*4 PROCESS, CMD, I, STATUS
	INTEGER*4 POLL_STATUS, TEMP, CONF
	INTEGER*4 DSAP, CONTYPE, ISAP
	INTEGER*4 NEXT, NEXT_STATION
	INTEGER*2 NUM_BCST,ACTIVE_BCST,TYPE
	LOGICAL   CHAIN, ACTIVATE 
	LOGICAL*1   PROCEED
	INTEGER*4   BCST_ROUTE			!V03
	INTEGER*4   SUBNETWORK			!V03
	INTEGER*4   CLASS			!V03
	LOGICAL	    TO_STATION, SINGLE

	IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *	    TYPE 900,(BUFFER(I),I=1,6)
900	    FORMAT(' buffer in x2rcmd ',10(1X,Z4))
C 
	STATUS = -1
	SUBNETWORK=-1				!V03
        ACTIVATE = .FALSE.
C 
	CALL ILBYTE(CMD,BUFFER,X2ROFF_CMD-1)
C
C=====================================================================
C       COMMAND MUST BE EITHER START OR STOP ...
C       START COMMANDS CAN BE INITIATED WITH A CALL TO X2RSTART
C       STOP  COMMANDS CAN BE INITIATED WTIH A CALL TO X2RSTOP
C       IF THIS IS AN INVALID COMMAND RETURN WITH A STATUS OF -1
C       THIS BUFFER WILL BE RELEASED
C=====================================================================
C
	IF(CMD.NE.X2RCMD_STOP .AND. CMD.NE.X2RCMD_START) THEN
          STATUS = -1
          RETURN
        ENDIF    
C
C       GET THE PROCESS NUMBER
C
	CALL ILBYTE(PROCESS,BUFFER,X2ROFF_PROCESS-1)
C	START OF V03 CHANGE BLOCK
C 
C
C       CHECK THE DESTINATION/MUST BE TO ALL (-1) OR A STATION/GROUP #
C
	CALL MOV4TOI4(DESTINATION,BUFFER,X2ROFF_DEST-1)
	IF (DESTINATION .EQ. 0 .OR.
     *      DESTINATION .LT. X2RDEST_ALL) THEN
           STATUS = -1
           RETURN
        ENDIF
C
	CALL ILBYTE(POLL_STATUS,BUFFER,X2ROFF_POLL-1)
	IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) THEN
      	   TYPE *,'CMD, PROCESS, DESTINATION '
           TYPE *, CMD, PROCESS, DESTINATION
        ENDIF
C
        IF(IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) THEN
          TYPE *,'PROCESS: ',PROCESS,'X2X_RELAY_APPS: ',X2X_RELAY_APPS
          TYPE *,'DESTINATION: ',DESTINATION
          IF(PROCESS.GT.0 .AND. PROCESS.LE.X2X_RELAY_APPS) THEN
            TYPE *,' X2XR_APP_STATUS(',PROCESS,')',
     *               X2XR_APP_STATUS(PROCESS)
            ENDIF
        ENDIF

C
	FIRST = DESTINATION
	LAST =  DESTINATION
C 
	IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) THEN
           TYPE *,'CMD,PROCESS,DESTINATION,FIRST,LAST '
           TYPE * ,CMD,PROCESS,DESTINATION,FIRST,LAST
        ENDIF
C
C=====================================================================
C
C       STOP COMMAND ACTIVATED
C
C=====================================================================
C 
	IF (CMD.EQ.X2RCMD_STOP) THEN
C 
C
C       PROCESS MUST BE DEFINED
C
           IF(PROCESS.GT.0 .AND. PROCESS.LE.X2X_RELAY_APPS) THEN
            IF (X2XR_APP_STATUS(PROCESS).EQ.X2XR_APPS_NOT_DEF) THEN
               STATUS = -1
	    IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)	!V03
     *		      TYPE *,'not def process ',PROCESS	!V03
               RETURN
            ENDIF
           ELSE
            STATUS = -1
	    IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)	!V03
     *		      TYPE *,'wrong process ',PROCESS	!V03
            RETURN
           ENDIF
C

C          DETERMINE IF THIS PROCESS IS A CHAINED RELAY
C          THIS CAN ONLY BE TRUE IF THE PROCESS WAS STARTED
C          AS SUCH.
C
           CHAIN = .FALSE.
	   IF(X2XR_APP_ATRIBUTE(PROCESS).EQ.X2XR_APPA_CHAIN)
     *        CHAIN = .TRUE.
C
	   IF (DESTINATION.EQ.X2RDEST_ALL) THEN
	      FIRST=1
	      LAST=X2X_STATIONS
	   ENDIF
C 
C          LOOP THRU ALL STATIONS
C
	   DO 100 STATION=FIRST,LAST
C

C	      CHECKING SUBNETWORK TYPE IS NOT NECESSARY	- V03
C****	      CLASS=X2XS_STNCLS(STATION)		!V03
C****	      IF (CLASS.LE.0) GOTO 100			!V03
C****	      IF (X2XC_SUBNETWORK(CLASS).NE.X2XR_SUBNETWORK(PROCESS)) GOTO 100 !V03
C
C             IS THIS STATION SET AS ACTIVE...
C             NO NEED TO CHECK IF SAP IS ONLINE, THIS IS DONE
C             WHEN WE START A PROCESS. IF WE ATTEMPT TO STOP A
C             PROCESS THAT IS NOT ACTIVE THEN NO HARM DONE. 
C
	      IF (TSBIT(X2XR_STATION_ACTIVE(STATION),PROCESS-1)) THEN
C
C                IF NOT A CHAINED RELAY DECREMENT THE NUMBER OF ACTIVE
C                PROCESS
C
	         IF (.NOT.CHAIN) THEN
	            IF(X2XR_NO_ACTIVE(PROCESS).GE.1)
     *	               X2XR_NO_ACTIVE(PROCESS) = 
     *                      X2XR_NO_ACTIVE(PROCESS) - 1
	         ELSE
C 
C                   DECREMENT ACTIVITY COUNT ONLY FOR HEAD OF RELAYS
C 
	            GROUP=X2XS_GROUP(STATION)
	            HEAD=X2XR_SEND_STATION(GROUP,PROCESS)
	            IF(HEAD.EQ.STATION) THEN
                      IF(X2XR_NO_ACTIVE(PROCESS).GE.1)
     *	              X2XR_NO_ACTIVE(PROCESS) = 
     *                     X2XR_NO_ACTIVE(PROCESS) - 1
	              X2XR_SEND_STATION(GROUP,PROCESS) = 0            
	              X2XR_GROUP_ACTIVE(GROUP,PROCESS) = 0
	              X2XR_GROUP_REQUEST(GROUP,PROCESS)= 0
C
C Renable polling for the rest of the stations in the chain...
C
                      DO 31, NEXT = 1, X2X_LIST_LEN
                        NEXT_STATION = X2XG_LIST(NEXT,GROUP)
                        IF(NEXT_STATION .LE. 0) GOTO 41
                        CALL X2RPOLLCHK(NEXT_STATION, PROCESS)
31                    CONTINUE
41                    CONTINUE
	            ENDIF
	         ENDIF     ! /* END .NOT. CHAIN */
	      ENDIF    !/* END TSBIT(X2XR_STATION_ACTIVE) */
C
C             CLEAR THESE FIELDS REGARDLESS
C
	      CALL BCLR(X2XR_STATION_ACTIVE(STATION),PROCESS-1)
	      CALL BCLR(X2XR_STATION_REQUEST(STATION),PROCESS-1)
C
C             IF POLLING HAD BEEN DISABLED FOR THIS STATION 
C             DURING THIS RELAY PROCESS THEN REENABLE NOW
C
	      CALL X2RPOLLCHK(STATION,PROCESS)
C
	      X2XR_STATION_TIMOUT(STATION,PROCESS) = 0
	      X2XR_STATION_ID(1,STATION,PROCESS)   = 0   
	      X2XR_STATION_ID(2,STATION,PROCESS)   = 0
C
100	   CONTINUE    !/* END OF STATION LOOP */
C
C          RESET VALUES THAT HAD BEEN PREVIOUSLY SAVED DURING THE START
C 
           IF((X2XR_NO_ACTIVE(PROCESS).LE.0. .AND.
     *         X2XR_NO_REQUEST(PROCESS).LE.0.)
     *         .OR. DESTINATION.EQ.X2RDEST_ALL) THEN
	        X2XR_APP_STATUS(PROCESS)=X2XR_APPS_IDLE
                SAVE_STATION_ATRIBUTES = 
     *             X2XR_SAVE_STATION_ATRIBUTES(PROCESS) 
	        IF(SAVE_STATION_ATRIBUTES .NE. 0) THEN
		   IF(SAVE_STATION_ATRIBUTES.EQ.-1) 
     *                SAVE_STATION_ATRIBUTES=0  
	           X2X_STATION_ATRIBUTES = SAVE_STATION_ATRIBUTES
		   X2XR_SAVE_STATION_ATRIBUTES(PROCESS) = 0
	        ENDIF
C****V03	        SAVE_STATS_TIME = X2XR_SAVE_STATS_TIME(PROCESS)
C
C****V03	        IF(SAVE_STATS_TIME.NE.0) THEN
C****V03	          IF(SAVE_STATS_TIME.EQ.-1) SAVE_STATS_TIME = 0
C****V03		  X2XS_STATS_TIME = SAVE_STATS_TIME
C****V03		  X2XR_SAVE_STATS_TIME(PROCESS) = 0
C****V03	        ENDIF
C
                X2XR_NO_ACTIVE(PROCESS) = 0
	        X2XR_NO_REQUEST(PROCESS)= 0
                CALL FASTSET(0,X2XR_SEND_STATION(1,PROCESS),
     *                       X2X_NUM_GROUPS)
	        IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *	            TYPE *,'Setting the process to idle '
C 
                IF(CHAIN) THEN
	            CALL FASTSET(0,X2XR_GROUP_ACTIVE(1,PROCESS),
     *	                                         X2X_NUM_GROUPS/2)
	            CALL FASTSET(0,X2XR_GROUP_REQUEST(1,PROCESS),
     *	                                         X2X_NUM_GROUPS/2)
	            CALL FASTSET(0,X2XR_SEND_STATION(1,PROCESS),
     *                                           X2X_NUM_GROUPS)
	            IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *	               TYPE *,'STOPPING ALL GROUPS'
	        ENDIF
	   ENDIF
C 
C 
C=====================================================================
C 
C          START PROCESS ACTIVATED
C
C=====================================================================
C
	ELSEIF (CMD.EQ.X2RCMD_START) THEN
C
           IF(IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) 
     *        TYPE *,'ENTERING START COMMAND PROCESSING #1'
C
C          ASSIGN A PROCESS NUMBER IF NOT DEFINED
C
	   IF (PROCESS.EQ.0) THEN
	      CALL ILBYTE(SUBNETWORK,BUFFER,X2ROFF_SUBNETWORK-1)	!V03
	      CALL X2RASNPR(BUFFER,SUBNETWORK,PROCESS)			!V03
C
C             THERE IS NO PROCESS AVAILABLE AT THIS TIME SO PLACE 
C             THE REQUEST ON THE WAIT QUEUE.
C
              IF(IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) THEN
                TYPE *,'PROCESS ASSIGNED : ',PROCESS
              ENDIF
C
              IF(PROCESS.EQ.0) THEN
                STATUS = -2
                RETURN
C
              ELSEIF (PROCESS.LT.0.OR.PROCESS.GT.X2X_RELAY_APPS) THEN
                STATUS = -1
                RETURN
              ENDIF
C	
           ENDIF  !/* END PROCESS.EQ.0 */
C
C	START OF V03 CHANGE BLOCK

	   IF (X2XR_POLL_LOGIC_ENABLE(PROCESS).NE.0) POLL_STATUS=X2RPOLL

	   BCST_ROUTE=X2XR_BCST_ROUTE(PROCESS)
	   IF (X2X_BCST_ROUTE.NE.0)	    BCST_ROUTE=X2X_BCST_ROUTE
	   IF (X2XR_BCST_ENABLE(PROCESS).NE.0) BCST_ROUTE=0
	   IF (X2X_BCST_ENABLE.NE.0)	    BCST_ROUTE=0

	   IF (PROCESS.GE.X2XR_FIRST_COMMON_PROCESS) SINGLE=.TRUE.
	   CALL ILBYTE(ATRIBUTE,BUFFER,X2ROFF_DATA_ATRIBUTE-1)
	   IF (ATRIBUTE.EQ.X2XR_APPA_ALL_STN_NO_FORMAT) 
     *		      TO_STATION=.TRUE.
	   IF (SINGLE .AND. TO_STATION .AND. 
     *	    X2XR_ENABLE_STATION_BROADCAST(PROCESS).NE.0) BCST_ROUTE=0
C
C
C	END OF V03 CHANGE BLOCK
C          MESSAGE NUMBER MAY BE NEGATIVE TO PRESERVE THE SIGN 
C          COPY TO AN INTEGER 2 FIRST 
C
           CALL MOV2TOI2(TEMP2,BUFFER,X2ROFF_DATA_MSGNUM-1)
           X2XR_APP_DATA_MSGNUM(PROCESS) = TEMP2
C
           IF(IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) THEN
             TYPE *,' APP_DATA_MSGNUM: ',X2XR_APP_DATA_MSGNUM(PROCESS) 
             TYPE *,' TEMP MSGNUM: ',TEMP2
           ENDIF 
C
C          SIZE OF MESSAGE
C
	   IF (X2XR_APP_DATA_MSGNUM(PROCESS).GT.MSGSIZ) RETURN
	   CALL MOV2TOI4(X2XR_APP_DATA_LEN(PROCESS),BUFFER,
     *	            X2ROFF_DATA_LEN-1)
C
           IF(IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) THEN
             TYPE *,' APP_DATA_LEN: ',X2XR_APP_DATA_LEN(PROCESS)
             TYPE *,' APP_DATA_MAX: ',X2XR_APP_DATA_MAX*4
           ENDIF
C
C          MESSAGE TOO LONG
C
	   IF(X2XR_APP_DATA_LEN(PROCESS).GT.X2XR_APP_DATA_MAX*4) THEN
             STATUS = -1 
	    IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)			!V03
     *		      TYPE *,'INVALID LENGTH',X2XR_APP_DATA_LEN(PROCESS)!V03
             RETURN
           ENDIF
C
C          GET MESSAGES DESTINATION
C
	   CALL ILBYTE(X2XR_APP_DATA_DEST(PROCESS),BUFFER,
     *	            X2ROFF_DATA_DEST-1)
C
           IF(IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) THEN
             TYPE *,' APP_DATA_DEST: ',X2XR_APP_DATA_DEST(PROCESS)
             TYPE *,' APP_DS_MASK: ',X2STMES_RELAYF_DS_MASK
           ENDIF
C
C          CHECK FOR VAILD DESTINATION RANGE
C
	   IF(X2XR_APP_DATA_DEST(PROCESS).GT.X2STMES_RELAYF_DS_CENTRAL
     *	      .OR. X2XR_APP_DATA_DEST(PROCESS).LE.0)THEN
              STATUS = -1
	      IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)	!V03
     *		      TYPE *,'INVALID DESTINATION'		!V03
              RETURN
           ENDIF
C
C          CHECK FOR VALID ATTRIBUTE
C
	   CALL ILBYTE(ATRIBUTE,BUFFER,X2ROFF_DATA_ATRIBUTE-1)
	   IF(ATRIBUTE.EQ.X2XR_APPA_ALL_STN_NO_FORMAT .OR.
     *	      ATRIBUTE.EQ.X2XR_APPA_ALL_NO_FORMAT     .OR.
     *	      ATRIBUTE.EQ.X2XR_APPA_ALL               .OR.
     *	      ATRIBUTE.EQ.X2XR_APPA_CHAIN) THEN
               X2XR_APP_ATRIBUTE(PROCESS)=ATRIBUTE
           ELSE
               STATUS = -1
	    IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)		!V03
     *		      TYPE *,'INVALID ATRIBUTE ',ATRIBUTE	!V03
               RETURN
           ENDIF
C
	   IF (X2XR_APP_DATA_LEN(PROCESS).GT.0) THEN
	      CALL MOVBYT(BUFFER,X2ROFF_DATA,X2XR_APP_DATA(1,PROCESS),
     *	                   1,X2XR_APP_DATA_LEN(PROCESS))
	   ENDIF
C
C          IS IT CHAINED OR NOT ?
C
           IF(X2XR_APP_ATRIBUTE(PROCESS).EQ.X2XR_APPA_CHAIN)
     *                                               CHAIN = .TRUE.
C
	   IF(CHAIN .AND. CMD.EQ.X2RCMD_START) THEN                   
              MAX=X2X_NUM_GROUPS
           ELSE
              MAX=X2X_STATIONS
           ENDIF
C
	   IF (DESTINATION.GT.MAX) THEN
	      IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)	!V03
     *		      TYPE *,'BAD DESTINATION ',DESTINATION	!V03
              STATUS = -1
              RETURN
           ENDIF
C
C
C          IF DESTINATION IS TO ALL STATIONS THEN DETERMINE THE BEGINNING
C          AND LAST STATION NUMBER. (NOTE THAT A CHAINED RELAY USES THE 
C          GROUP CONCEPT AND NOT THE STATIONS.
C
	   IF (DESTINATION.EQ.X2RDEST_ALL) THEN
	      FIRST=1
	      IF(CHAIN) THEN
	        LAST=X2X_NUM_GROUPS
	      ELSE
	        LAST=X2X_STATIONS
C
C               IF POLLING IS DISABLED THEN DISABLE AUTOSTATS AND 
C               BUMP UP STATISTICS INTERVAL TO ALLOW STATIONS TO
C               RECEIVE UPDATED CONFIGURATION AT END OF PROCESS.
C
	        IF(POLL_STATUS.EQ.X2RNOPOLL) THEN
	           SAVE_STATION_ATRIBUTES = X2X_STATION_ATRIBUTES
	           IF(SAVE_STATION_ATRIBUTES.EQ.0) 
     *                SAVE_STATION_ATRIBUTES = -1
	           X2XR_SAVE_STATION_ATRIBUTES(PROCESS) = 
     *                SAVE_STATION_ATRIBUTES
C****V03	           SAVE_STATS_TIME = X2XS_STATS_TIME
C****V03	           IF(SAVE_STATS_TIME.EQ.0) SAVE_STATS_TIME = -1
C****V03	           X2XR_SAVE_STATS_TIME(PROCESS) = SAVE_STATS_TIME 
	           STATION_ATRIBUTES = 0
	           IF(IAND(X2X_STATION_ATRIBUTES,X2XSA_STATS).NE.0) 
     *                STATION_ATRIBUTES = X2XSA_STATS
	           X2X_STATION_ATRIBUTES = STATION_ATRIBUTES
C****V03	           X2XS_STATS_TIME =  600
	        ENDIF
	      ENDIF
	   ENDIF
C
C
C=====================================================================
C             CHAINED RELAYS 
C=====================================================================
C
C
	      IF (CHAIN) THEN 
C
	         DO 150 GROUP = FIRST,LAST
C
C
C                WE NEED TO DETERMINE IF A SAP IS AVAILABLE FOR USE
C                THIS WILL DEPEND ON THE NETWORK CONNECTION TYPE
C                CERTAIN NETWORK TYPES (I.E. ASYNC) WILL HAVE TO 
C                CHECK A SPECIFIC SAP WHILE OTHERS MAY HAVE A CHOICE
C                OF SAPS. WE WILL ALSO HAVE TO WORRY ABOUT GROUP VS.
C                STATION (MULTI NETWORK STUFF SHOULD GO HERE)
C
C **** CHANGE HERE FOR MULTI NETWORK ****
C
C                   FIND FIRST ACTIVE STATION IN GROUP
C
	            DO 110 OFFSET=1,X2X_LIST_LEN
	               STATION=X2XG_LIST(OFFSET,GROUP)
	               IF (STATION.GT.0) THEN
C	START OF V03 CHANGE BLOCK
			    CLASS=X2XS_STNCLS(STATION)
			    IF (CLASS.LE.0) GOTO 110
			    IF (X2XC_SUBNETWORK(CLASS).NE.
     *				  X2XR_SUBNETWORK(PROCESS)) GOTO 110
C	END OF V03 CHANGE BLOCK
                            CONTYPE=ZEXT(BX2XS_CONN_TYPE(STATION))
C
C Some other method of handling multi-network relay needs to be developed,
C but this will work for now...
C
                          IF(CONTYPE.EQ.X2XSCT_GTECH_DIAL) GOTO 110
C
	                  CALL ILBYTE(STATE,IX2XS_STATE,STATION-1)
	                  IF (STATE.NE.X2XS_IDLE .AND.
     *	                      STATE.NE.X2XS_INIT .AND.
     *	                      STATE.NE.X2XS_DISABLED) GOTO 110
C
C                         USE RELAY PRIORITY TO DETERMINE WHO TO SEND TO.
C
                          IF(X2X_REL_PRI.GT.X2X_REL_ALL) THEN
                            IF(X2X_REL_PRI.EQ.X2X_REL_GRP) THEN
                              IF(X2XG_STATE(GROUP).LE.X2XGS_DISABLED)
     *                          GOTO 150
                            ENDIF    
                            IF(X2X_REL_PRI.GE.X2X_REL_STN) THEN
                              IF(STATE.EQ.X2XS_DISABLED) GOTO 110
                            ENDIF
                          ENDIF
C	     
                          X2XR_SEND_STATION(GROUP,PROCESS)   = STATION
	                  X2XR_GROUP_ACTIVE(GROUP,PROCESS)   = 0
	                  X2XR_GROUP_REQUEST(GROUP,PROCESS)  = -1
	                  X2XR_GROUP_TIMOUT_CNT(GROUP,PROCESS) = 0
	                  IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *	                     TYPE *,'STARTING STATION ',STATION,
     *                              ' GROUP ',GROUP
                          CALL X2R_START_STATION(STATION, POLL_STATUS,
	1                   PROCESS, ACTIVATE)
                          GOTO 140
                       ENDIF
110                 CONTINUE
C
C Disable the rest of the stations in a chain
C
140              CONTINUE
                 IF(POLL_STATUS .EQ. X2RNOPOLL)THEN
                     DO 160 OFFSET = OFFSET, X2X_LIST_LEN
                        STATION=X2XG_LIST(OFFSET,GROUP)
                        IF(.NOT.TSBIT(BX2XS_PARAM(STATION),7) .AND.
	1                   STATION .GT. 0 .AND.
	2                    STATION .LT. X2X_STATIONS) THEN
                            X2XR_SAVE_STATION_POLL_FLAG(STATION,PROCESS)
	1                       = -1
                            CALL BSET(BX2XS_PARAM(STATION),7)
C
C                         UPDATE STATION CONFIGURATION
C
                            TEMP=ZEXT (BX2XS_CONF(STATION))
                            CONF=ISHFT(IAND(TEMP,'E0'X),-5)
                            CONF=CONF+1
                            IF(MOD(CONF,8).EQ.0) CONF=0
                            CONF=ISHFT(CONF,5)
                            TEMP=IAND(TEMP,'1F'X)+CONF
                            BX2XS_CONF(STATION)=TEMP
                        ENDIF
160                  CONTINUE
                 ENDIF
150              CONTINUE
C
	      ELSE
C
C=====================================================================
C                NOT A CHAINED RELAY 
C=====================================================================
C
	         DO 200 STATION = FIRST,LAST
C
C	START OF V03 CHANGE BLOCK
		    CLASS=X2XS_STNCLS(STATION)
		    IF (CLASS.LE.0) GOTO 200
		    IF (X2XC_SUBNETWORK(CLASS).NE.
     *			  X2XR_SUBNETWORK(PROCESS)) GOTO 200
C	END OF V03 CHANGE BLOCK

	            CONTYPE=ZEXT(BX2XS_CONN_TYPE(STATION))
                    DSAP = ZEXT(BX2XS_SAP(STATION))	!V03
C
C                   WE NEED TO CHECK HERE FOR CONNECTION TYPE
C                   SINCE X25 HAS NO SAP NUMBER WE WILL FORCE
C                   A SAP NUMBER. USE THE FACT THAT ASYNC STATION
C                   WILL ALWAYS REPORT A CAPACITY OF ZERO. THE SAP
C                   NUMBER IS NEEDED TO FORCE CODE EXECUTION AT THIS
C                   TIME. MULTI_NETWORK CODE SHOULD REPLACE THIS
C                   KLUDGE. GSAT STATIONS MAY NOT HAVE A SAP 
C                   ASSIGNED IF NO INCALL HAS TAKEN PLACE. IN ORDER
C                   TO BROADCAST A SAP IS NEEDED. THIS CODE NEEDS
C                   TO BE REVIEWED ONEC MULTI-NETWORK IS INTEGRATED.
C                   PERHAPS PVC SHOULD ASSIGN THE SAP IN X2LODSTN. 
C
                    IF(CONTYPE.EQ.X2XSCT_X25SVC) THEN
                      DO 201 ISAP = X2X_ACTUAL_SAP, X2X_SAP
			 IF (X2XE_SUBNETWORK(ISAP).NE.			!V03
     *			      X2XR_SUBNETWORK(PROCESS)) GOTO 201	!V03
                         IF(X2XE_CAPACITY(ISAP).NE.0) DSAP =ISAP
201                   CONTINUE
                    ELSEIF(CONTYPE.EQ.X2XSCT_USAT_PVC) THEN
                        IF (DSAP.EQ.0) DSAP =
     *                     X2XPL_SAP(X2XS_PHYS(STATION))
                    ENDIF
C
                    IF(IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) THEN
                      TYPE *,' STATION: ',STATION,
     *                       ' CONN_TYPE: ',CONTYPE,
     *                       ' ASSIGNED SAP:',DSAP 
                      ENDIF
C
	            IF(CONTYPE.EQ.X2XSCT_GTECH_DIAL .OR.
     *                 CONTYPE.EQ.X2XSCT_X28PAD) GOTO 200
C
C
C                   WE NEED TO DETERMINE IF A SAP IS AVAILABLE FOR USE
C                   THIS WILL DEPEND ON THE NETWORK CONNECTION TYPE
C                   CERTAIN NETWORK TYPES (I.E. ASYNC) WILL HAVE TO 
C                   CHECK A SPECIFIC SAP WHILE OTHERS MAY HAVE A CHOICE
C                   OF SAPS. WE WILL ALSO HAVE TO WORRY ABOUT GROUP VS.
C                   STATION (MULTI NETWORK STUFF SHOULD GO HERE)
C
C
		    PROCEED=.FALSE.
                    IF(DSAP.GE.X2X_ACTUAL_SAP
     *                .AND. DSAP.LE.X2X_SAP) THEN
                      IF(X2XE_ACT_STATUS(DSAP).EQ.X2XES_ONLINE)
	1		PROCEED=.TRUE.
		    ENDIF
                    IF (CONTYPE.EQ.X2XSCT_X21SWC) PROCEED=.TRUE.
		    IF (.NOT. PROCEED) GOTO 200
C
                        GROUP = X2XS_GROUP(STATION)
                        CALL ILBYTE(STATE,IX2XS_STATE,STATION-1)
	                IF (STATE.NE.X2XS_IDLE .AND.
     *	                    STATE.NE.X2XS_INIT .AND.
     *	                    STATE.NE.X2XS_DISABLED) GOTO 200
C V02
               TYPE=X2XS_TYPE(STATION)
               NUM_BCST = X2XS_BCST_NUM(STATION)
               ACTIVE_BCST=X2XS_BCST_ACTIVE_BITMAP(STATION)
 
               IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
C              START OF V03 CHANGE BLOCK
     *         TYPE *,'ROUTING: STN, NUM_BCST, ACTIVE, TYPE:',
     *		     ' BCST ROUTE '
C              END OF V03 CHANGE BLOCK
C
C       IF ROUTING FOR ALL "NO BCST STATIONS" SKIP BROADCAST
C       STATION.  CHECK IF BROADCAST TO SINGLE STATION
C
               IF (BCST_ROUTE.EQ.X2X_BCSTR_NO_BCST .AND.	!V03
     *             FIRST.NE.LAST                   .AND.
     *             TYPE.EQ.X2XST_BCST) THEN
                 GOTO 200
C
C       IF BROADCAST TO ALL "BCST STATIONS"  ONLY
C       CHECK IF BROADCAST TO SINGLE STATION
C
               ELSEIF (BCST_ROUTE.EQ.X2X_BCSTR_BCST_ONLY .AND.	!V03
     *             FIRST.NE.LAST                         .AND.
     *             TYPE.NE.X2XST_BCST) THEN
                 GOTO 200
C
C       IF BROADCAST TO ALL "BCST STATIONS" AND "NOT CONFIGURED"
C       SKIP ALL THAT HAVE ACTUAL BCST CONNECTIONS CONFIGURED
C       CHECK IF BROADCAST TO SINGLE STATION
C
               ELSEIF (BCST_ROUTE.EQ.X2X_BCSTR_PLUS_NOT_CONF .AND.	!V03
     *             FIRST.NE.LAST                             .AND.
     *             NUM_BCST.NE.0                             .AND.
     *             TYPE.NE.X2XST_BCST) THEN
                GOTO 200
C
C       IF BROADCAST TO "BCST STATIONS" AND "NOT ACTIVE"
C       SKIP ALL THAT HAVE REPORTED A BCST CONNECTION(S)
C       CHECK IF BROADCAST TO SINGLE STATION
C
               ELSEIF (BCST_ROUTE.EQ.X2X_BCSTR_PLUS_NOT_ACTIVE.AND.	!V03
     *             FIRST.NE.LAST                               .AND.
     *             ACTIVE_BCST.NE.0                            .AND.
     *             TYPE.NE.X2XST_BCST) THEN
                 GOTO 200
               ENDIF
C END V02
C
                        IF(X2X_REL_PRI.GT.X2X_REL_ALL) THEN
                          IF(X2X_REL_PRI.EQ.X2X_REL_GRP
     *                      .AND. GROUP.NE.0) THEN
                            IF(X2XG_STATE(GROUP).LE.X2XGS_DISABLED)
     *                        GOTO 200
                          ENDIF
C
                          IF(X2X_REL_PRI.GE.X2X_REL_STN) THEN
                            IF(STATE.EQ.X2XS_DISABLED) GOTO 200
                          ENDIF
                        ENDIF
C
                    IF (GROUP.NE.0)
     *	                X2XR_GROUP_TIMOUT_CNT(GROUP,PROCESS)=0
C
		      CALL X2R_START_STATION(STATION, POLL_STATUS,
	1		PROCESS, ACTIVATE)
C 
200	         CONTINUE
              ENDIF  !/* END OF .NOT. CHAIN */
C
C          BOTH CHAINED AND NON CHAINED RELAYS GET HERE
C          IF THE STATUS IS ZERO THE SET THE PROCESS AS ACTIVE
C
	   X2XR_NON_ACTIVE_CNT(PROCESS)=0
	   X2XR_ACTIVITY_CNT(PROCESS)=0
	   X2XR_TIMOUT_CNT(PROCESS)=0
C 
	   IF (ACTIVATE) THEN
	      X2XR_APP_STATUS(PROCESS) = X2XR_APPS_ACTIVE
	   ENDIF
C 
	ENDIF !/* END OF START COMMAND */
C 

	RETURN
	END
