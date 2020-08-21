C
C PROGRAM X2BRO
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2BRO.FOV                                    $
C  $Date::   30 May 1996  9:27:40                                         $
C  $Revision::   1.4                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2bro.for;1 **
C
C X2BRO.FOR
C
C V15 17-FEB-2011 RXK VERIFY is external function
C V14 08-JUN-2000 UXN 1811 label added.
C V13 17-APR-1996 wsm Added option to choose relay process if > 1 found for same
C                     subnet (terminal download option only, n/a for single msg). 
C V12 30-MAR-1996 DAS Added code for initiate background loads
C V11 28-MAR-1996 wsm Chain relay broadcast available only to all stns option.
C V10 05-MAR-1996 wsm Added check for -ve app # when returning fr X2GETAPPNO.
C V09 05-sep-1995 DAS Added code to support background loads
C V08 22-AUG-1995 DAS RE-structured the code 
C V07 10-JUL-1995 SCD Unsolicited terminal messages (option 12 - unsolicited 
C	  	      application message) are no longer supported
C		      in X2BRO.   These should now be handled elsewhere, 
C		      possibly the GUI.
C V06 14-APR-1995 SCD Broadcasts without Resets are not supported for DC
C		      since the required changes to the MCP and terminal
C		      applications have not been made for DC.
C V05 29-DEC-1994 GPR Handle multiple MCPs per subnetwork
C V04 12-DEC-1994 GPR Integrate UK changes into X2X Baseline
C V03 19-JUL-1994 WS  MULTINETWORK CHANGES
C V02 08-JAN-1992 DAS DISABLED RELAYS  (NOT PERTINENT TO REMOTE STATIONS)        
C V01 01-DEC-1991 XXX RELEASED FOR VAX (NETHERLANDS)
C
C START RELAY BROADCASTING FUNCTIONS
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM X2BRO
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:MSGCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'					!V04
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:PRMDLL.DEF'
	INCLUDE 'INCLIB:X2RLOD.DEF'					!V04
C
	INTEGER*4 PROBUF /0/,POLL_STS 
	INTEGER*4 ST, CMD, FORMAT, ANS
        INTEGER*4 PROCESS, MES_NUM, OUT_LEN, MES_DEST
        INTEGER*4 MAX_GROUPS, GROUP 
	INTEGER*4 MES_BUF(32), STN, I
	INTEGER*4 OPTION,OPTION2,OPTION3
	INTEGER*4 BROTYPE, JUMP_LABEL
        INTEGER*4 GRPCNT, RESET_TYPE
	INTEGER*4 SUBNETWORK, APP_ROMREV
	INTEGER*4 CLASS, APP, OPT, CLASSX
	INTEGER*4 NEXT_PROCESS, NEXT_SUBNETWORK
	INTEGER*4 FIRST_PROC, LAST_PROC
	INTEGER*4 RELAY_PROCESS
	INTEGER*4 DLL_RESET_TYPE
	INTEGER*4 FIRST_STATION,LAST_STATION
        INTEGER*4 ACL_APPNO
        INTEGER*4 RELAYCNT, RELAY_OPTION
        INTEGER*4 RELTAB(X2XR_FIRST_COMMON_PROCESS-1)
        CHARACTER ATTRIB_STRING(4)*6/'CHAIN ','ALLREL',
     *                               'ALLBRO','STNBRO'/
C
C
C        CHARACTER PROC_TYPE(0:3)*10/'FOREGROUND','FOREGROUND',
C     *                              'BACKGROUND','FORE+BACK '/
	CHARACTER BELL*1 /Z07/
C
	LOGICAL   NXTSTN, BY_GROUP, BY_RANGE
	LOGICAL	  FOUND_SUBNETWORK, INITIATE
	LOGICAL	  MCP_CHANGED, VERIFY				
        EXTERNAL  VERIFY
C
	CALL COPYRITE
C
        TYPE *
        TYPE *,'<<<<< X2BRO X2X Broadcasting Facility >>>>>'
        TYPE *
C
10	CONTINUE
        INITIATE = .FALSE.
        BY_GROUP = .FALSE.
        BY_RANGE = .FALSE.
        POLL_STS = 0
	GRPCNT=0
	BROTYPE=0
	GROUP=0
	RESET_TYPE=0
        OPTION2 = 0
C
C
C
	TYPE *
	TYPE *,'*************** Options *****************'
	TYPE *,'1. START a broadcast/send unsolicted message '
	TYPE *,'2. STOP a broadcasting process'
	TYPE *
C
	CALL INPNUM('Enter command .............................. ',
     *	             CMD,1,2,ST)
	IF (ST.LT.0) THEN
	   IF (PROBUF.GT.0) CALL RELBUF(PROBUF)
	   CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
C       PROCESS ACCORDING TO COMMAND OPTION
C
C
C           START  STOP
C              |    |
	GOTO (50,  9000) CMD
C
C         YOU SHOULD NEVER GET HERE SINCE INPNUM WILL PREVENT ANY NUMBER
C         FROM BEING ENTERED THAT IS NOT WITH THIS RANGE
C
          GOTO 10
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       START A RELAY PROCESS.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
50      CONTINUE
	NXTSTN=.FALSE.
	FORMAT=X2XR_APPA_ALL_NO_FORMAT
	MAX_GROUPS=X2X_STATIONS
C
C       DISPLAY AND PROMPT FOR METHOD OF BRAODCAST
C
	TYPE *
	TYPE *,'1. Send to a specific station.......'
	TYPE *,'2. Send to a specific group.........'
	TYPE *,'3. Send to all stations.............'
        TYPE *,'4. Send to a range of stations......'         
C 
	TYPE *
	CALL INPNUM('Enter option [1..4]......................... ',
     *	             BROTYPE,1,4,ST)
	IF(ST.LT.0) GOTO 10
C
C             BY   BY   ALL  BY  
C             STN  GRP  STN RANGE
C              |    |    |    |
        GOTO (100, 200, 300, 400), BROTYPE
C
C         YOU SHOULD NEVER GET HERE SINCE INPNUM WILL PREVENT ANY NUMBER
C         FROM BEING ENTERED THAT IS NOT WITH THIS RANGE
C
          GOTO 10
C
C         SEND TO A SPECIFIC STATION
C
100       CONTINUE
          CALL INPNUM('Enter station number ....................... ',
     *	                FIRST_STATION,1, X2X_STATIONS,ST)
          IF(ST.LT.0) GOTO 10
          LAST_STATION = FIRST_STATION
          CLASS=X2XS_STNCLS(FIRST_STATION)
	  IF (CLASS.LE.0) THEN
	     TYPE *,'Invalid station class for station ',FIRST_STATION
	     GOTO 10
	  ENDIF
	  SUBNETWORK=X2XC_SUBNETWORK(CLASS)
          GOTO 1000 
C
C         SEND TO A SPECIFIC GROUP
C
200       CONTINUE
          CALL INPNUM('Enter relay group number ....................',
     *	                 GROUP,1,
     *	                 X2X_NUM_GROUPS,ST)
	  IF(ST.LT.0) GOTO 10
	  GRPCNT=1
          BY_GROUP = .TRUE.
	  FIRST_STATION = X2XG_LIST(GRPCNT,GROUP)
	  LAST_STATION  = X2XG_LIST(GRPCNT,GROUP) 
          IF (FIRST_STATION.LE.0) THEN
             TYPE *,' Invalid group, bad leading station '
             GOTO 10
          ENDIF
C
	  CLASS=X2XS_STNCLS(FIRST_STATION)
	  IF (CLASS.LE.0) THEN
	     TYPE *,'Invalid station class for station ',FIRST_STATION
	     GOTO 10
	  ENDIF
	  SUBNETWORK=X2XC_SUBNETWORK(CLASS)
          GOTO 1000 
C
C         SEND TO ALL STATIONS
C
300       CONTINUE
	  FIRST_STATION = -1
	  CALL INPNUM('Enter subnetwork no : ',
     *	               SUBNETWORK,0,254,ST)
          GOTO 1000 
C
C         SEND TO A RANGE OF STATIONS
C
400       CONTINUE
          CALL INPNUM('Enter first station..........................',
     *                 FIRST_STATION,1,X2X_STATIONS,ST)
          IF(ST.LT.0) GOTO 10
          CALL INPNUM('Enter last station...........................',
     *                 LAST_STATION,FIRST_STATION,X2X_STATIONS,ST)
          IF(ST.LT.0) GOTO 10
          BY_RANGE = .TRUE.
          CLASS=X2XS_STNCLS(FIRST_STATION)
          IF (CLASS.LE.0) THEN
	     TYPE *,'Invalid station class for station ',FIRST_STATION
             GOTO 10
          ENDIF
C
C         ALL STATIONS MUST BE OF THE SAME CLASS FOR THIS OPTION
C
          SUBNETWORK=X2XC_SUBNETWORK(CLASS)
          DO I = FIRST_STATION,LAST_STATION
             IF (X2XS_STNCLS(I).NE.CLASS) THEN
                TYPE *,'Station ',I,' doesn''t belong to class ',CLASS
                GOTO 10
             ENDIF
          END DO
          GOTO 1000 
C
C         DISPLAY MENU ITEMS AND PROMPT USER.
C
1000      CONTINUE
	  TYPE *
	  TYPE *,'*********** Send unsolicited messages ***********'
	  TYPE *,' 1 - terminal hard reset.........................'
	  TYPE *,' 2 - news message ...............................'
	  TYPE *,' 3 - terminal ultimate reset ....................'
	  TYPE *,' 4 - terminal soft reset / SWAP loads ...........'
	  TYPE *,' 5 - station  soft reset ........................'
	  TYPE *,' 6 - station hard reset .........................'
	  TYPE *,' 7 - request station statistics .................'
	  TYPE *,' 8 - terminal download ..........................'
	  TYPE *,' 9 - station disable message.....................'
	  TYPE *,'10 - station wakeup message .....................'
	  TYPE *,'11 - terminal restart ...........................'
C....     TYPE *,'12 - unsolicited application message.............'
          TYPE *,'12 - Initiate background request.................'
	  TYPE *,'13 - disable stations ...........................'	!V04
	  TYPE *,'14 - enable stations ............................'	!v04
	  TYPE *,'15 - disable polling ............................'	!V04
	  TYPE *,'16 - enable polling .............................'	!V04
	  TYPE *
C
	  CALL INPNUM('Enter OPTION number [1..16]................. ',
     *	               OPTION,1,16,ST)
	  IF (ST.LT.0) GOTO 10
C
C         BEFORE GOING ANY FARTHER DETERMINE IF A PROCESS EXISTS FOR 
C         THIS SUBNETWORK TYPE
C         NOTE: TWO SEPARATE RELAY APPLICATION MUST BE AVAILBLE FOR
C               EACH SUBNETWORK. ONE IS FOR DOWNLOADS (1 - X2XR_FIRST_
C               COMMON_PROCESS). THE SECOND IS USED FOR ALL OTHER KIND
C               OF MESSAGES. (X2XR_FIRST_COMMON_PROCESS - X2X_RELAY_APPS)
C               THE VALUES NEEDS TO BE CONFIGURED CORRECTLY IN X2XPRM.DEF
C
	  IF (OPTION.LE.12 ) THEN
	    FOUND_SUBNETWORK = .FALSE.
	    FIRST_PROC = X2XR_FIRST_COMMON_PROCESS
	    LAST_PROC  = X2X_RELAY_APPS
	    IF(OPTION.EQ.8) THEN	
	       FIRST_PROC = 1
	       LAST_PROC  = X2XR_FIRST_COMMON_PROCESS-1
	    ENDIF
	    DO 1025 NEXT_PROCESS = FIRST_PROC, LAST_PROC
		IF (X2XR_APP_STATUS(NEXT_PROCESS).EQ.0) GOTO 1025
		NEXT_SUBNETWORK = X2XR_SUBNETWORK(NEXT_PROCESS)
		IF (SUBNETWORK.GE.0 .AND. 
     *		    NEXT_SUBNETWORK.EQ.SUBNETWORK)FOUND_SUBNETWORK=.TRUE.
		IF (SUBNETWORK.LT.0) THEN
		    TYPE *,' Will try to activate subnetwork: ',
     *		    NEXT_SUBNETWORK, ' relay process: ',NEXT_PROCESS
		    FOUND_SUBNETWORK=.TRUE.
		ENDIF
1025	    CONTINUE
	    IF (.NOT.FOUND_SUBNETWORK) THEN
		TYPE *,' No valid relay process found for subnetwork ',
     *                   SUBNETWORK
		GOTO 10
	    ENDIF
	  ENDIF
C
C         CONFIRM USERS INPUT.
C
	  TYPE *
	  TYPE *,'You are sending the following message:  '
	  TYPE *
C
C         PROCESS CODE ACCORDING TO OPTION SELECTED
C
C               TERM  NEWS  TERM  TERM  STN   STN   STN   TERM
C               HARD  MSG   ULT   SOFT  HARD  SOFT  STAT  DLL
C                |     |     |     |     |     |     |     |
C
          GOTO (1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800,
     *          1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600),OPTION
C
C                |     |     |     |     |     |     |     |         
C               STN   STN   TERM  BCK   DIS   ENAB  DIS   ENAB  
C               DIS   WAKE  REST  REQ   STN   STN   POLL  POLL  
C
C         YOU SHOULD NEVER GET HERE SINCE INPNUM WILL PREVENT ANY NUMBER
C         FROM BEING ENTERED THAT IS NOT WITH THIS RANGE
C
          GOTO 10

C
C         OPTION 1 : 
C
1100      CONTINUE
          TYPE *,' Terminal hard reset....................',BELL,BELL
          IF(.NOT.VERIFY(0)) GOTO 10
          GOTO 3000
C
C         OPTION 2 : 
C
1200      CONTINUE
          TYPE *,' News message disabled..................'
          IF(.NOT.VERIFY(0)) GOTO 10
          CALL XWAIT(2,2,ST)
          GOTO 10
C
C         OPTION 3 :
C
1300      CONTINUE
          TYPE *,' Terminal ultimate reset ...............',
     *             BELL,BELL,BELL,BELL
          IF(.NOT.VERIFY(0)) GOTO 10
          GOTO 3000
C
C         OPTION 4 :
C
1400      CONTINUE
          TYPE *,' Terminal soft reset / SWAP loads ......'
          IF(.NOT.VERIFY(0)) GOTO 10
          GOTO 3000

C
C         OPTION 5 :
C
1500      CONTINUE
          TYPE *,' Station  soft reset ...................'
          IF(.NOT.VERIFY(0)) GOTO 10
          GOTO 3000
C
C         OPTION 6 :
C
1600      CONTINUE
          TYPE *,' Station hard reset ....................'
          IF(.NOT.VERIFY(0)) GOTO 10
          GOTO 3000
C
C         OPTION 7 :
C
1700      CONTINUE
          TYPE *,' Request station statistics ............'
          IF(.NOT.VERIFY(0)) GOTO 10
C
	  TYPE *,' ******* STATISTICS OPTIONS *******'
	  TYPE *,' 1 - Station port status report'
	  TYPE *,' 2 - Call history report'
	  TYPE *,' 3 - X.25 stats report'
	  TYPE *,' 4 - Broadcast server status report'
	  CALL INPNUM(' Enter option ',OPTION2,1,4,ST)
	  IF (ST.LT.0) GOTO 10
	  CALL WIMG(5,' Do want to clear these statistics Y/N ')
	  CALL YESNO(ANS)
	  IF(ANS.NE.1) THEN
	    OPTION3=0
	  ELSE           
	    OPTION3=X2STMES_STS_RESET
	  ENDIF
          GOTO 3000
C
C         OPTION 8 :
C
1800      CONTINUE
          TYPE *,' Terminal download .....................'
          IF(.NOT.VERIFY(0)) GOTO 10
C
          IF(SUBNETWORK.EQ.-1) THEN			
            TYPE *,'  '
	    TYPE *,' Cannot send download to all subnetworks '
            TYPE *,' Select by individual subnetwork only    '
            TYPE *,' '
	    GOTO 10					
	  ENDIF					
C
C         DETERMINE LOAD TYPE AND VERIFY THAT USER WISHES TO 
C         SET SUCH
C
          IF(P(COMPRESSED_LOAD).EQ.0) THEN
            TYPE *,' Down load will be COMPRESSED........'
          ELSE
            TYPE *,' Down load will be REGULAR...........'
          ENDIF
          CALL WIMG(6,' Is this correct Y/N ')
          CALL YESNO(ANS)
          IF(ANS.NE.1) THEN                  
            IF(P(COMPRESSED_LOAD).EQ.1) THEN 
                P(COMPRESSED_LOAD) = 0
            ELSE
                P(COMPRESSED_LOAD) = 1
            ENDIF
          ENDIF
C
C         DETERMINE IF THE MCP HAS CHANGED. IF IT HAS THEN ALL LOADS
C         MUST BE SENT. FORCE AN ULTIMATE RESET.
C         
C
          RELAYCNT = 0
          RELAY_OPTION = 0
          CALL FASTSET(0,RELTAB,X2XR_FIRST_COMMON_PROCESS-1)
          POLL_STS = 1
	  RELAY_PROCESS = 0
          MCP_CHANGED = .FALSE.
          DO 1810 NEXT_PROCESS = 1,X2XR_FIRST_COMMON_PROCESS-1
            IF (X2XR_APP_STATUS(NEXT_PROCESS).EQ.0) GOTO 1810
            IF (X2XR_SUBNETWORK(NEXT_PROCESS).EQ.SUBNETWORK) THEN
c-                TYPE *,' Will start relay process ',NEXT_PROCESS
c-                TYPE *,' Process is defined as ',
c-     *                  PROC_TYPE(X2XR_APP_TO_SEND(NEXT_PROCESS))
              RELAY_PROCESS=NEXT_PROCESS
              FORMAT=X2XR_APP_ATRIBUTE(NEXT_PROCESS)
              RELAYCNT = RELAYCNT + 1
              RELTAB(RELAYCNT) = NEXT_PROCESS
              IF (RELAYCNT.EQ.1) THEN
                TYPE*
                TYPE*,' THE FOLLOWING RELAY PROCESS FOUND'
              ENDIF
              TYPE*,RELAYCNT,'. PROCESS=',NEXT_PROCESS,
     *        '    ATTRIBUTE=', ATTRIB_STRING(FORMAT)
            ENDIF
1810      CONTINUE
1811      CONTINUE
          IF (RELAYCNT.GE.2) THEN
	    CALL INPNUM('Enter option  ................. ',
     *	                 RELAY_OPTION,1,RELAYCNT,ST)
	    IF (ST.LT.0) GOTO 10
            RELAY_PROCESS = RELTAB(RELAY_OPTION)
            FORMAT=X2XR_APP_ATRIBUTE(RELAY_PROCESS)
            IF (BROTYPE.EQ.1 .AND. 
     *          FORMAT.EQ.X2XR_APPA_CHAIN) THEN
              TYPE*,'Chain relay for single station not applicable'
              GOTO 1811
            ENDIF 
            IF (BY_GROUP .AND. FORMAT.EQ.X2XR_APPA_CHAIN)
     *         FIRST_STATION=GROUP
          ENDIF
C
C         SHOULD CHECK FOR ROM VERSION NOW
C
          DO 1825 CLASS=1,X2XC_CLASSES
C                         
             IF (X2XC_SUBNETWORK(CLASS).NE.SUBNETWORK
     *           .OR. BX2XC_STATE(CLASS).LE.0) GOTO 1825
C
C             SHOULD HAVE AT LEAST FIRST ROMREV SET,
C             IF NOT CHECK ALL APPLICATIONS
C
              APP_ROMREV = X2XC_ROMREV1(CLASS)
              IF (APP_ROMREV.EQ.0) THEN
                  DO APP = 1,MAXAPP
                     IF (SMFDLTAB(MCP_LOAD_NO,SNDFLG,APP).EQ.1) THEN
                        MCP_CHANGED = .TRUE.
                        GOTO 1830
                     ENDIF
                  END DO
                  GOTO 1830
              ELSE

                  CALL X2GETAPPNO(APP_ROMREV,APP,FOREGROUND_LOAD)
                  IF (APP.LE.0) GOTO 1825
                  IF (SMFDLTAB(MCP_LOAD_NO,SNDFLG,APP).EQ.1) THEN
                      MCP_CHANGED = .TRUE.
                      GOTO 1830
                  ENDIF
              ENDIF
C
              APP_ROMREV = X2XC_ROMREV2(CLASS)
              CALL X2GETAPPNO(APP_ROMREV,APP,FOREGROUND_LOAD)
              IF (APP.LE.0) GOTO 1825 
              IF (SMFDLTAB(MCP_LOAD_NO,SNDFLG,APP).EQ.1) THEN
                        MCP_CHANGED = .TRUE.
                        GOTO 1830
              ENDIF
C
              APP_ROMREV = X2XC_ROMREV3(CLASS)
              CALL X2GETAPPNO(APP_ROMREV,APP,FOREGROUND_LOAD)
              IF(APP.LE.0) GOTO 1825
              IF (SMFDLTAB(MCP_LOAD_NO,SNDFLG,APP).EQ.1) THEN
                  MCP_CHANGED = .TRUE.
                  GOTO 1830
              ENDIF
1825	  CONTINUE
C
C
C        NOTIFY USER THAT THE MCP HAS BEEN DETECTED AS CHANGED
C
1830	  CONTINUE
          IF (MCP_CHANGED) THEN
              IF(X2XR_APP_TO_SEND(RELAY_PROCESS).NE.
     *                                          X2XRF_BACKGROUND) THEN
               TYPE *,'MCP was exchanged for app. ',APP,BELL,BELL,BELL
               TYPE *,' '
               TYPE *,'************** SELECT OPTION ************'
               TYPE *,' '
               TYPE *,'1. Complete reload of terminal ..............'
               TYPE *,'2. Send MCP and other selected loads ........'
               TYPE *
               CALL INPNUM('Enter option ...........................',
     *                      OPT,0,2,ST)
               IF (ST.LT.0) GOTO 10
               IF (OPT.EQ.1) THEN
                   RESET_TYPE=5
               ELSE IF (OPT.EQ.2) THEN
                   RESET_TYPE=1
               ENDIF
            ELSE
               TYPE *,' '
               TYPE *,' The MCP has changed. The process is unable to '
               TYPE *,' to accomodate this. Change the process and re-'
               TYPE *,' submit the broadcast                          '
               TYPE *,' '
               GOTO 10 
            ENDIF 
C
C
C
          ELSE
	    TYPE *
	    TYPE *,'************** Broadcast options ************'
	    TYPE *,' '
	    TYPE *,'0. Use default from global/relay parameters .........'
	    TYPE *,'1. Soft reset - selective download + MCP seg 0 ......'
	    TYPE *,'2. Do not send reset nor MCP - selective download ...'
	    TYPE *,'3. Do not send reset nor MCP - full application load '
	    TYPE *,'4. Soft reset - full application load + MCP seg 0 ...'
	    TYPE *
	    CALL INPNUM('Enter option .................................. ',
     *	                 RESET_TYPE,0,4,ST)
	    IF (ST.LT.0) GOTO 10
C
C           CONFIRM RESET MESSAGE NUMBER.
C
	    TYPE *
	    TYPE *,'You selected following broadcast option:  '
	    TYPE *
	    IF(RESET_TYPE.EQ.0) THEN
              IF(X2XR_APP_TO_SEND(RELAY_PROCESS).NE.
     *                                          X2XRF_BACKGROUND) THEN
                TYPE *,'Use default from relay process ',RELAY_PROCESS
              ELSE
                TYPE *,'Invalid option for background relay process '
                GOTO 10
              ENDIF  
             IF (X2XR_DLL_RESET_MESSAGE(RELAY_PROCESS).LE.0) THEN !NOT DEFINED
	        TYPE *,'Send selective load, no reset, no MCP '
             ELSEIF(X2XR_DLL_RESET_MESSAGE(RELAY_PROCESS).EQ.HRESET)THEN !HARD RES
	        TYPE *,'Send hard reset and full load ',BELL,BELL
             ELSEIF(X2XR_DLL_RESET_MESSAGE(RELAY_PROCESS).EQ.MNEWS)THEN !BAD NO
	        TYPE *,'Send selective load, no reset, no MCP '
             ELSEIF(X2XR_DLL_RESET_MESSAGE(RELAY_PROCESS).EQ.URESET)THEN !ULTIMATE
		TYPE *,'Send ultimate reset and full load ',BELL,BELL
             ELSEIF(X2XR_DLL_RESET_MESSAGE(RELAY_PROCESS).EQ.SRESET)THEN !SOFT
		TYPE *,'Send soft reset and selective load ',BELL,BELL
             ELSEIF(X2XR_DLL_RESET_MESSAGE(RELAY_PROCESS).GE.RRESET)THEN !RESTART
		TYPE *,'Send selective load, no reset, no MCP '
             ENDIF
	    ELSEIF(RESET_TYPE.EQ.1) THEN
              IF(X2XR_APP_TO_SEND(RELAY_PROCESS).NE.
     *                                          X2XRF_BACKGROUND) THEN
	        TYPE *,'Soft reset, do not send MCP (selective load)..'
              ELSE
                TYPE *,'Invalid option for background relay process '
                GOTO 10
              ENDIF 
	    ELSEIF(RESET_TYPE.EQ.2) THEN
	      TYPE *,'Do not send reset nor MCP (selective broadcast)'
	    ELSEIF(RESET_TYPE.EQ.3) THEN
	      TYPE *,'Do not send reset nor MCP (full terminal load)'
	    ELSEIF(RESET_TYPE.EQ.4) THEN
              IF(X2XR_APP_TO_SEND(RELAY_PROCESS).NE.
     *                                          X2XRF_BACKGROUND) THEN
	        TYPE *,'Soft reset, do not send MCP (full load)'
              ELSE
                TYPE *,'Invalid option for background relay process '
                GOTO 10
              ENDIF
	    ENDIF
C
C...	    IF(RESET_TYPE.EQ.2 .OR. RESET_TYPE.EQ.3) THEN
C...	      TYPE *, ' '
C...	      TYPE *,' Broadcasts without resets are',
C... *               ' NOT supported for this site'
C...	      GOTO 1830
C...	    ENDIF
C
          ENDIF 
C
C
C
          CALL DISPLAY_LOAD_REQUEST(SUBNETWORK, RESET_TYPE)
          IF(.NOT.VERIFY(0)) GOTO 10
C
1850	  CONTINUE
	  IF(RESET_TYPE.EQ.0) THEN
	    DLL_RESET_TYPE=0
	  ELSEIF(RESET_TYPE.EQ.1) THEN
	    DLL_RESET_TYPE=X2RLOD_SOFT_SEL
	  ELSE IF(RESET_TYPE.EQ.2) THEN
	    DLL_RESET_TYPE=X2RLOD_NO_RESET_SEL
	  ELSE IF(RESET_TYPE.EQ.3) THEN
	    DLL_RESET_TYPE=X2RLOD_NO_RESET_FULL
	  ELSE IF(RESET_TYPE.EQ.4) THEN
	    DLL_RESET_TYPE=X2RLOD_SOFT_FULL
	  ELSEIF(RESET_TYPE.EQ.5) THEN
	    DLL_RESET_TYPE=X2RLOD_FULL
	  ENDIF
          GOTO 4000
C
C         OPTION 9 :
C
1900      CONTINUE
          TYPE *,' Station disable message ................'
          IF(.NOT.VERIFY(0)) GOTO 10
	  OPTION2 = 1
	  CALL INPNUM('Enter disable delay [0...] ................. ',
     *                 OPTION3,0,65535,ST)
	  IF (ST.LT.0) GOTO 10
          GOTO 3000

C
C         OPTION 10 :
C
2000      CONTINUE
          TYPE *,' Station wakeup message ................'
          IF(.NOT.VERIFY(0)) GOTO 10
          GOTO 3000
C
C         OPTION 11 :
C
2100      CONTINUE
          TYPE *,' Terminal restart / Start background....'
          IF(.NOT.VERIFY(0)) GOTO 10
          GOTO 3000
C
C         OPTION 12 :
C
2200      CONTINUE
          TYPE *,' Initiate background load request ......'
          IF(.NOT.VERIFY(0)) GOTO 10
C
          INITIATE = .TRUE.
          ACL_APPNO = 0
          DO 2290 CLASSX = 1,X2XC_CLASSES
             IF(X2XC_SUBNETWORK(CLASSX).NE.SUBNETWORK) GOTO 2290
C 
             APP_ROMREV = X2XC_ROMREV1(CLASSX)
             CALL X2GETAPPNO(APP_ROMREV,APP,FOREGROUND_LOAD)
             IF(APP.GT.0) THEN
               IF(SMFDLTAB(ACL_LOAD_NO,LODADR,APP).GT.0) THEN
                  ACL_APPNO = APP
                  GOTO 2291
               ENDIF  
             ENDIF
C
             APP_ROMREV = X2XC_ROMREV2(CLASSX)
             CALL X2GETAPPNO(APP_ROMREV,APP,FOREGROUND_LOAD)
             IF(APP.GT.0) THEN
               IF(SMFDLTAB(ACL_LOAD_NO,LODADR,APP).GT.0) THEN
                  ACL_APPNO = APP
                  GOTO 2291
               ENDIF
             ENDIF
C 
             APP_ROMREV = X2XC_ROMREV3(CLASSX)
             CALL X2GETAPPNO(APP_ROMREV,APP,FOREGROUND_LOAD)
             IF(APP.GT.0) THEN
               IF(SMFDLTAB(ACL_LOAD_NO,LODADR,APP).GT.0) THEN
                  ACL_APPNO = APP
                  GOTO 2291
               ENDIF
             ENDIF 
C
2290      CONTINUE
C
2291      CONTINUE
          IF(ACL_APPNO .EQ. 0) THEN
            TYPE *,' '
            TYPE *,' Background downloads do not exist ',
     *               'for subnetwork ',SUBNETWORK,
     *               ' class ',CLASS
            GOTO 10 
          ENDIF
          GOTO 3000
C
C...      TYPE *,' Unsolicted application message ........'
C...      IF(.NOT.VERIFY(0)) GOTO 10
C...      GOTO 3500
C
C         THIS IS NOT SUPPORTED IN PX2X SITES
C
C...      TYPE *,' Unsolicted application messages are', 
C..     *        ' NOT supported for this site'
C.        GOTO 10
C
C         OPTION 13 :
C
2300      CONTINUE
          TYPE *,' Disable stations ......................'
          IF(.NOT.VERIFY(0)) GOTO 10
C
          IF(FIRST_STATION.EQ.-1) THEN
            FIRST_STATION = 1
            LAST_STATION = X2X_STATIONS
          ENDIF 
C
2350      CONTINUE
	  CALL X2SETDISABLE(SUBNETWORK,FIRST_STATION,LAST_STATION)
          IF(BY_GROUP) THEN
            IF(GRPCNT.LT.X2XG_CNT(GROUP)) THEN
               GRPCNT = GRPCNT+1
               FIRST_STATION = X2XG_LIST(GRPCNT,GROUP)
               LAST_STATION  = FIRST_STATION 
               GOTO 2350
            ENDIF
           ENDIF
	  TYPE *,'Updated in central memory only for subnetwork ',
     *		  SUBNETWORK
	  TYPE *,'Run soft station reset to update stations'
	  GOTO 10
C
C         OPTION 14 :
C
2400      CONTINUE
          TYPE *,' Enable stations .......................'
          IF(.NOT.VERIFY(0)) GOTO 10
C
          IF(FIRST_STATION.EQ.-1) THEN
            FIRST_STATION = 1
            LAST_STATION = X2X_STATIONS
          ENDIF 
C
2450      CONTINUE
	  CALL X2SETENABLE(SUBNETWORK,FIRST_STATION,LAST_STATION)
          IF(BY_GROUP) THEN
            IF(GRPCNT.LT.X2XG_CNT(GROUP)) THEN
               GRPCNT = GRPCNT+1
               FIRST_STATION = X2XG_LIST(GRPCNT,GROUP)
               LAST_STATION = X2XG_LIST(GRPCNT,GROUP)
               GOTO 2450
            ENDIF
           ENDIF
C
	  TYPE *,' Updated in central memory only for subnetwork ',
     *		      SUBNETWORK
	  TYPE *,' Run soft station reset to update stations'
	  GOTO 10
C
C         OPTION 15 :
C
2500      CONTINUE
          TYPE *,' Disable polling .......................'
          IF(.NOT.VERIFY(0)) GOTO 10
C
          IF(FIRST_STATION.EQ.-1) THEN
            FIRST_STATION = 1
            LAST_STATION = X2X_STATIONS
          ENDIF  
2550      CONTINUE
 	  CALL X2SETNOPOLL(SUBNETWORK,FIRST_STATION,LAST_STATION)
          IF(BY_GROUP) THEN
            IF(GRPCNT.LT.X2XG_CNT(GROUP)) THEN
               GRPCNT = GRPCNT+1
               FIRST_STATION = X2XG_LIST(GRPCNT,GROUP)
               LAST_STATION = X2XG_LIST(GRPCNT,GROUP)
               GOTO 2550
            ENDIF
           ENDIF
C
	  TYPE *,' Updated in central memory only for subnetwork ',
     *		      SUBNETWORK
	  TYPE *,' Run soft station reset to update stations'
	  GOTO 10
C
C         OPTION 16 :
C
2600      CONTINUE
          TYPE *,' Enable polling ........................'
          IF(.NOT.VERIFY(0)) GOTO 10
C
          IF(FIRST_STATION.EQ.-1) THEN
            FIRST_STATION = 1
            LAST_STATION = X2X_STATIONS
          ENDIF 
2650      CONTINUE
	  CALL X2SETPOLL(SUBNETWORK,FIRST_STATION,LAST_STATION)
          IF(BY_GROUP) THEN
            IF(GRPCNT.LT.X2XG_CNT(GROUP)) THEN
               GRPCNT = GRPCNT+1
               FIRST_STATION = X2XG_LIST(GRPCNT,GROUP)
               LAST_STATION = X2XG_LIST(GRPCNT,GROUP)
               GOTO 2650
            ENDIF
           ENDIF
C
	  TYPE *,' Updated in central memory only for subnetwork ',
     *		      SUBNETWORK
	  TYPE *,' Run soft station reset to update stations'
	  GOTO 10  
C
C
C       OPTIONS  1 THRU 7, and 9 THRU 11 WILL GET YOU HERE
C       *** OPTION 12 (INITIATE BACKGROUND REQUEST) WILL GET YOU HERE
C   
C
3000    CONTINUE
        ASSIGN 3000 TO JUMP_LABEL
        RELAY_PROCESS=0
        IF (INITIATE) THEN
          IF (ACL_APPNO .GT. 0) THEN
            OPTION2 = SMFDLTAB(ACL_LOAD_NO,LODADR,ACL_APPNO)
D           TYPE *,'ACL MESS NUMBER ', OPTION2
          ELSE
            TYPE *,' No background application found'
            GOTO 10
          ENDIF
        ENDIF
C
        CALL X2BROMES(OPTION,OPTION2,OPTION3,MES_BUF,0,MES_NUM,
     *	              OUT_LEN,MES_DEST,FORMAT)
C       
        GOTO 6500
C
C       OPTION 12 WILL GET YOU HERE (UNSOLICITED APP. MESSAGE)
C
3500    CONTINUE
        ASSIGN 3500 TO JUMP_LABEL
        RELAY_PROCESS = 0
        CALL X2UNMESS(OPTION,OPTION2,OPTION3,MES_BUF,0,MES_NUM,
     *	                OUT_LEN,MES_DEST,FORMAT,NXTSTN)
 
        GOTO 6500
C
C       OPTION 8 (DOWNLOADS) WILL GET YOU HERE
C
4000    CONTINUE
        ASSIGN 4000 TO JUMP_LABEL
        MES_NUM=DLL_RESET_TYPE
	OUT_LEN=0
	MES_DEST=X2STMES_RELAYF_DS_TERM
C
C       CHECK FOR VALID FORMAT, SUBNET AND RELAY PROCESS
C
5000    CONTINUE
        IF ((FORMAT.EQ.X2XR_APPA_ALL_NO_FORMAT.OR.
     *       FORMAT.EQ.X2XR_APPA_CHAIN)
     *      .AND. SUBNETWORK.GE.0
     *      .AND. RELAY_PROCESS.GT.0) GOTO 6000
	TYPE *,'No valid process available for subnetwork: ',SUBNETWORK
	GOTO 10
C
C       START THE RELAY/BROADCAST MECHANISM.
C
6000	CONTINUE
            IF(X2XR_APP_TO_SEND(RELAY_PROCESS).EQ.
     *                          X2XRF_BACKGROUND) THEN
                POLL_STS = 0
            ENDIF
6500    CONTINUE
C
D	    TYPE *,' Calling X2rstart BUFFER: ',PROBUF
D           TYPE *,' Process: ',RELAY_PROCESS,' Poll status: ',POLL_STS
D           TYPE *,' Station: ',FIRST_STATION
D     	    TYPE *,' Msg # ',MES_NUM,' Length: ',OUT_LEN
D           TYPE *,' Dest: ',MES_DEST,' Format: ',FORMAT
D           TYPE *,' Subnetwork: ',SUBNETWORK
C
C
C       GET A BUFFER AND SEND TO X2XREL 
C
       	IF (PROBUF.LE.0) THEN
       	   CALL GETBUF(PROBUF)
        	   IF (PROBUF.LE.0) THEN
       	      TYPE *,' UNABLE to get a buffer ',PROBUF
       	      CALL XWAIT(3,2,ST)
       	      GOTO 6000
       	   ENDIF
       	ENDIF
C
D       CALL OPS(' Calling x2rstart',0,0)
   	CALL X2RSTART(PRO(1,PROBUF),RELAY_PROCESS,POLL_STS,
     *                FIRST_STATION,MES_NUM,
     *                OUT_LEN,MES_BUF,MES_DEST,FORMAT,SUBNETWORK)	!V03
	IF (SUBNETWORK.GE.0) THEN
   	    CALL X2RADDBF(PROBUF)
D	    TYPE *,' Submitting  to subnetwork ',SUBNETWORK,PROBUF
	    PROBUF=0
	ENDIF

7000	CONTINUE
	IF(BY_GROUP) THEN
	  IF(GRPCNT.LT.X2XG_CNT(GROUP)) THEN
	    GRPCNT=GRPCNT+1
            IF (FORMAT.NE.X2XR_APPA_CHAIN) THEN
	      FIRST_STATION=X2XG_LIST(GRPCNT,GROUP)
	      NXTSTN=.TRUE.
	      GOTO JUMP_LABEL
            ENDIF
	  ENDIF
	ENDIF        
C
	IF(BY_RANGE) THEN
	  IF(FIRST_STATION.LT.LAST_STATION) THEN
	    FIRST_STATION = FIRST_STATION + 1
	    NXTSTN=.TRUE.
	    GOTO JUMP_LABEL
	  ENDIF
	ENDIF        
C
C
C       EVENTUALLY YOU GET HERE FOR DOWNLOADS
C
	GOTO 10
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C      STOP A PROCESS 
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
9000    CONTINUE
C
	CALL INPNUM('Enter process no ',PROCESS,1,X2X_RELAY_APPS,ST)
	IF (ST.LT.0) GOTO 10
C
	MAX_GROUPS = X2X_NUM_GROUPS
	IF (X2XR_APP_ATRIBUTE(PROCESS).EQ.X2XR_APPA_CHAIN) THEN
	   CALL INPNUM('Enter group (-1 ALL)',GROUP,-1,MAX_GROUPS,ST)
	   IF (ST.LT.0) GOTO 10
	   IF (GROUP.LT.0) THEN
	      STN=-1
	      GOTO 9010
	   ELSEIF (GROUP.EQ.0) THEN
	      TYPE *,' Not a valid group'
	      GOTO 10
	   ENDIF
	   STN=X2XR_SEND_STATION(GROUP,PROCESS)
	   IF (STN.LE.0) THEN
	      TYPE *,' Not active group '
	      GOTO 10
	   ENDIF
	ELSE
         CALL INPNUM('Enter station (-1 all) ',STN,-1,X2X_STATIONS,ST)
	   IF (ST.LT.0) GOTO 10
	   IF (STN.EQ.0) THEN
	      TYPE *,'not a valid station '
	      GOTO 10
	   ENDIF
	ENDIF
C
C SEND MESSAGE TO X2XREL TO INFORM HIM TO STOP THE PROCESS.
C
9010	CONTINUE
    	IF (PROBUF.LE.0) THEN
    	   CALL GETBUF(PROBUF)
    	   IF (PROBUF.LE.0) THEN
    	      TYPE *,'could not get a buffer ',PROBUF
    	      CALL XWAIT(3,2,ST)
    	      GOTO 10
    	   ENDIF
    	ENDIF
	CALL X2RSTOP(PRO(1,PROBUF),PROCESS,STN)
	CALL X2RADDBF(PROBUF)
	PROBUF=0
	GOTO 10
	END
C
C       ASK USER FOR VERIFICATION  
C
        LOGICAL FUNCTION VERIFY(DUM)
C
        INTEGER*4 ANS,DUM
C
	CALL WIMG(6,' Is this option correct (Y/N) ')
	CALL YESNO(ANS)
	IF(ANS.EQ.1) THEN
	   VERIFY = .TRUE.
	ELSE           
           VERIFY = .FALSE.
        ENDIF
C
        RETURN
        END          
C
C DISPLAY_LOAD_REQUEST
C
C
        SUBROUTINE DISPLAY_LOAD_REQUEST(SUBNETWORK, RESET_TYPE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:MSGCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:X2XREL.DEF'
C

C
        INTEGER*4 SUBNETWORK
        INTEGER*4 RESET_TYPE, APP_ROMREV
        INTEGER*4 CLASS, ROMREV_IDX, PROCESS
        INTEGER*4 FB_LOAD, APP, LOAD, FLAG
        INTEGER*4 LOAD_CNT(3,2,X2XC_CLASSES)
C
        LOGICAL APP_MODE_MATCH, LOADS         
C
	DO CLASS=1,X2XC_CLASSES
	   CALL FASTSET(0,LOAD_CNT(1,1,CLASS),6)
           IF(SUBNETWORK.EQ.X2XC_SUBNETWORK(CLASS))THEN
C
             DO PROCESS = 1, X2XR_FIRST_COMMON_PROCESS-1
               IF((X2XR_APP_STATUS(PROCESS).NE.0).AND.
     *            (X2XR_SUBNETWORK(PROCESS).EQ.SUBNETWORK)) THEN
                  GOTO 215
               ENDIF
             ENDDO
             RETURN
C
215          CONTINUE
	     DO 220 ROMREV_IDX = 1,3
	        IF (ROMREV_IDX.EQ.1) THEN
	           APP_ROMREV = X2XC_ROMREV1(CLASS)
		ELSE IF (ROMREV_IDX.EQ.2) THEN
		   APP_ROMREV = X2XC_ROMREV2(CLASS)
		ELSE IF (ROMREV_IDX.EQ.3) THEN
		   APP_ROMREV = X2XC_ROMREV3(CLASS)
		ENDIF
C
		DO 217 FB_LOAD = FOREGROUND_LOAD,BACKGROUND_LOAD
		  CALL X2GETAPPNO(APP_ROMREV,APP,FB_LOAD)
		  IF (APP.LE.0) GOTO 217
C
C
C
		  DO LOAD = 1,MAXLOADS
		     IF (X2XR_APP_TO_SEND(PROCESS).EQ.X2XRF_BOTH .OR. 
     *		         (SMFDLTAB(LOAD,FOREGROUND_FLAG,APP).EQ.
     *		         FOREGROUND_LOAD .AND.
     *			 X2XR_APP_TO_SEND(PROCESS).EQ.X2XRF_FOREGROUND) .OR.
     *		         (SMFDLTAB(LOAD,FOREGROUND_FLAG,APP).EQ.
     *		         BACKGROUND_LOAD.AND.
     *			 X2XR_APP_TO_SEND(PROCESS).EQ.X2XRF_BACKGROUND)) THEN
			   APP_MODE_MATCH = .TRUE.
		     ELSE
			   APP_MODE_MATCH = .FALSE.
		     ENDIF
C
	             IF (SMFDLNAM(1,LOAD,APP) .NE. '    ' .AND.
     *	                 SMFDLNAM(1,LOAD,APP) .NE. 0 .AND.
     *			 APP_MODE_MATCH) THEN
			 FLAG = SMFDLTAB(LOAD,FOREGROUND_FLAG,APP)
			 IF (RESET_TYPE.EQ.1.OR.RESET_TYPE.EQ.2) THEN
		           IF (SMFDLTAB(LOAD,SNDFLG,APP) .EQ. 1) THEN
     			       LOAD_CNT(ROMREV_IDX,FLAG,CLASS) = 
     *			       LOAD_CNT(ROMREV_IDX,FLAG,CLASS) + 1
			   ENDIF
			 ELSE 
     			   LOAD_CNT(ROMREV_IDX,FLAG,CLASS) = 
     *			     LOAD_CNT(ROMREV_IDX,FLAG,CLASS) + 1
			 ENDIF
		     ENDIF
		  END DO	! END LOAD LOOP
217	         CONTINUE	! END FOREGROUND-BACKGROUND LOOP
220		CONTINUE	! END ROMREV LOOP
	    ENDIF
	  END DO
C
C
C
          LOADS = .FALSE.
	  TYPE *,'FOLLOWING LOADS WILL BE LOADED: '
	  WRITE (6,9400) 'Class','Rom Rev','Foreground', 'Background',
     *			 'Total'
9400	  FORMAT(T5,A,T13,A,T25,A,T40,A,T55,A)
	  DO CLASS=1,X2XC_CLASSES
	     DO ROMREV_IDX=1,3
	       IF (LOAD_CNT(ROMREV_IDX,1,CLASS).NE.0.OR.
     *		   LOAD_CNT(ROMREV_IDX,2,CLASS).NE.0) THEN
	          IF (ROMREV_IDX.EQ.1) THEN
	            APP_ROMREV = X2XC_ROMREV1(CLASS)
		  ELSE IF (ROMREV_IDX.EQ.2) THEN
		    APP_ROMREV = X2XC_ROMREV2(CLASS)
		  ELSE IF (ROMREV_IDX.EQ.3) THEN
		    APP_ROMREV = X2XC_ROMREV3(CLASS)
		  ENDIF
	 	  WRITE (6,9500) CLASS,APP_ROMREV,
     *				 LOAD_CNT(ROMREV_IDX,1,CLASS),
     *				 LOAD_CNT(ROMREV_IDX,2,CLASS),
     *				 LOAD_CNT(ROMREV_IDX,1,CLASS)+
     *				 LOAD_CNT(ROMREV_IDX,2,CLASS)
9500		  FORMAT(T5,I2,T13,A4,T25,I10,T40,I10,T55,I5)
                  LOADS = .TRUE.
	       ENDIF
	     END DO
	  END DO
          IF(.NOT.LOADS) THEN
            WRITE (6,9500) 0,0,0,0,0
            WRITE (6,9501)
9501        FORMAT(' There are no loads to send for this option',/
     *            ,' Run Mesmnt, select DLL to review loads ')
          ENDIF
C
          RETURN
          END
