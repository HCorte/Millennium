C
C SUBROUTINE X2RRELTM
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RRELTM.FOV                                 $
C  $Date::   30 May 1996  9:28:30                                         $
C  $Revision::   1.2                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2xrel.for **
C
C V07 21-May-96 wsm Use x2e_capacity instead of x2xe_max_capacity.
C V06 07-Feb-96 DAS bug fix by WS for California
C V05 15-sep-95 DAS changes for background load
C V04 12-DEC-94 DAS Integrate UK changes into X2X Baseline
C V03 19-JUL-94 WS MULTINETWORK CHANGES
C V02  8-MAR-94 JWE Add Finland changes...
C 
C X2RRELTM.FOR
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C     X2RRELTM(ST)             ;QUEUE TRANSACTIONS BY TIMEOUT
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C     SUBROUTINE:
C        X2RRELTM(STATUS)
C
C     PURPOSE:
C        STARTS A STATION BRAODCAST/RELAY
C        BUILDS MESSAGE AND SENDS TO RELAY APPLICATION INPUT QUEUE
C
C     INPUT: NONE
C
C     OUTPUT:
C       STATUS   -     0   TIMED OUT         
C                     -1   NOTHIMG TIMED OUT
C
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
	SUBROUTINE X2RRELTM(STATUS)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
	INCLUDE 'INCLIB:X2RCMD.DEF'
C 
	INTEGER*4 CHECK_NEW(X2X_RELAY_APPS) !CHECK NEW PROCESS TO START
	INTEGER*4 CHECK_OLD(X2X_RELAY_APPS) !CHECK OLD PROCESS TO CONT
	INTEGER*4 ST, STATE, NEXT_OFFSET, NEXT_STATION, NEXT, RETRIES
	INTEGER*4 RETRY_OFF, STATION, GROUP, PROCESS	!V03
	INTEGER*4 SAP, CHECK_OLD_SET, CHECK_NEW_SET	!V03
	INTEGER*4 STATUS, START_CNT/0/, DEST
	INTEGER*4 CONN_TYPE
	INTEGER*4 DSAP
	INTEGER*4 MAX_SUBNETWORKS			!V03
	PARAMETER (MAX_SUBNETWORKS=255)			!V03
	INTEGER*4 TOTAL_CAPACITY(0:MAX_SUBNETWORKS)	!V03
	INTEGER*4 MAX_ACTIVE(0:MAX_SUBNETWORKS)		!V03
	INTEGER*4 CLASS					!V03
C
        COMMON    START_CNT
C
	LOGICAL CHAIN            !.true. if chain type process
C 
	CHAIN = .FALSE.
	STATUS=-1
	CALL FASTSET(0,CHECK_NEW,X2X_RELAY_APPS)
	CALL FASTSET(0,CHECK_OLD,X2X_RELAY_APPS)
	CHECK_NEW_SET=0
	CHECK_OLD_SET=0
C 
C       FIND TOTAL AVAILABLE CAPACITY
C       NOTE THAT CAPACITY IS DETERMINE BY A FRONT END'S MAINTAINENCE
C       MESSAGE. ASYNC STATIONS HAVE A CAPACITITY BASED ON THE PORT
C       STATUS.
C 
C **** CHANGE HERE FOR MULTI NETWORK ****
C
	CALL FASTSET(0,TOTAL_CAPACITY,MAX_SUBNETWORKS+1)	
	DO 5 SAP=X2X_ACTUAL_SAP,X2X_SAP				
c-	     TOTAL_CAPACITY(X2XE_SUBNETWORK(SAP))=X2XE_MAX_CAPACITY(SAP) + 
	     TOTAL_CAPACITY(X2XE_SUBNETWORK(SAP))=X2XE_CAPACITY(SAP) + 
     *			  TOTAL_CAPACITY(X2XE_SUBNETWORK(SAP))	
5	CONTINUE
C 
C 
C       CHECK IF ANYTHING TO DO
C 
	DO 10 PROCESS = 1,X2X_RELAY_APPS
C 
C          SKIP IF PROCESS IS NOT ACTIVE OR # OF PROCESSES EXCEEDED MAX
C 
	   IF (X2XR_APP_STATUS(PROCESS).NE.X2XR_APPS_ACTIVE) GOTO 10
C 
	   MAX_ACTIVE(X2XR_SUBNETWORK(PROCESS))=		!V03
     *		  (TOTAL_CAPACITY(X2XR_SUBNETWORK(PROCESS)) * 	!V03
     *			  X2XR_MAX_ACTIVE(PROCESS))/100+1	!V03
C 
C          CHECK TO SEE IF WE HAVE TO ACTIVATE A NEW REQUEST FOR THIS
C          PROCESS.
C
	   IF (X2XR_NO_REQUEST(PROCESS).NE.0 .AND.
     *	       X2XR_NO_ACTIVE(PROCESS).LT.			!V03
     *		      MAX_ACTIVE(X2XR_SUBNETWORK(PROCESS)) )  THEN	!V03
	          CHECK_NEW(PROCESS)=-1
	          CHECK_NEW_SET=-1
	   ENDIF
C 
C          CHECK IF ANY TIMEOUT, ETC FOR A CURRENTLY ACTIVE PROCESS  
C
	   IF (X2XR_NO_ACTIVE(PROCESS).GT.0 .AND.
     *	       X2XR_APP_STATUS(PROCESS).EQ.X2XR_APPS_ACTIVE) THEN
	          CHECK_OLD(PROCESS)=-1
	          CHECK_OLD_SET=-1
	   ENDIF
10	CONTINUE
C 
C       EXIT IF NOTHING TO DO
C 
	IF(CHECK_NEW_SET.EQ.0 .AND. CHECK_OLD_SET.EQ.0) THEN
          START_CNT = 0
          RETURN
        ENDIF
C 
C=====================================================================
C     PROCESS TIMED OUT MESSAGES
C=====================================================================
C 
	IF (CHECK_OLD_SET.NE.0) THEN
C 
	   DO 100 PROCESS = 1,X2X_RELAY_APPS
C 
C             CHECK BY PROCESS 
C
	      IF (CHECK_OLD(PROCESS).NE.0) THEN
	         IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *             TYPE *,'CHECKING FOR TIMEOUTS FOR PROCESS ',PROCESS
                
	         IF(X2XR_APP_ATRIBUTE(PROCESS).EQ.X2XR_APPA_CHAIN) 
     *             THEN
		     CHAIN = .TRUE. 
		   ELSE
		     CHAIN = .FALSE.
		   ENDIF
C 
C=====================================================================
C                LOOP THROUGH GROUPS IF CHAINED 
C=====================================================================
C
	         IF (CHAIN) THEN    
	            DO 70 GROUP=1,X2X_NUM_GROUPS
	               IF(X2XR_GROUP_ACTIVE(GROUP,PROCESS).EQ.0)
     *                    GOTO 70
	               STATION=X2XR_SEND_STATION(GROUP,PROCESS)
	               IF (STATION.GT.0) THEN
			  CLASS=X2XS_STNCLS(STATION)			!V03
			  IF (X2XC_SUBNETWORK(CLASS).NE.		!V03
     *			      X2XR_SUBNETWORK(PROCESS)) GOTO 100	!V03
	                  IF(X2XR_STATION_TIMOUT(STATION,PROCESS).NE.0
     *	                   .AND. X2XR_STATION_TIMOUT(STATION,PROCESS)
     *	                         .LT.X2X_LOOP_TIME) THEN
C 
C                         CHECK IF RETRIES EXCEEDED
C 
	                  RETRY_OFF=X2X_STATIONS*(PROCESS-1)+STATION
	                  CALL ILBYTE(RETRIES,IX2XR_RETRY_CNT,
     *                                RETRY_OFF-1)
	                  X2XR_GROUP_TIMOUT_CNT(GROUP,PROCESS)=
     *	                      X2XR_GROUP_TIMOUT_CNT(GROUP,PROCESS)+1
C 
C                         IF SO, CLEAR THIS STATION AND TRY TO
C                         GET NEXT STATION
C
	                  IF (RETRIES.LE.0) THEN
	                     X2XR_STATION_TIMOUT(STATION,PROCESS)=0
	                     CALL BCLR(X2XR_STATION_ACTIVE(STATION),
     *	                                   PROCESS-1)
			     CALL X2RPOLLCHK(STATION,PROCESS)
C 
C                            FIND NEXT STATION TO PROCESS
C 
	                     DO 30, NEXT=1,X2X_LIST_LEN
	                        NEXT_STATION=X2XG_LIST(NEXT,GROUP)
	                        NEXT_OFFSET=NEXT+1
	                        IF (NEXT_STATION.EQ.STATION) GOTO 40
30	                     CONTINUE
C 
C                            DATA BASE SCREW UP, EXIT WITH ERROR
C 
	                     STATUS = -1
	                     RETURN
C 
40	                     CONTINUE
C
	                     DO 50, NEXT=NEXT_OFFSET,X2X_LIST_LEN
	                        NEXT_STATION=X2XG_LIST(NEXT,GROUP)
	                        IF (NEXT_STATION.NE.0) THEN
C 
C                                  START THIS PROCESS AS NEW PROCESS
C 
	                           CALL ILBYTE(STATE,IX2XS_STATE,
     *	                                         NEXT_STATION-1)
	                           IF (STATE.NE.X2XS_IDLE .AND.
     *	                               STATE.NE.X2XS_INIT .AND.
     *	                               STATE.NE.X2XS_DISABLED) GOTO 50
C
                                    IF(X2X_REL_PRI.GT.0) THEN
                                    IF(STATE.EQ.X2XS_DISABLED) GOTO 50
                                    ENDIF
C
	                           X2XR_SEND_STATION(GROUP,PROCESS)=
     *	                                         NEXT_STATION
	                           X2XR_STATION_ID(1,NEXT_STATION,
     *	                           PROCESS)=X2XR_LAST_STATION_ID(1,
     *	                                      STATION,PROCESS)
	                           X2XR_STATION_ID(2,NEXT_STATION,
     *	                           PROCESS)=X2XR_LAST_STATION_ID(2,
     *	                                      STATION,PROCESS)
	                           STATION=NEXT_STATION
	                           CALL BSET(X2XR_STATION_REQUEST
     *	                             (STATION),PROCESS-1)
	                           CALL BCLR(X2XR_STATION_ACTIVE
     *	                             (STATION),PROCESS-1)
	                           CHECK_NEW(PROCESS)=-1
	                           CHECK_NEW_SET=-1
	                           X2XR_NO_REQUEST(PROCESS)=
     *	                             X2XR_NO_REQUEST(PROCESS)+1
	                           X2XR_GROUP_REQUEST(GROUP,PROCESS)=-1
	                           X2XR_NO_ACTIVE(PROCESS)=
     *	                             X2XR_NO_ACTIVE(PROCESS)-1
	                           GOTO 70
	                        ENDIF
50	                     CONTINUE
C 
	                     X2XR_SEND_STATION(GROUP,PROCESS)=-1
	                     X2XR_NO_ACTIVE(PROCESS)=X2XR_NO_ACTIVE
     *	                                         (PROCESS)-1
	                     X2XR_GROUP_ACTIVE(GROUP,PROCESS)=
     *	                            X2XR_GROUP_ACTIVE(GROUP,PROCESS)-1
	                     IF (X2XR_NO_ACTIVE(PROCESS).LE.0 .AND.
     *	                         X2XR_NO_REQUEST(PROCESS).LE.0)
     *	                         CALL X2RHALT(X2RDEST_ALL,PROCESS)
	                     GOTO 70
	                  ENDIF
C 
C                         PROCESS TIMEOUT FOR THIS STATION
C 
	                  X2XR_STATION_ID(1,STATION,PROCESS)=
     *	                   X2XR_LAST_STATION_ID(1,STATION,PROCESS)
	                  X2XR_STATION_ID(2,STATION,PROCESS)=
     *	                   X2XR_LAST_STATION_ID(2,STATION,PROCESS)
	                  CALL X2RSTNTM(STATION,PROCESS,ST)
	                  IF (ST.EQ.0) THEN
	                     X2XR_TIMOUT_CNT(PROCESS)=
     *	                          X2XR_TIMOUT_CNT(PROCESS)+1
	                     RETRIES=RETRIES-1
	                     CALL ISBYTE(RETRIES,IX2XR_RETRY_CNT,
     *	                                      RETRY_OFF-1)
	                     STATUS=0
	                  ENDIF
	                  ENDIF
	               ENDIF
70	            CONTINUE
C 
C=====================================================================
C                ALL OTHER NON-CHAINED APPLICATIONS
C=====================================================================
	         ELSE
C
	            DO 90 STATION = 1,X2X_STATIONS

	               IF(TSBIT(X2XR_STATION_ACTIVE(STATION),PROCESS-1)
     *	               .AND. X2XR_STATION_TIMOUT(STATION,PROCESS).NE.0
     *	               .AND. X2XR_STATION_TIMOUT(STATION,PROCESS).LT.
     *	                     X2X_LOOP_TIME) THEN
C 
		         IF(IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *		           TYPE *,'CHECKING TIMEOUT STN ',STATION
C 
C                        CHECK IF RETRIES EXCEEDED
C 
	                 RETRY_OFF=X2X_STATIONS*(PROCESS-1)+STATION
	                 CALL ILBYTE(RETRIES,IX2XR_RETRY_CNT,
     *                               RETRY_OFF-1)
	                 GROUP=X2XS_GROUP(STATION)
	                 IF(GROUP.NE.0)
     *	                     X2XR_GROUP_TIMOUT_CNT(GROUP,PROCESS)=
     *	                      X2XR_GROUP_TIMOUT_CNT(GROUP,PROCESS)+1
C
C                        IF SO, CLEAR THIS STATION
C                        AND TRY TO GET NEXT STATION
C
                         IF (RETRIES.LE.0) THEN
	                     X2XR_STATION_TIMOUT(STATION,PROCESS)=0
	                     CALL BCLR(X2XR_STATION_ACTIVE(STATION),
     *	                                   PROCESS-1)
			     CALL X2RPOLLCHK(STATION,PROCESS)
	                     X2XR_NO_ACTIVE(PROCESS) = 
     *                            X2XR_NO_ACTIVE(PROCESS) - 1
	                     IF (X2XR_NO_ACTIVE(PROCESS).EQ.0 .AND.
     *	                         X2XR_NO_REQUEST(PROCESS).EQ.0)
     *	                         CALL X2RHALT(X2RDEST_ALL,PROCESS)
	                     GOTO 90
	                  ENDIF
C 
C                         PROCESS TIMEOUT FOR THIS STATION
C 
	                  GROUP = -1
C                         X2XR_STATION_ID(1,STATION,PROCESS)=
C    *                     X2XR_LAST_STATION_ID(1,STATION,PROCESS)
C                         X2XR_STATION_ID(2,STATION,PROCESS)=
C    *                     X2XR_LAST_STATION_ID(2,STATION,PROCESS)
	                  CALL X2RSTNTM(STATION,PROCESS,ST)
	                  IF (ST.EQ.0) THEN
                             X2XR_STATION_ID(1,STATION,PROCESS)=
     *                         X2XR_LAST_STATION_ID(1,STATION,PROCESS)
                             X2XR_STATION_ID(2,STATION,PROCESS)=
     *                         X2XR_LAST_STATION_ID(2,STATION,PROCESS)
	                     X2XR_TIMOUT_CNT(PROCESS)=
     *	                          X2XR_TIMOUT_CNT(PROCESS)+1
	                     RETRIES=RETRIES-1
	                     CALL ISBYTE(RETRIES,IX2XR_RETRY_CNT,
     *	                                      RETRY_OFF-1)
	                     STATUS=0
	                  ENDIF
	               ENDIF
90	            CONTINUE
	         ENDIF
	      ENDIF
100	   CONTINUE
	ENDIF
C 
C=====================================================================
C     CHECK IF IT HAS TO START NEW PROCESSES
C=====================================================================
C 
	IF (CHECK_NEW_SET.NE.0) THEN   !IF NEW MESSAGES TO KICK
C 
	   DO 200 PROCESS=1,X2X_RELAY_APPS
C
              MAX_ACTIVE(X2XR_SUBNETWORK(PROCESS)) =  		!V03
     *		   (TOTAL_CAPACITY(X2XR_SUBNETWORK(PROCESS)) 	!V03
     *             *X2XR_MAX_ACTIVE(PROCESS))/100+1		!V03
C 
	      IF (CHECK_NEW(PROCESS).NE.0) THEN !IF OUSTANDING 'START'
	         IF(X2XR_APP_ATRIBUTE(PROCESS).EQ.X2XR_APPA_CHAIN)
     *             THEN
		     CHAIN=.TRUE.
		 ELSE
		     CHAIN=.FALSE.
		 ENDIF
C
                 IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *	             TYPE *,'CHAIN: ',CHAIN,' PROCESS: ',PROCESS	!V03
C
C=====================================================================
C                LOOP THROUGH GROUPS FOR CHAINED RELAY
C=====================================================================
C 
	         IF (CHAIN) THEN 
C
	            DO 130, GROUP=1,X2X_NUM_GROUPS
                       IF(X2XR_GROUP_REQUEST(GROUP,PROCESS).EQ.0)
     *                             GOTO 130    !CHECK IF GROUP REQ.
                       STATION=X2XR_SEND_STATION(GROUP,PROCESS)
C  
		       IF(STATION.GE.1.AND.STATION.LE.X2X_STATIONS)THEN
		          CONN_TYPE = BX2XS_CONN_TYPE(STATION)
		       ELSE 
			  CONN_TYPE = 0
		       ENDIF
C 
C                     IF ASYNC PVC CONNECTION THEN DON'T WORRY
C                     ABOUT CAPACITY. JUST MAKE SURE DSAP IS ACTIVE
C
C **** CHANGE HERE FOR MULTI NETWORK ****
C
                       IF(CONN_TYPE.EQ.X2XSCT_ASYPVC .OR.
     *                    CONN_TYPE.EQ.X2XSCT_USAT_PVC) THEN
			  DSAP = ZEXT(BX2XS_SAP(STATION))		!V03
		          IF(DSAP.LT.1.OR.DSAP.GT.X2X_SAP) GOTO 130	!V03
			  IF(X2XE_ACT_STATUS(DSAP).NE.X2XES_ONLINE) 	!V03
     *                                                     GOTO 130	!V03
	               ELSE
                          IF(X2XR_NO_ACTIVE(PROCESS).GE.		!V03
     *			    MAX_ACTIVE(X2XR_SUBNETWORK(PROCESS)) )	!V03
     *                                                     GOTO 200
		       ENDIF
C
	               IF(X2XR_GROUP_REQUEST(GROUP,PROCESS).EQ.0)
     *	                           GOTO 130    !CHECK IF GROUP REQ.

C 
C                      PROCESS START FOR THIS STATION
C 
	               IF (STATION.GT.0) THEN
C	START OF V03 CHANGE BLOCK
			  CLASS=X2XS_STNCLS(STATION)
			  IF (CLASS.LE.0) GOTO 130
			  IF (X2XC_SUBNETWORK(CLASS).NE.
     *			      X2XR_SUBNETWORK(PROCESS)) GOTO 130 !WRONG SUBNET
C	END OF V03 CHANGE BLOCK
	                  IF (TSBIT(X2XR_STATION_REQUEST(STATION),
     *	                            PROCESS-1)) THEN
                          CALL X2RSTNST(STATION,CHAIN,GROUP,PROCESS,ST)
	                  IF (ST.EQ.0) STATUS=0
	                  ENDIF
	               ENDIF
130	            CONTINUE
C 
C=====================================================================
C               ALL NON-CHAINED PROCESSES
C=====================================================================
C
	         ELSE  
C 
	            DO 150 STATION=1,X2X_STATIONS
C	START OF V03 CHANGE BLOCK
		       CLASS=X2XS_STNCLS(STATION)
		       IF (CLASS.LE.0) GOTO 150
		       IF (X2XC_SUBNETWORK(CLASS).NE.
     *			  X2XR_SUBNETWORK(PROCESS))  GOTO 150
C	END OF V03 CHANGE BLOCK
		       CONN_TYPE = BX2XS_CONN_TYPE(STATION)
C 
C                     IF ASYNC PVC CONNECTION THEN DON'T WORRY
C                     ABOUT CAPACITY. JUST MAKE SURE DSAP IS ACTIVE
C
C **** CHANGE HERE FOR MULTI NETWORK ****
C
                       IF(CONN_TYPE.EQ.X2XSCT_ASYPVC .OR.
     *                    CONN_TYPE.EQ.X2XSCT_USAT_PVC) THEN
			  DSAP = ZEXT(BX2XS_SAP(STATION))	!V03
C Since usat stations have a high statistics timer. We have to force lookup
C of the dsap from the database because stats are not in as yet from the
C the satellite stations. ( Same as in x2rcmd.for )
                          IF (DSAP.EQ.0. .AND.
     *                        CONN_TYPE .EQ. X2XSCT_USAT_PVC ) THEN
                             DSAP = X2XPL_SAP(X2XS_PHYS(STATION))
                          END IF
		          IF(DSAP.LT.1.OR.DSAP.GT.X2X_SAP) GOTO 150
			  IF(X2XE_ACT_STATUS(DSAP).NE.X2XES_ONLINE) 
     *                                                    GOTO 150
	               ELSE
                          IF(X2XR_NO_ACTIVE(PROCESS).GE.		!V03
     *                          MAX_ACTIVE(X2XR_SUBNETWORK(PROCESS)))	!V03
     *                                                    GOTO 200
		       ENDIF
C 
C                         PROCESS START FOR THIS STATION
C
	               IF (TSBIT(X2XR_STATION_REQUEST(STATION),
     *	                      PROCESS-1)) THEN
	                  GROUP=-1
	                  CALL X2RSTNST(STATION,CHAIN,GROUP,PROCESS,ST)
	                  IF (ST.EQ.0) THEN
                             STATUS = 0
                             START_CNT=START_CNT+1
                          ENDIF
	               ENDIF
150	            CONTINUE
C
C                   IF NO STATIONS HAVE BEEN STARTED
C                   (NO STATIONS ARE MARKED AS WAITING)
C                   AND NO STATIONS ARE CURRENTLY ACTIVE
C                   STOP THE ACTIVE PROCESS.
C
                    IF(START_CNT.EQ.0 .AND.
     *                 X2XR_NO_ACTIVE(PROCESS).EQ.0 .AND.    ! v06
     *                 X2XR_NO_REQUEST(PROCESS).EQ.0) THEN   ! v06
                      DEST=-1                   !STOP ALL STATIONS
                      CALL X2RHALT(DEST,PROCESS)
                    ENDIF
	         ENDIF
	      ENDIF
200	   CONTINUE
	ENDIF
 
	RETURN
	END
