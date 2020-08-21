C
C SUBROUTINE X2RDELAK
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RDELAK.FOV                                 $
C  $Date::   17 Apr 1996 16:29:36                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2xrel.for **
C
C X2DELACK.FOR
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C     SUBROUTINE:
C        X2DELACK(BUFFER,STATION,PROCESS,STATUS)
C
C     PURPOSE:
C        ACKNOWLEDGE AND PROCESS DELIVERY ACKS
C
C     INPUT:
C       BUFFER       -     BUFFER MESSAGE
C       STATION      -     STATION NUMBER
C       PROCESS      -     PROCESS NUMBER
C                    
C     OUTPUT:
C       STATUS       -     -1   
C                           0
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
	SUBROUTINE X2RDELAK(BUFFER,STATION,PROCESS,STATUS)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2FEMES.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
C 
	INTEGER*2 BUFFER(*)
	INTEGER*4 I, RETRY_OFF, EB_FLAG, STATUS, PROCESS, STATION
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) THEN
      	   TYPE *,'X2RDELAK -- STATION: ',STATION
           TYPE *,' PROCESS: ',PROCESS
           TYPE 899,(BUFFER(I),I=1,10)
899        FORMAT(' BUFFER: ',10(1X,Z4))
        ENDIF
C
C       EXTRACT THE PROCESS NUMBER FROM THE ACKNOWLEDGEMENT
C       AND TEST FOR END OF BROADCAST FLAG
C 
	CALL ILBYTE(PROCESS,BUFFER,X2FEMES_HOST_ID-1)
        EB_FLAG=IAND(PROCESS,X2FEMES_HOST_ID_EB_FLAG)
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) 
     *      TYPE *,' PROCESS: ',PROCESS
C
	PROCESS=IAND(PROCESS,X2FEMES_HOST_ID_PROCESS_MASK)
C 
	IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) THEN
           TYPE *,' PROCESS: ',PROCESS
           TYPE *,' EB_FLAG: ',EB_FLAG
           TYPE *,' X2XR_APP_STATUS: ',X2XR_APP_STATUS(PROCESS)
           TYPE *,' X2XR_APP_ATRIBUTE: ',X2XR_APP_ATRIBUTE(PROCESS)
        ENDIF
C
C       PROCESS MUST BE IN VALID RANGE
C
	IF(PROCESS.EQ.0 .OR. PROCESS.GT.X2X_RELAY_APPS) THEN
          STATUS = -1 
          RETURN
        ENDIF
C 
C       PROCESS HAS ALREADY BEEN STOPPED 
C
	IF(X2XR_APP_STATUS(PROCESS).NE.X2XR_APPS_ACTIVE) THEN
          STATUS = -1
          RETURN
        ENDIF
C
C       FOR CHAINED RELAYS WE ONLY ????????
C
	IF(X2XR_APP_ATRIBUTE(PROCESS).EQ.X2XR_APPA_CHAIN) THEN
     	  IF(EB_FLAG.EQ.0) THEN
             STATUS = -1  
             RETURN
          ENDIF
        ELSE
C 
C          UPDATE LAST ACTIVE STATISTICS
C 
	   X2XS_TIME(STATION) = X2X_SYSTIM
	   X2XS_CNT_ACTIVE(STATION) = X2XS_CNT_ACTIVE(STATION)+1
           X2XS_CNT_ACTIVE_LAST(STATION,X2XS_STATS_PNT) =
     *          X2XS_CNT_ACTIVE_LAST(STATION,X2XS_STATS_PNT)+1
	ENDIF
C 
C       IF PROCESS HAD EB FLAG SET, TRY TO TERMINATE PROCESS FOR
C       THIS STATION,  IF NOT TRY TO SEND NEXT MESSAGE
C 
	IF (EB_FLAG.NE.0) THEN
	   CALL X2RHALT(STATION,PROCESS) !STOP THIS STATION
	ELSE
C 
C         IF DEL ACK AND HAS TO WAIT BEFORE SENDING
C         GENERATE WAIT WITH THE TIMEOUT MECHANISM
C 
C          DO NOT INCREASE GLOBAL TIMOUT COUNT
C 
	   RETRY_OFF=X2X_STATIONS*(PROCESS-1)+STATION
	   CALL ISBYTE(X2XR_MAX_RETRY(PROCESS),IX2XR_RETRY_CNT,
     *	               RETRY_OFF-1)
C
C          BROADCAST DELAY IS ONLY SET IN X2XRAPP
C          AND IS BASED ON EITHER RESET, SEGMENT, OR LOAD DELAY
C
	   IF(X2XR_BRO_DELAY(STATION,PROCESS).NE.0) THEN
C 
C             SET NEW STATION_ID
C             SET NEW MAX OF RETRIES
C             SET A TIMOUT TO BE X2X_LOOP_TIME+DELIVERY_DELAY
C             EXIT WITH ERROR
C 
	      X2XR_LAST_STATION_ID(1,STATION,PROCESS)=
     *	          X2XR_STATION_ID(1,STATION,PROCESS)
	      X2XR_LAST_STATION_ID(2,STATION,PROCESS)=
     *	          X2XR_STATION_ID(2,STATION,PROCESS)
	      RETRY_OFF = X2X_STATIONS*(PROCESS-1)+STATION
	      CALL ISBYTE(X2XR_MAX_RETRY(PROCESS)+1,IX2XR_RETRY_CNT,
     *	               RETRY_OFF-1)
	      X2XR_STATION_TIMOUT(STATION,PROCESS) = X2X_LOOP_TIME+
     *	                   X2XR_BRO_DELAY(STATION,PROCESS)
	      X2XR_TIMOUT_CNT(PROCESS) = X2XR_TIMOUT_CNT(PROCESS)-1
	      STATUS = -1
	      RETURN
	   ENDIF
C
C          BUILD STATION MESSAGE 
C
	   CALL X2RSTNMS(BUFFER,STATION,PROCESS)
	   X2XR_STATION_TIMOUT(STATION,PROCESS) = X2X_LOOP_TIME+
     *	                                   X2XR_SEND_TIMOUT(PROCESS)
	   X2XR_ACTIVITY_CNT(PROCESS)=X2XR_ACTIVITY_CNT(PROCESS)+1
	   STATUS = 0
           RETURN
	ENDIF
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) THEN
      	   TYPE *,'EXIT X2RDELAK -- STATION: ',STATION
           TYPE *,' PROCESS: ',PROCESS
           TYPE 899,(BUFFER(I),I=1,10)
        ENDIF
C
        STATUS = -1
	RETURN
	END
