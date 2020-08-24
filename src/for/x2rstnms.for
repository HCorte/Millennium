C
C SUBROUTINE X2RSTNMS
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RSTNMS.FOV                                 $
C  $Date::   17 Apr 1996 16:32:32                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2xrel.for **
C
C V02 05-DEC-94 SCD Integrate UK changes into X2X Baseline
C
C X2RSTNMS.FOR 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C     SUBROUTINE:
C        X2RSTNMS(BUFFER,STATION,PROCESS) 
C
C     PURPOSE:
C        PUTS STATION MESSAGE INTO BUFFER 
C 
C        SET BY APPLICATION (NOT THIS ROUTINE):
C          DELIVERY DELAY
C          ACKNOWLEDGEMENT DELAY
C          END OF BROADCAST (EB) FLAG
C          DESTINATION CODE IN FLAG FIELD
C          BROADCAST MESSAGE LENGTH
C          X2XR_STATION_ID FIELDS (FIRST BYTE SET BY X2XREL)
C          HOST_ID FIELD OVERRIDE IN PROCOM BUFFER, HOST ID FIELD
C          SHOULD CORRESPOND TO PROCESS NO, HIGH BIT SHOULD BE SET
C          IF THAT IS THE END OF BROADCAST
C 
C
C     INPUT:
C       STATION  -     STATION TO BE STARTED
C       PROCESS  -     PROCESS NUMBER ASSIGNED
C
C     OUTPUT:
C       BUFFER   -     BUFFER TO BE FILLED IN
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
	SUBROUTINE X2RSTNMS(BUFFER,STATION,PROCESS)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:X2FEMES.DEF'
C 
	INTEGER*4 I, FLAGS, PROCESS, STATION
	INTEGER*2 BUFFER(*)
C 
C       FORMAT RELAY "FORMAT" MESSAGE IN THE BUFFER
C 
	CALL ISBYTE(X2STMES_PROTID_VAL,BUFFER,X2STMES_PROTID-1)
	CALL ISBYTE(X2STMES_DATATYPE_RELAY,BUFFER,X2STMES_DATATYPE-1)
	CALL ISBYTE(0,BUFFER,X2STMES_CONFCHK-1)
	CALL I4TOBUF2(STATION,BUFFER,X2STMES_STATION_NO-1)
C 
	FLAGS=X2XR_APP_FLAGS(PROCESS)+X2XR_APP_DATA_DEST(PROCESS)
	IF (X2XR_APP_DATA_MSGNUM(PROCESS).GT.0 .OR. !IF MESSAGE NR SET
     *	   X2XR_APP_DATA_LEN(PROCESS).NE.0) THEN    !OR ANY DATA TO APPEND
	   FLAGS=IOR(FLAGS,X2STMES_RELAYF_EB)
	   FLAGS=IOR(FLAGS,X2STMES_RELAYF_PE)
	ENDIF
	CALL I4TOBUF2(FLAGS,BUFFER,X2STMES_RELAY_FLAGS-1)
	CALL ISBYTE(X2STMES_RELAY_HEADER_LEN,BUFFER,
     *	               X2STMES_RELAY_MSG_OFF-1)
	CALL I4TOBUF2(X2XR_APP_DATA_LEN(PROCESS),BUFFER,
     *	               X2STMES_RELAY_MSG_LEN-1)
	IF (X2XR_APP_DATA_LEN(PROCESS).NE.0) THEN
	   CALL MOVBYT(X2XR_APP_DATA(1,PROCESS),1,BUFFER,
     *	       X2STMES_RELAY_HEADER_LEN+1,X2XR_APP_DATA_LEN(PROCESS))
	ENDIF
	CALL I4TOBUF4(X2XR_STATION_ID(1,STATION,PROCESS),BUFFER,
     *	                        X2STMES_RELAY_ID-1)
           IF(IAND(X2X_DEBUG,128).NE.0 .AND.				!V02
     *		  X2XR_STATION_ID(2,STATION,PROCESS).EQ.0 .AND. 	!V02
     *		  X2XR_STATION_ID(1,STATION,PROCESS).EQ.0) 		!V02
     *	      TYPE *,' STATION ID1=0 ',PROCESS,STATION			!V02
	CALL ISBYTE(PROCESS,BUFFER,X2STMES_RELAY_ID-1)
	CALL I4TOBUF4(X2XR_STATION_ID(2,STATION,PROCESS),BUFFER,
     *	                         X2STMES_RELAY_ID+3)
C 
C       SAVE LAST STATION ID PASSED TO RELAY APPLICATION
C 
	X2XR_LAST_STATION_ID(1,STATION,PROCESS)=
     *	         X2XR_STATION_ID(1,STATION,PROCESS)
	X2XR_LAST_STATION_ID(2,STATION,PROCESS)=
     *	         X2XR_STATION_ID(2,STATION,PROCESS)
C 
	CALL ISBYTE(0,BUFFER,X2STMES_RELAY_ADR_LEN-1)
	CALL I4TOBUF4(0,BUFFER,X2STMES_RELAY_ADR-1)
	CALL I4TOBUF4(0,BUFFER,X2STMES_RELAY_ADR+3)
	CALL I4TOBUF2(0,BUFFER,X2STMES_RELAY_DELIVERY_DELAY-1)
	CALL I4TOBUF2(0,BUFFER,X2STMES_RELAY_ACK_DELAY-1)
C 
C 
	IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) THEN
           TYPE *,'X2RSTNMS:, STATION   PROCESS '
           TYPE *,'        ', STATION , PROCESS
           TYPE 900,(BUFFER(I),I=1,20)
        ENDIF
C
	RETURN
C
900     FORMAT(1H ,A,/,10(1X,Z4))
	END
