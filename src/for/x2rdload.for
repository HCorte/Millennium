C
C SUBROUTINE X2RDLOAD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RDLOAD.FOV                                 $
C  $Date::   17 May 1996 11:45:18                                         $
C  $Revision::   1.1                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2xrapp.for;1 **
C
C V07 06-FEB-96 DAS Added code for X.21
C V06 15-SEP-95 DAS REWRITE TO HANDLE BACKGROUND LOADS
C V05 29-DEC-94 GPR Modified to hande multiple MCPs per subnetwork
C V04 12-DEC-94 DAS Integrate UK changes into X2X Baseline
C V02  8-MAR-94 JWE Add temporary Finland kludge
C
C
C*++++++++++++++++++++++++++++++++++++++++++++++++++++++
C**
C**    PROCESS DOWNLOAD APPLICATION
C**
C**       X2RDLOAD(PROCESS,STATION,STATION_ID,FLAGS,
C**   *         HOST_ID,MES_NUM,MES_LEN,DELIVERY_OVR,DESTINATION,
C**   *         DELIVERY_DELAY)
C**    IN:
C**    PROCESS        - APPLICATION PROCESS NO
C**    STATION        - STATION #
C**    STATION_ID     - LAST BROADCAST ID
C**    FLAGS          - PROCESS FLAGS
C**    OUT:
C**    HOST_ID        - HOST_ID TO SET IN THE BUFFER
C**    MES_NUM        - MSGCOM MESSAGE NO TO USE
C**    MES_LEN        - MESSAGE LENGTH TO SET IN THE COMMON
C**    DELIVERY_OVR   - DELIVERY OVERRITE FLAG (AS IN PROCOM)
C**    DESTINATION    - DESTINATION FLAG IN PROCOM
C**
C**    STATION_ID IS DEFINED AS FOLLOWS:
C**    INTEGER*4 STATION_ID(2)
C**    HIGH BYTE OF STATION_ID(1) - PROCESS #,
C**    NEXT BYTE OF STATION_ID(1) - TIMES SEG 0 (CONTROL SEGMENT) OF THE LOAD
C**				    WAS SENT
C**    NEXT 2 BYTES OF STATION_ID(1) - NOT USED
C**    HIGH 2 BYTES OF STATION_ID(2) - TIMES TO SEND LOAD/LOAD NUMBER
C**    LOW  2 BYTES OF STATION_ID(2) - SEGMENT NUMBER
C**
C**    LOAD NUMBER 0 USED FOR RESET OF TERMINALS
C**
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
	SUBROUTINE X2RDLOAD(PROCESS,STATION,STATION_ID,FLAGS,
     *	         HOST_ID,MES_NUM,MES_LEN,DELIVERY_OVR,DESTINATION,
     *	         DELIVERY_DELAY)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:MSGCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMDLL.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:X2FEMES.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
        INCLUDE 'INCLIB:X2RLOD.DEF'
C
        INTEGER*4 TIMES_TO_SEND_SEG0
        INTEGER*4 SEG0_DELAY
	INTEGER*4 STATION_ID(2)
        INTEGER*4 LOAD_NO                           !Requested load number
        INTEGER*4 SEGMENT_NO                        !Requested segment #
        INTEGER*4 TIMES_SENT                        !Number of times load sent
        INTEGER*4 NEXT_SEGMENT                      !Next segment to send
        INTEGER*4 DELIVERY_DELAY          
        INTEGER*4 RESET_DELAY                       !Delay between resets
        INTEGER*4 DESTINATION
        INTEGER*4 STATION                           !Station number
        INTEGER*4 PROCESS                           !Process number
        INTEGER*4 HOST_ID                           !Host identifier
        INTEGER*4 MES_NUM                           !Message number
        INTEGER*4 MES_LEN                           !Message length
        INTEGER*4 DELIVERY_OVR
        INTEGER*4 ROMREV_IDX                        !DENOTES THE CURRENT ROMREV
                                                    !NUMBER USED
                                                    !IN THE STATION CLASS
        INTEGER*4 ACT_ROMREV/0/                     !CURRENT ROMREV IN ASCII
        INTEGER*4 STNCLS
        INTEGER*4 FLAGS
	INTEGER*4 I4
	INTEGER*4 TIMES_SEG0_SENT
        INTEGER*4 APPLICATION_NO                    !APPLICATION NUMBER     !V03
        INTEGER*4 FORE_BACKGND                     !DENOTES FOREGROUND     !V03
                                                    !-BACKGROUND LOAD
	INTEGER*2 I2(2)
	EQUIVALENCE (I4,I2)
	BYTE	  I1(4)
	EQUIVALENCE (I4,I1)
C                                                                              
	INTEGER*4   SEGPTR       !Segment pointer into MSGCOM         
	INTEGER*4   SEGMENT_NO_TO_SEND !Segment # to send out.        
	INTEGER*4   NBROFF       !Offset into SMFDLLTAB, NBRSEG field.   
C                                                                              
	LOGICAL     CMP_DLL_FLAG !Compressed DLL flag.                
C
C
C
D       type*, ' flags = ', flags
	CMP_DLL_FLAG = .FALSE.
	IF (P(COMPRESSED_LOAD).EQ.0) CMP_DLL_FLAG = .TRUE.
C
	I4=STATION_ID(2)
C
	LOAD_NO    = I2(2)
	SEGMENT_NO = I2(1)
C
	I4=STATION_ID(1)
	TIMES_SEG0_SENT = I1(3)
        ROMREV_IDX = 0
        CALL ILBYTE(ROMREV_IDX,I1,1)
        FORE_BACKGND = 0              !V03
        CALL ILBYTE(FORE_BACKGND,I1,0)
C
C     ALL BROADCAST/RELAYS ARE INITIATED BY X2BRO. THE CONVENTION IS SUCH
C     THAT ALL SEGMENT NUMBERS LESS THAN OR EQUAL TO ZERO ARE FROM X2BRO.
C     BECAUSE OF THE THE WAY X2BRO SETS UP ITS SEGMENT NUMBER THE VALUE
C     OF LOAD (AND HENCE TIMES_SENT) WILL ALSO BE NEGATIVE. MAKE AN
C     ADJUSTMENT TO COMPENSATE FOR THIS
C
	TIMES_SENT=LOAD_NO/256
	LOAD_NO=MOD(LOAD_NO,256)
C
	IF (SEGMENT_NO .LT. 0) THEN
	  IF(LOAD_NO .LT. 0) THEN
	     LOAD_NO = 0
	     TIMES_SENT = 0
	  ENDIF
	  NEXT_SEGMENT  = 1
	ELSE
	  NEXT_SEGMENT  = SEGMENT_NO+1
          CALL X2NEXT_APP(STATION,FORE_BACKGND,ROMREV_IDX,
     *                    ACT_ROMREV,APPLICATION_NO)
	ENDIF
C
C     INITIALIZE SOME VARIABLES
C
	DELIVERY_DELAY = 0
	RESET_DELAY = 0
	DESTINATION = X2DEST_APP_STATION
	IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XRAPP).NE.0) THEN
	    TYPE *,'STATION ',STATION,' LOAD NO,SEG NO,????????,STN_ID'
	    TYPE 9000,'DLL ',LOAD_NO,SEGMENT_NO,NEXT_SEGMENT,STATION_ID
	ENDIF
C
D       CALL OPS(' ',0,0)
D       CALL OPS(' ',0,0)
D       CALL OPS(' PROCESS: ',PROCESS,0)
D       CALL OPS(' LOAD NUMBER: ',LOAD_NO,0)
D       CALL OPS(' SEG NUMBER:  ',SEGMENT_NO,0)
D       CALL OPS(' ROM INDEX:   ',ROMREV_IDX,0)
D       CALL OPS(' FORE 1/BACK 2:',FORE_BACKGND,0)
D       CALL OPS(' TIMES SENT    ',TIMES_SENT,0)
C
C***************************************************************************
C      -- NEW PROCESS STARTED -- INITIALIZED
C***************************************************************************
C
       IF((LOAD_NO .EQ. 0) .AND. (SEGMENT_NO .LE. 0)) THEN
C
C        NOTE: X2BRO FILLS IN THE MESSAGE NUMBER WHICH IDENTIFIES WHICH
C        RESET NUMBER TO USE - IN NEGATIVE FORMAT.  IF NO MESSAGE NUMBER
C        DEFINED, USE THE DEFAULT VALUE FROM X2XCOM.
C
         IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XRAPP).NE.0) THEN
            TYPE *,' INITIALIZING NEW PROCESS ',SEGMENT_NO
         ENDIF
D        CALL OPS(' INITIALIZING NEW PROCESS ',0,0)
C
         IF(SEGMENT_NO.EQ.0) THEN
            IF (X2XR_DLL_RESET_MESSAGE(PROCESS).LE.0) THEN	!NOT DEFINED
                 X2XR_BRDCST_TYPE(PROCESS)   = X2RLOD_NO_RESET_SEL
            ELSEIF(X2XR_DLL_RESET_MESSAGE(PROCESS).EQ.HRESET) THEN !HARD RESET
                 X2XR_BRDCST_TYPE(PROCESS)   = X2RLOD_HARD_FULL
            ELSEIF(X2XR_DLL_RESET_MESSAGE(PROCESS).EQ.MNEWS) THEN !BAD NO
                 X2XR_BRDCST_TYPE(PROCESS)   = X2RLOD_NO_RESET_SEL
            ELSEIF(X2XR_DLL_RESET_MESSAGE(PROCESS).EQ.URESET) THEN !ULTIMATE
                 X2XR_BRDCST_TYPE(PROCESS)   = X2RLOD_FULL
            ELSEIF(X2XR_DLL_RESET_MESSAGE(PROCESS).EQ.SRESET) THEN !SOFT
                 X2XR_BRDCST_TYPE(PROCESS)   = X2RLOD_SOFT_SEL
            ELSEIF(X2XR_DLL_RESET_MESSAGE(PROCESS).GE.RRESET) THEN !RESTART
                 X2XR_BRDCST_TYPE(PROCESS)   = X2RLOD_NO_RESET_SEL
            ENDIF
         ELSE
            X2XR_BRDCST_TYPE(PROCESS)   =  SEGMENT_NO
         ENDIF
C
D        CALL OPS(' BRDCST TYPE: ',X2XR_BRDCST_TYPE(PROCESS),0)
C
C        DETERMINE THE MODE OF THE PROCESS        
C
         IF (FORE_BACKGND.EQ.0) THEN
            IF (X2XR_APP_TO_SEND(PROCESS).EQ.X2XRF_BACKGROUND) THEN
               FORE_BACKGND = BACKGROUND_LOAD
            ELSE
               FORE_BACKGND = FOREGROUND_LOAD
            ENDIF
         ENDIF
C
C        GET THE FIRST ROMREV TO SEND FOR THIS STATION
C

         ACT_ROMREV = 0 
         ROMREV_IDX = 1
         IF(STATION.NE.0) THEN
           STNCLS = X2XS_STNCLS(STATION)
           IF(STNCLS.NE.0) THEN
               ACT_ROMREV = X2XC_ROMREV1(STNCLS)
           ELSE
             CALL OPS('No station class defined',0,0)
             CALL OPS('for station - ',STATION,0)      
             HOST_ID=IOR(X2FEMES_HOST_ID_EB_FLAG,HOST_ID)
             MES_NUM = 0
             RETURN                          
           ENDIF
         ENDIF
C
C        DETERMNE THE APPLICATION NUMBER OF THIS PROCESS'S FIRST LOAD
C
100      CONTINUE
         CALL X2GETAPPNO(ACT_ROMREV,APPLICATION_NO,FORE_BACKGND)
         IF(APPLICATION_NO.EQ.-1) THEN
             CALL OPS('No application found for Romrev',
     *                  0,ACT_ROMREV)
             HOST_ID=IOR(X2FEMES_HOST_ID_EB_FLAG,HOST_ID)
             MES_NUM = 0
             RETURN                          
         ENDIF


D        CALL OPS(' FORE: 1 /BACK: 2 :: ',FORE_BACKGND,0)
D        CALL OPS(' ACT_ROMREV:',0,ACT_ROMREV)
D        CALL OPS(' ROMREV INDEX: ',ROMREV_IDX,0)
D        CALL OPS(' APPLICATION NUMBER: ',APPLICATION_NO,0)

C       DETERMINE THE LAST LOAD FOR THIS PROCESS AND APPLICATION
C
        CALL X2LOAD_LAST(PROCESS,APPLICATION_NO,
     *                   X2XR_LASTLOAD(PROCESS))
C
C      IF A PROBLEM EXIST WITH TOO MANY LOAD NUMBERS
C      STOP THE PROCESS GRACEFULLY
C
    
        IF(X2XR_LASTLOAD(PROCESS).GT.MAXLOADS.OR.
     *      X2XR_LASTLOAD(PROCESS).EQ. 0) THEN
           IF(FORE_BACKGND.EQ.FOREGROUND_LOAD) THEN
             FORE_BACKGND = BACKGROUND_LOAD
             GOTO 100
           ELSE
             CALL OPS(' LASTLOAD NUMBER IS TOO LARGE OR NOT FOUND',0,0)
             CALL OPS(' FOR RELAY PROCESS ',PROCESS, 0)
             HOST_ID=IOR(X2FEMES_HOST_ID_EB_FLAG,HOST_ID)
             MES_NUM = 0
             RETURN
           ENDIF
        ENDIF
C
C      DETERMINE THE FIRST LOAD TO SEND FOR THIS PROCESS AND APPLICATION
C
        CALL X2LOAD1(PROCESS,APPLICATION_NO,X2XR_FIRSTLOAD(PROCESS))
C
        IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XRAPP).NE.0) THEN
            TYPE*,'CALLED X2LOAD1 * ',PROCESS,X2XR_FIRSTLOAD(PROCESS)
        ENDIF
C
        IF (X2XR_FIRSTLOAD(PROCESS).GT.0) THEN   !MARK HOW MANY TIMES TO SEND
           IF (X2XR_TIMES_SEND_APP(PROCESS).GE.0 .AND. 
     *          ((X2XR_FIRSTLOAD(PROCESS) .EQ. ACL_LOAD_NO) .OR.
     *          (X2XR_FIRSTLOAD(PROCESS) .EQ. FCL_LOAD_NO))  
     *            ) THEN
           	    X2DLL_TIMES_TO_SEND(X2XR_FIRSTLOAD(PROCESS))=
     *                     X2XR_TIMES_SEND_APP(PROCESS)
           ENDIF
        ENDIF
C
C
D       CALL OPS(' First Load: ',X2XR_FIRSTLOAD(PROCESS),0)
D       CALL OPS(' Last  Load: ',X2XR_LASTLOAD(PROCESS),0)
C
        SEGMENT_NO = 1
C
C       IF SENDING AUTOBAUD SEGMENTS DETERMINE THE NUMBER OF TIMES
C       TO SEND SEG0 OF THE MCP
C
        IF(X2XR_AUTOBAUD(PROCESS).GT.0) THEN
           TIMES_TO_SEND_SEG0 = X2XR_AUTOBAUD(PROCESS)
           SEG0_DELAY = -2
D          CALL OPS(' Setting autobaud ',0,0)
        ELSE
           TIMES_TO_SEND_SEG0 = X2XR_SNDREST(PROCESS)
           SEG0_DELAY = -1
        ENDIF
C
        IF ((X2XR_BRDCST_TYPE(PROCESS).EQ.X2RLOD_NO_RESET_SEL) .OR.
     *       (X2XR_BRDCST_TYPE(PROCESS).EQ.X2RLOD_NO_RESET_FULL)) THEN
           SEG0_DELAY = 0
        ENDIF	    
C
      ENDIF   ! END OF PROCESS INITIALIZATION
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XRAPP).NE.0) THEN
	   TYPE*,'BCAST TYPE AFTER INIT ',X2XR_BRDCST_TYPE
	ENDIF
C
C***************************************************************************
C      -- SEND ANY RESETS --
C***************************************************************************
C

      IF((LOAD_NO .EQ. 0) .AND. (SEGMENT_NO .EQ. 1)) THEN
	 IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XRAPP).NE.0) THEN
	   TYPE *,' SENDING RESET:TIMES TO SEND =',TIMES_SENT
	 ENDIF
C
        NEXT_SEGMENT = 1
C
C       SEND A RESET

	IF(X2XR_BRDCST_TYPE(PROCESS)  .EQ. X2RLOD_FULL	    .OR.
     *	    X2XR_BRDCST_TYPE(PROCESS) .EQ. X2RLOD_SOFT_SEL  .OR.
     *	    X2XR_BRDCST_TYPE(PROCESS) .EQ. X2RLOD_HARD_FULL .OR.
     *	    X2XR_BRDCST_TYPE(PROCESS) .EQ. X2RLOD_SOFT_FULL) THEN

	   IF(X2XR_BRDCST_TYPE(PROCESS) .EQ. X2RLOD_FULL)      MES_NUM=URESET
	   IF(X2XR_BRDCST_TYPE(PROCESS) .EQ. X2RLOD_SOFT_SEL)  MES_NUM=SRESET
	   IF(X2XR_BRDCST_TYPE(PROCESS) .EQ. X2RLOD_HARD_FULL) MES_NUM=HRESET
	   IF(X2XR_BRDCST_TYPE(PROCESS) .EQ. X2RLOD_SOFT_FULL) MES_NUM=SRESET
C
C
C
	   IF(TIMES_SENT .LT. X2XR_SNDREST(PROCESS)+1) THEN
	      TIMES_SENT = TIMES_SENT + 1
	      RESET_DELAY   = -1
              IF (PROCESS.NE.0 .AND. X2XR_SUBNETWORK(PROCESS) 
     *            .NE.X2XC_SUBNETWORK(X2XC_CLASS_X21))
     *           FLAGS=IOR(FLAGS,X2STMES_RELAYF_ES)
D             CALL OPS(' Sending reset ',TIMES_SENT,0)
D             CALL OPS(' Message number: ',MES_NUM,0)
D             CALL OPS(' Segment number: ',SEGMENT_NO,0)
D             CALL OPS(' Next segment: ',NEXT_SEGMENT,0)
	      GOTO 1000
C
C
C
	    ELSE
              LOAD_NO = MCP_LOAD_NO
              IF (X2XR_TIMES_SEND_MCP(PROCESS).GE.0)
     *            X2DLL_TIMES_TO_SEND(MCP_LOAD_NO)=
     *                                X2XR_TIMES_SEND_MCP(PROCESS)
              TIMES_SEG0_SENT=0
              TIMES_SENT = 1
            ENDIF
C
C       IF THE BROADCAST SELECTED IS WITH NO RESET THEN DON'T SEND THE MCP
C
	ELSEIF (X2XR_BRDCST_TYPE(PROCESS) .EQ. X2RLOD_NO_RESET_SEL .OR.
     *		  X2XR_BRDCST_TYPE(PROCESS) .EQ. X2RLOD_NO_RESET_FULL) THEN
D           CALL OPS(' Not sending MCP ',0,0)
	    LOAD_NO =  X2XR_FIRSTLOAD(PROCESS)	    !DO NOT SEND MCP
	    TIMES_SENT = 1			
	    TIMES_SEG0_SENT=0
C
C
C
	ELSE
	    CALL OPS(' X2rdload sending invalid broadcast type ',
     *		       X2XR_BRDCST_TYPE(PROCESS),0)
	    HOST_ID=IOR(X2FEMES_HOST_ID_EB_FLAG,HOST_ID)
	    MES_NUM = 0
	    RETURN			
	ENDIF
      ENDIF   ! END OF RESET PROCESSING
C
C
C***************************************************************************
C      -- SEND MCP SEG0 IF NECESSARY --
C***************************************************************************
C
	IF(LOAD_NO .LE. MCP_LOAD_NO) THEN
	   IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XRAPP).NE.0) THEN
	      TYPE *,' SENDING SEG0 MCP'
	      TYPE *,' TIMES TO SEND = ',TIMES_SENT
	   ENDIF
C
	 IF((X2XR_BRDCST_TYPE(PROCESS) .NE. X2RLOD_NO_RESET_SEL) .AND.
     *	    (X2XR_BRDCST_TYPE(PROCESS) .NE. X2RLOD_NO_RESET_FULL)) THEN 

	     IF (TIMES_SEG0_SENT .LT. TIMES_TO_SEND_SEG0+1)THEN
		TIMES_SEG0_SENT=TIMES_SEG0_SENT+1
      		NEXT_SEGMENT = 1
	        RESET_DELAY = SEG0_DELAY
C
C	        FOR AUTO BAUD SET END OF SEGMENT FLAG
C
                IF (PROCESS.NE.0 .AND. X2XR_SUBNETWORK(PROCESS) 
     *              .NE.X2XC_SUBNETWORK(X2XC_CLASS_X21))
     *	           FLAGS=IOR(FLAGS,X2STMES_RELAYF_ES)
	        TIMES_SENT = 1
D               CALL OPS('Sending MCP SEG0 ',TIMES_SEG0_SENT,0)

	     ELSE
C
C               FOR SOFT RESET DO NOT SEND MCP,
C               SEG0 WAS ALREADY SENT MANY TIMES
C               FOR ALL OTHER TYPES SEND MCP
C
		IF (X2XR_BRDCST_TYPE(PROCESS) .EQ.X2RLOD_SOFT_SEL	.OR.
     *		    X2XR_BRDCST_TYPE(PROCESS) .EQ.X2RLOD_HARD_FULL	.OR.
     *		    X2XR_BRDCST_TYPE(PROCESS) .EQ.X2RLOD_SOFT_FULL)      THEN
	           LOAD_NO = X2XR_FIRSTLOAD(PROCESS)
	           TIMES_SENT = 1
      		   NEXT_SEGMENT = 1
	        ENDIF
	      ENDIF
C
C	    DO NOT SEND ANY PART OF MCP 
C
	  ELSE
      	    NEXT_SEGMENT = 1
	    LOAD_NO = X2XR_FIRSTLOAD(PROCESS)
	    TIMES_SENT = 1
	 ENDIF
	ENDIF  ! END OF MCP SEG0 PROCESSSING
C
C***************************************************************************
C      -- SEND ANY LOADS --
C***************************************************************************
C
	IF(LOAD_NO .GE. MCP_LOAD_NO) THEN
C
C           DOES THIS LOAD NEED TO BE SEND AGAIN ??
C
	   IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XRAPP).NE.0) THEN
	      TYPE *,' SENDING LOAD ',LOAD_NO
	   ENDIF
C
C         CHECK TO DETERMINE IF THE LAST LOAD HAS BEEN SENT, IF IT HAS
C     	  THEN MARK THIS AS THE END OF THE BROADCAST
C
D         CALL OPS(' Sending load : ',LOAD_NO,0)
D         CALL OPS(' segment: ',NBRSEG,0)
C
          NBROFF = NBRSEG
          IF (CMP_DLL_FLAG.AND.
     *        SMFDLTAB(LOAD_NO,C_NBRSEG,APPLICATION_NO).NE.0 .AND.
     *        SMFDLTAB(LOAD_NO,APPTYPE,APPLICATION_NO).EQ.APP_TYPE)
     *                    NBROFF = C_NBRSEG        

C
D          CALL OPS(' Next segment: ',NEXT_SEGMENT,0)

	   IF(NEXT_SEGMENT .EQ. SMFDLTAB(LOAD_NO,NBROFF,
     *                                   APPLICATION_NO)) THEN
C
C	     SEND END OF THE SEGMENT FLAG AFTER EACH LOAD
D            CALL OPS(' Send end of load',0,0)            
	     FLAGS=IOR(FLAGS,X2STMES_RELAYF_ES)
C
	   ELSEIF(NEXT_SEGMENT .GT. SMFDLTAB(LOAD_NO,NBROFF,
     *                                       APPLICATION_NO)) THEN
	     NEXT_SEGMENT = 1	
	     TIMES_SENT = TIMES_SENT + 1
D            CALL OPS(' New load OR resend ',TIMES_SENT,0)
D            CALL OPS(' DLL_TIMES_TO_SEND: ',
D    *                  X2DLL_TIMES_TO_SEND(LOAD_NO),0) 
	     IF(TIMES_SENT .GT. X2DLL_TIMES_TO_SEND(LOAD_NO)+1) THEN
                IF (LOAD_NO.EQ.MCP_LOAD_NO) THEN
D                  CALL OPS(' TESTING FOR MCP',0,0)
                   IF (ROMREV_IDX.LT.3) THEN
D                     CALL OPS(' Send next MCP',0,0)
                      SEGMENT_NO = 0
                      ROMREV_IDX = ROMREV_IDX+1
                      CALL X2NEXT_APP(STATION,FORE_BACKGND,ROMREV_IDX,
     *                                ACT_ROMREV,APPLICATION_NO)	
                      IF(APPLICATION_NO .EQ. -1) THEN
D                        CALL OPS(' Send other loads',0,0)
                         ACT_ROMREV = X2XC_ROMREV1(X2XS_STNCLS(STATION))
                         APPLICATION_NO = 1
                         ROMREV_IDX = 1             !ALL MCP'S HAVE BEEN SENT,
                         SEGMENT_NO = 0             !NOW SEND OTHER LOADS.
                         LOAD_NO = MCP_LOAD_NO+1    !GET LOAD AFTER MCP LOAD
                         CALL X2LOAD_LAST(PROCESS,APPLICATION_NO,
     *                                     X2XR_LASTLOAD(PROCESS))
                         IF(X2XR_LASTLOAD(PROCESS).EQ.0) THEN
                           HOST_ID=IOR(X2FEMES_HOST_ID_EB_FLAG,HOST_ID)
                           MES_NUM = 0
                           RETURN
                         ENDIF 
                         CALL X2LOAD1(PROCESS,APPLICATION_NO,
     *                                     X2XR_FIRSTLOAD(PROCESS))
                     ENDIF
                  ENDIF
                ENDIF
             CALL X2GETLOAD(STATION,PROCESS,LOAD_NO,ACT_ROMREV,
     *                         ROMREV_IDX,FORE_BACKGND)
             TIMES_SENT = 1
	   ENDIF
C

C
D          CALL OPS(' Testing for last load ',
D    *                  X2XR_LASTLOAD(PROCESS),0)
D          CALL OPS(' LOAD_NO: ',LOAD_NO,0)          
	   IF (LOAD_NO.GT.X2XR_LASTLOAD(PROCESS)) THEN

             IF (X2XR_APP_TO_SEND(PROCESS).EQ.X2XRF_BOTH) THEN
                 IF (FORE_BACKGND.EQ.FOREGROUND_LOAD) THEN
                    FORE_BACKGND = BACKGROUND_LOAD
                 ELSE
                    FORE_BACKGND = 0
                    ROMREV_IDX = ROMREV_IDX + 1
                 ENDIF
             ELSE
                 FORE_BACKGND = 0
                 ROMREV_IDX = ROMREV_IDX + 1
             ENDIF
D            CALL OPS('ROMREV IDX: ',ROMREV_IDX,0)
D            CALL OPS('FORE/BACK: ',FORE_BACKGND,0)
             CALL X2NEXT_APP(STATION,FORE_BACKGND,ROMREV_IDX,
     *                                ACT_ROMREV,APPLICATION_NO)	 
             IF(APPLICATION_NO.NE.-1) THEN
               CALL X2LOAD_LAST(PROCESS,APPLICATION_NO,
     *                          X2XR_LASTLOAD(PROCESS))
               IF(X2XR_LASTLOAD(PROCESS).EQ.0) THEN
                 HOST_ID=IOR(X2FEMES_HOST_ID_EB_FLAG,HOST_ID)
                 MES_NUM = 0
                 RETURN
               ENDIF
               CALL X2LOAD1(PROCESS,APPLICATION_NO,
     *                          X2XR_FIRSTLOAD(PROCESS))
               LOAD_NO = MCP_LOAD_NO + 1
               CALL X2GETLOAD(STATION,PROCESS,LOAD_NO,ACT_ROMREV,
     *                       ROMREV_IDX,FORE_BACKGND)
               TIMES_SENT = 1
               SEGMENT_NO = 0
             ELSE
               MES_NUM=0
	       MES_LEN=0
	       HOST_ID=IOR(X2FEMES_HOST_ID_EB_FLAG,HOST_ID)
	       DELIVERY_DELAY=0
	       FLAGS=IOR(FLAGS,X2STMES_RELAYF_EB)
	       FLAGS=IOR(FLAGS,X2STMES_RELAYF_PE)
	       FLAGS=IOR(FLAGS,X2STMES_RELAYF_AR)
	       IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XRAPP).NE.0) THEN
	          TYPE*,'IN DLOAD - PROC, LOAD, SEG: ',PROCESS,LOAD_NO,
     *	              SEGMENT_NO,NEXT_SEGMENT
               ENDIF
               RETURN
	     ENDIF
	   ENDIF
C
         ENDIF 
	ENDIF   ! END OF LOAD PROCESSING
C
C                                                                              
C CALCULATE SEGMENT POINTER INTO MSGCOM (NOW CONTAINS COMP. SEGMENTS)          
C
	SEGMENT_NO_TO_SEND = NEXT_SEGMENT - 1                         
	IF(SMFDLTAB(LOAD_NO,C_NBRSEG,APPLICATION_NO).NE.0) THEN          
	    SEGPTR = SEGMENT_NO_TO_SEND * 2                           
	ELSE                                                          
	    SEGPTR = SEGMENT_NO_TO_SEND                               
	ENDIF                                                         
C                                                                              
C CHECK IF COMPRESSED DLL IS REQUESTED, IF WE HAVE COMPRESSED SEGS.            
C IF NOT, WE WILL SEND REGULAR SEGS.                                           
C                                                                              
	IF(CMP_DLL_FLAG) THEN                      
          IF (SMFDLTAB(LOAD_NO,APPTYPE,APPLICATION_NO).EQ.APP_TYPE) THEN
                IF(SMFDLTAB(LOAD_NO,C_NBRSEG,APPLICATION_NO).NE.0) THEN
                    SEGPTR = SEGPTR + 1
                ENDIF
          ENDIF
	ENDIF                                                         
C                                                                              
C     GET THE MESSAGE NUMBER OF THIS LOADS SEGMENT
C
	MES_NUM=SMFDLTAB(LOAD_NO,LODADR,APPLICATION_NO) + SEGPTR
	MES_LEN=0
C
1000	CONTINUE
C
C PREPARE DELIVERY DELAY [SEC] FIELD IN THE RELAY MESSAGE    
C
C
	IF (RESET_DELAY.NE.0) THEN
           IF(RESET_DELAY.EQ.-1) THEN
             DELIVERY_DELAY=X2XR_DLL_RESET_DELAY(PROCESS)
           ELSE
             DELIVERY_DELAY=X2XR_DLL_BETWEEN_SEGS_DELAY(PROCESS)
           ENDIF
	ELSEIF(NEXT_SEGMENT.GE.SMFDLTAB(LOAD_NO,NBROFF,APPLICATION_NO))THEN
	   DELIVERY_DELAY=X2XR_DLL_BETWEEN_LOADS_DELAY(PROCESS)
        ELSE
	   DELIVERY_DELAY=X2XR_DLL_BETWEEN_SEGS_DELAY(PROCESS)
	ENDIF
        
C
C
D         CALL OPS(' LOAD_NO: ',LOAD_NO,0)
D         CALL OPS(' NEXT_SEGMENT:',NEXT_SEGMENT,0)
C

	I2(2)=LOAD_NO+TIMES_SENT*256
	I2(1)=NEXT_SEGMENT
	STATION_ID(2)=I4
	I4=STATION_ID(1)
	I1(3) = TIMES_SEG0_SENT
C       I1(2) = ROMREV_IDX
C       I1(1) = FORE_BACKGND
        CALL ISBYTE(ROMREV_IDX,I1,1)
        CALL ISBYTE(FORE_BACKGND,I1,0)
        STATION_ID(1) = I4
C
C DEBUG INFORMATION
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XRAPP).NE.0)
     *	 TYPE 9000,'RET FROM DLL ',LOAD_NO,NEXT_SEGMENT,DELIVERY_DELAY,
     *	          STATION_ID
9000	FORMAT(1X,A,3(1X,I4),2(1X,Z8))
C
D       type*, ' flags = ', flags
	RETURN
	END
C      
C     Determine the last load for a process based on application number.   
C
      SUBROUTINE X2LOAD_LAST(PROCESS,APPLICATION_NO,LOAD_NO)
C

      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
      INCLUDE 'INCLIB:MSGCOM.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:PRMDLL.DEF'
      INCLUDE 'INCLIB:X2STMES.DEF'
      INCLUDE 'INCLIB:X2FEMES.DEF'
      INCLUDE 'INCLIB:PROCOM.DEF'
      INCLUDE 'INCLIB:X2XCOM.DEF'
      INCLUDE 'INCLIB:X2XREL.DEF'
      INCLUDE 'INCLIB:X2RLOD.DEF'
C  
      INTEGER*4 JJ, LOAD_NO
      INTEGER*4 PROCESS, APPLICATION_NO
C
C     DETERMINE THE LAST LOAD TO SEND. A FULL BROADCAST SENDS ALL LOADS
C     WHILE A SELECTIVE WILL BE VARIABLE.
C
      LOAD_NO = 0
      IF((X2XR_BRDCST_TYPE(PROCESS).EQ.X2RLOD_SOFT_SEL)  .OR.
     *    (X2XR_BRDCST_TYPE(PROCESS).EQ.X2RLOD_NO_RESET_SEL)) THEN
            DO 15 JJ = 1,MAXLOADS
              IF (X2DLL_TIMES_TO_SEND(JJ).LT.0) GOTO 15
                 IF (SMFDLNAM(1,JJ,APPLICATION_NO) .NE. '    ' .AND.
     *               SMFDLNAM(1,JJ,APPLICATION_NO) .NE. 0)  THEN
                    IF (SMFDLTAB(JJ,SNDFLG,APPLICATION_NO) .EQ. 1)
     *                     LOAD_NO = JJ
                  ENDIF
15	       CONTINUE
	   ELSE
	       DO 25 JJ = 1,MAXLOADS
		  IF (X2DLL_TIMES_TO_SEND(JJ).LT.0) GOTO 25
                  IF (SMFDLNAM(1,JJ,APPLICATION_NO) .NE. '    ' .AND.
     *                SMFDLNAM(1,JJ,APPLICATION_NO) .NE. 0)
     *                     LOAD_NO = JJ
25	       CONTINUE
	   ENDIF
D         CALL OPS(' Broadcast type: ',X2XR_BRDCST_TYPE(PROCESS),0)
D         CALL OPS(' last load(return) : ',LOAD_NO,0)

      RETURN
      END
C
C     Determine what the next application number to send for a particular 
C     station.
C
      SUBROUTINE  X2NEXT_APP(STATION,FORE_BACKGRND,ROMREV_IDX,
     *                       ACT_ROMREV,APPLICATION_NO)	            
C
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'           
C
      INCLUDE 'INCLIB:X2XCOM.DEF'
C
      INTEGER*4 STATION, STNCLS
      INTEGER*4 ACT_ROMREV, FORE_BACKGRND
      INTEGER*4 APPLICATION_NO, ROMREV_IDX
      INTEGER*4 SPACES/'    '/
C
      ACT_ROMREV = 0 
      IF(STATION.NE.0) THEN
        STNCLS = X2XS_STNCLS(STATION)
        IF(STNCLS.NE.0) THEN
          IF(ROMREV_IDX.EQ.1) THEN
            ACT_ROMREV = X2XC_ROMREV1(STNCLS)
          ELSEIF(ROMREV_IDX.EQ.2) THEN
            ACT_ROMREV = X2XC_ROMREV2(STNCLS)
          ELSEIF(ROMREV_IDX.EQ.3) THEN
            ACT_ROMREV = X2XC_ROMREV3(STNCLS)
          ENDIF
          IF(ACT_ROMREV .EQ. 0 .OR.
     *       ACT_ROMREV .EQ. SPACES) THEN
            APPLICATION_NO = -1
            RETURN 
          ENDIF
        ELSE
          APPLICATION_NO = -1
          RETURN    
        ENDIF
      ENDIF
C
      CALL X2GETAPPNO(ACT_ROMREV,APPLICATION_NO,FORE_BACKGRND)

C
      RETURN
      END
