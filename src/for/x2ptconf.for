C
C SUBROUTINE X2PTCONF
C
C V07 31-MAY-2011 FJG OOB issue fixed
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2PTCONF.FOV                                 $
C  $Date::   17 Apr 1996 16:27:12                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2ptconf.for;1 **
C
C X2PTCONF.FOR
C
C V05 31-Jul-95 das Added X2CVNDRP                                         
C VO4 22-AUG-94 GPR USE STATION CLASS VALUE WHERE POSSIBLE - Integrate UK 
C		    changes into X2X Baseline
C VO3 22-JAN-91 DAS SEND DUMMY CONFIGURATIONS IF NEED BE
C V02 15-JAN-91 DAS REMOVED CODE THAT CAUSED TWO PORT 1 TO BE CONFIGUED.
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This subroutine will create the station port configuration
C message.  Most of the port information is obtained from
C memory except for the Polling Timeout - this is read from
C the station port configuration file.
C
C Calling sequence:
C
C     CALL X2PTCONF(TRABUF,MESS,MESLEN)
C
C Input parameters:
C
C     TRABUF      Int*4(TRALEN)       Transaction buffer
C
C Output parameters:
C
C     MESS        Int*4(*)            Output message
C     MESLEN      Int*2               Length of message buffer
C                                     If read error MESLEN=-1
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
	SUBROUTINE X2PTCONF(TRABUF,MESS,MESLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
CV04	INCLUDE 'INCLIB:X2XSTN.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*2   MESLEN                  !Output message length
	INTEGER*4   MESS(*)                 !Output message
	INTEGER*4   STN,PORT                !Station/port number
	INTEGER*4   TEMP                    !Work variable
	INTEGER*4   OFF                     !Data offset relative to hdr
	INTEGER*4   TERM                    !Term#
	INTEGER*4   MAXTER                  !Highest drop to poll
	INTEGER*4   STATE                   !Terminal state
	INTEGER*4   I, BAUD, FLAGS, AUTOSTATS, STNATR, PARAMS
	INTEGER*4   OUT_PORT, CHKVAL
	INTEGER*4   DRPOFFSET
        INTEGER*4   SYNC
        INTEGER*4   CLOCK
	CHARACTER   CDROP*2
	CHARACTER   C1TEMP(4)*1             !Work variable
	EQUIVALENCE (TEMP,C1TEMP)
C
	DATA        CDROP /'  '/
	DATA        STATE /0/
	
	INTEGER*4   CLASS			!V04
C
C INITIALIZE VARIABLES.
C
	STN=TRABUF(TXSTN)
	CLASS=X2XS_STNCLS(STN)			!V04

	CALL ILBYTE(PORT,MESS,X2STMES_STN_PORT-1)
	PORT=PORT+1
	MESLEN=0
	OFF=X2STMES_DATA-1
C
C CHECK FOR VALID STATION 
C
	IF(CHKVAL(STN,1,X2X_STATIONS,' STATION NUMBER ').NE.0) THEN
	  TYPE*,IAM(),'Error in Station: ',STN	  
	  MESLEN=-1
	  GOTO 8000
	ENDIF
C
C CHECK FOR VALID CLASS V07
C
	IF(CLASS.LE.0.OR.CLASS.GT.X2XC_CLASSES) THEN
	  TYPE*,IAM(),'Error in CLASS of Station: ',STN
	  MESLEN=-1
	  GOTO 8000
	ENDIF	
C
C READ THE STATION RECORD TO OBTAIN THE POLL TIMEOUT.
C
CV04	CALL READW(X2XSTN_FDB,STN,X2XSTN_REC,ST)
CV04	IF(ST.NE.0) THEN
CV04	  CALL OPS('X2PTCONF: Error reading X2XSTN.FIL ',STN,ST)
CV04	  MESLEN=-1
CV04	  GOTO 8000
CV04	ENDIF
C
C STORE PORT CONFIGURATION INTO OUTPUT MESSAGE.
C
	TEMP=0
	C1TEMP(1)=X2XS_CONF(STN)
	CALL ISBYTE(X2STMES_PORT_CONF,MESS,OFF+X2STMES_CMD-1)
	CALL ISBYTE(TEMP,MESS,OFF+X2STMES_PORT_CNTNO-1)
	CALL I4TOBUF2(STN,MESS,OFF+X2STMES_PORT_STNO-1)
       	OUT_PORT=PORT-1
C
	CALL ISBYTE(OUT_PORT,MESS,OFF+X2STMES_PORT_NUM-1)
CV04	CALL I4TOBUF2(X2XSTN_POLTIM,MESS,OFF+
	CALL I4TOBUF2(X2XC_POLTIM(CLASS),MESS,OFF+		!V04
     *	              X2STMES_PORT_POLTIM-1)
C
C STORE PORT CONFIGURATION FLAGS AND THE PROTOCOL TYPE.
C
	CALL ILBYTE(STNATR,IX2XS_ATRIBUTE,STN-1)
C
C USE GLOBAL IF SET
C
        AUTOSTATS=IAND(X2X_STATION_ATRIBUTES,X2XSA_AUTO_STATS)
	IF(AUTOSTATS.EQ.0) AUTOSTATS=IAND(STNATR,X2XSA_AUTO_STATS)
	FLAGS=0
	IF(AUTOSTATS.NE.0) FLAGS=FLAGS+X2STMES_PORT_AUTO
CV04	FLAGS=FLAGS+X2XSTN_PROTO
	FLAGS=FLAGS+X2XC_PROTO(CLASS)				!V04
	CALL ISBYTE(FLAGS,MESS,OFF+X2STMES_PORT_FLAGS-1)
C
C STORE PORT PARAMETERS AND CLEAR FREE BYTE
C
	PARAMS = BX2XS_PARAM(STN)
	CALL ISBYTE(PARAMS,MESS,OFF+X2STMES_PORT_PARAM-1)
	CALL ISBYTE(0,MESS,OFF+X2STMES_PORT_FREE-1)
C
C STORE BAUD RATE, CLOCK TYPE AND COMMUNICATIONS TYPE
C
        FLAGS = 0
        CLOCK = BX2XC_CLOCK(CLASS)			      !V04
        IF(CLOCK.NE.0) FLAGS = FLAGS + X2STMES_PORT_CLK
        SYNC  = BX2XC_SYNC(CLASS)			      !V04
        IF (SYNC.NE.0) FLAGS = FLAGS + X2STMES_PORT_SYNC
	BAUD=X2XC_BAUD(CLASS)				      !V04
        BAUD = BAUD + FLAGS
	CALL ISBYTE(BAUD,MESS,OFF+X2STMES_PORT_BAUD-1)
C
C LOAD THE DEFINED DROPS TO POLL.
C
	MAXTER=1
	DO 100 I=1,X2X_MAXTERMS
C
C SEND DUMMY CONFIGURATIONS IF PORT NUMBER EXCEEDS MAXPORTS DEFINED
C 
         IF(PORT.GE. 1 .AND. PORT .LE. X2X_MAXPORT) THEN
	    TERM=X2XS_TERMS(I,PORT,STN)
          ELSE
            TERM=0
          ENDIF
C
C TEST IF POLLING IS ENABLED
C    IF YES THEN SEND NORMAL PORT CONFIGURATION
C    OTHERWISE SET ALL DEFINED TERMINALS TO ZERO
C
	  IF(TERM.NE.0) THEN
	    CALL ILBYTE(STATE,IX2XT_STATE,TERM-1)
	    IF(STATE.NE.X2XTS_DISABLED) THEN
	      CDROP=X2XT_DROP_AD(TERM)
              CALL X2CNVDRP(CDROP,DRPOFFSET)           !...V05
	      IF(DRPOFFSET.GT.MAXTER) MAXTER=DRPOFFSET
	      CALL ISBYTE(CDROP(1:1),MESS,OFF+
     *	              X2STMES_PORT_DROPS+I-2)
	    ELSE
	      CALL ISBYTE(0,MESS,OFF+X2STMES_PORT_DROPS+I-2)
	    ENDIF
	  ELSE
	    CALL ISBYTE(0,MESS,OFF+X2STMES_PORT_DROPS+I-2)
	  ENDIF
100	CONTINUE
C
C STORE THE HIGHEST TERMINAL TO POLL.
C
	CALL ISBYTE(MAXTER,MESS,OFF+X2STMES_PORT_MAXTER-1)
C
C SET THE MESSAGE LENGTH.
C
	MESLEN=X2STMES_PORT_DROPS+X2X_MAXTERMS-1
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
	END
