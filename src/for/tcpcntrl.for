C TCPCNTRL.FOR
C
C V01 20-JUL-94 HHN INITIAL RELEASE FOR BELGIUM.
C                   ADDED OPTION SERVER MODE.
C
C V01 01-FEB-94 PXN INITIAL RELEASE FOR NETHERLANDS
C V01 01-JUN-93 PXN INITIAL RELEASE FOR IRELAND
C V01 12-DEC-91 KWP INITIAL RELEASE FOR VAX
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM TCPCNTRL
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:CRSCOM.DEF'
	INCLUDE 'INCLIB:TCPEVN.DEF'
C
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4   TEMP		!TEMPORARY WAIT TIME VARIABLE
	INTEGER*4   ST	                !Work variables
	INTEGER*4   STATUS		!SYSTEM CALL STATUS
C
	INTEGER*4   OPT



C --------------------------------------------------------
C
	CALL COPYRITE
	TYPE *,IAM(),'*'
	TYPE *,IAM(),'*** TCPCNTRL  V01  01-JUN-93'
	TYPE *,IAM(),'*'
C
C CREATE THE COMMON EVENT FLAG CLUSTER.
C
        STATUS=SYS$ASCEFC(%VAL(TC_EVNTIMER),TC_EVNNAME,0,0)
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
100	CONTINUE
C
C
400	CONTINUE
	TYPE *,' '
	TYPE *,' '
	TYPE *,IAM(),'OPTION  1:   CONNECT WITH LMS'
	TYPE *,IAM(),'OPTION  2:   DISCONNECT WITH LMS'
	TYPE *,IAM(),'OPTION  3:   ALLOW CONNECTIONS WITH LMS'
	TYPE *,IAM(),'OPTION  4:   DONT ALLOW CONNECTIONS WITH LMS'
	TYPE *,IAM(),'OPTION  5:   STOP TCPASST'
	TYPE *,IAM(),'OPTION  6:   TURN TCPASST DEBUGGING ON'
	TYPE *,IAM(),'OPTION  7:   TURN TCPASST DEBUGGING OFF'
	TYPE *,IAM(),'OPTION  8:   CHANGE THE CONNECTION WAIT TIME'
	TYPE *,IAM(),'OPTION  9:   CHANGE THE DISCONNECT WAIT TIME'
	TYPE *,IAM(),'OPTION 10:   MODE SERVER'
	TYPE *,' '
	CALL PRMNUM('ENTER OPTION  ',OPT,1,10,ST)
	IF(ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
C
	IF(OPT.EQ.1) GOTO 1000
	IF(OPT.EQ.2) GOTO 2000
	IF(OPT.EQ.3) GOTO 3000
	IF(OPT.EQ.4) GOTO 4000
	IF(OPT.EQ.5) GOTO 5000
	IF(OPT.EQ.6) GOTO 6000
	IF(OPT.EQ.7) GOTO 7000
	IF(OPT.EQ.8) GOTO 8000
	IF(OPT.EQ.9) GOTO 9000
	IF(OPT.EQ.10) GOTO 10000



C This option allows the operator to force a Connection with the
C LMS withoug waiting any time.
C
1000	CONTINUE
	IF(TCP_CONNSTS.EQ.TCCONN) THEN
	  TYPE *,IAM(),'ALREADY CONNECTED WITH LMS'
	  GOTO 400
	ELSE IF(TCP_CONNSTS.EQ.TCCONINP) THEN
	  TYPE *,IAM(),'ALREADY ATTEMPTING TO CONNECT WITH LMS'
	  GOTO 400
	ENDIF
	CALL TCPQUEUE(ACONASST,ST)
	IF(ST.NE.0) THEN
	  TYPE *,IAM(),'ERROR SENDING CONNECTION NOTICE TO TCPASST:  ',ST
	  GOTO 400
	ENDIF
	TYPE *,IAM(),'SENDING CONNECTION NOTICE TO TCPASST'
	GOTO 400
C
C
C This option allows the Operator to force TCPASST to disconnect from the
C LMS. Note, that TCPASST will attempt to reconnect again in
C 250 milliseconds, unless the connect override flag has been set.
C The 250 milliseconds time can be changed by changing the Disconnect wait
C time.
C
2000	CONTINUE
	CALL TCPQUEUE(DISASST,ST)
	IF(ST.NE.0) THEN
	  TYPE *,IAM(),'ERROR SENDING DISCONNECT NOTICE TO TCPASST:  ',ST
	ENDIF
	TYPE *,IAM(),'SENDING DISCONNECT NOTICE TO TCPASST'
	GOTO 400
C
C This option allows the Operator to change the connection override flag
C to okay which means that we will attempt to connect with the LMS.
C This flag is defaulted to okay at the beginning of the day.
C
3000	CONTINUE
	TCP_CONNOVR=TCOKAY
	TYPE *,IAM(),'CONNECTIONS ALLOWED WITH LMS'
	GOTO 400
C
C This option allows the Operator to change the connection override flag
C to no connections allowed. This means that TCPASST will not attempt to
C connect with the LMS. TCPASST will then set a timer and retry to
C connect after the timer expires. If the override flag is not changed then
C TCPASST will start another timer. This will continue until the system
C allows connections again. The length of the timer is 10000 milliseconds
C which is considered the Connect wait time.
C
4000	CONTINUE
	TCP_CONNOVR=TCNOT
	TYPE *,IAM(),'NO CONNECTIONS ALLOWED WITH LMS'
	GOTO 400
C
C
C This option will force TCPASST to stop. Before TCPASST stops, the connection
C will be closed.
C
5000	CONTINUE
	CALL TCPQUEUE(ENDASST,ST)
	IF(ST.NE.0) THEN
	  TYPE *,IAM(),'ERROR SENDING STOP NOTICE TO TCPASST:  ',ST
	ENDIF
	TYPE *,IAM(),'SENDING STOP NOTICE TO TCPASST'
	GOTO 400
C
C
C This option turns TCPASST debugging on. This option should only be used
C while debugging problems. By default, the debugging is turned off.
C
6000	CONTINUE
	TCP_DEBUG=1
	TYPE *,IAM(),'TURNING TCPASST DEBUGGING ON'
	GOTO 400
C
C
C This option turns TCPASST debugging off.
C
7000	CONTINUE
	TCP_DEBUG=0
	TYPE *,IAM(),'TURNING TCPASST DEBUGGING OFF'
	GOTO 400
C
C
C This option changes the Connect wait time. This is the number of
C milliseconds that TCPASST will wait after a failed connection, before 
C attempting to reconnect. By default, the connect wait time is 10000 
C milliseconds.
C
8000	CONTINUE
	TYPE *,IAM(),'CURRENT CONNECTION WAIT TIME IS ',TCP_CONWAIT,' MS'
	CALL PRMNUM('ENTER NEW CONNECTION WAIT TIME ',TEMP,100,99999999,ST)
	IF(ST.NE.0) THEN
	  TYPE *,IAM(),'ABORTING OPERATION'
	  GOTO 400 
	ENDIF
	TCP_CONWAIT = TEMP
	TYPE *,IAM(),'NEW CONNECTION WAIT TIME IS ',TCP_CONWAIT,' MS'
	GOTO 400
C
C
C This option changes the disconnect wait time. This is the number of
C milliseconds that TCPASST will wait after a disconnection, before attempting
C to reconnect. By default, the disconnect wait time is 250 milliseconds.
C
9000	CONTINUE
	TYPE *,IAM(),'CURRENT DISCONNECT WAIT TIME IS ',TCP_DISWAIT,' MS'
	CALL PRMNUM('ENTER NEW DISCONNECT WAIT TIME ',TEMP,25,99999999,ST)
	IF(ST.NE.0) THEN
	  TYPE *,IAM(),'ABORTING OPERATION'
	  GOTO 400 
	ENDIF
	TCP_DISWAIT = TEMP
	TYPE *,IAM(),'NEW DISCONNECT WAIT TIME IS ',TCP_DISWAIT,' MS'
	GOTO 400



C This option allows TCPASST to be in SERVER mode

10000	CONTINUE
c	IF(TCP_CONNSTS.EQ.TCCONN) THEN
c	  TYPE *,IAM(),'ALREADY CONNECTED WITH LMS'
c	  GOTO 400
c	ELSE IF(TCP_CONNSTS.EQ.TCCONINP) THEN
c	  TYPE *,IAM(),'ALREADY ATTEMPTING TO CONNECT WITH LMS'
c	  GOTO 400
c	ENDIF

	CALL TCPQUEUE(PCONASST,ST)
	IF(ST.NE.0) THEN
	  TYPE *,'ERROR SENDING SERVER CONNECTION NOTICE TO TCPASST:  ',ST
	  GOTO 400
	ENDIF
	TYPE *,IAM(),'SENDING SERVER CONNECTION NOTICE TO TCPASST'
	GOTO 400





	END
