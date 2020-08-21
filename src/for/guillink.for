C GUILINK.FOR
C
C V02 31-OCT-2000 UXN GUI prefix added.
C V01 16-JUN-1993 MP  INITIAL RELEASE FOR VAX (Produced From TCPASST).
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
C Copyright 1991-1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C This program is used as a Server to number of Clients 
C to transfer data segments via a TCP/IP Link and pass these data segments
C to GUIMGR.
C
C This program calls the following subroutines:
C
C GUITCPPTSKTRAP:
C		This routine is called via the DCLAST (declare AST) call in
C	        the main line of GUITCPPLINK. There are several event flags that
C		other programs (GUIMGR, GUICTRL) can set via the GUIQUE
C		routine. Once one of these event flags are set, the GUITCPPLINK
C		will DCLAST to this routine. This routine will call one of
C		several routines based on the parameter passed into the
C		routine.
C
C GUITCPPSTARTTIME:
C		This routine starts a timer trap for a specified amount
C		of time. There are 4 different types of timer traps. Only
C		one timer trap can be outstanding for each different type.
C		If a timer trap is currently outstanding for a certain type
C		and somebody wants to start another one for the same type then
C		the 2nd one will be ignored. Once the timer trap completes the
C		program will trap to the GUITCPPTIMTRAP routine.
C
C GUITCPPTIMTRAP:  
C		This routine is called once a timer trap completes. The
C		type of timer trap is passed into this routine. Based on
C		the type of timer trap, the routine calls one of several
C		routines.
C
C GUITCPPCHEKREAD:
C		This routine gets a GUI buffer and attempts to start
C		a read (using the DOREAD routine) for each read that can be
C		outstanding. If there is no connection then the routine
C		starts a timer trap to connect.
C
C GUITCPPDOREAD:
C		This routine starts a read( using QIO) with the passed-in
C		GUI buffer. Once the read is complete the program traps
C		to the GUITCPPRDIOCOMP routine.
C
C GUITCPPRDIOCOMP:
C		This routine is called once a read completes. If a read
C		completes with a good status then another read is started
C		(using the DOREAD routine). The buffer status in the GUI header
C		is set to GUI_GODRED for good reads. The GUI buffer is added 
C		to the GUI_FROM_LINK_QUE. If a read completes with an error 
C		then an	error message is printed. The buffer status is
C		set to GUI_BADRED. If the connection status is currently
C		connected then the routine forces a disconnect by calling
C		GUITCPPDODISC. In this case the buffer status is set to 
C		GUI_DISRED. The GUI buffer is added to the 
C		GUI_FROM_LINK_QUE.
C
C GUITCPPDOWRIT:
C		This routine starts a write (using QIO) if there is a
C		connection, a buffer on the GUI_TO_LINK_QUES and the number 
C		of outstanding writes has not exceeded the maximum.
C		Once the write is completed the program traps to the
C		GUITCPPWRIOCOMP routine. If there is no connection then the 
C		routine removes all buffers on the GUI_TO_LINK_QUE 
C		and add them to the GUI_TO_LINK_FRE_QUE.
C
C GUITCPPWRIOCOMP:
C		This routine is called once a write completes. If a write
C		completes with a good status then another write is started
C		using the DOWRIT routine. The GUI buffer is added to the 
C		GUI_FROM_LINK_QUES. If a write completes with an error then an
C		error message is printed. The buffer status is set
C		to GUI_BADWRT. If the connection status is currently connected 
C		then the routine forces a disconnect by calling GUITCPPDODISC. 
C		In this case the buffer status is set to GUI_DISWRT. The
C		GUI buffer is added to the GUI_FROM_LINK_QUES.
C
C GUITCPPCOIOCOMP:
C		This routine is called once the ACCESS completes. If the
C		connection completes with a good status then the connection
C		status is set to GUI_CONN( connected). The routine 
C		attempts to hang reads on the line using the GUITCPPCHEKREAD
C		routine. If the connection completes with an error then the
C		connection status is set to GUI_DISCON (disconnected).
C		The channel to UCX is deassigned using the GUITCPPDODASGN
C		routine. A timer trap is started to attempt to connect 
C		again in GUI_CONN_WAIT seconds.
C
C GUITCPPDOPCONN:
C		This routine allows passive connections on as many channels
C		as specified by GUI_MAX_CONN parameter.
C
C GUITCPPDODISC:
C		This routine attempts to disconnect. There are several
C		steps to a disconnection. The 1st step is to shutdown the local
C		socket. This is done by issuing a QIOW with a function code
C		of DEACCESS and SHUTDOWN. The 2nd step is to close the local
C		socket. This is done by issuing a QIOW with a function code
C		of DEACCESS. The 3rd step is to deassign the channel to UCX.
C		This is done by calling the GUITCPPDODASGN routine. Once the
C		disconnection has been completed a timer trap will be started
C		to attempt to connect again. The timer will be GUI_DISC_WAIT
C		milliseconds.
C Change NOTE:
C		the routine accepts two parameter: CONNection and RED_IGN_ONLY;
C		if CONN is not 0, only specified connection is disconnected;
C		if RED_IGN_ONLY is TRUE  or CONN is not 0 an attempt to 
C		connect is done after some GUI_DISC_WAIT time;
C		
C GUITCPPDODASGN:
C		This routine will deassign any channels into UCX. To do this
C		the SYS$DASSGN GUIice is called.
C
C GUITCPPSTOP:
C		This routine will attempt to disconnect using GUITCPPDODISC if
C		there is an active connection. The program will then stop.
C
C GUITCPPWATCHDOG:
C		This routine will detect connections CONNX in the CONNECTED
C		state that do not have GUITCP_WATCH_DOG(CONNX) equal .TRUE.
C		These connections will be automatically disconnected.
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM GUITCPPLINK
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
	INCLUDE 'INCLIB:GUILCOM.DEF'
C
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4   I,J              !Work variables
	INTEGER*4   STATUS		!Event status
	INTEGER*4   FLGSTS		!Status
	INTEGER*4   EVNMASK		!BITMAP OF ALL EVENTS SET
C
	INTEGER*4   NOFTLSIG
	EXTERNAL    NOFTLSIG
C
	EXTERNAL    GUITCPPTSKTRAP
C
	CALL LIB$ESTABLISH(NOFTLSIG)	    !No fatal errors
	CALL COPYRITE
C
 	CALL SNIF_AND_WRKSET
C
C CREATE THE COMMON EVENT FLAG CLUSTER.
C
        STATUS=SYS$ASCEFC(%VAL(GUI_EVNT_START),GUI_EVNNAME,0,0)
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
C CREATE THE EVENT FLAG MASK OF EVENTS FOR WHICH
C TO TRAP ON.
C
	EVNMASK=0
	DO 100 I=1,GUITCP_MAX_EVNS
	  EVNMASK = IBSET(EVNMASK, MOD(GUITCP_EVNS(I),32))
100	CONTINUE
C
C CLEAR ALL EVENT FLAGS.
C
        DO 200 I=1,GUITCP_MAX_EVNS
          STATUS=SYS$CLREF(%VAL(GUITCP_EVNS(I)))
200      CONTINUE
C
C CLEAR OUT ALL VARIABLES USED
C
	DO 800 J=1,GUI_MAX_CONN
	  GUITCP_CONNECTIONS(J)    = J		    ! to pass conn. to COIOCOMP 
	  GUI_CONN_CHAN(J)	= -1
	  GUI_CONN_STS(J)	= GUI_CONN_STS_DISCON
	  GUI_CONN_TYPE(J)	= GUI_PASS
	  GUI_RED_IGNORE(J)	= 0
C
	  DO 300 I=0,GUI_MAX_READS-1
	    GUI_READ_OUT(I,J)=GUI_READY
	    GUI_READ_BUF(I,J)=0
300	  CONTINUE
C
	  DO 400 I=0,GUI_MAX_WRITES-1
	    GUI_WRITE_OUT(I,J)=GUI_READY
	    GUI_WRITE_BUF(I,J)=0
400	  CONTINUE
C
800	CONTINUE
C
	DO 900 I=1,GUITCP_MAX_TIME_TRAPS
	    GUITCP_TIMEINMS(I)   = 0
	    GUITCP_TIMEDELY(1,I) = 0
	    GUITCP_TIMEDELY(2,I) = 0
	    GUITCP_TIMEINPR(I)   = GUI_READY
900	CONTINUE
C
	GUI_CHAN_SETUP = -1
C
C Start WATCH_DOG timer
C
	IF(GUI_WATCH_TIME.LE.0) THEN
	  TYPE *, IAM(), '********* WATCH_DOG TIME IS NOT SET **********'
C
C keep watching every minute without anu action untill GUI_WATCH_TIME
C becomes non zero
C
	  CALL GUITCPPSTARTTIME(GUITCP_TIME_WATCH, GUI_WATCH_TIME_DEFAULT)
	ELSE
	  CALL GUITCPPSTARTTIME(GUITCP_TIME_WATCH, GUI_WATCH_TIME)
	ENDIF
C
C
C PLACE TASK IN TRAP WAIT STATE. NOTE: TASK WILL STILL
C GUIICE AST TRAPS WHILE WAITING FOR EVENT FLAGS TO BE SET.
C
C ====================== MAIN PROCESSING =====================
C
1000    CONTINUE
	STATUS=SYS$WFLOR(%VAL(GUI_EVNT_START),%VAL(EVNMASK))
	IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
C CHECK FOR CRSPRO TASK TRAP.
C
	DO 1100 I=1,GUITCP_MAX_EVNS
	  STATUS=SYS$READEF(%VAL(GUITCP_EVNS(I)),FLGSTS)
	  IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
	  IF(STATUS.EQ.SS$_WASSET) THEN
	    STATUS=SYS$CLREF(%VAL(GUITCP_EVNS(I)))
	    IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
	    STATUS=SYS$DCLAST(GUITCPPTSKTRAP,I,)	 !TRAP TO TSKTRAP
	    IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
	    GOTO 1000
	  ENDIF
1100	CONTINUE
C
	GOTO 1000
	END
