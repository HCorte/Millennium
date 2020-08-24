C GUIDODISC.FOR
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C		This routine attempts to disconnect. There are several
C		steps to a disconnection. The 1st step is to shutdown the local
C		socket. This is done by issuing a QIOW with a function code
C		of DEACCESS and SHUTDOWN. The 2nd step is to close the local
C		socket. This is done by issuing a QIOW with a function code
C		of DEACCESS. The 3rd step is to deassign the channel to UCX.
C		This is done by calling the GUIDODASGN routine. Once the
C		disconnection has been completed a timer trap will be started
C		to attempt to connect again. The timer will be GUI_DISC_WAIT
C		milliseconds.
C Change NOTE:
C		the routine accepts two parameter: CONNection and RED_IGN_ONLY;
C		if CONN is not 0, only specified connection is disconnected;
C		if RED_IGN_ONLY is TRUE or CONN is not 0 an attempt to 
C		connect is done after some GUI_DISC_WAIT time;
C
C INPUT:
C	CONN		- connection nr
C	RED_IGN_ONLY	- if TRUE disconnect only those connections that
C			  have RED_IGNORE = 1.
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUITCPPDODISC(CONN, RED_IGN_ONLY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
	INCLUDE 'INCLIB:GUILCOM.DEF'
	INCLUDE 'INCLIB:INETDEF.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4 CONN		! IF EQ 0 - ALL CONNECTIONS
	LOGICAL   RED_IGN_ONLY  ! If .FALSE. no matter what RED_IGNORE is
	INTEGER*4 STATUS	!STATUS RETURNED FROM QIO
	INTEGER*4 IFUNC	        !FUNCTION CODE
C
C SOCKET PARAMETERS.
C
	INTEGER*2   SCK_PARM(2)			!SOCKET PARAMETERS
	BYTE        BSCK_PARM(4)		!BYTE PARAMETERS
	EQUIVALENCE (SCK_PARM,BSCK_PARM)
C
	INTEGER*4   CONN_FST, CONN_LST		!FIRST AND LAST CONNECTIONS
	INTEGER*4   CONN_INX			! CONNECTION INDEX
C
C STRUCTURE USED TO PASS PARAMETERS TO QIO CALLS (ITEMLIST 2).
C
C
        RECORD /GUI_IOSSTRUCT/ LOCAL_IOSB
C
	INTEGER*4   P4PARM
	INTEGER*4   DISC_WAIT	!WAIT TIME
C
	INTEGER*4   BLANK
	DATA	    BLANK/'    '/
C
	IF(GUI_DBG_UNIT.NE.0) THEN
	  TYPE *,IAM(),'GUILDODISC: CONN,RED_IGN_ONLY= ',CONN,RED_IGN_ONLY
	ENDIF
C
	IF(CONN.EQ.0) THEN
	    CONN_FST=1
	    CONN_LST=GUI_MAX_CONN
	ELSE
	    CONN_FST=CONN
	    CONN_LST=CONN
	ENDIF
C
	DO 1000 CONN_INX=CONN_FST, CONN_LST
C
	 IF(RED_IGN_ONLY.AND.GUI_RED_IGNORE(CONN_INX).NE.1) GOTO 1000
C
	 GUI_CONN_SIGNED_ON(CONN_INX) = .FALSE.
C
	 IF(GUI_CONN_STS(CONN_INX).NE.GUI_CONN_STS_CONNECTED) GOTO 800
C
	 GUI_CONN_STS(CONN_INX)=GUI_CONN_STS_DISINP
C
C SHUTDOWN THE LOCAL SOCKET
C
	 IFUNC=IO$_DEACCESS .OR. IO$M_SHUTDOWN
	 P4PARM = UCX$C_DSC_ALL
C
	 IF(GUI_DBG_UNIT.NE.0) THEN
	  TYPE *,IAM(),'GUILDODISC: CONN,CHAN= ', CONN_INX,
     *		      GUI_CONN_CHAN(CONN_INX)
	 ENDIF
C
	 STATUS=SYS$QIOW(,%VAL(GUI_CONN_CHAN(CONN_INX)),
     *                   %VAL(IFUNC),
     *                   %REF(LOCAL_IOSB),,,
     *                   ,                         !P1
     *                   ,			   !P2
     *                   ,		           !P3
     *                   %VAL(P4PARM),		   !P4
     *                   ,)		           !P5,P6
	 IF(.NOT.STATUS) THEN
	  CALL FASTSET(BLANK,GUI_MES_BUF,33)
	  WRITE(GUI_MES_CBUF,9000) IAM(),STATUS
9000	  FORMAT(A,'GUILDODISC: Error starting 1st DEACCESS  ',I8)
	  CALL WRITEBRK(GUI_MES_CBUF)
	  GOTO 700
	 ELSE IF(LOCAL_IOSB.STAT .NE. SS$_NORMAL) THEN
	  CALL FASTSET(BLANK,GUI_MES_BUF,33)
	  WRITE(GUI_MES_CBUF,9100) IAM(),LOCAL_IOSB.STAT
9100	  FORMAT(A,'GUILDODISC: Error completing 1st DEACCESS  ',I8)
	  CALL WRITEBRK(GUI_MES_CBUF)
	  GOTO 700
	 ENDIF
	 IF(GUI_DBG_UNIT.NE.0) THEN
	  TYPE *,IAM(),
     *	      'GUILDODISC: 1ST DEACCESS COMPLETED FOR conn=',CONN_INX
	 ENDIF
C
C CLOSE THE LOCAL SOCKET
C
	 IFUNC=IO$_DEACCESS
	 STATUS=SYS$QIOW(,%VAL(GUI_CONN_CHAN(CONN_INX)),
     *                   %VAL(IFUNC),
     *                   %REF(LOCAL_IOSB),,,
     *                   ,                         !P1
     *                   ,			   !P2
     *                   ,		           !P3
     *                   ,			   !P4
     *                   ,)		           !P5,P6
	 IF(.NOT.STATUS) THEN
	  CALL FASTSET(BLANK,GUI_MES_BUF,33)
	  WRITE(GUI_MES_CBUF,9200) IAM(),STATUS
9200	  FORMAT(A,'GUILDODISC: Error starting 2nd DEACCESS  ',I8)
	  CALL WRITEBRK(GUI_MES_CBUF)
	  GOTO 700
	 ELSE IF(LOCAL_IOSB.STAT .NE. SS$_NORMAL) THEN
	  CALL FASTSET(BLANK,GUI_MES_BUF,33)
	  WRITE(GUI_MES_CBUF,9300) IAM(),LOCAL_IOSB.STAT
9300	  FORMAT(A,'GUILDODISC: Error completing 2nd DEACCESS  ',I8)
	  CALL WRITEBRK(GUI_MES_CBUF)
	  GOTO 700
	 ENDIF
	 IF(GUI_DBG_UNIT.NE.0) THEN
	  TYPE *,IAM(),
     *	      'GUILDODISC: 2ND DEACCESS COMPLETED FOR conn=', CONN_INX
	 ENDIF
C
	 CALL FASTSET(BLANK,GUI_MES_BUF,33)
	 WRITE(GUI_MES_CBUF,9350) IAM(), CONN_INX
9350	 FORMAT(A,'GUILDODISC: Disconnected connection ',I4)
	 CALL WRITEBRK(GUI_MES_CBUF)
C
	 GOTO 800
C
700	 CONTINUE
	 GUI_DISCERRS(CONN_INX) = GUI_DISCERRS(CONN_INX) + 1
	 IF(.NOT.STATUS) THEN
	  GUI_DISCLERR(CONN_INX) = STATUS
	 ELSE
	  GUI_DISCLERR(CONN_INX) = LOCAL_IOSB.STAT
	 ENDIF
C
800	 CONTINUE
	 CALL GUITCPPDODASGN(GUI_CONN_CHAN(CONN_INX), CONN_INX)
	 GUI_CONN_STS(CONN_INX) = GUI_CONN_STS_DISCON
	 GUI_DISCONNS(CONN_INX) = GUI_DISCONNS(CONN_INX) + 1
C
1000	CONTINUE
C
	IF(CONN.GT.0.OR.RED_IGN_ONLY) THEN
C
C try to connect later
C
	    DISC_WAIT=GUI_DISC_WAIT
	    CALL GUITCPPSTARTTIME(GUITCP_TIME_PCON,DISC_WAIT)
	    GO TO 8000
	ENDIF
C
	IF(GUI_CHAN_SETUP.LT.0) GOTO 8000
C
C CLOSE THE LISTENER SOCKET
C
	IF(GUI_DBG_UNIT.NE.0) THEN
	    TYPE *,IAM(),'GUILDODISC: STARTING CHAN_SETUP DEACCESS '
	ENDIF
C
	IFUNC=IO$_DEACCESS .OR. IO$M_SHUTDOWN		    !also GUITCPASST...
	P4PARM = UCX$C_DSC_ALL				    !also GUITCPASST...
C
	STATUS=SYS$QIOW(,%VAL(GUI_CHAN_SETUP),
     *                     %VAL(IFUNC),
     *                     %REF(LOCAL_IOSB),,,
     *                     ,                       !P1
     *                     ,			   !P2
     *                     ,		           !P3
     *                     %VAL(P4PARM),	   !P4
     *                     ,)		           !P5,P6
	IF(.NOT.STATUS) THEN
	    CALL FASTSET(BLANK,GUI_MES_BUF,33)
	    WRITE(GUI_MES_CBUF,9400) IAM(),STATUS
9400	    FORMAT(A,'GUILDODISC: Error starting 3rd DEACCESS  ',I8)
	    CALL WRITEBRK(GUI_MES_CBUF)
	    GOTO 7000
	ELSE IF(LOCAL_IOSB.STAT .NE. SS$_NORMAL) THEN
	    CALL FASTSET(BLANK,GUI_MES_BUF,33)
	    WRITE(GUI_MES_CBUF,9500) IAM(),LOCAL_IOSB.STAT
9500	    FORMAT(A,'GUILDODISC: Error completing 3rd DEACCESS  ',I8)
	    CALL WRITEBRK(GUI_MES_CBUF)
	    GOTO 7000
	ENDIF
	IF(GUI_DBG_UNIT.NE.0) THEN
	    TYPE *,IAM(),'GUILDODISC: CHAN_SETUP DEACCESS COMPLETED'
	ENDIF
C
7000	CONTINUE
C
	CALL GUITCPPDODASGN(GUI_CHAN_SETUP,0)
C
8000	CONTINUE
	RETURN
C
	END
