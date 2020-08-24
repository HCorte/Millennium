C GUICOIOCOMP.FOR
C
C V02 13-NOV-2000 UXN GUI prefix added.
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
C INPUT:
C	CONN - connection number
C OUTPUT:
C	none
C RESULTS:
C	established connection 
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUITCPPCOIOCOMP(CONN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
	INCLUDE 'INCLIB:GUILCOM.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4   GUITCPPNETBYT
	INTEGER*4   CONN
	INTEGER*4   CONN_WAIT		!CONNECTION WAIT TIME
	INTEGER*4   J
C
	INTEGER*4   BLANK
	DATA	    BLANK/'    '/
C
	RECORD /SOCKADR_BYT/ REM_ADR
C
	IF(GUI_DBG_UNIT.NE.0) THEN
D	  TYPE *,IAM(),'GUILCOIOCOMP: CONN=', CONN
	ENDIF
C
	IF(GUI_CONN_IOSB(CONN).STAT .EQ. SS$_NORMAL) THEN
	  CALL MOVBYT(GUI_CONN_RHOST_ADD(1,CONN),1,REM_ADR,1,16)
	  CALL FASTSET(BLANK,GUI_MES_BUF,33)
	  WRITE(GUI_MES_CBUF,9000) 
     *	        'GUILCOIOCOMP: Good TCP/IP conn. ',
     *		'port,adr:', GUITCPPNETBYT(REM_ADR.INET_PORT),
     *		            (ZEXT(REM_ADR.ADRS_BYT(J)),J=1,4)
9000	  FORMAT(A,A,I5,4(1X,I3.3))
C
	  CALL WRITEBRK(GUI_MES_CBUF)
C
	  GUITCP_WATCH_DOG(CONN) = .TRUE.
C
	  GUI_CONN_STS(CONN)  = GUI_CONN_STS_CONNECTED	    !CONNECTED
	  GUI_CONNECTS(CONN)  = GUI_CONNECTS(CONN) + 1  
	  GUI_RED_IGNORE(CONN)	= 0			!needed also in GUITCPASST
	  CALL GUITCPPCHEKREAD
C
	ELSE IF(GUI_CONN_IOSB(CONN).STAT .NE. SS$_CANCEL       !also GUITCPASST...
     *     .AND.GUI_CONN_IOSB(CONN).STAT .NE. SS$_LINKABORT) THEN !also GUITCPASST...
	  CALL FASTSET(BLANK,GUI_MES_BUF,33)
	  WRITE(GUI_MES_CBUF,9100) GUI_CONN_IOSB(CONN).STAT
9100	  FORMAT('GUILCOIOCOMP: Error connecting ',I8)
	  CALL WRITEBRK(GUI_MES_CBUF)
	  GUI_CONN_STS(CONN) = GUI_CONN_STS_DISCON
	  GUI_CONNERRS(CONN) = GUI_CONNERRS(CONN) + 1
	  GUI_CONNLERR(CONN) = GUI_CONN_IOSB(CONN).STAT
	  CALL GUITCPPDODASGN(GUI_CONN_CHAN(CONN), CONN)
C
C	  TRY TO CONNECT LATER
C
	  IF(GUI_CONN_IOSB(CONN).STAT .NE. SS$_ACCVIO) THEN	!also GUITCPASST...
	    CONN_WAIT=GUI_CONN_WAIT
	    CALL GUITCPPSTARTTIME(GUITCP_TIME_PCON,CONN_WAIT)
	  ENDIF
	ENDIF
C
C
	RETURN
C
	END
