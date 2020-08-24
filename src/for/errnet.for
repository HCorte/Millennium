C
C SUBROUTINE ERRNET
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]ERRNET.FOV                                   $
C  $Date::   17 Apr 1996 13:05:18                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - net_netsub1.for ***
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1993 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose: Process error detected by driver
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE ERRNET(BUF)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:CTLCOM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:X2TDBH.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	BADSYS,
     *			BUF,
     *			BUF1,
     *			BWAY,
     *			CHECK_BUF,
     *			DSAP,
     *			I,				! GENERAL PURPOSE
     *			LSTNOT /0/,			! LAST ERR NOTIFICATION
     *			LSTTYP /0/,			! LAST ERROR TYPE
     *			MESSAGE,
     *			ST,
     *			TRCADR,
     *			TYPE
C
C COMMON AREA DECLARATIONS
C
	COMMON	/TEST_CHECK/	CHECK_BUF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	TYPE = IABS(NETBUF(MODE, BUF))			! ERROR TYPE
	BWAY = NETBUF(WAYNR, BUF)			! WAY NUMBER
C
	MESSAGE = 0
C
	IF (LSTTYP .NE. TYPE) CALL NOTIFY(TRCADR, NOTMOD, TYPE, BWAY)
	IF (LSTTYP .NE. TYPE) MESSAGE = -1
	LSTTYP = TYPE
C
	IF (LSTNOT .EQ. 0 .OR.
     *      LSTNOT .GT. NETTIMER) LSTNOT = NETTIMER
C
C EVERY 20 SEC
C
	IF (LSTNOT + 20000 .LT. NETTIMER .AND. MESSAGE .EQ. 0) THEN
	  CALL NOTIFY(TRCADR, NOTMOD, TYPE, BWAY)
	  LSTNOT = NETTIMER
	ENDIF
C
	BADSYS = NETBUF(PDEST, BUF)			! BAD SYSTEM
C
C FOR DRIVER COMMANDS, IGNORE ERRORS, JUST NOTIFY
C
	CHECK_BUF = 201
	CALL FREEBUF(BUF)
C
	IF (TYPE .EQ. DRVMD .OR. TYPE .EQ. CMDMD) GOTO 9999
C
C IF PRIMARY TO BACKUP XFER ...
C SEND SAME MESSAGE THRU LAN (TWICE)
C
	IF (NODEID .EQ. NETMASTER(BWAY) .AND.
     *      BADSYS .EQ. NETBACKUP(BWAY)) THEN		! TO BACKUP
	  DO 100 I = 1, 2
	    CALL LANGETX(BUF, ST)
	    IF (ST .NE. 2) THEN
	      DSAP = CTLSAPSYS(BADSYS)
	      LANBUF(LANDATAF+0, BUF) = X2NPLX
	      LANBUF(LANDATAF+1, BUF) = PLXPRIOK
	      LANBUF(LANDATAF+2, BUF) = DSAP
	      LANBUF(LANDATAF+3, BUF) = 16		! IN BYTES
	      CALL ABL(BUF, CTLEXECQ, ST)
	      IF (ST .NE. 0)
     *          CALL OPS(CHAR(7) // '*** ERRNET - CTLEXECQ OVF ***' //
     *                   CHAR(7), DSAP, ST)
	    ENDIF
	    CALL XWAIT(2, 2, ST)			! WAIT A COUPLE OF SECS
100	  CONTINUE
C
C DROP BACKUP SYSTEM SO WE CAN RELEASE BUFFERS FROM LOGGER
C
	  NETBACKUP(BWAY) = 0
	ENDIF
C
C NOTIFY ABOUT SYTEM BEING ABORTED
C
	CALL NOTIFY(TRCADR, NOTABRT, BADSYS, BWAY)
C
C DECLARE DEAD ALL CONNECTIONS TO NON-RESPONDING SYSTEM
C
	IF (NETROUT(BADSYS, BWAY) .EQ. ROUACT) THEN
200	  CONTINUE
	  CALL EXTRABUF(BUF, BWAY, ST)
	  IF (ST .EQ. 2) THEN
	    CALL XWAIT(20, 1, ST)
	    GOTO 200
	  ENDIF
C
	  NETBUF(HDRSIZ+1, BUF) = REMLINK
	  NETBUF(HDRSIZ+2, BUF) = BADSYS
	  NETBUF(HDRSIZ+4, BUF) = BWAY
C
	  CALL CMDNET(BUF)
C
	  NETROUT(BADSYS, BWAY) = ROUIDLE
	  NETSTAT(BADSYS, BWAY) = -IABS(NETSTAT(BADSYS, BWAY))
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
