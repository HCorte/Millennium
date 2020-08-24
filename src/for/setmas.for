C
C SUBROUTINE SETMAS
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]SETMAS.FOV                                   $
C  $Date::   17 Apr 1996 15:01:28                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - net_setmas.for ***
C
C V03 22-APR-91 JWE REMOVED DISCOM
C V02 23-JAN-91 KWP REMOVED LINCOM/TERCOM
C V01 01-AUG-90 XXX RELEASED FOR VAX
C V00 29-MAY-87 MK/LMF LATEST RELEASE -- FROM NEWY
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	SUBROUTINE TO CONTROL SETTING SECONDARY TO MASTER MODE.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE SETMAS(SYS, RET)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:CTLCOM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DN_LINK.DEF'
	INCLUDE 'INCLIB:DN_BLOCK.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
C PARAMETER DECLARATIONS
C
	INTEGER*4	CON
C
	PARAMETER	(CON = 5)
C
C LOCAL DECLARATIONS
C
	INTEGER*4	ANS,
     *			BUF,
     *			BUF1,
     *			MAXSM,
     *			RESP,
     *			RET,
     *			ST,
     *			SYS,
     *			TIMES
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	RET = 0
C
	WRITE(CON, 9000) IAM(), IAM(), IAM(), IAM()
	CALL WIMG(CON, ' Continue ? [Y/N]: ')
	CALL YESNO(ANS)
	IF (ANS .NE. 1) THEN
	  RET = 3
	  GOTO 9999
	ENDIF
C
	MAXSM = 900					! HALF OF A MIN.
C
	CALL XWAIT(1, 2, ST)				! WAIT A SEC.
C
C WAIT FOR NORMAL STATE.
C
10	CONTINUE
	IF (P(CMDFRZ) .NE. 0) THEN
	  CALL XWAIT(20, 1, ST)				! WAIT 20 MSEC.
	  GOTO 10
	ENDIF
C
C FREEZE THE SYSTEM AND FLUSH THE INPUT QUEUE.
C
	P(NETFLU) = FLUREQ
C
20	CONTINUE
	IF (P(NETFLU) .NE. FLUSHED) THEN
	  CALL XWAIT(20, 1, ST)				! WAIT 20 MSEC.
	  GOTO 20
	ENDIF
C
	FREEZDUMM = 1					! do NOT SEND DUMMIES.
C
	P(SYSTYP) = SPRSYS
	IF (SYS .EQ. NODEID) P(SYSTYP) = BAKSYS
C
30	CONTINUE
	CALL EXTRABUF(BUF, WAYINP, ST)
	IF (ST .EQ. 2) THEN
	  CALL XWAIT(20, 1, ST)				! WAIT 20 MSEC.
	  GOTO 30
	ENDIF
C
	RESP = 1
	IF (SYS .EQ. NODEID) RESP = 0
C
	NETBUF(NEXT,     BUF) = HDRSIZ+5
	NETBUF(MODE,     BUF) = CMDMD
	NETBUF(BUFTYP,   BUF) = INP
	NETBUF(HDRSIZ+1, BUF) = SETMASTER
	NETBUF(HDRSIZ+2, BUF) = SYS
	NETBUF(HDRSIZ+3, BUF) = RESP			! IF 1, NEW MASTER WILL
C							! SEND 'SETMASTER' BACK.
	NETBUF(HDRSIZ+4, BUF) = WAYINP
	NETBUF(WAYNR,    BUF) = WAYINP
	NETBUF(PDEST,    BUF) = SYS
	NETBUF(FDEST,    BUF) = SYS
C
	IF (SYS .EQ. NODEID) THEN
	  CALL ATL(BUF, NETFINISH, ST)
	  RET = 0
	  GOTO 300
	ENDIF
C
C SEND 'SET MASTER'.
C
	CALL TSNDNET(BUF, WAYINP)
	CALL XWAIT(2, 2, ST)				! WAIT 2 SECS.
D	TYPE *, 'SETMASTER SENT'
C
C REMOVE LINK FIRST.
C
40	CONTINUE
	CALL EXTRABUF(BUF1, WAYINP, ST)
	IF (ST .EQ. 2) THEN
	  CALL XWAIT(20, 1, ST)				! WAIT 20 MSEC.
	  GOTO 40
	ENDIF
C
	NETBUF(MODE,     BUF1) = DRVMD
	NETBUF(WAYNR,    BUF1) = WAYINP
	NETBUF(BUFTYP,   BUF1) = INP
	NETBUF(HDRSIZ+1, BUF1) = REMLINK
	NETBUF(HDRSIZ+2, BUF1) = SYS
	NETBUF(HDRSIZ+4, BUF1) = WAYINP
C
	NETROUT(SYS, WAYINP)   = ROUIDLE
	NETSTAT(SYS, WAYINP)   = -IABS(NETSTAT(SYS, WAYINP))
C
C SEND 'REMOVE LINK'.
C
	CALL SNDNET(BUF1, WAYINP)
C
	TIMES = 0
100	CONTINUE
	IF (TIMES .LE. MAXSM / 3) THEN
	  CALL XWAIT(50, 1, ST)				! WAIT 50 MSEC.
	  TIMES = TIMES + 1
	  IF (NETMASTER(WAYINP) .NE. SYS) GOTO 100
	  RET = 0
	  GOTO 300
	ENDIF
C
C SWITCH TO SECONDARY MODE.
C
150	CONTINUE
	CALL EXTRABUF(BUF1, WAYINP, ST)
	IF (ST .EQ. 2) THEN
	  CALL XWAIT(20, 1, ST)				! WAIT 20 MSEC.
	  GOTO 150
	ENDIF
C
	NETBUF(MODE,     BUF1) = DRVMD
	NETBUF(WAYNR,    BUF1) = WAYINP
	NETBUF(BUFTYP,   BUF1) = INP
	NETBUF(HDRSIZ+1, BUF1) = ADDLINK
	NETBUF(HDRSIZ+2, BUF1) = SYS
	NETBUF(HDRSIZ+3, BUF1) = NSTASEC
	NETBUF(HDRSIZ+4, BUF1) = WAYINP
C
	NETSTAT(SYS, WAYINP)   = NSTASEC
C
	CALL TSNDNET(BUF1, WAYINP)
C
C NOW WAIT FOR SETMASTER FROM SECONDARY.
C
	IF (RET .EQ. 2) GOTO 300			! CRT.
C
	TIMES = 0
200	CONTINUE
	IF (TIMES .GT. MAXSM) THEN
	  RET = 1					! TIMEOUT.
	ELSE
	  CALL XWAIT(50, 1, ST)
	  TIMES = TIMES + 1
	  IF (NETMASTER(WAYINP) .NE. SYS) GOTO 200
	  RET = 0
	ENDIF
C
C ALL DONE.
C
300	CONTINUE
	FREEZDUMM = 0
	P(CMDFRZ) = 0
	P(NETFLU) = NOFLU
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FORMAT STATEMENTS.
C
9000	FORMAT(X, A, '**********************************************', /,
     *         X, A, '*** THIS OPERATION INVOLVES A TAKEOVER !!! ***', /,
     *         X, A, '**********************************************', /,
     *         X, A)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
