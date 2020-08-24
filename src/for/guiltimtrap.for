C GUITIMTRAP.FOR
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
C		This routine is called once a timer trap completes. The
C		type of timer trap is passed into this routine. Based on
C		the type of timer trap, the routine calls one of several
C		routines.
C
C INPUT:
C	TIMETYPE - time trap type (1 to 4), only two are used:
C		    GUITCP_TIME_READ and GUITCP_TIME_PCON
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUITCPPTIMTRAP(TIMETYPE)
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
	INTEGER*4 TIMETYPE		!TIMER TRAP TYPE
C
	IF(GUI_DBG_UNIT.NE.0) THEN
	  TYPE *,IAM(),'GUILTIMTRAP:  ',TIMETYPE
	ENDIF
C
	IF(TIMETYPE.GE.1 .AND. TIMETYPE.LE.GUITCP_MAX_TIME_TRAPS) THEN
	  GUITCP_TIMEINPR(TIMETYPE) = GUI_READY
	ENDIF
C
	IF(TIMETYPE.EQ.GUITCP_TIME_READ) THEN
	  CALL GUITCPPCHEKREAD
C
	ELSE IF(TIMETYPE.EQ.GUITCP_TIME_WRITE) THEN  ! not used...
	  CALL GUITCPPDOWRIT(0)
C
	ELSE IF(TIMETYPE.EQ.GUITCP_TIME_WATCH) THEN  ! used to check watch-dogs
	  CALL GUITCPPWATCHDOG
C
	ELSE IF(TIMETYPE.EQ.GUITCP_TIME_PCON) THEN
	  CALL GUITCPPDOPCONN
C
	ENDIF
C
	RETURN
	END
