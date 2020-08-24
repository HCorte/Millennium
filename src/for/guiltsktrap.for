C GUITSKTRAP.FOR
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
C		This routine is called via the DCLAST (declare AST) call in
C	        the main line of GUITCPPLINK. There are several event flags that
C		other programs (GUIMGR, GUICTRL) can set via the GUIQUE
C		routine. Once one of these event flags are set, the GUITCPPLINK
C		will DCLAST to this routine. This routine will call one of
C		several routines based on the parameter passed into the
C		routine.
C INPUT:
C	PARAM - index into GUITCP_EVNS array
C
C OUTPUT:
C	none
C
C RESULTS:
C	call the routine corresponding to the PARAM
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUITCPPTSKTRAP(PARAM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
	INCLUDE 'INCLIB:GUILCOM.DEF'
C
	INTEGER*4   PARAM		!Parameter passed into routine
	INTEGER*4   CONN_WAIT
	LOGICAL   RED_IGN_ONLY  ! If .TRUE. disconnect those with RED_IGNORE=1
				! If .FALSE. no matter what RED_IGNORE is
C
	IF(GUI_DBG_UNIT.NE.0) THEN
	  TYPE *,IAM(),'GUILTSKTRAP: parameter= ', PARAM
	ENDIF
C
	IF(PARAM.EQ.GUI_PCONNECT) THEN
	  CONN_WAIT = 1000				! 1 second
	  CALL GUITCPPSTARTTIME(GUITCP_TIME_PCON, CONN_WAIT)
C
	ELSE IF(PARAM.EQ.GUI_WRITE) THEN
	  CALL GUITCPPDOWRIT(0)
C
	ELSE IF(PARAM.EQ.GUI_DISC_RED_IGN) THEN
	  RED_IGN_ONLY=.TRUE.
	  CALL GUITCPPDODISC(0, RED_IGN_ONLY)
C
	ELSE IF(PARAM.EQ.GUI_DISCONNECT) THEN
	  RED_IGN_ONLY=.FALSE.
	  CALL GUITCPPDODISC(0, RED_IGN_ONLY)
C
	ELSE IF(PARAM.EQ.GUI_STOP) THEN
	  CALL GUITCPPSTOP
C
	ENDIF
C
	RETURN
	END
