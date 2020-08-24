C GUISTOP.FOR
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
C		This routine will attempt to disconnect using GUITCPPDODISC if
C		there is an active connection. The program will then stop.
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUITCPPSTOP
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
C
	LOGICAL   RED_IGN_ONLY  ! If .FALSE. no matter what RED_IGNORE is
C
	IF(GUI_DBG_UNIT.NE.0) THEN
	  TYPE *,IAM(),'GUILSTOP: entered......... '
	ENDIF
C
C	DISCONNECT 
C
	RED_IGN_ONLY=.FALSE.
	CALL GUITCPPDODISC(0, RED_IGN_ONLY)
C
	CALL GSTOP(GEXIT_SUCCESS)
C
	RETURN
	END
