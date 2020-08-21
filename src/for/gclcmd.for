C
C SUBROUTINE GCLCMD
C
C V02 01-MAR-2001 UXN GPAUSE REMOVED.
C V01 XX-XXX-XXXX XXX INITIAL RELEASE.
C
C SUBROUTINE TO QUEUE COMMANDS TO SYSTEM AND CHECK FOR ERROR
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GCLCMD(CBUF,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INTEGER*4 CBUF(CDLEN), ST
C
10	CONTINUE
	CALL QUECMD(CBUF,ST)
	IF(ST.NE.0) THEN
	  CALL OPS('Queue command error, retrying...',ST,ST)
	  CALL XWAIT(500,1,ST)
	  GOTO 10
	ENDIF
	RETURN
	END
