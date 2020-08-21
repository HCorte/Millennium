C GUIQUE.FOR
C
C V02 31-OCT-2000 UXN GUI prefix added.
C V01 28-JUN-1993 MP  INITIAL RELEASE FOR VAX
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
C This subroutine will set one of GUILINK's event flags.
C This will cause GUILINK to do a predefined function.
C
C NOTE: THE TASK WHICH CALL THIS ROUTINE MUST HAVE
C PREVIOUSLY ATTACHED ITSELF TO THE COMMON EVENT
C CLUSTER (SEE GUIPRM.DEF and GUIMGR.FOR).
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUIQUE(EVN_INX,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUILCOM.DEF'
C
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4   ST		!RETURN STATUS
	INTEGER*4   STATUS	!SYSTEM FUNCTION CALL STATUS
	INTEGER*4   EVN_INX	!INDEX INTO GUITCP_EVNS TO BE SENT TO GUITCPASST
C
	ST = -1
	IF(EVN_INX.LT.1 .OR. EVN_INX.GT.GUITCP_MAX_EVNS) RETURN
C
C
	ST = 0
	STATUS=SYS$SETEF(%VAL(GUITCP_EVNS(EVN_INX)))
	IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
	RETURN
	END
