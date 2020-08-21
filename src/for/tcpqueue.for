C  GXSRC:TCPQUEUE.FOR
C  
C  $Log:   GXAFXT:[GOLS]TCPQUEUE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:31:32   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   03 Jan 1994 23:13:16   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 18:39:18   SYSTEM
C  Initial revision.
C
C
C
C V01 20-NOV-91 KWP INITIAL RELEASE FOR VAX
C
C This subroutine will set one of TCPASST's event flags.
C This will cause TCPASST to do a predefined function.
C
C NOTE: THE TASK WHICH CALL THIS ROUTINE MUST HAVE
C PREVIOUSLY ATTACHED ITSELF TO THE COMMON EVENT
C CLUSTER (SEE TCPEVN.DEF).
C
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TCPQUEUE(QUENUM,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:TCPEVN.DEF'
C
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4   ST		!RETURN STATUS
	INTEGER*4   STATUS	!SYSTEM FUNCTION CALL STATUS
	INTEGER*4   QUENUM	!QUEUE TYPE TO BE SENT TO TCPASST
C
	ST = -1
	IF(QUENUM.LT.1 .OR. QUENUM.GT.TC_MAXQUE) RETURN
C
C
	ST = 0
	STATUS=SYS$SETEF(%VAL(TC_EVNQUE(QUENUM)))
	IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
	RETURN
	END
