C
C SUBROUTINE RESTSK
C $Log:   GXAFXT:[GOLS]RESTSK.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:44:20   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:30:38   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - restsk.for **
C
C RESTSK.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C
C SUBROUTINE TO ACTIVATE TASKS WAITING
C FOR LOGGER BUFFERS
C
C CALLING SEQUENCE:
C     CALL RESTSK(BITMAP)
C INPUT
C     BITMAP  -  BIT MAP OF ALL TASKS WAITING
C                FOR LOGGING.
C OUTPUT
C     NONE
C
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
	SUBROUTINE RESTSK(BITMAP)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
C
	INTEGER*4 STATUS, WAIT, TASK, BITMAP
C
C ACTIVATE ALL TASKS WAITING FOR LOG BUFFER
C
	DO 10 TASK=1,NUMAPPQUE
	WAIT=IAND(BITMAP,LOGID(TASK))
	IF(WAIT.NE.0) CALL RELSE(TSKNAM(TASK),STATUS)
10	CONTINUE
	RETURN
	END
