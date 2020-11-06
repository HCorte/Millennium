C PROGRAM APULOG
C  
C     Rev 1.0   21 Jan 1993 15:39:30   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C V02 09-MAY-91 MP  ADDED CALL TO SNIF_AND_WRKSET
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
	PROGRAM APULOG
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:QUECOM.DEF'
C
	INTEGER*4 TASK
	INTEGER*4 ST
	INTEGER*4 BUF,XSER
C
C
C
	TASK=APU
	CALL COPYRITE
C V02
	CALL SNIF_AND_WRKSET
C
C
C WAIT FOR SOMETHING TO DO
C IF END OF DAY THEN CALL GSTOP(GEXIT_SUCCESS)
C
10	CONTINUE
	IF(DAYSTS.EQ.DSCLOS) CALL GSTOP(GEXIT_SUCCESS)
	CALL XWAIT(37,1,ST)
C
C GET BUFFER NUMBER FROM TOP OF MY QUEUE.
C IF NOTHING QUEUED, GO BACK TO WAIT STATE.
C
20	CONTINUE
	CALL TOPQUE(TASK,BUF)
	IF(BUF.EQ.0) GOTO 10
C
C DO APU_WLOG FOR THIS TRANSACTION
C
	XSER = IAND(PRO(WRKTAB,BUF),'3FFFFFFF'X) !filters the two most significative bits of the 4 bytes/Integer (30 lower bits)
C
	CALL APU_WLOG(XSER,PRO(WRKTAB,BUF))
C
	CALL QUETRA(LOG,BUF)
	CALL DQUTRA(TASK,BUF)
	GOTO 20
	END
