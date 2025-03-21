C
C PROGRAM OVERPRO
C OVERPRO.FOR
C
C V03 10-FEB-2000 LSTOVR as a local variable removed. LTLSTOVR added.
C V02 09-MAY-1991 MP ADDED CALL TO SNIF_AND_WRKSET
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C V01 01-FEB-1989 WS INITIAL RELEASE FOR SWEDEN
C
C=====================================================================
C
C DESCRIPTION  :
C
C    LOTTO POOLS PROCESSING TASK, WILL PROCESS ONLY OVERFLOWS
C
C     REV HISTORY:
C     4/21/88 INITIAL RELEASE
C
C
C SPECIAL NOTES:
C
C=====================================================================
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
	PROGRAM OVERPRO
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 MAX, REMST, OFFSET, ADDST, GAME
	INTEGER*4 ST, PHASE, INDX
	INTEGER*4 CHKNAM(2)/'CHKP','NT  '/
C
	CALL COPYRITE
C
C V02
	CALL SNIF_AND_WRKSET
C
	INDX=LTQNUM           !INITIALIZE QUEUE INDEX
	PHASE=1
10	CONTINUE
	CALL XWAIT(500,1,ST)    !RUN EVERY 500 MSECS
C
C     UPDATE ALL OVERFLOWS
C
20	CONTINUE
	CALL RTL(GAME,ADDOVR(1,INDX),ADDST)
	IF (ADDST.NE.2) THEN    !IF ANYTHING ON THE QUEUE
30	  CONTINUE
	  CALL RTL(OFFSET,ADDOVR(1,INDX),ADDST)
	  IF (ADDST.EQ.2) THEN
	    CALL XWAIT(20,1,ST)
	    GOTO 30
	  ENDIF
	  CALL INCOVR(OFFSET,GAME) !INCREMENT OVERFLOW NOW
C
	  GOTO 20          !DEQUEUE      NEXT
	ENDIF
C
	CALL RTL(GAME,REMOVR(1,INDX),REMST)
	IF (REMST.NE.2) THEN       !IF ANYTHING IN THE QUE
40	  CONTINUE
	  CALL RTL(OFFSET,REMOVR(1,INDX),REMST)
	  IF (REMST.EQ.2) THEN
	    CALL XWAIT(20,1,ST)
	    GOTO 40
	  ENDIF
	  CALL DECOVR(OFFSET,GAME)
C
	  GOTO 20
	ENDIF
C
C     CHECK IF PROCESSING CHECKPOINT, TO AVOID HAZARD ON LAST
C     RTL AND CHECKPOINT, PROCESS CHECKPOINT IN 2 PHASES
C
	IF (LCHKPNT.EQ.LOVRCHK) THEN
	   IF (PHASE.EQ.1) THEN
	      PHASE=2
	      GOTO 20
	   ELSE
	      PHASE=1
	      CALL FASTMOV(LTOVR,LSAVOVR,MAXOVR*2*LTNUMGAMES)
	      CALL FASTMOV(LTLSTOVR,LTSAVLST,LTNUMGAMES)
	      LSAVBLK=OVRBLK
	      CALL FASTMOV(LTPOOL_TOT,SAVLTPOOL_TOT,LTNUMGAMES)
	      LCHKPNT=LLODCHK            !MARK CHECKPOINT READY
	      CALL RELSE(CHKNAM,ST)     !'CONTINUE' CHECKPOINT TASK
C
	      INDX=LTQNUM                !PROCESS FROM THE RIGHT QUEUE
	   ENDIF
	ENDIF
C
	IF (LCHKSTOP.EQ.LSTOP) THEN  !REQUESTED STOP
C
C     WAIT TILL POOOLPRO FINISHED (UP TO 10 SECONDS) ????
C
	  MAX=20
60	  CONTINUE
	  IF (MAX.EQ.0.OR.LTSKSTAT.EQ.0) THEN
	    LTSKSTAT=-1
	    CALL GSTOP(GEXIT_SUCCESS)
	  ELSE
D	    TYPE *,IAM(),'WAITING FOR POOLPRO TO COMPLETE'
	    CALL XWAIT(500,1,ST)
	  ENDIF
	  MAX = MAX-1
	  GOTO 60
	ENDIF
C
C     PROCESS SUSPENSION
C
	IF (LCHKSTOP.EQ.LSUSPOVR) THEN
C
C     REINIT QUEUES
C
	  CALL DEFLST(ADDOVR(1,1),AOVRQUE-QHEDSZ)
	  CALL DEFLST(ADDOVR(1,2),AOVRQUE-QHEDSZ)
	  CALL DEFLST(REMOVR(1,1),ROVRQUE-QHEDSZ)
	  CALL DEFLST(REMOVR(1,2),ROVRQUE-QHEDSZ)
C
C       MARK SUSPEND "READY"
C
	  LCHKSTOP=LSUSPDON
70	  CONTINUE
	  IF (LCHKSTOP.NE.0.AND.LCHKSTOP.NE.LSTOP) THEN !SUSPENDED
	    IF (LCHKPNT.EQ.LOVRCHK) THEN   !PROCESS CHECKPOINTS
	       LCHKPNT=LLODCHK
	       INDX=LTQNUM                !PROCESS FROM THE RIGHT QUEUE
	       CALL RELSE(CHKNAM,ST)
	    ENDIF
C
	    CALL XWAIT(500,1,ST)
	    GOTO 70
	  ENDIF
	ENDIF
C
	GOTO 10
	END
