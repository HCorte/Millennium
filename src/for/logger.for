C
C PROGRAM LOGGER
C
C V11 27-JAN-2011 RXK IF command split
C V10 27-FEB-2001 UXN Added TMFMON, extending TM file added.
C V09 13-JAN-2001 CS  ADDED NEW REPROCESSING QUEUE FOR PASSIVE GAME
C V08 13-JUN-2000 UXN THRUSEND commented out. ICSLOG is used now for
C                     remote logging, HBLKRDY added.
C V07 14-JUN-1994 HXK CHANGED LOGIC OF CALL OF THRUSEND TO PREVENT WRITEBACK
C V06 07-APR-1994 HXK MULTIPLE REMOTE LOGGING
C V05 10-JAN-1994 WS  ADDED REMOTE LOGGING
C V04 29-MAY-1991 RRB SET X2X GAME STATE TO SHUTDOWN WHEN LOG DISK FAILS
C V03 16-MAY-1991 MTK FIXED BUG WHICH COULD CAUSE BLOCKS IN MEMORY TO GET
C                     OVERWRITTEN
C V02 09-MAY-1991 MP  ADDED CALL TO SNIF_AND_WRKSET
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C TRANSACTION DISK LOGGING TASK
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
	PROGRAM LOGGER
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:QUECOM.DEF'
	INCLUDE 'INCLIB:LOGCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	INCLUDE 'INCLIB:DESTRA.DEF'
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 	
C
	INTEGER*4 MFDB(7),BFDB(7),MESS(EDLEN)
	INTEGER*4 I, REPROCESS, HPPI, BLOCK, BL, NUMOUT
	INTEGER*4 STATUS, ST, BSTAT, MSTAT, NOCHECK0
C
	INTEGER*4 QUECNT    !FUNCTION TO SHOW # OF THINGS ON QUEUE
	INTEGER*4 REPROCESS_CNT/0/
C
	INTEGER*4 BLKS
	
	COMMON /NOCHECK0/ NOCHECK0
	DATA MSTAT/0/,BSTAT/0/
	INTEGER*4 WAIT_CNT
C
	CALL COPYRITE
C
C V02
	CALL SNIF_AND_WRKSET
C
	NOCHECK0=-1
C
C
        MESS(1)=LOG    
C
10	CONTINUE
	CALL OPENW(1,SFNAMES(1,PTMF),4,0,0,ST)
	CALL IOINIT(MFDB,1,32*256)
	IF(ST.NE.0) THEN
	  MESS(2)=TEGEN
	  MESS(3)=3
	  CALL FASTMOV(SFNAMES(1,PTMF),MESS(4),5)
	  MESS(9)=ST
	  CALL QUEMES(MESS)
	  TMFMON_OK_TO_DIE = 1
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
CV10
	CALL VAXGETFSIZ(1,BLKS)
	TMFMON_OK_TO_DIE = 0     ! LET TMFMON run
	MTM_BLK_CNT = BLKS/16    ! in 8kb buffers
	BTM_BLK_CNT = 0
CEV10
C
	IF(P(DISKSW).NE.0) THEN
	  CALL OPENW(2,SFNAMES(1,BTMF),4,0,0,ST)
	  CALL IOINIT(BFDB,2,32*256)
	  IF(ST.NE.0) THEN
	    MESS(2)=TEGEN
	    MESS(3)=3
	    CALL FASTMOV(SFNAMES(1,BTMF),MESS(4),5)
	    MESS(9)=ST
	    CALL QUEMES(MESS)
	    P(DISKSW)=0
	  ELSE
CV10
	    CALL VAXGETFSIZ(2,BLKS)
	    BTM_BLK_CNT = BLKS/16
CEV10
	  ENDIF
	ENDIF
CV10
C
C START TMFMON
C
	CALL START(8HTMFMON  )
CEV10

C
C START OF LOGGER RUN LOOP
C
20	CONTINUE
	CALL HOLD(0,STATUS)
C
C SAVE NUMBER OF TRANSACTIONS CURRENTLY IN OUTPUT QUEUE
C THEN FLUSH ALL LOGGER BUFFERS WITH PENDING I/O.
C
	NUMOUT=ACTTSK(LOG)
40	CONTINUE
	IF(LOKON(P(LOGFLG))) THEN
50	  CONTINUE
	  IF(P(LOGFLG).NE.0) THEN
	    CALL XWAIT(5,1,ST)
	    GOTO 50
	  ENDIF
	  GOTO 40
	ENDIF
C
C
	DO 1000 BL=1,NUMLOG
	IF(LOGBUF(BSTATE,BL).EQ.LTAPE) CALL RELSE(TSKNAM(TAP),ST)
	IF(LOGBUF(BSTATE,BL).EQ.LGROU.OR.
     *	 LOGBUF(BSTATE,BL).EQ.LGRDY) THEN
	  BLOCK=LOGBUF(BLONUM,BL)
	  LOGBUF(LRCNUM,BL)=BLOCK
	  LOGBUF(BSTATE,BL)=LGOUA
C
C WRITE BLOCK TO MASTER FILE 
C
	  CALL LOKOFF(P(LOGFLG))
C***	  CALL WRITEW(MFDB,BLOCK,LOGBUF(DSKREC,BL),MSTAT)
C***	  IF(MSTAT.NE.0) THEN
C***	    CALL FLUSH(MFDB,BFDB)
C***	    GOTO 9000
C***	  ENDIF
C
CV10
	  WAIT_CNT = 0
1200	  CONTINUE
	  IF(BLOCK.GT.MTM_BLK_CNT) THEN
	     CALL XWAIT(100,1,ST)
	     IF(MOD(WAIT_CNT,10).EQ.0) THEN   
	        CALL OPSTXT('Waiting for primary TMF extension to be cleared.')
	     ENDIF
	     WAIT_CNT = WAIT_CNT + 1
	     GOTO 1200
	  ENDIF
CEV10
C
	  CALL WRITENW(MFDB,BLOCK,LOGBUF(DSKREC,BL) )
C
C IF BACKUP DISK LOGGING IS ACTIVE THEN WRITE BLOCK
C TO BACKUP DISK. IF BACKUP WRITE ERROR THEN TERMINATE
C BACKUP DISK LOGGING AND NOTIFY OPERATOR.
C
	  IF(P(DISKSW).NE.0) THEN
CV10
	    WAIT_CNT = 0
1400	    CONTINUE
	    IF(BLOCK.GT.BTM_BLK_CNT) THEN
	       CALL XWAIT(100,1,ST)
	       IF(MOD(WAIT_CNT,10).EQ.0) THEN   
	          CALL OPSTXT('Waiting for backup TMF extension to be cleared.')
	       ENDIF
	       WAIT_CNT = WAIT_CNT + 1
	       GOTO 1400
	    ENDIF
CEV10
	    CALL WRITEW(BFDB,BLOCK,LOGBUF(DSKREC,BL),BSTAT)
	    IF(BSTAT.NE.0) THEN
	      MESS(2)=TEGEN
	      MESS(3)=5
	      CALL FASTMOV(SFNAMES(1,BTMF),MESS(4),5)
	      MESS(9)=BSTAT
	      CALL QUEMES(MESS)
	      P(DISKSW)=0
	    ENDIF
	  ENDIF
C
C WAIT FOR MASTER WRITE TO FINISH.
C
	  CALL IOSTAT(MFDB,MSTAT)
C
C IF MASTER FILE WRITE ERROR AND NO BACKUP EXISTS 
C (DUE TO BACKUP WRITE ERROR OR BACKUP NOT SET)
C THEN TERMINATE LOGGER (FATAL ERROR), IF BACKUP DOES EXIST
C MAKE THE BACKUP LOG FILE THE MASTER LOG FILE.
C 
	  IF(MSTAT.NE.0) THEN
	    CALL OPS('Primary disk write error ',BLOCK,MSTAT)
	    IF(P(DISKSW).NE.0) THEN
	       CALL FASTMOV(BFDB,MFDB,7)
	       CALL FASTMOV(SFNAMES(1,BTMF),SFNAMES(1,PTMF),5)
	       CALL FASTSET(0,SFNAMES(1,BTMF),5)
	       P(DISKSW) = 0
	       CALL OPS('Switching log backup to master ',BLOCK,MSTAT)
	       CALL OPS('Updating the SCF.FIL ',0,0)
	       CALL SWITMF
CV10
	       MTM_BLK_CNT = BTM_BLK_CNT
CEV10
	    ELSE
	       CALL FLUSH(MFDB,BFDB)
	       GOTO 9000
	    ENDIF
	  ENDIF
C
	  IF(LOGBUF(BLHISR,BL).GT.HSER) HSER=LOGBUF(BLHISR,BL)
C
C V05
C IF REMOTE LOGGING ACTIVE AND BLOCK FULL
C       LOG TO REMOTE SYSTEM
C
          IF ((LOGBUF(BLKCNT,BL).EQ.LBLK.AND.LOGBUF(BLTAPE,BL).EQ.0).OR. 
     *        DAYSTS.NE.DSOPEN) THEN

              IF(HBLKRDY.LT.BLOCK) HBLKRDY = BLOCK       
C
C              CALL THRUSEND(BLOCK,LOGBUF(DSKREC,BL))
C
          ENDIF
C EV05

C
C CHECK IF TAPE LOGGING ACTIVE
C AND FULL BLOCK THEN SET THE BUFFER
C STATUS AND ACTIVATE THE TAPE LOGGING TASK.
C
	  IF(P(TAPESW).NE.0)THEN
	    IF(LOGBUF(BLKCNT,BL).EQ.LBLK.AND.
     *	      LOGBUF(BLTAPE,BL).EQ.0) THEN
	      LOGBUF(BSTATE,BL)=LTAPE
	      LOGBUF(BLTAPE,BL)=1
	      CALL RELSE(TSKNAM(TAP),ST)
	    ENDIF
	  ENDIF
C
C UPDATE BUFFER STATUS AND RELEASE
C ALL TASKS WAITING FOR THIS BUFFER.
C
100	  CONTINUE
	  IF(LOKON(P(LOGFLG))) THEN
110	    CONTINUE
	    IF(P(LOGFLG).NE.0) THEN
	      CALL XWAIT(5,1,ST)
	      GOTO 110
	    ENDIF
	    GOTO 100
	  ENDIF
	  IF(LOGBUF(BSTATE,BL).EQ.LGOUA) LOGBUF(BSTATE,BL)=LGUSD !**V03**
	  CALL RESTSK(LOGBUF(TSKMAP,BL))
	  LOGBUF(TSKMAP,BL)=0
	  LOGBUF(LOGCNT,BL)=0
	ENDIF
C
C IF BUFFER IS WAITING TO BE READ THEN
C READ FROM MASTER FILE. IF MASTER FILE READ ERROR CHECK IF BACKUP EXISTS,
C IF BACKUP EXISTS READ FROM BACKUP AND IF SUCCESSFUL MAKE THE BACKUP
C FILE THE MASTER.
C
	IF(LOGBUF(BSTATE,BL).EQ.LGRIN) THEN
	  LOGBUF(BSTATE,BL)=LGINA
	  BLOCK=LOGBUF(BLONUM,BL)
	  CALL LOKOFF(P(LOGFLG))
	  CALL READW(MFDB,BLOCK,LOGBUF(DSKREC,BL),MSTAT)
	  IF(MSTAT.NE.0) THEN
	     IF(P(DISKSW).NE.0) THEN
	        CALL READW(BFDB,BLOCK,LOGBUF(DSKREC,BL),BSTAT)
		IF(BSTAT.EQ.0) THEN
		   CALL FASTMOV(BFDB,MFDB,7)
		   CALL FASTMOV(SFNAMES(1,BTMF),SFNAMES(1,PTMF),5)
		   CALL FASTSET(0,SFNAMES(1,BTMF),5)
		   P(DISKSW) = 0
		   CALL OPS('Master TM read error ',BLOCK,MSTAT)
		   CALL OPS('Switching to backup TM ',BLOCK,MSTAT)
		   CALL OPS('Updating the SCF.FIL ',0,0)
		   CALL SWITMF          !RESETTING NAMES IN THE SCF.FIL
CV10
	           MTM_BLK_CNT = BTM_BLK_CNT
CEV10
		   GOTO 120
		ENDIF
             ENDIF  
	     MESS(2)=TEGEN
	     MESS(3)=4
	     CALL FASTMOV(SFNAMES(1,PTMF),MESS(4),5)
	     MESS(9)=ST
	     MESS(10)=BLOCK
	     CALL QUEMES(MESS)
	     CALL FLUSH(MFDB,BFDB)
	     GOTO 9000
	  ENDIF
C
C UPDATE BUFFER STATUS AND RELEASE
C ALL TASKS WAITING FOR THIS BUFFER.
C
120	  CONTINUE
	  IF(LOKON(P(LOGFLG))) THEN
130	    CONTINUE
	    IF(P(LOGFLG).NE.0) THEN
	      CALL XWAIT(5,1,ST)
	      GOTO 130
	    ENDIF
	    GOTO 120
	  ENDIF
	  LOGBUF(BSTATE,BL)=LGRRW
	  IF(LOGBUF(BLKCNT,BL).EQ.LBLK) LOGBUF(BLTAPE,BL)=1
	  CALL RESTSK(LOGBUF(TSKMAP,BL))
	  LOGBUF(TSKMAP,BL)=0
	  LOGBUF(LOGCNT,BL)=0
	ENDIF
1000	CONTINUE
C
C EXIT CRITICAL REGION AND QUEUE ALL LOGGED TRANSACTIONS
C FROM LOGGER QUEUE TO COMMUNICATIONS OUTPUT QUEUE.
C
	CALL LOKOFF(P(LOGFLG))
	HPPI=MOD(P(PPISR3),SYSOFF)
	IF(P(SYSTYP).NE.LIVSYS)THEN
	  HPPI=HSER
	  GOTO 1010
	ENDIF
C
	IF(NETBACKUP(WAYINP).EQ.0) THEN                            !V11...
           HPPI=HSER
     	ELSEIF(NETMODE(NETBACKUP(WAYINP),WAYINP).NE.TRNMD) THEN
           HPPI=HSER
        ENDIF                                                      !...V11
C
1010	CONTINUE
	CALL LOGOUT(NUMOUT,HSER,HPPI)
C
C
	IF(DAYSTS.EQ.DSOPEN) GOTO 20
	REPROCESS=0
	IF (QUECNT(REPVQUE(1)).NE.0) REPROCESS=-1
C
	CALL RELSE(TSKNAM(RPC),ST)
	CALL RELSE(TSKNAM(VAL),ST)
C
C CHECK FOR PASSIVE GAME
C
        IF  (QUECNT(REPQUEPAS(1,RQPASPRO)).NE.0) THEN   !RETURN TICKETS
            CALL RELSE(TSKNAM(PST),ST)
            REPROCESS = -1
        ENDIF
        IF  (QUECNT(REPQUEPAS(1,RQPASVAL)).NE.0) THEN   !VALIDATIONS
            CALL RELSE(TSKNAM(PSV),ST)
            REPROCESS = -1
        ENDIF
C
	IF(ACTTSK(RPC).NE.0.OR.REPROCESS.NE.0) GOTO 20
	DO 1020 I=1,NUMLOG
	IF(LOGBUF(BSTATE,I).NE.LGUSD.AND.
     *	   LOGBUF(BSTATE,I).NE.LTAPE) GOTO 20
1020	CONTINUE
C
	IF(REPROCESS_CNT.EQ.0) REPROCESS_CNT = 999
	IF(REPROCESS_CNT.NE.0) THEN
	   REPROCESS_CNT = REPROCESS_CNT - 1
	   GOTO 20
	ENDIF
C
	HPPI=HSER                      !FORCE ALL TO WRITE
	CALL LOGOUT(ACTTSK(LOG),HSER,HPPI)
	P(LOGSTP)=1
	CALL CLOSEFIL(MFDB)
	IF(P(DISKSW).NE.0) CALL CLOSEFIL(BFDB)
	CALL RELSE(TSKNAM(TAP),ST)
	CALL RELSE(TSKNAM(VAL),ST)
	CALL RELSE(TSKNAM(RPC),ST)
	IF(DAYSTS.EQ.DSSUSP) THEN
1030	  CONTINUE
	  CALL HOLD(0,ST)
	  IF(DAYSTS.EQ.DSOPEN) GOTO 10
	  GOTO 1030
	ENDIF
CV10
	TMFMON_OK_TO_DIE = 1
CEV10
	CALL GSTOP(GEXIT_SUCCESS)
C
C
C COME HERE IF FATAL ERROR
C SET LOGGER STOP FLAG AND RELEASE TAPE LOGGING TASK.
C
9000	CONTINUE
	P(CMDFRZ)=800
	DAYSTS=DSCLOS
	X2X_GAME_STATE = X2X_GAMES_SHUTDOWN
	P(LOGSTP)=1
	CALL LOKOFF(P(LOGFLG))
	CALL RELSE(TSKNAM(TAP),ST)
	CALL RELSE(TSKNAM(RPC),ST)
	CALL RELSE(TSKNAM(VAL),ST)
CV10
	TMFMON_OK_TO_DIE = 1
CEV10
	CALL GSTOP(GEXIT_SUCCESS)
	END
