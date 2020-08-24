C GUIMMGR.FOR
C
C V03 13-NOV-2000 UXN Initial release for GUIs, unused code removed.
C                     Multiple GUIWORK threads added.
C V02 08-JUL-1993 MP  Added user/password.
C V01 22-JUN-1993 MP  RELEASE FOR VAX 
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C This program handles the interface between the game and GUITCPPLINK
C program as part of prepriatary 'PASS THROUGH open SERVER'.
C This program (together with subroutines) will do the following:
C
C 1.if Primary:
C	a. Deques PROBUFs from the task queue, print an error and process
C	   as on Secondary (this may be after takeover),
C	b. dequeue GUI_LINK buffers from the GUI_FROM_GAME_QUE,
C	c. spool (some) messages to the spool file (log to TM),
C	d. copy messages to one (or more) GUI_LINK_BUF buffer(s), 
C	e. queue buffer(s) to one (or more) GUI_TO_LINK_QUE,
C	f. set proper event flag for TCPPLINK to process CLIENT-bound messages.
C	g. Dequeue buffers from the GUI_FROM_LINK_QUE, verify and
C	   response (through GUI_TO_LINK_QUE) to the CLIENT or 
C	   pass the message to the terminal (as an unsolicited message)
C	   or other destination.
C
C 2. if Secondary:
C	a. dequeue GUI_LINK buffers from the GUI_FROM_GAME_QUE,
C	   report an error and purge the GUI_FROM_GAME_QUE que;
C	b. dequeue PROBUFs from the task queue,
C	c. log records into the TM file,
C	d. update CON_FIRST_SPOOL_PATH_THRU and CON_LAST_SPOOL_PATH_THRU
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM GUIMGR
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
C
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4   LINK_BUF            !LINK buffer
	INTEGER*4   ST,I,J              !Work variables
	INTEGER*4   STATUS		!Event status
C
	INTEGER*4   NR_BUFS		! Returned from GUIxBUF subroutines
C
	INTEGER*4   TASK_ID		! current task ID
	INTEGER*4   WAIT_TIME
C
	INTEGER*4   BLANK
	DATA	    BLANK/'    '/
C
	REAL*8      R8_LINK_NAME          !Assist name for RUNTSK
	DATA        R8_LINK_NAME /'GUILINK'/
	REAL*8      R8_GUIWORK_NAME/'GUIWORK '/
	
	INTEGER*4   MAX_THREADS
	PARAMETER(MAX_THREADS = 3)

	REAL*8      R8_PROC(MAX_THREADS)
	CHARACTER*8 PROC(MAX_THREADS)
	DATA PROC   /'GUIWORK1','GUIWORK2','GUIWORK3'/
	INTEGER*4   DUMMY
	LOGICAL*4   RUNNING

	EQUIVALENCE(PROC,R8_PROC)
C
	INTEGER*4   NOFTLSIG
	EXTERNAL    NOFTLSIG
C
	CALL COPYRITE
C
	TASK_ID = GUI
C
 	CALL SNIF_AND_WRKSET
C
C CREATE THE COMMON EVENT FLAG CLUSTER.
C
        STATUS=SYS$ASCEFC(%VAL(GUI_EVNT_START),GUI_EVNNAME,0,0)
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
C SET UP ALL QUEUES.
C
	CALL DEFLST(GUI_LINK_FRE_QUE,GUI_LINK_BUFS)
	GUI_LINK_FRE_BUFS_GOAL = GUI_TO_LINK_BUFS/3
C
	DO 100 I=1,GUI_MAX_CONN
	    CALL DEFLST(GUI_FROM_LINK_QUES(1,I),GUI_LINK_BUFS)
	    CALL DEFLST(GUI_TO_LINK_QUES(1,I),  GUI_LINK_BUFS)
100	CONTINUE
C
	CALL DEFLST(GUI_FROM_GAME_QUE,GUI_LINK_BUFS) ! GAME TO GUIER QUE
C
C Set buffer numbers and initialize free queues
C
	DO 200 LINK_BUF=1,GUI_LINK_BUFS
	  GUI_LINK_BUF(GUI_BUF_NUM_OFF,LINK_BUF)=LINK_BUF
	  CALL ABL(LINK_BUF,GUI_LINK_FRE_QUE,ST)
200	CONTINUE
C
	DO 300 LINK_BUF=1,GUI_LINK_BUFS
	  GUI_LINK_BUF(GUI_BUF_NUM_OFF,LINK_BUF)=LINK_BUF
	  CALL ABL(LINK_BUF,GUI_LINK_FRE_QUE,ST)
300	CONTINUE
C
	GUI_SHORT_WAIT=100	    ! GUIMGR wait time if there is work
	GUI_LONG_WAIT=500	    ! GUIMGR wait time if there is no work
C
	GUI_NUM_BUF=0		    ! just a buffer counter
	GUI_NUM_REC=0		    ! just a record counter
C
C Set _CONN tables
C
	CALL FASTSET(0, GUI_CONN_RHOST_ADD, 4*GUI_MAX_CONN)
	CALL FASTSET(0, GUI_CONN_RHOST_RETLEN, GUI_MAX_CONN)
	CALL FASTSET(0, GUI_CONN_AUTH_INX, GUI_MAX_CONN)
	CALL FASTSET(0, GUI_CONN_NEXT_SEQ_NR, GUI_MAX_CONN)
	CALL FASTSET(0, GUI_CONN_LAST_SPOOL_SENT, GUI_MAX_CONN)
	CALL FASTSET(0, GUI_CONN_LAST_MSN_IN, GUI_MAX_CONN)
	CALL FASTSET(0, GUI_CONN_LAST_MSN_OUT, GUI_MAX_CONN)
	CALL FASTSET(0, GUI_CONN_LAST_TIME, GUI_MAX_CONN)
C
	DO 500 J=1, GUI_MAX_CONN
	  GUI_READS(J)		= 0
	  GUI_READERRS(J)	= 0
	  GUI_READLERR(J)	= 0
	  GUI_WRITES(J)	= 0
	  GUI_WRITEERRS(J)	= 0
	  GUI_WRITELERR(J)	= 0
	  GUI_WRITENOCS(J)	= 0
	  GUI_CONNECTS(J)	= 0
	  GUI_CONNERRS(J)	= 0
	  GUI_CONNLERR(J)	= 0
	  GUI_DISCONNS(J)	= 0
	  GUI_DISCERRS(J)	= 0
	  GUI_DISCLERR(J)	= 0
	  GUI_CONN_SYSTEM_NAME(J) = 'Not Signed-on '
	  GUI_CONN_SIGNED_ON(J) = .FALSE.
500	CONTINUE
C
C SETUP GUIWORKER QUEUES
C
	CALL DEFLST(GUI_WORKER_FREE_QUEUE,GUI_MAX_WORKER_MSG)
	CALL DEFLST(GUI_TO_WORKER_QUEUE,GUI_MAX_WORKER_MSG)

	DO I=1,GUI_MAX_WORKER_MSG
	  CALL ABL(I,GUI_WORKER_FREE_QUEUE,ST)
	ENDDO
C
	GUI_WORKER_DIE = 0
	GUI_WORKER_RELOAD = 0
C
C Initialize authorization table
C
	CALL GUIMGR_INIT_AUTH_TABLE
C
C set WATCH_DOG timer interval
C
	GUI_WATCH_TIME = GUI_WATCH_TIME_DEFAULT
C
C LOAD AND START THE TCP_LINK TASK.
C
	CALL START(R8_LINK_NAME)
	DO I=1,MAX_THREADS
           CALL START2(R8_GUIWORK_NAME,R8_PROC(I))
	ENDDO
C
C WAIT FOR SOMETHING TO DO, IF END OF DAY THEN STOP
C
1000	CONTINUE

	CALL XWAIT(20, 1, ST)
C
C RE-LOAD GUIWORKers if requested.
C
	IF(GUI_WORKER_RELOAD.NE.0) THEN
             GUI_WORKER_DIE = 1          ! shut-down old threads.
1010         CONTINUE
	     RUNNING = .FALSE.
	     DO I=1,MAX_THREADS
                CALL STTSK(R8_PROC(I), DUMMY, ST)
		IF(ST.NE.4) RUNNING = .TRUE.
             ENDDO
	     IF(RUNNING) THEN
		CALL XWAIT(20,1,ST)
		GOTO 1010
	     ENDIF
	     CALL XWAIT(2,2,ST) ! wait 2 seconds
	     GUI_WORKER_DIE = 0
	     GUI_WORKER_RELOAD = 0
             DO I=1,MAX_THREADS
                CALL START2(R8_GUIWORK_NAME,R8_PROC(I))
	     ENDDO 	    
	ENDIF
C
	IF(DAYSTS.EQ.DSCLOS) THEN
C
	  CALL GUIQUE(GUI_DISCONNECT,ST)
	  IF(ST.NE.0) CALL OPS('Error setting event flag for GUILINK',ST,ST)
C
	  WAIT_TIME=GUI_DISC_WAIT/1000
C
	  CALL FASTSET(BLANK,GUI_MES_BUF,33)
	  WRITE(GUI_MES_CBUF,9020) 'Waiting ',WAIT_TIME,
     *	      ' seconds and then shutting GUILINK down'
9020	  FORMAT(A,I2,A)
	  CALL WRITEBRK(GUI_MES_CBUF)
C
	  CALL XWAIT(WAIT_TIME,2,ST)
C
	  CALL GUIQUE(GUI_STOP,ST)
	  IF(ST.NE.0) CALL OPS('Error setting event flag for GUILINK',ST,ST)
	  GUI_WORKER_DIE = 1
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
C if systyp is not 'LIVE', do something different
C
	IF(P(SYSTYP).NE.LIVSYS) GOTO 2000		! secondary processing
C
C if TCP_LINK is not UP, make it UP  
C
	IF(GUI_CHAN_SETUP.EQ.-1.AND.CON_PATH_THRU_SUPPRESS.EQ.0) THEN
	  CALL GUIQUE(GUI_PCONNECT,ST)
	  IF(ST.NE.0) CALL OPS('Error setting event flag for GUILINK',ST,ST)
	  CALL XWAIT(500, 1, ST)
	  GOTO 1000
	ENDIF
C
C	Assemble messages from Clients
C
	CALL GUICBUF(NR_BUFS)
	GOTO 1000
C
C Secondary system processing done here
C
2000	CONTINUE
C
C if TCP_LINK is not DOWN, make it DOWN  
C
	IF(GUI_CHAN_SETUP.NE.-1) THEN
	  CALL GUIQUE(GUI_DISCONNECT,ST)
	  IF(ST.NE.0) CALL OPS('Error setting event flag for GUILINK',ST,ST)
	ENDIF
C
	GOTO 1000				! continue at the beginning
C
9010	FORMAT('Error setting event for GUILINK:',I)
	END
