C SUBROUTINE QUETRAP
C
C V01 17 Apr 1996 HXK INITIAL RELEASE
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 2000 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	PROCESS NEXT REQUEST ...
C	  WHAT ABOUT FAILURE SYNCHRONIZATION?
C	  IF DRIVER IS NOT READY NEXT I/O SHOULD BE STARTED FROM DONE ROUTINE.
C         I.E. IOTRAP SHOULD CALL QUETRAP IN LOOP QUETRAP(PAR).
C
C Input:
C	PAR - IF !0, PROCESS JUST 1 I/O
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE QUETRAP(PAR)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DN_LINK.DEF'
	INCLUDE 'INCLIB:DN_BLOCK.DEF'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	BUF,				! TRAPPING BUFFER #
     *			BUF1,
     *			CHECK_BUF,
     *			COMMAND,
     *			FC,
     *			FINAL,
     *			HALTBLOK(7, NETLUN),		! PBLK FOR HALT I/O
     *			HALTIO,
     *			LASTFRZ			/0/,	! ID OF LAST FREEZE
     *			LENGTH,
     *			MAX_OUTSTANDING_WRITES,
     *			NBYTES,
     *			NODE,
     *			NODE_OUTSTANDING_WRITES,
     *			NODE_USEID(NETSYS)	/NETSYS * 13/,
     *			OFF,
     *			PAR,
     *			PBLOCK(7, NETNUM + 1, NETLUN),	!I/O PROCEED PARAM BLOCK
     *			QUECNT,
     *			RECOVERED,
     *			RLGSER/0/,    ! V02 NOT INITIALIZED AT ALL BEFORE!!!
     *			RND,
     *			SRN,
     *			ST,
     *			START,
     *			TYPE,
     *			UNIT,
     *			WAY,
     *			WRGO,
     *			XOPT
C
C COMMON DECLARATIONS
C
	COMMON /TEST_CHECK/ CHECK_BUF
	COMMON /PBLOCKS/    PBLOCK
	COMMON /RCVRYNODE/  RECOVERED
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IF LAST PHASE OF RECOVERY NOT ON MASTER SYSTEM
C IF LAST BLOCK IN FILE XFER MODE UNFREEZE DATA
C QUEUE BUFFER TO NETFINISH AND START LAST PHASE
C
	IF (NETCMDFRZ .EQ. 1001) THEN
C
C TRY TO SEND TO EXEC QUEUE AND TO NETMGR FREEZE
C TO NETLOG JUST TO DEQUEUE REMAINING BUFS,
C TO NETMGR FREEZE FINISH QUEUE
C
	  CALL EXTRABUF(BUF, RECOVWAY, ST)		! TRY TO GET A BUFFER.
	  IF (ST .EQ. 2) GOTO 100			! SKIP IF CANNOT.
C
	  CALL EXTRABUF(BUF1, RECOVWAY, ST)		! TRY TO GET A BUFFER.
	  IF (ST .EQ. 2) THEN
	    CHECK_BUF = 701
	    CALL FREEBUF(BUF)
	    GOTO 100
	  ENDIF
C
	  NETCMDFRZ = 1000
C
	  NETBUF(WAYNR, BUF) = RECOVWAY
	  NETBUF(MODE,  BUF) = FRZMD			! STOP DQING FR FINISH Q
C
	  CALL ABL(BUF, NETFINISH, ST)			! SHLD ALWAYS FIND ROOM
C
	  NETBUF(WAYNR, BUF1) = RECOVWAY
	  NETBUF(MODE,  BUF1) = FRZMD			! STOP DQING FR FINISH Q
C
	  LASTFRZ = LASTFRZ + 1
C
	  NETBUF(HDRSIZ+1, BUF1) = LASTFRZ
C
	  CALL ABL(BUF1, NETEXEC(1, RECOVWAY), ST)	! SHLD ALWAYS FIND ROOM
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
100	CONTINUE
	IF (NETCMDFRZ .EQ. 998) THEN			! XFERRED IN FILE MODE
C							! AND EXEC QUEUE FLUSHED
	  NETSER(RECOVERED,  RECOVWAY) = 0
	  NETMODE(RECOVERED, RECOVWAY) = TRNMD
C
	  SRN = NXTSER
	  IF (NETATR(RECOVWAY) .EQ. RLG .AND.
     *        NETMASTER(RECOVWAY) .NE. NODEID) SRN = RLGSER
C
	  CALL NOTIFY1(RECOVERED, NOTFIN, SRN, RECOVWAY)
C
	  IF (NODEID .EQ. NETMASTER(RECOVWAY)) P(CMDFRZ) = 0
C
	  NETCMDFRZ = 0
C
C RELEASE OUTSTANDING RECOVERY BUFFERS.
C
150	  CONTINUE
	  CALL RTL(BUF, RECOVQUE, ST)
	  IF (ST .NE. 2) THEN
	    CHECK_BUF = 702
	    CALL FREEBUF(BUF)
	    GOTO 150
	  ENDIF
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IF NO I/O IN TRANSACTION MODE,
C TRY TO EXECUTE REQUEST FROM EXECUTE LIST.
C
	DO 300 WAY = 1, NUMWAY
	  IF (NETCMDFRZ .EQ. 999 .AND. WAY .EQ. RECOVWAY) GOTO 300
	  ST = 2
C
C DO NOT SEND IF MORE THAN 2 OUTSTANDING WRITES TO ANY ACTIVE SYSTEM
C IN REGULAR MODE (NOT RECOVERY).
C
	  MAX_OUTSTANDING_WRITES = 0
C
	  DO 200 OFF = 1, NETSYS
	    IF (NETSTAT(OFF, WAY) .EQ. NSTAPRIM .AND.
     *          NETROUT(OFF, WAY) .EQ. ROUACT .AND.
     *          NETMODE(OFF, WAY) .EQ. TRNMD) THEN
	      NODE_OUTSTANDING_WRITES = QUECNT(NET_IOSTARTED(1, 1, OFF))
     *                                + QUECNT(NET_IOSTARTED(1, 2, OFF)) 
	      IF (NODE_OUTSTANDING_WRITES .GT. MAX_OUTSTANDING_WRITES)
     *          MAX_OUTSTANDING_WRITES = NODE_OUTSTANDING_WRITES
	    ENDIF
200	  CONTINUE
C
	  IF (MAX_OUTSTANDING_WRITES .LE. 3) THEN
	    CALL RTL(BUF, NETEXEC(1, WAY), ST)
	    IF (ST .NE. 2) THEN
	      IF (NETBUF(MODE, BUF) .NE. FRZMD) GOTO 500
	      IF (NETCMDFRZ .EQ. 1000 .AND.
     *            NETBUF(HDRSIZ+1, BUF) .EQ. LASTFRZ) NETCMDFRZ = 999
	      CHECK_BUF = 703
	      CALL FREEBUF(BUF)				! RELEASE BUFFER
	    ENDIF
	  ENDIF
300	CONTINUE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IF NO TRANSACTION IN FILE MODE,
C TRY TO EXECUTE NEXT REQUEST FROM "FILE" MODE QUEUE
C
	DO 400 WAY = 1, NUMWAY
	  ST = 2
	  CALL RTL(BUF, NETFIL(1, WAY), ST)
	  IF (ST .NE. 2) GOTO 500
400	CONTINUE
	GOTO 9999
C
500	CONTINUE
	TYPE = NETBUF(MODE, BUF)			! GET REQUEST TYPE.
	IF (TYPE .EQ. DRVMD) THEN			! IF DRIVER REQUEST.
	  COMMAND = NETBUF(HDRSIZ+1, BUF)		! ADD OR REMOVE LINK.
	  NODE    = NETBUF(HDRSIZ+2, BUF)
	  NET_LAST_TRANS_SENT(NODE) = 0
	  NET_LAST_ACK_TIME(NODE)   = 0
C
C REMOVE THE LINK
C
	  IF (COMMAND .EQ. REMLINK) THEN		! IF REMLINK.
	    IF (NODE .EQ. NETBACKUP(WAY)) NETBACKUP(WAY) = 0
C
C HALT I/O AND CLOSE UNIT
C
	    RND    = 0
	    HALTIO = '80'X    
	    UNIT   = NODE
	    XOPT   = 0
C
	    HALTBLOK(7, UNIT) = WAY
C
	    CALL SYSIO(HALTBLOK(1, UNIT),
     *                 HALTIO,
     *                 UNIT,
     *                 NETBUF(MODE, BUF),
     *                 (NETBUF(NEXT, BUF) - NCNLEN) * 4,
     *                 RND,
     *                 XOPT)
	    NETREM(NODE, WAY) = 0			! MARK NODE AS REMOVED.
C
C ELSE ...
C   OPEN UNIT AND INITIALIZE MODE - MASTER/SLAVE.
C   OPEN ALL CONNECTIONS AND INITIALIZE THEM.
C
	  ELSE
	    IF (NETBUF(HDRSIZ+3, BUF) .EQ. NSTASEC)
     *        NETMODE(NODEID, WAY) = TRNMD
C
	    NETDUMM(NODE,  WAY) = 0
	    NETERR(NODE,   WAY) = 0
	    NETMODE(NODE,  WAY) = TRNMD
	    NETRECOV(NODE, WAY) = 0
	    NETROUT(NODE,  WAY) = ROUIDLE	! AVOID SETTING SYS TO ACTIVE
C						! IF SYS HAS NO LOGICAL CONNECT.
	    NETSER(NODE,   WAY) = 0
	    NETTIM(NODE,   WAY) = 0
C
	    UNIT = NODE

C
C INITIALIZE TO PRIMARY SECONDARY MODE NOW
C
	    IF (NETDEV(1, UNIT) .NE. 0) THEN
	      NBYTES = 0
	      RND    = 0
	      START  = 0				! NOT USED.
	      XOPT   = NODE				! NODE TO OPEN.
C
	      IF (NETBUF(HDRSIZ+3, BUF) .EQ. NSTAPRIM) FC = '000000C0'X
	      IF (NETBUF(HDRSIZ+3, BUF) .EQ. NSTASEC)  FC = '000000A0'X
C
	      DO 600 OFF = 1, 6
		PBLOCK(OFF, NETNUM+1, UNIT) = 0
600	      CONTINUE
C
	      PBLOCK(7, NETNUM+1, UNIT) = WAY
C
	      CALL SYSIO(PBLOCK(1, NETNUM+1, UNIT),
     *                   FC,
     *                   UNIT,
     *                   START,
     *                   NBYTES,
     *                   RND,
     *                   XOPT)
	    ENDIF
	  ENDIF
C
	  CHECK_BUF = 704
	  CALL FREEBUF(BUF)				! FREE THIS BUFFER
	  GOTO 100					! DQ NEXT REQ.
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C SHOULD SEND DATA NOW.
C
	NODE = NETBUF(PDEST, BUF)			! DESTINATION UNIT
	NETBUF(ACTFROM, BUF) = NODEID
C
C ZERO # OF I/O
C
	IF (NETBUF(PPORG, BUF) .EQ. NODEID) NETIOCNT(BUF) = 0
	CALL TSTCHG2(BUF, 0, FINAL)
C
	LENGTH = NETBUF(NEXT, BUF)
	IF (LENGTH .LE. HDRSIZ .OR. LENGTH .GT. NETLEN - 1) THEN
	  CHECK_BUF = 705
	  CALL FREEBUF(BUF)
	  GOTO 100
	ENDIF
C
	IF (NODE .EQ. 0) THEN				! IF TO ALL SCNDRY DEVS
	  NETBUF(NETBUF_TIMOFF, BUF) = P(ACTTIM)
C
	  DO 700 OFF = 1, NETSYS
	    IF (NETSTAT(OFF, WAY) .EQ. NSTAPRIM .AND.
     *          NETROUT(OFF, WAY) .EQ. ROUACT) THEN
C
	      IF (NETBUF(MODE, BUF) .EQ. TRNMD .AND.
     *            NETMODE(OFF, WAY) .NE. TRNMD) GOTO 700
C
	      IF (NETBUF(MODE, BUF) .EQ. TRNMD)
     *          NET_LAST_TRANS_SENT(OFF) = P(ACTTIM)
C
	      UNIT = OFF	
C
C ONLY TRY TO SEND IF THE SYSTEM IS KEEPING UP ...
C
	      CALL TSTCHG2(BUF, 1, FINAL)		! INCREMENT I/O COUNT
C
C DO SVC WRITE NOW TO DEV OFF.
C
	      RND  = 0
	      WRGO = '20'X
	      XOPT = BUF
C
	      PBLOCK(7, BUF, UNIT) = WAY
C
	      SNDIOCHK(UNIT) = SNDIOCHK(UNIT) + 1
C
	      CALL SYSIO(PBLOCK(1, BUF, UNIT),
     *                   WRGO,
     *                   UNIT,
     *                   NETBUF(NCNLEN+1, BUF),
     *                   (NETBUF(NEXT, BUF) - NCNLEN) * 4,
     *                   RND,
     *                   XOPT)
	    ENDIF
700	  CONTINUE
C
	  IF (NETBUF(PPORG, BUF) .NE. NODEID)
     *      CALL TSTCHG2(BUF, -1, FINAL)
C
	  CHECK_BUF = 706
	  IF (FINAL .EQ. 0) CALL FREEBUF(BUF)		! IF NOTHING ACTIVE ...
	  GOTO 100					! TRY TO REPEAT REQUEST.
	ELSE
	  IF (NETROUT(NODE, WAY) .NE. ROUACT) THEN
	    CHECK_BUF = 707
	    CALL FREEBUF(BUF)
	    GOTO 100
	  ENDIF
C
	  UNIT = NODE
	  IF (UNIT .LT. 1 .OR.
     *        UNIT .GT. NETSYS) GOTO 9999		! INVALID CONNECTION
C
	  NETIOCNT(BUF) = 1
C
C ??? THIS SHOULD NOT BE HERE ???
C
	  NETBUF(USEID, BUF) = NODE_USEID(NODE) + 1
	  NODE_USEID(NODE)   = NETBUF(USEID, BUF)
C
C DO WRITE NOW
C
	  RND  = 0
	  WRGO = '20'X
	  XOPT = BUF
C
	  PBLOCK(7, BUF, UNIT) = WAY
C
	  SNDIOCHK(UNIT) = SNDIOCHK(UNIT) + 1
C
	  CALL SYSIO(PBLOCK(1, BUF, UNIT),
     *               WRGO,
     *               UNIT,
     *               NETBUF(NCNLEN+1, BUF),
     *               (NETBUF(NEXT, BUF) - NCNLEN) * 4,
     *               RND,
     *               XOPT)
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C LOOP BACK AROUND.
C
	GOTO 100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
