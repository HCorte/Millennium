C
C PROGRAM NETMON
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NETMON.FOV                                   $
C  $Date::   17 Apr 1996 14:10:22                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - netmonitor.for ***
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	MONITOR NETLOG, DCNPRO AND NETMGR ACTIVITY.
C	THIS TASK WILL GENERATE TIMEOUTS FOR ALL NETLOG I/Os
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	PROGRAM NETMON
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DCNEVN.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DN_LINK.DEF'
	INCLUDE 'INCLIB:DN_BLOCK.DEF'
C
	INCLUDE	'(LIB$ROUTINES)'
        INCLUDE '($SYSSRVNAM)'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	CHKFRZ_STATUS,
     *			CHKSLOW_STATUS,
     *			CHKSTS_STATUS,
     *			INTERVAL	/500/,
     *			NODE,
     *			NODE_STATUS,
     *			ST,
     *			STATUS
C
        CHARACTER*4	GXEVNNAM
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CALL COPYRITE & SNIF_AND_WRKSET.
C
	CALL COPYRITE
C
	CALL SNIF_AND_WRKSET
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CREATE THE COMMON EVENT FLAG CLUSTER.
C
	STATUS = SYS$ASCEFC(%VAL(DN_EVNTIMER),
     *                      GXEVNNAM() // DN_EVNNAME, 0, %VAL(1))
	IF (.NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
C WAIT TILL NETMGR UP
C
C 		NETRDY = ??? 0
C RESET		NETRDY = NETRDY_RESET
C NETLOG	wait for NETRDY_RESET  -> NETRDY_NETLOG
C NETMGR	wait for NETRDY_NETLOG -> NETRDY_NETMGR
C DISPAT	wait for NETRDY_NETMGR -> NETRDY_DISPAT
C
10	CONTINUE
	IF (NETRDY .LT. NETRDY_NETLOG. AND.
     *      NETRDY .LT. NETRDY_DISPAT .AND.
     *      NETRDY .LT. NETRDY_NETMGR) THEN
	  CALL XWAIT(1, 2, ST)				! WAIT A SECOND
	  GOTO 10
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C MAIN LOOP ...
C   WAKE UP DCNPRO AND NETLOG EVERY INTERVAL TIME
C   (JUST IF ANY TIMING PROBLEM IN THE WAKE UP LOGIC).
C
100	CONTINUE
	CALL XWAIT(INTERVAL, 1, ST)
C
	STATUS = SYS$SETEF(%VAL(NET_EVENT))
	STATUS = SYS$SETEF(%VAL(NETIOTRAP))
	STATUS = SYS$SETEF(%VAL(DCN_EVENT))
C
C UPDATE NETWORK STATS
C
	CALL NET_UPDSTA
C
C CHECK TIMEOUT AND GENERATE TIMEOUT MESSAGE
C CHECK IF I/O STUCK
C
	DO 200 NODE = 1, NETSYS
	  CALL NET_TIMEOUT(NODE, NET_IOSTARTED(1, 2, NODE),
     *                     NET_MAX_TIMEOUT, -1, NODE_STATUS)
	  IF (NODE_STATUS .NE. 0) THEN
	    IF (NETMODE(NODE, 1) .NE. FILMD)
     *        CALL OPS('*** DCNPRO - WRITE STUCK ***',
     *                 NODE, NODE_STATUS)
	    GOTO 200
	  ENDIF
C
C CHECK IF DCNPRO STUCK
C
	  CALL NET_TIMEOUT(NODE, NET_IOSTARTED(1, 1, NODE),
     *                     NET_MAX_TIMEOUT, 0, NODE_STATUS)
	  IF (NODE_STATUS .NE. 0) THEN
	    CALL OPS('*** DCNPRO - WRITE NOT RETRIEVED ***',
     *               NODE, NODE_STATUS)
	  ENDIF
200	CONTINUE
C
C CHECK IF ACK RECEIVED ON TIME
C
	DO 300 NODE = 1, NETSYS
	  CALL NET_CHKACK(NODE, NODE_STATUS)
300	CONTINUE	    
C
C CHECK IF NETLOG STATUSES AND DCNPRO STATUS CORRESPOND TO EACH OTHER.
C
	CHKSTS_STATUS = 0
	DO 400 NODE = 1, NETSYS
	  CALL NET_CHKSTS(NODE, NODE_STATUS)
	  IF (NODE_STATUS .NE. 0) THEN
	    CALL OPS('*** NETMON - System ***', NODE, NODE_STATUS)
	    CALL OPS('*** NETMON - STATUS NOT CORRECT ***',
     *               NETROUT(NODE, 1), DN_LINK(NODE).STATE)
	    CHKSTS_STATUS = -1
	  ENDIF
400	CONTINUE
C
C MAKE SURE NETLOG DOES NOT FREEZE THE SYSTEM LONGER THAN NEEDED.
C
	CALL NET_CHKFRZ(CHKFRZ_STATUS)
C
	CALL NET_CHKSLOW(CHKSLOW_STATUS)		! TELL ABOUT SLOW SYSTEM
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C LOOP BACK AROUND.
C
	GOTO 100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C END.
C
	END
