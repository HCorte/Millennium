C
C SUBROUTINE NET_INI
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NET_INI.FOV                                  $
C  $Date::   18 Dec 1996 12:00:30                                         $
C  $Revision::   1.1                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - net_ini.for ***
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
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE NET_INI
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
	INTEGER*4	OFF,
     *			OFF1
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C DEFINE ALL LISTS NOW.
C
	DO 100 OFF = 1, NUMWAY
	  CALL DEFLST(NETFREE(1, OFF),  NETNUM)
	  CALL DEFLST(NETFIL(1, OFF),   NETNUM)
	  CALL DEFLST(NETEXTRA(1, OFF), NETSYS * 2)
	  CALL DEFLST(NETEXEC(1, OFF),  MAXQUE)
100	CONTINUE
C
	DO 200 OFF = 1, NETSYS
	  CALL DEFLST(NET_IOSTARTED(1, 1, OFF), NETNUM)
	  CALL DEFLST(NET_IOSTARTED(1, 2, OFF), NETNUM)
	  CALL DEFLST(NETFREE_RECOVERY(1, OFF), NETNUM)
	  CALL DEFLST(NETIODONE(1, OFF),        NETNUM)
C
	  CALL FASTSET(0, NET_XFER_TIME(0, OFF),
     *                    NET_XFER_TIME_MAX + 1)
	  CALL FASTSET(0, NET_XFER_LAST_DELAYS(0, OFF, 1),
     *                    NET_XFER_TIME_MAX + 1)
	  CALL FASTSET(0, NET_XFER_LAST_DELAYS(0, OFF, 2),
     *                    NET_XFER_TIME_MAX + 1)
200	CONTINUE
C
	CALL FASTSET(0, NET_XFER_MSGS_CURRENT,   NETSYS * 2)
	CALL FASTSET(0, NET_RECV_MSGS_CURRENT,   NETSYS * 2)
	CALL FASTSET(0, SNDIOCHK,                NETSYS)
	CALL FASTSET(0, READIOCHK,               NETSYS)
	CALL FASTSET(0, NET_TIMES_RESPONSE_LONG, NETSYS)
	CALL FASTSET(0, NET_LAST_ACK_TIME,       NETSYS)
	CALL FASTSET(0, NET_LAST_TRANS_SENT,     NETSYS)
C
C DEFINE QUEUES.
C
	CALL DEFLST(NETFINISH, MAXQUE)
	CALL DEFLST(REMFINISH, NETNUM)
	CALL DEFLST(RECOVQUE,  NETNUM)
C
C INITIALIZE NETBUF.
C
	DO 300 OFF = 1, NETNUM
	  NETFROM(OFF)              = 0
	  NETIOCNT(OFF)             = 0
	  NET_LAST_BUFFER_USER(OFF) = 0
	  CALL FASTSET(0, NETBUF(1, OFF), NETLEN)
300	CONTINUE
C
	DO 500 OFF1 = 1, NUMWAY
	  DO 400 OFF = 1, NETSYS
	    NETDUMM(OFF,  OFF1) = 0
	    NETERR(OFF,   OFF1) = 0
	    NETHSER(OFF,  OFF1) = 0
	    NETMODE(OFF,  OFF1) = TRNMD
	    NETRECOV(OFF, OFF1) = 0
	    NETREM(OFF,   OFF1) = 0			! OUT NEXT RELEASE
	    NETSER(OFF,   OFF1) = 0
	    NETTIM(OFF,   OFF1) = 0
	    REMCLOSED(OFF)      = 0			! OUT IN NEXT RELEASE
400	  CONTINUE
	  NETBACKUP(OFF1)  = 0
	  NODEMASTER(OFF1) = 0				! MY CURRENT PRIMARY
500	CONTINUE
C
	FREEZDUMM = 0					! DUMMIES COULD BE SENT
	FREEZQUEU = 0					! DON'T FREEZE FINISH Q
	NETCMDFRZ = 0					! RCVRY FREEZE CMD STAGE
	NETTIMER  = 0					! NETWORK TIMER
	NETTST    = 0					! TEST MODE IS OFF
C
	NET_CURRENT_XFER_OFFSET = 1			! INDEX
C
	IF (P(CMDFRZ) .GE. 2) P(CMDFRZ) = 0
C
C INITIALIZE TASK QUEUE FOR NETLOG.
C
	CALL DEFLST(DCN_NETQUE, NETNUM * NETSYS)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C TO ADJUST.
C
	NETWAIT         =   125		! INIT WAIT VALUE. ADJUSTED BY NETMGR.
	NETWAIT_MIN     =    50		! MINIMUM TIME THAT NETWAIT  WILL WAIT.
	SYNCWAIT        =   125		! INIT WAIT VALUE. ADJUSTED BY NETMGR.
	SYNCWAIT_MIN    =    50		! MINIMUM TIME THET SYNCWAIT WILL WAIT.
	NET_MAX_TIMEOUT =     6		! # OF SECONDS TO GENERATE TIMEOUT
	TOFREEZ         =  1500		!   DURING RECOVERY FREEZE SYSTEM
C					!   IF SO MANY XACTIONS TO XFER LEFT.
	IDLETIM         = 45000		! NO TAKEOVER IF BACKUP IDLE THIS LONG.
	MAXFREEZ        =  4000		! NON-MASTER MAY FREEZE FOR THIS LONG.
	TOUNFREEZ       = 20000         ! IF UNFINISHED, WILL UNFREEZ ITSSELF
C					! FOR THIS LONG.
C MONITORING STATS.
C
	NET_SLOW_REPORT_INTERVAL =   10	! 10 RUNS OF NETMON.
	NET_FIRST_REPORT_DELAY   =    2	! DELAY AT LEAST 2 RUNS AFTER UNFREEZE.
	NET_UPDSTA_INTERVAL      =  120	! UPDATE STATS EVERY 120 RUNS.
	NET_START_RESYNC         =90000	! 90000, SET 30000 FOR TESTING ...
C					!   WAIT TO START SENDING RESYNC DATA.
	NET_MAX_ACKTIM           =    1	! RESPONSE TIME HAS TO BE <= 1 SEC.
C
C OTHER STUFF.
C
	MAXIDLE          = 1500		! EVERY XXXX MSEC DUMMY WILL BE SENT TO
C					!   SECONDARY IF NO TRAFFIC.
	NET_MAX_RESPONSE =    2		! ACK SHOULD COME BACK IN 2 SECS.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
	RETURN
	END
