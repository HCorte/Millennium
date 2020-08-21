C
C SUBROUTINE DN_INIT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_INIT.FOV                                  $
C  $Date::   17 Apr 1996 12:58:30                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - dn_init.for ***
C
C V02 28-APR-92 JWE ADD RETRY COUNT FOR QUEUE INTERLOCK FAILURE
C V01 21-APR-91 Steve Sullivan, DEC
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C This routine will initialize all the DECnet headers and Queues
C It will Open the network mailbox and declare our network name 
C so we can accept connections from other systems
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE DN_INIT
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
        INCLUDE 'INCLIB:DCNEVN.DEF'
        INCLUDE 'INCLIB:DESNET.DEF'
        INCLUDE 'INCLIB:DN_LINK.DEF'			! DECNET STRUCTURES
        INCLUDE 'INCLIB:DN_BLOCK.DEF'			! DECNET DATA BLOCKS
C
	INCLUDE '($IODEF)'
        INCLUDE '(LIB$ROUTINES)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'

C
	INTEGER*4	DN_EVNMASK		!BITMAP OF ALL EVENTS SET
C
C LOCAL DECLARATIONS
C
	INTEGER*4	FUN_DESCRIP(2),			! DECLARE NAME FUNCTION
     *			I,
     *			J,
     *			NAM_DESCRIP(2),			! NETWORK NAME STRING
     *			STATUS				! QIO STATUS
C
        CHARACTER*4	GXEVNNAM        
C
       	CHARACTER*1	INDEX				! LOGICAL NAME STRING
C
        RECORD /DN_FUNC_STRUCT/ FUNC			! NET NAME
C
	RECORD /DN_MBX_STRUCT/  MBXBUF			! MAILBOX MBXBUF ARG
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CREATE THE COMMON EVENT FLAG CLUSTER.
C
	STATUS = SYS$ASCEFC(%VAL(DN_EVNTIMER),
     *                      GXEVNNAM() // DN_EVNNAME, 0, 0)
	IF (.NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
C CREATE THE EVENT FLAG MASK OF EVENTS FOR WHICH TO TRAP ON.
C
        DN_EVNMASK = IBSET(0, MOD(DCN_EVENT, 32))
C
C CLEAR ALL EVENT FLAGS.
C
	STATUS = SYS$CLREF(%VAL(NET_EVENT))		! CLEAR NETLOG EF
C
C INITIALIZE QUEUE INTERLOCK RETRY COUNT
C
	DN_QUEUE_INTERLOCK_RETRY_COUNT = 1
C
C INITIALIZE DECNET QUEUES FOR BUFFERS
C
	CALL DEFLST(DCN_FREE,   NETNUM * NETSYS)
	CALL DEFLST(DCN_QUE,    NETNUM * NETSYS)
	CALL DEFLST(DCN_NETQUE, NETNUM * NETSYS)
C
C INITIALIZE AND STICK BUFFER HEADERS ONTO THE FREE QUEUE
C
	DO 100 I = 1, NETNUM * NETSYS
	  DN_BUFFER(I).QUEUE(1) = 0		! INITIALIZE QUEUE LINKS TO ZERO
	  DN_BUFFER(I).QUEUE(2) = 0		!...
	  DN_BUFFER(I).BUF_NO   = I
	  CALL DN_FREEBUF(I, STATUS)
	  IF (STATUS .NE. 0) CALL OPS('*** DN_INIT ***', 0, 0)
100	CONTINUE
C
C INITIALIZE DECNET LINK BLOCKS
C
	DO 300 I = 1, NETSYS
	  DN_LINK(I).MSGXMT       = 0
	  DN_LINK(I).BYTXMT       = 0
	  DN_LINK(I).MSGRCV       = 0
	  DN_LINK(I).BYTRCV       = 0
	  DN_LINK(I).MSGOUT       = 0
	  DN_LINK(I).MSGOUTMAX    = 0
	  DN_LINK(I).LAST_COMMAND = 0
	  DN_LINK(I).LAST_ERROR   = 0
	  DN_LINK(I).STATE        = STATE_DOWN		! STATE OF OUR LINK
C
C GET BLOCK'S NODE NAME FROM THE LOGICAL GTECH_REMSYSn
C
	  ENCODE(1, 9000, INDEX) I
C
	  CALL DN_GET_LOGICAL('GTECH_REMSYS' // INDEX,	! LOGICAL TO TRANSLATE
     *                        DN_LINK(I).NODE,		! WHAT WE GOT
     *                        32,			! MAX SIZE OF RESULT
     *                        DN_LINK(I).NODELEN)	! ACTUAL SIZE RETURNED
C
C STRING -> 0 = NO TRANSLATION
C
	  IF (DN_LINK(I).NODELEN .EQ. 0) THEN
	    CALL OPS('*** DN_INIT - CAN''T TRANSLATE GTECH_REMSYS% ***',
     *               I, 0)
	    DN_LINK(I).NODE    = 'SYS' // INDEX		! DEFAULT THE STRING
	    DN_LINK(I).NODELEN = LEN('SYS' // INDEX)
	  ENDIF
C
C GET BLOCK'S NETWORK NAME FROM THE LOGICAL GTECH_REMTASKn
C
	  CALL DN_GET_LOGICAL('GTECH_REMTASK' // INDEX,	! LOGICAL TO TRANSLATE
     *                        DN_LINK(I).TASK,		! WHAT WE GOT
     *                        12,			! MAX SIZE OF RESULT
     *                        DN_LINK(I).TASKLEN)	! ACTUAL SIZE RETURNED
C
C STRING -> 0 = NO TRANSLATION
C
	  IF (DN_LINK(I).TASKLEN .EQ. 0) THEN
	    CALL OPS('*** DN_INIT - CAN''T TRANSLATE GTECH_REMTSK% ***',
     *               I, 0)
	    DN_LINK(I).TASK    = 'GTECH' // INDEX	! DEFAULT THE STRING
	    DN_LINK(I).TASKLEN = LEN('GTECH' // INDEX)
	  ENDIF
C
	  DN_LINK(I).NCBLEN  = 0		! ZERO BYTES USED IN NCB 
	  DN_LINK(I).CHANNEL = 0		! CLEAR DECNET LINK I/O CHANNEL
	  DN_LINK(I).MSGXMT  = 0		! ZERO MESSAGES SENT
	  DN_LINK(I).BYTXMT  = 0		! ZERO BYTES SENT
	  DN_LINK(I).MSGRCV  = 0		! ZERO MESSAGES RECEIVED
	  DN_LINK(I).BYTRCV  = 0		! ZERO BYTES RECEIVED
	  DN_LINK(I).MSGOUT  = 0		! ZERO MESSAGES QUEUED FOR AN
C						!   AST TO COMPLETE
	  DN_LINK(I).MSGOUTMAX = 0		! ZERO HIGHWATER MARK OF MSGOUT
C
	  DO 200 J = 0, STATE_MAX		! CLEAR STATE COUNTERS
	    DN_LINK(I).STATE_COUNT(J) = 0	! TIMES A STATE WAS ENTERED
	    DN_LINK(I).STATE_TIME(J)  = 0	! TIME STATE WAS LAST ENTERED
200	  CONTINUE
C
	  DN_LINK(I).LAST_ERROR = 0		! CLEAR LAST ERROR ENCOUNTERED
	  DN_LINK(I).NEXT_LINK  = 0		! CLEAR POINTER TO NEXT DN_LINK	
300	CONTINUE
C
C INITIALIZE OUR DECNET SYSTEM MAILBOX AND ASSIGN DECNET CONTROL CHANNEL
C
	DN_SYS.MBXNAME      = 'DN_MBX'		! INITIALIZE DECNET MAILBOX NAME
        DN_SYS.MBXNAME_SIZE = 6			! NUMBER OF CHARS IN MBXNAME
C
	STATUS = SYS$CREMBX(,			! PERMANENT FLAG
     *                      DN_SYS.MBXCHAN,	! MAILBOX I/O CHANNEL
     *                      ,			! DEFAULT MAX
     *                      ,			! DEFAULT BUFFER QUOTA
     *                      ,			! DEFAULT PROTECTION MASK
     *                      ,			! DEFAULT ACCESS MODE (USER)
     *                      DN_SYS.MBXNAME)	! MAILBOX LOGICAL NAME
C
	IF (STATUS .NE. SS$_NORMAL) THEN
	  CALL OPS('*** FATAL: DN_INIT - ' //
     *             'CAN''T OPEN NETWORK MAILBOX ***', 0, 0)
	  STOP
	ENDIF
C
	STATUS = SYS$ASSIGN('_NET:',		! ASSIGNING DECNET NETWORK
     *                      DN_SYS.NETCHAN,	! NETWORK CHANNEL
     *                      ,			! ACCESS MODE (USER)
     *                      DN_SYS.MBXNAME)	! MAILBOX LOGICAL NAME
C
	IF (STATUS .NE. SS$_NORMAL) THEN
	  CALL OPS('*** FATAL: DN_INIT - ' //
     *             'CAN''T ASSIGN NET CHANNEL ***', 0, 0)
	  STOP
	ENDIF
C
C GET THE DEVICE UNIT FOR THIS CHANNEL
C
	CALL DN_GET_UNIT(DN_SYS.NETCHAN, DN_LINK(NODEID).UNIT)
C
C GET OUT NETWORK NAME FROM THE LOGICAL GTECH_NETNAME
C
	ENCODE(1, 9000, INDEX) NODEID			! GET DEFAULT UNIT...
C
	CALL DN_GET_LOGICAL('GTECH_NETNAME',		! LOGICAL TO TRANSLATE
     *                      DN_SYS.TSKNAM,		! WHAT WE GOT
     *                      12,				! MAX SIZE OF RESULT
     *                      DN_SYS.TSKNAM_SIZE)		! ACTUAL SIZE RETURNED
C
C STRING -> 0 = NO TRANSLATION
C
	IF (DN_SYS.TSKNAM_SIZE .EQ. 0) THEN
	  DN_SYS.TSKNAM      = 'GTECH' // INDEX		! DEFAULT THE STRING
	  DN_SYS.TSKNAM_SIZE = LEN('GTECH' // INDEX)
	  CALL OPS('*** DN_INIT - NETWORK NAME IS: ' //
     *             DN_SYS.TSKNAM(:DN_SYS.TSKNAM_SIZE), NODEID, 0)
	ENDIF
C
C SET UP AND DECLARE OUR NETWORK NAME SO WE CAN ACCEPT INCOMING CONNECTIONS
C
	FUNC.FUNC      = NFB$C_DECLNAME 		! FUNCTION BLOCK
	FUNC.FREEWORD  = 0				! ARG IS ZERO FOR TASK
	FUN_DESCRIP(1) = 5				! SIZE IN BYTES...
	FUN_DESCRIP(2) = %LOC(FUNC)			! PTR TO FUNC BLOCK...
	NAM_DESCRIP(1) = DN_SYS.TSKNAM_SIZE		! LEN OF TASK NAME STR
	NAM_DESCRIP(2) = %LOC(DN_SYS.TSKNAM)		! PTR TO TASK NAME STR
C
C DECLARE THE NETWORK NAME
C
	STATUS = SYS$QIOW(,				! EVENT FLAG
     *                    %VAL(DN_SYS.NETCHAN),		! CHANNEL
     *                    %VAL(IO$_ACPCONTROL),		! FUNCTION
     *                    %REF(DN_SYS.IOSB),		! IOSB
     *                    ,				! AST ADDRESS
     *                    ,				! AST PARAMETER
     *                    FUN_DESCRIP,			! P1
     *                    NAM_DESCRIP,			! P2
     *                    ,				! P3
     *                    ,				! P4
     *                    ,				! P5
     *                    )				! P6
C
	IF(STATUS .NE. SS$_NORMAL) THEN
	  CALL OPS('*** FATAL: DN_INIT - ' //
     *             'DECLARE NETWORK NAME FAILED ***', 0, 0)
	  STOP
C
	ELSE IF (DN_SYS.IOSB.STAT .NE. SS$_NORMAL) THEN
	  IF (DN_SYS.IOSB.STAT .EQ. SS$_NOPRIV) THEN
	    CALL OPS('*** FATAL: DN_INIT - ' //
     *               'DOES NOT HAVE SYSNAM PRIVILEGE ***', 0, 0)
	    STOP
	  ENDIF
C
	  CALL OPS('*** FATAL: DN_INIT - ' //
     *             'CAN''T DECLARE NETWORK NAME ***',
     *             DN_SYS.IOSB.STAT, DN_SYS.IOSB.STAT)
	  STOP
	ENDIF
C
C QUEUE A MAILBOX READ SO WE'LL KNOWN WHEN SOMETHING HAPPENS
C
	CALL QUEMBX(MBXBUF)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FORMAT STATEMENTS.
C
9000	FORMAT(I1)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
