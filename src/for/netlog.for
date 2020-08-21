C
C PROGRAM NETLOG
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NETLOG.FOV                                   $
C  $Date::   17 Apr 1996 14:10:04                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C  
C *** Pre-Baseline Source - netlog.for ***
C
C V02 23-APR-91 JWE MODIFIED FOR NEW DCNPRO
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
C	NETWORK LOGGING PROGRAM
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	PROGRAM NETLOG
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
 	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DCNEVN.DEF'
	INCLUDE 'INCLIB:DN_LINK.DEF'
	INCLUDE 'INCLIB:DN_BLOCK.DEF'
C
	INCLUDE	'(LIB$ROUTINES)'
	INCLUDE '($SYSSRVNAM)'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	BUF_NO,				! BUFFER NUMBER
     *			NETMASK,
     *			ST,
     *			STATUS
C
        CHARACTER*4	GXEVNNAM
C
	LOGICAL*4	RELOOP				! LOOP AGAIN ?
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CALL COPYRITE & SNIF_AND_WRKSET.
C
	CALL COPYRITE
C
	CALL SNIF_AND_WRKSET
C
C 		NETRDY = ??? 0
C RESET		NETRDY = NETRDY_RESET
C NETLOG	wait for NETRDY_RESET  -> NETRDY_NETLOG
C NETMGR	wait for NETRDY_NETLOG -> NETRDY_NETMGR
C DISPAT	wait for NETRDY_NETMGR -> NETRDY_DISPAT
C
10	CONTINUE
	IF (NETRDY .LT. NETRDY_RESET .AND.
     *      NETRDY .LT. NETRDY_DISPAT) THEN
	  CALL XWAIT(1, 2, ST)				! WAIT A SECOND
	  GOTO 10
	ENDIF
C
	CALL NET_INI
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
	NETMASK = IBSET(0, MOD(NETIOTRAP, 32))
	NETMASK = IBSET(NETMASK, MOD(NET_EVENT, 32))
	NETRDY  = NETRDY_NETLOG				! LET NETMGR GO ...
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C MAIN PROCESSING ...
C PLACE TASK IN TRAP WAIT STATE.
C NOTE: TASK WILL STILL SERVICE ASTs WHILE WAITING FOR EVENT FLAGS TO BE SET.
C
C
100	CONTINUE
	STATUS = SYS$WFLOR(%VAL(DN_EVNTIMER), %VAL(NETMASK))
	STATUS = SYS$CLREF(%VAL(NET_EVENT))
	STATUS = SYS$CLREF(%VAL(NETIOTRAP))
C
C CHECK APPLICATION QUEUE FROM DECNET TO DETERMINE IF ANYTHING IS THERE.
C
200	CONTINUE
	RELOOP = .FALSE.
	CALL QUETRAP(0)
C
300	CONTINUE
	CALL RTL(BUF_NO, DCN_NETQUE, STATUS)
	IF (STATUS .NE. 2) THEN
	  CALL NET_DCN_RESP(DN_BUFFER(BUF_NO))
	  RELOOP = .TRUE.
	  GOTO 300
	ENDIF
C
C GO BACK AROUND.
C
	IF (RELOOP) THEN
	  GOTO 200
	ELSE
	  GOTO 100
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C END.
C
	END
