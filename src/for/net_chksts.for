C
C SUBROUTINE NET_CHKSTS
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NET_CHKSTS.FOV                               $
C  $Date::   17 Apr 1996 14:11:06                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - net_chksts.for ***
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
C	CHECK SOME NODE STATUSES: NETLOG AND DCNPRO STATUSES
C
C Calling Sequence:
C	CALL NET_CHKSTS(NODE, NODE_STATUS)
C
C Input:
C	NODE - NODE (SYSTEM NUMBER) TO CHECK
C
C Output:
C	NODE_STATUS - STATUS OF THE NODE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE NET_CHKSTS(NODE, NODE_STATUS)
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
        INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DN_LINK.DEF'
	INCLUDE 'INCLIB:DN_BLOCK.DEF'
C
	INTEGER*4	LAST_NODE_STATUS(NETSYS, 2)
     *					/NETSYS * 0, NETSYS * 0/,
     *			LAST_STATUS_INTERVAL(NETSYS)
     *					/NETSYS * 0/,
     *			NET_CHKSTS_BCK	/90/,		! 45 SECS FOR BACKUP
     *			NET_CHKSTS_MAX	/10/,		! CHECK EVERY 5 SECS
     *			NODE,
     *			NODE_STATUS
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	NODE_STATUS = 0
C
	IF (LAST_NODE_STATUS(NODE, 1) .NE. NETROUT(NODE, 1)) THEN
	  LAST_NODE_STATUS(NODE, 1)  = NETROUT(NODE, 1)
	  LAST_STATUS_INTERVAL(NODE) = 0
	  GOTO 9999
	ENDIF
C
	IF (LAST_NODE_STATUS(NODE, 2) .NE. DN_LINK(NODE).STATE) THEN
	  LAST_NODE_STATUS(NODE, 2)  = DN_LINK(NODE).STATE
	  LAST_STATUS_INTERVAL(NODE) = 0
	  GOTO 9999
	ENDIF
C
	LAST_STATUS_INTERVAL(NODE) = LAST_STATUS_INTERVAL(NODE) + 1
C
	IF (LAST_STATUS_INTERVAL(NODE) .LT. NET_CHKSTS_BCK .AND.
     *      DN_LINK(NODE).STATE .NE. STATE_RUNNING .AND.
     *      NODE .EQ. NETBACKUP(WAYINP)) GOTO 9999
C
	IF (LAST_STATUS_INTERVAL(NODE) .LT. NET_CHKSTS_MAX) GOTO 9999
C
	LAST_STATUS_INTERVAL(NODE) = 0
C
	IF ((NETROUT(NODE, 1) .EQ. ROUACT .AND. 
     *       DN_LINK(NODE).STATE .NE. STATE_RUNNING) .OR.
     *      (NETROUT(NODE,1) .NE. ROUACT .AND.
     *       DN_LINK(NODE).STATE .EQ. STATE_RUNNING)) THEN
	  NODE_STATUS = -1
	  GOTO 9999
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PLACE ANY ADDITIONAL CHECKS HERE.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
