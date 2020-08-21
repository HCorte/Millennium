C
C SUBROUTINE NET_UPDSTA
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NET_UPDSTA.FOV                               $
C  $Date::   17 Apr 1996 14:11:52                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - net_updsta.for ***
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
C	UPDATE NETWORK STATS: DELAYS, XMIT AND RECEIVE COUNTS.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE NET_UPDSTA
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DN_LINK.DEF'			! DECNET STRUCTURES
	INCLUDE 'INCLIB:DN_BLOCK.DEF'			! DECNET DATA BLOCKS
C
C LOCAL DECLARATIONS
C
	INTEGER*4	CURRENT_INTERVAL	/0/,
     *			CURRENT_OFFSET,
     *			NODE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C THE FOLLOWING STATEMENT IS NOT ABSOULTELY NECESSARY UNDER VAX FORTRAN ...
C HOWEVER, IT WOULD BE REQUIRED TO COMPLY WITH THE ANSI F77 SPECIFICATION.
C
	SAVE		CURRENT_INTERVAL
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	CURRENT_INTERVAL = CURRENT_INTERVAL + 1
C
	IF (CURRENT_INTERVAL .LT. NET_UPDSTA_INTERVAL) GOTO 9999
C
	CURRENT_INTERVAL = 0
C
	CURRENT_OFFSET = NET_CURRENT_XFER_OFFSET + 1
C
	IF (CURRENT_OFFSET .GT. 2) CURRENT_OFFSET = 1
C
C COPY DELAYS.
C
	CALL FASTMOV(NET_XFER_TIME(0, 1),
     *               NET_XFER_LAST_DELAYS(0, 1, CURRENT_OFFSET),
     *               (NET_XFER_TIME_MAX + 1) * NETSYS)
C
C COPY NUMBER OF MESSAGES TRANSFERRED
C
	DO 100 NODE = 1, NETSYS
	  NET_RECV_MSGS_CURRENT(CURRENT_OFFSET, NODE) =
     *    DN_LINK(NODE).MSGRCV
C
	  NET_XFER_MSGS_CURRENT(CURRENT_OFFSET, NODE) =
     *    DN_LINK(NODE).MSGXMT
100	CONTINUE
C
	NET_CURRENT_XFER_OFFSET = CURRENT_OFFSET	
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
