C
C SUBROUTINE NET_CHKACK
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NET_CHKACK.FOV                               $
C  $Date::   17 Apr 1996 14:10:42                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C
C V02  PJS  19-DEC-95  ENSURE THAT NET_LAST_ACK_TIME() IS NOT ZERO AS A
C                      CONDITION OF CHECKING ACK DELAY TIME.
C V01  PJS  01-SEP-95  MODIFIED TO CHECK ACK RECEIVED TIME FROM WHEN THE
C                      BUFFER WAS SENT (NOT FROM THE LAST ACK RECEIVED TIME).
C                      THIS SHOULD GIVE A MORE ACCURATE INDICATION OF DELAYS.
C
C *** Pre-Baseline Source - net_chkack.for ***
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
C	CHECK AND GENERATE THE TIMEOUT FOR THE NODE
C
C Calling Sequence:
C	CALL NET_CHKACK(NODE, STATUS)
C
C Input:
C	NODE - NODE NUMBER TO CHECK
C
C Output:
C	STATUS
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE NET_CHKACK(NODE, NODE_STATUS)
C
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:GLIST.DEF'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	LAST_TIMEOUT_OK(NETSYS)		/NETSYS * 0/,
     *			NODE,
     *			NODE_STATUS
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	NODE_STATUS = 0
C
	IF (NET_LAST_ACK_TIME(NODE) .LT. NET_LAST_TRANS_SENT(NODE) .AND.
     *      NET_LAST_TRANS_SENT(NODE)+NET_MAX_RESPONSE.LT.P(ACTTIM).AND.
     *      NET_LAST_TRANS_SENT(NODE) .NE. 0 .AND.
     *      NET_LAST_ACK_TIME(NODE)   .NE. 0 .AND.
     *      NETMODE(NODE, 1) .EQ. TRNMD .AND.
     *      NETROUT(NODE, 1) .NE. ROUIDLE) THEN
C
	  IF (LAST_TIMEOUT_OK(NODE) .LE. 5)
     *      CALL OPS(CHAR(7) //
     *               '*** NET_CHKACK - ACK NOT RECEIVED IN TIME ***' //
     *               CHAR(7),NODE,P(ACTTIM)-NET_LAST_TRANS_SENT(NODE))
C
	  NODE_STATUS = -1
C
C IF WAITING ON I/O OUTSTANDING, INCREMENT LAST_TIMEOUT_OK FOR THIS NODE
C
	  LAST_TIMEOUT_OK(NODE) = LAST_TIMEOUT_OK(NODE) + 1
	  IF (LAST_TIMEOUT_OK(NODE) .GT. 40) LAST_TIMEOUT_OK(NODE) = 0
	ENDIF
C
	IF (NODE_STATUS .EQ. 0) LAST_TIMEOUT_OK(NODE) = 0
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
	RETURN
	END
