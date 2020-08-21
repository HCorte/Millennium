C
C SUBROUTINE GNBADMES
C $Log:   GXAFXT:[GOLS]GNBADMES.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:25:26   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:30:38   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - gnbadmes.for **
C
* GNBADMES.FOR
*
* V01 01-AUG-90 XXX RELEASED FOR VAX
*
* V01 10-JUN-90 MRM INITIAL RELEASE
*
* This subroutine will build the downline GNOS TCP/IP header
* and will inform the requesting PC that a bad message was
* received.
*
* Input parameters:
*
*     MESTYP      Int*4       GNOS message number
*     SUBTYP      Int*4       GNOS message subtype
*     MSGID       Int*4       PC message identifer
*
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* This item is the property of GTECH Corporation, Providence, Rhode
* Island, and contains confidential and trade secret information. It
* may not be transferred from the custody or control of GTECH except
* as authorized in writing by an officer of GTECH. Neither this item
* nor the information it contains may be used, transferred,
* reproduced, published, or disclosed, in whole or in part, and
* directly or indirectly, except as expressly authorized by an
* officer of GTECH, pursuant to written agreement.
*
* Copyright 1990 GTECH Corporation. All rights reserved.
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GNBADMES(MESTYP,SUBTYP,MSGID)
	IMPLICIT NONE
*
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
*
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GNSMES.DEF'
*
	INTEGER*4   MESTYP              !Message type
	INTEGER*4   SUBTYP              !Message subtype
	INTEGER*4   MSGID               !PC message id
	INTEGER*4   MSGSEQ              !Message sequence number
	INTEGER*4   MESLEN              !Message length
	INTEGER*4   MESS(400)           !Output buffer
	INTEGER*4   LSTTIM              !System time
*
* BUILD THE OUTPUT DOWNLINE HEADER MESSAGE.
*
	MSGSEQ=1
	CALL ISBYTE(GNHDRMES_PROTID_VAL,MESS,GNDWNMES_PROTID-1)
	CALL I4TOBUF2(MESTYP,MESS,GNDWNMES_MESTYP-1)
	CALL ISBYTE(SUBTYP,MESS,GNDWNMES_SUBTYP-1)
	CALL I4TOBUF2(MSGID,MESS,GNDWNMES_MSGID-1)
	CALL I4TOBUF2(MSGSEQ,MESS,GNDWNMES_SEQNUM-1)
	CALL I4TOBUF2(GNDWNMES_CMDDTA,MESS,GNDWNMES_MESLEN-1)
	CALL I4TOBUF2(0,MESS,GNDWNMES_DATOFF-1)
	LSTTIM=P(ACTTIM)
	CALL I4TOBUF4(LSTTIM,MESS,GNDWNMES_TIME-1)
	CALL ISBYTE(GNDWNMES_FLAGS_END,MESS,GNDWNMES_FLAGS-1)
	CALL ISBYTE(GNDWNMES_CMDSTS_CMD,MESS,GNDWNMES_CMDSTS-1)
	CALL ISBYTE(GNDWNMES_CMDDTA_INVALID,MESS,GNDWNMES_CMDDTA-1)
	MESLEN=GNDWNMES_CMDDTA
	CALL I4TOBUF2(MESLEN,MESS,GNDWNMES_MESLEN-1)
*
* SEND THE MESSAGE.
*
	CALL TCP_SNDBUF(MESS,MESLEN)
	RETURN
	END
