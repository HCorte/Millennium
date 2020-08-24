C
C SUBROUTINE GNGRPDET
C $Log:   GXAFXT:[GOLS]GNGRPDET.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:26:20   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:31:34   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - gngrpdet.for **
C
* GNGRPDET.FOR
*
* V01 01-AUG-90 XXX RELEASED FOR VAX
*
* V01 16-JUN-90 MRM INITIAL RELEASE
*
* This subroutine will build the requested group detail
* information messages for GNOS.  The routine will send
* each message to TCP/IP as it creates it.
*
* Input parameters:
*
*     MESTYP      Int*4       Upline requested message type
*     SUBTYP      Int*4       Upline requested message subtype
*     MSGID       Int*4       PC message identifier
*     TYP         Int*4(4)    Type of qualification data
*     CNT         Int*4(4)    Number of qualification parameters
*     QUADTA      Int*4(20,4) Qualification parameters
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
	SUBROUTINE GNGRPDET(MESTYP,SUBTYP,MSGID,TYP,CNT,QUADTA)
	IMPLICIT NONE
*
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
*
	INCLUDE 'INCLIB:GNSMES.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
*
	INTEGER*4   MESTYP              !Message type
	INTEGER*4   SUBTYP              !Message subtype
	INTEGER*4   MSGSEQ              !Message sequence number
	INTEGER*4   MSGID               !PC message identifier
	INTEGER*4   TYP(4)              !Type of qualification data
	INTEGER*4   CNT(4)              !Number of qualification parameters
	INTEGER*4   QUADTA(20,4)        !Qualification parameters
	INTEGER*4   OUTGRP              !Group number
	INTEGER*4   MESLEN
	INTEGER*4   I
*
* RANGE OF GROUPS REQUESTED.
*
	MSGSEQ=0
	IF(TYP(1).EQ.GNHDRMES_TYP_RANGE) THEN
	  DO 100 OUTGRP=QUADTA(1,1),QUADTA(2,1)
	    CALL BLDGRPDET(OUTGRP,MESTYP,SUBTYP,MSGID,MSGSEQ,MESLEN)
100	  CONTINUE
*
* SPECIFIC GROUP REQUESTED.
*
	ELSE IF(TYP(1).EQ.GNHDRMES_TYP_SPECIFIC) THEN
	  DO 200 I=1,CNT(1)
	    OUTGRP=QUADTA(I,1)
	    CALL BLDGRPDET(OUTGRP,MESTYP,SUBTYP,MSGID,MSGSEQ,MESLEN)
200	  CONTINUE
*
* ALL GROUPS WERE REQUESTED.
*
	ELSE
	  DO 300 OUTGRP=1,X2X_NUM_GROUPS
	    CALL BLDGRPDET(OUTGRP,MESTYP,SUBTYP,MSGID,MSGSEQ,MESLEN)
300	  CONTINUE
	ENDIF
*
* SEND A MESSAGE INFORMING THAT NO MORE MESSAGES WILL
* BE SENT FOR THIS REQUEST.
*
	CALL GNLAST(MESTYP,SUBTYP,MSGID,MSGSEQ)
*
	RETURN
	END
