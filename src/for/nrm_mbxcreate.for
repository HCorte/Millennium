C
C SUBROUTINE MBXCREATE
C $Log:   GXAFXT:[GOLS]MBXCREATE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:01:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:59:06   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_mbxcreate.for **
C
C MBXCREATE.FOR
C
C V01 26-MAR-91 TKO  INITIAL RELEASE
C
C This routine will create a temporary mailbox with the specified name.  If
C the specified mailbox already exists, it will simply assign this task to
C it.
C
C CALL MBXCREATE(PERMFLG, MBXNAME, CHANNEL, STATUS)
C
C Input:
C	PERMFLG 0 = make a temporary mailbox, 1 = make a permanent mailbox
C	MBXNAME	This is a character string containing the name of the mailbox.
C		Note that this should always include the project prefix
C               (see below for example).
C
C Output:
C	CHANNEL	If successful, this is the channel number assigned to the
C		mailbox.  It must be used for all other calls (i.e., MBXREAD,
C		MBXWRITE, MBXDELETE).
C	STATUS	1 = success, else error code (normal VMS error codes).
C
C
C Example:  To create a mailbox for WAGPRO.
C
C	INTEGER*4    I4PREFIXLEN
C	INTEGER*4    I4PREFIX
C	CHARACTER*4  CXPREFIX
C	EQUIVALENCE (I4PREFIX, CXPREFIX)
C	    .
C	    .
C	    .
C	CALL GETPRFX( I4PREFIX, I4PREFIXLEN)
C	CALL MBXCREATE(0, CXPREFIX(1:I4PREFIXLEN)//'WAGPRO', CHANNEL, STATUS)
C
C
C COPYRITF.DEF+++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C COPYRIGHT 1991 GTECH CORPORATION.  ALL RIGHTS RESERVED.
C
C CONFIDENTIAL PROPRIETARY INFORMATION
C This item is the property of GTECH Corporation, W. Greenwich, Rhode
C Island, and contains confidential and trade secret information.  It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH.  Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published or disclosed, in whold or in part, directly
C or indirectly, except as expressly authorized by an officer of
C GTECH pursuant to written agreement.
C COPYRITF.DEF-------------------------------------------------------
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MBXCREATE(PERMFLG, MBXNAME, CHANNEL, STATUS )
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE '($SYSSRVNAM)'
	INCLUDE	'($STSDEF)'
	INCLUDE '($SSDEF)'
C
	INTEGER*4   PERMFLG
	CHARACTER   MBXNAME*(*)
	INTEGER*4   CHANNEL
	INTEGER*4   STATUS
C
C
C
	IF(PERMFLG.NE.0 .AND. PERMFLG.NE.1)THEN
	  TYPE *,IAM(),'MBXCREATE INVALID PERMFLG = ',PERMFLG
	  STATUS = IAND(SS$_INSFARG,'FFFFFFF8'X) + STS$K_ERROR
	  CALL LIB$SIGNAL(%VAL(STATUS))
	ELSE
	  STATUS = SYS$CREMBX(%VAL(PERMFLG), CHANNEL, %VAL(80),,
     *	  	              %VAL('0000'X),,MBXNAME)
	ENDIF
C
	RETURN
	END
