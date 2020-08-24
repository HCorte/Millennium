C
C SUBROUTINE MBXASSIGN
C $Log:   GXAFXT:[GOLS]MBXASSIGN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:01:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:59:00   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_mbxassign.for **
C
C MBXASSIGN.FOR
C
C V01 26-MAR-91 TKO  INITIAL RELEASE
C
C This routine will ASSIGN a mailbox with the specified name.  If
C the specified mailbox does not exists, it will return an error.
C
C Note that MBXCREATE will assign a mailbox if it already exists and create
C a mailbox if it doesn't exist.  This routine will not create a mailbox if
C it already exists, thus this should be called instead of MBXCREATE if you
C want to be sure the mailbox still exists.
C
C CALL MBXASSIGN(MBXNAME, CHANNEL, STATUS)
C
C Input:
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
C Example:  To ASSIGN an existing mailbox for TELL.
C
C	INTEGER*4    I4PREFIXLEN
C	INTEGER*4    I4PREFIX
C	CHARACTER*4  CXPREFIX
C	EQUIVALENCE (I4PREFIX, CXPREFIX)
C	    .
C	    .
C	    .
C	CALL GETPRFX( I4PREFIX, I4PREFIXLEN)
C	CALL MBXASSIGN( CXPREFIX(1:I4PREFIXLEN)//'TELL', CHANNEL, STATUS)
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
	SUBROUTINE MBXASSIGN( MBXNAME, CHANNEL, STATUS )
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE '($SYSSRVNAM)'
C
	CHARACTER   MBXNAME*(*)
	INTEGER*4   CHANNEL
	INTEGER*4   STATUS
C
C
C
	STATUS = SYS$ASSIGN( MBXNAME, CHANNEL,,)
C
	RETURN
	END
