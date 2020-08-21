C
C SUBROUTINE MBXDELETE
C $Log:   GXAFXT:[GOLS]MBXDELETE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:01:22   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:59:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_mbxdelete.for **
C
C MBXDELETE.FOR
C
C V01 26-MAR-91 TKO  INITIAL RELEASE
C
C This routine will DELETE a permanent mailbox.  It should not be called to
C delete a temporary mailbox.
C
C????????????????????????????
C This routine will DEASSIGN a mailbox (or anything else) assigned to the
C channel specified.  This routine simply does a sys$DELETE, but it is provided
C to provide some symmetry within the mailbox handling routines.
C
C
C CALL MBXDELETE( CHANNEL, STATUS)
C
C Input:
C	CHANNEL This is the channel # to deassign
C
C Output:
C	STATUS	1 = success, else error code (normal VMS error codes).
C
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
	SUBROUTINE MBXDELETE( CHANNEL, STATUS )
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4   CHANNEL
	INTEGER*4   STATUS
C
C
C
	STATUS = SYS$DELMBX(  %VAL(CHANNEL) )
C
	RETURN
	END
