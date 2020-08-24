C
C SUBROUTINE MBXWRITE
C $Log:   GXAFXT:[GOLS]MBXWRITE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:01:30   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:59:26   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_mbxwrite.for **
C
C MBXWRITE.FOR
C
C V01 26-MAR-91 TKO  INITIAL RELEASE
C
C This routine will WRITE to a mailbox created by MBXCREATE.  You must have
C already created the mailbox and have a channel number.
C
C CALL MBXWRITE( CHANNEL, STRING, STATUS)
C
C Input:
C	CHANNEL	This is the channel number assigned to the mailbox by
C		MBXCREATE or MBXASSIGN
C	STRING	This is the string you want to write.  It must be a
C		character string whose length can be obtained via the
C		LEN function
C
C Output:
C	STATUS	1 = success, else error code (normal VMS error codes).
C
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
	SUBROUTINE MBXWRITE( CHANNEL, STRING, STATUS )
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INCLUDE '($SYSSRVNAM)'
	INCLUDE	'($IODEF)'
C
	INTEGER*4   CHANNEL
	CHARACTER   STRING*(*)
	INTEGER*4   STATUS
C
	STRUCTURE /MBXIOSB/
	  INTEGER*2 STAT
	  INTEGER*2 BLEN
	  INTEGER*4 SPID
	END STRUCTURE
	RECORD /MBXIOSB/ IOSB
C
C
	STATUS = SYS$QIOW( ,%VAL(CHANNEL),%VAL(IO$_WRITEVBLK + IO$M_NOW +
     *                                         IO$M_NORSWAIT),
     *	                   IOSB,,,
     *		           %REF(STRING), %VAL(LEN(STRING)),,,,)
	IF(STATUS)THEN
	  STATUS = IOSB.STAT
	ENDIF
C
	RETURN
	END
