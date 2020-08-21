C MBXREAD.FOR
C
C V02 08-MAY-96 GLS  ADDED AUTO RE-PROMPTING.
C V01 26-MAR-91 TKO  INITIAL RELEASE
C
C This routine will read a mailbox created by MBXCREATE.  You must have
C already created the mailbox and have a channel number.
C
C CALL MBXREAD( CHANNEL, STRING, STRINGLEN, STATUS)
C
C Input:
C	CHANNEL	This is the channel number assigned to the mailbox by
C		MBXCREATE.
C
C Output:
C	STRING	String of characters received from the mailbox.  (Must be
C		declared as CHARACTER*X.
C    STRINGLEN	Number of characters actually received.
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
	SUBROUTINE MBXREAD( CHANNEL, STRING, STRINGLEN, STATUS )
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INCLUDE '($SYSSRVNAM)'
	INCLUDE	'($IODEF)'
C
	INTEGER*4   CHANNEL
	CHARACTER   STRING*(*)
	INTEGER*4   STRINGLEN
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
	STATUS = SYS$QIOW( ,%VAL(CHANNEL),
     *                     %VAL(IO$_READVBLK.OR.IO$M_NOW),             !V02
     *	                   IOSB,,,
     *		           %REF(STRING), %VAL(LEN(STRING)),,,,)
	IF(STATUS)THEN
	  STATUS = IOSB.STAT
	ENDIF
	STRINGLEN = IOSB.BLEN
C
	RETURN
	END
