C  GXSRC:ENC_DESSTART.FOR
C  
C  $Log:   GXAGBR:[GOLS]ENC_DESSTART.FOV  $
C  
C     Rev 1.3   11 Feb 1998 18:44:02   NXA
C  Sret ASTSTOP to False [RFC 2066]
C  
C     Rev 1.2   15 Feb 1994 11:15:44   JPJ
C  Now contains support for 1 or two encpro's
C  
C     Rev 1.1   03 Jan 1994 20:21:50   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 17:41:40   SYSTEM
C  Initial revision.
C
C
C DESSTART.FOR
C
C V02 13-MAR-03 GPW DESENCR TAKEN FROM UK
C V01 18-MAR-91 TKO  Initial release
C
C This routine will initialize DESCOM and enable the encryption board
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DESSTART(OFFSET)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:DESPARAMS.DEF'		!GENERAL PARAMETERS
	INCLUDE	'INCLIB:DESCOM.DEF'		!CONTROL COMMON
C
	INTEGER*4   OFFSET,STATUS
C
C Allow the AST to go
C
        ASTSTOP = .FALSE.
C
C Set the time between polls
C
	CTRLMSEC = 30			!30 milliseconds
C
C Set the actual size of the control list
C
	CTRLCNT  = MAX_CTRLCNT
C
C Inititalize counters for encryptions/decryptions
C
	ENCRYPTCNT = 0
	DECRYPTCNT = 0
C
C Clear out control list
C
	CALL FASTSET( 0, CTRLBUF(1).CTRL_ENT(1), CTRLSIZ*CTRLCNT)
C
C Assign the driver channel, create a mailbox, and hang a read to the mailbox
C
	CALL DESASSIGN(OFFSET,STATUS)
	IF (.NOT.STATUS) THEN
	  TYPE *,IAM(),'Error from DESASSIGN = ',STATUS
	  CALL LIB$SIGNAL(%VAL(STATUS))
	ENDIF
C
C Start the microcode
C
	CALL DESINICOD(STATUS)
	IF (.NOT.STATUS) THEN
	  TYPE *,IAM(),'Error from DESINICOD = ',STATUS
	  CALL LIB$SIGNAL(%VAL(STATUS))
	ENDIF
C
C Send global area addresses to microcode
C
	CALL DESSNDADR(STATUS)
	IF (.NOT.STATUS) THEN
	  TYPE *,IAM(),'Error from DESSNDADR = ',STATUS
	  CALL LIB$SIGNAL(%VAL(STATUS))
	ENDIF
C
C Tell DES to start processing
C
	CALL DESBEGIN(STATUS)
	IF (.NOT.STATUS) THEN
	  TYPE *,IAM(),'Error from DESSTART  = ',STATUS
	  CALL LIB$SIGNAL(%VAL(STATUS))
	ENDIF
C
C
C --------------------------------------------------------------
C At this point, the board should be waiting for us to tell it what to do.
C
C Initialize PUTPOINTER (where to put next transaction) and
C            GETPOINTER (where to get next transaction)
C
	PUTPOINTER = 1
	GETPOINTER = 1
C
	RETURN
	END
