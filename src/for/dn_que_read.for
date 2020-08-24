C
C SUBROUTINE DN_QUE_READ
C $Log:   GXAFXT:[GOLS]DN_QUE_READ.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:58:48   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   17 Feb 1993 14:08:08   RXD
C  Correct spelling from increated to incremented
C  
C     Rev 1.0   21 Jan 1993 16:08:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - dn_que_read.for;1 **
C
C DN_QUE_READ.FOR
C
C V02 20-APR-92 JWE Add queue interlock retry count
C V01 21-APR-91 Steve Sullivan, DEC
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
C Copyright 1992 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DN_QUE_READ(IDX)
	IMPLICIT NONE

C This routine will allocate a data buffer and network header and
C queue a read to to the link indicated by the LINK field we are passed. 

C Include files

	INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
        INCLUDE '(LIB$ROUTINES)'
        INCLUDE '($LIBDEF)'
        INCLUDE '($LNMDEF)'
	INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:DESNET.DEF'
        INCLUDE 'INCLIB:DN_LINK.DEF' 	!DECnet Structures
        INCLUDE 'INCLIB:DN_BLOCK.DEF'  	!DECnet Data Blocks
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'

C Constant Declarations

C Structure Declarations
                     
C External Declarations

	EXTERNAL	DN_READ_AST

C Variable Declarations

	INTEGER*4	IDX		!Index to proper link structure
	INTEGER*4      	STATUS0		!Status Holder
	INTEGER*4      	STATUS		!Status Holder
	INTEGER*4	DBUFINDX	!Index into NETBUF(1,DBUFINDX)
	INTEGER*4	BUFPTR		!Pointer to a network buffer header
	INTEGER*4	IOSB_ADR	!Pointer to iosb in buffer header
	INTEGER*2	ERROR_LENGTH	!Used by SYS$GETMSG
	CHARACTER	ERROR_TEXT*256	!Used by SYS$GETMSG

C Begin Code

C Allocate a data buffer

 	CALL GRABBUF(DBUFINDX, 1, STATUS0)
	IF (.NOT. STATUS0) THEN
	   !Not at all clear what the appropriate action is here
	   !We will try to allocate a buffer header so we can report
	   !to the owner of the link what happened...
	   !
	ENDIF

C Allocate a buffer header from the free list

100	CONTINUE
	STATUS = LIB$REMQTI(DN_SYS.FREE_QUEUE,	!Queue to remove this from
	1		    BUFPTR,		!Item to remove from Queue
	2		    DN_QUEUE_INTERLOCK_RETRY_COUNT)
	IF (STATUS .EQ. LIB$_SECINTFAI) THEN 	!Did we fail?
	    DN_QUEUE_INTERLOCK_RETRY_COUNT	=
	1	DN_QUEUE_INTERLOCK_RETRY_COUNT + 1
	    CALL OPS('Queue interlock retry incremented',
	1	DN_QUEUE_INTERLOCK_RETRY_COUNT,
	2	 DN_QUEUE_INTERLOCK_RETRY_COUNT)
	    GOTO 100
	ELSEIF(STATUS .NE. SS$_NORMAL)THEN
	    CALL SYS$GETMSG(%VAL(STATUS),ERROR_LENGTH,ERROR_TEXT,,)
	    CALL OPS(ERROR_TEXT,STATUS,10)
	    IF(STATUS .NE. LIB$_ONEENTQUE)RETURN
	ENDIF
	IF (.NOT. STATUS0) THEN
	   CALL DN_AP_STATUS(%VAL(BUFPTR), DNE_READFAILED)
	   CALL IODONE(%VAL(BUFPTR))
	   RETURN
	ENDIF

C Initialize the buffer header

	CALL BUFINI(%VAL(BUFPTR),	!Buffer to initialize
	1	    DN_LINK(IDX).CHANNEL,   !Channel we are using
	2	    NTL,		!Source task
	3	    IDX,                !DN_LINK structure index
	4	    %LOC(NETBUF(1,DBUFINDX)),	!Data buffer address
	5	    IOSB_ADR)           !IOSB to use (address)

C Call DN_READ to queue the buffer for a read

	CALL DN_READ(%VAL(BUFPTR))

	RETURN 
	END
