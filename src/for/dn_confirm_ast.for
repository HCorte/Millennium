C
C SUBROUTINE DN_CONFIRM_AST
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_CONFIRM_AST.FOV                           $
C  $Date::   17 Apr 1996 12:57:14                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - dn_confirm_ast.for ***
C
C V02 28-APR-92 JWE Add queue interlock retry count
C V01 21-APR-91 Steve Sullivan, DEC
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C This routine will execute when a response to an incoming connection request
C completes. This is when it is a connection accept or a connection reject.
C We determine which by the link field of the buffer we are passed. If zero
C it is a connect reject and we just free the buffer and return... Not really
C clear we need an AST for a reject, but it was easy, so...
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE DN_CONFIRM_AST(BUFFER)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
        INCLUDE 'INCLIB:DESNET.DEF'
        INCLUDE 'INCLIB:DN_LINK.DEF'			! DECNET STRUCTURES
        INCLUDE 'INCLIB:DN_BLOCK.DEF'			! DECNET DATA BLOCKS
C
        INCLUDE '($SYSSRVNAM)'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	IDX,				! INDEX TO LINK STRUCT
     *			STATUS				! STATUS HOLDER
C
	RECORD /DN_BUFFER_STRUCT/ BUFFER		! BUFFER ARGUMENT
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FIND THE LINK BLOCK
C

	IDX = BUFFER.LINK				! LINK BLOCK INDEX
C
	IF (IDX .EQ. 0) THEN				! WAS THIS A REJECT ?
	  CALL DN_FREEBUF_AST(BUFFER.BUF_NO, STATUS)
	  GOTO 9999
	ENDIF
C
C CHECK FOR ANY ERRORS ON THE I/O
C
	IF (.NOT. BUFFER.IOSB.STAT) THEN
	  DN_LINK(IDX).STATE = STATE_DOWN		! CHANGE STATE TO DOWN
C
	  DN_LINK(IDX).STATE_COUNT(STATE_DOWN) =	! COUNT CHANGES 
     *    DN_LINK(IDX).STATE_COUNT(STATE_DOWN) + 1
C
C SAVE TIME OF CHANGE
C
	  STATUS = SYS$GETTIM(DN_LINK(IDX).STATE_TIME(STATE_DOWN))
C
	  DN_LINK(IDX).LAST_COMMAND = BUFFER.COMMAND	! SAVE LAST COMMAND
	  DN_LINK(IDX).LAST_ERROR   = BUFFER.IOSB.STAT	! SAVE LAST ERROR
C
C CLOSE DOWN THE CHANNEL SO WE CAN RE-USE IT....
C
	  STATUS=SYS$DASSGN(%VAL(DN_LINK(IDX).CHANNEL))	! LOGICAL LINK CHANNEL
C
	  IF (.NOT. STATUS) THEN
	    BUFFER.IOSB.STAT = STATUS
	    CALL DN_AP_STATUS_AST(BUFFER, DNE_CHECKIOSB)
	    CALL IODONE_AST(BUFFER)
	    GOTO 9999
	  ENDIF             
C
C NO I/O ERRORS... FINISH FILLING IN THE LINK BLOCK
C
	ELSE
	  DN_LINK(IDX).STATE     = STATE_RUNNING	! LINK ESTABLISHED !
	  DN_LINK(IDX).MSGXMT    = 0			! MESSAGES SENT
	  DN_LINK(IDX).BYTXMT    = 0			! BYTES SENT
	  DN_LINK(IDX).MSGRCV    = 0			! MESSAGES RECEIVED
	  DN_LINK(IDX).BYTRCV    = 0			! BYTES RECEIVED
	  DN_LINK(IDX).MSGOUT    = 0			! MESSAGES QUEUED
	  DN_LINK(IDX).MSGOUTMAX = 0			! HIGHWATER OF MSGOUT
C
	  DN_LINK(IDX).STATE_COUNT(STATE_RUNNING) = 
     *    DN_LINK(IDX).STATE_COUNT(STATE_RUNNING) + 1
C
C INDEXED BY STATE. 
C
	  STATUS = SYS$GETTIM(DN_LINK(IDX).STATE_TIME(STATE_RUNNING))
C
	  DN_LINK(IDX).LAST_COMMAND = BUFFER.COMMAND	! SAVE LAST COMMAND
	ENDIF
C
C ALERT THE LINK BLOCK OWNER THAT WE HAVE A NEW CONNECTION
C
	CALL DN_AP_STATUS_AST(BUFFER, DNE_SUCCESS)
	CALL IODONE_AST(BUFFER)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
