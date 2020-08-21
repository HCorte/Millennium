C
C SUBROUTINE DN_DISCONNECT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_DISCONNECT.FOV                            $
C  $Date::   17 Apr 1996 12:57:26                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - dn_disconnect.for ***
C
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
C This routine will queue a disconnect request for the link specified in 
C the buffer. A synchronous disconnect is performed and outstanding I/Os
C are completed before the link is terminated. 
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE DN_DISCONNECT(BUFFER)
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
	INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	IDX,				! INDEX TO LINK STRUCT
     *			QIO_FUNC,			! SYS$QIO FUNCTION #
     *			STATUS				! STATUS HOLDER
C
	RECORD /DN_BUFFER_STRUCT/ BUFFER		! BUFFER ARGUMENT
C
C EXTERNAL DECLARATIONS
C
	EXTERNAL	DN_DISCONNECT_AST
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FIND THE LINK BLOCK
C
	IDX = BUFFER.LINK				! LINK BLOCK INDEX
C
	IF (IDX .EQ. 0) THEN				! THIS IS CLEARLY WRONG!
	  BUFFER.IOSB.STAT = SS$_NORMAL			! NO I/O, SO FAKE IOSB
	  CALL DN_AP_STATUS(BUFFER, DNE_NOLINKBLOCK)
	  CALL IODONE(BUFFER)
	  GOTO 9999
	ENDIF
C
C IF WE MAKE IT HERE WE HAVE A VALID LINK BLOCK ID, IS IT RUNNING ?
C
	IF (DN_LINK(IDX).STATE .NE. STATE_RUNNING) THEN
	  BUFFER.IOSB.STAT = SS$_NORMAL			! NO I/O, SO FAKE IOSB
	  CALL DN_AP_STATUS(BUFFER, DNE_WRONGSTATE)
	  CALL IODONE(BUFFER)
	  GOTO 9999
	ENDIF
C
C IF WE MAKE IT HERE WE HAVE A VALID LINK BLOCK ID, IS IT A VALID CHANNEL ?
C
	IF (DN_LINK(IDX).CHANNEL .EQ. 0) THEN
	  BUFFER.IOSB.STAT = SS$_NORMAL			! NO I/O, SO FAKE IOSB
	  CALL DN_AP_STATUS(BUFFER, DNE_INVALIDCHAN)
	  CALL IODONE(BUFFER)
	  GOTO 9999
	ENDIF
C
C NOW CLOSE DOWN THE LINK SO THE OTHER END KNOWS WE WENT AWAY !
C
	CALL DN_AP_STATUS(BUFFER, DNE_SUCCESS)
	DN_LINK(IDX).STATE = STATE_DISCONNECT		! WE ARE IN THE PROCESS
C
	QIO_FUNC = (IO$_DEACCESS .OR. IO$M_SYNCH)
C
	STATUS = SYS$QIO(,				! EVENT FLAG
     *                   %VAL(DN_LINK(IDX).CHANNEL),	! CHANNEL
     *                   %VAL(QIO_FUNC),		! FUNCTION
     *                   BUFFER.IOSB,			! I/O STATUS BLOCK
     *                   DN_DISCONNECT_AST,		! AST ADDRESS
     *                   BUFFER,			! AST PARAMETER
     *                   ,				! P1
     *                   ,				! P2
     *                   ,				! P3
     *                   ,				! P4
     *                   ,				! P5
     *                   )				! P6
C
	IF (STATUS .NE. SS$_NORMAL) THEN
	  BUFFER.IOSB.STAT = STATUS
C
C HERE WE DON'T CARE ABOUT ANY ERRORS... THE LINK IS DOWN...
C
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
C NOW DEASSIGN THE CHANNEL SO WE CAN USE IT AGAIN...
C
	  STATUS=SYS$DASSGN(%VAL(DN_LINK(IDX).CHANNEL))	! LOGICAL LINK CHANNEL
C
	  CALL DN_AP_STATUS(BUFFER, DNE_CHECKIOSB)
	  CALL IODONE(BUFFER)
	  GOTO 9999
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
