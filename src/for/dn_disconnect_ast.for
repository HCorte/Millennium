C
C SUBROUTINE DN_DISCONNECT_AST
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_DISCONNECT_AST.FOV                        $
C  $Date::   17 Apr 1996 12:57:30                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C  
C *** Pre-Baseline Source - dn_disconnect_ast.for ***
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
C This routine will execute when a Synchronous Disconnect request has 
C completed. The parameter passed to us is a buffer header which contains 
C contextual information that allows us to properly process the result of 
C this I/O request to the issuing process/routine.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE DN_DISCONNECT_AST(BUFFER)
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
C HERE WE DON'T CARE ABOUT ANY ERRORS... THE LINK IS DOWN...
C
	DN_LINK(IDX).STATE = STATE_DOWN			! CHANGE STATE TO DOWN
C
	DN_LINK(IDX).STATE_COUNT(STATE_DOWN) =		! COUNT CHANGES 
     *  DN_LINK(IDX).STATE_COUNT(STATE_DOWN) + 1
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
	STATUS = SYS$DASSGN(%VAL(DN_LINK(IDX).CHANNEL))	! LOGICAL LINK CHANNEL
C
	IF (.NOT. STATUS) THEN
	  BUFFER.IOSB.STAT = STATUS
	  CALL DN_AP_STATUS_AST(BUFFER, DNE_CHECKIOSB)
	  CALL IODONE_AST(BUFFER)
	  GOTO 9999
        ENDIF
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
