C
C SUBROUTINE DN_READ_AST
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_READ_AST.FOV                              $
C  $Date::   17 Apr 1996 12:58:56                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C  
C *** Pre-Baseline Source - dn_read_ast.for ***
c
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	THIS ROUTINE WILL EXECUTE WHEN A READ I/O HAS COMPLETED. THE PARAMETER
C	PASSED TO US IS THE BUFFER HEADER WHICH CONTAINS CONTEXTUAL INFORMATION
C	THAT ALLOWS US TO PROPERLY PROCESS THE RESULT OF THIS I/O REQUEST.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE DN_READ_AST(BUFFER)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
        INCLUDE 'INCLIB:DESNET.DEF'
        INCLUDE 'INCLIB:DN_LINK.DEF'			! DECNET STRUCTURES.
        INCLUDE 'INCLIB:DN_BLOCK.DEF'			! DECNET DATA BLOCKS.
C
C LOCAL DECLARATIONS.
C
	INTEGER*4	IDX				! INDEX TO LINK STRUCT.
C
	RECORD /DN_BUFFER_STRUCT/ BUFFER		! BUFFER ARGUMENT.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FIND THE LINK BLOCK.
C
	IDX = BUFFER.LINK				! LINK BLOCK INDEX.
C
C CHECK FOR ANY ERRORS ON THE I/O. SHOULD HANDLE MORE SPECIFICALLY ... LATER.
C
	DN_LINK(IDX).LAST_COMMAND = BUFFER.COMMAND 	!SAVE LAST COMMAND.
	IF (.NOT. BUFFER.IOSB.STAT) THEN
	  DN_LINK(IDX).LAST_ERROR = BUFFER.IOSB.STAT 	!SAVE THE LAST ERROR.
	  CALL DN_AP_STATUS_AST(BUFFER, DNE_CHECKIOSB)
C
C NO I/O ERRORS ...
C JUST RETURN THE BUFFER TO THE INITIATOR AND QUEUE ANOTHER READ REQUEST.
C
	ELSE
	  DN_LINK(IDX).MSGRCV = DN_LINK(IDX).MSGRCV + 1	! MESSAGES RECEIVED.
	  DN_LINK(IDX).BYTRCV = DN_LINK(IDX).BYTRCV	! ACCUMULATE BYTES.
     *                        + BUFFER.IOSB.XSIZE
	   CALL DN_AP_STATUS_AST(BUFFER, DNE_SUCCESS)
	ENDIF
C
C RETURN THE STATUS TO THE INITIATING TASK.
C
	CALL IODONE_AST(BUFFER)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
	RETURN
	END
