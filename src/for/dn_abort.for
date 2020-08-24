C
C SUBROUTINE DN_ABORT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_ABORT.FOV                                 $
C  $Date::   17 Apr 1996 12:56:30                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - dn_abort.for ***
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
C Purpose: This routine will perform an immediate disconnect for the link
c specified in the buffer. This is an asynchronous disconnect and any
C incomplete I/Os are aborted. Outstanding AST's will see the aborted I/O
C error status in their IOSB's.
C
C Input: DCNPRO buffer as defined in DN_LINK.DEF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE DN_ABORT(BUFFER)
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
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
C LOCAL DECLARATIONS
C
	RECORD /DN_BUFFER_STRUCT/ BUFFER		! BUFFER ARGUMENT
C
	INTEGER*4	IDX,				! INDEX TO LINK STRUCT
     *			STATUS				! STATUS HOLDER
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FIND THE LINK BLOCK
C
	IDX = BUFFER.LINK				! LINK BLOCK INDEX
	IF (IDX .EQ. 0) THEN				! THIS IS CLEARLY WRONG!
D	  CALL OPS('*** DN_ABORT - INVALID BUFFER LINK ***',
D    *             IDX ,IDX)
	  BUFFER.IOSB.STAT = SS$_NORMAL			! NO I/O, SO FAKE IOSB
	  CALL DN_AP_STATUS(BUFFER, DNE_NOLINKBLOCK)
	  CALL IODONE(BUFFER)
	  GOTO 9999
	ENDIF
C
C IF WE MAKE IT HERE WE HAVE A VALID LINK BLOCK ID, CAN WE READ FROM IT ?
C
	IF (DN_LINK(IDX).STATE .EQ. STATE_DOWN) THEN
D	  CALL OPS('*** DN_ABORT - ATTEMPT TO ABORT WHEN LINK DOWN ***',
D    *             STATE_DOWN, STATE_DOWN)
	  BUFFER.IOSB.STAT = SS$_NORMAL			! NO I/O, SO FAKE IOSB
	  CALL DN_AP_STATUS(BUFFER, DNE_WRONGSTATE)
	  CALL IODONE(BUFFER)
	  GOTO 9999
	ENDIF
C
C IF WE MAKE IT HERE WE HAVE A VALID LINK BLOCK ID, IS IT A VALID CHANNEL?
C
	IF (DN_LINK(IDX).CHANNEL .EQ. 0) THEN
D	  CALL OPS('*** DN_ABORT - INVALID CHANNEL IN BUF HEADER ***',
D    *              0, 0)
	  BUFFER.IOSB.STAT = SS$_NORMAL			! NO I/O, SO FAKE IOSB
	  CALL DN_AP_STATUS(BUFFER, DNE_INVALIDCHAN)
	  CALL IODONE(BUFFER)
	  GOTO 9999
	ENDIF
C
C NOW CLOSE DOWN THE LINK QUICK AND DIRTY BECAUSE WE ARE IN A HURRY.
C WE ALSO GET TO RECYCLE THE CHANNEL RIGHT AWAY ...
C
	STATUS = SYS$DASSGN(%VAL(DN_LINK(IDX).CHANNEL))	! LOGICAL LINK CHANNEL
        DN_LINK(IDX).STATE = STATE_DOWN			! THIS LINK IS NOW DOWN
        IF (.NOT. STATUS) THEN
	  BUFFER.IOSB.STAT = STATUS
	  CALL DN_AP_STATUS(BUFFER, DNE_CHECKIOSB)                         
	  CALL IODONE(BUFFER)
	  GOTO 9999
        ENDIF             
C
	CALL DN_AP_STATUS(BUFFER, DNE_SUCCESS)		! RETURN SUCCESS CODE
	CALL IODONE(BUFFER)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END
C
9999	CONTINUE
C
	RETURN
	END
