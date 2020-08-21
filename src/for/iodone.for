C
C *** SUBROUTINE IODONE ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]IODONE.FOV                                   $
C  $Date::   17 Apr 1996 13:39:46                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C  
C *** Pre-Baseline Source - dn_iodone.for ***
C
C V03 21-NOV-95 PJS DISABLE/ENABLE ASTs BEFORE/AFTER CALLS TO GLIST ROUTINES.
C V02 28-APR-92 JWE ADD QUEUE INTERLOCK FAILURE RETRY
C V01 17-APR-91 JWE INITIAL RELEASE
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
C	Called to return a DECNET buffer to the task that sent it.
C	*** NOTE: If you change this you must also change IODONE_AST.
C
C Input:
C	DECNET buffer as declared in DN_LINK
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE IODONE(BUFFER)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:DCNEVN.DEF'
        INCLUDE 'INCLIB:DESNET.DEF'
        INCLUDE 'INCLIB:DN_LINK.DEF'			! DECNET STRUCTURES
        INCLUDE 'INCLIB:DN_BLOCK.DEF'			! DECNET DATA BLOCKS
C
        INCLUDE '($SYSSRVNAM)'
C
C LOCAL DECLARATIONS
C
	RECORD /DN_BUFFER_STRUCT/ BUFFER		! BUFFER ARGUMENT
C
	INTEGER*4	STATUS				! STATUS HOLDER
C
	INTEGER*2	ERROR_LENGTH
C
	CHARACTER*256	ERROR_TEXT
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ADD TO OTHER TASK'S QUEUE
C
	STATUS = SYS$SETAST(%VAL(0))			! DISABLE ASTs	(V03)
	CALL ABL(BUFFER.BUF_NO, DCN_NETQUE, STATUS)
	STATUS = SYS$SETAST(%VAL(1))			! ENABLE ASTs	(V03)
C
C WAKE-UP THE OTHER TASK
C
	STATUS = SYS$SETEF(%VAL(NETIOTRAP))
C
	IF (.NOT. STATUS) THEN
	  CALL SYS$GETMSG(%VAL(STATUS), ERROR_LENGTH, ERROR_TEXT,,)
	  CALL OPS(ERROR_TEXT, STATUS, 6)
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
	RETURN
	END
