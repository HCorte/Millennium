C
C SUBROUTINE DN_WRITE_AST
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_WRITE_AST.FOV                             $
C  $Date::   17 Apr 1996 12:59:04                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C  
C *** Pre-Baseline Source - dn_write_ast.for ***
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
C This routine will execute when a write I/O has completed. The parameter 
C passed to us is the buffer header which contains contextual information
C that allows us to properly process the result of this I/O request.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE DN_WRITE_AST(BUFFER)
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
	INTEGER*4	CURRENT_TIME(2),		! CURRENT TIME QUADWORD
     *			DAY /86400/,			! SECONDS PER DAY
     *			HOUR /3600/,			! SECONDS PER HOUR
     *			IDX,				! INDEX TO LINK STRUCT
     *			MIN /60/,			! SECONDS PER MINUTE
     *			NODE_USEID(NETSYS) /NETSYS*13/,
     *			STARTED_ST,
     *			START_INDEX,
     *			STATUS,				! STATUS HOLDER
     *			XFER_TIME
C
	INTEGER*2	CURRENT_TIME_TAB(7),		! FOR SYS$NUMTIM
     *			SEND_TIME_TAB(7)		! FOR SYS$NUMTIM
C
	RECORD /DN_BUFFER_STRUCT/ BUFFER		! BUFFER ARGUMENT
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FIND THE LINK BLOCK
C
	IDX = BUFFER.LINK				! LINK BLOCK INDEX
C
	CALL RTL_AST(START_INDEX, NET_IOSTARTED(1, 2, IDX), STARTED_ST)
C
	IF (STARTED_ST .EQ. GLIST_STAT_EMPTY) THEN
	  CALL OPS('*** DN_WRITE_AST - '//
     *             'WRITE TERMINATED, NET_IOSTARTED QUEUE EMPTY ***',
     *             STARTED_ST, 0)
	ELSE
	  IF (START_INDEX .LE. 0 .OR.
     *        START_INDEX .GT. NETNUM * NETSYS) THEN
	    CALL OPS('*** DN_WRITE_AST - ' //
     *               'INVALID INDEX ON NET_IOSTARTED QUEUE ***',
     *               START_INDEX, IDX)
	  ELSE
	    CALL NET_CHKBUF_AST(DN_BUFFER(START_INDEX), IDX,
     *                          NODE_USEID, STARTED_ST)
	  ENDIF
	ENDIF
C
C CHECK FOR ANY ERRORS ON THE I/O
C
	DN_LINK(IDX).LAST_COMMAND = BUFFER.COMMAND	! SAVE LAST COMMAND
C
	IF (.NOT. BUFFER.IOSB.STAT) THEN
	  DN_LINK(IDX).LAST_ERROR = BUFFER.IOSB.STAT 	! SAVE THE LAST ERROR
	  CALL DN_AP_STATUS_AST(BUFFER, DNE_CHECKIOSB)
C
C NO I/O ERRORS... JUST RETURN THE BUFFER TO THE INITIATOR
C
	ELSE
	  DN_LINK(IDX).MSGXMT = DN_LINK(IDX).MSGXMT + 1	! MESSAGES SENT
C
	  DN_LINK(IDX).BYTXMT = DN_LINK(IDX).BYTXMT	! ACCUMULATE BYTES SENT
     *                        + BUFFER.IOSB.XSIZE
C
	  DN_LINK(IDX).MSGOUT = DN_LINK(IDX).MSGOUT - 1	! COUNT DOWN MESSAGES
C							! QUEUED & WAITING
	  CALL DN_AP_STATUS_AST(BUFFER, DNE_SUCCESS)
C
C UPDATE XFER STATS (COULD BE MODIFIED TO USE ONE OF THE TIMERS, I.E. P(ACTTIM)
C
	  STATUS = SYS$GETTIM(CURRENT_TIME)
	  STATUS = SYS$NUMTIM(SEND_TIME_TAB, BUFFER.TIME)
	  STATUS = SYS$NUMTIM(CURRENT_TIME_TAB, CURRENT_TIME)
C
C THIS INTERNAL STRUCTURE LAYOUT ... LOOK INTO SYS$NUMTIM !
C
	  XFER_TIME = (HOUR * CURRENT_TIME_TAB(4)
     *              +  MIN  * CURRENT_TIME_TAB(5)
     *              +  CURRENT_TIME_TAB(6))
     *              - (HOUR * SEND_TIME_TAB(4)
     *              +  MIN  * SEND_TIME_TAB(5)
     *              +  SEND_TIME_TAB(6))
C
C IF XFER_TIME < 0, THEN ADD 86400 (SECONDS IN A DAY).
C THIS WILL GIVE CORRECT XFER TIMES WHEN CROSSING MIDNIGHT BOUNDARY.
C
	  IF (XFER_TIME .LT. 0) XFER_TIME = XFER_TIME + DAY
C
	  IF (XFER_TIME .GT. NET_XFER_TIME_MAX)
     *      XFER_TIME = NET_XFER_TIME_MAX
C
	  NET_XFER_TIME(XFER_TIME, IDX) =
     *    NET_XFER_TIME(XFER_TIME, IDX) + 1
	ENDIF
C
C RETURN THE STATUS TO THE INITIATING TASK
C
	CALL IODONE_AST(BUFFER)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
	RETURN
	END
