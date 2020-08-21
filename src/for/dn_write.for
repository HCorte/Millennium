C
C SUBROUTINE DN_WRITE
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_WRITE.FOV                                 $
C  $Date::   17 Apr 1996 12:59:00                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - dn_write.for ***
C
C V02 21-NOV-95 PJS DISABLE/ENABLE ASTs BEFORE/AFTER CALLS TO GLIST ROUTINES.
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
C This routine will queue a write to to the link indicated by the LINK
C field in the buffer header we are passed. We use the address of the 
C data buffer (DBUFFER field) as the address to send, and size field in the
C buffer to determine how big it is.
C If there is an error that prevents us from performing a successful QIO
C function we immediately return the buffer to the sending task using the
C IODONE subroutine
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
c
	SUBROUTINE DN_WRITE(BUFFER)
c
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
	INTEGER*4	ADD_ST,
     *			CHKBUF_ST,
     *			IDX,				! INDEX TO LINK STRUCT
     *			NODE_USEID(NETSYS) /NETSYS*13/,
     *			STARTED_ST,
     *			START_INDEX,
     *			STATUS				! STATUS HOLDER
C
	RECORD /DN_BUFFER_STRUCT/ BUFFER		! BUFFER ARGUMENT
C
C EXTERNAL DECLARATIONS
C
	EXTERNAL	DN_WRITE_AST
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FIND THE LINK BLOCK
C
	IDX = BUFFER.LINK				! LINK BLOCK INDEX
	IF (IDX .EQ. 0) THEN				! THIS IS CLEARLY WRONG!
	  CALL OPS('*** DN_WRITE - INVALID BUFFER LINK ***',
     *             IDX ,IDX)
	  BUFFER.IOSB.STAT = SS$_NORMAL			! NO I/O, SO FAKE IOSB
	  CALL DN_AP_STATUS(BUFFER, DNE_NOLINKBLOCK)
	  CALL IODONE(BUFFER)
	  GOTO 9999
	ENDIF
C
C TAKE IT FROM NETSTARTED LIST
C
	STATUS = SYS$SETAST(%VAL(0))			! DISABLE ASTs	(V02)
	CALL RTL(START_INDEX, NET_IOSTARTED(1, 1, IDX), STARTED_ST)
	STATUS = SYS$SETAST(%VAL(1))			! ENABLE ASTs	(V02)
C
	IF (STARTED_ST .EQ. 2) THEN
	  CALL OPS('*** DN_WRITE - ' //
     *             'REQUEST WITH EMPTY NET_IOSTARTED QUEUE ***',
     *             STARTED_ST, 0)
	ELSE
	  IF (START_INDEX .LE. 0 .OR.
     *        START_INDEX .GT. NETNUM * NETSYS) THEN
	    CALL OPS('*** DN_WRITE - ' //
     *               'INVALID INDEX ON NET_IOSTARTED QUEUE ***',
     *               START_INDEX, IDX)
	  ELSE
	    CALL NET_CHKBUF(DN_BUFFER(START_INDEX), IDX,
     *                      NODE_USEID, CHKBUF_ST)
	    IF (CHKBUF_ST .NE. 0)
     *        CALL OPS('*** DN_WRITE - INVALID NET_CHKBUF STATUS ***',
     *                 CHKBUF_ST, IDX)
	  ENDIF
	ENDIF
C
C IF WE MAKE IT HERE WE HAVE A VALID LINK BLOCK ID, CAN WE WRITE TO IT ?
C
	IF (DN_LINK(IDX).STATE .NE. STATE_RUNNING) THEN
	  CALL OPS('*** DN_WRITE - INVALID LINK STATE ***',
     *             DN_LINK(IDX).STATE, IDX)
	  BUFFER.IOSB.STAT = SS$_NORMAL			! NO I/O, SO FAKE IOSB
	  CALL DN_AP_STATUS(BUFFER, DNE_WRONGSTATE)
	  CALL IODONE(BUFFER)
	  GOTO 9999
	ENDIF
C
C MAKE SURE WE HAVE A VALID BUFFER POINTER...
C
	IF (BUFFER.DBUFFER .EQ. 0) THEN
	  CALL OPS('*** DN_WRITE -  INVALID BUFFER POINTER ***',
     *             BUFFER.DBUFFER, IDX)
	  BUFFER.IOSB.STAT = SS$_NORMAL			! NO I/O, SO FAKE IOSB
	  CALL DN_AP_STATUS(BUFFER, DNE_INVALIDBUFFER)
	  CALL IODONE(BUFFER)
	  GOTO 9999
	ENDIF
C
C MAKE SURE WE HAVE A VALID BUFFER SIZE... MORE THAN ZERO
C
	IF (BUFFER.DBUFFER_SIZE .LE. 0) THEN
	  CALL OPS('*** DN_WRITE - INVALID BUFFER LENGTH ***',
     *             BUFFER.DBUFFER_SIZE, IDX)
	  BUFFER.IOSB.STAT = SS$_NORMAL			! NO I/O, SO FAKE IOSB
	  CALL DN_AP_STATUS(BUFFER, DNE_INVALIDBUFLEN)
	  CALL IODONE(BUFFER)
	  GOTO 9999
	ENDIF
C
C MAKE SURE WE HAVE A VALID CHANNEL
C
	IF (DN_LINK(IDX).CHANNEL .EQ. 0) THEN
	  CALL OPS('*** DN_WRITE - INVALID CHANNEL ***',
     *             DN_LINK(IDX).CHANNEL, IDX)
	  BUFFER.IOSB.STAT = SS$_NORMAL			! NO I/O, SO FAKE IOSB
	  CALL DN_AP_STATUS(BUFFER, DNE_INVALIDCHAN)
	  CALL IODONE(BUFFER)
	  GOTO 9999
	ENDIF
C
C WE KNOW WE ARE CONNECTED, AND EXIST, PERHAPS WE SHOULD ATTEMPT A WRITE !
C QUEUE A DECNET WRITE REQUEST
C
	BUFFER.CHANNEL = DN_LINK(IDX).CHANNEL
	CALL DN_AP_STATUS(BUFFER, DNE_SUCCESS)
C
	ADD_ST = GLIST_STAT_FULL
C
	IF (STARTED_ST .NE. 2) THEN
	  STATUS = SYS$SETAST(%VAL(0))			! DISABLE ASTs	(V02)
	  CALL ABL(START_INDEX, NET_IOSTARTED(1, 2, IDX), ADD_ST)
	  STATUS = SYS$SETAST(%VAL(1))			! ENABLE ASTs	(V02)
	ENDIF
C
	STATUS = SYS$QIO(,				! EVENT FLAG 
     *                   %VAL(BUFFER.CHANNEL),		! CHANNEL
     *                   %VAL(IO$_WRITEVBLK),		! FUNCTION 
     *                   %REF(BUFFER.IOSB),		! STATUS BLOCK
     *                   DN_WRITE_AST,			! AST ADDRESS
     *                   %REF(BUFFER),			! AST PARAMETER
     *                   %VAL(BUFFER.DBUFFER +		! P1
     *                   %LOC(FRST_NETCOM(1))),         ! P1 (CONT)
     *                   %VAL(BUFFER.DBUFFER_SIZE),	! P2
     *                   ,				! P3
     *                   ,				! P4
     *                   ,				! P5
     *                   )				! P6
C
	IF (.NOT. STATUS) THEN
	  CALL OPS('*** DN_WRITE - UNABLE TO QUEUE NETWORK WRITE ***',
     *             STATUS, IDX)
	  BUFFER.IOSB.STAT = STATUS
	  CALL DN_AP_STATUS(BUFFER, DNE_CHECKIOSB)
	  CALL IODONE(BUFFER)
	  IF (ADD_ST .NE. GLIST_STAT_FULL) THEN
	    STATUS = SYS$SETAST(%VAL(0))		! DISABLE ASTs	(V02)
	    CALL RBL(START_INDEX, NET_IOSTARTED(1,2, IDX), STARTED_ST)
	    STATUS = SYS$SETAST(%VAL(1))		! ENABLE ASTs	(V02)
	  ENDIF
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
