C
C SUBROUTINE DN_CONNECT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_CONNECT.FOV                               $
C  $Date::   17 Apr 1996 12:57:18                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - dn_connect.for ***
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
C This routine will queue a connect request for another system (or our own if
C asked). The buffer header contains a index to a particular link block that
C has all the information we need to make the connection.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE DN_CONNECT(BUFFER)
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
     *			NCB_DESCRIP(2),			! NETWORK CONNECT BLOCK
     *			NAM_DESCRIP(2),			! NETWORK NAME STRING
     *			STATUS				! STATUS HOLDER
C
	CHARACTER*80	NCB				! NETWORK CONNECT BLOCK
C
	RECORD /DN_BUFFER_STRUCT/ BUFFER		! BUFFER ARGUMENT
C
C EXTERNAL DECLARATIONS
C
	EXTERNAL	DN_CONNECT_AST
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FIND THE LINK BLOCK
C
	IDX = BUFFER.LINK				! LINK BLOCK INDEX
	IF (IDX .EQ. 0) THEN				! THIS IS CLEARLY WRONG!
	  CALL OPS('*** DN_CONNECT - INVALID BUFFER LINK ***',
     *             IDX, IDX)
	  BUFFER.IOSB.STAT = SS$_NORMAL			! NO I/O, SO FAKE IOSB
	  CALL DN_AP_STATUS(BUFFER, DNE_NOLINKBLOCK)
	  CALL IODONE(BUFFER)
	  GOTO 9999
	ENDIF
C
C IF WE MAKE IT HERE WE HAVE A VALID LINK BLOCK ID, CAN WE READ FROM IT?
C
	IF (DN_LINK(IDX).STATE .NE. STATE_DOWN) THEN
	  CALL OPS('*** DN_CONNECT - ' //
     *             'ATTEMPT TO CONNECT TO ACTIVE LINK ***',
     *             DN_LINK(IDX).STATE, IDX)
	  BUFFER.IOSB.STAT = SS$_NORMAL			! NO I/O, SO FAKE IOSB
	  CALL DN_AP_STATUS(BUFFER, DNE_SUCCESS)
	  CALL IODONE(BUFFER)
	  GOTO 9999
	ELSE
	  DN_LINK(IDX).STATE = STATE_CONNECT		! ATTEMPTING CONNECTION
	ENDIF
C
C ASSIGN A CHANNEL TO DECNET
C
	STATUS = SYS$ASSIGN('_NET:',			! LOGICAL FOR CHANNEL
     *                      DN_LINK(IDX).CHANNEL,	! CHANNEL TO ASSIGN
     *                      ,				! ACCESS MODE
     *                      DN_SYS.MBXNAME)		! MAILBOX LOGICAL NAME
C
	IF (.NOT. STATUS) THEN
	  BUFFER.IOSB.STAT = SS$_NORMAL			! NO I/O, SO FAKE IOSB
	  CALL DN_AP_STATUS(BUFFER, DNE_INVALIDCHAN)
	  CALL	IODONE(BUFFER)
	  GOTO 9999
	ENDIF
C
C GET THE UNIT NUMBER AND PUT IT INTO THE LINK BLOCK
C
	CALL DN_GET_UNIT(DN_LINK(IDX).CHANNEL, DN_LINK(IDX).UNIT)
C
C BUILD A NETWORK CONNECT BLOCK
C
	DN_LINK(IDX).NCB = DN_LINK(IDX).NODE(1:DN_LINK(IDX).NODELEN) //
     *                     '::"TASK=' //
     *                     DN_LINK(IDX).TASK(1:DN_LINK(IDX).TASKLEN) //
     *                     '"'
C
	NCB_DESCRIP(1) = DN_LINK(IDX).NODELEN + DN_LINK(IDX).TASKLEN + 9
	NCB_DESCRIP(2) = %LOC(DN_LINK(IDX).NCB)
C
C CONNECT TO REMOTE NODE
C
	CALL DN_AP_STATUS(BUFFER, DNE_SUCCESS)
C
        STATUS = SYS$QIO(,				! EVENT FLAG
     *                   %VAL(DN_LINK(IDX).CHANNEL),	! CHANNEL
     *                   %VAL(IO$_ACCESS),		! FUNCTION 
     *                   %REF(BUFFER.IOSB),		! STATUS BLOCK
     *                   DN_CONNECT_AST,		! AST ADDRESS
     *                   BUFFER,			! AST PARAMETER
     *                   ,				! P1
     *                   %REF(NCB_DESCRIP),		! P2
     *                   ,				! P3
     *                   ,				! P4
     *                   ,				! P5
     *                   )				! P6
C
        IF (STATUS .NE. SS$_NORMAL) THEN
	  BUFFER.IOSB.STAT = STATUS			! FAKE IOSB STATUS
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
	  CALL	IODONE(BUFFER)
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END
C
9999	CONTINUE
C
	RETURN
	END
