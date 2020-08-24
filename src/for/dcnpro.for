C
C PROGRAM DCNPRO
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DCNPRO.FOV                                   $
C  $Date::   17 Apr 1996 12:49:18                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - dcnpro.for ***
C
C V04 21-NOV-95 PJS BUG FIX FOR V03 (WAS USING WRONG STATUS VARIABLE).
C V03 21-NOV-95 PJS DISABLE/ENABLE ASTs BEFORE/AFTER CALLS TO GLIST ROUTINES.
C V02 04-DEC-92 DAS NOTHING MAJOR REMOVED TABS / AND COMMENTED
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
C Purpose: The main program of the DECNET process.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	PROGRAM DCNPRO
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
	INTEGER*4	DN_EVNMASK		!BITMAP OF ALL EVENTS SET
C
C LOCAL DECLARATIONS
C
	INTEGER*4	BUF_NO,				! DCN BUFFER NUMBER
     *			COMMAND,			! CURRENT COMMAND
     *			ST,				! STATUS INDICATOR
     *			STATUS				! STATUS INDICATOR
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C SET-UP & INITIALIZATION
C
	CALL COPYRITE
	CALL SNIF_AND_WRKSET
C
	CALL DN_INIT					! GET INITIALIZED
C
C WAIT FOR NETLOG
C 
	DN_EVNMASK = IBSET(0, MOD(DCN_EVENT, 32))
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C MAIN LOOP
C
100	CONTINUE
	STATUS = SYS$WFLOR(%VAL(DN_EVNTIMER),		! WAIT FOR EVENT FLAG
     *                     %VAL(DN_EVNMASK))
	STATUS = SYS$CLREF(%VAL(DCN_EVENT))		! CLEAR EVENT FLAG
C
C GET A BUFFER NUMBER FROM TOP OF TASK LIST ...
C
200	CONTINUE
	ST = SYS$SETAST(%VAL(0))			! DISABLE ASTs	(V04)
	CALL RTL(BUF_NO, DCN_QUE, STATUS) 
	ST = SYS$SETAST(%VAL(1))			! ENABLE ASTs	(V04)
C
C WAS THERE A BUFFER THERE? IF NOT LOOP BACK AND WAIT FOR ONE!
C
	IF (STATUS .EQ. 2) GOTO 100
C
C GET THE COMMAND CODE FROM THE BUFFER ...
C
	CALL DN_GETCOMMAND(DN_BUFFER(BUF_NO), COMMAND)
C
C DISPATCH ON THE COMMAND ...
C
	IF (COMMAND .EQ. 'C0'X .OR.
     *      COMMAND .EQ. 'A0'X) THEN
	  CALL DN_CONNECT(DN_BUFFER(BUF_NO))
C
	ELSEIF (COMMAND .EQ. '20'X) THEN
	  CALL DN_WRITE(DN_BUFFER(BUF_NO))
C
	ELSEIF (COMMAND .EQ. '40'X) THEN
	  CALL DN_READ(DN_BUFFER(BUF_NO))
C
	ELSEIF (COMMAND .EQ .'100'X) THEN
	  CALL DN_DISCONNECT(DN_BUFFER(BUF_NO))
C
	ELSEIF (COMMAND .EQ. '80'X) THEN 
	  CALL DN_ABORT(DN_BUFFER(BUF_NO))
C
	ELSE						! NO GOOD ... TRY AGAIN
	  CALL OPS('*** DCNPRO - UNKNOWN FUNCTION REQUESTED ***',
     *             BUF_NO, COMMAND)
	  CALL DN_FREEBUF(BUF_NO, STATUS)
	  IF (STATUS .NE. 0)
     *      CALL OPS('*** DCNPRO - UNABLE TO FREE BUFFER ***',
     *               BUF_NO, STATUS)
	ENDIF
C
C GET ANOTHER BUFFER NUMBER
C
	GOTO 200
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C END
C
	END
