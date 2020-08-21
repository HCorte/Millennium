C
C *** SUBROUTINE MSCREADW ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSCREADW.FOV                                 $
C  $Date::   17 Apr 1996 14:06:28                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - mscreadw.for ***
C
C V02 12-JAN-93 RRB INCREASED READ TIMEOUT
C V01 25-JAN-90 RRB VAX INITIAL RELEASE
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
C	PERFORM READ WITH WAIT OPERATION FOR COMMUNICATIONS WITH THE TELENEX
C	MATRIX SWITCH CONTROLLER VIA AN RS232 ASCII INTERFACE
C
C	THIS ROUTINE HANDLES SUBSIQUENT READ OPERATIONS FOR MATRIX
C	SWITCH PROCESSING.
C
C Calling Sequence:
C		CALL MSCREADW(READ_DATA, LEN, ST)
C
C Input:	READ_DATA - DATA READ FROM MSC
C		LEN       - LENGTH TO READ
C
C Output:	STATUS    - STATUS OF THE OPERATION
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MSCREADW(READ_DATA,LEN,STATUS)
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:MSCCOM.DEF'
C
        INCLUDE '($SYSSRVNAM)'
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
C
	BYTE      READ_DATA(*)
	INTEGER*4 LEN
        INTEGER*4 STATUS
C
	INTEGER*4 TERM_SET                      
	INTEGER*4 TERMINATORS(2)
C
	INTEGER*4     READFUNCOD
	PARAMETER    (READFUNCOD = IO$_READVBLK + IO$M_CVTLOW +
     *                             IO$M_NOECHO + IO$M_NOFILTR + 
     *                             IO$M_TIMED)
C
	TERM_SET = 0                            !NO TERMINATOR CHARACTERS
	TERMINATORS(1) = 4                      !LENGTH OF TERM SET IN BYTES
	TERMINATORS(2) = %LOC(TERM_SET)         !ADDRESS OF TERMINATOR SET
C
C INITIATE READ
C
	STATUS = SYS$QIOW ( , %VAL(LAT_CHANNEL),        ! I/O CHANNEL
     *                        %VAL(READFUNCOD),         ! READ FUNCTION
     *                        MSC_READIOSB,,,           ! I/O STATUS BLOCK
     *                        READ_DATA,                ! READ DATA
     *                        %VAL(LEN),                ! LENGTH TO READ
     *                        %VAL(5),                  ! TIMEOUT IN SECS
     *                        %REF(TERMINATORS),,)      ! TERMINATOR SET      
	IF(STATUS.NE.SS$_NORMAL) THEN
	    CALL LIB$SIGNAL(%VAL(STATUS))
	ENDIF
	RETURN
	END
