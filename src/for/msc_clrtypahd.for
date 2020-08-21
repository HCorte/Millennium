C
C *** SUBROUTINE MSC_CLRTYPAHD ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSC_CLRTYPAHD.FOV                            $
C  $Date::   17 Apr 1996 14:07:20                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - msc_clrtypahd.for ***
C
C V01 14-DEC-93 RRB/PJS INITIAL RELEASE.
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
C	CLEAR THE TYPE-AHEAD BUFFER FOR OUT OF SYNCHRONIZATION CONDITION.
C	RELY ON THE OTHER ROUTINES TO PICK UP THE PIECES.
C
C Calling Sequence:
C	CALL MSC_CLRTYPAHD
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE MSC_CLRTYPAHD
C
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:MSCCOM.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
C PARAMETER DECLARATIONS
C
	INTEGER*4	READFUNCOD
	PARAMETER	(READFUNCOD = IO$_READVBLK + IO$M_CVTLOW
     *                              + IO$M_NOECHO + IO$M_NOFILTR
     *                              + IO$M_TIMED + IO$M_PURGE)
C
C LOCAL DECLARATIONS
C
        INTEGER*4	I,
     *			STATUS,
     *			TERMINATORS(2),
     *			TERM_SET                      
C
	BYTE		READ_DATA(1024)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	TERM_SET       = 0                      !NO TERMINATOR CHARACTERS
	TERMINATORS(1) = 4                      !LENGTH OF TERM SET IN BYTES
	TERMINATORS(2) = %LOC(TERM_SET)         !ADDRESS OF TERMINATOR SET
C
C INITIATE READ
C
	STATUS = SYS$QIOW (, %VAL(LAT_CHANNEL),        ! I/O CHANNEL
     *                       %VAL(READFUNCOD),         ! READ FUNCTION
     *                       MSC_READIOSB,,,           ! I/O STATUS BLOCK
     *                       READ_DATA,                ! READ DATA
     *                       %VAL(1024),               ! LENGTH TO READ
     *                       %VAL(1),                  ! TIMEOUT IN SECS
     *                       %REF(TERMINATORS),,)      ! TERMINATOR SET      
C
C CHECK THE STATUS OF THE QIOW.
C IF IT'S BAD, THEN DUMP EVERYTHING WE HAVE & CALL IT QUITS.
C
	IF (STATUS .NE. SS$_NORMAL) THEN
	  CALL LIB$SIGNAL(%VAL(STATUS))
	  TYPE *
	  TYPE *, '*** SYS$QIOW PARAMETERS ***'
	  TYPE *
	  TYPE 9000, 'LAT_CHANNEL = ', LAT_CHANNEL, LAT_CHANNEL
	  TYPE 9000, 'READFUNCOD  = ', READFUNCOD, READFUNCOD
	  TYPE 9010, 'IOSB.STAT   = ', MSC_READIOSB.STAT,
     *                                 MSC_READIOSB.STAT
	  TYPE 9010, 'IOSB.XSIZE  = ', MSC_READIOSB.XSIZE,
     *                                 MSC_READIOSB.XSIZE
	  TYPE 9000, 'IOSB.PARM   = ', MSC_READIOSB.PARM,
     *                                 MSC_READIOSB.PARM
	  TYPE 9000, 'READ_LENGTH = ', 1024, 1024
	  TYPE 9000, 'TIMEOUT     = ', 1, 1
	  TYPE 9000, 'TERMINATOR_1= ', TERMINATORS(1), TERMINATORS(1)
	  TYPE 9000, 'TERMINATOR_2= ', TERMINATORS(2), TERMINATORS(2)
	  TYPE 9020, 'READ_DATA   = ', (READ_DATA(I), I = 1, 1024)
	  TYPE *
	  TYPE *, '*** ABNORMAL TERMINATION ***'
	  TYPE *
	  CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FORMAT STATEMENTS.
C
9000	FORMAT(X, A, I12, Z12.8)
9010	FORMAT(X, A, I12, Z12.4)
9020	FORMAT(X, A, 32Z3.2, 31(/, 15X, 32Z3.2))
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
	RETURN
	END
