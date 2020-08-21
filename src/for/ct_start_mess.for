C
C SUBROUTINE CT_START_MESS
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CT_START_MESS.FOV                            $
C  $Date::   17 Apr 1996 12:46:08                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92
C  DEC Baseline
C
C ** Source - ct_start_trap.for **
C
C CT_START_TRAP.FOR
C
C V01 08-JAN-91 KWP INITIAL VAX VERSION
C
C
C CONTAINS THE FOLLOWING SUBROUTINES:
C
C	CT_START_MESS:	Starts a Message trap for CTLPRO
C	CT_START_TIME:	Starts a Timer trap for CTLPRO
C
C EACH OF THESE ROUTINES ARE CALLED IN THE MAIN LINE OF CTLPRO
C AND THEN AGAIN AT THE END OF CMESTRAP & CTIMTRAP
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CT_START_MESS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CTLCOM.DEF'
	INCLUDE 'INCLIB:CTLEVN.DEF'
C
	INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4 STATUS, FUNCTION
C
	RECORD /CT_IOSSTRUCT/ LOCAL_IOSB
C
	EXTERNAL CMESTRAP
C
        FUNCTION= IO$_SETMODE .OR. IO$M_WRTATTN
        STATUS=SYS$QIOW(,%VAL(CT_MESCHANNEL),%VAL(FUNCTION),
     *                  LOCAL_IOSB,,,%REF(CMESTRAP),
     *			%VAL(CT_MESCHANNEL),
     *                  ,,,,)
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
	RETURN
	END
