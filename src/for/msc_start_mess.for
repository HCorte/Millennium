C
C *** SUBROUTINE MSC_START_MESS ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSC_START_MESS.FOV                           $
C  $Date::   17 Apr 1996 14:08:16                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - msc_start_trap.for ***
C
C V03 09-DEC-93 RRB NO LONGER A READ TRAP. READ/WRITES ARE SYNCHRONOUS
C                   IN NATURE. WILL PEND READ AFTER WRITE IN SNDBUF.
C V02 12-JAN-93 RRB ONLY ONE POLL TIME PARAMETER IN TIMER TRAP
C V01 22-JAN-91 RRB INITIAL VAX VERSION
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
C	Starts a Message trap for MSCMGR
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MSC_START_MESS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:MSCCOM.DEF'
	INCLUDE 'INCLIB:MSCEVN.DEF'
C
	INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4 STATUS, FUNCTION
C
	RECORD /MSC_IOSSTRUCT/ LOCAL_IOSB
C
	EXTERNAL MSC_MESTRAP
C
        FUNCTION= IO$_SETMODE .OR. IO$M_WRTATTN
        STATUS=SYS$QIO (,%VAL(MSC_MESCHANNEL),%VAL(FUNCTION),
     *                  LOCAL_IOSB,,,%REF(MSC_MESTRAP),
     *			%VAL (MSC_MESCHANNEL),
     *                  ,,,,)
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
	RETURN
	END
