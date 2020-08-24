C
C SUBROUTINE PC_START_MESS
C $Log:   GXAFXT:[GOLS]PC_START_MESS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:23:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:16:38   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pc_start_trap.for **
C
C PC_START_TRAP.FOR
C
C V01 11-JAN-91 KWP INITIAL VAX VERSION
C
C
C CONTAINS THE FOLLOWING SUBROUTINES:
C
C	PC_START_MESS:	Starts a Message trap for PCLOG
C	PC_START_TIME:	Starts a Timer trap for PCLOG
C
C EACH OF THESE ROUTINES ARE CALLED IN THE MAIN LINE OF PCLOG
C AND THEN AGAIN AT THE END OF PC_MESTRAP & PC_TIMTRAP
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE PC_START_MESS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PCCOM.DEF'
	INCLUDE 'INCLIB:PCEVN.DEF'
C
	INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
C
	INTEGER*4 STATUS, FUNCTION
C
	RECORD /PC_IOSSTRUCT/ LOCAL_IOSB
C
	EXTERNAL PC_MESTRAP
C
        FUNCTION= IO$_SETMODE .OR. IO$M_WRTATTN
        STATUS=SYS$QIOW(,%VAL(PC_MESCHANNEL),%VAL(FUNCTION),
     *                  LOCAL_IOSB,,,%REF(PC_MESTRAP),
     *			%VAL(PC_MESCHANNEL),
     *                  ,,,,)
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
	RETURN
	END
