C
C SUBROUTINE PC_IOTRAP
C $Log:   GXAFXT:[GOLS]PC_IOTRAP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:22:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:16:20   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pc_iotrap.for **
C
C IOTRAP.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C THIS SUBROUTINE WILL INTERCEPT AND SERVICE ALL IO TRAPS
C      FROM A DEVICE CONNECTED TO THE PC
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
	SUBROUTINE PC_IOTRAP(PARAM)
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
	INTEGER*4 PARAM
C
	IF(PARAM.LT.1 .OR.PARAM.GT.NUMMES) THEN
	  TYPE *,'INVALID PC_IOTRAP   MESSAGE # ',PARAM
	  RETURN
	ENDIF
C
	PC_CNTIO(PARAM)=PC_CNTIO(PARAM)+1	!KEEP STATISTICS
C
	IF(WRITE_IOSB(PARAM).STAT .NE. SS$_NORMAL) THEN
	   TYPE*,'WRITE ERROR IN PC_IOTRAP'
	   CALL LIB$SIGNAL(%VAL(WRITE_IOSB(PARAM).STAT))
	ENDIF
C
	IOWPROG=0
	IF(TSKSTAT.EQ.ACT.AND.INTVAL(PARAM).GT.0) THEN
	  CALL PC_START_TIME(PARAM)
	  TIMINPROG(PARAM)=1
	ENDIF
C
	RETURN
	END
