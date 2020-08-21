C
C	THRUINI.FOR
C	___________
C
C	INITIALIZE REMOTE LOGGING DATA STRUCTURES
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
	SUBROUTINE THRUINI
	IMPLICIT NONE
C
	INTEGER*4   SYSTEM

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE	'INCLIB:LOGCOM.DEF'
	INCLUDE 'INCLIB:THRUCOM.DEF'

C
	INTEGER*4   ST, NEXT

	DO 100, SYSTEM=1,THRU_SYSTEMS
	    CALL DEFLST(THRU_FREEQUE(1,SYSTEM),THRU_BUFFS)
	    CALL DEFLST(THRU_SENDQUE(1,SYSTEM),THRU_BUFFS)
	    CALL DEFLST(THRU_DELAYQUE(1,SYSTEM),THRU_DELAYLEN)
C
	    DO 10, NEXT=1,THRU_BUFFS
		CALL ABL(NEXT,THRU_FREEQUE(1,SYSTEM),ST)
10	    CONTINUE
	    CALL FASTSET(0,THRU_BUF_LOG_BLOCK(1,SYSTEM),THRU_BUFFS)

	    THRU_DELAY_CNT(SYSTEM)=0
	    THRU_DELAY_FULL(SYSTEM)=0
100	CONTINUE

	THRU_LOOP_WAIT=50
	THRU_DELAY_WAIT=20

	RETURN
	END
	
