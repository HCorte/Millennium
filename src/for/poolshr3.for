C
C SUBROUTINE POOLSHR3
C $Log:   GXAFXT:[GOLS]POOLSHR3.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:26:14   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:19:32   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - poolshr3.for **
C
C POOLSHR3.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V02 15-JUL-89 WS RELEASED FOR SWEDEN
C V01 01-JUN-88 XXX RELEASED FOR MICHIGAN
C
C
C     POOLSHR3.FTN
C
C     POOLSHR3(FDB,WIN,BONUS_MASK,TOT3,TOT3P,GAM) ;GET MATCH3 SHARES
C     IN:
C       FDB - FILE CONTROL BLOCK OF POOL.FIL
C       WIN - NUMBERS DRAWN
C       BONUS_MASK - MASK OF BONUS NUMBERS
C       GAM - GAME #
C     OUT:
C       TOT3 - TOTAL NR OF MATCH 3 SHARES
C       TOT3P- TOTAL FOR 3 + BONUS MATCH
C
C
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
	SUBROUTINE POOLSHR3(FDB,WIN,BONUS_MASK,TOT3,TOT3P,GAM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:POOLSEE.DEF'
	INTEGER*4 FDB(7)         !FDB BLOCK
	INTEGER*4 WIN(*)
	INTEGER*4 BONUS_MASK(*)
	INTEGER*4 TOT3,TOT3P, GAM
	INTEGER*4 IND3(SEEBET-3,SEEMAX4)      !INDEXES FOR POOLBET-3
C                                         ;OUT OF LTPOOLBET(GAM)
C
	CALL GETSHR3(FDB,LTPOOLBET(GAM),LTPOOLNR(GAM),WIN,BONUS_MASK,
     *	           TOT3,TOT3P,IND3,GAM)
	RETURN
	END
