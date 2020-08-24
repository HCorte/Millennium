C
C SUBROUTINE POOLSHR
C $Log:   GXAFXT:[GOLS]POOLSHR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:26:10   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:19:26   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - poolshr.for **
C
C POOLSHR.FOR
C
C V04 24-APR-91 MP  CHANGED SIZE OF DATA MOVE FROM LTOPOL_SPACE
C
C V03 11-APR-91 MP  IMPLEMENTED RAM-BASED POOLS
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V02 15-JUL-89 WS RELEASED FOR SWEDEN
C V01 01-JUN-88 XXX RELEASED FOR MICHIGAN
C
C====================================================================
C POOLSHR(FDB,BONUS_MASK,WIN,TOTALSHR);CALCULATE SHARES FOR MATCH ALL
C                                ;MATCH ALL-1, MATCH ALL-1 + BONUS
C                                ;MATCH ALL-2, MATCH ALL-2 + BONUS
C
C
C
C     IN - FDB - FILE CONTROL BLOCK FOR POOLFIL.DEF
C     WIN - WINNING COMBINATIONS (HIGHEST OFFSET SET WITH BONUS)
C     BONUS_MASK(2)    - BIT MASK OF NUMBERS BET
C
C     OUT:
C     TOTALSHR - SHARES (LTPOOLFLAG STRUCTURE)
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
	SUBROUTINE POOLSHR(FDB,WIN,BONUS_MASK,TOTALSHR,GAM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:POOLSEE.DEF'
	INTEGER*4 FDB(7)         !FDB BLOCK
	INTEGER*4  MAX
	PARAMETER (MAX=SEEMXNR+1)
	INTEGER*4 WIN(*)
	INTEGER*4 BONUS_MASK(*)
	INTEGER*4 IND2(SEEBET-2,SEEMAX1),IND1(SEEBET-1,SEEBET)
	INTEGER*4 TOTALSHR(*)    !TABLE FOR TOTALS
	INTEGER*4 GAM
C
	CALL GETSHR(FDB,LTPOOLBET(GAM),LTPOOLNR(GAM),WIN,BONUS_MASK
     *	           ,TOTALSHR,IND1,IND2,GAM)
	RETURN
	END
