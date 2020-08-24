C
C SUBROUTINE PAGEUPD
C $Log:   GXAFXT:[GOLS]PAGEUPD.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:21:38   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:15:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pageupd.for **
C
C PAGEUPD.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C     07-JAN-91 MP  UPDATED FOR RAM-BASED POOLS
C
C V01 01-JUN-88 XXX RELEASED FOR MICHIGAN
C V02 15-JUL-89 WS RELESED FOR SWEDEN
C
C
C
C   PAGEUPD.FTN - THIS PROGRAM WILL UPDATE LOTTO POOL DATA PAGES
C                 FROM MEMORY TO DISK AND READ NEW PAGE IN MEMORY
C
C
C  CALLING SEQUENCE:
C    CALL PAGEUPD(FILE_DSCRP_BLOCK,NEW_PAGE,PHASE)
C     IN:
C     FDB - FILE DESCRIPTION BLOCK
C     NEW_PAGE - NEW PAGE TO BE LOADED INTO MEMORY
C     PHASE - CHECKPOINT PHASE (1 NO CHECKPOINT, 2 LAST PHASE)
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
	SUBROUTINE PAGEUPD(FDB,NEW_PAGE,PHASE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:LTOPOL.DEF'
C
	INTEGER*4 FDB(7), LAST, ST, PHASE, NEW_PAGE
C
C
	IF (LTCURPAG.NE.0) THEN
	  IF(P(LTOPOL_RAM) .NE. LTOPOL_FIL_VALUE) THEN
	   CALL FASTMOV(LTPAGE,LTOPOL_SPACE((LTCURPAG-1)*PAGESIZE+1),PAGESIZE)
	  ELSE
	   CALL WRITEQW(FDB,LTCURPAG,LTPAGE,ST)
	   IF (PHASE.EQ.2) CALL WRITEQW(FDB,LTCURPAG+LTNUMPAG,LTPAGE,ST)
	   IF (ST .NE. 0) THEN
	     CALL POOL_FILERR(LPR, 5, ST, LTCURPAG+LTNUMPAG)
	   ENDIF
	  ENDIF
	ENDIF
C
C
	LAST=LTCURPAG
	LTCURPAG=MOD(NEW_PAGE,LTNUMPAG)+1
	IF (LAST.EQ.LTCURPAG) RETURN
C
C
C
	IF(P(LTOPOL_RAM)  .NE. LTOPOL_FIL_VALUE) THEN
	 CALL FASTMOV(LTOPOL_SPACE((LTCURPAG-1)*PAGESIZE+1),LTPAGE,PAGESIZE)
	ELSE
	 CALL READQW(FDB,LTCURPAG,LTPAGE,ST)
	 IF (ST .NE. 0) THEN
	     CALL POOL_FILERR(LPR, 4, ST, LTCURPAG)
	 ENDIF
	ENDIF
C
	RETURN
	END
