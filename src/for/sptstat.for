C
C SUBROUTINE SPTSTAT
C $Log:   GXAFXT:[GOLS]SPTSTAT.FOV  $
C  
C V03 30-MAR-2015 MTK Modified Super 14 game
C V02 29-OCT-2003 FRP Modify for Batch2 Totobola Changes.
C
C     Rev 1.0   17 Apr 1996 15:16:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:42:34   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - sptstat.for **
C
C SPTSTAT.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 19-MAR-90 TDM INITIAL RELEASE FOR DENMARK
C
C SUBROUTINE TO UPDATE SPORT STATISTICS INFORMATION
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
	SUBROUTINE SPTSTAT(REPSBET,TRABUF,TOTBDS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*4 ROWS(SPGNBR,12), EVENT, BOARD
	INTEGER*4 REPSBET(SPGNBR,8), TOTBDS
	INTEGER*4 RROWS(2,TGGNBR,12)
	INTEGER*4 BCNT
C
C DETERMINE ROWS BET
C
	BCNT = 0
	IF(TRABUF(TWSPFRG).GT.0) BCNT = 1

	CALL GETROW(TRABUF,ROWS,RROWS)
C
	DO 500 BOARD=1,TRABUF(TWNBET)
C
	   IF(TRABUF(TWSYST).EQ.USYS.AND.BOARD.GT.1) GOTO 500
	   DO 550 EVENT=1,TRABUF(TWSRW)-BCNT
	      ROWS(EVENT,BOARD)=IAND(ROWS(EVENT,BOARD),7)
	      REPSBET(EVENT,ROWS(EVENT,BOARD)) =
     *	      REPSBET(EVENT,ROWS(EVENT,BOARD)) + 1
	      REPSBET(EVENT,8) = REPSBET(EVENT,8) + 1
550	   CONTINUE
C
C COUNT NUMBER OF BOARDS
C
	   TOTBDS = TOTBDS + 1
C
500	CONTINUE
C
	RETURN
	END
