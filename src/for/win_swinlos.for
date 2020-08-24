C SUBROUTINE WIN_SWINLOS
C  
C V06 24-APR-2017 MTK Modified for cancelled draws 
C V05 30-MAR-2017 MTK Modified Super 14 game
C V04 10-NOV-2003 FRP Modify for Batch2 Totobola Changes.
C V03 08-FEB-2000 OXK # of rows passed as parameter to SPTCHK (Vakio changes)
C V02 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C			Instant Pass Thru Phase 1
C V01 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE WIN_SWINLOS(TRABUF,SHARES,WIN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*4 I, M, DIV, HISHR, IND, LEN, NUMROW, BCNT, GIND, WIN
	INTEGER*4 SHARES(SPGDIV),MATCH(0:SPGNBR),RMATCH(0:TGGNBR)
	INTEGER*4 BOARD(2)
	INTEGER*2 ROWS(SPGNBR,12)
	INTEGER*2 RROWS(2,TGGNBR,12)
C
C
	WIN=0
	GIND=TRABUF(TGAMIND)
	BCNT = 0
	IF(TRABUF(TWSPFRG) .GT. 0) BCNT = 1   ! SUPER14 IS SETUP

	NUMROW=LSPMAX(GIND)
	CALL FASTSET(0,SHARES,SPGDIV)
C                                                         !V06
C CHECK FOR CANCELLED DRAW                                !V06
C                                                         !V06
	IF(LSPDCD(GIND).NE.0) THEN                        !V06
	  DIV = LSPRWD(GIND)                              !V06
	  IF(DIV.NE.0) THEN                               !V06
	    WIN = -1                                      !V06
	    SHARES(DIV) = TRABUF(TWSIMP)                  !V06
          ENDIF                                           !V06
          RETURN                                          !V06
        ENDIF                                             !V06

	IF(TRABUF(TWSYST).EQ.NOSYS) GOTO 1000
C
C PROCESS SYSTEM BETS
C
	IF(TRABUF(TWSYST).EQ.USYS) THEN
	  LEN=(NUMROW+1)/2
	  IND=LEN+1
	  CALL MOVBYT(TRABUF(TWBORD),IND,BOARD,1,LEN)
	ENDIF
	CALL SPTCHK(TRABUF(TWBORD),BOARD,TRABUF(TWSYSN),
     *	            LSPWIN(1,GIND),HISHR,MATCH,TRABUF(TWSRW), BCNT)  ! "TRABUF(TWSPFRG)" REPLACED BY "BCNT"
	DO 10 DIV=1+BCNT,LSPDIV(GIND)    !div1 is for SUPER14
	M=LSPMAT(DIV,GIND)
	IF(M.LE.0) GOTO 10
	IF(MATCH(M).EQ.0) GOTO 10
	WIN=-1
	SHARES(DIV)=SHARES(DIV)+MATCH(M)
10	CONTINUE
	IF(BCNT.GT.0 .AND. SHARES(1+BCNT).GT.0) THEN  !check if wins SUPER14
	  CALL SPTROW(TRABUF,ROWS,RROWS)
	  GOTO 2000
	ENDIF
	RETURN
C
C PROCESS NON SYSTEM BETS
C
1000	CONTINUE
	CALL SPTROW(TRABUF,ROWS,RROWS)
	DO 1100 I=1,TRABUF(TWNBET)
	HISHR=0
	CALL FASTSET(0,MATCH,SPGNBR+1)
	CALL FULLCHK(ROWS(1,I),LSPWIN(1,GIND),NUMROW-BCNT,HISHR,MATCH)
	IF(HISHR.EQ.0) GOTO 1100
	DO 1010 DIV=1+BCNT,LSPDIV(GIND)  !div1 is for SUPER14
	M=LSPMAT(DIV,GIND)
	IF(M.LE.0) GOTO 1010
	IF(MATCH(M).EQ.0) GOTO 1010
	WIN=-1
	SHARES(DIV)=SHARES(DIV)+MATCH(M)
1010	CONTINUE
1100	CONTINUE
	IF(BCNT.GT.0 .AND. SHARES(1+BCNT).GT.0) GOTO 2000	!check if wins SUPER14
	RETURN
C
C PROCESS SUPER 14 ROW
C
2000    CONTINUE
        HISHR=0
        CALL FASTSET(0,RMATCH,TGGNBR+1)
	CALL RFULLCHK(RROWS(1,1,1),LSPWIN(1,GIND),NUMROW,
     *                LSPFRG(GIND),HISHR,SHARES)
        IF(HISHR.NE.0) WIN=-1
	RETURN
	END
