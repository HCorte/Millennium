C SUBROUTINE TGWINLOS
C
C V01 02-DEC-2000 UXN INITIAL RELEASE.
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
	SUBROUTINE TGWINLOS(TRABUF,SHARES,WIN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*4 I, M, DIV, HISHR, NUMROW, GIND, WIN
	INTEGER*4 SHARES(TGGDIV),MATCH(0:TGGNBR)
	INTEGER*4 ROWS(2,TGGNBR,12)
C
C
	WIN=0
	GIND=TRABUF(TGAMIND)
	NUMROW=LTGMAX(GIND)
	CALL FASTSET(0,SHARES,TGGDIV)
C
C PROCESS NON SYSTEM AND SYSTEM BETS
C
1000	CONTINUE
	CALL TGL_GETROW(TRABUF,ROWS)
	DO 1100 I=1,TRABUF(TWNBET)
	HISHR=0
	CALL FASTSET(0,MATCH,TGGNBR+1)
	CALL TGFULLCHK(ROWS(1,1,I),LTGWIN(1,1,GIND),NUMROW,HISHR,MATCH)
	IF(HISHR.EQ.0) GOTO 1100
	DO 1010 DIV=1,LTTDIV(GIND)
	M=LTGMAT(DIV,GIND)
	IF(M.LE.0) GOTO 1010
	IF(MATCH(M).EQ.0) GOTO 1010
	WIN=-1
	SHARES(DIV)=SHARES(DIV)+MATCH(M)
1010	CONTINUE
1100	CONTINUE
	RETURN
	END
