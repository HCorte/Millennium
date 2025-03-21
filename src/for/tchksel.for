C
C SUBROUTINE TCHKSEL
C $Log:   GXAFXT:[GOLS]TCHKSEL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:29:24   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:49:46   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - twinsub.for **
C
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TCHKSEL(TRABUF,OKRUN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INTEGER*4 CNT, GIND, SEL, I, ROW, STS
	LOGICAL OKRUN
C
	CNT=0
	OKRUN=.FALSE.
	GIND=TRABUF(TGAMIND)
C
C CHECK IF ALREADY SELECTED OR NOT EXPIRED YET
C
	SEL=TRABUF(TWTSEL1)
	DO 10 I=0,SEL-1
	ROW=TRABUF(TWTROW1+I*TWTBLEN)
	STS=LTSSTA(ROW,GIND)
	IF(STS.LT.GAMENV) RETURN
	IF(STS.EQ.GAMENV.OR.STS.EQ.GAMCAN) CNT=CNT+1
10	CONTINUE
	IF(TRABUF(TWNBET).EQ.1) GOTO 100
C
C
	SEL=TRABUF(TWTSEL2)
	DO 20 I=0,SEL-1
	ROW=TRABUF(TWTROW2+I*TWTBLEN)
	STS=LTSSTA(ROW,GIND)
	IF(STS.LT.GAMENV) RETURN
	IF(STS.EQ.GAMENV.OR.STS.EQ.GAMCAN) CNT=CNT+1
20	CONTINUE
	IF(TRABUF(TWNBET).EQ.2) GOTO 100
C
C
	SEL=TRABUF(TWTSEL3)
	DO 30 I=0,SEL-1
	ROW=TRABUF(TWTROW3+I*TWTBLEN)
	STS=LTSSTA(ROW,GIND)
	IF(STS.LT.GAMENV) RETURN
	IF(STS.EQ.GAMENV.OR.STS.EQ.GAMCAN) CNT=CNT+1
30	CONTINUE
C
C
100	CONTINUE
	IF(CNT.NE.0) OKRUN=.TRUE.
	RETURN
	END
