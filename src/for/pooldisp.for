C
C SUBROUTINE POOLDISP
C $Log:   GXAFXT:[GOLS]POOLDISP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:24:46   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   04 Dec 1995 15:17:42   HXK
C  Made changes for LTPOOL_GAMENR not having MAXTYP as array size!
C  
C     Rev 1.0   21 Jan 1993 17:18:10   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pooldisp.for **
C
C POOLDISP.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C     POOLDISP(GAME)
C
C V01 15-JUL-89 WS RELEASED FOR SWEDEN
C
C     DISPLAY GAME INFO
C
C     IN - GAME - GAME #
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++
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
	SUBROUTINE POOLDISP(GAME)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	LOGICAL SPORT_GAME
	INTEGER*4 FASTBIN, I4, OFF, OFF1, GAME
C
	SPORT_GAME=.TRUE.
	DO 10, OFF1=1,MAXIND
	 IF (LTPOOL_GAMENR(TLTO,OFF1).EQ.GAME) SPORT_GAME=.FALSE.
10	CONTINUE
C
	DO 20, OFF=1,LTPOOL_MAXTYP
	DO 20, OFF1=1,MAXIND
	   IF (LTPOOL_GAMENR(OFF,OFF1).EQ.GAME)
     *	       TYPE *,IAM(),'Game type ',OFF,' game index ',OFF1
20	CONTINUE
C
	IF (SPORT_GAME) THEN
	   I4=3**LTPOOLBET(GAME)
	   TYPE *,IAM(),'This is sport type game '
	ELSE
	   TYPE *,IAM(),'This lotto type game '
	   I4=FASTBIN(LTPOOLNR(GAME),LTPOOLBET(GAME))
	ENDIF
C
	IF (.NOT.SPORT_GAME) THEN
	   TYPE *,IAM(),'Game ',GAME,' - ',LTPOOLBET(GAME),'/',LTPOOLNR(GAME)
     *	      ,' there are ',I4,' different combinations to bet'
	ELSE
	   TYPE *,IAM(),'Game ',GAME,' - no of rows ',LTPOOLBET(GAME)
     *	      ,' there are ',I4,' different combinations to bet'
	ENDIF
	TYPE *,IAM(),'current draw # ',LTPOOLDRAW(GAME)
	TYPE *,IAM(),'# of files merged during POOLSEE ',LTPOOLFILES(GAME)
	TYPE *,IAM(),'Shares are defined as match all, match all-1 + bonus,'
	TYPE *,IAM(),'match all-1, match all-2 + bonus, match all-2 ...'
	TYPE *,IAM(),'shares ',(LTPOOLFLAG(OFF,GAME),OFF=1,LTPOOLFLAGS(GAME))
	TYPE *,IAM(),'base offset ',LTPOOL_BASEOFF(GAME)
	TYPE *,IAM(),'First page: ',LTPOOL_GAMPAG(GAME)
	TYPE *,IAM(),'gamlimit ',LTPOOL_GAMLIMIT(GAME)
	IF (.NOT.SPORT_GAME)
     *	  TYPE *,IAM(),'bonus ',LTPOOL_BONUS(GAME)
	TYPE *,IAM(),'page info:   page,  game,inword,  base,off_base,  '
     *	      ,'start'
C
	DO 30, OFF=1,LTNUMPAG
	   IF (LTPOOL_PAGGAM(OFF).EQ.GAME) THEN
	      TYPE 900,IAM(),OFF,LTPOOL_PAGGAM(OFF),LTPOOL_INWORD(OFF)
     *	              ,LTPOOL_PAGBASE(OFF)
     *	              ,LTPOOL_OFFSETBASE(OFF),LTPOOL_PAGESTART(OFF)
900	      FORMAT(' ',A,10X,4I7,2I9)
	   ENDIF
30	CONTINUE
	RETURN
	END
