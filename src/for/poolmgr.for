C PROGRAM POOLMGR
C
C V09 16-FEB-2000 OXK Fixed IO to allow using in batch
C V08 31-JAN-2000 OXK SPGNBR used for TSPT (Vakio changes)
C V07 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting,
C			Instant Pass Thru Phase 1
C V06 04 Dec 1995 HXK Made changes for LTPOOL_GAMENR not having
C			MAXTYP as array size!
C V05 21 Jan 1993 DAB Initial Release
C			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C			DEC Baseline
C V04 10-APR-1991 MP  THERE WAS A LINE COMMENTED-OUT THAT CONTAINED 'POOLREAD'
C V03 01-AUG-1990 XXX RELEASED FOR VAX
C V02 15-JUL-1989 WS  RELEASED FOR SWEDEN
C V01 26-APR-1989 WS
C
C     PROGRAM TO MANAGE LOTTO POOLS
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
	PROGRAM POOLMGR
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
C
C
	INTEGER*4 POOLFLAG(LTPOOL_MAXSHR),POOLFLAGS
C
	INTEGER*4 OFF1, NEXT, OFF, PAGES, INPAGE, FASTBIN, COMB
	INTEGER*4 POOLFILES, FIRST_PAGE, BONUS, LIMIT, INWORD
	INTEGER*4 HIGH, LOW, GAMEINDEX, GAMETYP, CMD,  CHANGED
	INTEGER*4 ST, GAME, FLAG
C
	CALL COPYRITE
C
C
	TYPE *,IAM(),'This program is used for management of lotto pools'
	TYPE *,IAM(),'It may run only when pools are not updated'
	TYPE *,IAM(),'This program needs SCF.FIL and pool file on system volume'
	TYPE *,IAM(),'Game numbers used by lotto pools are independent '
	TYPE *,IAM(),'from game numbers used by the game subsystem'
	CALL INPYESNO('Do you want to continue? ',FLAG)
	IF (FLAG.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
	CALL POOLREAD
C
	CALL INPNUM('Enter game # to start with ',GAME,1,LTNUMGAMES,ST)
	IF (ST.LT.0) GAME=1
C
	CHANGED=0
C
10	CONTINUE
C
	CALL POOLDISP(GAME)
	TYPE *,IAM(),'1 - change current game you work on '
	TYPE *,IAM(),'2 - modify/enter game information'
	TYPE *,IAM(),'3 - delete the game'
	TYPE *,IAM(),'4 - initialize pools for the game'
	TYPE *,IAM(),'5 - save the changes'
	TYPE *,IAM(),'6 - restore game information from the file'
	TYPE *,IAM(),'E - to exit'
C
	CALL INPNUM('Enter command ',CMD,1,6,ST)
	IF (ST.LT.0.AND.CHANGED.NE.0) THEN
	   CALL INPYESNO('Do you want to exit without saving updates?',FLAG)
	   IF (FLAG.NE.1) GOTO 10
	ENDIF
	IF (ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
	GOTO (100,200,300,400,500,600) CMD
C
100	CONTINUE
	CALL INPNUM('Enter new game # ',GAME,1,LTNUMGAMES,ST)
	GOTO 10
C
C     GAME INFORMATION MODIFICATION
C
200	CONTINUE
	TYPE *,IAM(),'Lotto game type has nr ',TLTO
	TYPE *,IAM(),'1x2 sports betting has game type nr ',TSPT
	CALL INPNUM('Enter game type number ',GAMETYP,1,LTPOOL_MAXTYP,ST)
	IF (ST.LT.0) GOTO 10
	CALL INPNUM('Enter game index ',GAMEINDEX,1,MAXIND,ST)
	IF (ST.LT.0) GOTO 10
C
	IF (GAMETYP.EQ.TSPT) THEN
	   CALL INPNUM('Enter number of rows in bet ',LOW,1,SPGNBR,ST)
	   IF (ST.LT.0) GOTO 10
	   HIGH=LOW
	ELSE
	   CALL INPNUM('Enter high # bet ',HIGH,1,100,ST)
	   IF (ST.LT.0) GOTO 10
	   CALL INPNUM('Enter # of numbers bet ',LOW,1,HIGH,ST)
	   IF (ST.LT.0) GOTO 10
	ENDIF
C
      TYPE *,IAM(),'You can store info in bytes, nibles or halfwords'
	CALL INPYESNO('Do you want to store it in bytes?',FLAG)
	INWORD=8
	LIMIT=15
	IF (FLAG.EQ.1) THEN
	   INWORD=4
	   LIMIT=255
	ENDIF
	CALL INPYESNO('Do you want to store it in halfwords ?',FLAG)
	IF (FLAG.EQ.1) THEN
	   INWORD=2
	   LIMIT='0000FFFF'X
	ENDIF
C
	IF (GAMETYP.EQ.TSPT) THEN
	   BONUS=0
	ELSE
	   CALL INPNUM('Enter # of bonus numbers ',BONUS,0,HIGH-LOW,ST)
	   IF (ST.LT.0) GOTO 10
	ENDIF
C
	CALL INPNUM('Enter first page to store offsets ',FIRST_PAGE,
     *	             1,LTNUMPAG,ST)
	IF (ST.LT.0) GOTO 10
C
	CALL FASTSET(0,POOLFLAG,LTPOOL_MAXSHR)
	TYPE *,IAM(),'Entering now shares to win'
	CALL INPYESNO('Do you match all ?            ',FLAG)
	IF (FLAG.EQ.1) POOLFLAG(1)=1
	IF (FLAG.EQ.1) POOLFLAGS=1
	IF (BONUS.NE.0) THEN
	   CALL INPYESNO('Do you match "all-1" + bonus? ',FLAG)
	   IF (FLAG.EQ.1) POOLFLAG(2)=1
	   IF (FLAG.EQ.1) POOLFLAGS=2
	ENDIF
	CALL INPYESNO('Do you match "all-1"?         ',FLAG)
	IF (FLAG.EQ.1) POOLFLAG(3)=1
	IF (FLAG.EQ.1) POOLFLAGS=3
	IF (BONUS.NE.0) THEN
	   CALL INPYESNO('Do you match "all-2" + bonus? ',FLAG)
	   IF (FLAG.EQ.1) POOLFLAG(4)=1
	   IF (FLAG.EQ.1) POOLFLAGS=4
	ENDIF
	CALL INPYESNO('Do you match "all-2"?         ',FLAG)
	IF (FLAG.EQ.1) POOLFLAG(5)=1
	IF (FLAG.EQ.1) POOLFLAGS=5
	IF (BONUS.NE.0) THEN
	   CALL INPYESNO('Do you match "all-3" + bonus? ',FLAG)
	   IF (FLAG.EQ.1) POOLFLAG(6)=1
	   IF (FLAG.EQ.1) POOLFLAGS=6
	ENDIF
	CALL INPYESNO('Do you match "all-3"?         ',FLAG)
	IF (FLAG.EQ.1) POOLFLAG(7)=1
	IF (FLAG.EQ.1) POOLFLAGS=7
	IF (GAMETYP.EQ.TSPT) THEN
	   CALL INPYESNO('Do you match "all-4"?        ',FLAG)
	   IF (FLAG.EQ.1) POOLFLAG(9)=1
	   IF (FLAG.EQ.1) POOLFLAGS=9
	   CALL INPYESNO('Do you match "all-5"?        ',FLAG)
	   IF (FLAG.EQ.1) POOLFLAG(11)=1
	   IF (FLAG.EQ.1) POOLFLAGS=11
	   CALL INPYESNO('Do you match "all-6"?        ',FLAG)
	   IF (FLAG.EQ.1) POOLFLAG(13)=1
	   IF (FLAG.EQ.1) POOLFLAGS=13
	   CALL INPYESNO('Do you match "all-7"?        ',FLAG)
	   IF (FLAG.EQ.1) POOLFLAG(15)=1
	   IF (FLAG.EQ.1) POOLFLAGS=15
	ENDIF
C
	POOLFILES=1
	CALL INPYESNO('Do you merge more than 1 file for POOLSEE? ',FLAG)
	IF (FLAG.EQ.1) POOLFILES=2
C
C     CHECK IF PAGE IS LEGAL
C
	IF (GAMETYP.EQ.TSPT) THEN
	   COMB=3**HIGH
	ELSE
	   COMB=FASTBIN(HIGH,LOW)
	ENDIF
C
	INPAGE=PAGESIZE*INWORD
	PAGES=COMB/INPAGE
	IF (MOD(COMB,INPAGE).NE.0) PAGES=PAGES+1
	IF (FIRST_PAGE+PAGES-1.GT.LTNUMPAG) THEN
	   TYPE *,IAM(),'Last page invalid '
	   GOTO 10
	ENDIF
C
C     CHECK IF ALL PAGES AVAILABLE
C
	DO 210, OFF=FIRST_PAGE,FIRST_PAGE+PAGES-1
	   IF (LTPOOL_PAGGAM(OFF).NE.0) THEN
	      TYPE *,IAM(),'Cannot allocate enough pages ',PAGES,OFF
	      GOTO 10
	   ENDIF
210	CONTINUE
C
C     SET ALL DATA NOW
C
	LTPOOLFILES(GAME)=POOLFILES
	CALL FASTMOV(POOLFLAG,LTPOOLFLAG(1,GAME),LTPOOL_MAXSHR)
	LTPOOLFLAGS(GAME)=POOLFLAGS
	CALL FASTSET(GAME,LTPOOL_PAGGAM(FIRST_PAGE),PAGES)
	CALL FASTSET(INWORD,LTPOOL_INWORD(FIRST_PAGE),PAGES)
	CALL FASTSET(FIRST_PAGE,LTPOOL_PAGBASE(FIRST_PAGE),PAGES)
	LTPOOLNR(GAME)=HIGH
	LTPOOLBET(GAME)=LOW
	LTPOOL_BONUS(GAME)=BONUS
	LTPOOL_GAMPAG(GAME)=FIRST_PAGE
	LTPOOL_GAMLIMIT(GAME)=LIMIT
	LTPOOL_BASEOFF(GAME)=(FIRST_PAGE-1)*PAGESIZE*8
	LTPOOL_GAMENR(GAMETYP,GAMEINDEX)=GAME
	CALL FASTSET(LTPOOL_BASEOFF(GAME),LTPOOL_OFFSETBASE(FIRST_PAGE)
     *	             ,PAGES)
	NEXT=0
	DO 220, OFF=FIRST_PAGE,FIRST_PAGE+PAGES-1
	   LTPOOL_PAGESTART(OFF)=LTPOOL_BASEOFF(GAME)+NEXT*INPAGE
	   NEXT=NEXT+1
220	CONTINUE
C
	CHANGED=-1
	GOTO 10
C
C     GAME DELETION
C
300	CONTINUE
	CALL INPYESNO('Are you sure you want to delete the game? ',FLAG)
	IF (FLAG.NE.1) GOTO 10
C***  IF (CHANGED.NE.0) THEN
C***     TYPE *,IAM(),'you have to have your changes saved in file first'
C***     GOTO 10
C***  ENDIF
C
	CALL POOLCLR(GAME)
	LTPOOLNR(GAME)=0
	LTPOOLBET(GAME)=0
	CALL FASTSET(0,LTPOOLFLAG(1,GAME),LTPOOL_MAXSHR)
	LTPOOLFILES(GAME)=0
	LTPOOL_BASEOFF(GAME)=0
	LTPOOLFLAGS(GAME)=0
	LTPOOL_GAMPAG(GAME)=0
	LTPOOL_GAMLIMIT(GAME)=0
C
	DO 310, OFF=1,LTNUMPAG
	   IF (LTPOOL_PAGGAM(OFF).EQ.GAME) THEN
	      LTPOOL_PAGGAM(OFF)=0
	      LTPOOL_INWORD(OFF)=0
	      LTPOOL_PAGBASE(OFF)=0
	      LTPOOL_OFFSETBASE(OFF)=0
	      LTPOOL_PAGESTART(OFF)=0
	   ENDIF
310	CONTINUE
C
	DO 320, OFF=1,LTPOOL_MAXTYP
	DO 320, OFF1=1,MAXIND
	   IF (LTPOOL_GAMENR(OFF,OFF1).EQ.GAME)
     *	       LTPOOL_GAMENR(OFF,OFF1)=0
320	CONTINUE
C
	CHANGED=-1
	GOTO 10
C
C     CLEAR SELECTIVELY 1 GAME
C
400	CONTINUE
	TYPE *,IAM(),'This function will save all your changes in file'
	CALL INPYESNO('Are you sure you want to continue?',FLAG)
	IF (FLAG.NE.1) GOTO 10
	CALL POOLWRITE
	CALL POOLCLR(GAME)
	CALL POOLREAD
	CHANGED=0
	GOTO 10
C
C     WRITE THE POOLS DATA
C
500	CONTINUE
	CALL POOLWRITE
	CHANGED=0
	GOTO 10
C
C     READ POOLS DATA
C
600	CONTINUE
	CALL POOLREAD
	CHANGED=0
	GOTO 10
C
C
	END
