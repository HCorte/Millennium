C
C PROGRAM GAMSET
C
C GAMSET.FOR
C
C V17 29-NOV-2000 JHR Add Results Game Type
C V16 23-NOV-2000 UXN Logical unit for SYS$OUTPUT is 6.
C V15 13-OCT-1999 RXK World Tour added.
C V14 14-MAY-1999 UXN Super Triple added.
C V13 10-MAY-1999 UXN Today's Triple changed to Today's Trio.
C V12 19-MAY-1997 RXK Fix for RFSS #305.
C V11 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting, 
C                     Instant Pass Thru Phase 1
C V10 23-NOV-1995 PXB Added super double and todays couple
C V09 15-OCT-1994 HXK Adding /developing Bingo (15.Oct.94)
C V08 07-SEP-1993 HXK Changed STOP to GSTOP
C V07 07-SEP-1993 HXK Changed copyrite date
C V06 14-JUL-1993 HXK CHANGED FORMAT STATEMENT TO ALIGN OUTPUT TO SCREEN
C V05 13-JUL-1993 HXK AMENDED DATA ENTRY MENU
C V04 07-JUL-1993 HXK ADDED SPEDE, RAVI GAMES
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 07-OCT-1991 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C ODDSET GAME SETUP PROGRAM
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM GAMSET
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
	COMMON SCFREC
	INTEGER*4 ST,I,GIND,EXT,GNUM,GTYP
	CHARACTER*2 FUN
	CHARACTER*4 PROMPT
	DATA PROMPT/'FUN '/
C
C
	CALL COPYRITE
C
C READ SCF RECORD
C
	CALL GETSCONF(SCFREC,ST)
C
100	CONTINUE
	CALL CLRSCR(6)
	WRITE(6,900)
	CALL WIMG(6,PROMPT)
	READ(5,901) FUN
C
C PROCESS REQUEST
C
	IF(FUN.EQ.'TS') THEN
	  CALL INPNUM('Enter Toto Select game index ',GIND,1,6,EXT)
	  IF(EXT.EQ.0) CALL TSLSET(GIND)
	  GOTO 100
	ENDIF
C
C
	IF(FUN.EQ.'SC') THEN
	  CALL INPNUM('Enter Score game index ',GIND,1,MAXIND,EXT)
	  IF(EXT.EQ.0) CALL SCRSET(GIND)
	  GOTO 100
	ENDIF
C
C
	IF(FUN.EQ.'WT') THEN
	  CALL INPNUM('Enter Winners Tip game index ',GIND,1,MAXIND,EXT)
	  IF(EXT.EQ.0) CALL WITSET(GIND)
	  GOTO 100
	ENDIF
C
C
	IF(FUN.EQ.'SP') THEN
	   CALL INPNUM('Enter Sports game index ',GIND,1,MAXIND,EXT)
	   IF(EXT.EQ.0) CALL SPTSET(GIND)
	   GOTO 100
	ENDIF
C
C
        IF(FUN.EQ.'TG') THEN
           CALL INPNUM('Enter Results game index ',GIND,1,MAXIND,EXT)
           IF(EXT.EQ.0) CALL TGLSET(GIND)
           GOTO 100
        ENDIF
C
C
	IF(FUN.EQ.'DB') THEN
	   CALL INPNUM('Enter Super Double game index ',GIND,1,MAXIND,EXT)
	   IF(EXT.EQ.0) CALL DBLSET(GIND)
	   GOTO 100
	ENDIF
C
C
C
	IF(FUN.EQ.'CP') THEN
	   CALL INPNUM('Enter Todays Couple game index ',GIND,1,MAXIND,EXT)
	   IF(EXT.EQ.0) CALL CPLSET(GIND)
	   GOTO 100
	ENDIF
C
C
C
	IF(FUN.EQ.'SS') THEN
	   CALL INPNUM('Enter Superscore game index ',GIND,1,MAXIND,EXT)
	   IF(EXT.EQ.0) CALL SSCSET(GIND)
	   GOTO 100
	ENDIF
C
C
C
	IF(FUN.EQ.'TR') THEN
	   CALL INPNUM('Enter Todays Trio game index ',GIND,1,MAXIND,EXT)
	   IF(EXT.EQ.0) CALL TRPSET(GIND)
	   GOTO 100
	ENDIF	
C
        IF(FUN.EQ.'ST') THEN
	   CALL INPNUM('Enter Super Triple game index ',GIND,1,MAXIND,EXT)
	   IF(EXT.EQ.0) CALL STRSET(GIND)
	   GOTO 100
	ENDIF
C
C
	IF(FUN.EQ.'LO') THEN
C	  TYPE*,'SORRY, FUNCTION IS DISABLED'
C	  CALL XWAIT(2,2,ST)
	  CALL INPNUM('Enter game number ',GNUM,1,MAXGAM,EXT)
	  IF(EXT.EQ.0) CALL GETGAM(GNUM)
	  GOTO 100
	ENDIF
C
C
	IF(FUN.EQ.'SA') THEN
	  CALL SALES
	  GOTO 100
	ENDIF
C
C
	IF(FUN.EQ.'DA') THEN
200	  CONTINUE
	  CALL CLRSCR(6)
          TYPE*,'Game Types that can be entered:'
          DO I=1,MAXTYP
            IF(I.NE.TWIT.AND.I.NE.TSCR.AND.I.NE.TTSL.AND.I.NE.TDBL.AND.
     *         I.NE.TCPL.AND.I.NE.TSSC.AND.I.NE.TTRP.AND.I.NE.TSTR.AND.
     *         I.LT.16) THEN                   ! LAST ACTIVE GAME SET 
              WRITE(6,903) I,GTNAMES(I)
            ENDIF
          ENDDO   
	  CALL INPNUM('Enter game type:',GTYP,1,MAXTYP,EXT)
	  IF(EXT.LT.0) GOTO 100        
	  CALL INPNUM('Enter game index ',GIND,1,MAXIND,EXT)
	  IF(EXT.LT.0) GOTO 100
          IF(GTYP.EQ.TWIT.OR.GTYP.EQ.TSCR.OR.GTYP.EQ.TTSL.OR.
     *       GTYP.EQ.TDBL.OR.GTYP.EQ.TCPL.OR.
     *       GTYP.EQ.TTRP.OR.GTYP.EQ.TSSC.OR.
     *       GTYP.EQ.TSTR) THEN
            TYPE*,'Sorry, game data cannot be entered here'
            TYPE*,'for this game type'
            CALL XWAIT(2,2,ST)
            GOTO 200
          ENDIF  
	  GNUM=SCFGTN(GTYP,GIND)
	  IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
	    TYPE*,'Sorry, selected game not active'
	    CALL XWAIT(2,2,ST)
	    GOTO 200
	  ENDIF
C	  IF(GTYP.EQ.TSPT.AND.SCFSTP(GIND).NE.1) THEN
C	    TYPE*,'Use SP function to set special sports draws'
C	    CALL XWAIT(2,2,ST)
C	    GOTO 100
C	  ENDIF
	  CALL DATSUB(SCFSFN(1,DAF),SCFGFN(1,GNUM),GNUM,GTYP)
	  GOTO 100
	ENDIF
C
C
	IF(FUN.EQ.'HI') THEN
	  CALL HIDRAW
	  GOTO 100
	ENDIF
C
C PROGRAM EXIT
C
	IF(FUN.EQ.'EX')THEN
	  CALL CLRSCR(6)
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
C INVALID ENTRY
C
	CALL CLRSCR(6)
	CALL BELLS(1)
	WRITE(6,902)
	CALL XWAIT(1,2,ST)
	GOTO 100
C
C
900	FORMAT(/,' GAMSET functions:',
     *	/,T5,'TS',5X,'- Toto Select  game set',
     *	/,T5,'SC',5X,'- Score        game set',
     *	/,T5,'WT',5X,'- Winners tip  game set',
     *	/,T5,'SP',5X,'- Sports       game set',
     *  /,T5,'TG',5X,'- Results      game set',
     *	/,T5,'DB',5X,'- Super dbl    game set',
     *	/,T5,'CP',5X,'- Todays cpl   game set',
     *	/,T5,'SS',5X,'- Superscore   game set',
     *	/,T5,'TR',5X,'- Todays Trio  game set',
     *	/,T5,'ST',5X,'- Super Triple game set',
     *	/,T5,'SA',5X,'- Set/change sales dates',
     *	/,T5,'DA',5X,'- Set lotto/sports/results/joker/numbers',
     *               '/bingo draw dates ',
     *	/,T5,'HI',5X,'- Set high draw numbers ',
     *	/,T5,'LO',5X,'- Load game text in memory ',
     *	/,T5,'EX',5X,'- Program exit',//)
901	FORMAT(A2)
902	FORMAT(' *** Entry error *** ')
903     FORMAT(1X,I2,'- ',A8)
	END
