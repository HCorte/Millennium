C
C SUBROUTINE WITENT
C
C V07 30-JUN-2000 UXN ODDCLC changed to WITCLC
C V06 17-DEC-1999 PXO Added a call to report subroutine
C V05 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting,
C                     Instant Pass Thru Phase 1
C V04 21-JAN-1993 DAB Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - witent.for **
C
C WITENT.FOR
C
C V03 05-JAN-93 TD CHANGED CALL TO WDRAW AND CANROW TO INCLUDE FLAG TO 
C               SIGNIFY WITENT AS THE CALLING ROUTINE
C V02 14-MAY-92 GCAN CHANGED ROLL POOL AND BREAKAGE TO BE IN UNITS.
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C
C RESULT ENTRY SUBROUTINE FOR WINNERS TIP GAMES
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
	SUBROUTINE WITENT(GNUM,GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INTEGER*4 FDB(7)
	INTEGER*4 GNUM, GIND, DRAW, ST, ANS, ROW, S, FLAG, EXT
	INTEGER*4 TIES, I
	INTEGER*4 DWIWIN_INDEX    !FLAG IDENTIFIES WITENT CALL FOR WDRAW 
	PARAMETER (DWIWIN_INDEX=1)
C
	EPBFLG=.FALSE.
	CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,3,DWISEC*256)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
	CALL READW(FDB,DRAW,DWIREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
C
	IF(DWISTS.EQ.GFINAL) THEN
	  WRITE(5,900) IAM(),GTNAMES(TWIT),GIND,DRAW,DWISTS
	  PAUSE
	ENDIF
C
	DWILAT(LATCDC) = 0
	DWILAT(LATTIM) = 0
C
C SET ROW STATUS
C
10	CONTINUE
	CALL FASTMOV(DWISTA,WROWSTS(1,1),MAXWRW)
C
C CHECK IF ERALY PAYBACK, IF TRUE, DIAPLAY ROWS BEING PAYDBACK
C
	IF(DWISTS.EQ.GAMOPN.OR.EPBFLG) THEN
	   CALL WIMG(5,'Is this an early payback selection? ')
	   CALL YESNO(ANS)
	   IF(ANS.NE.1) THEN
	      WRITE(5,904) IAM(),GTNAMES(TWIT),GIND,DRAW,DWISTS
	      PAUSE
	   ENDIF
	   EPBFLG=.TRUE.
	   TYPE*,IAM(),' '
	   TYPE*,IAM(),' Following rows are marked to be included in the ',
     *	         'early payback selection. '
	   TYPE*,IAM(),' If you want to include some more rows, please ',
     *	         'cancell them now! '
	   WRITE(5,906) IAM()
	   DO 12 ROW=1,MAXWRW
	      IF(WROWSTS(ROW,1).EQ.GAMBFD.OR.WROWSTS(ROW,1).EQ.GAMCAN)
     *	         THEN
	         WRITE(5,905) IAM(),ROW,(DWINMS(S,ROW),S=1,3)
	         WROWSTS(ROW,1)=GAMCAN
	      ENDIF
12	   CONTINUE
	ENDIF
C
C ASK FOR CANCELLATION OF A ROW
C
15	CONTINUE
	CALL WIMG(5,'Do you want to cancel a row (Y/N) ')
	CALL YESNO(FLAG)
	IF(FLAG.NE.1) GOTO 30
	CALL INPNUM('Enter row number to cancel ',ROW,1,MAXWRW,EXT)
	IF(EXT.LT.0) GOTO 15
	CALL CANROW(ROW,DWIWIN_INDEX)
	GOTO 15
C
C IF EARLY PAYBACK SKIPP TIES AND WINNING ROW ENTRY
C
30	CONTINUE
	IF(EPBFLG) GOTO 60
C
C ASK IF ANY TIES FOR THIS EVENT
C
40	CONTINUE
	TIES=1
	CALL WIMG(5,'Are there any tie wins for this event (Y/N) ')
	CALL YESNO(FLAG)
	IF(FLAG.EQ.1) THEN
	  CALL INPNUM('Enter number of ties ',TIES,2,4,EXT)
	  IF(EXT.LT.0) GOTO 40
	ENDIF
C
C GET WINNING ROWS
C
	DO 50 I=1,4
	DWIWIN(I)=0
	DWIHLD(I)=0
	DWIODS(I)=0
50	CONTINUE
	CALL WDRAW(TIES,DWIWIN_INDEX)
C
60	CONTINUE
	DWISTS=GAMEN1
	OPDONE=1
C
C WAIT FOR VERIFICATION FROM REMOTE TERMINAL
C
2000	CONTINUE
	TYPE*,IAM(),' Waiting for remote terminal verification '
	IF(DWISTS.EQ.GAMBFD) THEN
	  TYPE*,IAM(),' Remote entry does not match, please re-enter'
	  GOTO 10
	ENDIF
	CALL XWAIT(5,2,ST)
	IF(DWISTS.NE.GAMENV) GOTO 2000
C
C CALCULATE WINNING ODDS
C
	WRITE(5,901) IAM(),GTNAMES(TWIT),GIND
	IF(DWIWIN(1).EQ.-1) THEN
	   WRITE(5,902) IAM(),GTNAMES(TWIT),GIND,DWIDRW
	   DWISTS=GAMCAN
	   DWIODS(1)=100
	   DWIWIN(1)=0
	   GOTO 2100
	ENDIF
	WRITE(5,903) IAM(),GTNAMES(TWIT),GIND
	IF(EPBFLG) THEN
	   DWISTS=GAMOPN
	ELSE
	   CALL WITCLC(GNUM,TIES)
	   DWISTS=GAMENV
	ENDIF
2100	CONTINUE
	CALL FASTMOV(WROWSTS(1,1),DWISTA,MAXWRW)
	CALL WRITEW(FDB,DWIDRW,DWIREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
	CALL CLOSEFIL(FDB)
	CALL VORESULT(GIND,DRAW)
	RETURN
C
C
900	FORMAT(1X,A,1X,A8,I1,' event ',I4,' invalid game status> ',I4)
901	FORMAT(1X,A,1X,A8,I1,' results verified at remote terminal')
902	FORMAT(1X,A,1X,A8,I1,' event> ',I4,' has been cancelled')
903	FORMAT(1X,A,' Calculating ',A8,I1,' payout odds')
904	FORMAT(1X,A,1X,A8,I1,' event ',I4,' game status> ',I4,
     *	       ' is invalid for enything BUT early paybacks ')
905	FORMAT(1X,A,1X,'Row ',I2,2X,3A4,' will be included in',
     *	       1X,'payback selection ')
906	FORMAT(1X,A,1X,70('='),/)
	END
