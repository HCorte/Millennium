C
C SUBROUTINE WITVER
C $Log:   GXAFXT:[GOLS]WITVER.FOV  $
C  
C     Rev 1.0   17 Apr 1996 16:02:30   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 18:07:02   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - witver.for **
C
C WITVER.FOR
C
C V03 05-JAN-93 TD  CHANGED CALL TO WDRAW and CANROW, PARAMETER IS PASSED TO
C		    THESE ROUTINES TO IDENTIFY WITVER AS CALLING ROUTINE
C V02 12-NOV-91 MTK INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C REMOTE RESULT ENTRY SUBROUTINE FOR WINNERS TIP GAMES.
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
	SUBROUTINE WITVER(GNUM,GIND,DRAW,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C
	INTEGER*4 GIND, DRAW, ANS, ST, ROW, S, FLAG, EXT, TIES
	INTEGER*4 I, GNUM
	INTEGER*4 DWIHLD_INDEX	!FLAG IDENTIFIES WITVER CALLED WDRAW
	PARAMETER (DWIHLD_INDEX=2)
C
	WRITE(5,901) GTNAMES(TWIT),GIND,DRAW
	CALL FASTMOV(DWISTA,WROWSTS(1,2),MAXWRW)
C
C CHECK IF EARLY PAYBACK, IF TRUE, DISPLAY ROWS BEING PAYDBACK
C
	IF(EPBFLG) THEN
	   CALL WIMG(5,'Is this an early payback selection? ')
	   CALL YESNO(ANS)
	   IF(ANS.NE.1) THEN
	      TYPE*,'Results do not match operator entry '
	      OPDONE=0
	      DWISTS=GAMBFD
	      ST=-1
	      RETURN
	   ENDIF
	   TYPE*,' '
	   TYPE*,'Following rows are marked to be included in the ',
     *	         'early payback selection. '
	   TYPE*,'If you want to include some more rows, please ',
     *	         'cancell them now! '
	   WRITE(5,904)
	   DO 12 ROW=1,MAXWRW
	      IF(WROWSTS(ROW,2).EQ.GAMBFD.OR.WROWSTS(ROW,2).EQ.GAMCAN)
     *	         THEN
	         WRITE(5,903) ROW,(DWINMS(S,ROW),S=1,3)
	         WROWSTS(ROW,2)=GAMCAN
	      ENDIF
12	   CONTINUE
	ENDIF
C
C ASK FOR CANCELLATION OF A ROW
C
15	CONTINUE
	CALL WIMG(5,'Do you want to cancel a row [Y/N] ')
	CALL YESNO(FLAG)
	IF(FLAG.NE.1) GOTO 30
	CALL INPNUM('Enter row number to cancel ',ROW,1,MAXWRW,EXT)
	IF(EXT.LT.0) GOTO 15
	CALL CANROW(ROW,DWIHLD_INDEX)
	GOTO 15
C
C IF EARLY PAYBACK SKIP TIES AND WINNING ROW ENTRY
C
30	CONTINUE
	IF(EPBFLG) GOTO 70
C
C ASK IF ANY TIES FOR THIS EVENT
C
40	CONTINUE
	TIES=1
	CALL WIMG(5,'Are there any tie wins for this event [Y/N] ')
	CALL YESNO(FLAG)
	IF(FLAG.EQ.1) THEN
	  CALL INPNUM('Enter number of ties ',TIES,2,4,EXT)
	  IF(EXT.LT.0) GOTO 40
	ENDIF
C
C GET WINNING ROWS
C
	DO 50 I=1,4
	DWIHLD(I)=0
50	CONTINUE
	CALL WDRAW(TIES,DWIHLD_INDEX)
C
C CHECK RESULTS AGANIST OPERATOR ENTRY
C
	DO 60 I=1,4
	IF(DWIWIN(I).NE.DWIHLD(I)) THEN
	  TYPE*,'Results do not match operator entry'
	  OPDONE=0
	  DWISTS=GAMBFD
	  ST=-1
	  RETURN
	ENDIF
60	CONTINUE
C
70	CONTINUE
C
	DO 80 I=1,MAXWRW
	IF(WROWSTS(I,1).NE.WROWSTS(I,2)) THEN
	  TYPE*,'Results do not match operator entry'
	  OPDONE=0
	  DWISTS=GAMBFD
	  ST=-1
	  RETURN
	ENDIF
80	CONTINUE
	ST=0
	DWISTS=GAMENV
	WRITE(5,902) GTNAMES(TWIT),GIND
	RETURN
C
C
901	FORMAT(1X,A8,I1,' event ',I4)
902	FORMAT(1X,A8,I1,' results have been verified')
903	FORMAT(1X,'Row ',I2,2X,3A4,' will be included in',
     *	       1X,'payback selection ')
904	FORMAT(1X,80('='),/)
	END
