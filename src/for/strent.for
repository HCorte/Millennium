C
C SUBROUTINE STRENT
C  
C V03 30-JUN-2000 UXN Refund too late played tickets.
C V02 17-DEC-1999 PXO Added a call to report subroutine
C V01 17-MAY-1999 UXN Initial release.
C
C RESULT ENTRY SUBROUTINE FOR SUPER TRIPLE GAMES
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

C=======OPTIONS /CHECK=NOOVERFLOW/EXT

	SUBROUTINE STRENT(GNUM,GIND,DRAW)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'

C---- Local variables used.

	INTEGER*4 FDB(7)
	INTEGER*4 GNUM
	INTEGER*4 GIND
	INTEGER*4 DRAW
	INTEGER*4 ST
	INTEGER*4 ROW
	INTEGER*4 FLAG
	INTEGER*4 EXT
	INTEGER*4 TIES1
	INTEGER*4 TIES2
	INTEGER*4 TIES3

	INTEGER*4 STR_IND         !FLAG IDENTIFIES STRENT CALL FOR STRDRAW 
	PARAMETER (STR_IND = 1)

C------------------------ Start of Code -----------------------------

	EPBFLG = .FALSE.

C---- Open and read files.

	CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,3,DSTSEC*256)
	IF (ST .NE. 0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)

	CALL READW(FDB,DRAW,DSTREC,ST)
	IF (ST .NE. 0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)

C---- Check game status.

	IF (DSTSTS .EQ. GFINAL) THEN
	  WRITE(6,900) IAM(),GTNAMES(TSTR),GIND,DRAW,DSTSTS
	  CALL GPAUSE
	END IF
C
	DSTLAT(LATCDC) = 0
	DSTLAT(LATTIM) = 0
C
C---- Set row status

10	CONTINUE

	CALL FASTMOV(DSTSTA,STROWSTS(1,1),MAXSTRRW)

C---- Ask for cancellation of a row

15	CONTINUE

	CALL INPYESNO('Do you want to cancel a row (Y/N) ',FLAG)
	IF (FLAG .NE. 1) GOTO 40

	CALL INPNUM('Enter row number to cancel ',ROW,1,MAXSTRRW,EXT)
	IF (EXT .LT. 0) GOTO 15

	CALL STR_CANROW(ROW,STR_IND)

	GOTO 15

C---- Ask if any ties for winner in this event

40	CONTINUE

	TIES1 = 1

	CALL INPYESNO('Are there any tied 1st for this event (Y/N) ',FLAG)
	IF (FLAG .EQ. 1) THEN
	  CALL INPNUM('Enter number of ties ',TIES1,2,3,EXT)
	  IF (EXT .LT. 0) GOTO 40
	  IF(TIES1.EQ.2) THEN
	     TIES2 = 0
	     GOTO 42
	  ELSEIF(TIES1.EQ.3) THEN
	     TIES2 = 0
	     TIES3 = 0
	     GOTO 45
          ENDIF
	ENDIF


C---- Ask if any ties for winner in this event.

41	CONTINUE

 	TIES2 = 1

	CALL INPYESNO('Are there any tied 2nd for this event [Y/N] ',FLAG)
	IF (FLAG .EQ. 1) THEN
	  CALL INPNUM('Enter number of ties ',TIES2,2,3,EXT)
	  IF (EXT .LT. 0) GOTO 41
	END IF

	IF(TIES1+TIES2.GE.3) THEN
	   TIES3 = 0
	   GOTO 45
	ENDIF

C---- Ask if any ties for winner in this event.

42	CONTINUE

 	TIES3 = 1

	CALL INPYESNO('Are there any tied 3rd for this event [Y/N] ',FLAG)
	IF (FLAG .EQ. 1) THEN
	  CALL INPNUM('Enter number of ties ',TIES3,2,3,EXT)
	  IF (EXT .LT. 0) GOTO 42
	END IF

C---- Get winning rows

45	CONTINUE

	CALL FASTSET(0,DSTWIN,3*MAXSTRTI)
	CALL FASTSET(0,DSTHLD,3*MAXSTRTI)
	CALL FASTSET(0,DSTODS,MAXSTRTI)

	CALL STRDRAW(TIES1,TIES2,TIES3,STR_IND)

60	CONTINUE

	DSTSTS = GAMEN1

	OPDONE = 1

C---- Wait for verification from remote terminal

2000	CONTINUE

	TYPE*,IAM(),' Waiting for remote terminal verification '

	IF (DSTSTS .EQ. GAMBFD) THEN
	  TYPE*,IAM(),' Remote entry does not match, please re-enter'
	  GOTO 10
	END IF

	CALL XWAIT(5,2,ST)

	IF (DSTSTS .NE. GAMENV) GOTO 2000

C---- Calculate winning odds.

	WRITE(6,901) IAM(),GTNAMES(TSTR),GIND

	IF (DSTWIN(1,1) .EQ. -1) THEN
	   WRITE(5,902) IAM(),GTNAMES(TSTR),GIND,DSTDRW
	   DSTSTS = GAMCAN
	   DSTODS(1) = 100
	   DSTWIN(1,1) = 0
	   DSTWIN(2,1) = 0
	   DSTWIN(3,1) = 0
	   GOTO 2100
	END IF

	WRITE(6,903) IAM(),GTNAMES(TSTR),GIND

	IF (EPBFLG) THEN
	   DSTSTS = GAMOPN
	ELSE
	   CALL STRCLC(GNUM)
	   DSTSTS = GAMENV
	END IF

2100	CONTINUE

	CALL FASTMOV(STROWSTS(1,1),DSTSTA,MAXSTRRW)

	CALL WRITEW(FDB,DSTDRW,DSTREC,ST)

	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)

	CALL CLOSEFIL(FDB)

	CALL STRESULT(GIND,DRAW)

	RETURN

C------------------------ Format statements ---------------------------

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
