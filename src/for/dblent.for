C
C SUBROUTINE DBLENT
C
C V06 30-JUN-2000 UXN DBL_ODDCLC renamed to DBLCLC
C V05 17-DEC-1999 PXO Added a call to report subroutine
C V04 18-MAY-1999 UXN MAXDBLTI added.
C V03 09-JAN-1996 HXK Changed PAUSE to GPAUSE
C V02 24-NOV-1995 HXK Changed TWIT to TDBL
C V01 23-NOV-1995 PXB Initial revision.
C  
C DBLENT.FOR
C
C RESULT ENTRY SUBROUTINE FOR SUPER DOUBLE GAMES
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW/EXT

	SUBROUTINE DBLENT(GNUM,GIND,DRAW)

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
	INTEGER*4 I

	INTEGER*4 DBL_IND         !FLAG IDENTIFIES DBLENT CALL FOR DBLDRAW 
	PARAMETER (DBL_IND = 1)

C------------------------ Start of Code -----------------------------

	EPBFLG = .FALSE.

C---- Open and read files.

	CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,3,DDBSEC*256)
	IF (ST .NE. 0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)

	CALL READW(FDB,DRAW,DDBREC,ST)
	IF (ST .NE. 0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)

C---- Check game status.

	IF (DDBSTS .EQ. GFINAL) THEN
	  WRITE(6,900) IAM(),GTNAMES(TDBL),GIND,DRAW,DDBSTS
	  CALL GPAUSE
	END IF

	DDBLAT(LATCDC) = 0
	DDBLAT(LATTIM) = 0

C---- Set row status

10	CONTINUE

	CALL FASTMOV(DDBSTA,DBROWSTS(1,1),MAXDBLRW)

C---- Ask for cancellation of a row

15	CONTINUE

	CALL PRMYESNO('Do you want to cancel a row (Y/N) ',FLAG)
	IF (FLAG .NE. 1) GOTO 40

	CALL PRMNUM('Enter row number to cancel ',ROW,1,MAXDBLRW,EXT)
	IF (EXT .LT. 0) GOTO 15

	CALL DBL_CANROW(ROW,DBL_IND)

	GOTO 15

C---- Ask if any ties for winner in this event

40	CONTINUE

	TIES1 = 1

	CALL PRMYESNO('Are there any tied 1st for this event (Y/N) ',FLAG)
	IF (FLAG .EQ. 1) THEN
	  CALL PRMNUM('Enter number of ties ',TIES1,2,3,EXT)
	  IF (EXT .LT. 0) GOTO 40
	  TIES2 = 0
	  GOTO 45
	END IF


C---- Ask if any ties for winner in this event.

41	CONTINUE

 	TIES2 = 1

	CALL PRMYESNO('Are there any tied 2nd for this event [Y/N] ',FLAG)
	IF (FLAG .EQ. 1) THEN
	  CALL PRMNUM('Enter number of ties ',TIES2,2,3,EXT)
	  IF (EXT .LT. 0) GOTO 41
	END IF


C---- Get winning rows

45	CONTINUE

	DO 50 I = 1,MAXDBLTI
	  DDBWIN(1,I) = 0
	  DDBWIN(2,I) = 0
	  DDBHLD(1,I) = 0
	  DDBHLD(2,I) = 0
	  DDBODS(I) = 0
50	CONTINUE

	CALL DBLDRAW(TIES1,TIES2,DBL_IND)

60	CONTINUE

	DDBSTS = GAMEN1

	OPDONE = 1

C---- Wait for verification from remote terminal

2000	CONTINUE

	TYPE*,IAM(),' Waiting for remote terminal verification '

	IF (DDBSTS .EQ. GAMBFD) THEN
	  TYPE*,IAM(),' Remote entry does not match, please re-enter'
	  GOTO 10
	END IF

	CALL XWAIT(5,2,ST)

	IF (DDBSTS .NE. GAMENV) GOTO 2000

C---- Calculate winning odds.

	WRITE(6,901) IAM(),GTNAMES(TDBL),GIND

	IF (DDBWIN(1,1) .EQ. -1) THEN
	   WRITE(6,902) IAM(),GTNAMES(TDBL),GIND,DDBDRW
	   DDBSTS = GAMCAN
	   DDBODS(1) = 100
	   DDBWIN(1,1) = 0
	   DDBWIN(2,1) = 0
	   GOTO 2100
	END IF

	WRITE(6,903) IAM(),GTNAMES(TDBL),GIND

	IF (EPBFLG) THEN
	   DDBSTS = GAMOPN
	ELSE
	   CALL DBLCLC(GNUM)
	   DDBSTS = GAMENV
	END IF

2100	CONTINUE

	CALL FASTMOV(DBROWSTS(1,1),DDBSTA,MAXDBLRW)

	CALL WRITEW(FDB,DDBDRW,DDBREC,ST)

	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)

	CALL CLOSEFIL(FDB)

	CALL DBRESULT(GIND,DRAW)

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
