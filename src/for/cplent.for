C
C SUBROUTINE CPLENT
C
C V08 30-JUN-2000 UXN CPL_ODDCLC renamed to CPLCLC
C V07 17-DEC-1999 PXO Added a call to report subroutine
C V06 18-MAY-1999 UXN MAXCPLTI added.
C V05 25-JAN-1996 HXK Fix fo cancelled event
C V04 04-JAN-1996 PXB Fixed bugin cancellations
C V03 28-NOV-1995 PXB Added event cancellation 
C V02 23-NOV-1995 HXK Changed a TWIT to a TCPL
C V01 23-NOV-1995 PXB Initial revision.
C  
C CPLENT.FOR
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

	SUBROUTINE CPLENT(GNUM,GIND,DRAW)

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

	INTEGER*4 CPL_IND         !FLAG IDENTIFIES CPLENT CALL FOR CPLDRAW 
	PARAMETER (CPL_IND = 1)

C------------------------ Start of Code -----------------------------

	EPBFLG = .FALSE.

C---- Open and read files.

	CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,3,DCPSEC*256)

	IF (ST .NE. 0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)

	CALL READW(FDB,DRAW,DCPREC,ST)
	IF (ST .NE. 0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)

C---- Check game status.

	IF (DCPSTS .EQ. GFINAL) THEN
	  WRITE(6,900) IAM(),GTNAMES(TCPL),GIND,DRAW,DCPSTS
	  CALL GPAUSE
	END IF

	DCPLAT(LATCDC) = 0
	DCPLAT(LATTIM) = 0

C---- Set row status

10	CONTINUE

	CALL FASTMOV(DCPSTA,CPROWSTS(1,1),MAXCPLRW)

C---- Ask for cancellation of a row in event 1

20	CONTINUE

	CALL PRMYESNO('Do you want to cancel a row in event 1 (Y/N) ',FLAG)
	IF (FLAG .NE. 1) GOTO 30

	CALL PRMNUM('Enter row number to cancel ',ROW,1,(MAXCPLRW/2),EXT)
	IF (EXT .LT. 0) GOTO 20

	CALL CPL_CANROW(ROW,CPL_IND)

	GOTO 20

C---- Ask for cancellation of a row in event 2

30	CONTINUE

	CALL PRMYESNO('Do you want to cancel a row in event 2 (Y/N) ',FLAG)
	IF (FLAG .NE. 1) GOTO 40

	CALL PRMNUM('Enter row number to cancel ',ROW,1,(MAXCPLRW/2),EXT)
	IF (EXT .LT. 0) GOTO 30

	ROW = ROW + (MAXCPLRW/2)

	CALL CPL_CANROW(ROW,CPL_IND)

	GOTO 30

C---- Ask if any ties for winner in this event

40	CONTINUE

	TIES1 = 1

	CALL PRMYESNO('Are there any tied winners for event 1 [Y/N] ',FLAG)
	IF (FLAG .EQ. 1) THEN
	  CALL PRMNUM('Enter number of ties ',TIES1,2,4,EXT)
	  IF (EXT .LT. 0) GOTO 40
	END IF


C---- Ask if any ties for winner in this event.

41	CONTINUE

 	TIES2 = 1

	CALL PRMYESNO('Are there any tied winners for event 2 [Y/N] ',FLAG)
	IF (FLAG .EQ. 1) THEN
	  CALL PRMNUM('Enter number of ties ',TIES2,2,4,EXT)
	  IF (EXT .LT. 0) GOTO 41
	END IF


C---- Get winning rows

	DO 50 I = 1,MAXCPLTI
	  DCPWIN(1,I) = 0
	  DCPWIN(2,I) = 0
	  DCPHLD(1,I) = 0
	  DCPHLD(2,I) = 0
	  DCPODS(I) = 0
50	CONTINUE

	CALL CPLDRAW(TIES1,TIES2,CPL_IND)

60	CONTINUE

	DCPSTS = GAMEN1

	OPDONE = 1

C---- Wait for verification from remote terminal

2000	CONTINUE

	TYPE*,IAM(),' Waiting for remote terminal verification '

	IF (DCPSTS .EQ. GAMBFD) THEN
	  TYPE*,IAM(),' Remote entry does not match, please re-enter'
	  GOTO 10
	END IF

	CALL XWAIT(5,2,ST)

	IF (DCPSTS .NE. GAMENV) GOTO 2000

C---- Calculate winning odds.

	WRITE(6,901) IAM(),GTNAMES(TCPL),GIND

	IF (DCPWIN(1,1) .EQ. -1) THEN
	   WRITE(5,902) IAM(),GTNAMES(TCPL),GIND,DCPDRW
	   DCPSTS = GAMCAN
	   DCPODS(1) = 100
	   DCPWIN(1,1) = 0
	   DCPWIN(2,1) = 0
	   GOTO 2100
	END IF

	WRITE(6,903) IAM(),GTNAMES(TCPL),GIND

	IF (EPBFLG) THEN
	   DCPSTS = GAMOPN
	ELSE
	   CALL CPLCLC(GNUM)
	   DCPSTS = GAMENV
	END IF

2100	CONTINUE

	CALL FASTMOV(CPROWSTS(1,1),DCPSTA,MAXCPLRW)

	CALL WRITEW(FDB,DCPDRW,DCPREC,ST)

	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)

	CALL CLOSEFIL(FDB)

	CALL CPRESULT(GIND,DRAW)

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

907	FORMAT(1X,A,' Cancellation of event ',I4,1X,4A4)

	END
