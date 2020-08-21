C
C SUBROUTINE CPLVER
C  
C V06 18-MAY-1999 UXN DCPWIN checking corrected.
C V05 25-JAN-1996 HXK Fix Couple game
C V04 11-JAN-1996 PXB bug fix on event cancelations
C V03 04-JAN-1996 PXB Fixed bug in cancellations
C V02 28-NOV-1995 PXB Added event cancellation
C V01 23-NOV-1995 PXB Initial revision.
C
C CPLVER.FOR
C
C SUBROUTINE TO PROCESS LOTTERY ENTRY OF SUPER DOUBLE RESULTS.
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

C=======OPTIONS /CHECK=NOOVERFLOW

	SUBROUTINE CPLVER(GNUM,GIND,DRAW,ST)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'

C---- Local variables.

	INTEGER*4 GIND
	INTEGER*4 DRAW
	INTEGER*4 X
	INTEGER*4 K
	INTEGER*4 NUM
	INTEGER*4 EXT
	INTEGER*4 FLAG
	INTEGER*4 ST
	INTEGER*4 GNUM
	INTEGER*4 TIES1
	INTEGER*4 TIES2
	INTEGER*4 ROW
	INTEGER*4 I
	INTEGER*4 ENUM

	INTEGER*4 CPL_IND
	PARAMETER (CPL_IND = 2)

	CHARACTER*45 BUF


C------------------------- Start of code  -----------------------------

	WRITE(5,901) IAM(),GTNAMES(TCPL),GIND,DRAW,
     *	             (DCPENM(X,1),X=1,4),
     *	             (DCPENM(X,2),X=1,4)

	CALL FASTMOV(DCPSTA,CPROWSTS(1,2),MAXCPLRW)

C---- Ask for cancellation of a row in event 1

20	CONTINUE

	CALL WIMG(5,'Do you want to cancel a row in event 1 (Y/N) ')

	CALL YESNO(FLAG)

	IF (FLAG .NE. 1) GOTO 30

	CALL INPNUM('Enter row number to cancel ',ROW,1,(MAXCPLRW/2),EXT)

	IF (EXT .LT. 0) GOTO 20

	CALL CPL_CANROW(ROW,CPL_IND)

	GOTO 20

C---- Ask for cancellation of a row in event 2

30	CONTINUE

	CALL WIMG(5,'Do you want to cancel a row in event 2 (Y/N) ')

	CALL YESNO(FLAG)

	IF (FLAG .NE. 1) GOTO 40

	CALL INPNUM('Enter row number to cancel ',ROW,1,(MAXCPLRW/2),EXT)

	IF (EXT .LT. 0) GOTO 30

	ROW = ROW + (MAXCPLRW/2)

	CALL CPL_CANROW(ROW,CPL_IND)

	GOTO 30

C---- Ask if any ties for winner in this event.

40	CONTINUE

	TIES1 = 1

	CALL WIMG(5,'Are there any tied winners for event 1 [Y/N] ')

	CALL YESNO(FLAG)

	IF (FLAG .EQ. 1) THEN
	  CALL INPNUM('Enter number of ties ',TIES1,2,4,EXT)
	  IF (EXT .LT. 0) GOTO 40
	END IF

C---- Ask if any ties for winner in this event.

41	CONTINUE

 	TIES2 = 1

	CALL WIMG(5,'Are there any tied winners for event 2 [Y/N] ')

	CALL YESNO(FLAG)

	IF (FLAG .EQ. 1) THEN
	  CALL INPNUM('Enter number of ties ',TIES2,2,4,EXT)
	  IF (EXT .LT. 0) GOTO 41
	END IF

C---- Get winning rows.

	DO I= 1,12
	  DCPHLD(1,I) = 0
	  DCPHLD(2,I) = 0
	END DO

	CALL CPLDRAW (TIES1,TIES2,CPL_IND)

C---- Check against operator entry.

80	CONTINUE

	DO I = 1,MAXCPLTI
	  IF (DCPWIN(1,I) .NE. DCPHLD(1,I) .OR.
     *	      DCPWIN(2,I) .NE. DCPHLD(2,I)) THEN
	    TYPE*,IAM(),'Verification error, please re-enter '
	    OPDONE = 0
	    DCPSTS = GAMBFD
	    ST = -1
	    RETURN
	  END IF
	END DO

C---- Verify Cancels

	DO I = 1,MAXCPLRW
	  IF (CPROWSTS(I,1) .NE. CPROWSTS(I,2)) THEN
	    TYPE*,IAM(),'Verification error in cancels, please re-enter '
	    OPDONE = 0
	    DCPSTS = GAMBFD
	    ST = -1
	    RETURN
	  END IF
	END DO

	ST = 0

	DCPSTS = GAMENV


C--------------------- Format Statements -----------------------------

901	FORMAT(1X,A,1X,A8,I1,' draw ',I4,' event 1 ',4A4,
     *         22X,' event 2 ',4A4)

902	FORMAT('Enter ',3A4,' score [C to cancel event]:')

903	FORMAT(1X,A,' Score entered for ',3A4,' is ',I2,/,
     *	       1X,A,' Score entered for ',3A4,' is ',I2)

907	FORMAT(1X,A,' Cancellation of event ',I4,1X,4A4)

	RETURN
	END
