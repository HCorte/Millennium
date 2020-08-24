C
C SUBROUTINE DBLVER
C
C V03 18-MAY-1999 UXN MAXDBLTI added.
C V02 04-JAN-1996 PXB Fixed bug in cancellations
C V01 23-NOV-1995 PXB Initial revision.
C
C DBLVER.FOR
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

	SUBROUTINE DBLVER(GNUM,GIND,DRAW,ST)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'

C---- Local variables.

	INTEGER*4 GIND, DRAW, X, K, NUM, EXT, FLAG, ST, GNUM
	INTEGER*4 TIES1, TIES2, ROW, I

	INTEGER*4 DBL_IND
	PARAMETER (DBL_IND = 2)

	CHARACTER*45 BUF


C------------------------- Start of code  -----------------------------

	WRITE(5,901) IAM(),GTNAMES(TDBL),GIND,DRAW,
     *	             (DDBENM(X),X=1,4)

	CALL FASTMOV(DDBSTA,DBROWSTS(1,2),MAXDBLRW)

C---- Ask if any rows have been cancelled.

5	CONTINUE

	CALL WIMG(5,'Do you want to cancel a row [Y/N] ')

	CALL YESNO(FLAG)

	IF (FLAG .NE. 1) GOTO 10

	CALL INPNUM('Enter row number to cancel ',ROW,1,MAXDBLRW,EXT)

	IF (EXT .LT. 0) GOTO 5

	CALL DBL_CANROW(ROW,DBL_IND)

	GOTO 5


C---- Ask if any ties for 1st in this event.

10	CONTINUE

	TIES1 = 1

	CALL WIMG(5,'Are there any tied 1st for this event [Y/N] ')

	CALL YESNO(FLAG)

	IF (FLAG .EQ. 1) THEN
	  CALL INPNUM('Enter number of ties ',TIES1,2,4,EXT)
	  IF (EXT .LT. 0) GOTO 10
	  TIES2 = 0
	  GOTO 12
	END IF

C---- Ask if any ties for 2nd in this event.

11	CONTINUE

 	TIES2 = 1

	CALL WIMG(5,'Are there any tied 2nd for this event [Y/N] ')

	CALL YESNO(FLAG)

	IF (FLAG .EQ. 1) THEN
	  CALL INPNUM('Enter number of ties ',TIES2,2,4,EXT)
	  IF (EXT .LT. 0) GOTO 11
	END IF

C---- Get winning rows.

12	CONTINUE

	CALL FASTSET(0,DDBHLD,2*MAXDBLTI)

	CALL DBLDRAW (TIES1,TIES2,DBL_IND)

C---- Check against operator entry.

C---- Check winners.

	DO I = 1,MAXDBLTI
	  IF (DDBWIN(1,I) .NE. DDBHLD(1,I) .OR.
     *	      DDBWIN(2,I) .NE. DDBHLD(2,I)) THEN
	    TYPE*,IAM(),'Verification error, please re-enter '
	    OPDONE = 0
	    DDBSTS = GAMBFD
	    ST = -1
	    RETURN
	  END IF
	END DO

C---- Verify Cancels

	DO I = 1,MAXDBLRW
	  IF (DBROWSTS(I,1) .NE. DBROWSTS(I,2)) THEN
	    TYPE*,IAM(),'Verification error in cancels, please re-enter '
	    OPDONE = 0
	    DDBSTS = GAMBFD
	    ST = -1
	    RETURN
	  END IF
	END DO

C---- Verification complete.

	ST = 0

	DDBSTS = GAMENV


C--------------------- Format Statements -----------------------------

901	FORMAT(1X,A,1X,A8,I1,' event ',I4,1X,4A4)

902	FORMAT('Enter ',3A4,' score [C to cancel event]:')

903	FORMAT(1X,A,' Score entered for ',3A4,' is ',I2,/,
     *	       1X,A,' Score entered for ',3A4,' is ',I2)


	RETURN
	END
