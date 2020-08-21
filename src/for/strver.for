C
C SUBROUTINE STRVER
C
C V01 18-MAY-1999 UXN INITIAL RELEASE.
C
C STRVER.FOR
C
C SUBROUTINE TO PROCESS LOTTERY ENTRY OF SUPER TRIPLE RESULTS.
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

C=======OPTIONS /CHECK=NOOVERFLOW

	SUBROUTINE STRVER(GNUM,GIND,DRAW,ST)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'

C---- Local variables.

	INTEGER*4 GIND, DRAW, X, EXT, FLAG, ST, GNUM
	INTEGER*4 TIES1, TIES2, TIES3, ROW, I

	INTEGER*4 STR_IND
	PARAMETER (STR_IND = 2)


C------------------------- Start of code  -----------------------------

	WRITE(5,901) IAM(),GTNAMES(TSTR),GIND,DRAW,
     *	             (DSTENM(X),X=1,4)

	CALL FASTMOV(DSTSTA,STROWSTS(1,2),MAXSTRRW)

C---- Ask if any rows have been cancelled.

5	CONTINUE

	CALL WIMG(5,'Do you want to cancel a row [Y/N] ')

	CALL YESNO(FLAG)

	IF (FLAG .NE. 1) GOTO 10

	CALL INPNUM('Enter row number to cancel ',ROW,1,MAXSTRRW,EXT)

	IF (EXT .LT. 0) GOTO 5

	CALL STR_CANROW(ROW,STR_IND)

	GOTO 5


C---- Ask if any ties for 1st in this event.

10	CONTINUE

	TIES1 = 1

	CALL WIMG(5,'Are there any tied 1st for this event [Y/N] ')

	CALL YESNO(FLAG)

	IF (FLAG .EQ. 1) THEN
	  CALL INPNUM('Enter number of ties ',TIES1,2,3,EXT)
	  IF (EXT .LT. 0) GOTO 10
	  IF(TIES1.EQ.2) THEN
	     TIES2 = 0
	     GOTO 12
	  ELSEIF(TIES1.EQ.3) THEN
	     TIES2 = 0
	     TIES3 = 0
	     GOTO 45
	  ENDIF
	END IF

C---- Ask if any ties for 2nd in this event.

11	CONTINUE

 	TIES2 = 1

	CALL WIMG(5,'Are there any tied 2nd for this event [Y/N] ')

	CALL YESNO(FLAG)

	IF (FLAG .EQ. 1) THEN
	  CALL INPNUM('Enter number of ties ',TIES2,2,3,EXT)
	  IF (EXT .LT. 0) GOTO 11
	END IF
	
	IF(TIES1+TIES2.GE.3) THEN
	    TIES3 = 0
	    GOTO 45
	ENDIF
C
12	CONTINUE

        TIES3 = 1

        CALL WIMG(5,'Are there any tied 3rd for this event [Y/N] ')

        CALL YESNO(FLAG)

        IF (FLAG .EQ. 1) THEN
          CALL INPNUM('Enter number of ties ',TIES3,2,3,EXT)
          IF (EXT .LT. 0) GOTO 12
        END IF

C---- Get winning rows.

45	CONTINUE

	CALL FASTSET(0,DSTHLD,3*MAXSTRTI)

	CALL STRDRAW (TIES1,TIES2,TIES3,STR_IND)

C---- Check against operator entry.

C---- Check winners.

	DO I = 1,MAXSTRTI
	  IF (DSTWIN(1,I) .NE. DSTHLD(1,I) .OR.
     *	      DSTWIN(2,I) .NE. DSTHLD(2,I) .OR.
     *	      DSTWIN(3,I) .NE. DSTHLD(3,I)) THEN
	    TYPE*,IAM(),'Verification error, please re-enter '
	    OPDONE = 0
	    DSTSTS = GAMBFD
	    ST = -1
	    RETURN
	  END IF
	END DO

C---- Verify Cancels

	DO I = 1,MAXSTRRW
	  IF (STROWSTS(I,1) .NE. STROWSTS(I,2)) THEN
	    TYPE*,IAM(),'Verification error in cancels, please re-enter '
	    OPDONE = 0
	    DSTSTS = GAMBFD
	    ST = -1
	    RETURN
	  END IF
	END DO

C---- Verification complete.

	ST = 0

	DSTSTS = GAMENV


C--------------------- Format Statements -----------------------------

901	FORMAT(1X,A,1X,A8,I1,' event ',I4,1X,4A4)

902	FORMAT('Enter ',3A4,' score [C to cancel event]:')

903	FORMAT(1X,A,' Score entered for ',3A4,' is ',I2,/,
     *	       1X,A,' Score entered for ',3A4,' is ',I2)


	RETURN
	END
