CGXSRC:BLDSTR.FOR
C
C V03 12-JUN-2000 UXN Cleaned up.
C V02 24-MAY-1999 UXN OUTPUT LUN CHANGED TO 6.
C V01 13-MAY-1999 UXN Initial release.  
C
C SUBROUTINE TO DEFINE SUPER TRIPLE GAME PARAMETERS.
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C ******************************************************************
C	discr	: allow user to modify permanent settings for the
C		  winners tip game with ability to 'undo'
C
C	input   : FILE   file name of winners tip game file
C		: GNAME	 long game name
C	output	: NONE   all error handling internal
C ******************************************************************
	

C=======OPTIONS /CHECK=NOOVERFLOW/EXT

	SUBROUTINE BLDSTR (FILE,GNAME)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DSTREC.DEF'

C---- Local variables.

	INTEGER*4  FILE(5)	    !file name
	INTEGER*4  GNAME(4)	    !long game name (display only)

	INTEGER*4  STATUS	    !misc use for file calls
	INTEGER*4  EXIT		    !flag to check for exit
	INTEGER*4  TEMP		    !misc use for undo purpose
	LOGICAL*4  ALL		    !change all parameters
	INTEGER*4  DRAW		    !current draw number
	INTEGER*4  FDB(7)	    !file discriptor block
	INTEGER*4  OPTION	    !menu choice

	INTEGER*4  CSTRTIM	    !closing time
	INTEGER*4  CSTRPRC	    !base price
	INTEGER*4  CSTRSPR	    !pool percentage

C---- Ask user for starting draw

	TYPE *,IAM(),'Select a draw that will be used as default'

	CALL INPNUM('Enter default draw number ',DRAW,1,10000,EXIT)

	IF (EXIT .LT. 0) GOTO 1000

C---- Read existing record and display settings

	CALL OPENW(2,
     *	           FILE,
     *		   4,
     *             0,
     *		   0,
     *		   STATUS)

	CALL IOINIT(FDB,
     *		    2,
     *		    DSTSEC*256)

	IF (STATUS .NE. 0) CALL FILERR(FILE,1,STATUS,0)  

	CALL READW(FDB,
     *	           DRAW,
     *		   DSTREC,
     *		   STATUS)

	IF (STATUS .EQ. 144) THEN
	  TYPE*,'Last draw initialized - ',DRAW-1
	  CALL CLOSEFIL(FDB)
	  CALL XWAIT(2,2,STATUS)
	  GOTO 1000
	END IF

	IF (STATUS .NE. 0) CALL FILERR(FILE,2,STATUS,DRAW)

	CALL CLOSEFIL(FDB)

C---- Assign local variables

	CSTRTIM = DSTTIM
	CSTRPRC = DSTPRC
	CSTRSPR = DSTSPR

C---- Display the record and prompt user for changes

10	CALL CLRSCR(6)

	WRITE(6,700) GNAME

	WRITE(6,710) DISTIM(CSTRTIM)

	WRITE(6,720) CMONY(CSTRPRC,10,BETUNIT)

	WRITE(6,730) DISPER(CSTRSPR)

	WRITE(6,880)

	WRITE(6,885)

	WRITE(6,887)

	WRITE(6,890)

	CALL INPNUM(' Enter option ',OPTION,1,5,EXIT)

	IF (EXIT .LT. 0) GOTO 1000

	ALL = .FALSE.

	IF (OPTION .EQ. 4) ALL = .TRUE.
	
	IF (OPTION .EQ. 1 .OR. ALL) THEN
	  CALL INPTIM('Enter pool close time HH:MM:SS ',TEMP,EXIT)
	  IF (EXIT .LT. 0) GOTO 10
	  CSTRTIM = TEMP
	END IF

	IF (OPTION .EQ. 2 .OR. ALL) THEN
	  CALL INPMONY('Enter base bet price ',TEMP,BETUNIT,EXIT)
	  IF (EXIT .LT. 0) GOTO 10
	  CSTRPRC = TEMP
	END IF

	IF (OPTION .EQ. 3 .OR. ALL) THEN
	  CALL INPPER('Enter pool percentage',TEMP,EXIT)
	  IF (EXIT .LT. 0) GOTO 10
	  CSTRSPR = TEMP
	END IF
	
	IF (OPTION .EQ. 5) THEN
	  GOTO 500
	END IF

	GOTO 10	
	

C--- Update the game file starting at draw DRAW

500	CONTINUE

	CALL INPNUM(
     *	 'Enter starting draw number to update (will update to eof)',
     *	  DRAW,1,10000,EXIT)

	IF (EXIT .LT. 0) GOTO 1000

C---- Update the file by looping through all the draws

	WRITE(6,900) FILE

	CALL OPENW(2,
     *	           FILE,
     *             4,
     *		   0,
     *             0,
     *             STATUS)

	CALL IOINIT(FDB,
     *              2,
     *		    DSTSEC*256)

	IF (STATUS .NE. 0) CALL FILERR(FILE,1,STATUS,0)

C---- write loop

510	CONTINUE

	CALL READW(FDB,DRAW,DSTREC,STATUS)

	IF (STATUS .EQ. 144) THEN
	  TYPE*,'Last draw initialized - ',DRAW-1
	  CALL CLOSEFIL(FDB)
	  CALL XWAIT(2,2,STATUS)
	  RETURN
	END IF

	IF (STATUS .NE. 0) CALL FILERR(FILE,2,STATUS,DRAW)

	IF (DSTSTS .GT. GAMOPN) THEN
	  TYPE*,'Game already closed for draw ',DRAW
	  CALL GPAUSE
	  CALL CLOSEFIL(FDB)
	  GOTO 500
	END IF

	DSTTIM = CSTRTIM
	DSTPRC = CSTRPRC
	DSTSPR = CSTRSPR

	CALL WRITEW(FDB,
     *              DRAW,
     *		    DSTREC,
     *		    STATUS)

	IF (STATUS .EQ. 144) THEN
	  TYPE*,'Last draw initialized ',DRAW
	  CALL XWAIT(2,2,STATUS)
	  GOTO 10
	END IF

	IF (STATUS .NE. 0) CALL FILERR(FILE,3,STATUS,DRAW)

	DRAW = DRAW + 1

	GOTO 510

1000	CONTINUE

	CALL CLRSCR(6)

	RETURN

C---- Format stuff

700	FORMAT(' Current settings for ',4A4,':')

710	FORMAT('   1  - game closing time......:',A8)

720	FORMAT('   2  - base price.............:',A10)

730	FORMAT('   3  - pool percentage........:',F10.2)

880	FORMAT('   4  - change all')

885	FORMAT('   - -')

887	FORMAT('   5  - update draws')

890	FORMAT('   E  - exit')

900	FORMAT(' Updating ',5A4,' with game parameters')

	END
