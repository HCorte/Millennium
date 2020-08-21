C
C SUBROUTINE BLDSSC
C
C V01 23-DEC-97 RXK Initial release.  
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
C	discr	: allow user to modify permanent settings for the
C		  superscore game with ability to 'undo'
C
C	input   : FILE   file name of superscore game file
C		: GNAME	 long game name
C	output	: NONE   all error handling internal
C	
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE BLDSSC(FILE,GNAME)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DSSREC.DEF'
C	INCLUDE 'INCLIB:TNAMES.DEF'
C
	INTEGER*4 FILE(5)	!file name
	INTEGER*4 GNAME(4)	!long game name (display only)
C
	INTEGER*4 STATUS	!misc use for file calls
	INTEGER*4 EXIT		!flag to check for exit
	INTEGER*4 TEMP		!misc use for undo purpose
	LOGICAL*4 ALL		!change all parameters
	INTEGER*4 DRAW		!current draw number
	INTEGER*4 FDB(7)	!file discriptor block
	INTEGER*4 OPTION	!menu choice
	INTEGER*4 CDSSTIM	!closing time
	INTEGER*4 CDSSPRC	!base price
	INTEGER*4 CDSSSPR	!pool percentage
C
C ask user for starting draw
C
	TYPE *,IAM(),'Select a draw that will be used as default'
	CALL INPNUM('Enter default draw number ',DRAW,1,10000,EXIT)
	IF(EXIT.LT.0) GOTO 1000
C
C read existing record and display settings
C
	CALL OPENW(2,FILE,4,0,0,STATUS)
	CALL IOINIT(FDB,2,DSSSEC*256)
	IF(STATUS.NE.0) CALL FILERR(FILE,1,STATUS,0)  
	CALL READW(FDB,DRAW,DSSREC,STATUS)
	IF(STATUS.EQ.144) THEN
	  TYPE*,'Last draw initialized - ',DRAW-1
	  CALL CLOSEFIL(FDB)
	  CALL XWAIT(2,2,STATUS)
	  GOTO 1000
	ENDIF
	IF(STATUS.NE.0) CALL FILERR(FILE,2,STATUS,DRAW)
	CALL CLOSEFIL(FDB)
C
C assign local variables
C
	CDSSTIM = DSSTIM
	CDSSPRC = DSSPRC
	CDSSSPR = DSSSPR
C
C display the record and prompt user for changes
C
10	CALL CLRSCR(5)
	WRITE(5,700) GNAME
	WRITE(5,710) DISTIM(CDSSTIM)
	WRITE(5,720) CMONY(CDSSPRC,10,BETUNIT)
	WRITE(5,730) DISPER(CDSSSPR)

	WRITE(5,880)

	WRITE(5,885)
	WRITE(5,887)
	WRITE(5,890)
	CALL INPNUM(' Enter option ',OPTION,1,5,EXIT)
	IF(EXIT.LT.0) GOTO 1000
	ALL = .FALSE.
	IF(OPTION.EQ.4) ALL = .TRUE.
	
	IF(OPTION.EQ.1.OR.ALL) THEN
	    CALL INPTIM('Enter pool close time HH:MM:SS ',TEMP,EXIT)
	    IF(EXIT.LT.0) GOTO 10
	    CDSSTIM = TEMP
	ENDIF

	IF(OPTION.EQ.2.OR.ALL) THEN
	    CALL INPMONY('Enter base bet price ',TEMP,BETUNIT,EXIT)
	    IF(EXIT.LT.0) GOTO 10
	    CDSSPRC = TEMP
	ENDIF

	IF(OPTION.EQ.3.OR.ALL) THEN
	    CALL INPPER('Enter pool percentage',TEMP,EXIT)
	    IF(EXIT.LT.0) GOTO 10
	    CDSSSPR = TEMP
	ENDIF
	
	IF(OPTION.EQ.5) THEN
	    GOTO 500
	ENDIF
	GOTO 10	
	
C
C update the game file starting at draw DRAW
C
500	CONTINUE
	CALL INPNUM(
     *	 'Enter starting draw number to update (will update to eof)',
     *	 DRAW,1,10000,EXIT)
	IF(EXIT.LT.0) GOTO 1000
C
C update the file by looping through all the draws
C
	WRITE(5,900) FILE
	CALL OPENW(2,FILE,4,0,0,STATUS)
	CALL IOINIT(FDB,2,DSSSEC*256)
	IF(STATUS.NE.0) CALL FILERR(FILE,1,STATUS,0)
C
C write loop
C
510	CONTINUE
	CALL READW(FDB,DRAW,DSSREC,STATUS)
	IF(STATUS.EQ.144) THEN
	  TYPE*,'Last draw initialized - ',DRAW-1
	  CALL CLOSEFIL(FDB)
	  CALL XWAIT(2,2,STATUS)
	  RETURN
	ENDIF
	IF(STATUS.NE.0) CALL FILERR(FILE,2,STATUS,DRAW)

	IF(DSSSTS.GT.GAMOPN) THEN
	  TYPE*,'Game already closed for draw ',DRAW
	  CALL GPAUSE
	  CALL CLOSEFIL(FDB)
	  GOTO 500
	ENDIF

	DSSTIM = CDSSTIM
	DSSPRC = CDSSPRC
	DSSSPR = CDSSSPR

	CALL WRITEW(FDB,DRAW,DSSREC,STATUS)
	IF(STATUS.EQ.144) THEN
	  TYPE*,'Last draw initialized ',DRAW
	  CALL XWAIT(2,2,STATUS)
	  GOTO 10
	ENDIF
	IF(STATUS.NE.0) CALL FILERR(FILE,3,STATUS,DRAW)
	DRAW=DRAW+1
	GOTO 510

1000	CONTINUE
	CALL CLRSCR(5)
	RETURN
C
C format stuff
C
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
