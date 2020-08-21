C
C SUBROUTINE BLDWIN
C $Log:   GXAFXT:[GOLS]BLDWIN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:20:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   09 Jan 1994 22:23:14   HXK
C  PUT IN MDS STUFF.
C  
C     Rev 1.0   21 Jan 1993 15:44:52   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - bldwin.for **
C
C BLDWIN.FOR
C
C 
C V01 06-APR-92 HdB created based on BLDNBR.FOR
C
C
C SUBROUTINE TO DEFINE WINNERSTIP GAME PARAMETERS.
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

C ******************************************************************
C	discr	: allow user to modify permanent settings for the
C		  winners tip game with ability to 'undo'
C
C	input   : FILE   file name of winners tip game file
C		: GNAME	 long game name
C	output	: NONE   all error handling internal
C	
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE BLDWIN(FILE,GNAME)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DWIREC.DEF'
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
        INTEGER*4  I                  ! counter
        INTEGER*4  ST                 ! status return
        INTEGER*4  EXT                ! status return
        INTEGER*4  FLAG               ! emblem of a country
        INTEGER*4  I4_MDS             ! multi-draw value
        INTEGER*4  PREVIOUS_I4_MDS    ! previous multi-draw value
        INTEGER*4  TOTAL_MDS_CHOSEN   ! total multi-draws chosen
C
	INTEGER*4 CDWITIM	!closing time
	INTEGER*4 CDWIPRC	!base price
	INTEGER*4 CDWISPR	!pool percentage
C
        BYTE  BYTE_MDS(MAXMLTD_AVL)   !
C

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
	CALL IOINIT(FDB,2,DWISEC*256)
	IF(STATUS.NE.0) CALL FILERR(FILE,1,STATUS,0)  
	CALL READW(FDB,DRAW,DWIREC,STATUS)
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
	CDWITIM = DWITIM
	CDWIPRC = DWIPRC
	CDWISPR = DWISPR
C
C display the record and prompt user for changes
C
10	CALL CLRSCR(5)
	WRITE(5,700) GNAME
	WRITE(5,710) DISTIM(CDWITIM)
	WRITE(5,720) CMONY(CDWIPRC,10,BETUNIT)
	WRITE(5,730) DISPER(CDWISPR)

        DO I = 2, MAXMLTD_AVL
            IF(DWIMDS(I).NE.0) THEN
                WRITE(5,9190) I
            ENDIF
        ENDDO
 

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
	    CDWITIM = TEMP
	ENDIF

	IF(OPTION.EQ.2.OR.ALL) THEN
	    CALL INPMONY('Enter base bet price ',TEMP,BETUNIT,EXIT)
	    IF(EXIT.LT.0) GOTO 10
	    CDWIPRC = TEMP

15          CONTINUE
            PREVIOUS_I4_MDS = 0
            DO I = 1, MAXMLTD_AVL
                BYTE_MDS(I)=0
            END DO
            BYTE_MDS(1)=1          !single week wagering always allowed

            CALL WIMG(5,'Is there multi-draw wagering [Y/N] ')
            CALL YESNO(FLAG)
            IF (FLAG.EQ.1) THEN

                CALL WIMG(5,'Enter multi-draws allowed ')
                CALL WIMG(5,'(e.g. Lotto =2,3,5 and 10)')
                I = 2
20              CONTINUE

                CALL INPNUM('Enter multi-draw allowed [E=finished] ',
     *                       I4_MDS,2,MAXMLTD_AVL,EXT)
                IF(EXT.EQ.0) THEN
                    IF (I.GT.MAXMLTD_SEL-1) THEN   !prevent more than max allow
                        TYPE*,'Too many multi-draws selected.'
                        TYPE*,'total cannot exceed ',MAXMLTD_SEL-1  !1 drw alre
                        CALL XWAIT(2,2,ST)
                        GOTO 15
                    ENDIF

                    IF(BYTE_MDS(I4_MDS).NE.0) THEN  !prevent writing over values
                        TYPE*,'Draw multiple already chosen '
                        TYPE*,'Re-enter data '
                        CALL XWAIT(2,2,ST)
                        GOTO 15
                    ENDIF

                    IF(I4_MDS.LE.PREVIOUS_I4_MDS) THEN
                        TYPE*,'Draw multiples MUST be entered in '
                        TYPE*,'ascending order'
                        CALL XWAIT(2,2,ST)
                        GOTO 15
                    ENDIF

                    BYTE_MDS(I4_MDS)=I
                    PREVIOUS_I4_MDS=I4_MDS
                    I = I + 1
                    GOTO 20
                ENDIF

                IF(EXT.EQ.-1) THEN
                    TOTAL_MDS_CHOSEN=I-1
                ELSE
                    TYPE*,'Incorrect data entered '
                    GOTO 20
                ENDIF

            ENDIF

	ENDIF

	IF(OPTION.EQ.3.OR.ALL) THEN
	    CALL INPPER('Enter pool percentage',TEMP,EXIT)
	    IF(EXIT.LT.0) GOTO 10
	    CDWISPR = TEMP
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
	CALL IOINIT(FDB,2,DWISEC*256)
	IF(STATUS.NE.0) CALL FILERR(FILE,1,STATUS,0)
C
C write loop
C
510	CONTINUE
	CALL READW(FDB,DRAW,DWIREC,STATUS)
	IF(STATUS.EQ.144) THEN
	  TYPE*,'Last draw initialized - ',DRAW-1
	  CALL CLOSEFIL(FDB)
	  CALL XWAIT(2,2,STATUS)
	  RETURN
	ENDIF
	IF(STATUS.NE.0) CALL FILERR(FILE,2,STATUS,DRAW)

	IF(DWISTS.GT.GAMOPN) THEN
	  TYPE*,'Game already closed for draw ',DRAW
	  CALL GPAUSE
	  CALL CLOSEFIL(FDB)
	  GOTO 500
	ENDIF

	DWITIM = CDWITIM
	DWIPRC = CDWIPRC
	DWISPR = CDWISPR

        DO I = 1, MAXMLTD_AVL
            DWIMDS(I) = BYTE_MDS(I)
        END DO

	CALL WRITEW(FDB,DRAW,DWIREC,STATUS)
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
720	FORMAT('   2  - base price.............:',A10,' ...multidraw options')
730	FORMAT('   3  - pool percentage........:',F10.2)
880	FORMAT('   4  - change all')
885	FORMAT('   - -')
887	FORMAT('   5  - update draws')
890	FORMAT('   E  - exit')
900	FORMAT(' Updating ',5A4,' with game parameters')
9190    FORMAT(1X,' multidraw:      -> ',I2.2,' drws ')
	END
