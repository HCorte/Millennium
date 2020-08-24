C PJKIKRESULTS.FOR
C
C V02 05-JAN-2011 FJG Lotto2 Batch: New identification
C     24-JAN-2011 FJG Out of bounds issue
C     23-FEB-2011 FJG Solving file layout
C V01 01-AUG-2003 CPH
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2003 DJ - SCML. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C SUBROUTINE GENERATE JOKER RESULTS FILE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C====== OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE PJKIKRESULTS(FILE,DRAW,GNUM,GIND)	
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:DKKREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4 ST 
        INTEGER*4 YEAR,WEEK    
	INTEGER*4 DRAW          !
	INTEGER*4 GNUM,GIND     !
	INTEGER*4 LINCNT	!
        INTEGER*2 DATE(LDATE_LEN)
        INTEGER*4 REPLU/7/,LUN/9/
	INTEGER*4 FILE(5)	
	CHARACTER*2 REGTYPE
        CHARACTER*27 REPNAM
C
C BEGIN CODE
C GET DATA FROM COMMON OR DISK
C ****************************
        IF(DRAW.EQ.DAYDRW(GNUM)) THEN
            CALL GAMLOG(TKIK,GIND,DKKREC,KIKSTS)
            GOTO 100
        ENDIF
C
C READ GAME FILE
Cthis subroutine open, read and close the file
        CALL READGFL(LUN,FILE,DKKSEC,DRAW,DKKREC)

100     CONTINUE

C CHECK IF DRAW HAVE WINNING NUMBERS VERIFIED
C***********************************************
        IF (DKKSTS.LT.GAMENV) THEN
            TYPE *,IAM(),'Sorry, Winning Numbers not set for draw: ',DRAW   
            GOTO 1000
        ENDIF

C GET WEEK / YEAR INFORMATION
C****************************
      CALL GETWEK(DRAW, GNUM, WEEK, YEAR, ST)
      IF(ST .NE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Error Getting Week / Year Draw Number'
        TYPE *, IAM()
        CALL GPAUSE
      ENDIF

C WRITE FILE NAME AND OPEN FILE
C ******************************
        WRITE (REPNAM,810) GNUM, WEEK, YEAR, WEEK, YEAR ! V02 Duplicated for New TOTOLOTO CCC
         CALL ROPEN(REPNAM,REPLU,ST)
         IF(ST.NE.0) THEN
            TYPE*,IAM(),REPNAM,' File open error > ',ST
            CALL USRCLOS1(REPLU)
            GOTO 1000
         ENDIF
C      
        TYPE *,IAM(),'Generating PJMC_NS file for JOKER'

        LINCNT=0
        REGTYPE ='01'

        DATE(VCDC)=DAYCDC
        CALL LCDATE(DATE)   

C WRITE HEADER  
C*************
          WRITE(REPLU, 9001)
     *          DATE(VYEAR2),DATE(VMON),DATE(VDAY)
               
          LINCNT=LINCNT+1
C
C WRITE JOKER WINNING NUMBERS
C*****************************	
	   WRITE(REPLU, 8010)
     *		 REGTYPE,	   
     *		 DKKWIN

	LINCNT=LINCNT+1

C WRITE TRAILER 
C *************        
           LINCNT=LINCNT+1
        
           WRITE(REPLU, 9002)LINCNT

C CLOSE FILE
C **********
        TYPE *,IAM(),'PJMC_NS file for JOKER COMPLETE'

        CALL USRCLOS1(REPLU)

1000    CONTINUE
	RETURN

C Format Statements
C ******************
8010	FORMAT(A2,14X,I7.7,'  ')
9001    FORMAT('HP',I4.4,I2.2,I2.2,15(' '))
9002    FORMAT('TP',I8.8,15(' '))
!810    FORMAT('PJMC_NS_',I2.2,I2.2,I4.4'.ASC')
810     FORMAT('PJMC_NS_',I2.2,I3.3,I4.4,I2.2,I4.4,'.ASC')   ! V02
C
	END
