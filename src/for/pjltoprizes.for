C PJLTOPRIZES.FOR
C
C V02 05-JAN-2011 FJG Lotto2 Batch: New identification
C     24-JAN-2011 FJG Out of Bounds issues
C     09-FEB-2011 FJG Draw status on disk
C V01 01-AUG-2003 CPH
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2003 DJ - SCML. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C SUBROUTINE GENERATE PRIZES FILE FOR LOTTO
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE PJLTOPRIZES(FILE,DRAW,GNUM,GIND)	
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:DLTREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*4 REPLU/7/
        INTEGER*4 ST 
        INTEGER*4 YEAR,WEEK
        INTEGER*4 FILE(5)       !
	INTEGER*4 DRAW          !
	INTEGER*4 GNUM          !
        INTEGER*4 GIND          !
        INTEGER*4 DIV           !
	INTEGER*4 LINCNT	!
        INTEGER*2 DATE(LDATE_LEN)
	INTEGER*4 LUN/9/
	
	CHARACTER*2  REGTYPE
        CHARACTER*21 REPNAM

C BEGIN CODE
C GET DATA FROM COMMON OR DISK
C******************************
       IF(DRAW.EQ.DAYDRW(GNUM)) THEN
            CALL GAMLOG(TLTO,GIND,DLTREC,LTOSTS)
            IF (DLTSTS.GE.GFINAL) GOTO 200
        ENDIF

C READ GAME FILE
Cthis subroutine open, read and close the file
C************************************************
        CALL READGFL(LUN,FILE,DLTSEC,DRAW,DLTREC)
C CHECK IF DRAW HAVE PRIZES NUMBERS
C**************************************
	IF (DLTSTS.LT.GFINAL) THEN
	    TYPE *,IAM(),'Sorry, Prizes not set for draw: ',DRAW   
	    GOTO 1000
	ENDIF

C GET WEEK / YEAR INFORMATION
C*******************************
200   CONTINUE
      CALL GETWEK(DRAW, GNUM, WEEK, YEAR, ST)
      IF(ST .NE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Error Getting Week / Year Draw Number'
        TYPE *, IAM()
        CALL GPAUSE
      ENDIF

C WRITE FILE NAME AND OPEN FILE
C********************************
        WRITE (REPNAM,810) GNUM, WEEK, YEAR
         CALL ROPEN(REPNAM,REPLU,ST)
         IF(ST.NE.0) THEN
            TYPE*,IAM(),REPNAM,' File open error > ',ST
            CALL USRCLOS1(REPLU)
            GOTO 1000
         ENDIF
C	
	TYPE *,IAM(),'Generating PJMC_PM file for LOTTO: ',GIND

	LINCNT=0
	REGTYPE ='01'
        
	DATE(VCDC)=DAYCDC
        CALL LCDATE(DATE)   

C WRITE HEADER  
C**************
          WRITE(REPLU, 9001)
     *          DATE(VYEAR2),DATE(VMON),DATE(VDAY)
               
          LINCNT=LINCNT+1
	
C WRITE DIVISIONS, SHARES AND SHARES VALUES
C*******************************************
        DO DIV=1,DLTDIV
	
	   WRITE(REPLU, 8010)
     *		 REGTYPE,
     *           DIV,	   
     *		 DLTSHV(DIV,1),
     *		 DLTSHR(DIV,1)

	LINCNT=LINCNT+1

	ENDDO

C WRITE TRAILER 
C****************        
	   LINCNT=LINCNT+1
        
           WRITE(REPLU, 9002)LINCNT

C CLOSE FILE
C*************
	TYPE *,IAM(),'PJMC_PM file for LOTTO: ',GIND,' COMPLETE'

        CALL USRCLOS1(REPLU)

1000    CONTINUE

	RETURN

C     ===================== Format Statements =================
C
8010	FORMAT(A2,I2.2,I12.12,I10.10)
9001    FORMAT('HP',I4.4,I2.2,I2.2,16(' '))
9002    FORMAT('TP',I8.8,16(' '))
!810    FORMAT('PJMC_PM_',I2.2,I2.2,I4.4'.ASC')
810     FORMAT('PJMC_PM_',I2.2,I3.3,I4.4'.ASC')    ! V02

	END
