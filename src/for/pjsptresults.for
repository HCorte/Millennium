C PJSPTRESULTS.FOR
C
C V02 30-MAR-2015 MTK Modified Super 14 game
C V01 02-FEB-2004 CPH
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2004 DJ - SCML. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C SUBROUTINE GENERATE SPORTS RESULTS FILE
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE PJSPTRESULTS(FILE,DRAW,GNUM,GIND)	
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:DSPREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4 ST 
        INTEGER*4 YEAR,WEEK,BCNT     
	INTEGER*4 DRAW          !
	INTEGER*4 GNUM          !
        INTEGER*4 GIND          !
	INTEGER*4 LINCNT	!
        INTEGER*2 DATE(LDATE_LEN)
	INTEGER*4 I,K,J
        INTEGER*4 FILE(5)
	INTEGER*4 REPLU/7/,LUN/9/	
        INTEGER*4 SCORE(2)
	CHARACTER*2  REGTYPE
        CHARACTER*20 REPNAM
        CHARACTER YXRISTI2(4)
        DATA YXRISTI2 /'1','X','C','2'/
        CHARACTER RCHVAL(4)
        DATA RCHVAL/'0','1','C','M'/
C

C
C BEGIN CODE
C GET DATA FROM COMMON OR DISK
        IF(DRAW.EQ.DAYDRW(GNUM)) THEN
            CALL GAMLOG(TSPT,GIND,DSPREC,SPTSTS)
            GOTO 100
        ENDIF

C READ GAME FILE
Cthis subroutine open, read and close the file
        CALL READGFL(LUN,FILE,DSPSEC,DRAW,DSPREC)

100     CONTINUE

C CHECK IF DRAW HAVE WINNING NUMBERS VERIFIED
C********************************************
	IF (DSPSTS.LT.GAMENV) THEN
	    TYPE *,IAM(),'Sorry, Winning Numbers not set for draw: ',DRAW   
	    GOTO 1000
	ENDIF

C GET WEEK / YEAR INFORMATION
C************************************
      CALL GETWEK(DRAW, GNUM, WEEK, YEAR, ST)
      IF(ST .NE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Error Getting Week / Year Draw Number'
        TYPE *, IAM()
        CALL GPAUSE
      ENDIF

C WRITE FILE NAME AND OPEN FILE
C*********************************
        WRITE (REPNAM,810) GNUM, WEEK, YEAR
         CALL ROPEN(REPNAM,REPLU,ST)
         IF(ST.NE.0) THEN
            TYPE*,IAM(),REPNAM,' File open error > ',ST
            CALL USRCLOS1(REPLU)
            GOTO 1000
         ENDIF
C	
	TYPE *,IAM(),'Generating PJMC_NS file for SPORTS: ',GIND

	LINCNT=0
	REGTYPE ='01'
	       
	DATE(VCDC)=DAYCDC
        CALL LCDATE(DATE)   

C WRITE HEADER  
C*****************
          WRITE(REPLU, 9001)
     *          DATE(VYEAR2),DATE(VMON),DATE(VDAY)
               
          LINCNT=LINCNT+1
	
C WRITE WINNING NUMBERS 
C**************************

	   BCNT = 0 
	   IF(DSPFRG.NE.0) BCNT = 1

	   LINCNT=LINCNT+1
	   IF(DSPFRG.EQ.1) THEN
      	    SCORE(1)=ISHFT(DSPWIN(DSPMAX),-4)
      	    SCORE(2)=IAND(DSPWIN(DSPMAX),'0F'X)
	    WRITE(REPLU, 8010)
     *		 REGTYPE,	   
     *		(YXRISTI2(DSPWIN(I)),I=1,DSPMAX-BCNT),
     *		(RCHVAL(SCORE(K)),K=1,2)
	   ENDIF

           IF(DSPFRG.EQ.2) THEN
            WRITE(REPLU, 8011)
     *           REGTYPE,
     *          (YXRISTI2(DSPWIN(I)),I=1,DSPMAX-BCNT),
     *          YXRISTI2(DSPWIN(DSPMAX))
           ENDIF
	   


C WRITE TRAILER 
C***************        
	   LINCNT=LINCNT+1
        
           WRITE(REPLU, 9002)LINCNT

C CLOSE FILE
	TYPE *,IAM(),'PJMC_NS file for SPORTS: ',GIND,' COMPLETE'

        CALL USRCLOS1(REPLU)

1000    CONTINUE

	RETURN

C Format Statements
C ********************************************
8010	FORMAT(A2,<DSPMAX-BCNT>(A2),2(A1))
8011	FORMAT(A2,<DSPMAX>(A2))
9001    FORMAT('HP',I4.4,I2.2,I2.2,20(' '))
9002    FORMAT('TP',I8.8,20(' '))
810     FORMAT('PJMC_NS_',I2.2,I2.2,I4.4'.ASC')

	END
