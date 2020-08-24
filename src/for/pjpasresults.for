C PJPASRESULTS.FOR
C
C V01 01-AUG-2003 CPH
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2003 SCML-DJ. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C SUBROUTINE GENERATE RESULT FILE FOR PASSIVE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE PJPASRESULTS(FILE,DRAW,GNUM,GIND)	
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
	INCLUDE 'INCLIB:DPAREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4 ST 
        INTEGER*4 YEAR,WEEK     
	INTEGER*4 DRAW          !
	INTEGER*4 GNUM          !
        INTEGER*4 GIND          !
        INTEGER*4 LINCNT        !
        INTEGER*2 DATE(LDATE_LEN)
        INTEGER*4 REPLU/7/,LUN/9/,FILE(5)	
	INTEGER*4 DIVS
	INTEGER*4 K
        INTEGER*4 EMIOFF,INDEMI
	LOGICAL   ON_MEMORY
	CHARACTER*2 REGTYPE
        CHARACTER*20 REPNAM
C
C BEGIN CODE -----------------------------------------

C VERIFY EMISSION NUMBER
C***********************
        IF(DRAW.LE.PAS_DRW_OFFSET) DRAW = 1 !DAYDRW(GNUM)
C
C GET DATA FROM COMMON  OR DISK
C******************************
        ON_MEMORY = .FALSE.
        DO INDEMI = 1, PAGEMI
            IF(DRAW.EQ.PASEMIS(INDEMI,GIND)) THEN
                ON_MEMORY = .TRUE.
                EMIOFF = INDEMI
            ENDIF
        ENDDO

        IF (ON_MEMORY) THEN
          CALL GAMLOGPAS(EMIOFF,GIND,DPAREC,PASSTS)
        ELSE

C READ GAME FILE
Cthis subroutine open, read and close the file
        CALL READGFL(LUN,FILE,DPASEC,DRAW,DPAREC)

	ENDIF

C CHECK IF DRAW HAVE WINNING NUMBERS VERIFIED
C *******************************************

        IF (DPASTS.LT.GAMENV) THEN
            TYPE *,IAM(),'Sorry, Winning Numbers not set for draw: ',DRAW   
            GOTO 1000
        ENDIF

C GET WEEK / YEAR INFORMATION
C ***************************
      CALL GETWEK(DRAW, GNUM, WEEK, YEAR, ST)
      IF(ST .NE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Error Getting Week / Year Draw Number'
        TYPE *, IAM()
        CALL GPAUSE
      ENDIF

C WRITE FILE NAME AND OPEN FILE
C *****************************
        WRITE (REPNAM,810) GNUM, WEEK, YEAR
         CALL ROPEN(REPNAM,REPLU,ST)
         IF(ST.NE.0) THEN
            TYPE*,IAM(),REPNAM,' File open error > ',ST
            CALL USRCLOS1(REPLU)
            GOTO 1000
         ENDIF
C
        TYPE *,IAM(),'Generating PJMC_NS file for PASSIVE: ',GIND

        LINCNT=0
        REGTYPE ='01'
       
        DATE(VCDC)=DAYCDC
        CALL LCDATE(DATE)   

C WRITE HEADER  

           WRITE(REPLU, 9001)
     *          DATE(VYEAR2),DATE(VMON),DATE(VDAY)

        LINCNT=LINCNT+1 

C WRITE PASSIVE WINNING serie
        IF (DPAWSER .NE. 0) THEN
           WRITE(REPLU,8050)
     *		 REGTYPE,
     *           '00',
     *           DPAWSER
                   
           LINCNT=LINCNT+1
        ENDIF  

C WRITE PASSIVE WINNING NUMBERS

	DO DIVS=1,DPADIV
	   IF(DPAWNUM(DIVS).NE.0) THEN
		
	   WRITE(REPLU, 8010)
     *		 REGTYPE,
     *		 DIVS,
     *		 (DPAWIN(K,DIVS),K=1,PAGNBR)
		   
	   LINCNT=LINCNT+1

	   ENDIF
	ENDDO

C WRITE TRAILER 
C *************
           LINCNT=LINCNT+1
        
           WRITE(REPLU, 9002)LINCNT

C CLOSE FILE
C***********
        TYPE *,IAM(),'PJMC_NS file for PASSIVE: ',GIND,' COMPLETE'

        CALL USRCLOS1(REPLU)

1000    CONTINUE

        RETURN

C
C     ===================== Format Statements =================
C
8010	FORMAT(A2,I2.2,<PAGNBR>(I5.5))
8050	FORMAT(A2,A2,I5.5,95('0'))
9001    FORMAT('HP',I4.4,I2.2,I2.2,94(' '))
9002    FORMAT('TP',I8.8,94(' '))
810     FORMAT('PJMC_NS_',I2.2,I2.2,I4.4'.ASC')

	END
