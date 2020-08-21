C PJLTORESULTS.FOR
C
C V02 05-JAN-2011 FJG Lotto2 Batch: New identification
C     24-JAN-2011 FJG Out of Bounds issues
C     09-FEB-2011 FJG Solving file layout
C V01 01-AUG-2003 CPH
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2003 DJ - SCML. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C SUBROUTINE GENERATE LOTTO RESULTS FILE
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE PJLTORESULTS(FILE,DRAW,GNUM,GIND)	
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
        INTEGER*4 ST 
        INTEGER*4 YEAR,WEEK     
        INTEGER*4 CCCWEK        ! V02 Function
        INTEGER*4 OWEK          ! V02
	INTEGER*4 DRAW          !
	INTEGER*4 GNUM          !
        INTEGER*4 GIND          !
	INTEGER*4 LINCNT	!
        INTEGER*2 DATE(LDATE_LEN)
	INTEGER*4 I
        INTEGER*4 FILE(5)
	INTEGER*4 REPLU/7/,LUN/9/
	CHARACTER*2  CHRNUM(7)/7*'  '/
	CHARACTER*2  REGTYPE
        CHARACTER*27 REPNAM
C	CHARACTER*21 RECREP
C
C BEGIN CODE
C GET DATA FROM COMMON OR DISK
        IF(DRAW.EQ.DAYDRW(GNUM)) THEN
            CALL GAMLOG(TLTO,GIND,DLTREC,LTOSTS)
            IF (DLTSTS.GE.GAMENV) GOTO 200
        ENDIF

C READ GAME FILE
Cthis subroutine open, read and close the file
        CALL READGFL(LUN,FILE,DLTSEC,DRAW,DLTREC)
C CHECK IF DRAW HAVE WINNING NUMBERS VERIFIED
C********************************************
	IF (DLTSTS.LT.GAMENV) THEN
	    TYPE *,IAM(),'Sorry, Winning Numbers not set for draw: ',DRAW   
	    GOTO 1000
	ENDIF

C GET WEEK / YEAR INFORMATION
C************************************
200   CONTINUE
      CALL GETWEK(DRAW, GNUM, WEEK, YEAR, ST)
      IF(ST .NE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Error Getting Week / Year Draw Number'
        TYPE *, IAM()
        CALL GPAUSE
      ENDIF
C
      IF(GIND.EQ.3.OR.GIND.EQ.4) THEN    ! V02         
        OWEK = CCCWEK(YEAR,WEEK,GNUM)    ! V02         
      ELSE                               ! V02 
        OWEK  = WEEK                     ! V02 
      ENDIF                              ! V02 
C WRITE FILE NAME AND OPEN FILE
C*********************************
        WRITE (REPNAM,810) GNUM, WEEK, YEAR, OWEK, YEAR   ! V02
         CALL ROPEN(REPNAM,REPLU,ST)
         IF(ST.NE.0) THEN
            TYPE*,IAM(),REPNAM,' File open error > ',ST
            CALL USRCLOS1(REPLU)
            GOTO 1000
         ENDIF
C	
	TYPE *,IAM(),'Generating PJMC_NS file for LOTTO: ',GIND

	LINCNT=0
	REGTYPE ='01'
        
	DATE(VCDC)=DAYCDC
        CALL LCDATE(DATE)   



C WRITE HEADER  
C*****************
          WRITE(REPLU, 9001)
     *          DATE(VYEAR2),DATE(VMON),DATE(VDAY)
               
          LINCNT=LINCNT+1
	
C WRITE WINNING NUMBERS AND WINNING BONUS NUMBER(S)
C**************************************************
	   LINCNT=LINCNT+1
	   DO I = 1, DLTNUM
	     WRITE(CHRNUM(I),'(I2.2)') DLTWIN(I,1)
	   ENDDO
	   IF(DLTBNM(1,1).GT.0) WRITE(CHRNUM(7),'(I2.2)') DLTBNM(1,1)
	   WRITE(REPLU, 8010)
     *		 REGTYPE,	   
     *		(CHRNUM(I),I=1,7),
     *          DLTLNM(1)


C WRITE TRAILER 
C***************        
	   LINCNT=LINCNT+1
        
           WRITE(REPLU, 9002)LINCNT

C CLOSE FILE
	TYPE *,IAM(),'PJMC_NS file for LOTTO: ',GIND,' COMPLETE'

        CALL USRCLOS1(REPLU)

1000    CONTINUE

	RETURN

C Format Statements
C ********************************************
8010	FORMAT(A2,7(A2),7(' '),I2.2)
9001    FORMAT('HP',I4.4,I2.2,I2.2,15(' '))
9002    FORMAT('TP',I8.8,15(' '))
!810    FORMAT('PJMC_NS_',I2.2,I2.2,I4.4'.ASC')
810     FORMAT('PJMC_NS_',I2.2,I3.3,I4.4,I2.2,I4.4,'.ASC')    ! V02
	END
