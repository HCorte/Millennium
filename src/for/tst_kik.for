C GXSRC:TST_KIK.FOR
C  
C V01 31-MAR-2017 HXK JOKER DEACTIVATION - ROLLOVER TO ROLLDOWN TEST SET UP
C
C JOKER (KICKER) SET UP TEST DATA FOR ROLLDOWN TEST                                                         
C                                                                               
C                                                                               
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C This item is the property of GTECH Corporation, W.Greenwich, Rhode            
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
C  
C=======OPTIONS /CHECK=NOOVERFLOW/EXT                              
        SUBROUTINE TST_KIK(FILE,DRAW)                        
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESPAR.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'

        ! arguments
        INTEGER*4  GNUM            ! game number
        INTEGER*4  GIND            ! game index
        INTEGER*4  DRAW            ! draw number

        ! variables
        INTEGER*4  FDB(7)          ! file description block
        INTEGER*4  GROSS(KIGDIV)   ! gross share values (in pennies)  
        INTEGER*4  ST              ! status
        INTEGER*4  EXT             ! error flag
        INTEGER*4  YESNO           ! yes/no flag
        INTEGER*4  I               ! counter variable
        INTEGER*4  J               ! counter variable
        INTEGER*4  TOTSAL          ! total sales
        REAL*8     ROLLOVER(KIGDIV)
        INTEGER*4  TOT2CNT

        INTEGER*4  JACKPOT
        INTEGER*4  NUMWIN
        INTEGER*4  FILE(5)
        
        REAL*8     DPOOL(KIGDIV)   ! prize pools
        REAL*8     TOTPOL,TOT2POL,PRIZE
	
	REAL*8     DKKPOL_R(KIGDIV),X

        LOGICAL    ROLL

        COMMON SCFREC
        ! start of code
C                                                           
	GNUM=5
	GIND=1
C                                                                               
	DRWSTS(MLWININD,GNUM)=WINSOK
C
        ! open game file
        CALL OPENW(3,FILE,4,0,0,ST)                      
        CALL IOINIT(FDB,3,DKKSEC*256)                                  
        IF(ST.NE.0) CALL FILERR(FILE,1,ST,0)             

        ! read record for selected draw
        CALL READW(FDB,DRAW,DKKREC,ST)                             
        IF(ST.NE.0) CALL FILERR(FILE,2,ST,DRAW)          
C                                                                               
C       
        ! revert game status                                        
        WRITE(6,900) IAM(),GTNAMES(TKIK),GIND,DRAW,DKKSTS            
        TYPE*,IAM(),'CHANGING JOKER GAME STATUS TO: WINNING NUMBERS VERIFIED'                                             
        DKKSTS = GAMENV                                                      
        WRITE(6,900) IAM(),GTNAMES(TKIK),GIND,DRAW,DKKSTS            
C                                                                               
C                                                                               
5       CONTINUE                                                   

        ! initialise variables to zero
        DKKTAX    = 0                                                   
        DKKRES(1) = 0                                              
        DKKRES(2) = 0                                              
        DO I = 1, DKKDIV                                           
            IF (DKKPER(I).NE.0) THEN
               DKKSHV(I) = 0
            ELSE
               IF(I.EQ.2) DKKSHV(I)=5000000
               IF(I.EQ.3) DKKSHV(I)=500000
               IF(I.EQ.4) DKKSHV(I)=50000
               IF(I.EQ.5) DKKSHV(I)=5000
               IF(I.EQ.6) DKKSHV(I)=500
            ENDIF
            DKKPOL(1,I) = 0 
            DKKPOL(2,I) = 0 
        END DO

35	CONTINUE
	JACKPOT = 0
	CALL PRMYESNO('Do you want to add money to Jackpot pool?',
     *		       YESNO)
	IF(YESNO .EQ. 1) THEN
	    CALL PRMMONY('Enter amount ',JACKPOT,1,EXT)	    ! Amount in pennies
	    IF(EXT.LT.0) GOTO 35
	    IF(JACKPOT.LE.0) GOTO 35
            DKKPOL(1,1) = JACKPOT
	ENDIF

	CALL PRMYESNO('Do you want to change number of winners per division?',
     *		       YESNO)
	IF(YESNO .EQ. 1) THEN

	    DO I=1,DKKDIV
40              CONTINUE  
	        TYPE*,IAM(),'Div:',I
	        TYPE*,IAM(),'Current Number of Winners:',DKKSHR(I)
	        CALL PRMNUM('Enter  New Number of Winners:',NUMWIN,0,99999999,EXT)
	        IF(EXT.LT.0) GOTO 40
	        IF(NUMWIN.LT.0) GOTO 40
                DKKSHR(I) = NUMWIN
            ENDDO
	ENDIF
                     
        CALL WRITEW(FDB,DRAW,DKKREC,ST)                                 
        IF(ST.NE.0) CALL FILERR(FILE,3,ST,DRAW)               

        WRITE(6,907) IAM(),CMONY(JACKPOT,13,BETUNIT)
	WRITE(6,905) IAM(),DKKSHR(1)               
        DO  I = 1, DKKDIV
           WRITE(6,906) IAM(),I,DKKSHR(I)                                              
	ENDDO

        CALL CLOSEFIL(FDB)

        RETURN
100	FORMAT(1X,'Money added to reach minimum value 1st DIV : ', A11)
124	FORMAT(1X,'Money added to reach minimum value 1st DIV for more than one winner : ', A11)
150	FORMAT(1X,A,'TOTAL MONEY TRANSFERED TO DIVISION 1 : ',F12.2,' Euros') 
900     FORMAT(1X,A,1X,A8,I1,' event ',I4,' game status> ',I4)       
901     FORMAT(1X,A,1X,A8,I1,' draw ',I4)                                    
902     FORMAT(1X,A,1X,'Total div 1 prize pool: ',I9,' Euros')
903 	FORMAT(A,F12.2,' rolled over into the next draw')
904     FORMAT(1X,A,'Div ',I2,' prize ', A10,' is less than div ',I2,
     *           ' prize ', A10)
905     FORMAT(1X,A,'Number of Jackpot winners:',I9)
906     FORMAT(1X,A,'Number of Div',I4,' winners:',I9)
907     FORMAT(1X,A,'Jackpot ',A13)
      END                                                                       
