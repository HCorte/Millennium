C  GXSRC:KIKCALC.FOR
C  
C V11 27-MAR-2017 HXK JOKER DEACTIVATION - CHANGE ROLLOVER TO ROLLDOWN
C V10 07-MAI-2002 MENT FIX ROLLOVER 
C V09 06-DEC-2000 EPG/ANG DEAL WITH MIXED FIXED/SHARED DIVISIONS 
C                         TAKE ALL FIXED DIVISION PRIZES FROM FIRST DIVISION SHARED VALUE
C V08 09-MAY-2000 OXK Added question about adding money for promotion draw
C V07 25-APR-2000 UXN DKKPOL changed from I4 to I8
C V06 24-MAR-2000 UXN UINT added.
C V05 14-OCT-1999 UXN Fix for rollover money.
C V04 28-APR-1999 RXK Stopsys optimization (some IAM()s added).
C V03 05-AUG-1998 RXK Changed for new kicker
C V02 05-FEB-1996 RXK Rfss 242. For share values is now used JIDINT 
C                     (instead of idnint) 
C V01 28-NOV-1994 JXP Initial revision.
C  
C
C JOKER (KICKER) SHARECLC                                                         
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
        SUBROUTINE KIKCALC(GNUM, GIND, DRAW)                        
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESPAR.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'

        ! arguments
        INTEGER*4  GNUM            ! game number
        INTEGER*4  GIND            ! game index
        INTEGER*4  DRAW            ! draw number

        ! variables
        INTEGER*4  FDB(7)          ! file description block
        INTEGER*4  GROSS(KIGDIV)   ! gross share values (in pennies)  
        INTEGER*4  ST              ! status
        INTEGER*4  I               ! counter variable
        INTEGER*4  J               ! counter variable
        INTEGER*4  TOTSAL          ! total sales
        REAL*8     ROLLOVER(KIGDIV)
        INTEGER*4  TOT2CNT

        REAL*8     DPOOL(KIGDIV)   ! prize pools
        REAL*8     TOTPOL,TOT2POL,PRIZE
	
	REAL*8     DKKPOL_R(KIGDIV),X

        LOGICAL    ROLL

        COMMON SCFREC
        ! start of code
C                                                           
C                                                                               
        ! open game file
        CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)                      
        CALL IOINIT(FDB,3,DKKSEC*256)                                  
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)             

        ! read record for CURRENT draw
        CALL READW(FDB,DRAW,DKKREC,ST)                             
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)          
C                                                                               
C       
        ! check game status                                        
        IF(DKKSTS .NE. GAMENV) THEN                                
            WRITE(6,900) IAM(),GTNAMES(TKIK),GIND,DRAW,DKKSTS            
            CALL GPAUSE                                            
        ENDIF                                                      
        WRITE(6,901) IAM(),GTNAMES(TKIK),GIND,DRAW                       
C                                                                               
C                                                                               
5       CONTINUE                                                   

        ! initialise variables to zero
        DKKTAX    = 0                                                   
        DKKRES(1) = 0                                              
        DKKRES(2) = 0                                              
        DO I = 1, DKKDIV                                           
            DKKTSR(I) = DKKSHR(I)                                  
            IF (DKKPER(I).NE.0) THEN
               DKKSHV(I) = 0
            ENDIF                                          
            GROSS(I)  = 0 
	    DKKPOL_R(I) = DFLOAT(DKKPOL(1,I))*DFLOAT(DYN_BETUNIT) + 
     *                    DFLOAT(DKKPOL(2,I))
	    ROLLOVER(I) = DKKPOL_R(I)
        END DO
        ROLL=.FALSE.

35	CONTINUE
C	DKKSPL = 0
C	CALL PRMYESNO('Do you want to add money to total pool (for promotion)?',
C     *		       YESNO)
C	IF(YESNO .EQ. 1) THEN
C	    CALL PRMMONY('Enter amount ',DKKSPL,1,EXT)	    ! Amount in pennies
C	    IF (EXT.LT.0) GOTO 35
C	    IF (DKKSPL.LE.0) GOTO 35
C	ENDIF
                                                                               
C                                                                               
C GET TOTAL SALES                                                               
C                                                                               
        TOTSAL=0                                                   
        DO J = 1, MAXGAM                                           
          DO I = 1, KIGENT                                           
            TOTSAL = TOTSAL + DKKSAL(I,J)                            
          END DO
        END DO
C
C GET POOL AMOUNTS                                                              
C                                                                               
         TOTPOL = DFLOAT(TOTSAL)*DFLOAT(DYN_BETUNIT)*CALPER(DKKSPR) 
C        TOTPOL = DFLOAT(TOTSAL)*DFLOAT(DYN_BETUNIT)*CALPER(DKKSPR) + 
C     *		 DFLOAT(DKKSPL)
C 
26      CONTINUE
C
C CALCULATE POOL PER DIVISION (IF SHARED DIVISION)
C
        DO I = 1, DKKDIV
           IF (DKKPER(I).GT.0) THEN
	      DPOOL(I) = TOTPOL * CALPER(DKKPER(I))
           ENDIF
           
C          DPOOL(I) = TOTPOL * CALPER(DKKPER(I)) + DKKPOL_R(I)
C          DPOOL(I) = DPOOL(I)+DFLOAT(DKKASH(I))*DFLOAT(DYN_BETUNIT) +
C     *                 DKKPOL_R(I)
        END DO

C
C	Take fixed divisions value from FIRST DIVISION POOL PRIZE
C
	DO I = 2, DKKDIV
           IF (DKKPER(I).EQ.0) THEN  !FIXED DIVISION
              DPOOL(1) = DPOOL(1) - (DKKSHV(I) * DKKSHR(I))
           ENDIF
        ENDDO

	IF (DPOOL(1).LT.DKKMIN .AND. DKKSHR(1).GT.0) THEN
	   WRITE(6,100) CMONY (DKKMIN-DPOOL(1), 11, VALUNIT)
           DPOOL(1) = DKKMIN
	ENDIF

C
C ADD ROLOVER TO DPOOL(1) - DO NOT CONSIDER PREVIOUS NEGATIVE AS PART OF ROLL
C
	IF(DPOOL(1).LT.0) DPOOL(1)=0      !V10
	DPOOL(1) = DPOOL(1) + ROLLOVER(1)

	IF (DKKSHR(1).GT.0) THEN
           IF (DPOOL(1)/DKKSHR(1).LT.DKKMIN) THEN
	      WRITE(6,124) CMONY (DKKMIN*DKKSHR(1)-DPOOL(1), 11, VALUNIT)
              DPOOL(1) = DKKSHR(1)*DKKMIN
           ENDIF
           ROLLOVER(1) = 0
        ENDIF

	IF (DPOOL(1).LT.0) THEN
C           DKKSPL = DKKSPL + DPOOL(1) * (-1)    !VALUE ADDED TO POOL TO GET TO ZERO
           DPOOL(1) = 0
        ENDIF

C        IF(DKKSHR(1).GT.0.AND.DPOOL(1).LT.DFLOAT(DKKMIN*DKKSHR(1))*DFLOAT(DYN_BETUNIT))
C     *     THEN
C           TYPE*,IAM(),' Transferring money to to cover'
C           TYPE*,IAM(),' difference between minimum amount and'
C           TYPE*,IAM(),' this draw''s jackpot'
C           USEMONY=0-DKKMIN+IDNINT(DPOOL(1)/DFLOAT(DYN_BETUNIT))
C           DKKASH(1)=DKKASH(1)-USEMONY
C           DKKWRF(USEAMT)=DKKWRF(USEAMT)-USEMONY
C           DKKSPL = DKKSPL + DFLOAT(DKKMIN)*DFLOAT(DYN_BETUNIT) - DPOOL(1)
C           DPOOL(1) = DFLOAT(DKKMIN*DKKSHR(1))*DFLOAT(DYN_BETUNIT)   !MINIMUM
C	   WRITE(6,150) IAM(),DPOOL(1)/100.0D0
C           ROLLOVER(1)=0
C        ENDIF

C        IF(DKKSHR(1).GT.0. AND.
C     *        DPOOL(1).LT.DFLOAT(SCFPAR(MINKJCK))*DFLOAT(DYN_BETUNIT)) THEN
C           TYPE*,IAM(),' Division 1 win pool',IDNINT(DPOOL(1)/100.D0),
C     *                 ' mk too small '
C           TYPE*,IAM(),' Transferring money to reach parameterized minimum '
C           TYPE*,IAM(),' value amount',
C     *                   SCFPAR(MINKJCK)*DYN_BETUNIT/100,' mk'
C           USEMONY=0-SCFPAR(MINKJCK)+IDNINT(DPOOL(1)/DFLOAT(DYN_BETUNIT))
C           DKKASH(1)=DKKASH(1)-USEMONY
C           DKKWRF(USEAMT)=DKKWRF(USEAMT)-USEMONY
C           DPOOL(1) = TOTPOL * CALPER(DKKPER(1))       
C           DPOOL(1) = DPOOL(1)+DFLOAT(DKKASH(1))*DFLOAT(DYN_BETUNIT) +
C     *                DKKPOL_R(1)
C           ROLLOVER(1)=0
C        ENDIF

C        IF(DKKSHR(1).GT.0.AND.
C     *        DPOOL(1).GT.DFLOAT(SCFPAR(MAXKJCK))*DFLOAT(DYN_BETUNIT)) THEN
C           ROLLOVER(1)=ANINT(DPOOL(1)-
C     *                 DFLOAT(SCFPAR(MAXKJCK))*DFLOAT(DYN_BETUNIT))
C           ROLL=.TRUE.
C           TYPE*,IAM(),'Division 1 win pool',IDNINT(DPOOL(1)/100.D0),
C     *                 ' mk exceeds'
C           TYPE*,IAM(),'parameterized maximum  value amount',
C     *                   SCFPAR(MAXKJCK)*DYN_BETUNIT/100,' mk'
C	   WRITE(6,903) IAM(), ROLLOVER(1)/100.0
C           DPOOL(1) = DPOOL(1) - ROLLOVER(1)
C        ENDIF

C                                                                               
C CALCULATE GROSS SHARE VALUE                                                   
C                                                                               
        DO  I =  DKKDIV,1,-1                                               
            IF(DKKSHR(I).NE.0) THEN                                     
                IF(.NOT.ROLL.OR.I.NE.1)ROLLOVER(I)=0
                IF (DKKPER(I).NE.0) THEN   !SHARED DIVISION
                   DKKSHV(I) = JIDNNT(DPOOL(I)/DFLOAT(DKKSHR(I)))
                   DKKRES(I) = DPOOL(I)-(DKKSHV(I)*DKKSHR(I))
                ENDIF

                ! update lottery tax amount - this is not required at the 
                ! moment, thus DKKSTX is not set
                !DKKTAX = DKKTAX + DKKSTX(I) * DKKSHR(I) 
            ENDIF                                                       
        ENDDO
C
C V11 ROLLDOWN INSTEAD OF ROLLOVER (FOR LAST JOKER DRAW)
C
        DO I = 1, DKKDIV
            IF(DKKSHR(I) .EQ. 0) THEN
              IF(I .GE. DKKDIV) GOTO 45   ! ALL DIVISIONS SCANED AND NO WINNERS
              DPOOL(I + 1) = DPOOL(I + 1) + DPOOL(I)
              DPOOL(I) = 0
            ELSE
              ! set share value to original share value + share of jackpot
              IF(I .EQ. 1) THEN
                 DKKSHV(I) = DKKSHV(I) ! ALREADY CALCULATED PREVIOUSLY DON'T ADD POOLS
              ELSE
                 DKKSHV(I) = DKKSHV(I) + JIDNNT(DPOOL(I)/DFLOAT(DKKSHR(I)))
                 TYPE 906, IAM(), I
              ENDIF
              GOTO 45
            ENDIF
        ENDDO    
45      CONTINUE
C
C COMBINE POOLS IF DIV(N) SHARE .LT. DIV(N-1) SHARE
C
        DO 50 I=DKKDIV,2,-1
        IF(DKKSHV(I).GT.DKKSHV(I-1).AND.DKKSHR(I-1).NE.0 .AND. 
     *    DKKPER(I).NE.0 .AND. DKKPER(I-1).NE.0) THEN
	  WRITE(6,904) IAM(),I-1,CSMONY(DKKSHV(I-1),10,DYN_VALUNIT),
     *                       I,CSMONY(DKKSHV(I),10,DYN_VALUNIT)
          TYPE*,IAM(),'Combining pools...'

          TOT2POL=DPOOL(I)+DPOOL(I-1)

          TOT2CNT=DKKSHR(I)+DKKSHR(I-1)                                 
          PRIZE = TOT2POL/DFLOAT(TOT2CNT)
C
          DPOOL(I)  = PRIZE*DKKSHR(I) - DKKPOL_R(I) - 
     *                DFLOAT(DKKASH(I)*DYN_BETUNIT)
          DPOOL(I-1)= PRIZE*DKKSHR(I-1) - DKKPOL_R(I-1) -
     *                DFLOAT(DKKASH(I-1)*DYN_BETUNIT)

          DKKPER(I)   = IDINT( DPOOL(I) / TOTPOL * 1.0D5 )
          DKKPER(I-1) = IDINT( DPOOL(I-1) / TOTPOL * 1.0D5 )
          GOTO 26
        ENDIF
50      CONTINUE

        DKKSTS=GAMDON

        CALL WRITEW(FDB,DRAW,DKKREC,ST)                                 
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)               

        WRITE(6,902) IAM(),DPOOL(1)/100.0D0
	WRITE(6,905) IAM(),DKKSHR(1)               

        DO  I = 1, DKKDIV                                               
            IF(DKKSHR(I).EQ.0 .AND. DKKPER(I).NE.0) THEN                                     
                ROLL =.FALSE.   !V11  [ was ROLL = .TRUE. ]
                ROLLOVER(I) = ANINT(DPOOL(I))
            ENDIF
        ENDDO
        IF(ROLL) THEN
           CALL READW(FDB,DRAW+1,DKKREC,ST)
           IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW+1)
	   DO I=1,DKKDIV
	      X = ROLLOVER(I)/DFLOAT(DYN_BETUNIT)
	      DKKPOL(1,I) = IDINT(X)
	      DKKPOL(2,I) = IDINT(ROLLOVER(I)-X*DFLOAT(DYN_BETUNIT))
	   ENDDO 
           CALL WRITEW(FDB,DRAW+1,DKKREC,ST)                                 
           IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW+1)
        ENDIF                                              
        CALL CLOSEFIL(FDB)

        RETURN
100	FORMAT(1X,'Money added to reach minimum value 1st DIV : ', A11)
124	FORMAT(1X,'Money added to reach minimum value 1st DIV for more than one winner : ', A11)
150	FORMAT(1X,A,'TOTAL MONEY TRANSFERED TO DIVISION 1 : ',F12.2,' Euros') 
900     FORMAT(1X,A,1X,A8,I1,' event ',I4,' invalid game status> ',I4)       
901     FORMAT(1X,A,1X,A8,I1,' draw ',I4)                                    
902     FORMAT(1X,A,1X,'Total div 1 prize pool: ',F12.2,' Euros')
903 	FORMAT(A,F12.2,' rolled over into the next draw')
904     FORMAT(1X,A,'Div ',I2,' prize ', A10,' is less than div ',I2,
     *           ' prize ', A10)
905     FORMAT(1X,A,'Number of Jackpot winners:',I9)
C
906     FORMAT(1X, A, 'Moved Jackpot To Division', X, I2.2, X, 'Due Last Draw Rules')
      END                                                                       
