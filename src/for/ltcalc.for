C  GXSRC:LTCALC.FOR
C  
C V11 28-JAN-2011 HXK lotto2 Batch: Modify DLTPRC by DOLL_BASE for Div 6
C V10 12-JAN-2010 FJG Lotto2 Batch: Joined JACKPO
C V09 10-DEC-2010 HXK LOTTO 2 CHANGE (new LOTTO 3 and LOTTO 4 games) 
C V07 01-AUG-2009 FJG Portugal Fiscal Legislation changes
C V06 07-DEC-2000 EPH/ANG PREPARE FOR PORTUGAL SHARE CALCULATION
C V06 05-AUG-1999 PXO Enabled bonus draw for Lotto 1
C V05 15-JUL-1999 PXO TMPAPL and ROLLOVER to real*8
C V04 28-APR-1999 RXK Stopsys optimization (some IAM()s added).
C V03 24-MAR-1995 HXK New rule - when jackpot amount promised:
C     i) if jackpot won at least the promised amount must be paid unless
C        the 95% rule applies.
C     2) if jackpot not won the difference between the jackpot as it would
C        have been if there had been a winner and the promised jackpot must
C        be rolled to the next round. 95% rule doe NOT apply in this case.
C V02 01-FEB-1995 HXK Fixes of various types according to customer requirements
C V01 28-NOV-1994 JXP Initial revision.
C  
C
C LOTTO SHARECLC                                                         
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
	SUBROUTINE LTCALC(GNUM, GIND, DRAW)                        
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'        

        ! arguments
        INTEGER*4  GNUM            ! game number
        INTEGER*4  GIND            ! game index
        INTEGER*4  DRAW            ! draw number

        ! variables
        INTEGER*4  OGNUM           ! game number for linked Jackpot
        INTEGER*4  OGIND           ! game index for linked Jackpot
        INTEGER*4  ODRAW           ! draw number for linked Jackpot
        INTEGER*4  TEMP
!        
        INTEGER*4  FDB(7)          ! file description block
        INTEGER*4  ST              ! status
        INTEGER*4  I               ! counter variable
        INTEGER*4  J               ! counter variable
        INTEGER*4  BNS             ! 1 = No bonus, 2 = Bonus
        INTEGER*4  DIV
        REAL*8     ROLLOVER
        REAL*8     TMPAPL
	INTEGER*4  REG/1/          ! regular draw

        INTEGER*4  YESNO

        INTEGER*4  TMPDPOOL(LTGDIV) ! for CLTPOOL
        REAL*8     DPOOL(LTGDIV)    ! prize pools
        REAL*8     TOTPOL           ! Finnish pools
        INTEGER*4  EXT              ! error status
        REAL*8     TOTSAL           ! total sales
        INTEGER*4  BONUS_PRIZE         ! bonus prize pool
	INTEGER*4  V_BONUS
	INTEGER*4 BONUS_PENNIES

        COMMON SCFREC
        COMMON /DLTCOM/ DLTREC


        ! open game file
        CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)                      
        CALL IOINIT(FDB,3,DLTSEC*256)                                  
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)             

        ! read record for CURRENT draw
        CALL READW(FDB,DRAW,DLTREC,ST)                             
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)          
                                                                               
        ! check game status                                        
        IF(DLTSTS .NE. GAMENV) THEN                                
            WRITE(5,900) IAM(),GTNAMES(TLTO),GIND,DRAW,DLTSTS            
            CALL GPAUSE                                            
        ENDIF                                                      
        WRITE(5,901) IAM(), GTNAMES(TLTO),GIND,DRAW                       
                                                                               
       ! set bonus flag
	BNS = DLTBDR + 1

        ! initialise variables to zero
        DLTRES(1) = 0                                              
        DLTRES(2) = 0                                              
        DLTTAX    = 0                                                   

        DO I = 1, DLTDIV                                           
            DO J = 1, BNS
                DLTSHV(I,J) = 0                                          
                DLTSTX(I,J) = 0                                          
            END DO
        END DO

C
C GET TOTAL SALES
C
        TOTSAL=0.0D0
        DO I = 1, LTGENT
           TOTSAL = TOTSAL + DFLOAT(DLTSAL(I))*DFLOAT(DYN_BETUNIT)
        ENDDO
C
C GET POOL AMOUNTS
C
	TOTPOL = TOTSAL*CALPER(DLTSPR) 
C
C BONUS DRAW
C
	BONUS_PENNIES = 0
	BONUS_PRIZE = 0
26 	CONTINUE
	IF(BNS.EQ.2) THEN !bonus draw enabled
	   IF(DLTSHR(1,BNS).NE.0) THEN
	      CALL PRMMONY(' Enter bonus division winning prize: ',
     *                      BONUS_PRIZE,1,EXT)
	   IF(EXT.LT.0) GOTO 26
	   TYPE*,IAM(),' '
	   TYPE*,IAM(),' Verification of bonus division winning prize'
	   TYPE*,IAM(),' '
	   CALL PRMMONY(' Enter bonus division winning prize: ',
     *                   V_BONUS,1,EXT)
	   IF(EXT.LT.0 .OR. BONUS_PRIZE.NE.V_BONUS) THEN
	      TYPE*,IAM(),' Verification failure ... '
	      GOTO 26
	   ENDIF
	   DLTSHV(1,BNS) = BONUS_PRIZE/DYN_BETUNIT
	   DLTSHV(1,BNS) = DLTSHV(1,BNS) /20 *20
	   WRITE(5,905) IAM(),DFLOAT(DLTSHV(1,BNS))/20.D0,DLTSHR(1,BNS)
	   CALL PRMYESNO('Are these values correct (Y/N) ',YESNO)
	   IF(YESNO.NE.1) GOTO 26
	   BONUS_PENNIES = (BONUS_PRIZE - DLTSHV(1,BNS)*DYN_BETUNIT)*
     *	                    DLTSHR(1,BNS)
	   WRITE(5,908) IAM(),DFLOAT(BONUS_PENNIES)/100.D0
	ELSE
	   TYPE*,IAM(),'No bonus division winner'
	ENDIF
       ENDIF

10 	CONTINUE

	ROLLOVER = 0
        DLTTAX = 0

        DO I = 1, DLTDIV    
           DPOOL(I) = TOTPOL * CALPER(DLTPER(I))
           IF (I.EQ.1) THEN
C===============================================================================
C INI V07
C===============================================================================            
C             DPOOL(I) = DPOOL(I) + DLTAPL + DLTPOL(1)
              DPOOL(I) = DPOOL(I) + DLTPOL(1)
              IF(DLTMIN.NE.0.AND.I.EQ.1.AND.DPOOL(1).LT.DLTMIN) THEN
                DLTAPL = DLTMIN - DPOOL(1)
                DPOOL(1) = DLTMIN
                TYPE*,IAM(),' Transferring more money to top division pool'
                TYPE*,IAM(),' to cover promised prize amount: ',DLTAPL
              ENDIF              
C===============================================================================
C FIN V07
C===============================================================================              
           ENDIF
        ENDDO
C                                                                               
C CALCULATE GROSS SHARE VALUE                                                   
C   
C===============================================================================
C INI V07
C===============================================================================                                                                            
C       TMPAPL = DFLOAT(DLTAPL)*DFLOAT(DYN_BETUNIT)   !new
        TMPAPL = 0.0D0
C===============================================================================
C FIN V07
C===============================================================================         
        DO I = 1,DLTDIV                                 
           IF (DLTSHR(I,REG).EQ.0) THEN                                    
              IF (I.EQ.1) THEN
		 ROLLOVER = DPOOL(1)
              ELSEIF(I.NE.DLTDIV) THEN
                 DPOOL(I+1)=DPOOL(I+1)+DPOOL(I)                      
                 DPOOL(I)=0.0D0                                          
              ELSEIF(I.EQ.DLTDIV) THEN                                
                 ROLLOVER = ROLLOVER + DPOOL(I)
                 DPOOL(I)=0.0D0                                          
              ENDIF                                                   
           ELSE
C===============================================================================
C INI V07
C===============================================================================              
C             IF(DLTMIN.NE.0.AND.I.EQ.1) THEN
C               IF (DPOOL(1).LT.DLTMIN) THEN
C                 TYPE*,IAM(),' Transferring more money to top division pool'
C                 TYPE*,IAM(),' to cover promised prize amount'
C                 DPOOL(1) = DLTMIN
C                 CONTINUE
C               ENDIF
C             ENDIF    
C===============================================================================
C FIN V07
C===============================================================================          
	      IF (I.EQ.1) THEN
                 ROLLOVER = 0.0D0
                 TMPAPL = 0.0D0
              ENDIF
C===============================================================================
C INI V09
C===============================================================================          
	      IF(I.EQ.6.AND.(GIND.EQ.3.OR.GIND.EQ.4)) THEN
	        DLTSHV(I,REG) = DLTPRC/DOLL_BASE
	        DPOOL(I)= DLTSHV(I,REG)*DLTSHR(I,REG)
	      ELSE
C===============================================================================
C FIN V09
C===============================================================================          
                DLTSHV(I,REG) = DPOOL(I)/DFLOAT(DLTSHR(I,REG))
                ! update lottery tax amount - this is not required at the 
                ! moment, thus DLTSTX is not set
                DLTTAX = DLTTAX + DLTSTX(I,REG) * DLTSHR(I,REG) 
	      ENDIF
           ENDIF                                                       
        ENDDO
C
C COMBINE POOLS IF DIV(N) SHARE .LT. DIV(N-1) SHARE
C
        DO 50 I = DLTDIV,2,-1
C===============================================================================
C INI V09
C===============================================================================          
	IF(I.EQ.6.AND.(GIND.EQ.3.OR.GIND.EQ.4)) CYCLE  !ALWAYS REFUND
C===============================================================================
C FIN V09
C===============================================================================          
        IF(DLTSHV(I,REG).GT.DLTSHV(I-1,REG).AND.DLTSHR(I-1,REG).NE.0) THEN
           TYPE*,IAM(),'Division ',I-1,' prize is less than division ',
     *           I,' prize'
           TYPE*,IAM(),'Combining pools...'
           TMPDPOOL(I)   = IDNINT(DPOOL(I)/DFLOAT(DYN_BETUNIT))
           TMPDPOOL(I-1) = IDNINT(DPOOL(I-1)/DFLOAT(DYN_BETUNIT))
           CALL CLTPOOL(TMPDPOOL,DLTSHR,DLTPER,I,I-1,REG)
	   DPOOL(I)   = DFLOAT(TMPDPOOL(I))*DFLOAT(DYN_BETUNIT)
	   DPOOL(I-1) = DFLOAT(TMPDPOOL(I-1))*DFLOAT(DYN_BETUNIT)
	   DLTSHV(I,REG)   = DPOOL(I)/DFLOAT(DLTSHR(I,REG))    
           DLTSHV(I-1,REG) = DPOOL(I-1)/DFLOAT(DLTSHR(I-1,REG))    
        ENDIF

50      CONTINUE

C
C CALCULATE PENNY MONEY
C
	DO I = 1, DLTDIV
	   IF	( DLTSHR(I,REG).GT.0 )
     *	        DLTRES(1) = DLTRES(1) + (DPOOL(I) - DLTSHV(I,REG)*DLTSHR(I,REG))
	ENDDO

        DLTSTS=GAMDON

C
C	WRITE BACK TO FILE
C
        DO I = 1, DLTDIV
            DO J = 1, BNS
                DLTTSR(I,J) = DLTSHR(I,J)
            END DO
        END DO
  
	CALL WRITEW(FDB,DRAW,DLTREC,ST)                                 
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)               

        DO DIV = 1,DLTDIV
           WRITE(5,902) IAM(),DIV,CMONY(DLTSHV(DIV,REG),12,DYN_BETUNIT),
     *                  DLTSHR(DIV,REG)
        ENDDO

C
C WRITE MOVING AMOUNT TO NEXT DRAW RECORD OF THE OTHER GAME
C
C DLTJPG is the INDEX for same TYPE to MOVE the JACKPOT for the next inmediate draw
C
        CALL READW(FDB,DRAW+1,DLTREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)

        IF(DLTJPG.EQ.0) THEN  ! V10
          DLTPOL(1) = IDNINT(ROLLOVER/DFLOAT(DYN_BETUNIT))
        ENDIF
        DLTAPL    = IDNINT(TMPAPL/DFLOAT(DYN_BETUNIT))
  
        CALL WRITEW(FDB,DRAW+1,DLTREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
        CALL CLOSEFIL(FDB)
C=======V10=====================================================================
        IF(DLTJPG.NE.0) THEN  
          OGIND = DLTJPG
          OGNUM = GTNTAB(TLTO,OGIND)
          ODRAW = DAYDRW(OGNUM)
          IF(ODRAW.GT.0) THEN     
            WRITE(5,910) IAM(),(GLNAMES(TEMP,OGNUM),TEMP=1,4),ODRAW
            CALL OPENW(3,SCFGFN(1,OGNUM),4,0,0,ST)                      
            CALL IOINIT(FDB,3,DLTSEC*256)                                  
            IF(ST.NE.0) CALL FILERR(SCFGFN(1,OGNUM),1,ST,0)             
    
            ! read record for NEXT draw
            CALL READW(FDB,ODRAW,DLTREC,ST)                             
            IF(ST.NE.0) CALL FILERR(SCFGFN(1,OGNUM),2,ST,ODRAW)          
                                                                                   
            ! check game status                                        
            IF(DLTSTS .NE. GAMOPN) THEN                                
              WRITE(5,900) IAM(),GTNAMES(TLTO),OGIND,ODRAW,DLTSTS            
              CALL GPAUSE                                            
            ENDIF            
            
            DLTPOL(1) = IDNINT(ROLLOVER/DFLOAT(DYN_BETUNIT))
      
            CALL WRITEW(FDB,ODRAW,DLTREC,ST)
            IF(ST.NE.0) CALL FILERR(SCFGFN(1,OGNUM),2,ST,ODRAW)
            CALL CLOSEFIL(FDB)            
          ELSE
            WRITE(5,911) IAM(),(GLNAMES(TEMP,OGNUM),TEMP=1,4)                 
          ENDIF
        ENDIF
C=======V10=====================================================================                  

        RETURN

900     FORMAT(1X,A,1X,A8,I1,' event ',I4,' invalid game status> ',I4)       
901     FORMAT(1X,A,1X,A8,I1,' draw ',I4)                                    
902     FORMAT(1X,A,1X,'Total div ',I1,' prize pool:',A12, ' euros,'
     *         1X,'number of winners:',I9)
905	FORMAT(1X,A,1X,F13.2,' euros for ',I3,' winners')
908	FORMAT(1X,A,1X,'Penny-money from the Bonus Draw is ',F8.2,' euros')
910     FORMAT(1X,A,1X,'Posting JACKPOT to ',4A4,' Draw: ',I4)
911     FORMAT(1X,A,1X,'ERROR: Undefined draw to post JACKPOT for ',4A4)
	END                                                                       
