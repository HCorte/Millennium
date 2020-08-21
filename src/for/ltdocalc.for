C LT1DOCALC.FOR
C  
C V16 09-MAY-2000 OXK Added question about adding money for promotion draw
C V15 12-OCT-1999 UXN Fix for promised amount.
C V14 05-AUG-1999 PXO Enabled bonus draw
C V13 04-AUG-1999 PXO Removed old prognosis code
C V12 26-JUL-1999 PXO Added questions about promised amount
C V11 15-JUL-1999 PXO Used real*8 variables to calculate in 1 pennies
C V10 13-JUL-1999 PXO Fixed penny breakage to work for big jackpots
C V09 08-JUL-1999 PXO Added DLTPRP in place of the fixed 95%,
C                     added two rules, 'erikoispotti' and 'lupaus'.
C                     In 'lupaus' there is no rollover money taken from the
C                     reserve funds, and for the promised amount to be paid,
C                     there is a maximum number of 1st prize winners (DLTPRN) 
C                     If DLTPRN equals -1, then it is 'erikoispotti'
C V08 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting, 
C                     Instant Pass Thru Phase 1
C V07 23-AUG-1995 HXK  
C V06 07-APR-1995 HXK Fix for penny money amount per division
C V05 24-MAR-1995 HXK New rule - when jackpot amount promised:
C  i) if jackpot won at least the promised amount must be paid unless
C     the 95% rule applies.
C  2) if jackpot not won the difference between the jackpot as it would
C     have been if there had been a winner and the promised jackpot must
C     be rolled to the next round. 
C V04 21-MAR-1995 HXK Further rule changes
C V03 02-FEB-1995 HXK Change for 95% rule, and use current day's sales for 
C                     prognosis  
C V02 01-FEB-1995 HXK Changes according to customer requirements
C V01 11-NOV-1994 JXP INITIAL RELEASE FOR FINLAND 
C
C LOTTO 7/39 SHARECLC                                                         
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
C Copyright 1995 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT                              
	SUBROUTINE LTDOCALC(GNUM,DRAW,DIVSHR,TMPPOL,TMPAPL,SWITCH,ST)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'

        ! arguments
        INTEGER*4  GNUM            ! game number
        INTEGER*4  DRAW            ! draw number
        INTEGER*4  DIVSHR(LTGDIV)  ! 
        REAL*8     TMPPOL
        REAL*8     TMPAPL
        REAL*8     ADDMARK
        INTEGER*4  SWITCH	   ! FINAL PROGNOSIS, 0 = NO, 1 = YES
	INTEGER*4  YESNO

        ! variables
        INTEGER*4  FDB(7)           ! file description block
        INTEGER*4  TMPDPOOL(LTGDIV) ! for CLTPOOL
        REAL*8     DPOOL(LTGDIV)    ! prize pools
        REAL*8     GROSS(LTGDIV)    ! gross share values (in pennies)  
        INTEGER*4  ST               ! status
        INTEGER*4  I                ! counter variable
        INTEGER*4  J                ! counter variable
        REAL*8     TOTPOL           ! Finnish pools
        INTEGER*4  BNS              ! 1 = No bonus, 2 = Bonus
        INTEGER*4  EXT              ! error status
        REAL*8     DRWSAL(3)        ! total sales for previous draws
        REAL*8     TOTSAL           ! total sales
        INTEGER*4  DRWSHR(LTGDIV,2,3)  ! total shares for previous draws
        INTEGER*4  DRWSHV(LTGDIV,2,3)  ! total shares for previous draws
        INTEGER*4  DIV
        REAL*8	   TOTSHV_CUT
	REAL*8	   TOTSHV
	INTEGER*4  INP_NUM(2)
	INTEGER*4  REG/1/              ! regular draw
	INTEGER*4  BONUS_PRIZE	       ! bonus prize pool
	INTEGER*4  BONUS_PENNIES       ! bonus draw cut pennies
	INTEGER*4  V_BONUS             ! verification amount for BONUS
	
        LOGICAL    POOLS_SET

	INTEGER*4  LUN
	PARAMETER (LUN=4)

        COMMON SCFREC
        COMMON /DLTCOM/ DLTREC

        ! open game file
        CALL OPENW(LUN,SCFGFN(1,GNUM),4,0,0,ST)
        CALL IOINIT(FDB,LUN,DLTSEC*256)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
C
C       Read previous three draws
C
        POOLS_SET = .FALSE.

	BNS = DLTBDR + 1

        DO J=1,3
            DO DIV = 1, DLTDIV
              DO I = 1, BNS
                DRWSHR(DIV,I,J) = 0
                DRWSHV(DIV,I,J) = 0
              ENDDO
              DRWSAL(J)=0.0D0
            ENDDO
	ENDDO

        DO J=1,3
          IF(DRAW.GT.J) THEN
            CALL READW(FDB,DRAW-J,DLTREC,ST)
            IF(ST.NE.0) THEN
              CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
            ELSE
	      DO DIV = 1, DLTDIV
                DO I = 1, BNS
                  DRWSHR(DIV,I,J) = DLTSHR(DIV,I)
                  DRWSHV(DIV,I,J) = DLTSHV(DIV,I)
                ENDDO
              ENDDO
              DO I = 1, LTGENT
                DRWSAL(J) = DRWSAL(J) + DFLOAT(DLTSAL(I))*DFLOAT(DYN_BETUNIT)
              ENDDO
            ENDIF
          ENDIF
        ENDDO

C	read record for CURRENT draw

        CALL READW(FDB,DRAW,DLTREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)

C
C INITIALIZE VARIABLES TO ZERO
C
	DLTTAX = 0
	DLTRES(1) = 0
	DLTRES(2) = 0

	DO I = 1, LTGDIV
	    DLTTSR(I,BNS) = DLTSHR(I,BNS)
	    DLTSHV(I,BNS) = 0
	    GROSS(I) = 0.0D0
	    DLTASH(I) = 0
	ENDDO

C
21	CONTINUE
	DLTMIN = 0
	CALL PRMYESNO('Do you want to set a promised amount? ',YESNO)
	IF(YESNO .NE. 1) GOTO 35
	CALL PRMMONYI8('Enter promised amount ',INP_NUM(1),DYN_BETUNIT,EXT)
	DLTMIN = INP_NUM(1)
	IF(EXT.LT.0) GOTO 21
	
	DLTPRP = 100000   ! THIS IS THE CASE OF
	DLTPRN = -1       ! 'ERIKOISPOTTI' !!

	CALL PRMPER('Enter percentage of the promised amount', DLTPRP,EXT)
	IF(EXT.LT.0) GOTO 21
	CALL PRMNUM('Enter max. winners (0=always)',DLTPRN,0,999999,EXT)
	IF(EXT.LT.0) GOTO 21

	IF(DLTMIN.NE.0) THEN
	  IF(DLTPRN.EQ.-1) THEN
	    WRITE(5,900) IAM(),CMONY(DLTMIN,12,DYN_BETUNIT)
	  ELSE
	    WRITE(5,901) IAM(),CMONY(DLTMIN,12,DYN_BETUNIT),DISPER(DLTPRP),
     *                         DLTPRN
	  ENDIF
	  CALL PRMYESNO('Is this correct [Y/N] ?',EXT)
	  IF(EXT.NE.1) GOTO 21
	ENDIF
C
C

35	CONTINUE
	DLTSPL = 0
	CALL PRMYESNO('Do you want to add money to total pool (for promotion)?',
     *		       YESNO)
	IF(YESNO .EQ. 1) THEN
	    CALL PRMMONY('Enter amount ',DLTSPL,1,EXT) ! Amount in pennies
	    IF (EXT.LT.0) GOTO 35
	    IF (DLTSPL.LE.0) GOTO 35
	ENDIF

	IF(SWITCH.EQ.0) THEN
	    DO I=1,LTGDIV
	        DLTSHR(I,BNS)=DIVSHR(I)
	    ENDDO
	ENDIF


C
C GET TOTAL SALES
C
        TOTSAL=0.0D0
        DO I = 1, LTGENT
            IF(SWITCH.EQ.0) THEN
               TOTSAL = TOTSAL + DFLOAT(LTOSAL(I,1))*DFLOAT(DYN_BETUNIT)
            ELSE   
               TOTSAL = TOTSAL + DFLOAT(DLTSAL(I))*DFLOAT(DYN_BETUNIT)
            ENDIF
        ENDDO
C
C GET POOL AMOUNTS
C

	TOTPOL = TOTSAL*CALPER(DLTSPR) + DLTSPL
 
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



C        IF(SWITCH.EQ.0) THEN  ! prognosis
C           DLTMIN = LTOMIN(1)
C           DO I = 1,LTGDIV
C              DLTASH(I) = LTOASH(I,1)
C           ENDDO
C        ENDIF

10 	CONTINUE

        IF(.NOT.POOLS_SET) THEN
           DO I = 1, DLTDIV    
              DPOOL(I) = TOTPOL * CALPER(DLTPER(I))       
C              DPOOL(I) = DPOOL(I)+DFLOAT(DLTASH(I))*DFLOAT(DYN_BETUNIT) 
              IF(I.EQ.1) DPOOL(I) = DPOOL(I) + 
     *                              DFLOAT(DLTAPL)*DFLOAT(DYN_BETUNIT)
           ENDDO
           IF(DLTSHR(1,REG).NE.0) THEN
	      J=1
              DO WHILE (J.LT.3)
                 IF(DRWSHR(1,REG,J).EQ.0) THEN
     	            DPOOL(1)=DPOOL(1)+DRWSAL(J)*CALPER(DLTPER(1))
		    J=J+1
	         ELSE
		    J=3		    ! SKIP OUT
		 ENDIF
              ENDDO
	   ELSEIF(DLTSHR(2,REG).NE.0) THEN
              IF(DRWSHR(1,REG,1).EQ.0.AND.
     *           DRWSHR(1,REG,2).EQ.0) 
     *	          DPOOL(2)=DPOOL(2)+DRWSAL(2)*CALPER(DLTPER(1))
	   ENDIF
           POOLS_SET = .FALSE.
        ENDIF
C                                                                               
C CALCULATE GROSS SHARE VALUE                                                   
C                                                                               
        TMPAPL = DFLOAT(DLTAPL)*DFLOAT(DYN_BETUNIT)   !new
        DO I = 1,DLTDIV                                 
           IF(DLTSHR(I,REG).EQ.0) THEN                                    
              IF(I.EQ.1) THEN
		 TMPPOL = DPOOL(1)
                 IF(DRWSHR(1,REG,1).EQ.0) THEN
                    DPOOL(1)=DPOOL(1) + DRWSAL(1)*CALPER(DLTPER(1))
		    TMPPOL = DPOOL(1)
                    IF(DRWSHR(1,REG,2).EQ.0) THEN
                       DPOOL(1)=DPOOL(1) + DRWSAL(2)*CALPER(DLTPER(1))
		    ENDIF
                 ENDIF
                 IF(DFLOAT(DLTMIN)*DFLOAT(DYN_BETUNIT).GT.DPOOL(1).AND.
     *              DLTPRN.EQ.-1) THEN   !'ERIKOISPOTTI'
                      TYPE*,IAM(),' Transferring money to rollover to cover'
                      TYPE*,IAM(),' difference between promised amount and'
                      TYPE*,IAM(),' this draw''s jackpot'
                      TMPPOL =  TMPPOL + (DFLOAT(DLTMIN)*DFLOAT(DYN_BETUNIT) -
     *                          DPOOL(1))
                      TMPAPL = TMPAPL + (DFLOAT(DLTMIN)*DFLOAT(DYN_BETUNIT) -
     *                                   DPOOL(1))
                      DLTRES(2)=0-IDNINT(DFLOAT(DLTMIN)*DFLOAT(DYN_BETUNIT)
     *                                   -DPOOL(1))
                      DLTASH(1)=DLTASH(1)-DLTRES(2)/DYN_BETUNIT
                      DLTWRF(USEAMT)=DLTWRF(USEAMT)-DLTRES(2)
                 ENDIF
              ELSEIF(I.NE.DLTDIV) THEN
                 DPOOL(I+1)=DPOOL(I+1)+DPOOL(I)                      
                 DPOOL(I)=0.0D0                                          
              ELSEIF(I.EQ.DLTDIV) THEN                                
                 DLTBRK(I)=DPOOL(I)
                 DPOOL(I)=0.0D0                                          
              ENDIF                                                   
           ELSE
              IF(DLTMIN.NE.0.AND.I.EQ.1) THEN
                 IF(DPOOL(1).LT.(CALPER(DLTPRP)*DFLOAT(DLTMIN)*
     *                           DFLOAT(DYN_BETUNIT)))          THEN
		    IF(DLTPRN.GT.0.AND.DLTSHR(I,REG).GT.DLTPRN) GOTO 40
                    TYPE*,IAM(),' Transferring more money to top division pool'
                    TYPE*,IAM(),' to cover promised prize amount'
                    ADDMARK = 0.0D0
                    IF(DLTSHR(I,REG).GT.1.AND.DLTPRN.EQ.-1) THEN
		       IF(MOD(DLTMIN*DYN_BETUNIT/DOLL_BASE, 
     *                        DLTSHR(I,REG)).NE.0)
     *                    ADDMARK = DLTSHR(I,REG)*DOLL_BASE !money NOT added if 'lupaus'
                    ENDIF
                    DLTRES(2)=0-IDNINT(DFLOAT(DLTMIN)*DFLOAT(DYN_BETUNIT)+
     *                           ADDMARK-DPOOL(1)) !add 1mk
                    DLTASH(1)=DLTASH(1)-DLTRES(2)/DYN_BETUNIT     !per win
                    DLTWRF(USEAMT)=DLTWRF(USEAMT)-DLTRES(2)
                    DPOOL(1)=DFLOAT(DLTMIN)*DFLOAT(DYN_BETUNIT)
     *                       +ADDMARK  !allow for 1 mk per win
40                  CONTINUE
                 ENDIF
              ENDIF
	      IF(I.EQ.1) THEN
                 TMPPOL = 0.0D0
                 TMPAPL = 0.0D0
              ENDIF
              GROSS(I) = DPOOL(I)/DFLOAT(DLTSHR(I,REG))    
              ! get penny breakage
              DLTBRK(I)=0
	      DLTSHV(I,REG) = INT(AINT(GROSS(I)/100.0D0)*100.0D0/DYN_BETUNIT)
              DLTBRK(I) = GROSS(I) - AINT(GROSS(I)/100.0D0)*100.0D0
              ! update reserve pool
              DLTRES(1) = DLTRES(1) + DLTBRK(I)*DLTSHR(I,REG)
              ! update lottery tax amount - this is not required at the 
              ! moment, thus DLTSTX is not set
              DLTTAX = DLTTAX + DLTSTX(I,REG) * DLTSHR(I,REG) 
              IF(DLTSHV(I,REG).LT.(1000/DYN_VALUNIT).AND.I.NE.1.AND. 
     *           DLTPER(I).NE.0) THEN
                 TYPE*,IAM(),'Division ',I,' prize less than 10.00 mks'    
                 TYPE*,IAM(),'Reallocating pools...'                       
                 DLTPER(I-1) = DLTPER(I-1) + DLTPER(I)               
                 DLTPER(I) = 0                                       
                 DLTASH(I-1)=DLTASH(I-1)+DLTASH(I)                   
                 DLTASH(I)=0
                 GOTO 10
              ENDIF                                                   
           ENDIF                                                       
        ENDDO
C
C COMBINE POOLS IF DIV(N) SHARE .LT. DIV(N-1) SHARE
C
        DO 50 I = DLTDIV,2,-1
        IF(DLTSHV(I,REG).GT.DLTSHV(I-1,REG).AND.DLTSHR(I-1,REG).NE.0) THEN
           TYPE*,IAM(),'Division ',I-1,' prize is less than division ',
     *           I,' prize'
           TYPE*,IAM(),'Combining pools...'
           TMPDPOOL(I)   = IDNINT(DPOOL(I)/DFLOAT(DYN_BETUNIT))
           TMPDPOOL(I-1) = IDNINT(DPOOL(I-1)/DFLOAT(DYN_BETUNIT))
           CALL CLTPOOL(TMPDPOOL,DLTSHR,DLTPER,I,I-1,REG)
	   DPOOL(I)   = DFLOAT(TMPDPOOL(I))*DFLOAT(DYN_BETUNIT)
	   DPOOL(I-1) = DFLOAT(TMPDPOOL(I-1))*DFLOAT(DYN_BETUNIT)
           POOLS_SET = .TRUE.
           GOTO 10
        ENDIF

50      CONTINUE

C
C CALCULATE PENNY MONEY
C
	TOTSHV = 0.0D0
	TOTSHV_CUT = 0.0D0
	DO I = 1, DLTDIV
	    TOTSHV = TOTSHV + GROSS(I)*DLTSHR(I,REG)
	    TOTSHV_CUT = TOTSHV_CUT + DLTSHV(I,REG)*DLTSHR(I,REG)
	ENDDO
	DLTRES(1) = IDNINT(TOTSHV - TOTSHV_CUT*DYN_BETUNIT)
	DLTRES(1) = DLTRES(1) + BONUS_PENNIES

        DLTWRF(PENAMT) = DLTRES(1)
        DLTWRF(RESAMT) = IDNINT(TOTSAL * CALPER(DLTSPR) * 0.009)
	IF(SWITCH.EQ.1) DLTSTS=GAMDON

        RETURN

900	FORMAT(1X,A,1X,'Erikoispotti ',A12)
901	FORMAT(1X,A,1X,'Lupaus ',A12,3X,'%',F6.2,3X,'Max.',1X,I4)
905	FORMAT(1X,A,1X,F13.2,' euros for ',I3,' winners')
908	FORMAT(1X,A,1X,'Penny-money from the Bonus Draw is ',F8.2,' euros')
	END                 
