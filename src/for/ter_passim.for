C
C V02 13-APR-2010 RXK SIMULATION OF WAGERS ADDED.
C V01 18-DEC-2000 ANG INITIAL RELEASE FOR PORTUGAL
C
C
C PASSIM  - PREPARE A SIMULATED PASSIVE WAGER (RESERVE,SALE AND RELEASE)
C VALPASS - PREPARE A SIMULATED PASSIVE VALIDATION
C UNSPASS - PREPARE A SIMULATED PASSIVE UNSOLD    
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PASSIM(WAGNO,NBRDS,QP,PERQP,NREGBRD,
     *                    BRDMAX,NDRWS,EXT)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER*4 WAGNO,NBRDS,PERQP,NREGBRD,IREGBRD,BRDMAX,NDRWS,EXT
        LOGICAL   QP

        INTEGER*4 IND,SEED1,GNUM,WY,WEEK,YEAR
        INTEGER*4 RAND,OPTYP,NF,SERIE,F,NN,LEN
        INTEGER*4 I

        INTEGER*2 OPTFLAGS
        BYTE      BOPTFLAGS(2)
        EQUIVALENCE (OPTFLAGS,BOPTFLAGS)

        INTEGER*4 I4TEMP
        BYTE      I1TEMP(4)
        EQUIVALENCE (I4TEMP,I1TEMP(1))

        LOGICAL FIRSTLOOP

        CHARACTER*5 MASK
        BYTE BMASK(5)   
        EQUIVALENCE(MASK,BMASK)
C
        BYTE PA1DAT(MXLEN)
C
CC ------------------------------------------------------------
C
C PASSIVE WAGER
C
        DATA (PA1DAT(I),I=1,MXLEN)/
     *                   Z20,   ! 1  control and sequence
     *                   Z00,   ! 2  type
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z80,   ! 5  statistics (simulator transaction)
     *                   Z10,   ! 6  gametype
     *                   Z00,   ! 6  game index and system
     *                   Z10,   ! 7  duration and # of boards
     *                   Z00,   ! 8  option flags
     *                   Z00,   ! 9  option flags
     *                38*Z00/   !    option data
C
C ------------------------------------------------------------
C
C
        ! For unused parameters
        NBRDS = NBRDS
        NREGBRD = NREGBRD
        IREGBRD = IREGBRD
        BRDMAX = BRDMAX
        NDRWS = NDRWS
C         
C INITIALIZE DATA FOR THE MAIN LOOP
C
        IF (WAGNO.EQ.0) THEN
          FIRSTLOOP=.TRUE.
          QP=PERQP.EQ.100
          SEED1=20000*INT(SECNDS(0.0))+1
        ENDIF
C
        IF(PERQP.LT.100.AND.PERQP.GT.0) THEN
          QP=PERQP.GE.INT(101*RAN(SEED1))
        ENDIF
C
C OPERATION TYPE
C
        CALL INPNUM('Enter (1-Reserve,2-Sale,3-Release) ',OPTYP,1,3,EXT)
	IF (EXT.EQ.-1) RETURN        
C
C WAGER HEADER
C
        CALL MOVBYT(PA1DAT,1,MESBUF1,1,MXLEN)    !
C
        MESBUF1(7) = GIND*16 + OPTYP
        OPTFLAGS=0                             
        MESBUF1(10)=BOPTFLAGS(1)                 !no optional data
        MESBUF1(9)=BOPTFLAGS(2)
C
C WEEK,YEAR
C
        GNUM = GTNTAB(TPAS,GIND)
        DO I=1,PAGEMI
          IF(PASEMIS(I,GIND).EQ.DAYDRW(GNUM)) THEN
             WY = PASDRAW(I,GIND)
             CALL GETPASDRW(WY,WEEK,YEAR) 
             TYPE*,'DAYDRW IS FOR WEEK',WEEK,' AND YEAR',YEAR
           ENDIF
        ENDDO 
        CALL INPNUM('Enter week #',RAND,WEEK,WEEK+PMAXSAL-1,EXT)
        IF (EXT.EQ.-1) RETURN
        !RAND = INT(4*RAN(SEED1))
        !RAND = 0
        !WEEK = WEEK + RAND
        !IF(WEEK.GT.52) THEN
        !   WEEK = WEEK  - 52
        !   YEAR = YEAR + 1
        !ENDIF 
        WEEK = RAND 
        MESBUF1(11) = WEEK
        MESBUF1(12) = MOD(YEAR,100)
        IND = 13 
C
C RESERVE
C
        IF(OPTYP.EQ.EPASRES) THEN
           CALL PRMTEXT('Enter Mask (incl.asterisks)',MASK,LEN)
           MESBUF1(IND+0) = BMASK(1)
           MESBUF1(IND+1) = BMASK(2)
           MESBUF1(IND+2) = BMASK(3)
           MESBUF1(IND+3) = BMASK(4)
           MESBUF1(IND+4) = BMASK(5)
           IND = IND +5
           CALL INPNUM('Enter # of fractions',NF,1,PMAXTIC,EXT)
           IF (EXT.EQ.-1) RETURN
           MESBUF1(IND) = NF
           IND = IND + 1
C
C SALE
C             
        ELSEIF(OPTYP.EQ.EPASSAL) THEN
           CALL INPNUM('Enter #',I4TEMP,1,99999,EXT)
           IF (EXT.EQ.-1) RETURN   
           MESBUF1(IND+0) = I1TEMP(4)    
           MESBUF1(IND+1) = I1TEMP(3)    
           MESBUF1(IND+2) = I1TEMP(2)    
           MESBUF1(IND+3) = I1TEMP(1)    
           IND = IND + 4
           IF(GIND.EQ.PSBCLA) THEN
              CALL INPNUM('Enter Serie',SERIE,1,PMAXSERCLA,EXT)
              IF (EXT.EQ.-1) RETURN
           ELSE
              SERIE = 1
           ENDIF
           IF(GIND.EQ.PSBCLA) THEN  
              CALL INPNUM('Enter Fraction',F,1,PMAXFRACLA,EXT)
           ELSE
              CALL INPNUM('Enter Fraction',F,1,PMAXFRAPOP,EXT)
           ENDIF
           IF (EXT.EQ.-1) RETURN
           MESBUF1(IND) = ISHFT(SERIE,4) + F
           IND = IND + 1
C
C RELEASE
C
        ELSEIF(OPTYP.EQ.EPASREL) THEN
           CALL INPNUM('Enter # of numbers to release',NN,1,3,EXT)
           IF (EXT.EQ.-1) RETURN
           MESBUF1(IND) = NN
           IND = IND + 1
           DO I = 1,NN
              CALL INPNUM('Enter number to release',I4TEMP,1,99999,EXT)
              IF (EXT.EQ.-1) RETURN
              MESBUF1(IND+0) = I1TEMP(4)    
              MESBUF1(IND+1) = I1TEMP(3)    
              MESBUF1(IND+2) = I1TEMP(2)    
              MESBUF1(IND+3) = I1TEMP(1)    
              IND = IND + 4
           ENDDO 
        ENDIF  

        MESLEN = IND
        MESBUF1(1) = (MESBUF1(1).AND.'F0'X) .OR. SEQNO(TER)
        CALL SETCHKSUM(MESBUF1,MESLEN)
        IF(MANCHG) CALL PERTUR(EXT)
        IF(EXT.LT.0) RETURN
C
        IF(NODISP) RETURN
C
C DISPLAY EPASSIVE WAGER MESSAGE
C
        TYPE 900, GTNAMES(GTYP),GIND,TER,WEEK,YEAR,
     *            (MESBUF1(I),I=1,MESLEN)
        
        RETURN
900     FORMAT(1X,A8,' GIND =',I2,'  TER=',I4,'  WEEK/YEAR ',I2.2,'/',I4.4,//,
     *         <MESLEN>Z3.2, /)
        END
C==========================================================================
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE VALPASS(EXT)
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'

	INTEGER*4 I4TEMP,I,IND,EXT,TKT,TYP,MYGIND
	integer*2 I2TEMP

	BYTE PA1DAT(MXLEN),I1TEMP(4)

        DATA (PA1DAT(I),I=1,MXLEN)/

     *                   Z20,   ! 1  control and sequence
     *                   Z40,   ! 2  type 
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z80,   ! 5  statistics (simulator transaction)
     *                   Z00,   ! 6  gametype 
     *                   Z00,   ! 6  game index and system
     *                   Z00,   ! 7  duration and # of boards
     *                   Z00,   ! 8  option flags
     *                   Z00,   ! 9  option flags
     *                38*Z00/   !    option data

	EQUIVALENCE (I1TEMP,I2TEMP,I4TEMP)

	CALL CLRSCR(6)

        CALL MOVBYT(PA1DAT,1,MESBUF1,1,MXLEN)    

        CALL INPNUM('Enter game index ',MYGIND,1,NUMPAS,EXT)
	IF (EXT.EQ.-1) RETURN

        CALL INPNUM('Enter (1-Regular 2-Mid Tier) ',TYP,1,10,EXT)
	IF (EXT.EQ.-1) RETURN
	IF (TYP.EQ.1) THEN
	    MESBUF1(2) = (MESBUF1(2).AND.'F0'X).OR.'08'X
	ELSE
	    MESBUF1(2) = (MESBUF1(2).AND.'F0'X).OR.'09'X
	ENDIF	

        MESBUF1(6) = ISHFT(MYGIND,4)
C**	MESBUF1(6) = IOR(IAND(MESBUF1(6),'F0'X),I4TEMP)

        CALL INPNUM('Enter offline agent number ',I4TEMP,0,99999999,EXT)
	IF (EXT.EQ.-1) RETURN

	MESBUF1(7)  = I1TEMP(4)
	MESBUF1(8)  = I1TEMP(3)
	MESBUF1(9)  = I1TEMP(2)
	MESBUF1(10) = I1TEMP(1)

        CALL INPNUM('How many tickets to validate? ',TKT,1,10,EXT)
	IF (EXT.EQ.-1) RETURN

	I4TEMP = TKT
	MESBUF1(11) = I1TEMP(1)                               !Quantidade de tickets a ser validados

	IND=12
	DO I=1,TKT

	   TYPE*,IAM()
	   TYPE*,IAM(),'****** TICKET ',I,' ******'
           CALL INPNUM('Enter week ',I4TEMP,1,35,EXT)
	   IF (EXT.EQ.-1) RETURN

	   MESBUF1(IND) = I1TEMP(1)
	   IND = IND + 1
	
           CALL INPNUM('Enter year (2 digits) ',I4TEMP,0,99,EXT)
	   IF (EXT.EQ.-1) RETURN

	   MESBUF1(IND) = I1TEMP(1)
	   IND = IND + 1	

           CALL INPNUM('Enter ticket number ',I4TEMP,0,9999999,EXT)
	   IF (EXT.EQ.-1) RETURN

	   MESBUF1(IND)   = I1TEMP(4)
	   MESBUF1(IND+1) = I1TEMP(3)
	   MESBUF1(IND+2) = I1TEMP(2)
	   MESBUF1(IND+3) = I1TEMP(1)

	   IND = IND + 4

           CALL INPNUM('Enter serie number ',I4TEMP,1,15,EXT)
	   IF (EXT.EQ.-1) RETURN

	   MESBUF1(IND) = I1TEMP(1)
	   IND = IND + 1
C	   MESBUF1(IND) = ISHFT(I1TEMP(1),4) 

           CALL INPNUM('Enter tenth number ',I4TEMP,1,15,EXT)
	   IF (EXT.EQ.-1) RETURN

	   MESBUF1(IND) = I1TEMP(1)
C	   MESBUF1(IND) = (MESBUF1(IND).AND.'F0'X).OR.I1TEMP(1)
	   IND = IND + 1

           CALL INPNUM('Enter validation key ',I4TEMP,1,999999999,EXT)
	   IF (EXT.EQ.-1) RETURN
	   
           MESBUF1(IND)   = I1TEMP(4)
           MESBUF1(IND+1) = I1TEMP(3)
           MESBUF1(IND+2) = I1TEMP(2)
           MESBUF1(IND+3) = I1TEMP(1)
	   IND = IND + 4

	ENDDO

	MESLEN = IND

	MESBUF1(1) = (MESBUF1(1).AND.'F0'X) .OR. SEQNO(TER)   !Control and sequence number
        CALL SETCHKSUM(MESBUF1,MESLEN)

	RETURN

	END

C*********************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE UNSPASS(EXT)
C*********************************************
C
C PASSIVE UNSOLD TICKETS

        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'

        INTEGER*4 I4TEMP,I,IND,EXT,TKT,MYGIND
        integer*2 I2TEMP

        BYTE PA1DAT(MXLEN),I1TEMP(4)

        DATA (PA1DAT(I),I=1,MXLEN)/

     *                   Z20,   ! 1  control and sequence
     *                   Z40,   ! 2  type 
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z80,   ! 5  statistics (simulator transaction)
     *                   Z00,   ! 6  gametype 
     *                   Z00,   ! 6  game index and system
     *                   Z00,   ! 7  duration and # of boards
     *                   Z00,   ! 8  option flags
     *                   Z00,   ! 9  option flags
     *                38*Z00/   !    option data

        EQUIVALENCE (I1TEMP,I2TEMP,I4TEMP)

        CALL CLRSCR(6)

        CALL MOVBYT(PA1DAT,1,MESBUF1,1,MXLEN)    

	MESBUF1(2) = (MESBUF1(2).AND.'F0'X).OR.'06'X

C	I1TEMP(1) = '4'X
C	I1TEMP(2) = '6'X
C
C	MESBUF1(2) = IOR(ISHFT(I1TEMP(1),4),I1TEMP(2))
	
        CALL INPNUM('Enter game index ',MYGIND,1,NUMPAS,EXT)
	IF (EXT.EQ.-1) RETURN

        CALL INPNUM('Enter return type ',I4TEMP,1,4,EXT)
        IF (EXT.EQ.-1) RETURN
	
        MESBUF1(6) = ISHFT(MYGIND,4)
	MESBUF1(6) = IOR(IAND(MESBUF1(6),'F0'X),I4TEMP)

        CALL INPNUM('Enter offline agent number ',I4TEMP,0,99999999,EXT)
	IF (EXT.EQ.-1) RETURN

	MESBUF1(7)  = I1TEMP(4)
	MESBUF1(8)  = I1TEMP(3)
	MESBUF1(9)  = I1TEMP(2)
	MESBUF1(10) = I1TEMP(1)

        CALL INPNUM('How many tickets to unsold ? ',TKT,1,10,EXT)
        IF (EXT.EQ.-1) RETURN

        I4TEMP = TKT
        MESBUF1(11) = I1TEMP(1)                               

        IND=12 
        DO I=1,TKT

           TYPE*,IAM()
           TYPE*,IAM(),'****** TICKET ',I,' ******'

           CALL INPNUM('Enter week ',I4TEMP,1,35,EXT)
	   IF (EXT.EQ.-1) RETURN

	   MESBUF1(IND) = I1TEMP(1)
	   IND = IND + 1
	
           CALL INPNUM('Enter year (2 digits) ',I4TEMP,0,99,EXT)
	   IF (EXT.EQ.-1) RETURN

	   MESBUF1(IND) = I1TEMP(1)
	   IND = IND + 1	

           CALL INPNUM('Enter ticket number ',I4TEMP,0,999999999,EXT)
           IF (EXT.EQ.-1) RETURN

           MESBUF1(IND)   = I1TEMP(4)
           MESBUF1(IND+1) = I1TEMP(3)
           MESBUF1(IND+2) = I1TEMP(2)
           MESBUF1(IND+3) = I1TEMP(1)

           IND = IND + 4

           CALL INPNUM('Enter serie number ',I4TEMP,1,15,EXT)
           IF (EXT.EQ.-1) RETURN

           MESBUF1(IND) = I1TEMP(1) 

           CALL INPNUM('Enter tenth number ',I4TEMP,1,15,EXT)
           IF (EXT.EQ.-1) RETURN

           IND = IND + 1
           MESBUF1(IND) = I1TEMP(1)

           CALL INPNUM('Enter validation key ',I4TEMP,1,999999999,EXT)
           IF (EXT.EQ.-1) RETURN

           IND = IND + 1
           MESBUF1(IND)   = I1TEMP(4)
           MESBUF1(IND+1) = I1TEMP(3)
           MESBUF1(IND+2) = I1TEMP(2)
           MESBUF1(IND+3) = I1TEMP(1)
           IND = IND + 4

        ENDDO

        MESLEN = IND

        MESBUF1(1) = (MESBUF1(1).AND.'F0'X) .OR. SEQNO(TER)   !Control and sequence number
        CALL SETCHKSUM(MESBUF1,MESLEN)

        RETURN

        END

