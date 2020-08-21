C LTOSIM.FOR
C
C PREPARE A SIMULATED LOTTO WAGER
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
C=======OPTIONS	/CHECK=NOOVERFLOW
        SUBROUTINE EUROSIM(WAGNO,NBRDS,QP,PERQP,NREGBRD,
     *			  BRDMAX,NDRWS,EXT)
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
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
        INTEGER*4 PERQP, NREGBRD, IREGBRD
        INTEGER*4 IND, EXT
        INTEGER*4 BRDMAX, NDRWS
        BYTE      NBRDS, BTMP1, NDRWSHFT
        INTEGER*4 I4TMP
        EQUIVALENCE (I4TMP,NDRWSHFT)
        INTEGER*2 I2TMP
        EQUIVALENCE (I2TMP,BTMP1)
C
        LOGICAL   QP, EVENIB, FIRSTLOOP
        INTEGER*4 SEED1
        INTEGER*4 I, J
        INTEGER*4 INTVLS(MXBET)
        INTEGER*4 BOARDS(MXBET,MXBRD)
        INTEGER*4 BUFERR, WAGNO, TNBRDS
C
        INTEGER*4 RLEN,ST,MULTI,RAND
        INTEGER*4 WEEKS(10) /1,2,3,5,10,1,2,3,5,10/
        INTEGER*2 OPTFLAGS,REVNUM
        BYTE      BOPTFLAGS(2),BREVNUM(2)
        EQUIVALENCE (OPTFLAGS,BOPTFLAGS)
        EQUIVALENCE (REVNUM,BREVNUM)
        BYTE      I1TMP(4)
        EQUIVALENCE (I4TMP,I1TMP(1))
C EURO
        INTEGER*4 GMAXEURO
        PARAMETER (GMAXEURO=50)
        INTEGER*4 GMAXSTAR
        PARAMETER (GMAXSTAR=9)
        INTEGER*4 STARS(MXBET,MXBRD)
        INTEGER*4 INTSTARVLS(MXBET)
        INTEGER*4 GSBET,MYCHKSUM
        INTEGER*4 OPTION
        INTEGER*4 TID
        BYTE TID1(4)
        EQUIVALENCE (TID,TID1(1))
C
        BYTE REP1
        PARAMETER (REP1 = '20'X)
        BYTE REP2
        PARAMETER (REP2 = '80'X)
        
        BYTE EU1DAT(MXLEN)
        INTEGER*2 I2CCITT(2)
        EQUIVALENCE (I4CCITT,I2CCITT)
        
C
CC ------------------------------------------------------------
        DATA (EU1DAT(I),I=1,MXLEN)/
C
C EURO LOTTO WAGER, WITHOUT BOARDS (TO BE GENERATED RANDOMLY)
C
     *                   Z20,   ! 1  control and sequence
     *                   Z00,   ! 2  type - WAGER
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z80,   ! 5  statistics (simulator transaction)
     *                   Z11,   ! 6  gametype - EURO MIL (17)
     *                   Z10,   ! 6  game index and system
     *                   Z00,   ! 7  duration and # of boards
     *                   Z00,   ! 8  option flags
     *                   Z00,   ! 9  option flags
     *                38*Z00/   !    option data
C
C ------------------------------------------------------------
C
C
C Menu de escolha de Euro Mil 
C
C
       
        IF (WAGNO .EQ. 0) THEN
C INITIALIZE DATA FOR THE MAIN LOOP
C
          FIRSTLOOP=.TRUE.
          QP=PERQP .EQ. 100
          IREGBRD=0
          BUFERR=0
          SEED1=20000*INT(SECNDS(0.0))+1
   
          GBET=5  ! numero de apostas
          GSBET=2 ! numero de estrelas
 
 
1          CONTINUE
           CALL CLRSCR(5)
           TYPE*
           TYPE*, IAM(), ' Choose Euro Mil config'
           TYPE*
           TYPE*, IAM(), '     0 = APOSTA SIMPLES'
           TYPE*, IAM(), '     1 = APOSTA MULTIPLA '
           TYPE*, IAM(), '     2 = REPRINT (TRANSACTION) '
           TYPE*, IAM(), '     3 = REPRINT (WAGER) '
           TYPE*, IAM(), '     4 = REPRINT (CANCEL) '
           TYPE*, IAM(), '     5 = REPRINT (VALIDATION) '
           OPTION=0
           CALL INPNUM(' CHOOSE OPTION # : ', OPTION,0,20,ST)                                    
           IF(ST.EQ.-1) CALL GSTOP(GEXIT_OPABORT)
           IF(OPTION.EQ.0) THEN
              GBET=5  ! numero de apostas
              GSBET=2 ! numero de estrelas
           ELSE IF (OPTION.EQ.1) THEN
2             CONTINUE
              CALL CLRSCR(5)
              TYPE*
              TYPE*, IAM(), '          APOSTA MULTIPLA'
              TYPE*
              TYPE*, IAM(), '     0 = NUMERO DE CRUZES   : ', GBET
              TYPE*, IAM(), '     1 = NUMERO DE ESTRELAS : ', GSBET
              OPTION=0
              CALL INPNUM(' CHOOSE OPTION # TO CHANGE OR ''C'' TO CONTINUE: ',OPTION,0,1,ST)
              IF(ST.EQ.-1) CALL GSTOP(GEXIT_OPABORT)
              IF(ST.EQ.-5) THEN
                 IF (GBET .EQ. 5 .AND. GSBET .EQ. 2) GOTO 2
                 SYSBET = .TRUE.
                 OPTION=-1
              ENDIF
              IF(OPTION .EQ. 0) THEN
                 TYPE*
                 CALL INPNUM(' NUMERO DE CRUZES: ', GBET ,5,11,ST)
                 GOTO 2
              ELSE IF (OPTION .EQ. 1) THEN
                 TYPE*
                 CALL INPNUM(' NUMERO DE ESTRELAS: ', GSBET ,2,9,ST)
                 GOTO 2
              ELSE IF (OPTION .NE. -1) THEN
                 GOTO 2
              ENDIF
           ELSE IF (OPTION .EQ. 2) THEN
                RLEN = 3
                MESLEN =4
                MESBUF1(1) = REP1
                MESBUF1(2) = REP2
                MESBUF1(2) = (MESBUF1(2).AND.'F0'X) .OR. 1
                MESBUF1(1) = (MESBUF1(1).AND.'F0'X) .OR. SEQNO(TER)
                CALL SETCHKSUMEURO(MESBUF1,4)
                TYPE 927,MESBUF1(1),MESBUF1(2),MESBUF1(3),MESBUF1(4)
                RETURN
           ELSE IF (OPTION .EQ. 3) THEN
                RLEN = 3
                MESLEN =4
                MESBUF1(1) = REP1
                MESBUF1(2) = REP2
                MESBUF1(2) = (MESBUF1(2).AND.'F0'X) .OR. 2
                MESBUF1(1) = (MESBUF1(1).AND.'F0'X) .OR. SEQNO(TER)
                CALL SETCHKSUMEURO(MESBUF1,4)
                TYPE 927,MESBUF1(1),MESBUF1(2),MESBUF1(3),MESBUF1(4)
                RETURN           	
           ELSE IF (OPTION .EQ. 4) THEN
                RLEN = 3
                MESLEN =4
                MESBUF1(1) = REP1
                MESBUF1(2) = REP2
                MESBUF1(2) = (MESBUF1(2).AND.'F0'X) .OR. 3
                MESBUF1(1) = (MESBUF1(1).AND.'F0'X) .OR. SEQNO(TER)
                CALL SETCHKSUMEURO(MESBUF1,4)
                TYPE 927,MESBUF1(1),MESBUF1(2),MESBUF1(3),MESBUF1(4)
                RETURN           
           ELSE IF (OPTION .EQ. 5) THEN
                RLEN = 3
                MESLEN =4
                MESBUF1(1) = REP1
                MESBUF1(2) = REP2
                MESBUF1(2) = (MESBUF1(2).AND.'F0'X) .OR. 4
                MESBUF1(1) = (MESBUF1(1).AND.'F0'X) .OR. SEQNO(TER)
                CALL SETCHKSUMEURO(MESBUF1,4)
                TYPE 927,MESBUF1(1),MESBUF1(2),MESBUF1(3),MESBUF1(4)
                RETURN
           ELSE 
              GOTO 1
           ENDIF
        ENDIF
C
        IF(PERQP .LT. 100 .AND. PERQP .GT. 0) THEN
          QP=PERQP .GE. INT(101*RAN(SEED1))
        ENDIF
C
C WAGER HEADER
C
        CALL MOVBYT(EU1DAT,1,MESBUF1,1,MXLEN)    !                         
C
C	MESBUF1(7) = GIND*16  ! Game Index = 0
C
        IF(SYSBET) THEN
           NBRDS=1
        ELSE
C          NBRDS=MAX(2,INT(BRDMAX*RAN(SEED1))+1) 
	   NBRDS=INT(BRDMAX*RAN(SEED1)) + 1 
        ENDIF        
        
        MULTI=WEEKS(MDRWMAX(MGNUM))
        RAND=INT(MULTI*RAN(SEED1))+1
        NDRWS=WEEKS(RAND)
        IF (NDRWS .GE. 4) THEN
           NDRWS = 5
        ELSE 
           NDRWS = 1
        ENDIF
        I4TMP=ISHFT(NDRWS,4)                    !EQUIVALENCED TO NDRWSHFT !!!
        MESBUF1(8)= NDRWSHFT .OR. NBRDS
C      
      OPTFLAGS=0

C OPTIONAL DATA
C
      IND=10                                 
      IF(QP) THEN                             !quick pick flags
         OPTFLAGS=IOR(OPTFLAGS,'0200'X) 
         MESBUF1(IND)='FF'X
         MESBUF1(IND+1)='F0'X
         IND=IND+2
      ENDIF

C      IF(SYSBET) THEN                         !system bet 
C         OPTFLAGS=IOR(OPTFLAGS,'0100'X) 
C         IF(GIND.EQ.1.AND.SYSBNUM.GE.8.AND.SYSBNUM.LE.11) THEN
C  	    MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'01'X   !full system
C         ELSE IF(GIND.EQ.1.AND.SYSBNUM.GE.12.AND.SYSBNUM.LE.18) THEN
C  	    MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'02'X   !reduced system
C         ELSE IF(GIND.EQ.2.AND.SYSBNUM.GE.7.AND.SYSBNUM.LE.11) THEN
C  	    MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'01'X   !full system
C         ELSE IF(GIND.EQ.2.AND.SYSBNUM.GE.12.AND.SYSBNUM.LE.24) THEN
C  	    MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'02'X   !reduced system
C         ELSE
C            TYPE*,'Invalid system number',SYSBNUM
C            CALL XWAIT(2,2,ST)
C            EXT=-2
C            RETURN
C         ENDIF
C         IF(GIND.EQ.1) THEN
C            TYPE *,'ESTOU NO GIND=1 E O SYSBNUM E: ',SYSBNUM
C            GBET=SYSBNUM
COXK            IF(SYSBNUM.EQ.18) SYSBNUM=17  ! #17 skipped on betslip
COXK            I4TMP=SYSBNUM-7
C            IF(SYSBNUM.EQ.18) THEN	      ! #17 skipped on betslip
C	        I4TMP=SYSBNUM-8
C	    ELSE
C	        I4TMP=SYSBNUM-7
C	    ENDIF
C         ENDIF 
C         IF(GIND.EQ.2) THEN
C            GBET=SYSBNUM
C            IF(SYSBNUM.GE.21) SYSBNUM=21  ! ##21,22,23 skipped
C            I4TMP=SYSBNUM+4
C         ENDIF 
C         MESBUF1(IND)='00'X
C         MESBUF1(IND+1)=I1TMP(1)
C         IND=IND+2
C         NBRDS=1
C      ENDIF

C      IF(BANK) THEN                           !bank attributes
C         OPTFLAGS=IOR(OPTFLAGS,'0008'X)
C         IF(TELEBET) THEN
C            I4TMP=BANKID+10000000
C         ELSE
C            I4TMP=BANKID
C         ENDIF  
C         MESBUF1(IND)=I1TMP(4)      !BANK ID
C         MESBUF1(IND+1)=I1TMP(3)
C         MESBUF1(IND+2)=I1TMP(2)
C         MESBUF1(IND+3)=I1TMP(1)
C         IND=IND+4
C         I4TMP=BANKACC
C         MESBUF1(IND)=I1TMP(4)     !BANK ACCOUNT
C         MESBUF1(IND+1)=I1TMP(3)
C         MESBUF1(IND+2)=I1TMP(2)
C         MESBUF1(IND+3)=I1TMP(1)
C         IND=IND+4
C      ENDIF

C      IF(GJOK.GT.0 .OR.TELEBET) THEN          !joker direction
C 	 MESBUF1(IND)=GJOK
C         IF(TELEBET) MESBUF1(IND)=TELEJOK
C         IND=IND+1
C         IF(TELEBET) THEN
C            I4TMP=1234567
C            MESBUF1(IND)=I1TMP(4)
C            MESBUF1(IND+1)=I1TMP(3)
C            MESBUF1(IND+2)=I1TMP(2)
C            MESBUF1(IND+3)=I1TMP(1)
C            IND=IND+4
C         ENDIF
C      ENDIF 
C
      OPTFLAGS=IOR(OPTFLAGS,'0100'X)
C      MESBUF1(9)=1
C      MESBUF1(10)=0
      MESBUF1(10)= BOPTFLAGS(1)                 !option flags now set
      MESBUF1(9) = BOPTFLAGS(2)
      MESBUF1(IND+1)=GBET
      MESBUF1(IND+2)=GSBET
      IND = IND + 2
C
C
C SET MONDAY FLAG INDICATOR ( ONLY FOR LOTTTO 1 )
C
      MESBUF1(IND+1) = 0
c      IF(GIND .EQ. 1) THEN
c        MESBUF1(IND) = INT(2*RAN(SEED1))  
c      ENDIF
      IND = IND + 1
C
C SET TRANSACTION ID
C
      TID = TID + 1
      
      MESBUF1(IND+1) = 0
      MESBUF1(IND+2) = 0
      MESBUF1(IND+3) = 0
      MESBUF1(IND+4) = 0
      MESBUF1(IND+5) = TID1(4) 
      MESBUF1(IND+6) = TID1(3) 
      MESBUF1(IND+7) = TID1(2) 
      MESBUF1(IND+8) = TID1(1)
      
      IND = IND + 9
C
C
      RLEN=IND-1                   !for display in first line
C
C BET DETAILS
C     
        
        EVENIB=.FALSE.
        DO 200 I=1,NBRDS
C          IF (FIRSTLOOP) THEN
            IREGBRD=IREGBRD+1
            IF(IREGBRD.EQ.NREGBRD) FIRSTLOOP=.FALSE.
C                BOARD NORMAL
            CALL RANCMB(BOARDS(1,I),GBET,GMAXEURO,SEED1)
            CALL BUBSORT(BOARDS(1,I),GBET)
            CALL CMB2INT(BOARDS(1,I),INTVLS,GBET)
C                BOARD STARS
            CALL RANCMB(STARS(1,I),GSBET,GMAXSTAR,SEED1)
            CALL BUBSORT(STARS(1,I),GSBET)
            CALL CMB2INT(STARS(1,I),INTSTARVLS,GSBET)
            
            TYPE *,' '
C            TYPE *,'VAlores de BOARDS(1,I) ', BOARDS(1,I) 
            TYPE *,' '
C            CALL CMBOFF(BOARDS(1,I),OFF,GBET)
C            SIMOFF(IREGBRD)=OFF
C          ELSE
C            IF(CYCLE) THEN                                           
C	      IREGBRD=MOD(IREGBRD,NREGBRD)+1                         
C	    ELSE
C	      IREGBRD=INT(RAN(SEED1)*(NREGBRD))+1
C	    ENDIF
C            OFF = SIMOFF(IREGBRD)
C            CALL OFFCMB(BOARDS(1,I),OFF,GBET)
C            CALL CMB2INT(BOARDS(1,I),INTVLS,GBET)
C          ENDIF
          DO 300 J=1,GBET
400         IF (INTVLS(J).GT.15) THEN
              IF (EVENIB) THEN
                I2TMP=ISHFT(I2TMP,4)
                MESBUF1(IND)=BTMP1
                IND=IND+1
              ELSE 
                I2TMP=0
              ENDIF
              INTVLS(J)=INTVLS(J)-15
              EVENIB= .NOT. EVENIB                
              GOTO 400
            ELSE
              IF (EVENIB) THEN
                I2TMP=ISHFT(I2TMP,4)+INTVLS(J)
                MESBUF1(IND)=BTMP1
                IND=IND+1
              ELSE
                I2TMP=INTVLS(J)
              ENDIF
              EVENIB=.NOT.EVENIB
            ENDIF
300       CONTINUE
          DO 500 J=1,GSBET
600         IF (INTSTARVLS(J).GT.15) THEN
              IF (EVENIB) THEN
                I2TMP=ISHFT(I2TMP,4)
                MESBUF1(IND)=BTMP1
                IND=IND+1
              ELSE 
                I2TMP=0
              ENDIF
              INTSTARVLS(J)=INTSTARVLS(J)-15
              EVENIB=.NOT.EVENIB                
              GOTO 600
            ELSE
              IF (EVENIB) THEN
                I2TMP=ISHFT(I2TMP,4)+INTSTARVLS(J)
                MESBUF1(IND)=BTMP1
                IND=IND+1
              ELSE
                I2TMP=INTSTARVLS(J)
              ENDIF
              EVENIB=.NOT.EVENIB
            ENDIF
500       CONTINUE
200     CONTINUE
        IF (EVENIB) THEN
          I2TMP=ISHFT(I2TMP,4)
          MESBUF1(IND)=BTMP1
        ELSE
          IND=IND-1
        ENDIF
	TNBRDS=TNBRDS+NBRDS
C
        MESLEN=IND
C
	MESBUF1(1) = (MESBUF1(1).AND.'F0'X) .OR. SEQNO(TER)
                       I4CCITT = DAYCDC + TER
                       MESBUF1(3) = I1CCITT(2)
                       MESBUF1(4) = I1CCITT(1)
C                       CHKLEN=11-1
                       CALL GETCCITT(MESBUF1,1,MESLEN-1,MYCHKSUM)
                       I4CCITT = MYCHKSUM
                       MESBUF1(3) = I1CCITT(2)
                       MESBUF1(4) = I1CCITT(1)  
C	CALL SETCHKSUMEURO(MESBUF1,MESLEN)
C
	IF(MANCHG) CALL PERTUR(EXT)
	IF(EXT.LT.0) RETURN
C
	IF(NODISP) RETURN
C
C LOTTO WAGER DISPLAY
C
        TYPE 920, QP,GTNAMES(GTYP),GIND,NDRWS,NBRDS,TER,
     *	          (MESBUF1(I),I=1,RLEN)
        IF(NBRDS.GT.1) THEN
           TYPE 926, (MESBUF1(I),I=RLEN+1,MESLEN)
        ELSE
           TYPE 925, (MESBUF1(I),I=RLEN+1,MESLEN)
        ENDIF
        DO 401 I=1,NBRDS
          TYPE 930,'     Board: ',(BOARDS(J,I),J=1,GBET),' Stars: ',(STARS(J,I),J=1,GSBET)
          TYPE *,' '
401     CONTINUE
C
	RETURN
C
920     FORMAT(' QP = ',L1,2X,A8,' GIND =',I2,'     NDRWS =',I2,
     *	       '     NBRDS=',I2,'     TER=',I4,/ '     mes: ',
     *         <RLEN>Z3.2)
925     FORMAT('     mes: ',<MESLEN-RLEN>Z3.2)
926     FORMAT('     mes: ',23Z3.2)
927     FORMAT('     mes: ',Z3.2,Z3.2,Z3.2,Z3.2)
930     FORMAT(A12,<GBET>I4.2,A12,<GSBET>I4.2)
C
	END
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE SETCHKSUMEURO(OUTTAB,OUTLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        BYTE      OUTTAB(*)
        INTEGER*4 OUTLEN
C
        INTEGER*4 MYCHKSUM, CHKLEN
C
C
        IF(TELEBET) THEN
           OUTTAB(3) = 0
           OUTTAB(4) = 0
           RETURN
        ENDIF
C
        BASECHKSUM = IAND(DAYCDC,'FFFF'X)
        I4CCITT   = IAND(BASECHKSUM+TER,'FFFF'X)
        OUTTAB(3) = I1CCITT(2)
        OUTTAB(4) = I1CCITT(1)
        CHKLEN=OUTLEN-1
        CALL GETCCITT(OUTTAB,1,CHKLEN,MYCHKSUM)
        I4CCITT = MYCHKSUM
        OUTTAB(3) = I1CCITT(2)
        OUTTAB(4) = I1CCITT(1)
C
        RETURN
        END
