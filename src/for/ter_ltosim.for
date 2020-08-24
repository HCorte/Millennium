C LTOSIM.FOR
C
C PREPARE A SIMULATED LOTTO WAGER
C
C V04 12-APR-2011 RXK  Fix for manual selections
C V03 11-apr-2011 RXK  Joker direction removed from message
C V02 13-DEC-2010 HXK  LOTTO 2 CHANGES - ADDED LOTTO 3, LOTTO 4 WAGERS
C                                      - ADDED LUCKY NUMBER
C                                      - ADDED MANUAL BET
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
	SUBROUTINE LTOSIM(WAGNO,NBRDS,QP,PERQP,NREGBRD,
     *			  BRDMAX,NDRWS,MANTAB,LUCKY,EXT)
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
        LOGICAL   QP, EVENIB, FIRSTLOOP, MANUAL
        INTEGER*4 SEED1
 	INTEGER*4 I, J
        INTEGER*4 INTVLS(MXBET)
        INTEGER*4 BOARDS(MXBET,MXBRD)
        INTEGER*4 BUFERR, WAGNO, TNBRDS
        INTEGER*4 MANTAB(11,0:10,10)   ! (#nums,#panels,#wagers)
        INTEGER*4 LUCKY(10)            ! (#wagers)
	INTEGER*4 WAGCNT
        INTEGER*4 LUC
CC
        INTEGER*4 RLEN,ST,MULTI,RAND, telejok
        INTEGER*4 WEEKS(10) /1,2,3,5,10,1,2,3,5,10/
        INTEGER*2 OPTFLAGS,REVNUM
        BYTE      BOPTFLAGS(2),BREVNUM(2)
        EQUIVALENCE (OPTFLAGS,BOPTFLAGS)
        EQUIVALENCE (REVNUM,BREVNUM)
        BYTE      I1TMP(4)
        EQUIVALENCE (I4TMP,I1TMP(1))
C
        BYTE LT1DAT(MXLEN)
C
CC ------------------------------------------------------------
	DATA (LT1DAT(I),I=1,MXLEN)/
C
C LOTTO WAGER, WITHOUT BOARDS (TO BE GENERATED RANDOMLY OR MANUALLY)
C
     *                   Z20,   ! 1  control and sequence
     *                   Z00,   ! 2  type 
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z80,   ! 5  statistics (simulator transaction)
     *                   Z01,   ! 6  gametype 
     *                   Z00,   ! 7  game index and system
     *                   Z00,   ! 8  duration and # of boards
     *                   Z00,   ! 9  option flags
     *                   Z00,   !10  option flags
     *                38*Z00/   !    option data
C
C ------------------------------------------------------------
C
C
	IF(MANTAB(1,1,1).NE.0) THEN   !manual wager(s) if bet data enetered
	  MANUAL=.TRUE.
	ELSE
	  MANUAL=.FALSE.
	ENDIF
C
	IF (WAGNO.EQ.0) THEN
C INITIALIZE DATA FOR THE MAIN LOOP
C
          FIRSTLOOP=.TRUE.
          QP=PERQP.EQ.100
          IREGBRD=0
          BUFERR=0
          SEED1=20000*INT(SECNDS(0.0))+1
	ENDIF
C
        IF(PERQP.LT.100.AND.PERQP.GT.0) THEN
          QP=PERQP.GE.INT(101*RAN(SEED1))
        ENDIF
C
C WAGER HEADER
C
	CALL MOVBYT(LT1DAT,1,MESBUF1,1,MXLEN)    !                         
C
	MESBUF1(7) = GIND*16
C
	WAGCNT = MOD(WAGNO,10)+1
	IF(MANUAL) THEN
	   NBRDS=MANTAB(1,0,WAGCNT)
        ELSEIF(SYSBET) THEN
           NBRDS=1
        ELSE
           NBRDS=MAX(1,INT(BRDMAX*RAN(SEED1))+1) 
        ENDIF        
        MULTI=WEEKS(MDRWMAX(MGNUM))
        RAND=INT(MULTI*RAN(SEED1))+1
        NDRWS=WEEKS(RAND)
        I4TMP=ISHFT(NDRWS,4)                    !EQUIVALENCED TO NDRWSHFT !!!
        MESBUF1(8)= NDRWSHFT .OR. NBRDS
C      
      OPTFLAGS=0
C*      IF(LTOREV(GIND).NE.0) THEN
C*         OPTFLAGS=IOR(OPTFLAGS,'9000'X)       !lotto '8000',joker '1000'
C*         REVNUM=LTOREV(GIND)
C*         CALL MOVBYT(BREVNUM,1,MESBUF1,11,2)  !lotto game rev
C*      ENDIF
C*      IF(KIKREV(1).NE.0) THEN
C*         REVNUM=KIKREV(1) 
C*         CALL MOVBYT(BREVNUM,1,MESBUF1,13,2)  !joker game rev
C*      ENDIF

      IF(GJOK.NE.0) THEN                      !joker participating
         OPTFLAGS=IOR(OPTFLAGS,'0080'X)
C         OPTFLAGS=IOR(OPTFLAGS,'0004'X)       !joker direction present
      ENDIF
CC      IF(GJOK.EQ.2) THEN                      !second joker
CC         OPTFLAGS=IOR(OPTFLAGS,'0040'X)      
CC      ENDIF 
CC      IF(GJOK.EQ.3) THEN                      !both jokers
CC         OPTFLAGS=IOR(OPTFLAGS,'0080'X)      
CC         OPTFLAGS=IOR(OPTFLAGS,'0040'X)
CC      ENDIF

      IF(GJOK .NE. 0) OPTFLAGS = IOR(OPTFLAGS,'0020'X) ! first joker requested
CC      OPTFLAGS=IOR(OPTFLAGS,'0010'X)        !second joker never requested

      IF(TELEBET) THEN
         OPTFLAGS=IOR(OPTFLAGS,'0080'X)    !joker participating
C         OPTFLAGS=IOR(OPTFLAGS,'0004'X)    !joker direction
         OPTFLAGS=IOR(OPTFLAGS,'0002'X)    !joker number
         telejok = 2
      ENDIF
      
C
C OPTIONAL DATA
C
      IND=11                                 
      IF(QP) THEN                             !quick pick flags
         OPTFLAGS=IOR(OPTFLAGS,'0200'X) 
         MESBUF1(IND)='FF'X
         MESBUF1(IND+1)='F0'X
         IND=IND+2
      ENDIF

      IF(MANUAL.AND.( MANTAB(6,1,WAGCNT).NE.0 .OR. 
     *                (MANTAB(5,1,WAGCNT).EQ.0.AND.
     *                 MANTAB(4,1,WAGCNT).NE.0) )) THEN
        SYSBET=.TRUE.
        IF(MANTAB( 5,1,WAGCNT).EQ.0.AND.MANTAB( 4,1,WAGCNT).NE.0) SYSBNUM=4
        IF(MANTAB( 7,1,WAGCNT).EQ.0.AND.MANTAB( 6,1,WAGCNT).NE.0) SYSBNUM=6
        IF(MANTAB( 8,1,WAGCNT).EQ.0.AND.MANTAB( 7,1,WAGCNT).NE.0) SYSBNUM=7
        IF(MANTAB( 9,1,WAGCNT).EQ.0.AND.MANTAB( 8,1,WAGCNT).NE.0) SYSBNUM=8
        IF(MANTAB(10,1,WAGCNT).EQ.0.AND.MANTAB( 9,1,WAGCNT).NE.0) SYSBNUM=9
        IF(MANTAB(11,1,WAGCNT).EQ.0.AND.MANTAB(10,1,WAGCNT).NE.0) SYSBNUM=10
        IF(MANTAB(11,1,WAGCNT).NE.0) SYSBNUM=11
      ELSE
	SYSBET=.FALSE.
      ENDIF
      IF(SYSBET) THEN                         !system bet 
         OPTFLAGS=IOR(OPTFLAGS,'0100'X) 
         IF(GIND.EQ.1.AND.SYSBNUM.GE.8.AND.SYSBNUM.LE.11) THEN
  	    MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'01'X   !full system
         ELSE IF(GIND.EQ.1.AND.SYSBNUM.GE.12.AND.SYSBNUM.LE.18) THEN
  	    MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'02'X   !reduced system
         ELSE IF(GIND.EQ.2.AND.SYSBNUM.GE.7.AND.SYSBNUM.LE.11) THEN
  	    MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'01'X   !full system
         ELSE IF(GIND.EQ.2.AND.SYSBNUM.GE.12.AND.SYSBNUM.LE.24) THEN
  	    MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'02'X   !reduced system
         ELSE IF(GIND.EQ.3.AND.SYSBNUM.GE.6.AND.SYSBNUM.LE.11) THEN
  	    MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'01'X   !full system
         ELSE IF(GIND.EQ.3.AND.SYSBNUM.EQ.4) THEN
  	    MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'01'X   !full system
         ELSE IF(GIND.EQ.4.AND.SYSBNUM.GE.6.AND.SYSBNUM.LE.11) THEN
  	    MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'01'X   !full system
         ELSE IF(GIND.EQ.4.AND.SYSBNUM.EQ.4) THEN
  	    MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'01'X   !full system
         ELSE
            TYPE*,'Invalid system number',SYSBNUM
            CALL XWAIT(2,2,ST)
            EXT=-2
            RETURN
         ENDIF
         IF(GIND.EQ.1) THEN
            GBET=SYSBNUM
COXK            IF(SYSBNUM.EQ.18) SYSBNUM=17  ! #17 skipped on betslip
COXK            I4TMP=SYSBNUM-7
            IF(SYSBNUM.EQ.18) THEN	      ! #17 skipped on betslip
	        I4TMP=SYSBNUM-8
	    ELSE
	        I4TMP=SYSBNUM-7
	    ENDIF
         ENDIF 
         IF(GIND.EQ.2) THEN
            GBET=SYSBNUM
            IF(SYSBNUM.GE.21) SYSBNUM=21  ! ##21,22,23 skipped
            I4TMP=SYSBNUM+4
         ENDIF 
	 IF(GIND.EQ.3.OR.GIND.EQ.4) THEN
            GBET=SYSBNUM
	    IF(SYSBNUM.EQ.4) THEN
	      I4TMP=7
	    ELSE
              I4TMP=SYSBNUM-5
	    ENDIF
	 ENDIF
         MESBUF1(IND)='00'X
         MESBUF1(IND+1)=I1TMP(1)
         IND=IND+2
         NBRDS=1
      ENDIF

      IF(BANK) THEN                           !bank attributes
         OPTFLAGS=IOR(OPTFLAGS,'0008'X)
         IF(TELEBET) THEN
            I4TMP=BANKID+10000000
         ELSE
            I4TMP=BANKID
         ENDIF  
         MESBUF1(IND)=I1TMP(4)      !BANK ID
         MESBUF1(IND+1)=I1TMP(3)
         MESBUF1(IND+2)=I1TMP(2)
         MESBUF1(IND+3)=I1TMP(1)
         IND=IND+4
         I4TMP=BANKACC
         MESBUF1(IND)=I1TMP(4)     !BANK ACCOUNT
         MESBUF1(IND+1)=I1TMP(3)
         MESBUF1(IND+2)=I1TMP(2)
         MESBUF1(IND+3)=I1TMP(1)
         IND=IND+4
      ENDIF

      !IF(GJOK.GT.0 .OR.TELEBET) THEN          !joker direction
      ! 	 MESBUF1(IND)=GJOK
      !   IF(TELEBET) MESBUF1(IND)=TELEJOK
      !   IND=IND+1
      !   IF(TELEBET) THEN
      !      I4TMP=1234567
      !      MESBUF1(IND)=I1TMP(4)
      !      MESBUF1(IND+1)=I1TMP(3)
      !      MESBUF1(IND+2)=I1TMP(2)
      !      MESBUF1(IND+3)=I1TMP(1)
      !      IND=IND+4
      !   ENDIF
      !ENDIF 
C
      MESBUF1(10)=BOPTFLAGS(1)                 !option flags now set
      MESBUF1(9)=BOPTFLAGS(2)
      
      IF(IND .EQ. 10) IND=IND+1 !NO OPTION DATA WAS ADDED 
C
C
C SET MONDAY FLAG INDICATOR ( ONLY FOR LOTTTO 1 )
C
      MESBUF1(IND) = 0
      IF(GIND .EQ. 1) THEN
        MESBUF1(IND) = INT(2*RAN(SEED1))  
      ENDIF
      IND = IND + 1
C
C
C SET LUCKY NUMBER ( ONLY FOR LOTTTO 3 AND LOTTO 4 )
C
      MESBUF1(IND) = 0
      IF(GIND .EQ. 3 .OR. GIND .EQ. 4) THEN
        !LUC = INT(LTGLUC*RAN(SEED1))+1 
        LUC = INT(13*RAN(SEED1))+1 
        MESBUF1(IND) = LUC  
	IF(MANUAL) MESBUF1(IND) = LUCKY(WAGCNT)
      ENDIF
      IND = IND + 1
C
      RLEN=IND-1                   !for display in first line
C
C BET DETAILS
C
        EVENIB=.FALSE.
        DO 200 I=1,NBRDS
          IREGBRD=IREGBRD+1
          IF(IREGBRD.EQ.NREGBRD) FIRSTLOOP=.FALSE.
C           
	  IF(MANUAL) THEN
	    DO J=1,GBET
	      BOARDS(J,I) = MANTAB(J,I,WAGCNT)
	    ENDDO
	  ELSE
            CALL RANCMB(BOARDS(1,I),GBET,GMAX,SEED1)
            CALL BUBSORT(BOARDS(1,I),GBET)
	  ENDIF
          CALL CMB2INT(BOARDS(1,I),INTVLS,GBET)
C
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
              EVENIB=.NOT.EVENIB                
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
	CALL SETCHKSUM(MESBUF1,MESLEN)
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
          TYPE 930,(BOARDS(J,I),J=1,GBET)
401     CONTINUE
        TYPE 940, LUC
C
	RETURN
C
920     FORMAT(' QP = ',L1,2X,A8,' GIND =',I2,'     NDRWS =',I2,
     *	       '     NBRDS=',I2,'     TER=',I4,/ '     mes: ',
     *         <RLEN>Z3.2)
925     FORMAT('     mes: ',<MESLEN-RLEN>Z3.2)
926     FORMAT('     mes: ',23Z3.2)
930     FORMAT(<GBET>I4.2)
940     FORMAT('     Lucky: ',I2)
C
	END
