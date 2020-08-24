C STRSIM.FOR
C
C V02 15-JUN-2000 OXK CLEANUP W/ WARNINGS=ALL
C V01 31-MAY-1999 UXN INITIAL RELEASE.
C
C PREPARE A SIMULATED SUPER TRIPLE WAGER
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
	SUBROUTINE STRSIM(WAGNO,NBRDS,QP,PERQP,
     *			  DRWMAX,BRDMAX,NDRWS,EXT)
C
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
        INCLUDE 'INCLIB:STRCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
        INTEGER*4 PERQP
        INTEGER*4 IND, EXT
	INTEGER*4 DRWMAX, BRDMAX, NDRWS
        INTEGER*4 RAND,REMRAND(MAXSTRRW),REMRAND2(MAXSTRRW)
        BYTE      NBRDS, BTMP1, NDRWSHFT
	INTEGER*4 I4TMP
	EQUIVALENCE (I4TMP,NDRWSHFT)
        INTEGER*2 I2TMP
        EQUIVALENCE (I2TMP,BTMP1)
C
        LOGICAL   QP
        INTEGER*4 SEED1
 	INTEGER*4 I, J, K
        INTEGER*4 WAGNO, TNBRDS
C
        INTEGER*4 RLEN
        INTEGER*4 FIRSTNUM,SECONDNUM,THIRDNUM
        INTEGER*4 FIRSTLIST(MAXSTRRW),SECONDLIST(MAXSTRRW),THIRDLIST(MAXSTRRW)
        INTEGER*4 REMIND
        INTEGER*2 OPTFLAGS,REVNUM
        BYTE      BOPTFLAGS(2),BREVNUM(2)
        EQUIVALENCE (OPTFLAGS,BOPTFLAGS)
        EQUIVALENCE (REVNUM,BREVNUM)
        BYTE      I1TMP(4)
        EQUIVALENCE (I4TMP,I1TMP(1))
C
        INTEGER*4 AMOUNT(10)
        INTEGER*4 AMTLIST(8)
        DATA AMTLIST /5,10,12,20,50,100,300,500/ ! In marks !!
        INTEGER*4 OPENCNT,OPENROWS(MAXSTRRW)
        INTEGER*4 PREVGIND/0/,SYSN
C
        BYTE DB1DAT(MXLEN)
C
CC ------------------------------------------------------------
C
C WINTIP WAGER, WITHOUT BOARDS (TO BE GENERATED RANDOMLY)
C
	DATA (DB1DAT(I),I=1,MXLEN)/
     *                   Z20,   ! 1  control and sequence
     *                   Z00,   ! 2  type 
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z80,   ! 5  statistics (simulator transaction)
     *                   Z0E,   ! 6  gametype
     *                   Z00,   ! 7  game index and system
     *                   Z00,   ! 8  duration and # of boards
     *                   Z00,   ! 9  option flags
     *                   Z00,   !10  option flags
     *                38*Z00/   !    option data
C
C ------------------------------------------------------------
C
C HACKS TO REMOVE COMPILER ERRORS
	TRABUF(1) = TRABUF(1)
	PERQP = PERQP
	DRWMAX = DRWMAX
	BRDMAX = BRDMAX
C
C INITIALIZE DATA FOR THE MAIN LOOP
C
      IF(GIND.NE.PREVGIND) THEN
         J=0
         DO I=1,MAXSTRRW                     !create a list of open rows
            IF(STRSTA(I,GIND).EQ.GAMOPN) THEN
               J=J+1
               OPENROWS(J)=I
               OPENCNT=OPENCNT+1
            ENDIF
         ENDDO
         PREVGIND=GIND
      ENDIF
C
      IF (WAGNO.EQ.0) THEN
         QP=.FALSE.
         SEED1=20000*INT(SECNDS(0.0))+1
      ENDIF
C
C WAGER HEADER
C
      CALL MOVBYT(DB1DAT,1,MESBUF1,1,MXLEN)                             
C
      MESBUF1(7) = (MESBUF1(7).AND.'00'X) .OR. GIND*16
      NDRWS=1
      NBRDS=1
      I4TMP=ISHFT(NDRWS,4)			!EQUIVALENCED TO NDRWSHFT !!!
      MESBUF1(8)= NDRWSHFT .OR. NBRDS
C
      OPTFLAGS=0
      IF(STRREV(GIND).NE.0) THEN
         OPTFLAGS=IOR(OPTFLAGS,'8000'X)       !Sup.double '8000', no joker
         REVNUM=STRREV(GIND)
         CALL MOVBYT(BREVNUM,1,MESBUF1,11,2)  !Sup.double game rev
      ENDIF
C
C OPTIONAL DATA
C
      IND=13
      IF(QP) THEN                             !quick pick flags
         OPTFLAGS=IOR(OPTFLAGS,'0200'X)
         MESBUF1(IND)='FF'X
         MESBUF1(IND+1)='F0'X
         IND=IND+2
      ENDIF
C
      IF(SYSBET) THEN                         !system bet
        MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'01'X
        OPTFLAGS=IOR(OPTFLAGS,'0100'X)
C
10      CONTINUE
        TYPE*
        CALL INPNUM('System bet. Enter number of winners',
     *              FIRSTNUM,1,MAXSTRRW,EXT)
        IF(EXT.LT.0) RETURN
        CALL INPNUM('System bet. Enter number of second bests',
     *              SECONDNUM,1,MAXSTRRW,EXT)
        IF(EXT.LT.0) RETURN
        CALL INPNUM('System bet. Enter number of third bests',
     *              THIRDNUM,1,MAXSTRRW,EXT)
        IF(EXT.LT.0) RETURN
        IF(FIRSTNUM+SECONDNUM+THIRDNUM.GT.TWSTBMAX) THEN
           TYPE*,'Number of winners + second + third bests should be <= ',
     *           TWSTBMAX
           GOTO 10
        ENDIF
C
        I4TMP=FIRSTNUM*SECONDNUM*THIRDNUM
        REMIND=IND                        !remember ind for later change
        MESBUF1(IND)=I1TMP(2)
        MESBUF1(IND+1)=I1TMP(1)
        IND=IND+2
      ENDIF
C
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
C
      MESBUF1(10)=BOPTFLAGS(1)           !option flags now set
      MESBUF1(9)=BOPTFLAGS(2)
C
      MESBUF1(IND)='00'X                !coupon id   
      IND=IND+1
C
C
      I2TMP=STRDRW(GIND)                !draw
      MESBUF1(IND)= I1TMP(2)
      MESBUF1(IND+1)=I1TMP(1)
      IND=IND+2
C
      RLEN=IND-1                   ! # bytes displayed in first line
C
C SIMPLE BET
C
      IF(.NOT.SYSBET) THEN
         MESBUF1(IND)=1            ! # of first place competitors
	 IND=IND+1
         MESBUF1(IND)=1            ! # of second place competitors
	 IND=IND+1
         MESBUF1(IND)=1            ! # of third place competitors
	 IND=IND+1
C
         DO I=1,NBRDS          
            RAND = MOD(INT(OPENCNT*RAN(SEED1)),MAXSTRRW)+1     !winner row #
            REMRAND(I) = RAND
            FIRSTLIST(I) = OPENROWS(RAND)
            MESBUF1(IND)=FIRSTLIST(I)
	    IND=IND+1
         ENDDO
C
         DO I=1,NBRDS          
5           CONTINUE
            RAND = MOD(INT(OPENCNT*RAN(SEED1)),MAXSTRRW)+1   !second best row #
            REMRAND2(I) = RAND
            IF(RAND.EQ.REMRAND(I)) GOTO 5
            SECONDLIST(I) = OPENROWS(RAND)
            MESBUF1(IND)=SECONDLIST(I)
	    IND=IND+1
         ENDDO

         DO I=1,NBRDS          
6           CONTINUE
            RAND = MOD(INT(OPENCNT*RAN(SEED1)),MAXSTRRW)+1   !third best row #
            IF(RAND.EQ.REMRAND(I).OR.
     *         RAND.EQ.REMRAND2(I)) GOTO 6
            THIRDLIST(I) = OPENROWS(RAND)
            MESBUF1(IND)=THIRDLIST(I)
	    IND=IND+1
         ENDDO
C
         DO I=1,NBRDS          
 	    RAND = INT(8*RAN(SEED1))+1           !amount
            AMOUNT(I) = AMTLIST(RAND)
            I4TMP=AMOUNT(I)*(DOLL_BASE/DYN_BETUNIT)
            MESBUF1(IND+0) = I1TMP(4)
            MESBUF1(IND+1) = I1TMP(3)
            MESBUF1(IND+2) = I1TMP(2)
            MESBUF1(IND+3) = I1TMP(1)
            IND=IND+4
         ENDDO
      ENDIF
C
C SYSTEM BET
C
      SYSN = 1
      IF(SYSBET) THEN
         MESBUF1(IND)=FIRSTNUM               ! # of first place competitors
	 IND=IND+1
         MESBUF1(IND)=SECONDNUM              ! # of second place competitors
	 IND=IND+1
         MESBUF1(IND)=THIRDNUM               ! # of THIRD place competitors
	 IND=IND+1
C
         DO I=1,FIRSTNUM          
20          CONTINUE
            RAND = MOD(INT(OPENCNT*RAN(SEED1)),MAXTRPRW)+1     !winner row #
            IF(I.EQ.1) REMRAND(I)=RAND
            DO K=1,I-1
               IF(RAND.EQ.REMRAND(K)) THEN
                  GOTO 20
               ELSE
                  REMRAND(I)=RAND
               ENDIF
            ENDDO
            FIRSTLIST(I) = OPENROWS(RAND)
            MESBUF1(IND)=FIRSTLIST(I)
	    IND=IND+1
         ENDDO
C
         DO I=1,SECONDNUM       
30          CONTINUE
            RAND = MOD(INT(OPENCNT*RAN(SEED1)),MAXTRPRW)+1  !second best row #
            IF(I.EQ.1) REMRAND(I)=RAND
            DO K=1,I-1
               IF(RAND.EQ.REMRAND(K)) THEN
                  GOTO 30
               ELSE
                  REMRAND(I)=RAND
               ENDIF
            ENDDO
            SECONDLIST(I) = OPENROWS(RAND)
            MESBUF1(IND)=SECONDLIST(I)
	    IND=IND+1
         ENDDO
         DO I=1,THIRDNUM       
32          CONTINUE
            RAND = MOD(INT(OPENCNT*RAN(SEED1)),MAXTRPRW)+1    !third best row #
            IF(I.EQ.1) REMRAND(I)=RAND
            DO K=1,I-1
               IF(RAND.EQ.REMRAND(K)) THEN
                  GOTO 32
               ELSE
                  REMRAND(I)=RAND
               ENDIF
            ENDDO
            THIRDLIST(I) = OPENROWS(RAND)
            MESBUF1(IND)=THIRDLIST(I)
	    IND=IND+1
         ENDDO
C
         I4TMP=0
         DO I=1,FIRSTNUM          
            DO J=1,SECONDNUM
	       DO K=1,THIRDNUM       
               IF(FIRSTLIST(I).NE.SECONDLIST(J).AND.
     *            FIRSTLIST(I).NE.THIRDLIST(K).AND.
     *            SECONDLIST(J).NE.THIRDLIST(K)) I4TMP = I4TMP + 1
               ENDDO
            ENDDO
         ENDDO 
	 SYSN = I4TMP
         MESBUF1(REMIND)=I1TMP(2)
         MESBUF1(REMIND+1)=I1TMP(1)
C
 	 IF(I4TMP.GE.10) THEN
            RAND = INT(4*RAN(SEED1))+1           !amount should be big enough
            AMOUNT(1) = AMTLIST(RAND+4)             !very rough estimation 
         ELSE
            RAND = INT(8*RAN(SEED1))+1 
            AMOUNT(1) = AMTLIST(RAND)     
         ENDIF
         I4TMP=AMOUNT(1)*(DOLL_BASE/DYN_BETUNIT)*SYSN
         MESBUF1(IND+0) = I1TMP(4)
         MESBUF1(IND+1) = I1TMP(3)
         MESBUF1(IND+2) = I1TMP(2)
         MESBUF1(IND+3) = I1TMP(1)
         IND=IND+4
        
      ENDIF
C
	TNBRDS=TNBRDS+NBRDS
        MESLEN=IND-1
C
	MESBUF1(1) = (MESBUF1(1).AND.'F0'X) .OR. SEQNO(TER)
	CALL SETCHKSUM(MESBUF1,MESLEN)
C
	IF(MANCHG) CALL PERTUR(EXT)
	IF(EXT.LT.0) RETURN
C
	IF(NODISP) RETURN
C
C WINTIP WAGER DISPLAY
C
        TYPE 920, QP,GTNAMES(GTYP),GIND,NDRWS,NBRDS,TER,
     *            (MESBUF1(I),I=1,RLEN)
        TYPE 925, (MESBUF1(I),I=RLEN+1,MESLEN)
        IF(SYSBET) THEN
           TYPE 930,(FIRSTLIST(J),J=1,FIRSTNUM)
           TYPE 935,(SECONDLIST(J),J=1,SECONDNUM)
           TYPE 936,(THIRDLIST(J),J=1,THIRDNUM)
           TYPE 940,AMOUNT(1)
        ELSE
           FIRSTNUM=NBRDS
           SECONDNUM=NBRDS
           THIRDNUM=NBRDS
           TYPE 930,(FIRSTLIST(J),J=1,FIRSTNUM)
           TYPE 935,(SECONDLIST(J),J=1,SECONDNUM)
           TYPE 936,(THIRDLIST(J),J=1,THIRDNUM)
           TYPE 940,(AMOUNT(J),J=1,NBRDS)
        ENDIF
C
	RETURN
C
920     FORMAT(' QP = ',L1,2X,A8,' GIND =',I2,'     NDRWS =',I2,
     *         '     NBRDS=',I2,'     TER=',I4,/ '     mes: ',
     *         <RLEN>Z3.2)
925     FORMAT('     mes: ',<MESLEN-RLEN>Z3.2)
930     FORMAT(1X'winner  :',<FIRSTNUM>I7)
935     FORMAT(1X'2nd best:',<SECONDNUM>I7)
936     FORMAT(1X'3rd best:',<THIRDNUM>I7)
940     FORMAT(1X'crsp.amt:',<NBRDS>I7)
C
        END
