C TRPSIM.FOR
C
C V03 15-JUN-2000 OXK CLEANUP W/ WARNINGS=ALL
C V02 31-MAY-1999 UXN TWTTBMAX added.
C V01 XX-XXX-XXXX RXK Initial release.
C
C PREPARE A SIMULATED TODAY'S TRIPLE WAGER
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
        SUBROUTINE TRPSIM(WAGNO,NBRDS,QP,PERQP,
     *                    DRWMAX,BRDMAX,NDRWS,EXT)
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
        INCLUDE 'INCLIB:TRPCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER*4 PERQP
        INTEGER*4 IND, EXT
        INTEGER*4 DRWMAX, BRDMAX, NDRWS
        INTEGER*4 RAND
        BYTE      NBRDS, BTMP1, NDRWSHFT
        INTEGER*4 I4TMP
        EQUIVALENCE (I4TMP,NDRWSHFT)
        INTEGER*2 I2TMP
        EQUIVALENCE (I2TMP,BTMP1)
C
        LOGICAL   QP
        INTEGER*4 SEED1
        INTEGER*4 I, J, K
        INTEGER*4 WAGNO
C
        INTEGER*4 RLEN, SYSNUM
        INTEGER*4 EVENTNUM(3)
        INTEGER*4 EVENTLIST(MAXTRPRW,3)
        INTEGER*4 HOLDER(MAXTRPRW)
        INTEGER*2 OPTFLAGS,REVNUM
        BYTE      BOPTFLAGS(2),BREVNUM(2)
        EQUIVALENCE (OPTFLAGS,BOPTFLAGS)
        EQUIVALENCE (REVNUM,BREVNUM)
        BYTE      I1TMP(4)
        EQUIVALENCE (I4TMP,I1TMP(1))
C
        INTEGER*4 AMOUNT
        INTEGER*4 MULTLIST(20)
        DATA MULTLIST /1,2,3,5,1,2,3,5,1,2,3,5,10,20,30,50,100,200,300,500/
        INTEGER*4 OPENCNT(3)
        INTEGER*4 NUMEVE
        INTEGER*4 OPENROWS(MAXTRPRW,3)
C
        BYTE TR1DAT(MXLEN)
C
CC ------------------------------------------------------------
C
C TRIPLE WAGER, WITHOUT BOARDS (TO BE GENERATED RANDOMLY)
C
        DATA (TR1DAT(I),I=1,MXLEN)/
     *                   Z20,   ! 1  control and sequence
     *                   Z00,   ! 2  type
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z80,   ! 5  statistics (simulator transaction)
     *                   Z0F,   ! 6  gametype
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
          NUMEVE=0
          DO K=1,3
            IF(TRPEST(K,GIND).EQ.GAMOPN) NUMEVE=NUMEVE+1
            OPENCNT(K)=0
            J=0
            DO I=1,MAXTRPRW            !create a list of open rows
              IF(TRPSTA(I,K,GIND).EQ.GAMOPN) THEN
                J=J+1
                OPENROWS(J,K)=I
                OPENCNT(K)=OPENCNT(K)+1
              ENDIF
            ENDDO
          ENDDO
C
        IF (WAGNO.EQ.0) THEN
          QP=.FALSE.
          SEED1=20000*INT(SECNDS(0.0))+1
        ENDIF
C
C WAGER HEADER
C
        CALL MOVBYT(TR1DAT,1,MESBUF1,1,MXLEN)                             
C
        MESBUF1(7) = (MESBUF1(7).AND.'00'X) .OR. GIND*16
        NDRWS=1
        NBRDS=1
        I4TMP=ISHFT(NDRWS,4)                    !EQUIVALENCED TO NDRWSHFT !!!
        MESBUF1(8)=(MESBUF1(8).AND.'00'X) .OR. NDRWSHFT .OR. NBRDS
C
      OPTFLAGS=0
      IF(TRPREV(GIND).NE.0) THEN
         OPTFLAGS=IOR(OPTFLAGS,'8000'X)       !Today's Triple '8000', no joker
         REVNUM=TRPREV(GIND)
         CALL MOVBYT(BREVNUM,1,MESBUF1,11,2)  !Today's Triple game rev
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
      IF(SYSBET.AND.MANCHG) THEN              !system bet
10      CONTINUE
        TYPE*
        CALL INPNUM('System bet. Enter number of winners of Event A ',
     *              EVENTNUM(1),1,MAXTRPRW,EXT)
        IF(EXT.LT.0) RETURN
        IF(NUMEVE.EQ.1) GOTO 19
        CALL INPNUM('System bet. Enter number of winners of Event B ',
     *              EVENTNUM(2),0,MAXTRPRW,EXT)
        IF(EXT.LT.0) RETURN
        CALL INPNUM('System bet. Enter number of winners of Event C ',
     *              EVENTNUM(3),0,MAXTRPRW,EXT)
        IF(EXT.LT.0) RETURN
        IF(NUMEVE.EQ.2) GOTO 19
        IF(EVENTNUM(1)+EVENTNUM(2)+EVENTNUM(3).GT.TWTTBMAX) THEN
           TYPE*,'Number of A + B + C winners should be <= ',TWTTBMAX
           GOTO 10
        ENDIF
        DO I=1,3
           IF(EVENTNUM(I).GT.OPENCNT(I).AND.OPENCNT(I).NE.0) THEN
              TYPE*,'Number of open rows too small for this system'
              GOTO 10
           ENDIF
        ENDDO
      ELSEIF(SYSBET) THEN
16       CONTINUE
         RAND = INT(OPENCNT(1)*RAN(SEED1))+1
         IF(RAND.LT.1 .OR. RAND.GT.OPENCNT(1)) GOTO 16
         EVENTNUM(1)=RAND
         IF(NUMEVE.EQ.1) GOTO 19
         K=MIN0(OPENCNT(2),(TWTTBMAX-EVENTNUM(1)-1))
17       CONTINUE
         RAND=INT(K*RAN(SEED1))+1
         IF(RAND.LT.1 .OR. RAND.GT.OPENCNT(2)) GOTO 17
         EVENTNUM(2)=RAND
         IF(NUMEVE.EQ.2) GOTO 19
         K=MIN0(OPENCNT(3),(TWTTBMAX-EVENTNUM(1)-EVENTNUM(2)))
18       CONTINUE
         RAND=INT(K*RAN(SEED1))+1
         IF(RAND.LT.1 .OR. RAND.GT.OPENCNT(3)) GOTO 18
         EVENTNUM(3)=RAND
      ELSE
        DO I=1,NUMEVE
           EVENTNUM(I)=1
        ENDDO
      ENDIF
C
19    CONTINUE
      SYSNUM = 1
      IF(SYSBET) THEN                         ! full system bet
        MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'01'X
        OPTFLAGS=IOR(OPTFLAGS,'0100'X)
        I4TMP=EVENTNUM(1)
        IF(NUMEVE.GT.1) I4TMP=I4TMP*EVENTNUM(2)
        IF(NUMEVE.GT.2) I4TMP=I4TMP*EVENTNUM(3)
        MESBUF1(IND)=I1TMP(2)
        MESBUF1(IND+1)=I1TMP(1)
        IND=IND+2
        SYSNUM=I4TMP
      ENDIF
C
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
      I4TMP=TRPDRW(GIND)                !draw
      MESBUF1(IND)= I1TMP(2)
      MESBUF1(IND+1)=I1TMP(1)
      IND=IND+2
C
      RLEN=IND-1                   ! # bytes displayed in first line
C
C BET
C
      MESBUF1(IND)= EVENTNUM(1)
      IND=IND+1
      MESBUF1(IND)= EVENTNUM(2)
      IND=IND+1
      MESBUF1(IND)= EVENTNUM(3)
      IND=IND+1

      IF(.NOT.MANCHG) THEN
         DO K=1,NUMEVE
            CALL FASTSET(-1,HOLDER,MAXTRPRW)
            DO I=1,EVENTNUM(K)
               CALL GENTRP(SEED1,OPENCNT(K),RAND,HOLDER)
               EVENTLIST(I,K) = RAND
            ENDDO
            CALL BUBSORT(EVENTLIST(1,K),EVENTNUM(K))
            DO I=1,EVENTNUM(K)
               MESBUF1(IND)=EVENTLIST(I,K)
               IND=IND+1
            ENDDO
         ENDDO
      ELSE
         DO I=1,EVENTNUM(1)
            CALL INPNUM('Enter winner of Event A ',
     *                   EVENTLIST(I,1),1,OPENCNT(1),EXT)
            MESBUF1(IND)=EVENTLIST(I,1)
            IND=IND+1
         ENDDO
         IF(TRPEST(2,GIND).NE.GAMOPN) GOTO 20
         DO I=1,EVENTNUM(2)
            CALL INPNUM('Enter winner of Event B ',
     *                   EVENTLIST(I,2),1,OPENCNT(2),EXT)
            MESBUF1(IND)=EVENTLIST(I,2)
            IND=IND+1
         ENDDO
         IF(TRPEST(3,GIND).NE.GAMOPN) GOTO 20
         DO I=1,EVENTNUM(3)
            CALL INPNUM('Enter winner of Event C ',
     *                   EVENTLIST(I,3),1,OPENCNT(3),EXT)
            MESBUF1(IND)=EVENTLIST(I,3)
            IND=IND+1
         ENDDO
      ENDIF     

20    CONTINUE
      RAND = INT(20*RAN(SEED1))+1
      AMOUNT = MULTLIST(RAND)
      IF(SYSNUM*AMOUNT.GT.25000) AMOUNT=1

      I4TMP=AMOUNT*(DOLL_BASE/DYN_BETUNIT)*SYSNUM ! AMOUNT BET
      MESBUF1(IND+0)=I1TMP(4)
      MESBUF1(IND+1)=I1TMP(3)
      MESBUF1(IND+2)=I1TMP(2)
      MESBUF1(IND+3)=I1TMP(1)
      IND=IND+4          
C
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
C TODAY'S COUPLE WAGER DISPLAY
C
        TYPE 920, QP,GTNAMES(GTYP),GIND,NDRWS,NBRDS,TER,
     *            (MESBUF1(I),I=1,RLEN)
        TYPE 925, (MESBUF1(I),I=RLEN+1,MESLEN)
        TYPE 930,(EVENTLIST(J,1), J=1,EVENTNUM(1))
        TYPE 935,(EVENTLIST(J,2), J=1,EVENTNUM(2))
        TYPE 936,(EVENTLIST(J,3), J=1,EVENTNUM(3))
        TYPE 940, AMOUNT
C
        RETURN
C
920     FORMAT(' QP = ',L1,2X,A8,' GIND =',I2,'     NDRWS =',I2,
     *         '     NBRDS=',I2,'     TER=',I4,/ '     mes: ',
     *         <RLEN>Z3.2)
925     FORMAT('     mes: ',<MESLEN-RLEN>Z3.2)
930     FORMAT(1X'Event A winner  :',<EVENTNUM(1)>I5)
935     FORMAT(1X'Event B winner  :',<EVENTNUM(2)>I5)
936     FORMAT(1X'Event C winner  :',<EVENTNUM(3)>I5)
940     FORMAT(1X'Multiplier      :',I3)
C
        END
        SUBROUTINE GENTRP(SEED1,OPENCNT,RAND,HOLDER)
        INTEGER*4  SEED1
        INTEGER*4  OPENCNT
        INTEGER*4  HOLDER(*)
        INTEGER*4  RAND

10      CONTINUE
        RAND = INT(OPENCNT*RAN(SEED1))+1
        IF(HOLDER(RAND).LT.0) THEN
           HOLDER(RAND)=RAND
           RETURN
        ELSE
           GOTO 10
        ENDIF
        END

