C SSCSIM.FOR
C
C PREPARE A SIMULATED SUPERSCORE WAGER
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
        SUBROUTINE SSCSIM(WAGNO,NBRDS,QP,PERQP,
     *                    DRWMAX,BRDMAX,NDRWS,EXT)
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
        INCLUDE 'INCLIB:SSCCOM.DEF'
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
        INTEGER*4 I, K
        INTEGER*4 WAGNO
C
        INTEGER*4 RLEN
        INTEGER*2 OPTFLAGS,REVNUM
        BYTE      BOPTFLAGS(2),BREVNUM(2)
        EQUIVALENCE (OPTFLAGS,BOPTFLAGS)
        EQUIVALENCE (REVNUM,BREVNUM)
        BYTE      I1TMP(4)
        EQUIVALENCE (I4TMP,I1TMP(1))
C
        INTEGER*4 AMOUNT
        INTEGER*4 AMTLIST(20)
        DATA AMTLIST /1,2,3,5,1,2,3,5,1,2,3,5,10,20,30,50,100,200,300,500/
        INTEGER*4 HOLDER(15)
        INTEGER*4 HOMENUM(3),AWAYNUM(3)
        INTEGER*4 HOMESSC(9,3),AWAYSSC(9,3)
        INTEGER*4 NUMEVE,SYSN
C
        BYTE SS1DAT(MXLEN)
C
CC ------------------------------------------------------------
        DATA (SS1DAT(I),I=1,MXLEN)/
C
C SUPERSCORE WAGER, WITHOUT BOARDS (TO BE GENERATED RANDOMLY)
C
     *                   Z20,   ! 1  control and sequence
     *                   Z00,   ! 2  type
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z80,   ! 5  statistics (simulator transaction)
     *                   Z0B,   ! 6  gametype
     *                   Z00,   ! 7  game index and system
     *                   Z00,   ! 8  duration and # of boards
     *                   Z00,   ! 9  option flags
     *                   Z00,   !10  option flags
     *                38*Z00/   !    option data
C
C ------------------------------------------------------------
C
C
C INITIALIZE DATA FOR THE MAIN LOOP
C
C
        IF (WAGNO.EQ.0) THEN
          QP=.FALSE.
          SEED1=20000*INT(SECNDS(0.0))+1
        ENDIF

        NUMEVE=0
        DO K=1,3
	    IF(SSCEST(K,GIND).EQ.GAMOPN) NUMEVE = NUMEVE+1
            HOMENUM(K)=0
            AWAYNUM(K)=0
        ENDDO
C
C WAGER HEADER
C
        CALL MOVBYT(SS1DAT,1,MESBUF1,1,MXLEN)                             
C
        MESBUF1(7) = (MESBUF1(7).AND.'00'X) .OR. GIND*16
        NBRDS=INT(BRDMAX*RAN(SEED1))+1
        NDRWS=1
        NBRDS=1
        I4TMP=ISHFT(NDRWS,4)                    !EQUIVALENCED TO NDRWSHFT !!!
        MESBUF1(8)=(MESBUF1(8).AND.'00'X) .OR. NDRWSHFT .OR. NBRDS
C
      OPTFLAGS=0
      IF(SSCREV(GIND).NE.0) THEN
         OPTFLAGS=IOR(OPTFLAGS,'8000'X)       !moniveto '8000', no joker
         REVNUM=SSCREV(GIND)
         CALL MOVBYT(BREVNUM,1,MESBUF1,11,2)  !moniveto game rev
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
C
      IF(SYSBET) THEN
        MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'01'X   
        OPTFLAGS=IOR(OPTFLAGS,'0100'X)

10      CONTINUE
        DO K=1,NUMEVE
           TYPE*,'System bet. Set',K 
           CALL INPNUM('Enter number of home team scores',
     *                  HOMENUM(K),1,9,EXT)
           IF(EXT.LT.0) RETURN
           CALL INPNUM('Enter number of away team scores',
     *                  AWAYNUM(K),1,9,EXT)
           IF(EXT.LT.0) RETURN
           IF(HOMENUM(K)+AWAYNUM(K).GT.10) THEN
              TYPE*,
     *        'Number of Home scores + Away scores should be <= 10'
              GOTO 10
           ENDIF
         ENDDO
      ELSE
         DO K=1,NUMEVE
            HOMENUM(K)=1
            AWAYNUM(K)=1
         ENDDO
      ENDIF
C
      SYSN = 1
      IF(SYSBET) THEN
         I4TMP=1
         DO K=1,NUMEVE       
            I4TMP=I4TMP*HOMENUM(K)*AWAYNUM(K)
         ENDDO
	 SYSN = I4TMP
         IF(I4TMP.GT.P(MAXSSN)) TYPE*,'System over limit',I4TMP
         MESBUF1(IND)=I1TMP(2)
         MESBUF1(IND+1)=I1TMP(1)
         IND=IND+2
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
      MESBUF1(10)=BOPTFLAGS(1)                 !option flags now set
      MESBUF1(9)=BOPTFLAGS(2)
C
      I1TMP(1)='FF'X                !couponid
      MESBUF1(IND)= I1TMP(1)
      IND=IND+1

      I4TMP=SSCDRW(GIND)                !draw
      MESBUF1(IND)= I1TMP(2)
      MESBUF1(IND+1)=I1TMP(1)
      IND=IND+2
C
      RLEN=IND-1                   ! # bytes displayed in first line
C
C BET
C
      DO K=1,3
        MESBUF1(IND)=HOMENUM(K)
        IND=IND+1
        MESBUF1(IND)=AWAYNUM(K)
        IND=IND+1
      ENDDO 
      MESBUF1(8)=(MESBUF1(8).AND.'F1'X)
C
      IF(MANCHG) THEN
         DO K=1,NUMEVE
            TYPE*,'SET',K
            CALL FASTSET(-1,HOLDER,15) 
            DO I=1,HOMENUM(K)                      !ask home scores
               CALL INPNUM('Enter home scores ',HOMESSC(I,K),0,15,EXT)
               IF(EXT.LT.0) RETURN
            ENDDO
            CALL BUBSORT(HOMESSC(1,K),HOMENUM(K))
            DO I=1,HOMENUM(K)  
               MESBUF1(IND)=HOMESSC(I,K)
               IND=IND+1
            ENDDO
C
            CALL FASTSET(-1,HOLDER,15) 
            DO I=1,AWAYNUM(K)                       !ask away scores
               CALL INPNUM('Enter away scores ',AWAYSSC(I,K),0,15,EXT)
               IF(EXT.LT.0) RETURN
            ENDDO
            CALL BUBSORT(AWAYSSC(1,K),AWAYNUM(K))
            DO I=1,AWAYNUM(K) 
               MESBUF1(IND)=AWAYSSC(I,K)
               IND=IND+1
            ENDDO
         ENDDO
      ELSE
         DO K=1,NUMEVE
            CALL FASTSET(-1,HOLDER,15) 
            DO I=1,HOMENUM(K)                      !generate home scores
               CALL GENSSC(SEED1,RAND,HOLDER)
               HOMESSC(I,K)=RAND-1
            ENDDO
            CALL BUBSORT(HOMESSC(1,K),HOMENUM(K))
            DO I=1,HOMENUM(K)  
               MESBUF1(IND)=HOMESSC(I,K)
               IND=IND+1
            ENDDO
C
            CALL FASTSET(-1,HOLDER,15) 
            DO I=1,AWAYNUM(K)                       !generate away scores
               CALL GENSSC(SEED1,RAND,HOLDER)
               AWAYSSC(I,K)=RAND-1
            ENDDO
            CALL BUBSORT(AWAYSSC(1,K),AWAYNUM(K))
            DO I=1,AWAYNUM(K) 
               MESBUF1(IND)=AWAYSSC(I,K)
               IND=IND+1
            ENDDO
         ENDDO
      ENDIF
C
      RAND = INT(20*RAN(SEED1))+1           !amount
      AMOUNT = AMTLIST(RAND)
      I4TMP=AMOUNT*(DOLL_BASE/DYN_BETUNIT)*SYSN
      MESBUF1(IND+0)=I1TMP(4)
      MESBUF1(IND+1)=I1TMP(3)
      MESBUF1(IND+2)=I1TMP(2)
      MESBUF1(IND+3)=I1TMP(1)
      IND=IND+4
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
C SCORE WAGER DISPLAY
C
        TYPE 920, QP,GTNAMES(GTYP),GIND,NDRWS,NBRDS,TER,
     *            (MESBUF1(I),I=1,RLEN)
        TYPE 925, (MESBUF1(I),I=RLEN+1,MESLEN)

        DO K=1,NUMEVE
           TYPE 930,K,(HOMESSC(I,K),I=1,HOMENUM(K))
           TYPE 932,(AWAYSSC(I,K),I=1,AWAYNUM(K))
        ENDDO  
        TYPE 940,AMOUNT
C
        RETURN
C
920     FORMAT(' QP = ',L1,2X,A8,' GIND =',I2,'     NDRWS =',I2,
     *         '     NBRDS=',I2,'     TER=',I4,/ '     mes: ',
     *         <RLEN>Z3.2)
925     FORMAT('     mes: ',<MESLEN-RLEN>Z3.2)
930     FORMAT(2X'Set ',I1,' Home : ',<HOMENUM(K)>(1X,I2.2))
932     FORMAT(2X'      Away : ',<AWAYNUM(K)>(1X,I2.2))
940     FORMAT(2X'Multip :',I3)
C
        END
        SUBROUTINE GENSSC(SEED1,RAND,HOLDER)
        INTEGER*4  SEED1
        INTEGER*4  CNT1
        INTEGER*4  HOLDER(15)
        INTEGER*4  RAND

        CNT1=15
10      CONTINUE
        RAND = INT(CNT1*RAN(SEED1))+1
        IF(HOLDER(RAND).LT.0) THEN
           HOLDER(RAND)=RAND
           RETURN
        ELSE
           GOTO 10
        ENDIF
        END

