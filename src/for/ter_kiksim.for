C KIKSIM.FOR
C
C PREPARE A SIMULATED JOKER WAGER
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
C=======OPTIONS        /CHECK=NOOVERFLOW
        SUBROUTINE KIKSIM(WAGNO,NBRDS,QP,PERQP,
     *                          DRWMAX,BRDMAX,NDRWS,EXT)
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER*4 PERQP
        INTEGER*4 IND, EXT
        INTEGER*4 DRWMAX, BRDMAX, NDRWS
        INTEGER*4 RAND
        BYTE      NBRDS, NDRWSHFT
        INTEGER*4 I4TMP
        EQUIVALENCE (I4TMP,NDRWSHFT)
        INTEGER*2 I2TMP(2)
C
        LOGICAL   QP
        INTEGER*4 SEED1
        INTEGER*4 I,ST
        INTEGER*4 WAGNO, TNBRDS
        INTEGER*4 BOARDS(10)
        BYTE      BBOARDS(10)
C
        INTEGER*4 RLEN
        INTEGER*2 OPTFLAGS,REVNUM
        BYTE      BOPTFLAGS(2),BREVNUM(2)
        EQUIVALENCE (OPTFLAGS,BOPTFLAGS)
        EQUIVALENCE (REVNUM,BREVNUM)
        BYTE      I1TMP(4)
        EQUIVALENCE (I4TMP,I1TMP(1))
        EQUIVALENCE (I4TMP,I2TMP(1))
C
        INTEGER*4 MULTI, LIMIT, OCTDIG, JOK_NUM, JOK_DRW
        INTEGER*4 WEEKS(10) /1,2,3,5,10,1,2,3,5,10/
C
        BYTE KK1DAT(MXLEN)
C
CC ------------------------------------------------------------
        DATA (KK1DAT(I),I=1,MXLEN)/
C
C SPORTS WAGER, WITHOUT BOARDS (TO BE GENERATED RANDOMLY)
C
     *                   Z20,   ! 1  control and sequence
     *                   Z00,   ! 2  type 
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z80,   ! 5  statistics (simulator transaction)
     *                   Z04,   ! 6  gametype
     *                   Z00,   ! 7  game index and system
     *                   Z00,   ! 8  duration and # of boards
     *                   Z00,   ! 9  option flags
     *                   Z00,   !10  option flags
     *                38*Z00/   !    option data
C
C ------------------------------------------------------------
C
        DRWMAX=0 !AVOID UNUSED WARNING DURING COMPILATION
C
C INITIALIZE DATA FOR THE MAIN LOOP
C
        IF (WAGNO.EQ.0) THEN
          QP=PERQP.EQ.100
          SEED1=20000*INT(SECNDS(0.0))+1
          LIMIT = KIKMAX(GIND)
          OCTDIG = KIKOCT(GIND)
          JOK_DRW = MOD(KIKDRW(GIND),64)
        ENDIF
c
        IF(PERQP.LT.100.AND.PERQP.GT.0) THEN
          QP=PERQP.GE.INT(101*RAN(SEED1))
        ENDIF
C
C WAGER HEADER
C
        CALL MOVBYT(KK1DAT,1,MESBUF1,1,MXLEN)                             
C
        MESBUF1(7) = (MESBUF1(7).AND.'00'X) .OR. GIND*16
C
        IF(SYSBET) THEN
           NBRDS=1
        ELSE
           NBRDS=MAX(GMIN,INT(BRDMAX*RAN(SEED1))+1)
        ENDIF
c
        MULTI=WEEKS(MDRWMAX(MGNUM))
        RAND=INT(MULTI*RAN(SEED1))+1
        NDRWS=WEEKS(RAND)
        I4TMP=ISHFT(NDRWS,4)                        !EQUIVALENCED TO NDRWSHFT !!!
        MESBUF1(8)=(MESBUF1(8).AND.'00'X) .OR. NDRWSHFT .OR. NBRDS
C
        OPTFLAGS=0
        OPTFLAGS=IOR(OPTFLAGS,'8000'X)          !game '8000'
        OPTFLAGS=IOR(OPTFLAGS,'0080'X)          !participating in Joker1
C
        IF(KIKREV(1).NE.0) THEN
           REVNUM=KIKREV(1)
           CALL MOVBYT(BREVNUM,1,MESBUF1,11,2)  !joker game rev
        ENDIF

C
C OPTIONAL DATA
C
        IND=13
        IF(SYSBET) THEN
           OPTFLAGS=IOR(OPTFLAGS,'0100'X)
           MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'01'X   !full system
           I2TMP(1) = 6    !system size hardcoded
           MESBUF1(IND)=I1TMP(2)
           MESBUF1(IND+1)=I1TMP(1)
           IND=IND+2
        ENDIF

        IF(BANK) THEN                           !bank attributes
           OPTFLAGS=IOR(OPTFLAGS,'0008'X)
           I4TMP=BANKID
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
C        IF(SYSBET) THEN
C          OPTFLAGS=IOR(OPTFLAGS,'0004'X)
C          MESBUF1(IND) = GJOK         !direction (1,2 or 3)
C          IND=IND+1
C        ENDIF
C
        IF(SERIALEM) THEN                 !SERIAL NUMBER EM
           OPTFLAGS=IOR(OPTFLAGS,'0004'X)
           I4TMP=SERIALEM_NR
           I4TMP=ISHFT(I4TMP,8) + SERIALEM_CC
           MESBUF1(IND)=I1TMP(4)     
           MESBUF1(IND+1)=I1TMP(3)
           MESBUF1(IND+2)=I1TMP(2)
           MESBUF1(IND+3)=I1TMP(1)
           IND=IND+4
        ENDIF

        OPTFLAGS=IOR(OPTFLAGS,'0002'X)          !joker number present

        MESBUF1(10)=BOPTFLAGS(1)                 !option flags now set
        MESBUF1(9)=BOPTFLAGS(2)
C
        RLEN=IND-1                   !for display in first line
C
C SIMPLE BET DETAILS
C
        IF(.NOT.SYSBET) THEN
          DO I=1,NBRDS
             JOK_NUM = KIKSED(1,GIND)
             CALL RND64(JOK_NUM,JOK_DRW,1,LIMIT,OCTDIG)
             KIKSED(1,GIND) = KIKSED(1,GIND) + 1
             IF(KIKSED(1,GIND).GT.KIKMAX(GIND)) KIKSED(1,GIND) = 0
C TO BE ABLE TO SET JOKER NUMBER IF WE HAVE ONLY ONE WAGER
             IF(MTOTNR(MGNUM) .EQ. 1) THEN
                CALL PRMYESNO('Do you want set joker number? [Y/N]',ST)
                IF(ST.EQ.1) THEN
                  CALL INPNUM(' Enter joker number [1-7 digits]', JOK_NUM ,0,9999999,ST)
                ENDIF
             ENDIF
             I4TMP=IOR(JOK_NUM,ISHFT(GJOK,24)) !here direction not varied
             I4TMP=IOR(I4TMP,ISHFT(QP,26))
             BOARDS(I) = JOK_NUM
             MESBUF1(IND)=I1TMP(4)      
             MESBUF1(IND+1)=I1TMP(3)
             MESBUF1(IND+2)=I1TMP(2)
             MESBUF1(IND+3)=I1TMP(1)
             IND=IND+4
          ENDDO
        ENDIF
C
C SYSTEM BET DETAILS
C
        IF(SYSBET) THEN
          MESBUF1(IND+0)='40'X
          MESBUF1(IND+1)='40'X
          MESBUF1(IND+2)='60'X
          MESBUF1(IND+3)='10'X
          MESBUF1(IND+4)='08'X
          MESBUF1(IND+5)='04'X
          MESBUF1(IND+6)='02'X
          MESBUF1(IND+7)='01'X
          MESBUF1(IND+8)='01'X
          MESBUF1(IND+9)='00'X
          CALL MOVBYT(MESBUF1,IND,BBOARDS,1,10)
          IND=IND+10
        ENDIF
C
C
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
C JOKER WAGER DISPLAY
C
        TYPE 920, QP,GTNAMES(GTYP),GIND,NDRWS,NBRDS,TER,
     *                  (MESBUF1(I),I=1,RLEN)
C
        TYPE 925, (MESBUF1(I),I=RLEN+1,MESLEN)
        
        TYPE*,'Direction=',GJOK
        IF(SYSBET) THEN
          TYPE*,'Bitmaps:'
          DO I=1,10
             TYPE 940, BBOARDS(I)
          ENDDO 
        ELSE
           IF(NBRDS.LT.10) THEN
             TYPE 930,(BOARDS(I),I=1,NBRDS)
             TYPE 932,(BOARDS(I),I=1,NBRDS)
           ELSE
             TYPE 931,(BOARDS(I),I=1,NBRDS)
             TYPE 933,(BOARDS(I),I=1,NBRDS)
           ENDIF
        ENDIF 

        RETURN
C
920     FORMAT(' QP = ',L1,2X,A8,' GIND =',I2,'     NDRWS =',I2,
     *         '     NBRDS=',I2,'     TER=',I4,/ '     mes: ',
     *         <RLEN>Z3.2)
925     FORMAT('     mes: ',<MESLEN-RLEN>Z3.2)
930     FORMAT(1X,'KICKS:'<NBRDS>I8)
931     FORMAT(1X,'KICKS:'9(I8),/,7X,I8)
932     FORMAT(1X,<NBRDS>(1X,Z8.8))
933     FORMAT(1X,9(1X,Z8.8),/,Z8.8)
940     FORMAT(1X,Z8.8)
C
        END
C
C
