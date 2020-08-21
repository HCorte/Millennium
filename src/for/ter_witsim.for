C WITSIM.FOR
C
C PREPARE A SIMULATED WINNERS TIP WAGER
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
	SUBROUTINE WITSIM(WAGNO,NBRDS,QP,PERQP,
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
        INCLUDE 'INCLIB:WITCOM.DEF'
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
 	INTEGER*4 I, J
        INTEGER*4 WAGNO, TNBRDS
C
        INTEGER*4 RLEN
        INTEGER*2 OPTFLAGS,REVNUM
        BYTE      BOPTFLAGS(2),BREVNUM(2)
        EQUIVALENCE (OPTFLAGS,BOPTFLAGS)
        EQUIVALENCE (REVNUM,BREVNUM)
        BYTE      I1TMP(4)
        EQUIVALENCE (I4TMP,I1TMP(1))
C
        INTEGER*4 AMOUNT(10)
        INTEGER*4 AMTLIST(8)
        DATA AMTLIST /5,10,20,50,100,200,300,500/
        INTEGER*4 ROWS(10)
        INTEGER*4 OPENCNT,OPENROWS(MAXWRW)
        INTEGER*4 PREVGIND/0/
C
        BYTE WI1DAT(MXLEN)
C
CC ------------------------------------------------------------
	DATA (WI1DAT(I),I=1,MXLEN)/
C
C WINTIP WAGER, WITHOUT BOARDS (TO BE GENERATED RANDOMLY)
C
     *                   Z20,   ! 1  control and sequence
     *                   Z00,   ! 2  type 
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z80,   ! 5  statistics (simulator transaction)
     *                   Z07,   ! 6  gametype
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
        IF(GIND.NE.PREVGIND) THEN
          J=0
          DO I=1,MAXWRW                     !create a list of open rows
             IF(WITSTA(I,GIND).EQ.GAMOPN) THEN
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
	CALL MOVBYT(WI1DAT,1,MESBUF1,1,MXLEN)                             
C
        MESBUF1(7) = (MESBUF1(7).AND.'00'X) .OR. GIND*16
        NDRWS=1
        NBRDS=MAX(GMIN,INT(BRDMAX*RAN(SEED1))+1)
	I4TMP=ISHFT(NDRWS,4)			!EQUIVALENCED TO NDRWSHFT !!!
        MESBUF1(8)=(MESBUF1(8).AND.'00'X) .OR. NDRWSHFT .OR. NBRDS
C
      OPTFLAGS=0
      IF(WITREV(GIND).NE.0) THEN
         OPTFLAGS=IOR(OPTFLAGS,'8000'X)       !voittaja '8000', no joker
         REVNUM=WITREV(GIND)
         CALL MOVBYT(BREVNUM,1,MESBUF1,11,2)  !voittaja game rev
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
      MESBUF1(IND)='00'X            !coupon id   ??? kas vastab manual entry -le
      IND=IND+1
C
      MESBUF1(10)=BOPTFLAGS(1)                 !option flags now set
      MESBUF1(9)=BOPTFLAGS(2)
C
      I2TMP=WITDRW(GIND)                !draw
      MESBUF1(IND)= I1TMP(2)
      MESBUF1(IND+1)=I1TMP(1)
      IND=IND+2
C
      RLEN=IND-1                   ! # bytes displayed in first line
C
C SIMPLE BET
C
         DO I=1,NBRDS          
           RAND = INT(OPENCNT*RAN(SEED1))+1     !row #
           ROWS(I) = OPENROWS(RAND)
           MESBUF1(IND)=ROWS(I)
	   IND=IND+1
C
 	   RAND = INT(8*RAN(SEED1))+1           !amount
           AMOUNT(I) = AMTLIST(RAND)
           I4TMP=AMOUNT(I)*(DOLL_BASE/DYN_BETUNIT)
           MESBUF1(IND+0) = I1TMP(4)
           MESBUF1(IND+1) = I1TMP(3)
           MESBUF1(IND+2) = I1TMP(2)
           MESBUF1(IND+3) = I1TMP(1)
           IND=IND+4
           I2TMP=0
         ENDDO
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
        TYPE 930,(ROWS(J),J=1,NBRDS)
        TYPE 935,(AMOUNT(J),J=1,NBRDS)
C
	RETURN
C
920     FORMAT(' QP = ',L1,2X,A8,' GIND =',I2,'     NDRWS =',I2,
     *         '     NBRDS=',I2,'     TER=',I4,/ '     mes: ',
     *         <RLEN>Z3.2)
925     FORMAT('     mes: ',<MESLEN-RLEN>Z3.2)
930     FORMAT(1X'row num :',<NBRDS>I7)
935     FORMAT(1X'crsp.amt:',<NBRDS>I7)
C
        END
