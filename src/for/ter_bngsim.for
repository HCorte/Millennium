C BNGSIM.FOR
C
C
C PREPARE A SIMULATED BINGO WAGER
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
	SUBROUTINE BNGSIM(WAGNO,EXT)
C
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
C
        INTEGER*4 EXT
	INTEGER*4 NDRWS
        INTEGER*4 NBRDS
C
        LOGICAL   QP/.FALSE./
        INTEGER*4 SEED1
 	INTEGER*4 I, IND,RLEN
        INTEGER*4 WAGNO, TNBRDS, QPFLAGS
        INTEGER*2 OPTFLAGS,REVNUM
        BYTE      BOPTFLAGS(2),BREVNUM(2)
        EQUIVALENCE (OPTFLAGS,BOPTFLAGS)
        EQUIVALENCE (REVNUM,BREVNUM)
        BYTE      I1TMP(4)
        INTEGER*4 I4TMP
        EQUIVALENCE (I4TMP,I1TMP(1))
C
C
        BYTE BN1DAT(MXLEN)
C
CC ------------------------------------------------------------
	DATA (BN1DAT(I),I=1,MXLEN)/
C
C BINGO WAGER
C
     *                   Z20,   ! 1  control and sequence
     *                   Z00,   ! 2  type
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z80,   ! 5  statistics (simulator transaction)
     *                   Z0C,   ! 6  gametype
     *                   Z00,   ! 7  game index and system
     *                   Z00,   ! 8  duration and # of boards
     *                   Z00,   ! 9  option flags
     *                   Z00,   !10  option flags
     *                38*Z00/   !    option data
C
C ------------------------------------------------------------
C
C
C
	IF (WAGNO.EQ.0) THEN	! INITIALIZE SEED ONLY ONCE
          SEED1=20000*INT(SECNDS(0.0))+1
	ENDIF
C
	CALL MOVBYT(BN1DAT,1,MESBUF1,1,MXLEN)                             
C
	QPFLAGS=0
	MESBUF1(7) = GIND*16
C
        NBRDS=1
        NDRWS=1
        MESBUF1(8) = NDRWS*16 + NBRDS

      OPTFLAGS=0
      IF(BNGREV(GIND).NE.0) THEN
         OPTFLAGS=IOR(OPTFLAGS,'8000'X)       !bingo '8000',no joker
         REVNUM=BNGREV(GIND)
         CALL MOVBYT(BREVNUM,1,MESBUF1,11,2)  !bingo game rev
      ENDIF
  
      IND=13
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
      MESBUF1(10)=BOPTFLAGS(1)                 !option flags now set
      MESBUF1(9)=BOPTFLAGS(2)
C
      RLEN=IND-1                   !for display in first line
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
C BINGO WAGER DISPLAY
C
        TYPE 920, QP,GTNAMES(GTYP),GIND,NDRWS,NBRDS,TER,
     *	          (MESBUF1(I),I=1,RLEN)
C
	RETURN
C
920     FORMAT(' QP = ',L1,2X,A8,' GIND =',I2,'     NDRWS =',I2,
     *	       '     NBRDS=',I2,'     TER=',I4,/ '     mes: ',<RLEN>Z3.2)
C
	END
