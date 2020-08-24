C SCRSIM.FOR
C
C PREPARE A SIMULATED SCORE WAGER
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
	SUBROUTINE SCRSIM(WAGNO,NBRDS,QP,PERQP,
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
        INCLUDE 'INCLIB:SCRCOM.DEF'
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
 	INTEGER*4 I
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
        INTEGER*4 HOLDER(11)
        INTEGER*4 HOMENUM,AWAYNUM
        INTEGER*4 HOMESCR(9),AWAYSCR(9)
C
        BYTE SC1DAT(MXLEN)
C
CC ------------------------------------------------------------
	DATA (SC1DAT(I),I=1,MXLEN)/
C
C SCORE WAGER, WITHOUT BOARDS (TO BE GENERATED RANDOMLY)
C
     *                   Z20,   ! 1  control and sequence
     *                   Z00,   ! 2  type
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z80,   ! 5  statistics (simulator transaction)
     *                   Z06,   ! 6  gametype
     *                   Z00,   ! 7  game index and system
     *                   Z00,   ! 8  duration and # of boards
     *                   Z00,   ! 9  option flags
     *                   Z00,   ! 10  option flags
     *                38*Z00/   !    option data
C
C ------------------------------------------------------------
C
C
C INITIALIZE DATA FOR THE MAIN LOOP
C
C
	IF(SEED1.LE.0) SEED1 = 20000*INT(SECNDS(0.0))+1
	IF (WAGNO.EQ.0) THEN
          QP=.FALSE.
	ENDIF
C
C WAGER HEADER
C
	CALL MOVBYT(SC1DAT,1,MESBUF1,1,MXLEN)                             
C
        MESBUF1(7) = (MESBUF1(7).AND.'00'X) .OR. GIND*16
        NBRDS=INT(BRDMAX*RAN(SEED1))+1
        NDRWS=1
	I4TMP=ISHFT(NDRWS,4)			!EQUIVALENCED TO NDRWSHFT !!!
        MESBUF1(8)=(MESBUF1(8).AND.'00'X) .OR. NDRWSHFT .OR. NBRDS
C
      OPTFLAGS=0
      IF(SCRREV(GIND).NE.0) THEN
         OPTFLAGS=IOR(OPTFLAGS,'8000'X)       !tulos '8000', no joker
         REVNUM=SCRREV(GIND)
         CALL MOVBYT(BREVNUM,1,MESBUF1,11,2)  !tulos game rev
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
        CALL INPNUM('System bet. Enter number of home team scores',
     *              HOMENUM,1,9,EXT)
        IF(EXT.LT.0) RETURN
        CALL INPNUM('System bet. Enter number of away team scores',
     *              AWAYNUM,1,9,EXT)
        IF(EXT.LT.0) RETURN
        IF(HOMENUM+AWAYNUM.GT.10) THEN
           TYPE*,'Number of Home scores + Away scores should be <= 10'
           GOTO 10
        ENDIF
C
        I4TMP=HOMENUM*AWAYNUM
        MESBUF1(IND)=I1TMP(2)
        MESBUF1(IND+1)=I1TMP(1)
        IND=IND+2
C
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
      MESBUF1(10)=BOPTFLAGS(1)                 !option flags now set
      MESBUF1(9)=BOPTFLAGS(2)
C
      I4TMP=SCRDRW(GIND)                !draw
      MESBUF1(IND)= I1TMP(2)
      MESBUF1(IND+1)=I1TMP(1)
      IND=IND+2
C
      RLEN=IND-1                   ! # bytes displayed in first line
C
C SIMPLE BET
C
      IF(.NOT.SYSBET) THEN
C
 	 HOMENUM=1
         AWAYNUM=1
C
         DO I=1,NBRDS
            MESBUF1(IND)=HOMENUM
            IND=IND+1
            MESBUF1(IND)=AWAYNUM
            IND=IND+1
CCCCC       HOMESCR(I)=INT(7*RAN(SEED1))       !home score
            HOMESCR(I)=INT(MAX_SCORE*RAN(SEED1))       !home score
            MESBUF1(IND)=HOMESCR(I)
	    IND=IND+1
C
CCCCC       AWAYSCR(I)=INT(6*RAN(SEED1))       !away score
            AWAYSCR(I)=INT(MAX_SCORE*RAN(SEED1))       !away score
            MESBUF1(IND)=AWAYSCR(I)
            IND=IND+1
C
            RAND = INT(8*RAN(SEED1))+1         !amount
            AMOUNT(I) = AMTLIST(RAND)
            I4TMP=AMOUNT(I)*(DOLL_BASE/DYN_BETUNIT)
            MESBUF1(IND+0) = I1TMP(4)
            MESBUF1(IND+1) = I1TMP(3)
            MESBUF1(IND+2) = I1TMP(2)
            MESBUF1(IND+3) = I1TMP(1)
            IND=IND+4
            I2TMP=0
         ENDDO
      ENDIF
C
C SYSTEM BET
C
      IF(SYSBET) THEN
C
        NBRDS=1
C
        MESBUF1(IND)=HOMENUM
        IND=IND+1
        MESBUF1(IND)=AWAYNUM
        IND=IND+1
        MESBUF1(8)=(MESBUF1(8).AND.'F0'X) .OR. MAX(HOMENUM,AWAYNUM)
C
        CALL FASTSET(-1,HOLDER,11) 
        DO I=1,HOMENUM                      !generate home scores
           HOMESCR(I)=INT(MAX_SCORE*RAN(SEED1))       !home score
        ENDDO
C        CALL BUBSORT(HOMESCR,HOMENUM)
        DO I=1,HOMENUM  
           MESBUF1(IND)=HOMESCR(I)
           IND=IND+1
        ENDDO
C
        CALL FASTSET(-1,HOLDER,11) 
        DO I=1,AWAYNUM                       !generate away scores
           AWAYSCR(I)=INT(MAX_SCORE*RAN(SEED1))       ! AWAY score
        ENDDO
C        CALL BUBSORT(AWAYSCR,AWAYNUM)
        DO I=1,AWAYNUM 
           MESBUF1(IND)=AWAYSCR(I)
           IND=IND+1
        ENDDO
C
 	RAND = INT(7*RAN(SEED1))+1           !amount
        AMOUNT(1) = AMTLIST(RAND)*HOMENUM*AWAYNUM
        I4TMP=AMOUNT(1)*(DOLL_BASE/DYN_BETUNIT)
        MESBUF1(IND+0) = I1TMP(4)
        MESBUF1(IND+1) = I1TMP(3)
        MESBUF1(IND+2) = I1TMP(2)
        MESBUF1(IND+3) = I1TMP(1)
        IND=IND+4
      ENDIF
C
	TNBRDS=TNBRDS+NBRDS
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
        IF(SYSBET) THEN
           DO I=1,MIN(HOMENUM,AWAYNUM)
              TYPE 935,HOMESCR(I),AWAYSCR(I)
           ENDDO
           IF(HOMENUM.GT.AWAYNUM) THEN
              DO I=MIN(HOMENUM,AWAYNUM)+1,HOMENUM
                 TYPE 936,HOMESCR(I)
              ENDDO
           ELSE IF(HOMENUM.LT.AWAYNUM) THEN
              DO I=MIN(HOMENUM,AWAYNUM)+1,AWAYNUM
                 TYPE 937,AWAYSCR(I)
              ENDDO
           ENDIF
           TYPE 940,AMOUNT(1)
        ELSE
           DO I=1,NBRDS
              TYPE 930,HOMESCR(I),AWAYSCR(I),AMOUNT(I)
           ENDDO
        ENDIF
C
	RETURN
C
920     FORMAT(' QP = ',L1,2X,A8,' GIND =',I2,'     NDRWS =',I2,
     *         '     NBRDS=',I2,'     TER=',I4,/ '     mes: ',
     *         <RLEN>Z3.2)
925     FORMAT('     mes: ',<MESLEN-RLEN>Z3.2)
930     FORMAT(2X'bet:  Home ',I2.2,'  Away ',I2.2,'  Amount',I5,'.00')
935     FORMAT(2X'bet:  Home ',I2.2,'  Away ',I2.2)
936     FORMAT(2X'bet:  Home ',I2.2)
937     FORMAT(2X'bet:           Away ',I2.2)
940     FORMAT(2X'amount',I5,'.00')
C
	END
