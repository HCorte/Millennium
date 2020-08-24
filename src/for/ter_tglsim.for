C TGLSIM.FOR
C
C V01 03-DEc-2000 UXN Initial release.
C
C PREPARE A SIMULATED Totogolo WAGER
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS	/CHECK=NOOVERFLOW
	SUBROUTINE TGLSIM(WAGNO,NBRDS,QP,PERQP,
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
        INCLUDE 'INCLIB:TGLCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER*4 PERQP
        INTEGER*4 IND, EXT
	INTEGER*4 DRWMAX, BRDMAX, NDRWS	 ! DRWMAX FOR CONSISTECY W/ OTHER GTYP'S
        INTEGER*4 RAND
        BYTE      NBRDS, BTMP1, NDRWSHFT
	INTEGER*4 I4TMP,TEMP
	EQUIVALENCE (I4TMP,NDRWSHFT)
        INTEGER*2 I2TMP
        EQUIVALENCE (I2TMP,BTMP1)
	INTEGER*4 ROW1,ROW2
	INTEGER*4 ROWS(2,TGGNBR)
C
        LOGICAL   QP, WQP/.FALSE./
        INTEGER*4 SEED1,ST
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
        INTEGER*4 MULTI
        INTEGER*4 WEEKS(5) /1,2,3,3,5/
C
        BYTE SP1DAT(MXLEN)
	LOGICAL JOKER /.FALSE./
	INTEGER*4 GNUM
C
CC ------------------------------------------------------------
	DATA (SP1DAT(I),I=1,MXLEN)/
C
C SPORTS WAGER, WITHOUT BOARDS (TO BE GENERATED RANDOMLY)
C
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
C
C INITIALIZE DATA FOR THE MAIN LOOP
C
	GBET=TGLMAX(GIND)

	GNUM=GTNTAB(TTGL,GIND)
	JOKER=(KGNTAB(GNUM).NE.0)

	IF (WAGNO.EQ.0) THEN
          QP=PERQP.EQ.100
          SEED1=20000*INT(SECNDS(0.0))+1
	ENDIF
C
        IF(PERQP.LT.100.AND.PERQP.GT.0) THEN
          QP=PERQP.GE.INT(101*RAN(SEED1))
        ENDIF
C
C WAGER HEADER
C
	CALL MOVBYT(SP1DAT,1,MESBUF1,1,MXLEN)                             
C
	MESBUF1(7) = ISHFT(GIND,4)
C
        IF(SYSBET) THEN
           NBRDS=1
	   DO 20,I=1,TGLMAX(GIND)
	      ROWS(1,I) = 0
	      ROWS(2,I) = 0
	      TYPE*,'MATCH#',I
10	      CONTINUE
	      CALL INPNUM('Enter home team score [E-no more]',TEMP,0,2,ST)
	      IF(ST.EQ.-3) THEN
	         TEMP = 3
	      ELSEIF(ST.NE.0) THEN
	         GOTO 15
	      ENDIF
	      ROWS(1,I) = IBSET(ROWS(1,I),TEMP)
	      GOTO 10
15	      CONTINUE
	      CALL INPNUM('Enter away team score [E-no more]',TEMP,0,2,ST)
	      IF(ST.EQ.-3) THEN
	         TEMP = 3
	      ELSEIF(ST.NE.0) THEN
	         GOTO 20
	      ENDIF
	      ROWS(2,I) = IBSET(ROWS(2,I),TEMP)
	      GOTO 15
20	   CONTINUE	   
	   MESBUF1(7) = MESBUF1(7) + FULSYS
        ELSE
           NBRDS=MAX(GMIN,INT(BRDMAX*RAN(SEED1))+1)
        ENDIF
        MULTI=WEEKS(MDRWMAX(MGNUM))
        RAND=INT(MULTI*RAN(SEED1))+1
        NDRWS=WEEKS(RAND)
	I4TMP=ISHFT(NDRWS,4)			!EQUIVALENCED TO NDRWSHFT !!!
        MESBUF1(8)=(MESBUF1(8).AND.'00'X) .OR. NDRWSHFT .OR. NBRDS
C
      OPTFLAGS=0
      IF(TGLREV(GIND).NE.0) THEN
	 IF (JOKER) OPTFLAGS=IOR(OPTFLAGS,'1000'X) !sports '8000',joker '1000'
	 OPTFLAGS=IOR(OPTFLAGS,'8000'X)		   !sports '8000',joker '1000'
         REVNUM=TGLREV(GIND)
         CALL MOVBYT(BREVNUM,1,MESBUF1,11,2)  !sports game rev
      ENDIF
      IF(KIKREV(1).NE.0.AND.JOKER) THEN
         REVNUM=KIKREV(1)
         CALL MOVBYT(BREVNUM,1,MESBUF1,13,2)  !joker game rev
      ENDIF

      IF(GJOK.NE.0.AND.JOKER) THEN                      !first joker participating
         OPTFLAGS=IOR(OPTFLAGS,'0080'X)
      ENDIF
CC      IF(GJOK.EQ.2) THEN                      !second joker
CC         OPTFLAGS=IOR(OPTFLAGS,'0040'X)
CC      ENDIF
CC      IF(GJOK.EQ.3) THEN                      !both jokers
CC         OPTFLAGS=IOR(OPTFLAGS,'0080'X)
CC         OPTFLAGS=IOR(OPTFLAGS,'0040'X)
CC      ENDIF

      IF (JOKER) OPTFLAGS=IOR(OPTFLAGS,'0020'X)          !first joker always set
CC      OPTFLAGS=IOR(OPTFLAGS,'0010'X)
C
C OPTIONAL DATA
C
      IF (JOKER) THEN 
         IND=15
      ELSE
         IND=13
      ENDIF
      IF(QP.OR.WQP) THEN                             !quick pick flags
         OPTFLAGS=IOR(OPTFLAGS,'0200'X)
         MESBUF1(IND)='FF'X
         MESBUF1(IND+1)='F0'X
         IND=IND+2
      ENDIF
C
      IF(SYSBET) THEN                         !system bet
	OPTFLAGS=IOR(OPTFLAGS,'0100'X)
	I4TMP = SYSBNUM
	MESBUF1(IND+0) = I1TMP(2)
	MESBUF1(IND+1) = I1TMP(1)
	IND = IND + 2
      ENDIF
C
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
c

C
      MESBUF1(10)=BOPTFLAGS(1)                 !option flags now set
      MESBUF1(9)=BOPTFLAGS(2)
C
      RLEN=IND-1                   ! # bytes displayd in first line
C
C SIMPLE BET DETAILS
C
      IF(.NOT.SYSBET) THEN
        DO 200 I=1,NBRDS
	  I2TMP=0
          DO J=1,TGLMAX(GIND)
            CALL GENTGL(SEED1,ROW1)
            CALL GENTGL(SEED1,ROW2)
	    TEMP = 0
	    TEMP = IBSET(TEMP,ROW1)
	    TEMP = ISHFT(TEMP,4)
	    TEMP = IBSET(TEMP,ROW2)
            MESBUF1(IND)= TEMP
            IND=IND+1
          ENDDO
200     CONTINUE
      ELSE
	 DO I=1,TGLMAX(GIND)
	    MESBUF1(IND) = IOR(ISHFT(ROWS(1,I),4),ROWS(2,I))
	    IND = IND + 1
	 ENDDO
      ENDIF
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
C TOTOGOLO WAGER DISPLAY
C
        TYPE 920, QP,GTNAMES(GTYP),GIND,NDRWS,NBRDS,TER,
     *	          (MESBUF1(I),I=1,RLEN)
        IF(NBRDS.GT.1) THEN
           TYPE 926, (MESBUF1(I),I=RLEN+1,MESLEN)
        ELSE
           TYPE 925, (MESBUF1(I),I=RLEN+1,MESLEN)
        ENDIF
C        DO I=1,NBRDS
C           TYPE 930,(ROW(ROWS(J,I)),J=1,GBET)
C        ENDDO
C
	RETURN
C
920     FORMAT(' QP = ',L1,2X,A8,' GIND =',I2,'     NDRWS =',I2,
     *         '     NBRDS=',I2,'     TER=',I4,/ '     mes: ',
     *         <RLEN>Z3.2)
925     FORMAT('     mes: ',<MESLEN-RLEN>Z3.2)
926     FORMAT('     mes: ',7Z3.2)
930     FORMAT(2X,<GBET>A4)
C
	END
C
        SUBROUTINE GENTGL(SEED1,RAND)
        INTEGER*4  SEED1
        INTEGER*4  RAND

	RAND = MOD(INT(10*RAN(SEED1)),4)

        END
C
