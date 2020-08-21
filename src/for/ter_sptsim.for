C SPTSIM.FOR
C
C V06 30-MAR-2017 MTK Modified Super 14 game
C V05 11-APR-2011 RXK Joker revision removed from message
C V04 28-OCT-2003 FRP Modify for Batch2 Totobola Changes.
C V03 08-JUN-2000 OXK Non-used variables removed
C V02 15-feb-2000 OXK Inserted Joker selection login (Vakio changes)
C V01 XX-XXX-XXXX XXX Initial revision
C
C PREPARE A SIMULATED SPORTS WAGER
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
	SUBROUTINE SPTSIM(WAGNO,NBRDS,QP,PERQP,
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
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER*4 PERQP
        INTEGER*4 IND, EXT
	INTEGER*4 DRWMAX, BRDMAX, NDRWS	 ! DRWMAX FOR CONSISTECY W/ OTHER GTYP'S
	INTEGER*4 ROWS(MXROW,MXBRD)      
        INTEGER*4 SIGNS                    !# of signs 1,X or 2 in row
        INTEGER*4 RAND
	INTEGER*4 BCNT
        INTEGER*4 HOLDER(MXROW)               ! how many signs where 
        BYTE      NBRDS, BTMP1, NDRWSHFT
	INTEGER*4 I4TMP,TEMP
	EQUIVALENCE (I4TMP,NDRWSHFT)
        INTEGER*2 I2TMP
        EQUIVALENCE (I2TMP,BTMP1)
C
	INTEGER*4 ROW(0:7)/'--- ',
     *            '1-- ','-X- ','1X- ','--2 ','1-2 ','-X2 ','1X2 '/
C
C
        LOGICAL   QP, WQP
        INTEGER*4 SEED1
 	INTEGER*4 I, J
        INTEGER*4 WAGNO, TNBRDS
C
        INTEGER*4 RLEN,ST
        INTEGER*2 OPTFLAGS,REVNUM
        BYTE      BOPTFLAGS(2),BREVNUM(2)
        EQUIVALENCE (OPTFLAGS,BOPTFLAGS)
        EQUIVALENCE (REVNUM,BREVNUM)
        BYTE      I1TMP(4)
        EQUIVALENCE (I4TMP,I1TMP(1))
C
        INTEGER*4 MULTI
        INTEGER*4 WEEKS(5) /1,2,3,3,5/
        INTEGER*4 RS2,RS3,FREESYSN
        INTEGER*4 ROW2S(SPGSYS)    ! # of rows with 2 signs (system bets only)
        INTEGER*4 ROW3S(SPGSYS)    ! # of rows with 3 signs (system bets only)
        INTEGER*4 SYSIND(SPGSYS+1) ! index to rows2s,rows3s 
        DATA ROW2S / 2, 1,  3,  0,  2,   4, 1,  3,  0,  5,
     *               2, 4,  1,  6,  3,   0, 5,  2,  7,  4,
     *               7, 0,  0,  8,  0,   8, 
     *		    84*0/
        DATA ROW3S / 0, 1,  0,  2,  1,   0, 2,  1,  3,  0,
     *               2, 1,  3,  0,  2,   4, 1,  3,  0,  2,
     *               3, 8,  9,  3, 10,   4,
     *		    84*0/
        DATA SYSIND/ 4, 6,  8,  9, 12,  16,18, 24, 27, 32,
     *              36,48, 54, 64, 72,  81,96,108,128,144,    !full systems
     *              60,81,222,432,729,1296,		      !reduced systems
     *              85*0/
C
        BYTE SP1DAT(MXLEN)
	LOGICAL JOKER /.FALSE./
	LOGICAL FIRST /.TRUE./
	INTEGER*4 GNUM
C
	INTEGER*4 RROW1,RROW2
	INTEGER*4 RROW(0:7)/'--- ',
     *            '0-- ','-1- ','01- ','--M ','0-M ','-1M ','01M '/
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
     *                   Z02,   ! 6  gametype
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
	IF (FIRST) THEN
	   DO I=27,SPGSYS
	      ROW2S(I)  = SPSNUM(3,I)-SPSNUM(2,I)
	      ROW3S(I)  = SPSNUM(2,I)-SPSNUM(1,I)
	      SYSIND(I) = SPSNUM(5,I)
	   ENDDO
	   SYSIND(SPGSYS+1)=9999
	   FIRST=.FALSE.
 	ENDIF

	BCNT = 0
	IF(SPTFRG(GIND).NE.0) BCNT = 1

	GBET=SPTMAX(GIND) - BCNT

	GNUM=GTNTAB(TSPT,GIND)
	JOKER=(KGNTAB(GNUM).NE.0)

	IF (WAGNO.EQ.0) THEN
          QP=PERQP.EQ.100
          SEED1=20000*INT(SECNDS(0.0))+1
	ENDIF
C
        IF(PERQP.LT.100.AND.PERQP.GT.0) THEN
          QP=PERQP.GE.INT(101*RAN(SEED1))
        ENDIF
        WQP = .FALSE.
        IF(MSQP(MGNUM).EQ.1) WQP=.TRUE. 

        QP=.FALSE.
        WQP=.FALSE.
C
C WAGER HEADER
C
	CALL MOVBYT(SP1DAT,1,MESBUF1,1,MXLEN)                             
C
	MESBUF1(7) = (MESBUF1(7).AND.'00'X) .OR. GIND*16
C
        IF(SYSBET) THEN
           NBRDS=1
        ELSE
           NBRDS=MAX(GMIN,INT(BRDMAX*RAN(SEED1))+1)
           NBRDS=NBRDS+MOD(NBRDS,2)
        ENDIF
        MULTI=WEEKS(MDRWMAX(MGNUM))
        RAND=INT(MULTI*RAN(SEED1))+1
        NDRWS=WEEKS(RAND)
	I4TMP=ISHFT(NDRWS,4)			!EQUIVALENCED TO NDRWSHFT !!!
        MESBUF1(8)=(MESBUF1(8).AND.'00'X) .OR. NDRWSHFT .OR. NBRDS
C
      OPTFLAGS=0
      IF(SPTREV(GIND).NE.0) THEN
	 ! IF (JOKER) OPTFLAGS=IOR(OPTFLAGS,'1000'X) !sports '8000',joker '1000'
	 OPTFLAGS=IOR(OPTFLAGS,'8000'X)		   !sports '8000',joker '1000'
         REVNUM=SPTREV(GIND)
         CALL MOVBYT(BREVNUM,1,MESBUF1,11,2)  !sports game rev
      ENDIF
      !IF(KIKREV(1).NE.0.AND.JOKER) THEN
      !   REVNUM=KIKREV(1)
      !   CALL MOVBYT(BREVNUM,1,MESBUF1,13,2)  !joker game rev
      !ENDIF

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
      !IF (JOKER) THEN 
      !   IND=15
      !ELSE
         IND=13
      !ENDIF
      IF(QP.OR.WQP) THEN                             !quick pick flags
         OPTFLAGS=IOR(OPTFLAGS,'0200'X)
         MESBUF1(IND)='FF'X
         MESBUF1(IND+1)='F0'X
         IND=IND+2
      ENDIF
C
      IF(SYSBET) THEN                         !system bet
	 IF (SYSBNUM.GE.1 .AND. SYSBNUM.LE.SPGSYS) THEN
  	    RS2=ROW2S(SYSBNUM)
	    RS3=ROW3S(SYSBNUM)
	 ELSEIF(SYSBNUM.EQ.9999)THEN
            RS2=INT(5*RAN(SEED1))+1
            RS3=INT(5*RAN(SEED1))+1
            FREESYSN=(2**RS2)*(3**RS3)
            TYPE 111,' Free system # ',FREESYSN,
     *                 RS2,' times 1X-,1-2 or -X2 and ',
     *                 RS3,' times 1X2 '
111         FORMAT(A,I4,/,10X,I1,A,/,10X,I1,A)
         ELSE
            TYPE*,'Invalid system number',SYSBNUM
            CALL XWAIT(2,2,ST)
            EXT=-2
            RETURN
         ENDIF
500      CONTINUE                    
         OPTFLAGS=IOR(OPTFLAGS,'0100'X)
         IF(SYSBNUM.LE.20) THEN
            MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'01'X   !full system
            I4TMP=SYSIND(SYSBNUM)
         ELSEIF(SYSBNUM.GE.21.AND.SYSBNUM.LE.SPGSYS) THEN
            MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'02'X   !reduced system
            I4TMP=SYSBNUM
         ELSE
            MESBUF1(7) = (MESBUF1(7).AND.'F0'X).OR.'01'X   !free system
            I4TMP=FREESYSN
         ENDIF
         MESBUF1(IND)=I1TMP(2)
         MESBUF1(IND+1)=I1TMP(1)
         IND=IND+2
         NBRDS=1
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

      IF(WQP) THEN
         OPTFLAGS=IOR(OPTFLAGS,'0001'X)
	 I4TMP=SPSVER(1)          !wqp table revision
         MESBUF1(IND)=I1TMP(4)    
         MESBUF1(IND+1)=I1TMP(3)
         MESBUF1(IND+2)=I1TMP(2)
         MESBUF1(IND+3)=I1TMP(1)
         IND=IND+4
      ENDIF
C
      MESBUF1(10)=BOPTFLAGS(1)                 !option flags now set
      MESBUF1(9)=BOPTFLAGS(2)
C
      RLEN=IND-1                   ! # bytes displayd in first line
C
C SUPER14 (RESULTS) ROW BET DETAILS
C
	IF(SPTFRG(GIND).EQ.1) THEN
          SIGNS=1
          CALL GEN1X2(SEED1,SIGNS,RROW1)
          CALL GEN1X2(SEED1,SIGNS,RROW2)
          MESBUF1(IND)= ISHFT(RROW1,4)+RROW2
          IND=IND+1
        ENDIF

        IF(SPTFRG(GIND).EQ.2) THEN
          SIGNS=1
          CALL GEN1X2(SEED1,SIGNS,RROW1)
          MESBUF1(IND)= IAND(RROW1,'0F'X)
          IND=IND+1
        ENDIF

100   CONTINUE
C
C SIMPLE BET DETAILS
C
      IF(.NOT.SYSBET) THEN
        DO 200 I=1,NBRDS
	  I2TMP=0
          DO 300 J=1,GBET
            SIGNS=1
            CALL GEN1X2(SEED1,SIGNS,RAND)
	    ROWS(J,I)=RAND
            I2TMP=ISHFT(I2TMP,4)+ROWS(J,I)
	    IF(MOD(J,2).EQ.0) THEN
              MESBUF1(IND)=BTMP1
              IND=IND+1
	      I2TMP=0
            ENDIF
300       CONTINUE
	  IF (I2TMP.NE.0) THEN
      	    I2TMP=ISHFT(I2TMP,4)
            MESBUF1(IND)=BTMP1
            IND=IND+1
	  ENDIF
200     CONTINUE
      ENDIF
C
C FULL, REDUCED OE FREE SYSTEM DETAILS
C
      IF(SYSBET) THEN
         CALL FASTSET(1,HOLDER,MXROW)
         IF(RS2.GT.0) THEN
            DO I=1,RS2
               CALL GENHOL(SEED1,2,HOLDER,GBET)
            ENDDO
         ENDIF
         IF(RS3.GT.0) THEN
            DO I=1,RS3
               CALL GENHOL(SEED1,3,HOLDER,GBET)
            ENDDO
         ENDIF
C
	 I2TMP=0                             ! create bet
         I=1                                 ! first board only 
         DO 600 J=1,GBET
            SIGNS=HOLDER(J)
            CALL GEN1X2(SEED1,SIGNS,RAND)
	    ROWS(J,I)=RAND
            I2TMP=ISHFT(I2TMP,4)+ROWS(J,I)
	    IF(MOD(J,2).EQ.0) THEN
               MESBUF1(IND)=BTMP1
               IND=IND+1
	       I2TMP=0
            ENDIF
600      CONTINUE
         I2TMP=ISHFT(I2TMP,4)
         MESBUF1(IND)=BTMP1
         IND=IND+1
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
C SPORTS WAGER DISPLAY
C
        TYPE 920, QP,GTNAMES(GTYP),GIND,NDRWS,NBRDS,TER,
     *	          (MESBUF1(I),I=1,RLEN)
        
        J=RLEN
        IF(SPTFRG(GIND).NE.0) THEN
           TYPE 921,MESBUF1(J+1)
           J=J+1
        ENDIF           

        TYPE 926, (MESBUF1(I),I=J+1,MESLEN)
        DO I=1,NBRDS
           TYPE 930,(ROW(ROWS(J,I)),J=1,GBET)
        ENDDO

	IF(SPTFRG(GIND).EQ.1) THEN
          TYPE 931,RROW(RROW1),RROW(RROW2)
	ENDIF

        IF(SPTFRG(GIND).EQ.2) THEN
          TYPE 931,ROW(RROW1)
        ENDIF

C
	RETURN
C
920     FORMAT(' QP = ',L1,2X,A8,' GIND =',I2,'     NDRWS =',I2,
     *         '     NBRDS=',I2,'     TER=',I4,/ '     mes: ',
     *         <RLEN>Z3.2)
921     FORMAT('     super14 :',Z3.2)
925     FORMAT('     mes: ',<MESLEN-RLEN>Z3.2)
926     FORMAT('     mes: ',7Z3.2)
930     FORMAT(2X,<GBET>A4)
931     FORMAT(2X,' S14: ',2A4)
C
	END
C
        SUBROUTINE GEN1X2(SEED1,SIGNS,RAND)
        INTEGER*4  SIGNS ! # of signs to generate
        INTEGER*4  SEED1
        INTEGER*4  RAND
        
        IF(SIGNS.EQ.3) THEN 
           RAND=7
           RETURN
        ENDIF
10      CONTINUE
	RAND = INT(6*RAN(SEED1))+1
        IF(SIGNS.EQ.1) THEN
          IF(RAND.EQ.1.OR.RAND.EQ.2.OR.RAND.EQ.4) THEN
             RETURN
          ELSE
             GOTO 10
          ENDIF
        ENDIF
        IF(SIGNS.EQ.2) THEN
          IF(RAND.EQ.3.OR.RAND.EQ.5.OR.RAND.EQ.6) THEN
             RETURN
          ELSE
             GOTO 10
          ENDIF
        ENDIF
        END
C
        SUBROUTINE GENHOL(SEED1,NUMS,HOLDER,ROWS)
        INTEGER*4  SEED1
        INTEGER*4  NUMS         ! 2 or 3
        INTEGER*4  HOLDER(14)	! 14 == SPGNBR
	INTEGER*4  ROWS		!     out of which this amount is used this time
        INTEGER*4  RAND
	INTEGER*4  LIMIT	! To avoid infinite loops
				! It's OK to have the output set up wrongly,
				! since we want to test how host handles it...

	LIMIT=0        
10      CONTINUE
	RAND = INT(ROWS*RAN(SEED1))+1
        IF(HOLDER(RAND).EQ.1) THEN
           HOLDER(RAND)= NUMS
           RETURN
        ELSE
	   LIMIT=LIMIT+1
	   IF (LIMIT.GE.9999) RETURN
           GOTO 10
        ENDIF
        END
C
