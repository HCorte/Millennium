C TSLSIM.FOR
C
C PREPARE A SIMULATED TOTO SELECT WAGER
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
	SUBROUTINE TSLSIM(WAGNO,NBRDS,QP,PERQP,
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
        INCLUDE 'INCLIB:TSLCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER*4 PERQP
        INTEGER*4 IND, EXT
	INTEGER*4 DRWMAX, BRDMAX, NDRWS
	INTEGER*4 ROWS(6)		      ! row numbers bet 	      
        INTEGER*4 SIGNS(6)                    ! sign on row
        INTEGER*4 RAND,SIGN
        BYTE      NBRDS, BTMP1, NDRWSHFT
	INTEGER*4 I4TMP
	EQUIVALENCE (I4TMP,NDRWSHFT)
        INTEGER*2 I2TMP
        EQUIVALENCE (I2TMP,BTMP1)
C
	INTEGER*4 ROW(0:3)/'--- ','1-- ','--2 ','-X- '/
C
C
        LOGICAL   QP
        INTEGER*4 SEED1
 	INTEGER*4 I, J
        INTEGER*4 WAGNO, TNBRDS
C
        LOGICAL   FIRST/.TRUE./
C
        INTEGER*4 RLEN,ST
        INTEGER*2 OPTFLAGS,REVNUM
        BYTE      BOPTFLAGS(2),BREVNUM(2)
        EQUIVALENCE (OPTFLAGS,BOPTFLAGS)
        EQUIVALENCE (REVNUM,BREVNUM)
        BYTE      I1TMP(4)
        EQUIVALENCE (I4TMP,I1TMP(1))
C
        INTEGER*4 NUMSIGNS,OPNCNT,AMOUNT
        INTEGER*4 SYSLIST(5),SIGNSLIST(5),AMTLIST(9) 
        DATA SYSLIST/4,10,20,5,15/
        DATA SIGNSLIST/4,5,6,5,6/
        DATA AMTLIST/5,10,20,50,100,200,300,400,500/
        INTEGER*4 OPENROWS(MAXSRW)
        INTEGER*4 HOLDER(MAXSRW)
C
        BYTE TS1DAT(MXLEN)
C
CC ------------------------------------------------------------
	DATA (TS1DAT(I),I=1,MXLEN)/
C
C PITKA WAGER, WITHOUT BOARDS (TO BE GENERATED RANDOMLY)
C
     *                   Z20,   ! 1  control and sequence
     *                   Z00,   ! 2  type
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z80,   ! 5  statistics (simulator transaction)
     *                   Z08,   ! 6  gametype
     *                   Z10,   ! 7  game index and system
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
        IF(FIRST) THEN                    !create a list of open rows
          J=0
          DO I=1,MAXSRW
             IF(TSLSTA(I,GIND).EQ.GAMOPN) THEN
                J=J+1
                OPENROWS(J)=I
                OPNCNT=OPNCNT+1
             ENDIF 
          ENDDO
          FIRST=.FALSE.
CCCCCCCCCCCCCCC
        TYPE 12345,(OPENROWS(I),I=1,40)
12345  FORMAT('OP.R.:',20(I3))
CCCCCCCCCCCCCCC
        ENDIF
C
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
	CALL MOVBYT(TS1DAT,1,MESBUF1,1,MXLEN)                             
        NBRDS=1
        NDRWS=1
	I4TMP=ISHFT(NDRWS,4)			!EQUIVALENCED TO NDRWSHFT !!!
        MESBUF1(8)=(MESBUF1(8).AND.'00'X) .OR. NDRWSHFT .OR. NBRDS
C
      OPTFLAGS=0
      IF(TSLREV(GIND).NE.0) THEN
         OPTFLAGS=IOR(OPTFLAGS,'8000'X)       !pitka '8000', no joker
         REVNUM=TSLREV(GIND)
         CALL MOVBYT(BREVNUM,1,MESBUF1,11,2)  !pitka game rev
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
         DO I=1,5
            IF(SYSBNUM.EQ.SYSLIST(I)) THEN     ! find # of signs 1,X,2 
               NUMSIGNS=SIGNSLIST(I)
               GOTO 500
            ENDIF
         ENDDO
         TYPE*,'Invalid system number',SYSBNUM
         CALL XWAIT(2,2,ST)
         EXT=-2
         RETURN
500      CONTINUE                    
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
      RLEN=IND-1                   ! # bytes displayed in first line
      CALL FASTSET(0,HOLDER,MAXSRW) 
C
C SIMPLE BET. NUMBER OF MARKS AND AMOUNT
C
      IF(.NOT.SYSBET) THEN
	  I2TMP=0
 	  GBET = INT(4*RAN(SEED1))+1+2     !3,4,5 or 6
          MESBUF1(IND)=GBET
          IND=IND+1
 	  RAND = INT(9*RAN(SEED1))+1
          AMOUNT = AMTLIST(RAND)
          I4TMP=AMOUNT/5
          MESBUF1(IND+0) = I1TMP(4)
          MESBUF1(IND+1) = I1TMP(3)
          MESBUF1(IND+2) = I1TMP(2)
          MESBUF1(IND+3) = I1TMP(1)
          IND=IND+4
          I4TMP=0
      ENDIF
C
C FULL OR REDUCED SYSTEM DETAILS. NUMBER OF MARKS AND AMOUNT
C
      IF(SYSBET) THEN
	  I2TMP=0
 	  GBET = NUMSIGNS
          MESBUF1(IND)=GBET
          IND=IND+1
 	  RAND = INT(6*RAN(SEED1))+1  ! 6 here because of liability problems 
          AMOUNT = AMTLIST(RAND)      ! for big amounts
          I4TMP=AMOUNT*SYSBNUM/5
          MESBUF1(IND+0) = I1TMP(4)
          MESBUF1(IND+1) = I1TMP(3)
          MESBUF1(IND+2) = I1TMP(2)
          MESBUF1(IND+3) = I1TMP(1)
          IND=IND+4
          I4TMP=0
      ENDIF
C
C ALL BETS. FIND ROW NUMBERS AND SORT THEM
C
          DO J=1,GBET
             CALL GENTSL1X2(SEED1,OPNCNT,HOLDER,RAND)
             ROWS(J) = OPENROWS(RAND)
          ENDDO
C
          CALL BUBSORT(ROWS,GBET)
C
C  ALL BETS. FIND MARKS ANS WRITE TO MESSAGE
C
          DO J=1,GBET
             SIGN = INT(3*RAN(SEED1))+1
             SIGNS(J) = SIGN
             I2TMP=ISHFT(ROWS(J),2)+SIGN
             MESBUF1(IND)=BTMP1
             IND=IND+1
             I2TMP=0
          ENDDO
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
C PITKA WAGER DISPLAY
C
        TYPE 920, QP,GTNAMES(GTYP),GIND,NDRWS,NBRDS,TER,
     *	          (MESBUF1(I),I=1,RLEN)
        TYPE 925, (MESBUF1(I),I=RLEN+1,MESLEN)
        TYPE 930, (ROWS(J),ROW(SIGNS(J)),J=1,GBET)
C
	RETURN
C
920     FORMAT(' QP = ',L1,2X,A8,' GIND =',I2,'     NDRWS =',I2,
     *         '     NBRDS=',I2,'     TER=',I4,/ '     mes: ',
     *         <RLEN>Z3.2)
925     FORMAT('     mes: ',<MESLEN-RLEN>Z3.2)
926     FORMAT('     mes: ',7Z3.2)
930     FORMAT(2X'bet: ',<GBET>(I4,'|'A4))
C
	END
        SUBROUTINE GENTSL1X2(SEED1,OPNCNT,HOLDER,RAND)
        INTEGER*4  SEED1
        INTEGER*4  OPNCNT
        INTEGER*4  HOLDER(*)
        INTEGER*4  RAND

10      CONTINUE
        RAND = INT(OPNCNT*RAN(SEED1))+1
        IF(HOLDER(RAND).EQ.0) THEN
           HOLDER(RAND)=1
           RETURN
        ELSE
           GOTO 10
        ENDIF
        END

