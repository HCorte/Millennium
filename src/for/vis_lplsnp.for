C SUBROUTINE LPLSNP
C
C V07 15-FEB-2000 OXK Added Sport indexes 2-6 (Vakio changes)
C V06 09-SEP-1998 RXK Jokeri pool display added
C V05 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C			Instant Pass Thru Phase 1
C V04 04 Dec 1995 HXK Made changes for LTPOOL_GAMENR not having MAXTYP 
C			as array size!
C V03 13 Jun 1993 HXK added AGTINF.DEF, PRMAGT.DEF
C V02 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE LPLSNP(NUM,GIND)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:POOLLTO.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:VISCOM.DEF'
C
        INTEGER*4 I, TOCLEAR, II, SUM, IND, OTHRBANK, TOVRBLK
        INTEGER*4 TLSTOVR, TLTSKSTAT, TLCHKSTOP, TLCHKSER, GAME
        INTEGER*4 TLCHKFIL, TLCHKPNT, TPAGENUM, TQNUM, TSWITCH
        INTEGER*4  PVOLNAME, STAT, OFF, TYPE, GAMENR, GIND
C
        CHARACTER*8 STATUS(2)
        INTEGER*4 BNKLEN(BANKSORT)
C
C V02   THE FOLLOWING INITIALIZATION IS NEEDED FOR FLINT
C V02   INITIALIZATION HAS TO BE DONE WITHOUT ARITHMETIC
C
        INTEGER*4   BOARD_SIZE
        PARAMETER   (BOARD_SIZE=20*LTNUMGAMES)
C
        INTEGER*4 BOARD(20,LTNUMGAMES)/BOARD_SIZE*0/
        INTEGER*4 LQUE(2),NUM,OUTSTAT
        INTEGER*4 ADDL(2),REML(2),NBET(LTNUMGAMES)
        INTEGER*4 POLDRW,POLGAM,POLMAX,POLBET,INDEX,POLTOT
        INTEGER*4 CALCULAT, LASTNUM
        PARAMETER(CALCULAT=99)
        DATA STATUS/' enabled','disabled'/
        INTEGER*4 CNV1X2(0:3) /'    ','1111','XXXX','2222'/
        DATA NBET/LTNUMGAMES*0/
        INTEGER*4 LASTGAME /0/
        INTEGER*4 GAME_TYPE /0/,GAME_INDEX /0/
        INTEGER*4 SPACES(4) /4*'    '/
        SAVE LASTGAME,BOARD
        LOGICAL DISP
C
C
C Adjust NUM value
C
        DISP=.TRUE.
        GAME=GIND

        IF(GAME.LT.1.OR.GAME.GT.LTNUMGAMES) GAME=1
C***  IF(NUM.LT.0) NUM=0
C***  IF(NUM.GT.LTPOOLNR(GAME).AND.NUM.LT.CALCULAT) THEN
C***     OUTSTAT=0
C***     RETURN
C***  ENDIF
C***  IF(NUM.GT.CALCULAT) THEN
C***     OUTSTAT=0
C***     RETURN
C***  ENDIF
        IF (LASTGAME.NE.GAME) OUTSTAT=0
        LASTGAME=GAME
C
        GAMENR=0
        DO 10, TYPE=1,LTPOOL_MAXTYP
        DO 10, INDEX=1,MAXIND
           IF (LTPOOL_GAMENR(TYPE,INDEX).EQ.GAME) THEN
              GAME_TYPE=TYPE
              GAME_INDEX=INDEX
              GAMENR=GTNTAB(TYPE,INDEX)
              GOTO 20
           ENDIF
10      CONTINUE
C
20      CONTINUE
C
C Call accept number routine to fill in board array
C
        IF (GAME_TYPE.EQ.TSPT) THEN
           OUTSTAT=0
           IF (NUM.EQ.99) THEN
              DO 25, OFF=1,LTPOOLBET(GAME)
                 IF (BOARD(OFF,GAME).LE.0) GOTO 30
                 IF (BOARD(OFF,GAME).GT.3) GOTO 30
25            CONTINUE
              OUTSTAT=1
              GOTO 30
           ENDIF
           IF (MOD(NUM,10).GT.3) GOTO 30
           IF (MOD(NUM,10).EQ.0) GOTO 30
           IF (NUM/10.LE.0)      GOTO 30
           IF (NUM/10.GT.LTPOOLBET(GAME)) GOTO 30
           BOARD(NUM/10,GAME)=MOD(NUM,10)
        ELSE
           IF(NUM.EQ.0.AND.LASTNUM.EQ.CALCULAT.OR.
     *        NUM.GT.LTPOOLNR(GAME).AND.NUM.NE.CALCULAT) GOTO 30
           CALL ACCBET(NUM,BOARD(1,GAME),OUTSTAT,LTPOOLBET(GAME))
           LASTNUM=NUM
           IF(NUM.NE.CALCULAT) DISP=.FALSE.
        ENDIF
C
30      CONTINUE
C
C Duplicate numbers entered
C
         IF (OUTSTAT.EQ.-1) THEN
            OUTSTAT=0
            RETURN
         ENDIF
C
C Calculate number of BETS
C
         IF (OUTSTAT.EQ.1) THEN
           CALL GETBETS(BOARD(1,GAME),NBET(GAME),STAT,GAME)
           IF (STAT.NE.0) NBET(GAME)=STAT           !DISPLAY ERROR
           OUTSTAT=0
           NUM=0
         ENDIF
C
C
        PVOLNAME = SFNAMES(1,LPR)
C
C SET POOL VALUES
C
        POLDRW=LTPOOLDRAW(GAME)
        POLGAM=GAME
        POLTOT=LTPOOL_TOT(GAME)
        POLMAX=LTPOOLNR(GAME)
        POLBET=LTPOOLBET(GAME)
C
C FIND SUPPRESSION INDEX
C
        INDEX=P(SUPPUD)
        INDEX=INDEX+1
C
C
        TSWITCH = LTQNUM
        TQNUM = LTQNUM
        TPAGENUM = LTCURPAG
        TLCHKPNT = LCHKPNT
        TLCHKFIL = 0
        IF (LCHKFIL .GT. 0 ) TLCHKFIL = LCHKFIL
        TLCHKSER = LCHKSER
        TLCHKSTOP = LCHKSTOP
        TLTSKSTAT = LTSKSTAT
C
        TLSTOVR = LTLSTOVR(GAME)
        TOVRBLK = OVRBLK
C
C
C     GET LENGTH OF QUEUES
C
        OTHRBANK=0
        IND=0
        CALL FASTSET(0,BNKLEN(1),BANKSORT)
        DO 40, OFF=1,BANKSORT
          IF (LTPOOL_PAGGAM(GAME).NE.GAME) GOTO 40
          IND=IND+1
          CALL NQIMAGE(BANK(1,OFF),SUM,1)
          IF (IND.GE.3) THEN
             OTHRBANK=OTHRBANK+SUM
          ELSE
             BNKLEN(IND)=SUM
          ENDIF
40      CONTINUE
C
        CALL NQIMAGE(LTOQ1(1,1),LQUE(1),1)
        CALL NQIMAGE(LTOQ1(1,2),LQUE(2),1)
        CALL NQIMAGE(ADDOVR(1,1),ADDL(1),1)
        CALL NQIMAGE(ADDOVR(1,2),ADDL(2),1)
        CALL NQIMAGE(REMOVR(1,1),REML(1),1)
        CALL NQIMAGE(REMOVR(1,2),REML(2),1)
C
        IF (GAMENR.NE.0) THEN
           WRITE(CLIN2,902) (GLNAMES(II,GAMENR),II=1,4)
        ELSE
           WRITE (CLIN2,902) (SPACES(II),II=1,4)
        ENDIF
        WRITE(CLIN3,903) PVOLNAME,GAME_TYPE,GAME_INDEX
        IF (GAME_TYPE.NE.TSPT) THEN
           WRITE(CLIN4,9044) POLBET,POLMAX,POLDRW,POLGAM,POLTOT
        ELSE
           WRITE(CLIN4,90441) POLBET,POLDRW,POLGAM,POLTOT
        ENDIF
        WRITE(CLIN5,904) STATUS(INDEX)
        WRITE(CLIN6,905) TLSTOVR
        WRITE(CLIN7,906) TOVRBLK
        WRITE(CLIN8,908) TSWITCH
        WRITE(CLIN9,909) TQNUM
        WRITE(CLIN10,910) TPAGENUM
        WRITE(CLIN11,911) TLCHKPNT
        WRITE(CLIN12,913) TLCHKFIL
        WRITE(CLIN13,914) TLCHKSER
        WRITE(CLIN14,915) TLCHKSTOP
        WRITE(CLIN15,916) TLTSKSTAT
        TOCLEAR=20-LTPOOLBET(GAME)-1
        CALL FASTSET(0,BOARD(LTPOOLBET(GAME)+1,GAME),TOCLEAR)
        IF (GAME_TYPE.EQ.TSPT) THEN
           WRITE(CLIN17,9171) (CNV1X2(BOARD(I,GAME)),I=1,13)
        ELSE
           WRITE(CLIN17,9172) (BOARD(I,GAME),I=1,13)
        ENDIF
        IF(DISP) THEN
           WRITE(CLIN18,918) NBET(GAME)
        ELSE
           WRITE(CLIN18,9181)
        ENDIF
        WRITE(CLIN19,919)
        WRITE(CLIN20,920)
        WRITE(CLIN21,921) LQUE(1),LQUE(2),BNKLEN(1),BNKLEN(2),
     !          OTHRBANK,ADDL(1),ADDL(2),REML(1),REML(2)
C
C
        RETURN
C
C
902     FORMAT(11X,'-------------- POOL FLAGS ----------',2X,4A4)
903     FORMAT(11X,'Pool volume:',A4,3X,' Type: ',I2,' Index: ',I2)
9044    FORMAT(11X,'Lotto ',I2,'/',I2,'      Draw # ',I4,' Pool # ',I2
     *            ,' total ',I8)
90441   FORMAT(11X,'Sport ',I2,' rows   ',' Draw # ',I4,' Pool # ',I2
     *            ,' total ',I8)
904     FORMAT(11X,'Pools status P(SUPPUD) ............. ',A8)
905     FORMAT(11X,'Last element in LTOVR...............',I9)
906     FORMAT(11X,'Last block written to POOLOVR.......',I9)
908     FORMAT(11X,'LTOQ1 index that LTOPRO is using....',I9)
909     FORMAT(11X,'ADDOVR & REMOVR index OVERPRO uses..',I9)
910     FORMAT(11X,'Current pool page in memory.........',I9)
911     FORMAT(11X,'Current LCHKPNT value...............',I9)
913     FORMAT(11X,'Current LCHKFIL value...............',I9)
914     FORMAT(11X,'Current LCHKSER value...............',I9)
915     FORMAT(11X,'Current LCHKSTOP value..............',I9)
916     FORMAT(11X,'Current LLTSKSTAT value.............',I9)
9171    FORMAT(11X,'B o a r d  -   ',20(A1,3X))
9172    FORMAT(11X,'B o a r d  -   ',20(I2.0,2X))
918     FORMAT(11X,'N u m b e r   o f   t i m e s   b e t  : ',I8)
9181    FORMAT(11X,'N u m b e r   o f   t i m e s   b e t  : ')
919   FORMAT(6X,'   LTOQ1   ',4X,'       BANKQ     ',2X,'  ADDOVR   ',
     !         2X,'   REMOVR  ')
920   FORMAT(6X,'-------------',2X,'-----------------',2X,'-----------',
     !         2X,'-----------')
921     FORMAT(4X,I7,1X,I7,2X,I5,1X,I5,1X,I5,2X,I5,1X,I5,2X,I5,1X,I5)
C
940     FORMAT(11X,'J O K E R I ,  DRAW #',I5)
950     FORMAT(11X,'NUMBER  ',I7.7'  BET',I5,'  TIMES')

        END
