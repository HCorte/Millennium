C
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
      PROGRAM SCANDAYS
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:PRMLOG.DEF'
      INCLUDE 'INCLIB:DESTRA.DEF'
      INCLUDE 'INCLIB:DESLOG.DEF'
      INCLUDE 'INCLIB:PRMAGT.DEF'
C
      INTEGER*4 ST, COUNT, TER, SER, I, EOF, TIME, EXT  
      INTEGER*4 FLAG, TABLE(TRALEN,2,NUMAGT),STAT(NUMAGT)
      INTEGER*4 GAM, FILNAM(4)
      INTEGER*4 TOTSAL(2,MAXGAM)
      CHARACTER*13 REPNAM
      INTEGER*4 LBUF(LREC*3)
      INTEGER*4 DUMMY(100) 
      LOGICAL FIRST,LOTTO
      DATA FIRST/.TRUE./
      DATA EOF/0/
C
C
      COUNT=0
      CALL COPYRITE
      CALL FASTSET(0,STAT,NUMAGT)
      CALL FASTSET(0,TABLE,TRALEN*NUMAGT*2)
      CALL FASTSET(0,TOTSAL,2*MAXGAM)
C
C OPEN TRANSACTION MASTER FILE
C
      CALL WIMG(5,'Enter TMF file name: ')
      READ(5,800) FILNAM
800   FORMAT(4A4)
      CALL OPENW(2,FILNAM,4,0,0,ST)
      CALL TOPEN(2)
      IF(ST.NE.0) THEN
        WRITE(5,901) IAM(),FILNAM,ST
        CALL GPAUSE
      ENDIF
      SER=0
C
C GET TIME LIMIT
C
      CALL INPNUM('Enter time limit in seconds [-1= NO LIMIT]',
     *            TIME,-1,100,EXT)
      IF(EXT.LT.0) STOP
C
C GET BET OPTION
C
      LOTTO=.FALSE.
      CALL WIMG(5,'Do you want just lotto games [Y/N]?')
      CALL YESNO(FLAG)
      IF(FLAG.EQ.1) LOTTO=.TRUE.
C
C SCAN TMF
C
40    CONTINUE
      SER=SER+1
      CALL RLOG(SER,LBUF,DUMMY,ST)
      IF(ST.GT.0) GOTO 40
      IF(ST.LT.0) THEN
        WRITE(5,903)IAM()
        CALL GPAUSE
      ENDIF
C
      CALL LOGTRA(TRABUF,LBUF)
      IF(TRABUF(TSTAT).EQ.NUSD) THEN
        EOF=EOF+1
        IF(EOF.GT.2000) GOTO 1000
        GOTO 40
      ENDIF
C
C OPEN REPORT FILE
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        WRITE(REPNAM,810) TRABUF(TCDC)
810     FORMAT('SCAN',I4.4,'.REP')
        CALL ROPEN(REPNAM,6,ST)
        IF(ST.NE.0) THEN
          TYPE*,IAM(),REPNAM,' open error ',ST
          CALL GPAUSE
        ENDIF
      ENDIF
C
      EOF=0
      IF(TRABUF(TINTRA).NE.0) GOTO 40   !SKIP INTERNAL TRANSACTIONS
      TER=TRABUF(TTER)
      IF(TER.LT.1.OR.TER.GT.NUMAGT) GOTO 40
C
C
      IF(STAT(TER).NE.0) THEN
        IF(TRABUF(TTYP).EQ.TSPE.AND.
     *     (TRABUF(TSFUN).EQ.1.OR.TRABUF(TSFUN).EQ.3.OR.
     *     TRABUF(TSFUN).EQ.15)) THEN
          WRITE(6,920) TABLE(TTSTCS,1,TER)
          CALL PRINTRA(TABLE(1,2,TER),6,1,.FALSE.,DUMMY,.FALSE.)
          CALL PRINTRA(TABLE(1,1,TER),6,1,.FALSE.,DUMMY,.FALSE.)
          CALL PRINTRA(TRABUF,6,1,.FALSE.,DUMMY,.FALSE.)
          COUNT=COUNT+1
          STAT(TER)=0
        ENDIF
      ENDIF
C
      IF(TRABUF(TTYP).NE.TWAG.OR.
     *  (TRABUF(TSTAT).NE.GOOD.AND.
     *   TRABUF(TSTAT).NE.VOID.AND.
     *   TRABUF(TSTAT).NE.CASH)) THEN
        TABLE(TTIM,1,TER)=0               !INVALIDATE LAST WAGER
        STAT(TER)=0
        GOTO 40
      ENDIF
C
C INCREMENT TOTAL SALES
C
      GAM=TRABUF(TGAM)
      IF(TRABUF(TSTAT).EQ.GOOD.OR.TRABUF(TSTAT).EQ.CASH) THEN
        TOTSAL(TRACNT,GAM)=TOTSAL(TRACNT,GAM)+1
        TOTSAL(DOLAMT,GAM)=TOTSAL(DOLAMT,GAM)+TRABUF(TWTOT)
      ENDIF
C
C GOOD WAGER
C
      IF(LOTTO.AND.TRABUF(TGAMTYP).NE.TLTO) GOTO 150
      IF(TRABUF(TTRN).NE.TABLE(TTRN,1,TER).AND.
     *   TABLE(TTIM,1,TER).NE.0) THEN
        DO 100 I=1,TRALEN
          IF(TIME.NE.-1) THEN
            IF((TRABUF(TTIM)-TABLE(TTIM,1,TER)).GT.TIME) GOTO 150
          ENDIF
          IF(I.EQ.TSTAT) GOTO 100        !SKIP STATUS (MAYBE CASH OR CAN)
          IF(I.EQ.TWCSER)GOTO 100        !SKIP CASH/CAN SER
          IF(I.EQ.TWCTER)GOTO 100        !SKIP CASH/CAN TER
          IF(I.EQ.TTRN)  GOTO 100        !SKIP SEQUENCE #
          IF(I.EQ.TCHK)  GOTO 100        !SKIP CHECKSUM
          IF(I.EQ.TTIM)  GOTO 100        !SKIP TIME
          IF(I.EQ.TSER)  GOTO 100        !SKIP SERIAL #
          IF(I.EQ.TWVSTS)GOTO 100        !SKIP VALIDATION STATUS
          IF(TRABUF(I).NE.TABLE(I,1,TER)) GOTO 150
100     CONTINUE
        STAT(TER)=1
        CALL FASTMOV(TABLE(1,1,TER),TABLE(1,2,TER),TRALEN)
        CALL FASTMOV(TRABUF,TABLE(1,1,TER),TRALEN)
        GOTO 40
      ENDIF
C
C SAVE CURRENT WAGER
C
150   CONTINUE
      STAT(TER)=0
      CALL FASTMOV(TRABUF(1),TABLE(1,1,TER),TRALEN)
      GOTO 40
C
C
1000  CONTINUE
      WRITE(6,900) COUNT
      DO 2000 GAM=1,MAXGAM
        IF(TOTSAL(1,GAM).EQ.0) GOTO 2000
        WRITE(6,910) GAM,TOTSAL(1,GAM),CMONY(TOTSAL(2,GAM),12,BETUNIT)
2000  CONTINUE
900   FORMAT(////,I8,' DUPLICATE TICKETS FOUND',//,
     *       ' APPROXIMATE SALES BY GAME',/)
910   FORMAT(1X,'GAME ',I2,' COUNT ',I10,' AMOUNT ',A12)
920   FORMAT(1X,'TERMINAL STATISTICS BYTE ',Z2)
      CALL USRCLOS1(2)
      CALL USRCLOS1(6)
      TYPE*,COUNT,' Duplicate wagers found'
      CALL GSTOP(GEXIT_SUCCESS)
C
901   FORMAT(1X,A,4A4,'   open error > ',Z8,' hex ')
903   FORMAT(1X,A,'TMF   read error')
      END
