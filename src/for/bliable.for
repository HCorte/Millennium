C
C SUBROUTINE BLIABLE
C  
C V04 01-Feb-2000 RXK Cleaned up.
C V03 05-Sep-1996 RXK Hardcoded condition for draw>262 replaced with check of
C                     game status 
C V02 30-Jan-1995 HXK Fix for showing non-played draws, and page breaks
C V01 24-Oct-1994 PXB Initial revision.
C  
C Subroutine to generate liability report for bingo game.
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW/EXT

        SUBROUTINE BLIABLE (BLIB,BPAY,GAMTAB)

        IMPLICIT NONE

C---- INCLUDE FILES USED.

        INCLUDE 'INCLIB:SYSPARAM.DEF / NOLIST'
        INCLUDE 'INCLIB:SYSEXTRN.DEF / NOLIST'

        INCLUDE 'INCLIB:GLOBAL.DEF / NOLIST'
        INCLUDE 'INCLIB:CONCOM.DEF / NOLIST'
        INCLUDE 'INCLIB:DBNREC.DEF / NOLIST'
        INCLUDE 'INCLIB:DATBUF.DEF / NOLIST'

        
C---- PARAMETERS USED.

        INTEGER*4  MDRAWS
        INTEGER*4  RDRAWS

        PARAMETER (MDRAWS=400)
        PARAMETER (RDRAWS=54)

C---- ARGUMENTS PASSED IN.

        INTEGER*4  BLIB(BGODIV+1,MDRAWS,NUMBGO)
        INTEGER*4  BPAY(BGODIV+1,MDRAWS,NUMBGO)
        INTEGER*4  GAMTAB(2,MAXGAM)

C---- VARIABLES USED.

        !---- INTEGER*4 VARIABLES.

        INTEGER*4     FTOTWON(BGODIV)
        INTEGER*4     FTOTPAD(BGODIV)
        INTEGER*4     FTOTLIB(BGODIV)   !---- FULLHOUSE.
        INTEGER*4     FTOTPRG(BGODIV)
        INTEGER*4     FTOTDAY(BGODIV)

        INTEGER*4     FWON(2)
        INTEGER*4     FPAD(2)
        INTEGER*4     FLIB(2)
        INTEGER*4     FPRG(2)
        INTEGER*4     FDAY(2)

        INTEGER*4     GRNWON(2)
        INTEGER*4     GRNPAD(2)
        INTEGER*4     GRNLIB(2)
        INTEGER*4     GRNPRG(2)
        INTEGER*4     GRNDAY(2)

        INTEGER*4     I 
        INTEGER*4     DRAW 
        INTEGER*4     DRWIND 
        INTEGER*4     LINCNT 
        INTEGER*4     PAGE 
        INTEGER*4     ST 
        INTEGER*4     K
        INTEGER*4     GNUM 
        INTEGER*4     GIND 
        INTEGER*4     EXT 
        INTEGER*4     COPY
        INTEGER*4     C
        INTEGER*4     Q
        INTEGER*4     W
        INTEGER*4     BFDB(7) 
        INTEGER*4     REPLU/7/
        INTEGER*4     RAPCODE
        INTEGER*4     GAMESUMS(MAXGAM,NUMFIN,NUMTOT)
        INTEGER*4     TOTSUMS(NO_BALSUMS)
        INTEGER*4     HOLDER
        INTEGER*4     FHDIVS
        INTEGER*4     DIVCNT

        !---- REAL VARIABLES.

        REAL*8    TOTREAL
        REAL*8    R_TOTDAY   !--- TOTAL OF PAID TODAY AMOUNTS OF ALL DIVISIONS,
                             !--- AND PER DRAW (LAST COLUMN OF REPORT LAYOUT)
        REAL*8    R_GRNDAY   !--- GRAND TOTAL OF PAID TODAY AMOUNTS.
 
        !---- INTEGER*2 VARIABLES.

        INTEGER*2  DATE(LDATE_LEN) /LDATE_LEN*0/

        !---- CHARACTER STRING VARIABLES.

        CHARACTER  STATUS*17
        CHARACTER  HEAD(7)*5
        CHARACTER  REPHDR*45
        CHARACTER  REPNAM*12

        CHARACTER*6  DIVHEAD(7)
        CHARACTER*13 TABLE(5,7)

        !---- DATA STATEMENT.

        DATA      HEAD/'DIV 1','DIV 2','DIV 3','DIV 4','DIV 5',
     *                 'DIV 6','TOTAL'/


        !---- FUNCTION USED.

        INTEGER*4  FRAMT

C************************* START OF PROGRAM CODE *************************

C---- INITIALIZE.

        COPY = 0

        DATE(5) = DAYCDC
        CALL LCDATE (DATE)

        DO 1000 I = 1,1

          GNUM = GTNTAB(TBNG,1)                 !--- GAME NUMBER.

          IF (GNUM .LT. 1) RETURN               !--- NO MORE ACTIVE GAMES.

          IF (DAYHDR(GNUM) .LT. 1) GOTO 1000    !--- GAME NOT ACTIVE.

          WRITE (5,8000) IAM(), (GLNAMES(K,GNUM),K=1,4)

          CALL OPENW (3,                        !--- OPEN GAME FILE.
     *                GFNAMES(1,GNUM),
     *                4,
     *                0,
     *                0,
     *                ST)

          IF (ST .NE. 0) CALL FILERR (GFNAMES(1,GNUM),1,ST,0)

          CALL IOINIT (BFDB,
     *                 3,
     *                 DBNSEC*256)

          WRITE (REPHDR,8001) (DATE(K),K=7,13)          !--- REPORT HEADER.

          WRITE (REPNAM,8002) I                         !--- REPORT NAME.

          CALL ROPEN (REPNAM,                           !--- OPEN REPORT FILE.
     *                REPLU,
     *                ST)         

          IF (ST .NE. 0) THEN                           !--- OPEN ERROR.
            TYPE*,IAM(),REPNAM,' REPORT FILE OPEN ERROR > ',ST
            CALL GPAUSE
          END IF

C---- INITIALIZE PAGE AND LINE COUNTER.

          PAGE = 0
          LINCNT = 70

C---- CLEAR GRAND TOTALS.

          CALL FASTSET (0,GRNWON,2)
          CALL FASTSET (0,GRNPAD,2)
          CALL FASTSET (0,GRNLIB,2)
          CALL FASTSET (0,GRNPRG,2)
          CALL FASTSET (0,GRNDAY,2)

          R_GRNDAY = 0.0D0

C---- LOOP THROUGH ALL ACTIVE DRAWS.

          DO 500 DRWIND = 1,RDRAWS

            DRAW = DAYHDR(GNUM) - DRWIND + 1
            IF(DRAW.LE.0) GOTO 500 

            CALL READW (BFDB,                   !--- READ DBN RECORD.
     *                  DRAW,
     *                  DBNREC,
     *                  ST)

            IF (ST .NE. 0) CALL FILERR (GFNAMES(1,GNUM),2,ST,DRAW) 
            IF(DBNSTS.LE.GAMDON) GOTO 500       

            IF (DBNUPD .GT. DAYCDC) GOTO 500    !--- LAST UPD GT CDC.

C---- CLEAR TOTALS.

            CALL FASTSET (0,FTOTWON,BGODIV)
            CALL FASTSET (0,FTOTPAD,BGODIV)
            CALL FASTSET (0,FTOTLIB,BGODIV)
            CALL FASTSET (0,FTOTPRG,BGODIV)
            CALL FASTSET (0,FTOTDAY,BGODIV)

            CALL FASTSET (0,FWON,2)
            CALL FASTSET (0,FPAD,2)
            CALL FASTSET (0,FLIB,2)
            CALL FASTSET (0,FPRG,2)
            CALL FASTSET (0,FDAY,2)

            R_TOTDAY = 0.0D0

C---- FULLHOUSE LOOP.

            DO 200 K = 1,DBNDIV(BGOFHS)
              FTOTWON(K) = FTOTWON(K) + DBNSHR(K,BGOFHS)
              FTOTLIB(K) = BLIB(K,DRWIND,I)
              FTOTPAD(K) = DBNPAD(K,BGOFHS)
              IF (DAYCDC .EQ. DBNUPD) THEN
                FTOTPAD(K) = FTOTPAD(K) - BPAY(K,DRWIND,I)
              END IF
              
              FTOTWON(K) = FTOTWON(K) * DBNSHV(K,BGOFHS)
              FTOTDAY(K) = BPAY(K,DRWIND,I)
              FTOTLIB(K) = FRAMT(MAXFRC(GNUM),FTOTLIB(K),DBNSHV(K,BGOFHS))
              FTOTPAD(K) = FRAMT(MAXFRC(GNUM),FTOTPAD(K),DBNSHV(K,BGOFHS))
              FTOTDAY(K) = FRAMT(MAXFRC(GNUM),FTOTDAY(K),DBNSHV(K,BGOFHS))
              FTOTPRG(K) = FTOTWON(K) - FTOTLIB(K) - 
     *                     FTOTPAD(K) - FTOTDAY(K)

              CALL ADDI8I4 (FWON,FTOTWON(K),VALUNIT)
              CALL ADDI8I4 (FPAD,FTOTPAD(K),VALUNIT)
              CALL ADDI8I4 (FLIB,FTOTLIB(K),VALUNIT)
              CALL ADDI8I4 (FPRG,FTOTPRG(K),VALUNIT)
              CALL ADDI8I4 (FDAY,FTOTDAY(K),VALUNIT)

              R_TOTDAY = R_TOTDAY + DFLOAT(FTOTDAY(K))

200         CONTINUE


C---- WORK OUT GRAND TOTALS.

            CALL ADDI8I8 (GRNWON,FWON,VALUNIT)
            CALL ADDI8I8 (GRNPAD,FPAD,VALUNIT)
            CALL ADDI8I8 (GRNLIB,FLIB,VALUNIT)
            CALL ADDI8I8 (GRNPRG,FPRG,VALUNIT)
            CALL ADDI8I8 (GRNDAY,FDAY,VALUNIT)

            R_GRNDAY = R_GRNDAY + R_TOTDAY

C---- WRITE REPORT.

            IF (LINCNT .GT. LINSPP) THEN
              CALL TITLE (REPHDR,REPNAM,1,REPLU,PAGE,DAYCDC)                    
              WRITE (REPLU,8004)
              LINCNT = 7
            END IF

            LINCNT = LINCNT + 7

            WRITE (REPLU,8005) DRAW

C---- NEW FULLHOUSE GAME.

              FHDIVS = 0
              DO K=1,DBNDIV(BGOFHS)        
                 IF(DBNDNR(K).NE.0)  FHDIVS = FHDIVS +1
              ENDDO
              IF(FHDIVS.EQ.0) THEN
                 TYPE*,IAM(), ' NUMBER OF FULLHOUSE DIVISIONS IS 0, ',
     *                'DRAW',DRAW
                 GOTO 500
              ENDIF
              DIVCNT = 0
              HOLDER = 1 
              DO K=1,DBNDIV(BGOFHS)        
                 IF(DBNDNR(K).NE.0) THEN
                    WRITE (DIVHEAD(HOLDER),9600) K 
                    WRITE (TABLE(1,HOLDER),9601) 
     *                     CSMONY(FTOTWON(K),13,VALUNIT)
                    WRITE (TABLE(2,HOLDER),9601) 
     *                     CSMONY(FTOTPAD(K),13,VALUNIT) 
                    WRITE (TABLE(3,HOLDER),9601) 
     *                    CSMONY(FTOTDAY(K),13,VALUNIT)
                    WRITE (TABLE(4,HOLDER),9601) 
     *                    CSMONY(FTOTPRG(K),13,VALUNIT)
                    WRITE (TABLE(5,HOLDER),9601) 
     *                     CSMONY(FTOTLIB(K),13,VALUNIT)
                    DIVCNT=DIVCNT+1 
                    HOLDER=HOLDER+1
                    IF(HOLDER.GT.6.AND.DIVCNT.LT.FHDIVS) THEN
                       WRITE(REPLU,9604) (DIVHEAD(Q),Q=1,6)
                       WRITE(REPLU,9001) (TABLE(1,Q),Q=1,6)
                       WRITE(REPLU,9002) (TABLE(2,Q),Q=1,6)
                       WRITE(REPLU,9003) (TABLE(3,Q),Q=1,6)
                       WRITE(REPLU,9004) (TABLE(4,Q),Q=1,6)
                       WRITE(REPLU,9005) (TABLE(5,Q),Q=1,6)
                       HOLDER = 1
                    ENDIF 
                 ENDIF
              ENDDO  
              WRITE(REPLU,9605) (DIVHEAD(Q),Q=1,HOLDER-1)
              WRITE (TABLE(1,HOLDER),9601) CSMONYI8(FWON,13,VALUNIT)
              WRITE (TABLE(2,HOLDER),9601) CSMONYI8(FPAD,13,VALUNIT) 
              WRITE (TABLE(3,HOLDER),9601) CSMONYI8(FDAY,13,VALUNIT)
              WRITE (TABLE(4,HOLDER),9601) CSMONYI8(FPRG,13,VALUNIT)
              WRITE (TABLE(5,HOLDER),9601) CSMONYI8(FLIB,13,VALUNIT)

              WRITE(REPLU,9606) (TABLE(1,Q),Q=1,HOLDER)
              WRITE(REPLU,9607) (TABLE(2,Q),Q=1,HOLDER)
              WRITE(REPLU,9608) (TABLE(3,Q),Q=1,HOLDER)
              WRITE(REPLU,9609) (TABLE(4,Q),Q=1,HOLDER)
              WRITE(REPLU,9610) (TABLE(5,Q),Q=1,HOLDER)


9600    FORMAT ('DIV ',I2.2)
9601    FORMAT (A13)
9604    FORMAT (1X,'TAYSKASI       ',2X,6(7X,A6,2X),/)
9605    FORMAT (/,1X,'TAYSKASI       ',2X,<HOLDER-1>(7X,A6,2X),7X,'TOTAL'/)
9606    FORMAT (1X,'TOTAL WON      ',2X,<HOLDER>(A13,2X))

9607    FORMAT (1X,'PREVIOUSLY PAID',2X,<HOLDER>(A13,2X))

9608    FORMAT (1X,'PAID TODAY     ',2X,<HOLDER>(A13,2X))

9609    FORMAT (1X,'PURGED/EXPIRED ',2X,<HOLDER>(A13,2X))

9610    FORMAT (1X,'OUTSTANDING    ',2X,<HOLDER>(A13,2X))


500       CONTINUE                              !--- END OF DRAW LOOP.

550       CONTINUE

C---- GRAND TOTALS.

          LINCNT = LINCNT + 7
          IF(LINCNT.GT.LINSPP) THEN
             CALL TITLE(REPHDR,REPNAM,1,REPLU,PAGE,DAYCDC)
             WRITE(REPLU,9000)
             LINCNT=7
          ENDIF

          WRITE (REPLU,9500) 

          WRITE (REPLU,9501) CSMONYI8(GRNWON,13,VALUNIT)

          WRITE (REPLU,9502) CSMONYI8(GRNPAD,13,VALUNIT)

          WRITE (REPLU,9503) CSMONYI8(GRNDAY,13,VALUNIT)

          WRITE (REPLU,9504) CSMONYI8(GRNPRG,13,VALUNIT)

          WRITE (REPLU,9505) CSMONYI8(GRNLIB,13,VALUNIT)


          CALL USRCLOS1(3)

          CALL USRCLOS1(REPLU)

          TOTREAL = R_GRNDAY

          RAPCODE = 80 + GNUM                !V08 
          CALL BALWRI(RAPCODE,GAMESUMS,TOTSUMS,TOTREAL)
                                                                                
          CALL SPOOL(REPNAM,COPY,STATUS)

1000    CONTINUE


C===================== FORMAT STATEMENTS =================

C---- REPORT HEADINGS.

8000    FORMAT (1X,A,' Generating ',4A4,' liability report')

8001    FORMAT ('BINGO',' LIABILITY REPORT FOR ',7A2)

8002    FORMAT ('BLIABLE',I1,'.REP')

8003    FORMAT (1X,A)

8004    FORMAT (1X,130('-'),/)

8005    FORMAT (/,1X,'DRAW ',I5,/)

C---- BINGO A B .

8500    FORMAT (1X,'RUUDUKKO A JA B',2X,3(8X,A5,2X),8X,A5,/)

8501    FORMAT (1X,'TOTAL WON      ',2X,3(A13,2X),A13)

8502    FORMAT (1X,'PREVIOUSLY PAID',2X,3(A13,2X),A13)

8503    FORMAT (1X,'PAID TODAY     ',2X,3(A13,2X),A13)

8504    FORMAT (1X,'PURGED/EXPIRED ',2X,3(A13,2X),A13)

8505    FORMAT (1X,'OUTSTANDING    ',2X,3(A13,2X),A13)

C---- FULLHOUSE.

9000    FORMAT (/,1X,'TAYSKASI       ',2X,7(8X,A5,2X),/)

9001    FORMAT (1X,'TOTAL WON      ',2X,7(A13,2X))

9002    FORMAT (1X,'PREVIOUSLY PAID',2X,7(A13,2X))

9003    FORMAT (1X,'PAID TODAY     ',2X,7(A13,2X))

9004    FORMAT (1X,'PURGED/EXPIRED ',2X,7(A13,2X))

9005    FORMAT (1X,'OUTSTANDING    ',2X,7(A13,2X))

C---- TOTALS.

9500    FORMAT (/,1X,'TOTALS DRAW ',/)

9501    FORMAT (1X,'TOTAL WON      ',2X,A13,2X)

9502    FORMAT (1X,'PREVIOUSLY PAID',2X,A13,2X)

9503    FORMAT (1X,'PAID TODAY     ',2X,A13,2X)

9504    FORMAT (1X,'PURGED/EXPIRED ',2X,A13,2X)

9505    FORMAT (1X,'OUTSTANDING    ',2X,A13,2X)


        RETURN
        END

C************************** End of Program *****************************
