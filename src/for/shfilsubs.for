C
C      SHFILSUBS.FOR
C
C
C V01 20-12-2016 SCML INITIAL RELEASE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2016 SCML. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE LTSHFIL(LTSHDATA, ST)
C
C       THIS SUBROUTINE GENERATES THE LOTTO SHARE INTERFACE FILE FOR
C       TOTOLOTO QUARTA AND TOTOLOTO SABADO.
C
C       INPUTS:
C        LTSHDATA       LOTTO SHARE DATA STRUCTURE
C
C       OUTPUTS:
C        ST             STATUS, 0=SUCCESS, ELSE FAILURE
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE LTSHFIL(LTSHDATA, ST)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:GTNAMES.DEF'
       INCLUDE 'INCLIB:LTSHFIL.DEF'
C
       INTEGER*4 ST
C
       RECORD /STCLTSHFIL/ LTSHDATA
C
       INTEGER*4    I
       CHARACTER*24 LTSHFNAM                                                    !INTERFACE FILE NAME
       INTEGER*4    LTSHFLUN                                                    !INTERFACE FILE LUN
       INTEGER*4    CTIM(2), CDAT(8)
C
       ST = -1                                                                  !ERROR STATUS
C
C=======================================================================
C      CHECK IF LTSHDATA STRUCTURE HAS BEEN LOADED
C=======================================================================
C
       IF(LTSHDATA.LTCTRFLG .EQ. 0) RETURN
C
C=======================================================================
C      CHECK LOTTO GAME INDEXES
C=======================================================================
C
       IF(LTSHDATA.LTGIND.NE.LTSAT .AND. LTSHDATA.LTGIND.NE.LTWED) RETURN
C
C=======================================================================
C      GET FILE CREATION DATE
C=======================================================================
C
       CALL ICLOCK(1,CTIM)
       CALL GDATE(CDAT(2),CDAT(3),CDAT(1))
       IF (CDAT(1) .LT. 77) THEN
         CDAT(1) = CDAT(1) + 2000
       ELSE
         CDAT(1) = CDAT(1) + 1900
       ENDIF
       WRITE(LTSHDATA.LTCRFDAT, '(I4.4,I2.2,I2.2)') CDAT(1),CDAT(2),CDAT(3)
C
C=======================================================================
C      BUILD FILE NAME
C=======================================================================
C
       WRITE (LTSHFNAM, FMT='(A6,I1,A6,I3.3,I4.4,A4)')
     *     'FILE:L',
     *     LTSHDATA.LTGIND,
     *     'SHARES',
     *     LTSHDATA.LTCCC,
     *     LTSHDATA.LTYEAR,
     *     '.ASC'
       LTSHDATA.LTSHFNAM = LTSHFNAM
       CALL DFILX(LTSHDATA.LTSHFNAM,0,0,ST)
       IF (ST.NE.0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'SHARERPT - Erro a remover versoes antigas do ficheiro: '
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         '//TRIM(LTSHDATA.LTSHFNAM(6:))
         TYPE*, IAM(), '       '
         CALL GPAUSE
       ENDIF
C
C=======================================================================
C      FIND A FREE LUN TO USE FOR LOTTO SHARES FILE
C=======================================================================
C
       CALL FIND_AVAILABLE_LUN(LTSHFLUN,ST)
       IF (ST.NE.0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'SHARERPT - Erro a obter uma LUN para o ficheiro: '
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         '//TRIM(LTSHDATA.LTSHFNAM(6:))
         TYPE*, IAM(), '       '
         CALL GPAUSE
       ENDIF
       LTSHDATA.LTSHFLUN = LTSHFLUN
C
C=======================================================================
C      OPEN THE LOTTO SHARES FILE
C=======================================================================
C
       CALL OPEN_FILASC(LTSHDATA.LTSHFNAM, LTSHDATA.LTSHFLUN, ST)
       IF (ST.NE.0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'SHARERPT - Erro a abrir o ficheiro: '
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         '//TRIM(LTSHDATA.LTSHFNAM(6:))
         TYPE*, IAM(), '       '
         !CLOSE AND DELETE CREATED FILES SO FAR
         CALL USRCLOS1(LTSHDATA.LTSHFLUN)
         CALL DFILX(LTSHDATA.LTSHFLUN,0,0,ST)
         CALL GPAUSE
       ENDIF
C
C=======================================================================
C       WRITE HEADER RECORD INTO THE FILE
C=======================================================================
C
       WRITE(UNIT=LTSHDATA.LTSHFLUN,FMT=10)
     *       LTRTYP_HDR,                                                        !HEADER RECORD TYPE
     *       LTSHDATA.LTCCC,                                                    !DRAW NUMBER
     *       LTSHDATA.LTYEAR,                                                   !DRAW YEAR
     *       CMONY(INT(LTSHDATA.LTTOTSALE), 13, BETUNIT),                       !TOTAL SALES AMOUNT
     *       CMONY(INT(LTSHDATA.LTTOTPOOL), 13, BETUNIT),                       !TOTAL SALES AMOUNT FOR PRIZE DISTRIBUTION
     *       CMONY(LTSHDATA.LTPREVJKT, 13, BETUNIT),                            !PREVIOUS JACKPOT AMOUNT
     *       LTSHDATA.LTTOTRESI,                                                !TOTAL ROUNDING AMOUNT
     *       CMONY(LTSHDATA.LTNEXTJKT, 13, BETUNIT),                            !NEXT JACKPOT AMOUNT
     *       LTSHDATA.LTLKYPOOL,                                                !TOTAL PRIZE AMOUNT POOL OF LUCKY NUMBER DIVISION
     *       LTSHDATA.LTLKYRESP,                                                !TOTAL PRIZE AMOUNT RESPONSABILITY OF LUCKY NUMBER DIVISION
     *       LTSHDATA.LTLKYDIFF                                                 !DIFFERENCE BETWEEN TOTAL PRIZE AMOUNT POOL AND TOTAL PRIZE AMOUNT RESPONSABILITY OF LUCKY NUMBER DIVISION
C
C=======================================================================
C       WRITE BODY RECORDS INTO THE FILE
C=======================================================================
C
        DO I=1,LTSHDATA.LTDIV
         WRITE(UNIT=LTSHDATA.LTSHFLUN,FMT=20)
     *       LTRTYP_BDY,                                                        !BODY RECORD TYPE
     *       LTSHDATA.LTDIVNUMB(I),                                             !PRIZE DIVISION NUMBER
     *       LTSHDATA.LTDIVNAME(I),                                             !PRIZE DIVISION NAME
     *       LTSHDATA.LTDIVPOOL(I),                                             !TOTAL POOL VALUE OF PRIZE DIVISION
     *       LTSHDATA.LTDIVPERC(I),                                             !ASSIGNED PERCENTAGE OF PRIZE DIVISION
     *       LTSHDATA.LTDIVREAL(I),                                             !TOTAL EFECTIVE VALUE OF PRIZE DIVISION
     *       LTSHDATA.LTDIVSHRQ(I),                                             !TOTAL PRIZED BETS OF PRIZE DIVISION
     *       CMONY(LTSHDATA.LTDIVSHRV(I),13,VALUNIT),                           !SHARE AMOUNT VALUE OF PRIZE DIVISION
     *       LTSHDATA.LTDIVRESI(I)                                              !TOTAL RESIDUAL VALUE OF PRIZE DIVISION
        ENDDO
C
C=======================================================================
C       WRITE TRAILER RECORD INTO THE FILE
C=======================================================================
C
       WRITE(UNIT=LTSHDATA.LTSHFLUN,FMT=30)
     *       LTRTYP_TRL,                                                        !TRAILER RECORD TYPE
     *       LTSHDATA.LTCRFDAT                                                  !LOTTO FILE CREATION DATE
C
C=======================================================================
C       CLOSE FILE
C=======================================================================
C
       CALL USRCLOS1(LTSHDATA.LTSHFLUN)
C
       ST = 0
       WRITE(5,900) IAM(),
     *              GTNAMES(LTSHDATA.LTGTYP),                                   !LOTTO GAME TYPE DESCRIPTION
     *              LTSHDATA.LTGIND                                             !LOTTO GAME INDEX
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
10     FORMAT(A2,                                                               !HEADER RECORD TYPE
     *        I3.3, I4.4,                                                       !DRAW NAME
     *        A13,                                                              !TOTAL SALES AMOUNT (WAGER UNITS)
     *        A13,                                                              !TOTAL SALES AMOUNT FOR PRIZE DISTRIBUTION
     *        A13,                                                              !PREVIOUS JACKPOT AMOUNT
     *        F13.2,                                                            !TOTAL ROUNDING AMOUNT
     *        A13,                                                              !NEXT JACKPOT AMOUNT
     *        F13.2,                                                            !TOTAL PRIZE AMOUNT POOL OF LUCKY NUMBER DIVISION
     *        F13.2,                                                            !TOTAL PRIZE AMOUNT RESPONSABILITY OF LUCKY NUMBER DIVISION
     *        F13.2)                                                            !DIFFERENCE BETWEEN TOTAL PRIZE AMOUNT POOL AND TOTAL PRIZE AMOUNT RESPONSABILITY OF LUCKY NUMBER DIVISION
C
20     FORMAT(A2,                                                               !BODY RECORD TYPE
     *        I2.2,                                                             !PRIZE DIVISION NUMBER
     *        A10,                                                              !PRIZE DIVISION NAME
     *        F13.2,                                                            !TOTAL POOL VALUE OF PRIZE DIVISION
     *        F7.2,                                                             !ASSIGNED PERCENTAGE OF PRIZE DIVISION
     *        F13.2,                                                            !TOTAL EFECTIVE VALUE OF PRIZE DIVISION
     *        I11,                                                              !TOTAL PRIZED BETS OF PRIZE DIVISION
     *        A13,                                                              !SHARE AMOUNT VALUE OF PRIZE DIVISION
     *        F13.2,                                                            !TOTAL RESIDUAL VALUE OF PRIZE DIVISION
     *        29(' '))                                                          !FILLER
C
30     FORMAT(A2,                                                               !TRAILER RECORD TYPE
     *        A8,                                                               !FILE CREATION DATE YYYYMMDD
     *        103(' '))                                                         !FILLER
C
900    FORMAT(1X,
     *        A,                                                                !TIME
     *        A8,                                                               !LOTTO GAME TYPE DESCRIPTION
     *        I1,                                                               !LOTTO GAME INDEX
     *        ' share interface file complete')
C
       RETURN
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE KISHFIL(KISHDATA, ST)
C
C       THIS SUBROUTINE GENERATES THE KICKER SHARE INTERFACE FILE.
C
C       INPUTS:
C        KISHDATA       KICKER SHARE DATA STRUCTURE
C
C       OUTPUTS:
C        ST             STATUS, 0=SUCCESS, ELSE FAILURE
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE KISHFIL(KISHDATA, ST)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:GTNAMES.DEF'
       INCLUDE 'INCLIB:KISHFIL.DEF'
C
       INTEGER*4 ST
C
       RECORD /STCKISHFIL/ KISHDATA
C
       INTEGER*4    I
       CHARACTER*24 KISHFNAM                                                    !INTERFACE FILE NAME
       INTEGER*4    KISHFLUN                                                    !INTERFACE FILE LUN
       INTEGER*4    CTIM(2), CDAT(8)
C
       ST = -1                                                                  !ERROR STATUS
C
C=======================================================================
C      CHECK IF KISHDATA STRUCTURE HAS BEEN LOADED
C=======================================================================
C
       IF(KISHDATA.KICTRFLG .EQ. 0) RETURN
C
C=======================================================================
C      CHECK KICKER GAME INDEXES
C=======================================================================
C
       IF(KISHDATA.KIGIND.NE.JOK1) RETURN
C
C=======================================================================
C      GET FILE CREATION DATE
C=======================================================================
C
       CALL ICLOCK(1,CTIM)
       CALL GDATE(CDAT(2),CDAT(3),CDAT(1))
       IF (CDAT(1) .LT. 77) THEN
         CDAT(1) = CDAT(1) + 2000
       ELSE
         CDAT(1) = CDAT(1) + 1900
       ENDIF
       WRITE(KISHDATA.KICRFDAT, '(I4.4,I2.2,I2.2)') CDAT(1),CDAT(2),CDAT(3)
C
C=======================================================================
C      BUILD FILE NAME
C=======================================================================
C
       WRITE (KISHFNAM, FMT='(A6,I1,A6,I3.3,I4.4,A4)')
     *     'FILE:J',
     *     KISHDATA.KIGIND,
     *     'SHARES',
     *     KISHDATA.KICCC,
     *     KISHDATA.KIYEAR,
     *     '.ASC'
       KISHDATA.KISHFNAM = KISHFNAM
       CALL DFILX(KISHDATA.KISHFNAM,0,0,ST)
       IF (ST.NE.0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'SHARERPT - Erro a remover versoes antigas do ficheiro: '
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         '//TRIM(KISHDATA.KISHFNAM(6:))
         TYPE*, IAM(), '       '
         CALL GPAUSE
       ENDIF
C
C=======================================================================
C      FIND A FREE LUN TO USE FOR LOTTO SHARES FILE
C=======================================================================
C
       CALL FIND_AVAILABLE_LUN(KISHFLUN,ST)
       IF (ST.NE.0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'SHARERPT - Erro a obter uma LUN para o ficheiro: '
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         '//TRIM(KISHDATA.KISHFNAM(6:))
         TYPE*, IAM(), '       '
         CALL GPAUSE
       ENDIF
       KISHDATA.KISHFLUN = KISHFLUN
C
C=======================================================================
C      OPEN THE LOTTO SHARES FILE
C=======================================================================
C
       CALL OPEN_FILASC(KISHDATA.KISHFNAM, KISHDATA.KISHFLUN, ST)
       IF (ST.NE.0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'SHARERPT - Erro a abrir o ficheiro: '
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         '//TRIM(KISHDATA.KISHFNAM(6:))
         TYPE*, IAM(), '       '
         !CLOSE AND DELETE CREATED FILES SO FAR
         CALL USRCLOS1(KISHDATA.KISHFLUN)
         CALL DFILX(KISHDATA.KISHFLUN,0,0,ST)
         CALL GPAUSE
       ENDIF
C
C=======================================================================
C       WRITE HEADER RECORD INTO THE FILE
C=======================================================================
C
       WRITE(UNIT=KISHDATA.KISHFLUN,FMT=10)
     *       KIRTYP_HDR,                                                        !HEADER RECORD TYPE
     *       KISHDATA.KICCC,                                                    !DRAW NUMBER
     *       KISHDATA.KIYEAR,                                                   !DRAW YEAR
     *       CMONY(INT(KISHDATA.KITOTSALE), 13, BETUNIT),                       !TOTAL SALES AMOUNT
     *       CMONY(INT(KISHDATA.KITOTPOOL), 13, BETUNIT),                       !TOTAL SALES AMOUNT FOR PRIZE DISTRIBUTION
     *       CMONY(IDINT(KISHDATA.KIPREVJKT), 13, BETUNIT),                     !PREVIOUS JACKPOT AMOUNT
     *       CMONY(KISHDATA.KITOTRESI, 13, BETUNIT)                             !TOTAL ROUNDING AMOUNT
C
C=======================================================================
C       WRITE BODY RECORDS INTO THE FILE
C=======================================================================
C
        DO I=1,KISHDATA.KIDIV
         WRITE(UNIT=KISHDATA.KISHFLUN,FMT=20)
     *       KIRTYP_BDY,                                                        !BODY RECORD TYPE
     *       KISHDATA.KIDIVNUMB(I),                                             !PRIZE DIVISION NUMBER
     *       KISHDATA.KIDIVNAME(I),                                             !PRIZE DIVISION NAME
     *       KISHDATA.KIDIVPOOL(I),                                             !TOTAL POOL VALUE OF PRIZE DIVISION
     *       KISHDATA.KIDIVPERC(I),                                             !ASSIGNED PERCENTAGE OF PRIZE DIVISION
     *       KISHDATA.KIDIVSHRQ(I),                                             !TOTAL PRIZED BETS OF PRIZE DIVISION
     *       KISHDATA.KIDIVSHRV(I)                                              !SHARE AMOUNT VALUE OF PRIZE DIVISION
        ENDDO
C
C=======================================================================
C       WRITE TRAILER RECORD INTO THE FILE
C=======================================================================
C
       WRITE(UNIT=KISHDATA.KISHFLUN,FMT=30)
     *       KIRTYP_TRL,                                                        !TRAILER RECORD TYPE
     *       KISHDATA.KICRFDAT                                                  !LOTTO FILE CREATION DATE
C
C=======================================================================
C       CLOSE FILE
C=======================================================================
C
       CALL USRCLOS1(KISHDATA.KISHFLUN)
C
       ST = 0
       WRITE(5,900) IAM(),
     *              GTNAMES(KISHDATA.KIGTYP),                                   !LOTTO GAME TYPE DESCRIPTION
     *              KISHDATA.KIGIND                                             !LOTTO GAME INDEX
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
10     FORMAT(A2,                                                               !HEADER RECORD TYPE
     *        I3.3, I4.4,                                                       !DRAW NAME
     *        A13,                                                              !TOTAL SALES AMOUNT (WAGER UNITS)
     *        A13,                                                              !TOTAL SALES AMOUNT FOR PRIZE DISTRIBUTION
     *        A13,                                                              !PREVIOUS JACKPOT AMOUNT
     *        A13)                                                              !TOTAL ROUNDING AMOUNT
C
20     FORMAT(A2,                                                               !BODY RECORD TYPE
     *        I2.2,                                                             !PRIZE DIVISION NUMBER
     *        A10,                                                              !PRIZE DIVISION NAME
     *        F13.2,                                                            !TOTAL POOL VALUE OF PRIZE DIVISION
     *        F7.2,                                                             !ASSIGNED PERCENTAGE OF PRIZE DIVISION
     *        I11,                                                              !TOTAL PRIZED BETS OF PRIZE DIVISION
     *        F13.2,                                                            !SHARE AMOUNT VALUE OF PRIZE DIVISION
     *        3(' '))                                                           !FILLER
C
30     FORMAT(A2,                                                               !TRAILER RECORD TYPE
     *        A8,                                                               !FILE CREATION DATE YYYYMMDD
     *        51(' '))                                                          !FILLER
C
900    FORMAT(1X,
     *        A,                                                                !TIME
     *        A8,                                                               !LOTTO GAME TYPE DESCRIPTION
     *        I1,                                                               !LOTTO GAME INDEX
     *        ' share interface file complete')
C
       RETURN
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE SPSHFIL(SPSHDATA, ST)
C
C       THIS SUBROUTINE GENERATES THE SPORTS SHARE INTERFACE FILE FOR
C       TOTOBOLA NORMAL AND TOTOBOLA EXTRA 1.
C
C       INPUTS:
C        SPSHDATA       SPORTS SHARE DATA STRUCTURE
C
C       OUTPUTS:
C        ST             STATUS, 0=SUCCESS, ELSE FAILURE
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE SPSHFIL(SPSHDATA, ST)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:GTNAMES.DEF'
       INCLUDE 'INCLIB:SPSHFIL.DEF'
C
       INTEGER*4 ST
C
       RECORD /STCSPSHFIL/ SPSHDATA
C
       INTEGER*4    I
       CHARACTER*24 SPSHFNAM                                                    !INTERFACE FILE NAME
       INTEGER*4    SPSHFLUN                                                    !INTERFACE FILE LUN
       INTEGER*4    CTIM(2), CDAT(8)
C
       ST = -1                                                                  !ERROR STATUS
C
C=======================================================================
C      CHECK IF SPSHDATA STRUCTURE HAS BEEN LOADED
C=======================================================================
C
       IF(SPSHDATA.SPCTRFLG .EQ. 0) RETURN
C
C=======================================================================
C      CHECK SPORTS GAME INDEXES
C=======================================================================
C
       IF(SPSHDATA.SPGIND.NE.SPTBN .AND. SPSHDATA.SPGIND.NE.SPTBE) RETURN
C
C=======================================================================
C      GET FILE CREATION DATE
C=======================================================================
C
       CALL ICLOCK(1,CTIM)
       CALL GDATE(CDAT(2),CDAT(3),CDAT(1))
       IF (CDAT(1) .LT. 77) THEN
         CDAT(1) = CDAT(1) + 2000
       ELSE
         CDAT(1) = CDAT(1) + 1900
       ENDIF
       WRITE(SPSHDATA.SPCRFDAT, '(I4.4,I2.2,I2.2)') CDAT(1),CDAT(2),CDAT(3)
C
C=======================================================================
C      BUILD FILE NAME
C=======================================================================
C
       WRITE (SPSHFNAM, FMT='(A6,I1,A6,I3.3,I4.4,A4)')
     *     'FILE:S',
     *     SPSHDATA.SPGIND,
     *     'SHARES',
     *     SPSHDATA.SPCCC,
     *     SPSHDATA.SPYEAR,
     *     '.ASC'
       SPSHDATA.SPSHFNAM = SPSHFNAM
       CALL DFILX(SPSHDATA.SPSHFNAM,0,0,ST)
       IF (ST.NE.0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'SHARERPT - Erro a remover versoes antigas do ficheiro: '
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         '//TRIM(SPSHDATA.SPSHFNAM(6:))
         TYPE*, IAM(), '       '
         CALL GPAUSE
       ENDIF
C
C=======================================================================
C      FIND A FREE LUN TO USE FOR LOTTO SHARES FILE
C=======================================================================
C
       CALL FIND_AVAILABLE_LUN(SPSHFLUN,ST)
       IF (ST.NE.0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'SHARERPT - Erro a obter uma LUN para o ficheiro: '
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         '//TRIM(SPSHDATA.SPSHFNAM(6:))
         TYPE*, IAM(), '       '
         CALL GPAUSE
       ENDIF
       SPSHDATA.SPSHFLUN = SPSHFLUN
C
C=======================================================================
C      OPEN THE LOTTO SHARES FILE
C=======================================================================
C
       CALL OPEN_FILASC(SPSHDATA.SPSHFNAM, SPSHDATA.SPSHFLUN, ST)
       IF (ST.NE.0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'SHARERPT - Erro a abrir o ficheiro: '
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         '//TRIM(SPSHDATA.SPSHFNAM(6:))
         TYPE*, IAM(), '       '
         !CLOSE AND DELETE CREATED FILES SO FAR
         CALL USRCLOS1(SPSHDATA.SPSHFLUN)
         CALL DFILX(SPSHDATA.SPSHFLUN,0,0,ST)
         CALL GPAUSE
       ENDIF
C
C=======================================================================
C       WRITE HEADER RECORD INTO THE FILE
C=======================================================================
C
       WRITE(UNIT=SPSHDATA.SPSHFLUN,FMT=10)
     *       SPRTYP_HDR,                                                        !HEADER RECORD TYPE
     *       SPSHDATA.SPCCC,                                                    !DRAW NUMBER
     *       SPSHDATA.SPYEAR,                                                   !DRAW YEAR
     *       CMONY(SPSHDATA.SPTOTSALE, 13, BETUNIT),                            !TOTAL SALES AMOUNT
     *       CMONY(SPSHDATA.SPTOTONPZ, 13, BETUNIT),                            !TOTAL ONLINE PRIZE AMOUNT FOR PRIZE DISTRIBUTION
     *       CMONY(SPSHDATA.SPPREVJKT, 13, BETUNIT),                            !PREVIOUS JACKPOT AMOUNT
     *       CMONY(SPSHDATA.SPEXTRJKT, 13, BETUNIT),                            !JACKPOT AMOUNT FROM EXTRA
     *       CMONY(SPSHDATA.SPTOTPOOL, 13, BETUNIT),                            !TOTAL POOL AMOUNT FOR PRIZE DISTRIBUTION
     *       CMONY(SPSHDATA.SPPREVTOT, 13, BETUNIT),                            !PREVIOUS TOTAL AMOUNT
     *       CMONY(SPSHDATA.SPNEXTJKT, 13, BETUNIT),                            !SAVE NEXT JACKPOT AMOUNT
     *       CMONY(SPSHDATA.SPTOTRESI, 13, BETUNIT)                             !TOTAL ROUNDING AMOUNT
C
C=======================================================================
C       WRITE BODY RECORDS INTO THE FILE
C=======================================================================
C
        DO I=1,SPSHDATA.SPDIV
         WRITE(UNIT=SPSHDATA.SPSHFLUN,FMT=20)
     *       SPRTYP_BDY,                                                        !BODY RECORD TYPE
     *       SPSHDATA.SPDIVNUMB(I),                                             !NUMBER OF EACH PRIZE DIVISION
     *       CMONY(SPSHDATA.SPDIVPOOL(I), 13, BETUNIT),                         !TOTAL POOL VALUE OF EACH PRIZE DIVISION
     *       CMONY(SPSHDATA.SPDIVPRJK(I), 13, BETUNIT),                         !PREVIOUS JACKPOT AMOUNT OF EACH PRIZE DIVISION
     *       CMONY(SPSHDATA.SPDIVEXJK(I), 13, BETUNIT),                         !EXTRA JACKPOT AMOUNT OF EACH PRIZE DIVISION
     *       CMONY(SPSHDATA.SPDIVRESI(I), 13, BETUNIT),                         !TOTAL RESIDUAL VALUE OF EACH PRIZE DIVISION
     *       DISPER(SPSHDATA.SPDIVPERC(I)),                                     !ASSIGNED PERCENTAGE OF EACH PRIZE DIVISION
     *       CMONY(SPSHDATA.SPDIVTOTV(I), 13, BETUNIT),                         !NUMBER OF EACH PRIZE DIVISION
     *       CMONY(SPSHDATA.SPDIVPRVV(I), 13, BETUNIT),                         !PREVIOUS PRIZE VALUE OF EACH PRIZE DIVISION
     *       SPSHDATA.SPDIVSHRQ(I),                                             !TOTAL PRIZED BETS OF EACH PRIZE DIVISION
     *       CMONY(SPSHDATA.SPDIVSHRV(I), 13, BETUNIT),                         !SHARE AMOUNT VALUE OF EACH PRIZE DIVISION
     *       CMONY(SPSHDATA.SPDIVNXJK(I), 13, BETUNIT)                          !NEXT JACKPOT AMOUNT OF EACH PRIZE DIVISION
        ENDDO
C
C=======================================================================
C       WRITE TRAILER RECORD INTO THE FILE
C=======================================================================
C
       WRITE(UNIT=SPSHDATA.SPSHFLUN,FMT=30)
     *       SPRTYP_TRL,                                                        !TRAILER RECORD TYPE
     *       SPSHDATA.SPCRFDAT                                                  !SPORTS FILE CREATION DATE
C
C=======================================================================
C       CLOSE FILE
C=======================================================================
C
       CALL USRCLOS1(SPSHDATA.SPSHFLUN)
C
       ST = 0
       WRITE(5,900) IAM(),
     *              GTNAMES(SPSHDATA.SPGTYP),                                   !SPORTS GAME TYPE DESCRIPTION
     *              SPSHDATA.SPGIND                                             !SPORTS GAME INDEX
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
10     FORMAT(A2,                                                               !HEADER RECORD TYPE
     *        I3.3, I4.4,                                                       !DRAW NAME
     *        A13,                                                              !TOTAL SALES AMOUNT (WAGER UNITS)
     *        A13,                                                              !TOTAL ONLINE PRIZE AMOUNT FOR PRIZE DISTRIBUTION
     *        A13,                                                              !PREVIOUS JACKPOT AMOUNT
     *        A13,                                                              !JACKPOT AMOUNT FROM EXTRA
     *        A13,                                                              !TOTAL POOL AMOUNT FOR PRIZE DISTRIBUTION
     *        A13,                                                              !PREVIOUS TOTAL AMOUNT
     *        A13,                                                              !SAVE NEXT JACKPOT AMOUNT
     *        A13,                                                              !TOTAL ROUNDING AMOUNT
     *        14(' '))                                                          !FILLER
C
20     FORMAT(A2,                                                               !BODY RECORD TYPE
     *        I2.2,                                                             !PRIZE DIVISION NUMBER
     *        A13,                                                              !TOTAL POOL VALUE OF EACH PRIZE DIVISION
     *        A13,                                                              !PREVIOUS JACKPOT AMOUNT OF EACH PRIZE DIVISION
     *        A13,                                                              !EXTRA JACKPOT AMOUNT OF EACH PRIZE DIVISION
     *        A13,                                                              !TOTAL RESIDUAL VALUE OF EACH PRIZE DIVISION
     *        F8.3,                                                             !ASSIGNED PERCENTAGE OF EACH PRIZE DIVISION
     *        A13,                                                              !NUMBER OF EACH PRIZE DIVISION
     *        A13,                                                              !PREVIOUS PRIZE VALUE OF EACH PRIZE DIVISION
     *        I11,                                                              !TOTAL PRIZED BETS OF EACH PRIZE DIVISION
     *        A13,                                                              !SHARE AMOUNT VALUE OF EACH PRIZE DIVISION
     *        A13)                                                              !NEXT JACKPOT AMOUNT OF EACH PRIZE DIVISION
C
30     FORMAT(A2,                                                               !TRAILER RECORD TYPE
     *        A8,                                                               !FILE CREATION DATE YYYYMMDD
     *        117(' '))                                                         !FILLER
C
900    FORMAT(1X,
     *        A,                                                                !TIME
     *        A8,                                                               !SPORTS GAME TYPE DESCRIPTION
     *        I1,                                                               !SPORTS GAME INDEX
     *        ' share interface file complete')
C
       RETURN
       END
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE OPEN_FILASC (REPFILNAM,REP_LUN,ST)
       IMPLICIT NONE
C=======================================================================
C
       INTEGER*4 ST, REP_LUN
       CHARACTER*(*) REPFILNAM
C
       OPEN (UNIT       = REP_LUN,
     *       FILE       = REPFILNAM,
     *       IOSTAT     = ST,
     *       FORM       = 'FORMATTED',
     *       RECL       = 256,
     *       STATUS     = 'NEW',
     *       RECORDTYPE = 'STREAM_CR')
C
       RETURN
C
       END
