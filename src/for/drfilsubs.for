C
C      DRFILSUBS.FOR
C
C
C V01 06-JAN-2017 SCML INITIAL RELEASE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2017 SCML. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE GMDRDATA(GMDRDATA, ST)
C
C       THIS SUBROUTINE GENERATES THE DRAW INTERFACE FILE OF THE
C       FOLLOWING GAMES:
C
C               TOTOLOTO QUARTA
C               TOTOLOTO SABADO
C               TOTOBOLA NORMAL
C               TOTOBOLA EXTRA 1
C               JOKER
C
C       INPUTS:
C        GMDRDATA       DRAW DATA STRUCTURE
C
C       OUTPUTS:
C        ST             STATUS, 0=SUCCESS, ELSE FAILURE
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE GMDRFIL(GMDRDATA, ST)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:CONCOM.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:GMDRFIL.DEF'
C
       INTEGER*4 ST
C
       CHARACTER*32 MY_CMONYI8                                                  !COPIED FROM RELCONC.FOR
C
       RECORD /STCGMDRFIL/ GMDRDATA
C
       INTEGER*4    I
       CHARACTER*32 DRFNAM                                                      !INTERFACE FILE NAME
       INTEGER*4    DRFLUN                                                      !INTERFACE FILE LUN
       INTEGER*4    CTIM(2), CDAT(8)
       CHARACTER*1  FILPREF                                                     !FILE PREFIX
       CHARACTER*8  DRWDESC                                                     !DRAW DESCRIPTION
C
       ST = -1                                                                  !ERROR STATUS
C
C=======================================================================
C      CHECK GAME TYPE/INDEX
C=======================================================================
C
       IF(GMDRDATA.GTYP.EQ.TLTO) THEN
         IF(GMDRDATA.GIND.NE.LTSAT .AND. GMDRDATA.GIND.NE.LTWED) RETURN
         FILPREF = LTFILPREF
         DRWDESC = LTDRWDESC
         GOTO 10
       ENDIF
       IF(GMDRDATA.GTYP.EQ.TSPT) THEN
         IF(GMDRDATA.GIND.NE.SPTBN .AND. GMDRDATA.GIND.NE.SPTBE) RETURN
         FILPREF = SPFILPREF
         DRWDESC = SPDRWDESC
         GOTO 10
       ENDIF
       IF(GMDRDATA.GTYP.EQ.TKIK) THEN
         IF(GMDRDATA.GIND.NE.JOK1) RETURN
         FILPREF = KIFILPREF
         DRWDESC = KIDRWDESC
         GOTO 10
       ENDIF
C
       RETURN
C
C=======================================================================
C      GET FILE CREATION DATE
C=======================================================================
C
10     CONTINUE
       WRITE (5,9000) IAM(), 
     *                TRIM(DRWDESC),                                            !DRAW DESCRIPTION
     *                GMDRDATA.DRCCC,                                           !DRAW NUMBER
     *                GMDRDATA.DRYEAR,                                          !DRAW YEAR
     *                (GLNAMES(I,GMDRDATA.GNUM),I=1,4)                          !GAME LONG NAME
       CALL ICLOCK(1,CTIM)
       CALL GDATE(CDAT(2),CDAT(3),CDAT(1))
       IF (CDAT(1) .LT. 77) THEN
         CDAT(1) = CDAT(1) + 2000
       ELSE
         CDAT(1) = CDAT(1) + 1900
       ENDIF
       WRITE(GMDRDATA.CRFDAT, '(I4.4,I2.2,I2.2)') CDAT(1),CDAT(2),CDAT(3)
C
C=======================================================================
C      BUILD FILE NAME
C=======================================================================
C
       WRITE (DRFNAM, FMT='(A5,A,I1,A4,I3.3,I4.4,A4)')
     *     'FILE:',
     *     FILPREF,
     *     GMDRDATA.GIND,
     *     'CONC',
     *     GMDRDATA.DRCCC,
     *     GMDRDATA.DRYEAR,
     *     '.ASC'
       GMDRDATA.DRFNAM = DRFNAM
       CALL DFILX(GMDRDATA.DRFNAM,0,0,ST)
       IF (ST.NE.0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'RELCONC - Erro a remover versoes antigas do ficheiro: '
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         '//TRIM(GMDRDATA.DRFNAM(6:))
         TYPE*, IAM(), '       '
         CALL GPAUSE
       ENDIF
C
C=======================================================================
C      FIND A FREE LUN TO USE FOR LOTTO DRAW FILE
C=======================================================================
C
       CALL FIND_AVAILABLE_LUN(DRFLUN,ST)
       IF (ST.NE.0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'RELCONC - Erro a obter uma LUN para o ficheiro: '
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         '//TRIM(GMDRDATA.DRFNAM(6:))
         TYPE*, IAM(), '       '
         CALL GPAUSE
       ENDIF
       GMDRDATA.DRFLUN = DRFLUN
C
C=======================================================================
C      OPEN THE LOTTO DRAW FILE
C=======================================================================
C
       CALL OPEN_FILASC(GMDRDATA.DRFNAM, GMDRDATA.DRFLUN, ST)
       IF (ST.NE.0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'RELCONC - Erro a abrir o ficheiro: '
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         '//TRIM(GMDRDATA.DRFNAM(6:))
         TYPE*, IAM(), '       '
         !CLOSE AND DELETE CREATED FILES SO FAR
         CALL USRCLOS1(GMDRDATA.DRFLUN)
         CALL DFILX(GMDRDATA.DRFLUN,0,0,ST)
         CALL GPAUSE
       ENDIF
C
C=======================================================================
C       WRITE HEADER RECORD INTO THE FILE
C=======================================================================
C
       WRITE(UNIT=GMDRDATA.DRFLUN,FMT=1000)
     *       RTYP_HDR,                                                          !HEADER RECORD TYPE
     *       GMDRDATA.DRCCC,                                                    !DRAW NUMBER
     *       GMDRDATA.DRYEAR,                                                   !DRAW YEAR
     *       GMDRDATA.TOTTCKT,                                                  !TOTAL VALID TICKETS
     *       MY_CMONYI8(GMDRDATA.TOTSALE, 13, BETUNIT),                         !TOTAL SALES AMOUNT
     *       GMDRDATA.TOTPOOL,                                                  !TOTAL NET PRIZE AMOUNT FOR DISTRIBUTION (REAL*8)
     *       DISPER(GMDRDATA.PYTPERC),                                          !PAYOUT PERCENTAGE
     *       CMONY(GMDRDATA.PREVJKT, 13, BETUNIT)                               !PREVIOUS JACKPOT AMOUNT
C
C=======================================================================
C       WRITE BODY RECORDS INTO THE FILE
C=======================================================================
C
        DO I=1,GMDRDATA.GDIV
         WRITE(UNIT=GMDRDATA.DRFLUN,FMT=1010)
     *       RTYP_BDY,                                                          !BODY RECORD TYPE
     *       GMDRDATA.DIVNUMB(I),                                               !NUMBER OF EACH PRIZE DIVISION
     *       DISPER(GMDRDATA.DIVPERC(I)),                                       !ASSIGNED PERCENTAGE OF EACH PRIZE DIVISION
     *       MY_CMONYI8(GMDRDATA.DIVPOOL(I), 13, BETUNIT)                       !TOTAL POOL VALUE OF EACH PRIZE DIVISION
        ENDDO
C
C=======================================================================
C       WRITE TRAILER RECORD INTO THE FILE
C=======================================================================
C
       WRITE(UNIT=GMDRDATA.DRFLUN,FMT=1020)
     *       RTYP_TRL,                                                          !TRAILER RECORD TYPE
     *       GMDRDATA.CRFDAT                                                    !LOTTO FILE CREATION DATE
C
C=======================================================================
C       CLOSE FILE
C=======================================================================
C
       CALL USRCLOS1(GMDRDATA.DRFLUN)
C
       ST = 0
       WRITE(5,9090) IAM(), TRIM(GMDRDATA.DRFNAM)
     *               
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
1000   FORMAT(A2,                                                               !HEADER RECORD TYPE
     *        I3.3, I4.4,                                                       !DRAW NAME
     *        I11,                                                              !TOTAL VALID TICKETS
     *        A13,                                                              !TOTAL SALES AMOUNT (WAGER UNITS)
     *        F13.2,                                                            !TOTAL NET PRIZE AMOUNT FOR DISTRIBUTION (REAL*8)
     *        F7.2,                                                             !PAYOUT PERCENTAGE
     *        A13)                                                              !PREVIOUS JACKPOT AMOUNT
C
1010   FORMAT(A2,                                                               !BODY RECORD TYPE
     *        I2.2,                                                             !NUMBER OF EACH PRIZE DIVISION
     *        F7.2,                                                             !ASSIGNED PERCENTAGE OF EACH PRIZE DIVISION
     *        A13,                                                              !TOTAL POOL VALUE OF EACH PRIZE DIVISION
     *        42(' '))                                                          !FILLER
C
1020   FORMAT(A2,                                                               !TRAILER RECORD TYPE
     *        A8,                                                               !FILE CREATION DATE YYYYMMDD
     *        56(' '))                                                          !FILLER
C
9000   FORMAT(1X,
     *        A,                                                                !TIME
     *        1X,
     *        'A gerar ficheiro de interface do ',
     *        A,                                                                !DRAW DESCRIPTION
     *        1X,
     *        I3.3,'/',I4.4,                                                    !DRAW NUMBER/DRAW YEAR
     *        1X, 
     *        4A4)                                                              !GAME LONG NAME
C
9090   FORMAT(1X,
     *        A,                                                                !TIME
     *        ' Ficheiro de interface ',
     *        A,                                                                !FILE NAME
     *        ' gerado com sucesso')
C
       RETURN
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE OPEN_FILASC(LTDRDATA, ST)
C
C       THIS SUBROUTINE OPENS AN ASCII FILE.
C
C       INPUTS:
C        FILNAM         FILE NAME TO OPEN
C        FLUN           FILE LOGICAL UNIT
C
C       OUTPUTS:
C        ST             IO STATUS
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE OPEN_FILASC(FILNAM, FLUN, ST)
       IMPLICIT NONE
C=======================================================================
C
       INTEGER*4 ST, FLUN
       CHARACTER*(*) FILNAM
C
       OPEN (UNIT       = FLUN,
     *       FILE       = FILNAM,
     *       IOSTAT     = ST,
     *       FORM       = 'FORMATTED',
     *       RECL       = 256,
     *       STATUS     = 'NEW',
     *       RECORDTYPE = 'STREAM_CR')
C
       RETURN
C
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       FUNCTION MY_CMONYI8(AMOUNT, LEN, FACTOR)
C
C       THIS FUNCTION FORMATS I*8 MONETARY VALUE RIGHT ALIGNED.
C       IT WAS COPIED FROM RELCONC.FOR.
C
C       INPUTS:
C        AMOUNT         I*8 MONETARY VALUE TO FORMAT
C        LEN            LENGTH OF FORMATTED OUTPUT
C        FACTOR         DECIMAL UNITS (NUMBER OF DIGITS TO THE RIGHT OF
C                                      THE DECIMAL POINT)
C
C       OUTPUTS:
C        MY_CMONYI8     MONETARY VALUE FORMATTED
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       CHARACTER*32 FUNCTION MY_CMONYI8(AMOUNT, LEN, FACTOR)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
C
C      FUNCTION PARAMETERS
C
       INTEGER*8 AMOUNT
       INTEGER*4 LEN
       INTEGER*4 FACTOR
C
C      LOCAL VARIABLES
C
       INTEGER*4 AMTAUX(2)
C
       AMTAUX(1) = AMOUNT/DYN_BETUNIT
       AMTAUX(2) = MOD(AMOUNT, DYN_BETUNIT)
C
       MY_CMONYI8 = CSMONYI8(AMTAUX, LEN, FACTOR)
C
       RETURN
       END
