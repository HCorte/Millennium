C
C      LOPPAS.FOR
C
C      PROGRAM LOPPAS
C
C      THIS PROGRAM GENERATES AN INTERFACE FILE FOR PJMC AND LN ORACLE
C      SYSTEMS. THIS FILE CONTAINS THE PRIZES OF A SPECIFIC EMISSION
C      OF THE FOLLOWING  GAMES:
C
C                             CLASSICA
C                             POPULAR
C
C      GENERATED FILE:
C
C           PJMC_LOP_<GN><EEYYYY>.ASC (PJMC INTERFACE FILE)
C
C      LEGEND:
C
C           GN       - GAME NUMBER
C           EE       - EMISSION NUMBER
C           YYYY     - DRAW YEAR
C
C               GAME NAME      GN
C           ----------------   --
C           CLASSICA           08
C           POPULAR            09
C
C
C V01 16-FEB-2015 SCML INITIAL RELEASE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2015 DJ - SCML. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C====== OPTIONS /CHECK=NOOVERFLOW
       PROGRAM LOPPAS
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:LOPPAS.DEF'
C
       INTEGER*4 I, ST
       INTEGER*4 GNUM, GTYP, GAM, GIND
C
       CHARACTER*16 GAMLNAM(MAXGAM)
C
       RECORD /STCLOPPAS/ LOPDATA
       INTEGER*4 EXTNUM, EXTYEAR
C
C=======================================================================
C       PRINT USER INTERFACE INTO SCREEN
C=======================================================================
C
       CALL CLRSCR(5)
       TYPE*,IAM(),' '
       TYPE*,IAM(),'-----------------------------------------------------------'
       TYPE*,IAM(),'<<<<< LOPPAS - LISTA OFICIAL DE PRÉMIOS LN            >>>>>'
       TYPE*,IAM(),' '
       TYPE*,IAM(),'        GERA PJMC_LOP_<GN><EEAAAA>.ASC                     '
       TYPE*,IAM(),' '
       TYPE*,IAM(),'        GN = número do jogo                                '
       TYPE*,IAM(),'        EE = número da extração                           '
       TYPE*,IAM(),'        AAAA = ano da extração                            '
       TYPE*,IAM(),'-----------------------------------------------------------'
C
       TYPE*,IAM(),'-----------------------------------------------------------'
C
100    CONTINUE
C
       CALL FASTSET(0, LOPDATA, SIZEOF(LOPDATA)/4) ! CLEAR DATA STRUCT OF LOPPAS
C
       TYPE*, IAM(), '       '
       CALL GAME_TYPNDX(GNUM, GTYP, GIND, GAMLNAM, ST)
       IF (ST .NE. 0) THEN
         TYPE*, IAM(),'       '
         CALL GSTOP(GEXIT_OPABORT)
       ENDIF
C
       IF (GTYP .EQ. TPAS) THEN
         LOPDATA.GTYP = GTYP
         IF (GNUM .EQ. 8 .OR. GNUM .EQ. 9) THEN
           LOPDATA.GNUM = GNUM
           LOPDATA.GLNAM = GAMLNAM(GNUM)
C
           IF (LOPDATA.GNUM .EQ. 8) LOPDATA.INDPAS = PSBCLA
           IF (LOPDATA.GNUM .EQ. 9) LOPDATA.INDPAS = PSBPOP
C
           CALL INQPASDRAW(EXTNUM,EXTYEAR,ST)
           IF (ST .NE. 0) GOTO 100
C
           LOPDATA.EXTNUM = EXTNUM
           LOPDATA.EXTYEAR = EXTYEAR
           WRITE(LOPDATA.EXTNAM,FMT='(I2.2,I4.4)') LOPDATA.EXTNUM,LOPDATA.EXTYEAR
C
           CALL VALPASDRAW(LOPDATA,ST)
           IF (ST .NE. 0) GOTO 100
C
           IF (LOPDATA.INDPAS .EQ. PSBCLA) THEN
             CALL GENLOPCLA(LOPDATA, ST)
           ELSEIF (LOPDATA.INDPAS .EQ. PSBPOP) THEN
             CALL GENLOPPOP(LOPDATA, ST)
           ENDIF
C
           GOTO 100
C
         ENDIF
       ENDIF
C
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE INQPASDRAW (EXTNUM,EXTYEAR,ST)
C
C       THIS SUBROUTINE INQUIRES THE OPERATOR THE NUMBER OF THE DRAW
C       TO PROCESS.
C
C       INPUTS:
C        *NONE*
C
C       OUTPUTS:
C        EXTNUM         NUMBER OF THE EXTRACTION
C        EXTYEAR        YEAR OF THE EXTRACTION
C        ST             0 = DRAW IS VALID; -1 = DRAW IS NOT VALID
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE INQPASDRAW(EXTNUM,EXTYEAR,ST)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
       INTEGER*4 EXTNUM
       INTEGER*4 EXTYEAR
       INTEGER*4 ST
C
       CHARACTER*6 INPUTDRAW
       CHARACTER*6 EXTNAM
C
       INTEGER*4 SZ
C
       CALL WIMG(5,'DESEJA PROCESSAR QUAL EXTRAÇÃO (EEAAAA)? ')
       READ(5,10) INPUTDRAW
       TYPE*, IAM(), INPUTDRAW
10     FORMAT(A6)
C
       IF (INPUTDRAW .EQ. 'e' .OR. INPUTDRAW .EQ. 'E' .OR.
     *     TRIM(INPUTDRAW) .EQ. '') THEN
         ST = -1
         RETURN
       ENDIF
C
       EXTYEAR = MOD(CTOI(INPUTDRAW,SZ) , 10000)
       EXTNUM  = INT(CTOI(INPUTDRAW,SZ) / 10000)
C
       IF (EXTYEAR .LT. 2000 .OR. EXTNUM .GT. 53 .OR. EXTNUM .LE. 0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Ano/Extração inválido!'
         ST = -1
         RETURN
       ENDIF

       WRITE(EXTNAM,FMT='(I2.2,I4.4)') EXTNUM, EXTYEAR

       CALL PRMYESNO('Confirma a EXTRAÇÃO '//EXTNAM//' [Y/N]? ', ST)
       IF (ST .NE. 1) THEN
         ST = -1
         RETURN
       ENDIF
C
       ST = 0
C
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE VALPASDRAW (LOPDATA,ST)
C
C       THIS SUBROUTINE VALIDATES THE DRAW.
C
C       INPUTS:
C        LOPDATA        LOPPAS DATA STRUCTURE
C
C       OUTPUTS:
C        LOPDATA        LOPPAS DATA STRUCTURE
C        ST             0 = DRAW IS VALID; -1 = DRAW IS NOT VALID
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE VALPASDRAW(LOPDATA,ST)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:PASCOM.DEF'
       INCLUDE 'INCLIB:LOPPAS.DEF'
C
       RECORD /STCLOPPAS/ LOPDATA
       INTEGER*4 ST
C
       INTEGER*4 INDEMIS, EMIOFF, DRWN
       INTEGER*4 GETDRW ! FUNCTION
C
C=======================================================================
C       CHECK IF DRAW NUMBER IS VALID
C=======================================================================
C
       DRWN = GETDRW(LOPDATA.EXTYEAR,LOPDATA.EXTNUM,LOPDATA.GNUM) ! GET DRAW NUMBER
       IF (DRWN .LE. 0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - EXTRAÇÃO inválida!'
         ST = -1
         RETURN
       ENDIF
       LOPDATA.DRAWNUM = DRWN
C
C=======================================================================
C       CHECK IF EMISSION IS IN MEMORY
C=======================================================================
       INDEMIS = -1
       DO EMIOFF=1, PAGEMI
         IF (PASEMIS(EMIOFF,LOPDATA.INDPAS) .EQ. LOPDATA.DRAWNUM) THEN
           INDEMIS = EMIOFF
           EXIT
         ENDIF
       ENDDO
C
       IF (INDEMIS .LE. 0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - EXTRAÇÃO não está em memória!'
         ST = -1
         RETURN
       ENDIF
       LOPDATA.INDEMIS = INDEMIS
C
C=======================================================================
C       CHECK IF RESULTS ARE FINAL
C=======================================================================
C
       IF (PASSTS(LOPDATA.INDEMIS,LOPDATA.INDPAS) .NE. GFINAL) THEN
         TYPE*, IAM(),'       '
         TYPE*, IAM(),'LOPPAS - Prémios não apurados para a EXTRAÇÃO ', LOPDATA.EXTNAM
         TYPE*, IAM(),'       '
         ST = -1
         RETURN
       ENDIF
C
       ST = 0
C
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE OPEN_FILASC (REPFILNAM,REP_LUN,ST)
C
C       THIS SUBROUTINE OPENS AN ASCII FILE
C
C       INPUTS:
C        REPFILNAM        REPORT FILE NAME
C        REP_LUN          LOGICAL UNIT
C
C       OUTPUTS:
C        ST               STATUS
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
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE GAME_TYPNDX(GNUM,GTYP,GNDX,GAMLNAM,ST)
C
C       THIS SUBROUTINE SHOWS THE USER ALL AVAILABLE GAME NAMES
C       AND AFTER PROMPTING THE USER TO SELECT A GAME
C       IT RETURNS THE GAME TYPE AND INDEX
C       GAME NAMES ARE RETRIEVED FROM THE SCF FILE
C
C       INPUTS:
C       *NONE*
C
C       OUTPUTS:
C        GNUM        GAME NUMBER
C        GTYP        GAME TYPE
C        GNDX        GAME INDEX
C        GAMLNAM     GAME LONG NAME
C        ST          STATUS, 0=SUCCESS, ELSE FAILURE
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE GAME_TYPNDX(GNUM,GTYP,GNDX,GAMLNAM,ST)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:DESPAR.DEF'
       INCLUDE 'INCLIB:RECSCF.DEF'
C
       LOGICAL OPNFLG, IS_BLANK   !LUN OPEN FLAG, IS_BLANK FUNCTION
C
       INTEGER*4 IOS,MAXLUN/30/,THELUN
       INTEGER*4 GNUM, ST, GCNT, GTYP, GNDX, GAMSEL, LANG
       INTEGER*4 FILE(5),FILE2(5)
       INTEGER*4 FDB(7)
       INTEGER*4 GAM_NUM(MAXGAM)
C
       CHARACTER*16 GAMLNAM(MAXGAM) ! GAME LONG FILE NAMES
C
       CHARACTER*16 GAME_NAME(MAXGAM)
       EQUIVALENCE(GAME_NAME,SCFLGN)
C
       CALL FASTSET(0,GAM_NUM,MAXGAM)
C
C=======================================================================
C       BEFORE OPENING THE SCF FILE, FIND AN AVAILABLE LUN
C=======================================================================
C
       CALL FIND_AVAILABLE_LUN(THELUN,ST)
       IF (ST .NE. 0) GOTO 10000  !FIND LUN ERROR
C
C=======================================================================
C       OPEN THE SCF FILE AND READ THE SCF RECORD
C=======================================================================
C
       CALL OPENX(THELUN,'SCF.FIL',4,0,0,ST)
       CALL IOINIT(FDB,THELUN,SCFSEC*256)
       IF (ST.NE.0) THEN
         TYPE*,'Error openning SCF.FIL > ',ST
         RETURN
       ENDIF
C
       CALL READW(FDB,1,SCFREC,ST)
       IF (ST.NE.0) THEN
         TYPE*,'Error reading SCF.FIL > ',ST
         RETURN
       ENDIF
C
C=======================================================================
C       RETRIEVE GAME NAMES AND DISPLAY THEM
C=======================================================================
C
       WRITE(5,905)  !GAME NAMES TITLE
       GCNT = 0      !GAME COUNT FOR NAMED GAMES
       DO GNUM=1,MAXGAM
         GAMLNAM(GNUM) = GAME_NAME(GNUM)
         IF (GNUM .EQ. 8 .OR. GNUM .EQ. 9) THEN
           IF (.NOT. IS_BLANK(GAME_NAME(GNUM),16)) THEN  !VALID GAME NAME
             GCNT = GCNT + 1
             GAM_NUM(GCNT) = GNUM  !STORE GAME NUMBER
             WRITE(5,910) GCNT,GAME_NAME(GNUM)
           ENDIF
         ENDIF
       END DO
C
       IF (GCNT .EQ. 0) THEN  !VALID GAME NAME COUNT = 0
         TYPE*, IAM(), 'NO GAME NAMES AVAILABLE'
         ST = -1   !ERROR STATUS
       ELSE   ! GAMES AVAILABLE, PROMPT FOR CHOICE
         WRITE(5,*)
         WRITE(5,911)
         WRITE(5,*)
         CALL PRMNUM(' Enter option: ',GAMSEL,1,GCNT,ST)
         IF (ST.LT.0) THEN !ERROR IN PRMNUM
           ST = -1
         ELSEIF (ST.EQ.0) THEN  !NO ERRORS IN INPNUM
C
C=======================================================================
C       RETRIEVE GAME TYPE AND INDEX CORRESPONDING
C       TO GAME NAME OF USER CHOICE
C=======================================================================
C
         GNUM = GAM_NUM(GAMSEL)                 !GAME NUMBER
         GTYP = SCFGNT(GAMTYP,GAM_NUM(GAMSEL))  !GAME TYPE
         GNDX = SCFGNT(GAMIDX,GAM_NUM(GAMSEL))  !GAME INDEX
         ENDIF
       ENDIF !ENDIF (GCNT.EQ.0)
C
       CALL CLOSEFIL(FDB)  !CLOSE THE SCF FILE
C
10000   CONTINUE
C
       RETURN
C
C=======================================================================
C       FORMAT STATEMENTS
C=======================================================================
C
905     FORMAT(18X,' J O G O S    D I S P O N Í V E I S',/)
910     FORMAT(18X,I2,' - ',A16)
911     FORMAT(18X,' E - Exit')
C
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_PJMC_LOP_HEADER (LOPDATA, ST)
C
C       THIS SUBROUTINE PRINTS THE HEADER OF PJMC_LOP FILE
C
C       INPUTS:
C        LOPDAT        LOPPAS DATA STRUCTURE
C
C       OUTPUTS:
C        ST            IO WRITE STATUS, 0=SUCCESS, ELSE FAILURE
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE PRINT_PJMC_LOP_HEADER (LOPDATA, ST)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:PRMPAS.DEF'
       INCLUDE 'INCLIB:LOPPAS.DEF'
C
       RECORD /STCLOPPAS/ LOPDATA
C
       INTEGER*4 ST
C
       INTEGER*4 LUN
       INTEGER*4 CTIM(2), CDAT(8)
C
       LUN = LOPDATA.TMP_LUN
C
       CALL ICLOCK(1,CTIM)
       CALL GDATE(CDAT(2),CDAT(3),CDAT(1))
       IF (CDAT(1) .LT. 77) THEN
         CDAT(1) = CDAT(1) + 2000
       ELSE
         CDAT(1) = CDAT(1) + 1900
       ENDIF
C
       LOPDATA.LOPGENDAT = CDAT
C
       IF (LOPDATA.INDPAS .EQ. PSBCLA) THEN                  ! CLASSICA
         WRITE(UNIT=LUN,FMT=10,ERR=100,IOSTAT=ST) RECTYP_HP, ! HEADER RECORD TYPE
     *                   LOPDATA.LOPGENDAT(1),               ! FILE GENERATION DATE: YEAR (YYYY)
     *                   LOPDATA.LOPGENDAT(2),               ! FILE GENERATION DATE: MONTH (MM)
     *                   LOPDATA.LOPGENDAT(3),               ! FILE GENERATION DATE: DAY (DD)
     *                   LOPDATA.GNUM,                       ! GAME NUMBER
     *                   LOPDATA.EXTNUM,                     ! EXTRACTION NUMBER (WW)
     *                   LOPDATA.EXTYEAR,                    ! EXTRACTION NAME (YYYY)
     *                   LOPDATA.EMICONF.STCNUMSER,          ! # OF SERIES
     *                   LOPDATA.EMICONF.STCNOFFRA,          ! # OF FRACTIONS
     *                   LOPDATA.EMICONF.STCNUMTCK,          ! # OF TICKETS/EMISSION
     *                   LOPDATA.EMICONF.STCWSER             ! WINNING SERIE
         LOPDATA.TOTLOPREC = LOPDATA.TOTLOPREC + 1
       ELSEIF (LOPDATA.INDPAS .EQ. PSBPOP) THEN              ! POPULAR
         WRITE(UNIT=LUN,FMT=10,ERR=100,IOSTAT=ST) RECTYP_HP, ! HEADER RECORD TYPE
     *                   LOPDATA.LOPGENDAT(1),               ! FILE GENERATION DATE: YEAR (YYYY)
     *                   LOPDATA.LOPGENDAT(2),               ! FILE GENERATION DATE: MONTH (YYYY)
     *                   LOPDATA.LOPGENDAT(3),               ! FILE GENERATION DATE: DAY (DD)
     *                   LOPDATA.GNUM,                       ! GAME NUMBER
     *                   LOPDATA.EXTNUM,                     ! EXTRACTION NUMBER (WW)
     *                   LOPDATA.EXTYEAR,                    ! EXTRACTION NAME (YYYY)
     *                   LOPDATA.EMICONF.STCNOFFRA,          ! # OF SERIES FOR POPULAR
     *                   LOPDATA.EMICONF.STCNUMSER,          ! # OF FRACTIONS FOR POPULAR
     *                   LOPDATA.EMICONF.STCNUMTCK,          ! # OF TICKETS/EMISSION
     *                   LOPDATA.EMICONF.STCWSER             ! WINNING SERIE
         LOPDATA.TOTLOPREC = LOPDATA.TOTLOPREC + 1
       ENDIF
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
10     FORMAT(A2,             ! HEADER RECORD TYPE
     *        I4.4,I2.2,I2.2, ! FILE GENERATION DATE
     *        I2.2,           ! GAME NUMBER
     *        I2.2,           ! EXTRACTION NUMBER
     *        I4.4,           ! EXTRACTION YEAR
     *        I2.2,           ! # OF SERIES (SEE PRMPAS.DEF FOR MAXIMUM NUMBER OF SERIES)
     *        I2.2,           ! # OF FRACTIONS (SEE PRMPAS.DEF FOR MAXIMUM NUMBER OF FRACTIONS)
     *        I6.6,           ! # OF TICKETS/EMISSION (SEE PRMPAS.DEF FOR MAXIMUM NUMBER OF TICKETES/EMISSION)
     *        I2.2,           ! WINNING SERIE
     *        20(' '))
C
       RETURN
C
100    CONTINUE
       TYPE*, IAM(), '       '
       TYPE*, IAM(), 'LOPPAS - Erro a escrever registo no ficheiro: '
       TYPE*, IAM(), '       '
       TYPE*, IAM(), '         '//TRIM(LOPDATA.TMPFILNAM(6:))
       TYPE*, IAM(), '       '
       TYPE*, IAM(), '          Tipo de Registo: '//RECTYP_HP
       TYPE*, IAM(), '          Erro de escrita #: ', ST
       TYPE*, IAM(), '       '
       TYPE*, IAM(), '          Data de Geração do Ficheiro '
       TYPE*, IAM(), '           Ano                     = ', LOPDATA.LOPGENDAT(1)
       TYPE*, IAM(), '           Mês                     = ', LOPDATA.LOPGENDAT(2)
       TYPE*, IAM(), '           Dia                     = ', LOPDATA.LOPGENDAT(3)
       TYPE*, IAM(), '          Número do Jogo           = ', LOPDATA.GNUM
       TYPE*, IAM(), '          Extração '
       TYPE*, IAM(), '           Número                  = ', LOPDATA.EXTNUM
       TYPE*, IAM(), '           Ano                     = ', LOPDATA.EXTYEAR
       IF (LOPDATA.INDPAS .EQ. PSBCLA) THEN
         TYPE*, IAM(), '           Número de Séries        = ', LOPDATA.EMICONF.STCNUMSER
         TYPE*, IAM(), '           Número de Frações       = ', LOPDATA.EMICONF.STCNOFFRA
       ELSEIF (LOPDATA.INDPAS .EQ. PSBPOP) THEN
         TYPE*, IAM(), '           Número de Séries        = ', LOPDATA.EMICONF.STCNOFFRA
         TYPE*, IAM(), '           Número de Frações       = ', LOPDATA.EMICONF.STCNUMSER
       ENDIF
       TYPE*, IAM(), '           Número Bilhetes/Emissão = ', LOPDATA.EMICONF.STCNUMTCK
       TYPE*, IAM(), '           Série Sorteada          = ', LOPDATA.EMICONF.STCWSER
C
       ST = -1
C
       RETURN
C
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_PJMC_LOP_BODY_CLA (LOPDATA, VALREC, VDETAIL,
C    *                                      ERRRECTYP, ST, ST2)
C
C       THIS SUBROUTINE PRINTS THE BODY RECORDS OF PJMC_LOP FILE FOR
C       PASSIVE GAMES
C
C       INPUTS:
C        LOPDATA       LOPPAS DATA STRUCTURE
C        VALREC        VALREC ARRAY
C        VDETAIL       VDETAIL ARRAY
C
C       OUTPUTS:
C        ST            IO WRITE STATUS, 0=SUCCESS, ELSE FAILURE
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE PRINT_PJMC_LOP_BODY_CLA (LOPDATA, VALREC, VDETAIL, ST)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:PRMPAS.DEF'
       INCLUDE 'INCLIB:LOPPAS.DEF'
C
       RECORD /STCLOPPAS/ LOPDATA

       INTEGER*4 ST
C
       INTEGER*4 I
       INTEGER*4 LUN
       INTEGER*4 DDIV, DSHR, DPRZ1, DPRZ2
       INTEGER*4 PRZ1, PRZ2
       INTEGER*4 TOTDPRZ1, TOTDPRZ2
C
       PRZ1 = 0
       PRZ2 = 0
       TOTDPRZ1 = 0
       TOTDPRZ2 = 0
       ST  = -1
C
       LUN  = LOPDATA.TMP_LUN
C
C=======================================================================
C       WRITE RECORD TYPE 01
C=======================================================================
C
       PRZ1 = VALREC(VPAMT) * LOPDATA.EMICONF.STCNUMSER * LOPDATA.EMICONF.STCNOFFRA
       PRZ2 = VALREC(VPAMT)
       WRITE(UNIT=LUN,FMT=10,ERR=100,IOSTAT=ST) RECTYP_01,     ! RECORD TYPE
     *                                          VALREC(VTCKT), ! TICKET NUMBER
     *                                          PRZ1,          ! PRIZE AMOUNT 1
     *                                          PRZ2           ! PRIZE AMOUNT 2
C
       LOPDATA.TOTLOPREC = LOPDATA.TOTLOPREC + 1
C
C=======================================================================
C       WRITE RECORD TYPE 02
C=======================================================================
C
       DO I = 1,VALREC(VPZOFF)
         DDIV = VDETAIL(VDIV,I)
         DSHR = VDETAIL(VSHR,I)
         DPRZ1 = LOPDATA.WSERSHV(DDIV) * LOPDATA.EMICONF.STCNUMSER
         TOTDPRZ1 = TOTDPRZ1 + DPRZ1
         DPRZ2 = LOPDATA.WSERSHV(DDIV) / LOPDATA.EMICONF.STCNOFFRA
         TOTDPRZ2 = TOTDPRZ2 + DPRZ2
         WRITE(UNIT=LUN,FMT=20,ERR=200,IOSTAT=ST) RECTYP_02,     ! RECORD TYPE (PRIZE DETAIL)
     *                                            DDIV,          ! PRIZED DIVISION NUMBER
     *                                            DSHR,          ! NUMBER OF SHARES
     *                                            VALREC(VTCKT), ! TICKET NUMBER
     *                                            DPRZ1,         ! PRIZE AMOUNT 1
     *                                            DPRZ2          ! PRIZE AMOUNT 2
         LOPDATA.TOTLOPREC = LOPDATA.TOTLOPREC + 1
       ENDDO
C
       IF ((PRZ1 .EQ. TOTDPRZ1) .AND. (PRZ2 .EQ. TOTDPRZ2)) THEN
         ST = 0
       ELSE
         ST = -1
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Erro - Valor de Prémio difere da soma do'
         TYPE*, IAM(), '                valor das suas divisões premiadas!'
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         Bilhete: '
         TYPE*, IAM(), '          Número              = ', VALREC(VTCKT)
         TYPE*, IAM(), '          Série               = ', VALREC(VSERN)
         TYPE*, IAM(), '          Fração              = ', VALREC(VPFRAC)
         TYPE*, IAM(), '         Prémio 1: '
         TYPE*, IAM(), '          Valor Total         = ', PRZ1
         TYPE*, IAM(), '          Valor Soma Divisões = ', TOTDPRZ1
         TYPE*, IAM(), '         Prémio 2: '
         TYPE*, IAM(), '          Valor Total         = ', PRZ2
         TYPE*, IAM(), '          Valor Soma Divisões = ', TOTDPRZ2
         TYPE*, IAM(), '       '
       ENDIF
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
10      FORMAT(A2,I5.5,I11.11,I11.11,21(' '))           ! RECORD TYPE 01
20      FORMAT(A2,I2.2,I2.2,I5.5,I11.11,I11.11,17(' ')) ! RECORD TYPE 02
C
       RETURN
C
100    CONTINUE
       TYPE*, IAM(), '       '
       TYPE*, IAM(), 'LOPPAS - Erro a escrever registo no ficheiro: '
       TYPE*, IAM(), '       '
       TYPE*, IAM(), '         '//TRIM(LOPDATA.TMPFILNAM(6:))
       TYPE*, IAM(), '       '
       TYPE*, IAM(), '         Tipo de Registo: '//RECTYP_01
       TYPE*, IAM(), '         Erro de escrita #: ', ST
       TYPE*, IAM(), '       '
       TYPE*, IAM(), '         Bilhete: '
       TYPE*, IAM(), '          Número           = ', VALREC(VTCKT)
       TYPE*, IAM(), '          Série            = ', VALREC(VSERN)
       TYPE*, IAM(), '          Fração           = ', VALREC(VPFRAC)
       TYPE*, IAM(), '         Prémio Tipo 1 '
       TYPE*, IAM(), '           Valor Total     = ', PRZ1
       TYPE*, IAM(), '         Prémio Tipo 2 '
       TYPE*, IAM(), '           Valor Total     = ', PRZ2
C
       ST = -1
C
       RETURN
C
200    CONTINUE
       TYPE*, IAM(), '       '
       TYPE*, IAM(), 'LOPPAS - Erro a escrever registo no ficheiro: '
       TYPE*, IAM(), '       '
       TYPE*, IAM(), '         '//TRIM(LOPDATA.TMPFILNAM(6:))
       TYPE*, IAM(), '       '
       TYPE*, IAM(), '         Tipo de Registo: '//RECTYP_02
       TYPE*, IAM(), '         Erro de escrita #: ', ST
       TYPE*, IAM(), '       '
       TYPE*, IAM(), '         Bilhete: '
       TYPE*, IAM(), '          Número          = ', VALREC(VTCKT)
       TYPE*, IAM(), '          Série           = ', VALREC(VSERN)
       TYPE*, IAM(), '          Fração          = ', VALREC(VPFRAC)
       TYPE*, IAM(), '          Divisão         = ', DDIV
       TYPE*, IAM(), '          Total de Shares = ', DSHR
       TYPE*, IAM(), '          Valor 1 Divisão = ', DPRZ1
       TYPE*, IAM(), '          Valor 2 Divisão = ', DPRZ2
C
       ST = -1
C
       RETURN
C
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_PJMC_LOP_BODY_POP (LOPDATA,VVALREC,VVDETAIL,
C    *                                      ERRRECTYP, ST)
C
C       THIS SUBROUTINE PRINTS THE BODY RECORDS OF PJMC_LOP FILE FOR
C       PASSIVE GAMES
C
C       INPUTS:
C        LOPDATA       LOPPAS DATA STRUCTURE
C        VVALREC       VALREC ARRAY
C        VVDETAIL      VDETAIL ARRAY
C
C       OUTPUTS:
C        ST            IO WRITE STATUS, 0=SUCCESS, ELSE FAILURE
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE PRINT_PJMC_LOP_BODY_POP (LOPDATA, VVALREC, VVDETAIL, ST)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:PRMPAS.DEF'
       INCLUDE 'INCLIB:LOPPAS.DEF'
C
       RECORD /STCLOPPAS/ LOPDATA
C
       INTEGER*4 ST
C
       INTEGER*4 I
       INTEGER*4 LUN
       INTEGER*4 DDIV, DSHR, DPRZ1, DPRZ2
       INTEGER*4 PRZ1, PRZ2
       INTEGER*4 TOTDPRZ1, TOTDPRZ2
C
       PRZ1 = 0
       PRZ2 = 0
       TOTDPRZ1 = 0
       TOTDPRZ2 = 0
       ST  = -1
C
       LUN = LOPDATA.TMP_LUN
C
C=======================================================================
C       WRITE RECORD TYPE 01
C=======================================================================
C
       PRZ1 = VVALREC(VPAMT,IDXWSER)
       PRZ2 = VVALREC(VPAMT,IDXOSER)
       WRITE(UNIT=LUN,FMT=10,ERR=100,IOSTAT=ST) RECTYP_01,               ! RECORD TYPE
     *                                          VVALREC(VTCKT,IDXWSER),  ! TICKET NUMBER
     *                                          PRZ1,                    ! PRIZE AMOUNT 1
     *                                          PRZ2                     ! PRIZE AMOUNT 2
       LOPDATA.TOTLOPREC = LOPDATA.TOTLOPREC + 1
C
C=======================================================================
C       WRITE RECORD TYPE 02
C=======================================================================
C
       DO I = 1,VVALREC(VPZOFF,IDXWSER)
         DDIV = VVDETAIL(VDIV,I,IDXWSER)
         DSHR = VVDETAIL(VSHR,I,IDXWSER)
         DPRZ1 = LOPDATA.WSERSHV(DDIV)
         TOTDPRZ1 = TOTDPRZ1 + DPRZ1
         DPRZ2 = LOPDATA.OSERSHV(DDIV)
         TOTDPRZ2 = TOTDPRZ2 + DPRZ2
         WRITE(UNIT=LUN,FMT=20,ERR=200,IOSTAT=ST) RECTYP_02,              ! RECORD TYPE (PRIZE DETAIL)
     *                                            DDIV,                   ! PRIZED DIVISION NUMBER
     *                                            DSHR,                   ! NUMBER OF SHARES
     *                                            VVALREC(VTCKT,IDXWSER), ! TICKET NUMBER
     *                                            DPRZ1,                  ! PRIZE AMOUNT 1
     *                                            DPRZ2                   ! PRIZE AMOUNT 2
         LOPDATA.TOTLOPREC = LOPDATA.TOTLOPREC + 1
       ENDDO
C
       IF ((PRZ1 .EQ. TOTDPRZ1) .AND. (PRZ2 .EQ. TOTDPRZ2)) THEN
         ST = 0
       ELSE
         ST = -1
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Erro - Valor de Prémio difere da soma do'
         TYPE*, IAM(), '                valor das suas divisões premiadas!'
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         Bilhete: '
         TYPE*, IAM(), '          Número              = ', VVALREC(VTCKT,IDXWSER)
         TYPE*, IAM(), '          Série               = ', VVALREC(VPFRAC,IDXWSER)
         TYPE*, IAM(), '          Fração              = ', VVALREC(VSERN,IDXWSER)
         TYPE*, IAM(), '         Prémio 1: '
         TYPE*, IAM(), '          Valor Total         = ', PRZ1
         TYPE*, IAM(), '          Valor Soma Divisões = ', TOTDPRZ1
         TYPE*, IAM(), '         Prémio 2: '
         TYPE*, IAM(), '          Valor Total         = ', PRZ2
         TYPE*, IAM(), '          Valor Soma Divisões = ', TOTDPRZ2
         TYPE*, IAM(), '       '
       ENDIF
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
10      FORMAT(A2,I5.5,I11.11,I11.11,21(' '))           ! RECORD TYPE 01
20      FORMAT(A2,I2.2,I2.2,I5.5,I11.11,I11.11,17(' ')) ! RECORD TYPE 02
C
       RETURN
C
100    CONTINUE
       TYPE*, IAM(), '       '
       TYPE*, IAM(), 'LOPPAS - Erro a escrever registo no ficheiro: '
       TYPE*, IAM(), '       '
       TYPE*, IAM(), '         '//TRIM(LOPDATA.TMPFILNAM(6:))
       TYPE*, IAM(), '       '
       TYPE*, IAM(), '         Tipo de Registo: '//RECTYP_01
       TYPE*, IAM(), '         Erro de escrita #: ', ST
       TYPE*, IAM(), '       '
       TYPE*, IAM(), '         Bilhete '
       TYPE*, IAM(), '          Número           = ', VVALREC(VTCKT,IDXWSER)
       TYPE*, IAM(), '          Série            = ', VVALREC(VPFRAC,IDXWSER)
       TYPE*, IAM(), '          Fração           = ', VVALREC(VSERN,IDXWSER)
       TYPE*, IAM(), '         Prémio Tipo 1 '
       TYPE*, IAM(), '           Valor Total     = ', PRZ1
       TYPE*, IAM(), '         Prémio Tipo 2 '
       TYPE*, IAM(), '           Valor Total     = ', PRZ2
C
       ST = -1
C
       RETURN
C
200    CONTINUE
       TYPE*, IAM(), '       '
       TYPE*, IAM(), 'LOPPAS - Erro a escrever registo no ficheiro: '
       TYPE*, IAM(), '       '
       TYPE*, IAM(), '         '//TRIM(LOPDATA.TMPFILNAM(6:))
       TYPE*, IAM(), '       '
       TYPE*, IAM(), '         Tipo de Registo: '//RECTYP_02
       TYPE*, IAM(), '         Erro de escrita #: ', ST
       TYPE*, IAM(), '       '
       TYPE*, IAM(), '         Bilhete '
       TYPE*, IAM(), '          Número          = ', VVALREC(VTCKT,IDXWSER)
       TYPE*, IAM(), '          Série           = ', VVALREC(VPFRAC,IDXWSER)
       TYPE*, IAM(), '          Fração          = ', VVALREC(VSERN,IDXWSER)
       TYPE*, IAM(), '          Divisão         = ', DDIV
       TYPE*, IAM(), '          Total de Shares = ', DSHR
       TYPE*, IAM(), '          Valor 1 Divisão = ', DPRZ1
       TYPE*, IAM(), '          Valor 2 Divisão = ', DPRZ2
C
       ST = -1
C
       RETURN
C
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_PJMC_LOP_FOOTER (LOPDATA,ST)
C
C       THIS SUBROUTINE PRINTS THE FOOTER RECORD OF PJMC_LOP FILE
C
C       INPUTS:
C        LOPDATA        LOPPAS DATA STRUCTURE
C
C       OUTPUTS:
C        ST             IO WRITE STATUS, 0=SUCCESS, ELSE FAILURE
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE PRINT_PJMC_LOP_FOOTER (LOPDATA, ST)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:PRMPAS.DEF'
       INCLUDE 'INCLIB:LOPPAS.DEF'
C
       RECORD /STCLOPPAS/ LOPDATA
       INTEGER*4 ST
C
       LOPDATA.TOTLOPREC = LOPDATA.TOTLOPREC + 1
       WRITE(UNIT=LOPDATA.TMP_LUN,FMT=10,ERR=100,IOSTAT=ST) RECTYP_TP,        ! TRAILER RECORD TYPE
     *                                                      LOPDATA.TOTLOPREC ! TOTAL NUMBER OF RECORDS
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
10      FORMAT(A2,I8.8,40(' '))
C
       RETURN
C
100    CONTINUE
       TYPE*, IAM(), '       '
       TYPE*, IAM(), 'LOPPAS - Erro a escrever registo no ficheiro: '
       TYPE*, IAM(), '       '
       TYPE*, IAM(), '         '//TRIM(LOPDATA.TMPFILNAM(6:))
       TYPE*, IAM(), '       '
       TYPE*, IAM(), '         Tipo de Registo: '//RECTYP_TP
       TYPE*, IAM(), '         Erro de escrita #: ', ST
       TYPE*, IAM(), '       '
       TYPE*, IAM(), '         Total de registos = ', LOPDATA.TOTLOPREC
C
       ST = -1
C
       RETURN
C
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE GENLOPPOP(LOPDATA,ST)
C
C       THIS SUBROUTINE GENERATES THE PJMC_LOP FILE FOR POPULAR.
C
C       INPUTS:
C        LOPDATA        LOPPAS DATA STRUCTURE
C
C       OUTPUTS:
C        LOPDATA        LOPPAS DATA STRUCTURE
C        ST             STATUS, 0=SUCCESS, ELSE FAILURE
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE GENLOPPOP(LOPDATA,ST)
       IMPLICIT NONE
C
       INCLUDE '(LIB$ROUTINES)'
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:PRMHSH.DEF'
       INCLUDE 'INCLIB:VALPASFIL.DEF'
       INCLUDE 'INCLIB:PASNAM.DEF'
       INCLUDE 'INCLIB:LOPPAS.DEF'
C
       INTEGER*4 ST
       LOGICAL   ISTHERE
C
       INTEGER*4 VPFBUF(I4BUCSIZ)
C
       INTEGER*4 LOP_LUN !OFFICIAL PRIZE LIST LUN
       INTEGER*4 TMP_LUN ! LUN FOR PJMC_LOP TEMPORARY FILE (WORKING FILE)
       INTEGER*4 VPF_LUN ! LUN FOR VPF FILE
C
       CHARACTER*27 LOPFILNAM ! INTERFACE FILE FILE NAME FOR PJMC
       CHARACTER*27 TMPFILNAM ! TEMPORARY FILE NAME
       CHARACTER*18 VPFFILNAM ! VPF FILE NAME
C
       CHARACTER*20 VPF_CFILNAM
       INTEGER*4    VPF_IFILNAM(5)
       EQUIVALENCE (VPF_IFILNAM, VPF_CFILNAM)
C
       INTEGER*4 I, J, K
       INTEGER*4 PAS_ROUND_VALUE ! FUNCTION
       CHARACTER*50 C50DIV
C
       CHARACTER*256 LIBCMD
       INTEGER*4 ST_RENCMD
       INTEGER*4 ST_DIRCMD

C
       INTEGER*4 VKEY(2)
       INTEGER*4 ERRN
       INTEGER*4 TICNUM, SERNUM, FRANUM
C
       LOGICAL VALDRW
C
       RECORD /STCEMIS/ RECEMI
       RECORD /STCLOPPAS/ LOPDATA
C
C=======================================================================
C       GET EMISSION INFO
C=======================================================================
C
       TYPE*, IAM(), '       '
       TYPE*, IAM(), 'A obter info da extração ...'
C
       CALL GETEMIDATA(LOPDATA.INDEMIS, LOPDATA.INDPAS, LOPDATA.EXTNUM, LOPDATA.EXTYEAR, RECEMI)
       LOPDATA.EMICONF = RECEMI
C
       WRITE(5,'(1X,A,A,I7)')  IAM(), ' Número da emissão:              ', LOPDATA.EMICONF.STCEMIS
       WRITE(5,'(1X,A,A,I7)')  IAM(), ' Número do plano:                ', LOPDATA.EMICONF.STCPLAN
       WRITE(5,'(1X,A,A,A3)')  IAM(), ' Tipo de emissão:                    ', NAMPLANTYP(LOPDATA.EMICONF.STCEMT)(1:3)
       WRITE(5,'(1X,A,A,I7)')  IAM(), ' Número de séries:               ', LOPDATA.EMICONF.STCNOFFRA
C
       IF (LOPDATA.EMICONF.STCNOFFRA .LE. 0) THEN
         TYPE*, IAM(),'       '
         TYPE*, IAM(),'LOPPAS - Erro - Número de séries inválido!'
         TYPE*, IAM(),'       '
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
C
       WRITE(5,'(1X,A,A,I7)') IAM(), ' Número de frações/série:        ', LOPDATA.EMICONF.STCNUMSER
       IF (LOPDATA.EMICONF.STCNUMSER .LE. 0) THEN
         TYPE*, IAM(),'       '
         TYPE*, IAM(),'LOPPAS - Erro - Número de frações/série inválido!'
         TYPE*, IAM(),'       '
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
C
       WRITE(5,'(1X,A,A,I7)') IAM(), ' Número de bilhetes/emissão:     ', LOPDATA.EMICONF.STCNUMTCK
       IF (LOPDATA.EMICONF.STCNUMTCK .LE. 0) THEN
         TYPE*, IAM(),'       '
         TYPE*, IAM(),'LOPPAS - Erro - Número de bilhetes/emissão inválido!'
         TYPE*, IAM(),'       '
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
C
       WRITE(5,'(1X,A,A,I7)') IAM(), ' Série sorteada:                 ', LOPDATA.EMICONF.STCWSER
       IF (LOPDATA.EMICONF.STCWSER .EQ. 0) THEN
         TYPE*, IAM(),'       '
         TYPE*, IAM(),'LOPPAS - Erro - Não existe série sorteada!'
         TYPE*, IAM(),'       '
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
C
       TYPE*, IAM(), ' Valor das shares:'
       TYPE*, IAM(), '       '
       WRITE(C50DIV,'(A,I0,A)') '             Série Sorteada (', LOPDATA.EMICONF.STCWSER, ')   Restantes Séries'
       TYPE*, IAM(), TRIM(C50DIV)
       LOPDATA.NUMDIVS = LOPDATA.EMICONF.STCDIV
       DO I = 1, LOPDATA.NUMDIVS
         IF (I .LE. PAGEDV) THEN
           IF (LOPDATA.EMICONF.STCEXSHV(I) .GT. 0) THEN
             LOPDATA.OSERSHV(I) = PAS_ROUND_VALUE(LOPDATA.EMICONF.STCEXSHV(I))
           ELSE
             LOPDATA.OSERSHV(I) = PAS_ROUND_VALUE(LOPDATA.EMICONF.STCSHV(I))
           ENDIF
         ELSE
           LOPDATA.OSERSHV(I) = PAS_ROUND_VALUE(LOPDATA.EMICONF.STCSHV(I))
         ENDIF
         LOPDATA.WSERSHV(I) = PAS_ROUND_VALUE(LOPDATA.EMICONF.STCSHV(I))
         WRITE(5,'(1X,A,A,I2,A,A13,A,A13)') IAM(), '  Divisão ',I,':      ',
     *         CMONY(LOPDATA.WSERSHV(I),13,VALUNIT),'      ',
     *         CMONY(LOPDATA.OSERSHV(I),13,VALUNIT)
       ENDDO
C
C=======================================================================
C       NAME THE FOLLOWING FILES:
C         PJMC_LOP INTERFACE FILE
C         PJMC_LOP TEMPORARY FILE
C=======================================================================
C
       WRITE (LOPFILNAM, FMT='(A14,I2.2,A6,A4)')
     *     'FILE:PJMC_LOP_',LOPDATA.GNUM,LOPDATA.EXTNAM,'.ASC'
       LOPDATA.LOPFILNAM = LOPFILNAM
C
       WRITE (TMPFILNAM, FMT='(A14,I2.2,A6,A4)')
     *     'FILE:PJMC_LOP_',LOPDATA.GNUM,LOPDATA.EXTNAM,'.TMP'
       LOPDATA.TMPFILNAM = TMPFILNAM
       CALL DFILX(LOPDATA.TMPFILNAM,0,0,ST)
       IF (ST .NE. 0) CALL GSTOP (GEXIT_FATAL)
C
C=======================================================================
C       CHECK IF PJMC_LOP FILE ALREADY EXISTS IN THE SYSTEM. IF SO,
C       EXIT THE PROGRAM IMMEDIATELY.
C=======================================================================
C
       INQUIRE(FILE=LOPDATA.LOPFILNAM, EXIST=ISTHERE)
       IF (ISTHERE) THEN
         TYPE*, IAM(),'       '
         TYPE*, IAM(),'LOPPAS - O ficheiro seguinte já existe no sistema:'
         TYPE*, IAM(),'       '
         TYPE*, IAM(),'         '//TRIM(LOPDATA.LOPFILNAM)
         TYPE*, IAM(),'       '
         TYPE*, IAM(),'Para continuar, o ficheiro deverá ser removido'
         TYPE*, IAM(),'       '
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
C
C=======================================================================
C      FIND A FREE LUN TO USE FOR PJMC_LOP TEMPORARY FILE
C=======================================================================
C
       CALL FIND_AVAILABLE_LUN (TMP_LUN,ST)
       IF (ST .NE. 0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Erro a obter uma LUN para o ficheiro: '
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         '//TRIM(LOPDATA.TMPFILNAM(6:))
         TYPE*, IAM(), '       '
         CALL GSTOP (GEXIT_FATAL)
       ENDIF
       LOPDATA.TMP_LUN = TMP_LUN
C
C=======================================================================
C       OPEN THE PJMC_LOP TEMPORARY FILE
C=======================================================================
C
       CALL OPEN_FILASC (LOPDATA.TMPFILNAM,LOPDATA.TMP_LUN,ST)
       IF (ST .NE. 0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Erro a abrir o ficheiro: '
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         '//TRIM(LOPDATA.TMPFILNAM(6:))
         TYPE*, IAM(), '       '
         ! CLOSE AND DELETE CREATED FILES SO FAR
         CALL USRCLOS1(LOPDATA.TMP_LUN)
         CALL DFILX(LOPDATA.TMPFILNAM,0,0,ST)
         CALL GSTOP (GEXIT_FATAL)
       ENDIF
C
C=======================================================================
C       OPEN VALIDATION FILE (VPF)
C=======================================================================
C
       WRITE(VPF_CFILNAM,FMT='(A8,I2.2,I4.4,A4)') 'VALX:VPF',LOPDATA.INDPAS,LOPDATA.DRAWNUM,'.FIL'
       CALL FIND_AVAILABLE_LUN (VPF_LUN,ST)
       IF (ST .NE. 0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Erro a obter uma LUN para o ficheiro: '
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         '//TRIM(VPF_CFILNAM)
         TYPE*, IAM(), '       '
         CALL FILERR(VPF_IFILNAM,0,ST,0)
         GOTO 2000
       ENDIF
C
       LOPDATA.VPFFILNAM = VPF_CFILNAM
       LOPDATA.VPF_LUN = VPF_LUN
C
       CALL IOPEN(VPF_IFILNAM,LOPDATA.VPF_LUN,VPFLEN*2,VFSCDC,VFSSER*2-1,ST)
       IF (ST .NE. 0) THEN
         CALL FILERR(VPF_IFILNAM,1,ST,0)
         GOTO 2000
       ENDIF
C
C=======================================================================
C       WRITE HEADER INTO THE FILE
C=======================================================================
C
       LOPDATA.TOTLOPREC = 0
       CALL PRINT_PJMC_LOP_HEADER (LOPDATA, ST)
       IF (ST .NE. 0) GOTO 2000
C
C=======================================================================
C       SET THE WINNING SERIE
C=======================================================================
C
       VSER(IDXWSER) = LOPDATA.EMICONF.STCWSER
C
C=======================================================================
C       GET A NOT WINNING SERIE
C=======================================================================
C
       DO I = 1, LOPDATA.EMICONF.STCNOFFRA
         IF (I .NE. LOPDATA.EMICONF.STCWSER) THEN
           VSER(IDXOSER) = I ! SAVE NOT WINNING SERIE
           EXIT
         ENDIF
       ENDDO
C
C=======================================================================
C       START READING VPF FILE USING A KEY
C=======================================================================
C
       LOPDATA.TOTREADVPF = 0
       LOPDATA.TOTVALREC  = 0
       LOPDATA.TOTINVREC  = 0
       LOPDATA.TOTNFREC   = 0
       LOPDATA.TOTOTHERR  = 0
C
       TYPE*, IAM(), '       '
       TYPE*, IAM(), 'A iniciar processamento ...'
C
       FRANUM = 1
       DO TICNUM = 0, LOPDATA.EMICONF.STCNUMTCK-1
         CALL FASTSET(0,VVALREC,SIZEOF(VVALREC)/4)
         CALL FASTSET(0,VVDETAIL,SIZEOF(VVDETAIL)/4)
         CALL FASTSET(-1,VERRN,SIZEOF(VERRN)/4)
         DO I = 1, MAXIDX
           SERNUM  = VSER(I)
           VKEY(1) = SERNUM
           VKEY(2) = ISHFT(FRANUM,24) + TICNUM
           CALL FASTSET(0, V4BUF_PAS, VPFLEN * VPFMAXREC)
           CALL IREAD(VKEY,V4BUF_PAS,VPF_LUN,ERRN)
           LOPDATA.TOTREADVPF = LOPDATA.TOTREADVPF + 1
C
           VERRN(I) = ERRN
           IF (ERRN .EQ. 0) THEN
C
             CALL LOGPAS(VALREC,V4BUF_PAS)
             DO K = 1, VALLEN
               VVALREC(K,I) = VALREC(K)
             ENDDO
C
C=======================================================================
C             CHECK IF VALREC EQUALS TICNUM-SERNUM-FFRANUM
C=======================================================================
C
             IF (VVALREC(VTCKT,I)  .NE. TICNUM .OR.
     *           VVALREC(VSERN,I)  .NE. FRANUM .OR.
     *           VVALREC(VPFRAC,I) .NE. SERNUM) THEN
               LOPDATA.TOTINVREC = LOPDATA.TOTINVREC + 1
               TYPE*, IAM(), '       '
               TYPE*, IAM(), 'LOPPAS - Bilhete pesquisado diferente do obtido!'
               TYPE*, IAM(), '       '
               TYPE*, IAM(), '         Bilhete pesquisado: '
               TYPE*, IAM(), '          Número           = ', TICNUM
               TYPE*, IAM(), '          Série            = ', FRANUM
               TYPE*, IAM(), '          Fração           = ', SERNUM
               TYPE*, IAM(), '       '
               TYPE*, IAM(), '         Bilhete obtido: '
               TYPE*, IAM(), '          Número           = ', VVALREC(VTCKT,I)
               TYPE*, IAM(), '          Série            = ', VVALREC(VSERN,I)
               TYPE*, IAM(), '          Fração           = ', VVALREC(VPFRAC,I)
               GOTO 2000
             ENDIF
C
C=======================================================================
C           CHECK IF VALREC BELONGS TO THE GAME TYPE WE WANT TO PROCESS
C=======================================================================
C
             IF (VVALREC(VGTYP,I) .NE. LOPDATA.GTYP) THEN
               LOPDATA.TOTINVREC = LOPDATA.TOTINVREC + 1
               TYPE*, IAM(), '       '
               TYPE*, IAM(), 'LOPPAS - Tipo de jogo do bilhete inválido!'
               TYPE*, IAM(), '       '
               TYPE*, IAM(), '         Tipo a processar:   ', LOPDATA.INDPAS
               TYPE*, IAM(), '       '
               TYPE*, IAM(), '         Bilhete processado: '
               TYPE*, IAM(), '          Número           = ', VVALREC(VTCKT,I)
               TYPE*, IAM(), '          Série            = ', VVALREC(VSERN,I)
               TYPE*, IAM(), '          Fração           = ', VVALREC(VPFRAC,I)
               TYPE*, IAM(), '           Tipo            = ', VVALREC(VGTYP,I)
               GOTO 2000
             ENDIF
C
C=======================================================================
C             CHECK IF VALREC BELONGS TO THE GAME WE WANT TO PROCESS
C=======================================================================
C
             IF (VVALREC(VGAM,I) .NE. LOPDATA.GNUM) THEN
               LOPDATA.TOTINVREC = LOPDATA.TOTINVREC + 1
               TYPE*, IAM(), '       '
               TYPE*, IAM(), 'LOPPAS - Número de jogo do bilhete inválido!'
               TYPE*, IAM(), '       '
               TYPE*, IAM(), '         Jogo a processar:   ', LOPDATA.GNUM
               TYPE*, IAM(), '       '
               TYPE*, IAM(), '         Bilhete processado: '
               TYPE*, IAM(), '          Número           = ', VVALREC(VTCKT,I)
               TYPE*, IAM(), '          Série            = ', VVALREC(VSERN,I)
               TYPE*, IAM(), '          Fração           = ', VVALREC(VPFRAC,I)
               TYPE*, IAM(), '           Jogo            = ', VVALREC(VGAM,I)
               GOTO 2000
             ENDIF
C
C=======================================================================
C             CHECK IF VALREC BELONGS TO THE INDICE OF PASSIVE GAME
C=======================================================================
C
             IF (VVALREC(VGIND,I) .NE. LOPDATA.INDPAS) THEN
               LOPDATA.TOTINVREC = LOPDATA.TOTINVREC + 1
               TYPE*, IAM(), '       '
               TYPE*, IAM(), 'LOPPAS - Índice do jogo do bilhete inválido!'
               TYPE*, IAM(), '       '
               TYPE*, IAM(), '         Índice a processar: ', LOPDATA.INDPAS
               TYPE*, IAM(), '       '
               TYPE*, IAM(), '         Bilhete processado: '
               TYPE*, IAM(), '          Número           = ', VVALREC(VTCKT,I)
               TYPE*, IAM(), '          Série            = ', VVALREC(VSERN,I)
               TYPE*, IAM(), '          Fração           = ', VVALREC(VPFRAC,I)
               TYPE*, IAM(), '           Índice          = ', VVALREC(VGIND,I)
               GOTO 2000
             ENDIF
C
C=======================================================================
C       CHECK IF VALIDATION RECORD BELONGS TO THE CHOSEN DRAW
C=======================================================================
C
             CALL DLOGPAS(VALREC,VDETAIL)
             DO K=1, VPLEN
               DO J=1, VMAX
                 VVDETAIL(K,J,I) = VDETAIL(K,J)
               ENDDO
             ENDDO
C
             VALDRW = .FALSE.
             DO K=1, VALREC(VPZOFF)
               IF (VVDETAIL(VDRW,K,I) .NE. LOPDATA.DRAWNUM) THEN
                 VALDRW = .FALSE.
                 EXIT
               ELSE
                 VALDRW = .TRUE.
               ENDIF
             ENDDO
C
             IF (.NOT. VALDRW) THEN
               LOPDATA.TOTINVREC = LOPDATA.TOTINVREC + 1
               TYPE*, IAM(), '       '
               TYPE*, IAM(), 'LOPPAS - Número da emissão da divisão inválido!'
               TYPE*, IAM(), '       '
               TYPE*, IAM(), '         Número da emissão = ', LOPDATA.DRAWNUM
               TYPE*, IAM(), '       '
               TYPE*, IAM(), '         Bilhete '
               TYPE*, IAM(), '          Número           = ', VVALREC(VTCKT,I)
               TYPE*, IAM(), '          Série            = ', VVALREC(VSERN,I)
               TYPE*, IAM(), '          Fração           = ', VVALREC(VPFRAC,I)
               TYPE*, IAM(), '           Divisão         = ', K
               TYPE*, IAM(), '            Emissão        = ', VVDETAIL(VDRW,K,I)
               GOTO 2000
             ENDIF
C
             LOPDATA.TOTVALREC = LOPDATA.TOTVALREC + 1
C
           ELSEIF (ERRN .EQ. ERRRNF) THEN
             LOPDATA.TOTNFREC = LOPDATA.TOTNFREC + 1
           ELSE
             LOPDATA.TOTOTHERR = LOPDATA.TOTOTHERR + 1
             TYPE*, IAM(), '       '
             TYPE*, IAM(), 'LOPPAS - Erro a obter bilhete do ficheiro: '
             TYPE*, IAM(), '       '
             TYPE*, IAM(), '         '//TRIM(LOPDATA.VPFFILNAM)
             TYPE*, IAM(), '       '
             TYPE*, IAM(), '         Bilhete '
             TYPE*, IAM(), '          Número           = ', TICNUM
             TYPE*, IAM(), '          Série            = ', SERNUM
             TYPE*, IAM(), '          Fração           = ', FRANUM
             CALL FILERR(VPF_IFILNAM,1,ST,0)
             GOTO 2000
           ENDIF
         ENDDO
C
         IF (VERRN(IDXWSER) .EQ. 0 .AND. VERRN(IDXOSER) .EQ. 0) THEN
           IF (VVALREC(VTCKT,IDXWSER) .EQ. VVALREC(VTCKT,IDXOSER) .AND.
     *         VVALREC(VSERN,IDXWSER) .EQ. VVALREC(VSERN,IDXOSER)) THEN
C
C=======================================================================
C       WRITE BODY RECORDS INTO PJMC_LOP INTERFACE FILE
C=======================================================================
C
             CALL PRINT_PJMC_LOP_BODY_POP (LOPDATA, VVALREC, VVDETAIL, ST)
             IF (ST .NE. 0) GOTO 2000
C
           ELSE
             TYPE*, IAM(), '       '
             TYPE*, IAM(), 'LOPPAS - Erro a processar bilhete: '
             TYPE*, IAM(), '       '
             TYPE*, IAM(), '         Bilhete série sorteada'
             TYPE*, IAM(), '          Número           = ', VVALREC(VTCKT,IDXWSER)
             TYPE*, IAM(), '          Série            = ', VVALREC(VSERN,IDXWSER)
             TYPE*, IAM(), '          Fração           = ', VVALREC(VPFRAC,IDXWSER)
             TYPE*, IAM(), '         Bilhete série não sorteada'
             TYPE*, IAM(), '          Número           = ', VVALREC(VTCKT,IDXOSER)
             TYPE*, IAM(), '          Série            = ', VVALREC(VSERN,IDXOSER)
             TYPE*, IAM(), '          Fração           = ', VVALREC(VPFRAC,IDXOSER)
             GOTO 2000
           ENDIF
         ENDIF
C
         IF (MOD(LOPDATA.TOTREADVPF, 20000) .EQ. 0) THEN
           TYPE*, IAM(), LOPDATA.TOTREADVPF / 2, 'bilhetes processados ...'
         ENDIF
       ENDDO
C
       IF (MOD(LOPDATA.TOTREADVPF, 20000) .NE. 0) THEN
         TYPE*, IAM(), LOPDATA.TOTREADVPF /2, 'bilhetes processados ...'
       ENDIF
C
C=======================================================================
C       WRITE FOOTER INTO PJMC_LOP INTERFACE FILE
C=======================================================================
C
       CALL PRINT_PJMC_LOP_FOOTER (LOPDATA, ST)
       IF (ST .NE. 0) GOTO 2000
C
C=======================================================================
C       CLOSE FILES
C=======================================================================
C
       CALL USRCLOS1(LOPDATA.TMP_LUN)
       CALL ICLOSE(LOPDATA.VPF_LUN,VPFBUF,ST)
C
C=======================================================================
C       PRINT PROCESS STATISTICS ON SCREEN
C=======================================================================
C
       TYPE*, IAM(), '       '
       WRITE(5, '(1X,A,A,A,A,I)') IAM(), 'Bilhetes consultados no ', TRIM(LOPDATA.VPFFILNAM), ':  ', LOPDATA.TOTREADVPF/2
C
       IF (LOPDATA.TOTOTHERR .GT. 0) THEN
         ST = -1
         WRITE(5, '(1X,A,A,I)')     IAM(), ' Bilhetes consultados com erro:            ', LOPDATA.TOTOTHERR
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Existem bilhetes consultados com erro!'
         TYPE*, IAM(), '       '
         GOTO 2000
       ENDIF
C
       IF (LOPDATA.TOTREADVPF .NE. LOPDATA.EMICONF.STCNUMTCK * 2) THEN
         ST = -1
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Total de Registos Consultados no ', TRIM(LOPDATA.VPFFILNAM)
         TYPE*, IAM(), '         é diferente do Número de Bilhetes/Emissão!'
         TYPE*, IAM(), '       '
         GOTO 2000
       ENDIF
C
       WRITE(5, '(1X,A,A,I)'),     IAM(), ' Bilhetes premiados:                ', LOPDATA.TOTVALREC / 2
C
       IF (LOPDATA.TOTINVREC .GT. 0) THEN
         ST = -1
         WRITE(5, '(1X,A,A,I)') IAM(), '  Válidos:                              ', LOPDATA.TOTVALREC
         WRITE(5, '(1X,A,A,I)') IAM(), '  Inválidos:                            ', LOPDATA.TOTINVREC
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Existem bilhetes premiados inválidos!'
         TYPE*, IAM(), '       '
         GOTO 2000
       ENDIF
C
       WRITE(5, '(1X,A,A,I)')     IAM(), ' Bilhetes não premiados:            ', LOPDATA.TOTNFREC / 2
C
       IF ((LOPDATA.TOTVALREC / 2 + LOPDATA.TOTNFREC / 2) .NE. LOPDATA.EMICONF.STCNUMTCK) THEN
         ST = -1
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Total Bilhetes Premiados + Bilhetes Não Premiados é '
         TYPE*, IAM(), '         diferente do Número de Bilhetes/Emissão!'
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '          Total Bilhetes Premiados    : ', LOPDATA.TOTVALREC / 2
         TYPE*, IAM(), '          Total Bilhetes Não Premiados: ', LOPDATA.TOTNFREC / 2
         TYPE*, IAM(), '          Total Bilhetes/Emissão      : ', LOPDATA.EMICONF.STCNUMTCK
         GOTO 2000
       ENDIF
C
C=======================================================================
C       RENAME THE PJMC_LOP TEMPORARY FILE TO ITS FINAL NAME
C=======================================================================
C
       WRITE(LIBCMD,'(A,A,A,A)') '$ RENAME ',TRIM(LOPDATA.TMPFILNAM(6:)),';0 ',TRIM(LOPDATA.LOPFILNAM(6:))
       ST = LIB$SPAWN(TRIM(LIBCMD),,,,,,ST_RENCMD)
       IF (.NOT. ST) CALL LIB$SIGNAL(%VAL(ST))
       IF (.NOT. ST_RENCMD) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Erro a renomear o ficheiro gerado!'
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         Número do Erro: ', ST_RENCMD
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         Comando DCL:'
         TYPE*, IAM(), '       '
         TYPE*, IAM(), ' ', TRIM(LIBCMD)
         TYPE*, IAM(), '       '
         ST = -1
         CALL GSTOP (GEXIT_FATAL)
       ENDIF
C
C=======================================================================
C       LIST THE LATEST VERSION OF THE GENERATED FILES
C=======================================================================
C
       TYPE*, IAM(), '       '
       TYPE*, IAM(), 'Ficheiro gerado:'
       WRITE(LIBCMD,'(A,A,A)') '$ DIR ',TRIM(LOPDATA.LOPFILNAM(6:)),';0 /DATE/SIZE=ALL'
       ST = LIB$SPAWN(TRIM(LIBCMD))
       IF (.NOT. ST) CALL LIB$SIGNAL(%VAL(ST))
       TYPE*, IAM(), '       '
C
       TYPE*, IAM(), 'LOPPAS - Fim do processamento'
       TYPE*, IAM(), '       '
C
       ST = 0
       RETURN
C
2000   CONTINUE
C
C=======================================================================
C       CLOSE FILES
C=======================================================================
C
       CALL USRCLOS1(LOPDATA.TMP_LUN)
       CALL ICLOSE(LOPDATA.VPF_LUN,VPFBUF,ST)
C
C=======================================================================
C       DELETE FILES WITH ERRORS
C=======================================================================
C
C       CALL DFILX(LOPDATA.TMPFILNAM,0,0,ST)
C       IF (ST .NE. 0) CALL GSTOP (GEXIT_FATAL)
C
       TYPE *,IAM(), '     '
       TYPE *,IAM(), 'LOPPAS - Procedimento terminou com erro'
C
       ST = -1
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
       RETURN
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE GENLOPCLA(LOPDATA,ST)
C
C       THIS SUBROUTINE GENERATES THE PJMC_LOP FILE FOR CLASSICA.
C
C       INPUTS:
C        LOPDATA        LOPPAS DATA STRUCTURE
C
C       OUTPUTS:
C        LOPDATA        LOPPAS DATA STRUCTURE
C        ST             STATUS, 0=SUCCESS, ELSE FAILURE
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE GENLOPCLA(LOPDATA,ST)
       IMPLICIT NONE
C
       INCLUDE '(LIB$ROUTINES)'
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:PRMHSH.DEF'
       INCLUDE 'INCLIB:VALPASFIL.DEF'
       INCLUDE 'INCLIB:PASNAM.DEF'
       INCLUDE 'INCLIB:LOPPAS.DEF'
C
       INTEGER*4 ST
       LOGICAL   ISTHERE
C
       INTEGER*4 VPFBUF(I4BUCSIZ)
C
       INTEGER*4 TMP_LUN
       INTEGER*4 LOP_LUN !OFFICIAL PRIZE LIST LUN
       INTEGER*4 VPF_LUN
C
       CHARACTER*27 LOPFILNAM ! INTERFACE FILE FILE NAME FOR PJMC
       CHARACTER*27 TMPFILNAM ! TEMPORARY FILE NAME
       CHARACTER*18 VPFFILNAM ! VPF FILE NAME
C
       CHARACTER*20 VPF_CFILNAM
       INTEGER*4    VPF_IFILNAM(5)
       EQUIVALENCE (VPF_IFILNAM, VPF_CFILNAM)
C
       INTEGER*4 I
       INTEGER*4 PAS_ROUND_VALUE ! FUNCTION
       CHARACTER*50 C50DIV
C
       INTEGER*4 ST_RENCMD
       INTEGER*4 ST_DIRCMD
       CHARACTER*256 LIBCMD
C
       INTEGER*4 VKEY(2)
       INTEGER*4 ERRN
       INTEGER*4 TICNUM,SERNUM,FRANUM
C
       LOGICAL VALDRW /.FALSE./
C
       RECORD /STCEMIS/ RECEMI
       RECORD /STCLOPPAS/ LOPDATA
C
C=======================================================================
C       GET EMISSION INFO
C=======================================================================
C
       TYPE*, IAM(), '       '
       TYPE*, IAM(), 'A obter info da extração ...'
C
       CALL GETEMIDATA(LOPDATA.INDEMIS, LOPDATA.INDPAS, LOPDATA.EXTNUM, LOPDATA.EXTYEAR, RECEMI)
       LOPDATA.EMICONF = RECEMI
C
       WRITE(5,'(1X,A,A,I7)')  IAM(), ' Número da emissão:              ', LOPDATA.EMICONF.STCEMIS
       WRITE(5,'(1X,A,A,I7)')  IAM(), ' Número do plano:                ', LOPDATA.EMICONF.STCPLAN
       WRITE(5,'(1X,A,A,A3)')  IAM(), ' Tipo de emissão:                    ', NAMPLANTYP(LOPDATA.EMICONF.STCEMT)(1:3)
       WRITE(5,'(1X,A,A,I7)')  IAM(), ' Número de séries:               ', LOPDATA.EMICONF.STCNUMSER
C
       IF (LOPDATA.EMICONF.STCNUMSER .LE. 0) THEN
         TYPE*, IAM(),'       '
         TYPE*, IAM(),'LOPPAS - Erro - Número de séries inválido!'
         TYPE*, IAM(),'       '
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
C
       WRITE(5,'(1X,A,A,I7)') IAM(), ' Número de frações/série:        ', LOPDATA.EMICONF.STCNOFFRA
       IF (LOPDATA.EMICONF.STCNOFFRA .LE. 0) THEN
         TYPE*, IAM(),'       '
         TYPE*, IAM(),'LOPPAS - Erro - Número de frações/série inválido!'
         TYPE*, IAM(),'       '
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
C
       WRITE(5,'(1X,A,A,I7)') IAM(), ' Número de bilhetes/emissão:     ', LOPDATA.EMICONF.STCNUMTCK
       IF (LOPDATA.EMICONF.STCNUMTCK .LE. 0) THEN
         TYPE*, IAM(),'       '
         TYPE*, IAM(),'LOPPAS - Erro - Número de bilhetes/emissão inválido!'
         TYPE*, IAM(),'       '
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
C
       WRITE(5,'(1X,A,A,I7)') IAM(), ' Série sorteada:                 ', LOPDATA.EMICONF.STCWSER
       IF (LOPDATA.EMICONF.STCWSER .NE. 0) THEN
         TYPE*, IAM(),'       '
         TYPE*, IAM(),'LOPPAS - Erro - Existe série sorteada!'
         TYPE*, IAM(),'       '
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
C
       TYPE*, IAM(), ' Valor das shares:'
       TYPE*, IAM(), '       '
       LOPDATA.NUMDIVS = LOPDATA.EMICONF.STCDIV
       DO I = 1, LOPDATA.NUMDIVS
         LOPDATA.WSERSHV(I) = PAS_ROUND_VALUE(LOPDATA.EMICONF.STCSHV(I))
         WRITE(C50DIV,'(A,I2,A)') '  Divisão ',I,': '
         TYPE*, IAM(), TRIM(C50DIV), ' ',CMONY(LOPDATA.WSERSHV(I),13,VALUNIT)
       ENDDO
C
C=======================================================================
C       NAME THE FOLLOWING FILES:
C         PJMC_LOP INTERFACE FILE
C         PJMC_LOP TEMPORARY FILE
C=======================================================================
C
       WRITE (LOPFILNAM, FMT='(A14,I2.2,A6,A4)')
     *     'FILE:PJMC_LOP_',LOPDATA.GNUM,LOPDATA.EXTNAM,'.ASC'
       LOPDATA.LOPFILNAM = LOPFILNAM
C
       WRITE (TMPFILNAM, FMT='(A14,I2.2,A6,A4)')
     *     'FILE:PJMC_LOP_',LOPDATA.GNUM,LOPDATA.EXTNAM,'.TMP'
       LOPDATA.TMPFILNAM = TMPFILNAM
       CALL DFILX(LOPDATA.TMPFILNAM,0,0,ST)
       IF (ST .NE. 0) CALL GSTOP (GEXIT_FATAL)
C
C=======================================================================
C       CHECK IF PJMC_LOP FILE ALREADY EXISTS IN THE SYSTEM. IF SO,
C       EXIT THE PROGRAM IMMEDIATELY.
C=======================================================================
C
       INQUIRE(FILE=LOPDATA.LOPFILNAM, EXIST=ISTHERE)
       IF (ISTHERE) THEN
         TYPE*, IAM(),'       '
         TYPE*, IAM(),'LOPPAS - O ficheiro seguinte já existe no sistema:'
         TYPE*, IAM(),'       '
         TYPE*, IAM(),'         '//TRIM(LOPDATA.LOPFILNAM)
         TYPE*, IAM(),'       '
         TYPE*, IAM(),'Para continuar, o ficheiro deverá ser removido'
         TYPE*, IAM(),'       '
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
C
C=======================================================================
C      FIND A FREE LUN TO USE FOR PJMC_LOP TEMPORARY FILE
C=======================================================================
C
       CALL FIND_AVAILABLE_LUN (TMP_LUN,ST)
       IF (ST .NE. 0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Erro a obter uma LUN para o ficheiro: '
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         '//TRIM(LOPDATA.TMPFILNAM(6:))
         TYPE*, IAM(), '       '
         CALL GSTOP (GEXIT_FATAL)
       ENDIF
       LOPDATA.TMP_LUN = TMP_LUN
C
C=======================================================================
C       OPEN THE PJMC_LOP TEMPORARY FILE
C=======================================================================
C
       CALL OPEN_FILASC (LOPDATA.TMPFILNAM,LOPDATA.TMP_LUN,ST)
       IF (ST .NE. 0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Erro a abrir o ficheiro: '
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         '//TRIM(LOPDATA.TMPFILNAM(6:))
         TYPE*, IAM(), '       '
         ! CLOSE AND DELETE CREATED FILES SO FAR
         CALL USRCLOS1(LOPDATA.TMP_LUN)
         CALL DFILX(LOPDATA.TMPFILNAM,0,0,ST)
         CALL GSTOP (GEXIT_FATAL)
       ENDIF
C
C=======================================================================
C       OPEN VALIDATION FILE (VPF)
C=======================================================================
C
       WRITE(VPF_CFILNAM,FMT='(A8,I2.2,I4.4,A4)') 'VALX:VPF',LOPDATA.INDPAS,LOPDATA.DRAWNUM,'.FIL'
       CALL FIND_AVAILABLE_LUN (VPF_LUN,ST)
       IF (ST .NE. 0) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Erro a obter uma LUN para o ficheiro: '
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         '//TRIM(VPF_CFILNAM)
         TYPE*, IAM(), '       '
         CALL FILERR(VPF_IFILNAM,0,ST,0)
         GOTO 2000
       ENDIF
C
       LOPDATA.VPFFILNAM = VPF_CFILNAM
       LOPDATA.VPF_LUN = VPF_LUN
C
       CALL IOPEN(VPF_IFILNAM,LOPDATA.VPF_LUN,VPFLEN*2,VFSCDC,VFSSER*2-1,ST)
       IF (ST .NE. 0) THEN
         CALL FILERR(VPF_IFILNAM,1,ST,0)
         GOTO 2000
       ENDIF
C
C=======================================================================
C       WRITE HEADER INTO THE FILE
C=======================================================================
C
       LOPDATA.TOTLOPREC = 0
       CALL PRINT_PJMC_LOP_HEADER (LOPDATA, ST)
       IF (ST .NE. 0) GOTO 2000
C
C=======================================================================
C       START READING VPF FILE USING A KEY
C=======================================================================
C
       LOPDATA.TOTREADVPF = 0
       LOPDATA.TOTVALREC  = 0
       LOPDATA.TOTINVREC  = 0
       LOPDATA.TOTNFREC   = 0
       LOPDATA.TOTOTHERR  = 0
C
       TYPE*, IAM(), '       '
       TYPE*, IAM(), 'A iniciar processamento ...'
C
       SERNUM = 1
       FRANUM = 1
       DO TICNUM = 0, LOPDATA.EMICONF.STCNUMTCK-1
         CALL FASTSET(0,VVALREC,SIZEOF(VVALREC)/4)
         CALL FASTSET(0,VVDETAIL,SIZEOF(VVDETAIL)/4)
         CALL FASTSET(0, V4BUF_PAS, VPFLEN * VPFMAXREC)
C
         VKEY(1) = FRANUM
         VKEY(2) = ISHFT(SERNUM,24) + TICNUM
C
         CALL IREAD(VKEY,V4BUF_PAS,VPF_LUN,ERRN)
         LOPDATA.TOTREADVPF = LOPDATA.TOTREADVPF + 1
C
         IF (ERRN .EQ. 0) THEN
C
           CALL LOGPAS(VALREC,V4BUF_PAS)
C
C=======================================================================
C             CHECK IF VALREC EQUALS TICNUM-SERNUM-FFRANUM
C=======================================================================
C
             IF (VALREC(VTCKT)  .NE. TICNUM .OR.
     *           VALREC(VSERN)  .NE. FRANUM .OR.
     *           VALREC(VPFRAC) .NE. SERNUM) THEN
               LOPDATA.TOTINVREC = LOPDATA.TOTINVREC + 1
               TYPE*, IAM(), '       '
               TYPE*, IAM(), 'LOPPAS - Bilhete pesquisado diferente do obtido!'
               TYPE*, IAM(), '       '
               TYPE*, IAM(), '         Bilhete pesquisado: '
               TYPE*, IAM(), '          Número           = ', TICNUM
               TYPE*, IAM(), '          Série            = ', FRANUM
               TYPE*, IAM(), '          Fração           = ', SERNUM
               TYPE*, IAM(), '       '
               TYPE*, IAM(), '         Bilhete obtido: '
               TYPE*, IAM(), '          Número           = ', VALREC(VTCKT)
               TYPE*, IAM(), '          Série            = ', VALREC(VSERN)
               TYPE*, IAM(), '          Fração           = ', VALREC(VPFRAC)
               GOTO 2000
             ENDIF
C
C=======================================================================
C           CHECK IF VALREC BELONGS TO THE GAME TYPE WE WANT TO PROCESS
C=======================================================================
C
           IF (VALREC(VGTYP) .NE. LOPDATA.GTYP) THEN
             LOPDATA.TOTINVREC = LOPDATA.TOTINVREC + 1
             TYPE*, IAM(), 'LOPPAS - Tipo de jogo do bilhete inválido!'
             TYPE*, IAM(), '       '
             TYPE*, IAM(), '         Tipo a processar:   ', LOPDATA.INDPAS
             TYPE*, IAM(), '       '
             TYPE*, IAM(), '         Bilhete processado: '
             TYPE*, IAM(), '          Número           = ', VALREC(VTCKT)
             TYPE*, IAM(), '          Série            = ', VALREC(VSERN)
             TYPE*, IAM(), '          Fração           = ', VALREC(VPFRAC)
             TYPE*, IAM(), '           Tipo            = ', VALREC(VGTYP)
             GOTO 2000
           ENDIF
C
C=======================================================================
C           CHECK IF VALREC BELONGS TO THE GAME WE WANT TO PROCESS
C=======================================================================
C
           IF (VALREC(VGAM) .NE. LOPDATA.GNUM) THEN
             LOPDATA.TOTINVREC = LOPDATA.TOTINVREC + 1
             TYPE*, IAM(), 'LOPPAS - Número de jogo do bilhete inválido!'
             TYPE*, IAM(), '       '
             TYPE*, IAM(), '         Jogo a processar:   ', LOPDATA.GNUM
             TYPE*, IAM(), '       '
             TYPE*, IAM(), '         Bilhete processado: '
             TYPE*, IAM(), '          Número           = ', VALREC(VTCKT)
             TYPE*, IAM(), '          Série            = ', VALREC(VSERN)
             TYPE*, IAM(), '          Fração           = ', VALREC(VPFRAC)
             TYPE*, IAM(), '           Jogo            = ', VALREC(VGAM)
             GOTO 2000
           ENDIF
C
C=======================================================================
C                CHECK IF VALREC BELONGS TO THE INDICE OF PASSIVE GAME
C=======================================================================
C
           IF (VALREC(VGIND) .NE. LOPDATA.INDPAS) THEN
             LOPDATA.TOTINVREC = LOPDATA.TOTINVREC + 1
             TYPE*, IAM(), 'LOPPAS - Índice do jogo do bilhete inválido!'
             TYPE*, IAM(), '       '
             TYPE*, IAM(), '         Índice a processar: ', LOPDATA.INDPAS
             TYPE*, IAM(), '       '
             TYPE*, IAM(), '         Bilhete processado: '
             TYPE*, IAM(), '          Número           = ', VALREC(VTCKT)
             TYPE*, IAM(), '          Série            = ', VALREC(VSERN)
             TYPE*, IAM(), '          Fração           = ', VALREC(VPFRAC)
             TYPE*, IAM(), '           Índice          = ', VALREC(VGIND)
             GOTO 2000
           ENDIF
C
C=======================================================================
C       CHECK IF VALIDATION RECORD BELONGS TO THE CHOSEN DRAW
C=======================================================================
C
           CALL DLOGPAS(VALREC,VDETAIL)
           VALDRW = .FALSE.
           DO I=1, VALREC(VPZOFF)
             IF (VDETAIL(VDRW,I) .NE. LOPDATA.DRAWNUM) THEN
               VALDRW = .FALSE.
               EXIT
             ELSE
               VALDRW = .TRUE.
             ENDIF
           ENDDO
C
           IF (.NOT. VALDRW) THEN
             LOPDATA.TOTINVREC = LOPDATA.TOTINVREC + 1
             TYPE*, IAM(), 'LOPPAS - Número da emissão da divisão inválido!'
             TYPE*, IAM(), '       '
             TYPE*, IAM(), '         Número da emissão  = ', LOPDATA.DRAWNUM
             TYPE*, IAM(), '       '
             TYPE*, IAM(), '         Bilhete '
             TYPE*, IAM(), '          Número           = ', VALREC(VTCKT)
             TYPE*, IAM(), '          Série            = ', VALREC(VSERN)
             TYPE*, IAM(), '          Fração           = ', VALREC(VPFRAC)
             TYPE*, IAM(), '           Divisão         = ', I
             TYPE*, IAM(), '            Emissão        = ', VDETAIL(VDRW,I)
             GOTO 2000
           ENDIF
           LOPDATA.TOTVALREC = LOPDATA.TOTVALREC + 1
C
C=======================================================================
C       WRITE BODY RECORDS INTO PJMC_LOP INTERFACE FILE
C=======================================================================
C
           CALL PRINT_PJMC_LOP_BODY_CLA (LOPDATA, VALREC, VDETAIL, ST)
           IF (ST .NE. 0) GOTO 2000
C
         ELSEIF (ERRN .EQ. ERRRNF) THEN
           LOPDATA.TOTNFREC = LOPDATA.TOTNFREC + 1
         ELSE
           LOPDATA.TOTOTHERR = LOPDATA.TOTOTHERR + 1
           TYPE*, IAM(), 'LOPPAS - Erro a obter bilhete do ficheiro: '
           TYPE*, IAM(), '       '
           TYPE*, IAM(), '         '//TRIM(LOPDATA.VPFFILNAM)
           TYPE*, IAM(), '       '
           TYPE*, IAM(), '         Bilhete '
           TYPE*, IAM(), '          Número           = ', VALREC(VTCKT)
           TYPE*, IAM(), '          Série            = ', VALREC(VSERN)
           TYPE*, IAM(), '          Fração           = ', VALREC(VPFRAC)
           CALL FILERR(VPF_IFILNAM,1,ST,0)
           GOTO 2000
         ENDIF
C
         IF (MOD(LOPDATA.TOTREADVPF, 20000) .EQ. 0) THEN
           TYPE*, IAM(), LOPDATA.TOTREADVPF, 'bilhetes processados ...'
         ENDIF
       ENDDO
C
       IF (MOD(LOPDATA.TOTREADVPF, 20000) .NE. 0) THEN
         TYPE*, IAM(), LOPDATA.TOTREADVPF, 'bilhetes processados ...'
       ENDIF
C
C=======================================================================
C       WRITE FOOTER INTO PJMC_LOP INTERFACE FILE
C=======================================================================
C
       CALL PRINT_PJMC_LOP_FOOTER (LOPDATA, ST)
       IF (ST .NE. 0) GOTO 2000
C
C=======================================================================
C       CLOSE FILES
C=======================================================================
C
       CALL USRCLOS1(LOPDATA.TMP_LUN)
       CALL ICLOSE(LOPDATA.VPF_LUN,VPFBUF,ST)
C
C=======================================================================
C       PRINT PROCESS STATISTICS ON SCREEN
C=======================================================================
C
       TYPE*, IAM(), '       '
       WRITE(5, '(1X,A,A,A,A,I)'), IAM(), 'Bilhetes consultados no ', TRIM(LOPDATA.VPFFILNAM), ':  ', LOPDATA.TOTREADVPF
C
       IF (LOPDATA.TOTOTHERR .GT. 0) THEN
         ST = -1
         WRITE(5, '(1X,A,A,I)'),     IAM(), ' Bilhetes consultados com erro:            ', LOPDATA.TOTOTHERR
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Existem bilhetes consultados com erro!'
         TYPE*, IAM(), '       '
         GOTO 2000
       ENDIF
C
       IF (LOPDATA.TOTREADVPF .NE. LOPDATA.EMICONF.STCNUMTCK) THEN
         ST = -1
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Total de Registos Consultados do ', TRIM(LOPDATA.VPFFILNAM),' é'
         TYPE*, IAM(), '         diferente do Número de Bilhetes/Emissão!'
         TYPE*, IAM(), '       '
         GOTO 2000
       ENDIF
C
       WRITE(5, '(1X,A,A,I)'),     IAM(), ' Bilhetes premiados:                ', LOPDATA.TOTVALREC
C
       IF (LOPDATA.TOTINVREC .GT. 0) THEN
         ST = -1
         WRITE(5, '(1X,A,A,I)'), IAM(), '  Válidos:                              ', LOPDATA.TOTVALREC
         WRITE(5, '(1X,A,A,I)'), IAM(), '  Inválidos:                            ', LOPDATA.TOTINVREC
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Existem bilhetes premiados inválidos!'
         TYPE*, IAM(), '       '
         GOTO 2000
       ENDIF
C
       WRITE(5, '(1X,A,A,I)'),     IAM(), ' Bilhetes não premiados:            ', LOPDATA.TOTNFREC
C
       IF ((LOPDATA.TOTVALREC + LOPDATA.TOTNFREC) .NE. LOPDATA.EMICONF.STCNUMTCK) THEN
         ST = -1
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Total Bilhetes Premiados + Bilhetes Não Premiados é '
         TYPE*, IAM(), '         diferente do Número de Bilhetes/Emissão!'
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '          Total Bilhetes Premiados    : ', LOPDATA.TOTVALREC
         TYPE*, IAM(), '          Total Bilhetes Não Premiados: ', LOPDATA.TOTNFREC
         TYPE*, IAM(), '          Total Bilhetes/Emissão      : ', LOPDATA.EMICONF.STCNUMTCK
         TYPE*, IAM(), '       '
         GOTO 2000
       ENDIF
C
C=======================================================================
C       RENAME THE PJMC_LOP TEMPORARY FILE TO ITS FINAL NAME
C=======================================================================
C
       WRITE(LIBCMD,'(A,A,A,A)') '$ RENAME ',TRIM(LOPDATA.TMPFILNAM(6:)),';0 ',TRIM(LOPDATA.LOPFILNAM(6:))
       ST = LIB$SPAWN(TRIM(LIBCMD),,,,,,ST_RENCMD)
       IF (.NOT. ST) CALL LIB$SIGNAL(%VAL(ST))
       IF (.NOT. ST_RENCMD) THEN
         TYPE*, IAM(), '       '
         TYPE*, IAM(), 'LOPPAS - Erro a renomear o ficheiro gerado!'
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         Número do Erro: ', ST_RENCMD
         TYPE*, IAM(), '       '
         TYPE*, IAM(), '         Comando DCL:'
         TYPE*, IAM(), '       '
         TYPE*, IAM(), ' ', TRIM(LIBCMD)
         TYPE*, IAM(), '       '
         ST = -1
         CALL GSTOP (GEXIT_FATAL)
       ENDIF
C
C=======================================================================
C       LIST THE LATEST VERSION OF THE GENERATED FILES
C=======================================================================
C
       TYPE*, IAM(), '       '
       TYPE*, IAM(), 'Ficheiro gerado com sucesso:'
       WRITE(LIBCMD,'(A,A,A)') '$ DIR ',TRIM(LOPDATA.LOPFILNAM(6:)),';0 /DATE/SIZE=ALL'
       ST = LIB$SPAWN(TRIM(LIBCMD))
       IF (.NOT. ST) CALL LIB$SIGNAL(%VAL(ST))
       TYPE*, IAM(), '       '
C
       TYPE*, IAM(), 'LOPPAS - Fim do processamento'
       TYPE*, IAM(), '       '
C
       ST = 0
       RETURN
C
2000    CONTINUE
C
C=======================================================================
C       CLOSE FILES
C=======================================================================
C
       CALL USRCLOS1(LOPDATA.TMP_LUN)
       CALL ICLOSE(LOPDATA.VPF_LUN,VPFBUF,ST)
C
C=======================================================================
C       DELETE FILES WITH ERRORS
C=======================================================================
C
C       CALL DFILX(LOPDATA.TMPFILNAM,0,0,ST)
C       IF (ST .NE. 0) CALL GSTOP (GEXIT_FATAL)
C
       TYPE *,IAM(), '     '
       TYPE *,IAM(), 'LOPPAS - Procedimento terminou com erro'
C
       ST = -1
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
       RETURN
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE GETEMIDATA(INDEMI, INDPAS, WEEK, YEAR, RECEMI)
C
C       THIS SUBROUTINE GETS EMISSION DATA INFO FROM DISK.
C
C       INPUTS:
C        INDEMI         EMISSION INDEX IN MEMORY
C        INDPAS         PASSIVE INDEX
C        WEEK           EMISSION WEEK
C        YEAR           EMISSION YEAR
C
C       OUTPUTS:
C        RECEMI         EMISSION DATA STRUCTURE
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE GETEMIDATA(INDEMI, INDPAS, WEEK, YEAR, RECEMI)
       IMPLICIT NONE

       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:SYSDEFINE.DEF'

       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:DPAREC.DEF'
       INCLUDE 'INCLIB:CONCOM.DEF'
       INCLUDE 'INCLIB:PASCOM.DEF'
       INCLUDE 'INCLIB:LOPPAS.DEF' 
C
C PARAMETERS
C
       INTEGER*4   INDEMI, INDPAS
C
C STRUCT WITH DATA EMISSION
C
       RECORD /STCEMIS/ RECEMI

       LOGICAL     WSER,FPOP,FEXT
C
C LOCAL VARIABLES
C
       INTEGER*4   INDARR, DIV, WEEK, YEAR
C
C CLEAR STRUCT OF EMISSION DATA INFORMATION
C
       CALL FASTSET(0, RECEMI, SIZEOF(RECEMI)/4)
C
       CALL GAMLOGPAS(INDEMI, INDPAS, DPAREC, PASSTS)
C
       CALL GETPASDRW(DPADRAW,WEEK,YEAR)
C
       ! EMISSION NUMBER
       RECEMI.STCEMIS = DPAEMIS
C
       ! EMISSION TYPE
       RECEMI.STCEMT = DPAEMT
C
       ! NUMBER OF PLAN
       RECEMI.STCPLAN = DPAPLAN
C
       ! NUMBER OF TICKETS/EMISSION
       RECEMI.STCNUMTCK = DPANUMTCK
C
       ! # OF SERIES
       RECEMI.STCNUMSER = DPANUMSER
C
       ! WINNING SERIE
       RECEMI.STCWSER = DPAWSER
C
       ! NUMBER OF DIVISIONS
       RECEMI.STCDIV = DPADIV
C
       ! BEGIN SALES DATE
       RECEMI.STCBSD = DPABSD
C
       ! END SALES DATE
       RECEMI.STCESD = DPAESD
C
       ! NUMBER OF FRACTIONS
       RECEMI.STCNOFFRA = DPANOFFRA
C
       ! PURGING CDC OF THIS EMISSION
       RECEMI.STCPRGCDC = DPAPRGCDC
C
       ! REGULAR WINNING NUMBERS
       DO DIV=1, DPADIV
          DO INDARR=1,PAGNBR
             RECEMI.STCWIN(INDARR,DIV) = DPAWIN(INDARR,DIV)
          ENDDO
       ENDDO
C
       ! SHARE VALUE
       DO INDARR=1, DPADIV
         RECEMI.STCSHV(INDARR) = DPASHV(INDARR)
       ENDDO
C
       ! EXTRA SHARE VALUE
       DO INDARR=1, PAGEDV
         RECEMI.STCEXSHV(INDARR) = DPAEXSHV(INDARR)
       ENDDO
C
       ! SHARES
       DO INDARR=1, DPADIV
         RECEMI.STCSHR(INDARR) = DPASHR(INDARR)
       ENDDO
C
       ! ERXTRA SHARES
       DO INDARR=1, PAGEDV
         RECEMI.STCEXSHR(INDARR) = DPAEXSHR(INDARR)
       ENDDO
C
       ! NUMBER OF WINNING NUMBERS
       DO INDARR=1, DPADIV
         RECEMI.STCWNUM(INDARR) = DPAWNUM(INDARR)
       ENDDO
C
       ! PRIZE TYPE
       DO INDARR=1,DPADIV
         RECEMI.STCTYP(INDARR) = DPATYP(INDARR)
       ENDDO
C
       ! # OF DIGITS
       DO INDARR=1, DPADIV
         RECEMI.STCDIG(INDARR) = DPADIG(INDARR)
       ENDDO
C
       ! CROSS REFERENCE
       DO INDARR=1, DPADIV
         RECEMI.STCIDNUM(INDARR) = DPAIDNUM(INDARR)
       ENDDO
C
       RETURN
       END
