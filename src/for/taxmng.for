C
C      TAXMNG.FOR
C
C      PROGRAM TAXMNG (NET PRIZE MANAGER)
C
C      THIS PROGRAM CALCULATES THE NET PRIZE AMOUNT AND GENERATES THE
C      REPORTING ASCII FILES FOR THE FOLLOWING GAMES:
C
C                             TOTOBOLA NORMAL
C                             JOKER
C                             TOTOLOTO SABADO
C                             TOTOLOTO QUARTA
C                             CLASSICA
C                             POPULAR
C                             TOTOBOLA EXTRA 1
C
C      THE NET PRIZE AMOUNT IS CALCULATED BASED ON THE CONFIGURATED
C      VALUES ON THE FILE TAXCONF.FIL (SEE TAXCONFIG.DEF).
C
C      THE TAX AMOUNT IS TRUNCATED (NOT ROUNDED).
C
C      FORMULA USED TO CALCULATE THE NET PRIZE AMOUNT:
C
C        NET PRIZE AMOUNT = PRIZE AMOUNT - TAX AMOUNT
C        TAX AMOUNT = INT( (PRIZE AMOUNT - REFUND AMOUNT) * TAX PERCENTAGE )

C      WHERE,
C
C        INT (X) IS A FUNCTION THAT TRUNCATES THE REAL VALUE X TO
C        ITS INTEGER PART (DECIMAL PART OF X IS DISCARDED)
C
C        (PRIZE AMOUNT - BASE AMOUNT) MUST BE GREATER THAN 0 (ZERO), IN
C         ORDER TO APPLY THE TAX.
C
C        THE VALUES OF BASE AMOUNT, REFUND AMOUNT AND TAX PERCENTAGE
C        DEPENDS ON THE GAME AND ARE CONFIGURATED ON THE FILE TAXCONF.FIL.
C
C
C      FOR MUTUAL GAMES, FOR EACH PRIZE AMOUNT TAXED, THE PROCESS
C      UPDATES THE CORRESPONDING RECORDS ON VLF AND OPS FILES (IF GENERATION 
C      FLAG IS ACTIVE) WITH THE NET PRIZE VALUES, GENERATING AT THE SAME 
C      TIME TWO ASCII FILES:
C      A REPORT AND AN INTERFACE FILE. EACH UPDATED FIELD OF THE RECORD
C      IS SET TO THE CORRESPONDING NET VALUE (THE OLD VALUE, WHICH
C      CORRESPONDS TO THE TOTAL PRIZE VALUE, IS OVERWRITTEN WITH THE NET
C      PRIZE VALUE).
C
C      NOTE: A NEW OPTION (OPGENFLG) WAS ADDED TO BLDSYS PROGRAM THAT SET 
C            THE OP GENERATION RECORDS TO 1-ACTIVE OR 0-INACTIVE. 
C            WHEN INACTIVE, THE UPDATE OF THE OPS FILE RECORDS ISNT DONE.
C
C        FIELDS UPDATED ON VPF FILE:     VALREC(VOPSAMT), VALREC(VKOPSAMT)
C        FIELDS UPDATED ON OPS FILE:     TOTAL_GAME, TOTAL_JOKER
C
C      (IN ORDER TO UPDATE THE VLF RECORD, THE PROGRAM CREATES THE VLC
C       FILE, RENAMING THIS FILE IN THE END TO VLF.)
C
C      GENERATED FILES:
C
C           PREMIOS_IS_<GN>_<CCCYYYY>_yyyymmd.REP (REPORT FILE)
C           IS_<GSD>_yyyymmdd.ASC (INTERFACE FILE FOR SAP INTEGRATION)
C
C
C      FOR PASSIVE GAMES, THE PROCESS DOES NOT UPDATE ANY FILE. IT JUST
C      GENERATES THE REPORTING FILES BASED ON THE VPF AND TPF FILES.
C      THERE ARE TWO MODES OF REPORTING:
C
C        (A) DEFINITIVE RESULTS, WHICH GENERATES
C
C            PREMIOS_IS_<GN>_<CCCYYYY>_yyyymmd.REP (REPORT FILE)
C            IS_<GSD>_yyyymmdd.ASC (INTERFACE FILE FOR SAP INTEGRATION)
C
C            NOTE: IN ORDER TO NOT TAX ANY WINNER TICKET THAT MAY BE
C                  RETURNED OFFLINE AFTER DRAW, THE TICKET STATUS, PBILKOF,
C                  IS CHECKED ON THE TPF FILE.
C
C        (B) PROVISIONAL RESULTS, WHICH GENERATES
C
C            PREMIOS_PREV_IS_<GN>_<CCCYYYY>_yyyymmd.REP (REPORT FILE)
C
C            NOTE: THIS REPORTING MODE DOES NOT USE THE TPF FILE IN
C                  ORDER TO BE GENERATED.
C
C       LEGEND:
C
C            GSD      - GAME SHORT DESCRIPTION
C            GN       - GAME NUMBER
C            CCC      - DRAW NUMBER
C            YYYY     - DRAW YEAR
C            yyyymmdd - CREATION DATE OF THE REPORT
C
C                GAME NAME         GSD                   GN
C            ----------------     ----                   --
C            TOTOBOLA NORMAL      TOTO                   01
C            JOKER                JKER                   05
C            TOTOLOTO SABADO      LOTS                   06
C            TOTOLOTO QUARTA      LOTQ                   07
C            CLASSICA             CLAS                   08
C            POPULAR              POPL                   09
C            TOTOBOLA EXTRA 1     TOT1                   10
C
C V05 04-FEB-2014 SCML BUGFIX ON TCKT VALUE CALCULATION DUE TO REPORTED
C                      SITUATION WHEN THERE ARE NO OPs
C V04 14-NOV-2013 SCML ADDED VALIDATION TO OPGENFLG(OP GENERATION RECORDS
C                      IN OPS FILE).
C V03 12-MAR-2013 SCML ADDED NEW AGENT FIELD TO REPORT (NOT FOR PASSIVE GAMES)
C                      ADDED ORDERING BY PRIZE DIVISION TO REPORT
C                      ADDED TOTAL NUMBER OF TAXED PRIZES TO REPORT
C V02 01-FEV-2013 SCML FIXED NET PRIZE CALCULATION OF PRIZED TICKETS
C                      OF LOTARIA POPULAR WHICH DON'T BELONG TO THE
C                      WINNING SERIE
C V01 28-NOV-2012 SCML INITIAL RELEASE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2012 DJ - SCML. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C====== OPTIONS /CHECK=NOOVERFLOW
        PROGRAM TAXMNG
        IMPLICIT NONE
C
        INCLUDE '(LIB$ROUTINES)'
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:DESLOG.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:PRMLVL.DEF'
        INCLUDE 'INCLIB:RECUSE.DEF'
        INCLUDE 'INCLIB:PRMHSH.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'
C
        INCLUDE 'INCLIB:TAXCONFIG.DEF'
C
        CHARACTER*8  PASPAS
        CHARACTER*20 PASENT
        EQUIVALENCE (PASPAS,PASENT)
C
        CHARACTER*7 CCCAAAA, CONCURSOANO, AAAACCC
        CHARACTER*9 GGAAAACCC
        INTEGER*4 CONCURSO, ANO
        INTEGER*4 DRWN
        INTEGER*4 GETDRW ! FUNCTION
        INTEGER*4 GNUM, GTYP, GAM, VST, GIND
        INTEGER*4 INDDRW, DRWVDT
C
        INTEGER*4 ST
C
        INTEGER*4 TUBSIZ
        PARAMETER (TUBSIZ=I4BUCSIZ*7)
        INTEGER*4 VLFBUF(TUBSIZ), NEWBUF(TUBSIZ)
C
        INTEGER*4 YESNO
        INTEGER*2 DAT(12)
C
        INTEGER*4   CNTREC
        CHARACTER*2 TPREC
C
        INTEGER*4 SZ
C
        INTEGER*4 REP_LUN
        INTEGER*4 SAP_LUN
        CHARACTER*39 REPFILNAM ! REPORT FILE NAME
        CHARACTER*25 SAPFILNAM ! INTERFACE FILE FILE NAME FOR SAP
C----|--!--------------------------------------------------------------
C V03   ! New ordering - start
C----|--!--------------------------------------------------------------
        INTEGER*4 REP_TMP_LUN
        INTEGER*4 REP_SRT_LUN
        CHARACTER*39 REP_TMP_FILNAM ! REPORT TEMP FILE NAME
        CHARACTER*39 REP_SRT_FILNAM ! REPORT SORTED TEMP FILE NAME
        CHARACTER*256 ASCII_REC
        INTEGER*4 TOT_NR_PRZ_IS
C----|--!--------------------------------------------------------------
C V03   ! New ordering - end
C----|--!--------------------------------------------------------------
        
C
        LOGICAL  VALDRW
        LOGICAL  ISTHERE
C
        INTEGER*4 I4TEMP
        INTEGER*2 I2TEMP(2)
        BYTE      I1TEMP(4)
        EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)
C
        INTEGER*4 GAM_LUN,GAMSTS, GAMSEC
        EQUIVALENCE(DLTREC,DSPREC,DKKREC,GAMSTS)
C
        INTEGER*4 CTIM(2), CDAT(8), FDB(7)
C
        DATA GTYP/0/
        DATA GAM_LUN/1/
C
        INTEGER*4 BASAMT, RFNAMT, TAXPER
        INTEGER*4 I
        INTEGER*4 MAXDIV
        PARAMETER(MAXDIV = 40)
        INTEGER*4 NUMDIVS
        INTEGER*4 GAMSHV(MAXDIV), KSPTSHV(MAXDIV)
        INTEGER*4 TOTDIVAMT(MAXDIV), NETDIVAMT(MAXDIV), TOTDIVPRZ(MAXDIV)
        INTEGER*4 KTOTDIVAMT(MAXDIV), KNETDIVAMT(MAXDIV), KTOTDIVPRZ(MAXDIV)
        INTEGER*4 OPAMT, NETOPAMT, KOPAMT, KNETOPAMT, TAXBLAMT
        INTEGER*4 TOTOPAMT, TOTNETOPAMT, TOTTAXBLAMT
        LOGICAL HASTAX /.FALSE./
        LOGICAL FINREP /.FALSE./
        CHARACTER*14 TCKT
C
        CHARACTER(4) GSHDSC(10)
        DATA GSHDSC /'TOTO',
     *               'LOTO','LOT2','TOT2',
     *               'JKER','LOTS','LOTQ','CLAS','POPL','TOT1'/
C
        CHARACTER(8) CONSOREXT(10)
        DATA CONSOREXT /'CONCURSO',
     *               'CONCURSO','CONCURSO','CONCURSO',
     *               'CONCURSO','SORTEIO ','SORTEIO ','EXTRACAO','EXTRACAO','CONCURSO'/
C
        CHARACTER*16 GAMLNAM(MAXGAM)
C
        INTEGER*2 DRDAT(LDATE_LEN) ! DRAWING DATE
        INTEGER*4 TOTSAPREC
        INTEGER*4 K
        CHARACTER*20 C20GNUM/'                    '/
        INTEGER*4 DDRW
        CHARACTER*50 C50DIV
        CHARACTER*256 LIBCMD
        INTEGER*4 TOTVALREC, TOTGTBASAMT
C
        INTEGER*4 TOTWRTVLC, TOTREADVLF, TOTUPDOPS, TOTUPDVLF
        BYTE      V1BUF(VFLEN*4*4)
        EQUIVALENCE (V4BUF,V1BUF)
        CHARACTER*20 C16BUF /'                    '/
        CHARACTER*20 C16BUF2 /'                    '/
C
C=======================================================================
C       PRINT USER INTERFACE INTO SCREEN
C=======================================================================
C
        CALL CLRSCR(5)
        TYPE*,IAM(),' '
        TYPE*,IAM(),'-----------------------------------------------------------'
        TYPE*,IAM(),'<<<<< TAXMNG - CALCULO DOS PREMIOS LIQUIDOS           >>>>>'
        TYPE*,IAM(),'      GERA PREMIOS_IS_<GN>_<CCCAAAA>_aaaammdd.REP          '
        TYPE*,IAM(),'      GERA PREMIOS_PREV_IS_<GN>_<CCCAAAA>_aaaammdd.REP     '
        TYPE*,IAM(),'      GERA IS_<GSD>_aaaammdd.ASC (interface para o SAP)    '
        TYPE*,IAM(),' '
        TYPE*,IAM(),'        GN = numero do jogo                                '
        TYPE*,IAM(),'        CCC = numero do sorteio/extraccao/concurso         '
        TYPE*,IAM(),'        AAAA = ano do sorteio/extraccao/concurso           '
        TYPE*,IAM(),'        aaaammdd = data de geracao do ficheiro             '
        TYPE*,IAM(),'        GSD = descricao abreviada do nome do jogo          '
        TYPE*,IAM(),'-----------------------------------------------------------'
C
C----|--!--------------------------------------------------------------
C V04   ! Adding new OP validation FLAG -start
C----|--!--------------------------------------------------------------
C
        IF (P(OPGENFLG).EQ.1) THEN
          TYPE*,IAM(),'<<<<< CERTIFIQUE-SE QUE HA UMA COPIA DE SEGURANCA DOS >>>>>'
          TYPE*,IAM(),'<<<<< FICHEIROS OPS e VLF ANTES DO PROCESSAMENTO DOS  >>>>>'
          TYPE*,IAM(),'<<<<< JOGOS DE APOSTAS MUTUAS                         >>>>>'
        ELSEIF (P(OPGENFLG).EQ.0) THEN
          TYPE*,IAM(),'<<<<< CERTIFIQUE-SE QUE HA UMA COPIA DE SEGURANCA DO  >>>>>'
          TYPE*,IAM(),'<<<<< FICHEIRO VLF ANTES DO PROCESSAMENTO DOS JOGOS   >>>>>'
          TYPE*,IAM(),'<<<<< DE APOSTAS MUTUAS                         >>>>>'
        ELSE
          TYPE*,IAM(),'<<<<< PARAMETRO P(OPGENFLG) INVALIDO >',P(OPGENFLG),' >>>>>' 
          CALL GSTOP(GEXIT_OPABORT)
        ENDIF
C
C----|--!--------------------------------------------------------------
C V04   ! Adding new OP validation FLAG -end
C----|--!--------------------------------------------------------------
C
        TYPE*,IAM(),'-----------------------------------------------------------'
C
C=======================================================================
C       CHOOSE GAME
C=======================================================================
C
100     CONTINUE
        TYPE*, IAM(), '       '
        CALL GAME_TYPNDX(GNUM, GTYP, GIND, GAMLNAM, ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(),'       '
          CALL GSTOP(GEXIT_OPABORT)
        ENDIF
        IF (GNUM .EQ. 8 .OR. GNUM .EQ. 9) THEN
          CALL PASTAX (GNUM, GSHDSC(GNUM), GAMLNAM(GNUM), ST)
          GOTO 100
        END IF
C
C=======================================================================
C       CHOOSE DRAW
C=======================================================================
C
        CALL WIMG(5,'DESEJA PROCESSAR QUAL '//CONSOREXT(GNUM)//' (CCCAAAA)?')
        READ(5,102) CONCURSOANO
        TYPE*,IAM(),CONCURSOANO
C
        IF (CONCURSOANO .EQ. 'e' .OR. CONCURSOANO .EQ. 'E' .OR.
     *     TRIM(CONCURSOANO) .EQ. '') GOTO 100
C
        ANO      = MOD(CTOI(CONCURSOANO,SZ) , 10000)
        CONCURSO = INT(CTOI(CONCURSOANO,SZ) / 10000)
C
        IF (ANO .LT. 2005 .OR. ANO .GT. 2100 .OR.
     *      CONCURSO .GT. 105 .OR. CONCURSO .LE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Ano/Concurso invalido'
          GOTO 100
        ENDIF
C
        WRITE(CCCAAAA,FMT='(I3.3,I4.4)') CONCURSO, ANO
        WRITE(AAAACCC,FMT='(I4.4,I3.3)') ANO, CONCURSO
        WRITE(GGAAAACCC,FMT='(I2.2,I4.4,I3.3)') GNUM, ANO, CONCURSO
C
        IF (ANO .LE. 2012) THEN
          CALL PRMYESNO('O '//CONSOREXT(GNUM)//' inserido e anterior a 0012013. Deseja continuar [Y/N]? ', YESNO)
          IF (YESNO .NE. 1) GOTO 100
        ENDIF
C
        CALL PRMYESNO('Confirma o '//CONSOREXT(GNUM)//' '//CCCAAAA//' [Y/N]? ', YESNO)
        IF (YESNO .NE. 1) GOTO 100
C
        DRWN = GETDRW(ANO,CONCURSO,GNUM) ! GET DRAW NUMBER
C
        IF (DRWN .LE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - '//CONSOREXT(GNUM) //' invalido'
          GOTO 100
        ENDIF
C
C=======================================================================
C       NAME THE FOLLOWING FILES:
C         REPORT FILE
C         SAP INTERFACE FILE
C=======================================================================
C
        CALL ICLOCK(1,CTIM)
        CALL GDATE(CDAT(2),CDAT(3),CDAT(1))
        IF(CDAT(1) .LT. 77) THEN
          CDAT(1) = CDAT(1) + 2000
        ELSE
          CDAT(1) = CDAT(1) + 1900
        ENDIF
C
        WRITE (REPFILNAM, FMT='(A16,I2.2,A1,A7,A1,I4.4,I2.2,I2.2,A4)')
     *     'FILE:PREMIOS_IS_',GNUM,'_',CCCAAAA,'_',CDAT(1),CDAT(2),CDAT(3),'.REP'
C----|--!--------------------------------------------------------------
C V03   ! New ordering - start
C----|--!--------------------------------------------------------------
        TOT_NR_PRZ_IS = 0
        WRITE (REP_TMP_FILNAM
     *   , FMT='(A16,I2.2,A1,A7,A1,I4.4,I2.2,I2.2,A4)')
     *     'FILE:PREMIOS_IS_', GNUM, '_', CCCAAAA,'_', CDAT(1), CDAT(2)
     *   , CDAT(3), '.TMP'
        WRITE (REP_SRT_FILNAM
     *   , FMT='(A16,I2.2,A1,A7,A1,I4.4,I2.2,I2.2,A4)')
     *     'FILE:PREMIOS_IS_', GNUM, '_', CCCAAAA,'_', CDAT(1), CDAT(2)
     *   , CDAT(3), '.SRT'
C----|--!--------------------------------------------------------------
C V03   ! New ordering - end
C----|--!--------------------------------------------------------------
C
        WRITE (SAPFILNAM, FMT='(A8,A4,A1,I4.4,I2.2,I2.2,A4)')
     *     'FILE:IS_',GSHDSC(GNUM),'_',CDAT(1),CDAT(2),CDAT(3),'.ASC'
C
C=======================================================================
C       CHECK IF REPORTING FILES ALREADY EXISTS IN THE SYSTEM. IF SO,
C       EXIT THE PROGRAM IMMEDIATELY.
C=======================================================================
C
        INQUIRE(FILE=REPFILNAM, EXIST=ISTHERE)
        IF (ISTHERE) THEN
          INQUIRE(FILE=SAPFILNAM, EXIST=ISTHERE)
          IF (ISTHERE) THEN
            TYPE*, IAM(),'       '
            TYPE*, IAM(),'TAXMNG - Os ficheiros seguintes ja existem no sistema:'
            TYPE*, IAM(),'       '
            TYPE*, IAM(),'         '//TRIM(REPFILNAM)
            TYPE*, IAM(),'         '//TRIM(SAPFILNAM)
            TYPE*, IAM(),'       '
            TYPE*, IAM(),'Para continuar, os ficheiros deverao ser removidos'
            TYPE*, IAM(),'       '
            CALL GSTOP(GEXIT_FATAL)
          ELSE
            TYPE*, IAM(),'       '
            TYPE*, IAM(),'TAXMNG - O ficheiro seguinte ja existe no sistema:'
            TYPE*, IAM(),'       '
            TYPE*, IAM(),'         '//TRIM(REPFILNAM)
            TYPE*, IAM(),'       '
            TYPE*, IAM(),'Para continuar, os ficheiros deverao ser removidos'
            TYPE*, IAM(),'       '
            CALL GSTOP(GEXIT_FATAL)
          ENDIF
        ENDIF
        INQUIRE(FILE=SAPFILNAM, EXIST=ISTHERE)
        IF (ISTHERE) THEN
          TYPE*, IAM(),'       '
          TYPE*, IAM(),'TAXMNG - O ficheiro seguinte ja existe no sistema:'
          TYPE*, IAM(),'       '
          TYPE*, IAM(),'         '//TRIM(SAPFILNAM)
          TYPE*, IAM(),'       '
          TYPE*, IAM(),'Para continuar, o ficheiro devera ser removido'
          TYPE*, IAM(),'       '
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C----|--!--------------------------------------------------------------
C V03   ! New ordering - start
C----|--!--------------------------------------------------------------
        INQUIRE(FILE=REP_TMP_FILNAM, EXIST=ISTHERE)
        IF (ISTHERE) THEN
            ! Delete file
            CALL DFILX(REP_TMP_FILNAM,0,0,ST)
        ENDIF
        INQUIRE(FILE=REP_SRT_FILNAM, EXIST=ISTHERE)
        IF (ISTHERE) THEN
            ! Delete file
            CALL DFILX(REP_SRT_FILNAM,0,0,ST)
        ENDIF
C----|--!--------------------------------------------------------------
C V03   ! New ordering - end
C----|--!--------------------------------------------------------------

C
C=======================================================================
C       READ GAME FILE (this subroutine open, read and close the file)
C=======================================================================
C
        CALL FIND_AVAILABLE_LUN (GAM_LUN,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          WRITE(C20GNUM,'(I0)') GNUM
          TYPE*, IAM(), 'TAXMNG - Erro a obter uma LUN para o jogo#'//TRIM(C20GNUM)
          TYPE*, IAM(), '       '
          CALL GSTOP (GEXIT_FATAL)
        ENDIF
C
        IF (GTYP .EQ. TSPT) GAMSEC = DSPSEC
        IF (GTYP .EQ. TLTO) GAMSEC = DLTSEC
        IF (GTYP .EQ. TKIK) GAMSEC = DKKSEC
        CALL READGFL(GAM_LUN,GFNAMES(1,GNUM),GAMSEC,DRWN,GAMSTS)
C
C=======================================================================
C       GET DRAW DATE
C=======================================================================
C
        IF (GTYP .EQ. TLTO) THEN
          DRDAT(VCDC) = DLTDAT(CURDRW)
          CALL LCDATE(DRDAT(1))
        ELSEIF(GTYP .EQ. TKIK) THEN
          DRDAT(VCDC) = DKKDAT(CURDRW)
          CALL LCDATE(DRDAT(1))
        ELSEIF(GTYP .EQ. TSPT) THEN
          DRDAT(VCDC) = DSPDAT(CURDRW)
          CALL LCDATE(DRDAT(1))
        ENDIF
C
C=======================================================================
C       CHECK IF DRAW RESULTS ARE FINAL
C=======================================================================
C
        IF (GAMSTS .LT. GFINAL) THEN
          TYPE*,IAM(),'       '
          TYPE*,IAM(),'TAXMNG - Premios nao apurados para o '//CONSOREXT(GNUM)//' ', CCCAAAA
          GOTO 100
        ENDIF
C
C=======================================================================
C       IF FILE OPS.FIL DOESN'T EXIST, EXIT PROGRAM
C=======================================================================
C
        INQUIRE(FILE='FILE:OPS.FIL', EXIST=ISTHERE)
        IF (.NOT. ISTHERE) THEN
          TYPE*, IAM(),'       '
          TYPE*, IAM(),'TAXMNG - Nao foi encontrado o ficheiro OPS.FIL'
          TYPE*, IAM(),'       '
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C=======================================================================
C       IF FILE TAXCONF.FIL DOESN'T EXIST, EXIT PROGRAM
C=======================================================================
C
        TYPE*, IAM(), '       '
        TYPE*, IAM(), 'A obter a configuracao do imposto de selo a aplicar...'
C
        INQUIRE(FILE='FILE:TAXCONF.FIL', EXIST=ISTHERE)
        IF (.NOT. ISTHERE) THEN
          TYPE*, IAM(),'       '
          TYPE*, IAM(),'TAXMNG - Nao foi encontrado o ficheiro TAXCONF.FIL'
          TYPE*, IAM(),'       '
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C=======================================================================
C       OPEN TAX CONFIG FILE AND LOAD CONFIGURATION VALUES
C=======================================================================
C
        CALL OPENX(1,'FILE:TAXCONF.FIL',4,0,0,ST)
        CALL IOINIT(FDB,1,TXCF_SEC*256)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(),'TAXMNG - Nao foi possivel abrir o ficheiro TAXCONF.FIL'
          TYPE*, IAM(), '       '
          CALL CLOSEFIL(FDB)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
        CALL READW(FDB,1,TXCF_REC,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(),'TAXMNG - Nao foi possivel ler o ficheiro TAXCONF.FIL'
          TYPE*, IAM(), '       '
          CALL CLOSEFIL(FDB)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
        CALL CLOSEFIL(FDB)
C
C=======================================================================
C       SET THE TAX PERCENTAGE, BASE AMOUNT AND REFUND AMOUNT, BASED
C       ON THE GAME NUMBER
C=======================================================================
C
        IF ((GNUM .EQ.  1) .OR. ! TOTOBOLA NORMAL
     *      (GNUM .EQ.  5) .OR. ! JOKER
     *      (GNUM .EQ.  6) .OR. ! TOTOLOTO QUARTA
     *      (GNUM .EQ.  7) .OR. ! TOTOLOTO SABADO
     *      (GNUM .EQ. 10)      ! TOTOBOLA EXTRA 1
     *     ) THEN
          TAXPER = TXCF_AMTAX
          BASAMT = TXCF_AMBSAMNT
          RFNAMT = TXCF_AMTAXRFN
        ENDIF
        WRITE(5,'(1X,A,A,F11.2)') IAM(), ' Taxa de imposto:               ', DISPER(TAXPER * 10)
        WRITE(5,'(1X,A,A,A11)')   IAM(), ' Menor premio aplicavel:        ', CMONY(BASAMT,11,VALUNIT)
        WRITE(5,'(1X,A,A,A11)')   IAM(), ' Valor a amortizar no imposto:  ', CMONY(RFNAMT,11,VALUNIT)
C
        IF (BASAMT .LT. RFNAMT) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(),'TAXMNG - Menor premio aplicavel MENOR'
          TYPE*, IAM(),'         que valor a amortizar no imposto!'
          TYPE*, IAM(), '       '
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C=======================================================================
C       LOAD SHARE VALUES FOR THE CHOSEN GAME AND DRAW
C=======================================================================
C
        TYPE*, IAM(), '       '
        TYPE*, IAM(), 'A obter o valor das shares do '//CONSOREXT(GNUM)//' '//CCCAAAA
        TYPE*, IAM(), 'do '//TRIM(GAMLNAM(GNUM))//'...'
        CALL GET_SHV(GNUM,DRWN,GAMSHV,NUMDIVS)
C
        DO I=1,NUMDIVS
          WRITE(C50DIV,'(A,I0,A)') ' Divisao ',I,': '
          TYPE*, IAM(), TRIM(C50DIV), ' ', CMONY(GAMSHV(I),11,VALUNIT)
        ENDDO
C
C=======================================================================
C       LOAD JOKER SHARE VALUES IF IT HAS BEEN CHOSEN TOTOBOLA NORMAL
C=======================================================================
C
        IF (GNUM .EQ. 1) THEN
           NUMDIVS = 0
           CALL FASTSET(0, KSPTSHV, SIZEOF(KSPTSHV) / 4)
           IF(DRWN .LE. DAYHDR(5)) THEN  ! IF JOKER GAME STILL ACTIVE IN THE SYSTEM
              TYPE*, IAM(), '       '
              TYPE*, IAM(), 'A obter o valor das shares do '//TRIM(CONSOREXT(5))//' '//CCCAAAA
              TYPE*, IAM(), 'do '//TRIM(GAMLNAM(5))//'...'
              CALL GET_SHV(5,DRWN,KSPTSHV,NUMDIVS)
C             
              DO I=1,NUMDIVS
                WRITE(C50DIV,'(A,I0,A)') ' Divisao ',I,': '
                TYPE*, IAM(), TRIM(C50DIV), ' ', CMONY(KSPTSHV(I),11,VALUNIT)
              ENDDO
           ENDIF
        ENDIF
C
C=======================================================================
C      FIND A FREE LUN TO USE FOR REPORT FILE
C=======================================================================
C
        CALL FIND_AVAILABLE_LUN (REP_LUN,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Erro a obter uma LUN para o ficheiro:'
          TYPE*, IAM(), '       '
          TYPE*, IAM(), '         '//TRIM(REPFILNAM(6:))
          TYPE*, IAM(), '       '
          CALL GSTOP (GEXIT_FATAL)
        ENDIF

C
C=======================================================================
C       OPEN THE REPORT FILE
C=======================================================================
C
        CALL OPEN_FILASC (REPFILNAM,REP_LUN,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Erro a abrir o ficheiro:'
          TYPE*, IAM(), '       '
          TYPE*, IAM(), '         '//TRIM(REPFILNAM(6:))
          TYPE*, IAM(), '       '
          CALL USRCLOS1(REP_LUN)
          CALL GSTOP (GEXIT_FATAL)
        ENDIF
C
C=======================================================================
C      FIND A FREE LUN TO USE FOR SAP FILE
C=======================================================================
C
        CALL FIND_AVAILABLE_LUN (SAP_LUN,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Erro a obter uma LUN para o ficheiro:'
          TYPE*, IAM(), '       '
          TYPE*, IAM(), '         '//TRIM(SAPFILNAM(6:))
          TYPE*, IAM(), '       '
          ! CLOSE AND DELETE CREATED FILES UNTIL NOW
          CALL USRCLOS1(REP_LUN)
          CALL DFILX(REPFILNAM,0,0,ST)
          CALL GSTOP (GEXIT_FATAL)
        ENDIF
C
C=======================================================================
C       OPEN THE SAP FILE
C=======================================================================
C
        CALL OPEN_FILASC (SAPFILNAM,SAP_LUN,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Erro a abrir o ficheiro: '
          TYPE*, IAM(), '       '
          TYPE*, IAM(), '         '//TRIM(SAPFILNAM(6:))
          TYPE*, IAM(), '       '
          ! CLOSE AND DELETE CREATED FILES SO FAR
          CALL USRCLOS1(REP_LUN)
          CALL USRCLOS1(SAP_LUN)
          CALL DFILX(REPFILNAM,0,0,ST)
          CALL DFILX(SAPFILNAM,0,0,ST)
          CALL GSTOP (GEXIT_FATAL)
        ENDIF
C----|--!--------------------------------------------------------------
C V03   ! New ordering - start
C----|--!--------------------------------------------------------------
        CALL FIND_AVAILABLE_LUN (REP_TMP_LUN,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Erro a obter uma LUN para o ficheiro:'
          TYPE*, IAM(), '       '
          TYPE*, IAM(), '         '//TRIM(REP_TMP_FILNAM(6:))
          TYPE*, IAM(), '       '
          CALL GSTOP (GEXIT_FATAL)
        ENDIF
C
C=======================================================================
C       OPEN THE REP TMP FILE
C=======================================================================
C
        CALL OPEN_FILASC (REP_TMP_FILNAM,REP_TMP_LUN,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Erro a abrir o ficheiro: '
          TYPE*, IAM(), '       '
          TYPE*, IAM(), '         '//TRIM(REP_TMP_FILNAM(6:))
          TYPE*, IAM(), '       '
          ! CLOSE AND DELETE CREATED FILES SO FAR
          CALL USRCLOS1(REP_LUN)
          CALL USRCLOS1(SAP_LUN)
          CALL USRCLOS1(REP_TMP_LUN)
          CALL DFILX(REPFILNAM,0,0,ST)
          CALL DFILX(SAPFILNAM,0,0,ST)
          CALL DFILX(REP_TMP_FILNAM,0,0,ST)
          CALL GSTOP (GEXIT_FATAL)
        ENDIF
C----|--!--------------------------------------------------------------
C V03   ! New ordering - end
C----|--!--------------------------------------------------------------

C
C=======================================================================
C       WRITE HEADER INTO EACH FILE
C=======================================================================
C
        FINREP = .TRUE.
        CALL PRINT_REPTXDPRZ_HEADER (REP_LUN,GNUM,CONCURSO,ANO,CDAT,
     *                               FINREP)
C
        TOTSAPREC = 0
        CALL PRINT_SAPTXDPRZ_HEADER (SAP_LUN,GNUM,DRDAT,CONCURSO,ANO)
        TOTSAPREC = TOTSAPREC + 1
C
C=======================================================================
C       OPEN VALIDATION FILE (VLF)
C=======================================================================
C
        CALL IOPEN(SFNAMES(1,VLF),VLF,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
        IF (ST .NE. 0) THEN
          CALL FILERR(SFNAMES(1,VLF),1,ST,0)
          GOTO 2000
        ENDIF
        CALL ITUBSIZE(VLF,TUBSIZ)
C
C=======================================================================
C       CREATE NEW VLC (VALIDATION COPY FILE) FILE AND THEN OPEN IT
C=======================================================================
C
        TYPE*, IAM(), '       '
        CALL CRTFIL(SFNAMES(1,VLC),SFSIZES(VLC),ST)
        IF(ST .NE. 0) THEN
          WRITE(5,'(1X,A,A,4A4)') IAM(),
     *                            'Erro a alocar o ficheiro ',
     *                            (SFNAMES(K,VLC),K=1,5)
          ! CLOSE AND DELETE CREATED FILES SO FAR
          CALL USRCLOS1(REP_LUN)
          CALL USRCLOS1(SAP_LUN)
C----|--!--------------------------------------------------------------
C V03   ! New ordering - start
C----|--!--------------------------------------------------------------
          CALL USRCLOS1(REP_TMP_LUN)
C----|--!--------------------------------------------------------------
C V03   ! New ordering - end
C----|--!--------------------------------------------------------------          
          CALL DFILX(REPFILNAM,0,0,ST)
          CALL DFILX(SAPFILNAM,0,0,ST)
C----|--!--------------------------------------------------------------
C V03   ! New ordering - start
C----|--!--------------------------------------------------------------
          CALL DFILX(REP_TMP_FILNAM,0,0,ST)
C----|--!--------------------------------------------------------------
C V03   ! New ordering - end
C----|--!--------------------------------------------------------------          
          CALL GSTOP (GEXIT_FATAL)
        ENDIF
C
        CALL IOPEN(SFNAMES(1,VLC),VLC,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
        IF (ST .NE. 0) THEN
          CALL FILERR(SFNAMES(1,VLC),1,ST,0)
          GOTO 2000
        ENDIF
        CALL ITUBSIZE(VLC,TUBSIZ)
C
        TYPE*, IAM(), '       '
        TYPE*, IAM(),'A calcular o valor liquido dos premios, aguarde...'
C
C=======================================================================
C       START READING SEQUENTIALLY VLF FILE
C=======================================================================
C
        TOTOPAMT = 0
        TOTNETOPAMT = 0
        TOTTAXBLAMT = 0
        TOTVALREC = 0
        TOTGTBASAMT = 0
        TAXBLAMT = 0
C
        TOTREADVLF = 0
        TOTWRTVLC = 0
        TOTUPDOPS = 0
        TOTUPDVLF = 0
300     CONTINUE
C
        CALL ISREAD(V4BUF,VLF,VLFBUF,ST)
        IF (ST .EQ. ERREND) GOTO 1000
C
        TOTREADVLF = TOTREADVLF + 1
        IF (ST .NE. 0) THEN
          CALL FILERR(SFNAMES(1,VLF),2,ST,0)
          CALL GPAUSE
          GOTO 300
        ENDIF
C
        CALL LOGVAL(VALREC,V4BUF)
C
C=======================================================================
C       CHECK IF VALREC BELONGS TO THE GAME WE WANT TO PROCESS
C=======================================================================
C
        IF (VALREC(VGAM) .NE. GNUM) THEN
         CALL ISWRIT(V4BUF,VLC,NEWBUF,ST)
         IF(ST .NE. 0) CALL FILERR(SFNAMES(1,VLC),3,ST,0)
         TOTWRTVLC = TOTWRTVLC + 1
         GOTO 300
        ENDIF
C
C=======================================================================
C       CHECK IF VALIDATION RECORD BELONGS TO THE CHOSEN DRAW
C=======================================================================
C
        CALL DLOGVAL(VALREC,VDETAIL)
        VALDRW = .FALSE.
        DO I=1, VALREC(VPZOFF)
          IF (VDETAIL(VDRW,I) .EQ. DRWN) THEN
            VALDRW = .TRUE.
            EXIT
          ENDIF
        ENDDO
        IF (.NOT. VALDRW) THEN
          CALL ISWRIT(V4BUF,VLC,NEWBUF,ST)
          IF(ST .NE. 0) CALL FILERR(SFNAMES(1,VLC),3,ST,0)
          TOTWRTVLC = TOTWRTVLC + 1
          GOTO 300
        ENDIF
        TOTVALREC = TOTVALREC + 1
C
C=======================================================================
C       CHECK FOR RESULTS NOT IN OF CHOSEN DRAW
C=======================================================================
C
        VST = V1BUF(VFSTS)
        IF (VST .EQ. VNOPAY .OR.
     *      VST .EQ. VNOPRZ .OR.
     *      VST .EQ. VPOST  .OR.
     *      VST .EQ. VPPNPZ .OR.
     *      VST .EQ. VPRPOST) THEN
C
          TYPE*, IAM(), 'O valor do premio ainda nao foi atribuido!'
          CALL GPAUSE
          CALL ISWRIT(V4BUF,VLC,NEWBUF,ST)
          IF(ST .NE. 0) CALL FILERR(SFNAMES(1,VLC),3,ST,0)
          TOTWRTVLC = TOTWRTVLC + 1
          GOTO 300
        ENDIF
C
C=======================================================================
C       CHECK IF VALIDATION RECORD SHOULD BE SUBJECT TO CHECKAMTAX
C=======================================================================
C
        IF (VALREC(VPAMT) .LE. BASAMT .AND. VALREC(VKPAMT) .LE. BASAMT) THEN
          CALL ISWRIT(V4BUF,VLC,NEWBUF,ST)
          IF(ST .NE. 0) CALL FILERR(SFNAMES(1,VLC),3,ST,0)
          TOTWRTVLC = TOTWRTVLC + 1
          GOTO 300
        ENDIF
        TOTGTBASAMT = TOTGTBASAMT + 1
C
        HASTAX = .FALSE.
        CALL CHECKAMTAX (VALREC,BASAMT,RFNAMT,TAXPER,GAMSHV,KSPTSHV,DRWN,
     *                 TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,OPAMT,NETOPAMT,
     *                 KTOTDIVAMT,KNETDIVAMT,KTOTDIVPRZ,KOPAMT,KNETOPAMT,
     *                 HASTAX)
C
C=======================================================================
C       CHECK IF THE OPAMT/KOPAMT VALUE EQUALS
C       VALREC(VPAMT)/VALREC(VKPAMT) VALUE. IF THEY DIFFER, IT MEANS
C       THAT SOME PROBLEM OCCURRED DURING THE CALCULATION OF NET PRIZES
C=======================================================================
C
        IF (OPAMT .NE. VALREC(VPAMT) .OR.
     *      KOPAMT .NE. VALREC(VKPAMT)) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Erro no calculo do premio liquido!'
          TYPE*, IAM(), '       '
          TYPE*, IAM(), '         OPAMT diferente de VALREC(VPAMT)'
          TYPE*, IAM(), '                       e/ou              '
          TYPE*, IAM(), '         KOPAMT diferente de VALREC(VKPAMT)'
          TYPE*, IAM(), '       '
          ! DEBUG
          TYPE*, IAM(), '         Input:'
          TYPE*, IAM(), '           VALREC(VPAMT)             = ',VALREC(VPAMT)
          TYPE*, IAM(), '           VALREC(VKPAMT)            = ',VALREC(VKPAMT)
          TYPE*, IAM(), '           VALREC(VSCDC)             = ',VALREC(VSCDC)
          TYPE*, IAM(), '           VALREC(VSSER)             = ',VALREC(VSSER)
          TYPE*, IAM(), '           VALREC(VSTER)             = ',VALREC(VSTER)
          TYPE*, IAM(), '           BASAMT                    = ',BASAMT
          TYPE*, IAM(), '           RFNAMT                    = ',RFNAMT
          TYPE*, IAM(), '           TAXPER                    = ',TAXPER
          TYPE*, IAM(), '           DRWN                      = ',DRWN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), '         Output:'
          DO I=1,MAXDIV
            IF (TOTDIVPRZ(I) .NE. 0) THEN
              TYPE*, IAM(), '           TOTDIVPRZ(',I,')  = ',TOTDIVPRZ(I)
              TYPE*, IAM(), '           TOTDIVAMT(',I,')  = ',TOTDIVAMT(I)
              TYPE*, IAM(), '           NETDIVAMT(',I,')  = ',NETDIVAMT(I)
            ENDIF
            IF (KTOTDIVPRZ(I) .NE. 0) THEN
              TYPE*, IAM(), '         KTOTDIVPRZ(',I,') = ',KTOTDIVPRZ(I)
              TYPE*, IAM(), '         KTOTDIVAMT(',I,') = ',KTOTDIVAMT(I)
              TYPE*, IAM(), '         KNETDIVAMT(',I,') = ',KNETDIVAMT(I)
            ENDIF
          ENDDO
          TYPE*, IAM(), '           NETOPAMT                  = ',NETOPAMT
          TYPE*, IAM(), '           OPAMT                     = ',OPAMT
          TYPE*, IAM(), '           KNETOPAMT                 = ',KNETOPAMT
          TYPE*, IAM(), '           KOPAMT                    = ',KOPAMT
          TYPE*, IAM(), '           HASTAX                    =           ',HASTAX
          TYPE*, IAM(), '       '
C
          CALL GPAUSE
          GOTO 300
        ENDIF
C
        IF (.NOT. HASTAX) THEN
          CALL ISWRIT(V4BUF,VLC,NEWBUF,ST)
          IF(ST .NE. 0) CALL FILERR(SFNAMES(1,VLC),3,ST,0)
          TOTWRTVLC = TOTWRTVLC + 1
          GOTO 300
        ENDIF
C
C=======================================================================
C       WRITE INTO VLC, OPS AND UPDATE THE REPORT
C=======================================================================
C
        IF (HASTAX) THEN
          CALL UPDATE_VALREC (VALREC,BASAMT,NETOPAMT,KNETOPAMT,V4BUF)
          CALL ISWRIT(V4BUF,VLC,NEWBUF,ST)
          IF(ST .NE. 0) CALL FILERR(SFNAMES(1,VLC),3,ST,0)
          TOTWRTVLC = TOTWRTVLC + 1
          TOTUPDVLF = TOTUPDVLF + 1
C
C----|--!--------------------------------------------------------------
C V04   ! Adding new OP validation FLAG -start
C----|--!--------------------------------------------------------------
C----+------------------------------------------------------------------
C V05| BUGFIX ON TCKT VALUE CALCULATION DUE TO REPORTED SITUATION
C    | SITUATION WHEN THERE ARE NO OPs
C----+------------------------------------------------------------------
          CALL GET_TCKT_FROM_VALREC(VALREC, TCKT)
C----+------------------------------------------------------------------
C V05| BUGFIX ON TCKT VALUE CALCULATION DUE TO REPORTED SITUATION
C    | SITUATION WHEN THERE ARE NO OPs
C----+------------------------------------------------------------------
          IF (P(OPGENFLG).EQ.1) THEN
            CALL UPDATE_OPSFIL (VALREC,BASAMT,NETOPAMT,KNETOPAMT,TCKT,ST)
            IF (ST .NE. 0) GOTO 2000
            TOTUPDOPS = TOTUPDOPS + 1
          ENDIF
C
C----|--!--------------------------------------------------------------
C V04   ! Adding new OP validation FLAG -end
C----|--!--------------------------------------------------------------

C
C----|--!--------------------------------------------------------------
C V03   ! Adding new agent field - start
C----|--!--------------------------------------------------------------
C          CALL PRINT_REPTXDPRZ_BODY (REP_LUN,
C     *                               TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,
C     *                               KTOTDIVAMT,KNETDIVAMT,KTOTDIVPRZ,
C     *                               TCKT,BASAMT,RFNAMT,
C     *                               AGTTAB(AGTNUM,VALREC(VSTER)),
C     *                               TAXBLAMT)
          CALL PRINT_REPTXDPRZ_BODY_TMP (REP_TMP_LUN,
     *                               TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,
     *                               KTOTDIVAMT,KNETDIVAMT,KTOTDIVPRZ,
     *                               TCKT,BASAMT,RFNAMT,
     *                               AGTTAB(AGTNUM,VALREC(VSTER)),
     *                               TAXBLAMT, TOT_NR_PRZ_IS)
C----|--!--------------------------------------------------------------
C V03   ! Adding new agent field - end
C----|--!--------------------------------------------------------------
          CALL PRINT_SAPTXDPRZ_BODY (SAP_LUN,DRDAT,CONCURSO,
     *                               ANO,TCKT,OPAMT+KOPAMT,
     *                               NETOPAMT+KNETOPAMT)
          TOTOPAMT = TOTOPAMT + OPAMT + KOPAMT
          TOTNETOPAMT = TOTNETOPAMT + NETOPAMT + KNETOPAMT
          TOTTAXBLAMT = TOTTAXBLAMT + TAXBLAMT
          TOTSAPREC = TOTSAPREC + 1
        ENDIF
C
        GOTO 300 ! READ NEXT RECORD FROM VLF
C
1000    CONTINUE
C
C=======================================================================
C       WRITE FOOTER INTO REPORT FILE AND SAP INTERFACE FILE
C=======================================================================
C
C        CALL PRINT_REPTXDPRZ_FOOTER (REP_LUN,TOTOPAMT,TOTNETOPAMT,
C     *                               TOTTAXBLAMT)
        TOTSAPREC = TOTSAPREC + 1
        CALL PRINT_SAPTXDPRZ_FOOTER (SAP_LUN,TOTSAPREC)
C
C=======================================================================
C       PRINT PROCESS STATISTICS ON SCREEN
C=======================================================================
C
        TYPE*, IAM(), '       '
        TYPE*, IAM(), 'Bilhetes premiados do ', CONSOREXT(GNUM), ' ', CCCAAAA,': ', TOTVALREC
        TYPE*, IAM(), ' Com imposto de selo:                   ', TOTUPDVLF
        TYPE*, IAM(), ' Sem imposto de selo:                   ', TOTVALREC-TOTUPDVLF
        TYPE*, IAM(), '       '
C
        WRITE(C16BUF,'(5A4)'), (SFNAMES(K,VLF),K=1,5)
        WRITE(5, '(1X,A,A,A,A,I)'), IAM(), 'Registos lidos do ', TRIM(C16BUF), ':         ', TOTREADVLF
        WRITE(C16BUF2,'(5A4)'), (SFNAMES(K,VLC),K=1,5)
        WRITE(5, '(1X,A,A,A,A,I)'), IAM(), 'Registos escritos no ', TRIM(C16BUF2), ':      ', TOTWRTVLC
        WRITE(5, '(1X,A,A,A,A,I)'), IAM(), 'Registos atualizados no ', TRIM(C16BUF), ':   ', TOTUPDVLF
C
C----|--!--------------------------------------------------------------
C V04   ! Adding new OP validation FLAG -start
C----|--!--------------------------------------------------------------
C
        IF (P(OPGENFLG).EQ.1) THEN
          WRITE(5, '(1X,A,A,I)'), IAM(), 'Registos atualizados no FILE:OPS.FIL:   ', TOTUPDOPS
          IF (TOTUPDVLF .NE. TOTUPDOPS) THEN
            TYPE*, IAM(), '       '
            TYPE*, IAM(), 'TAXMNG - Totais de registos actualizados no ', TRIM(C16BUF),' e'
            TYPE*, IAM(), '         FILE:OPS.FIL e diferente!'
            CALL GPAUSE
          ENDIF
        ENDIF

C----|--!--------------------------------------------------------------
C V04   ! Adding new OP validation FLAG -end
C----|--!--------------------------------------------------------------
        IF (TOTREADVLF .NE. TOTWRTVLC) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Totais de registos lidos do ', TRIM(C16BUF),' e'
          TYPE*, IAM(), '         escritos no ', TRIM(C16BUF2), ' e diferente!'
          CALL GPAUSE
        ENDIF
C
C=======================================================================
C       CLOSE FILES
C=======================================================================
C
C        CALL USRCLOS1(REP_LUN)
        CALL USRCLOS1(SAP_LUN)
        CALL USRCLOS1(REP_TMP_LUN)
        CLOSE(OPS_LUN)
        CALL ICLOSE(VLF,VLFBUF,ST)
        CALL ICLOSE(VLC,NEWBUF,ST)
        
C----|--!--------------------------------------------------------------
C V03   ! New ordering - start
C----|--!--------------------------------------------------------------
        WRITE(LIBCMD,'(A)') 
     *         '$ SORT ' 
     *      // '/KEY=(POSITION:1,SIZE:100,CHARACTER)'   ! First key
     *      // '/KEY=(POSITION:124,SIZE:10,CHARACTER) ' ! Second key
     *      // TRIM(REP_TMP_FILNAM(6:)) 
     *      // ' '
     *      // TRIM(REP_SRT_FILNAM(6:))
        CALL EXEC_SYS_CMD(LIBCMD,ST)
        
        
C----|--!--------------------------------------------------------------
C V03   ! Opening sorted file
C----|--!--------------------------------------------------------------
        CALL FIND_AVAILABLE_LUN (REP_SRT_LUN,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Erro a obter uma LUN para o ficheiro:'
          TYPE*, IAM(), '       '
          TYPE*, IAM(), '         '//TRIM(REP_SRT_FILNAM(6:))
          TYPE*, IAM(), '       '
          CALL GSTOP (GEXIT_FATAL)
        ENDIF
        CALL OPEN_FILE(REP_SRT_FILNAM, REP_SRT_LUN, ST)
        ST = 0
        DO WHILE (ST .EQ. 0)
            READ(UNIT = REP_SRT_LUN     !data record
     *          , IOSTAT = ST
     *          , FMT = '(256A)') ASCII_REC
            IF (ST .EQ. 0) THEN
                IF (ASCII_REC(124:133) .EQ. '       TOT') THEN
                    WRITE(REP_LUN, 103) ASCII_REC (134:214)
                ELSE
                    WRITE(REP_LUN,'(1X,A)'), ASCII_REC (85:214)
                ENDIF
            ENDIF
        ENDDO
        IF (ST .LT. 0) THEN ! EOF
            CALL USRCLOS1(REP_SRT_LUN)
        ENDIF
C=======================================================================
C       WRITE FOOTER INTO REPORT FILE 
C=======================================================================
C
        CALL PRINT_REPTXDPRZ_FOOTER (REP_LUN,TOTOPAMT,TOTNETOPAMT,
     *                               TOTTAXBLAMT, TOT_NR_PRZ_IS)
        CALL USRCLOS1(REP_LUN)

        CALL DFILX(REP_TMP_FILNAM,0,0,ST)
        IF (ST .NE. 0) CALL GSTOP (GEXIT_FATAL)
        CALL DFILX(REP_SRT_FILNAM,0,0,ST)
        IF (ST .NE. 0) CALL GSTOP (GEXIT_FATAL)
C----|--!--------------------------------------------------------------
C V03   ! New ordering - end
C----|--!--------------------------------------------------------------
        
C
C=======================================================================
C       LIST THE LATEST VERSION OF THE GENERATED FILES
C=======================================================================
C
        TYPE*, IAM(), '       '
        TYPE*, IAM(), 'Ficheiros gerados:'
        WRITE(LIBCMD,'(A,A,A,A,A)') '$ DIR ',TRIM(REPFILNAM(6:)),';0, ',TRIM(SAPFILNAM(6:)),';0 /DATE/SIZE=ALL'
        ST = LIB$SPAWN(TRIM(LIBCMD))
        IF(.NOT. ST) CALL LIB$SIGNAL(%VAL(ST))
        TYPE*, IAM(), '       '
C
        TYPE*, IAM(), 'TAXMNG - Fim do processamento'
        TYPE*, IAM(), '       '
C
C=======================================================================
C       ASK THE OPERATOR IF PROCEDURE ENDED WITHOUT ERRORS
C=======================================================================
C
        CALL PRMYESNO('Procedimento terminou sem erros [Y/N]? ', YESNO)
        IF (YESNO .NE. 1) GOTO 100
        TYPE*, IAM(), '       '
        TYPE*, IAM(), 'A renomear ficheiros...'
        CALL FMAINT(VLF,VLC,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(),'Ocorreu um problema a renomear os ficheiros VLF e VLC'
          CALL GPAUSE
        ENDIF
C
        GOTO 100
C
2000    CONTINUE
C
C=======================================================================
C       CLOSE FILES
C=======================================================================
C
        CALL USRCLOS1(SAP_LUN)
        CALL USRCLOS1(REP_LUN)
        CALL USRCLOS1(REP_TMP_LUN)
        CLOSE(OPS_LUN)
        CALL ICLOSE(VLF,VLFBUF,ST)
        CALL ICLOSE(VLC,NEWBUF,ST)
C
C=======================================================================
C       DELETE FILES WITH ERRORS
C=======================================================================
C
        CALL DFILX(REPFILNAM,0,0,ST)
        IF (ST .NE. 0) CALL GSTOP (GEXIT_FATAL)
C
        CALL DFILX(SAPFILNAM,0,0,ST)
        IF (ST .NE. 0) CALL GSTOP (GEXIT_FATAL)
C
        CALL DFILX(REP_TMP_FILNAM,0,0,ST)
        IF (ST .NE. 0) CALL GSTOP (GEXIT_FATAL)
C
        TYPE *,IAM(), '     '
        TYPE *,IAM(), 'TAXMNG - Procedimento terminou com erro'
C
        GOTO 100 ! GOTO MAIN MENU OF THIS PROGRAM
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
102     FORMAT(A7)
C
103     FORMAT(67X, 64('-'), /, 5X, 'TOTAL', 40X, A,/) ! Linha de Total 
C103     FORMAT(71X, 60('-'), /, 5X, 'TOTAL', 40X, A,/) ! Linha de Total 
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
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
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'
C
        INTEGER*4 ST, REP_LUN
        CHARACTER*(*) REPFILNAM
C
        OPEN (UNIT       = REP_LUN,
     *        FILE       = REPFILNAM,
     *        IOSTAT     = ST,
     *        FORM       = 'FORMATTED',
     *        RECL       = 256,
     *        STATUS     = 'NEW',
     *        RECORDTYPE = 'STREAM_CR')
C
        RETURN
C
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE CHECKAMTAX(VALREC,BASAMT,RFNAMT,TAXPER,GAMSHV,KSPTSHV,DRWN,
C                           TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,OPAMT,NETOPAMT,
C                           KTOTDIVAMT,KNETDIVAMT,KTOTDIVPRZ,KOPAMT,KNETOPAMT,
C                           HASTAX,ST)
C
C       THIS SUBROUTINE CALCULATES THE NET AMOUNT VALUE FOR EACH PRIZE DIVISION.
C       FOR EACH PRIZE DIVISION VALUE GREATER THAN <BASAMT> THE VALUE IS TAXED,
C       ACCORDINGLY TO THE PERCENTAGE VALUE <TAXPER>.
C       THE TAX AMOUNT IS TRUNCATED (NOT ROUNDED).
C
C       INPUTS:
C        VALREC           VALIDATION RECORD TO PROCESS
C        BASAMT           MINIMUM AMOUNT TO WHICH THE TAX APPLIES
C        RFNAMT           TAX REFUND AMOUNT
C        TAXPER           TAX AMOUNT
C        GAMSHR           GAME SHARE VALUES
C        KSPTSHV          GAME SHARE VALUES OF JOKER (TOTOBOLA NORMAL)
C        DRWN             DRAW NUMBER
C
C       OUTPUTS:
C        TOTDIVAMT        TOTAL AMOUNT BY DIVISION FOR MAIN GAME (INTEGER*4 * MAXDIV)
C        NETDIVAMT        TOTAL NET AMOUNT BY DIVISION FOR MAIN GAME (INTEGER*4 * MAXDIV)
C        TOTDIVPRZ        TOTAL PRIZES BY DIVISION FOR MAIN GAME (INTEGER*4 * MAXDIV)
C        OPAMT            PAYMENT ORDER AMOUNT FOR MAIN GAME
C        NETOPAMT         PAYMENT ORDER NET AMOUNT FOR MAIN GAME
C
C        KTOTDIVAMT       TOTAL AMOUNT BY DIVISION FOR JOKER OF TOTOBOLA NORMAL (INTEGER*4 * MAXDIV)
C        KNETDIVAMT       TOTAL NET AMOUNT BY DIVISION FOR JOKER OF TOTOBOLA NORMAL (INTEGER*4 * MAXDIV)
C        KTOTDIVPRZ       TOTAL PRIZES BY DIVISION FOR JOKER OF TOTOBOLA NORMAL (INTEGER*4 * MAXDIV)
C        KOPAMT           PAYMENT ORDER AMOUNT FOR FOR JOKER OF TOTOBOLA NORMAL
C        KNETOPAMT        PAYMENT ORDER NET AMOUNT FOR JOKER OF TOTOBOLA NORMAL
C
C        HASTAX           TRUE IF THE TOTAL PRIZE AMOUNT HAS BEEN TAXED,
C                         FALSE OTHERWISE
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CHECKAMTAX (VALREC,BASAMT,RFNAMT,TAXPER,GAMSHV,KSPTSHV,DRWN,
     *                       TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,OPAMT,NETOPAMT,
     *                       KTOTDIVAMT,KNETDIVAMT,KTOTDIVPRZ,KOPAMT,KNETOPAMT,
     *                       HASTAX)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
C
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'
        INCLUDE 'INCLIB:WINUPD.DEF'
C
C=======================================================================
C       VARIABLES
C=======================================================================
C
        INTEGER*4 I, ST
        INTEGER*4 BASAMT, RFNAMT, TAXPER
        INTEGER*4 SHRAMT, TAXAMT
        INTEGER*4 MAXDIV
        PARAMETER(MAXDIV = 40)
        INTEGER*4 GAMSHV(MAXDIV), KSPTSHV(MAXDIV)
        INTEGER*4 DRWN
C
C       DATA STRUCTURES FOR:
C          TOTOLOTO QUARTA, TOTOLOTO SABADO, TOTOBOLA NORMAL,
C          TOTOBOLA EXTRA 1 AND JOKER
        INTEGER*4 TOTDIVAMT(MAXDIV) ! IT MUST BE NOT LESS THAN THE MAXIMUM NUMBER OF DIVISIONS
        INTEGER*4 NETDIVAMT(MAXDIV) !
        INTEGER*4 TOTDIVPRZ(MAXDIV) !
        INTEGER*4 OPAMT, NETOPAMT
C
C       DATA STRUCTURES FOR:
C          JOKER OF TOTOBOLA NORMAL
        INTEGER*4 KTOTDIVAMT(MAXDIV)
        INTEGER*4 KNETDIVAMT(MAXDIV)
        INTEGER*4 KTOTDIVPRZ(MAXDIV)
        INTEGER*4 KOPAMT, KNETOPAMT
C
        INTEGER*4 GTYP, GIND, KGAM, GAM, FRCS
        INTEGER*4 DDRW, DDIV, DSHR, DBNS, DKIK, BIND, KIND

        INTEGER*4 KIKNUM
        LOGICAL   HASTAX
C
        CALL FASTSET(0,TOTDIVAMT,MAXDIV)
        CALL FASTSET(0,NETDIVAMT,MAXDIV)
        CALL FASTSET(0,TOTDIVPRZ,MAXDIV)
        CALL FASTSET(0,KTOTDIVAMT,MAXDIV)
        CALL FASTSET(0,KNETDIVAMT,MAXDIV)
        CALL FASTSET(0,KTOTDIVPRZ,MAXDIV)
C
        HASTAX = .FALSE.
        TAXAMT = 0
        OPAMT = 0
        NETOPAMT = 0
        KOPAMT = 0
        KNETOPAMT = 0
C
        GTYP = VALREC(VGTYP)
        GIND = VALREC(VGIND)
        KGAM = VALREC(VKGME)
        GAM  = VALREC(VGAM)
        FRCS = VALREC(VFRAC)
        IF(KGAM .NE. 0) KIND = SCFGNT(GAMIDX,KGAM) ! KICKER INDEX
C
        CALL DLOGVAL(VALREC,VDETAIL)
        DO 100 I = 1,VALREC(VPZOFF)
          DDRW = VDETAIL(VDRW,I) ! DRAW
          DDIV = VDETAIL(VDIV,I) ! DIVISION
          DSHR = VDETAIL(VSHR,I) ! NUMBER OF SHARES IN DIVISION
          DKIK = VDETAIL(VKIK,I) ! KICK FLAG
          IF (DDRW .LE. 0) GOTO 100
C
          SHRAMT = 0
          TAXAMT = 0
C
C=======================================================================
C       TOTOLOTO QUARTA E TOTOLOTO SABADO
C=======================================================================
C
        IF (GTYP .EQ. TLTO) THEN
          BIND = 1
          DBNS = VDETAIL(VBDR,I)
          IF (DBNS .NE. 0) BIND = 2
          IF (DDRW .NE. DRWN) GOTO 100
          IF (FRCS .EQ. 0 .OR. FRCS .EQ. SCFFRC(GAM)) THEN
            TAXAMT = 0
            SHRAMT = GAMSHV(DDIV)
            IF (SHRAMT .GT. BASAMT) THEN
              HASTAX = .TRUE.
              ! TAX AMOUNT (ADD 0.50 TO GET THE ROUNDED VALUE; REMOVE IT IF YOU WANT THE TRUNCATED VALUE)
              ! TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10) + 0.50D0))
              TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10)))
            ENDIF
            ! TOTAL AMOUNT
            TOTDIVAMT(DDIV) = TOTDIVAMT(DDIV) + SHRAMT * DSHR
            ! NET AMOUNT
            NETDIVAMT(DDIV) = NETDIVAMT(DDIV) + SHRAMT * DSHR - TAXAMT * DSHR
            ! TOTAL PRIZES
            TOTDIVPRZ(DDIV) = TOTDIVPRZ(DDIV) + DSHR
            ! OP AMOUNT AND OP NET AMOUNT
            NETOPAMT = NETOPAMT + SHRAMT * DSHR - TAXAMT * DSHR
            OPAMT = OPAMT + SHRAMT * DSHR
          ELSE
            TAXAMT = 0
            SHRAMT = (GAMSHV(DDIV)/SCFFRC(GAM)) * FRCS
            IF (SHRAMT .GT. BASAMT) THEN
              HASTAX = .TRUE.
              ! TAX AMOUNT (ADD 0.50 TO GET THE ROUNDED VALUE; REMOVE IT IF YOU WANT THE TRUNCATED VALUE)
              ! TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10) + 0.50D0))
              TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10)))
            ENDIF
            ! TOTAL AMOUNT
            TOTDIVAMT(DDIV) = TOTDIVAMT(DDIV) + SHRAMT * DSHR
            ! NET AMOUNT
            NETDIVAMT(DDIV) = NETDIVAMT(DDIV) + SHRAMT * DSHR - TAXAMT * DSHR
            ! TOTAL PRIZES
            TOTDIVPRZ(DDIV) = TOTDIVPRZ(DDIV) + DSHR
            ! OP AMOUNT AND OP NET AMOUNT
            NETOPAMT = NETOPAMT + SHRAMT * DSHR - TAXAMT * DSHR
            OPAMT = OPAMT + SHRAMT * DSHR
          ENDIF
          GOTO 100
!        ENDIF
C
C=======================================================================
C       JOKER (EUROMILHOES, TOTOLOTO QUARTA AND TOTOLOTO SABADO)
C=======================================================================
C
        ELSEIF (GTYP .EQ. TKIK) THEN
          IF (DDRW .NE. DRWN) GOTO 100
          IF (FRCS .EQ. 0 .OR. FRCS .EQ. SCFFRC(KGAM)) THEN
            TAXAMT = 0
            SHRAMT = GAMSHV(DDIV)
            IF (SHRAMT .GT. BASAMT) THEN
              HASTAX = .TRUE.
              ! TAX AMOUNT (ADD 0.50 TO GET THE ROUNDED VALUE; REMOVE IT IF YOU WANT THE TRUNCATED VALUE)
              ! TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10) + 0.50D0))
              TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10)))
            ENDIF
            ! TOTAL AMOUNT
            KTOTDIVAMT(DDIV) = KTOTDIVAMT(DDIV) + SHRAMT * DSHR
            ! NET AMOUNT
            KNETDIVAMT(DDIV) = KNETDIVAMT(DDIV) + SHRAMT * DSHR - TAXAMT * DSHR
            ! TOTAL PRIZES
            KTOTDIVPRZ(DDIV) = KTOTDIVPRZ(DDIV) + DSHR
            ! OP AMOUNT AND OP NET AMOUNT
            KNETOPAMT = KNETOPAMT + SHRAMT * DSHR - TAXAMT * DSHR
            KOPAMT = KOPAMT + SHRAMT * DSHR
          ELSE
            TAXAMT = 0
            SHRAMT = (GAMSHV(DDIV)/SCFFRC(GAM)) * FRCS
            IF (SHRAMT .GT. BASAMT) THEN
              HASTAX = .TRUE.
              ! TAX AMOUNT (ADD 0.50 TO GET THE ROUNDED VALUE; REMOVE IT IF YOU WANT THE TRUNCATED VALUE)
              ! TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10) + 0.50D0))
              TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10)))
            ENDIF
            ! TOTAL AMOUNT
            KTOTDIVAMT(DDIV) = KTOTDIVAMT(DDIV) + SHRAMT * DSHR
            ! NET AMOUNT
            KNETDIVAMT(DDIV) = KNETDIVAMT(DDIV) + SHRAMT * DSHR - TAXAMT * DSHR
            ! TOTAL PRIZES
            KTOTDIVPRZ(DDIV) = KTOTDIVPRZ(DDIV) + DSHR
            ! OP AMOUNT AND OP NET AMOUNT
            KNETOPAMT = KNETOPAMT + SHRAMT * DSHR - TAXAMT * DSHR
            KOPAMT = KOPAMT + SHRAMT * DSHR
          ENDIF
          GOTO 100
!        ENDIF
C
C=======================================================================
C       TOTOBOLA NORMAL (WITH JOKER), TOTOBOLA EXTRA 1
C=======================================================================
C
        ELSEIF (GTYP .EQ. TSPT) THEN
            IF (GAM .EQ. 1 .AND. DKIK .EQ. 1) THEN ! JOKER OF TOTOBOBOLA NORMAL
              IF (DDRW .NE. DRWN) GOTO 100
              IF (FRCS .EQ. 0 .OR. FRCS .EQ. SCFFRC(KGAM)) THEN
                TAXAMT = 0
                SHRAMT = KSPTSHV(DDIV)
                IF (SHRAMT .GT. BASAMT) THEN
                  HASTAX = .TRUE.
                  ! TAX AMOUNT (ADD 0.50 TO GET THE ROUNDED VALUE; REMOVE IT IF YOU WANT THE TRUNCATED VALUE)
                  ! TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10) + 0.50D0))
                  TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10)))
                ENDIF
                ! TOTAL JOKER AMOUNT
                KTOTDIVAMT(DDIV) = KTOTDIVAMT(DDIV) + SHRAMT * DSHR
                ! JOKER NET AMOUNT
                KNETDIVAMT(DDIV) = KNETDIVAMT(DDIV) + SHRAMT * DSHR - TAXAMT * DSHR
                ! TOTAL JOKER PRIZES
                KTOTDIVPRZ(DDIV) = KTOTDIVPRZ(DDIV) + DSHR
                ! OP AMOUNT AND OP NET AMOUNT
                KNETOPAMT = KNETOPAMT + SHRAMT * DSHR - TAXAMT * DSHR
                KOPAMT = KOPAMT + SHRAMT * DSHR
              ELSE
                TAXAMT = 0
                SHRAMT = (GAMSHV(DDIV)/SCFFRC(GAM)) * FRCS
                IF (SHRAMT .GT. BASAMT) THEN
                  HASTAX = .TRUE.
                  ! TAX AMOUNT (ADD 0.50 TO GET THE ROUNDED VALUE; REMOVE IT IF YOU WANT THE TRUNCATED VALUE)
                  ! TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10) + 0.50D0))
                  TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10)))
                ENDIF
                ! TOTAL JOKER AMOUNT
                KTOTDIVAMT(DDIV) = KTOTDIVAMT(DDIV) + SHRAMT * DSHR
                ! JOKER NET AMOUNT
                KNETDIVAMT(DDIV) = KNETDIVAMT(DDIV) + SHRAMT * DSHR - TAXAMT * DSHR
                ! TOTAL JOKER PRIZES
                KTOTDIVPRZ(DDIV) = KTOTDIVPRZ(DDIV) + DSHR
                ! OP JOKER AMOUNT AND OP JOKER NET AMOUNT
                KNETOPAMT = KNETOPAMT + SHRAMT * DSHR - TAXAMT * DSHR
                KOPAMT = KOPAMT + SHRAMT * DSHR
              ENDIF
              GOTO 100
            ENDIF
            ! TOTOBOLA NORMAL AND TOTOBOLA EXTRA 1
            IF (DDRW .NE. DRWN) GOTO 100
            IF (FRCS .EQ. 0 .OR. FRCS .EQ. SCFFRC(GAM)) THEN
              TAXAMT = 0
              SHRAMT = GAMSHV(DDIV)
              IF (SHRAMT .GT. BASAMT) THEN
                HASTAX = .TRUE.
                ! TAX AMOUNT (ADD 0.50 TO GET THE ROUNDED VALUE; REMOVE IT IF YOU WANT THE TRUNCATED VALUE)
                ! TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10) + 0.50D0))
                TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10)))
              ENDIF
              ! TOTAL AMOUNT
              TOTDIVAMT(DDIV) = TOTDIVAMT(DDIV) + SHRAMT * DSHR
              ! NET AMOUNT
              NETDIVAMT(DDIV) = NETDIVAMT(DDIV) + SHRAMT * DSHR - TAXAMT * DSHR
              ! TOTAL PRIZES
              TOTDIVPRZ(DDIV) = TOTDIVPRZ(DDIV) + DSHR
              ! OP AMOUNT AND OP NET AMOUNT
              NETOPAMT = NETOPAMT + SHRAMT * DSHR - TAXAMT * DSHR
              OPAMT = OPAMT + SHRAMT * DSHR
            ELSE
              TAXAMT = 0
              SHRAMT = (GAMSHV(DDIV)/SCFFRC(GAM)) * FRCS
              IF (SHRAMT .GT. BASAMT) THEN
                HASTAX = .TRUE.
                ! TAX AMOUNT (ADD 0.50 TO GET THE ROUNDED VALUE; REMOVE IT IF YOU WANT THE TRUNCATED VALUE)
                ! TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10) + 0.50D0))
                TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10)))
              ENDIF
              ! TOTAL AMOUNT
              TOTDIVAMT(DDIV) = TOTDIVAMT(DDIV) + SHRAMT * DSHR
              ! NET AMOUNT
              NETDIVAMT(DDIV) = NETDIVAMT(DDIV) + SHRAMT * DSHR - TAXAMT * DSHR
              ! TOTAL PRIZES
              TOTDIVPRZ(DDIV) = TOTDIVPRZ(DDIV) + DSHR
              ! OP AMOUNT AND OP NET AMOUNT
              NETOPAMT = NETOPAMT + SHRAMT * DSHR - TAXAMT * DSHR
              OPAMT = OPAMT + SHRAMT * DSHR
            ENDIF
            GOTO 100
        ENDIF
100     CONTINUE
C
        RETURN
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE UPDATE_VALREC(VALREC,BASAMT,NETOPAMT,NETKOPAMT,V4BUF)
C
C       THIS SUBROUTINE UPDATES THE FOLLOWING FIELDS OF VAL RECORD TO
C       ITS NET AMOUNTS:
C
C        VALREC(VPAMT)    AMOUNT IN OPS FOR THE GAME
C        VALREC(VKPAMT)   AMOUNT IN OPS FOR KICKER
C
C       INPUTS:
C        VALREC           VALIDATION RECORD TO UPDATE
C        BASAMT           BASE AMOUNT
C        NETOPAMT         PAYMENT ORDER NET AMOUNT OF MAIN GAME
C        NETKOPAMT        PAYMENT ORDER NET AMOUNT OF KICKER GAME
C
C       OUTPUTS:
C        V4BUF
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE UPDATE_VALREC (VALREC,BASAMT,NETOPAMT,NETKOPAMT,V4BUF)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
        INCLUDE 'INCLIB:NAMCMD.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:PRMHSH.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'
        INCLUDE 'INCLIB:OPS_REC.DEF'
C
        INCLUDE 'INCLIB:RECSCF.DEF'
C
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
        INCLUDE 'INCLIB:DBNREC.DEF'
C
        INCLUDE 'INCLIB:WINCOM.DEF'
C
        INCLUDE 'INCLIB:VALFIL.DEF'
C
C=======================================================================
C       LOCAL VARIABLES
C=======================================================================
C
        INTEGER*4  BASAMT, NETOPAMT, NETKOPAMT
C
C       UPDATE VALREC(VOPSAMT) AMOUNT TO NET AMOUNT
        IF (VALREC(VPAMT) .GT. BASAMT) THEN
          VALREC(VOPSAMT) = NETOPAMT
        ENDIF
C       UPDATE VALREC(VKOPSAMT) AMOUNT TO NET AMOUNT
        IF (VALREC(VKPAMT) .GT. BASAMT) THEN
          VALREC(VKOPSAMT) = NETKOPAMT
        ENDIF
C
C=======================================================================
C       REWRITE UPDATED VALREC
C=======================================================================
C
        CALL VALLOG(VALREC,V4BUF)
C
        RETURN
        END
C

C----+------------------------------------------------------------------
C V05| BUGFIX ON TCKT VALUE CALCULATION DUE TO REPORTED SITUATION
C    | SITUATION WHEN THERE ARE NO OPs
C----+------------------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE GET_TCKT_FROM_VALREC(VALREC,TCKT)
C
C       THIS SUBROUTINE GETS THE TICKET NUMBER FROM A VALREC
C
C       INPUTS:
C        VALREC           CONTAINS THE FIELDS TO BE UPDATED
C
C       OUTPUTS:
C        TCKT             TICKET SERIAL NUMBER
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE GET_TCKT_FROM_VALREC(VALREC,TCKT)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
C
C=======================================================================
C       LOCAL VARIABLES
C=======================================================================
C
        INTEGER*4  ST
        CHARACTER*(*) TCKT
        INTEGER*2  DAT(12)
        INTEGER*4  SSER
        INTEGER*4  SCHK

        DAT(VCDC) = VALREC(VSCDC)
        CALL CDATE(DAT)
        CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),SSER,SCHK)
        WRITE (TCKT, '(I3.3,I8.8,I3.3)')  DAT(VJUL), SSER, SCHK
        RETURN
        END
C----+------------------------------------------------------------------
C V05| BUGFIX ON TCKT VALUE CALCULATION DUE TO REPORTED SITUATION
C    | SITUATION WHEN THERE ARE NO OPs
C----+------------------------------------------------------------------

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE UPDATE_OPSFIL(VALREC,BASAMT,NETOPAMT,NETKOPAMT,TCKT,ST)
C
C       THIS SUBROUTINE UPDATES THE VALUES OF OPS_REC.TOTAL_GAME AND
C       OPS_REC.TOTAL_JOKER OF A OP RECORD FOR THE NET AMOUNT VALUES,
C       BASED ON THE VALUES OF VALREC(VPAMT) AND VALREC(VKPAMT)
C
C        OPS_REC.TOTAL_GAME    VALUE IN PRIZE JUST FOR THE MAIN GAME
C        OPS_REC.TOTAL_JOKER   VALUE IN PRIZE JUST FOR JOKER
C
C       INPUTS:
C        VALREC           CONTAINS THE FIELDS TO BE UPDATED
C        BASAMT           MINIMUM AMOUNT TO WHICH THE TAX APPLIES
C        NETOPAMT         PAYMENT ORDER NET AMOUNT OF MAIN GAME
C        NETKOPAMT        PAYMENT ORDER NET AMOUNT OF KICKER GAME
C        TCKT             TICKET SERIAL NUMBER
C
C       OUTPUTS:
C        ST               STATUS, 0=SUCCESS, ELSE FAILURE
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE UPDATE_OPSFIL (VALREC,BASAMT,NETOPAMT,NETKOPAMT,TCKT,ST)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMHSH.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:OPS_REC.DEF'
C
C=======================================================================
C       LOCAL VARIABLES
C=======================================================================
C
        INTEGER*4  ST
        INTEGER*4  BASAMT, NETOPAMT, NETKOPAMT
        CHARACTER*(*) TCKT
        INTEGER*2  DAT(12)
        INTEGER*4  SSER
        INTEGER*4  SCHK
        INTEGER*4  MSG_LUN /6/
C
C=======================================================================
C       OPEN OPS.FIL TO GET OP RECORD
C=======================================================================
C
        CALL OPEN_OPS('KEYED',ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), 'TAXMNG - Erro ao abrir ficheiro OPS'
          STOP
        ENDIF
C
C=======================================================================
C       GET OP RECORD
C=======================================================================
C
        DAT(VCDC) = VALREC(VSCDC)
        CALL CDATE(DAT)
        CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),SSER,SCHK)
        WRITE (TCKT, '(I3.3,I8.8,I3.3)')  DAT(VJUL), SSER, SCHK
        READ(OPS_LUN, KEYID=1, KEY=TCKT, IOSTAT=ST) OPS_REC
C
        IF (ST .NE. 0) THEN
          CALL DISPERR (MSG_LUN, 'Error Reading OPS FILE', 0, 'STATUS = ', ST, ' ', 0)
          TYPE*, IAM(), 'O registo da OP nao foi atualizado'
          CALL GSTOP (GEXIT_FATAL)
        ENDIF
C       UPDATE THE OP TOTAL_GAME AMOUNT TO NET AMOUNT
        IF (VALREC(VPAMT) .GT. BASAMT) THEN
          OPS_REC.TOTAL_GAME = NETOPAMT
        ENDIF
C       UPDATE THE OP TOTAL_JOKER AMOUNT TO NET AMOUNT
        IF (VALREC(VKPAMT) .GT. BASAMT) THEN
          OPS_REC.TOTAL_JOKER = NETKOPAMT
        ENDIF
C
C=======================================================================
C       REWRITE UPDATED OP BACK TO FILE
C=======================================================================
C
        REWRITE(UNIT=OPS_LUN, IOSTAT=ST) OPS_REC
        IF (ST .NE. 0) THEN
           TYPE*, IAM(), "WRITE STATUS: ", ST
           TYPE*, IAM(),'>>> Erro de escrita no arquivo de OPS'
           CALL GPAUSE
        ENDIF
C
        RETURN
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
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
          IF (GNUM .NE. 2 .AND. GNUM .NE. 3 .AND. GNUM .NE. 4) THEN
            ! DON'T PRINT TOTOLOTO, TOTOBOLA EXTRA 2 AND LOT2
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
          ELSEIF(ST.EQ.0) THEN  !NO ERRORS IN INPNUM
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
        ENDIF !ENDIF(GCNT.EQ.0)
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
905     FORMAT(18X,' J O G O S    D I S P O N I V E I S',/)
910     FORMAT(18X,I2,' - ',A16)
911     FORMAT(18X,' E - Exit')
C
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_REPTXDPRZ_HEADER(LUN,GNUM,DRWNBR,DRWYEAR,
C                                         CDAT,FINREP)
C
C       THIS SUBROUTINE PRINTS THE TITLE REPORT
C
C       INPUTS:
C        LUN         LOGICAL UNIT
C        GNUM        GAME NUMBER
C        DRWNBR      DRAW NUMBER
C        DRWYEAR     DRAW YEAR
C        CDAT        DATE
C        FINREP      FLAG (FOR PASSIVE GAMES, INDICATING WHETHER THE
C                    REPORT IS FINAL OR PROVISIONAL)
C
C       OUTPUTS:
C       *NONE*
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINT_REPTXDPRZ_HEADER (LUN,GNUM,DRWNBR,DRWYEAR,
     *                                     CDAT,FINREP)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER*4 LUN, K, TLEN
        INTEGER*4 GNUM
        INTEGER*4 DRWNBR, DRWYEAR, DRWSZ
        INTEGER*4 CDAT(8)
        CHARACTER REPTITLE*53
        CHARACTER CONSOREXT*8
        LOGICAL   FINREP
C
        IF (GNUM .EQ. 1 .OR. GNUM .EQ. 5 .OR. GNUM .EQ. 10) THEN
          REPTITLE = ' RELATORIO DEFINITIVO DE PREMIOS COM IMPOSTO DE SELO '
          CONSOREXT = 'CONCURSO'
          IF (GNUM .EQ. 1 .OR. GNUM .EQ. 10) TLEN = 51
          IF (GNUM .EQ. 5) TLEN = 56
          DRWSZ = 2 ! CC
        ELSEIF (GNUM .EQ. 6 .OR. GNUM .EQ. 7) THEN
          REPTITLE = ' RELATORIO DEFINITIVO DE PREMIOS COM IMPOSTO DE SELO '
          CONSOREXT = ' SORTEIO'
          TLEN = 51
          DRWSZ = 3 ! SSS
        ELSE
          CONSOREXT = ' SORTEIO'
          TLEN = 51
          DRWSZ = 3 ! CC
        ENDIF
C----|--!--------------------------------------------------------------
C V03   ! Adding new agent field - start
C----|--!--------------------------------------------------------------
C
C ==================================================================================================================================
C SCML - Departamento de Jogos            RELATORIO DEFINITIVO DE PREMIOS COM IMPOSTO DE SELO                       Data: 20.12.2012
C                                               SORTEIO 093/2012 TOTOLOTO QUARTA
C                                               SORTEIO 094/2012 TOTOLOTO SABADO
C                                               CONCURSO 48/2012 JOKER
C                                               CONCURSO 48/2012 TOTOBOLA NORMAL
C                                               CONCURSO 48/2012 TOTOBOLA EXTRA 1
C ==================================================================================================================================
C                                            CLASSE     VALOR BRUTO     VALOR BRUTO   VALOR SUJEITO        VALOR DO   VALOR LIQUIDO
C CODIGO DA APOSTA     MEDIADOR      QTD.    PREMIO       DA CLASSE       DO PREMIO       A IMPOSTO         IMPOSTO       DO PREMIO    
C ----------------------------------------------------------------------------------------------------------------------------------
C
        WRITE (LUN, 100) REPTITLE,
     *                   CDAT(3), CDAT(2), CDAT(1),
     *                   CONSOREXT,
     *                   DRWNBR, DRWYEAR,
     *                   (GLNAMES(K,GNUM),K=1,4)
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
100     FORMAT(1X, 130('='), /,
     *         1X, 'SCML - Departamento de Jogos', T41, A53, T116, 'Data: ', I2.2, '.', I2.2, '.', I4.4,/,
     *         1X, '                            ', T<TLEN>, A, 1X, I<DRWSZ>.<DRWSZ>, '/', I4.4, 1X, 4A4,/,
     *         1X, 130('='), /,
     *         1X, '                                           CLASSE',
     *             '     VALOR BRUTO     VALOR BRUTO   VALOR SUJEITO',
     *             '        VALOR DO   VALOR LIQUIDO',/,
     *         1X, 'CODIGO DA APOSTA     MEDIADOR      QTD.    PREMIO',
     *             '       DA CLASSE       DO PREMIO       A IMPOSTO',
     *             '         IMPOSTO       DO PREMIO',/,
     *         1X, 130('-')/)
C
C----|--!--------------------------------------------------------------
C V03   ! Adding new agent field - end
C----|--!--------------------------------------------------------------
        RETURN
C
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_SAPTXDPRZ_HEADER (LUN,GNUM,DRDAT,DRWNBR,DRWYEAR)
C
C       THIS SUBROUTINE PRINTS THE HEADER OF SAP FILE
C
C       INPUTS:
C        LUN         LOGICAL UNIT
C        GNUM        GAME NUMBER
C        DRDAT       DRAW DATE
C        DRWNBR      DRAW NUMBER
C        DRWYEAR     DRAW YEAR
C
C       OUTPUTS:
C       *NONE*
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINT_SAPTXDPRZ_HEADER (LUN,GNUM,DRDAT,DRWNBR,DRWYEAR)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4 LUN, K
        INTEGER*4 GNUM, DRWNBR, DRWYEAR
        INTEGER*2 DRDAT(LDATE_LEN) ! DRAWING DATE
        CHARACTER*10  C10DRWDT
        CHARACTER     CDRWDT10(10)
        EQUIVALENCE   (C10DRWDT,CDRWDT10)
C
        WRITE(C10DRWDT,'(5A2)') (DRDAT(K),K=9,13)
        WRITE(LUN,10) 'IS1',
     *                CDRWDT10(7:10),CDRWDT10(4:5),CDRWDT10(1:2),
     *                GNUM,DRWNBR,DRWYEAR
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
10      FORMAT(A3,4A1,2A1,2A1,I2.2,I3.3,I4.4,51('0'))
C
        RETURN
C
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_SAPTXDPRZ_BODY (LUN,DRDAT,DRWNBR,DRWYEAR,
C                                        TCKT,TOTAMT,NETAMT)
C
C       THIS SUBROUTINE PRINTS THE BODY RECORDS OF SAP FILE
C
C       INPUTS:
C        LUN         LOGICAL UNIT
C        DRDAT       DRAW DATE
C        DRWNBR      DRAW NUMBER
C        DRWYEAR     DRAW YEAR
C        TCKT        TICKET
C        TOTAMT      TOTAL AMOUNT
C        NETAMT      NET AMOUNT
C
C       OUTPUTS:
C       *NONE*
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINT_SAPTXDPRZ_BODY (LUN,DRDAT,DRWNBR,DRWYEAR,
     *                                   TCKT,TOTAMT,NETAMT)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4 LUN, K
        INTEGER*4 GNUM, DRWNBR, DRWYEAR, TOTAMT, NETAMT
        CHARACTER*(*) TCKT
        INTEGER*2 DRDAT(LDATE_LEN) ! DRAWING DATE
        CHARACTER*10  C10DRWDT
        CHARACTER     CDRWDT10(10)
        EQUIVALENCE   (C10DRWDT,CDRWDT10)
C
        WRITE(C10DRWDT,'(5A2)') (DRDAT(K),K=9,13)
        WRITE(LUN,10) 'IS2',
     *                CDRWDT10(7:10),CDRWDT10(4:5),CDRWDT10(1:2),
     *                DRWNBR,DRWYEAR,TCKT,TOTAMT,TOTAMT-NETAMT,NETAMT
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
10      FORMAT(A3,4A1,2A1,2A1,I3.3,I4.4,A<LEN(TCKT)>,I13.13,I13.13,I13.13)
C
        RETURN
C
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_SAPTXDPRZ_FOOTER (LUN,TOTREC)
C
C       THIS SUBROUTINE PRINTS THE FOOTER RECORD OF SAP FILE
C
C       INPUTS:
C        LUN         LOGICAL UNIT
C        TOTREC      TOTAL NUMBER OF RECORDS (INCLUDING THE HEADER AND THE FOOTER)
C
C       OUTPUTS:
C       *NONE*
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINT_SAPTXDPRZ_FOOTER (LUN,TOTREC)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4 LUN
        INTEGER*4 TOTREC
C
        WRITE(LUN,10) 'IS9', TOTREC
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
10      FORMAT(A3,I6.6,62('0'))
C
        RETURN
C
        END
C
C----|--!--------------------------------------------------------------
C V03   ! Adding new agent field - start
C----|--!--------------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_REPTXDPRZ_BODY(LUN,
C                                       TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,
C                                       KTOTDIVAMT,KNETDIVAMT,KTOTDIVPRZ,
C                                       TCKT,BASAMT,RFNAMT,
C                                       AGENT,
C                                       TXBLPRZAMT)
C
C       THIS SUBROUTINE PRINTS NET PRIZES INTO REPORT FILE FOR MUTUAL GAMES
C
C       INPUTS:
C        LUN          LOGICAL UNIT
C        TOTDIVAMT    TOTAL AMOUNT BY DIVISION FOR MAIN GAME (INTEGER*4 * MAXDIV)
C        NETDIVAMT    TOTAL NET AMOUNT BY DIVISION FOR MAIN GAME (INTEGER*4 * MAXDIV)
C        TOTDIVPRZ    TOTAL PRIZES BY DIVISION FOR MAIN GAME (INTEGER*4 * MAXDIV)
C        KTOTDIVAMT   TOTAL AMOUNT BY DIVISION FOR JOKER OF TOTOBOLA NORMAL (INTEGER*4 * MAXDIV)
C        KNETDIVAMT   TOTAL NET AMOUNT BY DIVISION FOR JOKER OF TOTOBOLA NORMAL (INTEGER*4 * MAXDIV)
C        KTOTDIVPRZ   TOTAL PRIZES BY DIVISION FOR JOKER OF TOTOBOLA NORMAL (INTEGER*4 * MAXDIV)
C        TCKT         TICKET NUMBER
C        BASAMT       MINIMUM AMOUNT TO WHICH THE TAX APPLIES
C        RFNAMT       REFUND AMOUNT
C        AGENT        AGENT REFERENCE
C
C       OUTPUTS:
C        TXBLPRZAMT   TAXABLE PRIZE AMOUNT
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINT_REPTXDPRZ_BODY (LUN,
     *                                   TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,
     *                                   KTOTDIVAMT,KNETDIVAMT,KTOTDIVPRZ,
     *                                   TCKT,BASAMT,RFNAMT, AGENT,
     *                                   TXBLPRZAMT)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER*4 TXBLPRZAMT
        INTEGER*4 LUN, DIV, K
        INTEGER*4 GNUM
        INTEGER*4 BASAMT, RFNAMT
        INTEGER*4 DRWNBR, DRWYEAR, DRWSZ
        INTEGER*4 CDAT(8)
        CHARACTER REPTITLE*31
        CHARACTER*(*) TCKT
        INTEGER*4 MAXDIV
        PARAMETER(MAXDIV = 40)
        INTEGER*4  TOTDIVAMT(MAXDIV),  NETDIVAMT(MAXDIV),  TOTDIVPRZ(MAXDIV)
        INTEGER*4 KTOTDIVAMT(MAXDIV), KNETDIVAMT(MAXDIV), KTOTDIVPRZ(MAXDIV)
        INTEGER*4 TOTAMT, TOTNETAMT, TOTTAX
        INTEGER*4 TXBLAMT, SHRAMT
        INTEGER*4 AGENT
C
C ==================================================================================================================================
C SCML - Departamento de Jogos            RELATORIO DEFINITIVO DE PREMIOS COM IMPOSTO DE SELO                       Data: 20.12.2012
C                                                  CONCURSO 51/2012 TOTOBOLA NORMAL 
C ==================================================================================================================================
C                                            CLASSE     VALOR BRUTO     VALOR BRUTO   VALOR SUJEITO        VALOR DO   VALOR LIQUIDO
C CODIGO DA APOSTA       AGENTE      QTD.    PREMIO       DA CLASSE       DO PREMIO       A IMPOSTO         IMPOSTO       DO PREMIO    
C ----------------------------------------------------------------------------------------------------------------------------------
C
C 350-07312550-243     12-34567         1        J1       523491.90       523491.90       518491.90       103698.38       419793.52
C                                                                      -------------------------------------------------------------
C     TOTAL                                                               523491.90       518491.90       103698.38       419793.52
C                                                                                                                    
C 350-04822176-013     12-34567         1         1        58431.99        58431.99        53431.99        10686.39        47745.60
C 350-04822176-013     12-34567         1         2         5500.00         5500.00          500.00          100.00         5400.00
C 350-04822176-013     12-34567         1        J2        50000.00        50000.00        45000.00         9000.00        41000.00
C                                                                      -------------------------------------------------------------
C     TOTAL                                                               113931.99        98931.99        19786.39        94145.60
C ----------------------------------------------------------------------------------------------------------------------------------
C
C     TOTAIS                                                             1423713.76      1343713.76       268742.72      1154971.04
C
C=======================================================================
C       PRINT TICKET DATA OF TOTOBOLA NORMAL, JOKER, TOTOLOTO QUARTA,
C       TOTOLOTO SABADO AND TOTOBOLA EXTRA 1
C=======================================================================
C
        TOTAMT = 0
        TOTNETAMT = 0
        TXBLPRZAMT = 0
        TOTTAX = 0
        SHRAMT = 0
        DO 100 DIV=1,MAXDIV
          IF (TOTDIVPRZ(DIV) .GT. 0) THEN
            SHRAMT = TOTDIVAMT(DIV) / TOTDIVPRZ(DIV) ! VALOR BRUTO DA CLASSE DE PRÉMIO
            IF (SHRAMT .GT. BASAMT) THEN
              TXBLAMT = TOTDIVAMT(DIV) - RFNAMT * TOTDIVPRZ(DIV)
              TXBLPRZAMT = TXBLPRZAMT + TXBLAMT
            ELSE
              TXBLAMT = 0
            ENDIF
C
            WRITE (LUN, 110) TCKT(1:3), TCKT(4:11), TCKT(12:14), ! TICKET NUMBER: JUL-SER-CHK
     *                       AGENT / 100000, MOD(AGENT,100000),  ! AGENT    
     *                       TOTDIVPRZ(DIV),                     ! QUANTIDADE
     *                       DIV,                                ! CLASSE DE PRÉMIO
     *                       CMONY(SHRAMT,11,VALUNIT),           ! VALOR BRUTO DA CLASSE DE PRÉMIO
     *                       CMONY(TOTDIVAMT(DIV),11,VALUNIT),   ! VALOR BRUTO DO PRÉMIO
     *                       CMONY(TXBLAMT,11,VALUNIT),          ! VALOR SUJEITO A IMPOSTO
     *      CMONY(TOTDIVAMT(DIV) - NETDIVAMT(DIV),11,VALUNIT),   ! VALOR DO IMPOSTO
     *                       CMONY(NETDIVAMT(DIV),11,VALUNIT)    ! VALOR LÍQUIDO DO PRÉMIO
C
            TOTAMT = TOTAMT + TOTDIVAMT(DIV)
            TOTNETAMT = TOTNETAMT + NETDIVAMT(DIV)
            TOTTAX = TOTTAX + TXBLAMT
          ENDIF
100     CONTINUE
C
C=======================================================================
C       PRINT TICKET DATA OF JOKER OF TOTOBOLA NORMAL
C=======================================================================
C
        DO 200 DIV=1,MAXDIV
          IF (KTOTDIVPRZ(DIV) .GT. 0) THEN
            SHRAMT = KTOTDIVAMT(DIV) / KTOTDIVPRZ(DIV) ! VALOR BRUTO DA CLASSE DE PRÉMIO
            IF (SHRAMT .GT. BASAMT) THEN
              TXBLAMT = KTOTDIVAMT(DIV) - RFNAMT * KTOTDIVPRZ(DIV)
              TXBLPRZAMT = TXBLPRZAMT + TXBLAMT
            ELSE
              TXBLAMT = 0
            ENDIF
C
            WRITE (LUN, 210) TCKT(1:3), TCKT(4:11), TCKT(12:14), ! TICKET NUMBER: JUL-SER-CHK
     *                       AGENT / 100000, MOD(AGENT,100000),  ! AGENT    
     *                       KTOTDIVPRZ(DIV),                    ! QUANTIDADE
     *                       DIV,                                ! CLASSE DE PRÉMIO
     *                       CMONY(SHRAMT,11,VALUNIT),           ! VALOR BRUTO DA CLASSE DE PRÉMIO
     *                       CMONY(KTOTDIVAMT(DIV),11,VALUNIT),  ! VALOR BRUTO DO PRÉMIO
     *                       CMONY(TXBLAMT,11,VALUNIT),          ! VALOR SUJEITO A IMPOSTO
     *      CMONY(KTOTDIVAMT(DIV) - KNETDIVAMT(DIV),11,VALUNIT), ! VALOR DO IMPOSTO
     *                       CMONY(KNETDIVAMT(DIV),11,VALUNIT)   ! VALOR LÍQUIDO DO PRÉMIO
C
            TOTAMT = TOTAMT + KTOTDIVAMT(DIV)
            TOTNETAMT = TOTNETAMT + KNETDIVAMT(DIV)
            TOTTAX = TOTTAX + TXBLAMT
          ENDIF
200     CONTINUE
C
C=======================================================================
C       PRINT TICKET TOTAL PRIZE AMOUNTS
C=======================================================================
C
        WRITE(LUN,120) CMONY(TOTAMT,11,VALUNIT),             ! VALOR TOTAL BRUTO DO PRÉMIO
     *                 CMONY(TOTTAX,11,VALUNIT),             ! VALOR TOTAL SUJEITO A IMPOSTO
     *                 CMONY(TOTAMT - TOTNETAMT,11,VALUNIT), ! VALOR TOTAL DO IMPOSTO
     *                 CMONY(TOTNETAMT,11,VALUNIT)           ! VALOR TOTAL LÍQUIDO DO PRÉMIO
C
C=======================================================================
C       FORMAT STATEMENTS
C=======================================================================
C
110     FORMAT(1X, A3, '-', A8, '-', A3, ! TICKET
     *         5X, I2.2, '-', I5.5,      ! AGENTE
     *         2X, I8,                   ! QUANTIDADE
     *         2X, I8,                   ! CLASSE DE PRÉMIO
     *         5X, A11,                  ! VALOR BRUTO DA CLASSE DE PRÉMIO
     *         5X, A11,                  ! VALOR BRUTO DO PRÉMIO
     *         5X, A11,                  ! VALOR SUJEITO A IMPOSTO
     *         5X, A11,                  ! VALOR DO IMPOSTO
     *         5X, A11)                  ! VALOR LÍQUIDO DO PRÉMIO
C
120     FORMAT(69X, 61('-'), /,           ! SEPARADOR
     *          5X, 'TOTAL',              ! TOTAL
     *         61X, A11,                  ! VALOR TOTAL BRUTO DO PRÉMIO
     *          5X, A11,                  ! VALOR TOTAL SUJEITO A IMPOSTO
     *          5X, A11,                  ! VALOR TOTAL DO IMPOSTO
     *          5X, A11, /)               ! VALOR TOTAL LÍQUIDO DO PRÉMIO
C
210     FORMAT( 1X, A3, '-', A8, '-', A3, ! TICKET
     *          5X, I2.2, '-', I5.5,      ! AGENTE
     *          2X, I8,                   ! QUANTIDADE - JOKER (TOTOBOLA NORMAL)
     *          8X, 'J', I0,              ! CLASSE DE PRÉMIO - JOKER (TOTOBOLA NORMAL)
     *          5X, A11,                  ! VALOR BRUTO DA CLASSE DE PRÉMIO - JOKER (TOTOBOLA NORMAL)
     *          5X, A11,                  ! VALOR BRUTO DO PRÉMIO - JOKER (TOTOBOLA NORMAL)
     *          5X, A11,                  ! VALOR SUJEITO A IMPOSTO - JOKER (TOTOBOLA NORMAL)
     *          5X, A11,                  ! VALOR DO IMPOSTO - JOKER (TOTOBOLA NORMAL)
     *          5X, A11)                  ! VALOR LÍQUIDO DO PRÉMIO - JOKER (TOTOBOLA NORMAL)
C
        RETURN
C
        END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_REPTXDPRZ_BODY_TMP(
C                     LUN,
C                     TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,
C                     KTOTDIVAMT,KNETDIVAMT,KTOTDIVPRZ,
C                     TCKT,BASAMT,RFNAMT,
C                     AGENT,
C                     TXBLPRZAMT, TOT_NR_PRZ_IS)
C
C       THIS SUBROUTINE PRINTS NET PRIZES INTO REPORT FILE FOR MUTUAL GAMES
C
C       INPUTS:
C        LUN          LOGICAL UNIT
C        TOTDIVAMT    TOTAL AMOUNT BY DIVISION FOR MAIN GAME (INTEGER*4 * MAXDIV)
C        NETDIVAMT    TOTAL NET AMOUNT BY DIVISION FOR MAIN GAME (INTEGER*4 * MAXDIV)
C        TOTDIVPRZ    TOTAL PRIZES BY DIVISION FOR MAIN GAME (INTEGER*4 * MAXDIV)
C        KTOTDIVAMT   TOTAL AMOUNT BY DIVISION FOR JOKER OF TOTOBOLA NORMAL (INTEGER*4 * MAXDIV)
C        KNETDIVAMT   TOTAL NET AMOUNT BY DIVISION FOR JOKER OF TOTOBOLA NORMAL (INTEGER*4 * MAXDIV)
C        KTOTDIVPRZ   TOTAL PRIZES BY DIVISION FOR JOKER OF TOTOBOLA NORMAL (INTEGER*4 * MAXDIV)
C        TCKT         TICKET NUMBER
C        BASAMT       MINIMUM AMOUNT TO WHICH THE TAX APPLIES
C        RFNAMT       REFUND AMOUNT
C        AGENT        AGENT REFERENCE
C        TOT_NR_PRZ_IS
C                     TOTAL NR PRIZES SUBJECT TO TAX
C
C       OUTPUTS:
C        TXBLPRZAMT   TAXABLE PRIZE AMOUNT
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINT_REPTXDPRZ_BODY_TMP (LUN,
     *                                   TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,
     *                                   KTOTDIVAMT,KNETDIVAMT,KTOTDIVPRZ,
     *                                   TCKT,BASAMT,RFNAMT, AGENT,
     *                                   TXBLPRZAMT, TOT_NR_PRZ_IS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER*4 TXBLPRZAMT
        INTEGER*4 LUN, DIV, K
        INTEGER*4 GNUM
        INTEGER*4 BASAMT, RFNAMT
        INTEGER*4 DRWNBR, DRWYEAR, DRWSZ
        INTEGER*4 CDAT(8)
        CHARACTER REPTITLE*31
        CHARACTER*(*) TCKT
        INTEGER*4 MAXDIV
        PARAMETER(MAXDIV = 40)
        INTEGER*4  TOTDIVAMT(MAXDIV),  NETDIVAMT(MAXDIV),  TOTDIVPRZ(MAXDIV)
        INTEGER*4 KTOTDIVAMT(MAXDIV), KNETDIVAMT(MAXDIV), KTOTDIVPRZ(MAXDIV)
        INTEGER*4 TOTAMT, TOTNETAMT, TOTTAX
        INTEGER*4 TXBLAMT, SHRAMT
        INTEGER*4 AGENT
        INTEGER*4 MAX_CLASSE, NUM_CLASSES, MAX_CLASSE_JOKER
        INTEGER*4 CLASS_CNT
        LOGICAL*1 JOKER_FLAG
        CHARACTER*83 KEY_ORDER
        INTEGER*4 TOT_NR_PRZ_IS
C
C 01 350-07312550-243     12-34567         1        J1       523491.90       523491.90       518491.90       103698.38       419793.52
C 01 350-07312550-243                      1       TOT                       523491.90       518491.90       103698.38       419793.52
C 01 350-04822176-013     12-34567         1         1        58431.99        58431.99        53431.99        10686.39        47745.60
C 01 350-04822176-013     12-34567         1         2         5500.00         5500.00          500.00          100.00         5400.00
C 01 350-04822176-013     12-34567         1        J2        50000.00        50000.00        45000.00         9000.00        41000.00
C 01 350-04822176-013                      3       TOT                       113931.99        98931.99        19786.39        94145.60
C
C=======================================================================
C       PRINT TICKET DATA OF TOTOBOLA NORMAL, JOKER, TOTOLOTO QUARTA,
C       TOTOLOTO SABADO AND TOTOBOLA EXTRA 1
C=======================================================================
C
        WRITE(KEY_ORDER,'(83X)') 
        TOTAMT = 0
        TOTNETAMT = 0
        TXBLPRZAMT = 0
        TOTTAX = 0
        SHRAMT = 0
        
        MAX_CLASSE = 9999
        MAX_CLASSE_JOKER = 9999
        NUM_CLASSES = 0
        CLASS_CNT = 1
        JOKER_FLAG = .FALSE.
        ! Iterate first in main game
        DO DIV = 1, MAXDIV
           IF (TOTDIVPRZ(DIV) .GT. 0) THEN
               MAX_CLASSE = MIN(MAX_CLASSE,DIV)
               NUM_CLASSES =  NUM_CLASSES + 1
               WRITE(KEY_ORDER(CLASS_CNT * 2 - 1 : CLASS_CNT * 2)
     *              , '(I2.2)') DIV
               CLASS_CNT = CLASS_CNT + 1
           ENDIF
        ENDDO
        ! Iterate second in kicker game
        DO DIV = 1, MAXDIV
           IF (KTOTDIVPRZ(DIV) .GT. 0) THEN
               JOKER_FLAG = .TRUE.
               MAX_CLASSE_JOKER = MIN(MAX_CLASSE_JOKER,DIV)
               NUM_CLASSES =  NUM_CLASSES + 1
               WRITE(KEY_ORDER(CLASS_CNT * 2 - 1 : CLASS_CNT * 2)
     *              , '(I2.2)') DIV
               CLASS_CNT = CLASS_CNT + 1
           ENDIF
        ENDDO
        
C        DO 100 DIV=1,MAXDIV
C           IF (KTOTDIVPRZ(DIV) .GT. 0) THEN
C               JOKER_FLAG = .TRUE.
C               MAX_CLASSE_JOKER = MIN(MAX_CLASSE_JOKER,DIV)
C               NUM_CLASSES =  NUM_CLASSES + 1
C               WRITE(KEY_ORDER(CLASS_CNT * 2 - 1 : CLASS_CNT * 2)
C     *              , '(I2.2)') DIV
C               CLASS_CNT = CLASS_CNT + 1
C           ENDIF
C           IF (TOTDIVPRZ(DIV) .GT. 0) THEN
C               MAX_CLASSE = MIN(MAX_CLASSE,DIV)
C               NUM_CLASSES =  NUM_CLASSES + 1
C               WRITE(KEY_ORDER(CLASS_CNT * 2 - 1 : CLASS_CNT * 2)
C     *              , '(I2.2)') DIV
C               CLASS_CNT = CLASS_CNT + 1
C           ENDIF
C100     CONTINUE
        IF (JOKER_FLAG .EQ. .TRUE.) THEN
            WRITE(KEY_ORDER(CLASS_CNT * 2 - 1 : CLASS_CNT * 2 - 1)
     *              , '(A1)') 'J'
        ENDIF
        IF ( MAX_CLASSE .EQ. 9999 
     *       .AND. MAX_CLASSE_JOKER .NE. 9999 ) THEN
            MAX_CLASSE = MIN (MAX_CLASSE, MAX_CLASSE_JOKER)
        ENDIF
        DO 101 DIV=1,MAXDIV
          IF (TOTDIVPRZ(DIV) .GT. 0) THEN
            SHRAMT = TOTDIVAMT(DIV) / TOTDIVPRZ(DIV) ! VALOR BRUTO DA CLASSE DE PRÉMIO
            IF (SHRAMT .GT. BASAMT) THEN
              TXBLAMT = TOTDIVAMT(DIV) - RFNAMT * TOTDIVPRZ(DIV)
              TXBLPRZAMT = TXBLPRZAMT + TXBLAMT
              TOT_NR_PRZ_IS = TOT_NR_PRZ_IS + TOTDIVPRZ(DIV)
            ELSE
              TXBLAMT = 0
            ENDIF
C
            WRITE (LUN, 110) KEY_ORDER,                          ! MAX CLASSE PREMIO
     *                       TCKT(1:3), TCKT(4:11), TCKT(12:14), ! TICKET NUMBER: JUL-SER-CHK
     *                       AGENT / 100000, MOD(AGENT,100000),  ! AGENT    
     *                       TOTDIVPRZ(DIV),                     ! QUANTIDADE
     *                       DIV,                                ! CLASSE DE PRÉMIO
     *                       CMONY(SHRAMT,11,VALUNIT),           ! VALOR BRUTO DA CLASSE DE PRÉMIO
     *                       CMONY(TOTDIVAMT(DIV),11,VALUNIT),   ! VALOR BRUTO DO PRÉMIO
     *                       CMONY(TXBLAMT,11,VALUNIT),          ! VALOR SUJEITO A IMPOSTO
     *      CMONY(TOTDIVAMT(DIV) - NETDIVAMT(DIV),11,VALUNIT),   ! VALOR DO IMPOSTO
     *                       CMONY(NETDIVAMT(DIV),11,VALUNIT)    ! VALOR LÍQUIDO DO PRÉMIO
C
            TOTAMT = TOTAMT + TOTDIVAMT(DIV)
            TOTNETAMT = TOTNETAMT + NETDIVAMT(DIV)
            TOTTAX = TOTTAX + TXBLAMT
          ENDIF
101     CONTINUE
C
C=======================================================================
C       PRINT TICKET DATA OF JOKER OF TOTOBOLA NORMAL
C=======================================================================
C

        DO 200 DIV=1,MAXDIV
          IF (KTOTDIVPRZ(DIV) .GT. 0) THEN
            SHRAMT = KTOTDIVAMT(DIV) / KTOTDIVPRZ(DIV) ! VALOR BRUTO DA CLASSE DE PRÉMIO
            IF (SHRAMT .GT. BASAMT) THEN
              TXBLAMT = KTOTDIVAMT(DIV) - RFNAMT * KTOTDIVPRZ(DIV)
              TXBLPRZAMT = TXBLPRZAMT + TXBLAMT
              TOT_NR_PRZ_IS = TOT_NR_PRZ_IS + KTOTDIVPRZ(DIV)
            ELSE
              TXBLAMT = 0
            ENDIF
C
            WRITE (LUN, 210) KEY_ORDER,                          ! MAX CLASSE PREMIO
     *                       TCKT(1:3), TCKT(4:11), TCKT(12:14), ! TICKET NUMBER: JUL-SER-CHK
     *                       AGENT / 100000, MOD(AGENT,100000),  ! AGENT    
     *                       KTOTDIVPRZ(DIV),                    ! QUANTIDADE
     *                       DIV,                                ! CLASSE DE PRÉMIO
     *                       CMONY(SHRAMT,11,VALUNIT),           ! VALOR BRUTO DA CLASSE DE PRÉMIO
     *                       CMONY(KTOTDIVAMT(DIV),11,VALUNIT),  ! VALOR BRUTO DO PRÉMIO
     *                       CMONY(TXBLAMT,11,VALUNIT),          ! VALOR SUJEITO A IMPOSTO
     *      CMONY(KTOTDIVAMT(DIV) - KNETDIVAMT(DIV),11,VALUNIT), ! VALOR DO IMPOSTO
     *                       CMONY(KNETDIVAMT(DIV),11,VALUNIT)   ! VALOR LÍQUIDO DO PRÉMIO
C
            TOTAMT = TOTAMT + KTOTDIVAMT(DIV)
            TOTNETAMT = TOTNETAMT + KNETDIVAMT(DIV)
            TOTTAX = TOTTAX + TXBLAMT
          ENDIF
200     CONTINUE
C
C=======================================================================
C       PRINT TICKET TOTAL PRIZE AMOUNTS
C=======================================================================
C
        WRITE(LUN,120) KEY_ORDER,                            ! MAX CLASSE PREMIO
     *                 TCKT(1:3), TCKT(4:11), TCKT(12:14),   ! TICKET NUMBER: JUL-SER-CHK
     *                 NUM_CLASSES,                          ! QUANTIDADE (CLASSES PREMIO)
     *                 CMONY(TOTAMT,11,VALUNIT),             ! VALOR TOTAL BRUTO DO PRÉMIO
     *                 CMONY(TOTTAX,11,VALUNIT),             ! VALOR TOTAL SUJEITO A IMPOSTO
     *                 CMONY(TOTAMT - TOTNETAMT,11,VALUNIT), ! VALOR TOTAL DO IMPOSTO
     *                 CMONY(TOTNETAMT,11,VALUNIT)           ! VALOR TOTAL LÍQUIDO DO PRÉMIO
C
C=======================================================================
C       FORMAT STATEMENTS
C=======================================================================
C
110     FORMAT(A83,                      ! CLASSES
     *         1X, A3, '-', A8, '-', A3, ! TICKET
     *         5X, I2.2, '-', I5.5,      ! MEDIADOR
     *         2X, I8,                   ! QUANTIDADE
     *         2X, I8,                   ! CLASSE DE PRÉMIO
     *         5X, A11,                  ! VALOR BRUTO DA CLASSE DE PRÉMIO
     *         5X, A11,                  ! VALOR BRUTO DO PRÉMIO
     *         5X, A11,                  ! VALOR SUJEITO A IMPOSTO
     *         5X, A11,                  ! VALOR DO IMPOSTO
     *         5X, A11)                  ! VALOR LÍQUIDO DO PRÉMIO
C
120     FORMAT(A83,                      ! CLASSES
     *         1X, A3, '-', A8, '-', A3, ! TICKET
     *        13X,                       ! MEDIADOR
     *         2X, I8,                   ! QUANTIDADE (CLASSES PREMIO)
     *         7X, 'TOT'                 ! CLASSE DE PRÉMIO
     *        16X,                       ! VALOR BRUTO DA CLASSE DE PRÉMIO
     *         5X, A11,                  ! VALOR TOTAL BRUTO DO PRÉMIO
     *         5X, A11,                  ! VALOR TOTAL SUJEITO A IMPOSTO
     *         5X, A11,                  ! VALOR TOTAL DO IMPOSTO
     *         5X, A11)                  ! VALOR TOTAL LÍQUIDO DO PRÉMIO
C
210     FORMAT(A83,                      ! CLASSES
     *         1X, A3, '-', A8, '-', A3, ! TICKET
     *         5X, I2.2, '-', I5.5,      ! MEDIADOR
     *         2X, I8,                   ! QUANTIDADE - JOKER (TOTOBOLA NORMAL)
     *         8X, 'J', I0,              ! CLASSE DE PRÉMIO - JOKER (TOTOBOLA NORMAL)
     *         5X, A11,                  ! VALOR BRUTO DA CLASSE DE PRÉMIO - JOKER (TOTOBOLA NORMAL)
     *         5X, A11,                  ! VALOR BRUTO DO PRÉMIO - JOKER (TOTOBOLA NORMAL)
     *         5X, A11,                  ! VALOR SUJEITO A IMPOSTO - JOKER (TOTOBOLA NORMAL)
     *         5X, A11,                  ! VALOR DO IMPOSTO - JOKER (TOTOBOLA NORMAL)
     *         5X, A11)                  ! VALOR LÍQUIDO DO PRÉMIO - JOKER (TOTOBOLA NORMAL)
C
        RETURN
C
        END
C----|--!--------------------------------------------------------------
C V03   ! Adding new agent field - end
C----|--!--------------------------------------------------------------
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_REPTXDPRZ_FOOTER (LUN,TOTAMT,TOTNETAMT,
C                                          TOTTAXBLAMT)
C
C       THIS SUBROUTINE PRINTS TOTAL NET PRIZES INTO REPORT FILE
C
C       INPUTS:
C        LUN         LOGICAL UNIT
C        TOTAMT      TOTAL AMOUNT
C        TOTNETAMT   TOTAL NET AMOUNT
C        TOTTAXBLAMT TOTAL TAXABLE AMOUNT
C        TOT_NR_PRZ_IS
C                    TOTAL NR PRIZES SUBJECT TO TAX
C
C       OUTPUTS:
C       *NONE*
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINT_REPTXDPRZ_FOOTER (LUN,TOTAMT,TOTNETAMT,
     *                                     TOTTAXBLAMT, TOT_NR_PRZ_IS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER*4 LUN
        INTEGER*4 TOTAMT, TOTNETAMT, TOTTAXBLAMT, TOT_NR_PRZ_IS
C
        WRITE(LUN,100) CMONY(TOTAMT,11,VALUNIT),           ! VALOR TOTAL BRUTO DOS PRÉMIOS
     *                 CMONY(TOTTAXBLAMT,11,VALUNIT),      ! VALOR TOTAL SUJEITO A IMPOSTO
     *                 CMONY(TOTAMT-TOTNETAMT,11,VALUNIT), ! VALOR TOTAL DO IMPOSTO
     *                 CMONY(TOTNETAMT,11,VALUNIT),        ! VALOR TOTAL LÍQUIDO DOS PRÉMIOS
     *                 TOT_NR_PRZ_IS                       ! NR TOTAL PRÉMIOS SUJEITOS A IMPOSTO
C----|--!--------------------------------------------------------------
C V03   ! Adding new agent field - start
C----|--!--------------------------------------------------------------
C
C=======================================================================
C       FORMAT STATEMENTS
C=======================================================================
C
100     FORMAT( 1X, 130('-'),/,/ ! SEPARADOR
     *          5X, 'TOTAIS',    ! TOTAIS
     *         60X, A11,         ! VALOR TOTAL BRUTO DOS PRÉMIOS
     *          5X, A11,         ! VALOR TOTAL SUJEITO A IMPOSTO
     *          5X, A11,         ! VALOR TOTAL DO IMPOSTO
     *          5X, A11,/,/,     ! VALOR TOTAL LÍQUIDO DOS PRÉMIOS
     *          5X, 'TOTAL DE PREMIOS COM IMPOSTO DE SELO: ', I0, /)! NR TOTAL PRÉMIOS SUJEITOS A IMPOSTO
     
C----|--!--------------------------------------------------------------
C V03   ! Adding new agent field - end
C----|--!--------------------------------------------------------------
C
        RETURN
C
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE GET_SHV(GNUM,DRW,GAMSHV,NUMDIVS)
C
C       THIS SUBROUTINE GETS THE SHARE VALUES FOR THE GIVEN GAME NUMBER
C       AND DRAW
C
C       INPUTS:
C        GNUM        GAME NUMBER
C        DRW         DRAW NUMBER
C
C       OUTPUTS:
C        GAMSHV      GAME SHARE VALUES
C        NUMDIVS     TOTAL DIVISIONS
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE GET_SHV(GNUM,DRW,GAMSHV,NUMDIVS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
        INCLUDE 'INCLIB:DTGREC.DEF'
C
        INTEGER*4 MAXDIV
        PARAMETER(MAXDIV = 40)
C
        INTEGER*4 ST
        INTEGER*4 GAMSHV(MAXDIV), DRW, NUMDIVS
        INTEGER*4 GLUN, FDB(7), GNUM, DIV, GTYP
C
        CALL FASTSET(0,GAMSHV,MAXDIV)
        NUMDIVS = 0
C
        GTYP = GNTTAB(GAMTYP,GNUM)
        IF (DRW .GT. 0 .AND. GTYP .NE. TPAS) THEN
          CALL FIND_AVAILABLE_LUN(GLUN,ST)
          IF (ST .NE. 0) THEN
            TYPE*, IAM(), 'Erro a obter uma LUN para o jogo: ',GNUM
            CALL GPAUSE()
            CALL GSTOP(GEXIT_FATAL)
          ENDIF
          CALL OPENW(GLUN,GFNAMES(1,GNUM),4,0,0,ST)
C
          IF (GTYP .EQ. TLTO) THEN
            CALL IOINIT(FDB,GLUN,DLTSEC*256)
          ELSEIF (GTYP .EQ. TSPT) THEN
            CALL IOINIT(FDB,GLUN,DSPSEC*256)
          ELSEIF (GTYP .EQ. TKIK) THEN
            CALL IOINIT(FDB,GLUN,DKKSEC*256)
          ENDIF
          IF (ST .NE. 0) THEN
            CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
            CALL GSTOP(GEXIT_FATAL)
          ENDIF
C
          IF (GTYP .EQ. TLTO) THEN
            CALL READW(FDB,DRW,DLTREC,ST)
            DO DIV=1,DLTDIV
              GAMSHV(DIV) = DLTSHV(DIV,1)
            ENDDO
            NUMDIVS = DLTDIV
          ELSEIF (GTYP .EQ. TSPT) THEN
            CALL READW(FDB,DRW,DSPREC,ST)
            DO DIV=1,DSPDIV
              GAMSHV(DIV) = DSPSHV(DIV)
            ENDDO
            NUMDIVS = DSPDIV
          ELSEIF (GTYP .EQ. TKIK) THEN
            IF(DRW .LE. DAYHDR(GNUM)) THEN   ! IF JOKER GAME STILL ACTIVE IN THE SYSTEM
               CALL READW(FDB,DRW,DKKREC,ST)
               DO DIV=1,DKKDIV
                 GAMSHV(DIV) = DKKSHV(DIV)
               ENDDO
               NUMDIVS = DKKDIV
            ENDIF
          ENDIF
          IF (ST .NE. 0) THEN
            CALL FILERR(GFNAMES(1,GNUM),2,ST,DRW)
          ENDIF
          CALL CLOSEFIL(FDB)
        ENDIF
C
        RETURN
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE GET_PASSHV(GNUM,DRW,GAMSHV,GAMEXSHV,NUMDIVS,DRDAT,
C                             NOFFRAC)
C
C       THIS SUBROUTINE GETS THE SHARE VALUES FOR THE GIVEN PASSIVE GAME
C       AND DRAW
C
C       INPUTS:
C        GNUM        GAME NUMBER (CLASSICA OR POPULAR)
C        DRW         DRAW NUMBER
C
C       OUTPUTS:
C        GAMSHV      SHARE VALUES (IF POPULAR THIS IS THE WINNING SERIE SHARE VALUES)
C        GAMEXSHV    NOT WINNING SERIES SHARE VALUES (POPULAR ONLY)
C        NUMDIVS     TOTAL DIVISIONS
C        DRDAT       DRAW DATE
C        NOFFRAC     NUMBER OF FRACTIONS PER TICKET (FOR POPULAR THE VALUE
C                    IS SET TO ONE; FOR CLASSICA THE VALUE IS TAKEN FROM
C                    GAME FILE)
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE GET_PASSHV(GNUM,DRW,GAMSHV,GAMEXSHV,NUMDIVS,DRDAT,
     *                        NOFFRAC)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DPAREC.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4 MAXDIV
        PARAMETER(MAXDIV = 40)
C
        INTEGER*4 ST
        INTEGER*8 GAMSHV(MAXDIV), GAMEXSHV(MAXDIV)
        INTEGER*4 DRW, NUMDIVS, DRWDT, NOFFRAC
        INTEGER*4 GLUN, FDB(7), GNUM, DIV, GTYP
        INTEGER*2 DRDAT(LDATE_LEN) ! DRAWING DATE
C
        CALL FASTSET(0,GAMSHV,MAXDIV)
        CALL FASTSET(0,GAMEXSHV,MAXDIV)
        NUMDIVS = 0
C
        GTYP = GNTTAB(GAMTYP,GNUM)
        IF (DRW .GT. 0 .AND. GTYP .EQ. TPAS) THEN
          CALL FIND_AVAILABLE_LUN(GLUN,ST)
          IF (ST .NE. 0) THEN
            TYPE*, IAM(), 'Erro a obter uma LUN para o jogo: ',GNUM
            CALL GPAUSE()
            CALL GSTOP(GEXIT_FATAL)
          ENDIF
          CALL OPENW(GLUN,GFNAMES(1,GNUM),4,0,0,ST)
C
          IF (GTYP .EQ. TPAS) THEN
            CALL IOINIT(FDB,GLUN,DPASEC*256)
          ENDIF
          IF (ST .NE. 0) THEN
            CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
            CALL GSTOP(GEXIT_FATAL)
          ENDIF
C
          IF (GTYP .EQ. TPAS) THEN
            CALL READW(FDB,DRW,DPAREC,ST)
            DRDAT(VCDC) = DPAESD
            CALL LCDATE(DRDAT(1))
C
            IF (GNUM .EQ. 8 ) THEN ! CLASSICA
              NOFFRAC = DPANOFFRA
            ELSEIF (GNUM .EQ. 9 ) THEN ! POPULAR
              NOFFRAC = 1
            ENDIF
C
            DO DIV=1,PAGDIV
              ! GET SHARE VALUES
              ! IF POPULAR ALL THIS SHARES BELONGS TO THE WINNING SERIE
              ! (SOME OF THEM ALSO BELONG TO ALL NOT WINNING SERIES)
              GAMSHV(DIV) = DPASHV(DIV)
            ENDDO
            NUMDIVS = MIN(PAGDIV,DPADIV)
C
            IF (GNUM .EQ. 9) THEN ! POPULAR
              ! FIRST, GET SHARE VALUES OF NOT WINNING SERIES
              DO DIV=1,PAGEDV
                GAMEXSHV(DIV) = DPAEXSHV(DIV)
              ENDDO
              ! THEN, FILL THE REMAINING SHARES OF NOT WINNING SERIES
              ! WITH VALUES OF THE WINNING SERIE (THERE ARE VALUES IN
              ! COMMON WITH ALL SERIES)
              DO DIV=1,NUMDIVS
                IF (GAMEXSHV(DIV) .EQ. 0) GAMEXSHV(DIV) = GAMSHV(DIV)
              ENDDO
            ENDIF
          ENDIF
          IF (ST .NE. 0) THEN
            CALL FILERR(GFNAMES(1,GNUM),2,ST,DRW)
          ENDIF
          CALL CLOSEFIL(FDB)
        ENDIF
C
        RETURN
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE CHECKPASTAX(VALREC,BASAMT,RFNAMT,TAXPER,GAMSHV,DRWN,
C                              NOFFRAC,GAMEXSHV,POPWSER,
C                              TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,PRZAMT,
C                              NETPRZAMT,HASTAX)
C
C       THIS SUBROUTINE CALCULATES THE NET AMOUNT VALUE FOR EACH PRIZE DIVISION
C       OF PASSIVE GAMES.
C       FOR EACH PRIZE DIVISION VALUE GREATER THAN <BASAMT> THE VALUE IS TAXED,
C       ACCORDINGLY TO THE PERCENTAGE VALUE <TAXPER>.
C       THE TAX AMOUNT IS TRUNCATED (NOT ROUNDED).
C
C       INPUTS:
C        VALREC           VALIDATION RECORD TO PROCESS
C        BASAMT           MINIMUM AMOUNT TO WHICH THE TAX APPLIES
C        RFNAMT           TAX REFUND AMOUNT
C        TAXPER           TAX PERCENTAGE
C        GAMSHR           GAME SHARE VALUES
C        DRWN             DRAW NUMBER
C        NOFFRAC          NUMBER OF FRACTIONS PER TICKET
C        GAMEXSHV         NOT WINNING SERIES SHARE VALUES (POPULAR ONLY)
C        POPWSER          WINNING SERIE (POPULAR ONLY)
C
C       OUTPUTS:
C        TOTDIVAMT        TOTAL AMOUNT BY DIVISION FOR MAIN GAME (INTEGER*4 * MAXDIV)
C        NETDIVAMT        TOTAL NET AMOUNT BY DIVISION FOR MAIN GAME (INTEGER*4 * MAXDIV)
C        TOTDIVPRZ        TOTAL PRIZES BY DIVISION FOR MAIN GAME (INTEGER*4 * MAXDIV)
C        PRZAMT           TOTAL PRIZE AMOUNT
C        NETPRZAMT        TOTAL NET PRIZE AMOUNT
C        HASTAX           TRUE IF THE TOTAL PRIZE AMOUNT HAS BEEN TAXED,
C                         FALSE OTHERWISE
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CHECKPASTAX (VALREC,BASAMT,RFNAMT,TAXPER,GAMSHV,DRWN,
     *                          NOFFRAC,GAMEXSHV,POPWSER,
     *                          TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,
     *                          PRZAMT,NETPRZAMT,HASTAX)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
C
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'
        INCLUDE 'INCLIB:WINUPD.DEF'
C
C=======================================================================
C       VARIABLES
C=======================================================================
C
        INTEGER*4 I
        INTEGER*4 BASAMT, RFNAMT, TAXPER
        INTEGER*4 SHRAMT, TAXAMT
        INTEGER*4 MAXDIV
        PARAMETER(MAXDIV = 40)
        INTEGER*8 GAMSHV(MAXDIV), GAMEXSHV(MAXDIV)
        INTEGER*4 DRWN
        INTEGER*4 NOFFRAC
        INTEGER*4 POPWSER
C
C       DATA STRUCTURES FOR:
C          CLASSICA AND POPULAR
        INTEGER*4 TOTDIVAMT(MAXDIV) ! IT MUST BE NOT LESS THAN THE MAXIMUM NUMBER OF DIVISIONS
        INTEGER*4 NETDIVAMT(MAXDIV) !
        INTEGER*4 TOTDIVPRZ(MAXDIV) !
        INTEGER*4 PRZAMT, NETPRZAMT
C
        INTEGER*4 PAS_ROUND_VALUE
        INTEGER*4 GTYP, GIND, KGAM, GAM, FRCS
        INTEGER*4 DDRW, DDIV, DSHR
C
        LOGICAL   HASTAX
C
        CALL FASTSET(0,TOTDIVAMT,MAXDIV)
        CALL FASTSET(0,NETDIVAMT,MAXDIV)
        CALL FASTSET(0,TOTDIVPRZ,MAXDIV)
C
        HASTAX = .FALSE.
        TAXAMT = 0
        PRZAMT = 0
        NETPRZAMT = 0
C
        GTYP = VALREC(VGTYP)
        GIND = VALREC(VGIND)
        GAM  = VALREC(VGAM)
C
        IF (GTYP .EQ. TPAS) THEN
          CALL DLOGPAS(VALREC,VDETAIL)
          DO 100 I = 1,VALREC(VPZOFF)
            DDRW = VDETAIL(VDRW,I) ! DRAW
            DDIV = VDETAIL(VDIV,I) ! DIVISION
            DSHR = VDETAIL(VSHR,I) ! NUMBER OF SHARES IN DIVISION
            IF (DDRW .LE. 0) GOTO 100
            IF (GIND .NE. 1 .AND. GIND .NE. 2) GOTO 100
            IF (DDRW .NE. DRWN) GOTO 100
            SHRAMT = 0
            TAXAMT = 0
C
            IF (GIND .EQ. 2) THEN ! POPULAR
              IF (VALREC(VPFRAC) .EQ. POPWSER) THEN
                ! WINNER SERIE
                SHRAMT = PAS_ROUND_VALUE(GAMSHV(DDIV)) / NOFFRAC
              ELSE
                ! NOT A WINNER SERIE
                SHRAMT = PAS_ROUND_VALUE(GAMEXSHV(DDIV)) / NOFFRAC
              ENDIF
            ELSE
              ! CLASSICA
              SHRAMT = PAS_ROUND_VALUE(GAMSHV(DDIV)) / NOFFRAC
            ENDIF
C
            IF (SHRAMT .GT. BASAMT) THEN
              HASTAX = .TRUE.
              ! TAX AMOUNT (ADD 0.50 TO GET THE ROUNDED VALUE; REMOVE IT IF YOU WANT THE TRUNCATED VALUE)
              ! TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10) + 0.50D0))
              TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10)))
            ENDIF
            ! TOTAL AMOUNT
            TOTDIVAMT(DDIV) = TOTDIVAMT(DDIV) + SHRAMT * DSHR
            ! NET AMOUNT
            NETDIVAMT(DDIV) = NETDIVAMT(DDIV) + SHRAMT * DSHR - TAXAMT * DSHR
            ! TOTAL PRIZES
            TOTDIVPRZ(DDIV) = TOTDIVPRZ(DDIV) + DSHR
            ! TOTAL AMOUNT AND TOTAL NET AMOUNT
            NETPRZAMT = NETPRZAMT + SHRAMT * DSHR - TAXAMT * DSHR
            PRZAMT = PRZAMT + SHRAMT * DSHR
            GOTO 100
C
100       CONTINUE
C
        ENDIF
C
        RETURN
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINTPAS_REPTXDPRZ_HEADER(LUN,GNUM,DRWNBR,DRWYEAR,
C                                         CDAT,FINREP)
C
C       THIS SUBROUTINE PRINTS THE TITLE REPORT FOR PASSIVE GAMES
C
C       INPUTS:
C        LUN         LOGICAL UNIT
C        GNUM        GAME NUMBER
C        DRWNBR      DRAW NUMBER
C        DRWYEAR     DRAW YEAR
C        CDAT        DATE
C        FINREP      FLAG (FOR PASSIVE GAMES, INDICATING WHETHER THE
C                    REPORT IS FINAL OR PROVISIONAL)
C
C       OUTPUTS:
C       *NONE*
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINTPAS_REPTXDPRZ_HEADER (LUN, GNUM, DRWNBR, DRWYEAR,
     *                                       CDAT, FINREP)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER*4 LUN, K, TLEN
        INTEGER*4 GNUM
        INTEGER*4 DRWNBR, DRWYEAR, DRWSZ
        INTEGER*4 CDAT(8)
        CHARACTER REPTITLE*53
        CHARACTER CONSOREXT*8
        LOGICAL   FINREP
C
        IF (GNUM .EQ. 8 .OR. GNUM .EQ. 9) THEN
          IF (FINREP) THEN ! FINAL REPORTING
            REPTITLE = ' RELATORIO DEFINITIVO DE PREMIOS COM IMPOSTO DE SELO '
          ELSE             ! PROVISIONAL REPORTING
            REPTITLE = ' RELATORIO PROVISORIO DE PREMIOS COM IMPOSTO DE SELO '
          ENDIF
          CONSOREXT = 'EXTRACAO'
          TLEN = 55
          DRWSZ = 2 ! EE
        ENDIF
C
C ==================================================================================================================================
C SCML - Departamento de Jogos            RELATORIO PROVISORIO DE PREMIOS COM IMPOSTO DE SELO                       Data: 20.12.2012
C                                                      EXTRACAO 51/2012 CLASSICA
C                                                      EXTRACAO 51/2012 POPULAR
C ==================================================================================================================================
C                                            CLASSE     VALOR BRUTO     VALOR BRUTO   VALOR SUJEITO        VALOR DO   VALOR LIQUIDO
C  BILHETE-SER-FRA     MEDIADOR      QTD.    PREMIO       DA CLASSE       DO PREMIO       A IMPOSTO         IMPOSTO       DO PREMIO    
C ----------------------------------------------------------------------------------------------------------------------------------
C
        WRITE (LUN, 100) REPTITLE,
     *                   CDAT(3), CDAT(2), CDAT(1),
     *                   CONSOREXT,
     *                   DRWNBR, DRWYEAR,
     *                   (GLNAMES(K,GNUM),K=1,4)
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
100     FORMAT(1X, 130('='), /,
     *         1X, 'SCML - Departamento de Jogos', T41, A53, T116, 'Data: ', I2.2, '.', I2.2, '.', I4.4,/,
     *         1X, '                            ', T<TLEN>, A, 1X, I<DRWSZ>.<DRWSZ>, '/', I4.4, 1X, 4A4,/,
     *         1X, 130('='), /,
     *         1X, '                                           CLASSE',
     *             '     VALOR BRUTO     VALOR BRUTO   VALOR SUJEITO',
     *             '        VALOR DO   VALOR LIQUIDO',/,
     *         1X, ' BILHETE-SER-FRA     MEDIADOR      QTD.    PREMIO',
     *             '       DA CLASSE       DO PREMIO       A IMPOSTO',
     *             '         IMPOSTO       DO PREMIO',/,
     *         1X, 130('-')/)

C100     FORMAT(1X, 130('='), /,
C     *         1X, 'SCML - Departamento de Jogos', T41, A53, T116, 'Data: ', I2.2, '.', I2.2, '.', I4.4,/,
C     *         1X, '                            ', T<TLEN>, A, 1X, I<DRWSZ>.<DRWSZ>, '/', I4.4, 1X, 4A4,/,
C     *         1X, 130('='), /,
C     *         1X, '                              CLASSE DE       ',
C     *             'VALOR BRUTO       VALOR BRUTO     VALOR SUJEITO          VALOR DO     VALOR LIQUIDO',/,
C     *         1X, ' BILHETE-SER-FRA      QTD.       PREMIO       ',
C     *             '  DA CLASSE         DO PREMIO         A IMPOSTO           IMPOSTO         DO PREMIO',/,
C     *         1X, 130('-')/)
        RETURN
C
        END
C
C----|--!--------------------------------------------------------------
C V03   ! Adding new agent field - start
C----|--!--------------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINTPAS_REPTXDPRZ_BODY(LUN,TOTDIVAMT,NETDIVAMT,
C                                          TOTDIVPRZ,TCKT,
C                                          BASAMT,RFNAMT,
C                                          AGENT,
C                                          TXBLPRZAMT)
C
C       THIS SUBROUTINE PRINTS PASSIVE NET PRIZES INTO REPORT FILE
C
C       INPUTS:
C        LUN         LOGICAL UNIT
C        TOTDIVAMT   TOTAL AMOUNT BY DIVISION (INTEGER*4 * MAXDIV)
C        NETDIVAMT   TOTAL NET AMOUNT BY DIVISION (INTEGER*4 * MAXDIV)
C        TOTDIVPRZ   TOTAL PRIZES BY DIVISION (INTEGER*4 * MAXDIV)
C        TCKT        TICKET (BBBBBSSFF), BBBBB - TICKET
C                                        SS - SERIE
C                                        FF - FRACTION
C        BASAMT      MINIMUM AMOUNT TO WHICH THE TAX APPLIES
C        RFNAMT      REFUND AMOUNT
C        AGENT       AGENT
C
C       OUTPUTS:
C        TXBLPRZAMT  TAXABLE PRIZE AMOUNT
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINTPAS_REPTXDPRZ_BODY (LUN,
     *                                   TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,
     *                                   TCKT,BASAMT,RFNAMT, AGENT,
     *                                   TXBLPRZAMT)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER*4 TXBLPRZAMT
        INTEGER*4 LUN, DIV, K
        INTEGER*4 GNUM
        INTEGER*4 BASAMT, RFNAMT
        INTEGER*4 DRWNBR, DRWYEAR, DRWSZ
        INTEGER*4 CDAT(8)
        CHARACTER REPTITLE*31
        CHARACTER*(*) TCKT
        INTEGER*4 MAXDIV
        PARAMETER(MAXDIV = 40)
        INTEGER*4  TOTDIVAMT(MAXDIV),  NETDIVAMT(MAXDIV),  TOTDIVPRZ(MAXDIV)
        INTEGER*4 TOTAMT, TOTNETAMT, TOTTAX
        INTEGER*4 TXBLAMT, SHRAMT
        INTEGER*4 AGENT
C
C ==================================================================================================================================
C SCML - Departamento de Jogos            RELATORIO PROVISORIO DE PREMIOS COM IMPOSTO DE SELO                       Data: 20.12.2012
C                                                      EXTRACAO 51/2012 CLASSICA
C ==================================================================================================================================
C                                            CLASSE     VALOR BRUTO     VALOR BRUTO   VALOR SUJEITO        VALOR DO   VALOR LIQUIDO
C  BILHETE-SER-FRA       AGENTE      QTD.    PREMIO       DA CLASSE       DO PREMIO       A IMPOSTO         IMPOSTO       DO PREMIO    
C ----------------------------------------------------------------------------------------------------------------------------------
C                                                                                                                
C      12345-08-01     12-34567         1         3        20000.00        20000.00        15000.00         3000.00        17000.00
C                                                                       ------------------------------------------------------------
C     TOTAL                                                                20000.00        15000.00         3000.00        17000.00
C
C      12345-08-02     12-34567         1         3        20000.00        20000.00        15000.00         3000.00        17000.00
C                                                                       ------------------------------------------------------------
C     TOTAL                                                                20000.00        15000.00         3000.00        17000.00
C
C ----------------------------------------------------------------------------------------------------------------------------------

C     TOTAIS                                                               40000.00        30000.00         6000.00        34000.00
C
C=======================================================================
C       PRINT TICKET DATA OF PASSIVE GAMES (CLASSICA AND POPULAR)
C=======================================================================
C
        TOTAMT = 0
        TOTNETAMT = 0
        TXBLPRZAMT = 0
        TOTTAX = 0
        SHRAMT = 0
        DO 100 DIV=1,MAXDIV
          IF (TOTDIVPRZ(DIV) .GT. 0) THEN
            SHRAMT = TOTDIVAMT(DIV) / TOTDIVPRZ(DIV) ! VALOR BRUTO DA CLASSE DE PRÉMIO
            IF (SHRAMT .GT. BASAMT) THEN
              TXBLAMT = TOTDIVAMT(DIV) - RFNAMT * TOTDIVPRZ(DIV)
              TXBLPRZAMT = TXBLPRZAMT + TXBLAMT
            ELSE
              TXBLAMT = 0
            ENDIF
C
            WRITE (LUN, 110) TCKT(1:5), TCKT(6:7), TCKT(8:9),  ! TICK-SER-FRAC
     *                       AGENT / 100000, MOD(AGENT,100000),! AGENT                
     *                       TOTDIVPRZ(DIV),                   ! QUANTIDADE
     *                       DIV,                              ! CLASSE DE PRÉMIO
     *                       CMONY(SHRAMT,11,VALUNIT),         ! VALOR BRUTO DA CLASSE DE PRÉMIO
     *                       CMONY(TOTDIVAMT(DIV),11,VALUNIT), ! VALOR BRUTO DO PRÉMIO
     *                       CMONY(TXBLAMT,11,VALUNIT),        ! VALOR SUJEITO A IMPOSTO
     *      CMONY(TOTDIVAMT(DIV) - NETDIVAMT(DIV),11,VALUNIT), ! VALOR DO IMPOSTO
     *                       CMONY(NETDIVAMT(DIV),11,VALUNIT)  ! VALOR LÍQUIDO DO PRÉMIO
C
            TOTAMT = TOTAMT + TOTDIVAMT(DIV)
            TOTNETAMT = TOTNETAMT + NETDIVAMT(DIV)
            TOTTAX = TOTTAX + TXBLAMT
          ENDIF
100     CONTINUE
C
C=======================================================================
C       PRINT TICKET TOTAL PRIZE AMOUNTS
C=======================================================================
C
        WRITE(LUN,120) CMONY(TOTAMT,11,VALUNIT),             ! VALOR TOTAL BRUTO DO PRÉMIO
     *                 CMONY(TOTTAX,11,VALUNIT),             ! VALOR TOTAL SUJEITO A IMPOSTO
     *                 CMONY(TOTAMT - TOTNETAMT,11,VALUNIT), ! VALOR TOTAL DO IMPOSTO
     *                 CMONY(TOTNETAMT,11,VALUNIT)           ! VALOR TOTAL LÍQUIDO DO PRÉMIO
C
C=======================================================================
C       FORMAT STATEMENTS
C=======================================================================
C
110     FORMAT(6X, A5, '-', A2, '-', A2, ! TICKET-SER-FRAC
     *         5X, I2.2, '-', I5.5,      ! AGENTE
     *         2X, I8,                   ! QUANTIDADE
     *         2X, I8,                   ! CLASSE DE PRÉMIO
     *         5X, A11,                  ! VALOR BRUTO DA CLASSE DE PRÉMIO
     *         5X, A11,                  ! VALOR BRUTO DO PRÉMIO
     *         5X, A11,                  ! VALOR SUJEITO A IMPOSTO
     *         5X, A11,                  ! VALOR DO IMPOSTO
     *         5X, A11)                  ! VALOR LÍQUIDO DO PRÉMIO

C
120     FORMAT(69X, 61('-'), /,           ! SEPARADOR
     *          5X, 'TOTAL',              ! TOTAL
     *         61X, A11,                  ! VALOR TOTAL BRUTO DO PRÉMIO
     *          5X, A11,                  ! VALOR TOTAL SUJEITO A IMPOSTO
     *          5X, A11,                  ! VALOR TOTAL DO IMPOSTO
     *          5X, A11, /)               ! VALOR TOTAL LÍQUIDO DO PRÉMIO
C
        RETURN
C
        END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINTPAS_REPTXDPRZ_BODY_TMP(LUN,TOTDIVAMT,NETDIVAMT,
C                                          TOTDIVPRZ,TCKT,
C                                          BASAMT,RFNAMT,
C                                          AGENT, IS_WIN_SER,
C                                          TXBLPRZAMT, TOT_NR_PRZ_IS)
C
C       THIS SUBROUTINE PRINTS PASSIVE NET PRIZES INTO REPORT FILE
C
C       INPUTS:
C        LUN         LOGICAL UNIT
C        TOTDIVAMT   TOTAL AMOUNT BY DIVISION (INTEGER*4 * MAXDIV)
C        NETDIVAMT   TOTAL NET AMOUNT BY DIVISION (INTEGER*4 * MAXDIV)
C        TOTDIVPRZ   TOTAL PRIZES BY DIVISION (INTEGER*4 * MAXDIV)
C        TCKT        TICKET (BBBBBSSFF), BBBBB - TICKET
C                                        SS - SERIE
C                                        FF - FRACTION
C        BASAMT      MINIMUM AMOUNT TO WHICH THE TAX APPLIES
C        RFNAMT      REFUND AMOUNT
C        AGENT       AGENT (IF AGENT .GT. 0 THEN TICKET IS ON-LINE)
C        IS_WIN_SER  IS WINNING SERIES ?
C
C       OUTPUTS:
C        TXBLPRZAMT  TAXABLE PRIZE AMOUNT
C        TOT_NR_PRZ_IS
C                    TOTAL NR PRIZES SUBJECT TO TAX
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINTPAS_REPTXDPRZ_BODY_TMP (LUN,
     *                                   TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,
     *                                   TCKT,BASAMT,RFNAMT, AGENT,
     *                                   IS_WIN_SER,
     *                                   TXBLPRZAMT, TOT_NR_PRZ_IS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER*4 TXBLPRZAMT
        INTEGER*4 LUN, DIV, K
        INTEGER*4 GNUM
        INTEGER*4 BASAMT, RFNAMT
        INTEGER*4 DRWNBR, DRWYEAR, DRWSZ
        INTEGER*4 CDAT(8)
        CHARACTER REPTITLE*31
        CHARACTER*(*) TCKT
        INTEGER*4 MAXDIV
        PARAMETER(MAXDIV = 40)
        INTEGER*4  TOTDIVAMT(MAXDIV),  NETDIVAMT(MAXDIV),  TOTDIVPRZ(MAXDIV)
        INTEGER*4 TOTAMT, TOTNETAMT, TOTTAX
        INTEGER*4 TXBLAMT, SHRAMT
        INTEGER*4 AGENT, AGENT_TMP
        INTEGER*4 MAX_CLASSE, NUM_CLASSES
        CHARACTER*83 KEY_ORDER
        INTEGER*4 TOT_NR_PRZ_IS, CLASS_CNT
        LOGICAL   IS_WIN_SER
        CHARACTER*1 WIN_SER
C
C      12345-08-01     00-00000         1         3        20000.00        20000.00        15000.00         3000.00        17000.00
C      12345-08-01     00-00000         1       TOT        20000.00        20000.00        15000.00         3000.00        17000.00
C      12345-08-02     00-00000         1         3        20000.00        20000.00        15000.00         3000.00        17000.00
C      12345-08-02     00-00000         1       TOT        20000.00        20000.00        15000.00         3000.00        17000.00
C
C=======================================================================
C       PRINT TICKET DATA OF PASSIVE GAMES (CLASSICA AND POPULAR)
C=======================================================================
C
        WRITE(KEY_ORDER,'(83X)') 
        AGENT_TMP = AGENT
        TOTAMT = 0
        TOTNETAMT = 0
        TXBLPRZAMT = 0
        TOTTAX = 0
        SHRAMT = 0
        MAX_CLASSE = 9999
        NUM_CLASSES = 0
        CLASS_CNT = 1
        
        ! First we have to determine which is the maximum prize class
        ! in order to have a group-by by bet
        DO 100 DIV=1,MAXDIV
          IF (TOTDIVPRZ(DIV) .GT. 0) THEN
              MAX_CLASSE = MIN(MAX_CLASSE,DIV)
              NUM_CLASSES =  NUM_CLASSES + 1
              WRITE(KEY_ORDER(CLASS_CNT * 2 - 1 : CLASS_CNT * 2)
     *              , '(I2.2)') DIV
              CLASS_CNT = CLASS_CNT + 1
          ENDIF
100     CONTINUE
            
        DO 101 DIV=1,MAXDIV
          IF (TOTDIVPRZ(DIV) .GT. 0) THEN
            SHRAMT = TOTDIVAMT(DIV) / TOTDIVPRZ(DIV) ! VALOR BRUTO DA CLASSE DE PRÉMIO
            IF (SHRAMT .GT. BASAMT) THEN
              TXBLAMT = TOTDIVAMT(DIV) - RFNAMT * TOTDIVPRZ(DIV)
              TXBLPRZAMT = TXBLPRZAMT + TXBLAMT
              TOT_NR_PRZ_IS = TOT_NR_PRZ_IS + TOTDIVPRZ(DIV)
            ELSE
              TXBLAMT = 0
            ENDIF
C
            IF (IS_WIN_SER) THEN
                WIN_SER = ' '
            ELSE
                WIN_SER = 'N'
            ENDIF
            
            IF (AGENT_TMP .LT. 0) THEN
                ! AGENT TYPE IS OFF-LINE
                WRITE (LUN, 111) KEY_ORDER,                        ! MAX CLASSE PREMIO
     *                           WIN_SER,                          ! WINNING SERIES
     *                           TCKT(1:5), TCKT(6:7), TCKT(8:9),  ! TICK-SER-FRAC
     *                           TOTDIVPRZ(DIV),                   ! QUANTIDADE
     *                           DIV,                              ! CLASSE DE PRÉMIO
     *                           CMONY(SHRAMT,11,VALUNIT),         ! VALOR BRUTO DA CLASSE DE PRÉMIO
     *                           CMONY(TOTDIVAMT(DIV),11,VALUNIT), ! VALOR BRUTO DO PRÉMIO
     *                           CMONY(TXBLAMT,11,VALUNIT),        ! VALOR SUJEITO A IMPOSTO
     *          CMONY(TOTDIVAMT(DIV) - NETDIVAMT(DIV),11,VALUNIT), ! VALOR DO IMPOSTO
     *                           CMONY(NETDIVAMT(DIV),11,VALUNIT)  ! VALOR LÍQUIDO DO PRÉMIO
            ELSE
                WRITE (LUN, 110) KEY_ORDER,                        ! MAX CLASSE PREMIO
     *                           WIN_SER,                          ! WINNING SERIES
     *                           TCKT(1:5), TCKT(6:7), TCKT(8:9),  ! TICK-SER-FRAC
     *                           AGENT_TMP / 100000,               ! AGENT (2 digitos)
     *                           MOD(AGENT_TMP,100000),            ! AGENT (5 digitos)    
     *                           TOTDIVPRZ(DIV),                   ! QUANTIDADE
     *                           DIV,                              ! CLASSE DE PRÉMIO
     *                           CMONY(SHRAMT,11,VALUNIT),         ! VALOR BRUTO DA CLASSE DE PRÉMIO
     *                           CMONY(TOTDIVAMT(DIV),11,VALUNIT), ! VALOR BRUTO DO PRÉMIO
     *                           CMONY(TXBLAMT,11,VALUNIT),        ! VALOR SUJEITO A IMPOSTO
     *          CMONY(TOTDIVAMT(DIV) - NETDIVAMT(DIV),11,VALUNIT), ! VALOR DO IMPOSTO
     *                           CMONY(NETDIVAMT(DIV),11,VALUNIT)  ! VALOR LÍQUIDO DO PRÉMIO
            ENDIF
C
            TOTAMT = TOTAMT + TOTDIVAMT(DIV)
            TOTNETAMT = TOTNETAMT + NETDIVAMT(DIV)
            TOTTAX = TOTTAX + TXBLAMT
          ENDIF
101     CONTINUE
C
C=======================================================================
C       PRINT TICKET TOTAL PRIZE AMOUNTS
C=======================================================================
C
        WRITE(LUN,120) KEY_ORDER,                            ! MAX CLASSE PREMIO
     *                 WIN_SER,                              ! WINNING SERIES
     *                 TCKT(1:5), TCKT(6:7), TCKT(8:9),      ! TICKET NUMBER: JUL-SER-CHK
     *                 NUM_CLASSES,                          ! QUANTIDADE (CLASSES PREMIO)
     *                 CMONY(TOTAMT,11,VALUNIT),             ! VALOR TOTAL BRUTO DO PRÉMIO
     *                 CMONY(TOTTAX,11,VALUNIT),             ! VALOR TOTAL SUJEITO A IMPOSTO
     *                 CMONY(TOTAMT - TOTNETAMT,11,VALUNIT), ! VALOR TOTAL DO IMPOSTO
     *                 CMONY(TOTNETAMT,11,VALUNIT)           ! VALOR TOTAL LÍQUIDO DO PRÉMIO
C
C=======================================================================
C       FORMAT STATEMENTS
C=======================================================================
C
110     FORMAT(A83,                      ! MAX CLASSE PREMIO
     *         A1,                       ! WINNING SERIES
     *         6X, A5, '-', A2, '-', A2, ! TICKET-SER-FRAC
     *         5X, I2.2, '-', I5.5,      ! AGENTE
     *         2X, I8,                   ! QUANTIDADE
     *         2X, I8,                   ! CLASSE DE PRÉMIO
     *         5X, A11,                  ! VALOR BRUTO DA CLASSE DE PRÉMIO
     *         5X, A11,                  ! VALOR BRUTO DO PRÉMIO
     *         5X, A11,                  ! VALOR SUJEITO A IMPOSTO
     *         5X, A11,                  ! VALOR DO IMPOSTO
     *         5X, A11)                  ! VALOR LÍQUIDO DO PRÉMIO

C
111     FORMAT(A83,                      ! MAX CLASSE PREMIO
     *         A1,                       ! WINNING SERIES
     *         6X, A5, '-', A2, '-', A2, ! TICKET-SER-FRAC
     *        13X,                       ! AGENTE (ON-LINE)
     *         2X, I8,                   ! QUANTIDADE
     *         2X, I8,                   ! CLASSE DE PRÉMIO
     *         5X, A11,                  ! VALOR BRUTO DA CLASSE DE PRÉMIO
     *         5X, A11,                  ! VALOR BRUTO DO PRÉMIO
     *         5X, A11,                  ! VALOR SUJEITO A IMPOSTO
     *         5X, A11,                  ! VALOR DO IMPOSTO
     *         5X, A11)                  ! VALOR LÍQUIDO DO PRÉMIO

C
120     FORMAT(A83,                      ! MAX CLASSE PREMIO
     *         A1,                       ! WINNING SERIES
     *         6X, A5, '-', A2, '-', A2, ! TICKET-SER-FRAC
     *        13X,                       ! AGENTE
     *         2X, I8,                   ! QUANTIDADE (CLASSES PREMIO)
     *         7X, 'TOT'                 ! CLASSE DE PRÉMIO
     *        16X,                       ! VALOR BRUTO DA CLASSE DE PRÉMIO
     *         5X, A11,                  ! VALOR TOTAL BRUTO DO PRÉMIO
     *         5X, A11,                  ! VALOR TOTAL SUJEITO A IMPOSTO
     *         5X, A11,                  ! VALOR TOTAL DO IMPOSTO
     *         5X, A11)                  ! VALOR TOTAL LÍQUIDO DO PRÉMIO

C110     FORMAT(A83,                      ! MAX CLASSE PREMIO
C     *         6X, A5, '-', A2, '-', A2, ! TICKET-SER-FRAC
C     *         2X, I8,                   ! QUANTIDADE
C     *         5X, I8,                   ! CLASSE DE PRÉMIO
C     *         7X, A11,                  ! VALOR BRUTO DA CLASSE DE PRÉMIO
C     *         7X, A11,                  ! VALOR BRUTO DO PRÉMIO
C     *         7X, A11,                  ! VALOR SUJEITO A IMPOSTO
C     *         7X, A11,                  ! VALOR DO IMPOSTO
C     *         7X, A11)                  ! VALOR LÍQUIDO DO PRÉMIO
CC
C120     FORMAT(A83,                      ! MAX CLASSE PREMIO
C     *         6X, A5, '-', A2, '-', A2, ! TICKET-SER-FRAC
C     *         2X, I8,                   ! QUANTIDADE (CLASSES PREMIO)
C     *        10X, 'TOT'                 ! CLASSE DE PRÉMIO
C     *        18X,                       ! VALOR BRUTO DA CLASSE DE PRÉMIO
C     *         7X, A11,                  ! VALOR BRUTO DO PRÉMIO
C     *         7X, A11,                  ! VALOR SUJEITO A IMPOSTO
C     *         7X, A11,                  ! VALOR DO IMPOSTO
C     *         7X, A11)                  ! VALOR LÍQUIDO DO PRÉMIO
CC
        RETURN
C
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINTPAS_REPTXDPRZ_FOOTER (LUN,TOTAMT,TOTNETAMT,
C                                             TOTTAXBLAMT)
C
C       THIS SUBROUTINE PRINTS TOTAL NET PRIZES INTO REPORT FILE
C       FOR PASSIVE GAMES
C
C       INPUTS:
C        LUN         LOGICAL UNIT
C        TOTAMT      TOTAL PRIZE AMOUNT
C        TOTNETAMT   TOTAL PRIZE NET AMOUNT
C        TOTTAXBLAMT TOTAL TAXABLE AMOUNT
C        TOT_NR_PRZ_IS
C                    TOTAL NR PRIZES SUBJECT TO TAX
C
C       OUTPUTS:
C       *NONE*
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINTPAS_REPTXDPRZ_FOOTER (LUN,TOTAMT,TOTNETAMT,
     *                                        TOTTAXBLAMT,
     *                                        TOT_NR_PRZ_IS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER*4 LUN
        INTEGER*4 TOTAMT, TOTNETAMT, TOTTAXBLAMT, TOT_NR_PRZ_IS
C
        WRITE(LUN,100) CMONY(TOTAMT,11,VALUNIT),           ! VALOR TOTAL BRUTO DOS PRÉMIOS
     *                 CMONY(TOTTAXBLAMT,11,VALUNIT),      ! VALOR TOTAL SUJEITO A IMPOSTO
     *                 CMONY(TOTAMT-TOTNETAMT,11,VALUNIT), ! VALOR TOTAL DO IMPOSTO
     *                 CMONY(TOTNETAMT,11,VALUNIT),        ! VALOR TOTAL LÍQUIDO DOS PRÉMIOS
     *                 TOT_NR_PRZ_IS                       ! NR TOTAL PRÉMIOS SUJEITO A IMPOSTO
C
C=======================================================================
C       FORMAT STATEMENTS
C=======================================================================
C
100     FORMAT( 1X, 130('-'),/,/ ! SEPARADOR
     *          5X, 'TOTAIS',    ! TOTAIS
     *         60X, A11,         ! VALOR TOTAL BRUTO DOS PRÉMIOS
     *          5X, A11,         ! VALOR TOTAL SUJEITO A IMPOSTO
     *          5X, A11,         ! VALOR TOTAL DO IMPOSTO
     *          5X, A11,/,/,     ! VALOR TOTAL LÍQUIDO DOS PRÉMIOS
     *          5X, 'TOTAL DE PREMIOS COM IMPOSTO DE SELO: ', I0, /)! NR TOTAL PRÉMIOS SUJEITOS A IMPOSTO
C100     FORMAT( 1X, 130('-'),/,/ ! SEPARADOR
C     *          5X, 'TOTAIS',    ! TOTAIS
C     *         54X, A11,         ! VALOR TOTAL BRUTO DOS PRÉMIOS
C     *          7X, A11,         ! VALOR TOTAL SUJEITO A IMPOSTO
C     *          7X, A11,         ! VALOR TOTAL DO IMPOSTO
C     *          7X, A11,/,/,     ! VALOR TOTAL LÍQUIDO DOS PRÉMIOS
C     *          5X, 'TOTAL DE PREMIOS COM IMPOSTO DE SELO: ', I0, /)! NR TOTAL PRÉMIOS SUJEITOS A IMPOSTO

C
        RETURN
C
        END
C----|--!--------------------------------------------------------------
C V03   ! Adding new agent field - end
C----|--!--------------------------------------------------------------
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINTPAS_SAPTXDPRZ_HEADER (LUN,GNUM,DRDAT,DRWNBR,DRWYEAR)
C
C       THIS SUBROUTINE PRINTS THE HEADER OF SAP FILE FOR PASSIVE GAMES
C
C       INPUTS:
C        LUN         LOGICAL UNIT
C        GNUM        GAME NUMBER
C        DRDAT       DRAW DATE
C        DRWNBR      DRAW NUMBER
C        DRWYEAR     DRAW YEAR
C
C       OUTPUTS:
C       *NONE*
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINTPAS_SAPTXDPRZ_HEADER (LUN,GNUM,DRDAT,DRWNBR,DRWYEAR)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4 LUN, K
        INTEGER*4 GNUM, DRWNBR, DRWYEAR
        INTEGER*2 DRDAT(LDATE_LEN) ! DRAWING DATE
        CHARACTER*10  C10DRWDT
        CHARACTER     CDRWDT10(10)
        EQUIVALENCE   (C10DRWDT,CDRWDT10)
C
        WRITE(C10DRWDT,'(5A2)') (DRDAT(K),K=9,13)
        WRITE(LUN,10) 'IS1',
     *                CDRWDT10(7:10),CDRWDT10(4:5),CDRWDT10(1:2),
     *                GNUM,DRWNBR,DRWYEAR
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
10      FORMAT(A3,4A1,2A1,2A1,I2.2,I3.3,I4.4,51('0'))
C
        RETURN
C
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINTPAS_SAPTXDPRZ_BODY (LUN,DRDAT,DRWNBR,DRWYEAR,
C                                           TCKT,TOTAMT,NETAMT)
C
C       THIS SUBROUTINE PRINTS THE BODY RECORDS OF SAP FILE FOR PASSIVE
C       GAMES
C
C       INPUTS:
C        LUN         LOGICAL UNIT
C        DRDAT       DRAW DATE
C        DRWNBR      DRAW NUMBER
C        DRWYEAR     DRAW YEAR
C        TCKT        TICKET
C        TOTAMT      TOTAL PRIZE AMOUNT
C        NETAMT      NET AMOUNT
C
C       OUTPUTS:
C       *NONE*
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINTPAS_SAPTXDPRZ_BODY (LUN,DRDAT,DRWNBR,DRWYEAR,
     *                                   TCKT,TOTAMT,NETAMT)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4 LUN, K
        INTEGER*4 GNUM, DRWNBR, DRWYEAR, TOTAMT, NETAMT
        CHARACTER*(*) TCKT
        INTEGER*2 DRDAT(LDATE_LEN) ! DRAWING DATE
        CHARACTER*10  C10DRWDT
        CHARACTER     CDRWDT10(10)
        EQUIVALENCE   (C10DRWDT,CDRWDT10)
C
        WRITE(C10DRWDT,'(5A2)') (DRDAT(K),K=9,13)
        WRITE(LUN,10) 'IS2',
     *                CDRWDT10(7:10),CDRWDT10(4:5),CDRWDT10(1:2),
     *                DRWNBR,
     *                DRWYEAR,
     *                '00000',
     *                TCKT,
     *                TOTAMT,
     *                TOTAMT-NETAMT,
     *                NETAMT
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
10      FORMAT(A3,4A1,2A1,2A1,I3.3,I4.4,A,A<LEN(TCKT)>,I13.13,I13.13,I13.13)
C
        RETURN
C
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINTPAS_SAPTXDPRZ_FOOTER (LUN,TOTREC)
C
C       THIS SUBROUTINE PRINTS THE FOOTER RECORD OF SAP FILE FOR
C       PASSIVE GAMES
C
C       INPUTS:
C        LUN         LOGICAL UNIT
C        TOTREC      TOTAL NUMBER OF RECORDS (INCLUDING THE HEADER AND THE FOOTER)
C
C       OUTPUTS:
C       *NONE*
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINTPAS_SAPTXDPRZ_FOOTER (LUN,TOTREC)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4 LUN
        INTEGER*4 TOTREC
C
        WRITE(LUN,10) 'IS9', TOTREC
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
10      FORMAT(A3,I6.6,62('0'))
C
        RETURN
C
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PASTAX(GNUM,GSHDSC,GAMLNAM,ST)
C
C       THIS SUBROUTINE TAXES THE PRIZES OF LOTARIA CLASSICA AND
C       LOTARIA POPULAR
C
C       INPUTS:
C        GNUM        GAME NUMBER
C        GSHDSC      GAME SHORT DESCRIPTION
C        GAMLNAM     GAME LONG NAME
C
C       OUTPUTS:
C        ST
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PASTAX(GNUM,GSHDSC,GAMLNAM,ST)
        IMPLICIT NONE
C
        INCLUDE '(LIB$ROUTINES)'
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:PRMHSH.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:VALPASFIL.DEF'
        INCLUDE 'INCLIB:TAXCONFIG.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:DPAREC.DEF'
        INCLUDE 'INCLIB:PASIOSUBS.DEF'
C
        CHARACTER*4 GSHDSC
        CHARACTER*16 GAMLNAM
C
        CHARACTER*6 EEAAAA, EXTRACAOANO
        INTEGER*4   EXTRACAO, ANO, SZ, ST
        INTEGER*4   YESNO
        LOGICAL     FINREP /.FALSE./      ! FINAL REPORTING?
        LOGICAL     ISTHERE
        INTEGER*4   CTIM(2), CDAT(8), FDB(7)
        INTEGER*4   INDEMIS, EMIOFF, IPAS
        INTEGER*4   DRWN, GETDRW, GNUM
        INTEGER*2   DATEBUF(12)
C
        INTEGER*4 TUBSIZ
        PARAMETER (TUBSIZ=I4BUCSIZ*7)
        INTEGER*4 VPFBUF(TUBSIZ)
C
        INTEGER*4 BASAMT, RFNAMT, TAXPER
C
        INTEGER*4 REP_LUN
        INTEGER*4 SAP_LUN
        INTEGER*4 VPF_LUN
C----|--!--------------------------------------------------------------
C V03   ! New ordering - start
C----|--!--------------------------------------------------------------
        INTEGER*4 REP_TMP_LUN
        INTEGER*4 REP_SRT_LUN
        INTEGER*4 TOT_NR_PRZ_IS
C----|--!--------------------------------------------------------------
C V03   ! New ordering - end
C----|--!--------------------------------------------------------------
C
        CHARACTER*43 REPFILNAM ! REPORT FILE NAME
        CHARACTER*25 SAPFILNAM ! INTERFACE FILE FILE NAME FOR SAP
        CHARACTER*18 VPFFILNAM ! VPF FILE NAME
C----|--!--------------------------------------------------------------
C V03   ! New ordering - start
C----|--!--------------------------------------------------------------
         CHARACTER*43 REP_TMP_FILNAM ! REPORT TEMP FILE NAME
         CHARACTER*43 REP_SRT_FILNAM ! REPORT SORTED TEMP FILE NAME
         CHARACTER*256 ASCII_REC
C----|--!--------------------------------------------------------------
C V03   ! New ordering - end
C----|--!--------------------------------------------------------------
C
        CHARACTER*20 VPF_CFILNAM   !
        INTEGER*4    VPF_IFILNAM(5)
        EQUIVALENCE (VPF_IFILNAM, VPF_CFILNAM)
C
        INTEGER*4 MAXDIV
        PARAMETER(MAXDIV = 40)
        INTEGER*4 NUMDIVS
        INTEGER*8 GAMSHV(MAXDIV)
        INTEGER*4 NUMEXDIVS        ! NOT WINNING SERIES TOTAL DIVISIONS (POPULAR ONLY)
        INTEGER*8 GAMEXSHV(MAXDIV) ! NOT WINNING SERIES SHARE VALUES (POPULAR ONLY)
        INTEGER*4 POPWSER          ! WINNING SERIE (POPULAR ONLY)
        INTEGER*4 I
        CHARACTER*50 C50DIV
        INTEGER*4 TOTSAPREC
        INTEGER*4 TOTREADVPF, TOTVALREC, TOTINVREC, TOTNOTAX, TOTTAX
        INTEGER*2 DRDAT(LDATE_LEN) ! DRAWING DATE
C
        INTEGER*4 TOTDIVAMT(MAXDIV), NETDIVAMT(MAXDIV), TOTDIVPRZ(MAXDIV)
        INTEGER*4 PRZAMT, NETPRZAMT, TAXBLAMT
        INTEGER*4 TOTPRZAMT, TOTNETPRZAMT, TOTTAXBLAMT
        INTEGER*4 PAS_ROUND_VALUE
        INTEGER*4 DRWDT, NOFFRAC
        CHARACTER*9 TCKT /'         '/
        CHARACTER*256 LIBCMD
C
        LOGICAL VALDRW /.FALSE./
        LOGICAL HASTAX /.FALSE./
C----|--!--------------------------------------------------------------
C V03   ! New ordering - start
C----|--!--------------------------------------------------------------
        LOGICAL IS_WIN_SER /.FALSE./
C----|--!--------------------------------------------------------------
C V03   ! New ordering - end
C----|--!--------------------------------------------------------------
C
        RECORD /STPASFDB/  PASFDB
        RECORD /STPASREC/  PASREC
C
        CHARACTER*11 CKEY
C
C=======================================================================
C       CHOOSE DRAW
C=======================================================================
C
        CALL WIMG(5,'DESEJA PROCESSAR QUAL EXTRACAO (EEAAAA)? ')
        READ(5,901) EXTRACAOANO
        TYPE*, IAM(), EXTRACAOANO
901     FORMAT(A6)
C
        IF (EXTRACAOANO .EQ. 'e' .OR. EXTRACAOANO .EQ. 'E' .OR.
     *      TRIM(EXTRACAOANO) .EQ. '') RETURN
C
        ANO      = MOD(CTOI(EXTRACAOANO,SZ) , 10000)
        EXTRACAO = INT(CTOI(EXTRACAOANO,SZ) / 10000)
C
        IF (ANO .LT. 2000 .OR. EXTRACAO .GT. 53 .OR. EXTRACAO .LE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Ano/Extracao invalido'
          ST = 0
          RETURN
        ENDIF
C
        WRITE(EEAAAA,FMT='(I2.2,I4.4)') EXTRACAO,ANO
C
        IF (ANO .LE. 2012) THEN
          CALL PRMYESNO('A EXTRACAO inserida e anterior a 012013. Deseja continuar [Y/N]? ', YESNO)
          IF (YESNO .NE. 1) THEN
            ST = 0
            RETURN
          ENDIF
        ENDIF
C
        CALL PRMYESNO('Confirma a EXTRACAO '//EEAAAA//' [Y/N]? ', YESNO)
        IF (YESNO .NE. 1) THEN
          ST = 0
          RETURN
        ENDIF
C
        CALL PRMYESNO('Deseja gerar apenas o relatorio provisorio [Y/N]? ', YESNO)
        IF (YESNO .EQ. 1) THEN
          FINREP = .FALSE. ! REPORT IS FINAL
        ELSEIF (YESNO .EQ. 2) THEN
          FINREP = .TRUE. ! REPORT IS PROVISIONAL
        ELSE
          ST = 0
          RETURN
        ENDIF
C
C=======================================================================
C       CHECK IF DRAW NUMBER IS VALID
C=======================================================================
C
        DRWN = GETDRW(ANO,EXTRACAO,GNUM) ! GET DRAW NUMBER
        IF (DRWN .LE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - EXTRACAO invalida'
          ST = -1
          RETURN
        ENDIF
C
C=======================================================================
C       CHECK IF EMISSION IS IN MEMORY
C=======================================================================
C
        IF (GNUM .EQ. 8) THEN
          IPAS = 1 ! CLASSICA
        ELSEIF(GNUM .EQ. 9) THEN
          IPAS = 2 ! POPULAR
        ENDIF
C
        INDEMIS = -1
        DO EMIOFF=1, PAGEMI
          IF (PASEMIS(EMIOFF,IPAS) .EQ. DRWN) THEN
            INDEMIS = EMIOFF
            EXIT
          ENDIF
        ENDDO
C
        IF (INDEMIS .LE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - EXTRACAO nao esta em memoria'
          ST = -1
          RETURN
        ENDIF
C
C=======================================================================
C       CHECK IF RESULTS ARE FINAL
C=======================================================================
C
        IF (PASSTS(INDEMIS,IPAS) .NE. GFINAL) THEN
          TYPE*, IAM(),'       '
          TYPE*, IAM(),'TAXMNG - Premios nao apurados para a EXTRACAO ', EEAAAA
          TYPE*, IAM(),'       '
          ST = -1
          RETURN
        ENDIF
C
C=======================================================================
C       NAME THE FOLLOWING FILES:
C         REPORT FILE
C         SAP INTERFACE FILE (ONLY IF REPORTING MODE IS FINAL)
C=======================================================================
C
        CALL ICLOCK(1,CTIM)
        CALL GDATE(CDAT(2),CDAT(3),CDAT(1))
        IF(CDAT(1) .LT. 77) THEN
          CDAT(1) = CDAT(1) + 2000
        ELSE
          CDAT(1) = CDAT(1) + 1900
        ENDIF
C
        IF (FINREP) THEN
          WRITE (REPFILNAM, FMT='(A16,I2.2,A1,A6,A1,I4.4,I2.2,I2.2,A4)')
     *     'FILE:PREMIOS_IS_',GNUM,'_',EEAAAA,'_',CDAT(1),CDAT(2),CDAT(3),'.REP'
C----|--!--------------------------------------------------------------
C V03   ! New ordering - start
C----|--!--------------------------------------------------------------
           WRITE (REP_TMP_FILNAM, FMT='(A16,I2.2,A1,A6,A1,I4.4,I2.2,I2.2,A4)')
     *      'FILE:PREMIOS_IS_',GNUM,'_',EEAAAA,'_',CDAT(1),CDAT(2),CDAT(3),'.TMP'
           WRITE (REP_SRT_FILNAM, FMT='(A16,I2.2,A1,A6,A1,I4.4,I2.2,I2.2,A4)')
     *      'FILE:PREMIOS_IS_',GNUM,'_',EEAAAA,'_',CDAT(1),CDAT(2),CDAT(3),'.SRT'
C----|--!--------------------------------------------------------------
C V03   ! New ordering - end
C----|--!--------------------------------------------------------------
     
        ELSE
          WRITE (REPFILNAM, FMT='(A21,I2.2,A1,A6,A1,I4.4,I2.2,I2.2,A4)')
     *     'FILE:PREMIOS_PREV_IS_',GNUM,'_',EEAAAA,'_',CDAT(1),CDAT(2),CDAT(3),'.REP'
C----|--!--------------------------------------------------------------
C V03   ! New ordering - start
C----|--!--------------------------------------------------------------
          WRITE (REP_TMP_FILNAM, FMT='(A21,I2.2,A1,A6,A1,I4.4,I2.2,I2.2,A4)')
     *     'FILE:PREMIOS_PREV_IS_',GNUM,'_',EEAAAA,'_',CDAT(1),CDAT(2),CDAT(3),'.TMP'
          WRITE (REP_SRT_FILNAM, FMT='(A21,I2.2,A1,A6,A1,I4.4,I2.2,I2.2,A4)')
     *     'FILE:PREMIOS_PREV_IS_',GNUM,'_',EEAAAA,'_',CDAT(1),CDAT(2),CDAT(3),'.SRT'
C----|--!--------------------------------------------------------------
C V03   ! New ordering - end
C----|--!--------------------------------------------------------------
        ENDIF
C
        IF (FINREP) THEN
          WRITE (SAPFILNAM, FMT='(A8,A4,A1,I4.4,I2.2,I2.2,A4)')
     *       'FILE:IS_',GSHDSC,'_',CDAT(1),CDAT(2),CDAT(3),'.ASC'
        ENDIF
C
C=======================================================================
C       CHECK IF REPORTING FILES ALREADY EXISTS IN THE SYSTEM. IF SO,
C       EXIT THE PROGRAM IMMEDIATELY.
C=======================================================================
C
        IF (FINREP) THEN
          INQUIRE(FILE=REPFILNAM, EXIST=ISTHERE)
          IF (ISTHERE) THEN
            INQUIRE(FILE=SAPFILNAM, EXIST=ISTHERE)
            IF (ISTHERE) THEN
              TYPE*, IAM(),'       '
              TYPE*, IAM(),'TAXMNG - Os ficheiros seguintes ja existem no sistema:'
              TYPE*, IAM(),'       '
              TYPE*, IAM(),'         '//TRIM(REPFILNAM)
              TYPE*, IAM(),'         '//TRIM(SAPFILNAM)
              TYPE*, IAM(),'       '
              TYPE*, IAM(),'Para continuar, os ficheiros deverao ser removidos'
              TYPE*, IAM(),'       '
              CALL GSTOP(GEXIT_FATAL)
            ELSE
              TYPE*, IAM(),'       '
              TYPE*, IAM(),'TAXMNG - O ficheiro seguinte ja existe no sistema:'
              TYPE*, IAM(),'       '
              TYPE*, IAM(),'         '//TRIM(REPFILNAM)
              TYPE*, IAM(),'       '
              TYPE*, IAM(),'Para continuar, os ficheiros deverao ser removidos'
              TYPE*, IAM(),'       '
              CALL GSTOP(GEXIT_FATAL)
            ENDIF
          ENDIF
          INQUIRE(FILE=SAPFILNAM, EXIST=ISTHERE)
          IF (ISTHERE) THEN
            TYPE*, IAM(),'       '
            TYPE*, IAM(),'TAXMNG - O ficheiro seguinte ja existe no sistema:'
            TYPE*, IAM(),'       '
            TYPE*, IAM(),'         '//TRIM(SAPFILNAM)
            TYPE*, IAM(),'       '
            TYPE*, IAM(),'Para continuar, o ficheiro devera ser removido'
            TYPE*, IAM(),'       '
            CALL GSTOP(GEXIT_FATAL)
          ENDIF
        ELSE
          INQUIRE(FILE=REPFILNAM, EXIST=ISTHERE)
          IF (ISTHERE) THEN
            TYPE*, IAM(),'       '
            TYPE*, IAM(),'TAXMNG - O ficheiro seguinte ja existe no sistema:'
            TYPE*, IAM(),'       '
            TYPE*, IAM(),'         '//TRIM(REPFILNAM)
            TYPE*, IAM(),'       '
            TYPE*, IAM(),'Para continuar, o ficheiro devera ser removido'
            TYPE*, IAM(),'       '
            CALL GSTOP(GEXIT_FATAL)
          ENDIF
        ENDIF
C----|--!--------------------------------------------------------------
C V03   ! New ordering - start
C----|--!--------------------------------------------------------------
        INQUIRE(FILE=REP_TMP_FILNAM, EXIST=ISTHERE)
        IF (ISTHERE) THEN
            CALL DFILX(REP_TMP_FILNAM,0,0,ST)
        ENDIF
        INQUIRE(FILE=REP_SRT_FILNAM, EXIST=ISTHERE)
        IF (ISTHERE) THEN
            CALL DFILX(REP_SRT_FILNAM,0,0,ST)
        ENDIF
C----|--!--------------------------------------------------------------
C V03   ! New ordering - end
C----|--!--------------------------------------------------------------
C
C=======================================================================
C       CHECK IF DRAW RESULTS ARE FINAL
C=======================================================================
C
        IF (PASSTS(INDEMIS,IPAS) .NE. GFINAL) THEN
          TYPE*, IAM(),'       '
          TYPE*, IAM(),'TAXMNG - Premios nao apurados para a EXTRACAO ', EEAAAA
          TYPE*, IAM(),'       '
          ST = -1
          RETURN
        ENDIF
C
C=======================================================================
C       IF FILE TAXCONF.FIL DOESN'T EXIST, EXIT THE PROGRAM
C=======================================================================
C
        TYPE*, IAM(), '       '
        TYPE*, IAM(), 'A obter a configuracao do imposto de selo a aplicar...'
C
        INQUIRE(FILE='FILE:TAXCONF.FIL', EXIST=ISTHERE)
        IF (.NOT. ISTHERE) THEN
          TYPE*, IAM(),'       '
          TYPE*, IAM(),'TAXMNG - Nao foi encontrado o ficheiro TAXCONF.FIL'
          TYPE*, IAM(),'       '
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C=======================================================================
C       OPEN TAX CONFIG FILE AND LOAD CONFIGURATION
C=======================================================================
C
        CALL OPENX(1,'FILE:TAXCONF.FIL',4,0,0,ST)
        CALL IOINIT(FDB,1,TXCF_SEC*256)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(),'TAXMNG - Nao foi possivel abrir o ficheiro TAXCONF.FIL'
          TYPE*, IAM(), '       '
          CALL CLOSEFIL(FDB)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
        CALL READW(FDB,1,TXCF_REC,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(),'TAXMNG - Nao foi possivel ler o ficheiro TAXCONF.FIL'
          TYPE*, IAM(), '       '
          CALL CLOSEFIL(FDB)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
        CALL CLOSEFIL(FDB)
C
C=======================================================================
C       SET THE TAX PERCENTAGE, BASE AMOUNT AND REFUND AMOUNT
C=======================================================================
C
        TAXPER = TXCF_LNTAX
        BASAMT = TXCF_LNBSAMNT
        RFNAMT = TXCF_LNTAXRFN
C
        WRITE(5,'(1X,A,A,F11.2)') IAM(), ' Taxa de imposto:               ', DISPER(TAXPER * 10)
        WRITE(5,'(1X,A,A,A11)')   IAM(), ' Menor premio aplicavel:        ', CMONY(BASAMT,11,VALUNIT)
        WRITE(5,'(1X,A,A,A11)')   IAM(), ' Valor a amortizar no imposto:  ', CMONY(RFNAMT,11,VALUNIT)
C
        IF (BASAMT .LT. RFNAMT) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(),'TAXMNG - Menor premio aplicavel MENOR'
          TYPE*, IAM(),'         que valor a amortizar no imposto!'
          TYPE*, IAM(), '       '
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C=======================================================================
C       LOAD WINNING SERIE FROM MEMORY (POPULAR)
C=======================================================================
C
        IF (GNUM .EQ. 9) THEN
          POPWSER = PASWSER(INDEMIS,IPAS)
          IF (POPWSER .EQ. 0) THEN
            TYPE*, IAM(), '       '
            TYPE*, IAM(),'TAXMNG - Serie sorteada igual a zero!'
            TYPE*, IAM(), '       '
            CALL GSTOP(GEXIT_FATAL)
          ENDIF
        ENDIF
C
C=======================================================================
C       LOAD SHARE VALUES FOR THE CHOSEN GAME AND DRAW FROM DISK
C=======================================================================
C
        TYPE*, IAM(), '       '
        TYPE*, IAM(), 'A obter o valor das shares da EXTRACAO'//' '//EEAAAA
        TYPE*, IAM(), 'da '//TRIM(GAMLNAM)//'...'
        CALL GET_PASSHV(GNUM,DRWN,GAMSHV,GAMEXSHV,NUMDIVS,DRDAT,NOFFRAC)
C
        IF (GNUM .EQ. 8) THEN ! CLASSICA
          DO I=1,NUMDIVS
            WRITE(C50DIV,'(A,I2,A)') ' Divisao ',I,': '
            TYPE*, IAM(), TRIM(C50DIV), ' ', CMONY(PAS_ROUND_VALUE(GAMSHV(I)),13,VALUNIT)
          ENDDO
        ENDIF
C
        IF (GNUM .EQ. 9) THEN ! POPULAR
          WRITE(C50DIV,'(A,I0,A)') '             Serie Sorteada (', POPWSER, ')   Restantes Series'
          TYPE*, IAM(), TRIM(C50DIV)
          DO I=1,NUMDIVS
            WRITE(5,'(1X,A,A,I2,A,A13,A,A13)') IAM(), ' Divisao ',I,':      ',
     *         CMONY(PAS_ROUND_VALUE(GAMSHV(I)),13,VALUNIT),'      ',
     *         CMONY(PAS_ROUND_VALUE(GAMEXSHV(I)),13,VALUNIT)
          ENDDO
        ENDIF
C
C=======================================================================
C      FIND A FREE LUN TO USE FOR REPORT FILE
C=======================================================================
C
        CALL FIND_AVAILABLE_LUN (REP_LUN,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Erro a obter uma LUN para o ficheiro: '
          TYPE*, IAM(), '       '
          TYPE*, IAM(), '         '//TRIM(REPFILNAM(6:))
          TYPE*, IAM(), '       '
          CALL GSTOP (GEXIT_FATAL)
        ENDIF
C
C=======================================================================
C       OPEN FILE THE REPORT FILE
C=======================================================================
C
        CALL OPEN_FILASC (REPFILNAM,REP_LUN,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Erro a abrir o ficheiro: '
          TYPE*, IAM(), '       '
          TYPE*, IAM(), '         '//TRIM(REPFILNAM(6:))
          TYPE*, IAM(), '       '
          CALL USRCLOS1(REP_LUN)
          CALL GSTOP (GEXIT_FATAL)
        ENDIF
C
C=======================================================================
C      FIND A FREE LUN TO USE FOR SAP FILE (ONLY IF REPORTING MODE IS FINAL)
C=======================================================================
C
        IF (FINREP) THEN
          CALL FIND_AVAILABLE_LUN (SAP_LUN,ST)
          IF (ST .NE. 0) THEN
            TYPE*, IAM(), '       '
            TYPE*, IAM(), 'TAXMNG - Erro a obter uma LUN para o ficheiro: '
            TYPE*, IAM(), '       '
            TYPE*, IAM(), '         '//TRIM(SAPFILNAM(6:))
            TYPE*, IAM(), '       '
            ! CLOSE AND DELETE CREATED FILES SO FAR
            CALL USRCLOS1(REP_LUN)
            CALL DFILX(REPFILNAM,0,0,ST)
            CALL GSTOP (GEXIT_FATAL)
          ENDIF
C
C=======================================================================
C       OPEN THE SAP FILE (ONLY IF REPORTING MODE IS FINAL)
C=======================================================================
C
          CALL OPEN_FILASC (SAPFILNAM,SAP_LUN,ST)
          IF (ST .NE. 0) THEN
            TYPE*, IAM(), '       '
            TYPE*, IAM(), 'TAXMNG - Erro a abrir o ficheiro: '
            TYPE*, IAM(), '       '
            TYPE*, IAM(), '         '//TRIM(SAPFILNAM(6:))
            TYPE*, IAM(), '       '
            ! CLOSE AND DELETE CREATED FILES SO FAR
            CALL USRCLOS1(REP_LUN)
            CALL USRCLOS1(SAP_LUN)
            CALL DFILX(REPFILNAM,0,0,ST)
            CALL DFILX(SAPFILNAM,0,0,ST)
            CALL GSTOP (GEXIT_FATAL)
          ENDIF
        ENDIF
C----|--!--------------------------------------------------------------
C V03   ! New ordering - start
C----|--!--------------------------------------------------------------
C
C=======================================================================
C      FIND A FREE LUN TO USE FOR REPORT FILE
C=======================================================================
C
        CALL FIND_AVAILABLE_LUN (REP_TMP_LUN,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Erro a obter uma LUN para o ficheiro: '
          TYPE*, IAM(), '       '
          TYPE*, IAM(), '         '//TRIM(REP_TMP_FILNAM(6:))
          TYPE*, IAM(), '       '
          CALL GSTOP (GEXIT_FATAL)
        ENDIF
C
C=======================================================================
C       OPEN FILE THE REPORT FILE
C=======================================================================
C
        CALL OPEN_FILASC (REP_TMP_FILNAM,REP_TMP_LUN,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Erro a abrir o ficheiro: '
          TYPE*, IAM(), '       '
          TYPE*, IAM(), '         '//TRIM(REP_TMP_FILNAM(6:))
          TYPE*, IAM(), '       '
          CALL USRCLOS1(REP_TMP_LUN)
          CALL USRCLOS1(REP_LUN)
          CALL USRCLOS1(SAP_LUN)
          CALL DFILX(REPFILNAM,0,0,ST)
          CALL DFILX(SAPFILNAM,0,0,ST)
          CALL DFILX(REP_TMP_FILNAM,0,0,ST)
          CALL GSTOP (GEXIT_FATAL)
        ENDIF
C----|--!--------------------------------------------------------------
C V03   ! New ordering - end
C----|--!--------------------------------------------------------------
C
C=======================================================================
C       WRITE HEADER INTO EACH FILE
C=======================================================================
C
        CALL PRINTPAS_REPTXDPRZ_HEADER (REP_LUN,GNUM,EXTRACAO,ANO,
     *                                  CDAT,FINREP)
C
        IF (FINREP) THEN
          TOTSAPREC = 0
          CALL PRINTPAS_SAPTXDPRZ_HEADER (SAP_LUN,GNUM,DRDAT,
     *                                    EXTRACAO,ANO)
          TOTSAPREC = TOTSAPREC + 1
        ENDIF
C
C=======================================================================
C       OPEN VALIDATION FILE (VPF)
C=======================================================================
C
        WRITE(VPF_CFILNAM,FMT='(A8,I2.2,I4.4,A4)') 'VALX:VPF',IPAS,DRWN,'.FIL'
        CALL FIND_AVAILABLE_LUN(VPF_LUN,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Erro a obter uma LUN para o ficheiro: '
          TYPE*, IAM(), '       '
          TYPE*, IAM(), '         '//TRIM(VPF_CFILNAM)
          TYPE*, IAM(), '       '
          CALL FILERR(VPF_IFILNAM,0,ST,0)
          GOTO 2000
        ENDIF
C
        CALL IOPEN(VPF_IFILNAM,VPF_LUN,VPFLEN*2,VFSCDC,VFSSER*2-1,ST)
        IF(ST .NE. 0) THEN
          CALL FILERR(VPF_IFILNAM,1,ST,0)
          GOTO 2000
        ENDIF
        CALL FASTSET(0, V4BUF_PAS, VPFLEN * VPFMAXREC)
        CALL ITUBSIZE(VPF_LUN,TUBSIZ)
C
C=======================================================================
C       OPEN TPF FILE
C=======================================================================
C
C        IF (FINREP) THEN
          CALL PASIO_INIT(PASFDB,IPAS,PASEMIS(INDEMIS,IPAS),
     *                    PASNUMTCK(INDEMIS,IPAS)-1,PASNUMSER(INDEMIS,IPAS),
     *                    PASNOFFRA(INDEMIS,IPAS),CPASTPFFIL(INDEMIS,IPAS))
          CALL PASIO_OPENRO(PASFDB)
          IF(PASFDB.ERR .NE. IOE_NOERR) THEN
            TYPE*, IAM(), '       '
            TYPE*, IAM(), 'TAXMNG - Erro: ', PASFDB.ERR, ' a abrir o ficheiro: ', PASFDB.FILNAM
            TYPE*, IAM(), '       '
            TYPE*, IAM(), 'DUMP:'
            CALL PASIO_DUMP(PASFDB)
            GOTO 2000
          ENDIF
C        ENDIF
C
C=======================================================================
C       START READING SEQUENTIALLY VPF FILE
C=======================================================================
C
        TOTPRZAMT = 0
        TOTNETPRZAMT = 0
        TOTTAXBLAMT = 0
        TOTREADVPF = 0
        TOTVALREC = 0
        TOTINVREC = 0
        TOTNOTAX = 0
        TOTTAX = 0
        TAXBLAMT = 0
        TOT_NR_PRZ_IS = 0
300     CONTINUE
C
        CALL ISREAD(V4BUF_PAS,VPF_LUN,VPFBUF,ST)
        IF (ST .EQ. ERREND) GOTO 1000
C
        TOTREADVPF = TOTREADVPF + 1
        IF (ST .NE. 0) THEN
          CALL FILERR(VPF_IFILNAM,2,ST,0)
          CALL GPAUSE
          GOTO 300
        ENDIF
C
        CALL LOGPAS(VALREC,V4BUF_PAS)
C
C=======================================================================
C       CHECK IF VALREC BELONGS TO THE GAME WE WANT TO PROCESS
C=======================================================================
C
        IF (VALREC(VGAM) .NE. GNUM) THEN
         TOTINVREC = TOTINVREC + 1
         CALL GPAUSE
         GOTO 300
        ENDIF
C
C=======================================================================
C       CHECK IF VALIDATION RECORD BELONGS TO THE CHOSEN DRAW
C=======================================================================
C
        CALL DLOGPAS(VALREC,VDETAIL)
        VALDRW = .FALSE.
        DO I=1, VALREC(VPZOFF)
          IF (VDETAIL(VDRW,I) .EQ. DRWN) THEN
            VALDRW = .TRUE.
            EXIT
          ENDIF
        ENDDO
C
        IF (.NOT. VALDRW) THEN
          TOTINVREC = TOTINVREC + 1
          CALL GPAUSE
          GOTO 300
        ENDIF
        TOTVALREC = TOTVALREC + 1
C
C=======================================================================
C       CHECK STATUS OF VALREC: IF VDEL OR VCXL THEN IGNORE IT AND READ
C       NEXT RECORD FROM VPF
C=======================================================================
C
        IF (VALREC(VSTAT) .EQ. VDEL .OR. VALREC(VSTAT) .EQ. VCXL) THEN
          TOTNOTAX = TOTNOTAX + 1
          GOTO 300
        ENDIF
C
C=======================================================================
C       CHECK IF VALIDATION RECORD SHOULD BE SUBJECT TO TAX
C=======================================================================
C
        IF (VALREC(VPAMT) .LE. BASAMT) THEN
          TOTNOTAX = TOTNOTAX + 1
          GOTO 300
        ENDIF
C
C=======================================================================
C       CHECK STATUS OF PASREC FROM TPF FILE IF FINAL REPORTING:
C       IF STATUS IS PBILKOF THEN IGNORE IT AND READ NEXT RECORD FROM VPF
C=======================================================================
C
C        IF (FINREP) THEN
          CALL PASIO_READ(PASFDB,VALREC(VTCKT),VALREC(VSERN),
     *                    VALREC(VPFRAC),PASREC)
          IF (PASFDB.ERR .NE. IOE_NOERR) THEN
            TYPE*, IAM(), '       '
            WRITE(CKEY,9000) VALREC(VTCKT),VALREC(VSERN),VALREC(VPFRAC)
            TYPE*, IAM(), 'TAXMNG - Erro: ', PASFDB.ERR, ' a ler o registo: ', CKEY
            TYPE*, IAM(), '       '
            TYPE*, IAM(), 'DUMP:'
            CALL PASIO_DUMP(PASFDB)
            TYPE*, IAM(), '       '
            CALL GPAUSE
            GOTO 300
          ENDIF
          IF (FINREP) THEN
            IF (PASREC.STAT .EQ. PBILKOF) THEN ! Returned offline after draw and winner
              TOTNOTAX = TOTNOTAX + 1
              GOTO 300
            ENDIF
          ENDIF
C        ENDIF
C
C=======================================================================
C       CHECK IF PRIZE HAS TO BE TAXED AND IF SO CALCULATE TAX VALUE
C=======================================================================
C
        HASTAX = .FALSE.
        CALL CHECKPASTAX (VALREC,BASAMT,RFNAMT,TAXPER,GAMSHV,DRWN,
     *                    NOFFRAC,GAMEXSHV,POPWSER,
     *                    TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,PRZAMT,
     *                    NETPRZAMT,HASTAX)
C----|--!--------------------------------------------------------------
C V03   ! New ordering - start
C       ! Determining which is the winning series
C----|--!--------------------------------------------------------------
        IS_WIN_SER = (POPWSER .EQ. VALREC(VPFRAC))
C----|--!--------------------------------------------------------------
C V03   ! New ordering - end
C----|--!--------------------------------------------------------------
C
C=======================================================================
C       CHECK IF THE PRZAMT VALUE EQUALS VALREC(VPAMT) VALUE.
C       IF THEY DIFFER, IT MEANS THAT SOME PROBLEM OCCURRED
C       DURING THE CALCULATION OF NET PRIZES
C=======================================================================
C
        IF (PRZAMT .NE. VALREC(VPAMT)) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Erro no calculo do premio liquido!'
          TYPE*, IAM(), '       '
          TYPE*, IAM(), '         PRZAMT diferente de VALREC(VPAMT)'
          TYPE*, IAM(), '       '
          ! DEBUG
          TYPE*, IAM(), '         Input:'
          TYPE*, IAM(), '           VALREC(VPAMT)            = ',VALREC(VPAMT)
          TYPE*, IAM(), '           VALREC(VTCKT)            = ',VALREC(VTCKT)
          TYPE*, IAM(), '           VALREC(VSERN)            = ',VALREC(VSERN)
          TYPE*, IAM(), '           VALREC(VPFRAC)           = ',VALREC(VPFRAC)
          TYPE*, IAM(), '           BASAMT                   = ',BASAMT
          TYPE*, IAM(), '           RFNAMT                   = ',RFNAMT
          TYPE*, IAM(), '           TAXPER                   = ',TAXPER
          TYPE*, IAM(), '           DRWN                     = ',DRWN
          TYPE*, IAM(), '           NOFFRAC                  = ',NOFFRAC
          TYPE*, IAM(), '           POPWSER                  = ',POPWSER
          TYPE*, IAM(), '       '
          TYPE*, IAM(), '         Output:'
          DO I=1,MAXDIV
            IF (TOTDIVPRZ(I) .NE. 0) THEN
              TYPE*, IAM(), '           TOTDIVPRZ(',I,') = ',TOTDIVPRZ(I)
              TYPE*, IAM(), '           TOTDIVAMT(',I,') = ',TOTDIVAMT(I)
              TYPE*, IAM(), '           NETDIVAMT(',I,') = ',NETDIVAMT(I)
            ENDIF
          ENDDO
          TYPE*, IAM(), '           NETPRZAMT                = ',NETPRZAMT
          TYPE*, IAM(), '           PRZAMT                   = ',PRZAMT
          TYPE*, IAM(), '           HASTAX                   =           ',HASTAX
          TYPE*, IAM(), '       '
C
          CALL GPAUSE
          GOTO 300
        ENDIF
C
        IF (.NOT. HASTAX) THEN
          TOTNOTAX = TOTNOTAX + 1
          GOTO 300
        ENDIF
C
        IF (HASTAX) THEN
          TOTTAX = TOTTAX + 1
          TOTPRZAMT = TOTPRZAMT + PRZAMT
          TOTNETPRZAMT = TOTNETPRZAMT + NETPRZAMT
C
          IF (IPAS .EQ. 1) THEN ! CLASSICA
            WRITE(TCKT, '(I5.5,I2.2,I2.2)') VALREC(VTCKT),
     *                                      VALREC(VSERN),
     *                                      VALREC(VPFRAC)
          ELSEIF(IPAS .EQ. 2) THEN ! POPULAR
            WRITE(TCKT, '(I5.5,I2.2,I2.2)') VALREC(VTCKT),
     *                                      VALREC(VSERN),
     *                                      VALREC(VPFRAC)
          ENDIF
C
              
C----|--!--------------------------------------------------------------
C V03   ! Adding new agent field - start
C----|--!--------------------------------------------------------------
C          CALL PRINTPAS_REPTXDPRZ_BODY (REP_LUN,
C     *                               TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,
C     *                               TCKT,BASAMT,RFNAMT,
C     *                               AGTTAB(AGTNUM,VALREC(VSTER)),
C     *                               TAXBLAMT)
          ! VALREC(VPASTYP) is updated at WINPAS.FOR (see line 446 - if (pasrec.stat .gt. pbilnot))
          ! VALREC(VSTER) is not filled for passive games, so we had to use PASREC.AGT to get the terminal number
          IF (VALREC(VPASTYP) .EQ. VPASONL) THEN
              ! ON-LINE TICKET
              CALL PRINTPAS_REPTXDPRZ_BODY_TMP (REP_TMP_LUN,
     *                                   TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,
     *                                   TCKT,BASAMT,RFNAMT,
     *                                   AGTTAB(AGTNUM,PASREC.AGT),
     *                                   IS_WIN_SER,
     *                                   TAXBLAMT,TOT_NR_PRZ_IS)
          ELSE
              ! OFF-LINE TICKET
              CALL PRINTPAS_REPTXDPRZ_BODY_TMP (REP_TMP_LUN,
     *                                   TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,
     *                                   TCKT,BASAMT,RFNAMT,
     *                                   -1,
     *                                   IS_WIN_SER,
     *                                   TAXBLAMT,TOT_NR_PRZ_IS)
          ENDIF
C----|--!--------------------------------------------------------------
C V03   ! Adding new agent field - end
C----|--!--------------------------------------------------------------
C
          TOTTAXBLAMT = TOTTAXBLAMT + TAXBLAMT
C
          IF (FINREP) THEN
            CALL PRINTPAS_SAPTXDPRZ_BODY (SAP_LUN,DRDAT,EXTRACAO,
     *                                    ANO,TCKT,PRZAMT,NETPRZAMT)
            TOTSAPREC = TOTSAPREC + 1
          ENDIF
        ENDIF
C
        GOTO 300 ! READ NEXT RECORD FROM VPF

1000    CONTINUE
C
C=======================================================================
C       WRITE FOOTER INTO REPORT FILE AND SAP INTERFACE FILE
C=======================================================================
C
C        CALL PRINTPAS_REPTXDPRZ_FOOTER (REP_LUN,TOTPRZAMT,TOTNETPRZAMT,
C     *                                  TOTTAXBLAMT)
        IF (FINREP) THEN
          TOTSAPREC = TOTSAPREC + 1
          CALL PRINTPAS_SAPTXDPRZ_FOOTER (SAP_LUN,TOTSAPREC)
        ENDIF
C
C=======================================================================
C       PRINT PROCESS STATISTICS ON SCREEN
C=======================================================================
C
        TYPE*, IAM(), '       '
        TYPE*, IAM(), 'Bilhetes premiados da EXTRACAO ', EEAAAA,': ', TOTVALREC
        TYPE*, IAM(), ' Com imposto de selo:                  ', TOTTAX
        TYPE*, IAM(), ' Sem imposto de selo:                  ', TOTNOTAX
        TYPE*, IAM(), '       '

        IF ((TOTTAX + TOTNOTAX) .NE. TOTVALREC) THEN
          TYPE*, IAM(), 'TAXMNG - Total de bilhetes premiados difere da'
          TYPE*, IAM(), '         soma de bilhetes com e sem imposto!'
          TYPE*, IAM(), '       '
          CALL GPAUSE
        ENDIF
C
        WRITE(5, '(1X,A,A,A,A,I)'), IAM(), 'Registos lidos do ', TRIM(VPF_CFILNAM), ':  ', TOTREADVPF
        IF (TOTREADVPF .NE. TOTVALREC ) THEN
          WRITE(5, '(1X,A,A,I)'), IAM(), ' Validos:                              ', TOTVALREC
          WRITE(5, '(1X,A,A,I)'), IAM(), ' Invalidos:                            ', TOTINVREC
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Totais de registos lidos do ', TRIM(VPF_CFILNAM),' e'
          TYPE*, IAM(), '         registos validos e diferente!'
          TYPE*, IAM(), '       '
          CALL GPAUSE
        ENDIF
C
C=======================================================================
C       CLOSE FILES
C=======================================================================
C
C        CALL USRCLOS1(REP_LUN)
        CALL USRCLOS1(REP_TMP_LUN)
        IF (FINREP) CALL USRCLOS1(SAP_LUN)
        CALL ICLOSE(VPF_LUN,VPFBUF,ST)
        CALL PASIO_CLOSE(PASFDB)
C        IF (FINREP) CALL PASIO_CLOSE(PASFDB)

C----|--!--------------------------------------------------------------
C V03   ! New ordering - start
C----|--!--------------------------------------------------------------
        WRITE(LIBCMD,'(A)') 
     *         '$ SORT ' 
     *      // '/KEY=(POSITION:1,SIZE:101,CHARACTER)'   ! First key
     *      // '/KEY=(POSITION:125,SIZE:10,CHARACTER) ' ! Second key
     *      // TRIM(REP_TMP_FILNAM(6:)) 
     *      // ' '
     *      // TRIM(REP_SRT_FILNAM(6:))
C        WRITE(LIBCMD,'(A)') 
C     *         '$ SORT ' 
C     *      // '/KEY=(POSITION:1,SIZE:100,CHARACTER)'   ! First key
C     *      // '/KEY=(POSITION:111,SIZE:13,CHARACTER) ' ! Second key
C     *      // TRIM(REP_TMP_FILNAM(6:)) 
C     *      // ' '
C     *      // TRIM(REP_SRT_FILNAM(6:))
        CALL EXEC_SYS_CMD(LIBCMD,ST)
        
C----|--!--------------------------------------------------------------
C V03   ! Opening sorted file
C----|--!--------------------------------------------------------------
        CALL FIND_AVAILABLE_LUN (REP_SRT_LUN,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'TAXMNG - Erro a obter uma LUN para o ficheiro:'
          TYPE*, IAM(), '       '
          TYPE*, IAM(), '         '//TRIM(REP_SRT_FILNAM(6:))
          TYPE*, IAM(), '       '
          CALL GSTOP (GEXIT_FATAL)
        ENDIF
        CALL OPEN_FILE(REP_SRT_FILNAM, REP_SRT_LUN, ST)
        ST = 0
        DO WHILE (ST .EQ. 0)
            READ(UNIT = REP_SRT_LUN     !data record
     *          , IOSTAT = ST
     *          , FMT = '(256A)') ASCII_REC
            IF (ST .EQ. 0) THEN
                IF (ASCII_REC(125:134) .EQ. '       TOT') THEN
                    WRITE(REP_LUN, 103) ASCII_REC (144:215)
                ELSE
                    WRITE(REP_LUN,'(1X,A)'), ASCII_REC (86:215)
                ENDIF
            ENDIF
C            IF (ST .EQ. 0) THEN
C                IF (ASCII_REC(111:124) .EQ. '          TOT') THEN
C                    WRITE(REP_LUN, 103) ASCII_REC (143:214)
C                ELSE
C                    WRITE(REP_LUN,'(1X,A)'), ASCII_REC (85:214)
C                ENDIF
C            ENDIF
        ENDDO
        IF (ST .LT. 0) THEN ! EOF
            CALL USRCLOS1(REP_SRT_LUN)
        ENDIF
C=======================================================================
C       WRITE FOOTER INTO REPORT FILE 
C=======================================================================
C
        CALL PRINTPAS_REPTXDPRZ_FOOTER (REP_LUN,TOTPRZAMT,TOTNETPRZAMT,
     *                                  TOTTAXBLAMT, TOT_NR_PRZ_IS)
        CALL USRCLOS1(REP_LUN)

        CALL DFILX(REP_TMP_FILNAM,0,0,ST)
        IF (ST .NE. 0) CALL GSTOP (GEXIT_FATAL)
        CALL DFILX(REP_SRT_FILNAM,0,0,ST)
        IF (ST .NE. 0) CALL GSTOP (GEXIT_FATAL)
C----|--!--------------------------------------------------------------
C V03   ! New ordering - end
C----|--!--------------------------------------------------------------

C
C=======================================================================
C       LIST THE LATEST VERSION OF THE GENERATED FILES
C=======================================================================
C
        TYPE*, IAM(), '       '
        TYPE*, IAM(), 'Ficheiros gerados:'
        IF (FINREP) THEN
          WRITE(LIBCMD,'(A,A,A,A,A)') '$ DIR ',TRIM(REPFILNAM(6:)),';0, ',TRIM(SAPFILNAM(6:)),';0 /DATE/SIZE=ALL'
        ELSE
          WRITE(LIBCMD,'(A,A,A,A,A)') '$ DIR ',TRIM(REPFILNAM(6:)),';0 /DATE/SIZE=ALL'
        ENDIF
        ST = LIB$SPAWN(TRIM(LIBCMD))
        IF(.NOT. ST) CALL LIB$SIGNAL(%VAL(ST))
        TYPE*, IAM(), '       '
C
        TYPE*, IAM(), 'TAXMNG - Fim do processamento'
        TYPE*, IAM(), '       '

C
C=======================================================================
C       ASK THE OPERATOR IF PROCEDURE ENDED WITHOUT ERRORS
C=======================================================================
C
        CALL PRMYESNO('Procedimento terminou sem erros [Y/N]? ', YESNO)
        IF(YESNO .NE. 1) THEN
          ST = -1
          RETURN
        ENDIF
        ST = 0
        RETURN
C
2000    CONTINUE
C
C=======================================================================
C       CLOSE FILES
C=======================================================================
C
        IF (FINREP) CALL USRCLOS1(SAP_LUN)
        CALL USRCLOS1(REP_LUN)
        CALL ICLOSE(VPF_LUN,VPFBUF,ST)
        IF (FINREP) CALL PASIO_CLOSE(PASFDB)
C
C=======================================================================
C       DELETE FILES WITH ERRORS
C=======================================================================
C
        CALL DFILX(REPFILNAM,0,0,ST)
        IF (ST .NE. 0) CALL GSTOP (GEXIT_FATAL)
C
        IF (FINREP) THEN
          CALL DFILX(SAPFILNAM,0,0,ST)
          IF (ST .NE. 0) CALL GSTOP (GEXIT_FATAL)
        ENDIF
C
        TYPE *,IAM(), '     '
        TYPE *,IAM(), 'TAXMNG - Procedimento terminou com erro'
C
        ST = -1
C
C=======================================================================
C              FORMAT STATEMENTS
C=======================================================================
C
9000    FORMAT(I5.5,'S',I2.2,'F',I2.2)

103     FORMAT(67X, 64('-'), /,           ! SEPARADOR
     *          5X, 'TOTAL',              ! TOTAL
     *         49X, A, /)                 ! VALOR TOTAL LÍQUIDO DO PRÉMIO
C103     FORMAT(65X, 66('-'), /,           ! SEPARADOR
C     *          5X, 'TOTAL',              ! TOTAL
C     *         49X, A, /)                 ! VALOR TOTAL LÍQUIDO DO PRÉMIO

        RETURN
        END
C

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE EXEC_SYS_CMD(CMD,COMMENT,ST)
C
C       THIS SUBROUTINE EXECUTES A SYSTEM COMMAND
C
C       INPUTS:
C        CMD         SYSTEM COMMAND
C        COMMENT     COMMENT
C
C       OUTPUTS:
C        ST
C
C=======================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE EXEC_SYS_CMD(CMD,ST)
        IMPLICIT NONE
C
        INCLUDE '(LIB$ROUTINES)'
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        CHARACTER*256 CMD
C        CHARACTER*(*) COMMENT
        INTEGER*4  ST
           
        ST = LIB$SPAWN(TRIM(CMD))
        IF(.NOT. ST) CALL LIB$SIGNAL(%VAL(ST))
        
        END

      SUBROUTINE OPEN_FILE(FILNAM,LUN,ST)
      IMPLICIT NONE
C
      CHARACTER*(*) FILNAM
      INTEGER*4 ST,LUN
C
           OPEN (UNIT           =  LUN,
     *           FILE           =  FILNAM,
     *           STATUS         = 'OLD',
     *           ORGANIZATION   = 'SEQUENTIAL',
     *           ACCESS         = 'SEQUENTIAL',
     *           FORM           = 'FORMATTED',
     *           RECORDTYPE     = 'STREAM_CR',
     *           DISPOSE        = 'KEEP',
     *           IOSTAT         =  ST)

      RETURN
      END
