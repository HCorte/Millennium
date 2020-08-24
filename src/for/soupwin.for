CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SOUPWIN.FOR
C CHANGE LOG:
C ---+-------------+----+----------------------------------------------
C VER|DATE         |USER|DESCRIPTION
C ---+-------------+----+----------------------------------------------
C
C V03 15-OCT-2014   SCML  NEW LEVEL 3 PRIZES REPORT.
C
C V02 17-FEB-2014   SCML  INCLUDED GENERATION OF LEVEL 2 & 3
C                         PRIZES REPORT (ALL PRIZES REPORT WAS ALREADY
C                         IMPLEMENTED BUT IS DISABLED BY DEFAULT - SEE
C                         FLAGREP PARAMETER).
C                         REMOVED THE HARDCODED PUNIT VALUE (7) -
C                         FIND_AVAILABLE_LUN IS USED INSTEAD.
C
C V01 26-JAN-2014   SCML  INITIAL RELEASE OF SOUPWIN PROGRAM
C
C
C THIS ROUTINE PROCESS SETTLED PRIZES FROM SCML VLF FILE AND
C GENERATE OUTPUT FILE TO SOUP PLATFORM. GENERATES LEVEL 2 AND 3 PRIZES
C REPORT WITH TOTAL AMOUNTS BY PRIZE AND DIVISIONS.
C
C    INPUT:  VLF.FIL
C
C    OUTPUT: SOUP_AM_PA_GGSSSAAAA.ASC
C             ( GG=GAME / SSS=SORTEIO / AAAA=ANO )
C            REL_PA23_GG_SSSAAAA_AAAAMMDD.REP [LOTO]
C             ( GG=GAME / SSS=SORTEIO / AAAA=ANO / MM=MES / DD=DIA)
C            OR
C            REL_PA23_GG_CCCAAAA_AAAAMMDD.REP [SPORTS AND KICKER]
C             ( GG=GAME / CCC=CONCURSO / AAAA=ANO / MM=MES / DD=DIA)
C            OR
C            REL_PA3_GG_SSSAAAA_AAAAMMDD.REP [LOTO]
C             ( GG=GAME / SSS=SORTEIO / AAAA=ANO / MM=MES / DD=DIA)
C            OR
C            REL_PA3_GG_CCCAAAA_AAAAMMDD.REP [SPORTS AND KICKER]
C             ( GG=GAME / CCC=CONCURSO / AAAA=ANO / MM=MES / DD=DIA)
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2003 DJ - SCML. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C====== OPTIONS /CHECK=NOOVERFLOW
        PROGRAM SOUPWIN
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

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
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE '(LIB$ROUTINES)'                ! LIB FOR COMMAND LINE


        CHARACTER*7    CCCAAAA, CONCURSOANO, AAAACCC
        CHARACTER*9    GGAAAACCC
        INTEGER*4      CONCURSO, ANO
        INTEGER*4      DRWN, DRWNKIK, GETDRW       !FUNCTION
        INTEGER*4      GNUM, GTYP, VST,GIND
        INTEGER*4      INDDRW, DRWVDT
        INTEGER*4      ST, NOCHECK0
        INTEGER*4      TUBSIZ
        PARAMETER      (TUBSIZ=I4BUCSIZ*7)
        INTEGER*4      VLFBUF(TUBSIZ)
        INTEGER*4      YESNO
        INTEGER*4      DAT(8)    ! SYSDATE FROM MACHINE
        INTEGER*2      BEGDAT(12),ENDAT(12)
        INTEGER*2      DRDAT(LDATE_LEN) ! DRAWING DATE
        INTEGER*4      CNTREC,CTIM(2)
        CHARACTER*2    TPREC
        INTEGER*4      SZ
        INTEGER*4      PUNIT
        CHARACTER*30   PFILENAM ! INTERFACE FILE NAME
        CHARACTER*16   GAMLNAM(MAXGAM)  ! GAME LONG NAME
        LOGICAL        VALDRW
        INTEGER*4      FLUN, FLUNKIK,GAMSTS,GAMSTSKIK,GAMSEC
        EQUIVALENCE(DLTREC,DSPREC,GAMSTS)
        EQUIVALENCE(DKKREC,GAMSTSKIK)
        INTEGER*4      GNUMKIK
        PARAMETER(GNUMKIK=5)
        INTEGER*4      I, MGDIVNUM, KIKDIVNUM
        CHARACTER*50   C50DIV
        INTEGER*4      CUCSH, CPPAY, CCASH, CBANK, CNT_PRM
        LOGICAL*4      ERRORFLAG/.FALSE./   ! ERROR FLAG CONTROL
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
C        CHARACTER*256  LIBCMD        ! DIR GEN FILES COMMAND
        CHARACTER*512  LIBCMD        ! DIR GEN FILES COMMAND
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------

        INTEGER*4      REPORTFLAG   ! SET REPORT GENERATION FLAG
        INTEGER*4      FLUNREP      ! LUN FOR REL_PA FILE
        INTEGER*4      FLUNREP23    ! LUN FOR REL_PA23 FILE
        CHARACTER*35   PREPORTNAM   ! REL_PA FILE NAME
        CHARACTER*37   PREPORT23NAM ! REL_PA23 FILE NAME
        CHARACTER*54   REPTITLE     ! REL_PA REPORT TITLE
        CHARACTER*54   REPTITLE23   ! REL_PA23 REPORT TITLE
        INTEGER*4      K            ! ITER COUNTER FOR GLNAMES STRUCT
        INTEGER*4      MAXREPDIVCOUNT
           PARAMETER(MAXREPDIVCOUNT=12)
        INTEGER*4      REPTOTDIV(MAXREPDIVCOUNT), KITER ! DIV TOTALS STRUCT
        INTEGER*4      REPTOTDIV23(MAXREPDIVCOUNT) ! REL_PA23 DIV TOTALS STRUCT
        INTEGER*4      FLAGREP      ! REL_PA REPORT GEN FLAG
           PARAMETER(FLAGREP=0)     ! 0 - DISABLED; 1 - ACTIVE

        CHARACTER*32   DMONY        ! FORMAT CURRENCY FUNCTION
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
        INTEGER*4      FLUNREP3     ! LUN FOR REL_PA3 FILE
        CHARACTER*37   PREPORT3NAM  ! REL_PA3 FILE NAME
        CHARACTER*70   REPTITLE3    ! REL_PA3 REPORT TITLE
        INTEGER*4      REPTOTDIV3(MAXREPDIVCOUNT) ! REL_PA3 DIV TOTALS STRUCT
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------

        DATA GTYP/0/
C        DATA PUNIT/7/
        DATA REPORTFLAG/1/

C
C SHOW TITLE
C***************

        CALL CLRSCR(5)
        TYPE*,IAM()
        TYPE*,IAM(),'*********************************************************'
        TYPE*,IAM(),'*                        SOUPWIN                        *'
        TYPE*,IAM(),'*                  INTERFACE COM O SOUP                 *'
        TYPE*,IAM(),'*********************************************************'
        TYPE*,IAM(),'*                                                       *'
        TYPE*,IAM(),'*    * GERAÇÃO DO FICHEIRO DE PRÉMIOS ATRIBUIDOS *      *'
        TYPE*,IAM(),'*                                                       *'
        TYPE*,IAM(),'*            MUTUAS  (SOUP_PA_GNCCCAAAA.ASC)            *'
        TYPE*,IAM(),'*             GN=GAME/CCC=CONCURSO/AAAA=ANO             *'
        TYPE*,IAM(),'*                                                       *'
        TYPE*,IAM(),'*            LOTARIA  (SOUP_PA_GNEEAAAA.ASC)            *'
        TYPE*,IAM(),'*       GN=GAME(08 ou 09)/ EE=EXTRACÇÃO /AAAA=ANO       *'
        TYPE*,IAM(),'*                                                       *'
        TYPE*,IAM(),'* * * * * * * * * * * * * * * * * * * * * * * * * * * * *'
        TYPE*,IAM(),'*                                                       *'
        TYPE*,IAM(),'*  * GERAÇÃO DO  RELATORIO DE PREMIOS DE NIVEL 2 E 3 *  *'
        TYPE*,IAM(),'*                                                       *'
        TYPE*,IAM(),'*       MUTUAS  (REL_PA23_GG_SSSAAAA_AAAAMMDD.REP)      *'
        TYPE*,IAM(),'*  GG=GAME / SSS=SORTEIO / AAAA=ANO / MM=MES / DD=DIA   *'
        TYPE*,IAM(),'*                                                       *'
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
        TYPE*,IAM(),'* * * * * * * * * * * * * * * * * * * * * * * * * * * * *'
        TYPE*,IAM(),'*                                                       *'
        TYPE*,IAM(),'*    * GERAÇÃO DO  RELATORIO DE PREMIOS DE NIVEL 3 *    *'
        TYPE*,IAM(),'*                                                       *'
        TYPE*,IAM(),'*       MUTUAS  (REL_PA3_GG_SSSAAAA_AAAAMMDD.REP)       *'
        TYPE*,IAM(),'*  GG=GAME / SSS=SORTEIO / AAAA=ANO / MM=MES / DD=DIA   *'
        TYPE*,IAM(),'*                                                       *'
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
        TYPE*,IAM(),'*********************************************************'
        TYPE*,IAM()


100     CONTINUE


                CALL GAME_TYPNDX(GNUM, GTYP, GIND, GAMLNAM, ST)
                IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
                IF (GNUM .GT.7 .AND. GNUM .LT. 10) THEN
                    CALL SOUPWINPAS (GNUM,ST)
                    IF (ST .NE. 0) GOTO 2000
                    GOTO 100
                END IF


C       CHOOSE DRAW
        CALL WIMG(5,'DESEJA PROCESSAR QUAL CONCURSO (CCCAAAA)?')
        READ(5,102) CONCURSOANO
        TYPE*,IAM(),CONCURSOANO

        IF (CONCURSOANO .EQ. 'e' .OR. CONCURSOANO .EQ. 'E' .OR. CONCURSOANO .EQ. '  ') GOTO 100

        ANO = MOD(CTOI(CONCURSOANO,SZ),10000)
        CONCURSO = INT(CTOI(CONCURSOANO,SZ)/10000)


        IF (ANO.LT.2005 .OR. ANO.GT.2100 .OR. CONCURSO.GT.105 .OR. CONCURSO.LE.0) THEN !V05
          TYPE*,IAM(),'SOUPWIN - Ano/Concurso Inválido'
            GOTO 100
        ENDIF

        WRITE(CCCAAAA,FMT='(I3.3,I4.4)') CONCURSO,ANO
        WRITE(AAAACCC,FMT='(I4.4,I3.3)') ANO, CONCURSO
        WRITE(GGAAAACCC,FMT='(I2.2,I4.4,I3.3)') GNUM, ANO, CONCURSO

        CALL PRMYESNO('Confirma o concurso '//CCCAAAA//',  (Y/N) ? ', YESNO)
        IF(YESNO.NE.1) GOTO 100

C GET MAIN GAME DRAW NUMBER
          DRWN = GETDRW(ANO,CONCURSO,GNUM)
          IF(GTYP .EQ. TSPT) THEN
C GET KICKER DRAW NUMBER - IF MAIN GAME IS SPORTS
             DRWNKIK = GETDRW(ANO,CONCURSO,GNUMKIK)
             IF(DRWNKIK .GT. DAYHDR(GNUMKIK)) DRWNKIK = 0
          ENDIF


        IF (DRWN.LE.0) THEN
           TYPE*,IAM(),'SOUPWIN - Nº do DRAW inválido'
           TYPE*,IAM(),'Erro Fatal'
           goto 100
        ENDIF


C READ GAME FILE
Cthis subroutine open, read and close the file
C************************************************
        CALL FIND_AVAILABLE_LUN (FLUN,ST)
        IF (ST .NE. 0) THEN
           TYPE*, IAM(), '       '
           TYPE*, IAM(), 'SOUPWIN - Erro a obter uma LUN para o ficheiro!'
           TYPE*, IAM(), '       '
           TYPE*, IAM(), '       '
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
        CALL FIND_AVAILABLE_LUN (FLUNKIK,ST)
        IF (ST .NE. 0) THEN
           TYPE*, IAM(), '       '
           TYPE*, IAM(), 'SOUPWIN - Erro a obter uma LUN para o ficheiro!'
           TYPE*, IAM(), '       '
           TYPE*, IAM(), '       '
           CALL GSTOP (GEXIT_FATAL)
        ENDIF


        MGDIVNUM = 0
        KIKDIVNUM = 0
C IF SPORTS THEN LOAD SPORTS AND JOKER SHARES
        IF(GTYP .EQ. TSPT) THEN
C          MAIN GAME FILE READ
           TYPE*,IAM(),' '
           TYPE*,IAM(), 'A obter o valor das shares do concurso' //' '//CCCAAAA
           TYPE*,IAM(), 'do '//TRIM(GAMLNAM(GNUM))//'...'
           CALL READGFL(FLUN,GFNAMES(1,GNUM),DSPSEC,DRWN,GAMSTS)
           MGDIVNUM = DSPDIV
           I=1
           DO 222 I=1,MGDIVNUM
              WRITE(C50DIV,'(A,I0,A)') ' Divisao ',I,': '
              TYPE*,IAM(),TRIM(C50DIV),CMONY(DSPSHV(I),13,VALUNIT)
222        CONTINUE

C          JOKER GAME FILE READ
           CALL FASTSET(0, DKKREC, SIZEOF(DKKREC) / 4)
           IF(DRWNKIK .GT. 0) THEN
              TYPE*,IAM(),' '
              TYPE*, IAM(), 'A obter o valor das shares do concurso' //' '//CCCAAAA
              TYPE*, IAM(), 'do '//TRIM(GAMLNAM(GNUMKIK))//'...'
              CALL READGFL(FLUNKIK,GFNAMES(1,GNUMKIK),DKKSEC,DRWNKIK, GAMSTSKIK)
              KIKDIVNUM = DKKDIV
              I=1
              DO 223 I=1,KIKDIVNUM
                 WRITE(C50DIV,'(A,I0,A)') ' Divisao ',I,': '
                 TYPE*,IAM(),TRIM(C50DIV),CMONY(DKKSHV(I),13,VALUNIT)
223           CONTINUE
           ENDIF

C ELSE IF LOTO LOAD ONLY LOTO SHARES
        ELSEIF (GTYP .EQ. TLTO) THEN
           TYPE*,IAM(),' '
           TYPE*, IAM(), 'A obter o valor das shares do concurso' //' '//CCCAAAA
           TYPE*, IAM(), 'do '//TRIM(GAMLNAM(GNUM))//'...'
           CALL READGFL(FLUN,GFNAMES(1,GNUM),DLTSEC,DRWN,GAMSTS)
           MGDIVNUM = DLTDIV
           I=1
           DO 224 I=1,MGDIVNUM
              WRITE(C50DIV,'(A,I0,A)') ' Divisao ',I,': '
              TYPE*,IAM(),TRIM(C50DIV),CMONY(DLTSHV(I,1),13,VALUNIT)
224        CONTINUE

C ELSE IF KICKER LOAD ONLY JOKER SHARES
        ELSEIF (GTYP .EQ. TKIK) THEN
           TYPE*,IAM(),' '
           TYPE*, IAM(), 'A obter o valor das shares do concurso' //' '//CCCAAAA
           TYPE*, IAM(), 'do '//TRIM(GAMLNAM(GNUM))//'...'
           CALL READGFL(FLUN,GFNAMES(1,GNUM),DKKSEC,DRWN,GAMSTSKIK)
           MGDIVNUM = DKKDIV
           I=1
           DO 225 I=1,MGDIVNUM
              WRITE(C50DIV,'(A,I0,A)') ' Divisao ',I,': '
              TYPE*,IAM(),TRIM(C50DIV),CMONY(DKKSHV(I),13,VALUNIT)
225        CONTINUE
        ENDIF
        TYPE *,IAM(),' '


C CHECK IF DRAW HAVE PRIZES NUMBERS
C**************************************
        IF (GTYP .EQ. TKIK) THEN
           IF (GAMSTSKIK.LT.GFINAL) THEN
               TYPE *,IAM(),'SOUPWIN-Prémios não apurados para o concurso: ',CCCAAAA
               TYPE *,IAM(),'       '
               GOTO 100
           ENDIF
        ELSE
           IF (GAMSTS.LT.GFINAL) THEN
               TYPE *,IAM(),'SOUPWIN-Prémios não apurados para o concurso: ',CCCAAAA
               TYPE *,IAM(),'       '
               GOTO 100
           ENDIF
        ENDIF


C WRITE SOUP_PA INTERFACE FILE NAME
C***********************************
        WRITE (PFILENAM, FMT='(A16,I2.2,A7,A4)')
     *     'FILE:SOUP_AM_PA_',GNUM,CCCAAAA,'.ASC'


C     TRY TO DELETE FILE FIRST
C******************************
        CALL DFILX(PFILENAM,0,0,ST)
        IF (ST.NE.0) CALL GSTOP (GEXIT_FATAL)


C OPEN SOUP_PA INTERFACE FILE
C*****************************
        CALL FIND_AVAILABLE_LUN (PUNIT,ST)
        IF (ST .NE. 0) THEN
           TYPE*, IAM(), '       '
           TYPE*, IAM(), 'SOUPWIN - Erro a obter uma LUN para o ficheiro!'
           TYPE*, IAM(), '       '
           TYPE*, IAM(), '       '
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
        CALL OPEN_FILASC (PFILENAM,PUNIT,ST)
        IF (ST.NE.0) THEN
            TYPE*,IAM(),'SOUPWIN - Erro a criar/abrir o ficheiro:      '
            TYPE*,IAM(),'SOUP_AM_PA_',GNUM,CCCAAAA,'.ASC '
            TYPE*, IAM(),' '
            GOTO 2000
        ENDIF


C WRITE SOUP_PA HEADER
C**********************
        TPREC='HP'

C MIGRATION OF THE GENERATION DATE MEMORIE FREE - START
        CALL ICLOCK(1,CTIM)
        CALL GDATE(DAT(2),DAT(3),DAT(1))
        IF(DAT(1).LT.77) THEN
          DAT(1) = DAT(1) + 2000
        ELSE
          DAT(1) = DAT(1) + 1900
        ENDIF
C MIGRATION OF THE GENERATION DATE MEMORIE FREE - END

C DRAW DATE
            IF (GTYP .EQ. TLTO) THEN
               DRDAT(VCDC) = DLTDAT(CURDRW)
            ELSEIF(GTYP .EQ. TKIK) THEN
               DRDAT(VCDC) = DKKDAT(CURDRW)
            ELSEIF(GTYP .EQ. TSPT) THEN
               DRDAT(VCDC) = DSPDAT(CURDRW)
            ENDIF
            CALL LCDATE(DRDAT(1))
C SALES BEGIN DATE
            IF (GTYP .EQ. TLTO) THEN
               BEGDAT(VCDC) = DLTBSD
            ELSEIF(GTYP .EQ. TKIK) THEN
               BEGDAT(VCDC) = DKKBSD
            ELSEIF(GTYP .EQ. TSPT) THEN
               BEGDAT(VCDC) = DSPBSD
            ENDIF
            CALL CDATE(BEGDAT)
C SALES END DATE
            IF (GTYP .EQ. TLTO) THEN
               ENDAT(VCDC) = DLTESD
            ELSEIF(GTYP .EQ. TKIK) THEN
               ENDAT(VCDC) = DKKESD
            ELSEIF(GTYP .EQ. TSPT) THEN
               ENDAT(VCDC) = DSPESD
            ENDIF
            CALL CDATE(ENDAT)

         WRITE(PUNIT,10) TPREC,DAT(1),DAT(2),DAT(3)
     *                        ,2000+DRDAT(VYEAR),DRDAT(VMON),DRDAT(VDAY)
     *                        ,2000+BEGDAT(VYEAR),BEGDAT(VMON),BEGDAT(VDAY)
     *                        ,2000+ENDAT(VYEAR),ENDAT(VMON),ENDAT(VDAY)

         CNTREC = 1


C----|--!--------------------------------------------------------------
C V02   !  Write REL_PA and REL_PA23 interface file name - start
C----|--!--------------------------------------------------------------
C OPEN IF(REPORTFLAG)
        IF(REPORTFLAG) THEN
           IF(FLAGREP) THEN
              WRITE (PREPORTNAM, FMT='(A12,I2.2,A1,A7,A1,I4.4,I2.2,I2.2,A4)')
     *           'FILE:REL_PA_',GNUM,'_',CCCAAAA,'_',DAT(1),DAT(2),DAT(3),'.REP'
           ENDIF
           WRITE (PREPORT23NAM, FMT='(A14,I2.2,A1,A7,A1,I4.4,I2.2,I2.2,A4)')
     *        'FILE:REL_PA23_',GNUM,'_',CCCAAAA,'_',DAT(1),DAT(2),DAT(3),'.REP'
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           WRITE (PREPORT3NAM, FMT='(A14,I2.2,A1,A7,A1,I4.4,I2.2,I2.2,A4)')
     *        'FILE:REL_PA3_',GNUM,'_',CCCAAAA,'_',DAT(1),DAT(2),DAT(3),'.REP'
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
C----|--!--------------------------------------------------------------
C V02   !  Write REL_PA and REL_PA23 interface file name - end
C----|--!--------------------------------------------------------------

C----|--!--------------------------------------------------------------
C V02   !  Open REL_PA and REL_PA23 output files- start
C----|--!--------------------------------------------------------------
C REL_PA
           IF(FLAGREP) THEN
              CALL FIND_AVAILABLE_LUN (FLUNREP,ST)
              IF (ST .NE. 0) THEN
                 TYPE*, IAM(), '       '
                 TYPE*, IAM(), 'SOUPWIN - Erro a obter uma LUN para o ficheiro de Report!'
                 TYPE*, IAM(), '       '
                 TYPE*, IAM(), '       '
                 CALL GSTOP (GEXIT_FATAL)
              ENDIF
              CALL OPEN_FILE (PREPORTNAM,FLUNREP,ST)
              IF (ST.NE.0) THEN
                  TYPE*,IAM(),'SOUPWIN - Erro a criar/abrir o ficheiro:      '
                  TYPE*,IAM(),PREPORTNAM
                  TYPE*, IAM(),' '
                  GOTO 2000
              ENDIF
           ENDIF
C REL_PA23
           CALL FIND_AVAILABLE_LUN (FLUNREP23,ST)
           IF (ST .NE. 0) THEN
              TYPE*, IAM(), '       '
              TYPE*, IAM(), 'SOUPWIN - Erro a obter uma LUN para o ficheiro de Report !'
              TYPE*, IAM(), '       '
              TYPE*, IAM(), '       '
              CALL GSTOP (GEXIT_FATAL)
           ENDIF
           CALL OPEN_FILE (PREPORT23NAM,FLUNREP23,ST)
           IF (ST.NE.0) THEN
               TYPE*,IAM(),'SOUPWIN - Erro a criar/abrir o ficheiro:      '
               TYPE*,IAM(),PREPORT23NAM
               TYPE*, IAM(),' '
               GOTO 2000
           ENDIF
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
C REL_PA3
           CALL FIND_AVAILABLE_LUN (FLUNREP3,ST)
           IF (ST .NE. 0) THEN
              TYPE*, IAM(), '       '
              TYPE*, IAM(), 'SOUPWIN - Erro a obter uma LUN para o ficheiro de Report (3)!'
              TYPE*, IAM(), '       '
              TYPE*, IAM(), '       '
              CALL GSTOP (GEXIT_FATAL)
           ENDIF
           CALL OPEN_FILE (PREPORT3NAM,FLUNREP3,ST)
           IF (ST.NE.0) THEN
               TYPE*,IAM(),'SOUPWIN - Erro a criar/abrir o ficheiro:      '
               TYPE*,IAM(),PREPORT3NAM
               TYPE*, IAM(),' '
               GOTO 2000
           ENDIF
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
C----|--!--------------------------------------------------------------
C V02   !  Open REL_PA and REL_PA23 output files- end
C----|--!--------------------------------------------------------------

C----|--!--------------------------------------------------------------
C V02   !  Write Header REL_PA and REL_PA23 files- start
C----|--!--------------------------------------------------------------
C RESET REPTOTDIV & REPTOTDIV23 STRUCTS
           IF(FLAGREP) THEN
              K=1
              DO WHILE(K .LE. MAXREPDIVCOUNT)
                 REPTOTDIV(K) = 0
                 K=K+1
              ENDDO
           ENDIF
           K=1
           DO WHILE(K .LE. MAXREPDIVCOUNT)
              REPTOTDIV23(K) = 0
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
              REPTOTDIV3(K) = 0
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
              K=K+1
           ENDDO
           REPTITLE = '       RELATORIO DE TODOS OS PREMIOS ATRIBUIDOS       '
           REPTITLE23 = 'RELATORIO DE PREMIOS ATRIBUIDOS SUPERIORES A 150 EUROS'
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           REPTITLE3 = 'RELATORIO DE APOSTAS PREMIADAS - VALOR IGUAL OU SUPERIOR A 5.000 EUROS'
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           IF(GNUM .EQ. 6 .OR. GNUM .EQ. 7) THEN
              IF(FLAGREP) THEN
                 WRITE(FLUNREP,30) REPTITLE,DAT(3),DAT(2),DAT(1),
     *                             'SORTEIO',CONCURSO,ANO,
     *                             (GLNAMES(K,GNUM),K=1,4)
                 ENDIF
              WRITE(FLUNREP23,30) REPTITLE23,DAT(3),DAT(2),DAT(1),
     *                            'SORTEIO',CONCURSO,ANO,
     *                            (GLNAMES(K,GNUM),K=1,4)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
              WRITE(FLUNREP3,303) REPTITLE3,DAT(3),DAT(2),DAT(1),
     *                            'SORTEIO',CONCURSO,ANO,
     *                            (GLNAMES(K,GNUM),K=1,4)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           ELSEIF(GNUM .EQ. 5) THEN
              IF(FLAGREP) THEN
                 WRITE(FLUNREP,33) REPTITLE,DAT(3),DAT(2),DAT(1),
     *                             'SORTEIO',CONCURSO,ANO,
     *                             (GLNAMES(K,GNUM),K=1,4)
              ENDIF
              WRITE(FLUNREP23,33) REPTITLE23,DAT(3),DAT(2),DAT(1),
     *                            'SORTEIO',CONCURSO,ANO,
     *                            (GLNAMES(K,GNUM),K=1,4)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
              WRITE(FLUNREP3,333) REPTITLE3,DAT(3),DAT(2),DAT(1),
     *                            'SORTEIO',CONCURSO,ANO,
     *                            (GLNAMES(K,GNUM),K=1,4)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           ELSEIF(GNUM .EQ. 1) THEN
              IF(FLAGREP) THEN
                 WRITE(FLUNREP,31) REPTITLE,DAT(3),DAT(2),DAT(1),
     *                             'CONCURSO',CONCURSO,ANO,
     *                             (GLNAMES(K,GNUM),K=1,4)
              ENDIF
              WRITE(FLUNREP23,31) REPTITLE23,DAT(3),DAT(2),DAT(1),
     *                            'CONCURSO',CONCURSO,ANO,
     *                            (GLNAMES(K,GNUM),K=1,4)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
              WRITE(FLUNREP3,313) REPTITLE3,DAT(3),DAT(2),DAT(1),
     *                            'CONCURSO',CONCURSO,ANO,
     *                            (GLNAMES(K,GNUM),K=1,4)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           ELSEIF(GNUM .EQ. 10) THEN
              IF(FLAGREP) THEN
                 WRITE(FLUNREP,32) REPTITLE,DAT(3),DAT(2),DAT(1),
     *                             'CONCURSO',CONCURSO,ANO,
     *                             (GLNAMES(K,GNUM),K=1,4)
              ENDIF
              WRITE(FLUNREP23,32) REPTITLE23,DAT(3),DAT(2),DAT(1),
     *                            'CONCURSO',CONCURSO,ANO,
     *                            (GLNAMES(K,GNUM),K=1,4)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
              WRITE(FLUNREP3,323) REPTITLE3,DAT(3),DAT(2),DAT(1),
     *                            'CONCURSO',CONCURSO,ANO,
     *                            (GLNAMES(K,GNUM),K=1,4)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           ENDIF

C CLOSE IF(REPORTFLAG)
        ENDIF
C----|--!--------------------------------------------------------------
C V02   !  Write Header REL_PA and REL_PA23 files- end
C----|--!--------------------------------------------------------------

C OPEN VALIDATION FILE
C*********************
        CALL IOPEN(SFNAMES(1,VLF),VLF,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
           IF(ST.NE.0) THEN
              CALL FILERR(SFNAMES(1,VLF),1,ST,0)
              GOTO 2000
           ENDIF
        CALL ITUBSIZE(VLF,TUBSIZ)

        TYPE*, IAM(),'SOUPWIN-Aguarde, ficheiro de interface em processamento'
        TYPE*, IAM(),' '

C SET STATISTICS COUNTERS
        CPPAY = 0
        CUCSH = 0
        CCASH = 0
        CBANK = 0
        CNT_PRM=0

C READ TRANSACTION
C*****************
C PSEUDO ITERATION STRUCTURE
300     CONTINUE

          CALL ISREAD(V4BUF,VLF,VLFBUF,ST)

          IF(ST.EQ.ERREND) THEN
             GOTO 1000
          ENDIF

          IF(ST.NE.0) THEN
             CALL FILERR(SFNAMES(1,VLF),2,ST,0)
             CALL GPAUSE
             GOTO 300
          ENDIF

          CALL LOGVAL(VALREC,V4BUF)
          VST=VALREC(VSTAT)

C CHECK IF RECORD HAS OP
          IF(VALREC(VOPSCNT).NE.0) THEN
             GOTO 300
          ENDIF

C CHECK IF TRANSACTION SHOULD BE PRINTED
C****************************************
        IF(VALREC(VGAM).NE.GNUM.AND.GNUM.GT.0)   GOTO 300

C CHECK FOR RESULTS NOT IN
C**************************
        IF(VST.EQ.VNOPAY.OR.VST.EQ.VNOPRZ.OR.VST.EQ.VPOST.OR.
     *               VST.EQ.VPPNPZ.OR.VST.EQ.VPRPOST) THEN
           GTYP = VALREC(VGTYP)
           CALL DLOGVAL(VALREC,VDETAIL)
C compare draw number from the user with the vdetail
           DRWVDT = VDETAIL(VDRW,1)
           IF (DRWVDT .EQ. DRWN) THEN
              TYPE*,IAM(),'SOUPWIN - Foi encontrado um prémio para o '
              TYPE*,IAM(),'qual o valor ainda não foi atribuído!'
              CALL GPAUSE
              GOTO 300
           ENDIF
        ENDIF

C CHECK FOR RESULTS IN
        IF (VALREC(VSTAT).EQ.VPRPAY.OR.
     *            VALREC(VSTAT).EQ.VUNCSH.OR.
     *                  VALREC(VSTAT).EQ.VCASH.OR.
     *                        VALREC(VSTAT).EQ.VBANK) THEN

C PRINT TRANSACTION
C*******************
           GTYP = VALREC(VGTYP)
           CALL DLOGVAL(VALREC,VDETAIL)

C compare draw number from the user with the vdetail
           VALDRW = .TRUE.
           DO INDDRW=1, VALREC(VPZOFF)
              DRWVDT = VDETAIL(VDRW,INDDRW)
              IF (DRWVDT .NE. DRWN) THEN
                 VALDRW = .FALSE.
                 GOTO 300
              ENDIF
           ENDDO

C CHECK DRAW TO PRINT
C*********************
           IF (VALDRW) THEN
              IF (GTYP .EQ. TSPT) THEN
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
                 CALL WRITEV_AM(VALREC,PUNIT,VDETAIL,CNTREC,ST,
     *                                DSPSHV,DKKSHV,
     *                                FLUNREP, FLUNREP23, FLUNREP3,
     *                                REPORTFLAG,
     *                                REPTOTDIV, REPTOTDIV23, REPTOTDIV3, 
     *                                FLAGREP,GNUM)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
                 CNT_PRM=1 + CNT_PRM
              ELSEIF (GTYP .EQ. TLTO) THEN
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
                 CALL WRITEV_AM(VALREC,PUNIT,VDETAIL,CNTREC,ST,
     *                                DLTSHV,DKKSHV,
     *                                FLUNREP, FLUNREP23, FLUNREP3,
     *                                REPORTFLAG,
     *                                REPTOTDIV, REPTOTDIV23, REPTOTDIV3, 
     *                                FLAGREP,GNUM)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
                 CNT_PRM=1 + CNT_PRM
              ELSEIF (GTYP .EQ. TKIK) THEN
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
                 CALL WRITEV_AM(VALREC,PUNIT,VDETAIL,CNTREC,ST,
     *                                DKKSHV,DKKSHV,
     *                                FLUNREP, FLUNREP23, FLUNREP3,
     *                                REPORTFLAG,
     *                                REPTOTDIV, REPTOTDIV23, REPTOTDIV3, 
     *                                FLAGREP,GNUM)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
                 CNT_PRM=1 + CNT_PRM
              ENDIF

C INCREASE STATISTICS COUNTERS
              IF(VALREC(VSTAT).EQ.VPRPAY) CPPAY = CPPAY + 1
              IF(VALREC(VSTAT).EQ.VUNCSH) CUCSH = CUCSH + 1
              IF(VALREC(VSTAT).EQ.VCASH) CCASH = CCASH + 1
              IF(VALREC(VSTAT).EQ.VBANK) CBANK = CBANK + 1

           ENDIF

           IF (ST .NE. 0) GOTO 2000

C ENDIF [IF(VST.EQ.VNOPAY.OR.VST.EQ.VNOPRZ.OR.VST.EQ.VPOST.OR.]
        ENDIF

        GOTO 300

1000    CONTINUE

C WRITE TRAILER
C******************
        TPREC='TP'
        CNTREC =CNTREC+1

        WRITE(PUNIT,20)TPREC,CNTREC

C PRINT STATISTICS PER FILE
        TYPE*,IAM(),'Total de registos processados:  ',CNT_PRM
        TYPE*,IAM(),'   UCSH:                        ',CUCSH
        TYPE*,IAM(),'   PPAY:                        ',CPPAY
        TYPE*,IAM(),'   CASH:                        ',CCASH
        TYPE*,IAM(),'   BANK:                        ',CBANK
        TYPE*,IAM(),' '

        IF(CNT_PRM.NE.(CUCSH+CPPAY+CCASH+CBANK)) THEN
           TYPE*,IAM(),'* * * * * * * * * * * * * * * * * * * * * * * *'
           TYPE*,IAM(),'  O TOTAL DE REGISTOS CONTABILIZADOS NAO ESTA  '
           TYPE*,IAM(),'  COERENTE COM OS SUBTOTAIS CONTABILIZADOS!!!  '
           TYPE*,IAM(),'* * * * * * * * * * * * * * * * * * * * * * * *'
           ERRORFLAG = .TRUE.
           CALL GPAUSE
        ENDIF


C CHECK IF ERRORS OCURRED DURING FILE PROCESSING
       IF(ERRORFLAG) THEN
          TYPE*,IAM(),'   Ficheiros gerados com ERROS:                      '
       ELSE
          TYPE*,IAM(),'   Ficheiros gerados com SUCESSO:                    '
       ENDIF


C----|--!--------------------------------------------------------------
C V02   !  WRITE FOOTER STATISTICS IN REL_PA and REL_PA23 files - start
C----|--!--------------------------------------------------------------
        IF(REPORTFLAG) THEN
C SET DIV ITERATOR FOR EACH GAME TYPE
C TOTOLOTO QUARTA & SABADO
           IF(GNUM .EQ. 6 .OR. GNUM .EQ. 7) THEN
              KITER = 6
C JOKER
           ELSEIF(GNUM .EQ. 5) THEN
              KITER = 6
C TOTOBOLA NORMAL
           ELSEIF(GNUM .EQ. 1) THEN
              KITER = 10
C TOTOBOLA EXTRA
           ELSEIF(GNUM .EQ. 10) THEN
              KITER = 4
           ENDIF
C WRITE STAT FOOTER
           IF(GNUM .EQ. 6 .OR. GNUM .EQ. 7) THEN
              IF(FLAGREP) THEN
                 WRITE(FLUNREP,36) (REPTOTDIV(K),K=1,KITER),
     *                             DMONY(REPTOTDIV(12)),
     *                             REPTOTDIV(11)
              ENDIF
              WRITE(FLUNREP23,36) (REPTOTDIV23(K),K=1,KITER),
     *                            DMONY(REPTOTDIV23(12)),
     *                            REPTOTDIV23(11)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
              WRITE(FLUNREP3,363) (REPTOTDIV3(K),K=1,KITER),
     *                            DMONY(REPTOTDIV3(12)),
     *                            REPTOTDIV3(11)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           ELSEIF(GNUM .EQ. 5) THEN
              IF(FLAGREP) THEN
                 WRITE(FLUNREP,38) (REPTOTDIV(K),K=1,KITER),
     *                             DMONY(REPTOTDIV(12)),
     *                             REPTOTDIV(11)
              ENDIF
              WRITE(FLUNREP23,38) (REPTOTDIV23(K),K=1,KITER),
     *                            DMONY(REPTOTDIV23(12)),
     *                            REPTOTDIV23(11)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
              WRITE(FLUNREP3,383) (REPTOTDIV3(K),K=1,KITER),
     *                            DMONY(REPTOTDIV3(12)),
     *                            REPTOTDIV3(11)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           ELSEIF(GNUM .EQ. 1) THEN
              IF(FLAGREP) THEN
                 WRITE(FLUNREP,37) (REPTOTDIV(K),K=1,KITER),
     *                             DMONY(REPTOTDIV(12)),
     *                             REPTOTDIV(11)
              ENDIF
              WRITE(FLUNREP23,37) (REPTOTDIV23(K),K=1,KITER),
     *                            DMONY(REPTOTDIV23(12)),
     *                            REPTOTDIV23(11)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
              WRITE(FLUNREP3,373) (REPTOTDIV3(K),K=1,KITER),
     *                            DMONY(REPTOTDIV3(12)),
     *                            REPTOTDIV3(11)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           ELSEIF(GNUM .EQ. 10) THEN
              IF(FLAGREP) THEN
                 WRITE(FLUNREP,39) (REPTOTDIV(K),K=1,KITER),
     *                             DMONY(REPTOTDIV(12)),
     *                             REPTOTDIV(11)
              ENDIF
              WRITE(FLUNREP23,39) (REPTOTDIV23(K),K=1,KITER),
     *                            DMONY(REPTOTDIV23(12)),
     *                            REPTOTDIV23(11)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
              WRITE(FLUNREP3,393) (REPTOTDIV3(K),K=1,KITER),
     *                            DMONY(REPTOTDIV3(12)),
     *                            REPTOTDIV3(11)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           ENDIF
        ENDIF
C----|--!--------------------------------------------------------------
C V02   !  WRITE FOOTER STATISTICS IN REL_PA and REL_PA23 files - start
C----|--!--------------------------------------------------------------

C
C=======================================================================
C       LIST THE LATEST VERSION OF THE GENERATED FILES
C=======================================================================
C
CCC       WRITE(LIBCMD,'(A,A,A)') '$ DIR ',TRIM(PFILENAM(6:)),';0 /DATE/SIZE=ALL'

C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
       WRITE(LIBCMD,'(A,A,A,A,A,A,A)') '$ DIR ',
     *          TRIM(PFILENAM(6:)),';0,',
     *          TRIM(PREPORT23NAM(6:)),';0,',
     *          TRIM(PREPORT3NAM(7:)),';0 /DATE/SIZE=ALL'
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------

       ST = LIB$SPAWN(TRIM(LIBCMD))
       IF(.NOT. ST) CALL LIB$SIGNAL(%VAL(ST))
       TYPE*,IAM(),'***********************************************************'
       TYPE*,IAM(),' '
       TYPE*,IAM(),'***********************************************************'
       TYPE*,IAM(),'                       SOUPWIN ENDED                       '
       TYPE*,IAM(),'***********************************************************'
       TYPE*,IAM(),' '
       TYPE*,' '


C CLOSE FILES
C***********
        CALL USRCLOS1(PUNIT)
        CALL ICLOSE(VLF,VLFBUF,ST)
C----|--!--------------------------------------------------------------
C V02   !  CLOSE REL_PA and REL_PA23 files - start
C----|--!--------------------------------------------------------------
        CALL USRCLOS1(FLUNREP)
        CALL USRCLOS1(FLUNREP23)
C----|--!--------------------------------------------------------------
C V02   !  CLOSE REL_PA and REL_PA23 files - end
C----|--!--------------------------------------------------------------
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
        CALL USRCLOS1(FLUNREP3)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
        GOTO 100


2000    CONTINUE

C CLOSE FILES
C***********
        CALL USRCLOS1(PUNIT)
        CALL ICLOSE(VLF,VLFBUF,ST)
C     DELETE FILE WITH ERRORS
        CALL DFILX(PFILENAM,0,0,ST)
        IF (ST.NE.0) CALL GSTOP (GEXIT_FATAL)

        TYPE *,IAM(), 'SOUPWIN-PROCEDIMENTO COM ERROS'
        TYPE *,IAM(), ' '

C----|--!--------------------------------------------------------------
C V02   !  CLOSE REL_PA and REL_PA23 files - start
C----|--!--------------------------------------------------------------
        CALL USRCLOS1(FLUNREP)
        CALL USRCLOS1(FLUNREP23)
C----|--!--------------------------------------------------------------
C V02   !  CLOSE REL_PA and REL_PA23 files - end
C----|--!--------------------------------------------------------------
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
        CALL USRCLOS1(FLUNREP3)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
        GOTO 100

C*********************************************
C              FORMAT MESSAGES
C*********************************************

102    FORMAT(A7)
C HEADER
10     FORMAT(A2,I4.4,I2.2,I2.2,I4.4,I2.2,I2.2,I4.4,I2.2,I2.2,I4.4,I2.2,I2.2,45(' '))
C TRAILER
20     FORMAT(A2,I8.8,69(' '))

C----|--!--------------------------------------------------------------
C V02   !  FORMATS FOR REL_PA and REL_PA23 files - start
C----|--!--------------------------------------------------------------
C REPORT HEADERL (totoloto quarta e sabado)
30      FORMAT(1X, 130('='), /,
     *         1X, 'SCML - Departamento de Jogos', T44, A54, T116, 'Data: ', I2.2, '.', I2.2, '.', I4.4,/,
     *         1X, '                            ', T56, A, 1X, I3.3, '/', I4.4, 1X, 4A4,/,
     *         1X, 130('='), /,
     *         1X, T123,'VALOR DO', /,
     *         1X, T3, 'CODIGO APOSTA',T22,'N. MAQUINA',T44,'1',
     *             T56,'2',T68,'3',T80,'4',T92,'5',T103,'NS',T125,'PREMIO', /,
     *         1X, 130('-'),/)

C REPORT HEADERL1 (totobola normal)
31      FORMAT(1X, 130('='), /,
     *         1X, 'SCML - Departamento de Jogos ', T44, A54, T116, 'Data: ', I2.2, '.', I2.2, '.', I4.4,/,
     *         1X, '                            ', T55, A, 1X, I2.2, '/', I4.4, 1X, 4A4,/,
     *         1X, 130('='), /,
     *         1X, T123,'VALOR DO', /,
     *         1X, T3,'CODIGO APOSTA',T22,'N. MAQUINA',T39,'S14',
     *             T49,'1',T57,'2',T65,'3',T72,'J1',T80,'J2',T88,'J3',
     *             T96,'J4',T104,'J5',T112,'J6',T125,'PREMIO', /,
     *         1X, 130('-'),/)

C REPORT HEADERL2 (totobola extra)
32      FORMAT(1X, 130('='), /,
     *         1X, 'SCML - Departamento de Jogos ', T44, A54, T116, 'Data: ', I2.2, '.', I2.2, '.', I4.4,/,
     *         1X, '                            ', T55, A, 1X, I2.2, '/', I4.4, 1X, 4A4,/,
     *         1X, 130('='), /,
     *         1X, T123,'VALOR DO', /,
     *         1X, T3,'CODIGO APOSTA',T22,'N. MAQUINA',T39,'S14',
     *             T49,'1',T57,'2',T65,'3',T125,'PREMIO', /,
     *         1X, 130('-'),/)

C REPORT HEADERL3 (joker)
33      FORMAT(1X, 130('='), /,
     *         1X, 'SCML - Departamento de Jogos', T44, A54, T116, 'Data: ', I2.2, '.', I2.2, '.', I4.4,/,
     *         1X, '                            ', T60, A, 1X, I2.2, '/', I4.4, 1X, 4A4,/,
     *         1X, 130('='), /,
     *         1X, T123,'VALOR DO', /,
     *         1X, T3, 'CODIGO APOSTA',T22,'N. MAQUINA',T44,'1',
     *             T56,'2',T68,'3',T80,'4',T92,'5',T104,'6',T125,'PREMIO', /,
     *         1X, 130('-'),/)

C REPORT FOOTERL1 (totoloto quarta e sabado)
36      FORMAT(/,1X,130('-'), /, /,
     *         1X,T7,'TOTAIS',T33,6I12,T118,A13, /,
     *         1X, /,
     *         1X,T7,'TOTAL DE APOSTAS PREMIADAS',T38,I7)

C REPORT FOOTERL2 (totobola normaL)
37      FORMAT(/,1X,130('-'), /, /,
     *         1X,T7,'TOTAIS',T34,10I8,T118,A13, /,
     *         1X, /,
     *         1X,T7,'TOTAL DE APOSTAS PREMIADAS',T35,I7)

C REPORT FOOTERL3 (joker)
38      FORMAT(/,1X,130('-'), /, /,
     *         1X,T7,'TOTAIS',T33,6I12,T118,A13, /,
     *         1X, /,
     *         1X,T7,'TOTAL DE APOSTAS PREMIADAS',T38,I7)

C REPORT FOOTERL4 (totobola extra)
39      FORMAT(/,1X,130('-'), /, /,
     *         1X,T7,'TOTAIS',T34,4I8,T118,A13, /,
     *         1X, /,
     *         1X,T7,'TOTAL DE APOSTAS PREMIADAS',T35,I7)
C----|--!--------------------------------------------------------------
C V02   !  FORMATS FOR REL_PA and REL_PA23 files - end
C----|--!--------------------------------------------------------------
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
C REPORT HEADERL (totoloto quarta e sabado)
303     FORMAT(1X, 130('='), /,
     *         1X, 'SCML - Departamento de Jogos', T37, A70, T116, 'Data: ', I2.2, '.', I2.2, '.', I4.4,/,
     *         1X, '                            ', T56, A, 1X, I3.3, '/', I4.4, 1X, 4A4,/,
     *         1X, 130('='), /,
     *         1X, T120,'VALOR BRUTO', /,
     *         1X, T3, 'CODIGO APOSTA',T22,'N. MAQUINA',T44,'1',
     *             T56,'2',T68,'3',T80,'4',T92,'5',T103,'NS',T122,'DO PREMIO', /,
     *         1X, 130('-'),/)

C REPORT HEADERL1 (totobola normal)
313     FORMAT(1X, 130('='), /,
     *         1X, 'SCML - Departamento de Jogos ', T37, A70, T116, 'Data: ', I2.2, '.', I2.2, '.', I4.4,/,
     *         1X, '                            ', T55, A, 1X, I2.2, '/', I4.4, 1X, 4A4,/,
     *         1X, 130('='), /,
     *         1X, T120,'VALOR BRUTO', /,
     *         1X, T3,'CODIGO APOSTA',T22,'N. MAQUINA',T39,'S14',
     *             T49,'1',T57,'2',T65,'3',T72,'J1',T80,'J2',T88,'J3',
     *             T96,'J4',T104,'J5',T112,'J6',T122,'DO PREMIO', /,
     *         1X, 130('-'),/)

C REPORT HEADERL2 (totobola extra)
323     FORMAT(1X, 130('='), /,
     *         1X, 'SCML - Departamento de Jogos ', T37, A70, T116, 'Data: ', I2.2, '.', I2.2, '.', I4.4,/,
     *         1X, '                            ', T55, A, 1X, I2.2, '/', I4.4, 1X, 4A4,/,
     *         1X, 130('='), /,
     *         1X, T120,'VALOR BRUTO', /,
     *         1X, T3,'CODIGO APOSTA',T22,'N. MAQUINA',T39,'S14',
     *             T49,'1',T57,'2',T65,'3',T122,'DO PREMIO', /,
     *         1X, 130('-'),/)

C REPORT HEADERL3 (joker)
333     FORMAT(1X, 130('='), /,
     *         1X, 'SCML - Departamento de Jogos', T37, A70, T116, 'Data: ', I2.2, '.', I2.2, '.', I4.4,/,
     *         1X, '                            ', T60, A, 1X, I2.2, '/', I4.4, 1X, 4A4,/,
     *         1X, 130('='), /,
     *         1X, T120,'VALOR BRUTO', /,
     *         1X, T3, 'CODIGO APOSTA',T22,'N. MAQUINA',T44,'1',
     *             T56,'2',T68,'3',T80,'4',T92,'5',T104,'6',T122,'DO PREMIO', /,
     *         1X, 130('-'),/)

C REPORT FOOTERL1 (totoloto quarta e sabado)
363     FORMAT(/,1X,130('-'), /, /,
     *         1X,T7,'TOTAIS',T33,6I12,T118,A13, /,
     *         1X, /,
     *         1X,T7,'TOTAL DE APOSTAS PREMIADAS',T38,I7)

C REPORT FOOTERL2 (totobola normaL)
373     FORMAT(/,1X,130('-'), /, /,
     *         1X,T7,'TOTAIS',T34,10I8,T118,A13, /,
     *         1X, /,
     *         1X,T7,'TOTAL DE APOSTAS PREMIADAS',T35,I7)

C REPORT FOOTERL3 (joker)
383     FORMAT(/,1X,130('-'), /, /,
     *         1X,T7,'TOTAIS',T33,6I12,T118,A13, /,
     *         1X, /,
     *         1X,T7,'TOTAL DE APOSTAS PREMIADAS',T38,I7)

C REPORT FOOTERL4 (totobola extra)
393     FORMAT(/,1X,130('-'), /, /,
     *         1X,T7,'TOTAIS',T34,4I8,T118,A13, /,
     *         1X, /,
     *         1X,T7,'TOTAL DE APOSTAS PREMIADAS',T35,I7)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------

       END


C       **************************************************
C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE OPEN_FILASC(PFILENAM,PUNIT,ST)
        IMPLICIT NONE
C       **************************************************

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'

        INTEGER*4 ST, PUNIT
        CHARACTER*30 PFILENAM

        OPEN (UNIT       =  PUNIT,
     *        FILE       = PFILENAM,
     *        IOSTAT     = ST,
     *        FORM       = 'FORMATTED',
     *        RECL       = 135,
     *        STATUS     = 'NEW',
     *        RECORDTYPE = 'STREAM_CR')

        IF(ST.NE.0) THEN
          TYPE *, IAM()
          TYPE *, IAM(), 'SOUPWIN- Erro ao criar/abrir: ', PFILENAM
          TYPE *, IAM(),'                '
          CALL USRCLOS1(PUNIT)
          CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
        RETURN
        END


C       **************************************************
C====== OPTIONS /CHECK=NOOVERFLOW
C       **************************************************
      SUBROUTINE OPEN_FILE(FILNAM,LUN,ST)
         IMPLICIT NONE
C
         CHARACTER*(*) FILNAM
         INTEGER*4 ST,LUN
C
              OPEN (UNIT           =  LUN,
     *              FILE           =  FILNAM,
     *              STATUS         = 'NEW',
     *              ORGANIZATION   = 'SEQUENTIAL',
     *              ACCESS         = 'SEQUENTIAL',
     *              FORM           = 'FORMATTED',
     *              RECORDTYPE     = 'STREAM_CR',
     *              DISPOSE        = 'KEEP',
     *              IOSTAT         =  ST)

         RETURN
      END


C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
C*************************************************
C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE WRITEV_AM(VALREC,PUNIT,VDETAIL,CNTREC,ST,
     *               ESHRARR,EKIKSHRARR,
     *               FLUNREP,FLUNREP23,FLUNREP3,
     *               REPORTFLAG,
     *               REPTOTDIV,REPTOTDIV23,REPTOTDIV3,
     *               FLAGREP, GNUM)
        IMPLICIT NONE
C**************************************************
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

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

        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
        INCLUDE 'INCLIB:DBNREC.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'

C  LOCAL VARIABLES

        INTEGER*2      AUX
        INTEGER*4      AGT
        INTEGER*4      I
        INTEGER*4      CNTREC
        INTEGER*4      PUNIT
        INTEGER*4      DIV,SHR,KIK,SHRAMT
        INTEGER*4      GTYP, GNUM
        INTEGER*4      DRW
        INTEGER*4      SSER              ! WAGER SERIAL NUMBER
        INTEGER*4      SCHK              ! WAGER CHECK DIGITS
        INTEGER*4      TOTNUMPRIZES
        INTEGER*2      DAT(12)
        INTEGER*4      ST
        INTEGER*4      CHANNEL, TEMPNETPRIZE, TEMPNETKICKER
        INTEGER*4      PORTALSAP
        PARAMETER     (PORTALSAP=007456)

        CHARACTER*79   CPRIZES(VMAX)

        CHARACTER*99   ZEROUMSTRING
        INTEGER*4      ESHRARR(*)    ! Array with shares from game file
        INTEGER*4      EKIKSHRARR(*) ! Array with shares from kicker

        INTEGER*4      FLUNREP,FLUNREP23 ! REL_PA & REL_PA23 LUNS
        INTEGER*4      REPORTFLAG    ! SET REPORT GENERATION FLAG
        INTEGER*4      MAXREPDIVCOUNT
           PARAMETER(MAXREPDIVCOUNT=12)
        INTEGER*4      REPDIVCOUNT(MAXREPDIVCOUNT) ! REPORT DIV COUNTER
        INTEGER*4      REPTOTDIV(*),REPTOTDIV23(*) ! REPORT TOTAL DIV COUNT STRUCTS
        INTEGER*4      KCOUNT, KITER ! COUNTER & ITERATOR FOR REPDIVCOUNT STRUCT
        INTEGER*4      SPTKIKPRIZE   ! TOTOBOLA AND JOKER PRIZES SUM
        INTEGER*4      FLAGREP        ! REL_PA REC GEN FLAG
        INTEGER*4      FLAGREP23        ! REL_PA23 REC GEN FLAG

        CHARACTER*32   DMONY       ! FORMAT CURRENCY FUNCTION
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
        INTEGER*4      FLUNREP3        ! REL_PA3 LUN
        INTEGER*4      FLAGREP3        ! REL_PA3 REC GEN FLAG
        INTEGER*4      REPTOTDIV3(*)   ! REPORT TOTAL DIV COUNT STRUCT
        ! We really should get these values from the P(...) global
        ! structure, but in order to avoid run-time dependencies, we
        ! will hammer it this way:
        INTEGER*4      P_VALORDER      ! Should be P(VALORDER)
        PARAMETER     (P_VALORDER = 15001)
        INTEGER*4      P_VALPRZHI      ! Should be P(VALPRZHI)
        PARAMETER     (P_VALPRZHI = 500000)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------

        GTYP = VALREC(VGTYP)
        AGT = AGTTAB(AGTNUM,VALREC(VSTER))
        AUX = 1


C CHECK CHANNEL TYPE
C ********************************************************************************* REMOVE ON FINAL VERSION
C        IF(AGT.GE.1040.AND.AGT.LE.1060) THEN
C ********************************************************************************* REMOVE ON FINAL VERSION

C ********************************************************************************* UNCOMMENT ON FINAL VERSION
        IF ( AGTSAP(VALREC(VSTER)).EQ. PORTALSAP ) THEN
C ********************************************************************************* UNCOMMENT ON FINAL VERSION
           CHANNEL = 2
        ELSE
           CHANNEL = 1
        ENDIF

C Check if Netprize exists
        IF (VALREC(VOPSAMT).EQ.0) THEN
           TEMPNETPRIZE = VALREC(VPAMT)
        ELSE
           TEMPNETPRIZE = VALREC(VOPSAMT)
        ENDIF

C Check if KICKER Netprize exists
        IF (VALREC(VKOPSAMT).EQ.0) THEN
           TEMPNETKICKER = VALREC(VKPAMT)
        ELSE
           TEMPNETKICKER = VALREC(VKOPSAMT)
        ENDIF


C----|--!--------------------------------------------------------------
C V02   !  INIT RECORD DIV REC COUNT STRUCT - start
C----|--!--------------------------------------------------------------
        IF(REPORTFLAG) THEN
C RESET REPDIVCOUNT STRUCT
           KCOUNT=1
           DO WHILE(KCOUNT .LE. MAXREPDIVCOUNT)
              REPDIVCOUNT(KCOUNT) = 0
              KCOUNT = KCOUNT + 1
           ENDDO
C SET DIV ITERATOR FOR EACH GAME TYPE
           FLAGREP23 = 0
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           FLAGREP3 = 0
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           IF(GNUM .EQ. 6 .OR. GNUM .EQ. 7) THEN
              KITER = 6
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
C              IF(VALREC(VPAMT) .GT. 15000) THEN
              IF(VALREC(VPAMT) .GE. P_VALORDER) THEN ! Source: OUTVAL.FOR
                 FLAGREP23 = 1
              ENDIF
              IF(VALREC(VPAMT) .GE. P_VALPRZHI) THEN ! Source: OUTVAL.FOR
                 FLAGREP3 = 1
              ENDIF
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           ELSEIF(GNUM .EQ. 5) THEN
              KITER = 6
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
C              IF(VALREC(VKPAMT) .GT. 15000) THEN
              IF(VALREC(VKPAMT) .GE. P_VALORDER) THEN ! Source: OUTVAL.FOR
                 FLAGREP23 = 1
              ENDIF
              IF(VALREC(VKPAMT) .GE. P_VALPRZHI) THEN ! Source: OUTVAL.FOR
                 FLAGREP3 = 1
              ENDIF
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           ELSEIF(GNUM .EQ. 1) THEN
              KITER = 10
              SPTKIKPRIZE = VALREC(VPAMT)+VALREC(VKPAMT)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
C              IF(SPTKIKPRIZE .GT. 15000) THEN
              IF(SPTKIKPRIZE .GE. P_VALORDER) THEN ! Source: OUTVAL.FOR
                 FLAGREP23 = 1
              ENDIF
              IF(SPTKIKPRIZE .GE. P_VALPRZHI) THEN ! Source: OUTVAL.FOR
                 FLAGREP3 = 1
              ENDIF
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           ELSEIF(GNUM .EQ. 10) THEN
              KITER = 4
              SPTKIKPRIZE = VALREC(VPAMT)
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
C              IF(SPTKIKPRIZE .GT. 15000) THEN
              IF(SPTKIKPRIZE .GE. P_VALORDER) THEN ! Source: OUTVAL.FOR
                 FLAGREP23 = 1
              ENDIF
              IF(SPTKIKPRIZE .GE. P_VALPRZHI) THEN ! Source: OUTVAL.FOR
                 FLAGREP3 = 1
              ENDIF
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           ENDIF

        ENDIF
C----|--!--------------------------------------------------------------
C V02   !  INIT DIV COUNT STRUCT - end
C----|--!--------------------------------------------------------------

C PRINT VLF INFORMATION
C**********************
        TOTNUMPRIZES=0
C PROCESS VALREC DETAILS
        DO 100 I=1,VALREC(VPZOFF)

C GET DIVISION NUMBER
           DIV = VDETAIL(VDIV,I)

C GET NUMBER OF PRIZES PER DEIVISION
           SHR = VDETAIL(VSHR,I)

C GET DRAW NUMBER FROM DETAIL
           DRW = VDETAIL(VDRW,I)

C CHECK IF IT IS JOKER PRIZE
           KIK = VDETAIL(VKIK,I)
           IF(KIK.EQ.0) THEN
C GET SHARE AMOUNT VALUE OF MAIN GAME
              SHRAMT = ESHRARR(DIV)
           ELSEIF(KIK.EQ.1) THEN
              SHRAMT = EKIKSHRARR(DIV)
           ENDIF

C SUM TOTAL NUMBER OF PRIZES PER VALREC
           TOTNUMPRIZES = TOTNUMPRIZES + SHR

C WRITE 02 RECORD IN TMP ARRAY - CPRIZES
           WRITE (CPRIZES(AUX),800) DIV,SHR,SHRAMT,KIK

           AUX = AUX + 1

C----|--!--------------------------------------------------------------
C V02   !  WRITE NUM PRIZES IN DIV COUNT STRUCT - start
C----|--!--------------------------------------------------------------
        IF(REPORTFLAG) THEN
           IF(KIK.EQ.0 .OR. GNUM .EQ. 5) THEN
              REPDIVCOUNT(DIV) = SHR
C SUM DIV # OF PRIZES
              IF(FLAGREP) THEN
                 REPTOTDIV(DIV) = REPTOTDIV(DIV) + SHR
              ENDIF
              IF(FLAGREP23) THEN
                 REPTOTDIV23(DIV) = REPTOTDIV23(DIV) + SHR
              ENDIF
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
              IF(FLAGREP3) THEN
                 REPTOTDIV3(DIV) = REPTOTDIV3(DIV) + SHR
              ENDIF
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           ELSEIF(KIK.EQ.1 .AND. GNUM .EQ. 1) THEN
              REPDIVCOUNT(DIV+4) = SHR
C SUM DIV # OF PRIZES
              IF(FLAGREP) THEN
                 REPTOTDIV(DIV+4) = REPTOTDIV(DIV+4) + SHR
              ENDIF
              IF(FLAGREP23) THEN
                 REPTOTDIV23(DIV+4) = REPTOTDIV23(DIV+4) + SHR
              ENDIF
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
              IF(FLAGREP3) THEN
                 REPTOTDIV3(DIV) = REPTOTDIV3(DIV) + SHR
              ENDIF
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
           ENDIF
        ENDIF


C----|--!--------------------------------------------------------------
C V02   !  WRITE NUM PRIZES IN DIV COUNT STRUCT - end
C----|--!--------------------------------------------------------------


100     CONTINUE

C CONVERT SALES DATE
           DAT(VCDC) = VALREC(VSCDC)
           CALL CDATE(DAT)
C CONVERT INTERNAL SERIAL TO EXTERNAL SERIAL
           CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),SSER,SCHK)


600     CONTINUE

           WRITE(PUNIT,1900)
     *                CHANNEL,             ! CHANNEL ID (1)
     *                AGT,                 ! AGT NUMBER (7)
     *                2000+DAT(VYEAR),     ! SELLING YEAR (4)
     *                DAT(VMON),           ! SELLING MOTH (2)
     *                DAT(VDAY),           ! SELLING DAY (2)
     *                VALREC(VSCDC),       ! WAGER CDC DATE (4)
     *                VALREC(VSSER),       ! INTERNAL SERIAL NUMBER (8)
     *                VALREC(VPAMT),       ! PRIZE AMOUNT (11)
     *                TEMPNETPRIZE,        ! NET PAY AMOUNT (11)
     *                VALREC(VKPAMT),      ! KICKER AMT (11)
     *                TEMPNETKICKER,       ! KICKER NET AMT (11)
     *                TOTNUMPRIZES         ! TOTAL NUMBER OF PRIZES (4)

           CNTREC = CNTREC + 1

           DO 1001 I=1,AUX-1
              WRITE(PUNIT,810) CPRIZES(I)
              CNTREC = CNTREC + 1
1001       CONTINUE


C----|--!--------------------------------------------------------------
C V02   !  WRITE RECORD LINE IN REL_PA and REL_PA23 files - start
C----|--!--------------------------------------------------------------
        IF(REPORTFLAG) THEN
           KCOUNT=0
C TOTOLOTO
           IF(GNUM .EQ. 6 .OR. GNUM .EQ. 7) THEN
              IF(FLAGREP) THEN
                 WRITE(FLUNREP,40) DAT(VJUL),SSER,AGT / 100000,
     *                             MOD(AGT,100000),
     *                             (REPDIVCOUNT(KCOUNT),KCOUNT=1,KITER),
     *                             DMONY(VALREC(VPAMT))
C INCREMENT STATISTICS
                 REPTOTDIV(12) = REPTOTDIV(12) + VALREC(VPAMT)
                 REPTOTDIV(11) = REPTOTDIV(11) + 1
              ENDIF
C CHECK IF PRIZE IS GREATER THAN 150
              IF(FLAGREP23) THEN
                 WRITE(FLUNREP23,40) DAT(VJUL),SSER,AGT / 100000,
     *                               MOD(AGT,100000),
     *                               (REPDIVCOUNT(KCOUNT),KCOUNT=1,KITER),
     *                               DMONY(VALREC(VPAMT))
C INCREMENT STATISTICS
                 REPTOTDIV23(12) = REPTOTDIV23(12) + VALREC(VPAMT)
                 REPTOTDIV23(11) = REPTOTDIV23(11) + 1
              ENDIF
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
C CHECK IF PRIZE IS GREATER THAN 5000
              IF(FLAGREP3) THEN
                 WRITE(FLUNREP3,40) DAT(VJUL),SSER,AGT / 100000,
     *                               MOD(AGT,100000),
     *                               (REPDIVCOUNT(KCOUNT),KCOUNT=1,KITER),
     *                               DMONY(VALREC(VPAMT))
C INCREMENT STATISTICS
                 REPTOTDIV3(12) = REPTOTDIV3(12) + VALREC(VPAMT)
                 REPTOTDIV3(11) = REPTOTDIV3(11) + 1
              ENDIF
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------

           ELSEIF(GNUM .EQ. 1) THEN
C TOTBOLA NORMAL
              IF(FLAGREP) THEN
                 WRITE(FLUNREP,41) DAT(VJUL),SSER,AGT / 100000,
     *                             MOD(AGT,100000),
     *                             (REPDIVCOUNT(KCOUNT),KCOUNT=1,KITER),
     *                             DMONY(SPTKIKPRIZE)
C INCREMENT STATISTICS
                 REPTOTDIV(12) = REPTOTDIV(12) + SPTKIKPRIZE
                 REPTOTDIV(11) = REPTOTDIV(11) + 1
              ENDIF
C CHECK IF PRIZE IS GREATER THAN 150
              IF(FLAGREP23) THEN
                 WRITE(FLUNREP23,41) DAT(VJUL),SSER,AGT / 100000,
     *                               MOD(AGT,100000),
     *                               (REPDIVCOUNT(KCOUNT),KCOUNT=1,KITER),
     *                               DMONY(SPTKIKPRIZE)
C INCREMENT STATISTICS
                 REPTOTDIV23(12) = REPTOTDIV23(12) + SPTKIKPRIZE
                 REPTOTDIV23(11) = REPTOTDIV23(11) + 1
              ENDIF
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
C CHECK IF PRIZE IS GREATER THAN 5000
              IF(FLAGREP3) THEN
                 WRITE(FLUNREP3,41) DAT(VJUL),SSER,AGT / 100000,
     *                               MOD(AGT,100000),
     *                               (REPDIVCOUNT(KCOUNT),KCOUNT=1,KITER),
     *                               DMONY(SPTKIKPRIZE)
C INCREMENT STATISTICS
                 REPTOTDIV3(12) = REPTOTDIV3(12) + SPTKIKPRIZE
                 REPTOTDIV3(11) = REPTOTDIV3(11) + 1
              ENDIF
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------

C TOTBOLA EXTRA
           ELSEIF(GNUM .EQ. 10) THEN
              IF(FLAGREP) THEN
                 WRITE(FLUNREP,43) DAT(VJUL),SSER,AGT / 100000,
     *                             MOD(AGT,100000),
     *                             (REPDIVCOUNT(KCOUNT),KCOUNT=1,KITER),
     *                             DMONY(SPTKIKPRIZE)
C INCREMENT STATISTICS
                 REPTOTDIV(12) = REPTOTDIV(12) + SPTKIKPRIZE
                 REPTOTDIV(11) = REPTOTDIV(11) + 1
              ENDIF
C CHECK IF PRIZE IS GREATER THAN 150
              IF(FLAGREP23) THEN
                 WRITE(FLUNREP23,43) DAT(VJUL),SSER,AGT / 100000,
     *                               MOD(AGT,100000),
     *                               (REPDIVCOUNT(KCOUNT),KCOUNT=1,KITER),
     *                               DMONY(SPTKIKPRIZE)
C INCREMENT STATISTICS
                 REPTOTDIV23(12) = REPTOTDIV23(12) + SPTKIKPRIZE
                 REPTOTDIV23(11) = REPTOTDIV23(11) + 1
              ENDIF
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
C CHECK IF PRIZE IS GREATER THAN 5000
              IF(FLAGREP3) THEN
                 WRITE(FLUNREP3,43) DAT(VJUL),SSER,AGT / 100000,
     *                               MOD(AGT,100000),
     *                               (REPDIVCOUNT(KCOUNT),KCOUNT=1,KITER),
     *                               DMONY(SPTKIKPRIZE)
C INCREMENT STATISTICS
                 REPTOTDIV3(12) = REPTOTDIV3(12) + SPTKIKPRIZE
                 REPTOTDIV3(11) = REPTOTDIV3(11) + 1
              ENDIF
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------

C JOKER
           ELSEIF(GTYP .EQ. TKIK) THEN
              IF(FLAGREP) THEN
                 WRITE(FLUNREP,42) DAT(VJUL),SSER,AGT / 100000,
     *                             MOD(AGT,100000),
     *                             (REPDIVCOUNT(KCOUNT),KCOUNT=1,KITER),
     *                             DMONY(VALREC(VKPAMT))
C INCREMENT STATISTICS
                 REPTOTDIV(12) = REPTOTDIV(12) + VALREC(VKPAMT)
                 REPTOTDIV(11) = REPTOTDIV(11) + 1
              ENDIF
C CHECK IF PRIZE IS GREATER THAN 150
              IF(FLAGREP23) THEN
                 WRITE(FLUNREP23,42) DAT(VJUL),SSER,AGT / 100000,
     *                               MOD(AGT,100000),
     *                               (REPDIVCOUNT(KCOUNT),KCOUNT=1,KITER),
     *                               DMONY(VALREC(VKPAMT))
C INCREMENT STATISTICS
                 REPTOTDIV23(12) = REPTOTDIV23(12) + VALREC(VKPAMT)
                 REPTOTDIV23(11) = REPTOTDIV23(11) + 1
              ENDIF
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------
C CHECK IF PRIZE IS GREATER THAN 5000
              IF(FLAGREP3) THEN
                 WRITE(FLUNREP3,42) DAT(VJUL),SSER,AGT / 100000,
     *                               MOD(AGT,100000),
     *                               (REPDIVCOUNT(KCOUNT),KCOUNT=1,KITER),
     *                               DMONY(VALREC(VKPAMT))
C INCREMENT STATISTICS
                 REPTOTDIV3(12) = REPTOTDIV3(12) + VALREC(VKPAMT)
                 REPTOTDIV3(11) = REPTOTDIV3(11) + 1
              ENDIF
C----|--!--------------------------------------------------------------
C V03| Generation of new level 3 prizes report
C----|--!--------------------------------------------------------------

           ENDIF

        ENDIF
C----|--!--------------------------------------------------------------
C V02   !  WRITE RECORD LINE IN REL_PA and REL_PA23 files - end
C----|--!--------------------------------------------------------------

C exit subroutine write_am
           RETURN


C FORMAT FOR LOTO
1900    FORMAT('01',I1.1,I7.7,I4.4,I2.2,I2.2,I4.4,I9.9,
     *              I11.11,I11.11,I11.11,I11.11,I4.4)
C TEMP 02 RECORD
800     FORMAT('02',I2.2,I5.5,I11.11,I1,58(' '))
C FINAL 02 RECORD
810     FORMAT(A79)

C----|--!--------------------------------------------------------------
C V02   !  FORMATS FOR REL_PA and REL_PA23 files - start
C----|--!--------------------------------------------------------------
C REPORT BODYL1(totoloto)
40      FORMAT(1X,I3.3,'-',I8.8,'-','***',T24,I2.2,'-',I5.5,T33,6I12,T118,A13)

C REPORT BODYL2(totobola normal)
41      FORMAT(1X,I3.3,'-',I8.8,'-','***',T24,I2.2,'-',I5.5,T34,10I8,T118,A13)

C REPORT BODYL3(joker)
42      FORMAT(1X,I3.3,'-',I8.8,'-','***',T24,I2.2,'-',I5.5,T33,6I12,T118,A13)

C REPORT BODYL4(totobola extra)
43      FORMAT(1X,I3.3,'-',I8.8,'-','***',T24,I2.2,'-',I5.5,T34,4I8,T118,A13)
C----|--!--------------------------------------------------------------
C V02   !  FORMATS FOR REL_PA and REL_PA23 files - end
C----|--!--------------------------------------------------------------

        END

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
          TYPE*,IAM(),'Error openning SCF.FIL > ',ST
          RETURN
        ENDIF
C
        CALL READW(FDB,1,SCFREC,ST)
        IF (ST.NE.0) THEN
          TYPE*,IAM(),'Error reading SCF.FIL > ',ST
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
          WRITE(5,913)
          WRITE(5,*)
          CALL PRMNUM(' Enter option: ',GAMSEL,1,GCNT+2,ST)
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
913     FORMAT(18X,' E - Exit')

C
        END


CC=======================================================================
CC=======================================================================
CC=======OPTIONS /CHECK=NOOVERFLOW
CC    FUNCTION: DMONY
CC       INPUT: INTEGER*4   (AMOUNT IN CENTS - 1000000000)
CC      OUTPUT: CHARATER*32 (AMOUNT IN CURRENCY FORMAT - 10.000.000,00)
CC=======================================================================
C        CHARACTER FUNCTION DMONY*32(IN_VALUE)
C        IMPLICIT NONE
C        INCLUDE 'INCLIB:SYSPARAM.DEF'
C        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C        INCLUDE 'INCLIB:GLOBAL.DEF'
C
C
C        INTEGER*4 IN_VALUE
C        CHARACTER*32 OUT_BUF
C        CHARACTER*32 AUX_OUT
C
C        INTEGER*4 I,J,DIG_CNT
C        INTEGER*4 AUX
C        INTEGER*4 REMAIN
C        INTEGER*4 DIGIT
C        CHARACTER*1 CH
C
C        DO I = 1, 32
C            AUX_OUT(I:I) = ' '        ! reset to SPACES
C            OUT_BUF(I:I) = ' '        ! reset to SPACES
C        ENDDO
C
C        AUX = ABS(IN_VALUE)
C        I = 1                        !!iterator counter
C        J = 1                        !!struct position
C        DIG_CNT = 1
C        DO WHILE(AUX .NE. 0)
C            REMAIN = AUX / 10
C            DIGIT = MOD(AUX,10)
C            CH = CHAR(48 + DIGIT)
CC           SET ',' CHAR AT 3RD POSITION
C            IF(I .EQ. 2) THEN
C               DIG_CNT = 1
C               AUX_OUT(J:J) = CH
C               J = J + 1
C               CH = ','
C               AUX_OUT(J:J) = CH
C               J = J + 1
CC           SET '.' AT 7TH AND 11TH POSITION
C            ELSEIF(MOD(DIG_CNT-1,3) .EQ. 0 .AND. DIG_CNT .NE. 1) THEN
C               AUX_OUT(J:J) = '.'
C               J = J + 1
C               AUX_OUT(J:J) = CH
C               DIG_CNT = DIG_CNT + 1
C               J = J + 1
C            ELSE
C               AUX_OUT(J:J) = CH
C               J = J + 1
C               DIG_CNT = DIG_CNT + 1
C            ENDIF
C            I = I + 1
C            AUX = REMAIN
C        ENDDO
C
CC       IF ONLY ONE DIGIT FORCE LEFT ZERO(S)
C        IF(I .EQ. 1) THEN
C           AUX_OUT(1:4) = '00,0'
C        ENDIF
C
CC       IF ONLY ONE DIGIT FORCE LEFT ZERO(S)
C        IF(I .EQ. 2) THEN
C           AUX_OUT(2:4) = '0,0'
C        ENDIF
CC       IF ONLY TWO DIGITS FORCE LEFT ZERO(S)
C        IF(I .EQ. 3) THEN
C           AUX_OUT(4:4) = '0'
C        ENDIF
C
CC       INVERT STRING
C        J=13
C        DO I = 1, J
C            OUT_BUF(I:I) = AUX_OUT(J+1-I:J+1-I)
C        ENDDO
C
CC       RETURN OUT_BUF
C        DMONY=OUT_BUF
C
C        RETURN
C
C        END
