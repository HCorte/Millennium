CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SOUPWINALL.FOR
C CHANGE LOG:
C ---+-------------+----+----------------------------------------------
C VER|DATE         |USER|DESCRIPTION       
C ---+-------------+----+----------------------------------------------
C V01 26-JAN-2014   SCML  INITIAL RELEASE OF SOUPWINPASALL PROGRAM 
C
C THIS SUBROUTINE SHOULD ONLY BE EXECUTED ONCE. THIS IS A KIND OF 
C CONTINGENCY OF SOUPWIN ROUTINE. THIS ROUTINE PROCESS THE VPF FILE
C EXISTING IN THE SYSTEM AT THE EXECUTION TIME AND SEARCH
C FOR SETTLED PRIZES NOT YET PROCESSED. AS OUTPUT GENERATES ALL THE 
C SOUP_AM_PA FILES FOR SOUP PLATFORM.
C
C    INPUT:  VLF.FIL
C
C    OUTPUT: SOUP_AM_PA_GGSSSAAAA.ASC
C       ( GG=GAME / SSS=SORTEIO / AAAA=ANO )
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2003 DJ - SCML. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C====== OPTIONS /CHECK=NOOVERFLOW
        PROGRAM SOUPWINALL
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

        CHARACTER*7   CCCAAAA, CONCURSOANO, AAAACCC
        CHARACTER*9   GGAAAACCC
        INTEGER*4     CONCURSO, ANO
        INTEGER*4     DRWN, DRWNKIK, GETDRW       !FUNCTION  
        INTEGER*4     GNUM, GTYP, VST,GIND
           DATA       GTYP/0/
        INTEGER*4     INDDRW, DRWVDT
        INTEGER*4     ST
        INTEGER*4     TUBSIZ
           PARAMETER (TUBSIZ=I4BUCSIZ*7)
        INTEGER*4     VLFBUF(TUBSIZ)
        INTEGER*4     DAT(8)    ! SYSDATE FROM MACHINE
        INTEGER*2     BEGDAT(12),ENDAT(12)
        INTEGER*2     DRDAT(LDATE_LEN) ! DRAWING DATE
        INTEGER*4     CNTREC, CTIM(2)
        CHARACTER*2   TPREC
        INTEGER*4     PUNIT
        CHARACTER*30  PFILENAM ! INTERFACE FILE NAME
        CHARACTER*16  GAMLNAM(MAXGAM)  ! GAME LONG NAME
        LOGICAL       VALDRW
        INTEGER*4     WEEK, YEAR
        INTEGER*4     FLUN, FLUNKIK,GAMSTS,GAMSTSKIK,GAMSEC
        EQUIVALENCE  (DLTREC,DSPREC,GAMSTS)
        EQUIVALENCE  (DKKREC,GAMSTSKIK)
        INTEGER*4     GNUMKIK
           PARAMETER (GNUMKIK=5)
        INTEGER*4     I, MGDIVNUM, KIKDIVNUM, IARR, IARR2
        CHARACTER*50  C50DIV
        INTEGER*4     CUCSH, CPPAY, CNT_PRM
        LOGICAL*4     ERRORFLAG/.FALSE./   ! ERROR FLAG CONTROL
        CHARACTER*256 LIBCMD        ! DIR GEN FILES COMMAND
        CHARACTER*1   ENTER ! INITIAL MESSAGE READ VARIABLE

C STRUCT FOR DRAW ID INDEX
        INTEGER*4     MAXDRWID
           PARAMETER (MAXDRWID=1000)
        INTEGER*4     DRWINDEX(MAXDRWID)
        INTEGER*4     MAININDDRW, TMPDRWID

C MULTI PROCESS ITERATION STRUCT
        INTEGER*4     MMODEARR(5)  ! ARRAY WITH ALL PASSIVE GAME NUMBERS
           DATA MMODEARR(1)/1/
           DATA MMODEARR(2)/5/
           DATA MMODEARR(3)/6/
           DATA MMODEARR(4)/7/
           DATA MMODEARR(5)/10/
        INTEGER*4     IPROC, MAXPROC, MODEFLAG
           DATA MODEFLAG/0/

C
C SHOW TITLE
C***************

        CALL CLRSCR(5)
        TYPE*,IAM()
        TYPE*,IAM(),'*******************************************************'
        TYPE*,IAM(),'                                                       '
        TYPE*,IAM(),'                 INTERFACE COM O SOUP                  '
        TYPE*,IAM(),'  GERACAO DE TODOS OS FICHEIROS DE PREMIOS ATRIBUIDOS  '
        TYPE*,IAM(),'                                                       '
        TYPE*,IAM(),'*******************************************************'
        TYPE*,IAM(),'* * * * * * * * * * * * * * * * * * * * * * * * * * * *'
        TYPE*,IAM(),'                     ATENCAO ! ! !                     '
        TYPE*,IAM(),'* * * * * * * * * * * * * * * * * * * * * * * * * * * *'
        TYPE*,IAM(),'*******************************************************'
        TYPE*,IAM(),'                                                       '
        TYPE*,IAM(),'      ESTE PROCESSO SO DEVE SER EXECUTADO UMA VEZ      '
        TYPE*,IAM(),'                                                       '
        TYPE*,IAM(),'            EXECUTAR ANTES DA PRIMEIRA EXECUCAO        '
        TYPE*,IAM(),'                   DO PROGRAMA SOUPWIN!                '
        TYPE*,IAM(),'                                                       '
        TYPE*,IAM(),'  TODOS OS FICHEIROS VLF/VPF EXISTENTES NA DIRECTORIA  '
        TYPE*,IAM(),'            DE INPUT IRAO SER PROCESSADOS!!!           '
        TYPE*,IAM(),'                                                       '
        TYPE*,IAM(),'*******************************************************'
        TYPE*,IAM()

99     CONTINUE
C ASK USER TO PROCED AFTER MESSAGE READ
        CALL WIMG(5,'PRIMA ENTER PARA CONTINUAR: ')
        READ(5,'(A1)') ENTER
        IF(TRIM(ENTER).EQ.'E'.OR.TRIM(ENTER).EQ.'e') THEN
           CALL GSTOP(GEXIT_OPABORT)
        ENDIF
C CHECK IF ENTER WAS INSERTED
        IF(LEN(TRIM(ENTER)).NE.0) THEN
           GOTO 99
        ENDIF


100     CONTINUE

C CALL GAME MAIN MENU
        CALL GAME_TYPNDX(GNUM, GTYP, GIND, GAMLNAM, ST)
        IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
        IF (GNUM.EQ.8.OR.GNUM.EQ.9.OR.GNUM.EQ.30) THEN
            CALL SOUPWINPASALL (GNUM,ST)
            IF (ST .NE. 0) THEN
               CALL GSTOP (GEXIT_FATAL)
               TYPE *,IAM(), 'SOUPWINPASALL-PROCEDIMENTO COM ERROS'
               TYPE *,IAM(), ' '
            ENDIF
            GOTO 100
        END IF


C=======================================================================
C              SINGLE OR MULTI MODE PROCESS  -   START
C=======================================================================
       IF(GNUM.EQ.20) THEN
          MAXPROC = 5
          MODEFLAG = 1
       ELSE 
          MAXPROC = 1
          MODEFLAG = 0
       ENDIF

C SINGLE OR MULTI PROCESS MODE CICLE - START
       DO IPROC=1, MAXPROC
C IF MULTI MODE GNUM GETS MMODEARR VALUE
          IF(MODEFLAG.EQ.1) THEN
             GNUM = MMODEARR(IPROC)
          ENDIF



C INITIALIZE DRWINDEX STRUCT
        DO I=1, MAXDRWID
           DRWINDEX(I) = -1
        ENDDO

C=======================================================================
C                READ VLF FOR INDEX GENERATION  -   START
C=======================================================================
C OPEN VALIDATION FILE
C*********************
        CALL IOPEN(SFNAMES(1,VLF),VLF,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
           IF(ST.NE.0) THEN
              CALL FILERR(SFNAMES(1,VLF),1,ST,0)
              GOTO 3000
           ENDIF
        CALL ITUBSIZE(VLF,TUBSIZ)

        TYPE*, IAM(),'SOUPWINALL-Aguarde, ficheiro de interface em processamento'

        ST=0
C START READING VLF FILE
        DO WHILE(ST .NE. ERREND)
           CALL ISREAD(V4BUF,VLF,VLFBUF,ST)
C IF GET END OF FILE
           IF(ST .EQ. ERREND) GOTO 209
C IF GET AN ERROR READING FILE
           IF(ST.NE.0) THEN
              CALL FILERR(SFNAMES(1,VLF),2,ST,0)
              CALL GPAUSE
              GOTO 209
           ENDIF

           CALL LOGVAL(VALREC,V4BUF)
           VST=VALREC(VSTAT)

C CHECK IF TRANSACTION SHOULD BE PRINTED
           IF(VALREC(VGAM).NE.GNUM.AND.GNUM.GT.0)   GOTO 209

C CHECK FOR RESULTS NOT IN
           IF(VST.EQ.VNOPAY.OR.VST.EQ.VNOPRZ.OR.VST.EQ.VPOST.OR.
     *        VST.EQ.VPPNPZ.OR.VST.EQ.VPRPOST) THEN
              TYPE*,IAM(),'SOUPWINALL-O valor dos prémios ainda não foi atribuído.'
              GOTO 209
           ENDIF

C CHECK FOR RESULTS IN (PPAY OR UCSH)
           IF (VALREC(VSTAT).EQ.VPRPAY.OR.
     *               VALREC(VSTAT).EQ.VUNCSH) THEN
              GTYP = VALREC(VGTYP)
              CALL DLOGVAL(VALREC,VDETAIL)

C compare draw number from the user with the vdetail
              VALDRW = .TRUE.
              DRWN = VDETAIL(VDRW,1)

              DO INDDRW=1, VALREC(VPZOFF)
                 DRWVDT = VDETAIL(VDRW,INDDRW)
                 IF (DRWVDT .NE. DRWN) THEN
                    VALDRW = .FALSE.
                 ENDIF
              ENDDO
C IF DRAWID IS COHERENT PROCEED WITH INDEXING
              IF(VALDRW .EQ. .TRUE.) THEN
                 IF(DRWINDEX(DRWN).EQ.-1) THEN
                    DRWINDEX(DRWN) = DRWN
                 ENDIF
              ENDIF

           ENDIF

209       CONTINUE
        ENDDO

        CALL ICLOSE(VLF,VLFBUF,ST)

3000    CONTINUE

        CALL ICLOSE(VLF,VLFBUF,ST)

C=======================================================================
C                READ VLF FOR INDEX GENERATION  -   END
C=======================================================================



C=======================================================================
C                  ITERATE INDEX STRUCT   -   START
C=======================================================================
        DO MAININDDRW=1, MAXDRWID
C CHECK IF DRAWID IS ACTIVE IN STRUCT
           IF(DRWINDEX(MAININDDRW).NE.-1) THEN
              TMPDRWID = DRWINDEX(MAININDDRW)

C=======================================================================
C               COPY OF SOUPWIN MAIN CORE   -   START
C=======================================================================
C GET WEEK AND YEAR BASED ON DRAW NUMBER
          CALL GETWEK(TMPDRWID, GNUM,WEEK,YEAR,ST)
          CONCURSO = WEEK
          ANO = YEAR


C SIMULATE USER CHOICE USING DRAWID ARRAY INDEX
          WRITE(CCCAAAA,FMT='(I3.3,I4.4)') CONCURSO,ANO
          WRITE(AAAACCC,FMT='(I4.4,I3.3)') ANO, CONCURSO
          WRITE(GGAAAACCC,FMT='(I2.2,I4.4,I3.3)') GNUM, ANO, CONCURSO


C GET MAIN GAME DRAW NUMBER FROM SYSTEM
          DRWN = GETDRW(ANO,CONCURSO,GNUM)
          IF(GTYP .EQ. TSPT) THEN
C GET KICKER DRAW NUMBER - IF MAIN GAME IS SPORTS
             DRWNKIK = GETDRW(ANO,CONCURSO,GNUMKIK)
          ENDIF

C COMPARE DRAWID FROM DRWINDEX STRUCT TO DRAWID FROM GETDRW FUNCTION
          IF(DRWN.NE.TMPDRWID) THEN
             TYPE*,IAM(),'ERROR PROCESSING DRAWID!'
             CALL GSTOP (GEXIT_FATAL)
          ENDIF

          IF (DRWN.LE.0) THEN
             TYPE*,IAM(), 'SOUPWINALL - Nº do DRAW inválido'
             TYPE*,IAM(), 'Erro Fatal'
             GOTO 100
          ENDIF


C READ GAME FILE
Cthis subroutine open, read and close the file
C************************************************
        CALL FIND_AVAILABLE_LUN (FLUN,ST)
        IF (ST .NE. 0) THEN
           TYPE*, IAM(), '       '
           TYPE*, IAM(), 'SOUPWINALL - Erro a obter uma LUN para o ficheiro!'
           TYPE*, IAM(), '       '
           TYPE*, IAM(), '       '
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
        CALL FIND_AVAILABLE_LUN (FLUNKIK,ST)
        IF (ST .NE. 0) THEN
           TYPE*, IAM(), '       '
           TYPE*, IAM(), 'SOUPWINALL - Erro a obter uma LUN para o ficheiro!'
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
           TYPE*, IAM(), 'A obter o valor das shares do concurso' //' '//CCCAAAA
           TYPE*, IAM(), 'do '//TRIM(GAMLNAM(GNUM))//'...'
           CALL READGFL(FLUN,GFNAMES(1,GNUM),DSPSEC,DRWN,GAMSTS)
           MGDIVNUM = DSPDIV
           I=1
           DO 222 I=1,MGDIVNUM
              WRITE(C50DIV,'(A,I0,A)') ' Divisao ',I,': '
              TYPE*,IAM(),TRIM(C50DIV),CMONY(DSPSHV(I),13,VALUNIT)
222        CONTINUE

C          JOKER GAME FILE READ
           TYPE*,IAM(),' '
           TYPE*, IAM(), 'A obter o valor das shares do concurso' //' '//CCCAAAA
           TYPE*, IAM(), 'do '//TRIM(GAMLNAM(GNUMKIK))//'...'
           CALL READGFL(FLUNKIK,GFNAMES(1,GNUMKIK),DKKSEC,DRWNKIK, GAMSTSKIK)
           KIKDIVNUM = DKKDIV
           I=1
           DO 223 I=1,KIKDIVNUM
              WRITE(C50DIV,'(A,I0,A)') ' Divisao ',I,': '
              TYPE*,IAM(),TRIM(C50DIV),CMONY(DKKSHV(I),13,VALUNIT)
223        CONTINUE

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
               TYPE *,IAM(),'SOUPWINALL-Prémios não apurados para o concurso: ',CCCAAAA 
               TYPE *,IAM(),'       '  
               GOTO 100
           ENDIF
        ELSE
           IF (GAMSTS.LT.GFINAL) THEN
               TYPE *,IAM(),'SOUPWINALL-Prémios não apurados para o concurso: ',CCCAAAA 
               TYPE *,IAM(),'       '  
               GOTO 100
           ENDIF
        ENDIF


C WRITE INTERFACE FILE NAME
C************************* 
        WRITE (PFILENAM, FMT='(A16,I2.2,A7,A4)')
     *     'FILE:SOUP_AM_PA_',GNUM,CCCAAAA,'.ASC'


C     TRY TO DELETE FILE FIRST
C******************************
        CALL DFILX(PFILENAM,0,0,ST)
        IF (ST.NE.0) CALL GSTOP (GEXIT_FATAL)


C OPEN INTERFACE FILE
C*****************************  
        CALL OPEN_FILASC (PFILENAM,PUNIT,ST)
        IF (ST.NE.0) THEN
            TYPE*,IAM(),'SOUPWINALL - Erro a criar/abrir o ficheiro:      '
            TYPE*,IAM(),'SOUP_AM_PA_',GNUM,CCCAAAA,'.ASC '
            TYPE*, IAM(),' '
            GOTO 2000
        ENDIF


C WRITE HEADER
C******************
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

C OPEN VALIDATION FILE
C*********************
        CALL IOPEN(SFNAMES(1,VLF),VLF,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
           IF(ST.NE.0) THEN
              CALL FILERR(SFNAMES(1,VLF),1,ST,0)
              GOTO 2000
           ENDIF
        CALL ITUBSIZE(VLF,TUBSIZ)

        TYPE*, IAM(),'SOUPWINALL-Aguarde, ficheiro de interface em processamento'

C SET STATISTICS COUNTERS
        CPPAY = 0
        CUCSH = 0
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
     *            VALREC(VSTAT).EQ.VUNCSH) THEN


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
                 CALL WRITEV_AM(VALREC,PUNIT,VDETAIL,CNTREC,ST,
     *                                DSPSHV,DKKSHV)
                 CNT_PRM=1 + CNT_PRM
              ELSEIF (GTYP .EQ. TLTO) THEN
                 CALL WRITEV_AM(VALREC,PUNIT,VDETAIL,CNTREC,ST,
     *                                DLTSHV,DKKSHV)
                 CNT_PRM=1 + CNT_PRM
              ELSEIF (GTYP .EQ. TKIK) THEN
                 CALL WRITEV_AM(VALREC,PUNIT,VDETAIL,CNTREC,ST,
     *                                DKKSHV,DKKSHV)
                 CNT_PRM=1 + CNT_PRM
              ENDIF

C INCREASE STATISTICS COUNTERS
              IF(VALREC(VSTAT).EQ.VPRPAY) CPPAY = CPPAY + 1
              IF(VALREC(VSTAT).EQ.VUNCSH) CUCSH = CUCSH + 1

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
        TYPE*,IAM(),' '

        IF(CNT_PRM.NE.(CUCSH+CPPAY)) THEN
           TYPE*,IAM(),'* * * * * * * * * * * * * * * * * * * * * * * *'
           TYPE*,IAM(),'  O TOTAL DE REGISTOS CONTABILIZADOS NAO ESTA  '
           TYPE*,IAM(),'  COERENTE COM OS SUBTOTAIS CONTABILIZADOS!!!  '
           TYPE*,IAM(),'* * * * * * * * * * * * * * * * * * * * * * * *'
           ERRORFLAG = .TRUE.
           CALL GPAUSE
        ENDIF


C CHECK IF ERRORS OCURRED DURING FILE PROCESSING
       IF(ERRORFLAG) THEN
          TYPE*,IAM(),'   Ficheiro gerado com ERROS:                      '
       ELSE
          TYPE*,IAM(),'   Ficheiro gerado com SUCESSO:                    '
       ENDIF

C
C=======================================================================
C       LIST THE LATEST VERSION OF THE GENERATED FILES
C=======================================================================
C
        WRITE(LIBCMD,'(A,A,A)') '$ DIR ',TRIM(PFILENAM(6:)),';0 /DATE/SIZE=ALL'

        ST = LIB$SPAWN(TRIM(LIBCMD))
        IF(.NOT. ST) CALL LIB$SIGNAL(%VAL(ST))


C CLOSE FILE
C***********
        CALL USRCLOS1(PUNIT)
        CLOSE(OPS_LUN)
        CALL ICLOSE(VLF,VLFBUF,ST)
        GOTO 2001

C WHEN IN ERROR
2000    CONTINUE


C CLOSE FILE
C***********
        CALL USRCLOS1(PUNIT)
        CLOSE(OPS_LUN)
        CALL ICLOSE(VLF,VLFBUF,ST)
C     DELETE FILE WITH ERRORS
        CALL DFILX(PFILENAM,0,0,ST)
        IF (ST.NE.0) THEN
           CALL GSTOP (GEXIT_FATAL)
           TYPE *,IAM(), 'SOUPWINALL-PROCEDIMENTO COM ERROS'
           TYPE *,IAM(), ' '
        ENDIF


2001    CONTINUE
C*********************************************
C              FORMAT MESSAGES
C*********************************************

102    FORMAT(A7)
C HEADER
10     FORMAT(A2,I4.4,I2.2,I2.2,I4.4,I2.2,I2.2,I4.4,I2.2,I2.2,I4.4,I2.2,I2.2,45(' '))
C TRAILER
20     FORMAT(A2,I8.8,69(' '))



C=======================================================================
C                COPY OF SOUPWIN MAIN CORE   -   END
C=======================================================================
C  [ IF(DRWINDEX(INDDRW).NE.-1) ]
           ENDIF
        ENDDO


C SINGLE OR MULTI PROCESS MODE CICLE - END
      ENDDO
C=======================================================================
C              SINGLE OR MULTI MODE PROCESS  -   END
C=======================================================================

       TYPE*,IAM(),'***********************************************************'
       TYPE*,IAM(),' '
       TYPE*,IAM(),'***********************************************************'
       TYPE*,IAM(),'                    SOUPWINALL ENDED                       '
       TYPE*,IAM(),'***********************************************************'
       TYPE*,IAM(),' '

C=======================================================================
C                   ITERATE INDEX STRUCT   -   END
C=======================================================================

C SHOWS MAIN MENU AGAIN
        GOTO 100


C END PROGRAM SOUPWINALL
       END
C**********************************************************************
C**********************************************************************


C       **************************************************
C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE OPEN_FILASC (PFILENAM,PUNIT,ST)
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
          TYPE *, IAM(), 'SOUPWINALL- Erro ao criar/abrir: ', PFILENAM
          TYPE *, IAM(),'                '
          CALL USRCLOS1(PUNIT)
          CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
        RETURN
        END


C*************************************************
C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE WRITEV_AM(VALREC,PUNIT,VDETAIL,CNTREC,ST,
     *               ESHRARR,EKIKSHRARR)
        IMPLICIT NONE
C**************************************************

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
        INTEGER*4      GTYP
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

C  ********************************************************************

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
           RETURN


C FORMAT FOR LOTO
1900    FORMAT('01',I1.1,I7.7,I4.4,I2.2,I2.2,I4.4,I9.9,
     *              I11.11,I11.11,I11.11,I11.11,I4.4)
C TEMP 02 RECORD
800     FORMAT('02',I2.2,I5.5,I11.11,I1,58(' '))
C FINAL 02 RECORD
810     FORMAT(A79) 


C END WRITEV_AM
        END
C======================================================================


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
          WRITE(5,911) GCNT+1
          WRITE(5,912) GCNT+2
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
C   IF ALL MUTUAS
             IF(GAMSEL.EQ.8) THEN
                GNUM = 20                 !GAME NUMBER
                GTYP = 20                 !GAME TYPE
                GNDX = 20                 !GAME INDEX
C   IF ALL PASSIVE
             ELSEIF(GAMSEL.EQ.9) THEN
                GNUM = 30                 !GAME NUMBER
                GTYP = 30                 !GAME TYPE
                GNDX = 30                 !GAME INDEX
C   IF ONLY ONE GAME
             ELSE
                GNUM = GAM_NUM(GAMSEL)                 !GAME NUMBER
                GTYP = SCFGNT(GAMTYP,GAM_NUM(GAMSEL))  !GAME TYPE
                GNDX = SCFGNT(GAMIDX,GAM_NUM(GAMSEL))  !GAME INDEX
             ENDIF

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
911     FORMAT(18X,I2,' - ALL MUTUAS')
912     FORMAT(18X,I2,' - ALL PASSIVE')
913     FORMAT(18X,' E - Exit')

C
        END

