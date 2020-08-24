C
C      MAAUDIT.FOR
C
C      V01 24-NOV-2016 SCML INITIAL RELEASE
C
C      PROGRAM MAAUDIT
C
C      THIS PROGRAM EXTRACTS FROM A MTMF FILE, SPECIFIC DATA OF PLACARD
C      WAGER TRANSACTIONS AND PLACARD CANCELLATION TRANSACTIONS, FOR
C      FURTHER RECONCILLIATION WITH THE SAME TYPE OF TRANSACTIONS
C      REGISTERED IN THE ABP SYSTEM (CENTRAL SYSTEM OF PLACARD GAME).
C      ONLY "GOOD NOER" TRANSACTIONS ARE CONSIDERED FOR EXTRACTION.
C
C      THE EXTRACTED DATA IS OUTPUT TO THE ASCII FILE
C      MAAUDIT_<YYYYMMDD>.ASC, WHERE YYYYMMDD CORRESPONDS TO THE CDC
C      DATE OF MTMF FILE.
C
C      THIS PROGRAM ALSO GENERATES A LOG FILE WITH THE NAME
C      MAAUDIT_<YYYYMMDDHH24MISS>.LOG. THE LOG LEVEL CONFIGURATION IS
C      DONE IN MAAUDIT.DEF FILE.
C
C      THE ASCII FILE IS COMPOSED BY 3 SECTIONS, IN THE FOLLOWING ORDER,
C      EACH ONE HAVING NONE, ONE OR MORE RECORDS:
C
C        HEADER.: COMPOSED BY ONE RECORD OF TYPE 'HP' (FIRST LINE OF FILE)
C        BODY...: COMPOSED BY NONE OR MORE RECORDS OF TYPES '01' AND '02'
C        TRAILER: COMPOSED BY ONE RECORD OF TYPE 'TP' (LAST LINE OF FILE)
C
C      EACH RECORD TYPE HAS A FIXED NUMBER OF FIELDS:
C
C        HEADER.: 'HP' RECORD TYPE HAS 4 FIELDS (RECORD TYPE INCLUDED)
C        BODY...: '01' RECORD TYPE HAS 6 FIELDS (RECORD TYPE INCLUDED)
C                 '02' RECORD TYPE HAS 7 FIELDS (RECORD TYPE INCLUDED)
C        TRAILER: 'TP' RECORD TYPE HAS 2 FIELDS (RECORD TYPE INCLUDED)
C
C      'HP' RECORD TYPE FIELDS:
C        G.01...: HEADER RECORD TYPE REFERENCE
C        G.02...: ASCII FILE GENERATION DATE (YYYYMMDD)
C        G.03...: MTMF CDC DATE IN THE FORMAT YYYYMMDD
C        G.04...: MTMF CDC DATE
C
C      '01' RECORD TYPE FIELDS
C        G.01...: PLACARD WAGER TRANSACTION RECORD TYPE REFERENCE
C        G.02...: AGENT NUMBER
C        G.03...: TERMINAL MESSAGE ID
C        G.04...: BET REFERENCE NUMBER (YYMMDDGGSSSSSSSSSSCCC)
C        G.05...: BET CREATION TIMESTAMP (YYYYMMDDHH24MISS)
C        G.06...: BET TOTAL STAKE (WAGER UNITS)
C
C      '02' RECORD TYPE FIELDS
C        G.01...: PLACARD CANCELLATION TRANSACTION RECORD TYPE REFERENCE
C        G.02...: AGENT NUMBER
C        G.03...: TERMINAL MESSAGE ID
C        G.04...: BET REFERENCE NUMBER (YYMMDDGGSSSSSSSSSSCCC)
C        G.05...: CANCEL REFERENCE NUMBER (YYMMDDGGSSSSSSSSSSCCC)
C        G.06...: BET CANCEL TIMESTAMP (YYYYMMDDHH24MISS)
C        G.07...: CANCEL AMOUNT (WAGER UNITS)
C
C      'TP' RECORD TYPE FIELDS:
C        G.01...: TRAILER RECORD TYPE REFERENCE
C        G.02...: TOTAL NUMBER OF RECORDS WRITTEN TO ASCII FILE (HEADER
C                 AND TRAILER INCLUDED)
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      THIS ITEM IS THE PROPERTY OF SCML.
C
C      COPYRIGHT 2016 SCML. ALL RIGHTS RESERVED.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C======OPTIONS /CHECK=NOOVERFLOW/EXT
       PROGRAM MAAUDIT
       IMPLICIT NONE
C
       INCLUDE '(LIB$ROUTINES)'
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:PRMAGT.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:MAAUDIT.DEF'
C=======================================================================
C      LOCALS
C=======================================================================
       INTEGER*4     YNFLG
       CHARACTER*38  QUESTION
       LOGICAL       EXISTS/.FALSE./
       CHARACTER*20  MTMFNAM/'                    '/
C
       INTEGER*4     ST
       INTEGER*4     FLUN
       INTEGER*4     WFCDC
       CHARACTER*8   WFDAT
       CHARACTER*8   CURDAT
       INTEGER*4     TOTRECFIL, TOTRECHDR, TOTRECBDY, TOTRECTRL
C
       INTEGER*2     VDAT(LDATE_LEN)
       INTEGER*4     CTIM(2), CDAT(8)
       CHARACTER*132 OUTFILENAME                                                !OUTPUT FILENAME
       CHARACTER*132 TMPFILENAME                                                !TEMPORARY FILENAME
       CHARACTER*132 LIBCMD
C
       INTEGER*4     LOGLUN
       CHARACTER*132 LOGMSG
C=======================================================================
C-----------------------------------------------------------------------
C                         MAIN PROGRAM STARTS HERE
C-----------------------------------------------------------------------
C=======================================================================
       IF(LOGLEVEL .GT. OFF) THEN
         CALL CREATELOG(LOGLUN)
       ENDIF
       LOGMSG='MAAUDIT .MAIN [Inicio do programa.]'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
C
       TYPE*,IAM()
       WRITE(6, 9000) IAM(),
     * 'Copyright 2016 SCML. All rights reserved.        '
       TYPE*,IAM()
       WRITE(6, 9000) IAM(),
     * '-----------------------------------------------------------'
       WRITE(6, 9000) IAM(),
     * '<<<<<     FICHEIRO DE AUDITORIA MILLENNIUM-ABP        >>>>>'
       WRITE(6, 9000) IAM(),
     * '<<<<<         GERA MAAUDIT_<AAAAMMDD>.ASC             >>>>>'
       WRITE(6, 9000) IAM(),
     * '<<<<<                                                 >>>>>'
       WRITE(6, 9000) IAM(),
     * '<<<<<             <AAAAMMDD> = data                   >>>>>'
       WRITE(6, 9000) IAM(),
     * '-----------------------------------------------------------'
C
C=======================================================================
C      INQUIRE THE OPERATOR FOR THE CDC FILE TO EXTRACT
C=======================================================================
10     CONTINUE
       TYPE*, IAM()
       CALL INPNUM('Insira a data CDC', WFCDC, 1, 9999, ST)
       IF(ST .NE. 0) THEN
         LOGMSG='MAAUDIT .MAIN [O ficheiro de auditoria nao sera criado ...]'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
         LOGMSG='MAAUDIT .MAIN [A sair do programa ...]'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
         CALL GSTOP(GEXIT_OPABORT)
       ENDIF
       WRITE(LOGMSG,FMT='(A,I0,A)') 'MAAUDIT .MAIN [Data CDC inserida pelo operador: ',
     *                              WFCDC,
     *                              ']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(MTMFNAM,9100) WFCDC
C=======================================================================
C      INQUIRE ABOUT FILE'S EXISTENCE
C=======================================================================
       INQUIRE(FILE=MTMFNAM, EXIST = EXISTS)
       IF(.NOT. EXISTS) THEN
         TYPE*, IAM(), ' Ficheiro nao encontrado: ',TRIM(MTMFNAM)
         LOGMSG='MAAUDIT .MAIN ['//'Ficheiro nao encontrado: '//TRIM(MTMFNAM)//']'
         CALL LOGGER(TRIM(LOGMSG),WARN,LOGLUN)
         GOTO 10
       ELSE
         LOGMSG='MAAUDIT .MAIN ['//'Ficheiro para processar encontrado: '//TRIM(MTMFNAM)//']'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       END IF
C=======================================================================
C      ASK THE OPERATOR IF THE FILE THAT WAS FOUND IS TO BE PROCESSED
C=======================================================================
       WRITE(QUESTION, 9400) TRIM(MTMFNAM)
       CALL PRMYESNO(QUESTION,YNFLG)
       IF(YNFLG .EQ. 1) GOTO 20
       IF(YNFLG .EQ. 2) GOTO 10
       IF(YNFLG .EQ. 3) THEN
         LOGMSG='MAAUDIT .MAIN [O ficheiro de auditoria nao sera criado ...]'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
         LOGMSG='MAAUDIT .MAIN [A sair do programa ...]'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
         CALL GSTOP(GEXIT_OPABORT)
       ENDIF
C=======================================================================
C      GET CDC DATE IN THE FORMAT YYYYMMDD
C=======================================================================
20     CONTINUE
       VDAT(VCDC)=WFCDC
       CALL LCDATE(VDAT)
       IF(VDAT(VYEAR).LT.77) THEN
         WRITE (WFDAT, FMT='(I4.4,I2.2,I2.2)') VDAT(VYEAR)+2000,
     *                                         VDAT(VMON),
     *                                         VDAT(VDAY)
       ELSE
         WRITE (WFDAT, FMT='(I4.4,I2.2,I2.2)') VDAT(VYEAR)+1900,
     *                                         VDAT(VMON),
     *                                         VDAT(VDAY)
       ENDIF
C=======================================================================
C      GET CURRENT DATE IN THE FORMAT YYYYMMDD
C=======================================================================
       CALL ICLOCK(1,CTIM)
       CALL GDATE(CDAT(2),CDAT(3),CDAT(1))
       IF(CDAT(1).LT.77) THEN
         WRITE (CURDAT, FMT='(I4.4,I2.2,I2.2)') CDAT(1)+2000,
     *                                          CDAT(2),
     *                                          CDAT(3)
       ELSE
         WRITE (CURDAT, FMT='(I4.4,I2.2,I2.2)') CDAT(1)+1900,
     *                                          CDAT(2),
     *                                          CDAT(3)
       ENDIF
C=======================================================================
C      DEFINE FILE NAMES
C=======================================================================
       WRITE (OUTFILENAME, 9200) OUTFILEPREF,
     *                           WFDAT,
     *                           OUTFILEEXT                                     !OUTPUT FILE NAME
       WRITE (TMPFILENAME, 9300) TMPFILEPREF,
     *                           WFDAT,
     *                           TMPFILEEXT                                     !TEMPORARY FILE NAME
C=======================================================================
C      FIND A FREE LUN TO USE
C=======================================================================
       CALL FIND_AVAILABLE_LUN(FLUN,ST)
       IF(ST .NE. 0) THEN
         TYPE*, IAM(), 'Nao foi possivel abrir o ficheiro '//TRIM(TMPFILENAME)//': LUN nao disponivel'
         WRITE(LOGMSG,'(A)') 'MAAUDIT .MAIN [Nao foi possivel abrir o ficheiro '//TRIM(TMPFILENAME)//': LUN nao disponivel'//']'
         CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
         CLOSE(LOGLUN)
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
C=======================================================================
C      OPEN THE TEMPORARY FILE
C=======================================================================
       OPEN(UNIT            = FLUN,
     *      FILE            = TRIM(TMPFILENAME),
     *      STATUS          = 'NEW',
     *      CARRIAGECONTROL = 'LIST',
     *      ACCESS          = 'SEQUENTIAL',
     *      IOSTAT          = ST)
       IF(ST .NE. 0) THEN
         TYPE*, IAM(),' Erro#',ST,' a abrir o ficheiro ',TRIM(TMPFILENAME)
         WRITE(LOGMSG,'(A,I0,A)') 'MAAUDIT .MAIN ['//'Erro#',
     *                             ST,
     *                             ' a abrir o ficheiro '//TRIM(TMPFILENAME)//']'
         CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
         CLOSE(LOGLUN)
         CALL GSTOP(GEXIT_FATAL)
       ELSE
         LOGMSG='MAAUDIT .MAIN ['//'Ficheiro '//TRIM(TMPFILENAME)//' aberto com sucesso.'//']'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       ENDIF
C=======================================================================
C      START EXTRACTION TO TEMPORARY FILE
C=======================================================================
       TYPE*, IAM()
       TYPE*, IAM(),'A gerar o ficheiro ', TRIM(OUTFILENAME), ' ...'
       CALL LOGGER('MAAUDIT .MAIN ['//'A gerar o ficheiro '//TRIM(OUTFILENAME)//' ...]',INFO,LOGLUN)
       TYPE*, IAM()
C
       TOTRECFIL = 0
       TOTRECHDR = 0
       CALL PRINT_HEADER(FLUN, WFCDC, WFDAT, CURDAT, LOGLUN, TOTRECHDR)
       TOTRECFIL = TOTRECFIL + TOTRECHDR
C
       TOTRECBDY = 0
       CALL START_EXTRACTION(FLUN, WFCDC, LOGLUN, TOTRECBDY, ST)                !START DATA EXTRACTION (FILE BODY)
       TOTRECFIL = TOTRECFIL + TOTRECBDY
C
       IF(ST .EQ. 0) THEN
         CALL PRINT_TRAILER(FLUN, LOGLUN, TOTRECFIL, TOTRECTRL)
         TOTRECFIL = TOTRECFIL + TOTRECTRL
         WRITE(LOGMSG,FMT='(A,I0,A)') 'MAAUDIT .MAIN ['//'Total de registos escritos no ficheiro '//TRIM(TMPFILENAME)//': ',
     *                                 TOTRECFIL,
     *                                 ']'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       ENDIF
C
       CLOSE(FLUN)                                                              !CLOSE TEMPORARY FILE
       LOGMSG='MAAUDIT .MAIN ['//'Ficheiro '//TRIM(TMPFILENAME)//' fechado.'//']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
C
       IF(ST .NE. 0) THEN
         CLOSE(LOGLUN)
         CALL GSTOP(GEXIT_FATAL)
       ELSE
         LOGMSG='MAAUDIT .MAIN ['//'A renomear o ficheiro '//TRIM(TMPFILENAME)//' para '//TRIM(OUTFILENAME)//' ...]'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
         CALL LIB$RENAME_FILE(TMPFILENAME,OUTFILENAME)                          !RENAME TEMPORARY FILE
         TYPE*, IAM(), 'Ficheiro ',TRIM(OUTFILENAME),' gerado com sucesso:'
         LOGMSG='MAAUDIT .MAIN ['//'Ficheiro '//TRIM(OUTFILENAME)//' gerado com sucesso.'//']'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
C=======================================================================
C        LIST THE LATEST VERSION OF THE RENAMED FILE
C=======================================================================
         TYPE*, IAM()
         WRITE(LIBCMD,'(A,A,A)') '$ DIR ',TRIM(OUTFILENAME),';0 /DATE/SIZE=ALL'
         ST = LIB$SPAWN(TRIM(LIBCMD))
         IF(.NOT. ST) CALL LIB$SIGNAL(%VAL(ST))
         TYPE*
C
         TYPE*, IAM()
         TYPE*, IAM(), 'Fim do programa'
         LOGMSG='MAAUDIT .MAIN ['//'Fim do programa.'//']'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
         CLOSE(LOGLUN)
         CALL GSTOP(GEXIT_SUCCESS)
       ENDIF
C=======================================================================
C      FORMAT STATEMENTS
C=======================================================================
9000   FORMAT(1X,A18,A59)
9100   FORMAT('BCKS:MT',I4.4,'.FIL')
9200   FORMAT(A,A,A)
9300   FORMAT(A,A,A)
9400   FORMAT('Processar o ficheiro ',A, ' ?')
C
       END
C
C***********************************************************************
C SUBROUTINE: PRINTS THE HEADER RECORD INTO ASCII FILE.
C
C INPUT:
C        FLUN - LOGICAL UNIT OF TEMPORARY FILE
C        WFCDC - CDC DATE
C        WFDAT - CDC DATE IN THE FORMAT YYYYMMDD
C        CURDAT - CURRENT DATE IN THE FORMAT YYYYMMDD
C        LOGLUN - LOGICAL UNIT OF LOG FILE
C
C OUTPUT:
C        TOTRECHDR - TOTAL RECORDS WRITTEN TO FILE HEADER SECTION
C
C***********************************************************************
C======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE PRINT_HEADER(FLUN, WFCDC, WFDAT, CURDAT, LOGLUN, TOTRECHDR)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:MAAUDIT.DEF'
C=======================================================================
C      INPUT
C=======================================================================
       INTEGER*4   FLUN
       INTEGER*4   WFCDC
       CHARACTER*8 WFDAT
       CHARACTER*8 CURDAT
       INTEGER*4   LOGLUN
C=======================================================================
C      OUTPUT
C=======================================================================
       INTEGER*4   TOTRECHDR
C=======================================================================
C      LOCALS
C=======================================================================
       CHARACTER*132 LOGMSG
C=======================================================================
C      GLOBALS
C=======================================================================
C
C=======================================================================
C      WRITE HEADER RECORD INTO FILE
C=======================================================================
       TOTRECHDR = 0
       WRITE(FLUN, 100) RECTYP_HP,                                              !G.01 RECORD TYPE: HEADER
     *                  CURDAT,                                                 !G.02 ASCII FILE GENERATION DATE (YYYYMMDD)
     *                  WFDAT,                                                  !G.03 MTMF CDC DATE IN THE FORMAT YYYYMMDD
     *                  WFCDC                                                   !G.04 MTMF CDC DATE
       TOTRECHDR = TOTRECHDR + 1
       LOGMSG='MAAUDIT .PRINT_HEADER ['//'Header do ficheiro escrito.'//']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
C
C=======================================================================
C      WRITE FORMAT OF HEADER RECORD
C=======================================================================
100    FORMAT(
     *        A2,                                                               !G.01 RECORD TYPE: HEADER
     *        A8,                                                               !G.02 ASCII FILE GENERATION DATE (YYYYMMDD)
     *        A8,                                                               !G.03 MTMF CDC DATE IN THE FORMAT YYYYMMDD
     *        I4.4                                                              !G.04 MTMF CDC DATE
     *       )
C
       END
C
C***********************************************************************
C SUBROUTINE: EXTRACTS TRANSACTION DATA FROM THE MT<CDC>.FIL FILE AND
C             WRITE THEM TO A TEMPORARY FILE IN A FORMATTED WAY. THE
C             TRANSACTIONS CONSIDERED FOR EXTRACTION ARE WAGER AND
C             CANCELLATION TRANSACTIONS OF PLACARD GAME.
C
C INPUT:
C        FLUN - LOGICAL UNIT OF TEMPORARY FILE
C        WFCDC - CDC DATE
C        LOGLUN - LOGICAL UNIT OF LOG FILE
C
C OUTPUT:
C        TOTRECBDY - TOTAL RECORDS WRITTEN TO FILE BODY SECTION
C        ST - STATUS (OK - ST = 0, NOT OK - ST != 0)
C
C***********************************************************************
C======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE START_EXTRACTION(FLUN, WFCDC, LOGLUN, TOTRECBDY, ST)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:PRMLOG.DEF'
       INCLUDE 'INCLIB:DESTRA.DEF'
       INCLUDE 'INCLIB:PRMAGT.DEF'
       INCLUDE 'INCLIB:MAAUDIT.DEF'
C=======================================================================
C      INPUT
C=======================================================================
       INTEGER*4   FLUN
       INTEGER*4   WFCDC
       CHARACTER*8 WFDAT
       CHARACTER*8 CURDAT
       INTEGER*4   LOGLUN
C=======================================================================
C      OUTPUT
C=======================================================================
       INTEGER*4   TOTRECBDY                                                    !TOTAL RECORDS WRITTEN TO FILE BODY
       INTEGER*4   ST
C=======================================================================
C      LOCALS
C=======================================================================
       CHARACTER*132 LOGMSG
       INTEGER*4     SER
       INTEGER*4     LOGREC(LREC*3)
       INTEGER*8     I8TMP
       INTEGER*4     I4TMP(2)
       EQUIVALENCE   (I8TMP,I4TMP)
       INTEGER*8     TMSGID                                                     !TERMINAL MESSAGE ID
       INTEGER*8     WEXSER                                                     !WAGER EXTERNAL SERIAL
       INTEGER*8     CEXSER                                                     !CANCEL EXTERNAL SERIAL
       INTEGER*2     VDAT(LDATE_LEN)
       INTEGER*4     CTIM(2)
       INTEGER*4     CDAT(8)
       LOGICAL       EOT/.FALSE./
       INTEGER*4     MLUN
       PARAMETER     (MLUN=1)
       INTEGER*4     I4MTMFNAM(5)
       CHARACTER*20  MTMFNAM/'                    '/
       EQUIVALENCE   (I4MTMFNAM,MTMFNAM)
C=======================================================================
C      GLOBALS
C=======================================================================

C=======================================================================
C
       WRITE(MTMFNAM,'(A7,I0,A4)') 'BCKS:MT', WFCDC,'.FIL'
C
C=======================================================================
C      OPEN MILLENNIUM TRANSACTION MASTER FILE (MT<CDC>.FIL)
C=======================================================================
       ST=-1
C
       CALL OPENWY(MLUN, I4MTMFNAM, 0, 4, 0, ST)
       CALL TOPEN(MLUN)
       IF(ST.NE.0) THEN
         TYPE*, IAM(), 'Nao foi possivel abrir o ficheiro '//TRIM(MTMFNAM)
         LOGMSG='MAAUDIT .START_EXTRACT ['//'Nao foi possivel abrir o ficheiro '//TRIM(MTMFNAM)//'.]'
         CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
         CLOSE(LOGLUN)
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
       LOGMSG='MAAUDIT .START_EXTRACT ['//'Ficheiro '//TRIM(MTMFNAM)//' aberto com sucesso.'//']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       ST=0
       SER=0
       LOGMSG='MAAUDIT .START_EXTRACT ['//'A iniciar a extracao de dados ...'//']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       TOTRECBDY = 0
10     CONTINUE
       CALL READTMF(LOGREC,SER,EOT)
       IF(EOT) THEN
         LOGMSG='MAAUDIT .START_EXTRACT ['//'Extracao de dados finalizada.'//']'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
         WRITE(LOGMSG,FMT='(A,I0,A)') 'MAAUDIT .START_EXTRACT ['//'Total de registos escritos no body do ficheiro: ',
     *                                 TOTRECBDY,
     *                                 ']'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
         ST=0
         CLOSE(MLUN)
         LOGMSG='MAAUDIT .START_EXTRACT ['//'Ficheiro '//TRIM(MTMFNAM)//' fechado.'//']'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
         CALL TRXPRINT(LOGLUN)
         RETURN
       ENDIF
C
       CALL LOGTRA(TRABUF, LOGREC)                                              !GET NEXT TRANSACTION FROM MTMF
C
       CALL TRXCOUNT(TRABUF, LOGLUN)                                            !FOR STATISTICS PURPOSES
C=======================================================================
C      PLACARD WAGER TRANSACTION
C=======================================================================
       IF((TRABUF(TTYP)      .EQ. TIGS)   .AND.
     *    (TRABUF(TIGS_TTYP) .EQ. IGSWAG) .AND.
     *    (TRABUF(TSTAT)     .EQ. GOOD)   .AND.
     *    (TRABUF(TERR)      .EQ. NOER)   .AND.
     *    (TRABUF(TGAMTYP)   .EQ. TODS)   .AND.
     *    (TRABUF(TGAMIND)   .EQ. 1)) THEN
         I4TMP(2) = TRABUF(TIGSW_MIDH)
         I4TMP(1) = TRABUF(TIGSW_MIDL)
         TMSGID = I8TMP
         I4TMP(2) = TRABUF(TIGSW_WRSH)
         I4TMP(1) = TRABUF(TIGSW_WRSL)
         WEXSER = I8TMP
         WRITE(FLUN, 905) RECTYP_01,                                            !G.01 TRANSACTION RECORD TYPE: PLACARD WAGER
     *                   TRABUF(TAGT),                                          !G.02 AGENT NUMBER
     *                   TMSGID,                                                !G.03 TERMINAL MESSAGE ID
     *                   TRABUF(TIGSW_WRDY),                                    !G.04 BET REFERENCE DATE YEAR (YY, LAST TWO DIGITS ONLY)
     *                   TRABUF(TIGSW_WRDM),                                    !G.04 BET REFERENCE DATE MONTH (MM)
     *                   TRABUF(TIGSW_WRDD),                                    !G.04 BET REFERENCE DATE DAY (DD)
     *                   TRABUF(TIGSW_WRGM),                                    !G.04 BET REFERENCE GAME (GG)
     *                   WEXSER,                                                !G.04 BET REFERENCE SERIAL NUMBER (SSSSSSSSSS)
     *                   TRABUF(TIGSW_WRCD),                                    !G.04 BET REFERENCE SERIAL NUMBER (CCC)
     *                   TRABUF(TIGSW_WCDY),                                    !G.05 BET CREATION DATE YEAR IN ABP SYSTEM (YYYY)
     *                   TRABUF(TIGSW_WCDM),                                    !G.05 BET CREATION DATE MONTH IN ABP SYSTEM (MM)
     *                   TRABUF(TIGSW_WCDD),                                    !G.05 BET CREATION DATE DAY IN ABP SYSTEM (DD)
     *                   TRABUF(TIGSW_WCTH),                                    !G.05 BET CREATION DATE HOUR IN ABP SYSTEM (HH24)
     *                   TRABUF(TIGSW_WCTM),                                    !G.05 BET CREATION DATE MIN IN ABP SYSTEM (MI)
     *                   TRABUF(TIGSW_WCTS),                                    !G.05 BET CREATION DATE SECOND IN ABP SYSTEM (SS)
     *                   TRABUF(TIGSW_TSTK)                                     !G.06 BET TOTAL STAKE (WAGER UNITS)
         TOTRECBDY = TOTRECBDY + 1
C=======================================================================
C      PLACARD CANCELLATION TRANSACTION
C=======================================================================
       ELSEIF((TRABUF(TTYP)      .EQ. TIGS)   .AND.
     *        (TRABUF(TIGS_TTYP) .EQ. IGSCAN) .AND.
     *        (TRABUF(TSTAT)     .EQ. GOOD)   .AND.
     *        (TRABUF(TERR)      .EQ. NOER)   .AND.
     *        (TRABUF(TGAMTYP)   .EQ. TODS)   .AND.
     *        (TRABUF(TGAMIND)   .EQ. 1)) THEN
         I4TMP(2) = TRABUF(TIGSC_MIDH)
         I4TMP(1) = TRABUF(TIGSC_MIDL)
         TMSGID = I8TMP
C
         I4TMP(2) = TRABUF(TIGSC_WRSH)
         I4TMP(1) = TRABUF(TIGSC_WRSL)
         WEXSER = I8TMP                                                         !WAGER REFERENCE SERIAL NUMBER
C
         I4TMP(2) = TRABUF(TIGSC_CRSH)
         I4TMP(1) = TRABUF(TIGSC_CRSL)
         CEXSER = I8TMP                                                         !CANCEL REFERENCE SERIAL NUMBER
C
         WRITE(FLUN, 906) RECTYP_02,                                            !G.01 TRANSACTION RECORD TYPE: PLACARD CANCELLATION
     *                   TRABUF(TAGT),                                          !G.02 AGENT NUMBER
     *                   TMSGID,                                                !G.03 TERMINAL MESSAGE ID
     *                   TRABUF(TIGSC_WRDY),                                    !G.04 BET REFERENCE DATE YEAR (YY, LAST TWO DIGITS ONLY)
     *                   TRABUF(TIGSC_WRDM),                                    !G.04 BET REFERENCE DATE MONTH (MM)
     *                   TRABUF(TIGSC_WRDD),                                    !G.04 BET REFERENCE DATE DAY (DD)
     *                   TRABUF(TIGSC_WRGM),                                    !G.04 BET REFERENCE GAME (GG)
     *                   WEXSER,                                                !G.04 BET REFERENCE SERIAL NUMBER (SSSSSSSSSS)
     *                   TRABUF(TIGSC_WRCD),                                    !G.04 BET REFERENCE SERIAL NUMBER (CCC)
     *                   TRABUF(TIGSC_CRDY),                                    !G.05 CANCEL REFERENCE DATE YEAR (YY, LAST TWO DIGITS ONLY)
     *                   TRABUF(TIGSC_CRDM),                                    !G.05 CANCEL REFERENCE DATE MONTH (MM)
     *                   TRABUF(TIGSC_CRDD),                                    !G.05 CANCEL REFERENCE DATE DAY (DD)
     *                   TRABUF(TIGSC_CRGM),                                    !G.05 CANCEL REFERENCE GAME (GG)
     *                   CEXSER,                                                !G.05 CANCEL REFERENCE SERIAL NUMBER (SSSSSSSSSS)
     *                   TRABUF(TIGSC_CRCD),                                    !G.05 CANCEL REFERENCE SERIAL NUMBER (CCC)
     *                   TRABUF(TIGSC_WCDY),                                    !G.06 BET CANCEL DATE YEAR IN ABP SYSTEM (YYYY)
     *                   TRABUF(TIGSC_WCDM),                                    !G.06 BET CANCEL DATE MONTH IN ABP SYSTEM (MM)
     *                   TRABUF(TIGSC_WCDD),                                    !G.06 BET CANCEL DATE DAY IN ABP SYSTEM (DD)
     *                   TRABUF(TIGSC_WCTH),                                    !G.06 BET CANCEL DATE HOUR IN ABP SYSTEM (HH24)
     *                   TRABUF(TIGSC_WCTM),                                    !G.06 BET CANCEL DATE MIN IN ABP SYSTEM (MI)
     *                   TRABUF(TIGSC_WCTS),                                    !G.06 BET CANCEL DATE SECOND IN ABP SYSTEM (SS)
     *                   TRABUF(TIGSC_CAMT)                                     !G.07 CANCEL AMOUNT (WAGER UNITS)
         TOTRECBDY = TOTRECBDY + 1
       ENDIF
C
C=======================================================================
C      WRITE FORMAT OF PLACARD WAGER TRANSACTION RECORD
C=======================================================================
905    FORMAT(
     *        A2,                                                               !G.01 RECORD TYPE: PLACARD WAGER TRANSACTION
     *        I7.7,                                                             !G.02 AGENT NUMBER
     *        I13.13,                                                           !G.03 TERMINAL MESSAGE ID
     *        I2.2,                                                             !G.04 BET REFERENCE DATE YEAR (YY, LAST TWO DIGITS ONLY)
     *        I2.2,                                                             !G.04 BET REFERENCE DATE MONTH (MM)
     *        I2.2,                                                             !G.04 BET REFERENCE DATE DAY (DD)
     *        I2.2,                                                             !G.04 BET REFERENCE GAME (GG)
     *        I10.10,                                                           !G.04 BET REFERENCE SERIAL NUMBER (SSSSSSSSSS)
     *        I3.3,                                                             !G.04 BET REFERENCE SERIAL NUMBER (CCC)
     *        I4.4,                                                             !G.05 BET CREATION DATE YEAR IN ABP SYSTEM (YYYY)
     *        I2.2,                                                             !G.05 BET CREATION DATE MONTH IN ABP SYSTEM (MM)
     *        I2.2,                                                             !G.05 BET CREATION DATE DAY IN ABP SYSTEM (DD)
     *        I2.2,                                                             !G.05 BET CREATION DATE HOUR IN ABP SYSTEM (HH24)
     *        I2.2,                                                             !G.05 BET CREATION DATE MIN IN ABP SYSTEM (MI)
     *        I2.2,                                                             !G.05 BET CREATION DATE SECOND IN ABP SYSTEM (SS)
     *        I6.6                                                              !G.06 BET TOTAL STAKE (WAGER UNITS)
     *       )
C
C=======================================================================
C      WRITE FORMAT OF PLACARD CANCEL TRANSACTION RECORD
C=======================================================================
906    FORMAT(
     *        A2,                                                               !G.01 TRANSACTION RECORD TYPE: PLACARD CANCELLATION
     *        I7.7,                                                             !G.02 AGENT NUMBER
     *        I13.13,                                                           !G.03 TERMINAL MESSAGE ID
     *        I2.2,                                                             !G.04 BET REFERENCE DATE YEAR (YY, LAST TWO DIGITS ONLY)
     *        I2.2,                                                             !G.04 BET REFERENCE DATE MONTH (MM)
     *        I2.2,                                                             !G.04 BET REFERENCE DATE DAY (DD)
     *        I2.2,                                                             !G.04 BET REFERENCE GAME (GG)
     *        I10.10,                                                           !G.04 BET REFERENCE SERIAL NUMBER (SSSSSSSSSS)
     *        I3.3,                                                             !G.04 BET REFERENCE SERIAL NUMBER (CCC)
     *        I2.2,                                                             !G.05 CANCEL REFERENCE DATE YEAR (YY, LAST TWO DIGITS ONLY)
     *        I2.2,                                                             !G.05 CANCEL REFERENCE DATE MONTH (MM)
     *        I2.2,                                                             !G.05 CANCEL REFERENCE DATE DAY (DD)
     *        I2.2,                                                             !G.05 CANCEL REFERENCE GAME (GG)
     *        I10.10,                                                           !G.05 CANCEL REFERENCE SERIAL NUMBER (SSSSSSSSSS)
     *        I3.3,                                                             !G.05 CANCEL REFERENCE SERIAL NUMBER (CCC)
     *        I4.4,                                                             !G.06 BET CANCEL DATE YEAR IN ABP SYSTEM (YYYY)
     *        I2.2,                                                             !G.06 BET CANCEL DATE MONTH IN ABP SYSTEM (MM)
     *        I2.2,                                                             !G.06 BET CANCEL DATE DAY IN ABP SYSTEM (DD)
     *        I2.2,                                                             !G.06 BET CANCEL DATE HOUR IN ABP SYSTEM (HH24)
     *        I2.2,                                                             !G.06 BET CANCEL DATE MIN IN ABP SYSTEM (MI)
     *        I2.2,                                                             !G.06 BET CANCEL DATE SECOND IN ABP SYSTEM (SS)
     *        I6.6                                                              !G.07 CANCEL AMOUNT (WAGER UNITS)
     *       )
C
       GOTO 10
C
       END
C
C***********************************************************************
C SUBROUTINE: PRINTS THE TRAILER RECORD INTO ASCII FILE.
C
C INPUT:
C        FLUN - LOGICAL UNIT OF TEMPORARY FILE
C        LOGLUN - LOGICAL UNIT OF LOG FILE
C        TOTRECFIL - TOTAL RECORDS WRITTEN SO FAR TO ASCII FILE
C
C OUTPUT:
C        TOTRECTRL - TOTAL RECORDS WRITTEN TO FILE TRAILER SECTION
C
C***********************************************************************
C======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE PRINT_TRAILER(FLUN, LOGLUN, TOTRECFIL, TOTRECTRL)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:MAAUDIT.DEF'
C=======================================================================
C      INPUT
C=======================================================================
       INTEGER*4   FLUN
       INTEGER*4   WFCDC
       CHARACTER*8 WFDAT
       CHARACTER*8 CURDAT
       INTEGER*4   LOGLUN
       INTEGER*4   TOTRECFIL
C=======================================================================
C      OUTPUT
C=======================================================================
       INTEGER*4   TOTRECTRL
C=======================================================================
C      LOCALS
C=======================================================================
       CHARACTER*132 LOGMSG
C=======================================================================
C      GLOBALS
C=======================================================================
C
C=======================================================================
C      WRITE TRAILER RECORD INTO FILE
C=======================================================================
       TOTRECTRL = 0
       WRITE(FLUN, 100) RECTYP_TP,                                              !G.01 RECORD TYPE: TRAILER
     *                  TOTRECFIL + 1                                           !G.02 TOTAL NUMBER OF RECORDS WRITTEN TO ASCII FILE (HEADER AND TRAILER RECORDS INCLUDED)
       TOTRECTRL = TOTRECTRL + 1
       LOGMSG='MAAUDIT .PRINT_TRAILER ['//'Trailer escrito no ficheiro.'//']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
C=======================================================================
C      WRITE FORMAT OF TRAILER RECORD
C=======================================================================
100    FORMAT(
     *        A2,                                                               !G.01 RECORD TYPE: TRAILER
     *        I8.8                                                              !G.02 TOTAL NUMBER OF RECORDS WRITTEN TO ASCII FILE
     *       )
C
       END
C
C***********************************************************************
C SUBROUTINE: UPDATES THE TRANSACTION COUNTERS DEFINED IN
C             COMMON /TRXCDATA/.
C
C GLOBAL VARIABLES UPDATED:
C             TRXCDATA - CONTAINS THE TRANSACTION COUNTING
C
C INPUT:
C        TRABUF - THE TRANSACTION
C        LOGLUN - LOGICAL UNIT OF LOG FILE
C
C OUTPUT:
C        NONE.
C
C***********************************************************************
C
C======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE TRXCOUNT(TRABUF, LOGLUN)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:PRMAGT.DEF'
       INCLUDE 'INCLIB:DESTRA.DEF'
       INCLUDE 'INCLIB:MAAUDIT.DEF'
C=======================================================================
C      LOCALS
C=======================================================================
       INTEGER*4     LOGLUN
       CHARACTER*132 LOGMSG
C=======================================================================
C
       IF(TRABUF(TTYP) .EQ. TWAG) THEN
         TC=TC+1
         TCWAG=TCWAG+1
       ELSEIF(TRABUF(TTYP) .EQ. TEUR) THEN
         TC=TC+1
         TCEUR=TCEUR+1
         IF(TRABUF(TEUTYP) .EQ. TWAG) THEN
           TCEURWAG=TCEURWAG+1
         ELSEIF(TRABUF(TEUTYP) .EQ. TCAN) THEN
           TCEURCAN=TCEURCAN+1
         ELSEIF(TRABUF(TEUTYP) .EQ. TVAL) THEN
           TCEURVAL=TCEURVAL+1
         ELSEIF((TRABUF(TEUCHK) .EQ. 0) .AND.
     *          (TRABUF(TEUSER) .EQ. 0)) THEN
           TCEURERR=TCEURERR+1
         ENDIF
       ELSEIF(TRABUF(TTYP) .EQ. TIGS) THEN
         TC=TC+1
         TCIGS=TCIGS+1
         IF(TRABUF(TIGS_TTYP) .EQ. IGSWAG) THEN
           TCIGSWAG=TCIGSWAG+1
         ELSEIF(TRABUF(TIGS_TTYP) .EQ. IGSCAN) THEN
           TCIGSCAN=TCIGSCAN+1
         ELSEIF(TRABUF(TIGS_TTYP) .EQ. IGSVAL) THEN
           TCIGSVAL=TCIGSVAL+1
         ELSEIF(TRABUF(TIGS_TTYP) .EQ. IGSPAY) THEN
           TCIGSPAY=TCIGSPAY+1
         ELSEIF(TRABUF(TIGS_TTYP) .EQ. IGSREP) THEN
           TCIGSREP=TCIGSREP+1
         ENDIF
       ELSEIF(TRABUF(TTYP) .EQ. TCAN) THEN
         TC=TC+1
         TCCAN=TCCAN+1
       ELSEIF(TRABUF(TTYP) .EQ. TVAL) THEN
         TC=TC+1
         TCVAL=TCVAL+1
       ELSEIF(TRABUF(TTYP) .EQ. TSPE) THEN
         TC=TC+1
         TCSPC=TCSPC+1
       ELSEIF(TRABUF(TTYP) .EQ. TCMD) THEN
         TC=TC+1
         TCCMD=TCCMD+1
       ELSEIF(TRABUF(TTYP) .EQ. TCRS) THEN
         TC=TC+1
         TCIPS=TCIPS+1
       ELSEIF(TRABUF(TTYP) .EQ. TINC) THEN
         TC=TC+1
         TCINC=TCINC+1
       ELSEIF(TRABUF(TTYP) .EQ. TREF) THEN
         TC=TC+1
         TCREF=TCREF+1
       ELSEIF(TRABUF(TTYP) .EQ. TGUI) THEN
         TC=TC+1
         TCGUI=TCGUI+1
       ELSEIF(TRABUF(TTYP) .EQ. TRET) THEN
         TC=TC+1
         TCDEVLOT=TCDEVLOT+1
       ELSE
         TC=TC+1
         TCUNKNOWN=TCUNKNOWN+1
         WRITE(LOGMSG,'(A,I0,A,I0,A)') 'MAAUDIT .TRXCOUNT [TSER#',
     *                                 TRABUF(TSER),
     *                                 ': tipo de transacao desconhecido: ',
     *                                 TRABUF(TTYP),
     *                                 ']'
         CALL LOGGER(TRIM(LOGMSG),WARN,LOGLUN)
       ENDIF
C
       RETURN
C
       END
C
C***********************************************************************
C SUBROUTINE: PRINT THE TRANSACTION COUNTING.
C
C INPUT:
C        LOGLUN - LOGICAL UNIT OF LOG FILE
C
C OUTPUT:
C        NONE.
C
C***********************************************************************
C
C======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE TRXPRINT(LOGLUN)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:PRMAGT.DEF'
       INCLUDE 'INCLIB:MAAUDIT.DEF'
C=======================================================================
C      LOCALS
C=======================================================================
       INTEGER*4     LOGLUN
       CHARACTER*132 LOGMSG
C=======================================================================
C
       LOGMSG='MAAUDIT .TRXPRINT ['//' # Transacoes Processadas '//']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//' Wager      : ',TCWAG,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//' Euro       : ',TCEUR,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//'   Wager    : ',TCEURWAG,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//'   Cancel   : ',TCEURCAN,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//'   Valid    : ',TCEURVAL,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//'   Error    : ',TCEURERR,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//' IGS        : ',TCIGS,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//'   Wager    : ',TCIGSWAG,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//'   Cancel   : ',TCIGSCAN,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//'   Valid    : ',TCIGSVAL,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//'   Payment  : ',TCIGSPAY,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//'   Report   : ',TCIGSREP,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//' Cancel     : ',TCCAN,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//' Validation : ',TCVAL,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//' Special    : ',TCSPC,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//' Command    : ',TCCMD,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//' IPS        : ',TCIPS,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//' Internal   : ',TCINC,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//' Refund     : ',TCREF,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//' GUI        : ',TCGUI,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//' Devolution : ',TCDEVLOT,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//' Unknown    : ',TCUNKNOWN,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'MAAUDIT .TRXPRINT ['//' TOTAL      : ',TC,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
C
       RETURN
C
       END
C
C***********************************************************************
C SUBROUTINE: CREATES THE LOG FILE.
C
C INPUT:
C        NONE.
C OUTPUT:
C        LUN - LOGICAL UNIT OF LOG FILE
C
C***********************************************************************
C
C======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE CREATELOG(LUN)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:MAAUDIT.DEF'
C
C=======================================================================
C      LOCALS
C=======================================================================
       INTEGER*4     ISTHERE
       INTEGER*4     ST
       INTEGER*4     CTIM(2),CDAT(8)
       INTEGER*4     K
       CHARACTER*8   C8TIM
       CHARACTER     CTIM8(8)
       EQUIVALENCE   (C8TIM,CTIM8)
       CHARACTER*132 LOGMSG
       CHARACTER*132 LOGFILENAME                                                !LOG FILENAME
C=======================================================================
C      OUTPUT
C=======================================================================
       INTEGER*4 LUN
C
C=======================================================================
C      FIND A FREE LUN TO USE
C=======================================================================
       CALL FIND_AVAILABLE_LUN(LUN,ST)
       IF(ST .NE. 0) THEN
         TYPE*,IAM(),'Nao foi possivel criar o ficheiro de log'
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
C=======================================================================
C      CREATE AND OPEN THE LOG FILE
C=======================================================================
       CALL ICLOCK(1,CTIM)
       CALL GDATE(CDAT(2),CDAT(3),CDAT(1))
       IF(CDAT(1).LT.77) THEN
         CDAT(1) = CDAT(1) + 2000
       ELSE
         CDAT(1) = CDAT(1) + 1900
       ENDIF
C
       WRITE(C8TIM,'(2A4)') CTIM
       WRITE(LOGFILENAME, 9000) LOGNAME,
     *                          CDAT(1),CDAT(2),CDAT(3),
     *                          (CTIM8(K),K=1,2),
     *                          (CTIM8(K),K=4,5),
     *                          (CTIM8(K),K=7,8),
     *                          LOGEXT

       OPEN(UNIT            = LUN,
     *      FILE            = LOGFILENAME,
     *      STATUS          = 'NEW',
     *      CARRIAGECONTROL = 'LIST',
     *      ACCESS          = 'SEQUENTIAL',
     *      IOSTAT          = ST)
       IF(ST .NE. 0) THEN
         TYPE*, IAM(), ' Erro#', ST, ' A criar o ficheiro de log', TRIM(LOGFILENAME)
         CALL GSTOP(GEXIT_FATAL)
       ELSE
         LOGMSG='MAAUDIT .CREATELOG [Ficheiro de log criado com sucesso: '//TRIM(LOGFILENAME)//']'
         CALL LOGGER(TRIM(LOGMSG),INFO,LUN)
         LOGMSG='MAAUDIT .CREATELOG [Nivel de log das mensagens configurado: '//LTAGS(LOGLEVEL)//']'
         CALL LOGGER(TRIM(LOGMSG),INFO,LUN)
       ENDIF
C=======================================================================
C      FORMAT STATEMENTS
C=======================================================================
9000   FORMAT(A,I4.4,I2.2,I2.2,2A1,2A1,2A1,A)
       RETURN
       END
C
C***********************************************************************
C SUBROUTINE: WRITES THE GIVEN MESSAGE INTO LOG FILE.
C
C INPUT:
C        MSG - MESSAGE TO LOG
C        MSGLEVEL - LOG LEVEL OF THE MESSAGE
C        LUN - LOGICAL UNIT OF LOG FILE
C
C OUTPUT:
C        NONE.
C
C***********************************************************************
C
C======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE LOGGER(MSG,MSGLEVEL,LUN)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIb:MAAUDIT.DEF'
C
C=======================================================================
C      GLOBALS
C=======================================================================
C=======================================================================
C      LOCALS
C=======================================================================
       INTEGER*4     MSGLEN
       INTEGER*4     CTIM(2),CDAT(8)
       CHARACTER*132 BUFFER
       INTEGER*4     ST
C=======================================================================
C      INPUT
C=======================================================================
       INTEGER*4     LUN
       INTEGER*4     MSGLEVEL
       CHARACTER*(*) MSG
C=======================================================================
C      LOGS THE MESSAGE INTO THE FILE
C=======================================================================
       IF((MSGLEVEL .GE. HLEVEL) .AND. (MSGLEVEL .LE. LLEVEL)) THEN
         IF(MSGLEVEL .LE. LOGLEVEL) THEN
           CALL ICLOCK(1,CTIM)
           CALL GDATE(CDAT(2),CDAT(3),CDAT(1))
           IF(CDAT(1).LT.77) THEN
             CDAT(1) = CDAT(1) + 2000
           ELSE
             CDAT(1) = CDAT(1) + 1900
           ENDIF
           BUFFER=MSG
           MSGLEN=MIN(LEN(MSG),132)
           WRITE(LUN, 9000) CDAT(1),CDAT(2),CDAT(3),
     *                      CTIM,
     *                      LTAGS(MSGLEVEL),
     *                      BUFFER(1:MSGLEN)
         ENDIF
       ENDIF
C
C=======================================================================
C      FORMAT STATEMENTS
C=======================================================================
9000   FORMAT(I4.4,'-',I2.2'-',I2.2,1X,2A4,1X,'- [',A,'] -',1X,A)
       RETURN
C
       END
