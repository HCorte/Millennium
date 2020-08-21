C
C      PMXTRACT.FOR
C
C      V01 29-SEP-2011 ACN INITIAL RELEASE
C
C      PROGRAM PMXTRACT (PORTAL-MILLENNIUM TRANSACTION EXTRACTOR)
C
C      THIS PROGRAM GENERATES AN ASCII FILE OF TRANSACTION DATA FIELDS
C      BELONGING TO AGENTS OF PORTAL JSC. THE TRANSACTIONS ARE READ FROM
C      THE MT<CDC>.FIL FILE. THE FILE IS A FORMATTED FILE: A SET OF
C      LINES CORRESPONDING TO A SET OF TRANSACTIONS, EACH LINE IS
C      COMPOSED BY THE VALUES OF THE TRANSACTION FIELDS AND EACH ONE IS
C      SEPARATED BY A DELIMITER CHARACTER.
C
C      THE DATA EXTRACTED OF EACH TRANSACTION DEPENDS ON BOTH GAME
C      AND TRANSACTION TYPE:
C
C      GAME NUMBER     GAME NAME      SET OF TRANSACTION TYPES EXTRACTED
C          1        Totobola Normal          WAG, CAN AND VAL
C          5        Joker                    WAG, CAN AND VAL
C          6        Totoloto Sábado          WAG, CAN AND VAL
C          7        Totoloto Quarta          WAG, CAN AND VAL
C          8        Lotaria Clássica         VAL
C          9        Lotaria Popular          VAL
C         10        Totobola Extra 1         WAG, CAN AND VAL
C
C      THIS PROGRAM GENERATES ALSO A LOG FILE WITH NAME
C      PMXTRACT_<CDC>_<YYYYMMDDHHMISS>.LOG.
C
C      THE LOG LEVEL CONFIGURATION IS DONE IN PMXTRACT.DEF FILE.
C
C      CDC - CONTINUOUS DAY COUNT
C      YYYYMMDDHHMISS - CREATION DATE OF LOG FILE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      THIS ITEM IS THE PROPERTY OF ACCENTURE/SCML.
C
C      COPYRIGHT 2011 ACCENTURE/SCML. ALL RIGHTS RESERVED.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C======OPTIONS /CHECK=NOOVERFLOW/EXT
       PROGRAM PMXTRACT
       IMPLICIT NONE

       INCLUDE '(LIB$ROUTINES)'
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:PRMAGT.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:PMXTRACT.DEF'
C=======================================================================
C      LOCALS
C=======================================================================
       INTEGER*4    YNFLG
       CHARACTER*38 QUESTION
       LOGICAL      EXISTS/.FALSE./
       CHARACTER*20 MTMFNAM/'                    '/
C
       INTEGER*4 ST
       INTEGER*4 FLUN
       INTEGER*4 WFCDC
C
       INTEGER*2     VDAT(LDATE_LEN)
       CHARACTER*132 OUTFILENAME ! OUTPUT FILENAME
       CHARACTER*132 TMPFILENAME ! TEMPORARY FILENAME
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
       LOGMSG='PMXTRACT .MAIN[Inicio do programa.]'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
C
       TYPE*,IAM()
       WRITE(6, 9000) IAM(),
     * 'Copyright 2011 SCML/Accenture. All rights reserved.        '
       TYPE*,IAM()
       WRITE(6, 9000) IAM(),
     * '-----------------------------------------------------------'
       WRITE(6, 9000) IAM(),
     * '<<<<<     FICHEIRO DE AUDITORIA PORTAL-MILLENNIUM     >>>>>'
       WRITE(6, 9000) IAM(),
     * '<<<<<              GERA PMAUDIT_<CDC>.ASC             >>>>>'
       WRITE(6, 9000) IAM(),
     * '<<<<<                                                 >>>>>'
       WRITE(6, 9000) IAM(),
     * '<<<<<                 <CDC> = data CDC                >>>>>'
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
         LOGMSG='PMXTRACT .MAIN[A sair do programa ...]'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
         CALL GSTOP(GEXIT_OPABORT)
       ENDIF
       WRITE(MTMFNAM,9100) WFCDC
C=======================================================================
C      INQUIRE ABOUT FILE'S EXISTENCE
C=======================================================================
       INQUIRE(FILE=MTMFNAM, EXIST = EXISTS)
       IF(.NOT. EXISTS) THEN
         TYPE*, IAM(), ' Ficheiro nao encontrado: ',TRIM(MTMFNAM)
         LOGMSG='PMXTRACT .MAIN['//'Ficheiro nao encontrado: '//TRIM(MTMFNAM)//']'
         CALL LOGGER(TRIM(LOGMSG),WARN,LOGLUN)
         GOTO 10
       END IF
C=======================================================================
C      ASK THE OPERATOR IF THE FILE THAT WAS FOUND IS TO BE PROCESSED
C=======================================================================
       WRITE(QUESTION, 9400) TRIM(MTMFNAM)
       CALL PRMYESNO(QUESTION,YNFLG)
       IF(YNFLG .EQ. 1) GOTO 20
       IF(YNFLG .EQ. 2) GOTO 10
       IF(YNFLG .EQ. 3) THEN
         LOGMSG='PMXTRACT .MAIN[A sair do programa ...]'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
         CALL GSTOP(GEXIT_OPABORT)
       ENDIF
C=======================================================================
C      GET DATA FROM AGENT SALES FILE (ASF)
C=======================================================================
20     CONTINUE
       TYPE*, IAM()
       CALL LOADASFAGTP(LOGLUN)
C=======================================================================
C      GET DATA FROM SYSTEM CONFIGURATION FILE (SCF)
C=======================================================================
       TYPE*, IAM()
       CALL LOADSCFCONF(LOGLUN)
C=======================================================================
C      GET DATA FROM DAILY ACTIVITY FILE (DAF)
C=======================================================================
       TYPE*, IAM()
       CALL LOADDAFHDR(WFCDC, LOGLUN)
C=======================================================================
C      DEFINE FILE NAMES
C=======================================================================
       WRITE (OUTFILENAME, 9200) OUTFILEPREF, WFCDC, OUTFILEEXT ! OUTPUT FILE NAME
       WRITE (TMPFILENAME, 9300) TMPFILEPREF, WFCDC, TMPFILEEXT ! TEMPORARY FILE NAME
C=======================================================================
C      FIND A FREE LUN TO USE
C=======================================================================
       CALL FIND_AVAILABLE_LUN(FLUN,ST)
       IF(ST .NE. 0) THEN
         TYPE*, IAM(), 'Nao foi possivel abrir o ficheiro '//TRIM(TMPFILENAME)//': LUN nao disponivel'
         WRITE(LOGMSG,'(A)') 'PMXTRACT .MAIN[Nao foi possivel abrir o ficheiro '//TRIM(TMPFILENAME)//': LUN nao disponivel'//']'
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
         WRITE(LOGMSG,'(A,I0,A)') 'PMXTRACT .MAIN['//'Erro#',
     *                             ST,
     *                             ' a abrir o ficheiro '//TRIM(TMPFILENAME)//']'
         CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
         CLOSE(LOGLUN)
         CALL GSTOP(GEXIT_FATAL)
       ELSE
         LOGMSG='PMXTRACT .MAIN['//'Ficheiro '//TRIM(TMPFILENAME)//' aberto com sucesso.'//']'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       ENDIF
C=======================================================================
C      START EXTRACTION TO TEMPORARY FILE
C=======================================================================
       TYPE*, IAM()
       TYPE*, IAM(),'A gerar o ficheiro ', TRIM(OUTFILENAME), ' ...'
       CALL LOGGER('PMXTRACT .MAIN['//'A gerar o ficheiro '//TRIM(OUTFILENAME)//' ...]',INFO,LOGLUN)
       TYPE*, IAM()
       CALL START_EXTRACTION(FLUN, WFCDC, LOGLUN, ST)
C
       CLOSE(FLUN) ! CLOSE TEMPORARY FILE
       LOGMSG='PMXTRACT .MAIN['//'Ficheiro '//TRIM(TMPFILENAME)//' fechado.'//']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
C
       IF(ST .NE. 0) THEN
         CLOSE(LOGLUN)
         CALL GSTOP(GEXIT_FATAL)
       ELSE
         LOGMSG='PMXTRACT .MAIN['//'A renomear o ficheiro '//TRIM(TMPFILENAME)//' para '//TRIM(OUTFILENAME)//' ...]'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
         CALL LIB$RENAME_FILE(TMPFILENAME,OUTFILENAME) ! RENAME TEMPORARY FILE
         TYPE*, IAM(), 'Ficheiro ',TRIM(OUTFILENAME),' gerado com sucesso:'
         LOGMSG='PMXTRACT .MAIN['//'Ficheiro '//TRIM(OUTFILENAME)//' gerado com sucesso.'//']'
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
         LOGMSG='PMXTRACT .MAIN['//'Fim do programa.'//']'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
         CLOSE(LOGLUN)
         CALL GSTOP(GEXIT_SUCCESS)
       ENDIF
C=======================================================================
C      FORMAT STATEMENTS
C=======================================================================
9000   FORMAT(1X,A18,A59)
9100   FORMAT('BCKS:MT',I0,'.FIL')
9200   FORMAT(A,I0,A)
9300   FORMAT(A,I0,A)
9400   FORMAT('Processar o ficheiro ',A, ' ?')
       END

C***********************************************************************
C SUBROUTINE: EXTRACTS TRANSACTION DATA FROM THE MT<CDC>.FIL FILE AND
C             WRITE THEM TO A TEMPORARY FILE IN A FORMATTED WAY. THE
C             DATA EXTRACTED OF EACH TRANSACTION DEPENDS ON THE GAME
C             AND THE TRANSACTION TYPE.
C
C      GAME NUMBER     GAME NAME       SET OF TRANSACTION TYPES
C          1        Totobola Normal      WAG, CAN AND VAL
C          5        Joker                WAG, CAN AND VAL
C          6        Totoloto Sábado      WAG, CAN AND VAL
C          7        Totoloto Quarta      WAG, CAN AND VAL
C          8        Lotaria Clássica     VAL
C          9        Lotaria Popular      VAL
C         10        Totobola Extra 1     WAG, CAN AND VAL
C
C INPUT:
C        FLUN - LOGICAL UNIT OF TEMPORARY FILE
C        WFCDC - CDC DATE
C        LOGLUN - LOGICAL UNIT OF LOG FILE
C
C OUTPUT:
C        ST - STATUS (OK - ST = 0, NOT OK - ST != 0)
C
C***********************************************************************
C======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE START_EXTRACTION(FLUN, WFCDC, LOGLUN, ST)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:PRMLOG.DEF'
       INCLUDE 'INCLIB:DESTRA.DEF'
       INCLUDE 'INCLIB:PRMAGT.DEF'
       INCLUDE 'INCLIB:PMXTRACT.DEF'
C=======================================================================
C      INPUT
C=======================================================================
       INTEGER*4 FLUN
       INTEGER*4 WFCDC
       INTEGER*4 LOGLUN
C=======================================================================
C      OUTPUT
C=======================================================================
       INTEGER*4 ST
C=======================================================================
C      LOCALS
C=======================================================================
       CHARACTER*132 LOGMSG
       INTEGER*4     SER
       INTEGER*4     LOGREC(LREC*3)
       INTEGER*4     JUL, SERIAL, CHECK
       INTEGER*4     CANJUL, CANSERIAL, CANCHECK
       INTEGER*4     VALJUL, VALSERIAL, VALCHECK
       INTEGER*4     K
       INTEGER*2     VDAT(LDATE_LEN)
       CHARACTER*10  C10DAT
       CHARACTER     CDAT10(10)
       EQUIVALENCE   (C10DAT,CDAT10)
       INTEGER*2     DBUF(12)
       CHARACTER*8   DDRW
       LOGICAL       ISAGTP/.FALSE./
       INTEGER*4     TCKS
       LOGICAL       EOT/.FALSE./
       INTEGER*4     BDGTAB(MAXGAM), EDGTAB(MAXGAM)
       CHARACTER*8   BDCGTAB(MAXGAM), EDCGTAB(MAXGAM)
       INTEGER*4     MLUN
       PARAMETER     (MLUN=1)
       INTEGER*4     I4MTMFNAM(5)
       CHARACTER*20  MTMFNAM/'                    '/
       EQUIVALENCE   (I4MTMFNAM,MTMFNAM)
C=======================================================================
C      GLOBALS
C=======================================================================
C      AGENT SYSTEM DATA
       INTEGER*4 ASF_AGTPTAB(NUMAGT), ASF_NUMAGTP
       COMMON /ASFDATA/ ASF_AGTPTAB, ASF_NUMAGTP
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
         LOGMSG='PMXTRACT .START_EXTRACT['//'Nao foi possivel abrir o ficheiro '//TRIM(MTMFNAM)//'.]'
         CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
         CLOSE(LOGLUN)
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
       LOGMSG='PMXTRACT .START_EXTRACT['//'Ficheiro '//TRIM(MTMFNAM)//' aberto com sucesso.'//']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
C=======================================================================
       ST=0
       SER=0
       LOGMSG='PMXTRACT .START_EXTRACT['//'A iniciar a extraccao de dados ...'//']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
10     CONTINUE
       ISAGTP=.FALSE.
       CALL READTMF(LOGREC,SER,EOT)
       IF(EOT) THEN
         LOGMSG='PMXTRACT .START_EXTRACT['//'Extraccao de dados finalizada.'//']'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
         ST=0
         CLOSE(MLUN)
         LOGMSG='PMXTRACT .START_EXTRACT['//'Ficheiro '//TRIM(MTMFNAM)//' fechado.'//']'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
         CALL TRXPRINT(LOGLUN)
         RETURN
       ENDIF
C
       CALL LOGTRA(TRABUF, LOGREC) ! GET NEXT TRANSACTION FROM MTMF
C
       CALL TRXCOUNT(TRABUF,LOGLUN) ! FOR STATISTICS
C=======================================================================
C      WAGERS
C=======================================================================
       IF((TRABUF(TTYP) .EQ. TWAG) .AND.
     *    (TRABUF(TSTAT) .EQ. GOOD) .AND.
     *    (TRABUF(TERR) .EQ. NOER)) THEN
         CALL CHECKAGTP(TRABUF(TAGT), ISAGTP)
         IF(ISAGTP) THEN
           IF((TRABUF(TGAMTYP) .EQ. TLTO) .OR.
     *        (TRABUF(TGAMTYP) .EQ. TSPT) .OR.
     *        (TRABUF(TGAMTYP) .EQ. TKIK)) THEN
!             VDAT(VCDC)=WFCDC
            VDAT(VCDC)=TRABUF(TCDC)
            CALL LCDATE(VDAT)
            WRITE(C10DAT, '(5A2)') (VDAT(K),K=9,13)
            DBUF(VCDC)=TRABUF(TCDC)
            CALL CDATE(DBUF)
            JUL=DBUF(VJUL)
            CALL OUTGEN(TRABUF(TCDC), TRABUF(TSER), SERIAL, CHECK)
            CALL DDRWBECCC(LOGLUN, TRABUF(TGAM), TRABUF(TWBEG), TRABUF(TWEND), BDGTAB, BDCGTAB, EDGTAB, EDCGTAB)
!            IF(BDGTAB(TRABUF(TGAM)) .NE. TRABUF(TWBEG)) THEN
!              CALL LOADDRWCCC(TRABUF(TGAM), TRABUF(TWBEG), LOGLUN, BDGTAB,
!     *                        BDCGTAB)
!            ENDIF
!            IF(EDGTAB(TRABUF(TGAM)) .NE. TRABUF(TWEND)) THEN
!              CALL LOADDRWCCC(TRABUF(TGAM), TRABUF(TWEND), LOGLUN, EDGTAB,
!     *                         EDCGTAB)
!            ENDIF
            WRITE(FLUN, 905) TRABUF(TAGT),
     *                      TRABUF(TGAMTYP),
     *                      TRABUF(TGAMIND),
     *                      JUL, SERIAL, CHECK,
     *                      JUL, SERIAL, CHECK,
     *                      (CDAT10(K),K=7,10), (CDAT10(K),K=4,5),
     *                                          (CDAT10(K),K=1,2),! DATE
     *                      DISTIM(TRABUF(TTIM)), ! TIME
     *                      TRABUF(TWTOT), ! WAGER TOTAL AMOUNT
     *                      TRIM(BDCGTAB(TRABUF(TGAM))),
     *                      TRIM(EDCGTAB(TRABUF(TGAM)))
           ENDIF
         ENDIF
C=======================================================================
C      CANCELLATIONS
C=======================================================================
!       ELSEIF((TRABUF(TTYP) .EQ. TWAG) .AND.
!     *       (TRABUF(TSTAT) .EQ. VOID) .AND.
!     *       (TRABUF(TERR) .EQ. NOER)) THEN
       ELSEIF((TRABUF(TTYP) .EQ. TCAN) .AND.
     *        (TRABUF(TSTAT) .EQ. GOOD) .AND.
     *        (TRABUF(TERR) .EQ. NOER)) THEN
         CALL CHECKAGTP(TRABUF(TAGT), ISAGTP)
         IF(ISAGTP) THEN
           IF((TRABUF(TGAMTYP) .EQ. TLTO) .OR.
     *        (TRABUF(TGAMTYP) .EQ. TSPT) .OR.
     *        (TRABUF(TGAMTYP) .EQ. TKIK)) THEN
!              VDAT(VCDC)=WFCDC
             VDAT(VCDC)=TRABUF(TCDC)
             CALL LCDATE(VDAT)
             WRITE(C10DAT, '(5A2)') (VDAT(K),K=9,13)
             DBUF(VCDC) = TRABUF(TCDC)
             CALL CDATE(DBUF)
             JUL = DBUF(VJUL)
             CANJUL = JUL
!             CALL OUTGEN(TRABUF(TCDC), TRABUF(TSER), SERIAL, CHECK)
!             CALL OUTGEN(TRABUF(TCDC), TRABUF(TWCSER), CANSERIAL,
!     *                   CANCHECK)
             CALL OUTGEN(TRABUF(TCDC), TRABUF(TSER), CANSERIAL,
     *                   CANCHECK)
             CALL OUTGEN(TRABUF(TCDC), TRABUF(TWCSER), SERIAL,
     *                   CHECK)
             WRITE(FLUN, 906) TRABUF(TAGT),
     *                       TRABUF(TGAMTYP),
     *                       TRABUF(TGAMIND),
     *                       CANJUL, CANSERIAL, CANCHECK,
     *                       JUL, SERIAL, CHECK,
     *                       (CDAT10(K),K=7,10), (CDAT10(K),K=4,5),
     *                                           (CDAT10(K),K=1,2),! DATE
     *                       DISTIM(TRABUF(TTIM)), ! TIME
     *                       TRABUF(TWTOT) ! WAGER TOTAL AMOUNT
           ENDIF
         ENDIF
C=======================================================================
C      VALIDATIONS
C=======================================================================
       ELSEIF((TRABUF(TTYP) .EQ. TVAL) .AND.
     *        (TRABUF(TSTAT) .EQ. GOOD) .AND.
     *        (TRABUF(TERR) .EQ. NOER) .AND.
     *        (TRABUF(TVCODE) .EQ. 5)) THEN
         CALL CHECKAGTP(TRABUF(TAGT), ISAGTP)
         IF(ISAGTP) THEN
           IF((TRABUF(TGAMTYP) .EQ. TLTO) .OR.
     *        (TRABUF(TGAMTYP) .EQ. TSPT) .OR.
     *        (TRABUF(TGAMTYP) .EQ. TKIK)) THEN
!              VDAT(VCDC)=WFCDC
             VDAT(VCDC)=TRABUF(TCDC)
             CALL LCDATE(VDAT)
             WRITE(C10DAT, '(5A2)') (VDAT(K),K=9,13)
             ! VALIDATION SERIAL NUMBER
             DBUF(VCDC)=TRABUF(TCDC)
             CALL CDATE(DBUF)
             JUL = DBUF(VJUL)
             CALL OUTGEN(TRABUF(TCDC), TRABUF(TSER), SERIAL, CHECK)
             ! WAGER SERIAL NUMBER
             DBUF(VCDC)=TRABUF(TVCDC)
             CALL CDATE(DBUF)
             VALJUL = DBUF(VJUL)
             CALL OUTGEN(TRABUF(TVCDC), TRABUF(TVSER), VALSERIAL,
     *                   VALCHECK)

             WRITE(FLUN, 907) TRABUF(TAGT),
     *                       TRABUF(TGAMTYP),
     *                       TRABUF(TGAMIND),
     *                       JUL, SERIAL, CHECK,
     *                       VALJUL, VALSERIAL, VALCHECK,
     *                       (CDAT10(K),K=7,10), (CDAT10(K),K=4,5),
     *                       (CDAT10(K),K=1,2),! DATE
     *                       DISTIM(TRABUF(TTIM)), ! TIME
     *                       TRABUF(TVPAY)+TRABUF(TVKPAY) ! WAGER TOTAL AMOUNT
           ENDIF
         ENDIF
C=======================================================================
C      PASSIVE VALIDATIONS
C=======================================================================
       ELSEIF((TRABUF(TTYP) .EQ. TVAL) .AND.
     *       (TRABUF(TSTAT) .EQ. GOOD) .AND.
     *       (TRABUF(TERR) .EQ. NOER)) THEN
         CALL CHECKAGTP(TRABUF(TAGT), ISAGTP)
         IF(ISAGTP) THEN
           IF((TRABUF(TGAMTYP) .EQ. TPAS)) THEN
!             VDAT(VCDC)=WFCDC
             VDAT(VCDC)=TRABUF(TCDC)
             CALL LCDATE(VDAT)
             WRITE(C10DAT, '(5A2)') (VDAT(K),K=9,13)
             ! VALIDATION SERIAL NUMBER
             DBUF(VCDC) = TRABUF(TCDC)
             CALL CDATE(DBUF)
             JUL = DBUF(VJUL)
             CALL OUTGEN(TRABUF(TCDC), TRABUF(TSER), SERIAL, CHECK)
C
             DO TCKS=1, TRABUF(TPTCK)
               IF(TRABUF(TPSTS1+OFFTRA*(TCKS-1)).EQ.VWINNER) THEN
                 WRITE(FLUN, 908) TRABUF(TAGT),
     *                           TRABUF(TGAMTYP),
     *                           TRABUF(TGAMIND),
     *                           JUL, SERIAL, CHECK,
     *                           (CDAT10(K),K=7,10), (CDAT10(K),K=4,5),
     *                                               (CDAT10(K),K=1,2),! DATE
     *                           DISTIM(TRABUF(TTIM)), ! TIME
     *                           TRABUF(TPPAY1+OFFTRA*(TCKS-1)), ! PRIZE
     *                           TRABUF(TPNUM1+OFFTRA*(TCKS-1)), ! TICKET NUMBER
     *                           TRABUF(TPSER1+OFFTRA*(TCKS-1)), ! SERIE
     *                           TRABUF(TPTEN1+OFFTRA*(TCKS-1)), ! FRACTION
     *                           TRABUF(TPEMIS1+OFFTRA*(TCKS-1)) ! EMISSION NUMBER
               ENDIF
             ENDDO
           ENDIF
         ENDIF
       ENDIF
C
!              WG   |  AGNBR |GMTYP |GMIDX |WAG_JD  |WAG_SER |WAG_CD  |WAG_JD  |WAG_SER |WAG_CD  |DATE  |AMOUNT|  0  | 0  | CONTEST/YEAR    |CONTEST/YEAR
C=======================================================================
C                TIME STAMP FORMAT YYYY-MM-DD HH24:MI:SS
C=======================================================================
905    FORMAT('WG','|',I7.7,'|',I0,'|',I0,'|',I3.3,'|',I8.8,'|',I3.3,
     *             '|',I3.3,'|',I8.8,'|',I3.3,'|',4A1,'-',2A1,'-',2A1,
     *              ' ', A8,'|',I0,'|0|0|',A,'|',A)
906    FORMAT('CW','|',I7.7,'|',I0,'|',I0,'|',I3.3,'|',I8.8,'|',I3.3,
     *            '|',I3.3,'|',I8.8,'|',I3.3,'|',4A1,'-',2A1,'-',2A1,
     *              ' ', A8,'|0|',I0,'|0| | ')
907    FORMAT('VW','|',I7.7,'|',I0,'|',I0,'|',I3.3,'|',I8.8,'|',I3.3,
     *             '|',I3.3,'|',I8.8,'|',I3.3,'|',4A1,'-',2A1,'-',2A1,
     *              ' ', A8,'|0|0|',I0,'| | ')
908    FORMAT('VL','|',I7.7,'|',I0,'|',I0,'|',I3.3,'|',I8.8,'|',I3.3,
     *             '|',4A1,'-',2A1,'-',2A1,' ',A8,'|',I0,'|',I0,'|',
     *                 I2.2,'|',I2.2,'|',I0)
C=======================================================================
C                     TIME STAMP FORMAT YYYY-MM-DD
C=======================================================================
C905    FORMAT('WG','|',I7.7,'|',I0,'|',I0,'|',I3.3,'|',I8.8,'|',I3.3,
C     *             '|',I3.3,'|',I8.8,'|',I3.3,'|',4A1,'-',2A1,'-',2A1,
C     *              '|',I0,'|0|0|',A,'|',A)
C906    FORMAT('CW','|',I7.7,'|',I0,'|',I0,'|',I3.3,'|',I8.8,'|',I3.3,
C     *            '|',I3.3,'|',I8.8,'|',I3.3,'|',4A1,'-',2A1,'-',2A1,
C     *              '|0|',I0,'|0| | ')
C907    FORMAT('VW','|',I7.7,'|',I0,'|',I0,'|',I3.3,'|',I8.8,'|',I3.3,
C     *             '|',I3.3,'|',I8.8,'|',I3.3,'|',4A1,'-',2A1,'-',2A1,
C     *              '|0|0|',I0,'| | ')
C908    FORMAT('VL','|',I7.7,'|',I0,'|',I0,'|',I3.3,'|',I8.8,'|',I3.3,
C     *             '|',4A1,'-',2A1,'-',2A1,'|',I0,'|',I0,'|',
C     *                 I2.2,'|',I2.2,'|',I0)
       GOTO 10
       END

C***********************************************************************
C
C SUBROUTINE: CONVERTS TIME FOR DISPLAY
C
C INPUT:
C        NUM - NUMBER OF SECONDS TO CONVERT
C
C OUTPUT:
C        HH:MIN:SS
C***********************************************************************
C
       DOUBLE PRECISION FUNCTION DISTIM(NUM)
       IMPLICIT NONE
C
       CHARACTER*8 C_TIME
       REAL*8      R_TIME
       INTEGER*4   SEC, NUM, HR, MIN
       EQUIVALENCE (C_TIME, R_TIME)
C
       SEC=NUM
       IF(SEC.GT.'40000000'X) SEC=SEC-'40000000'X
       HR=SEC/3600
       SEC=SEC-(HR*3600)
       MIN=SEC/60
       SEC=SEC-(MIN*60)
       IF(HR.GT.99) HR=99
       WRITE (C_TIME,900) HR,MIN,SEC
900    FORMAT(I2.2,':',I2.2,':',I2.2)
C
       DISTIM=R_TIME
C
       RETURN
       END

C***********************************************************************
C SUBROUTINE: LOADS THE PORTAL AGENTS NUMBER FROM ASF FILE. A PORTAL
C             AGENT IS THE ONE WHICH HAVE THE VALUE 007456 AS THE SAP
C             NUMBER.
C
C GLOBAL VARIABLES LOADED:
C             ASF_AGTPTAB - ARRAY WITH ALL THE PORTAL AGENTS
C             ASF_NUMAGTP - TOTAL NUMBER OF PORTAL AGENTS
C
C INPUT:
C       LOGLUN - LOGICAL UNIT OF LOG FILE.
C
C OUPUT:
C       NONE.
C
C***********************************************************************
C
C======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE LOADASFAGTP(LOGLUN)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:ASFSUBS.DEF'
       INCLUDE 'INCLIB:PMXTRACT.DEF'
C
C=======================================================================
C      LOCALS
C=======================================================================
       INTEGER*4   I, K, COUNT
       INTEGER*4   ANUM, CERR, ST
       INTEGER*4   SAPNUM
       CHARACTER*7 CAGTNBR
       CHARACTER   CTEMP(7)
       EQUIVALENCE (CAGTNBR, CTEMP)
       INTEGER*4 ASFNAM(5)
       DATA ASFNAM/'GXTS','K:AS','F.FI','L   ','    '/
       CHARACTER*132 LOGMSG
C=======================================================================
C      INPUT
C=======================================================================
       INTEGER*4     LOGLUN
C=======================================================================
C      GLOBALS
C=======================================================================
C      AGENT SYSTEM DATA
       INTEGER*4 ASF_AGTPTAB(NUMAGT), ASF_NUMAGTP
       COMMON /ASFDATA/ ASF_AGTPTAB, ASF_NUMAGTP
C=======================================================================
C
       TYPE*, IAM(), 'A obter os agentes do Portal JSC do ficheiro ASF ...'
       CALL LOGGER('PMXTRACT .LOADASFAGTP['//'A obter os agentes do Portal JSC do ficheiro ASF ...]',INFO,LOGLUN)

       ST=-1
       COUNT=0
       ASF_NUMAGTP=0
       CALL FASTSET(0, ASF_AGTPTAB, NUMAGT)
C=======================================================================
C      OPEN THE ASF.FIL AND GET PORTAL AGENTS NUMBER
C=======================================================================
       LUN=ASF
       CALL OPENW(LUN, ASFNAM, 4, 0, 0, ST)
       IF(ST .NE. 0)THEN
         CALL IOFERR(ASFNAM, 1, ST, 0)
         TYPE *, IAM(), ' Nao foi possivel abrir o ficheiro ASF.FIL'
         WRITE(LOGMSG,'(A)')
     *     'PMXTRACT .LOADASFAGTP['//' Nao foi possivel abrir o ficheiro ASF.FIL'//']'
         CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
         CLOSE(LOGLUN)
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
C
       LOGMSG='PMXTRACT .LOADASFAGTP['//' Ficheiro ASF.FIL aberto com sucesso.'//']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
C
       CALL IOINIT(FDB, LUN, ASFSEC*RECSPERBKT*256)
       DO I=1,NUMAGT
         ANUM=0
         CALL READASF(I, ASFREC, ST)
         IF(ST .NE. 0) THEN
         	 CALL IOFERR(ASFNAM, 2, ST, I)
           TYPE*, IAM(), ' Erro a ler o terminal ',I
           WRITE(LOGMSG,'(A,I,A)')
     *         'PMXTRACT .LOADASFAGTP['//' Erro a ler o terminal ',I,'.]'
           CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
           CLOSE(LUN)
           LOGMSG='PMXTRACT .LOADASFAGTP['//' Ficheiro ASF.FIL fechado.'//']'
           CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
           !CALL CLOSEFIL(FDB)
           CLOSE(LOGLUN)
           CALL GSTOP(GEXIT_FATAL)
         ENDIF
         CALL ASCBIN(ASFINF, SSAPN, LSAPN, SAPNUM, CERR)
         IF((CERR .EQ. 0) .AND. (SAPNUM .GT. 0) .AND.
     *      (SAPNUM .EQ. SAPAGTP)) THEN
           CALL ASCBIN(ASFINF, SAGNO, LAGNO, ANUM, CERR)
           IF(CERR .LT. 0) THEN
             WRITE(5, 9001) IAM(), I
             WRITE(LOGMSG,'(A,I5,A)') 'PMXTRACT .LOADASFAGTP[Terminal ',
     *                               I,
     *                               ' - numero de agente invalido.]'
             CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
             CLOSE(LUN)
             LOGMSG='PMXTRACT .LOADASFAGTP['//' Ficheiro ASF.FIL fechado.'//']'
             CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
             CLOSE(LOGLUN)
             !CALL CLOSEFIL(FDB)
             CALL GSTOP(GEXIT_FATAL)
           ELSE
             COUNT=COUNT+1
             ASF_AGTPTAB(COUNT)=ANUM
             WRITE(CAGTNBR, 9000) ANUM
             WRITE(LOGMSG,'(A,2A1,A1,5A1,A)')
     *                      'PMXTRACT .LOADASFAGTP['//' Foi obtido o agente numero ',
     *                      (CTEMP(K),K=1,2),
     *                      '-',
     *                      (CTEMP(K),K=3,7),
     *                      ']'
             CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
           ENDIF
         ENDIF
       ENDDO
       CLOSE(LUN)
       LOGMSG='PMXTRACT .LOADASFAGTP['//' Ficheiro ASF.FIL fechado.'//']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       !CALL CLOSEFIL(FDB)
       ASF_NUMAGTP=COUNT
C
       IF(ASF_NUMAGTP .EQ. 0) THEN
         TYPE*, IAM(), ' Nao foram encontrados agentes!'
         CALL LOGGER('PMXTRACT .LOADASFAGTP['//'Nao foram encontrados agentes!'//']',FATAL,LOGLUN)
         CLOSE(LOGLUN)
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
       TYPE*, IAM(), ' ', ASF_NUMAGTP, 'agentes obtidos'
       WRITE(LOGMSG, '(A,I0,1X,A)') 'PMXTRACT .LOADASFAGTP[ Foram obtidos ',ASF_NUMAGTP, 'agentes.]'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
C=======================================================================
C      FORMAT STATEMENTS
C=======================================================================
9000   FORMAT(I7.7)
9001   FORMAT(1X, A18, ' Terminal ', I5,' - numero de agente invalido')

       RETURN
       END

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
       INCLUDE 'INCLIB:PMXTRACT.DEF'
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
         WRITE(LOGMSG,'(A,I0,A,I0,A)') 'PMXTRACT .TRXCOUNT[TSER#',
     *                                 TRABUF(TSER),
     *                                 ': tipo de transaccao desconhecido: ',
     *                                 TRABUF(TTYP),
     *                                 ']'
         CALL LOGGER(TRIM(LOGMSG),WARN,LOGLUN)
       ENDIF
C
       RETURN
       END

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
       INCLUDE 'INCLIB:PMXTRACT.DEF'
C=======================================================================
C      LOCALS
C=======================================================================
       INTEGER*4     LOGLUN
       CHARACTER*132 LOGMSG
C=======================================================================
C
       LOGMSG='PMXTRACT. TRXPRINT['//'   Total de Transaccoes   '//']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'PMXTRACT. TRXPRINT['//' Wager      : ',TCWAG,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'PMXTRACT. TRXPRINT['//' Euro       : ',TCEUR,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'PMXTRACT. TRXPRINT['//'   Wager    : ',TCEURWAG,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'PMXTRACT. TRXPRINT['//'   Cancel   : ',TCEURCAN,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'PMXTRACT. TRXPRINT['//'   Valid    : ',TCEURVAL,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'PMXTRACT. TRXPRINT['//'   Error    : ',TCEURERR,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'PMXTRACT. TRXPRINT['//' Cancel     : ',TCCAN,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'PMXTRACT. TRXPRINT['//' Validation : ',TCVAL,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'PMXTRACT. TRXPRINT['//' Special    : ',TCSPC,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'PMXTRACT. TRXPRINT['//' Command    : ',TCCMD,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'PMXTRACT. TRXPRINT['//' IPS        : ',TCIPS,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'PMXTRACT. TRXPRINT['//' Internal   : ',TCINC,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'PMXTRACT. TRXPRINT['//' Refund     : ',TCREF,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'PMXTRACT. TRXPRINT['//' GUI        : ',TCGUI,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'PMXTRACT. TRXPRINT['//' Devolution : ',TCDEVLOT,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'PMXTRACT. TRXPRINT['//' Unknown    : ',TCUNKNOWN,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       WRITE(LOGMSG,'(A,I,A)') 'PMXTRACT. TRXPRINT['//' TOTAL      : ',TC,']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
C
       RETURN
       END

C***********************************************************************
C SUBROUTINE: CHECKS IF A GIVEN AGENT NUMBER IS A PORTAL AGENT.
C
C INPUT:
C        KEYAGT - AGENT NUMBER TO SEARCH
C
C OUTPUT:
C        AGTPFOUND - BOOLEAN FLAG INDICATING IF THE KEYAGT IS A PORTAL
C                    AGENT
C***********************************************************************
C
C======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE CHECKAGTP(KEYAGT, AGTPFOUND)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:PRMAGT.DEF'
C=======================================================================
C      GLOBALS
C=======================================================================
C      AGENT SYSTEM DATA
       INTEGER*4 ASF_AGTPTAB(NUMAGT), ASF_NUMAGTP
       COMMON /ASFDATA/ ASF_AGTPTAB, ASF_NUMAGTP
C=======================================================================
C
       INTEGER*4 I
       INTEGER*4 KEYAGT
       LOGICAL   AGTPFOUND
C
       DO I=1, ASF_NUMAGTP
         IF(ASF_AGTPTAB(I) .EQ. KEYAGT) THEN
           AGTPFOUND=.TRUE.
           RETURN
         ENDIF
       ENDDO
C
       RETURN
       END

C***********************************************************************
C SUBROUTINE: LOADS DATA FROM SCF FILE.
C
C GLOBAL VARIABLES LOADED:
C        SCF_GNTTAB  - GAME NUMBER -> GAME TYPE TABLE
C        SCF_GFNAMES - GAME FILE NAMES
C INPUT:
C        LOGLUN - LOGICAL UNIT OF LOG FILE
C
C***********************************************************************
C
C======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE LOADSCFCONF(LOGLUN)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:CONCOM.DEF'
       INCLUDE 'INCLIB:RECSCF.DEF'
       INCLUDE 'INCLIB:PMXTRACT.DEF'
C=======================================================================
C      GLOBALS
C=======================================================================
C      SYSTEM CONFIGURATION DATA
       INTEGER*4 SCF_GNTTAB(2,MAXGAM)  ! GAME NUMBER -> GAME TYPE TABLE
       INTEGER*4 SCF_GFNAMES(5,MAXGAM) ! GAME FILE NAMES
       COMMON /SCFDATA/ SCF_GNTTAB, SCF_GFNAMES
C=======================================================================
C      INPUT
C=======================================================================
       INTEGER*4 LOGLUN
C=======================================================================
C      LOCALS
C=======================================================================
       INTEGER*4 LUN
       LOGICAL*4 ISTHERE
       INTEGER*4 SFDB(7)
       INTEGER*4 ST
       INTEGER*4 SCFNAM(5)
       DATA SCFNAM/'GXTS','K:SC','F.FI','L   ','    '/
       CHARACTER*132 LOGMSG
C
       TYPE*, IAM(), 'A obter dados do ficheiro SCF ...'
       CALL LOGGER('PMXTRACT .LOADSCFCONF['//'A obter dados do ficheiro SCF ...]',INFO,LOGLUN)
C
       ST=-1
C=======================================================================
C      FIND A FREE LUN TO USE
C=======================================================================
       CALL FIND_AVAILABLE_LUN(LUN,ST)
       IF(ST .NE. 0) THEN
         TYPE*, IAM(), 'Nao foi possivel abrir o ficheiro SCF.FIL: LUN nao disponivel'
         WRITE(LOGMSG,'(A)') 'PMXTRACT .LOADSCFCONF[Nao foi possivel abrir o ficheiro SCF.FIL: LUN nao disponivel'//']'
         CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
         CLOSE(LOGLUN)
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
C=======================================================================
C      OPEN THE SCF.FIL AND READ THE CONFIGURATION
C=======================================================================
       CALL OPENW(LUN,SCFNAM,4,0,0,ST)
       IF(ST .NE. 0) THEN
         CALL IOFERR(SCFNAM,1,ST,0)
         TYPE *, IAM(), ' Nao foi possivel abrir o ficheiro SCF.FIL'
         WRITE(LOGMSG,'(A)')
     *     'PMXTRACT .LOADSCFCONF['//' Nao foi possivel abrir o ficheiro SCF.FIL'//']'
         CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
         CLOSE(LOGLUN)
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
C
       LOGMSG='PMXTRACT .LOADSCFCONF['//' Ficheiro SCF.FIL aberto com sucesso.'//']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
C
       CALL IOINIT(SFDB,LUN,SCFSEC*256)
       CALL READW(SFDB,1,SCFREC,ST)
       IF(ST .NE. 0) THEN
         CALL IOFERR(SCFNAM,2,ST,1)
         CALL CLOSEFIL(SFDB)
         LOGMSG='PMXTRACT .LOADSCFCONF['//' Ficheiro SCF.FIL fechado.'//']'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
         WRITE(LOGMSG,'(A,I0,A)')
     *     'PMXTRACT .LOADSCFCONF['//' Nao foi possivel ler o registo#',
     *     1,
     *     ' do ficheiro SCF.FIL'//']'
         CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
         CLOSE(LOGLUN)
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
       CALL CLOSEFIL(SFDB)
       LOGMSG='PMXTRACT .LOADSCFCONF['//' Ficheiro SCF.FIL fechado.'//']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
C=======================================================================
C      MOVE RECORD OVER TO OUTPUT RECORD
C=======================================================================
       CALL FASTMOV(SCFGNT, SCF_GNTTAB, MAXGAM*2)
       TYPE*, IAM(), ' Numero/Tipo de Jogo obtido'
       CALL LOGGER('PMXTRACT .LOADSCFCONF['//' Numero/Tipo de Jogo obtido.]',INFO,LOGLUN)

       CALL FASTMOV(SCFGFN, SCF_GFNAMES, MAXGAM*5)
       TYPE*, IAM(), ' Nome dos Ficheiros de Jogo obtidos'
       CALL LOGGER('PMXTRACT .LOADSCFCONF['//' Nome dos Ficheiros de Jogo obtidos.]',INFO,LOGLUN)
C
       RETURN
       END

C***********************************************************************
C SUBROUTINE: LOADS DATA FROM DAF FILE.
C
C INPUT:
C        CDC - INDEX TO DAF FILE RECORD
C        LOGLUN - LOGICAL UNIT OF LOG FILE
C
C OUTPUT:
C        NONE.
C
C GLOBAL VARIABLES LOADED:
C        HDGTAB - HIGH DRAW -> GAME TABLE
C***********************************************************************
C======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE LOADDAFHDR(CDC,LOGLUN)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:RECDAF.DEF'
       INCLUDE 'INCLIB:PMXTRACT.DEF'
C
C=======================================================================
C      INPUT
C=======================================================================
       INTEGER*4 CDC
       INTEGER*4 LOGLUN
C=======================================================================
C      LOCALS
C=======================================================================
       CHARACTER*132 LOGMSG
       INTEGER*4     FDB(7)
       INTEGER*4     I, ST
       INTEGER*4     DRWGAM(MAXGAM)
       INTEGER*4     DAFNAM(5)
       DATA DAFNAM/'GXTS','K:DA','F.FI','L   ','    '/
C=======================================================================
C      GLOBALS
C=======================================================================
C      HIGH DRAW DATA
       INTEGER*4 HDGTAB(MAXGAM)
       COMMON /HDRWDATA/ HDGTAB
C=======================================================================
       TYPE*, IAM(),'A obter os High Draw Numbers do ficheiro DAF ...'
       CALL LOGGER('PMXTRACT .LOADDAFHDR['//'A obter os High Draw Numbers do ficheiro DAF ...]',INFO,LOGLUN)
C
       CALL FASTSET(0, HDGTAB, MAXGAM)
       CALL FASTSET(0, DRWGAM, MAXGAM)
C=======================================================================
C      OPEN THE DAF.FIL AND READ THE CONFIGURATION
C=======================================================================
       CALL OPENW(3,DAFNAM,4,0,0,ST)
       IF(ST .NE. 0) THEN
         CALL IOFERR(DAFNAM,1,ST,0)
         TYPE *, IAM(), ' Nao foi possivel abrir o ficheiro DAF.FIL'
         WRITE(LOGMSG,'(A)')
     *     'PMXTRACT .LOADDAFHDR['//' Nao foi possivel abrir o ficheiro DAF.FIL'//']'
         CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
         CLOSE(LOGLUN)
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
C
       LOGMSG='PMXTRACT .LOADDAFHDR['//' Ficheiro DAF.FIL aberto com sucesso.'//']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
C
       CALL IOINIT(FDB,3,DAFSEC*256)
       CALL READW(FDB,CDC,DAFREC,ST)
       IF(ST .NE. 0) THEN
         CALL IOFERR(DAFNAM,2,ST,CDC)
         CALL CLOSEFIL(FDB)
         LOGMSG='PMXTRACT .LOADDAFHDR['//' Ficheiro DAF.FIL fechado.'//']'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
         WRITE(LOGMSG,'(A,I0,A)')
     *     'PMXTRACT .LOADDAFHDR['//' Nao foi possivel ler o registo#',
     *     CDC,
     *     ' do ficheiro DAF.FIL'//']'
         CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
         CLOSE(LOGLUN)
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
C
       WRITE(LOGMSG,'(A,I0,A)')
     *     'PMXTRACT .LOADDAFHDR['//' Registo#',
     *     CDC,
     *     ' lido com sucesso'//']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       CALL CLOSEFIL(FDB)
       LOGMSG='PMXTRACT .LOADDAFHDR['//' Ficheiro DAF.FIL fechado.'//']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
C=======================================================================
C      FINDS THE HIGH DRAW NUMBER FOR EACH GAME
C=======================================================================
       LOGMSG='PMXTRACT .LOADDAFHDR['//' A determinar o High Draw number dos jogos ...'//']'
       CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       DO I=1, MAXGAM
         DRWGAM(I)=DAFDRW(I)
         HDGTAB(I)=DAFHDR(I)
         IF(HDGTAB(I) .LT. DRWGAM(I)) HDGTAB(I)=DRWGAM(I)
       ENDDO
C=======================================================================
C      LOGGING
C=======================================================================
       DO I=1, MAXGAM
         IF(HDGTAB(I) .GT. 0) THEN
           WRITE(LOGMSG,'(A,A,I2,A,I0,A)') 'PMXTRACT .LOADDAFHDR',
     *                                 '[ High Draw Number do jogo ',
     *                                 I,
     *                                 ': ',
     *                                 HDGTAB(I),
     *                                 ']'
           CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
         ENDIF
       ENDDO
C=======================================================================
C      FORMAT STATEMENTS
C=======================================================================
900    FORMAT(1X,A18,' High Draw Number do jogo ', I2, ': ', I0)

       RETURN
       END

C***********************************************************************
C SUBROUTINE: UPDATES THE BEGINNING AND ENDING DRAW COUNTERS FOR THE
C             GIVEN GAME.
C
C INPUT:
C        LOGLUN - LOGICAL UNIT OF LOG FILE
C        GAMKEY - GAME NUMBER
C        BDRAWKEY - BEGINNING DRAW ID
C        EDRAWKEY - ENDING DRAW ID
C
C OUTPUT:
C        BDGTAB - BEGIN DRAW ID GAME TABLE
C        BDCGTAB - BEGIN DRAW COUNTER GAME TABLE
C        EDGTAB - END DRAW ID GAME TABLE
C        EDCGTAB - END DRAW COUNTER GAME TABLE
C
C***********************************************************************
C======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE DDRWBECCC(LOGLUN, GAMKEY, BDRAWKEY, EDRAWKEY, BDGTAB, BDCGTAB, EDGTAB, EDCGTAB)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:DLTREC.DEF'
       INCLUDE 'INCLIB:DSPREC.DEF'
       INCLUDE 'INCLIB:DTGREC.DEF'
       INCLUDE 'INCLIB:DKKREC.DEF'
       INCLUDE 'INCLIB:PMXTRACT.DEF'
C=======================================================================
C      INPUT
C=======================================================================
       INTEGER*4 GAMKEY
       INTEGER*4 LOGLUN
       INTEGER*4 BDRAWKEY
       INTEGER*4 EDRAWKEY
       INTEGER*4 BDGTAB(MAXGAM)
       INTEGER*4 EDGTAB(MAXGAM)
C=======================================================================
C      OUTPUT
C=======================================================================
       CHARACTER*8 BDCGTAB(MAXGAM)
       CHARACTER*8 EDCGTAB(MAXGAM)
C=======================================================================
C      LOCALS
C=======================================================================
       INTEGER*4 BFLAG
       INTEGER*4 EFLAG
       CHARACTER*132 LOGMSG
C
       BFLAG=.FALSE.
       EFLAG=.FALSE.
C
       IF(BDGTAB(GAMKEY) .NE. BDRAWKEY) THEN
         CALL LOADDRWCCC(GAMKEY, BDRAWKEY, LOGLUN, BDGTAB,
     *                   BDCGTAB)
         BFLAG=.TRUE.
       ENDIF
       IF(EDGTAB(GAMKEY) .NE. EDRAWKEY) THEN
         CALL LOADDRWCCC(GAMKEY, EDRAWKEY, LOGLUN, EDGTAB,
     *                    EDCGTAB)
         EFLAG=.TRUE.
       ENDIF

       IF((BFLAG .EQ. .TRUE.) .OR. (EFLAG .EQ. .TRUE.)) THEN
         WRITE(LOGMSG,'(A,A,I0,A,I0,A,A,A,I0,A,A,A)')
     *        'PMXTRACT .DDRWBECCC[',
     *       'Tipo de Jogo=',GAMKEY,
     *       ', DrawI=',BDRAWKEY,
     *       ', SorteioI=',TRIM(BDCGTAB(GAMKEY)),
     *       ', DrawF=',EDRAWKEY,
     *       ', SorteioF=',TRIM(EDCGTAB(GAMKEY)),
     *       ']'
        CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)

       ENDIF
C=======================================================================
C      FORMAT STATEMENTS
C=======================================================================
9021   FORMAT(I3.3,'/',I4.4)
9031   FORMAT(I2.2,'/',I4.4)
9041   FORMAT(1X,A18,I2,'   ',I4,'   ',A8,'   ',I0,'.',I0,'.',I0)
       RETURN
       END

C***********************************************************************
C SUBROUTINE: LOADS DRAW COUNTER FROM FILE GIVEN THE GAME AND THE DRAW
C             ID.
C
C INPUT:
C        GAMKEY - GAME NUMBER
C        DRWKEY - DRAW ID
C        LOGLUN - LOGICAL UNIT OF LOG FILE
C
C OUTPUT:
C        DGTAB  - DRAW ID GAME TABLE
C        DCGTAB - DRAW COUNTER GAME TABLE
C
C***********************************************************************
C======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE LOADDRWCCC(GAMKEY, DRWKEY, LOGLUN, DGTAB, DCGTAB)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:DLTREC.DEF'
       INCLUDE 'INCLIB:DSPREC.DEF'
       INCLUDE 'INCLIB:DTGREC.DEF'
       INCLUDE 'INCLIB:DKKREC.DEF'
       INCLUDE 'INCLIB:PMXTRACT.DEF'
C
C=======================================================================
C      INPUT
C=======================================================================
       INTEGER*4 GAMKEY
       INTEGER*4 DRWKEY
       INTEGER*4 LOGLUN
C=======================================================================
C      OUTPUT
C=======================================================================
       INTEGER*4   DGTAB(MAXGAM)
       CHARACTER*8 DCGTAB(MAXGAM)
C=======================================================================
C      LOCALS
C=======================================================================
       CHARACTER*132 LOGMSG
       INTEGER*4 ST
       INTEGER*4 GTYP
       INTEGER*4 ERRO
       INTEGER*4 XDRW
       INTEGER*4 XWEK
       INTEGER*4 XANO
       INTEGER*4 FFDB(7)
       INTEGER*4 FLUN
       INTEGER*2 IDAT(LDATE_LEN) /LDATE_LEN*0/
       INTEGER*4 K
       CHARACTER*20 CGFNAME/'                    '/
C=======================================================================
C      GLOBALS
C=======================================================================
C      HIGH DRAW DATA
       INTEGER*4 HDGTAB(MAXGAM) ! HIGH DRAW GAME TABLE
       COMMON /HDRWDATA/ HDGTAB
C      SYSTEM CONFIGURATION DATA
       INTEGER*4 SCF_GNTTAB(2,MAXGAM)  ! GAME NUMBER -> GAME TYPE TABLE
       INTEGER*4 SCF_GFNAMES(5,MAXGAM) ! GAME FILE NAMES TABLE
       COMMON /SCFDATA/ SCF_GNTTAB, SCF_GFNAMES
C
C=======================================================================
C      FIND A FREE LUN TO USE
C=======================================================================
       CALL FIND_AVAILABLE_LUN(FLUN,ST)
       IF(ST .NE. 0) THEN
         TYPE*, IAM(), 'Nao foi possivel abrir o ficheiro do jogo#',GAMKEY,': LUN nao disponivel'
         WRITE(LOGMSG,'(A,I0,A)') 'PMXTRACT .LOADDRWCCC[Nao foi possivel abrir o ficheiro do jogo#',
     *                            GAMKEY,
     *                            ': LUN nao disponivel'//']'
         CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
         CLOSE(LOGLUN)
         CALL GSTOP(GEXIT_FATAL)
       ENDIF
C
       GTYP = SCF_GNTTAB(GAMTYP,GAMKEY)
       IF(GTYP .NE. TPAS) THEN
         IF(HDGTAB(GAMKEY) .GT. 0) THEN
           CALL OPENW(FLUN,SCF_GFNAMES(1,GAMKEY),0,0,0,ERRO)
           WRITE(CGFNAME, '(5A4)') (SCF_GFNAMES(K,GAMKEY),K=1,5)
           IF(ERRO .NE. 0) THEN
             CALL IOFERR(SCF_GFNAMES(1,GAMKEY), 1, ERRO, 0)
             TYPE*,IAM(),' Erro#',ERRO,' a abrir o ficheiro do jogo#',GAMKEY
             WRITE(LOGMSG,'(A,I0,A,I0,A,A,A)')
     *                             'PMXTRACT .LOADDRWCCC['//'Erro#',
     *                             ERRO,
     *                             ' a abrir o ficheiro do jogo#',
     *                             GAMKEY,
     *                             ' com o nome ',
     *                             TRIM(CGFNAME),
     *                             ']'
             CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
             CLOSE(LOGLUN)
             CLOSE(FLUN)
             CALL GSTOP(GEXIT_FATAL)
           ELSE
             WRITE(LOGMSG,'(A,I0,A,A,A)') 'PMXTRACT .LOADDRWCCC[Ficheiro do jogo#',
     *                                GAMKEY,
     *                                ' com o nome ',
     *                                TRIM(CGFNAME),
     *                                ' aberto com sucesso.]'
             CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
           ENDIF
C
           IF(GTYP .EQ. TLTO) THEN
             CALL IOINIT(FFDB,FLUN,DLTSEC*256)
           ELSEIF(GTYP .EQ. TSPT) THEN
             CALL IOINIT(FFDB,FLUN,DSPSEC*256)
           ELSEIF(GTYP .EQ. TKIK) THEN
             CALL IOINIT(FFDB,FLUN,DKKSEC*256)
           ELSE
             TYPE*,IAM(),'Tipo de Jogo ',GTYP, ' nao suportado'
             WRITE(LOGMSG,'(A,I0,A)') 'PMXTRACT .LOADDRWCCC['//'Tipo de Jogo ',GTYP,' nao suportado.'//']'
             CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
             CLOSE(LOGLUN)
             CLOSE(FLUN)
             CALL GSTOP(GEXIT_FATAL)
           ENDIF
C
           XDRW=DRWKEY
           IF(XDRW .GT. 0 .AND. XDRW .LE. HDGTAB(GAMKEY)) THEN
             IF(GTYP.EQ.TLTO) THEN
               CALL READW(FFDB,XDRW,DLTREC,ERRO)
               IF(ERRO .NE. 0) THEN
               	 CALL IOFERR(SCF_GFNAMES(1,GAMKEY), 2, ERRO, XDRW)
                 TYPE*,IAM(),'Erro#',ERRO,' a ler o registo#',XDRW,' do ficheiro do jogo#',GAMKEY
                 WRITE(LOGMSG,'(A,I0,A,I0,A,I0,A)') 'PMXTRACT .LOADDRWCCC['//'Erro#',
     *                                              ERRO,
     *                                              ' a ler o registo#',
     *                                              XDRW,
     *                                              ' do ficheiro do jogo#',
     *                                              GAMKEY,
     *                                              ']'
                 CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
                 CLOSE(LOGLUN)
                 CLOSE(FLUN)
                 CALL GSTOP(GEXIT_FATAL)
               ENDIF
               IDAT(VCDC) = DLTDAT(1)
               CALL LCDATE(IDAT)
               IF(DLTCCC.GT.0) THEN
                 ! Totoloto 4 (game number = 7) e Totoloto Sabado (game number = 6)
                 XWEK = DLTCCC
                 XANO = IDAT(VYEAR2)
               ELSE
                 ! Totoloto Antigo (game num = 2)
                 CALL FIGWEK(DLTBSD,XWEK,XANO) ! V03 INSTEAD OF DLTDAT(1)
               ENDIF
C
               WRITE(DCGTAB(GAMKEY),9021) XWEK,XANO
               DGTAB(GAMKEY) = XDRW
             ELSEIF(GTYP.EQ.TSPT) THEN
               CALL READW(FFDB,XDRW,DSPREC,ERRO)
               IF(ERRO .NE. 0) THEN
               	 CALL IOFERR(SCF_GFNAMES(1,GAMKEY), 2, ERRO, XDRW)
                 TYPE*,IAM(),'Erro#',ERRO,' a ler o registo#',XDRW,' do ficheiro do jogo#',GAMKEY
                 WRITE(LOGMSG,'(A,I0,A,I0,A,I0,A)') 'PMXTRACT .LOADDRWCCC['//'Erro#',
     *                                              ERRO,
     *                                              ' a ler o registo#',
     *                                              XDRW,
     *                                              ' do ficheiro do jogo#',
     *                                              GAMKEY,
     *                                              ']'
                 CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
                 CLOSE(LOGLUN)
                 CLOSE(FLUN)
                 CALL GSTOP(GEXIT_FATAL)
               ENDIF
               IDAT(VCDC) = DSPDAT(1)
               CALL LCDATE(IDAT)
               CALL FIGWEK(DSPBSD,XWEK,XANO) ! V03 INSTEAD OF DSPDAT(1)
C
               WRITE(DCGTAB(GAMKEY),9031) XWEK,XANO
               DGTAB(GAMKEY) = XDRW
             ELSEIF(GTYP.EQ.TKIK) THEN ! KICKER
               CALL READW(FFDB,XDRW,DKKREC,ERRO)
               IF(ERRO .NE. 0) THEN
               	 CALL IOFERR(SCF_GFNAMES(1,GAMKEY), 2, ERRO, XDRW)
                 TYPE*,IAM(),'Erro#',ERRO,' a ler o registo#',XDRW,' do ficheiro do jogo#',GAMKEY
                 WRITE(LOGMSG,'(A,I0,A,I0,A,I0,A)') 'PMXTRACT .LOADDRWCCC['//'Erro#',
     *                                              ERRO,
     *                                              ' a ler o registo#',
     *                                              XDRW,
     *                                              ' do ficheiro do jogo#',
     *                                              GAMKEY,
     *                                              ']'
                 CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
                 CLOSE(LOGLUN)
                 CLOSE(FLUN)
                 CALL GSTOP(GEXIT_FATAL)
               ENDIF
               IDAT(VCDC) = DKKDAT(1)
               CALL LCDATE(IDAT)
               CALL FIGWEK(DKKESD,XWEK,XANO) ! V03 INSTEAD OF DKKDAT(1)
C
               WRITE(DCGTAB(GAMKEY),9031) XWEK,XANO
               DGTAB(GAMKEY) = XDRW
             ELSE
               TYPE*,IAM(),'Tipo de Jogo ',GTYP,' nao suportado'
               WRITE(LOGMSG,'(A,I0,A)') 'PMXTRACT .LOADDRWCCC['//'Tipo de Jogo ',GTYP,' nao suportado.'//']'
               CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
               CLOSE(LOGLUN)
               CLOSE(FLUN)
               CALL GSTOP(GEXIT_FATAL)
             ENDIF
           ELSE
             TYPE*,IAM(),'Draw#',XDRW, ' do jogo#',GAMKEY,' incorrecto'
             WRITE(LOGMSG,'(A,I0,A,I0,A,A)') 'PMXTRACT .LOADDRWCCC['//'Draw#',
     *                          XDRW,
     *                          ' do jogo#',
     *                          GAMKEY,
     *                          ' invalido',
     *                          ']'
             CALL LOGGER(TRIM(LOGMSG),FATAL,LOGLUN)
             CLOSE(LOGLUN)
             CLOSE(FLUN)
             CALL GSTOP(GEXIT_FATAL)
          ENDIF
         ENDIF
         CLOSE(FLUN)
         WRITE(LOGMSG,'(A,I0,A)') 'PMXTRACT .LOADDRWCCC[Ficheiro do jogo#',
     *                            GAMKEY,
     *                            ' fechado.]'
         CALL LOGGER(TRIM(LOGMSG),INFO,LOGLUN)
       ENDIF
C=======================================================================
C      FORMAT STATEMENTS
C=======================================================================
9021   FORMAT(I3.3,'/',I4.4)
9031   FORMAT(I2.2,'/',I4.4)
9041   FORMAT(1X,A18,I2,'   ',I4,'   ',A8,'   ',I0,'.',I0,'.',I0)
       RETURN
       END

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
       INCLUDE 'INCLIB:PMXTRACT.DEF'
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
       CHARACTER*132 LOGFILENAME ! LOG FILENAME
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
         LOGMSG='PMXTRACT .CREATELOG[Ficheiro de log criado com sucesso: '//TRIM(LOGFILENAME)//']'
         CALL LOGGER(TRIM(LOGMSG),INFO,LUN)
         LOGMSG='PMXTRACT .CREATELOG[Nivel de log das mensagens configurado: '//LTAGS(LOGLEVEL)//']'
         CALL LOGGER(TRIM(LOGMSG),INFO,LUN)
       ENDIF
C=======================================================================
C      FORMAT STATEMENTS
C=======================================================================
9000   FORMAT(A,I4.4,I2.2,I2.2,2A1,2A1,2A1,A)
       RETURN
       END

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
       INCLUDE 'INCLIb:PMXTRACT.DEF'
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
       END

C***********************************************************************
C SUBROUTINE: REPORT FILE I/O ERRORS TO OPERATOR
C
C INPUT:
C        NAME   - FILE NAME
C        FUN    - LOGICAL UNIT OF THE FILE
C        ST     - STATUS
C        RECORD - RECORD HANDLED
C
C OUTPUT:
C        NONE.
C
C***********************************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE IOFERR(NAME,FUN,ST,RECORD)
       IMPLICIT NONE
C      
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
C=======================================================================
C      INPUT
C=======================================================================
       INTEGER*4 NAME(5)
       INTEGER*4 RECORD
       INTEGER*4 ST
       INTEGER*4 FUN
C=======================================================================
C
       IF(FUN.EQ.0) WRITE(5,100) IAM(),NAME,ST
       IF(FUN.EQ.1) WRITE(5,110) IAM(),NAME,ST
       IF(FUN.EQ.2) WRITE(5,120) IAM(),NAME,ST,RECORD
       IF(FUN.EQ.3) WRITE(5,130) IAM(),NAME,ST,RECORD
       IF(FUN.EQ.4) WRITE(5,140) IAM(),NAME,ST
C
C=======================================================================
C      FORMAT STATEMENTS                                                
C=======================================================================
100    FORMAT(1X,A,1X,5A4,' erro de manipulacao > ',I9)
110    FORMAT(1X,A,1X,5A4,' erro de abertura    > ',I9)
120    FORMAT(1X,A,1X,5A4,' erro de leitura     > ',I9,' registo > ',I5)
130    FORMAT(1X,A,1X,5A4,' erro de escrita     > ',I9,' registo > ',I5)
140    FORMAT(1X,A,1X,5A4,' erro de fecho       > ',I9)
       END
