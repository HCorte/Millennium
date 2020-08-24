C
C UPDPAS8_ASF.FOR
C
C V01  13-SEP-2016 SCML  M16 PROJECT
C
C UTILITY PROGRAM TO SET PAS8 FIELD (PASSWORD MANAGER) OF EACH AGENT RECORD IN
C ASF FILE.
C
C TO BE RUN JUST ONCE BEFORE THE GO LIVE OF M16 PROJECT.
C
C PAS8 FIELD IS SET TO 1111.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2016 SCML Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C

C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM UPDPAS8_ASF
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
        INCLUDE '(LIB$ROUTINES)'
C
        INTEGER*4 I,J,K,L
        INTEGER*4 ST
        INTEGER*4 REPLUN
        CHARACTER CZERO*1/Z0/
C
        INTEGER*2 VDAT(LDATE_LEN)
        INTEGER*4 CTIM(2),CDAT(8),SYS,WEEK,YEAR
        CHARACTER SYSID(6)/'?','A','B','C','D','E'/
C
        CHARACTER*29 REPNAME                                                    !REPORT FILE NAME
        INTEGER*4  NEW_PAS8/1111/                                               !DEFAUL MANAGER PASSWORD
        INTEGER*4  OLD_PAS8                                                     !OLD PASSSWORD 8
        INTEGER*4  AGTTYP_TAB(NUMAGT)                                           !CONTAINS AGENT TYPE #
        INTEGER*4  AGTNUM_TAB(NUMAGT)                                           !CONTAINS AGENT #
        INTEGER*4  AGTPAS1_TAB(NUMAGT)                                          !PASSWORD 1
        INTEGER*4  TERNUM_TAB(NUMAGT)                                           !CONTAINS TERMINAL #
        INTEGER*4  TOTTER                                                       !TOTAL TERMINALS READ FROM ASF FILE (PROCESSED TERMINALS)
        INTEGER*4  TOTUPD                                                       !TOTAL TERMINALS READ TO UPDATE
        INTEGER*4  TOTOK                                                        !TOTAL TERMINALS REWRITTEN SUCCESSFULLY TO ASF
        INTEGER*4  TOTERR                                                       !TOTAL TERMINALS NOT REWRITTEN SUCCESSFULLY TO ASF
        INTEGER*4  ANSWER
        INTEGER*4  RUNMODE                                                      !PROGRAM RUN MODE
        INTEGER*4  REPMODE
        PARAMETER (REPMODE = 0)                                                 !REPORT MODE
        INTEGER*4  UPDMODE
        PARAMETER (UPDMODE = 1)                                                 !UPDATE MODE
        INTEGER*8  SPAWN_ST                                                     !SPAWN STATUS CODE
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        CALL CLRSCR(5)                                                          !CLEARS THE SCREEN
C
        TYPE*,IAM()
        TYPE*,IAM(),'Copyright 2016 SCML Corp. All rights reserved.'
        TYPE*,IAM()
        TYPE*,IAM(),'-------------------------------------------------------------'
        TYPE*,IAM(),'<  Atualizacao da Password do Gerente no ficheiro ASF.FIL   >'
        TYPE*,IAM(),'-------------------------------------------------------------'
        TYPE*,IAM()
        TYPE*,IAM(),'Ficheiro ASF a atualizar:'
        ST = LIB$SPAWN('$ DIR FILE:ASF.FIL;0 /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
        TYPE*,IAM()
        TYPE*,IAM(),'         Terminais a processar......: ', NUMAGT
        TYPE*,IAM()
C
        CALL FIND_AVAILABLE_LUN(REPLUN,ST)                                      !FINDS AN AVAILABLE LOGICAL UNIT FOR REPORT FILE
        IF (ST.NE.0) THEN
          TYPE*,IAM(),'ERROR GETTING LOGICAL UNIT TO OPEN FILE'
          TYPE*,IAM()
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
        VDAT(VCDC) = DAYCDC
        CALL LCDATE(VDAT)
        CALL FIGWEK(DAYCDC,WEEK,YEAR)
C
        SYS = P(SYSNAM)+1
        IF(SYS.LT.1.OR.SYS.GT.6) SYS = 1
C
        CALL ICLOCK(1,CTIM)
        CALL GDATE(CDAT(2),CDAT(3),CDAT(1))
        IF(CDAT(1).LT.77) THEN
          CDAT(1) = CDAT(1) + 2000
        ELSE
          CDAT(1) = CDAT(1) + 1900
        ENDIF
C
        WRITE(REPNAME,900) CDAT(1),CDAT(2),CDAT(3)                              !BUILD REPORT FILENAME
        OPEN(REPLUN,FILE=REPNAME,STATUS='NEW')                                  !OPEN REPORT FILE
        WRITE(REPLUN,901) CTIM,                                                 !REPORT TITLE
     *                    CDAT(3),CDAT(2),CDAT(1),(VDAT(K),K=9,13),VDAT(VCDC),
     *                    SYSID(SYS)
C
        RUNMODE = REPMODE                                                       !REPORT MODE BY DEFAULT
        CALL WIMG(5,'Executar programa em modo REPORT ? [Y/N] ')
        CALL YESNO(ANSWER)
        IF(ANSWER.NE.1) THEN
          CALL WIMG(5,'Executar programa em modo UPDATE ? [Y/N] ')
          CALL YESNO(ANSWER)
          IF(ANSWER.NE.1) THEN
            WRITE(REPLUN,9031)                                                  !RUN MODE NOT SELECTED
            WRITE(REPLUN,9022)                                                  !ASF SECURITY COPY NOT DONE
            WRITE(REPLUN,904) NUMAGT                                            !MAX NUMBER OF TERMINALS TO PROCESS
            WRITE(REPLUN,9041) NEW_PAS8                                         !NEW PASSWORD 8
            WRITE(REPLUN,911) 0                                                 !
            WRITE(REPLUN,912) 0                                                 !
            WRITE(REPLUN,905)                                                   !FILE HEADER  
            WRITE(REPLUN,910)                                                   !SEPARATOR LINE
            WRITE(REPLUN,913) 0
            WRITE(REPLUN,914) 0
            WRITE(REPLUN,915)                                                   !END OF REPORT
            CLOSE(REPLUN)                                                       !CLOSE REPORT FILE
            TYPE*,IAM()
            TYPE*,IAM(),'Ficheiro ASF NAO atualizado.'
            TYPE*,IAM()
            TYPE*,IAM(),'Relatorio gerado: ', REPNAME
            TYPE*,IAM()
            CALL GSTOP(GEXIT_OPABORT)
          ENDIF
          RUNMODE = UPDMODE                                                     !UPDATE MODE SELECTED
          WRITE(REPLUN,902)                                                     !UPDATE MODE SELECTED
          TYPE*,IAM()
          CALL WIMG(5,'Fazer copia de seguranca do ficheiro ASF ? [Y/N] ')
          CALL YESNO(ANSWER)
          IF(ANSWER.NE.1) THEN                                                  !ASF SECURITY COPY NOT SELECTED
            WRITE(REPLUN,9022)                                                  !ASF SECURITY COPY NOT DONE
            TYPE*,IAM()
            CALL WIMG(5,'Copia de seguranca NAO efetuada. Prosseguir ? [Y/N]')
            CALL YESNO(ANSWER)
            IF(ANSWER.NE.1) THEN
              WRITE(REPLUN,904) NUMAGT                                          !MAX NUMBER OF TERMINALS TO PROCESS
              WRITE(REPLUN,9041) NEW_PAS8                                       !NEW PASSWORD 8
              WRITE(REPLUN,911) 0                                               !
              WRITE(REPLUN,912) 0                                               !
              WRITE(REPLUN,905)                                                 !FILE HEADER
              WRITE(REPLUN,910)                                                 !SEPARATOR LINE
              WRITE(REPLUN,913) 0
              WRITE(REPLUN,914) 0
              WRITE(REPLUN,915)                                                 !END OF REPORT
              CLOSE(REPLUN)
              TYPE*,IAM()
              TYPE*,IAM(),'Relatorio gerado: ', REPNAME
              TYPE*,IAM()
              CALL GSTOP(GEXIT_OPABORT)
            ELSE
              WRITE(REPLUN,904) NUMAGT                                          !MAX NUMBER OF TERMINALS TO PROCESS
              WRITE(REPLUN,9041) NEW_PAS8                                       !NEW PASSWORD 8
            ENDIF
          ELSE
            TYPE*,IAM()
            TYPE*,IAM(),'A criar a copia de seguranca FILE:ASF_BEF_M16.OLD ...'
            TYPE*,IAM()
            ST = LIB$SPAWN('$ COPY FILE:ASF.FIL;0 FILE:ASF_BEF_M16.OLD;0 /LOG',,,,,,SPAWN_ST,,,,,,)
            IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
            TYPE*,IAM()
            IF(.NOT. (SPAWN_ST.AND.1)) THEN
              WRITE(REPLUN,9022)                                                !ASF SECURITY COPY NOT DONE
              WRITE(REPLUN,904) NUMAGT                                          !MAX NUMBER OF TERMINALS TO PROCESS
              WRITE(REPLUN,9041) NEW_PAS8                                       !NEW PASSWORD 8
              WRITE(REPLUN,911) 0                                               !
              WRITE(REPLUN,912) 0                                               !
              WRITE(REPLUN,905)                                                 !FILE HEADER
              WRITE(REPLUN,910)                                                 !SEPARATOR LINE
              WRITE(REPLUN,913) 0
              WRITE(REPLUN,914) 0
              WRITE(REPLUN,915)                                                 !END OF REPORT
              CLOSE(REPLUN)
              TYPE*,IAM(),'Relatorio gerado: ', REPNAME
              TYPE*,IAM()
              CALL GSTOP(GEXIT_FATAL)
            ENDIF
            WRITE(REPLUN,9021)                                                  !ASF SECURITY COPY DONE
            WRITE(REPLUN,904) NUMAGT                                            !MAX NUMBER OF TERMINALS TO PROCESS
            WRITE(REPLUN,9041) NEW_PAS8                                         !NEW PASSWORD 8
          ENDIF
        ELSE                                                                    !REPORT MODE SELECTED
          WRITE(REPLUN,903)                                                     !RUN MODE IS REPORT
          WRITE(REPLUN,9022)                                                    !ASF SECURITY COPY NOT DONE
          WRITE(REPLUN,904) NUMAGT                                              !MAX NUMBER OF TERMINALS TO PROCESS
          WRITE(REPLUN,9041) NEW_PAS8                                           !NEW PASSWORD 8
        ENDIF
C
        CALL OPENASF(ASF)                                                       !OPEN ASF FILE
C
        TYPE*,IAM()
        TYPE*,IAM(),'A obter a lista de terminais ...'
        TYPE*,IAM()
C
        TOTTER = 0
        TOTUPD = 0
        CALL FASTSET(0,AGTPAS1_TAB,NUMAGT)
        CALL FASTSET(0,AGTTYP_TAB,NUMAGT)
        CALL FASTSET(0,AGTNUM_TAB,NUMAGT)
        CALL FASTSET(0,TERNUM_TAB,NUMAGT)
        DO 101 I=1, NUMAGT
          CALL READASF(I,ASFREC,ST)
          IF(ST.NE.0) THEN
            TYPE*,IAM(),'FILE:ASF.FIL READ ERROR ',ST,' TERMINAL ',I
            TYPE*,IAM()
            TYPE*,IAM(),'NAO foi possivel obter a lista de terminais!'
            TYPE*,IAM()
            WRITE(REPLUN,911) TOTTER                                            !
            WRITE(REPLUN,912) TOTUPD                                            !
            WRITE(REPLUN,905)                                                   !FILE HEADER  
            WRITE(REPLUN,910)                                                   !SEPARATOR LINE
            WRITE(REPLUN,913) 0
            WRITE(REPLUN,914) 0
            WRITE(REPLUN,915)                                                   !END OF REPORT
            CLOSE(REPLUN)                                                       !CLOSE REPORT FILE
            TYPE*,IAM(),'Ficheiro ASF NAO atualizado.'
            TYPE*,IAM()
            TYPE*,IAM(),'Relatorio gerado: ', REPNAME
            TYPE*,IAM()
            CALL CLOSASF
            CALL GSTOP(GEXIT_FATAL)
          ENDIF
C
          TOTTER = TOTTER + 1
          DO J=SAGNO,EAGNO
            IF(ASFBYT(J).NE.' '.AND.ASFBYT(J).NE.CZERO) THEN
              L = 0
              DO K=SATYP,EATYP
                IF(ASFBYT(K).NE.' '.AND.ASFBYT(K).NE.CZERO) THEN
                  TOTUPD = TOTUPD + 1
                  TERNUM_TAB(TOTUPD) = I                                        !TERMINAL SLOT #
                  CALL ASCBIN(ASFINF,SAGNO,LAGNO,AGTNUM_TAB(TOTUPD),ST)
                  CALL ASCBIN(ASFINF,SPAS1,LPAS1,AGTPAS1_TAB(TOTUPD),ST)
                  CALL ASCBIN(ASFINF,SATYP+L,LATYP-L,AGTTYP_TAB(TOTUPD),ST)
                  GOTO 101
                ELSE
                  L = L + 1
                ENDIF
              ENDDO
            ENDIF
          ENDDO
101     CONTINUE
C
        TYPE*,IAM(),'         Terminais processados......: ', TOTTER
        TYPE*,IAM(),'         Terminais a atualizar......: ', TOTUPD
C
        IF(RUNMODE.EQ.UPDMODE) THEN                                             !UPDATE RUN MODE
          WRITE(REPLUN,911) TOTTER
          WRITE(REPLUN,912) TOTUPD
          WRITE(REPLUN,905)
          TYPE*,IAM()
          CALL WIMG(5,'Atualizar a lista de terminais   ? [Y/N] ')
          CALL YESNO(ANSWER)
          IF(ANSWER.NE.1) THEN
            WRITE(REPLUN,910)                                                   !SEPARATOR LINE
            WRITE(REPLUN,913) 0
            WRITE(REPLUN,914) TOTUPD
            WRITE(REPLUN,915)                                                   !END OF REPORT
            CLOSE(REPLUN)
            TYPE*,IAM()
            TYPE*,IAM(),'Ficheiro ASF NAO atualizado.'
            TYPE*,IAM()
            CALL CLOSASF
            CALL GSTOP(GEXIT_OPABORT)
          ENDIF
        ELSE                                                                    !REPORT RUN MODE
          WRITE(REPLUN,911) TOTTER
          WRITE(REPLUN,912) TOTUPD
          WRITE(REPLUN,9051)
        ENDIF
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       UPDATE MANAGER PASSWORD (PAS8)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        TYPE*,IAM()
        TYPE*,IAM(),'A iniciar a atualizacao da lista de terminais ...'
        TYPE*,IAM()
C
        TOTOK  = 0
        TOTERR = 0
        DO 301 I=1,TOTUPD
          CALL READASF(TERNUM_TAB(I),ASFREC,ST)
          IF(ST.NE.0) THEN
            TOTERR = TOTERR + 1                                                 !COUNTS UNSUCCESSFULLY UPDATES
            TYPE*,IAM(),'FILE:ASF.FIL READ ERROR ',ST,' TERMINAL ',TERNUM_TAB(I),' CONTINUING ...'
            WRITE(REPLUN,906) TERNUM_TAB(I),                                    !TERMINAL NUMBER
     *                        AGTNUM_TAB(I),                                    !AGENT NUMBER
     *                        AGTTYP_TAB(I),                                    !AGENT TYPE (ONLNE/OFFLINE)
     *                        AGTPAS1_TAB(I),                                   !PASSWORD
     *                        ST                                                !READ ERROR #
            GOTO 201
          ENDIF
C
C         NO ERROR GETTING ASF RECORD. PROCEED...
          CALL ASCBIN(ASFINF,SPAS8,LPAS8,OLD_PAS8,ST)                           !GET PASSWORD 8 BEFORE UPDATE
          IF(ST.NE.0) THEN
            TYPE*,IAM(), 'ERRO A OBTER PAS8 ANTES DO UPDATE ',ST,' TERMINAL ', TERNUM_TAB(I)
          ENDIF
C
          L = 0
          DO K=SATYP,EATYP
            IF(ASFBYT(K).NE.' '.AND.ASFBYT(K).NE.CZERO) THEN
              EXIT
            ELSE
              L = L + 1
            ENDIF
          ENDDO
C
          CALL BINASC(ASFINF,SPAS8,LPAS8,NEW_PAS8)                              !SET NEW PASSWORD 8 (MANAGER PASSWORD)
          IF(RUNMODE.EQ.UPDMODE) THEN                                           !UPDATE RUN MODE
            CALL WRITASF(TERNUM_TAB(I),ASFREC,ST)                               !WRITE CHANGES TO ASF
            IF(ST.EQ.0) THEN                                                    !NO ERROR UPDATING ASF RECORD
              TOTOK = TOTOK + 1                                                 !COUNTS SUCCESSFULLY UPDATES
              WRITE(REPLUN,907) TERNUM_TAB(I),                                  !TERMINAL #
     *                          (ASFBYT(K),K=SAGNO,EAGNO),                      !AGENT #
     *                          (ASFBYT(K),K=SATYP+L,EATYP),                    !AGENT TYPE (ONLNE/OFFLINE)
     *                          (ASFBYT(K),K=SPAS1,EPAS1),                      !PASSWORD 1
     *                          OLD_PAS8,                                       !OLD PASSWORD 8 (BEFORE UPDATE)
     *                          (ASFBYT(K),K=SPAS8,EPAS8)                       !NEW PASSWORD 8 (AFTER UPDATE)
            ELSE                                                                !ERROR UPDATING ASF RECORD
              TYPE*,IAM(),'FILE:ASF.FIL WRITE ERROR ',ST,' TERMINAL ',TERNUM_TAB(I)
              TOTERR = TOTERR + 1                                               !COUNTS UNSUCCESSFULLY UPDATES
              WRITE(REPLUN,908) TERNUM_TAB(I),                                  !TERMINAL #
     *                          (ASFBYT(K),K=SAGNO,EAGNO),                      !AGENT #
     *                          (ASFBYT(K),K=SATYP+L,EATYP),                    !AGENT TYPE (ONLNE/OFFLINE)
     *                          (ASFBYT(K),K=SPAS1,EPAS1),                      !PASSWORD 1
     *                          OLD_PAS8,                                       !OLD PASSWORD 8 (AFTER UPDATE)
     *                          ST,                                             !WRITE ERROR #
     *                          OLD_PAS8                                        !PASSWORD NOT UPDATED
            ENDIF
            GOTO 201
          ELSE                                                                  !REPORT RUN MODE
            TOTERR = TOTERR + 1                                                 !COUNTS SUCCESSFULLY UPDATES
            WRITE(REPLUN,909) TERNUM_TAB(I),                                    !TERMINAL #
     *                        (ASFBYT(K),K=SAGNO,EAGNO),                        !AGENT #
     *                        (ASFBYT(K),K=SATYP+L,EATYP),                      !AGENT TYPE (ONLNE/OFFLINE)
     *                        (ASFBYT(K),K=SPAS1,EPAS1),                        !PASSWORD 1
     *                        OLD_PAS8,                                         !OLD PASSWORD 8 (BEFORE UPDATE)
     *                        (ASFBYT(K),K=SPAS8,EPAS8)                         !NEW PASSWORD 8 (AFTER UPDATE)
            GOTO 201
          ENDIF
201     CONTINUE
        IF(MOD(I,1000).EQ.0) THEN
          TYPE*,IAM(),' ',I, ' terminais processados ...'
        ENDIF
301     CONTINUE
C
        IF(MOD(I-1,1000).NE.0) THEN
          TYPE*,IAM(),' ',I-1, ' terminais processados ...'
        ENDIF
C
        TYPE*,IAM()
        TYPE*,IAM(),'         Terminais atualizados......: ', TOTOK
        TYPE*,IAM(),'         Terminais nao atualizados..: ', TOTERR
        TYPE*,IAM()
C
        TYPE*,IAM(),'Relatorio gerado: ', REPNAME
        TYPE*,IAM()
C
        WRITE(REPLUN,910)                                                       !SEPARATOR LINE
        WRITE(REPLUN,913) TOTOK
        WRITE(REPLUN,914) TOTERR
        WRITE(REPLUN,915)                                                       !END OF REPORT
        CLOSE(REPLUN)
        CALL CLOSASF
C
        IF(RUNMODE.EQ.UPDMODE) THEN
          IF(TOTOK.LT.TOTUPD) THEN
            TYPE*,IAM(),'Ficheiro ASF PARCIALMENTE atualizado:'
          ELSE
            TYPE*,IAM(),'Ficheiro ASF atualizado:'
          ENDIF
        ELSE
          TYPE*,IAM(),'Ficheiro ASF NAO atualizado:'
        ENDIF
        ST = LIB$SPAWN('$ DIR FILE:ASF.FIL;0 /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
        TYPE*,IAM()
C
        CALL GSTOP(GEXIT_SUCCESS)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       FORMAT STATEMENTS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
900     FORMAT('FILE:UPDPAS8_ASF_',I4.4,I2.2,I2.2,'.REP')
C
901     FORMAT(58X,
     *         'SCML - DEP JOGOS',
     *         39X,
     *         2A4,
     *         /,
     *         2X,I2.2,'.',I2.2,'.',I4.4,18X,
     *         'ATUALIZACAO DA PASSWORD DE GERENTE DE TODOS OS TERMINAIS NO FICHEIRO ASF',
     *         11X,
     *         5A2,1X,'CDC',1X,I4
     *         /,
     *         124(' '),'SYS',A5
     *         /,
     *         132('='),
     *         /
     *        )
C
902     FORMAT(1X,'MODO DE EXECUCAO..................: UPDATE')
C
9021    FORMAT(1X,'COPIA DE SEGURANCA DO FICHEIRO ASF: EFETUADA')
9022    FORMAT(1X,'COPIA DE SEGURANCA DO FICHEIRO ASF: NAO EFETUADA')
C
903     FORMAT(1X,'MODO DE EXECUCAO..................: REPORT')
C
9031    FORMAT(1X,'MODO DE EXECUCAO..................: NAO SELECIONADO')
C
904     FORMAT(1X,'TOTAL DE TERMINAIS A PROCESSAR....: ',I0)
C
9041    FORMAT(1X,'PASSWORD DE GERENTE A CONFIGURAR..: ',I5.5,/)
C
905     FORMAT(132('-'),/,
     *         '   NUMERO    NUMERO      TIPO   PAS1  |  READ      PAS8  |  WRITE    PAS8   |         PAS8',/,
     *         ' TERMINAL  MEDIADOR  MEDIADOR         |  ERROR #  ANTES  |  ERROR #  ATUAL  |  ATUALIZADA?',/,
     *         132('-'))
C
9051    FORMAT(132('-'),/,
     *         '   NUMERO    NUMERO      TIPO   PAS1  |  READ      PAS8  |  WRITE     PAS8  |         PAS8',/,
     *         ' TERMINAL  MEDIADOR  MEDIADOR         |  ERROR #  ANTES  |  ERROR #   ALVO  |  ATUALIZADA?',/,
     *         132('-'))
906     FORMAT(4X,I5,                                                           !TERMINAL NUMBER
     *         3X,I7.7,                                                         !AGENT NUMBER
     *         8X,I2,                                                           !AGENT TYPE (ONLINE/OFFLINE)
     *         2X,I5.5,                                                         !PASSWORD 1
     *         2X,'|',
     *         2X,I7,                                                           !READ ERROR
     *         3X,'????',                                                       !OLD PASSWORD 8 COULD NOT BE READ
     *         2X,'|',
     *         2X,'     NA',                                                    !WRITE ERROR NOT APPLICABLE
     *         3X,'????',                                                       !CURRENT PASSWORD 8 NOT KNOWN
     *         2X,'|'                                                           !
     *         2X,'         NO'                                                 !PASSWORD 8 NOT UPDATED
     *        )
C
907     FORMAT(4X,I5,                                                           !TERMINAL NUMBER
     *         3X,<LAGNO>A1,                                                    !AGENT NUMBER
     *         <8+L>X,<LATYP-L>A1,                                              !AGENT TYPE
     *         2X,<LPAS1>A1,                                                    !PASSWORD 1
     *         2X,'|',
     *         2X,'     NO',                                                    !NO READ ERROR
     *         2X,I5.5,                                                         !OLD PASSWORD 8
     *         2X,'|',
     *         2X,'     NO',                                                    !NO WRITE ERROR
     *         2X,<LPAS8>A1,                                                    !NEW PASSWORD 8
     *         2X,'|',
     *         2X,'        YES'                                                 !PASSWORD 8 UPDATED
     *        )
C
908     FORMAT(4X,I5,                                                           !TERMINAL NUMBER
     *         3X,<LAGNO>A1,                                                    !AGENT NUMBER
     *         <8+L>X,<LATYP-L>A1,                                              !AGENT TYPE
     *         2X,<LPAS1>A1,                                                    !PASSWORD 1
     *         2X,'|',
     *         2X,'     NO',                                                    !NO READ ERROR
     *         2X,I5.5,                                                         !OLD PASSWORD 8
     *         2X,'|',
     *         2X,I7,                                                           !WRITE ERROR OCCURRED
     *         2X,I5.5,                                                         !OLD PASSWORD 8
     *         2X,'|',
     *         2X,'         NO'                                                 !PASSWORD 8 NOT UPDATED
     *        )
C
909     FORMAT(4X,I5,                                                           !TERMINAL NUMBER
     *         3X,<LAGNO>A1,                                                    !AGENT NUMBER
     *         <8+L>X,<LATYP-L>A1,                                              !AGENT TYPE
     *         2X,<LPAS1>A1,                                                    !PASSWORD 1
     *         2X,'|',
     *         2X,'     NO',                                                    !NO READ ERROR
     *         2X,I5.5,                                                         !OLD PASSWORD 8
     *         2X,'|',
     *         2X,'     NA',                                                    !NO WRITE ERROR
     *         2X,<LPAS8>A1,                                                    !NEW PASSWORD 8
     *         2X,'|',
     *         2X,'         NO'                                                 !PASSWORD 8 UPDATED
     *        )
C
910     FORMAT(132('-'),/)
C
911     FORMAT(1X,'TOTAL DE TERMINAIS PROCESSADOS....:',I6)
C
912     FORMAT(1X,'TOTAL DE TERMINAIS A ATUALIZAR....:',I6,/)
C
913     FORMAT(1X,'TOTAL DE TERMINAIS ATUALIZADOS....:',I6)
C
914     FORMAT(1X,'TOTAL DE TERMINAIS NAO ATUALIZADOS:',I6)
C
915     FORMAT(//,24X,'*****  FIM DO RELATORIO  *****')
C
        END
