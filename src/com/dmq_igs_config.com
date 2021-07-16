$ ON CONTROL_Y THEN CALL REPOR_FICH
$ ON ERROR THEN CALL REPOR_FICH 
$ ON SEVEREERROR THEN CALL REPOR_FICH
$ !---------------------------------------------------------
$ !-----------VARIAVEIS AMBIENTE----------------------------
$ DMQ_DIR:=="DKD0:[PORT.TSK.IGS]"
$ VALIDA_MAQ:==0
$ VALIDA_CONF:==0
$ VALIDA_DATA:==0
$ esc[0,8] = %x1b
$ bold 	   = "''esc'[1m"
$ blink    = "''esc'[5m"
$ normal   = "''esc'[0m"
$ Dia_df1 = f$cvtime("today",,"date")
$ Dia1 = "''f$elem(2,"-",Dia_df1)'"
$ Mes1 = "''f$elem(1,"-",Dia_df1)'"
$ Ano1 = "''f$elem(0,"-",Dia_df1)'"
$ Ano_Mes_dia1 = f$fao("!#ZL",8,'Ano1''Mes1''Dia1')
$ SAY :== "WRITE SYS$OUTPUT"
$ !------------------------------------------------------------------------------------------------
$ !--------------------------------VERIFICA SE DIRECTORIA IGS EXISTE-------------------------------
$ !------------------------------------------------------------------------------------------------
$ BEGIN:
$ IF F$PARSE("''DMQ_DIR'") .EQS. ""
$ THEN
$  SAY ""
$  SAY "''bold'Directorio ''DMQ_DIR' nao existe. Devera ser criado para prosseguir.''normal'"
$  SAY "''bold'A sair...''normal'"
$  WAIT 00:00:02
$  EXIT
$ ENDIF
$ !------------------------------------------------------------------------------------------------
$ !-------------------PERGUNTA QUAL A DATA PARA A COPIA DE SEGURANCA-------------------------------
$ !------------------------------------------------------------------------------------------------
$ SET DEF GXTSK
$ INQUIRE_DATA:
$ INQUIRE/nopun data1 "''bold'QUAL A DATA DE HOJE,(aaaammdd)? [''Ano_Mes_dia1']''normal'"
$ IF data1 .EQS. "" THEN data1 = "''Ano_Mes_dia1'"
$ CALL VALIDA_DATA
$  IF VALIDA_DATA .NE. 0
$   THEN SAY "''bold'Data Incorreta.''normal'"
$    GOTO INQUIRE_DATA
$     ENDIF
$ SAY ""
$ !------------------------------------------------------------------------------------------------
$ !----------------------------COPIA SEGURANCA-----------------------------------------------------
$ !------------------------------------------------------------------------------------------------
$ SET DEF 'DMQ_DIR'
$ SAY ""
$ SAY "A CRIAR COPIA DE SEGURANCA"
$ WAIT 00:00:01
$ SAY ""
$ IF F$SEARCH("DMQ.INI") .NES. ""
$ THEN
$  COPY/LOG DMQ.INI DMQ_'data1'.INI
$ ELSE
$  SAY "''bold'Nao e possivel fazer copia de seguranca: ficheiro DMQ.INI nao existe.''normal'"
$  SAY "''bold'A continuar...''normal'"
$  WAIT 00:00:02
$ ENDIF
$ SAY ""
$ SET DEF GXTSK
$ !------------------------------------------------------------------------------------------------
$ !------------------PERGUNTA QUAL A MAQUINA QUE SERA PRIMARIO-------------------------------------
$ !------------------------------------------------------------------------------------------------
$ BEGIN:
$ INQUIRE_MAQ:
$  INQUIRE MAQ "QUAL A MAQUINA DO IGS QUE FICARA DEFINIDA COMO PRIMARIO? [1/2]"
$  SAY ""
$   CALL VALIDA_MAQ 'MAQ'
$   IF VALIDA_MAQ .NE. 0 
$   THEN
$      SAY "''bold'Maquina Incorreta.''normal'" 
$      GOTO INQUIRE_MAQ
$   ENDIF
$  IF MAQ .EQ. "1"
$      THEN
$        GOTO MAQ1
$  ENDIF 
$   IF MAQ .EQ. "2"
$      THEN
$        GOTO MAQ2 
$   ENDIF
$ !------------------------------------------------------------------------------------------------
$ !----------------------CRIA FICHEIRO DE CONFIGURACAO MAQUINA 1 PRIMARIO--------------------------
$ !------------------------------------------------------------------------------------------------
$ MAQ1:
$ OPEN/WRITE MAQ1 'DMQ_DIR'DMQ.INI
$ WRITE MAQ1 "[Default Server]"
$ WRITE MAQ1 "TransportType=TCP/IP"
$ WRITE MAQ1 "Hostname=PLUTAO107" 
$ WRITE MAQ1 "Endpoint=5000"
$ WRITE MAQ1 "ReconnectMsgInterval=0"
$ WRITE MAQ1 "ReconnectTimerInterval=0"
$ WRITE MAQ1 ""
$ WRITE MAQ1 "[Failover]"
$ WRITE MAQ1 "EnableAutomaticFailover=1"
$ WRITE MAQ1 "TransportType=TCP/IP"
$ WRITE MAQ1 "Hostname=PLUTAO106"
$ WRITE MAQ1 "Endpoint=5000"
$ WRITE MAQ1 ""
$ WRITE MAQ1 "[Logging]"
$ WRITE MAQ1 "ErrorEvents=1"
$ WRITE MAQ1 "SentMessages=0"
$ WRITE MAQ1 "ReceivedMessages=0"
$ WRITE MAQ1 ""
$ WRITE MAQ1 "[MRS]"
$ WRITE MAQ1 "Enabled=0"
$ WRITE MAQ1 ""
$ WRITE MAQ1 "JournalPath=[]"
$ WRITE MAQ1 "JournalFileSize=49150"
$ WRITE MAQ1 "JournalCycle=0"
$ WRITE MAQ1 "JournalSizeFixed=0"
$ WRITE MAQ1 "PreAllocate=1"
$ WRITE MAQ1 "JournalBlockSize=0"
$ WRITE MAQ1 ""
$ WRITE MAQ1 "[Trace]"
$ WRITE MAQ1 "PamsTrace=0"
$ WRITE MAQ1 "DmqclTrace=0"
$ CLOSE MAQ1
$  SAY ""
$ SAY "''blink'A ALTERAR A CONFIGURACAO...AGUARDE''normal'"
$ WAIT 00:00:02
$  SAY ""
$ SAY "A LIGACAO SERA ESTABELECIDA PELA SEGUINTE ORDEM:"
$  SAY ""
$ SEARCH 'DMQ_DIR'DMQ.INI HOSTNAME
$  SAY ""
$ INQUIRE CONF_FICH "A CONFIGURACAO ESTA CORRETA? [Y/N]
$   IF CONF_FICH .EQS. "Y" .OR. CONF_FICH .EQS. "y" THEN GOTO PURGE_FILES
$   IF CONF_FICH .EQS. "N" .OR. CONF_FICH .EQS. "n" THEN GOTO INQUIRE_MAQ
$   IF CONF_FICH .EQS. "" THEN
$   SAY ""
$   SAY "''bold'RESPOSTA NAO VALIDA!''normal'"
$    GOTO INQUIRE_MAQ
$ !------------------------------------------------------------------------------------------------
$ !----------------CRIA FICHEIRO DE CONFIGURACAO MAQUINA 2 PRIMARIO--------------------------------
$ !------------------------------------------------------------------------------------------------
$ MAQ2:
$ OPEN/WRITE MAQ2 'DMQ_DIR'DMQ.INI
$ WRITE MAQ2 "[Default Server]"
$ WRITE MAQ2 "TransportType=TCP/IP"
$ WRITE MAQ2 "Hostname=PLUTAO106" 
$ WRITE MAQ2 "Endpoint=5000"
$ WRITE MAQ2 "ReconnectMsgInterval=0"
$ WRITE MAQ2 "ReconnectTimerInterval=0"
$ WRITE MAQ2 ""
$ WRITE MAQ2 "[Failover]"
$ WRITE MAQ2 "EnableAutomaticFailover=1"
$ WRITE MAQ2 "TransportType=TCP/IP"
$ WRITE MAQ2 "Hostname=PLUTAO107"
$ WRITE MAQ2 "Endpoint=5000"
$ WRITE MAQ2 ""
$ WRITE MAQ2 "[Logging]"
$ WRITE MAQ2 "ErrorEvents=1"
$ WRITE MAQ2 "SentMessages=0"
$ WRITE MAQ2 "ReceivedMessages=0"
$ WRITE MAQ2 ""
$ WRITE MAQ2 "[MRS]"
$ WRITE MAQ2 "Enabled=0"
$ WRITE MAQ2 ""
$ WRITE MAQ2 "JournalPath=[]"
$ WRITE MAQ2 "JournalFileSize=49150"
$ WRITE MAQ2 "JournalCycle=0"
$ WRITE MAQ2 "JournalSizeFixed=0"
$ WRITE MAQ2 "PreAllocate=1"
$ WRITE MAQ2 "JournalBlockSize=0"
$ WRITE MAQ2 ""
$ WRITE MAQ2 "[Trace]"
$ WRITE MAQ2 "PamsTrace=0"
$ WRITE MAQ2 "DmqclTrace=0"
$ CLOSE MAQ2
$ SAY "''blink'A ALTERAR A CONFIGURACAO...AGUARDE''normal'"
$  SAY ""
$ SAY "A LER FICHEIRO AGUARDE"
$ WAIT 00:00:02
$  SAY ""
$ SAY "A LIGACAO SERA ESTABELECIDA PELA SEGUINTE ORDEM:"
$  SAY ""
$ SEARCH 'DMQ_DIR'DMQ.INI HOSTNAME
$  SAY ""
$ INQUIRE CONF_FICH "A CONFIGURACAO ESTA CORRETA? [Y/N]
$   IF CONF_FICH .EQS. "Y" .OR. CONF_FICH .EQS. "y" THEN GOTO PURGE_FILES
$   IF CONF_FICH .EQS. "N" .OR. CONF_FICH .EQS. "n" THEN GOTO INQUIRE_MAQ
$   IF CONF_FICH .EQS. "" THEN
$   SAY ""
$   SAY "''bold'RESPOSTA NAO VALIDA!''normal'"
$   GOTO INQUIRE_MAQ 
$ !------------------------------------------------------------------------------------------------
$ !---------------------EFECTUA O PURGE E DEIXA AS 2 ULTIMAS VERSOES-------------------------------
$ !------------------------------------------------------------------------------------------------
$ PURGE_FILES:
$ !------------------------------------------------------------------------------------------------
$ PURGE/KEEP=2 DKD0:[PORT.TSK.IGS]DMQ.INI
$ !------------------------------------------------------------------------------------------------
$ SAY ""
$ WAIT 00:00:02
$ SAY "''bold'AS CONFIGURACOES FORAM ALTERADAS COM SUCESSO''normal'"
$ WAIT 00:00:01
$ SAY ""
$ SAY ""
$ !------------------------------------------------------------------------------------------------
$ VALIDA_DATA:SUBROUTINE
$ !------------------------------------------------------------------------------------------------
$ VALIDA_DATA:==1
$ IF F$LENGHT(DATA1) .EQ. 8
$ THEN VALIDA_DATA:==0
$ ENDIF
$ ENDSUBROUTINE 
$ !------------------------------------------------------------------------------------------------
$ VALIDA_MAQ:SUBROUTINE
$ !------------------------------------------------------------------------------------------------
$ VALIDA_MAQ:==1
$ IF MAQ .EQS. "1" THEN VALIDA_MAQ:==0
$ IF MAQ .EQS. "2" THEN VALIDA_MAQ:==0
$ ENDSUBROUTINE
$ !------------------------------------------------------------------------------------------------
$ !---------------------REPOE FICHEIRO ANTERIOR COM BASE NA COPIA DE SEGURANCA---------------------
$ !------------------------------------------------------------------------------------------------
$ REPOR_FICH:SUBROUTINE
$ !------------------------------------------------------------------------------------------------
$ SET DEF 'DMQ_DIR'
$ REN/LOG DMQ_'DATA1'.INI DMQ.INI
$ SET DEF GXTSK
$ STOP
$ ENDSUBROUTINE
$ END:
