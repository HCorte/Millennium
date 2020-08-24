C
C      TAXCONFIG.FOR
C
C      V01 22-NOV-2012 SCML INITIAL RELEASE
C
C      PROGRAM TAXCONFIG (SMG APPLICATION)
C
C      THIS PROGRAM DEFINES THE CONFIGURATION VALUES OF THE TAX 
C      APPLICABLE TO HIGH PRIZES OF AM (MUTUAL BETS) 
C      AND LN (NACIONAL LOTTERY) GAMES.
C
C         MUTUAL PRIZES:
C
C             1. TAX VALUE FOR AM PRIZES(%)
C             2. MINIMUM PRIZE AMOUNT TO WHICH THE AM TAX APPLIES (EUR)
C                (MAX VALUE IS 9.999.999,99 EUR)
C             3. UNAMORTIZED PRIZE AMOUNT IN AM TAX (EUR)
C                (MAX VALUE IS 9.999.999,99 EUR)
C
C         LOTTERY PRIZES:
C
C             1. TAX VALUE FOR LN (%)
C             2. MINIMUM AMOUNT TO WHICH THE LN TAX APPLIES (EUR)
C                (MAX VALUE IS 9.999.999,99 EUR)
C             3. UNAMORTIZED AMOUNT IN LN TAX (EUR)
C                (MAX VALUE IS 9.999.999,99 EUR)
C
C+-------------------------------------------------------------------- SCML ----+
C¦         Menu de Configuração de Imposto dos Altos Premiados     v1.0         ¦
C+------------------------------------------------------------------------------+
C+------------------------------------------------------------------------ VER -+
C¦                                                                              ¦
C¦                    Dados de Configuração de Imposto                          ¦
C¦                                                                              ¦
C¦              Taxa de Imposto Prémio AM       :       20,00 %                 ¦
C¦                                                                              ¦
C¦              Menor Prémio Aplicável AM       :     4999,99 EUR               ¦
C¦                                                                              ¦
C¦              Valor a Amortizar no Imposto AM :     4999,99 EUR               ¦
C¦                                                                              ¦
C¦              Taxa de Imposto Prémio LN       :       20,00 %                 ¦
C¦                                                                              ¦
C¦              Menor Prémio Aplicável LN       :     4999,99 EUR               ¦
C¦                                                                              ¦
C¦              Valor a Amortizar no Imposto LN :     4999,99 EUR               ¦
C¦                                                                              ¦
C¦                                                                              ¦
C+------------------------------------------------------------------------------+
C+-------------------------------------------------------------------- Mensagem +
C¦Para voltar ao menu anterior prima qualquer tecla...                          ¦
C+------------------------------------------------------------------------------+
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      THIS ITEM IS THE PROPERTY OF SCML.
C
C      COPYRIGHT 2012 SCML. ALL RIGHTS RESERVED.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM TAXCONFIG
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:TAXCONFIG.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
C
C LOCAL VARIABLES
C
        INTEGER*4 ST
        COMMON TXCF_REC

        INTEGER*4 TAMMENU
        PARAMETER (TAMMENU = 4)
        CHARACTER*78  cMSG
        CHARACTER*5   VERSAO          /'v1.0 '/
        CHARACTER*28  MYMENU(TAMMENU) /'[      VER       ]',
     *                                 '[    ALTERAR     ]',
     *                                 '[ CRIAR FICHEIRO ]',
     *                                 '[      SAIR      ]'/
        INTEGER PB,           ! PASTEBOARD
     *        KEYB,           ! VIRTUAL KEYBOARD
     *        DISP1,          ! header frame window
     *        DISP2,          ! output data frame window
     *        DISP3,          ! message frame window
     *        DISPMENU        ! INITIAL MENU

        INTEGER   DEFAULT/1/
        INTEGER*4 MENUOPT
        TXCF_FRE(1) = 0
C
C CREATES SCREEN HEADER - DISP1
C
        ST = SMG$CREATE_VIRTUAL_DISPLAY( 1, 78, DISP1, SMG$M_BORDER)
        ST = SMG$LABEL_BORDER (DISP1,' SCML ',
     *                       SMG$K_TOP, 70,
     *                       SMG$M_BLINK, SMG$M_BOLD)
C
C CREATES INPUT DISPLAY - DISP2
C
        ST = SMG$CREATE_VIRTUAL_DISPLAY ( 16, 78, DISP2, SMG$M_BORDER)
        ST = SMG$LABEL_BORDER (DISP2,' Configuração ',
     *                       SMG$K_TOP, 65, SMG$M_REVERSE)
C
C CREATES MESSAGE AND ERROR DISPLAY - DISP3
C
        ST = SMG$CREATE_VIRTUAL_DISPLAY ( 1, 78, DISP3, SMG$M_BORDER)
        ST = SMG$LABEL_BORDER (DISP3,' Mensagem ',
     *                       SMG$K_TOP, 70, SMG$M_REVERSE)
C
C CREATES VIRTUAL  KEYBOARD - KEYB
C (inhibit numeric keypad mode if debug)
C
        ST = SMG$CREATE_VIRTUAL_KEYBOARD (KEYB)
C
C CREATES PASTEBOARD - PB
C
        ST = SMG$CREATE_PASTEBOARD (PB)
        IF (.NOT. ST) CALL LIB$STOP (%VAL(ST))
C
C CREATES MENU
C
        ST = SMG$CREATE_VIRTUAL_DISPLAY(11, 40, DISPMENU)
        ST = SMG$CREATE_MENU(DISPMENU, MYMENU, SMG$K_BLOCK ,
     *       SMG$M_DOUBLE_SPACE + SMG$M_WRAP_MENU , 2, 0)
C
C PASTING  VIRTUAL  DISPLAYS  ON  TERMINAL  SCREEN
C
        ST = SMG$PASTE_VIRTUAL_DISPLAY (DISP1, PB,  2, 2)
        ST = SMG$PASTE_VIRTUAL_DISPLAY (DISP2, PB,  5, 2)
        ST = SMG$PASTE_VIRTUAL_DISPLAY (DISP3, PB, 23, 2)

        ST = SMG$SET_CURSOR_MODE (PB, SMG$M_CURSOR_OFF )
        ST = SMG$ERASE_DISPLAY (DISP3)

        MENUOPT = 0
        DO WHILE(MENUOPT .NE. TAMMENU)
          ST = SMG$SET_CURSOR_MODE (DISP2, SMG$M_CURSOR_OFF )
          ST = SMG$LABEL_BORDER (DISP2,' Configuração ',
     *                       SMG$K_TOP, 65, SMG$M_REVERSE)

          cMSG = 'Menu de Configuração de Imposto dos Altos Premiados'
          ST   = SMG$PUT_CHARS (DISP1, cMSG, 1, 10)
          ST   = SMG$PUT_CHARS (DISP1, VERSAO, 1, 66)

          cMSG = ' Use setas para movimentar cursor e <ENTER> para escolher '
          ST = SMG$PUT_CHARS (DISP3, cMSG, 1,1)
          ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)

          ST = SMG$PASTE_VIRTUAL_DISPLAY(DISPMENU, PB, 7, 30)

          ST = SMG$SELECT_FROM_MENU(KEYB, DISPMENU, MENUOPT,
     *                        DEFAULT,,,,,,SMG$M_BOLD + SMG$M_REVERSE )

          ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)

          ST = SMG$UNPASTE_VIRTUAL_DISPLAY (DISPMENU,PB)

          IF(MENUOPT .EQ. 1) THEN
            CALL LIST_FILE(KEYB, DISP2, DISP3)
          ELSEIF (MENUOPT .EQ. 2) THEN
            CALL CHANGE_FILE(KEYB, DISP2, DISP3,PB)
          ELSEIF (MENUOPT .EQ. 3) THEN
            CALL CREATE_FILE(KEYB, DISP2, DISP3,PB)
          ENDIF

        ENDDO

        ST = SMG$UNPASTE_VIRTUAL_DISPLAY (DISP1,PB)
        ST = SMG$UNPASTE_VIRTUAL_DISPLAY (DISP2,PB)
        ST = SMG$UNPASTE_VIRTUAL_DISPLAY (DISP3,PB)

        ST = SMG$DELETE_VIRTUAL_DISPLAY (DISP1,PB)
        ST = SMG$DELETE_VIRTUAL_DISPLAY (DISP2,PB)
        ST = SMG$DELETE_VIRTUAL_DISPLAY (DISP3,PB)
        ST = SMG$DELETE_VIRTUAL_DISPLAY (DISPMENU,PB)

        ST = SMG$DELETE_PASTEBOARD (PB)
        CALL GSTOP(GEXIT_SUCCESS)
        END
C******************************************************************
C SUBROUTINE CHANGE_FILE (UPDATE DATA IN FILE)
C******************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CHANGE_FILE( KEYB, DISP2, DISP3,PB)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:TAXCONFIG.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
        INCLUDE       '($TRMDEF)'

        INTEGER*4 ST,FDB(7)
        LOGICAL ONDISK

        INTEGER KEYB,PB,            ! VIRTUAL KEYBOARD
     *        DISP2,                ! output data frame window
     *        DISP3                 ! message frame window

        CHARACTER*78 cMSG
        CHARACTER*20 cMSG1
        CHARACTER*78 cMSG2
        CHARACTER*78 cMSG3
        INTEGER      KEY, IST
        COMMON       TXCF_REC
        LOGICAL      CHANGED
        INTEGER*4    MULTWEEK(56),IND,I,ACTVALUE
        INTEGER*4    I4AUX1,I4AUX2

        TXCF_FRE(1) = 0
        CHANGED     = .FALSE.
C
C CREATES MENU
C
        ST = SMG$LABEL_BORDER (DISP2,' ALTERAR ',
     *                       SMG$K_TOP, 70, SMG$M_REVERSE)

        ST = SMG$ERASE_DISPLAY(DISP2)

        cMSG = ' Deseja alterar as configurações de Imposto?'
        CALL MENU_CONFIRM(cMSG,2,4,5,50,DISP2,KEYB,PB,ST)
        IF (ST .NE. 0 ) THEN
          ST = SMG$ERASE_DISPLAY(DISP2)
          RETURN
        ENDIF

        INQUIRE(FILE='FILE:TAXCONF.FIL', EXIST=ONDISK)
        IF (.NOT. ONDISK) THEN
          cMSG = ' O ficheiro <FILE:TAXCONF.FIL> não existe no Millennium...'
          CALL SMG_WERROR(cMSG,DISP3,KEYB,KEY)
          ST = SMG$ERASE_DISPLAY(DISP2)
          RETURN
        ENDIF
C
C
C
        CALL OPENX(1,'FILE:TAXCONF.FIL',4,0,0,ST)
        CALL IOINIT(FDB,1,TXCF_SEC*256)
        IF (ST .NE. 0) THEN
          cMSG = ' Erro ao abrir o ficheiro <FILE:TAXCONF.FIL>...'
          CALL SMG_WERROR(cMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
        CALL READW(FDB,1,TXCF_REC,ST)
        IF (ST .NE. 0) THEN
          cMSG = ' Erro ao ler o ficheiro <FILE:TAXCONF.FIL>...'
          CALL SMG_WERROR(cMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF

!        CALL MENU_CONFIRM(cMSG,14,4,10,50,DISP2,KEYB,PB,ST)
!        IF (ST .EQ. 0 ) THEN
!           IF (ECFACTIVE .EQ. 0 ) THEN
!              CMSG = ' Deseja activar o Euro Milhões? '
!              ACTVALUE = 0
!           ELSE
!              CMSG = ' Deseja desactivar o Euro Milhões? '
!              ACTVALUE = 1
!           ENDIF
!
!           CALL MENU_CONFIRM(CMSG,16,4,10,40,DISP2,KEYB,PB,ST)
!           IF (ST .EQ. 0) THEN
!              IF (ACTVALUE .EQ. 0) THEN
!                ECFACTIVE = 1
!              ELSE
!                ECFACTIVE = 0
!              ENDIF
!              CHANGED = .TRUE.
!           ENDIF
!        ENDIF
!        ST = SMG$ERASE_DISPLAY(DISP2)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                   MODIFICATION OF TAX VALUE FOR AM
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        cMSG2 = ' A alterar as configurações de Imposto...'
        ST = SMG$PUT_CHARS (DISP2, cMSG2, 2, 5,,)
        IF (.NOT. ST) CALL LIB$SIGNAL(%VAL(ST))
C
        cMSG3 = ' Use setas para movimentar cursor e <ENTER> para escolher '
        ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)
C
        cMSG2 = ' Deseja alterar a Taxa de Imposto Prémios AM?'
        CALL MENU_CONFIRM(cMSG2,4,4,10,55,DISP2,KEYB,PB,ST)
        IF (ST .EQ. 0 ) THEN
          IST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)

          cMSG3 = ' Digite a nova Taxa de Imposto Prémios AM ou <NEXT> para sair'
          ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)

          cMSG2 = ' Digite a nova Taxa: '
          ST = SMG$PUT_CHARS(DISP2, cMSG2, 6, 25)
          cMSG2 = ','
          ST = SMG$PUT_CHARS(DISP2, cMSG2, 6, 70)

          I4AUX1 = INT(TXCF_AMTAX / 100)
          I4AUX2 = MOD(TXCF_AMTAX , 100)
          CALL SMG_INPNUM2(PB,KEYB,DISP2,DISP3,I4AUX1,6,62,3,0,100,ST)

          cMSG3 = ' Digite a nova Taxa de Imposto Prémios AM ou <NEXT> para sair'
          ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)

          IF (I4AUX1 .LT. 100) THEN
	          CALL SMG_INPNUM2(PB,KEYB,DISP2,DISP3,I4AUX2,6,71,2,0,99,ST)
          ELSE
            CALL SMG_INPNUM2(PB,KEYB,DISP2,DISP3,I4AUX2,6,71,2,0,00,ST)
          ENDIF
          TXCF_AMTAX = I4AUX1*100 + I4AUX2
          CHANGED = .TRUE.
        ENDIF
C
        ST = SMG$ERASE_DISPLAY(DISP2)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C         MODIFICATION OF MINIMUM AMOUNT TO WHICH THE AM TAX APPLIES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        cMSG2 = ' A alterar as configurações de Imposto...'
        ST = SMG$PUT_CHARS (DISP2, cMSG2, 2, 5,,)
C
        cMSG3 = ' Use setas para movimentar cursor e <ENTER> para escolher '
        ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)
C
        cMSG2 = ' Deseja alterar o Menor Prémio Aplicável AM?'
        CALL MENU_CONFIRM(cMSG2,4,4,10,55,DISP2,KEYB,PB,ST)
        IF (ST .EQ. 0 ) THEN
          IST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)

          cMSG2 = ' Digite o novo valor: '
          IST = SMG$PUT_CHARS(DISP2, cMSG2, 6, 25)
          cMSG2 = ','
          IST = SMG$PUT_CHARS(DISP2, cMSG2, 6, 70)

          cMSG3 = ' Digite o novo Menor Prémio Aplicável AM ou <NEXT> para sair'
          ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)

          I4AUX1 = INT(TXCF_AMBSAMNT / 100)
          I4AUX2 = MOD(TXCF_AMBSAMNT , 100)
          CALL SMG_INPNUM2(PB,KEYB,DISP2,DISP3,I4AUX1,6,54,7,0,9999999,ST)

          cMSG3 = ' Digite o novo Menor Prémio Aplicável AM ou <NEXT> para sair'
          ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)

	        CALL SMG_INPNUM2(PB,KEYB,DISP2,DISP3,I4AUX2,6,71,2,0,99,ST)
	        TXCF_AMBSAMNT = I4AUX1*100 + I4AUX2
          CHANGED = .TRUE.
        ENDIF
C
        ST = SMG$ERASE_DISPLAY(DISP2)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                MODIFICATION OF UNAMORTIZED VALUE IN AM TAX
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        cMSG2 = ' A alterar as configurações de Imposto...'
        ST = SMG$PUT_CHARS (DISP2, cMSG2, 2, 5,,)
C
        cMSG3 = ' Use setas para movimentar cursor e <ENTER> para escolher '
        ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)
C
        cMSG2 = ' Deseja alterar o valor a amortizar no Imposto AM?'
        CALL MENU_CONFIRM(cMSG2,4,4,10,55,DISP2,KEYB,PB,ST)
        IF (ST .EQ. 0 ) THEN
          IST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)

          cMSG2 = ' Digite o novo valor: '
          IST = SMG$PUT_CHARS(DISP2, cMSG2, 6, 25)
          cMSG2 = ','
          IST = SMG$PUT_CHARS(DISP2, cMSG2, 6, 70)

          cMSG3 = ' Digite o novo Valor a Amortizar no Imposto AM ou <NEXT> para sair'
          ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)

          I4AUX1 = INT(TXCF_AMTAXRFN / 100)
          I4AUX2 = MOD(TXCF_AMTAXRFN , 100)
          CALL SMG_INPNUM2(PB,KEYB,DISP2,DISP3,I4AUX1,6,54,7,0,9999999,ST)

          cMSG3 = ' Digite o novo Valor a Amortizar no Imposto AM ou <NEXT> para sair'
          ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)

	        CALL SMG_INPNUM2(PB,KEYB,DISP2,DISP3,I4AUX2,6,71,2,0,99,ST)
	        TXCF_AMTAXRFN = I4AUX1*100 + I4AUX2
          CHANGED = .TRUE.
        ENDIF
C
        ST = SMG$ERASE_DISPLAY(DISP2)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                   MODIFICATION OF TAX VALUE FOR LN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        cMSG2 = ' A alterar as configurações de Imposto...'
        ST = SMG$PUT_CHARS (DISP2, cMSG2, 2, 5,,)
C
        cMSG3 = ' Use setas para movimentar cursor e <ENTER> para escolher '
        ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)
C
        cMSG2 = ' Deseja alterar a Taxa de Imposto Prémios LN?'
        CALL MENU_CONFIRM(cMSG2,4,4,10,55,DISP2,KEYB,PB,ST)
        IF (ST .EQ. 0 ) THEN
          IST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)

          cMSG3 = ' Digite a nova Taxa de Imposto Prémios LN ou <NEXT> para sair'
          ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)

          cMSG2 = ' Digite a nova Taxa: '
          IST = SMG$PUT_CHARS(DISP2, cMSG2, 6, 25)
          cMSG2 = ','
          IST = SMG$PUT_CHARS(DISP2, cMSG2, 6, 70)

          I4AUX1 = INT(TXCF_LNTAX / 100)
          I4AUX2 = MOD(TXCF_LNTAX , 100)
          CALL SMG_INPNUM2(PB,KEYB,DISP2,DISP3,I4AUX1,6,62,3,0,100,ST)

          cMSG3 = ' Digite a nova Taxa de Imposto Prémios LN ou <NEXT> para sair'
          ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)

          IF (I4AUX1 .LT. 100) THEN
	          CALL SMG_INPNUM2(PB,KEYB,DISP2,DISP3,I4AUX2,6,71,2,0,99,ST)
          ELSE
            CALL SMG_INPNUM2(PB,KEYB,DISP2,DISP3,I4AUX2,6,71,2,0,00,ST)
          ENDIF
          TXCF_LNTAX = I4AUX1*100 + I4AUX2
          CHANGED = .TRUE.
        ENDIF
C
        ST = SMG$ERASE_DISPLAY(DISP2)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C         MODIFICATION OF MINIMUM AMOUNT TO WHICH THE LN TAX APPLIES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        cMSG2 = ' A alterar as configurações de Imposto...'
        ST = SMG$PUT_CHARS (DISP2, cMSG2, 2, 5,,)
C
        cMSG3 = ' Use setas para movimentar cursor e <ENTER> para escolher '
        ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)
C
        cMSG2 = ' Deseja alterar o Menor Prémio Aplicável LN?'
        CALL MENU_CONFIRM(cMSG2,4,4,10,55,DISP2,KEYB,PB,ST)
        IF (ST .EQ. 0 ) THEN
          IST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)

          cMSG2 = ' Digite o novo valor: '
          IST = SMG$PUT_CHARS(DISP2, cMSG2, 6, 25)
          cMSG2 = ','
          IST = SMG$PUT_CHARS(DISP2, cMSG2, 6, 70)

          cMSG3 = ' Digite o novo Menor Prémio Aplicável LN ou <NEXT> para sair'
          ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)

          I4AUX1 = INT(TXCF_LNBSAMNT / 100)
          I4AUX2 = MOD(TXCF_LNBSAMNT , 100)
          CALL SMG_INPNUM2(PB,KEYB,DISP2,DISP3,I4AUX1,6,54,7,0,9999999,ST)

          cMSG3 = ' Digite o novo Menor Prémio Aplicável LN ou <NEXT> para sair'
          ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)

	        CALL SMG_INPNUM2(PB,KEYB,DISP2,DISP3,I4AUX2,6,71,2,0,99,ST)
	        TXCF_LNBSAMNT = I4AUX1*100 + I4AUX2
          CHANGED = .TRUE.
        ENDIF
C
        ST = SMG$ERASE_DISPLAY(DISP2)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                MODIFICATION OF UNAMORTIZED VALUE IN LN TAX
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        cMSG2 = ' A alterar as configurações de Imposto...'
        ST = SMG$PUT_CHARS (DISP2, cMSG2, 2, 5,,)
C
        cMSG3 = ' Use setas para movimentar cursor e <ENTER> para escolher '
        ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)
C
        cMSG2 = ' Deseja alterar o valor a amortizar no Imposto LN?'
        CALL MENU_CONFIRM(cMSG2,4,4,10,55,DISP2,KEYB,PB,ST)
        IF (ST .EQ. 0 ) THEN
          IST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)

          cMSG2 = ' Digite o novo valor: '
          IST = SMG$PUT_CHARS(DISP2, cMSG2, 6, 25)
          cMSG2 = ','
          IST = SMG$PUT_CHARS(DISP2, cMSG2, 6, 70)

          cMSG3 = ' Digite o novo Valor a Amortizar no Imposto LN ou <NEXT> para sair'
          ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)

          I4AUX1 = INT(TXCF_LNTAXRFN / 100)
          I4AUX2 = MOD(TXCF_LNTAXRFN , 100)
          CALL SMG_INPNUM2(PB,KEYB,DISP2,DISP3,I4AUX1,6,54,7,0,9999999,ST)

          cMSG3 = ' Digite o novo Valor a Amortizar no Imposto LN ou <NEXT> para sair'
          ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)

	        CALL SMG_INPNUM2(PB,KEYB,DISP2,DISP3,I4AUX2,6,71,2,0,99,ST)
	        TXCF_LNTAXRFN = I4AUX1*100 + I4AUX2
          CHANGED = .TRUE.
        ENDIF
C
        ST = SMG$ERASE_DISPLAY(DISP2)
C
        CALL CLOSEFIL(FDB)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                         GRAVAR AS ALTERAÇÕES ?
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        IF (CHANGED) THEN
          cMSG3 = ' Use setas para movimentar cursor e <ENTER> para escolher '
          ST = SMG$PUT_CHARS (DISP3, cMSG3, 1,1)
          cMSG2 = ' DESEJA GRAVAR AS ALTERAÇÕES?'
          CALL MENU_CONFIRM(cMSG2,8,4,10,35,DISP2,KEYB,PB,ST)
          IF (ST .EQ. 0 ) THEN
C+++++++++++++++
            IF (TXCF_AMBSAMNT .LT. TXCF_AMTAXRFN ) THEN
              ST = SMG$ERASE_DISPLAY(DISP2)
              cMSG2 = ' Valor a amortizar no imposto AM maior que Menor Prémio Aplicável AM!'
              ST = SMG$PUT_CHARS (DISP2, cMSG2, 9, 5,,)
              cMSG3 = ' As alterações efectuadas NAO SERÃO guardadas!'
              CALL SMG_WERROR(cMSG3,DISP3,KEYB,KEY)
              ST = SMG$ERASE_DISPLAY(DISP2)
              RETURN
            ENDIF
C           
            IF (TXCF_LNBSAMNT .LT. TXCF_LNTAXRFN ) THEN
              ST = SMG$ERASE_DISPLAY(DISP2)
              cMSG2 = ' Valor a amortizar no imposto LN maior que Menor Prémio Aplicável LN!'
              ST = SMG$PUT_CHARS (DISP2, cMSG2, 9, 5,,)
              cMSG3 = ' As alterações efectuadas NAO SERÃO guardadas!'
              CALL SMG_WERROR(cMSG3,DISP3,KEYB,KEY)
              ST = SMG$ERASE_DISPLAY(DISP2)
              RETURN
            ENDIF
C+++++++++++++++
            CALL OPENX(1,'FILE:TAXCONF.FIL',4,0,0,ST)
            CALL IOINIT(FDB,1,TXCF_SEC*256)
            IF (ST .NE. 0) THEN
              cMSG3 = ' Erro ao abrir o ficheiro <FILE:TAXCONF.FIL>...'
              CALL SMG_WERROR(cMSG3,DISP3,KEYB,KEY)
              RETURN
            ENDIF
C
            CALL WRITEW(FDB,1,TXCF_REC,ST)
            IF (ST .NE. 0) THEN
              cMSG3 = ' Erro ao escrever no ficheiro <FILE:TAXCONF.FIL>...'
              CALL SMG_WERROR(cMSG3,DISP3,KEYB,KEY)
              RETURN
            ELSE
              CALL CLOSEFIL(FDB)
              ST = SMG$ERASE_DISPLAY(DISP2)
              cMSG3 = ' Alterações gravadas com SUCESSO no ficheiro <FILE:TAXCONF.FIL>'
              CALL SMG_WERROR(cMSG3,DISP3,KEYB,KEY)
              CALL LIST_FILE(KEYB, DISP2, DISP3)
            ENDIF
          ENDIF
        ENDIF
C
        ST = SMG$ERASE_DISPLAY(DISP2)
C
        RETURN
100     FORMAT (Z2.2)
        END
C***********************************************************************
C       SUBROUTINE LIST_FILE (LIST DATA FROM FILE:TAXCONF.FIL)
C***********************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE LIST_FILE( KEYB, DISP2, DISP3)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:TAXCONFIG.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'

        INTEGER*4 ST,FDB(7)
        LOGICAL ONDISK

        INTEGER KEYB,         ! VIRTUAL KEYBOARD
     *        DISP2,          ! output data frame window
     *        DISP3           ! message frame window

        CHARACTER*78  cMSG,cMSG4
        CHARACTER*39  cMSG1
        CHARACTER*43  cMSG2
        CHARACTER*16  cMSG3
        INTEGER       KEY
        INTEGER*4     MULTWEEK(56),IND,I
        INTEGER*4     I4AUX1, I4AUX2

        TXCF_FRE(1) = 0

C
C CREATES MENU
C
        ST = SMG$LABEL_BORDER (DISP2,' VER ',
     *                       SMG$K_TOP, 74, SMG$M_REVERSE)

        ST = SMG$ERASE_DISPLAY(DISP2)
        cMSG = 'A ler Configurações Registadas. Aguarde...'
        ST = SMG$PUT_CHARS (DISP3, cMSG, 1, 1, SMG$M_ERASE_LINE)

        INQUIRE(FILE='FILE:TAXCONF.FIL', EXIST=ONDISK)
        IF (.NOT. ONDISK) THEN
           cMSG = ' O ficheiro <FILE:TAXCONF.FIL> não existe no Millennium...'
           CALL SMG_WERROR(cMSG,DISP3,KEYB,KEY)
           RETURN
        ENDIF
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                   OPENING OF FILE:TAXCONF.FIL FILE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        CALL OPENX(1,'FILE:TAXCONF.FIL',4,0,0,ST)
        CALL IOINIT(FDB,1,TXCF_SEC*256)
        IF(ST .NE. 0) THEN
          cMSG = ' Erro ao abrir o ficheiro <FILE:TAXCONF.FIL>...'
          CALL SMG_WERROR(cMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
        CALL READW(FDB,1,TXCF_REC,ST)
        IF(ST.NE.0) THEN
          cMSG = ' Erro ao ler o ficheiro <FILE:TAXCONF.FIL>...'
          CALL SMG_WERROR(cMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
        cMSG1 = ' Dados de Configuração de Imposto'
        ST = SMG$PUT_CHARS (DISP2, cMSG1, 2, 20,,SMG$M_BOLD)
        cMSG = 'Para voltar ao menu anterior prima qualquer tecla...'
        ST = SMG$PUT_CHARS (DISP3, cMSG, 1, 1, SMG$M_ERASE_LINE)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                  TAX VALUE FOR AM (MUTUAL BETS)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        cMSG2 = 'Taxa de Imposto Prémio AM       : '
        I4AUX1 = INT(TXCF_AMTAX / 100)
        I4AUX2 = MOD(TXCF_AMTAX , 100)
        WRITE(cMSG3,110) I4AUX1, I4AUX2
        ST = SMG$PUT_CHARS (DISP2, cMSG2, 4, 15,, )
        ST = SMG$PUT_CHARS (DISP2, cMSG3, 4, 50,, SMG$M_BOLD)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C           MINIMUM AMOUNT TO WHICH THE AM TAX APPLIES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        cMSG2 = 'Menor Prémio Aplicável AM       : '
        I4AUX1 = INT(TXCF_AMBSAMNT / 100)
        I4AUX2 = MOD(TXCF_AMBSAMNT , 100)
        WRITE(cMSG3,111) I4AUX1, I4AUX2
        ST = SMG$PUT_CHARS (DISP2, cMSG2, 6, 15,, )
        ST = SMG$PUT_CHARS (DISP2, cMSG3, 6, 50,, SMG$M_BOLD)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                   UNAMORTIZED VALUE IN AM TAX
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        cMSG2 = 'Valor a Amortizar no Imposto AM : '
        I4AUX1 = INT(TXCF_AMTAXRFN / 100)
        I4AUX2 = MOD(TXCF_AMTAXRFN , 100)
        WRITE(cMSG3,111) I4AUX1, I4AUX2
        ST = SMG$PUT_CHARS (DISP2, cMSG2, 8, 15,,)
        ST = SMG$PUT_CHARS (DISP2, cMSG3, 8, 50,, SMG$M_BOLD)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                  TAX VALUE FOR LN (MUTUAL BETS)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        cMSG2 = 'Taxa de Imposto Prémio LN       : '
        I4AUX1 = INT(TXCF_LNTAX / 100)
        I4AUX2 = MOD(TXCF_LNTAX , 100)
        WRITE(cMSG3,110) I4AUX1, I4AUX2
        ST = SMG$PUT_CHARS (DISP2, cMSG2, 10, 15,, )
        ST = SMG$PUT_CHARS (DISP2, cMSG3, 10, 50,, SMG$M_BOLD)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C           MINIMUM AMOUNT TO WHICH THE LN TAX APPLIES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        cMSG2 = 'Menor Prémio Aplicável LN       : '
        I4AUX1 = INT(TXCF_LNBSAMNT / 100)
        I4AUX2 = MOD(TXCF_LNBSAMNT , 100)
        WRITE(cMSG3,111) I4AUX1, I4AUX2
        ST = SMG$PUT_CHARS (DISP2, cMSG2, 12, 15,, )
        ST = SMG$PUT_CHARS (DISP2, cMSG3, 12, 50,, SMG$M_BOLD)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                   UNAMORTIZED VALUE IN LN TAX
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        cMSG2 = 'Valor a Amortizar no Imposto LN : '
        I4AUX1 = INT(TXCF_LNTAXRFN / 100)
        I4AUX2 = MOD(TXCF_LNTAXRFN , 100)
        WRITE(cMSG3,111) I4AUX1, I4AUX2
        ST = SMG$PUT_CHARS (DISP2, cMSG2, 14, 15,,)
        ST = SMG$PUT_CHARS (DISP2, cMSG3, 14, 50,, SMG$M_BOLD)

        ST = SMG$READ_KEYSTROKE(KEYB, KEY, , DISP3)
        ST = SMG$ERASE_DISPLAY(DISP2)

        CALL CLOSEFIL(FDB)
C
        RETURN
100     FORMAT (Z4.4)
101     FORMAT (I4.4)
102     FORMAT (I4.4)
103     FORMAT (I4.4)
104     FORMAT (I1)
105     FORMAT (I5)
106     FORMAT (I7.7)
107     FORMAT (I7.5)
108     FORMAT (F7.2)
109     FORMAT (F11.2)
110     FORMAT (I7, ',', I2.2, ' %')
111     FORMAT (I7, ',', I2.2, ' EUR')
        END
C***********************************************************************
C        SUBROUTINE CREATE_FILE (CREATES THE FILE:TAXCONF.FIL IF DOES NOT EXIST)
C***********************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CREATE_FILE( KEYB, DISP2, DISP3,PB)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:TAXCONFIG.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'

        INTEGER*4 ST,SECTORS,KEY
        LOGICAL ONDISK
        COMMON TXCF_REC
        INTEGER PB,KEYB,        ! VIRTUAL KEYBOARD
     *        DISP2,            ! output data frame window
     *        DISP3             ! message frame window

        CHARACTER*78  cMSG
        CHARACTER*30  cMSG1

        TXCF_FRE(1) = 0

        ST = SMG$LABEL_BORDER (DISP2,' CRIAR FICHEIRO ',
     *                       SMG$K_TOP, 63, SMG$M_REVERSE)

        ST = SMG$ERASE_DISPLAY(DISP2)
        cMSG = 'O utilizador vai criar o ficheiro <FILE:TAXCONF.FIL>...'
        ST = SMG$PUT_CHARS (DISP3, cMSG, 1, 1, SMG$M_ERASE_LINE)

        INQUIRE(FILE='FILE:TAXCONF.FIL', EXIST=ONDISK)
        IF (ONDISK) THEN
           cMSG = ' O ficheiro <FILE:TAXCONF.FIL> já existe no Millennium...'
           CALL SMG_WERROR(cMSG,DISP3,KEYB,KEY)
           RETURN
        ENDIF
        cMSG = ' A criar o ficheiro de configuração de Imposto...'
        ST = SMG$PUT_CHARS (DISP2, cMSG, 2, 5,,)

        cMSG1 = ' Digite o número de blocos: '
        ST = SMG$PUT_CHARS (DISP2, cMSG1, 6, 15,,)
        CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,SECTORS,6,45,3,200,999,ST)

        WRITE(cMSG,100) SECTORS
        ST = SMG$PUT_CHARS (DISP2, cMSG, 8, 15,,)

        CALL SMG_NEWFIL(7,'FILE:TAXCONF.FIL',SECTORS,ST)
        IF (ST .NE. 0) THEN
           cMSG = ' Erro o ficheiro <FILE:TAXCONF.FIL> não foi criado...'
           CALL SMG_WERROR(cMSG,DISP3,KEYB,KEY)
           RETURN
        ENDIF

        cMSG1 = ' Ficheiro Criado...'
        ST = SMG$PUT_CHARS (DISP2, cMSG1, 10, 15,,)

        ST = SMG$READ_KEYSTROKE(KEYB, KEY, , DISP3)
        ST = SMG$ERASE_DISPLAY(DISP2)
        RETURN
100     FORMAT (' Vai ser criado o ficheiro <FILE:TAXCONF.FIL> com ',I3.3,' blocos')
        END
C***********************************************************************
C       SUBROUTINE MENU_CONFIRM
C***********************************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE MENU_CONFIRM(CMSG,LIN,LINEND,COL,COLEND,DISP2,IKEYB,PB,ST)
        IMPLICIT NONE

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
C
C ROUTINE PARAMETERS
C
        CHARACTER*50 CMSG

        INTEGER DISP2,IKEYB,PB
        INTEGER*4 ST,LIN,COL,LINEND,COLEND
C
C LOCAL VARIABLES
C
        CHARACTER*5 CONFIRM(2)/' Sim ', ' Não '/
        CHARACTER*5 OPTION    /' SIM '/

        INTEGER DISP4
        INTEGER*4 IOPT
C
C ASK FOR CONFIRMATION
C
        ST = SMG$PUT_CHARS (DISP2, CMSG, LIN, COL)
C
C CREATES YES/NO MENU DISPLAY - DISPCONF
C
        ST = SMG$CREATE_VIRTUAL_DISPLAY(1, 15, DISP4)
        ST = SMG$CREATE_MENU(DISP4, CONFIRM,
     *                       SMG$K_HORIZONTAL,,, SMG$M_REVERSE)

        ST = SMG$PASTE_VIRTUAL_DISPLAY(DISP4, PB, LIN+LINEND, COL+COLEND)
        ST = SMG$SELECT_FROM_MENU(IKEYB, DISP4, IOPT, 2,
     *            ,,,, OPTION, SMG$M_BOLD)
        ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)
        ST = SMG$UNPASTE_VIRTUAL_DISPLAY(DISP4, PB)
        ST = SMG$DELETE_VIRTUAL_DISPLAY (DISP4,PB)
        IF (IOPT .NE. 1) THEN
          ST = -1
        ELSE
          ST = 0
        ENDIF

        RETURN
        END
C***********************************************************************
C       SUBROUTINE SMG_INPNUM
C***********************************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE SMG_INPNUM2(IPB,IKEYB,IDISP2,IDISP3,
     *                         OUTVAR,ILIN,ICOL,ISIZE,IMIN,IMAX,IST)
        IMPLICIT NONE

        INCLUDE '($SMGDEF)'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
C EXTERNAL VARIABLES
C
        INTEGER   IPB, IKEYB, IDISP2, IDISP3
        INTEGER*4 OUTVAR, ILIN, ICOL, ISIZE, IMIN, IMAX, IST
C
C LOCAL VARIABLES
C
        CHARACTER*10  CIDX
        CHARACTER*76  CMSG

        INTEGER   SMG$ERASE_DISPLAY, SMG$PUT_CHARS, SMG$RING_BELL

        INTEGER*4 ICIDXLEN, INEXT
        INTEGER*4 AUXOUTVAR

        LOGICAL   REPEAT
C
C BEGIN OF CODE
C
        REPEAT = .TRUE.

        DO WHILE(REPEAT)
          INEXT = 2
          IF (OUTVAR.EQ.0) THEN
            CIDX = '          '
          ELSE
            WRITE(CIDX,100) OUTVAR
          ENDIF

          CALL INPUT2(IPB, IKEYB, IDISP2, ILIN, ICOL, ISIZE, 1, 'N',
     *               CIDX, ICIDXLEN, INEXT)

          DECODE(ICIDXLEN,100,CIDX) AUXOUTVAR

          IF (AUXOUTVAR.LT.IMIN .OR. AUXOUTVAR.GT.IMAX) THEN
            WRITE (CMSG,200) IMIN, IMAX
            IST = SMG$PUT_CHARS (IDISP3, CMSG, 1,5,
     *                           SMG$M_ERASE_LINE, SMG$M_BOLD)
            IST = SMG$RING_BELL(IDISP3, 2)
          ELSE
            REPEAT = .FALSE.
            IST = SMG$ERASE_DISPLAY(IDISP3)
            OUTVAR = AUXOUTVAR

            IF (INEXT.EQ.-1) THEN
              IST = -2
            ELSE
              IST = 1
            ENDIF
          ENDIF
        ENDDO

        RETURN
100     FORMAT(I<ISIZE>.<ISIZE>)
200     FORMAT('VALOR INCORRETO, LIMITES SAO DE ',I10,' A ',I10)
        END

C=============================================================================
C
C Subroutine INPUT()
C
C V01 12-MAR-97 GIL Released for CEF - Alpha
C
C Subroutine to read a typed string from keyboard and to store it in a variable
C
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C=======OPTIONS /CHECK=NOOVERFLOW

      subroutine INPUT2 (PB, KB, VD, LINE, COL_INI, TAM_MAX, FLAG, KIND,
     *			OUTSTRING, COUNT, NEXT_LINE)

      implicit NONE
      include '($SMGDEF)'

      integer     SMG$Put_Chars
      integer     SMG$Read_From_Display,	SMG$Set_Cursor_Abs
      integer     SMG$Set_Cursor_Mode
      integer     SMG$Read_Keystroke
      integer     SMG$Repaint_Screen
      integer     SMG$Set_Keypad_Mode,		STR$Trim

      integer       PB              ! PASTEBOARD
      integer       KB              ! VIRTUAL KEYBOARD
      integer       VD              ! VIRTUAL DISPLAY
      integer       LINE            ! LINE OF THE FIELD
      integer       COL_INI         ! INITIAL COLUMN OF THE FIELD
      integer       TAM_MAX         ! MAXIMUM SIZE OF STRING
      integer       FLAG            ! INDICATES FIELD ASPECT OF STRING INPUT:
                                    !       0 - STRING = ______
                                    !       1 - STRING = |_|_|_|_|
      character*1   KIND            ! STRING TYPE:
                                    !       'N' - NUMERIC STRING
                                    !       'A' - ALPHANUMERIC STRING
      character*(*) OUTSTRING       ! VARIABLE RECEIVING THE TYPED CHARACTERS
      integer       COUNT           ! TYPED CHARACTERS COUNTER
      integer       NEXT_LINE       ! LINE ATTITUDE (first/prev/stay/next/last)

C***************************************************************************

      character*80  INSTRING         ! internal variable of user typed chars
      character*1   CARACTERE  /' '/ ! user typed char
      integer       COL_BASE   /0/   ! column of string begining
      integer       COL        /0/   ! column position of cursor
      integer       ST         /0/   ! smg$ error code
      integer       NESQ       /0/   ! left hand of cursor char counter
      integer       KEY        /0/   ! ASCII code of typed key
      integer       STEP       /0/
      integer       I, J, K, L
      integer       IK, KK
      integer       IRET       /0/
      integer       LF         /0/
      integer       COMMA      /0/
      logical       VALID      /.false./
      logical       EDITA      /.true./
      logical       UPDOWN_KEY /.false./  ! enables up/down keys as return field
      logical       PRVNXT_KEY /.false./  ! enables also prev/next keys as return field

C*****************************************************************************
C2345#***  Checks if input() end with ENTER or reaching the end of field

      ST = SMG$Set_Keypad_Mode (KB, 0)         ! INIBIR QUANDO DEBUGAR!!!
      if (COUNT .eq. -1) then
         IRET = -1                   ! ends input at reaching the end of field
      else
         IRET = 0                    ! ends input w/ENTER
      end if
      COUNT = 0

C2345#***  Builds data input field: underlined (___) or spaced (|_|)
      do I = 1, TAM_MAX              ! initialize resulting string
         INSTRING(I:I) = ' '
      end do
      COMMA = 0
      ! receives previous string and removes trailing blanks
      ST = STR$Trim (INSTRING, OUTSTRING, COUNT) ! dest<-sour, c
      if (FLAG .eq. 1) then           ! format = spaced field
         COL_BASE = COL_INI + 1
         STEP = 2
         ST = SMG$Put_Chars (VD, '|', LINE, COL_INI,, SMG$M_REVERSE, SMG$M_BOLD)
         do I = 1, COUNT
            ST = SMG$Put_Chars (VD, INSTRING(I:I)//'|', LINE, COL_BASE+2*(I-1),
     *                          ,SMG$M_REVERSE, SMG$M_BOLD)
            if ((INSTRING(I:I) .eq. ',') .or. (INSTRING(I:I) .eq. '.')) COMMA = COMMA + 1
         end do
         if (COUNT .lt. TAM_MAX) then
            do I = COUNT+1, TAM_MAX
               ST = SMG$Put_Chars (VD, '0|', LINE, COL_BASE+2*(I-1),, SMG$M_REVERSE, SMG$M_BOLD)
            end do
         endif
         K = 0
      else                            ! format = underlined field
         COL_BASE = COL_INI
         STEP = 1
         do I = 1, COUNT
            ST = SMG$Put_Chars (VD, INSTRING(I:I), LINE, (COL_BASE - 1 + I),, SMG$M_REVERSE, SMG$M_BOLD)
            if ((INSTRING(I:I) .eq. ',') .or. (INSTRING(I:I) .eq. '.')) COMMA = COMMA + 1
         end do
         if (COUNT .lt. TAM_MAX) then
            do I = COUNT+1, TAM_MAX
               ST = SMG$Put_Chars (VD, '_', LINE, (COL_BASE - 1 + I),, SMG$M_REVERSE, SMG$M_BOLD)
            end do
         endif
      end if

      COL = COL_BASE + COUNT*STEP                ! position cursor at end of string
      NESQ = COUNT
      KEY = 0
      ST = SMG$Set_Cursor_Mode (PB, SMG$M_CURSOR_ON)
      if (NEXT_LINE .eq. 0) then
	UPDOWN_KEY = .false.
	PRVNXT_KEY = .false.
      elseif (NEXT_LINE .eq. 1) then
	UPDOWN_KEY = .true.
	PRVNXT_KEY = .false.
      elseif (NEXT_LINE .eq. 2) then
	UPDOWN_KEY = .true.
	PRVNXT_KEY = .true.
      endif

C2345#*****  TYPING STRING CHARS (while not return keys => accepts data) ******
      EDITA = .true.
      do while (EDITA)
         ST = SMG$Set_Cursor_Abs (VD, LINE, COL)
         ST = SMG$Read_Keystroke (KB, KEY, , VD)

C2345#*****  aborts edition  *******
         if (KEY .eq. 26) then    			! [CTRL+Z] aborts and gets off
            ST = STR$Trim (OUTSTRING, OUTSTRING, COUNT) ! keeps out string unchanged
            ST = SMG$Set_Cursor_Mode (PB, SMG$M_CURSOR_OFF)
	    EDITA = .false.
            return

C2345#*****  deletes one character  *******

         elseif ((KEY .eq. SMG$K_TRM_DELETE) .or. (KEY .eq. 311)) then       ! DEL key = #127
            if (COL .gt. COL_BASE) then                  ! count > 0
              if (KIND .eq. 'N') then
                 ST = SMG$Set_Cursor_Abs (VD, LINE, COL - STEP)
                 ST = SMG$Read_From_Display (VD, CARACTERE)
                 if (CARACTERE .eq. ',') COMMA = COMMA - 1
                 ST = SMG$Set_Cursor_Abs (VD, LINE, COL)
              end if
              J = COL_BASE + (TAM_MAX-1)*STEP           ! picks last field position
              do I = COL, J, STEP                       ! from curret to last position ...
                 ST = SMG$Set_Cursor_Abs (VD, LINE, I)
                 ST = SMG$Read_From_Display (VD, CARACTERE)
                 ST = SMG$Put_Chars (VD, CARACTERE, LINE, I-STEP,, SMG$M_REVERSE, SMG$M_BOLD)
              end do
              ST = SMG$Put_Chars (VD, '_', LINE, J, , SMG$M_REVERSE, SMG$M_BOLD)

C             do I = J,(COL_BASE+TAM_MAX-1)*STEP, STEP
C                 ST = SMG$Put_Chars (VD, '_', LINE, I, , SMG$M_REVERSE, SMG$M_BOLD)
C             end do

              COUNT = COUNT - 1
              NESQ = NESQ - 1
              COL = COL - STEP
              ST = SMG$Set_Cursor_Abs (VD, LINE, COL)
            end if

C2345#*****  clear all characters  *******

         elseif (KEY .eq. 313) then                            ! REMOVE key
            COL = COUNT*STEP + COL_BASE
            ST = SMG$Set_Cursor_Abs (VD, LINE, COL)
            do while (COL_BASE .lt. COL)
              COL = COL - STEP
              ST = SMG$Set_Cursor_Abs (VD, LINE, COL)
              ST = SMG$Put_Chars (VD, '_', LINE, COL, , SMG$M_REVERSE, SMG$M_BOLD)
            end do
	    COMMA = 0
            COUNT = 0
            NESQ = 0

C2345#*****  refresh screen  ******
         elseif ((KEY .eq. 23) .or. (KEY .eq. 312)) then     ! CTRL+W or INSERT key
            ST = SMG$Repaint_Screen (PB)
            ST = SMG$Set_Cursor_Abs (VD, LINE, COL)
         end if


C2345#*****  arrows allowed only in alphanumeric field  *******************

         if (KIND .eq. 'A') then

C2345#*****  moves cursor to the right  *******
           if (KEY .eq. 277) then               ! RIGHT ARROW key
              if (COL .lt. (COL_BASE + COUNT*STEP)) then
                 COL = COL + STEP
                 NESQ = NESQ + 1
              else                              ! if reach end, comes to begining
                 COL = COL_BASE
                 NESQ = 0
              end if
           end if

C2345#*****  moves cursor to the left  ******
           if (KEY .eq. 276) then               ! LEFT ARROW key
              if (COL .gt. COL_BASE) then
                 COL = COL - STEP
                 NESQ = NESQ - 1
              else                              ! if reach begining, goes to end
                 COL = COL_BASE + COUNT*STEP
                 NESQ = COUNT
              end if
           end if
        end if                                  ! end of cursor moving keys

C2345#*****  Checks if typed character is valid  ******

        if ((COUNT .lt. TAM_MAX) .or. (NESQ .lt. COUNT)) then

           ! *****  only numeric variables (unsigned integer) *****
           if (KIND .eq. 'N') then
              if ((KEY .ge. 48) .and. (KEY .le. 57)) then
                 VALID = .true.                         ! digits
              else
                 VALID = .false.                        ! invalid key
              end if

           ! *****  currency (cents, sign & comma) ******
           else if (KIND .eq. 'M') then                 ! numeric var only
              if ((KEY .ge. 48) .and. (KEY .le. 57)) then
                 VALID = .true.                         ! digits
              else if (((KEY .eq. 44) .or. (KEY .eq. 46)) .and. (COMMA .eq.  0)) then
                 KEY = 44                               ! only ','
                 COMMA = 1                              ! only one comma allowed
                 VALID = .true.
              else if ((KEY .eq. 45) .and. (COUNT .eq. 0)) then
                 VALID = .true.                         ! minus sign allowed at firs column
              else
                 VALID = .false.                        ! invalid key
              end if

           ! *****  alphanumeric variables  ******
           else if (KIND .eq. 'A') then
              if ((KEY .ge. 32) .and. (KEY .le. 126)) then
                 VALID = .true.                         ! characters (except control)
              else
                 VALID = .false.                        ! invalid key
              end if
           end if

C2345#*****  Writes typed character on screen  *******

           if (VALID) then
              CARACTERE = Char (KEY)
              ST = SMG$Set_Cursor_Abs (VD, LINE, COL)
              ST = SMG$Put_Chars (VD, CARACTERE, LINE, COL,, SMG$M_REVERSE, SMG$M_BOLD)
              COL = COL + STEP
              COUNT = COUNT + 1
              NESQ = NESQ + 1
              if (NESQ .lt. COUNT) then                 ! deals overwrite
                COUNT = COUNT - 1
              end if

c2345#*****  Checks if reached end of field and doesn't need ENTER to input

              if ((COUNT .eq. TAM_MAX) .and. (IRET .eq. -1)) KEY = 13                   ! implicit ENTER
           end if                               ! (VALID)
        end if                                  ! (COUNT < TAM_MAX)

	if (PRVNXT_KEY) then
          if (KEY .eq. 315) then  		! PREV key
	    NEXT_LINE = 1
	    EDITA = .false.
          end if
          if (KEY .eq. 316) then		! NEXT key
	    NEXT_LINE = -1
	    EDITA = .false.
          end if
	endif

	if (UPDOWN_KEY) then
          if (KEY .eq. 274) then  		! UP ARROW key
	    NEXT_LINE = LINE - 1
	    EDITA = .false.
          end if
          if (KEY .eq. 275) then		! DOWN ARROW key
	    NEXT_LINE = LINE + 1
	    EDITA = .false.
          end if
	endif

        if (KEY .eq. 13) then
	  NEXT_LINE = LINE + 1	! RETURN key
	  EDITA = .false.
	endif

      enddo		! while (EDITA)

C2345#*******  Reads field from the display and stores into OUTVAR  ******

      if (FLAG .eq. 0) then                                     ! underlined field
         ST = SMG$Set_Cursor_Abs (VD, LINE, COL_INI)
         ST = SMG$Read_From_Display (VD, INSTRING(1:COUNT))     ! spots typed chars
      else                                                      ! spaced field
         IK = 0
         L = COL_INI + 1
         LF = COL_INI + (COUNT*2)
         do KK = L,LF,2
            ST = SMG$Set_Cursor_Abs (VD, LINE, KK)
            IK = IK + 1
            ST = SMG$Read_From_Display (VD, INSTRING(IK:IK))
         end do
      end if
      ST = SMG$Set_Cursor_Mode (PB, SMG$M_CURSOR_OFF)
      do I = COUNT+1,80
         INSTRING(I:I) = ' '
      enddo
      do I = 1, TAM_MAX
         OUTSTRING(I:I) = ' '
      enddo
      ST = STR$Trim (OUTSTRING, INSTRING, COUNT)        ! dest <- sour, c
      ST = SMG$Set_Keypad_Mode (KB, SMG$M_KEYPAD_APPLICATION)
      return
      end

