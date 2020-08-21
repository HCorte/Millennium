C
C BLDEURSYS.FOR
C
C V01  23-MAR-2016 SCML  M16 PROJECT
C
C PROGRAM WILL DISPLAY/UPDATE EUROMILLIONS SYSTEM GAMES FILES
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2004 SCML Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM BLDEURSYS
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
        INCLUDE 'INCLIB:BLDEURSYS.DEF'
C
C        LOCAL VARIABLES
C
        INTEGER*4 ST
C
        INTEGER*4 TAMMENU
        PARAMETER (TAMMENU = 3)
        CHARACTER*78  cMSG
        CHARACTER*15  MYMENU(TAMMENU) /'[ EUROMILHÕES ]',
     *                                 '[   M1LHÃO    ]',
     *                                 '[    SAIR     ]'/
        INTEGER PB,                                                             !PASTEBOARD
     *        KEYB,                                                             !VIRTUAL KEYBOARD
     *        DISP1,                                                            !HEADER FRAME WINDOW
     *        DISP2,                                                            !OUTPUT DATA FRAME WINDOW
     *        DISP3,                                                            !MESSAGE FRAME WINDOW
     *	      DISPMENU                                                          !INITIAL MENU

        INTEGER DEFAULT/1/
        INTEGER*4 MENUOPT
C
C       CREATES SCREEN HEADER - DISP1
C
        ST = SMG$CREATE_VIRTUAL_DISPLAY( 1, 78, DISP1, SMG$M_BORDER)
        ST = SMG$LABEL_BORDER (DISP1,' SCML ',
     *                         SMG$K_TOP, 70,
     *                         SMG$M_BLINK, SMG$M_BOLD)
C
C       CREATES INPUT DISPLAY - DISP2
C
        ST = SMG$CREATE_VIRTUAL_DISPLAY ( 16, 78, DISP2, SMG$M_BORDER)
        ST = SMG$LABEL_BORDER (DISP2,' Configuração ',
     *                         SMG$K_TOP, 65, SMG$M_REVERSE)
C
C     CREATES MESSAGE AND ERROR DISPLAY - DISP3
C
        ST = SMG$CREATE_VIRTUAL_DISPLAY ( 1, 78, DISP3, SMG$M_BORDER)
        ST = SMG$LABEL_BORDER (DISP3,' Mensagem ',
     *                         SMG$K_TOP, 70, SMG$M_REVERSE)
C
C       CREATES VIRTUAL  KEYBOARD - KEYB
C       (inhibit numeric keypad mode if debug)
C
        ST = SMG$CREATE_VIRTUAL_KEYBOARD (KEYB)
C
C       CREATES PASTEBOARD - PB
C
        ST = SMG$CREATE_PASTEBOARD (PB)
        IF (.NOT. ST) CALL LIB$STOP (%VAL(ST))
C
C       CREATES MENU
C
        ST = SMG$CREATE_VIRTUAL_DISPLAY(6, 17, DISPMENU)
        ST = SMG$CREATE_MENU(DISPMENU, MYMENU, SMG$K_VERTICAL,
     *	     SMG$M_DOUBLE_SPACE + SMG$M_WRAP_MENU , 1, 0)
C
C       PASTING VIRTUAL DISPLAYS ON TERMINAL SCREEN
C
        ST = SMG$PASTE_VIRTUAL_DISPLAY (DISP1, PB,  2, 2)
        ST = SMG$PASTE_VIRTUAL_DISPLAY (DISP2, PB,  5, 2)
        ST = SMG$PASTE_VIRTUAL_DISPLAY (DISP3, PB, 23, 2)

        ST = SMG$SET_CURSOR_MODE (PB, SMG$M_CURSOR_OFF )
        ST = SMG$ERASE_DISPLAY (DISP3)
C
        MENUOPT = 0
        DO WHILE(MENUOPT .NE. TAMMENU)
          ST = SMG$SET_CURSOR_MODE (DISP2, SMG$M_CURSOR_OFF )
          ST = SMG$LABEL_BORDER (DISP2,' Configuração ',
     *                           SMG$K_TOP, 65, SMG$M_REVERSE)
C
          cMSG = 'CONFIGURAÇÃO DOS JOGOS DO SISTEMA EUROMILHÕES'
          ST   = SMG$PUT_CHARS(DISP1, cMSG, 1, 17)
          ST   = SMG$PUT_CHARS(DISP1, BLDEURSYS_VERSION, 1, 66)
C
          cMSG = ' Use setas para movimentar cursor e <ENTER>'//
     *           ' para escolher o jogo'
          ST = SMG$PUT_CHARS(DISP3, cMSG, 1, 1)
          ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)
          ST = SMG$PASTE_VIRTUAL_DISPLAY(DISPMENU, PB, 10, 32)                  !LINE 10 COLUMUN 32
          ST = SMG$SELECT_FROM_MENU(KEYB, DISPMENU, MENUOPT,
     *                              DEFAULT,,,,,,SMG$M_BOLD)
          ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)
          ST = SMG$UNPASTE_VIRTUAL_DISPLAY (DISPMENU,PB)
C
          IF(MENUOPT.EQ.1) THEN
            CALL EUM1CONF(PB, KEYB, DISP1, DISP2, DISP3)                        !EUM GAME MENU
          ELSEIF(MENUOPT.EQ.2) THEN
            CALL RAF2CONF(PB, KEYB, DISP1, DISP2, DISP3)                        !SM GAME MENU
          ENDIF
        ENDDO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       EXIT SMG APPLICATION
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$UNPASTE_VIRTUAL_DISPLAY(DISP1,PB)
        ST = SMG$UNPASTE_VIRTUAL_DISPLAY(DISP2,PB)
        ST = SMG$UNPASTE_VIRTUAL_DISPLAY(DISP3,PB)
C
        ST = SMG$DELETE_VIRTUAL_DISPLAY(DISP1,PB)
        ST = SMG$DELETE_VIRTUAL_DISPLAY(DISP2,PB)
        ST = SMG$DELETE_VIRTUAL_DISPLAY(DISP3,PB)
        ST = SMG$DELETE_VIRTUAL_DISPLAY(DISPMENU,PB)
C
        ST = SMG$DELETE_PASTEBOARD(PB)
        CALL GSTOP(GEXIT_SUCCESS)
        END
C
C*******************************************************************************
C       SUBROUTINE TO CONFIGURE M1LHAO GAME
C*******************************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE RAF2CONF(PB, KEYB, DISP1, DISP2, DISP3)
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
        INCLUDE 'INCLIB:RECRAF2CF.DEF'
        INCLUDE 'INCLIB:BLDEURSYS.DEF'
C
C LOCAL VARIABLES
C
        INTEGER*4 ST
C
        INTEGER*4 TAMMENU
        PARAMETER (TAMMENU = 4)
        CHARACTER*78  CMSG
        CHARACTER*18  MYMENU(TAMMENU) /'[    MOSTRAR     ]',
     *                                 '[    ALTERAR     ]',
     *                                 '[ CRIAR FICHEIRO ]',
     *                                 '[     VOLTAR     ]'/
C
        INTEGER PB,                                                             !PASTEBOARD
     *          KEYB,                                                           !VIRTUAL KEYBOARD
     *          DISP1,                                                          !HEADER FRAME WINDOW
     *          DISP2,                                                          !OUTPUT DATA FRAME WINDOW
     *          DISP3,                                                          !MESSAGE FRAME WINDOW
     *	        DISPMENU                                                        !INITIAL MENU

        INTEGER DEFAULT/1/
        INTEGER*4 MENUOPT
C
C       CREATES MENU
C
        ST = SMG$CREATE_VIRTUAL_DISPLAY(10, 20, DISPMENU)
        ST = SMG$CREATE_MENU(DISPMENU, MYMENU, SMG$K_VERTICAL,
     *                       SMG$M_DOUBLE_SPACE + SMG$M_WRAP_MENU ,
     *                       1, 0)
C
        ST = SMG$SET_CURSOR_MODE (PB, SMG$M_CURSOR_OFF )
        ST = SMG$ERASE_DISPLAY (DISP1)
        ST = SMG$ERASE_DISPLAY (DISP2)
        ST = SMG$ERASE_DISPLAY (DISP3)
C
        MENUOPT = 0
        DO WHILE(MENUOPT .NE. TAMMENU)
          ST = SMG$SET_CURSOR_MODE (DISP2, SMG$M_CURSOR_OFF )
          ST = SMG$LABEL_BORDER (DISP2,' Configuração ',
     *                           SMG$K_TOP, 65, SMG$M_REVERSE)
C
          CMSG = 'CONFIGURAÇÃO DO JOGO M1LHÃO'
          ST   = SMG$PUT_CHARS(DISP1, CMSG, 1, 26)
          ST   = SMG$PUT_CHARS(DISP1, BLDEURSYS_VERSION, 1, 66)
C
          CMSG = ' Use setas para movimentar cursor e <ENTER> para'//
     *           ' escolher '
          ST = SMG$PUT_CHARS(DISP3, CMSG, 1, 1)
          ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)
          ST = SMG$PASTE_VIRTUAL_DISPLAY(DISPMENU, PB, 9, 31)                   !LINE 9 COLUMUN 31
          ST = SMG$SELECT_FROM_MENU(KEYB, DISPMENU, MENUOPT,
     *                              DEFAULT,,,,,,SMG$M_BOLD)
          ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)
          ST = SMG$UNPASTE_VIRTUAL_DISPLAY (DISPMENU,PB)
C
         IF(MENUOPT.EQ.1) THEN
           CALL DSP_RAF2CONF(DISP2, DISP3, KEYB, PB)
         ELSEIF (MENUOPT.EQ.2) THEN
           CALL UPD_RAF2CONF(DISP2, DISP3, KEYB, PB)
         ELSEIF (MENUOPT.EQ.3) THEN
           CALL CRT_RAF2CONF(DISP2, DISP3, KEYB, PB)
         ENDIF
        ENDDO
        RETURN
        END
C
C*******************************************************************************
C       SUBROUTINE DSP_RAF2CONF (DISPLAYS DATA FROM RAF2CF.FIL)
C*******************************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE DSP_RAF2CONF(DISP2, DISP3, KEYB, PB)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECRAF2CF.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
C
        INTEGER*4 ST
        INTEGER*4 RAF2FDB(7)
        LOGICAL   ONDISK
        INTEGER*4 DSP2LN
C
        INTEGER PB,                                                             !PASTE BOARD
     *          KEYB,                                                           !VIRTUAL KEYBOARD
     *          DISP2,                                                          !OUTPUT DATA FRAME WINDOW
     *          DISP3                                                           !MESSAGE FRAME WINDOW
C
        CHARACTER*78  CMSG, CMSG4
        CHARACTER*39  CMSG1
        CHARACTER*16  CMSG2
        CHARACTER*20  CMSG3
        INTEGER KEY
        INTEGER*4     IND, I, J
C
        RAF2CF_FREESPC(1) = 0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$ERASE_DISPLAY(DISP2)
        CMSG = 'A ler as configurações registadas. '//
     *         'Aguarde, por favor...'
        ST = SMG$PUT_CHARS(DISP3, CMSG, 1, 1, SMG$M_ERASE_LINE)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       CHECK IF CONFIGURATION FILE EXISTS IN THE SYSTEM
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        INQUIRE(FILE=TRIM(CRAF2CFNAM), EXIST=ONDISK)
        IF(.NOT. ONDISK) THEN
          CMSG = ' O ficheiro '//TRIM(CRAF2CFNAM)//
     *           ' não existe no sistema MILLENNIUM...'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       OPEN THE FILE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CALL OPENX(1,TRIM(CRAF2CFNAM),4,0,0,ST)
        CALL IOINIT(RAF2FDB,1,RAF2CF_SEC*256)
        IF(ST.NE.0) THEN
          CALL CLOSEFIL(RAF2FDB)                                                !CLOSE THE FILE
          CMSG = ' Erro ao abrir o ficheiro '//TRIM(CRAF2CFNAM)//'...'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       READ THE FILE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CALL READW(RAF2FDB,1,RAF2CF_REC,ST)
        IF(ST.NE.0) THEN
          CALL CLOSEFIL(RAF2FDB)                                                !CLOSE THE FILE
          CMSG = ' Erro ao ler o ficheiro '//TRIM(CRAF2CFNAM)//'...'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       DISPLAY THE FILE CONTENTS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        CMSG = 'Para voltar ao menu anterior prima qualquer tecla...'
        ST = SMG$PUT_CHARS(DISP3, CMSG, 1, 2, SMG$M_ERASE_LINE)
C
        CMSG = 'ESTADO DO JOGO......................................:'          !ESTADO DO JOGO SM
        IF (RAF2CF_GACTIVE.EQ.0) THEN
          CMSG4 = 'DESATIVADO'
        ELSE
          CMSG4 = 'ATIVADO'
        ENDIF
        DSP2LN = 1
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN,  2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG4, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'REVISÃO DO JOGO.....................................:'          !SM GAME REVISION
        DSP2LN = DSP2LN + 1
        WRITE(CMSG3, 100) RAF2CF_CTRLREV
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG3, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'TIPO/ÍNDICE DO JOGO.................................:'          !SM GAME TYPE/INDEX
        DSP2LN = DSP2LN + 1
        WRITE(CMSG3, 102) RAF2CF_GAMETYP, RAF2CF_GAMEIND
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG3, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'NÚMERO DO JOGO (EXTERNO)............................:'          !SM EXTERNAL GAME NUMBER
        DSP2LN = DSP2LN + 1
        WRITE(CMSG3, 101) RAF2CF_XGAMNUM
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG3, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'PREÇO DA APOSTA (CÊNTIMOS DE EURO)..................:'          !SM GAME BASE PRICE
        DSP2LN = DSP2LN + 1
        WRITE(CMSG3, 101) RAF2CF_GBPRICE
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG3, DSP2LN, 56,, SMG$M_BOLD)
C
!        CMSG = 'NIF OBRIGATÓRIO P/ REGISTO DE APOSTAS ?.............:'          !PLAYER NIF REQUIRED FLAG
!        DSP2LN = DSP2LN + 1
!        IF (RAF2CF_PNIFREQ.EQ.0) THEN
!          CMSG4 = 'NÃO'
!        ELSE
!          CMSG4 = 'SIM'
!        ENDIF
!        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
!        ST = SMG$PUT_CHARS(DISP2, CMSG4, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'MULTI-SEMANAS (BITMAP)..............................:'          !MULTI-DRAW BITMAP
        DSP2LN = DSP2LN + 1
        WRITE(CMSG4, '(<EMAXDBM>(1X,Z2.2))')
     *                (RAF2CF_DBITMAP(I),I=1,EMAXDBM)
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG4, DSP2LN, 55,, SMG$M_BOLD)
C
        CMSG = '# MÍN. DE APOSTAS C/ REPRESENTAÇÃO EM INTERVALO.....:'          !MINIMUM NUMBER OF RAFFLES FROM WHICH THE RAFFLE NUMBERS HAVE TO BE PRINTED IN THE WAGER TICKET USING INTERVAL MODE REPRESENTATION (IMR)
        DSP2LN = DSP2LN + 1
        WRITE(CMSG3, 101) RAF2CF_IMRBEGI
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG3, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'JOGO STANDALONE ?...................................:'          !SM GAME STANDALONE FLAG
        DSP2LN = DSP2LN + 1
        IF (RAF2CF_SMACTIV.EQ.0) THEN
          CMSG4 = 'NÃO'
        ELSE
          CMSG4 = 'SIM'
        ENDIF
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG4, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'NIF OBRIGATÓRIO P/ REGISTO DE APOSTAS (standalone) ?:'          !PLAYER NIF REQUIRED FLAG (STANDALONE ONLY)
        DSP2LN = DSP2LN + 1
        IF (RAF2CF_PNIFREQ.EQ.0) THEN
          CMSG4 = 'NÃO'
        ELSE
          CMSG4 = 'SIM'
        ENDIF
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG4, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = '# MÁX. DE APOSTAS (standalone)......................:'          !MAXIMUM BET LIMIT (STANDALONE ONLY)
        DSP2LN = DSP2LN + 1
        WRITE(CMSG3, 101) RAF2CF_MAXBETS
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG3, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'NOME ABREVIADO DO JOGO..............................:'          !SM SHORT GAME NAME
        DSP2LN = DSP2LN + 1
        WRITE(CMSG4, '(A4)') RAF2CF_SHGNAME
        DO J=1,4
          IF(CMSG4(J:J).EQ.CHAR(0)) CMSG4(J:J) = ' '
        ENDDO
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG4, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'NOME LONGO DO JOGO..................................:'          !SM LONG GAME NAME
        DSP2LN = DSP2LN + 1
        WRITE(CMSG4, '(5A4)') (RAF2CF_LNGNAME(I),I=1,5)
        DO I=1,20
          IF(CMSG4(I:I).EQ.CHAR(0)) CMSG4(I:I) = ' '
        ENDDO
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG4, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'NOME DO FICHEIRO DE CONFIGURAÇÃO....................:'          !SM GAME CONFIGURATION FILENAME
        DSP2LN = DSP2LN + 1
        WRITE(CMSG4, '(5A4)') (RAF2CF_FILNAME(I),I=1,5)
        DO I=1,20
          IF(CMSG4(I:I).EQ.CHAR(0)) CMSG4(I:I) = ' '
        ENDDO
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG4, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'OPTION FLAGS DO JOGO NO SIGN-ON.....................:'          !GAME OPTION FLAGS AT SON
        DSP2LN = DSP2LN + 2
        WRITE(CMSG3, 100) RAF2CF_GOPTSON
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG3, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'OPTION FLAGS DO JOGO NO CONTROL REQUEST.............:'          !GAME OPTION FLAGS AT CONTROL REQUEST
        DSP2LN = DSP2LN + 1
        WRITE(CMSG3, 100) RAF2CF_GOPTCTR
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG3, DSP2LN, 56,, SMG$M_BOLD)
C
        ST = SMG$READ_KEYSTROKE(KEYB, KEY, , DISP3)
        ST = SMG$ERASE_DISPLAY(DISP2)
C
        CALL CLOSEFIL(RAF2FDB)
C
        RETURN
100     FORMAT (Z4.4)
101     FORMAT (I0)
102     FORMAT (I0,' / ', I0)
        END
C
C*******************************************************************************
C       SUBROUTINE UPD_RAF2CONF (UPDATE DATA IN FILE)
C*******************************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE UPD_RAF2CONF(DISP2, DISP3, KEYB, PB)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECRAF2CF.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
        INCLUDE 'INCLIB:BLDEURSYS.DEF'
C
        INTEGER*4 ST
        INTEGER*4 RAF2FDB(7)
        LOGICAL   ONDISK
        LOGICAL   CHANGED
C
        INTEGER PB,                                                             !PASTE BOARD
     *          KEYB,                                                           !VIRTUAL KEYBOARD
     *          DISP2,                                                          !OUTPUT DATA FRAME WINDOW
     *          DISP3                                                           !MESSAGE FRAME WINDOW
C
        CHARACTER*78  CMSG, CMSG4
        CHARACTER*20  CMSG1
        CHARACTER*30  CMSG2
        CHARACTER*5   CMSG3

        INTEGER       KEY, DEFAULT
        INTEGER*4     I, J, K, M, ACTVAL, NEWVAL
        INTEGER*4     USRVAL                                                    !USER ENTERED VALUE
        INTEGER*4     DSP2LN
        INTEGER*4     MDSTAB(MAXMLTD_AVL)                                       !MULTI-DRAW SELECTED TABLE
C
        INTEGER*4     I4USRVAL
        CHARACTER*4   C4USRVAL
        EQUIVALENCE  (I4USRVAL, C4USRVAL)
C
        INTEGER*4     I4USRVAL5(5), I4ACTVAL5(5), I4NEWVAL5(5)
        CHARACTER*20  C20USRVAL, C20ACTVAL, C20NEWVAL
        EQUIVALENCE  (I4USRVAL5, C20USRVAL)
        EQUIVALENCE  (I4ACTVAL5, C20ACTVAL)
        EQUIVALENCE  (I4NEWVAL5, C20NEWVAL)
C
        CHARACTER*14  C14DRWVAL

        INTEGER*2     I2USRVAL
        CHARACTER*2   C2USRVAL
        EQUIVALENCE  (I2USRVAL,C2USRVAL)
C
        CHANGED = .FALSE.
        RAF2CF_FREESPC(1) = 0
C
        ST = SMG$ERASE_DISPLAY(DISP2)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       CHECK IF CONFIGURATION FILE EXISTS IN THE SYSTEM
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        INQUIRE(FILE=TRIM(CRAF2CFNAM), EXIST=ONDISK)
        IF(.NOT. ONDISK) THEN
          CMSG = ' O ficheiro '//TRIM(CRAF2CFNAM)//
     *           ' não existe no sistema MILLENNIUM...'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       ASK THE USER IF HE WANTS TO CHANGE THE GAME CONFIGURATION FILE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$ERASE_DISPLAY(DISP2)
        CMSG = 'Deseja alterar as configurações do jogo M1LHÃO ?'
        DEFAULT = 2
        CALL MENU_CONFIRM(TRIM(CMSG), 8, 4, 3, 58, DISP2, KEYB, PB,
     *                    DEFAULT, ST)
        IF(ST.NE.0) THEN
          ST = SMG$ERASE_DISPLAY(DISP2)
          RETURN
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       OPEN THE FILE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        CALL OPENX(1,TRIM(CRAF2CFNAM),4,0,0,ST)
        CALL IOINIT(RAF2FDB,1,RAF2CF_SEC*256)
        IF(ST.NE.0) THEN
          CMSG = ' Erro ao abrir o ficheiro '//TRIM(CRAF2CFNAM)//'...'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          CALL CLOSEFIL(RAF2FDB)                                                !CLOSE THE FILE
          RETURN
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       READ THE FILE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CALL READW(RAF2FDB,1,RAF2CF_REC,ST)
        IF(ST.NE.0) THEN
          CMSG = ' Erro ao ler o ficheiro '//TRIM(CRAF2CFNAM)//'...'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          CALL CLOSEFIL(RAF2FDB)                                                !CLOSE THE FILE
          RETURN
        ENDIF
C
        CALL CLOSEFIL(RAF2FDB)                                                  !CLOSE THE FILE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       ACTIVATE/DEACTIVATE GAME
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$ERASE_DISPLAY(DISP2)
        ST = SMG$PUT_CHARS(DISP3, DSP3_MENU_CONFIRM_MSG, 1, 2)
        ACTVAL = RAF2CF_GACTIVE
        IF(ACTVAL.EQ.0) THEN
          CMSG = 'Deseja ATIVAR o jogo M1LHÃO ?'
        ELSE
          CMSG = 'Deseja DESATIVAR o jogo M1LHÃO ?'
        ENDIF
        DSP2LN  = 1
        DEFAULT = 2
        CALL MENU_CONFIRM(TRIM(CMSG), DSP2LN, 4, 3, 55, DISP2, KEYB,
     *       PB, DEFAULT, ST)
        IF(ST.EQ.-2) RETURN
        IF(ST.EQ.0) THEN                                                        !USER HAS CHOOSEN "Sim"
          NEWVAL = 1 - ACTVAL                                                   !TOGGLE VALUE OF ACTVAL
          ST = SMG$PUT_CHARS(DISP2, 'SIM', DSP2LN, 59,, SMG$M_BOLD)
          RAF2CF_GACTIVE = NEWVAL
          CHANGED = .TRUE.
          ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2)
        ELSE
          ST = SMG$PUT_CHARS(DISP2, 'NÃO', DSP2LN, 59,, SMG$M_BOLD)
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       MINIMUM NUMBER OF RAFFLES FROM WHICH THE RAFFLE NUMBERS HAVE TO BE
C       PRINTED IN THE WAGER TICKET USING INTERVAL MODE REPRESENTATION (IMR)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC)
        ST = SMG$ERASE_DISPLAY(DISP3)
        DSP2LN = DSP2LN + 1
        CMSG = 'Digite o núm. mín. de apostas c/ repr. em inter.'
        ST = SMG$PUT_CHARS(DISP2, CMSG, DSP2LN, 3)
        ACTVAL = RAF2CF_IMRBEGI
        USRVAL = ACTVAL
        CALL SMG_INPNUM(PB, KEYB, DISP2, DISP3, USRVAL, DSP2LN,
     *                  59, 3, 1, 255, ST)
        IF(ST.EQ.-2) RETURN
        NEWVAL = USRVAL
        IF(ACTVAL.NE.NEWVAL) THEN
          RAF2CF_IMRBEGI = NEWVAL
          CHANGED = .TRUE.
          ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2)
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SM GAME BASE PRICE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$ERASE_DISPLAY(DISP3)
        DSP2LN =  DSP2LN + 1
        CMSG = 'Digite o preço da aposta (cêntimos de euro)'
        ST = SMG$PUT_CHARS(DISP2, CMSG, DSP2LN, 3)
        ACTVAL = RAF2CF_GBPRICE
        USRVAL = ACTVAL
        CALL SMG_INPNUM(PB, KEYB, DISP2, DISP3, USRVAL, DSP2LN,
     *                  59, 5, 1, 65635, ST)
        IF(ST.EQ.-2) RETURN
        NEWVAL = USRVAL
        IF(ACTVAL.NE.NEWVAL) THEN
          RAF2CF_GBPRICE = NEWVAL
          CHANGED = .TRUE.
          ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2)
        ENDIF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C         PLAYER NIF REQUIRED FOR WAGERING?
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!          DSP2LN = DSP2LN + 1
!          CMSG = 'NIF obrigatório p/ registo de apostas ?'
!          DEFAULT = 2
!          ACTVAL = RAF2CF_PNIFREQ
!          IF(ACTVAL.EQ.1) DEFAULT = 1
!          CALL MENU_CONFIRM(TRIM(CMSG), DSP2LN, 4, 3, 55,
!     *                      DISP2, KEYB, PB, DEFAULT, ST)
!          IF(ST.EQ.0) THEN
!            NEWVAL = 1
!            ST = SMG$PUT_CHARS(DISP2, 'SIM', DSP2LN, 59,, SMG$M_BOLD)
!            IF(ACTVAL.NE.NEWVAL) THEN
!              RAF2CF_PNIFREQ = NEWVAL
!              RAF2CF_GOPTCTR = IOR(RAF2CF_GOPTCTR,PLANIF_GOPT)
!              CHANGED = .TRUE.
!              ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2,, SMG$M_BOLD)
!            ENDIF
!          ELSE
!            NEWVAL = 0
!            ST = SMG$PUT_CHARS(DISP2, 'NÃO', DSP2LN, 59,, SMG$M_BOLD)
!            IF(ACTVAL.NE.NEWVAL) THEN
!              RAF2CF_PNIFREQ = NEWVAL
!              RAF2CF_GOPTCTR = IAND(RAF2CF_GOPTCTR,.NOT.PLANIF_GOPT)
!              CHANGED = .TRUE.
!              ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2,, SMG$M_BOLD)
!            ENDIF
!          ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SM STANDALONE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$PUT_CHARS(DISP3, DSP3_MENU_CONFIRM_MSG, 1, 2)
        DSP2LN = DSP2LN + 1
        CMSG = 'Jogo standalone ?'
        DEFAULT = 2
        ACTVAL = RAF2CF_SMACTIV
        IF(ACTVAL.EQ.1) DEFAULT = 1
        CALL MENU_CONFIRM(CMSG, DSP2LN, 4, 3, 55, DISP2, KEYB, PB,
     *                    DEFAULT, ST)
        IF(ST.EQ.0) THEN
          NEWVAL = 1
          ST = SMG$PUT_CHARS(DISP2, 'SIM', DSP2LN, 59,, SMG$M_BOLD)
          IF(ACTVAL.NE.NEWVAL) THEN
            RAF2CF_SMACTIV = NEWVAL
            RAF2CF_GOPTCTR = IOR(RAF2CF_GOPTCTR,SMACTV_GOPT)
            CHANGED = .TRUE.
            ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2,, SMG$M_BOLD)
          ENDIF
C         PLAYER NIF REQUIRED FOR WAGERING?
          DSP2LN = DSP2LN + 1
          CMSG = 'NIF obrigatório p/ registo de apostas (standalone) ?'
          DEFAULT = 2
          ACTVAL = RAF2CF_PNIFREQ
          IF(ACTVAL.EQ.1) DEFAULT = 1
          CALL MENU_CONFIRM(TRIM(CMSG), DSP2LN, 4, 3, 55,
     *                      DISP2, KEYB, PB, DEFAULT, ST)
          IF(ST.EQ.0) THEN
            NEWVAL = 1
            ST = SMG$PUT_CHARS(DISP2, 'SIM', DSP2LN, 59,, SMG$M_BOLD)
            IF(ACTVAL.NE.NEWVAL) THEN
              RAF2CF_PNIFREQ = NEWVAL
              RAF2CF_GOPTCTR = IOR(RAF2CF_GOPTCTR,PLANIF_GOPT)
              CHANGED = .TRUE.
              ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2,, SMG$M_BOLD)
            ENDIF
          ELSE
            NEWVAL = 0
            ST = SMG$PUT_CHARS(DISP2, 'NÃO', DSP2LN, 59,, SMG$M_BOLD)
            IF(ACTVAL.NE.NEWVAL) THEN
              RAF2CF_PNIFREQ = NEWVAL
              RAF2CF_GOPTCTR = IAND(RAF2CF_GOPTCTR,.NOT.PLANIF_GOPT)
              CHANGED = .TRUE.
              ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2,, SMG$M_BOLD)
            ENDIF
          ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C         MAXIMUM BET LIMIT - IF STANDALONE GAME
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          ST = SMG$ERASE_DISPLAY(DISP3)
          DSP2LN = DSP2LN + 1
          CMSG = 'Digite o número máximo de apostas (standalone)'
          ST = SMG$PUT_CHARS(DISP2, CMSG, DSP2LN, 3)
          ACTVAL = RAF2CF_MAXBETS
          USRVAL = ACTVAL
          CALL SMG_INPNUM(PB, KEYB, DISP2, DISP3, USRVAL, DSP2LN,
     *                    59, 5, 1, 65635, ST)
          IF(ST.EQ.-2) RETURN
          NEWVAL = USRVAL
          IF(ACTVAL.NE.NEWVAL) THEN
            RAF2CF_MAXBETS = NEWVAL
            CHANGED = .TRUE.
            ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2)
          ENDIF
!          RAF2CF_GOPTCTR = IOR(RAF2CF_GOPTCTR,BETLIM_GOPT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C         MULTIDRAW - IF STANDALONE GAME
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          ST = SMG$PUT_CHARS(DISP3, DSP3_MENU_CONFIRM_MSG, 1, 2)
          DSP2LN = DSP2LN + 1
          CMSG = 'Deseja alterar as apostas multi-semana ?'
          DEFAULT = 2                                                           ! "Não"
          CALL MENU_CONFIRM(TRIM(CMSG), DSP2LN, 4, 3, 55,
     *                      DISP2, KEYB, PB, DEFAULT, ST)
          IF(ST.EQ.0) THEN
            ST = SMG$ERASE_DISPLAY(DISP3)
            CMSG = 'Digite as apostas multi semana'
            ST = SMG$PUT_CHARS(DISP2, CMSG, DSP2LN, 3)
            C14DRWVAL = '              '
            CALL FASTSET(0, MDSTAB, MAXMLTD_AVL)
            MDSTAB(1) = 1                                                       !SINGLE WEEK WAGERING ALWAYS ALLOWED
            M = 1
            DO I=1,EMAXDBM
              DO J=0,7
                IF(TSTBIT_BSTRNG(RAF2CF_DBITMAP(I),J)) THEN
                  MDSTAB(M) = (I-1)*8+J+1
                  M = M + 1
                ENDIF
              ENDDO
            ENDDO
C
            DO I=2,MAXMLTD_AVL                                                  !SKIP I = 1
              CMSG = 'Digite 0 ou limpe o campo seguido da tecla '//
     *               '<ENTER> para finalizar'
              ST = SMG$PUT_CHARS(DISP3, CMSG, 1, 2)
              ACTVAL = MDSTAB(I)
              USRVAL = ACTVAL
              CALL MY_SMG_INPNUM(PB, KEYB, DISP2, DISP3, USRVAL, DSP2LN,
     *                           74, 2, MDSTAB(I-1)+1, MAXMLTD_AVL, ST)
              IF(ST.EQ.-2) RETURN
C
              WRITE(C2USRVAL,'(I2.2)') USRVAL
              J=LEN_TRIM(C14DRWVAL)
              IF(USRVAL.GT.0 .AND.
     *           USRVAL.LT.MAXMLTD_AVL .AND.
     *           I.LT.MAXMLTD_AVL) THEN
C
                IF(J.EQ.12) THEN
                  C14DRWVAL(1:J) = C14DRWVAL(4:J)//C2USRVAL//','                !LEFT SHIFT
                  ST = SMG$PUT_CHARS(DISP2, '...'//TRIM(C14DRWVAL),
     *                               DSP2LN,
     *                               71-LEN_TRIM(C14DRWVAL)
     *                               ,,SMG$M_REVERSE+SMG$M_BOLD)
                ELSE
                  C14DRWVAL(J+1:J+3) = C2USRVAL//','
                  ST = SMG$PUT_CHARS(DISP2, TRIM(C14DRWVAL), DSP2LN,
     *                               73-LEN_TRIM(C14DRWVAL)+1
     *                               ,,SMG$M_REVERSE+SMG$M_BOLD)
                ENDIF

                NEWVAL = USRVAL
                IF(ACTVAL.NE.NEWVAL) THEN
                  MDSTAB(I) = NEWVAL
                  CHANGED = .TRUE.
                  ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2)
                ENDIF
                MDSTAB(I) = NEWVAL
              ELSEIF(USRVAL.EQ.0 .OR.
     *               USRVAL.EQ.MAXMLTD_AVL .OR.
     *               I.EQ.MAXMLTD_AVL) THEN                                     !STOP CONDITION
                IF(USRVAL.EQ.0) THEN
                  IF(I.EQ.2) THEN
                    C14DRWVAL = 'NÃO DEFINIDO  '
                  ELSE
                    C14DRWVAL(LEN_TRIM(C14DRWVAL):LEN_TRIM(C14DRWVAL))
     *                                                             = ' '        !REMOVE LAST COMMA
                  ENDIF
                  ST = SMG$PUT_CHARS(DISP2, '                 ',
     *                               DSP2LN, 59)                                !CLEAN FIRST
                  IF(J.EQ.12) THEN
                    ST = SMG$PUT_CHARS(DISP2, '...'//TRIM(C14DRWVAL),
     *                                 DSP2LN,
     *                                 59
     *                                 ,,SMG$M_REVERSE+SMG$M_BOLD)
                  ELSE
                    ST = SMG$PUT_CHARS(DISP2, TRIM(C14DRWVAL), DSP2LN,
     *                                 59
     *                                 ,,SMG$M_REVERSE+SMG$M_BOLD)
                  ENDIF
                ELSEIF(USRVAL.EQ.MAXMLTD_AVL .OR. I.EQ.MAXMLTD_AVL) THEN
                  C14DRWVAL(J+1:J+3) = C2USRVAL                                 !SET LAST USER VALUE
                  ST = SMG$PUT_CHARS(DISP2, '                 ',
     *                               DSP2LN, 59)                                !CLEAN FIRST
                  IF(J.EQ.12) THEN
                    ST = SMG$PUT_CHARS(DISP2, '...'//TRIM(C14DRWVAL),
     *                                 DSP2LN,
     *                                 59
     *                                 ,,SMG$M_REVERSE+SMG$M_BOLD)
                  ELSE
                    ST = SMG$PUT_CHARS(DISP2, TRIM(C14DRWVAL), DSP2LN,
     *                                 59
     *                                 ,,SMG$M_REVERSE+SMG$M_BOLD)
                  ENDIF
                ENDIF
                NEWVAL = USRVAL
                IF(ACTVAL.NE.NEWVAL) THEN
                  MDSTAB(I) = NEWVAL
                  CHANGED = .TRUE.
                  ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2)
                ENDIF
                DO J=I+1,MAXMLTD_AVL
                  MDSTAB(J) = 0                                                 !REMOVE MULTIDRAW OF REMAINING
                ENDDO
                EXIT
              ENDIF
            ENDDO
C
            DO I = 1, EMAXDBM                                                   !CLEAR MULTIDRAW BITMAP
              RAF2CF_DBITMAP(I) = 0
            END DO
            DO M = 1, MAXMLTD_AVL
              IF(MDSTAB(M).GT.0) THEN
                K = (MDSTAB(M)-1) / 8 + 1                                       !K=1,2,3,4,5,6,7
                J = MOD(MDSTAB(M)-1,8)                                          !J=0,1,2,3,4,5,6,7
                CALL SETBIT_BSTRNG(RAF2CF_DBITMAP(K),J)                         !UPDATE RAF2CF_DBITMAP
              ENDIF
            ENDDO
          ELSE
            ST = SMG$PUT_CHARS(DISP2, 'NÃO', DSP2LN, 59,, SMG$M_BOLD)
          ENDIF
        ELSE                                                                    !SM GAME IS NOT STANDALONE
          NEWVAL = 0
          ST = SMG$PUT_CHARS(DISP2, 'NÃO', DSP2LN, 59,, SMG$M_BOLD)
          IF(ACTVAL.NE.NEWVAL) THEN
            RAF2CF_SMACTIV = NEWVAL
            RAF2CF_GOPTCTR = IAND(RAF2CF_GOPTCTR,.NOT.SMACTV_GOPT)
            RAF2CF_GOPTCTR = IAND(RAF2CF_GOPTCTR,.NOT.PLANIF_GOPT)
!            RAF2CF_GOPTCTR = IAND(RAF2CF_GOPTCTR,.NOT.BETLIM_GOPT)
            CHANGED = .TRUE.
            ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2,, SMG$M_BOLD)
          ENDIF
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SM SHORT GAME NAME
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$ERASE_DISPLAY(DISP3)
        DSP2LN = DSP2LN + 1
        CMSG = 'Digite o nome abreviado do jogo'
        ST = SMG$PUT_CHARS (DISP2, CMSG, DSP2LN, 3)
        ACTVAL = RAF2CF_SHGNAME
        WRITE(C4USRVAL,'(A4)') ACTVAL
        CALL SMG_INPTEXT(PB,KEYB,DISP2,DISP3,C4USRVAL,1,
     *                   DSP2LN,59,4,1,ST)
        IF(ST.EQ.-2) RETURN
        NEWVAL = I4USRVAL
        IF(ACTVAL.NE.NEWVAL) THEN
          RAF2CF_SHGNAME = NEWVAL
          CHANGED = .TRUE.
          ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2,, SMG$M_BOLD)
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SM LONG GAME NAME
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$ERASE_DISPLAY(DISP3)
        DSP2LN = DSP2LN + 1
        CMSG = 'Digite o nome longo do jogo'
        ST = SMG$PUT_CHARS (DISP2, CMSG, DSP2LN, 3)
        CALL FASTMOV(RAF2CF_LNGNAME,I4ACTVAL5,5)
        WRITE(C20USRVAL,'(5A4)') I4ACTVAL5
        CALL SMG_INPTEXT(PB,KEYB,DISP2,DISP3,C20USRVAL,1,
     *                   DSP2LN,59,20,1,ST)
        IF(ST.EQ.-2) RETURN
        C20USRVAL = ADJUSTL(C20USRVAL)
        CALL FASTMOV(I4USRVAL5,I4NEWVAL5,5)
        ST = SMG$PUT_CHARS(DISP2, C20USRVAL, DSP2LN, 59,,
     *                     SMG$M_REVERSE, SMG$M_BOLD)
        IF(C20ACTVAL.NE.C20NEWVAL) THEN
          CALL FASTMOV(I4NEWVAL5,RAF2CF_LNGNAME,5)
          CHANGED = .TRUE.
          ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2,, SMG$M_BOLD)
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SAVE THE CHANGES IN FILE IF CHANGES HAVE BEEN MADE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        IF(CHANGED) THEN
          CALL OPENX(1,TRIM(CRAF2CFNAM),4,0,0,ST)                               !OPEN THE FILE
          CALL IOINIT(RAF2FDB,1,RAF2CF_SEC*256)
          IF(ST.NE.0) THEN
            CALL CLOSEFIL(RAF2FDB)                                              !CLOSE THE FILE
            CMSG = ' Erro ao abrir o ficheiro '//TRIM(CRAF2CFNAM)//'...'
            CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
            RETURN
          ENDIF
          ST = SMG$PUT_CHARS (DISP3, DSP3_MENU_CONFIRM_MSG, 1, 2)
          DSP2LN = DSP2LN + 4
          CMSG = '*DESEJA GRAVAR AS ALTERAÇÕES EFETUADAS ?'
          DEFAULT = 2
          CALL MENU_CONFIRM(TRIM(CMSG), DSP2LN, 4, 2, 56,
     *                      DISP2, KEYB, PB, DEFAULT, ST)
          IF(ST.EQ.0) THEN
            RAF2CF_CTRLREV = RAF2CF_CTRLREV + 1                                 !SET NEW REVISION NUMBER
            CALL WRITEW(RAF2FDB,1,RAF2CF_REC,ST)
            IF(ST.NE.0) THEN
              CALL CLOSEFIL(RAF2FDB)                                            !CLOSE THE FILE
              CMSG = ' Erro ao gravar as alterações no ficheiro '//
     *               TRIM(CRAF2CFNAM)//'...'
              CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
              RETURN
            ENDIF
            CMSG = 'SUCESSO'
            ST = SMG$PUT_CHARS(DISP2, TRIM(CMSG), DSP2LN, 59,,
     *                         SMG$M_REVERSE + SMG$M_BOLD)
            CMSG = ' Para voltar ao menu anterior prima'//
     *             ' qualquer tecla...'
            CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          ENDIF
        ELSE
          CALL CLOSEFIL(RAF2FDB)
          DSP2LN = DSP2LN + 4
          CMSG = 'NÃO EXISTEM ALTERAÇÕES A GRAVAR.'
          ST = SMG$PUT_CHARS(DISP2, CMSG, DSP2LN, 3,, SMG$M_BOLD)
          CMSG = ' Para voltar ao menu anterior prima qualquer tecla...'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
        ENDIF
C
        ST = SMG$ERASE_DISPLAY(DISP2)
C
        RETURN
100     FORMAT (Z2.2)
        END
C
C*******************************************************************************
C       SUBROUTINE CRT_RAF2CONF (CREATES RAF2CF.FIL GAME FILE DATA
C*******************************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CRT_RAF2CONF(DISP2, DISP3, KEYB, PB)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECRAF2CF.DEF'
        INCLUDE 'INCLIB:BLDEURSYS.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
C
        INTEGER*4 ST, SECTORS, KEY, I
        INTEGER*4 RAF2FDB(7)
        LOGICAL ONDISK
C
        INTEGER PB,                                                             !PASTE BOARD
     *          KEYB,                                                           !VIRTUAL KEYBOARD
     *          DISP2,                                                          !OUTPUT DATA FRAME WINDOW
     *          DISP3                                                           !MESSAGE FRAME WINDOW
C
        CHARACTER*78 CMSG
        CHARACTER*30 CMSG1
C
        RAF2CF_FREESPC(1) = 0
C
        ST = SMG$ERASE_DISPLAY(DISP2)
        CMSG = 'O utilizador vai criar o ficheiro '//
     *          TRIM(CRAF2CFNAM)//'...'
        ST = SMG$PUT_CHARS(DISP3, CMSG, 1, 2, SMG$M_ERASE_LINE)
C
        INQUIRE(FILE=TRIM(CRAF2CFNAM), EXIST = ONDISK)
        IF(ONDISK) THEN
          CMSG = ' O ficheiro '//TRIM(CRAF2CFNAM)//
     *           ' já existe no sistema MILLENNIUM'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
        RETURN
        ENDIF
        CMSG = 'CRIAÇÃO DO FICHEIRO DE CONFIGURAÇÃO DO JOGO M1LHÃO'
        ST = SMG$PUT_CHARS(DISP2, TRIM(CMSG), 2, 2,, SMG$M_UNDERLINE)

        CMSG1 = 'Digite o número de blocos: '
        ST = SMG$PUT_CHARS(DISP2, CMSG1, 5, 2,,)
        CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,SECTORS,5,34,3,200,999,ST)
C
        WRITE(CMSG,100) TRIM(CRAF2CFNAM), SECTORS
        ST = SMG$PUT_CHARS(DISP2, CMSG, 8, 2,,)
C
        CALL SMG_NEWFIL(7,TRIM(CRAF2CFNAM), SECTORS, ST)
        IF(ST .NE. 0) THEN
          CMSG = 'ERRO: o ficheiro '//TRIM(CRAF2CFNAM)//
     *           ' não foi criado.'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
C
        ST = SMG$PUT_CHARS(DISP2, ' SUCESSO ', 8, 56,,
     *                     SMG$M_REVERSE + SMG$M_BOLD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SET BASELINE VALUES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CMSG = 'A configurar as definições baseline do jogo...'
        ST = SMG$PUT_CHARS(DISP2, TRIM(CMSG), 10, 2,,)
        CALL OPENX(1,TRIM(CRAF2CFNAM),4,0,0,ST)
        CALL IOINIT(RAF2FDB,1,RAF2CF_SEC*256)
        IF(ST.NE.0) THEN
          CALL CLOSEFIL(RAF2FDB)                                                !CLOSE THE FILE
          CMSG = ' Erro ao abrir o ficheiro '//TRIM(CRAF2CFNAM)//'.'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       READ THE FILE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CALL READW(RAF2FDB,1,RAF2CF_REC,ST)
        IF(ST.NE.0) THEN
          CALL CLOSEFIL(RAF2FDB)                                                !CLOSE THE FILE
          CMSG = ' Erro ao ler o ficheiro '//TRIM(CRAF2CFNAM)//'.'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
C
        CALL FASTSET(0,RAF2CF_REC,RAF2CF_LEN + RAF2CF_FREESIZ)                  !CLEAR RECORD
        RAF2CF_GAMETYP    = TRAF
        RAF2CF_GAMEIND    = RAF2GI
        RAF2CF_XGAMNUM    = RAF2GN
        CALL FASTMOV(RAF2SGN_BASELINE,RAF2CF_SHGNAME,1)
        CALL FASTMOV(RAF2LGN_BASELINE,RAF2CF_LNGNAME,5)
        CALL FASTMOV(RAF2CFNAM,RAF2CF_FILNAME,5)
        CALL SETBIT_BSTRNG(RAF2CF_DBITMAP(1),0)                                 !SINGLE WEEK WAGERING ALWAYS ALLOWED (MOST SIGNIFICANT BIT OF FIRST BYTE = 1)
        RAF2CF_GOPTSON    = RAF2GOPTSON_DEFAULT
        RAF2CF_GOPTCTR    = RAF2GOPTCTR_DEFAULT
        CALL WRITEW(RAF2FDB,1,RAF2CF_REC,ST)
        IF(ST.NE.0) THEN
          CALL CLOSEFIL(RAF2FDB)                                                !CLOSE THE FILE
          CMSG = ' Erro ao gravar a conf. baseline no ficheiro '//
     *           TRIM(CRAF2CFNAM)//'...'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
        CALL CLOSEFIL(RAF2FDB)                                                  !CLOSE THE FILE
C
        ST = SMG$PUT_CHARS(DISP2, ' SUCESSO ', 10, 56,,
     *                     SMG$M_REVERSE + SMG$M_BOLD)
        ST = SMG$PUT_CHARS(DISP2, 'Concluído.', 13, 2,,SMG$M_UNDERLINE)
C
        CMSG = 'Qualquer tecla prossegue'
        ST = SMG$PUT_CHARS(DISP3, CMSG, 1, 2,, SMG$M_BOLD)
        ST = SMG$READ_KEYSTROKE(KEYB, KEY, , DISP3)
        ST = SMG$ERASE_DISPLAY(DISP2)
        RETURN
100     FORMAT ('A criar o ficheiro ', A,' com ',I3.3,' blocos...')
        END
C
C*******************************************************************************
C       SUBROUTINE TO CONFIGURE EUROMILLIONS GAME
C*******************************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE EUM1CONF(PB, KEYB, DISP1, DISP2, DISP3)
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
        INCLUDE 'INCLIB:RECEUM1CF.DEF'
        INCLUDE 'INCLIB:BLDEURSYS.DEF'
C
C LOCAL VARIABLES
C
        INTEGER*4 ST
C
        INTEGER*4 TAMMENU
        PARAMETER (TAMMENU = 4)
        CHARACTER*78  CMSG
        CHARACTER*18  MYMENU(TAMMENU) /'[    MOSTRAR     ]',
     *                                 '[    ALTERAR     ]',
     *                                 '[ CRIAR FICHEIRO ]',
     *                                 '[     VOLTAR     ]'/
C
        INTEGER PB,                                                             !PASTEBOARD
     *          KEYB,                                                           !VIRTUAL KEYBOARD
     *          DISP1,                                                          !HEADER FRAME WINDOW
     *          DISP2,                                                          !OUTPUT DATA FRAME WINDOW
     *          DISP3,                                                          !MESSAGE FRAME WINDOW
     *	        DISPMENU                                                        !INITIAL MENU

        INTEGER DEFAULT/1/
        INTEGER*4 MENUOPT
C
C       CREATES MENU
C
        ST = SMG$CREATE_VIRTUAL_DISPLAY(10, 20, DISPMENU)
        ST = SMG$CREATE_MENU(DISPMENU, MYMENU, SMG$K_VERTICAL,
     *                       SMG$M_DOUBLE_SPACE + SMG$M_WRAP_MENU ,
     *                       1, 0)
C
        ST = SMG$SET_CURSOR_MODE (PB, SMG$M_CURSOR_OFF )
        ST = SMG$ERASE_DISPLAY (DISP1)
        ST = SMG$ERASE_DISPLAY (DISP2)
        ST = SMG$ERASE_DISPLAY (DISP3)
C
        MENUOPT = 0
        DO WHILE(MENUOPT .NE. TAMMENU)
          ST = SMG$SET_CURSOR_MODE (DISP2, SMG$M_CURSOR_OFF )
          ST = SMG$LABEL_BORDER (DISP2,' Configuração ',
     *                           SMG$K_TOP, 65, SMG$M_REVERSE)
C
          CMSG = 'CONFIGURAÇÃO DO JOGO EUROMILHÕES'
          ST   = SMG$PUT_CHARS(DISP1, CMSG, 1, 24)
          ST   = SMG$PUT_CHARS(DISP1, BLDEURSYS_VERSION, 1, 66)
C
          CMSG = ' Use setas para movimentar cursor e <ENTER>'//
     *           ' para escolher '
          ST = SMG$PUT_CHARS(DISP3, CMSG, 1, 1)
          ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)
          ST = SMG$PASTE_VIRTUAL_DISPLAY(DISPMENU, PB, 9, 31)                   !LINE 9 COLUMUN 31
          ST = SMG$SELECT_FROM_MENU(KEYB, DISPMENU, MENUOPT,
     *                              DEFAULT,,,,,,SMG$M_BOLD)
          ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)
          ST = SMG$UNPASTE_VIRTUAL_DISPLAY (DISPMENU,PB)
C
         IF(MENUOPT.EQ.1) THEN
           CALL DSP_EUM1CONF(DISP2, DISP3, KEYB, PB)
         ELSEIF (MENUOPT.EQ.2) THEN
           CALL UPD_EUM1CONF(DISP2, DISP3, KEYB, PB)
         ELSEIF (MENUOPT.EQ.3) THEN
           CALL CRT_EUM1CONF(DISP2, DISP3, KEYB, PB)
         ENDIF
        ENDDO
        RETURN
        END
C
C*******************************************************************************
C       SUBROUTINE DSP_EUM1CONF (DISPLAYS DATA FROM EUM1CF.FIL)
C*******************************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE DSP_EUM1CONF(DISP2, DISP3, KEYB, PB)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECEUM1CF.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
C
        INTEGER*4 ST
        INTEGER*4 EUM1FDB(7)
        LOGICAL   ONDISK
        INTEGER*4 DSP2LN
C
        INTEGER PB,                                                             !PASTE BOARD
     *          KEYB,                                                           !VIRTUAL KEYBOARD
     *          DISP2,                                                          !OUTPUT DATA FRAME WINDOW
     *          DISP3                                                           !MESSAGE FRAME WINDOW
C
        CHARACTER*78  CMSG, CMSG4
        CHARACTER*39  CMSG1
        CHARACTER*16  CMSG2
        CHARACTER*20  CMSG3
        INTEGER KEY
        INTEGER*4     IND, I, J
C
        EUM1CF_FREESPC(1) = 0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$ERASE_DISPLAY(DISP2)
        CMSG = 'A ler as configurações registadas. '//
     *         'Aguarde, por favor...'
        ST = SMG$PUT_CHARS(DISP3, CMSG, 1, 1, SMG$M_ERASE_LINE)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       CHECK IF CONFIGURATION FILE EXISTS IN THE SYSTEM
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        INQUIRE(FILE=TRIM(CEUM1CFNAM), EXIST=ONDISK)
        IF(.NOT. ONDISK) THEN
          CMSG = ' O ficheiro '//TRIM(CEUM1CFNAM)//
     *           ' não existe no MILLENNIUM...'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       OPEN THE FILE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CALL OPENX(1,TRIM(CEUM1CFNAM),4,0,0,ST)
        CALL IOINIT(EUM1FDB,1,EUM1CF_SEC*256)
        IF(ST.NE.0) THEN
          CALL CLOSEFIL(EUM1FDB)                                                !CLOSE THE FILE
          CMSG = ' Erro ao abrir o ficheiro '//TRIM(CEUM1CFNAM)//'...'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       READ THE FILE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CALL READW(EUM1FDB,1,EUM1CF_REC,ST)
        IF(ST.NE.0) THEN
          CALL CLOSEFIL(EUM1FDB)                                                !CLOSE THE FILE
          CMSG = ' Erro ao ler o ficheiro '//TRIM(CEUM1CFNAM)//'...'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       DISPLAY THE FILE CONTENTS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        CMSG = 'Para voltar ao menu anterior prima qualquer tecla...'
        ST = SMG$PUT_CHARS(DISP3, CMSG, 1, 2, SMG$M_ERASE_LINE)
C
        CMSG = 'ESTADO DO JOGO......................................:'          !ESTADO DO JOGO EUROMILLIONS
        IF (EUM1CF_GACTIVE.EQ.0) THEN
          CMSG4 = 'DESATIVADO'
        ELSE
          CMSG4 = 'ATIVADO'
        ENDIF
        DSP2LN = 1
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN,  2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG4, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'REVISÃO DO JOGO.....................................:'          !EUROMILLIONS GAME REVISION
        DSP2LN = DSP2LN + 1
        WRITE(CMSG3, 100) EUM1CF_CTRLREV
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG3, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'TIPO/ÍNDICE DO JOGO.................................:'          !EUROMILLIONS GAME TYPE/INDEX
        DSP2LN = DSP2LN + 1
        WRITE(CMSG3, 102) EUM1CF_GAMETYP, EUM1CF_GAMEIND
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG3, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'NÚMERO DO JOGO (EXTERNO)............................:'          !EUROMILLIONS EXTERNAL GAME NUMBER
        DSP2LN = DSP2LN + 1
        WRITE(CMSG3, 101) EUM1CF_XGAMNUM
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG3, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'PREÇO DA APOSTA (CÊNTIMOS DE EURO)..................:'          !EUROMILLIONS GAME BASE PRICE
        DSP2LN = DSP2LN + 1
        WRITE(CMSG3, 101) EUM1CF_GBPRICE
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG3, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = '# MÁX. DE APOSTAS...................................:'          !EUROMILLIONS MAXIMUM BET LIMIT
        DSP2LN = DSP2LN + 1
        WRITE(CMSG3, 101) EUM1CF_MAXBETS
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG3, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'MULTI-SEMANAS (BITMAP)..............................:'          !MULTI-DRAW BITMAP
        DSP2LN = DSP2LN + 1
        WRITE(CMSG4, '(<EMAXDBM>(1X,Z2.2))')
     *                (EUM1CF_DBITMAP(I),I=1,EMAXDBM)
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG4, DSP2LN, 55,, SMG$M_BOLD)
C
        CMSG = 'JOGO M1LHÃO ASSOCIADO ?.............................:'          !SM ACTIVE FOR THIS GAME FLAG
        DSP2LN = DSP2LN + 1
        IF (EUM1CF_SMACTIV.EQ.0) THEN
          CMSG4 = 'NÃO'
        ELSE
          CMSG4 = 'SIM'
        ENDIF
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG4, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'NIF OBRIGATÓRIO P/ REGISTO DE APOSTAS ?.............:'          !PLAYER NIF REQUIRED FLAG
        DSP2LN = DSP2LN + 1
        IF (EUM1CF_PNIFREQ.EQ.0) THEN
          CMSG4 = 'NÃO'
        ELSE
          CMSG4 = 'SIM'
        ENDIF
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG4, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = '# MÍN. DE APOSTAS CMIL C/ REPRESENTAÇÃO EM INTERVALO:'          !MINIMUM NUMBER OF RAFFLES FROM WHICH THE RAFFLE NUMBERS HAVE TO BE PRINTED IN THE WAGER TICKET USING INTERVAL MODE REPRESENTATION (IMR)
        DSP2LN = DSP2LN + 1
        WRITE(CMSG3, 101) EUM1CF_SOMIMRB
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG3, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'NOME ABREVIADO DO JOGO..............................:'          !EUROMILLIONS SHORT GAME NAME
        DSP2LN = DSP2LN + 1
        WRITE(CMSG4, '(A4)') EUM1CF_SHGNAME
        DO J=1,4
          IF(CMSG4(J:J).EQ.CHAR(0)) CMSG4(J:J) = ' '
        ENDDO
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG4, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'NOME LONGO DO JOGO..................................:'          !EUROMILLIONS LONG GAME NAME
        DSP2LN = DSP2LN + 1
        WRITE(CMSG4, '(5A4)') (EUM1CF_LNGNAME(I),I=1,5)
        DO I=1,20
          IF(CMSG4(I:I).EQ.CHAR(0)) CMSG4(I:I) = ' '
        ENDDO
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG4, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'NOME DO FICHEIRO DE CONFIGURAÇÃO....................:'          !EUROMILLIONS GAME CONFIGURATION FILENAME
        DSP2LN = DSP2LN + 1
        WRITE(CMSG4, '(5A4)') (EUM1CF_FILNAME(I),I=1,5)
        DO I=1,20
          IF(CMSG4(I:I).EQ.CHAR(0)) CMSG4(I:I) = ' '
        ENDDO
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG4, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'OPTION FLAGS DO JOGO NO SIGN-ON.....................:'          !GAME OPTION FLAGS AT SON
        DSP2LN = DSP2LN + 2
        WRITE(CMSG3, 100) EUM1CF_GOPTSON
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG3, DSP2LN, 56,, SMG$M_BOLD)
C
        CMSG = 'OPTION FLAGS DO JOGO NO CONTROL REQUEST.............:'          !GAME OPTION FLAGS AT CONTROL REQUEST
        DSP2LN = DSP2LN + 1
        WRITE(CMSG3, 100) EUM1CF_GOPTCTR
        ST = SMG$PUT_CHARS(DISP2, CMSG , DSP2LN, 2,, )
        ST = SMG$PUT_CHARS(DISP2, CMSG3, DSP2LN, 56,, SMG$M_BOLD)
C
        ST = SMG$READ_KEYSTROKE(KEYB, KEY, , DISP3)
        ST = SMG$ERASE_DISPLAY(DISP2)
C
        CALL CLOSEFIL(EUM1FDB)
C
        RETURN
100     FORMAT (Z4.4)
101     FORMAT (I0)
102     FORMAT (I0,' / ', I0)
        END
C
C*******************************************************************************
C       SUBROUTINE UPD_EUM1CONF (UPDATE DATA IN EUM1CF.FIL FILE)
C*******************************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE UPD_EUM1CONF(DISP2, DISP3, KEYB, PB)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECEUM1CF.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
        INCLUDE 'INCLIB:BLDEURSYS.DEF'
C
        INTEGER*4 ST
        INTEGER*4 EUM1FDB(7)
        LOGICAL   ONDISK
        LOGICAL   CHANGED
C
        INTEGER PB,                                                             !PASTE BOARD
     *          KEYB,                                                           !VIRTUAL KEYBOARD
     *          DISP2,                                                          !OUTPUT DATA FRAME WINDOW
     *          DISP3                                                           !MESSAGE FRAME WINDOW
C
        CHARACTER*78  CMSG, CMSG4
        CHARACTER*20  CMSG1
        CHARACTER*30  CMSG2
        CHARACTER*5   CMSG3

        INTEGER       KEY, DEFAULT
        INTEGER*4     I, J, K, M, ACTVAL, NEWVAL
        INTEGER*4     USRVAL                                                    !USER ENTERED VALUE
        INTEGER*4     DSP2LN
        INTEGER*4     MDSTAB(MAXMLTD_AVL)                                       !MULTI-DRAW SELECTED TABLE
C
        INTEGER*4     I4USRVAL
        CHARACTER*4   C4USRVAL
        EQUIVALENCE  (I4USRVAL, C4USRVAL)
C
        INTEGER*4     I4USRVAL5(5), I4ACTVAL5(5), I4NEWVAL5(5)
        CHARACTER*20  C20USRVAL, C20ACTVAL, C20NEWVAL
        EQUIVALENCE  (I4USRVAL5, C20USRVAL)
        EQUIVALENCE  (I4ACTVAL5, C20ACTVAL)
        EQUIVALENCE  (I4NEWVAL5, C20NEWVAL)
C
        CHARACTER*17  C17DRWVAL
        CHARACTER*14  C14DRWVAL

        INTEGER*2     I2USRVAL
        CHARACTER*2   C2USRVAL
        EQUIVALENCE  (I2USRVAL,C2USRVAL)
C
        CHANGED = .FALSE.
        EUM1CF_FREESPC(1) = 0
C
        ST = SMG$ERASE_DISPLAY(DISP2)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       CHECK IF CONFIGURATION FILE EXISTS IN THE SYSTEM
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        INQUIRE(FILE=TRIM(CEUM1CFNAM), EXIST=ONDISK)
        IF(.NOT. ONDISK) THEN
          CMSG = ' O ficheiro '//TRIM(CEUM1CFNAM)//
     *           ' não existe no MILLENNIUM...'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       ASK THE USER IF HE WANTS TO CHANGE THE GAME CONFIGURATION FILE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$ERASE_DISPLAY(DISP2)
        CMSG = 'Deseja alterar as configurações do jogo EUROMILHÕES ?'
        DEFAULT = 2
        CALL MENU_CONFIRM(TRIM(CMSG), 8, 4, 3, 58,
     *                    DISP2, KEYB, PB, DEFAULT, ST)
        IF(ST.NE.0) THEN
          ST = SMG$ERASE_DISPLAY(DISP2)
          RETURN
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       OPEN THE FILE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        CALL OPENX(1,TRIM(CEUM1CFNAM),4,0,0,ST)
        CALL IOINIT(EUM1FDB,1,EUM1CF_SEC*256)
        IF(ST.NE.0) THEN
          CALL CLOSEFIL(EUM1FDB)                                                !CLOSE THE FILE
          CMSG = ' Erro ao abrir o ficheiro '//TRIM(CEUM1CFNAM)//'...'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       READ THE FILE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CALL READW(EUM1FDB,1,EUM1CF_REC,ST)
        IF(ST.NE.0) THEN
          CALL CLOSEFIL(EUM1FDB)                                                !CLOSE THE FILE
          CMSG = ' Erro ao ler o ficheiro '//TRIM(CEUM1CFNAM)//'...'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
C
        CALL CLOSEFIL(EUM1FDB)                                                  !CLOSE THE FILE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       ACTIVATE/DEACTIVATE GAME
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$ERASE_DISPLAY(DISP2)
        ST = SMG$PUT_CHARS(DISP3, DSP3_MENU_CONFIRM_MSG, 1, 2)
        ACTVAL = EUM1CF_GACTIVE
        IF(ACTVAL.EQ.0) THEN
          CMSG = 'Deseja ATIVAR o jogo EUROMILHÕES ?'
        ELSE
          CMSG = 'Deseja DESATIVAR o jogo EUROMILHÕES ?'
        ENDIF
        DSP2LN  = 1
        DEFAULT = 2
        CALL MENU_CONFIRM(TRIM(CMSG), DSP2LN, 4, 3, 55,
     *                    DISP2, KEYB, PB,
     *                    DEFAULT, ST)
        IF(ST.EQ.-2) RETURN
        IF(ST.EQ.0) THEN                                                        !USER HAS CHOOSEN "Sim"
          NEWVAL = 1 - ACTVAL                                                   !TOGGLE VALUE OF ACTVAL
          ST = SMG$PUT_CHARS(DISP2, 'SIM', DSP2LN, 59,, SMG$M_BOLD)
          EUM1CF_GACTIVE = NEWVAL
          CHANGED = .TRUE.
          ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2)
        ELSE
          ST = SMG$PUT_CHARS(DISP2, 'NÃO', DSP2LN, 59,, SMG$M_BOLD)
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       EUROMILLIONS GAME BASE PRICE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$ERASE_DISPLAY(DISP3)
        DSP2LN =  DSP2LN + 1
        CMSG = 'Digite o preço da aposta (cêntimos de euro)'
        ST = SMG$PUT_CHARS(DISP2, CMSG, DSP2LN, 3)
        ACTVAL = EUM1CF_GBPRICE
        USRVAL = ACTVAL
        CALL SMG_INPNUM(PB, KEYB, DISP2, DISP3, USRVAL, DSP2LN, 59, 5,
     *                  1, 65635, ST)
        IF(ST.EQ.-2) RETURN
        NEWVAL = USRVAL
        IF(ACTVAL.NE.NEWVAL) THEN
          EUM1CF_GBPRICE = NEWVAL
          CHANGED = .TRUE.
          ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2)
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       MAXIMUM BET LIMIT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$PUT_CHARS(DISP3, DSP3_MENU_CONFIRM_MSG, 1, 2)
        DSP2LN = DSP2LN + 1
        CMSG = 'Digite o limite de apostas máximo'
        ST = SMG$PUT_CHARS(DISP2, CMSG, DSP2LN, 3)
        ACTVAL = EUM1CF_MAXBETS
        USRVAL = ACTVAL
        CALL SMG_INPNUM(PB, KEYB, DISP2, DISP3, USRVAL, DSP2LN, 59, 5,
     *                  1, 65635, ST)
        IF(ST.EQ.-2) RETURN
        NEWVAL = USRVAL
        IF(ACTVAL.NE.NEWVAL) THEN
          EUM1CF_MAXBETS = NEWVAL
          CHANGED = .TRUE.
          ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2)
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       MULTIDRAW
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$PUT_CHARS(DISP3, DSP3_MENU_CONFIRM_MSG, 1, 2)
        DSP2LN = DSP2LN + 1
        CMSG = 'Deseja alterar as apostas multi-semana ?'
        DEFAULT = 2                                                             ! "Não"
        CALL MENU_CONFIRM(TRIM(CMSG), DSP2LN, 4, 3, 55, DISP2, KEYB,
     *                    PB, DEFAULT, ST)
        IF(ST.EQ.0) THEN
          ST = SMG$ERASE_DISPLAY(DISP3)
          CMSG = 'Digite as apostas multi semana'
          ST = SMG$PUT_CHARS(DISP2, CMSG, DSP2LN, 3)
          C14DRWVAL = '              '
          CALL FASTSET(0, MDSTAB, MAXMLTD_AVL)
          MDSTAB(1) = 1                                                         !SINGLE WEEK WAGERING ALWAYS ALLOWED
          M = 1
          DO I=1,EMAXDBM
            DO J=0,7
              IF(TSTBIT_BSTRNG(EUM1CF_DBITMAP(I),J)) THEN
                MDSTAB(M) = (I-1)*8+J+1
                M = M + 1
              ENDIF
            ENDDO
          ENDDO
C
          DO I=2,MAXMLTD_AVL                                                    !SKIP I = 1
            CMSG = 'Digite 0 ou limpe o campo seguido da tecla '//
     *             '<ENTER> para finalizar'
            ST = SMG$PUT_CHARS(DISP3, CMSG, 1, 2)
            ACTVAL = MDSTAB(I)
            USRVAL = ACTVAL
            CALL MY_SMG_INPNUM(PB, KEYB, DISP2, DISP3, USRVAL, DSP2LN,
     *                         74, 2, MDSTAB(I-1)+1, MAXMLTD_AVL, ST)
            IF(ST.EQ.-2) RETURN
C
            WRITE(C2USRVAL,'(I2.2)') USRVAL
            J=LEN_TRIM(C14DRWVAL)
            IF(USRVAL.GT.0 .AND.
     *         USRVAL.LT.MAXMLTD_AVL .AND.
     *         I.LT.MAXMLTD_AVL) THEN
C
              IF(J.EQ.12) THEN
                C14DRWVAL(1:J) = C14DRWVAL(4:J)//C2USRVAL//','                  !LEFT SHIFT
                ST = SMG$PUT_CHARS(DISP2, '...'//TRIM(C14DRWVAL),
     *                             DSP2LN,
     *                             71-LEN_TRIM(C14DRWVAL)
     *                             ,,SMG$M_REVERSE+SMG$M_BOLD)
              ELSE
                C14DRWVAL(J+1:J+3) = C2USRVAL//','
                ST = SMG$PUT_CHARS(DISP2, TRIM(C14DRWVAL), DSP2LN,
     *                             73-LEN_TRIM(C14DRWVAL)+1
     *                             ,,SMG$M_REVERSE+SMG$M_BOLD)
              ENDIF

              NEWVAL = USRVAL
              IF(ACTVAL.NE.NEWVAL) THEN
                MDSTAB(I) = NEWVAL
                CHANGED = .TRUE.
                ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2)
              ENDIF
              MDSTAB(I) = NEWVAL
            ELSEIF(USRVAL.EQ.0 .OR.
     *             USRVAL.EQ.MAXMLTD_AVL .OR.
     *             I.EQ.MAXMLTD_AVL) THEN                                       !STOP CONDITION
              IF(USRVAL.EQ.0) THEN
                IF(I.EQ.2) THEN
                  C14DRWVAL = 'NÃO DEFINIDO  '
                ELSE
                  C14DRWVAL(LEN_TRIM(C14DRWVAL):LEN_TRIM(C14DRWVAL))
     *                                                             = ' '        !REMOVE LAST COMMA
                ENDIF
                ST = SMG$PUT_CHARS(DISP2, '                 ',
     *                             DSP2LN, 59)                                  !CLEAN FIRST
                IF(J.EQ.12) THEN
                  ST = SMG$PUT_CHARS(DISP2, '...'//TRIM(C14DRWVAL),
     *                               DSP2LN,
     *                               59
     *                               ,,SMG$M_REVERSE+SMG$M_BOLD)
                ELSE
                  ST = SMG$PUT_CHARS(DISP2, TRIM(C14DRWVAL), DSP2LN,
     *                               59
     *                               ,,SMG$M_REVERSE+SMG$M_BOLD)
                ENDIF
              ELSEIF(USRVAL.EQ.MAXMLTD_AVL .OR. I.EQ.MAXMLTD_AVL) THEN
                C14DRWVAL(J+1:J+3) = C2USRVAL                                   !SET LAST USER VALUE
                ST = SMG$PUT_CHARS(DISP2, '                 ',
     *                             DSP2LN, 59)                                  !CLEAN FIRST
                IF(J.EQ.12) THEN
                  ST = SMG$PUT_CHARS(DISP2, '...'//TRIM(C14DRWVAL),
     *                               DSP2LN,
     *                               59
     *                               ,,SMG$M_REVERSE+SMG$M_BOLD)
                ELSE
                  ST = SMG$PUT_CHARS(DISP2, TRIM(C14DRWVAL), DSP2LN,
     *                               59
     *                               ,,SMG$M_REVERSE+SMG$M_BOLD)
                ENDIF
              ENDIF
              NEWVAL = USRVAL
              IF(ACTVAL.NE.NEWVAL) THEN
                MDSTAB(I) = NEWVAL
                CHANGED = .TRUE.
                ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2)
              ENDIF
              DO J=I+1,MAXMLTD_AVL
                MDSTAB(J) = 0                                                   !REMOVE MULTIDRAW OF REMAINING
              ENDDO
              EXIT
            ENDIF
          ENDDO
C
          DO I = 1, EMAXDBM                                                     !CLEAR MULTIDRAW BITMAP
            EUM1CF_DBITMAP(I) = 0
          END DO
          DO M = 1, MAXMLTD_AVL
            IF(MDSTAB(M).GT.0) THEN
              K = (MDSTAB(M)-1) / 8 + 1                                         !K=1,2,3,4,5,6,7
              J = MOD(MDSTAB(M)-1,8)                                            !J=0,1,2,3,4,5,6,7
              CALL SETBIT_BSTRNG(EUM1CF_DBITMAP(K),J)                           !UPDATE EUM1CF_DBITMAP
            ENDIF
          ENDDO
        ELSE
          ST = SMG$PUT_CHARS(DISP2, 'NÃO', DSP2LN, 59,, SMG$M_BOLD)
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       ACTIVATE/DEACTIVATE SM GAME
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$PUT_CHARS(DISP3, DSP3_MENU_CONFIRM_MSG, 1, 2)
        DSP2LN = DSP2LN + 1
        ACTVAL = EUM1CF_SMACTIV
        IF(ACTVAL.EQ.0) THEN
          CMSG = 'Deseja ASSOCIAR o jogo M1LHÃO ?'
        ELSE
          CMSG = 'Deseja DESASSOCIAR o jogo M1LHÃO ?'
        ENDIF
        DEFAULT = 2
        CALL MENU_CONFIRM(TRIM(CMSG), DSP2LN, 4, 3, 55, DISP2, KEYB,
     *                    PB, DEFAULT, ST)
        IF(ST.EQ.-2) RETURN
        IF(ST.EQ.0) THEN                                                        !USER HAS CHOOSEN "Sim"
          NEWVAL = 1 - ACTVAL                                                   !TOGGLE VALUE OF ACTVAL
          ST = SMG$PUT_CHARS(DISP2, 'SIM', DSP2LN, 59,, SMG$M_BOLD)
          EUM1CF_SMACTIV = NEWVAL
          IF(NEWVAL.EQ.1) THEN
            EUM1CF_GOPTCTR = IOR(EUM1CF_GOPTCTR,SMACTV_GOPT)
          ELSE
            EUM1CF_GOPTCTR = IAND(EUM1CF_GOPTCTR,.NOT.SMACTV_GOPT)
          ENDIF
          CHANGED = .TRUE.
          ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2)
        ELSE
          ST = SMG$PUT_CHARS(DISP2, 'NÃO', DSP2LN, 59,, SMG$M_BOLD)
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       PLAYER NIF REQUIRED
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$PUT_CHARS(DISP3, DSP3_MENU_CONFIRM_MSG, 1, 2)
        DSP2LN = DSP2LN + 1
        CMSG = 'NIF obrigatório p/ registo de apostas ?'
        DEFAULT = 2
        ACTVAL = EUM1CF_PNIFREQ
        IF(ACTVAL.EQ.1) DEFAULT = 1
        CALL MENU_CONFIRM(TRIM(CMSG), DSP2LN, 4, 3, 55, DISP2, KEYB,
     *                    PB, DEFAULT, ST)
        IF(ST.EQ.0) THEN
          NEWVAL = 1
          ST = SMG$PUT_CHARS(DISP2, 'SIM', DSP2LN, 59,, SMG$M_BOLD)
          IF(ACTVAL.NE.NEWVAL) THEN
            EUM1CF_PNIFREQ = NEWVAL
            EUM1CF_GOPTCTR = IOR(EUM1CF_GOPTCTR,PLANIF_GOPT)
            CHANGED = .TRUE.
            ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2,, SMG$M_BOLD)
          ENDIF
        ELSE
          NEWVAL = 0
          ST = SMG$PUT_CHARS(DISP2, 'NÃO', DSP2LN, 59,, SMG$M_BOLD)
          IF(ACTVAL.NE.NEWVAL) THEN
            EUM1CF_PNIFREQ = NEWVAL
            EUM1CF_GOPTCTR = IAND(EUM1CF_GOPTCTR,.NOT.PLANIF_GOPT)
            CHANGED = .TRUE.
            ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2,, SMG$M_BOLD)
          ENDIF
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       MINIMUM NUMBER OF RAFFLES FROM WHICH THE RAFFLE NUMBERS HAVE TO BE
C       PRINTED IN THE WAGER TICKET USING INTERVAL MODE REPRESENTATION (IMR)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC)
        ST = SMG$ERASE_DISPLAY(DISP3)
        DSP2LN = DSP2LN + 1
        CMSG = 'Digite o núm. mín. de apostas CMIL c/ repr. em inter.'
        ST = SMG$PUT_CHARS(DISP2, CMSG, DSP2LN, 3)
        ACTVAL = EUM1CF_SOMIMRB
        USRVAL = ACTVAL
        CALL SMG_INPNUM(PB, KEYB, DISP2, DISP3, USRVAL, DSP2LN, 59, 3,
     *                  1, 255, ST)
        IF(ST.EQ.-2) RETURN
        NEWVAL = USRVAL
        IF(ACTVAL.NE.NEWVAL) THEN
          EUM1CF_SOMIMRB = NEWVAL
          CHANGED = .TRUE.
          ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2)
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SM SHORT GAME NAME
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$ERASE_DISPLAY(DISP3)
        DSP2LN = DSP2LN + 1
        CMSG = 'Digite o nome abreviado do jogo'
        ST = SMG$PUT_CHARS (DISP2, CMSG, DSP2LN, 3)
        ACTVAL = EUM1CF_SHGNAME
        WRITE(C4USRVAL,'(A4)') ACTVAL
        CALL SMG_INPTEXT(PB,KEYB,DISP2,DISP3,C4USRVAL,1,
     *                   DSP2LN,59,4,1,ST)
        IF(ST.EQ.-2) RETURN
        NEWVAL = I4USRVAL
        IF(ACTVAL.NE.NEWVAL) THEN
          EUM1CF_SHGNAME = NEWVAL
          CHANGED = .TRUE.
          ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2,, SMG$M_BOLD)
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SM LONG GAME NAME
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$ERASE_DISPLAY(DISP3)
        DSP2LN = DSP2LN + 1
        CMSG = 'Digite o nome longo do jogo'
        ST = SMG$PUT_CHARS (DISP2, CMSG, DSP2LN, 3)
        CALL FASTMOV(EUM1CF_LNGNAME,I4ACTVAL5,5)
        WRITE(C20USRVAL,'(5A4)') I4ACTVAL5
        CALL SMG_INPTEXT(PB,KEYB,DISP2,DISP3,C20USRVAL,1,DSP2LN,
     *                   59,20,1,ST)
        IF(ST.EQ.-2) RETURN
        C20USRVAL = ADJUSTL(C20USRVAL)
        CALL FASTMOV(I4USRVAL5,I4NEWVAL5,5)
        ST = SMG$PUT_CHARS(DISP2, C20USRVAL, DSP2LN, 59,,
     *                     SMG$M_REVERSE, SMG$M_BOLD)
        IF(C20ACTVAL.NE.C20NEWVAL) THEN
          CALL FASTMOV(I4NEWVAL5,EUM1CF_LNGNAME,5)
          CHANGED = .TRUE.
          ST = SMG$PUT_CHARS(DISP2, '*', DSP2LN, 2,, SMG$M_BOLD)
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SAVE THE CHANGES IN FILE IF CHANGES HAVE BEEN MADE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        IF(CHANGED) THEN
          CALL OPENX(1,TRIM(CEUM1CFNAM),4,0,0,ST)                               !OPEN THE FILE
          CALL IOINIT(EUM1FDB,1,EUM1CF_SEC*256)
          IF(ST.NE.0) THEN
            CALL CLOSEFIL(EUM1FDB)                                              !CLOSE THE FILE
            CMSG = ' Erro ao abrir o ficheiro '//TRIM(CEUM1CFNAM)//'...'
            CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
            RETURN
          ENDIF
          ST = SMG$PUT_CHARS (DISP3, DSP3_MENU_CONFIRM_MSG, 1, 2)
          DSP2LN = DSP2LN + 4
          CMSG = '*DESEJA GRAVAR AS ALTERAÇÕES EFETUADAS ?'
          DEFAULT = 2
          CALL MENU_CONFIRM(TRIM(CMSG), DSP2LN, 4, 2, 56, DISP2, KEYB,
     *                      PB, DEFAULT, ST)
          IF(ST.EQ.0) THEN
            EUM1CF_CTRLREV = EUM1CF_CTRLREV + 1                                 !SET NEW REVISION NUMBER
            CALL WRITEW(EUM1FDB,1,EUM1CF_REC,ST)
            IF(ST.NE.0) THEN
              CALL CLOSEFIL(EUM1FDB)                                            !CLOSE THE FILE
              CMSG = ' Erro ao gravar as alterações no ficheiro '//
     *               TRIM(CEUM1CFNAM)//'...'
              CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
              RETURN
            ENDIF
            CMSG = 'SUCESSO'
            ST = SMG$PUT_CHARS(DISP2, TRIM(CMSG), DSP2LN, 59,,
     *                         SMG$M_REVERSE + SMG$M_BOLD)
            CMSG = ' Para voltar ao menu anterior prima qualquer'//
     *             ' tecla...'
            CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          ENDIF
        ELSE
          CALL CLOSEFIL(EUM1FDB)
          DSP2LN = DSP2LN + 4
          CMSG = 'NÃO EXISTEM ALTERAÇÕES A GRAVAR.'
          ST = SMG$PUT_CHARS(DISP2, CMSG, DSP2LN, 3,, SMG$M_BOLD)
          CMSG = ' Para voltar ao menu anterior prima qualquer tecla...'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
        ENDIF
C
        ST = SMG$ERASE_DISPLAY(DISP2)
C
        RETURN
100     FORMAT (Z2.2)
        END
C
C*******************************************************************************
C       SUBROUTINE CRT_EUM1CONF (CREATES EUM1CF.FIL GAME FILE DATA)
C*******************************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CRT_EUM1CONF(DISP2, DISP3, KEYB, PB)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECEUM1CF.DEF'
        INCLUDE 'INCLIB:BLDEURSYS.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
C
        INTEGER*4 ST, SECTORS, KEY, I
        INTEGER*4 EUM1FDB(7)
        LOGICAL ONDISK
C
        INTEGER PB,                                                             !PASTE BOARD
     *          KEYB,                                                           !VIRTUAL KEYBOARD
     *          DISP2,                                                          !OUTPUT DATA FRAME WINDOW
     *          DISP3                                                           !MESSAGE FRAME WINDOW
C
        CHARACTER*78 CMSG
        CHARACTER*30 CMSG1
C
        EUM1CF_FREESPC(1) = 0
C
        ST = SMG$ERASE_DISPLAY(DISP2)
        CMSG = 'O utilizador vai criar o ficheiro '//
     *         TRIM(CEUM1CFNAM)//'...'
        ST = SMG$PUT_CHARS(DISP3, CMSG, 1, 2, SMG$M_ERASE_LINE)
C
        INQUIRE(FILE=TRIM(CEUM1CFNAM), EXIST = ONDISK)
        IF(ONDISK) THEN
          CMSG = ' O ficheiro '//TRIM(CEUM1CFNAM)//
     *           ' já existe no sistema MILLENNIUM.'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
        RETURN
        ENDIF
        CMSG = 'CRIAÇÃO DO FICHEIRO DE CONFIGURAÇÃO DO JOGO EUROMILHÕES'
        ST = SMG$PUT_CHARS(DISP2, TRIM(CMSG), 2, 2,, SMG$M_UNDERLINE)

        CMSG1 = 'Digite o número de blocos: '
        ST = SMG$PUT_CHARS(DISP2, CMSG1, 5, 2,,)
        CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,SECTORS,5,34,3,200,999,ST)
C
        WRITE(CMSG,100) TRIM(CEUM1CFNAM), SECTORS
        ST = SMG$PUT_CHARS(DISP2, CMSG, 8, 2,,)
C
        CALL SMG_NEWFIL(7,TRIM(CEUM1CFNAM), SECTORS, ST)
        IF(ST .NE. 0) THEN
          CMSG = 'ERRO: o ficheiro '//TRIM(CEUM1CFNAM)//
     *           ' não foi criado.'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
C
        ST = SMG$PUT_CHARS(DISP2, ' SUCESSO ', 8, 56,,
     *                     SMG$M_REVERSE + SMG$M_BOLD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SET BASELINE VALUES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CMSG = 'A configurar as definições baseline do jogo...'
        ST = SMG$PUT_CHARS(DISP2, TRIM(CMSG), 10, 2,,)
        CALL OPENX(1,TRIM(CEUM1CFNAM),4,0,0,ST)
        CALL IOINIT(EUM1FDB,1,EUM1CF_SEC*256)
        IF(ST.NE.0) THEN
          CALL CLOSEFIL(EUM1FDB)                                                !CLOSE THE FILE
          CMSG = ' Erro ao abrir o ficheiro '//TRIM(CEUM1CFNAM)//'.'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       READ THE FILE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CALL READW(EUM1FDB,1,EUM1CF_REC,ST)
        IF(ST.NE.0) THEN
          CALL CLOSEFIL(EUM1FDB)                                                !CLOSE THE FILE
          CMSG = ' Erro ao ler o ficheiro '//TRIM(CEUM1CFNAM)//'.'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
C
        CALL FASTSET(0,EUM1CF_REC,EUM1CF_LEN + EUM1CF_FREESIZ)                  !CLEAR RECORD
        EUM1CF_GAMETYP    = TEUM
        EUM1CF_GAMEIND    = EUM1GI
        EUM1CF_XGAMNUM    = EUM1GN
        CALL FASTMOV(EUM1SGN_BASELINE,EUM1CF_SHGNAME,1)
        CALL FASTMOV(EUM1LGN_BASELINE,EUM1CF_LNGNAME,5)
        CALL FASTMOV(EUM1CFNAM,EUM1CF_FILNAME,5)
        CALL SETBIT_BSTRNG(EUM1CF_DBITMAP(1),0)                                 !SINGLE WEEK WAGERING ALWAYS ALLOWED (MOST SIGNIFICANT BIT OF FIRST BYTE = 1)
        EUM1CF_GOPTSON    = EUM1GOPTSON_DEFAULT
        EUM1CF_GOPTCTR    = EUM1GOPTCTR_DEFAULT
        CALL WRITEW(EUM1FDB,1,EUM1CF_REC,ST)
        IF(ST.NE.0) THEN
          CALL CLOSEFIL(EUM1FDB)                                                !CLOSE THE FILE
          CMSG = ' Erro ao gravar a conf. baseline no ficheiro '//
     *           TRIM(CEUM1CFNAM)//'...'
          CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
        CALL CLOSEFIL(EUM1FDB)                                                  !CLOSE THE FILE
C
        ST = SMG$PUT_CHARS(DISP2, ' SUCESSO ', 10, 56,,
     *                     SMG$M_REVERSE + SMG$M_BOLD)
        ST = SMG$PUT_CHARS(DISP2, 'Concluído.', 13, 2,,SMG$M_UNDERLINE)
C
        CMSG = 'Qualquer tecla prossegue'
        ST = SMG$PUT_CHARS(DISP3, CMSG, 1, 2,, SMG$M_BOLD)
        ST = SMG$READ_KEYSTROKE(KEYB, KEY, , DISP3)
        ST = SMG$ERASE_DISPLAY(DISP2)
        RETURN
100     FORMAT ('A criar o ficheiro ', A,' com ',I3.3,' blocos...')
        END
C
C*******************************************************************************
C       SUBROUTINE MENU_CONFIRM
C*******************************************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE MENU_CONFIRM(CMSG,
     *                          LIN, LINEND,
     *                          COL, COLEND,
     *                          DISP2, IKEYB, PB, DEFAULT, ST)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       ROUTINE PARAMETERS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CHARACTER*(*) CMSG
        INTEGER       DISP2, IKEYB, PB
        INTEGER       DEFAULT                                                   !DEFAULT MENU OPTION (1 - Sim, 2 - Não)
        INTEGER*4     ST, LIN, COL, LINEND, COLEND
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       LOCAL VARIABLES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CHARACTER*4 CONFIRM(2)/' Sim', ' Não'/
C
        INTEGER   DISP4
        INTEGER*4 IOPT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       ASK FOR CONFIRMATION
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$PUT_CHARS(DISP2, CMSG, LIN, COL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       CREATES YES/NO MENU DISPLAY - DISPCONF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ST = SMG$CREATE_VIRTUAL_DISPLAY(1, 14, DISP4)
        ST = SMG$CREATE_MENU(DISP4, CONFIRM, SMG$K_HORIZONTAL,,,
     *                       SMG$M_REVERSE)
        ST = SMG$PASTE_VIRTUAL_DISPLAY(DISP4, PB,
     *                                 LIN+LINEND, COL+COLEND)
        ST = SMG$SELECT_FROM_MENU(IKEYB, DISP4, IOPT, DEFAULT,,,,,,
     *                            SMG$M_BOLD)
        ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)
        ST = SMG$UNPASTE_VIRTUAL_DISPLAY(DISP4, PB)
        ST = SMG$DELETE_VIRTUAL_DISPLAY (DISP4, PB)
        IF(IOPT.NE.1) THEN
          ST = -1
        ELSE
          ST = 0
        ENDIF
C
        RETURN
        END
C
C*******************************************************************************
C       SUBROUTINE MY_SMG_INPNUM
C*******************************************************************************
        SUBROUTINE MY_SMG_INPNUM(IPB,IKEYB,IDISP2,IDISP3,
     *                           OUTVAR,ILIN,ICOL,ISIZE,IMIN,IMAX,IST)
        IMPLICIT NONE
C
        INCLUDE '($SMGDEF)'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
C       EXTERNAL VARIABLES
C
        INTEGER   IPB, IKEYB, IDISP2, IDISP3
        INTEGER*4 OUTVAR, ILIN, ICOL, ISIZE, IMIN, IMAX, IST
C
C       LOCAL VARIABLES
C
        CHARACTER*10 CIDX
        CHARACTER*76 CMSG
C
        INTEGER SMG$ERASE_DISPLAY, SMG$PUT_CHARS, SMG$RING_BELL
C
        INTEGER*4 ICIDXLEN, INEXT
        INTEGER*4 AUXOUTVAR
C
        LOGICAL REPEAT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       BEGIN OF CODE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        REPEAT = .TRUE.

        DO WHILE(REPEAT)
          INEXT = 2
          IF(OUTVAR.EQ.0) THEN
            CIDX = '          '
          ELSE
            WRITE(CIDX,100) OUTVAR
          ENDIF
          CALL INPUT(IPB, IKEYB, IDISP2, ILIN, ICOL, ISIZE, 0, 'N',
     *               CIDX, ICIDXLEN, INEXT)
          DECODE(ICIDXLEN,100,CIDX) AUXOUTVAR

          IF(LEN_TRIM(CIDX).EQ.0) THEN
              WRITE (CMSG,200) IMIN, IMAX
              IST = SMG$PUT_CHARS (IDISP3, CMSG, 1,2,
     *                             SMG$M_ERASE_LINE, SMG$M_BOLD)
              IST = SMG$RING_BELL(IDISP3, 2)
          ELSE
            IF((AUXOUTVAR.NE.0) .AND.
     *         (AUXOUTVAR.LT.IMIN .OR. AUXOUTVAR.GT.IMAX)) THEN
              WRITE (CMSG,200) IMIN, IMAX
              IST = SMG$PUT_CHARS (IDISP3, CMSG, 1,2,
     *                             SMG$M_ERASE_LINE, SMG$M_BOLD)
              IST = SMG$RING_BELL(IDISP3, 2)
            ELSE
              REPEAT = .FALSE.
              IST = SMG$ERASE_DISPLAY(IDISP3)
              OUTVAR = AUXOUTVAR
              IF(INEXT.EQ.-1) THEN
                IST = -2
              ELSE
                IST = 1
              ENDIF
            ENDIF
          ENDIF
        ENDDO

        RETURN
100     FORMAT(I<ISIZE>)
200     FORMAT('VALOR INCORRETO, LIMITES SAO DE ',I0,' A ',I0,
     *         ' OU 0 PARA SAIR')
        END
C
C END BLDEURSYS.FOR
C
