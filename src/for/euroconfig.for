C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM EUROCONFIG
        IMPLICIT NONE
	
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:EUROCONFIG.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
C
C LOCAL VARIABLES
C
C        CHARACTER*60 FILENAME
C        LOGICAL ONDISK
        INTEGER*4 ST
        COMMON ECFREC
        
        INTEGER*4 TAMMENU
        PARAMETER (TAMMENU = 4)
        CHARACTER*78  cMSG
        CHARACTER*5     VERSAO     /'v1.0 '/
        CHARACTER*28    MYMENU(TAMMENU) /'[      VER       ]',
     *                                   '[    ALTERAR     ]',
     *                                   '[ CRIAR FICHEIRO ]',
     *                                   '[      SAIR      ]'/         
        INTEGER PB,			      ! PASTEBOARD
     *        KEYB,			      ! VIRTUAL KEYBOARD
     *        DISP1,			      ! header frame window
     *        DISP2,			      ! output data frame window
     *        DISP3,			      ! message frame window
     *	      DISPMENU			      ! INITIAL MENU
        
        INTEGER DEFAULT/1/
        INTEGER*4 MENUOPT
        ECFFRE(1) = 0 
C
C CREATES SCREEN HEADER - DISP1
C
        ST = SMG$CREATE_VIRTUAL_DISPLAY( 1, 78, DISP1, SMG$M_BORDER)
        ST = SMG$LABEL_BORDER (DISP1,' SCML ',
     *			     SMG$K_TOP, 70,
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
        ST = SMG$LABEL_BORDER (DISP3,' Mensagem ', SMG$K_TOP, 70, SMG$M_REVERSE)
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
     *	     SMG$M_DOUBLE_SPACE + SMG$M_WRAP_MENU , 2, 0)
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

          cMSG = 'Menu de configuração do Euro Milhões'
          ST   = SMG$PUT_CHARS (DISP1, cMSG, 1, 20)
          ST   = SMG$PUT_CHARS (DISP1, VERSAO, 1, 66)

	  cMSG = '    Use setas para movimentar cursor e <ENTER> para escolher '
      	  ST = SMG$PUT_CHARS (DISP3, cMSG, 1,1)
	  ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)

      	  ST = SMG$PASTE_VIRTUAL_DISPLAY(DISPMENU, PB, 7, 30)

       	  ST = SMG$SELECT_FROM_MENU(KEYB, DISPMENU, MENUOPT, 
     *				       DEFAULT,,,,,,SMG$M_BOLD + SMG$M_REVERSE )

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
        INCLUDE 'INCLIB:EUROCONFIG.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
        INCLUDE       '($TRMDEF)'        
        
        INTEGER*4 ST,FDB(7)
        LOGICAL ONDISK
        
        INTEGER KEYB,PB,			      ! VIRTUAL KEYBOARD
     *        DISP2,			      ! output data frame window
     *        DISP3			      ! message frame window

        CHARACTER*78  cMSG
        CHARACTER*20 cMSG1 
        CHARACTER*30 cMSG2
        INTEGER KEY
        COMMON ECFREC
        LOGICAL CHANGED
        INTEGER*4 MULTWEEK(56),IND,I,ACTVALUE
	
	ECFFRE(1) = 0 
	CHANGED = .FALSE.
C
C CREATES MENU 
C
        ST = SMG$ERASE_DISPLAY(DISP2)
        CMSG = ' Deseja alterar as configurações do Euro Milhões?' 
        CALL MENU_CONFIRM(CMSG,10,4,5,50,DISP2,KEYB,PB,ST)
        IF (ST .NE. 0 ) THEN
           ST = SMG$ERASE_DISPLAY(DISP2)
           RETURN
        ENDIF
        
        INQUIRE(FILE='SYSX:EUROCONF.FIL', EXIST=ONDISK)
        IF (.NOT. ONDISK) THEN
           CMSG = ' O ficheiro <EUROCONF.FIL> não existe no Millennium...'
	   CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
	   RETURN
        ENDIF
C
C
C        
        CALL OPENX(1,'EUROCONF.FIL',4,0,0,ST)
        CALL IOINIT(FDB,1,ECFSEC*256)
        IF(ST .NE. 0) THEN
          CMSG = ' Erro ao abrir o ficheiro <EUROCONF.FIL>...'
	  CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
        CALL READW(FDB,1,ECFREC,ST)
        IF(ST .NE. 0) THEN
          CMSG = ' Erro ao ler o ficheiro <EUROCONF.FIL>...'
	  CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF

        CMSG = ' Deseja alterar o estado do Euro Milhões?' 
        CALL MENU_CONFIRM(CMSG,14,4,10,50,DISP2,KEYB,PB,ST)
        IF (ST .EQ. 0 ) THEN
           IF (ECFACTIVE .EQ. 0 ) THEN
              CMSG = ' Deseja activar o Euro Milhões? '
              ACTVALUE = 0
           ELSE 
              CMSG = ' Deseja desactivar o Euro Milhões? '
              ACTVALUE = 1
           ENDIF
           
	   CALL MENU_CONFIRM(CMSG,16,4,10,40,DISP2,KEYB,PB,ST)
           IF (ST .EQ. 0) THEN
              IF (ACTVALUE .EQ. 0) THEN 
              	ECFACTIVE = 1
              ELSE
                ECFACTIVE = 0
              ENDIF
              CHANGED = .TRUE.
           ENDIF
        ENDIF
        ST = SMG$ERASE_DISPLAY(DISP2)         
        CMSG = ' Deseja alterar o Preço?' 
        CALL MENU_CONFIRM(CMSG,4,4,10,30,DISP2,KEYB,PB,ST)
        IF (ST .EQ. 0 ) THEN
           CMSG1 = ' Digite o novo preço '
	   ST = SMG$PUT_CHARS (DISP2, CMSG1, 6, 25,,)        
           CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,ECFPRICE,6,50,5,1,65535,ST)
           CHANGED = .TRUE.
        ENDIF
           
        CMSG = ' Deseja alterar Multi-Semanas?' 
        CALL MENU_CONFIRM(CMSG,10,4,10,35,DISP2,KEYB,PB,ST)
        IF (ST .EQ. 0 ) THEN
           DO I=1,56
              MULTWEEK(I) = 0
           ENDDO
           MULTWEEK(1) = 1
           IND = 0 
           CMSG2 = ' Digite o número de semanas '
	   ST = SMG$PUT_CHARS (DISP2, CMSG2, 12, 25,,)        
           CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,IND,12,55,2,1,56,ST)
           CHANGED = .TRUE.
10         CONTINUE
           MULTWEEK(IND) = 1
           CMSG = ' Deseja incluir mais semanas?' 
           CALL MENU_CONFIRM(CMSG,14,4,10,35,DISP2,KEYB,PB,ST)
           IF (ST .EQ. 0 ) THEN
              IND = 0 
              CMSG2 = ' Digite o número de semanas '
	      ST = SMG$PUT_CHARS (DISP2, CMSG2, 16, 25,,)
              CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,IND,16,55,2,1,56,ST)
              MULTWEEK(IND) = 1
              CMSG = '                                                '
              ST = SMG$PUT_CHARS (DISP2,CMSG , 16, 25,,SMG$M_ERASE_LINE)
              GOTO 10
           ENDIF
           
           ECFBITMAP1 = 0
           ECFBITMAP2 = 0
           ECFBITMAP3 = 0
           ECFBITMAP4 = 0
           ECFBITMAP5 = 0
           ECFBITMAP6 = 0
           ECFBITMAP7 = 0
           
           DO IND=1,8
              IF (MULTWEEK(IND)    .EQ. 1) CALL SETBIT_BSTRNG(ECFBITMAP1,IND-1)
              IF (MULTWEEK(IND+8)  .EQ. 1) CALL SETBIT_BSTRNG(ECFBITMAP2,IND-1) 
              IF (MULTWEEK(IND+16) .EQ. 1) CALL SETBIT_BSTRNG(ECFBITMAP3,IND-1) 
              IF (MULTWEEK(IND+24) .EQ. 1) CALL SETBIT_BSTRNG(ECFBITMAP4,IND-1) 
              IF (MULTWEEK(IND+32) .EQ. 1) CALL SETBIT_BSTRNG(ECFBITMAP5,IND-1) 
              IF (MULTWEEK(IND+40) .EQ. 1) CALL SETBIT_BSTRNG(ECFBITMAP6,IND-1) 
              IF (MULTWEEK(IND+48) .EQ. 1) CALL SETBIT_BSTRNG(ECFBITMAP7,IND-1) 
           ENDDO
        ENDIF
        
        ST = SMG$ERASE_DISPLAY(DISP2)
        CMSG = ' Deseja alterar o numero maximo de apostas?' 
        CALL MENU_CONFIRM(CMSG,4,4,10,50,DISP2,KEYB,PB,ST)
        IF (ST .EQ. 0 ) THEN
           CMSG = ' Digite o novo numero maximo de apostas '
	   ST = SMG$PUT_CHARS (DISP2, CMSG, 6, 11,,)        
           CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,ECFMAXBETS,6,60,5,1,65535,ST)
           CHANGED = .TRUE.
        ENDIF
        CALL CLOSEFIL(FDB)
        
        IF (CHANGED) THEN
           CMSG = ' DESEJA GRAVAR AS ALTERAÇÕES?' 
           CALL MENU_CONFIRM(CMSG,8,4,10,35,DISP2,KEYB,PB,ST)
           IF (ST .EQ. 0 ) THEN
              CALL OPENX(1,'EUROCONF.FIL',4,0,0,ST)
              CALL IOINIT(FDB,1,ECFSEC*256)
              IF(ST .NE. 0) THEN
                 CMSG = ' Erro ao abrir o ficheiro <EUROCONF.FIL>...'
	         CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
                 RETURN
              ENDIF
              ECFGREVSON = ECFGREVSON + 1  
              ECFGTGISON = '89'X
              ECFGOPTSON = '0004'X
              CALL WRITEW(FDB,1,ECFREC,ST)
              IF(ST .NE. 0) THEN
                CMSG = ' Erro ao escrever no ficheiro <EUROCONF.FIL>...'
	        CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
	        RETURN
              ENDIF 
              CALL CLOSEFIL(FDB) 
           ENDIF
        ENDIF

        
        ST = SMG$ERASE_DISPLAY(DISP2)
C 
         RETURN 
100      FORMAT (Z2.2)
         END
C******************************************************************
C SUBROUTINE LIST_FILE (LIST DATA FROM EUROCONF.FIL)
C******************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE LIST_FILE( KEYB, DISP2, DISP3)
        IMPLICIT NONE  
     
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:EUROCONFIG.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'        
        
        INTEGER*4 ST,FDB(7)
        LOGICAL ONDISK
C        COMMON ECFREC
        
        INTEGER KEYB,			      ! VIRTUAL KEYBOARD
     *        DISP2,			      ! output data frame window
     *        DISP3			      ! message frame window

        CHARACTER*78  cMSG,cMSG4
        CHARACTER*39  cMSG1
        CHARACTER*16  cMSG2
        CHARACTER*5  cMSG3
        INTEGER KEY
        INTEGER*4 MULTWEEK(56),IND,I
        ECFFRE(1) = 0 
C
C CREATES MENU 
C
        ST = SMG$ERASE_DISPLAY(DISP2)
        CMSG = 'Lendo Configurações Registadas. Aguarde...'        
        ST = SMG$PUT_CHARS (DISP3, CMSG, 1, 1, SMG$M_ERASE_LINE)

        INQUIRE(FILE='SYSX:EUROCONF.FIL', EXIST=ONDISK)
        IF (.NOT. ONDISK) THEN
           CMSG = ' O ficheiro <EUROCONF.FIL> não existe no Millennium...'
	   CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
	   RETURN
        ENDIF
C
C
C        
        CALL OPENX(1,'SYSX:EUROCONF.FIL',4,0,0,ST)
        CALL IOINIT(FDB,1,ECFSEC*256)
        IF(ST .NE. 0) THEN
          CMSG = ' Erro ao abrir o ficheiro <EUROCONF.FIL>...'
	  CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
        CALL READW(FDB,1,ECFREC,ST)
        IF(ST.NE.0) THEN
          CMSG = ' Erro ao ler o ficheiro <EUROCONF.FIL>...'
	  CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
          RETURN
        ENDIF
        CMSG1 = ' Dados de Configuração do Euro Milhões '
        ST = SMG$PUT_CHARS (DISP2, CMSG1, 2, 20,,)
        CMSG = 'Para voltar ao menu anterior prima qualquer tecla...'
      	ST = SMG$PUT_CHARS (DISP3, CMSG, 1, 1, SMG$M_ERASE_LINE)  

        CMSG2 = 'Estado E.M.  :  '
        IF (ECFACTIVE .EQ. 0) THEN 
          CMSG4 = 'DESACTIVADO'
        ELSE
          CMSG4 = 'ACTIVADO'
        ENDIF
        ST = SMG$PUT_CHARS (DISP2, CMSG2, 4, 15,, )
        ST = SMG$PUT_CHARS (DISP2, CMSG4, 4, 31,, SMG$M_BOLD)
      	
        WRITE(CMSG3,100) ECFGREVSON
        CMSG2 = 'Game Revision:  '        
        ST = SMG$PUT_CHARS (DISP2, CMSG2, 6, 15,, )
        ST = SMG$PUT_CHARS (DISP2, CMSG3, 6, 31,, SMG$M_BOLD)
      
        WRITE(CMSG3,101) ISHFT(ECFGTGISON,-3)
        CMSG2 = 'Game Type    :  '        
        ST = SMG$PUT_CHARS (DISP2, CMSG2, 8, 15,,)
        ST = SMG$PUT_CHARS (DISP2, CMSG3, 8, 31,, SMG$M_BOLD)
        
        WRITE(CMSG3,102) IAND(ECFGTGISON,'07'X)
        CMSG2 = 'Game Index   :  '        
        ST = SMG$PUT_CHARS (DISP2, CMSG2, 10, 15,,)
        ST = SMG$PUT_CHARS (DISP2, CMSG3, 10, 31,, SMG$M_BOLD)
        
        WRITE(CMSG3,103) ECFPRICE
        CMSG2 = 'Preço        :  '        
        ST = SMG$PUT_CHARS (DISP2, CMSG2, 12, 15,,)
        ST = SMG$PUT_CHARS (DISP2, CMSG3, 12, 31,, SMG$M_BOLD)
        
        CMSG2 = 'Multi-Semanas:  '        
        ST = SMG$PUT_CHARS (DISP2, CMSG2, 14, 15,,)

        DO I=1,56
           MULTWEEK(I) = 0
        ENDDO
        
        IND = 1
        IF (ECFBITMAP1 .NE. 0) THEN
           DO I=0 , 7 
              IF (TSTBIT_BSTRNG (ECFBITMAP1,I)) MULTWEEK(IND) = 1
              IND = IND + 1
           ENDDO
        ENDIF
        IF (ECFBITMAP2 .NE. 0) THEN
           DO I=0 , 7 
              IF (TSTBIT_BSTRNG (ECFBITMAP2,I)) MULTWEEK(IND) = 1
              IND = IND + 1
           ENDDO
        ENDIF
        IF (ECFBITMAP3 .NE. 0) THEN
           DO I=0 , 7 
              IF (TSTBIT_BSTRNG (ECFBITMAP3,I)) MULTWEEK(IND) = 1
              IND = IND + 1
           ENDDO
        ENDIF
        IF (ECFBITMAP4 .NE. 0) THEN
           DO I=0 , 7 
              IF (TSTBIT_BSTRNG (ECFBITMAP4,I)) MULTWEEK(IND) = 1
              IND = IND + 1
           ENDDO
        ENDIF
        IF (ECFBITMAP5 .NE. 0) THEN
           DO I=0 , 7 
              IF (TSTBIT_BSTRNG (ECFBITMAP5,I)) MULTWEEK(IND) = 1
              IND = IND + 1
           ENDDO
        ENDIF
        IF (ECFBITMAP6 .NE. 0) THEN
           DO I=0 , 7 
              IF (TSTBIT_BSTRNG (ECFBITMAP6,I)) MULTWEEK(IND) = 1
              IND = IND + 1
           ENDDO
        ENDIF
        IF (ECFBITMAP7 .NE. 0) THEN
           DO I=0 , 7 
              IF (TSTBIT_BSTRNG (ECFBITMAP7,I)) MULTWEEK(IND) = 1
              IND = IND + 1
           ENDDO
        ENDIF
        IND = 0 
        DO I=1 ,56
           IF (MULTWEEK(I) .NE. 0) THEN
                WRITE(CMSG3,104) I
           	ST = SMG$PUT_CHARS (DISP2, CMSG3, 14, 31 + IND,, SMG$M_BOLD)
           	IND = IND + 2
           ENDIF
        ENDDO
        
        WRITE(CMSG4,105) ECFMAXBETS
        CMSG2 = 'Max # apostas:  '        
        ST = SMG$PUT_CHARS (DISP2, CMSG2, 16, 15,,)
        ST = SMG$PUT_CHARS (DISP2, CMSG4, 16, 31,, SMG$M_BOLD)
        
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
        END
C******************************************************************
C SUBROUTINE LIST_FILE (LIST DATA FROM EUROCONF.FIL)
C******************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CREATE_FILE( KEYB, DISP2, DISP3,PB)
        IMPLICIT NONE  
     
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:EUROCONFIG.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'        
        
        INTEGER*4 ST,SECTORS,KEY
        LOGICAL ONDISK
        COMMON ECFREC
        INTEGER PB,KEYB,			      ! VIRTUAL KEYBOARD
     *        DISP2,			      ! output data frame window
     *        DISP3			      ! message frame window

        CHARACTER*78  cMSG
        CHARACTER*30  cMSG1 
        
        ECFFRE(1) = 0 
        ST = SMG$ERASE_DISPLAY(DISP2)
        CMSG = 'O utilizador vai criar o ficheiro <EUROCONF.FIL>...'        
        ST = SMG$PUT_CHARS (DISP3, CMSG, 1, 1, SMG$M_ERASE_LINE)
        
        INQUIRE(FILE='SYSX:EUROCONF.FIL', EXIST=ONDISK)
        IF (ONDISK) THEN
           CMSG = ' O ficheiro <EUROCONF.FIL> já existe no Millennium...'
	   CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
	   RETURN
        ENDIF 
        CMSG = ' A Criar o ficheiro de configuração do Euro Milhões '
        ST = SMG$PUT_CHARS (DISP2, CMSG, 2, 15,,)

        CMSG1 = ' Digite o número de blocos: '
	ST = SMG$PUT_CHARS (DISP2, CMSG1, 6, 15,,)
	CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,SECTORS,6,45,3,200,999,ST)

        WRITE(CMSG,100) SECTORS
        ST = SMG$PUT_CHARS (DISP2, CMSG, 8, 15,,)

        CALL SMG_NEWFIL(7,'SYSX:EUROCONF.FIL',SECTORS,ST)
        IF (ST .NE. 0) THEN
           CMSG = ' Erro o ficheiro <EUROCONF.FIL> não foi criado...'
	   CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
	   RETURN
        ENDIF
        
        CMSG1 = ' Ficheiro Criado...'
	ST = SMG$PUT_CHARS (DISP2, CMSG1, 10, 15,,) 

        ST = SMG$READ_KEYSTROKE(KEYB, KEY, , DISP3)  
        ST = SMG$ERASE_DISPLAY(DISP2)      
        RETURN
100     FORMAT (' Vai ser criado o ficheiro <EUROCONF.FIL> com ',I3.3,' blocos')
        END
C**********************************************
C SUBROUTINE MENU_CONFIRM
C**********************************************
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
        CHARACTER*5 OPTION  /' SIM '/  

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
     *				     SMG$K_HORIZONTAL,,, SMG$M_REVERSE)

	ST = SMG$PASTE_VIRTUAL_DISPLAY(DISP4, PB, LIN+LINEND, COL+COLEND)
	ST = SMG$SELECT_FROM_MENU(IKEYB, DISP4, IOPT, 2,
     *					  ,,,, OPTION, SMG$M_BOLD)
	ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)
	ST = SMG$UNPASTE_VIRTUAL_DISPLAY(DISP4, PB)
	ST = SMG$DELETE_VIRTUAL_DISPLAY (DISP4,PB)
        IF(IOPT .NE. 1) THEN
	    ST = -1
        ELSE
	    ST = 0
        ENDIF

        RETURN
        END
