C
C BLDPLAN.FOR (Change the # of version on variable VERSAO)
C
C V10   01-AUG-2009 FJG Portugal Fiscal Legislation changes
C V1.0  12-DEC-00 CS  INITIAL RELEASE FOR MILLENNIUM
C
C PROGRAM WILL DISPLAY/UPDATE PASSIVE LOTTERY PLAN FILE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1993 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
      PROGRAM BLDPLAN
      IMPLICIT NONE

      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:GTECHSMG.DEF'
C
C LOCAL VARIABLES
C
      INTEGER*4	      TAMMENU
      PARAMETER	      (TAMMENU = 6)

      CHARACTER*5     VERSAO     /'v10.0'/

      CHARACTER*27    MYMENU(TAMMENU) /'[      MOSTRAR PLANO      ]',
     *                                 '[      DEFINIR PLANO      ]',
     *                                 '[      ALTERAR PLANO      ]',
     *                                 '[LISTAR PLANOS CADASTRADOS]',
     *                                 '[ LISTAR TIPOS DE PREMIOS ]',
     *                                 '[          SAIR           ]'/

      CHARACTER*78  cMSG

      INTEGER PB,			      ! PASTEBOARD
     *        KEYB,			      ! VIRTUAL KEYBOARD
     *        DISP1,			      ! header frame window
     *        DISP2,			      ! output data frame window
     *        DISP3,			      ! message frame window
     *	      DISPMENU			      ! INITIAL MENU

	INTEGER DEFAULT/1/
	INTEGER*4 ST,MENUOPT			      
C
C CREATES SCREEN HEADER - DISP1
C
      ST = SMG$CREATE_VIRTUAL_DISPLAY( 1, 78, DISP1, SMG$M_BORDER)
      ST = SMG$LABEL_BORDER (DISP1,' SCML ',
     *			     SMG$K_TOP, 70,
     *                       SMG$M_BLINK, SMG$M_BOLD)

      cMSG = 'PLANOS DE EXTRACAO DE LOTARIA NACIONAL DE BILHETES'
      ST   = SMG$PUT_CHARS (DISP1, cMSG, 1, 12)
      ST   = SMG$PUT_CHARS (DISP1, VERSAO, 1, 66)
C
C CREATES INPUT DISPLAY - DISP2
C
      ST = SMG$CREATE_VIRTUAL_DISPLAY ( 16, 78, DISP2, SMG$M_BORDER)
      ST = SMG$LABEL_BORDER (DISP2,' Dados do Plano ',
     *                       SMG$K_TOP, 60, SMG$M_REVERSE)
C
C CREATES MESSAGE AND ERROR DISPLAY - DISP3
C
      ST = SMG$CREATE_VIRTUAL_DISPLAY ( 1, 78, DISP3, SMG$M_BORDER)
      ST = SMG$LABEL_BORDER (DISP3,' Mensagem ', SMG$K_TOP, 65, SMG$M_REVERSE)
C
C CREATES VIRTUAL  KEYBOARD - KEYB 
C (inhibit numeric keypad mode if debug)
C
      ST = SMG$CREATE_VIRTUAL_KEYBOARD (KEYB)         
C
C CREATES PASTEBOARD - PB 
C
      ST = SMG$CREATE_PASTEBOARD (PB)
      IF (.NOT.ST) CALL LIB$STOP (%VAL(ST))
C
C CREATES MENU 
C
      ST = SMG$CREATE_VIRTUAL_DISPLAY(13, 30, DISPMENU)
      ST = SMG$CREATE_MENU(DISPMENU, MYMENU, SMG$K_VERTICAL, 
     *			   SMG$M_DOUBLE_SPACE, 2, SMG$M_REVERSE)
C
C PASTING  VIRTUAL  DISPLAYS  ON  TERMINAL  SCREEN 
C
      ST = SMG$PASTE_VIRTUAL_DISPLAY (DISP1, PB,  2, 2)
      ST = SMG$PASTE_VIRTUAL_DISPLAY (DISP2, PB,  5, 2)
      ST = SMG$PASTE_VIRTUAL_DISPLAY (DISP3, PB, 23, 2)

      ST = SMG$SET_CURSOR_MODE (PB, SMG$M_CURSOR_ON )
      ST = SMG$ERASE_DISPLAY (DISP3)
	
	MENUOPT = 0
	DO WHILE(MENUOPT.NE.TAMMENU)
      	    cMSG = '    Use setas para movimentar cursor e <ENTER> '
     *           //'para selecionar '
      	    ST = SMG$PUT_CHARS (DISP3, cMSG, 1,1)
	    ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_ON)

      	    ST = SMG$PASTE_VIRTUAL_DISPLAY(DISPMENU, PB, 7, 25)
      	    ST = SMG$SELECT_FROM_MENU(KEYB, DISPMENU, MENUOPT, 
     *				       DEFAULT,,,,,,SMG$M_BOLD)
	    ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)
      	    ST = SMG$UNPASTE_VIRTUAL_DISPLAY (DISPMENU,PB)

	    ST = SMG$ERASE_DISPLAY(DISP2)
	    ST = SMG$ERASE_DISPLAY(DISP3)

	    IF(MENUOPT.EQ.1) THEN		
		CALL DISP_PLAN(DISP2,DISP3,KEYB,PB)
	    ELSEIF(MENUOPT.EQ.2) THEN
		CALL DEF_PLAN(DISP2,DISP3,KEYB,PB)		
		ST = SMG$ERASE_DISPLAY(DISP2)
	    ELSEIF(MENUOPT.EQ.3) THEN
		CALL UPD_PLAN(DISP2,DISP3,KEYB,PB)		
		ST = SMG$ERASE_DISPLAY(DISP2)
	    ELSEIF(MENUOPT.EQ.4) THEN
		CALL DSP_ALLPLANS(DISP2,DISP3,KEYB)
	    ELSEIF(MENUOPT.EQ.5) THEN
		CALL DSP_PRZTYP(DISP2,DISP3,KEYB)
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

      CALL GSTOP (GEXIT_SUCCESS)
      END
C
C**********************************************
C SUBROUTINE DISP_PLAN
C**********************************************
C DISPLAY INFORMATION FOR SPECIFIED PLAN
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DISP_PLAN(DISP2,DISP3,KEYB,PB)
	IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DPPREC.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
        INCLUDE 'INCLIB:PASNAM.DEF'

	CHARACTER*80 CMSG,CTIT
	CHARACTER*14 CAUX

	INTEGER	    KEYB,KEY,DISP2,DISP3,PB
	INTEGER*4   PLAN,UNIT,ST,FDB(7),DIVS
	INTEGER*4   TOTSHR, LINHA, DIG
        INTEGER*8   TOTSHV
	REAL*8      R8VAL
C
C FUNCTIONS
C
        INTEGER*4 PAS_ROUND_VALUE
C
C
C

	CTIT = 
     *       'DIVISAO    GANHADORES           PREMIO       TIPO      BOLAS   DIGITOS    DO'	

C	CTIT =
C     *      'DIVISAO          GANHADORES           PREMIO          TIPO       DIGITOS  DO'

	CALL GET_PLANNO(PLAN,DISP2,DISP3,KEYB,PB)

	IF  (PLAN.NE.0) THEN
C
C	  FIND UNIT FOR FILE
C
	  CALL FIND_AVAILABLE_LUN(UNIT,ST)
	  IF (ST.NE.0) THEN
	     CMSG = ' Nao ha handler disponivel para abrir o arquivo. '
     *            //'Tecle <Return>'
	     CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
	  ELSE
	     CALL OPENW(UNIT,SFNAMES(1,PPF),4,0,0,ST)
	     IF (ST.NE.0) THEN
	        CMSG = ' Erro de abertura no ficheiro de planos; '
     *             //'Qualquer tecla prossegue'
		CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
	     ELSE
		CALL IOINIT(FDB,UNIT,DPPSEC*256)
		CALL READW(FDB,PLAN,DPPREC,ST)
		CALL CLOSEFIL(FDB)
		IF(ST.NE.0) THEN
		    CMSG = ' Erro de leitura no ficheiro de planos;'
     *                   //' Qualquer tecla prossegue'
		    CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
		ELSE
C 
C FIRST SCREEN
C
		    ST = SMG$ERASE_DISPLAY(DISP2)

		    WRITE (CMSG,100) DPPPLAN
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 2, 2)

		    WRITE (CMSG,200) DPPNUMTCK
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 4, 2)

		    WRITE (CMSG,300) CMONY(DPPPRC,6,VALUNIT)
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 6, 2)

		    WRITE (CMSG,400) DPPDIV
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 8, 2)

C**		    WRITE (CMSG,500) DPPRNUM
C**		    ST = SMG$PUT_CHARS (DISP2, CMSG, 10, 2)

		    WRITE (CMSG,600) DPPNOFFRA
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 10, 2)

	            IF (DPPPLT.EQ.0) THEN
                        CAUX = '--------------'
		    ELSE
			CAUX = NAMPLANTYP(DPPPLT)
		    ENDIF

		    WRITE (CMSG,700) CAUX
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 12, 2)

		    CMSG = ' Qualquer tecla prossegue'
		    CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)

		    TOTSHR  = 0
		    TOTSHV  = 0

		    IF (DPPEXSHV(1).GT.0) THEN
		        ST = SMG$ERASE_DISPLAY(DISP2)
			CMSG = 'SERIE NAO SORTEADA'
		        ST = SMG$PUT_CHARS (DISP2, CMSG, 1,31)
		        ST = SMG$PUT_CHARS (DISP2, CTIT, 2, 2)
			LINHA = 4
			DO DIVS=1,PAGEDV
                           DIG = 1
                           DO WHILE(FATOR(DIG).NE.DPPDIG(DIVS).AND.DIG.LE.5)
                              DIG = DIG + 1
                           ENDDO

                           IF (DIG.GT.5) DIG = 0
                           CAUX = NAMPRZTYP(DPPTYP(DIVS))

			   IF (DPPEXSHR(DIVS).NE.0) THEN
			     R8VAL = DFLOAT(DPPEXSHV(DIVS))/10000.00
                             WRITE(CMSG,900) 
     *                         DIVS,DPPEXSHR(DIVS),
C     *                         CMONY(PAS_ROUND_VALUE(DPPEXSHV(DIVS)),12,VALUNIT),
     *			       R8VAL,
     *                         CAUX,DPPWNUM(DIVS),DIG,DPPIDNUM(DIVS)

                             ST = SMG$PUT_CHARS (DISP2, CMSG, LINHA, 2)
                             LINHA = LINHA +1
			     TOTSHR  = TOTSHR + DPPEXSHR(DIVS)
		             TOTSHV  = TOTSHV + (DPPEXSHV(DIVS)*DPPEXSHR(DIVS))
                           ENDIF
			ENDDO

		        CMSG = ' Qualquer tecla prossegue'
		        CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)

		    ENDIF
C
C SECOND SCREEN
C
		    ST = SMG$ERASE_DISPLAY(DISP2)
		    ST = SMG$PUT_CHARS (DISP2, CTIT, 1, 2)

		    LINHA   = 3
		    DO DIVS=1,MIN(14,DPPDIV)
			DIG = 1
			DO WHILE(FATOR(DIG).NE.DPPDIG(DIVS).AND.DIG.LE.5)
			    DIG = DIG + 1
			ENDDO

			IF (DIG.GT.5) DIG = 0
			IF (DPPTYP(DIVS).EQ.0) THEN
			    CAUX = '-----------'
			ELSE
			    CAUX = NAMPRZTYP(DPPTYP(DIVS))
			ENDIF

			R8VAL = DFLOAT(DPPSHV(DIVS))/10000.00
			WRITE(CMSG,900) 
     *                     DIVS,DPPSHR(DIVS),
C     *		           CMONY(PAS_ROUND_VALUE(DPPSHV(DIVS)),12,VALUNIT),
     *                     R8VAL,
     *                     CAUX,DPPWNUM(DIVS),DIG,DPPIDNUM(DIVS)
			ST = SMG$PUT_CHARS (DISP2, CMSG, LINHA, 2)
C
C			ADD VALUES TO TOTAL
C
			LINHA   = LINHA  + 1
			TOTSHR  = TOTSHR + DPPSHR(DIVS)
			TOTSHV  = TOTSHV + (DPPSHV(DIVS)*DPPSHR(DIVS))
		    ENDDO
C
C THIRD SCREEN, IF NECESSARY
C
		    IF(DPPDIV.GT.14) THEN
			CMSG = ' Qualquer tecla prossegue'
			CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)

			ST = SMG$ERASE_DISPLAY(DISP2)
			ST = SMG$PUT_CHARS (DISP2, CTIT, 1, 2)

			LINHA = 3
			DO DIVS=15,MIN(PAGDIV,DPPDIV)
			    DIG = 1
                            DO WHILE(FATOR(DIG).NE.DPPDIG(DIVS).AND.DIG.LE.5)
                               DIG = DIG + 1
                            ENDDO

                            IF (DIG.GT.5) DIG = 0
                            IF (DPPTYP(DIVS).EQ.0) THEN
                                CAUX = '-----------'
                            ELSE
                                CAUX = NAMPRZTYP(DPPTYP(DIVS))
                            ENDIF

			    R8VAL = DFLOAT(DPPSHV(DIVS))/10000.00
			    WRITE(CMSG,900) 
     *                         DIVS,DPPSHR(DIVS),
C     *			       CMONY(PAS_ROUND_VALUE(DPPSHV(DIVS)),12,VALUNIT),
     *                         R8VAL,
     *                         CAUX,DPPWNUM(DIVS),DIG,DPPIDNUM(DIVS)

			    ST = SMG$PUT_CHARS (DISP2, CMSG, LINHA, 2)
C
C			    ADD VALUES TO TOTAL
C
			    LINHA   = LINHA   + 1
			    TOTSHR  = TOTSHR  + DPPSHR(DIVS)
			    TOTSHV  = TOTSHV  + (DPPSHV(DIVS)*DPPSHR(DIVS))
			ENDDO
		    ENDIF
C
C		    SHOW TOTAL LINE
C
		    ST = SMG$PUT_CHARS (DISP2, '----------', LINHA, 12)
		    ST = SMG$PUT_CHARS (DISP2, '--------------', LINHA, 28)
C**		    ST = SMG$PUT_CHARS (DISP2, '-----------', LINHA, 39)
		    ST = SMG$PUT_CHARS (DISP2, 'TOTAL', LINHA+1, 1)

		    WRITE(CMSG,950) TOTSHR,
     *                              CMONY(PAS_ROUND_VALUE(TOTSHV),13,VALUNIT)
		    ST = SMG$PUT_CHARS (DISP2, CMSG, LINHA+1, 13)

		    CMSG = ' Qualquer tecla prossegue'
		    CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
		    ST = SMG$ERASE_DISPLAY(DISP2)
		ENDIF
	     ENDIF
          ENDIF
	ENDIF

	RETURN
C
C FORMAT STATEMENTS
C
100	FORMAT(' PLANO DE EXTRACAO NUMERO           : ',I6)
200	FORMAT(' QUANTIDADE DE BILHETES POR SERIE   : ',I6)
300	FORMAT(' PRECO DE CADA FRACCAO              : ',A6)
400	FORMAT(' NUMERO DE DIVISOES                 : ',I6)
500	FORMAT(' QTD. DE NUMEROS REGULARES SORTEADOS: ',I6)
600	FORMAT(' NUMERO DE FRACOES DO BILHETE       : ',I6)
700	FORMAT(' TIPO DA EMISSAO                    : ',A14)
900	FORMAT(1X,I3,T12,I6,T27,F13.4,T44,A11,T58,I2.2,T67,I2.2,T75,I2.2)
950	FORMAT(I8,8X,A13)
	END
C
C**********************************************
C SUBROUTINE DEF_PLAN
C**********************************************
C DEFINE INFORMATION FOR SPECIFIED PLAN
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DEF_PLAN(DISP2,DISP3,KEYB,PB)
	IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GTECHSMG.DEF'
	INCLUDE 'INCLIB:PASNAM.DEF'
	INCLUDE 'INCLIB:DPPREC.DEF'

	CHARACTER*5  CONFIRM(2)/' SIM ', ' NAO '/
	CHARACTER*5  OPTION  /' SIM '/  
	CHARACTER*75 CMSG
	CHARACTER*16 CPLAN
	CHARACTER*14 CPRZ
	CHARACTER*2  CENT

	INTEGER   KEYB,KEY,DISP2,DISP3,DISP4,PB
	INTEGER*4 PLAN,UNIT,ST,FDB(7),DIVS
	INTEGER*4 IOPT
	INTEGER*4 LINE,ALLDIVS,DIGITOS,CNT,IND,EXTDIVS,LEN
	LOGICAL REPEAT
C
        INTEGER*4 DPPPRCAUXINT(2)
        INTEGER*8 DPPPRCAUX
        EQUIVALENCE(DPPPRCAUX, DPPPRCAUXINT)
C
C	FIND UNIT FOR FILE
C
	CALL FIND_AVAILABLE_LUN(UNIT,ST)
	IF  (ST.NE.0) THEN
	    CMSG = ' Nao ha handler disponivel para abrir o arquivo. '
     *           //'Tecle <Return>'
	    CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
	    RETURN
	ENDIF

	CALL GET_PLANNO(PLAN,DISP2,DISP3,KEYB,PB)
C
C OPEN FILE
C
	CALL OPENW(UNIT,SFNAMES(1,PPF),4,0,0,ST)
	IF(ST.NE.0) THEN
	    CMSG = ' Erro de abertura no arquivo de planos;'
     *           //' Qualquer tecla prossegue'
	    CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
	ELSE
	    CALL IOINIT(FDB,UNIT,DPPSEC*256)
	    CALL READW(FDB,PLAN,DPPREC,ST)
	    CALL CLOSEFIL(FDB)
	    IF(ST.NE.0) THEN
		CMSG = ' Erro de leitura no arquivo de planos;'
     *               //' Qualquer tecla prossegue'
		CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
	    ELSE
C
C RECEIVE INFORMATION
C
		IF  (DPPPLAN.GT.1.AND.DPPPLAN.LE.1000) THEN
		    CMSG = ' Plano Ja Registado. Use a Opcao ALTERA PLANO.'
     *                   //' Tecle <Return>'
		    CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
		ELSE
		    ST = SMG$ERASE_DISPLAY(DISP2)

		    DPPPLAN = PLAN

      		    ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)
		    CMSG = ' Digite a quantidade de bilhetes por serie '
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 2, 4)
		    CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,DPPNUMTCK,
     *				    2,60,6,1,100000,ST)
		    IF(ST.EQ.-2) RETURN

		    CMSG = ' Digite o preco de cada fraccao'
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 4, 4)
		    REPEAT = .TRUE.
		    DO	WHILE(REPEAT)
                      DPPPRCAUX = DPPPRC
		      CALL SMG_INPMONY(PB,KEYB,DISP2,DISP3,DPPPRCAUX,4,67,6,ST)
		      IF  (ST.EQ.-2) THEN
			RETURN
		      ELSEIF(ST.EQ.-1 .OR. DPPPRCAUX.EQ.0) THEN
		        CMSG = ' VALOR INCORRETO; DIGITE-O NOVAMENTE,'
     *                         //' POR FAVOR'
		        ST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)
		        ST = SMG$RING_BELL(DISP3, 2)
		        DPPPRC = 0
		        REPEAT = .TRUE.
		      ELSE
			REPEAT = .FALSE.
                        DPPPRC = DPPPRCAUXINT(1)
		      ENDIF
		    ENDDO

		    ST = SMG$ERASE_DISPLAY(DISP3)

		    CMSG = ' Digite o numero de divisoes'
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 6, 4)
		    CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,DPPDIV,
     *				    6,68,2,1,PAGDIV,ST)
		    IF(ST.EQ.-2) RETURN

		    CMSG = ' Digite o tipo da extraccao relacionada '
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 8, 4)

                    IND = 1
                    KEY = 0
                    CMSG = 'USE AS SETAS PARA SELECIONAR E <ENTER> PARA CONFIRMAR'
                    ST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)
                    DO WHILE (KEY.NE.SMG$K_TRM_ENTER)
                       WRITE(CPLAN,10) NAMPLANTYP(IND)
                       ST = SMG$PUT_CHARS(DISP2,CPLAN,8,43)

                       ST = SMG$READ_KEYSTROKE(KEYB, KEY, , DISP2)
                       IF (KEY.EQ.SMG$K_TRM_UP) THEN
                           IF (IND.EQ.MAXPLTYP) THEN
                               IND = 1
                           ELSE
                               IND = IND + 1
                           ENDIF
                       ELSEIF (KEY.EQ.SMG$K_TRM_DOWN) THEN
                           IF (IND.EQ.1) THEN
                               IND = MAXPLTYP
                           ELSE
                               IND = IND - 1
                           ENDIF
                       ENDIF
                    ENDDO
		    ST = SMG$ERASE_DISPLAY(DISP3)
		    DPPPLT = IND

		    CMSG = ' Digite o numero de fracoes do bilhete'
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 10, 4)
		    CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,DPPNOFFRA,
     *				    10,68,2,5,NOFFRA,ST)
		    IF(ST.EQ.-2) RETURN

		    CMSG = ' Digite o numero de premios para series restantes (0 - nenhum)'
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 12, 4)
		    CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,EXTDIVS,
     *				        12,68,2,0,PAGEDV,ST)
		    IF(ST.EQ.-2) RETURN

	           IF (DPPPLT.EQ.EM_ESP) THEN
		       EXTDIVS = 1               !ALLOW ONLY 1 EXTRA DIVISION FOR ESPECIAL
	           ENDIF
C
C LAST CHANGE CDC
C
		    DPPCHGCDC = DAYCDC
C
C PRIZE SCREEN(S)
C
		    ST = SMG$ERASE_DISPLAY(DISP2)
		    CMSG = ' Digite a quantidade e valor dos premios, por divisao'
		    ST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)
		    ST = SMG$RING_BELL(DISP3, 2)

		    CNT = 0
		    DO ALLDIVS = 1,DPPDIV,4
		      DO DIVS = ALLDIVS,MIN((ALLDIVS+3),DPPDIV)
			LINE = MOD((DIVS*4-3),16)

			WRITE(CMSG,300) DIVS
			ST = SMG$PUT_CHARS (DISP2, CMSG, LINE, 1)

			WRITE(CMSG,310)
			ST = SMG$PUT_CHARS (DISP2, CMSG, LINE+1, 1)

			WRITE(CMSG,320)
			ST = SMG$PUT_CHARS (DISP2, CMSG, LINE+2, 1)

			IF (DIVS.LE.EXTDIVS) THEN
			    WRITE(CMSG,325)
                            ST = SMG$PUT_CHARS (DISP2, CMSG, LINE+3, 1)
			ENDIF
C===============================================================================
C INI V10
C===============================================================================
			CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,DPPSHR(DIVS),
     *					LINE,35,6,1,999999,ST)
C===============================================================================
C FIN V10
C===============================================================================     
			IF(ST.EQ.-2) RETURN

			REPEAT = .TRUE.
			DO  WHILE(REPEAT)
			    CALL SMG_INPMONY(PB,KEYB,DISP2,DISP3,
     *					     DPPSHV(DIVS),LINE,67,12,ST)
			    IF (ST.EQ.-2) RETURN
			    IF (ST.EQ.-1) THEN
				CMSG = ' VALOR INCORRETO; DIGITE-O NOVAMENTE, POR FAVOR'
				ST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)
				ST = SMG$RING_BELL(DISP3, 2)
				DPPSHV(DIVS) = 0
				REPEAT = .TRUE.
			    ELSE
				IF (DPPSHV(DIVS).GT.0) THEN
				    REPEAT = .FALSE.
				ELSE
				    CMSG = ' VALOR INCORRETO; DIGITE-O NOVAMENTE, POR FAVOR'
				    ST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)
				    ST = SMG$RING_BELL(DISP3, 2)
				ENDIF
			    ENDIF
			    ST = SMG$ERASE_DISPLAY(DISP3)
			ENDDO

                        IND = 1
                        KEY = 0
                        CMSG = 'USE AS SETAS PARA SELECIONAR E <ENTER> PARA CONFIRMAR'
                        ST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)
                        DO WHILE (KEY.NE.SMG$K_TRM_ENTER)
                           WRITE(CPRZ,330) NAMPRZTYP(IND)
                           ST = SMG$PUT_CHARS(DISP2,CPRZ,LINE+1,30)

                           ST = SMG$READ_KEYSTROKE(KEYB, KEY, , DISP2)
                           IF (KEY.EQ.SMG$K_TRM_UP) THEN
                               IF (IND.EQ.MAXPRZTYP) THEN
                                   IND = 1
                               ELSE
                                   IND = IND + 1
                               ENDIF
                           ELSEIF (KEY.EQ.SMG$K_TRM_DOWN) THEN
                               IF (IND.EQ.1) THEN
                                   IND = MAXPRZTYP
                               ELSE
                                   IND = IND - 1
                               ENDIF
                           ENDIF
                        ENDDO
 		        ST = SMG$ERASE_DISPLAY(DISP3)
			DPPTYP(DIVS) = IND

			DIGITOS = 0
		        IF (DPPTYP(DIVS).EQ.PR_CENT) THEN
                            DIGITOS = 3
			    CENT = ITOC(DIGITOS,LEN)
			    ST = SMG$PUT_CHARS (DISP2, CENT, LINE+1,74)
	                ELSE
			    CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,DIGITOS,
     *					    LINE+1,74,1,1,5,ST)
			    IF(ST.EQ.-2) RETURN
                        ENDIF
			DPPDIG(DIVS) = FATOR(DIGITOS)

		        REPEAT = .TRUE.
			DO WHILE (REPEAT)
			      CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,DPPWNUM(DIVS),
     *					      LINE+2,43,2,0,PAGNBR,ST)
			      IF(ST.EQ.-2) RETURN

			      IF (DPPTYP(DIVS).EQ.PR_SEQ) THEN
	                         IF (DPPWNUM(DIVS).EQ.0) THEN
				    CMSG = ' ESTA DIVISAO E UMA SEQUENCIA; BOLAS SAIDAS NAO PODE SER 0'
				    ST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)
				    ST = SMG$RING_BELL(DISP3, 2)
                                 ELSE
                                    REPEAT = .FALSE.
				 ENDIF
			      ELSE
				  REPEAT = .FALSE.
			          ST = SMG$ERASE_DISPLAY(DISP3)
                              ENDIF
			ENDDO

			IF  (DPPTYP(DIVS).NE.PR_SEQ) THEN
			    CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,DPPIDNUM(DIVS),
     *					    LINE+2,72,2,0,DPPDIV,ST)
			    IF(ST.EQ.-2) RETURN
			ELSE
			    DPPIDNUM(DIVS) = 0
			    CENT = ITOC(DPPIDNUM(DIVS),LEN)
			    ST = SMG$PUT_CHARS (DISP2, CENT, LINE+2, 72)
			ENDIF
C
                        IF (DIVS.LE.EXTDIVS) THEN
C===============================================================================
C INI V10
C===============================================================================                          
			    CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,DPPEXSHR(DIVS),
     *					    LINE+3,35,6,1,999999,ST)
C===============================================================================
C FIN V10
C===============================================================================     
			    IF(ST.EQ.-2) RETURN

			    REPEAT = .TRUE.
		     	    DO  WHILE(REPEAT)
			        CALL SMG_INPMONY(PB,KEYB,DISP2,DISP3,
     *					         DPPEXSHV(DIVS),LINE+3,67,12,ST)
			        IF (ST.EQ.-2) RETURN
			        IF (ST.EQ.-1) THEN
				    CMSG = ' VALOR INCORRETO; DIGITE-O NOVAMENTE, POR FAVOR'
				    ST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)
				    ST = SMG$RING_BELL(DISP3, 2)
				    DPPSHV(DIVS) = 0
				    REPEAT = .TRUE.
			        ELSE
				    IF (DPPSHV(DIVS).GT.0) THEN
				       REPEAT = .FALSE.
				    ELSE
				       CMSG = ' VALOR INCORRETO; DIGITE-O NOVAMENTE, POR FAVOR'
				       ST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)
				       ST = SMG$RING_BELL(DISP3, 2)
				    ENDIF
			        ENDIF
			        ST = SMG$ERASE_DISPLAY(DISP3)
			    ENDDO
                        ENDIF
		      ENDDO
		      ST = SMG$ERASE_DISPLAY(DISP2)
		    ENDDO

		    CMSG = ' Deseja gravar dados deste plano de extracao ?'
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 15, 4)
C
C CREATES YES/NO MENU DISPLAY - DISPCONF 
C
		    ST = SMG$CREATE_VIRTUAL_DISPLAY(1, 15, DISP4)
		    ST = SMG$CREATE_MENU(DISP4, CONFIRM,
     *					 SMG$K_HORIZONTAL,,, SMG$M_REVERSE)

		    ST = SMG$PASTE_VIRTUAL_DISPLAY(DISP4, PB, 19, 50)
		    ST = SMG$SELECT_FROM_MENU(KEYB, DISP4, IOPT, 2,
     *					      ,,,, OPTION, SMG$M_BOLD)
		    ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)
		    ST = SMG$UNPASTE_VIRTUAL_DISPLAY(DISP4, PB)
		    ST = SMG$DELETE_VIRTUAL_DISPLAY (DISP4,PB)

		    IF	(IOPT.NE.1) RETURN
C
C FIND UNIT FOR FILE
C
		    CALL FIND_AVAILABLE_LUN(UNIT,ST)
		    IF  (ST.NE.0) THEN
			CMSG = ' Nao ha handler disponivel para abrir o'
     *			     //' arquivo. Tecle <Return>'
			CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
		    ELSE
C
C OPEN FILE
C
			CALL OPENW(UNIT,SFNAMES(1,PPF),4,0,0,ST)
			IF  (ST.NE.0) THEN
			    CMSG = ' Erro de abertura no arquivo de planos;'
     *			         //' Qualquer tecla prossegue'
			    CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
			ELSE
C
C WRITE IT TO FILE
C
			    CALL IOINIT(FDB,UNIT,DPPSEC*256)
			    CALL WRITEW(FDB,PLAN,DPPREC,ST)
			    CALL CLOSEFIL(FDB)
			    IF	(ST.NE.0) THEN
				CMSG = ' Erro de escrita no arquivo de planos;'
     *                               //' Qualquer tecla prossegue'
				CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
			    ENDIF
			ENDIF
		    ENDIF
		    ST = SMG$ERASE_DISPLAY(DISP2)
		    ST = SMG$ERASE_DISPLAY(DISP3)
		ENDIF
	    ENDIF
	ENDIF

	RETURN
C
C FORMAT STATEMENTS
C
10	FORMAT('(',A14,')')
300	FORMAT(' Divisao ',I2.2,' - Qtde.Ganhadores             ',
     *	       'Valor Premio')
310	FORMAT(14X,'Tipo de premio                    ','Digitos')
320	FORMAT(14X,'Bolas saidas                      ','Do')
325	FORMAT(' Restante '4X,'Qtde.Ganhadores                   ','Premio')
330	FORMAT('- ',A11)
335	FORMAT(' - ',A8)
	END
C
C**********************************************
C SUBROUTINE UPD_PLAN
C**********************************************
C UPDATE INFORMATION FOR SPECIFIED PLAN
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE UPD_PLAN(DISP2,DISP3,KEYB,PB)
	IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GTECHSMG.DEF'
	INCLUDE 'INCLIB:PASNAM.DEF'
	INCLUDE 'INCLIB:DPPREC.DEF'

	CHARACTER*5  CONFIRM(2)/' SIM ', ' NAO '/
	CHARACTER*5  OPTION  /' SIM '/  
	CHARACTER*75 CMSG
	CHARACTER*16 CPLAN
	CHARACTER*14 CPRZ
	CHARACTER*2  CENT

	INTEGER   KEYB,KEY,DISP2,DISP3,DISP4,PB
	INTEGER*4 PLAN,UNIT,ST,FDB(7),DIVS
	INTEGER*4 IOPT
	INTEGER*4 LINE,AUXDIV,DIGITOS,IND,EXTDIVS,LEN
C
	LOGICAL REPEAT
C
        INTEGER*4 DPPPRCAUXINT(2)
        INTEGER*8 DPPPRCAUX
        EQUIVALENCE(DPPPRCAUX, DPPPRCAUXINT)
C
C	FIND UNIT FOR FILE
C
	CALL FIND_AVAILABLE_LUN(UNIT,ST)
	IF  (ST.NE.0) THEN
	    CMSG = ' Nao ha handler disponivel para abrir o arquivo.'
     *           //' Tecle <Return>'
	    CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
	    RETURN
	ENDIF

	CALL GET_PLANNO(PLAN,DISP2,DISP3,KEYB,PB)
C
C OPEN FILE
C
	CALL OPENW(UNIT,SFNAMES(1,PPF),4,0,0,ST)
	IF  (ST.NE.0) THEN
	    CMSG = ' Erro de abertura no arquivo de planos;'
     *           //' Qualquer tecla prossegue'
	    CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
	ELSE
	    CALL IOINIT(FDB,UNIT,DPPSEC*256)
	    CALL READW(FDB,PLAN,DPPREC,ST)
	    CALL CLOSEFIL(FDB)
	    IF	(ST.NE.0) THEN
		CMSG = ' Erro de leitura no arquivo de planos;'
     *               //' Qualquer tecla prossegue'
		CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
	    ELSE
C
C RECEIVE INFORMATION
C
		IF  (DPPPLAN.LT.1.OR.DPPPLAN.GT.1000) THEN
		    CMSG = ' Plano NAO Registado. Use a Opcao DEFINE PLANO.'
     *                   //' Tecle <Return>'
		    CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
		ELSE
		    ST = SMG$ERASE_DISPLAY(DISP2)

		    DPPPLAN = PLAN

      		    ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)
		    CMSG = ' Digite a quantidade de bilhetes por serie '
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 2, 4)
		    CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,DPPNUMTCK,
     *				    2,60,6,1,100000,ST)
		    IF(ST.EQ.-2) RETURN

		    CMSG = ' Digite o preco de cada fraccao'
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 4, 4)
		    REPEAT = .TRUE.
		    DO	WHILE(REPEAT)
                      DPPPRCAUX = DPPPRC
		      CALL SMG_INPMONY(PB,KEYB,DISP2,DISP3,DPPPRCAUX,4,67,6,ST)
		      IF  (ST.EQ.-2) THEN
			  RETURN
		      ELSEIF(ST.EQ.-1 .OR. DPPPRCAUX.EQ.0) THEN
		        CMSG = ' VALOR INCORRETO; DIGITE-O NOVAMENTE, POR FAVOR'
			ST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)
			ST = SMG$RING_BELL(DISP3, 2)
			DPPPRC = 0
			REPEAT = .TRUE.
		      ELSE
                        DPPPRC = DPPPRCAUXINT(1)
			REPEAT = .FALSE.
	              ENDIF
		    ENDDO

		    ST = SMG$ERASE_DISPLAY(DISP3)

		    AUXDIV = DPPDIV
		    CMSG = ' Digite o numero de divisoes'
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 6, 4)
		    CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,DPPDIV,
     *				    6,68,2,1,99,ST)
		    IF(ST.EQ.-2) RETURN
C
C		    CLEAR ALL DIVISIONS GREATER THAN DPPDIV
C
		    IF	(DPPDIV.LT.AUXDIV) THEN
			DO  DIVS = DPPDIV+1, AUXDIV
			    DPPSHR(DIVS)  = 0
			    DPPSHV(DIVS)  = 0
			ENDDO
		    ENDIF

		    CMSG = ' Digite o numero de fracoes do bilhete'
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 8, 4)
		    CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,DPPNOFFRA,
     *				    8,68,2,5,NOFFRA,ST)
		    IF(ST.EQ.-2) RETURN
C
		    CMSG = ' Digite o tipo da extraccao relacionada '
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 10, 4)

                    IND = DPPPLT
                    KEY = 0
                    CMSG = 'USE AS SETAS PARA SELECIONAR E <ENTER> PARA CONFIRMAR'
                    ST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)
                    DO WHILE (KEY.NE.SMG$K_TRM_ENTER)
                       WRITE(CPLAN,10) NAMPLANTYP(IND)
                       ST = SMG$PUT_CHARS(DISP2,CPLAN,10,43)

                       ST = SMG$READ_KEYSTROKE(KEYB, KEY, , DISP2)
                       IF (KEY.EQ.SMG$K_TRM_UP) THEN
                           IF (IND.EQ.MAXPLTYP) THEN
                               IND = 1
                           ELSE
                               IND = IND + 1
                           ENDIF
                       ELSEIF (KEY.EQ.SMG$K_TRM_DOWN) THEN
                           IF (IND.EQ.1) THEN
                               IND = MAXPLTYP
                           ELSE
                               IND = IND - 1
                           ENDIF
                       ENDIF
                    ENDDO
		    ST = SMG$ERASE_DISPLAY(DISP3)
		    DPPPLT = IND

	            EXTDIVS = 0
		    DO IND=1,PAGEDV
		       IF (DPPEXSHR(IND).GT.0) THEN
			   EXTDIVS = EXTDIVS + 1
		       ENDIF
		    ENDDO

		    CMSG = ' Digite o numero de premios para series restantes (0 - nenhum)'
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 12, 4)
		    CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,EXTDIVS,
     *				        12,68,2,0,PAGEDV,ST)
		    IF(ST.EQ.-2) RETURN

		    IF (DPPPLT.EQ.EM_ESP) THEN
			EXTDIVS = 1
	            ENDIF
C
C LAST CHANGE CDC
C
		    DPPCHGCDC = DAYCDC
C
C PRIZE SCREEN(S)
C
		    ST = SMG$ERASE_DISPLAY(DISP2)
		    CMSG = ' Digite Campos a Alterar. <Return> na Divisao'
     *			 //' Atualiza, <NEXT> Abandona.'
		    ST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)
		    ST = SMG$RING_BELL(DISP3, 2)

		    CMSG = ' Entre com informacoes da divisao a ser alterada !'
		    ST = SMG$PUT_CHARS (DISP2, CMSG, 1,1)

		    DIVS = 1
		    DO	WHILE(DIVS.NE.0)

			LINE = 3
			WRITE(CMSG,300)
			ST = SMG$PUT_CHARS (DISP2, CMSG, LINE, 1)

			WRITE(CMSG,310)
			ST = SMG$PUT_CHARS (DISP2, CMSG, LINE+1, 1)

			WRITE(CMSG,320)
			ST = SMG$PUT_CHARS (DISP2, CMSG, LINE+2, 1)

			DIVS = 0
			CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,DIVS,
     *				        LINE,10,2,0,DPPDIV,ST)
			IF  (ST.EQ.-2) RETURN
C
C			TEST END OF DATA ENTRY
C
			IF  (DIVS.NE.0) THEN

			    IF (DIVS.LE.EXTDIVS) THEN
			       WRITE(CMSG,325)
                               ST = SMG$PUT_CHARS (DISP2, CMSG, LINE+3, 1)
			    ENDIF
C===============================================================================
C INI V10
C===============================================================================
			    CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,DPPSHR(DIVS),
     *					    LINE,29,6,1,999999,ST)
C===============================================================================
C FIN V10
C===============================================================================     
			    IF(ST.EQ.-2) RETURN

			    REPEAT = .TRUE.
			    DO	WHILE(REPEAT)
				CALL SMG_INPMONY(PB,KEYB,DISP2,DISP3,
     *						 DPPSHV(DIVS),LINE,56,12,ST)
				IF  (ST.EQ.-2) THEN
				    RETURN
				ELSEIF(ST.EQ.-1) THEN
				    CMSG = ' VALOR INCORRETO; DIGITE-O NOVAMENTE, POR FAVOR'
				    ST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)
				    ST = SMG$RING_BELL(DISP3, 2)
				    DPPSHV(DIVS) = 0
				    REPEAT = .TRUE.
				ELSE
				    IF	(DPPSHV(DIVS).GT.0) THEN
					REPEAT = .FALSE.
				    ELSE
					CMSG = ' VALOR INCORRETO; DIGITE-O NOVAMENTE, POR FAVOR'
					ST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)
					ST = SMG$RING_BELL(DISP3, 2)
				    ENDIF
				ENDIF
			    ENDDO

			    IF (DPPTYP(DIVS).GT.0) THEN
                                IND=DPPTYP(DIVS)
	                    ELSE
                                IND = 1
	                    ENDIF
                            KEY = 0
                            CMSG = 'USE AS SETAS PARA SELECIONAR E <ENTER> PARA CONFIRMAR'
                            ST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)
                            DO WHILE (KEY.NE.SMG$K_TRM_ENTER)
                               WRITE(CPRZ,330) NAMPRZTYP(IND)
                               ST = SMG$PUT_CHARS(DISP2,CPRZ,LINE+1,30)

                               ST = SMG$READ_KEYSTROKE(KEYB, KEY, , DISP2)
                               IF (KEY.EQ.SMG$K_TRM_UP) THEN
                                   IF (IND.EQ.MAXPRZTYP) THEN
                                       IND = 1
                                   ELSE
                                       IND = IND + 1
                                   ENDIF
                               ELSEIF (KEY.EQ.SMG$K_TRM_DOWN) THEN
                                   IF (IND.EQ.1) THEN
                                       IND = MAXPRZTYP
                                   ELSE
                                       IND = IND - 1
                                   ENDIF
                               ENDIF
                            ENDDO
 		            ST = SMG$ERASE_DISPLAY(DISP3)
			    DPPTYP(DIVS) = IND

			    IF (DPPDIG(DIVS).GT.0) THEN
				DIGITOS = 1
                                DO WHILE(FATOR(DIGITOS).NE.DPPDIG(DIVS).AND.DIGITOS.LE.5)
                                   DIGITOS = DIGITOS + 1
                                ENDDO
                                IF (DIGITOS.GT.5) DIGITOS = 0
			    ELSE
			        DIGITOS = 0
			    ENDIF

                            IF (DPPTYP(DIVS).EQ.PR_CENT) THEN
                                DIGITOS = 3
                                CENT = ITOC(DIGITOS,LEN)
                                ST = SMG$PUT_CHARS (DISP2, CENT, LINE+1,74)
                            ELSE
                                CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,DIGITOS,
     *                                          LINE+1,74,1,1,5,ST)
                                IF(ST.EQ.-2) RETURN
                            ENDIF
			    DPPDIG(DIVS) = FATOR(DIGITOS)

                            REPEAT = .TRUE.
                            DO WHILE (REPEAT)
                                 CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,DPPWNUM(DIVS),
     *                                           LINE+2,43,2,0,PAGNBR,ST)
                                 IF(ST.EQ.-2) RETURN

                                 IF (DPPTYP(DIVS).EQ.PR_SEQ) THEN
                                    IF (DPPWNUM(DIVS).EQ.0) THEN
                                        CMSG = ' ESTA DIVISAO E UMA SEQUENCIA; BOLAS SAIDAS ESTA ERRADO'
                                        ST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)
                                        ST = SMG$RING_BELL(DISP3, 2)
                                    ELSE
                                        REPEAT = .FALSE.
                                    ENDIF
                                  ELSE
                                    REPEAT = .FALSE.
                                    ST = SMG$ERASE_DISPLAY(DISP3)
                                  ENDIF
                            ENDDO

			    IF  (DPPTYP(DIVS).NE.PR_SEQ) THEN
				CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,DPPIDNUM(DIVS),
     *					        LINE+2,72,2,0,99,ST)
				IF(ST.EQ.-2) RETURN
			    ELSE
				DPPIDNUM(DIVS) = 0
				CENT = ITOC(DPPIDNUM(DIVS),LEN)
				ST = SMG$PUT_CHARS (DISP2, CENT, LINE+2, 72)
			    ENDIF

                            IF (DIVS.LE.EXTDIVS) THEN      
C===============================================================================
C INI V10
C===============================================================================                              
			        CALL SMG_INPNUM(PB,KEYB,DISP2,DISP3,DPPEXSHR(DIVS),
     *					        LINE+3,35,6,1,999999,ST)
C===============================================================================
C FIN V10
C===============================================================================     
			        IF(ST.EQ.-2) RETURN

			        REPEAT = .TRUE.
		     	        DO  WHILE(REPEAT)
			            CALL SMG_INPMONY(PB,KEYB,DISP2,DISP3,
     *					             DPPEXSHV(DIVS),LINE+3,67,
     *                                               12,ST)
			            IF (ST.EQ.-2) RETURN
			            IF (ST.EQ.-1) THEN
				        CMSG = ' VALOR INCORRETO; DIGITE-O NOVAMENTE, POR FAVOR'
				        ST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)
				        ST = SMG$RING_BELL(DISP3, 2)
				        DPPSHV(DIVS) = 0
				        REPEAT = .TRUE.
			            ELSE
				        IF (DPPSHV(DIVS).GT.0) THEN
				            REPEAT = .FALSE.
				        ELSE
				            CMSG = ' VALOR INCORRETO; DIGITE-O NOVAMENTE, POR FAVOR'
				            ST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)
				            ST = SMG$RING_BELL(DISP3, 2)
				        ENDIF
			            ENDIF
			            ST = SMG$ERASE_DISPLAY(DISP3)
			        ENDDO
                            ENDIF

			    ST = SMG$ERASE_DISPLAY(DISP3)
			ENDIF
			ST = SMG$ERASE_DISPLAY(DISP2)
		    ENDDO
C
C CHECK ALL DIVISIONS
C
		    REPEAT = .TRUE.
		    DO  DIVS = 1, DPPDIV
			IF  (DPPSHR(DIVS).EQ.0 .OR.
     *			     DPPSHV(DIVS).EQ.0) REPEAT = .FALSE.
		    ENDDO
		    DO DIVS = 1, EXTDIVS
			IF (DPPEXSHV(DIVS).EQ.0 .OR. DPPEXSHR(DIVS).EQ.0) REPEAT = .FALSE.
	            ENDDO

		    IF  (.NOT. REPEAT) THEN
			CMSG = ' Existem divisoes com Valores ZERADOS.'
     *			     //' Tecle <Return> p/ Sair'
			CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
		    ELSE

			CMSG = ' Deseja gravar dados deste plano de extracao ?'
			ST = SMG$PUT_CHARS (DISP2, CMSG, 15, 4)
C
C CREATES YES/NO MENU DISPLAY - DISPCONF 
C
			ST = SMG$CREATE_VIRTUAL_DISPLAY(1, 15, DISP4)
			ST = SMG$CREATE_MENU(DISP4, CONFIRM,
     *					     SMG$K_HORIZONTAL,,, SMG$M_REVERSE)

			ST = SMG$PASTE_VIRTUAL_DISPLAY(DISP4, PB, 19, 50)
			ST = SMG$SELECT_FROM_MENU(KEYB, DISP4, IOPT, 2,
     *					      ,,,, OPTION, SMG$M_BOLD)
			ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)
			ST = SMG$UNPASTE_VIRTUAL_DISPLAY(DISP4, PB)
			ST = SMG$DELETE_VIRTUAL_DISPLAY (DISP4,PB)

			IF	(IOPT.NE.1) RETURN
C
C FIND UNIT FOR FILE
C
			CALL FIND_AVAILABLE_LUN(UNIT,ST)
			IF  (ST.NE.0) THEN
			    CMSG = ' Nao ha handler disponivel para abrir o'
     *			         //' arquivo. Tecle <Return>'
			    CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
			ELSE
C
C OPEN FILE
C
			    CALL OPENW(UNIT,SFNAMES(1,PPF),4,0,0,ST)
			    IF  (ST.NE.0) THEN
				CMSG = ' Erro de abertura no arquivo de planos;'
     *			             //' Qualquer tecla prossegue'
				CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
			    ELSE
C
C WRITE IT TO FILE
C
				CALL IOINIT(FDB,UNIT,DPPSEC*256)
				CALL WRITEW(FDB,PLAN,DPPREC,ST)
				CALL CLOSEFIL(FDB)
				IF  (ST.NE.0) THEN
				    CMSG = ' Erro de escrita no arquivo de'
     *                                   //' planos; Tecle <Return>'
				    CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
				ENDIF
			    ENDIF
			ENDIF
		    ENDIF
		    ST = SMG$ERASE_DISPLAY(DISP2)
		    ST = SMG$ERASE_DISPLAY(DISP3)
		ENDIF
	    ENDIF
	ENDIF

	RETURN
C
C FORMAT STATEMENTS
C
10	FORMAT('(',A14,')')
300	FORMAT(' Divisao ',2X,' - Qtde.Ganhadores             ',
     *	       'Valor Premio')
310	FORMAT(14X,'Tipo de premio                    ','Digitos')
320	FORMAT(14X,'Bolas saidas                      ','Do')
325	FORMAT(' Restante '4X,'Qtde.Ganhadores                   ','Premio')
330	FORMAT('- ',A11)
	END
C
C**********************************************
C SUBROUTINE DSP_ALLPLANS
C**********************************************
C DISPLAY ALL PLANS
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DSP_ALLPLANS(DISP2,DISP3,KEYB)
	IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GTECHSMG.DEF'
	INCLUDE 'INCLIB:PASNAM.DEF'
	INCLUDE 'INCLIB:DPPREC.DEF'
C
C	PARAMETERS
C
	INTEGER		DISP2,DISP3,KEYB
C
C	LOCAL VARIABLES
C
	LOGICAL		EOF
	INTEGER		IST, KEY
	CHARACTER*80	CMSG, CMSG1, CMSG2

	INTEGER*4	MAXLIN
	PARAMETER	(MAXLIN=16)

	INTEGER*4	UNIT, ST, LINHA, PLAN, FDB(7), MAXPREM, DIV
C
C
	KEY = 0

	IST = SMG$ERASE_DISPLAY(DISP2)

	CMSG2 = '|Plano |Bilhetes |Fracao |Prc. Fracao |Nro. Divs |'
     *        //'Emis.Relacionada|Ganhadores |'
	IST = SMG$PUT_CHARS (DISP2, CMSG2, 1, 1,, SMG$M_REVERSE)

	CMSG1 = 'Lendo Planos Cadastrados. Aguarde...'
      	IST = SMG$PUT_CHARS (DISP3, CMSG1, 1, 1, SMG$M_ERASE_LINE)
C
C	FIND UNIT FOR FILE
C
	CALL FIND_AVAILABLE_LUN(UNIT,ST)
	IF (ST.NE.0) THEN
	   CMSG = ' Nao ha handler disponivel para abrir o arquivo. '
     *          //'Tecle <Return>'
	ELSE
	   CALL OPENW(UNIT,SFNAMES(1,PPF),4,0,0,ST)
	   IF (ST.NE.0) THEN
	      CMSG = ' Erro de abertura no arquivo de planos. '
     *             //'Tecle <Return>'
	   ELSE
	      CALL IOINIT(FDB,UNIT,DPPSEC*256)
	      PLAN  = 1
	      LINHA = 3
	      EOF   = .TRUE.

	      DO WHILE(EOF)
	   	 CALL READW(FDB,PLAN,DPPREC,ST)
		 IF(ST.NE.0) THEN
		   EOF = .FALSE.

		   IF(ST.EQ.144) THEN
		     CMSG = 'Tecle <Return> para Finalizar.'
		   ELSE
		     CMSG = ' Erro de leitura no arquivo de planos. '
     *                    //'Tecle <Return>.'
		   ENDIF
		 ELSE
C
C		   CHECK DPPREC
C
		   IF(DPPPLAN.GE.1.AND.DPPPLAN.LE.1000) THEN
C
C		     COUNT # OF SHRS. AND IF PLAN HAS SWEEPSTAKE
C
		     MAXPREM = 0
		     DO	DIV = 1, DPPDIV
			MAXPREM = MAXPREM + DPPSHR(DIV) 
		     ENDDO

		     DO DIV = 1, PAGEDV
			MAXPREM = MAXPREM + DPPEXSHR(DIV) 
		     ENDDO
		     WRITE(CMSG,100) 
     *                     DPPPLAN, DPPNUMTCK, DPPNOFFRA,
     *                     CMONY(DPPPRC,6,VALUNIT),
     *                     DPPDIV, NAMPLANTYP(DPPPLT), MAXPREM   
	             IST = SMG$PUT_CHARS (DISP2, CMSG, LINHA, 2)
		     LINHA = LINHA + 1

		     IF (LINHA.GT.MAXLIN) THEN
		        CMSG = 'Tecle <Return> para Proxima Tela ou <Next>'
     *                       //'para Sair'
			IST = SMG$PUT_CHARS (DISP3, CMSG, 1, 1,
     *                                        SMG$M_ERASE_LINE)
		        IST = SMG$READ_KEYSTROKE(KEYB, KEY, , DISP3)
			IF (KEY.EQ.iNEXT_KEY) THEN
			   EOF = .FALSE.
			ELSE
		           LINHA = 3

		           IST = SMG$ERASE_DISPLAY(DISP2)

	                   IST = SMG$PUT_CHARS (DISP2, CMSG2, 1, 1,,
     *                                          SMG$M_REVERSE)
			   IST = SMG$PUT_CHARS (DISP3, CMSG1, 1, 1,
     *                                          SMG$M_ERASE_LINE)
			ENDIF
		     ENDIF
		   ENDIF
		   PLAN = PLAN + 1
		 ENDIF
	      ENDDO
	      CALL CLOSEFIL(FDB)
	   ENDIF
	ENDIF
C
	IF (KEY.NE.iNEXT_KEY) THEN
	  CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)
	ENDIF

	IST = SMG$ERASE_DISPLAY(DISP2)
	IST = SMG$ERASE_DISPLAY(DISP3)

	RETURN
C
C FORMAT STATEMENTS
C
100	FORMAT(I4,3X,I6,6X,I2,7X,A6,9X,I2,6X,A14,4X,I6)
	END


C
C**********************************************
C SUBROUTINE DSP_PRZTYP
C**********************************************
C DISPLAY PRIZES TYPE
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DSP_PRZTYP(DISP2,DISP3,KEYB)
	IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GTECHSMG.DEF'
	INCLUDE 'INCLIB:PASNAM.DEF'

	
        INTEGER DISP2,DISP3,KEYB,KEY

	INTEGER*4 ST,LINHA,I

	CHARACTER*80 CMSG1,CMSG

	ST = SMG$ERASE_DISPLAY(DISP2)

	CMSG1 = '                        Numero           Tipo'

        ST = SMG$PUT_CHARS (DISP2, CMSG1, 1, 1,, SMG$M_REVERSE)

	LINHA = 3
	DO I=1,MAXPRZTYP
	   WRITE(CMSG,10) I,NAMPRZTYP(I)
           ST = SMG$PUT_CHARS (DISP2, CMSG, LINHA, 2)
	   LINHA = LINHA + 1
	ENDDO

	CMSG = ' Qualquer tecla prossegue'
	CALL SMG_WERROR(CMSG, DISP3, KEYB, KEY)

        ST = SMG$ERASE_DISPLAY(DISP2)

	RETURN

10	FORMAT(25X,I2.2,10X,A11)
	END
C
C**********************************************
C SUBROUTINE GET_PLANNO
C**********************************************
C PROMPT USER FOR PLAN NUMBER
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GET_PLANNO(IPLAN,DISP2,DISP3,IKEYB,PB)
	IMPLICIT NONE

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GTECHSMG.DEF'
C 
C LOCAL VARIABLES
C
	CHARACTER*55 CMSG

	INTEGER IKEYB,DISP2,DISP3,IST,PB

	INTEGER*4 IPLAN

	IPLAN = 0
	IST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)

      	CMSG = ' Entre com o Numero do Plano: '
      	IST = SMG$PUT_CHARS(DISP2, CMSG, 9, 19, , SMG$M_BOLD)

      	CMSG = ' Digite o numero do plano ou <NEXT> para sair '
      	IST = SMG$PUT_CHARS (DISP3, CMSG, 1,1)

	CALL SMG_INPNUM(PB,IKEYB,DISP2,DISP3,IPLAN,9,52,4,
     *			  1,1000,IST)

	RETURN
	END
