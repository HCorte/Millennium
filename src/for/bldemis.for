C
C BLDEMIS.FOR (Change the # of version on variable VERSAO)
C
C V01 12-DEC-00 CS  INITIAL RELEASE FOR PORTUGAL
C
C MENU TO CHOOSE PASSIVE FOR CHANGES.
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
      PROGRAM BLDEMIS
      IMPLICIT NONE

      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:GTECHSMG.DEF'
C
C LOCAL VARIABLES
C
      INTEGER*4	      TAMMENU
      PARAMETER	      (TAMMENU = 2)

      CHARACTER*5     VERSAO     /'v0.1 '/

      CHARACTER*28    MYMENU(TAMMENU) /'[     LOTARIA NACIONAL     ]',
     *                                 '[          SAIR            ]'/

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
C
C CREATES INPUT DISPLAY - DISP2
C
      ST = SMG$CREATE_VIRTUAL_DISPLAY ( 16, 78, DISP2, SMG$M_BORDER)
      ST = SMG$LABEL_BORDER (DISP2,' Dados da Extracao ',
     *                       SMG$K_TOP, 59, SMG$M_REVERSE)
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
      ST = SMG$CREATE_VIRTUAL_DISPLAY(11, 30, DISPMENU)
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

        cMSG = 'REGISTO PARA LOTERIA NACIONAL DE BILHETES'
        ST   = SMG$PUT_CHARS (DISP1, cMSG, 1, 12)
        ST   = SMG$PUT_CHARS (DISP1, VERSAO, 1, 66)

	cMSG = '    Use setas para movimentar cursor e <ENTER> para selecionar '
      	ST = SMG$PUT_CHARS (DISP3, cMSG, 1,1)
	ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_ON)

      	ST = SMG$PASTE_VIRTUAL_DISPLAY(DISPMENU, PB, 7, 25)

      	ST = SMG$SELECT_FROM_MENU(KEYB, DISPMENU, MENUOPT, 
     *				       DEFAULT,,,,,,SMG$M_BOLD)

	ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)

      	ST = SMG$UNPASTE_VIRTUAL_DISPLAY (DISPMENU,PB)

	IF(MENUOPT.EQ.1) THEN		
	  CALL BLDPASEMIS(PB, KEYB, DISP1, DISP2, DISP3)
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
