C EUROTMIR.FOR
C
C V04 12-APR-2011 FJG ACCENTURE MERGE FOR EM2
C V03 30-MAR-2011 ACN Euromillions Validation Prize over 42M
C                     Euromillions Evolution Project (Matrix with 50 Numbers + 11 Stars)
C                     Fix NOBOUNDS checking errors
C                     Fix Print Validation if validation error (there is no cash amount value to print)
C V02 18-NOV-2010 FJG Batch2: Fix NOBOUNDS checking errors!
C
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM EUROTMIR
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
C       CHARACTER*4  DEVTYP(2)
C       DATA DEVTYP/'DISK','TAPE'/     REMOVE WARNING
        INTEGER*4 ST
        !COMMON ECFREC    <- IS WITHOUT VALUES
        
        INTEGER*4 TAMMENU
        PARAMETER (TAMMENU = 3)
        CHARACTER*78  cMSG
        CHARACTER*5     VERSAO     /'v1.0 '/
        CHARACTER*28    MYMENU(TAMMENU) /'[ DISK ]',
     *                                   '[ TAPE ]',
     *                                   '[ EXIT ]'/         
        INTEGER PB,			      ! PASTEBOARD
     *        KEYB,			      ! VIRTUAL KEYBOARD
     *        DISP1,			      ! header frame window
     *        DISP4,                          ! menu frame window
     *        DISP2,			      ! output data frame window
     *        DISP3,			      ! message frame window
     *	      DISPMENU			      ! INITIAL MENU
        
        INTEGER DEFAULT/1/
        INTEGER*4 MENUOPT
C
C TMIR VAR
C        
        LOGICAL DISK
        CHARACTER*15 TMFFILENAME
C
C DEFAULT VALUES
C
        DISK = .TRUE.
        TMFFILENAME = 'PRIM:MTMF01.FIL'
        ECFFRE(1) = 0 
C
C CREATES SCREEN HEADER - DISP1
C
        ST = SMG$CREATE_VIRTUAL_DISPLAY( 1, 58, DISP1, SMG$M_BORDER)
        ST = SMG$LABEL_BORDER (DISP1,' SCML ',
     *			     SMG$K_TOP, 50,
     *                       SMG$M_BLINK, SMG$M_BOLD)
C
C CREATES OUTPUT DISPLAY - DISP2
C
        ST = SMG$CREATE_VIRTUAL_DISPLAY ( 19, 18, DISP2, SMG$M_BORDER)
c        ST = SMG$LABEL_BORDER (DISP2,' Euro Milhões TMIR ',
c     *                       SMG$K_TOP, 1, SMG$M_REVERSE)
C
C CREATES MESSAGE AND ERROR DISPLAY - DISP3
C
        ST = SMG$CREATE_VIRTUAL_DISPLAY ( 1, 78, DISP3, SMG$M_BORDER)
        ST = SMG$LABEL_BORDER (DISP3,' Mensagem ', SMG$K_TOP, 60, SMG$M_REVERSE)
C
C CREATES INPUT DISPLAY - DISP4
C
        ST = SMG$CREATE_VIRTUAL_DISPLAY ( 16, 58, DISP4, SMG$M_BORDER)
        ST = SMG$LABEL_BORDER (DISP4,' Output Window ',
     *                       SMG$K_TOP, 40, SMG$M_REVERSE)

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
        ST = SMG$CREATE_VIRTUAL_DISPLAY(11, 10, DISPMENU)
        ST = SMG$CREATE_MENU(DISPMENU, MYMENU, SMG$K_BLOCK , 
     *	     SMG$M_DOUBLE_SPACE + SMG$M_WRAP_MENU , 2, 0)
C
C PASTING  VIRTUAL  DISPLAYS  ON  TERMINAL  SCREEN 
C
        ST = SMG$PASTE_VIRTUAL_DISPLAY (DISP1, PB,  2, 22)
        ST = SMG$PASTE_VIRTUAL_DISPLAY (DISP2, PB,  2, 2)
        ST = SMG$PASTE_VIRTUAL_DISPLAY (DISP3, PB, 23, 2)
        ST = SMG$PASTE_VIRTUAL_DISPLAY (DISP4, PB, 5, 22)

        ST = SMG$SET_CURSOR_MODE (PB, SMG$M_CURSOR_OFF )
        ST = SMG$ERASE_DISPLAY (DISP3)
	
        MENUOPT = 0
        DO WHILE(MENUOPT .NE. TAMMENU)
          ST = SMG$SET_CURSOR_MODE (DISP2, SMG$M_CURSOR_OFF )
          ST = SMG$LABEL_BORDER (DISP2,' EuroM TMIR ',
     *                       SMG$K_TOP, 5, SMG$M_REVERSE)
          cMSG = 'Menu'
          ST = SMG$PUT_CHARS (DISP2, cMSG, 2,7,SMG$M_REVERSE,SMG$M_BOLD)
          cMSG = ' Euro Milhões Transaction Master File Report'
          ST   = SMG$PUT_CHARS (DISP1, cMSG, 1, 1)
          ST   = SMG$PUT_CHARS (DISP1, VERSAO, 1, 50)

	  cMSG = 'Use setas para movimentar e <ENTER> para selecionar '
      	  ST = SMG$PUT_CHARS (DISP3, cMSG, 1,1)
	  ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)

      	  ST = SMG$PASTE_VIRTUAL_DISPLAY(DISPMENU, PB, 6, 5)

       	  ST = SMG$SELECT_FROM_MENU(KEYB, DISPMENU, MENUOPT, 
     *				       DEFAULT,,,,,,SMG$M_BOLD + SMG$M_REVERSE )

	  ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)

      	  ST = SMG$UNPASTE_VIRTUAL_DISPLAY (DISPMENU,PB)

	  IF(MENUOPT .EQ. 1) THEN
	     CALL DISK_MENU(KEYB, DISP2, DISP3, DISP4,PB)
	  ELSEIF (MENUOPT .EQ. 2) THEN
	     CALL TAPE_MENU(KEYB, DISP2, DISP3, DISP4,PB)
	  ENDIF

        ENDDO 

        ST = SMG$UNPASTE_VIRTUAL_DISPLAY (DISP1,PB)
        ST = SMG$UNPASTE_VIRTUAL_DISPLAY (DISP2,PB)
        ST = SMG$UNPASTE_VIRTUAL_DISPLAY (DISP3,PB)
        ST = SMG$UNPASTE_VIRTUAL_DISPLAY (DISP4,PB)

        ST = SMG$DELETE_VIRTUAL_DISPLAY (DISP1,PB)
        ST = SMG$DELETE_VIRTUAL_DISPLAY (DISP2,PB)
        ST = SMG$DELETE_VIRTUAL_DISPLAY (DISP3,PB)
        ST = SMG$DELETE_VIRTUAL_DISPLAY (DISP4,PB)
        ST = SMG$DELETE_VIRTUAL_DISPLAY (DISPMENU,PB)

        ST = SMG$DELETE_PASTEBOARD (PB)
 
        CALL GSTOP(GEXIT_SUCCESS)
        END
C******************************************************************
C SUBROUTINE DISK_MENU (READ FROM FILE)
C******************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE DISK_MENU(KEYB, DISP2, DISP3, DISP4,PB)
        IMPLICIT NONE  
     
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
        INCLUDE '($TRMDEF)' 
        
        INTEGER PB,			      ! PASTEBOARD
     *        KEYB,			      ! VIRTUAL KEYBOARD
     *        DISP4,                          ! menu frame window
     *        DISP2,			      ! output data frame window
     *        DISP3,			      ! message frame window
     *	      DISPMENU			      ! DISK MENU
        
        INTEGER*4 TAMMENU,MENUOPT,ST
        PARAMETER (TAMMENU = 5)
        INTEGER DEFAULT/1/
        CHARACTER*78  cMSG
        CHARACTER*28    MYMENU(TAMMENU) /'  APOSTAS',
     *                                   ' VALIDAÇÕES',
     *                                   'CANCELAMENTOS',
     *					 '   TODAS ',
     *                                   '   VOLTAR'/         
        
        
C
C CREATES MENU 
C
        ST = SMG$CREATE_VIRTUAL_DISPLAY(11, 16, DISPMENU)
        ST = SMG$CREATE_MENU(DISPMENU, MYMENU, SMG$K_BLOCK , 
     *	     SMG$M_DOUBLE_SPACE + SMG$M_WRAP_MENU , 2, 0)
        
        MENUOPT = 0
        DO WHILE(MENUOPT .NE. TAMMENU)
          cMSG = 'DISK MENU'
          ST = SMG$PUT_CHARS (DISP2, cMSG, 2,5,SMG$M_REVERSE,SMG$M_BOLD)

	  cMSG = 'Use setas para movimentar e <ENTER> para selecionar '
      	  ST = SMG$PUT_CHARS (DISP3, cMSG, 1,1)
	  ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)

      	  ST = SMG$PASTE_VIRTUAL_DISPLAY(DISPMENU, PB, 6, 3)

       	  ST = SMG$SELECT_FROM_MENU(KEYB, DISPMENU, MENUOPT, 
     *				       DEFAULT,,,,,,SMG$M_BOLD + SMG$M_REVERSE )

	  ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)

      	 

	  IF(MENUOPT .EQ. 1) THEN
	     CALL WAGGERTRANSACT(KEYB, DISP4, DISP3,PB,.TRUE.)
	  ELSEIF (MENUOPT .EQ. 2) THEN
	     CALL VALIDATRANSACT(KEYB, DISP4, DISP3,PB,.TRUE.)
	  ELSEIF (MENUOPT .EQ. 3) THEN
	     CALL CANCELTRANSACT(KEYB, DISP4, DISP3,PB,.TRUE.)
	  ELSEIF (MENUOPT .EQ. 4) THEN
	     CALL ALLTRANSACT(KEYB, DISP4, DISP3,PB,.TRUE.)
	  ENDIF 
	   ST = SMG$UNPASTE_VIRTUAL_DISPLAY (DISPMENU,PB)       
        ENDDO 
        
        RETURN
        END
C******************************************************************
C SUBROUTINE TAPE_MENU (READ FROM FILE)
C******************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TAPE_MENU(KEYB, DISP2, DISP3, DISP4,PB)
        IMPLICIT NONE  
     
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
        INCLUDE '($TRMDEF)' 
        
        INTEGER PB,			      ! PASTEBOARD
     *        KEYB,			      ! VIRTUAL KEYBOARD
     *        DISP4,                          ! menu frame window
     *        DISP2,			      ! output data frame window
     *        DISP3,			      ! message frame window
     *	      DISPMENU			      ! DISK MENU
        
        INTEGER*4 TAMMENU,MENUOPT,ST
        PARAMETER (TAMMENU = 5)
        INTEGER DEFAULT/1/
        CHARACTER*78  cMSG
        CHARACTER*28    MYMENU(TAMMENU) /'  APOSTAS',
     *                                   ' VALIDAÇÕES',
     *                                   'CANCELAMENTOS',
     *					 '   TODAS ',
     *                                   '   VOLTAR'/         
        
        
C
C CREATES MENU 
C
        ST = SMG$CREATE_VIRTUAL_DISPLAY(11, 16, DISPMENU)
        ST = SMG$CREATE_MENU(DISPMENU, MYMENU, SMG$K_BLOCK , 
     *	     SMG$M_DOUBLE_SPACE + SMG$M_WRAP_MENU , 2, 0)
        
        MENUOPT = 0
        DO WHILE(MENUOPT .NE. TAMMENU)
          cMSG = 'TAPE MENU'
          ST = SMG$PUT_CHARS (DISP2, cMSG, 2,5,SMG$M_REVERSE,SMG$M_BOLD)

	  cMSG = 'Use setas para movimentar e <ENTER> para selecionar '
      	  ST = SMG$PUT_CHARS (DISP3, cMSG, 1,1)
	  ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)

      	  ST = SMG$PASTE_VIRTUAL_DISPLAY(DISPMENU, PB, 6, 3)

       	  ST = SMG$SELECT_FROM_MENU(KEYB, DISPMENU, MENUOPT, 
     *				       DEFAULT,,,,,,SMG$M_BOLD + SMG$M_REVERSE )

	  ST = SMG$SET_CURSOR_MODE(PB, SMG$M_CURSOR_OFF)


	  IF(MENUOPT .EQ. 1) THEN
	     CALL WAGGERTRANSACT(KEYB, DISP4, DISP3,PB,.FALSE.)
	  ELSEIF (MENUOPT .EQ. 2) THEN
	     CALL VALIDATRANSACT(KEYB, DISP4, DISP3,PB,.FALSE.)
	  ELSEIF (MENUOPT .EQ. 3) THEN
	     CALL CANCELTRANSACT(KEYB, DISP4, DISP3,PB,.FALSE.)
	  ELSEIF (MENUOPT .EQ. 4) THEN
	     CALL ALLTRANSACT(KEYB, DISP4, DISP3,PB,.FALSE.)
	  ENDIF 
	   ST = SMG$UNPASTE_VIRTUAL_DISPLAY (DISPMENU,PB)       
        ENDDO 
        
        RETURN
        END
C******************************************************************
C SUBROUTINE WAGGERTRANSACT (READ FROM FILE)
C******************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE WAGGERTRANSACT(KEYB, DISP4, DISP3,PB,DISK)
        IMPLICIT NONE          
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
        INCLUDE '($TRMDEF)' 
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:DESLOG.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INTEGER PB,			      ! PASTEBOARD
     *        KEYB,			      ! VIRTUAL KEYBOARD
     *        DISP4,			      ! output data frame window
     *        DISP3			      ! message frame window
C
C TMIR VAR
C        
        LOGICAL DISK,ONDISK
        CHARACTER*15 TMFFILENAME
        CHARACTER*15 REPFILENAME
        CHARACTER*78  cMSG
        INTEGER*4 ST,KEY,TER,EXTSER,EXTCHK,PUNIT
        INTEGER*4   PAGE,SER,EOT,FDB(7)
        INTEGER*4   LOGREC(LREC*3)
        INTEGER*4   FILNAME(7)
        INTEGER*4   LINECOUNT
        LOGICAL NOTFOUND
        
C
C DEFAULT VALUES
C
        TMFFILENAME = 'PRIM:MTMF01.FIL'
        WRITE (REPFILENAME,100) DAYCDC
        PAGE     =  0
        LINECOUNT = 0
C        DISK = .FALSE.
        NOTFOUND = .TRUE.
        IF (DISK) THEN

C
C TMF FILENAME
C
           ST = SMG$PUT_CHARS (DISP4, 'TMF FileName = PRIM:MTMF01.FIL', 2,2)
           
           INQUIRE(FILE=TMFFILENAME, EXIST=ONDISK)
           IF (.NOT. ONDISK) THEN
              WRITE(CMSG,101) TMFFILENAME
	      CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
	      ST = SMG$ERASE_DISPLAY(DISP4)
	      RETURN
           ENDIF
           FILNAME(1) = 'PRIM'
           FILNAME(2) = ':MTM'
           FILNAME(3) = 'F01.'
           FILNAME(4) = 'FIL'
C
C INQUIRE DATA 
C
           CALL INQ_DATA(KEYB,DISP3,DISP4,PB,REPFILENAME,TER,EXTSER,EXTCHK)
           
           CALL OPENWY(1,FILNAME,0,4,0,ST)
           CALL TOPEN(1)
           
           IF(ST .NE. 0) THEN
              CMSG = ' ERRO!!! Ao abrir o ficheiro PRIM:MTMF01.FIL...'
              CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
	      ST = SMG$ERASE_DISPLAY(DISP4)
	      RETURN
           ENDIF
           
           CALL ROPEN(REPFILENAME,PUNIT,ST)
           IF(ST .NE. 0) THEN
              CMSG = ' ERRO!!! Ao abrir o ficheiro de report...'
              CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
	      ST = SMG$ERASE_DISPLAY(DISP4)
	      CALL USRCLOS1(PUNIT)
	      RETURN
           ENDIF
        
           CMSG = ' A ler do ficheiro PRIM:MTMF01.FIL. Por favor aguarde...'
           ST = SMG$PUT_CHARS (DISP3, CMSG, 1, 1,,SMG$M_BLINK)
        ENDIF
             
        CALL TITLE('TRANSACTION FILE REPORT','EUROTMIR',1, PUNIT,PAGE,DAYCDC)
40      CONTINUE
        IF (DISK) CALL READTMF(LOGREC,SER,EOT)
        IF(EOT) GOTO 1000
        
        CALL LOGTRA(TRABUF,LOGREC)
        
        
        IF ((TRABUF(TTYP) .EQ. TEUR) .AND. (TRABUF(TEUTYP) .EQ. TWAG)) THEN
C           CMSG = 'ESTA E UMA TRANSACÇÃO DE EUROMIL'
C 	   ST = SMG$PUT_CHARS (DISP4, CMSG, 10, 5,,)
 	   
 	   IF (TER .NE. -1) THEN
 	      IF (TRABUF(TTER) .EQ. TER ) THEN 	      	
 	         IF (EXTSER .NE. -1) THEN
 	             IF ((TRABUF(TEUSER) .EQ. EXTSER) .AND. (TRABUF(TEUCHK) .EQ. EXTCHK)) THEN
 	               CALL PRINT_BET(TRABUF,PUNIT,LINECOUNT)
 	               NOTFOUND = .FALSE.
                       RETURN
 	             ENDIF
 	             GOTO 40
 	         ENDIF
  	         CALL PRINT_BET(TRABUF,PUNIT,LINECOUNT)
  	         NOTFOUND = .FALSE.
 	      ENDIF
 	      GOTO 40
 	   ENDIF
 	   IF (EXTSER .NE. -1) THEN
 	      IF ((TRABUF(TEUSER) .EQ. EXTSER) .AND. (TRABUF(TEUCHK) .EQ. EXTCHK)) THEN
 	         CALL PRINT_BET(TRABUF,PUNIT,LINECOUNT)
 	         NOTFOUND = .FALSE.
 	         RETURN
 	      ENDIF 
 	      GOTO 40
 	   ENDIF
 	   CALL PRINT_BET(TRABUF,PUNIT,LINECOUNT)
 	   NOTFOUND = .FALSE.
        ENDIF
        GOTO 40
1000    CONTINUE
        ! CALL CLOSEFIL(FDB)  <- NOTHING WAS OPENED
        CALL USRCLOS1(PUNIT)
        
        IF (NOTFOUND) THEN
           IF ((EXTSER .NE. -1) .AND. (TER .NE. -1)) CMSG = ' Aposta de Euro Milhões não foi encontrada para o agente escolhido...'
           IF ((EXTSER .NE. -1) .AND. (TER .EQ. -1)) CMSG = ' Aposta de Euro Milhões não foi encontrada...'
           IF (EXTSER .EQ. -1) CMSG = ' Não existem apostas de Euro Milhões...'
           CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
        ENDIF
        
        ST = SMG$ERASE_DISPLAY(DISP4)             
        RETURN
        
100     FORMAT ('EMAP_',I4.4,'.REP')
101     FORMAT (' O ficheiro <',A15,'> não existe...')
102     FORMAT ('Nome do ficheiro de report = ',A15)
103     FORMAT (7A4)
901     FORMAT(7A4)
300     FORMAT (Z2.2)
        END

C******************************************************************
C SUBROUTINE VALIDATRANSACT (READ FROM FILE)
C******************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE VALIDATRANSACT(KEYB, DISP4, DISP3,PB,DISK)
        IMPLICIT NONE          
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
        INCLUDE '($TRMDEF)' 
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:DESLOG.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INTEGER PB,			      ! PASTEBOARD
     *        KEYB,			      ! VIRTUAL KEYBOARD
     *        DISP4,			      ! output data frame window
     *        DISP3			      ! message frame window
C
C TMIR VAR
C        
        LOGICAL DISK,ONDISK
        CHARACTER*15 TMFFILENAME
        CHARACTER*15 REPFILENAME
        CHARACTER*78  cMSG
        INTEGER*4 ST,KEY,TER,EXTSER,EXTCHK,PUNIT
        INTEGER*4   PAGE,SER,EOT,FDB(7)
        INTEGER*4   LOGREC(LREC*3)
        INTEGER*4   FILNAME(7)
        INTEGER*4   LINECOUNT
        LOGICAL NOTFOUND
        
C
C DEFAULT VALUES
C
        TMFFILENAME = 'PRIM:MTMF01.FIL'
        WRITE (REPFILENAME,100) DAYCDC
        PAGE     =  0
        LINECOUNT = 0
C        DISK = .FALSE.
        NOTFOUND = .TRUE.
        IF (DISK) THEN
C
C TMF FILENAME
C
           ST = SMG$PUT_CHARS (DISP4, 'TMF FileName = PRIM:MTMF01.FIL', 2,2)
           
           INQUIRE(FILE=TMFFILENAME, EXIST=ONDISK)
           IF (.NOT. ONDISK) THEN
              WRITE(CMSG,101) TMFFILENAME
	      CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
	      ST = SMG$ERASE_DISPLAY(DISP4)
	      RETURN
           ENDIF
           FILNAME(1) = 'PRIM'
           FILNAME(2) = ':MTM'
           FILNAME(3) = 'F01.'
           FILNAME(4) = 'FIL'
C
C INQUIRE DATA 
C
           CALL INQ_DATA(KEYB,DISP3,DISP4,PB,REPFILENAME,TER,EXTSER,EXTCHK)
           CALL OPENWY(1,FILNAME,0,4,0,ST)
           CALL TOPEN(1)
           
           IF(ST .NE. 0) THEN
              CMSG = ' ERRO!!! Ao abrir o ficheiro PRIM:MTMF01.FIL...'
              CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
	      ST = SMG$ERASE_DISPLAY(DISP4)
	      RETURN
           ENDIF
           
           CALL ROPEN(REPFILENAME,PUNIT,ST)
           IF(ST .NE. 0) THEN
              CMSG = ' ERRO!!! Ao abrir o ficheiro de report...'
              CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
	      ST = SMG$ERASE_DISPLAY(DISP4)
	      CALL USRCLOS1(PUNIT)
	      RETURN
           ENDIF
           
           CMSG = ' A ler do ficheiro PRIM:MTMF01.FIL. Por favor aguarde...'
           ST = SMG$PUT_CHARS (DISP3, CMSG, 1, 1,,SMG$M_BLINK)
        ENDIF
                
        CALL TITLE('TRANSACTION FILE REPORT','EUROTMIR',1, PUNIT,PAGE,DAYCDC)
        SER = 0
40      CONTINUE
        IF (DISK) CALL READTMF(LOGREC,SER,EOT)
        IF(EOT) GOTO 1000
        CALL LOGTRA(TRABUF,LOGREC)
        
        
        IF ((TRABUF(TTYP) .EQ. TEUR) .AND. (TRABUF(TEUTYP) .EQ. TVAL)) THEN 	   

 	   IF (TER .NE. -1) THEN
 	      IF (TRABUF(TTER) .EQ. TER ) THEN 	      	
 	         IF (EXTSER .NE. -1) THEN
 	             IF ((TRABUF(TEUSER) .EQ. EXTSER) .AND. (TRABUF(TEUCHK) .EQ. EXTCHK)) THEN
 	               CALL PRINT_VALID(TRABUF,PUNIT,LINECOUNT)
 	               NOTFOUND = .FALSE.
                       RETURN
 	             ENDIF
 	             GOTO 40
 	         ENDIF
  	         CALL PRINT_VALID(TRABUF,PUNIT,LINECOUNT)
  	         NOTFOUND = .FALSE.
 	      ENDIF
 	      GOTO 40
 	   ENDIF
 	   IF (EXTSER .NE. -1) THEN
 	      IF ((TRABUF(TEUSER) .EQ. EXTSER) .AND. (TRABUF(TEUCHK) .EQ. EXTCHK)) THEN
 	         CALL PRINT_VALID(TRABUF,PUNIT,LINECOUNT)
 	         NOTFOUND = .FALSE.
 	         RETURN
 	      ENDIF 
 	      GOTO 40
 	   ENDIF
 	   CALL PRINT_VALID(TRABUF,PUNIT,LINECOUNT)
 	   NOTFOUND = .FALSE.
        ENDIF
        GOTO 40
1000    CONTINUE
        ! CALL CLOSEFIL(FDB)  <- NOTHING WAS OPENED
        CALL USRCLOS1(PUNIT)
        
        IF (NOTFOUND) THEN
           IF ((EXTSER .NE. -1) .AND. (TER .NE. -1)) THEN 
              CMSG = ' Validação de Euro Milhões não foi encontrada para o agente escolhido...'
           ENDIF
           IF ((EXTSER .NE. -1) .AND. (TER .EQ. -1)) CMSG = ' Validação de Euro Milhões não foi encontrada...'
           IF (EXTSER .EQ. -1) CMSG = ' Não existem Validações de Euro Milhões...'
           CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
        ENDIF
        
        ST = SMG$ERASE_DISPLAY(DISP4)             
        RETURN
        
100     FORMAT ('EMVA_',I4.4,'.REP')
101     FORMAT (' O ficheiro <',A15,'> não existe...')
102     FORMAT ('Nome do ficheiro de report = ',A15)
103     FORMAT (7A4)
901     FORMAT(7A4)
300     FORMAT (Z8.8)
        END
C******************************************************************
C SUBROUTINE CANCELTRANSACT (READ FROM FILE)
C******************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CANCELTRANSACT(KEYB, DISP4, DISP3,PB,DISK)
        IMPLICIT NONE          
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
        INCLUDE '($TRMDEF)' 
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:DESLOG.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INTEGER PB,			      ! PASTEBOARD
     *        KEYB,			      ! VIRTUAL KEYBOARD
     *        DISP4,			      ! output data frame window
     *        DISP3			      ! message frame window
C
C TMIR VAR
C        
        LOGICAL DISK,ONDISK
        CHARACTER*15 TMFFILENAME
        CHARACTER*15 REPFILENAME
        CHARACTER*78  cMSG
        INTEGER*4 ST,KEY,TER,EXTSER,EXTCHK,PUNIT
        INTEGER*4   PAGE,SER,EOT,FDB(7)
        INTEGER*4   LOGREC(LREC*3)
        INTEGER*4   FILNAME(7)
        INTEGER*4   LINECOUNT
        LOGICAL NOTFOUND
        
C
C DEFAULT VALUES
C
        TMFFILENAME = 'PRIM:MTMF01.FIL'
        WRITE (REPFILENAME,100) DAYCDC
        PAGE     =  0
        LINECOUNT = 0
C        DISK = .FALSE.
        NOTFOUND = .TRUE.
        IF (DISK) THEN
C
C TMF FILENAME
C
           ST = SMG$PUT_CHARS (DISP4, 'TMF FileName = PRIM:MTMF01.FIL', 2,2)
           
           INQUIRE(FILE=TMFFILENAME, EXIST=ONDISK)
           IF (.NOT. ONDISK) THEN
              WRITE(CMSG,101) TMFFILENAME
	      CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
	      ST = SMG$ERASE_DISPLAY(DISP4)
	      RETURN
           ENDIF
           FILNAME(1) = 'PRIM'
           FILNAME(2) = ':MTM'
           FILNAME(3) = 'F01.'
           FILNAME(4) = 'FIL'
C
C INQUIRE DATA 
C
           CALL INQ_DATA(KEYB,DISP3,DISP4,PB,REPFILENAME,TER,EXTSER,EXTCHK)
           CALL OPENWY(1,FILNAME,0,4,0,ST)
           CALL TOPEN(1)
           
           IF(ST .NE. 0) THEN
              CMSG = ' ERRO!!! Ao abrir o ficheiro PRIM:MTMF01.FIL...'
              CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
	      ST = SMG$ERASE_DISPLAY(DISP4)
	      RETURN
           ENDIF
           
           CALL ROPEN(REPFILENAME,PUNIT,ST)
           IF(ST .NE. 0) THEN
              CMSG = ' ERRO!!! Ao abrir o ficheiro de report...'
              CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
	      ST = SMG$ERASE_DISPLAY(DISP4)
	      CALL USRCLOS1(PUNIT)
	      RETURN
           ENDIF
           
           CMSG = ' A ler do ficheiro PRIM:MTMF01.FIL. Por favor aguarde...'
           ST = SMG$PUT_CHARS (DISP3, CMSG, 1, 1,,SMG$M_BLINK)
        ENDIF        
        CALL TITLE('TRANSACTION FILE REPORT','EUROTMIR',1, PUNIT,PAGE,DAYCDC)
40      CONTINUE
        
        IF (DISK) CALL READTMF(LOGREC,SER,EOT)

        IF(EOT) GOTO 1000
        
        CALL LOGTRA(TRABUF,LOGREC)
        
        IF ((TRABUF(TTYP) .EQ. TEUR) .AND. (TRABUF(TEUTYP) .EQ. TCAN)) THEN 	   
 	   IF (TER .NE. -1) THEN
 	      IF (TRABUF(TTER) .EQ. TER ) THEN 	      	
 	         IF (EXTSER .NE. -1) THEN
 	             IF ((TRABUF(TEUSER) .EQ. EXTSER) .AND. (TRABUF(TEUCHK) .EQ. EXTCHK)) THEN
 	               CALL PRINT_CANCEL(TRABUF,PUNIT,LINECOUNT)
 	               NOTFOUND = .FALSE.
                       RETURN
 	             ENDIF
 	             GOTO 40
 	         ENDIF
  	         CALL PRINT_CANCEL(TRABUF,PUNIT,LINECOUNT)
  	         NOTFOUND = .FALSE.
 	      ENDIF
 	      GOTO 40
 	   ENDIF
 	   IF (EXTSER .NE. -1) THEN
 	      IF ((TRABUF(TEUSER) .EQ. EXTSER) .AND. (TRABUF(TEUCHK) .EQ. EXTCHK)) THEN
 	         CALL PRINT_CANCEL(TRABUF,PUNIT,LINECOUNT)
 	         NOTFOUND = .FALSE.
 	         RETURN
 	      ENDIF 
 	      GOTO 40
 	   ENDIF
 	   CALL PRINT_CANCEL(TRABUF,PUNIT,LINECOUNT)
 	   NOTFOUND = .FALSE.
        ENDIF
        GOTO 40
1000    CONTINUE
        ! CALL CLOSEFIL(FDB) <- NOTHING WAS OPENED
        CALL USRCLOS1(PUNIT)
        
        IF (NOTFOUND) THEN
           IF ((EXTSER .NE. -1) .AND. (TER .NE. -1)) THEN 
              CMSG = ' Cancelamento de Euro Milhões não foi encontrado para o agente escolhido...'
           ENDIF
           IF ((EXTSER .NE. -1) .AND. (TER .EQ. -1)) CMSG = ' Cancelamento de Euro Milhões não foi encontrado...'
           IF (EXTSER .EQ. -1) CMSG = ' Não existem Cancelamentos de Euro Milhões...'
           CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
        ENDIF
        
        ST = SMG$ERASE_DISPLAY(DISP4)             
        RETURN
        
100     FORMAT ('EMCA_',I4.4,'.REP')
101     FORMAT (' O ficheiro <',A15,'> não existe...')
102     FORMAT ('Nome do ficheiro de report = ',A15)
103     FORMAT (7A4)
901     FORMAT(7A4)
300     FORMAT (Z2.2)
        END

C******************************************************************
C SUBROUTINE ALLTRANSACT (READ FROM FILE)
C******************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE ALLTRANSACT(KEYB, DISP4, DISP3,PB,DISK)
        IMPLICIT NONE          
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
        INCLUDE '($TRMDEF)' 
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:DESLOG.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INTEGER PB,			      ! PASTEBOARD
     *        KEYB,			      ! VIRTUAL KEYBOARD
     *        DISP4,			      ! output data frame window
     *        DISP3			      ! message frame window
C
C TMIR VAR
C        
        LOGICAL DISK,ONDISK
        CHARACTER*15 TMFFILENAME
        CHARACTER*15 REPFILENAME
        CHARACTER*78  cMSG
        INTEGER*4 ST,KEY,TER,EXTSER,EXTCHK,PUNIT
        INTEGER*4   PAGE,SER,EOT,FDB(7)
        INTEGER*4   LOGREC(LREC*3)
        INTEGER*4   FILNAME(7)
        INTEGER*4   LINECOUNT
        LOGICAL NOTFOUND
        
C
C DEFAULT VALUES
C
        TMFFILENAME = 'PRIM:MTMF01.FIL'
        WRITE (REPFILENAME,100) DAYCDC
        PAGE     =  0
        LINECOUNT = 0
C        DISK = .FALSE.
        NOTFOUND = .TRUE.
        IF (DISK) THEN
C
C TMF FILENAME
C
           ST = SMG$PUT_CHARS (DISP4, 'TMF FileName = PRIM:MTMF01.FIL', 2,2)
           
           INQUIRE(FILE=TMFFILENAME, EXIST=ONDISK)
           IF (.NOT. ONDISK) THEN
              WRITE(CMSG,101) TMFFILENAME
	      CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
	      ST = SMG$ERASE_DISPLAY(DISP4)
	      RETURN
           ENDIF
           FILNAME(1) = 'PRIM'
           FILNAME(2) = ':MTM'
           FILNAME(3) = 'F01.'
           FILNAME(4) = 'FIL'
C
C INQUIRE DATA 
C
           CALL INQ_DATA(KEYB,DISP3,DISP4,PB,REPFILENAME,TER,EXTSER,EXTCHK)
           CALL OPENWY(1,FILNAME,0,4,0,ST)
           CALL TOPEN(1)
           
           IF(ST .NE. 0) THEN
              CMSG = ' ERRO!!! Ao abrir o ficheiro PRIM:MTMF01.FIL...'
              CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
	      ST = SMG$ERASE_DISPLAY(DISP4)
	      RETURN
           ENDIF
           
           CALL ROPEN(REPFILENAME,PUNIT,ST)
           IF(ST .NE. 0) THEN
              CMSG = ' ERRO!!! Ao abrir o ficheiro de report...'
              CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
	      ST = SMG$ERASE_DISPLAY(DISP4)
	      CALL USRCLOS1(PUNIT)
	      RETURN
           ENDIF
           
           CMSG = ' A ler do ficheiro PRIM:MTMF01.FIL. Por favor aguarde...'
           ST = SMG$PUT_CHARS (DISP3, CMSG, 1, 1,,SMG$M_BLINK)
        ENDIF        
        CALL TITLE('TRANSACTION FILE REPORT','EUROTMIR',1, PUNIT,PAGE,DAYCDC)
40      CONTINUE
        IF (DISK) CALL READTMF(LOGREC,SER,EOT)
        IF(EOT) GOTO 1000
        
        CALL LOGTRA(TRABUF,LOGREC)

        IF ((TRABUF(TTYP) .EQ. TEUR)) THEN 	   
 	   IF (TER .NE. -1) THEN
 	      IF (TRABUF(TTER) .EQ. TER ) THEN 	      	
 	         IF (EXTSER .NE. -1) THEN
 	             IF ((TRABUF(TEUSER) .EQ. EXTSER) .AND. (TRABUF(TEUCHK) .EQ. EXTCHK)) THEN
 	               IF (TRABUF(TEUTYP) .EQ. TWAG) CALL PRINT_BET(TRABUF,PUNIT,LINECOUNT)
 	               IF (TRABUF(TEUTYP) .EQ. TVAL) CALL PRINT_VALID(TRABUF,PUNIT,LINECOUNT)
 	               IF (TRABUF(TEUTYP) .EQ. TCAN) CALL PRINT_CANCEL(TRABUF,PUNIT,LINECOUNT)
 	               NOTFOUND = .FALSE.
                       RETURN
 	             ENDIF
 	             GOTO 40
 	         ENDIF
                 IF (TRABUF(TEUTYP) .EQ. TWAG) CALL PRINT_BET(TRABUF,PUNIT,LINECOUNT)
                 IF (TRABUF(TEUTYP) .EQ. TVAL) CALL PRINT_VALID(TRABUF,PUNIT,LINECOUNT)
                 IF (TRABUF(TEUTYP) .EQ. TCAN) CALL PRINT_CANCEL(TRABUF,PUNIT,LINECOUNT)
  	         NOTFOUND = .FALSE.
 	      ENDIF
 	      GOTO 40
 	   ENDIF
 	   IF (EXTSER .NE. -1) THEN
 	      IF ((TRABUF(TEUSER) .EQ. EXTSER) .AND. (TRABUF(TEUCHK) .EQ. EXTCHK)) THEN
                 IF (TRABUF(TEUTYP) .EQ. TWAG) CALL PRINT_BET(TRABUF,PUNIT,LINECOUNT)
                 IF (TRABUF(TEUTYP) .EQ. TVAL) CALL PRINT_VALID(TRABUF,PUNIT,LINECOUNT)
                 IF (TRABUF(TEUTYP) .EQ. TCAN) CALL PRINT_CANCEL(TRABUF,PUNIT,LINECOUNT)
 	         NOTFOUND = .FALSE.
 	         RETURN
 	      ENDIF 
 	      GOTO 40
 	   ENDIF
           IF (TRABUF(TEUTYP) .EQ. TWAG) CALL PRINT_BET(TRABUF,PUNIT,LINECOUNT)
           IF (TRABUF(TEUTYP) .EQ. TVAL) CALL PRINT_VALID(TRABUF,PUNIT,LINECOUNT)
           IF (TRABUF(TEUTYP) .EQ. TCAN) CALL PRINT_CANCEL(TRABUF,PUNIT,LINECOUNT)
 	   NOTFOUND = .FALSE.
        ENDIF
        GOTO 40
1000    CONTINUE
        ! CALL CLOSEFIL(FDB)  <- NOTHING WAS OPENED
        CALL USRCLOS1(PUNIT)
        
        IF (NOTFOUND) THEN
           IF ((EXTSER .NE. -1) .AND. (TER .NE. -1)) THEN 
              CMSG = ' Transacção de Euro Milhões não foi encontrada para o agente escolhido...'
           ENDIF
           IF ((EXTSER .NE. -1) .AND. (TER .EQ. -1)) CMSG = ' Transacção de Euro Milhões não foi encontrada...'
           IF (EXTSER .EQ. -1) CMSG = ' Não existem Transacções de Euro Milhões...'
           CALL SMG_WERROR(CMSG,DISP3,KEYB,KEY)
        ENDIF
        
        ST = SMG$ERASE_DISPLAY(DISP4)             
        RETURN
        
100     FORMAT ('EMAL_',I4.4,'.REP')
101     FORMAT (' O ficheiro <',A15,'> não existe...')
102     FORMAT ('Nome do ficheiro de report = ',A15)
103     FORMAT (7A4)
901     FORMAT(7A4)
        END

C**********************************************
C SUBROUTINE INQ_DATA
C**********************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE INQ_DATA(KEYB,DISP3,DISP4,PB,REPFILENAME,TER,EXTSER,EXTCHK)
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GTECHSMG.DEF'
        INCLUDE '($TRMDEF)'         
        CHARACTER*50 CMSG
        CHARACTER*15 REPFILENAME
        INTEGER*4 TER,EXTSER,EXTCHK,ST
        INTEGER PB,			      ! PASTEBOARD
     *        KEYB,			      ! VIRTUAL KEYBOARD
     *        DISP4,			      ! output data frame window
     *        DISP3			      ! message frame window
     
C
C REPORT FILE NAME
C       
        WRITE(CMSG,102) REPFILENAME
        ST = SMG$PUT_CHARS (DISP4, CMSG, 6,2)
        CMSG = 'Deseja alterar?'
        CALL MENU_CONFIRM(CMSG,8,4,6,44,DISP4,KEYB,PB,ST)
        IF (ST .EQ. 0) THEN
          CALL SMG_INPTEXT(PB,KEYB,DISP4,DISP3,REPFILENAME,1,8,22,15,1,ST)
        ENDIF 
C
C TERMINAL NUMBER
C       
        ST = SMG$ERASE_DISPLAY(DISP4) 
        CMSG = 'Todos os terminais?'
        CALL MENU_CONFIRM(CMSG,4,4,2,44,DISP4,KEYB,PB,ST)
        IF (ST .NE. 0) THEN
           CMSG = ' Digite o número do terminal: '
	   ST = SMG$PUT_CHARS (DISP4, CMSG, 6, 5,,)
	   CALL SMG_INPNUM(PB,KEYB,DISP4,DISP3,TER,6,35,4,-1,6144,ST)
        ELSE 
           TER = -1
        ENDIF 
C
C EXTERNAL SERIAL NUMBER AND CHECK DIGITS
C       
        ST = SMG$ERASE_DISPLAY(DISP4) 
        CMSG = 'Todas as transacções?'
        CALL MENU_CONFIRM(CMSG,8,4,2,44,DISP4,KEYB,PB,ST)
        IF (ST .NE. 0) THEN
           CMSG = ' Digite o Serial externo: '
	   ST = SMG$PUT_CHARS (DISP4, CMSG, 10, 5,,)
	   CALL SMG_INPNUM(PB,KEYB,DISP4,DISP3,EXTSER,10,35,8,-1,99999999,ST)
	   CMSG = ' Digite o check digit externo: '
	   ST = SMG$PUT_CHARS (DISP4, CMSG, 12, 5,,)
	   CALL SMG_INPNUM(PB,KEYB,DISP4,DISP3,EXTCHK,12,45,3,1,255,ST)
        ELSE 
           EXTSER = -1
           EXTCHK = -1
        ENDIF 
        ST = SMG$ERASE_DISPLAY(DISP4)        
     
        RETURN
102     FORMAT ('Nome do ficheiro de report = ',A15)
       
        END   
C**********************************************
C SUBROUTINE PRINT_BET
C**********************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINT_BET(TRABUF,PUNIT,LINECOUNT)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:DESLOG.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'               
        
        INTEGER*4 PUNIT,LINECOUNT,ST
        CHARACTER PQFLAG
        BYTE OUTTAB(500)
        INTEGER*4 I,IND
        
        PQFLAG = 'T'
        
        IF (LINECOUNT .EQ. 0 ) WRITE(PUNIT,900)
        
        WRITE(PUNIT,901)     STAT(TRABUF(TSTAT)),
     *                       ERROR(TRABUF(TERR)),
     *                       TTYPE(TRABUF(TTYP)),
     *                       TRABUF(TSER),
     *                       DISTIM(TRABUF(TTIM)),
     *                       TRABUF(TTER),
     *                       TRABUF(TAGT),
     *                       TRABUF(TTRN),
     *                       TRABUF(TCDC),
     *                       GTNAMES(TRABUF(TGAMTYP)),
     *                       TRABUF(TGAMIND),
     *                       TRABUF(TEUSER),
     *                       TRABUF(TEUCHK)
        
        IF (TRABUF(TEUWQP) .EQ. 0) PQFLAG = 'F'
       
        WRITE(PUNIT,902) TRABUF(TEUWBEGW),
     *                   TRABUF(TEUWBEGY),
     *                   TRABUF(TEUWENDW),
     *                   TRABUF(TEUWENDY),
     *                   TRABUF(TEUWDUR),
     *                   TRABUF(TEUWDRWIND)
        WRITE(PUNIT,903) TRABUF(TEUWNBET),
     *                   PQFLAG,
     *                   TRABUF(TEUWNMK),
     *                   TRABUF(TEUWNST),
     *                   TRABUF(TEUWTIMEH),
     *                   TRABUF(TEUWTIMEM),
     *                   TRABUF(TEUWTIMES)
        
        IND = 1
        DO I=TEUWBOARD,120
	   IF(TRABUF(I) .NE. 0) THEN
              CALL MOVBYT(TRABUF(I),1,OUTTAB,IND,4) 
	      IND=IND+4
           ENDIF
        ENDDO
C           DO I=0 ,20
C              WRITE(PUNIT,300) OUTTAB(I)
C           ENDDO
        CALL TRANSFBOARD(OUTTAB,TRABUF(TEUWNBET),TRABUF(TEUWNMK),TRABUF(TEUWNST),50,11,ST,PUNIT) ! V03
C       CALL TRANSFBOARD(OUTTAB,TRABUF(TEUWNBET),TRABUF(TEUWNMK),TRABUF(TEUWNST),50,9,ST,PUNIT)

        WRITE(PUNIT,904)
        WRITE(PUNIT,*)
        LINECOUNT = LINECOUNT + 1
        IF (LINECOUNT .EQ. 10) LINECOUNT = 0
        
        RETURN
        
900     FORMAT(/,' STATUS ERROR  TYPE',2X,'MIL SERIAL',2X,
     *         ' TIME    TERM  AGT #  SEQ DATE   GTYP   GIND',2X ,'EXTSER',1X,'CHK',1X,
     *         /,1X,130('='),/)
901     FORMAT(2X,A4,3X,A4,2X,A4,1X,I10,3X,A8,I4,I8,Z4,I6,2X,A8,I3,4X,I10.8,'-',I3.3)
C                S ,   E,    T,    SE,   TIM,T,AG, TR, CDC, GNA,GIN,
C902     FORMAT('                       Draw Beg: ',I3.2,'/',I2.2,
C     *         '   Draw End: ',I3.2,'/',I2.2,' Duration: ',I2)
902     FORMAT('                       Draw Beg: ',I4.3,'/',I2.2,
     *         '   Draw End: ',I4.3,'/',I2.2,' Duration: ',I2.2,' Draw Indicator: ',I1)  !V03
903     FORMAT(' # Of Boards: <',I2,'> Qp: <',A1,'> # Of Marks: <',I2,
     *         '> # Of Stars: <',I1,'> Euromil Time: <',I2.2,':',I2.2,':',I2.2,'>')
904     FORMAT(1X,130('='))
300     FORMAT (Z2.2)
        END

C**********************************************
C SUBROUTINE PRINT_CANCEL
C**********************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINT_CANCEL(TRABUF,PUNIT,LINECOUNT)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:DESLOG.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'               
        
        INTEGER*4 PUNIT,LINECOUNT
        CHARACTER*30 CANSTATUS(0:4)
  
        CANSTATUS(0) = 'Good Cancel'     
        CANSTATUS(1) = 'Time Limit Exceeded'
        CANSTATUS(2) = 'Invalid Cancel'
        CANSTATUS(3) = 'Already Cancel'
        CANSTATUS(4) = 'Wrong Terminal'
        
        IF (LINECOUNT .EQ. 0 ) WRITE(PUNIT,900)
                
        WRITE(PUNIT,901)     STAT(TRABUF(TSTAT)),
     *                       ERROR(TRABUF(TERR)),
     *                       TTYPE(TRABUF(TTYP)),
     *                       TRABUF(TSER),
     *                       DISTIM(TRABUF(TTIM)),
     *                       TRABUF(TTER),
     *                       TRABUF(TAGT),
     *                       TRABUF(TTRN),
     *                       TRABUF(TCDC),
     *                       GTNAMES(TRABUF(TGAMTYP)),
     *                       TRABUF(TGAMIND),
     *                       TRABUF(TEUSER),
     *                       TRABUF(TEUCHK)

        IF (TRABUF(TEUCST) .NE. 0) TRABUF(TEUCAM) = 0
        
        WRITE(PUNIT,902) CANSTATUS(TRABUF(TEUCST)),
     *                   CMONY(TRABUF(TEUCAM),11,VALUNIT)

        WRITE(PUNIT,903) TRABUF(TEUCWJUL),
     *                   TRABUF(TEUCWSER),
     *                   TRABUF(TEUCWCKD)
C     *                   TRABUF(TEUCTIMEH),
C     *                   TRABUF(TEUCTIMEM),
C     *                   TRABUF(TEUCTIMES)
                
        WRITE(PUNIT,904)
        WRITE(PUNIT,*)
        LINECOUNT = LINECOUNT + 1
        IF (LINECOUNT .EQ. 10) LINECOUNT = 0
        
        RETURN
        
900     FORMAT(/,' STATUS ERROR  TYPE',2X,'MIL SERIAL',2X,
     *         ' TIME    TERM  AGT #  SEQ DATE   GTYP   GIND',2X ,'EXTSER',1X,'CHK',1X,
     *         /,1X,130('='),/)
901     FORMAT(2X,A4,3X,A4,2X,A4,1X,I10,3X,A8,I4,I8,Z4,I6,2X,A8,I3,4X,I10.8,'-',I3.3)
C                S ,   E,    T,    SE,   TIM,T,AG, TR, CDC, GNA,GIN,
902     FORMAT(' Can Status: <',A51,'> Cancel Amount: <',A11,'>')
903     FORMAT(' Wager External Serial: <',I3.3,'-',I8.8,'-',I3.3,'>')
C903     FORMAT(' Wager External Serial: <',I3.3,'-',I8.8,'-',I3.3,
C     *         '> Euromil Time: <',I2.2,':',I2.2,':',I2.2,'>')
904     FORMAT(1X,130('='))
300     FORMAT (Z2.2)
        END

C**********************************************
C SUBROUTINE PRINT_VALID
C**********************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRINT_VALID(TRABUF,PUNIT,LINECOUNT)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:DESLOG.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'               
        
        INTEGER*4 PUNIT,LINECOUNT
        CHARACTER*20 VALSUBTYP(16)
        CHARACTER*30 VALSTATUS(18)
        CHARACTER*30 VALSTATUSE(0:30)
        CHARACTER PQFLAG
C       BYTE OUTTAB(500)   REMOVE WARNINGS
C       INTEGER*4 I,IND,ST REMOVE WARNINGS
        
        INTEGER*4  EMVALAMT(2)
        INTEGER*8  EMVALAMTI8
        REAL*8     EMVALAMTR8
        EQUIVALENCE (EMVALAMT,EMVALAMTI8)

        DATA VALSTATUS /'Not A Winner','Not Cashed','Cashed',
     *                  'Cashed With Exchange','Deleted Winner',
     *                  'Cancelled Winner','Validation On Hold','Cant Pay Yet',
     *                  'Prize Values Not Set','Host Game Postponed','No Exchange Ticket',
     *                  'Cashed With Exchange','Priv Pay','No Prize Priv Pay',
     *                  'Priv Pay Postponed','Payment Issued To Bank',
     *                  'Set Banking Info','Set Banking Info/Multi-Draw'/
        
        DATA VALSUBTYP /'Regular','Mid-Tier Cash','Claim','Validation Detail',' ',' ',' ',' ',' ',' ',
     *                  ' ',' ',' ',' ',' ','Validation Error'/
        
        VALSTATUSE(0) = 'No Results Yet Or Not A Winner'
        VALSTATUSE(1) = 'Results Not Confirmed'
        VALSTATUSE(2) = 'No Such Ticket'
        VALSTATUSE(3) = 'Cant Pay Yet'
        VALSTATUSE(4) = 'Already Cashed'
        VALSTATUSE(9) = 'Cash At Lottery'
        VALSTATUSE(18) = 'No Details Available'
        VALSTATUSE(30) = 'Winner Holding Limit'

        PQFLAG = 'T'
        
        IF (LINECOUNT .EQ. 0 ) WRITE(PUNIT,900)
        
c        write(*,*) TRABUF(TEUVTIMEM),':',TRABUF(TEUVTIMES),' ',TRABUF(TEUVSBT),' ',TRABUF(TEUVST)
        
        WRITE(PUNIT,901)     STAT(TRABUF(TSTAT)),
     *                       ERROR(TRABUF(TERR)),
     *                       TTYPE(TRABUF(TTYP)),
     *                       TRABUF(TSER),
     *                       DISTIM(TRABUF(TTIM)),
     *                       TRABUF(TTER),
     *                       TRABUF(TAGT),
     *                       TRABUF(TTRN),
     *                       TRABUF(TCDC),
     *                       GTNAMES(TRABUF(TGAMTYP)),
     *                       TRABUF(TGAMIND),
     *                       TRABUF(TEUSER),
     *                       TRABUF(TEUCHK)
        
        EMVALAMT(2) = TRABUF(TEUVCAMH)
        EMVALAMT(1) = TRABUF(TEUVCAM)
        EMVALAMTR8  = DFLOAT(EMVALAMTI8)/100.0D0

        IF (TRABUF(TEUVSBT) .EQ. 15) WRITE(PUNIT,908) VALSUBTYP(TRABUF(TEUVSBT)+1),
     *                   VALSTATUSE(TRABUF(TEUVST)+1),
     *                   '              ' !V03
C    *                   CMONY(TRABUF(TEUVCAM),11,VALUNIT)
        IF (TRABUF(TEUVSBT) .NE. 15) WRITE(PUNIT,902) VALSUBTYP(TRABUF(TEUVSBT)+1),
     *                   VALSTATUS(TRABUF(TEUVST)+1),
     *                   EMVALAMTR8 !V03
C    *                   CMONY(TRABUF(TEUVCAM),11,VALUNIT)

        WRITE(PUNIT,903) TRABUF(TEUVWJUL),
     *                   TRABUF(TEUVWSER),
     *                   TRABUF(TEUVWCKD),
     *                   TRABUF(TEUVTIMEH),
     *                   TRABUF(TEUVTIMEM),
     *                   TRABUF(TEUVTIMES)
        
C        IF (TRABUF(TEUVST) .EQ. 11) THEN
C           IF (TRABUF(TEUVEQP) .EQ. 0) PQFLAG = 'F'
C
C           WRITE(PUNIT,907) TRABUF(TEUEVWSER), TRABUF(TEUEVWCKD)
C           
C           WRITE(PUNIT,905) TRABUF(TEUVEBEGW),
C     *                      TRABUF(TEUVEBEGY),
C     *                      TRABUF(TEUVEENDW),
C     *                      TRABUF(TEUVEENDY),
C     *                      TRABUF(TEUVEDUR)   
C           WRITE(PUNIT,906) TRABUF(TEUVENBET),
C     *                      PQFLAG,
C     *                      TRABUF(TEUVENMK),
C     *                      TRABUF(TEUVENST),
C     *                      TRABUF(TEUVETIMEH),
C     *                      TRABUF(TEUVETIMEM),
C     *                      TRABUF(TEUVETIMES)
C           
C           IND = 1
C           DO I=TEUVEBOARD,120
C	      IF(TRABUF(I) .NE. 0) THEN
C                 CALL MOVBYT(TRABUF(I),1,OUTTAB,IND,4) 
C	         IND=IND+4
C              ENDIF
C           ENDDO
Cc              DO I=0 ,200
Cc                 WRITE(PUNIT,300) OUTTAB(I)
Cc              ENDDO
C           CALL TRANSFBOARD(OUTTAB,TRABUF(TEUVENBET),TRABUF(TEUVENMK),TRABUF(TEUVENST),50,9,ST,PUNIT)        
C        ENDIF
                
        WRITE(PUNIT,904)
        WRITE(PUNIT,*)
        LINECOUNT = LINECOUNT + 1
        IF (LINECOUNT .EQ. 10) LINECOUNT = 0
        
        RETURN
        
900     FORMAT(/,' STATUS ERROR  TYPE',2X,'MIL SERIAL',2X,
     *         ' TIME    TERM  AGT #  SEQ DATE   GTYP   GIND',2X ,'EXTSER',1X,'CHK',1X,
     *         /,1X,130('='),/)
901     FORMAT(2X,A4,3X,A4,2X,A4,1X,I10,3X,A8,I4,I8,Z4,I6,2X,A8,I3,4X,I10.8,'-',I3.3)
C                S ,   E,    T,    SE,   TIM,T,AG, TR, CDC, GNA,GIN,
C902     FORMAT(' Val Subtyp: <',A16,'> Val Status: <',A51,'> Cash Amount: <',A11,'>')
902     FORMAT(' Val Subtyp: <',A16,'> Val Status: <',A51,'> Cash Amount: <',F14.2,'>') !V03
908     FORMAT(' Val Subtyp: <',A16,'> Val Status: <',A51,'> Cash Amount: <',A14,'>') !V03
C903     FORMAT(' Wager External Serial: <',I3.3,'-',I8.8,'-',I3.3,'>')
903     FORMAT(' Wager External Serial: <',I3.3,'-',I8.8,'-',I3.3,
     *         '> Euromil Time: <',I2.2,':',I2.2,':',I2.2,'>')
904     FORMAT(1X,130('='))
905     FORMAT('                       Draw Beg: ',I3.2,'/',I2.2,
     *         '   Draw End: ',I3.2,'/',I2.2,' Duration: ',I2)
906     FORMAT(' # Of Boards: <',I2,'> Qp: <',A1,'> # Of Marks: <',I2,
     *         '> # Of Stars: <',I1,'> Euromil Time: <',I2.2,':',I2.2,':',I2.2,'>')
907     FORMAT('              Exchange Serial: <',I10.8,'-',I3.3,'>')
300     FORMAT (Z2.2)
        END
        
C**********************************************
C SUBROUTINE TRANSFBOARD
C**********************************************
C        
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TRANSFBOARD(TERMES,NBOARD,SYSTEMMARK,SYSTEMSTAR,MAXNUM,MAXSTAR,ST,PUNIT)
        IMPLICIT NONE
C
        BYTE     TERMES(*)
        INTEGER*4   NBOARD
        INTEGER*4   SYSTEMMARK
        INTEGER*4   MAXNUM
        INTEGER*4   ST,PUNIT,SYSTEMSTAR,MAXSTAR
        INTEGER*4 MARKS(20)
        INTEGER*4 STARS(20)
C
C
        INTEGER*4   BRD,PNT,VAL,CNT,XBYT,NIB,SVAL,SCNT,J
        LOGICAL     LEFT
C
C
	PNT = 0
C       DO J=0, 20 
        DO J=1, 20 !V03
	   MARKS(J) = 0
	   STARS(J) = 0
        ENDDO
C
C START DECODE SYSTEM BET
C
        LEFT = .TRUE.
        DO 2900 BRD = 1, NBOARD
C
C MARKS NUMBERS
C
	  VAL = 0
	  CNT = 0
2100	  CONTINUE
	  IF(LEFT)THEN
	    PNT  = PNT+1	  
	    XBYT = TERMES(PNT)
	    XBYT = IAND(XBYT, '000000FF'X)
	    NIB  = ISHFT(XBYT,-4)
	    LEFT = .FALSE.
	  ELSE
	    NIB  = XBYT
	    LEFT = .TRUE.
	  ENDIF
	  NIB = IAND (NIB, '0F'X)
	  IF(NIB .EQ. 0) THEN
	    VAL = VAL+15
	    IF(VAL .GT. MAXNUM)THEN
	      ST = -2
	      GOTO 9000
	    ENDIF
	    GOTO 2100
	  ENDIF
C
	  VAL = VAL + NIB
       
C
C          WRITE(MARKS,100) VAL
          CNT = CNT+1
          MARKS(CNT) = VAL
	  IF(CNT .LT. SYSTEMMARK) GOTO 2100
C
C STAR NUMBERS 
C	  
          SVAL = 0
	  SCNT = 0
2200	  CONTINUE
	  IF(LEFT)THEN
	    PNT  = PNT+1
	    XBYT = TERMES(PNT)
	    XBYT = IAND(XBYT, '000000FF'X)
	    NIB  = ISHFT(XBYT,-4)
	    LEFT = .FALSE.
	  ELSE
	    NIB  = XBYT
	    LEFT = .TRUE.
	  ENDIF
	  NIB = IAND (NIB, '0F'X)
	  IF(NIB .EQ. 0)THEN
	    SVAL = SVAL+15
	    IF(SVAL .GT. MAXSTAR)THEN
	      ST = -2
	      GOTO 9000
	    ENDIF
	    GOTO 2200
	  ENDIF
C
	  SVAL = SVAL + NIB
	  IF(SVAL .GT. MAXSTAR)THEN
	    ST = -3
	    GOTO 9000
	  ENDIF
C
          SCNT = SCNT+1
          STARS(SCNT) = SVAL
	  
	  IF(SCNT .LT. SYSTEMSTAR)GOTO 2200
C
C WRITE BET'S TO REPORT FILE
C
	  WRITE(PUNIT,100) ' Board ',BRD,' Numbers: ',(MARKS(J+1),J=0,SYSTEMMARK) 
	  WRITE(PUNIT,101) '         ',  ' Stars  : ',(STARS(J+1),J=0,SYSTEMSTAR)	
C
2900    CONTINUE
	ST = 0
        
C
9000    CONTINUE
        IF (ST .NE. 0) WRITE(PUNIT,*) 'ERROR ',ST 
        RETURN
C 100     FORMAT(A7,I2,A10,<SYSTEMMARK>I3)
C 101     FORMAT(A9,A10,<SYSTEMSTAR>I3)
100     FORMAT(A7,I2,A10,<SYSTEMMARK>I3.2) !V03
101     FORMAT(A9,A10,<SYSTEMSTAR>I3.2) !V03
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
