C FORMAT_OPS_TO_PRINT.FOR                                                                    
C
C V14 15-MAR-2011 FRP Request week draw for each game
C V13 04-MAR-2011 FRP Separate low and high OPs.
C V12 28-FEB-2011 FRP Print number of player wagers for LuckyNumber division.
C V11 24-DEC-2010 FRP Lotto2 Changes
C V10 09-JUL-2009 CPH DONT PRINT BRANCH NAME FOR OPS
C V09 06-APR-2009 FRP Modify for EM Joker
C v08 23-MAR-2009 MMO JOKER/EM.
C v07 25-out-2004 CMB Dont print Portal P.Orders and accept bank without branch
C V06 22-JAN-2004 CPH FORMAT DIVISION IV TO HAVE 3 DIGITS
C V05 13-NOV-2003 FRP Modify for Batch2 Totobola Changes.
C V04 14-JAN-2002 JHR EURO IS HERE, SCML DOES NOT NEED ESCUDOS INFORMATION
C V03 07-JUL-2001 JHR ADDED PRINT LABELS WITH PAYMENT ORDERES
C V02 28-JUN-2001 EPH ONE SPACE TO THE RIGHT FOR OCR LINE (RODRIGUES WANTS THIS)
C V02 27-MAY-2001 EPH FORMAT ACCOUNT IN OCR LINE WITH LAST 10 POSITIONS OF ACCOUNT (11 POSITIONS)
C V01 22-MAR-2001 EPH INITIAL VERSION FOR SCML
C                                                                               
C FORMAT OPS FROM  OPSGENgg_tt_yyyywww.FIL  FILE INTO 
C OPSPRNgg_tt_yyyywww.LIS    (ITS A PRINT FILE) 
C LBOPSPRNgg_tt_yyyywww.LIS  (ITS A PRINT FILE)
C                                                                               
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C This item is the property of GTECH Corporation, W.Greenwich, Rhode            
C Island, and contains confidential and trade secret information. It            
C may not be transferred from the custody or control of GTECH except            
C as authorized in writing by an officer of GTECH. Neither this item            
C nor the information it contains may be used, transferred,                     
C reproduced, published, or disclosed, in whole or in part, and                 
C directly or indirectly, except as expressly authorized by an                  
C officer of GTECH, pursuant to written agreement.                              
C                                                                               
C Copyright 1993 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C====== OPTIONS/CHECK=NOOVERFLOW/EXT
        PROGRAM SELECT_OPS_TO_PRINT
        IMPLICIT NONE                                                  
	                                                
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:OPS_REC.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:PRN_BUFF.DEF'
	INCLUDE 'INCLIB:BANK_REC.DEF'
	INCLUDE 'INCLIB:BRANCH_REC.DEF'
	INCLUDE 'INCLIB:INTERFACES_REC.DEF'
                                                                               
        INTEGER*4   OPSGEN_LUN
        INTEGER*4   MSG_LUN /6/
        INTEGER*4   IDFIL_FILASF / 78 /   ! IDENFIFICATION FILE FOR ASF FILE

        INTEGER*4   ANOSEMANA, ANO, SEMANA, GNUM, GTYP, GIND
	INTEGER*4   OPCAO, OPCAO2, BOTGAM, TOPGAM, GAM
	INTEGER*4   ST, SZ, USRANS
        INTEGER*4   OPS_PRINTED
        INTEGER*4   GAME1, NUM_ORDER1
        INTEGER*4   GAME2, NUM_ORDER2
        INTEGER*4   GAME , NUM_ORDER
	LOGICAL     REIMPRESSAO
	LOGICAL	    MAY_PRINT
	LOGICAL	    HI_SEPARATED
        LOGICAL     EXIST_OP_LOW_PRIZE
	INTEGER*4   TIPO_OP 
	INTEGER*4   LAST_GAME
        INTEGER*4   BANK            !v07
        INTEGER*4   BANK_PJMC       !v07 - Bank OP for Portal

	CHARACTER   OPSGEN_FILE*28, OPSPRN_FILE*28

        INTEGER*4 GAME_SEL(MAXGAM)
        INTEGER*4 AAAA(MAXGAM),CCC(MAXGAM)
        CHARACTER AAAACCC(MAXGAM)*7
	
	BANK_PJMC=0099
        GNUM = 0
C
C START PROGRAM
C
        CALL COPYRITE                                                             

        CALL GETSCONF(SCFREC,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
  	                                                                         
C
C	INITIALIZE PRN_BUFF STRUCTURE
C	
	PRN_BUFF.LUN   = TMP2_LUN
        PRNT_LABEL.IDFIL_ASF = IDFIL_FILASF
	PRN_BUFF.POINT = 0
	PRNT_LABEL.POINT = 0
        CALL CLEAN_BUFF()        
        CALL CLEAR_LABELS_INFO()

        TYPE*,IAM(),' '
        TYPE*,IAM(),'--------------------------------------------------------------'   
        TYPE*,IAM(),'<<<<< FORMATACAO DAS ORDENS DE PAGAMENTO PARA IMPRESSAO  >>>>>'   
        TYPE*,IAM(),'--------------------------------------------------------------'
        TYPE*,IAM(),' GERA ARQUIVOS OPSPRNjj_tt_aaaaccc.LIS COM AS ORDENS FORMATADAS'
        TYPE*,IAM(),' ONDE:'
        TYPE*,IAM(),'    jj   = NUMERO DO JOGO'
        TYPE*,IAM(),'    tt   = 00 (ORDENS NORMAIS E DE DOZE DIAS)'
        TYPE*,IAM(),'    tt   = 99 (SEGUNDA VIA DE ORDENS)'
        TYPE*,IAM(),'    aaaa = ANO'
        TYPE*,IAM(),'    ccc  = SEMANA'
        TYPE*,IAM(),'--------------------------------------------------------------'
        TYPE*,IAM(),' '
	TYPE*,' '
	TYPE*,' '
C
C	CHOOSE ORDER TYPE
C	-----------------	
	TYPE*,'QUAL TIPO DE ORDEM (DEVE TER SIDO SELECIONADA ANTERIORMENTE):'
	TYPE*,' '
	TYPE*,'   1)  NORMAIS E DE 12 DIAS (PREMIOS ALTOS)'
        TYPE*,'   2)  SEGUNDA VIA'
	TYPE*,' '
        TYPE*,'OPCAO :'
        ACCEPT *, OPCAO
	TYPE*,' '
	TYPE*,' '
	TYPE*,' '
        IF (OPCAO.NE.1 .AND. OPCAO.NE.2) THEN
	   CALL DISPERR (MSG_LUN, 'Opcao Invalida', 0, ' ', 0, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
        ENDIF
C
C	CHOOSE GAMES/DRAWS TO PRINT (WEEK) ONLY IF IT IS NOT A SEGUNDA VIA
C	------------------------------------------------------------------
        IF (OPCAO.NE.2) THEN     
           CALL INPUT_SORTEIO_PER_GAME(GAME_SEL,AAAACCC,AAAA,CCC)
           TYPE*,' '
           TYPE*,' '
           GOTO 10
	ENDIF
C
C       CHOOSE GAME NUMBER
C       ------------------
        TYPE*,'ENTRE O NUMERO DO JOGO [0 - ALL] : '
        ACCEPT *, GNUM
        TYPE*,' '
        TYPE*,' '
        IF (GNUM.LT.0 .OR. GNUM.GT.MAXGAM) THEN
           CALL DISPERR (MSG_LUN, 'Jogo Invalido', 0, ' ', 0, ' ', 0)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
        IF (GNUM.GT.0) THEN
           GIND = SCFGNT(GAMIDX,GNUM)
           GTYP = SCFGNT(GAMTYP,GNUM)
           IF (GTYP.LE.0 .OR.
     *         GTYP.EQ.TPAS .OR. GTYP.EQ.TTGL) THEN
              CALL DISPERR (MSG_LUN, 'Jogo Invalido', 0, ' ', 0, ' ', 0)
              CALL GSTOP (GEXIT_FATAL)
           ENDIF
        ENDIF
C
10      CONTINUE
C
C	ARE WE PRINTING ALL THE OPS FOR THE DRAW OR JUST THOSE THAT WERE DAMAGED DURING PRINTING
C	----------------------------------------------------------------------------------------
	TYPE*,'DESEJA REALIZAR:'
	TYPE*,' '
	TYPE*,'   1)  REIMPRESSAO DE UMA SEQUENCIA DE ORDENS'
	TYPE*,'   2)  IMPRESSAO NORMAL'
	TYPE*,' '
        TYPE*,'OPCAO :'
        ACCEPT *, OPCAO2
	TYPE*,' '
	TYPE*,' '
	TYPE*,' '
        IF (OPCAO2.NE.1 .AND. OPCAO2.NE.2) THEN
	   CALL DISPERR (MSG_LUN, 'Opcao Invalida', 0, ' ', 0, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
        ENDIF
C
C	IF WE ARE GONNA PRINT JUST AN INTERVAL, LETS DEFINE IT
C	------------------------------------------------------
	REIMPRESSAO = .FALSE.

	IF (OPCAO2.EQ.1) THEN

           REIMPRESSAO = .TRUE.

	   TYPE*,'ENTRE O NUMERO DO JOGO SEGUIDO DO NUMERO DA ULTIMA ORDEM IMPRESSA CORRETAMENTE'
	   TYPE*,'E DO NUMERO DA PRIMEIRA ORDEM IMPRESSA CORRETAMENTE APOS O PROBLEMA'
	   TYPE*,'JOGO :'
           ACCEPT*,  GAME1
	   GAME2 = GAME1
	   TYPE*,'ULTIMA ORDEM CORRETA ANTES DA FALHA DE IMPRESSAO:'
           ACCEPT*,  NUM_ORDER1
	   TYPE*,'PRIMEIRA ORDEM CORRETA APOS FALHA DE IMPRESSAO:'
           ACCEPT*,  NUM_ORDER2
        ENDIF
	TYPE*,' '
	TYPE*,' '
C
C ASK IF USER WANTS PRIN LABELS FOR THIS PRINT OPS
C
	PRNT_LABEL.PRINT = .FALSE.
        CALL PRMYESNO('Quer Imprimir Labels [Y/N] ',  USRANS)
	IF(USRANS .EQ. 1) PRNT_LABEL.PRINT = .TRUE.
	TYPE *, ' '
C
C IF USER GENERATE LABELS FILE ASF FILE SHOULD BE OPENED
C
        IF(PRNT_LABEL.PRINT) CALL OPENASF(PRNT_LABEL.IDFIL_ASF)
C
C	BRANCH FILE
C	-----------
	CALL OPEN_BRANCH(ST)
        IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error Opening Branch file (BRANCH.FIL)', 0, ' ', 0, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
        ENDIF
C
C	LOAD BANK TABLE STRUCTURE
C	-------------------------
	CALL LOAD_BANK_TABLE(ST)
        IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error loading BANK TABLE', 0, 'STATUS = ', ST, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
        ENDIF

C
C	NAME INPUT FILE (SELECTED OPS)
C	------------------------------
	IF    (OPCAO.EQ.1) THEN
C          REGULAR OP AND 12 DAY OP
           TIPO_OP = 0
        ELSE
C	   SEGUNDA VIA
           TIPO_OP = 99
	   ANOSEMANA = 9999999
           ANO       = 9999
           SEMANA    = 999
        ENDIF

        BOTGAM=1
        TOPGAM=MAXGAM
        IF (GNUM.NE.0) THEN
           BOTGAM=GNUM
           TOPGAM=GNUM
        ENDIF

        DO 200 GAM=BOTGAM,TOPGAM
          GIND = SCFGNT(GAMIDX,GAM)
          GTYP = SCFGNT(GAMTYP,GAM)
          IF (GTYP.LE.0 .OR.
     *        GTYP.EQ.TPAS .OR. GTYP.EQ.TTGL) GOTO 200

          IF (OPCAO.NE.2) THEN     
            IF(GAME_SEL(GAM) .EQ. 0) GOTO 200
            ANO = AAAA(GAM)
            SEMANA = CCC(GAM)
            ANOSEMANA = CTOI(AAAACCC(GAM),SZ)
          ENDIF

	WRITE(OPSGEN_FILE, FMT='(A11,I2.2,A1,I2.2,A1,I4.4,I3.3,A4)') 
     *    'FILE:OPSGEN', GAM, '_', TIPO_OP, '_', ANO, SEMANA, '.FIL'

C
C	CREATE A SORTED FILE OF SAME NAME AND HIGHER VERSION FROM INPUT FILE (OPSGENgg_tt_yyyyww.FIL)
C	------------------------------------------------------------------------------------------
	CALL CREATE_OPSGEN_SORTED (OPSGEN_FILE, ST) 
	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error opening ' // OPSGEN_FILE//' INPUT file for SORT', 0, ' ', 0, 
     *                            'Or error during SORT.    STATUS = ', ST)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF

C	OPEN OPSGEN SORTED FILE WITH DISPOSE = DELETE
C	---------------------------------------------		
        CALL OPEN_OPSGEN (OPSGEN_LUN, TIPO_OP, 'READ', ANO, SEMANA, GAM, ST)
      	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error Opening '//OPSGEN_FILE//' SORT FILE', 0, 'STATUS = ', ST, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
      	ENDIF    

C
C	LOOP READING SORTED FILE
C	=========================

        IF (REIMPRESSAO) THEN
           MAY_PRINT = .FALSE.
        ELSE
	   MAY_PRINT = .TRUE.
        ENDIF

	LAST_GAME = 0
        HI_SEPARATED = .FALSE.
        EXIST_OP_LOW_PRIZE = .FALSE.

	DO 100 WHILE (.TRUE.)

	   READ(OPSGEN_LUN, END=300, IOSTAT=ST) OPS_REC
      	   IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Error Reading '//OPSGEN_FILE//' SORT FILE', 0, 'STATUS = ', ST, ' ', 0)
              CLOSE(PRN_BUFF.LUN)
      	      CALL GSTOP (GEXIT_FATAL)
      	   ENDIF    

	   GAME      = CTOI(OPS_REC.GAME,SZ)
	   NUM_ORDER = CTOI(OPS_REC.ORDER,SZ)
	   
             BANK      = CTOI(OPS_REC.BANK,SZ)
                 
           IF(GAME.NE.GAM) GOTO 100  !game in PO must be equal to file OPSGENgg
           
           IF(BANK.EQ.BANK_PJMC) GOTO 100   ! it's a Portal OP

           IF (REIMPRESSAO .AND. NUM_ORDER.EQ.NUM_ORDER2 .AND. GAME.EQ.GAME2) THEN
C             HAVE FOUND FIRST OP PRINTED OK AFTER PRINTER CRASH (STOP PRINTING NOW)
              MAY_PRINT = .FALSE.
           ENDIF

C
C	   CHECK IF THIS OP IS TO BE PRINTED
C	   ---------------------------------
	   IF (MAY_PRINT) THEN

              IF (LAST_GAME.NE.GAME) THEN
C
C                HAVE TO OPEN A NEW OUTPUT FILE FOR EACH NEW GAME
C
C                IF (LAST_GAME.NE.0) THEN
C		    FLUSH PRINT BUFFER AND CLOSE LAST OPSPRN FILE OPENED 
C		    ----------------------------------------------------
 		    CALL PRINT_BUFFER()
                    CALL PRINT_LABELS()
                    CLOSE (PRN_BUFF.LUN)
C                ENDIF

C	         OPEN OPSPRN PRINT FORMATTED FILE FOR THE GAME
C	         ---------------------------------------------	
	         WRITE(OPSPRN_FILE, FMT='(A11,I2.2,A1,I2.2,A1,I4.4,I3.3,A4)') 
     *                              'FILE:OPSPRN', GAME, '_', 
     *                              TIPO_OP, '_', ANO, SEMANA, '.LIS' 
                 OPEN (UNIT   = PRN_BUFF.LUN,
     *	                        FILE   = OPSPRN_FILE,
     *                          STATUS = 'NEW',
     *                          CARRIAGECONTROL = 'LIST',
     *                          RECORDTYPE = 'FIXED',
     *                          RECL = 132,
     *		   	        IOSTAT = ST)
      	         IF (ST.NE.0) THEN
	            CALL DISPERR (MSG_LUN, 'Error Opening '//OPSPRN_FILE//' PRINT FILE', 0, 'STATUS = ', ST, ' ', 0)
                    CLOSE(OPSGEN_LUN)
      	            CALL GSTOP (GEXIT_FATAL)
      	         ENDIF		 
                 LAST_GAME = GAME
C
C IF WE HAVE TO PRINT LABELS, OPEN LABES FILE
C
                 IF(PRNT_LABEL.PRINT) THEN
                   CALL OPEN_LABEL_FILE(GAME, TIPO_OP, ANO, SEMANA)
                 ENDIF
C
              ELSE
C
C CHECK IF HIGH PRIZE OP
C
                 IF(OPS_REC.HI_PRIZE .AND. .NOT. HI_SEPARATED .AND. EXIST_OP_LOW_PRIZE) THEN
 		   CALL PRINT_BUFFER()
C                  CALL PRINT_LABELS()
                   CALL PRINT_ASTERISKS(GAME)
                   HI_SEPARATED = .TRUE.
                 ENDIF
    	      ENDIF

              IF(.NOT. OPS_REC.HI_PRIZE) EXIST_OP_LOW_PRIZE = .TRUE.  !Just in case there're
                                                           !only High OPs (not to print *'s)

              OPS_PRINTED = OPS_PRINTED + 1

              CALL SEND_OP_TO_PRINT_BUFFER ()

           ENDIF	   
 
          
           IF (REIMPRESSAO .AND. NUM_ORDER.EQ.NUM_ORDER1 .AND. GAME.EQ.GAME1) THEN
C	      HAVE FOUND LAST GOOD OP PRINTED (START PRINTING FROM NEXT OP)
              MAY_PRINT = .TRUE.
           ENDIF

100	CONTINUE


300     CLOSE(OPSGEN_LUN)

200	CONTINUE
     
        IF(PRNT_LABEL.PRINT) THEN
          CALL CLOSASF(PRNT_LABEL.IDFIL_ASF)   ! AGENT SALES FILE
        ENDIF
	CLOSE(BRH_LUN)                       ! BRANCH FILE
C
C 	PRINT WHAT THERE IS IN BUFFER AND CLOSE THE LAST PRINT FILE
C
	CALL PRINT_BUFFER()
        CALL PRINT_LABELS()
        CLOSE(PRN_BUFF.LUN) 
        IF(PRNT_LABEL.PRINT) THEN
          CALL USRCLOS1(PRNT_LABEL.IDFIL_LBL)  ! CLOSE PRINT LABELS FILE 
        ENDIF
        TYPE*,IAM(),'-----------------------------------------------------------'
        TYPE*,IAM(),'TOTAL DE ORDENS ENVIADAS PARA IMPRESSAO : ', OPS_PRINTED
        TYPE*,IAM(),'-----------------------------------------------------------'

      	CALL GSTOP (GEXIT_SUCCESS)
      	END



C	*************************************************
	SUBROUTINE CREATE_OPSGEN_SORTED (OPSGEN_FILE, ST)
C	*************************************************
        CHARACTER  OPSGEN_FILE*28
        INTEGER*4  ISTATUS

	INTEGER*4  ST

	CHARACTER*30	INPUTNAME
	CHARACTER*30	OUTPUTNAME

	INTEGER*2	KEYBUF(17)

	INTEGER*4	SOR$PASS_FILES
	INTEGER*4	SOR$BEGIN_SORT
	INTEGER*4	SOR$SORT_MERGE
	INTEGER*4	SOR$END_SORT

        EXTERNAL	SS$_ENDOFFILE
        EXTERNAL	DSC$K_DTYPE_T
        EXTERNAL	DSC$K_DTYPE_LU
        EXTERNAL	SOR$GK_RECORD
	INTEGER*4       SRTTYPE

	INPUTNAME  = OPSGEN_FILE
	OUTPUTNAME = OPSGEN_FILE

	KEYBUF(1) = 4			    !NUMBER OF KEYS

	KEYBUF(2) = %LOC(DSC$K_DTYPE_LU)
	KEYBUF(3) = 0			    !0 = ASCENDING / 1 = DESCENDING
	KEYBUF(4) = 74			    !OFFSET FOR THE KEY (HIGH PRIZE) -> LOOK AT DUMP UTILITY FOR OFFSET
	KEYBUF(5) = 4			    !KEY SIZE 

	KEYBUF(6) = %LOC(DSC$K_DTYPE_T)
	KEYBUF(7) = 0			    !0 = ASCENDING / 1 = DESCENDING
	KEYBUF(8) = 0			    !OFFSET FOR THE KEY  (GAME) -> LOOK AT DUMP UTILITY FOR OFFSET
	KEYBUF(9) = 2			    !KEY SIZE 

	KEYBUF(10) = %LOC(DSC$K_DTYPE_T)
	KEYBUF(11) = 0			    !0 = ASCENDING / 1 = DESCENDING
	KEYBUF(12) = 114		    !OFFSET FOR THE KEY (BANK) -> LOOK AT DUMP UTILITY FOR OFFSET
	KEYBUF(13) = 4			    !KEY SIZE 

	KEYBUF(14) = %LOC(DSC$K_DTYPE_T)
	KEYBUF(15) = 0			    !0 = ASCENDING / 1 = DESCENDING
	KEYBUF(16) = 35			    !OFFSET FOR THE KEY (AGENT) -> LOOK AT DUMP UTILITY FOR OFFSET
	KEYBUF(17) = 7			    !KEY SIZE 

	SRTTYPE = %LOC(SOR$GK_RECORD)

	ST = 0

	ISTATUS = SOR$PASS_FILES (INPUTNAME, OUTPUTNAME)
	IF (.NOT.ISTATUS) THEN
           ST = 1
           RETURN
        ENDIF

	ISTATUS = SOR$BEGIN_SORT (KEYBUF,,,,,,SRTTYPE,%REF(3))
	IF (.NOT.ISTATUS) THEN
           ST = 1
           RETURN
        ENDIF

	ISTATUS = SOR$SORT_MERGE ()
	IF (.NOT.ISTATUS) THEN
           ST = 1
           RETURN
        ENDIF

	ISTATUS = SOR$END_SORT()
	IF (.NOT.ISTATUS) THEN
           ST = 1
           RETURN
        ENDIF

	RETURN
	END



C	**************************************
	SUBROUTINE SEND_OP_TO_PRINT_BUFFER ()
C	**************************************
        IMPLICIT NONE                                                  
	                                                
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:OPS_REC.DEF'
	INCLUDE 'INCLIB:PRN_BUFF.DEF'

	INTEGER*4 SZ

        PRN_BUFF.POINT = PRN_BUFF.POINT + 1
        IF(PRNT_LABEL.PRINT) PRNT_LABEL.POINT = PRNT_LABEL.POINT + 1
C
C	UPDATE AGENT
C
        PRN_BUFF.AGENT(PRN_BUFF.POINT) = CTOI(OPS_REC.AGENT,SZ) 
        IF(PRNT_LABEL.PRINT) THEN
          PRNT_LABEL.AGENT(PRNT_LABEL.POINT) = CTOI(OPS_REC.AGENT,SZ) 
        ENDIF
C
C	IT IS THE THIRD OP, MUST PRINT BUFFER
C
        IF (PRN_BUFF.POINT.EQ.3) THEN
           CALL PRINT_BUFFER()
        ENDIF
C
C       IF IS THE FOURTH LABEL, MUST PRINT LABELS
C        
	IF(PRNT_LABEL.POINT .EQ. 4) THEN
          CALL PRINT_LABELS()    
        ENDIF
C
C	FORMAT OP IN PRINT BUFFER
C
	CALL FORMAT_OP()

	RETURN
	END


C	***********************
	SUBROUTINE CLEAN_BUFF()
C	***********************
        IMPLICIT NONE                                                  
	INCLUDE 'INCLIB:PRN_BUFF.DEF'
	INTEGER*4 LIN, BUF
        DO BUF=1,2
           DO LIN=1,17
	      WRITE(PRN_BUFF.LINOP(BUF,LIN),10) 
10            FORMAT(70(' '))
	   ENDDO
           PRN_BUFF.AGENT(BUF) = 0
        ENDDO
	RETURN
	END



C	*************************
	SUBROUTINE PRINT_BUFFER()
C	*************************
        IMPLICIT NONE                                                  
	INCLUDE 'INCLIB:PRN_BUFF.DEF'
	INTEGER*4 LIN, I

        IF (PRN_BUFF.POINT.EQ.0) RETURN
C
C	PUT ASTERISC IN CORRECT LINE BEFORE WRITING FORMATTED OP TO FILE
C
        CALL RETURN_ASTERISC_LIN (LIN)
        PRN_BUFF.LINOP(1,LIN)(1:1) = '*'

        DO LIN=1,17
	   WRITE(PRN_BUFF.LUN,11) PRN_BUFF.LINOP(1,LIN)(1:61), PRN_BUFF.LINOP(2,LIN)(3:61), '            '
11         FORMAT(A61, A59, A12)
	ENDDO

	PRN_BUFF.OPS_ON_PAGE = PRN_BUFF.OPS_ON_PAGE + 2

	IF (PRN_BUFF.OPS_ON_PAGE.EQ.6) THEN
C
C	   END OF PAGE
C
C	   WRITE(PRN_BUFF.LUN,13) CHAR(12)     !FORMFEED
C13	   FORMAT(A1)
	   DO I=1,5 
	      WRITE(PRN_BUFF.LUN,13)
           ENDDO
	   PRN_BUFF.OPS_ON_PAGE = 0
	ELSE
	   DO I=1,5             
	      WRITE(PRN_BUFF.LUN,13)
           ENDDO
	ENDIF
13	FORMAT(132(' '))

        CALL CLEAN_BUFF()
C
C	IF I HAVE THE THIRD OP TO FORMAT, SAVE THE CORRESPONDING AGENT TO THE FIRST POSITION.
C       THIS WILL BE THE FIRST OP ON NEXT PRINT_BUFFER.
C       UPDATE POINTER TO OPS.
C
        IF (PRN_BUFF.POINT.EQ.3) THEN
           PRN_BUFF.AGENT(1) = PRN_BUFF.AGENT(3)
           PRN_BUFF.AGENT(3) = 0
           PRN_BUFF.POINT = 1
        ELSE
           PRN_BUFF.POINT = 0
        ENDIF

	RETURN
	END



C	************************************
	SUBROUTINE RETURN_ASTERISC_LIN (LIN)
C	************************************
        IMPLICIT NONE                                                  
	INCLUDE 'INCLIB:PRN_BUFF.DEF'

	INTEGER*4 LIN

        IF     (PRN_BUFF.POINT.EQ.1) THEN

	   LIN = 17

        ELSEIF (PRN_BUFF.POINT.EQ.2) THEN

	   IF  (PRN_BUFF.AGENT(1) .EQ. PRN_BUFF.AGENT(2)) THEN
	      LIN = 8
           ELSE
	      LIN = 17
           ENDIF

        ELSEIF (PRN_BUFF.POINT.EQ.3) THEN

	   IF (PRN_BUFF.AGENT(1) .EQ. PRN_BUFF.AGENT(2)) THEN
	      IF (PRN_BUFF.AGENT(2) .EQ. PRN_BUFF.AGENT(3)) THEN
	         LIN = 1
              ELSE
	         LIN = 9
              ENDIF
	   ELSE
	      IF (PRN_BUFF.AGENT(2) .EQ. PRN_BUFF.AGENT(3)) THEN
	         LIN = 12
              ELSE
	         LIN = 17
              ENDIF
           ENDIF
	    
	ENDIF

	RETURN
	END



C	**********************
	SUBROUTINE FORMAT_OP()
C	**********************
        IMPLICIT NONE                                                  
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:OPS_REC.DEF'
	INCLUDE 'INCLIB:PRN_BUFF.DEF'

        INTEGER*4     SZ, ST
        INTEGER*4     MSG_LUN /6/ 
	CHARACTER*40  BANK_NAME
	CHARACTER*40  BRANCH_NAME
	CHARACTER*8   AGENTE
	CHARACTER*7   CONCURSO
	CHARACTER*12  BILHETE
	INTEGER*2     DATE(12)
        CHARACTER*10  PAGAVEL_ATE
	CHARACTER*18  TOTAL_EUROS
C	CHARACTER*18  TOTAL_ESCUDOS      ! EURO IS HERE, SCML DOES NOT NEED ESCUDOS
	CHARACTER*9   ZONA_INTER
	CHARACTER*12  CONTA
	INTEGER*4     TOTAL_DA_ORDEM
	CHARACTER*13  IMPORTANCIA
	CHARACTER*15  CONTA_PREMIO       ! AAAACCCCCCCCCCC  = agencia(4)/conta(11)
	CHARACTER*11  NUMERO_ORDEM
	INTEGER*4     GTYPE, GAME, GINDX
	CHARACTER*5   SHARES
	INTEGER*4     POSDIV, DIV, TOPDIV
	CHARACTER*3   TIPO
	CHARACTER*18  FORMAT_EURO_VALUE
	CHARACTER*29  OCR_LINE
	INTEGER*4     DV
	CHARACTER*1   GAUX
	
C  BES AND PJMC DONT HAVE BRANCH	
	CHARACTER*4   ZERO_BRANCH       !v07      
        ZERO_BRANCH='0000'              !v07 

C
C	GET GAME NUMBER
C	---------------
        GAME = CTOI(OPS_REC.GAME,SZ)
C
C	NOME DO BANCO E CONTA PREMIO (ZONA INTERBANCARIA E NUMERO DA CONTA)
C	-------------------------------------------------------------------


	CALL GET_BANK_INFO (OPS_REC.BANK, BANK_NAME, CONTA_PREMIO, ST)
	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'BANK not Found : '//ops_rec.bank, 0, ' ', 0, 'From AGENT '//ops_rec.agent, 0)
	   CALL DISPERR (MSG_LUN, '*** PROGRAMA ABORTADO ***', 0, ' ', 0, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
        ENDIF
C
C	NOME DA AGENCIA (BALCAO) ONLY IF IS A OP OF BALCAO
C       --------------------------------------------------
        IF (OPS_REC.TOTAL_GAME+OPS_REC.TOTAL_JOKER .GE. P(VALORDHI)) THEN
        
         IF (OPS_REC.BRANCH .NE. ZERO_BRANCH)  THEN                         !v7
	   CALL GET_BRANCH_NAME(OPS_REC.BANK, OPS_REC.BRANCH, BRANCH_NAME,ST)
	   IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'BRANCH not Found : '//OPS_REC.BRANCH, 0, 'In BANK : '//ops_rec.bank, 0, 
     *                      'From AGENT '//ops_rec.agent, 0)
C      	      CALL GSTOP (GEXIT_FATAL)
           ENDIF
         ENDIF   
        ELSE
	   BRANCH_NAME = '                                        '
        ENDIF
C
C	NUMERO DO AGENTE
C       ----------------
	AGENTE    = OPS_REC.AGENT(1:2) // '.' // OPS_REC.AGENT(3:7)
C
C	NUMERO DO CONCURSO
C	------------------
	CONCURSO = ' ' // OPS_REC.YEARWEEK(5:7) // '/' // OPS_REC.YEARWEEK(3:4)
C
C	NUMERO DO BILHETE
C	-----------------
	IF (OPS_REC.ONLINE_ORDER) THEN
           BILHETE = OPS_REC.BILHETE(1:11)
        ELSE
	   BILHETE = '  ' // OPS_REC.BILHETE(1:7) // '  '
	ENDIF	
C
C	DATA LIMITE DE PAGAMENTO (PAGAVEL ATE)
C	--------------------------------------
	DATE(VCDC) = OPS_REC.PAYABLE_CDC
	CALL LCDATE(DATE)	
	IF (DATE(VYEAR).LE.99) THEN
	   IF (DATE(VYEAR).GT.90) THEN
              DATE(VYEAR) = DATE(VYEAR)+1900
           ELSE
              DATE(VYEAR) = DATE(VYEAR)+2000
	   ENDIF
	ENDIF
        WRITE(PAGAVEL_ATE,FMT='(I2.2,A1,I2.2,A1,I4.4)') DATE(VDAY), '/', DATE(VMON), '/', DATE(VYEAR)
C
C	TOTAL A PAGAR
C	-------------
	TOTAL_DA_ORDEM = OPS_REC.TOTAL_GAME+OPS_REC.TOTAL_JOKER
	TOTAL_EUROS   = FORMAT_EURO_VALUE (TOTAL_DA_ORDEM, 'Eur')
C	TOTAL_ESCUDOS = FORMAT_EURO_VALUE (TOTAL_DA_ORDEM, 'Esc')  ! EURO IS HERE, SCML DON'T NEED ESCUDOS
C
C	ZONA INTERBANCARIA
C	------------------
	ZONA_INTER = OPS_REC.BANK // CONTA_PREMIO(1:4) // '<'
C
C	NUMERO DA CONTA
C	---------------
C
C	ANY GAME GREATER THAN 9 IS SUBTRACTED OF 1 (SCML SPECIFICATION - NOT MINE!!!)
C        
	IF (GAME.GT.9) THEN
           WRITE(GAUX,FMT='(I1.1)') GAME-1
	ELSE
           WRITE(GAUX,FMT='(I1.1)') GAME
	ENDIF
	CONTA = GAUX // CONTA_PREMIO(6:15) // '+'     !V02 - CONTA_PREMIO(5:14) // '+'
C
C	IMPORTANCIA
C	-----------	
	WRITE (IMPORTANCIA, FMT='(I12.12,A1)') TOTAL_DA_ORDEM, '<'
C
C	TIPO
C	----
	TIPO = '73+'
C
C	NUMERO DA ORDEM
C	---------------
C	CALCULATES DV FOR OCR LINE TO BE INSERTED IN NUMERO_ORDEM
C
	OCR_LINE = ZONA_INTER(1:8) // CONTA(1:11) // OPS_REC.CWEEK(1:2) // OPS_REC.ORDER // TIPO(1:2)
	CALL CALC_DV_OCR (OCR_LINE,DV)
	WRITE (NUMERO_ORDEM, FMT='(I2.2,A2,A6,A1)') DV, OPS_REC.CWEEK(1:2), OPS_REC.ORDER, '>' 

C
C       >>> LINE 1
C
	PRN_BUFF.LINOP(PRN_BUFF.POINT, 1)  (25:64) = BANK_NAME
C
C WE DONT WANT BRANCH NAME TO BE PRINTED IN OPS
C	>>> LINE 2
C 
C	PRN_BUFF.LINOP(PRN_BUFF.POINT, 2)  (25:64) = BRANCH_NAME
C
C	>>> LINE 9
C
	GTYPE = GNTTAB(GAMTYP,GAME)
        GINDX = GNTTAB(GAMIDX,GAME)
C
	IF(GTYPE.EQ.TKIK) THEN
         IF (OPS_REC.JOKER_DIV.NE.0) THEN
           DIV = OPS_REC.JOKER_DIV
           POSDIV = 36+((DIV-1)*4)
           PRN_BUFF.LINOP(PRN_BUFF.POINT, 9) (POSDIV+2:POSDIV+2) = 'x'
         ENDIF
         GOTO 999
        ENDIF
C
	IF (GTYPE.EQ.TLTO) THEN
	   DO DIV=1,5
              POSDIV = 42+((DIV-1)*3)
              IF (OPS_REC.WINS(DIV).EQ.0) THEN
                 SHARES = '     '
	      ELSE
                 WRITE (SHARES, FMT='(I3)') OPS_REC.WINS(DIV)
	      ENDIF
              IF (DIV.EQ.5) THEN
C	         WE HAVE SPACE FOR 3 DIGIT NUMBER HERE (DIVISION V)
	         PRN_BUFF.LINOP(PRN_BUFF.POINT, 9) (POSDIV:POSDIV+2) = SHARES(1:3)
               ELSE
              IF (DIV.EQ.4) THEN  !V06
C	         WE HAVE SPACE FOR 3 DIGIT NUMBER HERE (DIVISION IV)
	         PRN_BUFF.LINOP(PRN_BUFF.POINT, 9) (POSDIV-1:POSDIV+1) = SHARES(1:3)
              ELSE
C	         2 DIGIT NUMBER FOR OTHER DIVISIONS
	         PRN_BUFF.LINOP(PRN_BUFF.POINT, 9) (POSDIV:POSDIV+1) = SHARES(2:3)
              ENDIF
	      ENDIF
           ENDDO
           IF(GINDX.EQ.3 .OR. GINDX.EQ.4) THEN  !Totoloto S and Q
              SHARES = '     '
              IF (OPS_REC.WINS(6).NE.0) THEN  !Lucky Number won
                 WRITE (SHARES, FMT='(I3)') OPS_REC.WINS(6)
	         PRN_BUFF.LINOP(PRN_BUFF.POINT, 9) (57:59) = SHARES(1:3)
	      ENDIF
              GOTO 999
           ENDIF
	ELSE
           TOPDIV=3
	   IF (GTYPE.EQ.TSPT) TOPDIV=4
	   DO DIV=1,TOPDIV
              POSDIV = 40+((DIV-1)*4)
              IF (OPS_REC.WINS(DIV).EQ.0) THEN
                 SHARES = '     '
	      ELSE
                 WRITE (SHARES, FMT='(I4)') OPS_REC.WINS(DIV)
              ENDIF
	      PRN_BUFF.LINOP(PRN_BUFF.POINT, 9) (POSDIV:POSDIV+3) = SHARES
           ENDDO
        ENDIF
        IF (OPS_REC.JOKER_DIV.EQ.0) THEN
	   PRN_BUFF.LINOP(PRN_BUFF.POINT, 9)  (59:59) = ' '
        ELSE
	   PRN_BUFF.LINOP(PRN_BUFF.POINT, 9)  (59:59) = ITOC(OPS_REC.JOKER_DIV,SZ)
	ENDIF
C
999     CONTINUE
C
C	>>> LINE 12
C
	PRN_BUFF.LINOP(PRN_BUFF.POINT, 12) = '   ' // AGENTE // CONCURSO // 
     *                                       BILHETE // PAGAVEL_ATE // ' ' // TOTAL_EUROS
C
C	>>> LINE 13 ( V04, EURO IS HERE, SCML DOES NOT NEED ESCUDOS INFORMATION, PRINT BLANKS )
C
C	PRN_BUFF.LINOP(PRN_BUFF.POINT, 13) (42:59)  = TOTAL_ESCUDOS
C
C	>>> LINE 17
C
	PRN_BUFF.LINOP(PRN_BUFF.POINT, 17) = '      ' // ZONA_INTER // ' ' // CONTA // ' ' // NUMERO_ORDEM //
     *					     ' ' // IMPORTANCIA // ' ' // TIPO

C
C IF USER ASKED TO PRINT LABELS, FORMAT IT
C
        IF(PRNT_LABEL.PRINT) THEN
           CALL FORMAT_LABELS()
        ENDIF        
	RETURN
	END



C	***************************************************************
	SUBROUTINE GET_BANK_INFO (BANK, BANK_NAME, CONTA_PREMIO, ST)
C	***************************************************************
	IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:BANK_REC.DEF'

	CHARACTER*(*) BANK
        CHARACTER*(*) BANK_NAME
        CHARACTER*(*) CONTA_PREMIO
	INTEGER*4 ST, I

        ST = -1
	DO I=1,MAXBANKS
           IF (BANK_TAB(I).BANK.EQ.BANK) THEN
              BANK_NAME = BANK_TAB(I).LONG_NAME
              CONTA_PREMIO = BANK_TAB(I).CONTA_PREMIO
              ST = 0
              EXIT
           ENDIF
	ENDDO

	RETURN
	END



C	**********************************************************
	SUBROUTINE GET_BRANCH_NAME (BANK, BRANCH, BRANCH_NAME, ST)
C	**********************************************************
	IMPLICIT NONE
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:BRANCH_REC.DEF'

	CHARACTER*(*) BANK
	CHARACTER*(*) BRANCH
        CHARACTER*(*) BRANCH_NAME
      
  	CHARACTER*8   CHAVE
	INTEGER*4 ST

        ST = 0

	CHAVE = BANK // BRANCH
	READ (BRH_LUN, KEYID=0, KEYEQ=CHAVE, IOSTAT=ST) BRANCH_REC
	IF (ST.EQ.0) THEN
           BRANCH_NAME = BRANCH_REC.LONG_NAME
        ELSE
	   BRANCH_NAME = '  '
	ENDIF

	RETURN
	END


C ******************************************************************************
C
C     SUBROUTINE: OPEN_LABEL_FILE
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 06 / 07 / 2001
C
C ******************************************************************************
C
C FUNCTION TO OPEN FILE TO FORMAT LABELS INFORMATION FOR PAYMENT ORDERS
C
C==== OPTIONS /CHECK = NOOVERFLOW /EXT
      SUBROUTINE OPEN_LABEL_FILE(GAME, TIPO_OP, ANYO, SEMANA)
      IMPLICIT NONE
C
C INCLUDES DEFINITION TO OPEN LABELS FILE FOR PAYMENT ORDERS
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:PRN_BUFF.DEF'
C
C PARAMETERS DFINITION TO OPEN LABELS FILE FOR PAYMENT ORDERS
C
      INTEGER * 4 GAME             ! GAME NUMBER
      INTEGER * 4 TIPO_OP          ! PAYMENT ORDER TYPE
      INTEGER * 4 ANYO             ! YEAR
      INTEGER * 4 SEMANA           ! WEEK
C
C CONSTANTS PARAMETER DEFINITION TO OPEN LABELS FILE FOR PAYMENT ORDERS
C
      INTEGER * 4 IDFIL_LABELS     ! IDENFIFICATION FILE FOR LABELS FILE
C
C INIT. CONSTANTS PARAMETER DEFINITION TO OPEN LABELS FILE FOR PAYMENT ORDERS
C
      PARAMETER(IDFIL_LABELS = 77) ! IDENFIFICATION FILE FOR LABELS FILE
C
C VARIABLES DEFINITION TO OPEN LABELS FILE FOR PAYMENT ORDERS
C
      INTEGER * 4 FSTS             ! FUNCTION STATUS
C
      CHARACTER * 30 FILENAME      ! NAME OF THE FILE
C
C SET IDENTIFICATION FILE USED TO PRINT LABELS IN THE FILE
C
      PRNT_LABEL.IDFIL_LBL = IDFIL_LABELS
C
C SET FORMATED FILE NAME
C
      WRITE(FILENAME, 100) GAME, TIPO_OP, ANYO, SEMANA
C
C OPEN FILE NAME
C
      OPEN (UNIT = PRNT_LABEL.IDFIL_LBL,
     *	    FILE = FILENAME,
     *      STATUS = 'NEW',
     *      CARRIAGECONTROL = 'LIST',
     *      RECORDTYPE = 'FIXED',
     *      RECL = 132,
     *      IOSTAT = FSTS)
C
C CHECK IF OPEN FILE IS OK OR NOT
C
      IF(FSTS .NE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Error Opening File: ', FILENAME
        TYPE *, IAM()
      	CALL GSTOP (GEXIT_FATAL)
      ENDIF
C
C IF WE ARE OPENING A NEW FILE, HISTORICAL PRINTED DATA SHOULD BE RESET
C
      PRNT_LABEL.LSTAGTPRNT = 0
      PRNT_LABEL.LSTBNKPRNT = '0000'
C
C FORMATS DEFINITION TO OPEN LABELS FILE FOR PAYMENT ORDERS
C
100   FORMAT('FILE:LBOPSPRN', I2.2, '_', I2.2, '_', I4.4, I3.3, '.LIS')
C
C THIS IS THE END TO OPEN LABELS FILE FOR PAYMENT ORDERS
C
      END


C ******************************************************************************
C
C     SUBROUTINE: PRINT_LABELS
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 06 / 07 / 2001
C
C ******************************************************************************
C
C FUNCTION TO PRINT LABELS INFORMATION IN THE FILE
C
C==== OPTIONS /CHECK = NOOVERFLOW /EXT
      SUBROUTINE PRINT_LABELS
      IMPLICIT NONE
C
C INCLUDES DEFINITION TO PRINT LABELS INFORMATION IN THE FILE
C
      INCLUDE 'INCLIB:PRN_BUFF.DEF'
C
C VARIABLES DEFINITION TO PRINT LABELS INFORMATION IN THE FILE
C
      INTEGER * 4 CNTA              ! COUNTER A
C
C IF USER ASKED TO PRINT LABELS, PRINT IT, IF NOT RETURN
C
      IF(PRNT_LABEL.PRINT .EQ. .FALSE.) RETURN
C
C LOOP TO PRINT ALL LABEL INFORMATION
C
      DO CNTA = 1, 9
	WRITE(PRNT_LABEL.IDFIL_LBL, 100) 
     *        PRNT_LABEL.LINLBL(1, CNTA),
     *        PRNT_LABEL.LINLBL(2, CNTA),
     *        PRNT_LABEL.LINLBL(3, CNTA)
      ENDDO
C
C WHEN INFORMATION IS PRINTED IN THE FILE, CLEAR INFORMATION
C
      CALL CLEAR_LABELS_INFO()
C
C IF I HAVE FOURTH LABEL TO FORMAT, SAVE THE CORRESPONDING AGENT TO THE FIRST 
C POSITION. THIS WILL BE THE FIRST LABEL ON NEXT PRINTER LABEL BUFFER
C
       IF(PRNT_LABEL.POINT .EQ. 4) THEN
         PRNT_LABEL.AGENT(1) = PRNT_LABEL.AGENT(4)
         PRNT_LABEL.AGENT(4) = 0
         PRNT_LABEL.POINT = 1
       ELSE
         PRNT_LABEL.POINT = 0
       ENDIF
C
C FORMATS DEFINITION TO PRINT LABELS INFORMATION IN THE FILE
C
100   FORMAT(X, A, 6X, A, 6X, A)
C
C THIS IS THE END TO PRINT LABELS INFORMATION IN THE FILE
C
      END


C ******************************************************************************
C
C     SUBROUTINE: CLEAR_LABELS_INFO
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 06 / 07 / 2001
C
C ******************************************************************************
C
C FUNCTION TO CLEAR WITH BLANK SPACES ALL INFORMATION WRITE IN FILE
C
C==== OPTIONS /CHECK = NOOVERFLOW /EXT
      SUBROUTINE CLEAR_LABELS_INFO
      IMPLICIT NONE
C
C INCLUDES DEFINITION TO CLEAR ALL INFORMATION FOR LABELS
C
      INCLUDE 'INCLIB:PRN_BUFF.DEF'
C
C VARIABLES DEFINITION TO CLEAR ALL INFORMATION FOR LABELS
C
      INTEGER * 4 CNTA              ! COUNTER A
      INTEGER * 4 CNTB              ! COUNTER B
C
C LOOP TO CLEAR ALL LABEL INFORMATION
C
      DO CNTA = 1, 3
        PRNT_LABEL.AGENT(CNTA) = 0
	DO CNTB = 1, 9
	  WRITE(PRNT_LABEL.LINLBL(CNTA, CNTB), 100)
        ENDDO
      ENDDO
C
C FORMATS DEFINITION TO CLEAR ALL INFORMATION FOR LABELS
C
100   FORMAT(35(' '))
C
C THIS IS THE END TO CLEAR ALL INFORMATION FOR LABELS
C
      END


C ******************************************************************************
C
C     SUBROUTINE: SEARCH_TERMINAL
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 06 / 07 / 2001
C
C ******************************************************************************
C
C FUNCTION THAT SEARCH TERMINAL NUMBER STARTING FROM AGENT NUMBER
C
C==== OPTIONS /CHECK = NOOVERFLOW /EXT
      SUBROUTINE SEARCH_TERMINAL(AGT, TERM, FOUND)
      IMPLICIT NONE
C
C INCLUDES DEFINITION TO SEARCH TERMINAL NUMBER
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF' 
      INCLUDE 'INCLIB:AGTCOM.DEF'
C
C PARAMETERS DEFINITION TO SEARCH TERMINAL NUMBER
C
      INTEGER * 4 AGT         ! AGENT NUMBER
      INTEGER * 4 TERM        ! TERMINAL NUMBER
C
      LOGICAL FOUND           ! SAYS IF WE FOUND RELATION AGENT - TERMINAL
C
C IF AGENT NUMBER IS CERO WE DO NOT FOUND AGENT NUMBER
C
      FOUND = .FALSE.
      IF(AGT .LE. 0) GOTO 1000
C
C LOOP TO SEARCH TERMINAL NUMBER
C
      DO TERM = 1, NUMAGT
        IF(AGT .EQ. AGTTAB(AGTNUM, TERM)) THEN
          FOUND = .TRUE.
          GOTO 1000
        ENDIF
      ENDDO
C
C IF WE DO NOT FOUND TERMINAL NUMBER, DISPLAY AN ERROR AND FINISH PROGRAM
C
1000  CONTINUE
      IF(FOUND .EQ. .FALSE.) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Don''t Found Terminal Number For Agent: ', AGT 
        TYPE *, IAM()
      ENDIF
C
C THIS IS THE END TO SEARCH TERMINAL NUMBER
C
      END


C ******************************************************************************
C
C     SUBROUTINE: FORMAT_LABELS
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 06 / 07 / 2001
C
C ******************************************************************************
C
C FUNCTION TO WRITE FORMATED INFORMATION IN LABELS BUFFER
C
C==== OPTIONS /CHECK = NOOVERFLOW /EXT
      SUBROUTINE FORMAT_LABELS
      IMPLICIT NONE
C
C INCLUDES DEFINITION TO WRITE FORMATED INFORAMTION IN LABELS BUFFER
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:AGTINF.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:PRMAGT.DEF'
      INCLUDE 'INCLIB:RECAGT.DEF'
      INCLUDE 'INCLIB:PRN_BUFF.DEF'
      INCLUDE 'INCLIB:OPS_REC.DEF'
C
C VARIABLES DEFINITION TO WRITE FORMATED INFORAMTION IN LABELS BUFFER
C
      INTEGER * 4 FSTS        ! FUNCTION STATUS
      INTEGER * 4 CNT         ! COUNTER
      INTEGER * 4 IDX         ! LOOP INDEX
      INTEGER * 4 TERM        ! TEMINAL NUMBER
      INTEGER * 4 ZIPCODE     ! AGENT ZIP CODE
C
      LOGICAL FOUND           ! SAYS IF WE FOUND RELATION AGENT - TERMINAL
C
      CHARACTER * 30 CITYNAME ! AGENT CITY NAME
C
C FUNCTIONS DEFINITION TO WRITE FORMATED INFORAMTION IN LABELS BUFFER
C
      CHARACTER*8  IAGT_NO    ! FUNCTION FORMATED AGENT NUMBER XX.XXXXX
      
C    Bank OP for Portal

       CHARACTER * 4   BANK_PJMC       !V07 
       BANK_PJMC='0099'      
C
C SET INDEX TO FORMAT PRINTED LABEL
C
      IDX = PRNT_LABEL.POINT
C
C CHECK IF WE HAVE ONE VALID AGENT NUMBER
C
      IF(PRNT_LABEL.AGENT(IDX) .LE. 0) RETURN
C DONT PRINT PORTAL LABELS
      
      IF(OPS_REC.BANK .EQ. BANK_PJMC) RETURN
C
C CHEC IF WE HAVE TO PRINT LABEL OR THIS LABEL IS ALREADY PRINTTED
C
      IF(PRNT_LABEL.LSTAGTPRNT .EQ. PRNT_LABEL.AGENT(IDX)) THEN
        IF(PRNT_LABEL.LSTBNKPRNT .EQ. OPS_REC.BANK) THEN
          PRNT_LABEL.POINT = PRNT_LABEL.POINT - 1
          RETURN
        ENDIF
      ENDIF
C
C SAVE LAST PRINT LABEL FOR NEXT FORMAT LABEL CALL
C
      PRNT_LABEL.LSTAGTPRNT = PRNT_LABEL.AGENT(IDX)      
      PRNT_LABEL.LSTBNKPRNT = OPS_REC.BANK
C
C READ AGENT INFORMATION FROM ASF FILE
C
      CALL SEARCH_TERMINAL(PRNT_LABEL.AGENT(IDX), TERM, FOUND)
      IF(FOUND .EQ. .FALSE.) RETURN
      CALL READASF(TERM, ASFREC, FSTS)
      IF(FSTS .NE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), ' Error Reading Agent Sales File'
        TYPE *, IAM()
      ENDIF
C
C FORMAT LINE 1 ( AGENT - BANK CODE - DISTRIBUTION LINE - RECEPTION CENTER )
C
      WRITE(PRNT_LABEL.LINLBL(IDX, 2), 100) 
     *
     *      IAGT_NO(PRNT_LABEL.AGENT(IDX)),          ! AGENT NUMBER 
     *      OPS_REC.BANK,                            ! BANK NUMBER
     *      (ASFBYT(CNT), CNT = SLIND, ELIND),       ! DISTRIBUTION LINE
     *      (ASFBYT(CNT), CNT = SCENR, ECENR)        ! RECEPTION CENTER
C
C FORMAT LINE 2 ( AGENT NAME )
C
      WRITE(PRNT_LABEL.LINLBL(IDX, 4), 200) 
     *
     *     (ASFBYT(CNT), CNT = SNAME, SNAME + 34)    ! AGENT NAME ( 1 PART )
C
      WRITE(PRNT_LABEL.LINLBL(IDX, 5), 200) 
     *
     *     (ASFBYT(CNT), CNT = SNAME + 35, ENAME)    ! AGENT NAME ( 2 PART )
C
C FORMAT LINE 3 ( AGENT ADRESS )
C
      WRITE(PRNT_LABEL.LINLBL(IDX, 6), 400)          
     *      
     *     (ASFBYT(CNT), CNT = SSTRT, SSTRT + 34)    ! AGENT ADRESS ( 1 PART )
C
      WRITE(PRNT_LABEL.LINLBL(IDX, 7), 500) 
     *           
     *     (ASFBYT(CNT), CNT = SSTRT + 35, ESTRT)    ! AGENT ADRESS ( 2 PART )
C
C GET AGENT POSTAL CODE
C
      CALL ASCBIN(ASFINF, SZIPC, LZIPC, ZIPCODE, FSTS)
      IF(FSTS .NE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Zip Code Error For Agent: ', PRNT_LABEL.AGENT(IDX)
        TYPE *, IAM()
      ENDIF
C
C GET AGENT CITY NAME
C
      CALL MOVBYT(%REF(ASFBYT), SZIPA, %REF(CITYNAME), 1, EZIPA - SZIPA + 1)
C
C FORMAT LINE 4 ( AGENT POSTAL CODE - AGENT CITY )
C
      WRITE(PRNT_LABEL.LINLBL(IDX, 8), 600)
     *
     *      ZIPCODE / 1000,                 ! ZIP CODE ( FIRST PART )
     *      MOD(ZIPCODE, 1000),             ! ZIP CODE ( SECOND PART )
     *      CITYNAME(1:26)                  ! AGENT CITY NAME
C
C FORMATS DEFINITION TO WRITE FORMATED INFORAMTION IN LABELS BUFFER
C
100   FORMAT(A8, 2X, A4, 2X, <LLIND>A1, 2X, <LCENR>A1)
200   FORMAT(<MIN(35, LNAME)>A1)
300   FORMAT(<LNAME - 35>A1)
400   FORMAT(<MIN(35, LSTRT)>A1)
500   FORMAT(<LSTRT - 35>A1)
600   FORMAT(I4.4, '-', I3.3, X, A26)
C
C THIS IS THE END TO WRITE FORMATED INFORAMTION IN LABELS BUFFER
C
      END

C	********************************
	SUBROUTINE PRINT_ASTERISKS(GAME)
C	********************************
        IMPLICIT NONE                                                  
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRN_BUFF.DEF'

        INTEGER*4 GAME,GTYP,GIND,I,J

	GTYP = GNTTAB(GAMTYP,GAME)
        GIND = GNTTAB(GAMIDX,GAME)

        DO J=1,9  !3 pages having 6 OPs per page
          WRITE(PRN_BUFF.LUN,20) '***********************************',
     *                           '***********************************'
C
          DO I=1,7
            WRITE(PRN_BUFF.LUN,10)
          ENDDO
C
          IF(GTYP.EQ.TKIK) THEN
            WRITE(PRN_BUFF.LUN,50) '*   *   *   *   *   * ','*   *   *   *   *   * '
          ELSEIF(GTYP.EQ.TLTO) THEN
            IF(GIND .EQ. 3 .OR. GIND .EQ. 4) THEN
              WRITE(PRN_BUFF.LUN,60) '** ** ***** ******','** ** ***** ******'
            ELSE
              WRITE(PRN_BUFF.LUN,60) '** ** ***** ***  *','** ** ***** ***  *'
            ENDIF
          ELSEIF(GTYP.EQ.TSPT) THEN
            IF(GIND .EQ. 1) THEN
              WRITE(PRN_BUFF.LUN,70) '****************   *','****************   *'
            ELSE
              WRITE(PRN_BUFF.LUN,70) '****************    ','****************    '
            ENDIF
          ELSE
            WRITE(PRN_BUFF.LUN,10)
          ENDIF
C
          DO I=1,2
            WRITE(PRN_BUFF.LUN,10)
          ENDDO
C
          WRITE(PRN_BUFF.LUN,30) '**.***** ***/************* **/**/**** ***********,** Eur',
     *                           '**.***** ***/************* **/**/**** ***********,** Eur'
C
          DO I=1,4
            WRITE(PRN_BUFF.LUN,10)
          ENDDO
C
          WRITE(PRN_BUFF.LUN,40) '********< ***********+ **********> ************< 73+ ',
     *                           '********< ***********+ **********> ************< 73+ '
C
          DO I=1,5
            WRITE(PRN_BUFF.LUN,10)
          ENDDO
        ENDDO
C
	RETURN
C
10      FORMAT(132(' '))
20      FORMAT(2(24X,A35))
30      FORMAT(2(3X,A56))
40      FORMAT(2(6X,A53))
50      FORMAT(2(37X,A22))
60      FORMAT(2(41X,A18))
70      FORMAT(2(39X,A20))
C
	END
