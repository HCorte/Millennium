C
C DUMP_OPS.FOR                                                                    
C
C V04 24-DEC-2010 FRP Lotto2 Changes
C V03 25-NOV-2005 CMB SEE ALL OP NUMBERS(10 DIGIT) 
C V02 19-JUN-2001 EPH ALLOW TO SEND SELECTED OPS TO A FILE
C V01 02-JAN-2001 EPH INITIAL VERSION FOR SCML
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
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
      PROGRAM DUMP_OPS 
      IMPLICIT NONE                                                  
                                                
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF' 
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:RECSCF.DEF'
      INCLUDE 'INCLIB:DATBUF.DEF'
      INCLUDE 'INCLIB:OPS_REC.DEF'
      INCLUDE 'INCLIB:DLTREC.DEF'
      INCLUDE 'INCLIB:DSPREC.DEF'
      INCLUDE 'INCLIB:DBNREC.DEF'
      INCLUDE 'INCLIB:AGTCOM.DEF'
      INCLUDE 'INCLIB:WINCOM.DEF'            
      INCLUDE 'INCLIB:BANK_REC.DEF'           !v03
      INTEGER*4 ST, POS, YESNO
      CHARACTER CHAVE*15, CHAVE_FILE*15, AUX1*12, AUX2*12
      INTEGER*4 TOT_GAME, TOT_JOKER
C     CHARACTER*100 X
      LOGICAL END_TASK      ! WE HAVE TO FINISH THE TASK OR NOT
C
C DISPLAY URSER PRESENTATION
C
1000  CONTINUE
      CALL CLRSCR(6)
                                                                         
      TYPE *,'======================================================================'
      TYPE *,'<<<<<               LISTA OPS NO ARQUIVO DE OPS                  >>>>>'                  
      TYPE *,'======================================================================'
      TYPE *,'    EXIBE TODAS AS OPS COINCIDENTES COM A INICIAL DA CHAVE ABAIXO     '
      TYPE *,'======================================================================'
C
C       OPEN ORDENS DE PAGAMENTO FILE
C       *****************************
	CALL OPEN_OPS('KEYED', ST)
        IF (ST.NE.0) THEN
           TYPE*,'>>> ERRO NA ABERTURA DO ARQUIVO DE OPS'
	   STOP
        ENDIF
        



	TYPE*,'>>> Entre com a CHAVE de pesquisa (GGAAAACCCOOOOOO)  (E -> SAI)'
	TYPE*,'    onde GG      = Numero do Jogo (zero a esquerda)'
	TYPE*,'         AAAACCC = Ano e semana (zero a esquerda da semana)'
	TYPE*,'         OOOOOO  = Numero da OP (zeros a esquerda)'

	READ(5,12) CHAVE
12      FORMAT(A15)

	IF (CHAVE(1:1).EQ.'E') STOP

	CALL PRMYESNO('Deseja enviar para um ficheiro (Y/N) ? ', YESNO)
        IF (YESNO.EQ.1) THEN 
	   TYPE*,'  '
           TYPE*,'>>> OPS serão gravadas em  * DUMP_OPS.LIS *'
	   TYPE*,'  '
	   OPEN(1,FILE='DUMP_OPS.LIS', STATUS='NEW')
           CALL WRITE_OP_TITLE(1)
	ENDIF

        IF (CHAVE(3:9).EQ.'       ') THEN
           POS = 2
        ELSEIF(CHAVE(10:15).EQ.'      ') THEN
           POS = 9
        ELSE
           POS = 15
        ENDIF

	TOT_JOKER = 0
        TOT_GAME  = 0

        READ(OPS_LUN, KEYID=0, KEYEQ=CHAVE(1:POS), ERR=900) OPS_REC

600     CONTINUE

        IF (YESNO.EQ.1) THEN
	   CALL WRITE_OP_LINE(1)
        ELSE
	   CALL SHOW_OP()
C	   READ(5,FMT='(A100)') X
C          IF (X(1:1).EQ.'E') STOP
        ENDIF

	TOT_JOKER = TOT_JOKER + OPS_REC.TOTAL_JOKER
	TOT_GAME  = TOT_GAME  + OPS_REC.TOTAL_GAME

        READ(OPS_LUN, END=700, ERR=900) OPS_REC

	CHAVE_FILE = OPS_REC.GAME // OPS_REC.YEARWEEK // OPS_REC.ORDER

	IF (CHAVE(1:POS).EQ.CHAVE_FILE(1:POS)) GOTO 600

700	CLOSE (OPS_LUN)

	AUX1 = CMONY(TOT_GAME,12,VALUNIT)
	AUX2 = CMONY(TOT_JOKER,12,VALUNIT)
	TYPE*,'JOGO =' // AUX1 // '    JOKER =' // AUX2
        CALL ASK_USER_FOR_END_TASK(END_TASK)
        IF(END_TASK .EQ. .FALSE.) GOTO 1000
	STOP

900     TYPE*,'  '
	TYPE*, '>>> Nenhuma Ordem foi encontrada.'
	TYPE*,'  '
	CLOSE (OPS_LUN)
	IF (YESNO.EQ.1) THEN
           CLOSE(1)
        ENDIF
        CALL ASK_USER_FOR_END_TASK(END_TASK)
        IF(END_TASK .EQ. .FALSE.) GOTO 1000
	STOP

	END


C	***********************************
	SUBROUTINE WRITE_OP_LINE (LUN)
C	***********************************
	IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:OPS_REC.DEF'

	INTEGER*4 LUN
	CHARACTER WHODID*3
	CHARACTER TIPO*3
	CHARACTER TIPOPRZ*3
	INTEGER*4 K, SZ,ST
	CHARACTER*3   TP             !V03
	CHARACTER*9   ZONA_INTER     !V03
        CHARACTER*12  CONTA          !V03
        CHARACTER*15  CONTA_PREMIO   !v03 AAAACCCCCCCCCCC  = agencia(4)/conta(11)
        CHARACTER*10  NUMERO_ORDEM   !V03
        CHARACTER*29  OCR_LINE       !V03
        INTEGER*4     DV             !V03
        CHARACTER*1   GAUX           !V03
        INTEGER*4     GAME           !V03

	IF (OPS_REC.ONLINE_ORDER) THEN
	   WHODID = 'ON '
	ELSE
	   WHODID = 'OFF'
	ENDIF	
	IF (OPS_REC.CLAIM) THEN
	   TIPO = 'CLM'
	ELSE
	   TIPO = 'REG'
        ENDIF
	IF (OPS_REC.HI_PRIZE) THEN
	   TIPOPRZ = 'HI '
        ELSE
	   TIPOPRZ = 'LOW'
	ENDIF
	
C       LOAD BANK TABLE STRUCTURE
C       -------------------------
        CALL LOAD_BANK_TABLE(ST)
        IF (ST.NE.0) THEN
           Type*, 'Error loading BANK TABLE'
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
	
CC GET BANK ACCOUNT NUMBER TO CALCULATE DV	   !V03
c-----------------------------------------------
	
	   CALL GET_BANK_ACC (OPS_REC.BANK, CONTA_PREMIO, ST)
        IF (ST.NE.0) THEN
           TYPE*, 'BANK not Found : '//ops_rec.bank//' From AGENT '//ops_rec.agent
           tYPE*, '*** PROGRAMA ABORTADO ***'
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
        
C       GET GAME NUMBER
C       ---------------
        GAME = CTOI(OPS_REC.GAME,SZ)
        
        
C       ZONA INTERBANCARIA
C       ------------------
        ZONA_INTER = OPS_REC.BANK // CONTA_PREMIO(1:4) 
C
C       NUMERO DA CONTA
C       ---------------
C
C       ANY GAME GREATER THAN 9 IS SUBTRACTED OF 1 (SCML SPECIFICATION)
C        
        IF (GAME.GT.9) THEN
           WRITE(GAUX,FMT='(I1.1)') GAME-1
        ELSE
           WRITE(GAUX,FMT='(I1.1)') GAME
        ENDIF
        CONTA = GAUX // CONTA_PREMIO(6:15) // '+'     
C
C       TIPO
C       ----
        TP = '73+'       
        
C       NUMERO DA ORDEM
C       ---------------
C       CALCULATES DV FOR OCR LINE TO BE INSERTED IN NUMERO_ORDEM
C
        OCR_LINE = ZONA_INTER(1:8) // CONTA(1:11) // OPS_REC.CWEEK(1:2) // OPS_REC.ORDER // TP(1:2)
        CALL CALC_DV_OCR (OCR_LINE,DV)
            WRITE (NUMERO_ORDEM, FMT='(I2.2,A2,A6)') DV, OPS_REC.CWEEK(1:2), OPS_REC.ORDER 


	WRITE(LUN,50) GSNAMES(CTOI(OPS_REC.GAME,SZ)),
     *                OPS_REC.YEARWEEK(5:7), OPS_REC.YEARWEEK(3:4),
     *                NUMERO_ORDEM,
C     *                OPS_REC.ORDER,
     *                OPS_REC.BILHETE,
     *                OPS_REC.AGENT(1:2), OPS_REC.AGENT(3:7),
     *		      WHODID,
     *	              TIPO,
     *		      TIPOPRZ,
     *	              (OPS_REC.WINS(K),K=1,6),
     *	              CMONY(OPS_REC.TOTAL_GAME,11,VALUNIT),
     *	              OPS_REC.JOKER_DIV,
     *	              CMONY(OPS_REC.TOTAL_JOKER,11,VALUNIT),
     *	              OPS_REC.BANK,
     *		      OPS_REC.BRANCH,
     *                OPS_REC.PAID_CDC

50	FORMAT(1X,A4, 2X, A3,'/',A2, 3X, A10, 2X, A14, 2X, A2,'.',A5,
     *         2X, A3, 2X, A3, 1X, A3, 1X, 6I3, 1X, A11, 2X, 
     *         I2, 2X, A11, 2X, A4, 2X, A4, 2X, I4)
  
	RETURN
	END	


C	***********************************
	SUBROUTINE WRITE_OP_TITLE (LUN)
C	***********************************
	IMPLICIT NONE
	INTEGER*4 LUN
	WRITE(LUN,20)
20	FORMAT(' GAME  SORTEIO    ORDER       BILHETE       AGENTE    TIPO OP  PRZ   1  2  3  4  5  6    '
     *         'VAL.GAME  JDV   VAL.JOKER  BANK  BALC  PCDC',/,1X,131('-'))
	RETURN
	END
	
C	***************************************************************
          SUBROUTINE GET_BANK_ACC (BANK, CONTA_PREMIO, ST)
C       ***************************************************************
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:BANK_REC.DEF'

        CHARACTER*(*) BANK
        CHARACTER*(*) CONTA_PREMIO
        INTEGER*4 ST, I

        ST = -1
        DO I=1,MAXBANKS
           IF (BANK_TAB(I).BANK.EQ.BANK) THEN
              CONTA_PREMIO = BANK_TAB(I).CONTA_PREMIO
              ST = 0
              EXIT
           ENDIF
        ENDDO

        RETURN
        END
C******************************************************************************
      SUBROUTINE SHOW_OP 
      IMPLICIT NONE                                                  
******************************************************************************                                                
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF' 
      INCLUDE 'INCLIB:DATBUF.DEF' 
      INCLUDE 'INCLIB:CONCOM.DEF' 
      INCLUDE 'INCLIB:OPS_REC.DEF'

	CHARACTER*10 DATE1, DATE2, DATE3
	CHARACTER*8  IAGT_NO      !FUNCTION
	INTEGER*4    SZ, ST
	CHARACTER*4  GAM_NAME
	INTEGER*2    DATE(12)
	
	CHARACTER*3   TP             !V03
	CHARACTER*9   ZONA_INTER     !V03
        CHARACTER*12  CONTA          !V03
        CHARACTER*15  CONTA_PREMIO   !v03 AAAACCCCCCCCCCC  = agencia(4)/conta(11)
        CHARACTER*10  NUMERO_ORDEM   !V03
        CHARACTER*29  OCR_LINE       !V03
        INTEGER*4     DV             !V03
        CHARACTER*1   GAUX           !V03
        INTEGER*4     GAME           !V03

	CHARACTER*3  SIMNAO   !FUNCTION	

	CHARACTER*14 MANUALPAY

	WRITE(GAM_NAME,FMT='(A4)') GSNAMES(CTOI(OPS_REC.GAME,SZ))

        IF (OPS_REC.PROC_PAID_CDC.GT.0) THEN
	   DATE(VCDC) = OPS_REC.PROC_PAID_CDC
	   CALL LCDATE(DATE)
           WRITE(DATE1,FMT='(I2.2,A1,I2.2,A1,I4.4)') DATE(VDAY), '/', DATE(VMON), '/', 2000+DATE(VYEAR)
        ELSE
           DATE1 = '00/00/0000'
        ENDIF

        IF (OPS_REC.PAYABLE_CDC.NE.0) THEN
	   DATE(VCDC) = OPS_REC.PAYABLE_CDC
	   CALL LCDATE(DATE)
           WRITE(DATE2,FMT='(I2.2,A1,I2.2,A1,I4.4)') DATE(VDAY), '/', DATE(VMON), '/', 2000+DATE(VYEAR)
        ELSE
           DATE2 = '00/00/0000'
        ENDIF

        IF (OPS_REC.PAID_CDC.NE.0) THEN
	   DATE(VCDC) = OPS_REC.PAID_CDC
	   CALL LCDATE(DATE)
           WRITE(DATE3,FMT='(I2.2,A1,I2.2,A1,I4.4)') DATE(VDAY), '/', DATE(VMON), '/', 2000+DATE(VYEAR)
        ELSE
           DATE3 = '00/00/0000'
        ENDIF

	IF (OPS_REC.PAID_CDC.NE.0 .AND. OPS_REC.PAID_MANUALLY) THEN
           MANUALPAY = ' -> MANUAL PAY'
        ELSE
           MANUALPAY = '              '
        ENDIF
        
C       LOAD BANK TABLE STRUCTURE
C       -------------------------
        CALL LOAD_BANK_TABLE(ST)
        IF (ST.NE.0) THEN
           Type*, 'Error loading BANK TABLE'
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
	
CC GET BANK ACCOUNT NUMBER TO CALCULATE DV	   !V03
c-----------------------------------------------
	
	   CALL GET_BANK_ACC (OPS_REC.BANK, CONTA_PREMIO, ST)
        IF (ST.NE.0) THEN
           TYPE*, 'BANK not Found : '//ops_rec.bank//' From AGENT '//ops_rec.agent
           tYPE*, '*** PROGRAMA ABORTADO ***'
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
        
C       GET GAME NUMBER
C       ---------------
        GAME = CTOI(OPS_REC.GAME,SZ)
        
        
C       ZONA INTERBANCARIA
C       ------------------
        ZONA_INTER = OPS_REC.BANK // CONTA_PREMIO(1:4) 
C
C       NUMERO DA CONTA
C       ---------------
C
C       ANY GAME GREATER THAN 9 IS SUBTRACTED OF 1 (SCML SPECIFICATION)
C        
        IF (GAME.GT.9) THEN
           WRITE(GAUX,FMT='(I1.1)') GAME-1
        ELSE
           WRITE(GAUX,FMT='(I1.1)') GAME
        ENDIF
        CONTA = GAUX // CONTA_PREMIO(6:15) // '+'     
C
C       TIPO
C       ----
        TP = '73+'       
        
C       NUMERO DA ORDEM
C       ---------------
C       CALCULATES DV FOR OCR LINE TO BE INSERTED IN NUMERO_ORDEM
C
        OCR_LINE = ZONA_INTER(1:8) // CONTA(1:11) // OPS_REC.CWEEK(1:2) // OPS_REC.ORDER // TP(1:2)
        CALL CALC_DV_OCR (OCR_LINE,DV)
            WRITE (NUMERO_ORDEM, FMT='(I2.2,A2,A6)') DV, OPS_REC.CWEEK(1:2), OPS_REC.ORDER 
            
            

	TYPE*,'================================================================================'
        TYPE*,'GAME             = ',OPS_REC.GAME, '  (',GAM_NAME,')'
        TYPE*,'DRAW             = ',OPS_REC.YEARWEEK(5:7),'/',OPS_REC.YEARWEEK(3:4)
C        TYPE*,'ORDER            = ',OPS_REC.ORDER
        TYPE*,'ORDER (10 DIG)   = ',NUMERO_ORDEM
        TYPE*,'BILHETE          = ',OPS_REC.BILHETE
        TYPE*,'SPLIT FLAG       = ',SIMNAO(OPS_REC.SPLITTED)
        TYPE*,'AGENT            = ',IAGT_NO(CTOI(OPS_REC.AGENT,SZ))        !OPS_REC.AGENT
        TYPE*,'PRINTED_BY_OFF   = ',SIMNAO(OPS_REC.PRINTED_BY_OFF)
	TYPE*,'CLAIM            = ',SIMNAO(OPS_REC.CLAIM)
        TYPE*,'PAID OP PROC CDC = ',DATE1, '  ( CDC =', OPS_REC.PROC_PAID_CDC, ' )'
        TYPE*,'GENERATED        = ',SIMNAO(OPS_REC.GENERATED)
        TYPE*,'PAYABLE_CDC      = ',DATE2, '  ( CDC =', OPS_REC.PAYABLE_CDC, ' )'
        TYPE*,'PAID_CDC         = ',DATE3, '  ( CDC =', OPS_REC.PAID_CDC,' ) ' // MANUALPAY
        TYPE*,'PAID_SENT_SAP    = ',SIMNAO(OPS_REC.PAID_SENT_SAP)
        TYPE*,'ONLINE_ORDER     = ',SIMNAO(OPS_REC.ONLINE_ORDER)
        TYPE*,'HI_PRIZE         = ',SIMNAO(OPS_REC.HI_PRIZE)
	WRITE(6,19) OPS_REC.WINS(1), 
     *              OPS_REC.WINS(2), 
     *              OPS_REC.WINS(3), 
     *              OPS_REC.WINS(4), 
     *              OPS_REC.WINS(5), 
     *              OPS_REC.WINS(6), 
     *              OPS_REC.JOKER_DIV
19	FORMAT(1X, 'DIV 1    DIV 2    DIV 3    DIV 4    DIV 5    DIV 6      JOKER DIV',/,
     *         1X, I5, 4X, I5, 4X, I5, 4X, I5, 4X, I5, 4X, I5, 10X, I1)
        TYPE*,'TOTAL_GAME       = ',CMONY (OPS_REC.TOTAL_GAME, 12, VALUNIT)
        TYPE*,'TOTAL_JOKER      = ',CMONY (OPS_REC.TOTAL_JOKER, 12, VALUNIT)
        WRITE(6,18) OPS_REC.BANK, OPS_REC.BRANCH
18	FORMAT(1X,'BANK  =  ', A4, '    /    BRANCH  =  ', A4)
	TYPE*,'================================================================================'

C	RETURN
	END


	CHARACTER*3 FUNCTION SIMNAO (LOGVAR)
	LOGICAL LOGVAR
	IF (LOGVAR) THEN
           SIMNAO = 'SIM'
        ELSE
           SIMNAO = 'NÃO'
        ENDIF
        RETURN
	END

C ******************************************************************************
C
C     SUBROUTINE: ASK_USER_FOR_END_TASK
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 13 / 07 / 2001
C
C ******************************************************************************
C
C FUNCTION TO ASK TO THE USER IF SHE/HE WANTS END TASK OR NOT
C
C=======OPTIONS /CHECK = NOOVERFLOW /EXT
      SUBROUTINE ASK_USER_FOR_END_TASK(END_TASK)
      IMPLICIT NONE
C
C INCLUDES DEFINITION TO ASK TO THE USER IF SHE/HE WANTS END TASK OR NOT
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
C PARAMETERS DEFINITION TO ASK TO THE USER IF SHE/HE WANTS END TASK OR NOT
C
      LOGICAL END_TASK       ! WE WANT TO EN TASK OR NOT
C
C VARIABLES DEFINITION TO ASK TO THE USER IF SHE/HE WANTS END TASK OR NOT
C
      INTEGER * 4 ANSW       ! USER ANSWER
C
C INITIATE VARIABLES TO ASK TO THE USER IF SHE/HE WANTS END TASK OR NOT
C
      END_TASK = .TRUE.
C
C ASK TO USER IF HE/SHE WANTS END TASK OR NOT 
C
      TYPE *, IAM()
      CALL PRMYESNO('Quer Visualizar Outra Ordem De Pagamento [Y/N] ?', ANSW)
      IF(ANSW .EQ. 1) END_TASK = .FALSE.
C
C THIS IS THE END TO ASK TO THE USER IF SHE/HE WANTS END TASK OR NOT
C
      END


