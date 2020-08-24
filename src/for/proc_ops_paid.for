C
C PROC_OPS_PAID.FOR                                                                       
C
C
C V06 09-OCT-2012 FRP Fix change of year when reading bank OPs from previous year
C V05 24-DEC-2010 FRP Lotto2 Changes
C V04 06-APR-2009 FRP Modify for EM Joker
C V03 27-MAY-2001 EPH WHEN BUILDING OCR LINE TO CHECK, USE SCML GAME NUMBER SHIFTED (-1)
C V02 27-MAY-2001 EPH FOR OCR LINE USE ONLY 10 DIGITS ON THE RIGHT (WITHOUT DV)
C V01 27-APR-2001 EPH INITIAL VERSION FOR SCML
C                                                                               
C SEND OPS (ODJ FORMAT) TO BANK FROM OPSGENtt_yyyywww.FIL  FILE 
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
        PROGRAM PROC_OPS_PAID   
        IMPLICIT NONE                                                  
		                                                
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:OPS_REC.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:BANK_REC.DEF'
	INCLUDE 'INCLIB:BRANCH_REC.DEF'
	INCLUDE 'INCLIB:INTERFACES_REC.DEF'
                                                                               
        INTEGER*4   ODJERR_LUN /TMP1_LUN/
	INTEGER*4   RELPAID_LUN /TMP2_LUN/
        INTEGER*4   MSG_LUN /6/

	INTEGER*4   ST, SZ, STVER
	INTEGER*4   PAID_CDC
	INTEGER*4   TOTAL_GAME, TOTAL_JOKER
	INTEGER*2   DATE(12)
	INTEGER*4   BANK, BANKIND, BANKPOS
	INTEGER*4   GNUMLOTO2 /4/, GNUMLOTO /2/
	INTEGER*4   YESNO, OPCAO
	INTEGER*4   TOT_BANK_OP_AMT, TOT_BANK_OP_CNT
	INTEGER*4   BANK_OP_AMT(MAXBANKS), BANK_OP_CNT(MAXBANKS)
	INTEGER*4   DIA, MES, ANO
        INTEGER*4   YEAR, WEEK, GNUM, CCC, WEKCCC
        INTEGER*4   THISCCC,THISWEEK,THISYEAR

	LOGICAL     SPLITTED
	LOGICAL     ERRO

	CHARACTER   ODJERR_FILE*24
	CHARACTER   RELPAID_FILE*25
	CHARACTER   KEYOP*15, KEYOP2*15
	CHARACTER   STRING_ERR*130

        CALL COPYRITE                                                             
  	                                                                         
        TYPE*,IAM(),' '
        TYPE*,IAM(),'-------------------------------------------------------'
        TYPE*,IAM(),'<<<<<      PROCESSA OPS PAGAS (FICHEIROS ODJ)     >>>>>'
        TYPE*,IAM(),'-------------------------------------------------------'
        TYPE*,IAM(),'  GERA ODJERR_aaaammdd.REP  COM ERROS NO PROCESSAMENTO '
        TYPE*,IAM(),'  GERA RELPAID_aaaammdd.REP COM PRESTACAO DE CONTAS DE '
        TYPE*,IAM(),'                            PREMIOS PAGOS '
        TYPE*,IAM(),'-------------------------------------------------------'
	TYPE*,' '
C
C	NAME ERROR FILE AND RELPAID REPORT FILE
C
        DATE(VCDC) = DAYCDC
	CALL LCDATE(DATE)
	DIA = DATE(VDAY)
        MES = DATE(VMON)
	ANO = DATE(VYEAR) + 2000

	WRITE(ODJERR_FILE,  FMT='(A12,I4.4,I2.2,I2.2,A4)') 'FILE:ODJERR_',  ANO, MES, DIA, '.REP'
	WRITE(RELPAID_FILE, FMT='(A13,I4.4,I2.2,I2.2,A4)') 'FILE:RELPAID_', ANO, MES, DIA, '.REP'
	
800	TYPE*,' '
	TYPE*,' '
	TYPE*,' '
	TYPE*,' 1) VERIFICACAO DE ERROS NOS FICHEIROS ODJ'
	TYPE*,' '
	TYPE*,' 2) PROCESSAMENTO DOS PAGAMENTOS'
	TYPE*,' '
	TYPE*,' 3) SAI'
	TYPE*,' '
        TYPE*,' '
        TYPE*,' OPCAO:'
	ACCEPT *, OPCAO
	IF (OPCAO.NE.1 .AND. OPCAO.NE.2 .AND. OPCAO.NE.3) GOTO 800
	
	IF (OPCAO.EQ.3) STOP

	IF (OPCAO.EQ.2) THEN
           TYPE*,'-------------------------------------------------------------'
	   TYPE*,'>>> O ARQUIVO DE OPS SERA ATUALIZADO. '
           TYPE*,'    VERIFIQUE SE O BACKUP DO FICHEIRO  OPS.FIL  FOI FEITO <<<'
           TYPE*,'-------------------------------------------------------------'
	   CALL PRMYESNO('CONTINUA O PROCESSAMENTO (Y/N) ? ', YESNO)
           IF (YESNO.NE.1) GOTO 800
        ELSE
           TYPE*,'-----------------------------------------------------------'
	   TYPE*,'>>> REALIZANDO A VERIFICACAO DE ERROS NOS ARQUIVOS ODJ  <<<'
           TYPE*,'-----------------------------------------------------------'
	   CALL VERIFY_ODJ_FILES(ST)
	   IF (ST.NE.0) THEN
	      TYPE*, ' '
              TYPE*,'------------------------------------------------------------------'
	      TYPE*,'>>> HOUVE ERROS DURANTE A VERIFICACAO :  ' // ODJERR_FILE 
              TYPE*,'------------------------------------------------------------------'
           ENDIF
           GOTO 800
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
C	OPEN ERROR FILE
C	---------------
	OPEN (UNIT   = ODJERR_LUN,
     *        FILE   = ODJERR_FILE,
     *        STATUS = 'NEW',
     *        IOSTAT = ST)
	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error opening ODJERR file', 0, ' ', 0, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
	ENDIF
	WRITE(ODJERR_LUN,99)       !TITLE REPORT LINE
99	FORMAT(4X, 'BANCO  JOGO   ORDEM   SEMANA   AGENTE        VALOR                    NIB      BILHETE  BALCAO',/,
     *         1X, 130('-'),/)

C
C	OPEN RELPAID REPORT FILE AND WRITE TITLE (ONE PAGE ONLY)
C	---------------------------------------------------------
	OPEN (UNIT   = RELPAID_LUN,
     *        FILE   = RELPAID_FILE,
     *        STATUS = 'NEW',
     *        IOSTAT = ST)
	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error opening RELPAID report file', 0, ' ', 0, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
	ENDIF

        WRITE (RELPAID_LUN, 105) 1, DIA, MES, ANO
105     FORMAT(1X, 130('='), /, 
     *         1X,'SCML - Departamento de Jogos',T43,'PRESTACAO DE CONTAS DO PAGAMENTO DE PREMIOS',T123,'Pag.:',I4.4,/, 
     *         T122, I2.2,'/',I2.2,'/',I4.4,/,   
     *         1X, 130('='), /,
     *         1X, '              BANCO              ORD.PAGTO    IMPORTANCIA',/
     *         1X, 130('-'),/)

C
C     	OPEN ORDENS DE PAGAMENTO FILE
C       -----------------------------
        CALL OPEN_OPS('KEYED',ST)
        IF (ST.NE.0) THEN
           CALL DISPERR (MSG_LUN, 'Error opening ** OPS.FIL ** file. ', 0, ' ', 0, ' ', 0)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF             
C
C	OPEN ODJ FILES (FROM THE BANKS)
C	-------------------------------
        CALL OPEN_BNK (9999, 'R', 'ODJ', ST)
        IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error opening ODJ files.', 0, ' ', 0, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
        ENDIF

C
C	LOOP THROUGH ALL ODJ FILES FOUND
C	================================
C	
	ERRO = .FALSE.

	CALL FASTSET(0,BANK_OP_CNT,MAXBANKS)
	CALL FASTSET(0,BANK_OP_AMT,MAXBANKS)

	TOT_BANK_OP_AMT = 0
	TOT_BANK_OP_CNT = 0

	DO BANKIND = 1, BNK_REC.TOT_BANKR

	   BANK = BNK_REC.BANKR(BANKIND)

	   CALL GET_BANK_POS (BANK, BANKPOS)
	   IF (BANKPOS.EQ.0) THEN
              CALL DISPERR (MSG_LUN, 'Could not find the BANK in bank table.', 0, 'BANK= ', BANK, 
     *                     'Report will show values for this bank in the first BANK', 0)
	      BANKPOS = 1
	   ENDIF

	   TYPE*,' '
	   TYPE*,'>>> PROCESSANDO BANCO : ', BANK_TAB(BANKPOS).LONG_NAME
C	   
C	   READ ONE OF THE ODJ BANK FILES
C          ------------------------------
C
	   DO 300 WHILE (.TRUE.)

              CALL READ_BNK (BANK,ST)
              IF (ABS(ST).EQ.144) EXIT  !OUT OF READ LOOP
              IF (ST.NE.0) THEN
		 CALL ODJERR_ID_LIN (ODJERR_LUN, BANK)
	         CALL ODJERR_MESS_LIN (ODJERR_LUN, BNK_REC.ERRSTR)		 
	         ERRO = .TRUE.
	         GOTO 300       !READ NEXT ODJ REC
	      ENDIF
C
C	      BUILD KEY TO SEEK THIS OP IN OPS.FIL
C	      ------------------------------------
              YEAR = BNK_REC.ANO
              WEEK = BNK_REC.SEMANA
              GNUM = BNK_REC.GAME

              IF(GNUM.EQ.6 .OR. GNUM.EQ.7) THEN
                CCC = WEKCCC(YEAR,WEEK,GNUM)
C	              DETERMINE IF DRAW YEAR = THIS OR LAST ONE (BASED ON CCC OBTAINED FROM OCR LINE)
                CALL FIGCCC(DAYCDC,THISCCC,THISYEAR)
                IF(THISCCC .LT. CCC) THISYEAR = THISYEAR - 1
              ELSE
                CCC = WEEK
C	              DETERMINE IF CIVIL YEAR = THIS OR LAST ONE (BASED ON WEEK FROM OCR LINE)
                CALL FIGWEK(DAYCDC,THISWEEK,THISYEAR)
                IF(THISWEEK .LT. WEEK) THISYEAR = THISYEAR - 1
              ENDIF

	      WRITE (KEYOP, FMT='(I2.2,I4.4,I3.3,I6.6)') BNK_REC.GAME, THISYEAR, CCC, BNK_REC.ORDEM
C
C	      TRY TO FIND IT IN OP.FIL
C	      ------------------------

	      TOTAL_GAME  = 0
	      TOTAL_JOKER = 0
	      SPLITTED    = .FALSE.

	      READ (OPS_LUN, KEYID=0, KEYEQ=KEYOP, IOSTAT=ST) OPS_REC
	      IF (ST.EQ.0) THEN  !NO ERROR

	         IF (OPS_REC.SPLITTED .AND. BNK_REC.GAME.EQ.GNUMLOTO) THEN        
C
C	            HAVE TO FIND LOTO2 RECORD FOR THE OP AND KEEP SOME INFO FROM LOTO REC FOR ERROR VERIFYING
C
                    TOTAL_GAME  = OPS_REC.TOTAL_GAME
	            TOTAL_JOKER = OPS_REC.TOTAL_JOKER
		    SPLITTED = .TRUE.	            

	            WRITE (KEYOP2, FMT='(I2.2,I4.4,I3.3,I6.6)') GNUMLOTO2, BNK_REC.ANO, CCC, BNK_REC.ORDEM

	            READ (OPS_LUN, KEYID=0, KEYEQ=KEYOP2, IOSTAT=ST) OPS_REC

                 ENDIF

	      ENDIF
C
C             CHECK FOR ERRORS IN "ODJ" RECORD OR DISCREPANCY BETWEEN "ODJ" AND "OPS" FILE RECORD
C             AND WRITE IT TO "ODJERR" FILE
C	      ---------------------------------------------------------------------------------------
	      CALL VERIFY_FOR_ERRORS (SPLITTED, ST, TOTAL_GAME, TOTAL_JOKER, BANK, STRING_ERR, STVER)
	      IF (STVER.NE.0) THEN
		 CALL ODJERR_ID_LIN (ODJERR_LUN, BANK)
		 CALL ODJERR_MESS_LIN (ODJERR_LUN, STRING_ERR)
	         ERRO = .TRUE.
	         GOTO 300       !READ NEXT ODJ REC
              ENDIF
C
C	      THAT'S ALL FINE, SO UPDATES OP FILE RECORD WITH THE PAYMENT INFO
C	      ----------------------------------------------------------------

              DATE(VYEAR) = CTOI(BNK_REC.DATA_PROC(1:4),SZ)
              DATE(VMON)  = CTOI(BNK_REC.DATA_PROC(5:6),SZ)
              DATE(VDAY)  = CTOI(BNK_REC.DATA_PROC(7:8),SZ)
	      CALL BDATE(DATE)
	      PAID_CDC = DATE(VCDC)

	      OPS_REC.PAID_CDC = PAID_CDC
	      OPS_REC.PROC_PAID_CDC = DAYCDC

              REWRITE(OPS_LUN, IOSTAT=ST) OPS_REC
              IF (ST.NE.0) THEN
	         CALL DISPERR (MSG_LUN, 'Error writing payment info back to OP file.', 0, ' ', 0, ' ', 0)
      	         CALL GSTOP (GEXIT_FATAL)
              ENDIF

	      IF (SPLITTED) THEN        
C
C                AN OLD AND UNIQUE OP THAT IS EQUIVALENT TO 2 OP REC'S (LOTO AND LOTO2)
C	         I HAVE UPDATED LOTO2 REC, AND NOW HAVE TO UPDATE LOTO REC
C                MUST FIND IT BACK "AGAIN" TO PUT PAY INFO AND REWRITE IT
C
	         READ (OPS_LUN, KEYID=0, KEYEQ=KEYOP, IOSTAT=ST) OPS_REC
                 IF (ST.NE.0) THEN
	            CALL DISPERR (MSG_LUN, 'Error Locating LOTO REC back in OPS FILE', 0, 'BANK = ', BANK, ' ', 0)
      	            CALL GSTOP (GEXIT_FATAL)
                 ENDIF
	         OPS_REC.PAID_CDC = PAID_CDC
	         OPS_REC.PROC_PAID_CDC = DAYCDC
                 REWRITE(OPS_LUN, IOSTAT=ST) OPS_REC
                 IF (ST.NE.0) THEN
	            CALL DISPERR (MSG_LUN, 'Error writing LOTO payment info back to OP file.', 0, 'BANK = ', BANK, ' ', 0)
      	            CALL GSTOP (GEXIT_FATAL)
                 ENDIF
	      ENDIF
C
C	      STORE VALUES FOR REPORT
C
	      BANK_OP_AMT(BANKPOS) = BANK_OP_AMT(BANKPOS) + BNK_REC.VALUE
	      BANK_OP_CNT(BANKPOS) = BANK_OP_CNT(BANKPOS) + 1

	      TOT_BANK_OP_AMT = TOT_BANK_OP_AMT + BNK_REC.VALUE
	      TOT_BANK_OP_CNT = TOT_BANK_OP_CNT + 1

300	   CONTINUE    !INDIVIDUAL ODJ BANK FILE READING LOOP

	   WRITE(RELPAID_LUN,35) BANK_TAB(BANKPOS).LONG_NAME, BANK_OP_CNT(BANKPOS), CSMONY(BANK_OP_AMT(BANKPOS),11,BETUNIT)
35	   FORMAT(1X,A33,4X,I5,4X,A11)

	ENDDO   !LOOP FOR ALL ODJ BANK FILES

	WRITE(RELPAID_LUN,36) TOT_BANK_OP_CNT, CSMONY(TOT_BANK_OP_AMT,11,BETUNIT)
36	FORMAT(/,1X,130('-'),/,/
     *           1X,'TOTAL GERAL                          ',I5,4X,A11)

	CLOSE (OPS_LUN)
	CLOSE (RELPAID_LUN)
	CLOSE (ODJERR_LUN)
	CALL CLOSE_BNK (9999, ST)
	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error CLOSING ODJ FILES', 0, ' ', 0, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
        ENDIF

	IF (ERRO) THEN
	   CALL DISPERR (MSG_LUN, 'There were errors during ODJ files processing', 0, 
     *                            'Check on '//ODJERR_FILE, 0, ' ', 0)
	ENDIF

      	CALL GSTOP (GEXIT_SUCCESS)
	END



C	***********************************
        SUBROUTINE VERIFY_ODJ_FILES (STVER)
C	***********************************
C
C	CHECK AND NOTIFY ERROS IN ANY OF THE ODJ FILES AVAILABLE TO READ
C
	IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:OPS_REC.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:BANK_REC.DEF'
	INCLUDE 'INCLIB:BRANCH_REC.DEF'
	INCLUDE 'INCLIB:INTERFACES_REC.DEF'

        INTEGER*4   MSG_LUN /6/
	INTEGER*4   ODJERR_LUN /TMP1_LUN/

	INTEGER*4   STVER			!(OUTPUT)

	INTEGER*4   BANK, BANKIND
	INTEGER*4   ST
	INTEGER*2   DATE(12)
	INTEGER*4   DV
	INTEGER*4   GAMSHIFT

	CHARACTER   ODJERR_FILE*24
	CHARACTER   OCR_LINE*29
	CHARACTER   STRING_ERR*130
	
C
C	OPEN ERROR FILE
C	---------------
        DATE(VCDC) = DAYCDC
	CALL LCDATE(DATE)
        DATE(VYEAR) = DATE(VYEAR) + 2000
	WRITE(ODJERR_FILE, FMT='(A12,I4.4,I2.2,I2.2,A4)') 'FILE:ODJERR_', DATE(VYEAR), DATE(VMON), DATE(VDAY), '.REP'

	OPEN (UNIT   = ODJERR_LUN,
     *        FILE   = ODJERR_FILE,
     *        STATUS = 'NEW',
     *        IOSTAT = ST)
	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error opening ODJERR file', 0, ' ', 0, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
	ENDIF

	WRITE(ODJERR_LUN,99)       !TITLE REPORT LINE
99	FORMAT(4X, 'BANCO  JOGO   ORDEM   SEMANA   AGENTE        VALOR                    NIB      BILHETE  BALCAO',/,
     *         1X, 130('-'),/)

C
C	OPEN ODJ FILES (FROM THE BANKS)
C	-------------------------------
        CALL OPEN_BNK (9999, 'R', 'ODJ', ST)
        IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error opening ODJ files.', 0, BNK_REC.ERRSTR, 0, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
        ENDIF

C
C	LOOP THROUGH ALL ODJ FILES FOUND
C	================================
C	
	STVER = 0

	DO BANKIND = 1, BNK_REC.TOT_BANKR

	   BANK = BNK_REC.BANKR(BANKIND)

	   TYPE*, '>>> VERIFYING BANK : ', BANK
C	   
C	   READ ONE OF THE ODJ BANK FILES
C          ------------------------------
C
	   DO 300 WHILE (.TRUE.)

              CALL READ_BNK (BANK,ST)

              IF (ST.EQ.144) EXIT  !END OF FILE

              IF (ST.NE.0) THEN
                 IF (ST.LE.-60 .AND. ST.GE.-170 .AND. ST.NE.-144) THEN
		    CALL ODJERR_ID_LIN (ODJERR_LUN, BANK)		 
		    CALL ODJERR_MESS_LIN (ODJERR_LUN, BNK_REC.ERRSTR)		 
                 ELSE
	            WRITE(STRING_ERR,FMT='(A70, A12, I4.4)') BNK_REC.ERRSTR, '   BANCO = ', BANK 
		    CALL ODJERR_MESS_LIN (ODJERR_LUN, STRING_ERR)		 
	            EXIT
		 ENDIF
                 STVER = 1
              ENDIF
C
C	      CHECK FOR OCR LINE DV'S
C	      -----------------------
C
C	      BUILD OCR LINE 
C             ACCOUNT WITH 10 DIGITS (FIRST ONE IS DISCARDED)  -   BNK_REC.NIB_DT(10:19)
C
C	      MUST USE GAME NUMBER SHIFTED HERE (SCML CONVENTION), SINCE IT WAS THE ONE USED DURING DV GENERATION BEFORE
C
	      IF (BNK_REC.GAME.LE.9) THEN
	         GAMSHIFT = BNK_REC.GAME
	      ELSE
	         GAMSHIFT = BNK_REC.GAME - 1
	      ENDIF
	
	      WRITE (OCR_LINE, FMT='(A8,I1.1,A10,I2.2,I6.6,I2.2)') BNK_REC.NIB_DT(1:8), GAMSHIFT, BNK_REC.NIB_DT(10:19), 
     *                                                             BNK_REC.SEMANA_OCR, BNK_REC.ORDEM, BNK_REC.TIPO_DOC 

	      CALL CALC_DV_OCR (OCR_LINE, DV)

	      IF (DV.NE.BNK_REC.DV_OCR) THEN
                 WRITE(STRING_ERR,77) BNK_REC.DV_OCR, DV
77	         FORMAT ('ERRO DURANTE CALCULO DO DV DA LINHA INFERIOR DA OP  (DV RECEBIDO = ', I2.2, 
     *                   '/ DV CALCULADO = ', I2.2, ')')
	         STVER = 1
                 CALL ODJERR_ID_LIN (ODJERR_LUN, BANK)		 
	         CALL ODJERR_MESS_LIN (ODJERR_LUN, STRING_ERR)		 
	      ENDIF

300	   CONTINUE

	ENDDO

	CALL CLOSE_BNK (9999, ST)
	CLOSE(ODJERR_LUN)

	RETURN
	END


C	**********************************************************************************************
        SUBROUTINE VERIFY_FOR_ERRORS (SPLITTED, ST, TOTAL_GAME, TOTAL_JOKER, BANK, STRING_ERR, STVER)
C	**********************************************************************************************
C
C	CHECK AND NOTIFY ERRORS IN OPSERR REPORT
C
	IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:OPS_REC.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:BANK_REC.DEF'
	INCLUDE 'INCLIB:BRANCH_REC.DEF'
	INCLUDE 'INCLIB:INTERFACES_REC.DEF'

	LOGICAL   SPLITTED                  !THIS IS PART OF AN  UNIQUE OLD OP (LOTO+LOTO2)
	INTEGER*4 ST                        !STATUS OF OP FILE READING
	INTEGER*4 TOTAL_GAME, TOTAL_JOKER   !THESE ARE LOTO VALUES IN CASE OF SPLIT
	INTEGER*4 BANK

	CHARACTER STRING_ERR*(*)            !(OUTPUT) CONTAINS MESSAGE WITH CORRESPONDING ERROR
	INTEGER*4 STVER			    !(OUTPUT) <> 0 INDICATES THERE WAS AN ERROR WITH THE OP
	INTEGER*4 VALUE

	INTEGER*2 DATE(12)
	INTEGER*4 SZ
	
	STVER = 0
C
C	CHECK FOR OP FILE READING STATUS
C
	IF (ST.NE.0) THEN
	   WRITE (STRING_ERR, 78) ST
78	   FORMAT ('ORDEM NAO ENCONTRADA NO FICHEIRO DE OP.  STATUS ERRO = ', I4.4)
	   STVER = -20
	   RETURN
	ENDIF

	IF (SPLITTED) THEN
C	   MUST ADD VALUES FROM LOTO TO THIS OP RECORD (LOTO2)
	   VALUE = OPS_REC.TOTAL_GAME + OPS_REC.TOTAL_JOKER + TOTAL_GAME + TOTAL_JOKER
	ELSE
	   VALUE = OPS_REC.TOTAL_GAME + OPS_REC.TOTAL_JOKER 
        ENDIF
	IF (BNK_REC.VALUE .NE. VALUE) THEN
	   WRITE (STRING_ERR, 79) VALUE, BNK_REC.VALUE 
79	   FORMAT ('VALOR ENCONTRADO NO ARQUIVO OP = ', I11, ' / VALOR RECEBIDO NO ODJ = ', I11)
	   STVER = -30
	   RETURN
	ENDIF

	IF (OPS_REC.PROC_PAID_CDC.NE.0) THEN
	   WRITE (STRING_ERR, 80) OPS_REC.PROC_PAID_CDC 
80	   FORMAT ('O PAGAMENTO DESTA ORDEM DE PAGAMENTO JA FOI PROCESSADO NO CDC ', I5)
	   STVER = -40
	   RETURN
	ENDIF

	IF (OPS_REC.PAID_CDC.NE.0) THEN
	   WRITE (STRING_ERR, 97) OPS_REC.PAID_CDC 
97	   FORMAT ('ESTA ORDEM DE PAGAMENTO JA FOI PAGA PELO BANCO NO NO CDC ', I5)
	   STVER = -40
	   RETURN
	ENDIF

	DATE(VDAY)  = CTOI(BNK_REC.DATA_PROC(7:8),SZ)
	DATE(VMON)  = CTOI(BNK_REC.DATA_PROC(5:6),SZ)
	DATE(VYEAR) = CTOI(BNK_REC.DATA_PROC(1:4),SZ)
	CALL BDATE(DATE)
	IF (DATE(VCDC).GT.OPS_REC.PAYABLE_CDC+P(DAYHDPRG)) THEN
	   WRITE (STRING_ERR, 81) DATE(VDAY), DATE(VMON), DATE(VYEAR)+2000
81	   FORMAT ('DATA DO PAGAMENTO : ', I2.2, '/', I2.2, '/', I4.4, ' ESTA ALEM DA DATA LIMITE DE PAGAMENTO')
	   STVER = -50
	   RETURN
	ENDIF

	RETURN
	END



C	**********************************************
        SUBROUTINE ODJERR_ID_LIN (ODJERR_LUN, ODJBANK)
C	**********************************************
C
C	PRINTS ODJ RECORD INFO LINE IN ODJ ERROR FILE
C
	IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:OPS_REC.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:BANK_REC.DEF'
	INCLUDE 'INCLIB:BRANCH_REC.DEF'
	INCLUDE 'INCLIB:INTERFACES_REC.DEF'

	INTEGER*4  ODJERR_LUN
	INTEGER*4  ODJBANK
        INTEGER*4  YEAR, WEEK, GNUM, CCC, WEKCCC
        INTEGER*4   THISCCC,THISWEEK,THISYEAR

        YEAR = BNK_REC.ANO
        WEEK = BNK_REC.SEMANA
        GNUM = BNK_REC.GAME

        IF(GNUM.EQ.6 .OR. GNUM.EQ.7) THEN
          CCC = WEKCCC(YEAR,WEEK,GNUM)
C         DETERMINE IF DRAW YEAR = THIS OR LAST ONE (BASED ON CCC OBTAINED FROM OCR LINE)
          CALL FIGCCC(DAYCDC,THISCCC,THISYEAR)
          IF(THISCCC .LT. CCC) THISYEAR = THISYEAR - 1
        ELSE
          CCC = WEEK
C         DETERMINE IF CIVIL YEAR = THIS OR LAST ONE (BASED ON WEEK FROM OCR LINE)
          CALL FIGWEK(DAYCDC,THISWEEK,THISYEAR)
          IF(THISWEEK .LT. WEEK) THISYEAR = THISYEAR - 1
        ENDIF

	WRITE (ODJERR_LUN,50) ODJBANK, 
     *                        BNK_REC.GAME, 
     *                        BNK_REC.ORDEM, 
     *                        CCC, THISYEAR, 
     *                        BNK_REC.AGENTE,
     *			      BNK_REC.VALUE, 
     *                        BNK_REC.NIB_DT, 
     *                        BNK_REC.BILHETE, 
     *                        BNK_REC.BALCAO_TO_PAY   

50	FORMAT (1X,'ODJ',I4.4,'R', 4X, 
     *             I2.2, 2X, 
     *             I6.6, 2X, 
     *             I3.3, '/', I4.4, 2X,
     *             I7.7, 2X,
     *             I11, 2X
     *             A21, 2X, 
     *             A11, 4X,
     *             I4.4 )

	RETURN
	END


C	************************************************************
        SUBROUTINE ODJERR_MESS_LIN (ODJERR_LUN, STRING_ERR)
C	************************************************************
C
C	PRINTS ODJ RECORD INFO LINE IN ODJ ERROR FILE
C
	IMPLICIT NONE

	INTEGER*4  ODJERR_LUN
	CHARACTER  STRING_ERR*(*)

	WRITE (ODJERR_LUN, 10) STRING_ERR
10      FORMAT (1X, '>>> ',A125,/)

	STRING_ERR = ' '

	RETURN
	END
