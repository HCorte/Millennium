C
C SEND_OPS_PAID_SAP.FOR                                                                       
C
C
C V04 30-SEP-2011 FJG Change WEEK size
C V03 13-DEC-2010 FRP Lotto2 Changes
C V02 27-JUN-2001 EPH SEND CLAIMS TO SAP (NOW)
C V01 02-MAY-2001 EPH INITIAL VERSION FOR SCML
C                                                                               
C GENERATE OPS PAID FILE TO SAP AND GENERATE CORRESPONDING REPORT
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
        PROGRAM SEND_OPS_PAID_SAP
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
                                                                               
        INTEGER*4    WORSAP_LUN /TMP1_LUN/
	INTEGER*4    REP_LUN /TMP2_LUN/
	INTEGER*4    CASHOPS_LUN /99/
        INTEGER*4    MSG_LUN /6/

	INTEGER*4    ST, SZ, K
	INTEGER*2    DATE(12)

	INTEGER*4    DIA, MES, ANO
	INTEGER*4    CDCPROC

	CHARACTER    REP_FILE*25
	CHARACTER    DATAPROC*10

	CHARACTER*7  SEMANA(120)
	INTEGER*4    SEMANA_CNT /0/
	INTEGER*4    SEMANA_POS
	CHARACTER*7  LAST_YEARWEEK*7

	INTEGER*4    LAST_GAME

	INTEGER*4    OP_AMT (MAXGAM, 120, MAXBANKS)
	INTEGER*4    OP_CNT (MAXGAM, 120, MAXBANKS)

	INTEGER*4    GAM, SEMP, BNKP
	INTEGER*4    AMT, CNT
	INTEGER*4    GAM_AMT, GAM_CNT
        INTEGER*4    TOTAL_SAP, TOTAL_SAP2
        INTEGER*4    TOTAL_CLAIM
	INTEGER*4    PAID_MANUALLY

	LOGICAL	     PRINTED_GAME_TITLE
	INTEGER*4    NUM_JOKER /5/
	INTEGER*4    NUM_LOTO  /2/
	INTEGER*4    NUM_LOTO2 /4/
	INTEGER*4    BANKPOS
	INTEGER*4    PAGE
	INTEGER*4    GAMS_IN_PAGE
	LOGICAL      TEM_VALOR

	CHARACTER    CASHOPS_FILE*25
	LOGICAL      SENDSAP

        CALL COPYRITE                                                             
  	                                                                         
        TYPE*,IAM(),' '
        TYPE*,IAM(),'---------------------------------------------------'
        TYPE*,IAM(),'<<<<< GERA  WORSAP_aaaammdd.ASC  (FICHEIRO P/ SAP) '
        TYPE*,IAM(),'<<<<< GERA  WORSAP_aaaammdd.REP  (RELAT. CONC/JOGO)'
	TYPE*,IAM(),'<<<<< GERA  CASHOPS_aaaammdd.LIS (ORDENS PAGAS)    '
        TYPE*,IAM(),'---------------------------------------------------'
	TYPE*,' '
	TYPE*,' '
	TYPE*,' '


	SENDSAP = .FALSE.

C
C	CHECK ABOUT WICH DATE IS WANTED
C
500	TYPE*,'QUAL O DIA DE PROCESSAMENTO DAS OPS PAGAS (dd/mm/aaaa) ? '
	READ(5,13) DATAPROC
13	FORMAT(A10)
	DIA = CTOI(DATAPROC(1:2),SZ)
	MES = CTOI(DATAPROC(4:5),SZ)
	ANO = CTOI(DATAPROC(7:10),SZ)
	IF (DIA.LE.0 .OR. DIA.GT.31) GOTO 500
	IF (MES.LE.0 .OR. MES.GT.12) GOTO 500
	IF (ANO.LE.1900 .OR. ANO.GT.2100) GOTO 500
	TYPE*,' '
C
C	CONVERT TO CDC
C	
	DATE(VDAY)  = DIA
	DATE(VMON)  = MES
	DATE(VYEAR) = ANO
	CALL BDATE(DATE)
	CDCPROC = DATE(VCDC)

C
C       LOAD BANK TABLE STRUCTURE
C       -------------------------
        CALL LOAD_BANK_TABLE(ST)
        IF (ST.NE.0) THEN
           CALL DISPERR (MSG_LUN, 'Error loading BANK TABLE', 0, 'STATUS = ', ST, ' ', 0)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
C
C	OPEN CASHED OPS LIST FILE
C
	WRITE(CASHOPS_FILE, FMT='(A13,I4.4,I2.2,I2.2,A4)') 'FILE:CASHOPS_', ANO, MES, DIA, '.LIS'
	OPEN (UNIT    = CASHOPS_LUN,
     *        FILE    = CASHOPS_FILE,
     *        STATUS  = 'NEW',
     *        IOSTAT  = ST)
        IF (ST.NE.0) THEN
           CALL DISPERR (MSG_LUN, 'Error OPENING CASHED OPS LIST FILE', 0, 'STATUS = ', ST, ' ', 0)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
	CALL WRITE_OP_TITLE(CASHOPS_LUN)
C
C     	OPEN ORDENS DE PAGAMENTO FILE
C
        CALL OPEN_OPS('SEQUENTIAL',ST)
        IF (ST.NE.0) THEN
           CALL DISPERR (MSG_LUN, 'Error opening ** OPS.FIL ** file. ', 0, ' ', 0, ' ', 0)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF             

	TYPE*,' '
	TYPE*,'>>> Aguarde... lendo arquivo de OPs...'
	TYPE*,' '

	READ (OPS_LUN, END=200, IOSTAT=ST) OPS_REC
	IF (ST.NE.0) THEN  
           CALL DISPERR (MSG_LUN, 'Error reading ** OPS.FIL ** file. ', 0, ' ', 0, ' ', 0)
           CALL GSTOP (GEXIT_FATAL)
	ENDIF

	CALL FASTSET (0, OP_AMT, MAXGAM * 50 * MAXBANKS)
	CALL FASTSET (0, OP_CNT, MAXGAM * 50 * MAXBANKS)

	TOTAL_SAP2    = 0
	TOTAL_CLAIM   = 0
	PAID_MANUALLY = 0

	DO WHILE (.TRUE.)

	   LAST_GAME = CTOI(OPS_REC.GAME,SZ)

	   DO WHILE (CTOI(OPS_REC.GAME,SZ).EQ.LAST_GAME)

              LAST_YEARWEEK = OPS_REC.YEARWEEK

	      CALL UPD_WEEK (OPS_REC.YEARWEEK, SEMANA, SEMANA_CNT, SEMANA_POS)

              DO WHILE (OPS_REC.YEARWEEK.EQ.LAST_YEARWEEK .AND. CTOI(OPS_REC.GAME,SZ).EQ.LAST_GAME)        

	         IF (OPS_REC.PROC_PAID_CDC.NE.CDCPROC) GOTO 110	   !READ NEXT

                 IF (OPS_REC.PAID_SENT_SAP) THEN
                    SENDSAP = .TRUE.   !ALREADY SENT TO SAP (JUMP IT)
                    GOTO 110           !READ NEXT
                 ELSE
                    OPS_REC.PAID_SENT_SAP = .TRUE.
                    REWRITE(OPS_LUN,IOSTAT=ST) OPS_REC
                    IF (ST.NE.0) THEN
                       CALL DISPERR (MSG_LUN, 'Error REWRITING to OPS.FIL.', 0, ' ', 0, ' ', 0)
                       CALL GSTOP (GEXIT_FATAL)
                    ENDIF
                 ENDIF

		 CALL WRITE_OP_LINE(CASHOPS_LUN)

	         IF (OPS_REC.CLAIM) THEN
		    TOTAL_CLAIM = TOTAL_CLAIM + OPS_REC.TOTAL_GAME + OPS_REC.TOTAL_JOKER
C	!V02	    GOTO 110                       !DON'T WANT CLAIMS SENT TO SAP OR IN REPORT
		 ENDIF                 

                 IF (CTOI(OPS_REC.BANK,SZ).EQ.0) THEN
                    CALL DISPERR (MSG_LUN, 'OP '//OPS_REC.ORDER//' has BANK number equals to zero', 0, ' ', 0, ' ', 0)
                    GOTO 110
		 ENDIF

		 CALL GET_BANK_POS (CTOI(OPS_REC.BANK,SZ), BANKPOS)
	         IF (BANKPOS.EQ.0) THEN
                    CALL DISPERR (MSG_LUN, 'OP '//OPS_REC.ORDER//' has INVALID BANK number.', 0, 'BANK = ',
     *                            CTOI(OPS_REC.BANK,SZ), ' ', 0)
                    GOTO 110
		 ENDIF

                 TOTAL_SAP2 = TOTAL_SAP2 + OPS_REC.TOTAL_GAME + OPS_REC.TOTAL_JOKER
                 IF (OPS_REC.PAID_MANUALLY) THEN
		    PAID_MANUALLY = PAID_MANUALLY + OPS_REC.TOTAL_GAME + OPS_REC.TOTAL_JOKER
		 ENDIF

                 IF (OPS_REC.TOTAL_GAME.GT.0) THEN
		    OP_CNT(LAST_GAME,SEMANA_POS,BANKPOS) = OP_CNT(LAST_GAME,SEMANA_POS,BANKPOS) + 1
		    OP_AMT(LAST_GAME,SEMANA_POS,BANKPOS) = OP_AMT(LAST_GAME,SEMANA_POS,BANKPOS) + OPS_REC.TOTAL_GAME
		    IF (LAST_GAME.EQ.NUM_LOTO .AND. OPS_REC.SPLITTED) THEN
C
C	               TAKE COUNT OUT OF LOTO2 (OR WILL HAVE DUPLICITY)
C
		       OP_CNT(NUM_LOTO2,SEMANA_POS,BANKPOS) = OP_CNT(NUM_LOTO2,SEMANA_POS,BANKPOS) - 1
	            ENDIF
                 ENDIF
                 IF (OPS_REC.TOTAL_JOKER.GT.0) THEN
                    IF (OPS_REC.TOTAL_GAME.EQ.0 .AND. .NOT.OPS_REC.SPLITTED) THEN
		       OP_CNT(NUM_JOKER,SEMANA_POS,BANKPOS) = OP_CNT(NUM_JOKER,SEMANA_POS,BANKPOS) + 1
		    ENDIF
		    OP_AMT(NUM_JOKER,SEMANA_POS,BANKPOS) = OP_AMT(NUM_JOKER,SEMANA_POS,BANKPOS) + OPS_REC.TOTAL_JOKER
                 ENDIF

110		 READ (OPS_LUN, END=200, IOSTAT=ST) OPS_REC
		 IF (ST.NE.0) THEN  
      		    CALL DISPERR (MSG_LUN, 'Error reading ** OPS.FIL ** file. ', 0, ' ', 0, ' ', 0)
      		    CALL GSTOP (GEXIT_FATAL)
		 ENDIF

              ENDDO   ! LOOP FOR THE SEMANA 

	   ENDDO ! LOOP FOR THE GAME

	ENDDO ! LOOP FOR ALL GAMES

200	CONTINUE

	CLOSE (OPS_LUN)
	CLOSE (CASHOPS_LUN)

	IF (SENDSAP) THEN
 	   TYPE*,' '
	   TYPE*,'--------------------------------------------------'
	   TYPE*,' ALGUMAS ORDENS NAO FORAM ENVIADAS AO SAP POIS JA '
	   TYPE*,' TINHAM SIDO ENVIADAS ANTERIORMENTE               '
	   TYPE*,'--------------------------------------------------'
	   TYPE*,' '
	ENDIF
C
C	PRINT REPORT
C       ------------
C
C	NAME REPORT FILE
C
	WRITE(REP_FILE,    FMT='(A12,I4.4,I2.2,I2.2,A4)') 'FILE:WORSAP_', ANO, MES, DIA, '.REP'
C
C	OPEN REPORT FILE
C
	OPEN (UNIT   = REP_LUN,
     *	      FILE   = REP_FILE,
     *	      STATUS = 'NEW',
     *	      IOSTAT = ST)
	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error OPENING REPORT file : '//REP_FILE, 0, 'STATUS = ', ST, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
	ENDIF
	
	PAGE = 0
	GAMS_IN_PAGE = 0
	TOTAL_SAP = 0

	DO GAM = 1,MAXGAM

	   TEM_VALOR = .FALSE.
	   DO SEMP = 1,SEMANA_CNT
              DO BNKP = 1, MAXBANKS
		 IF (OP_AMT(GAM,SEMP,BNKP).GT.0) THEN
                    TEM_VALOR = .TRUE.
                    EXIT
                 ENDIF
              ENDDO
              IF (TEM_VALOR) THEN
                 EXIT
              ENDIF
	   ENDDO
	
	   IF ((TEM_VALOR .AND. GAMS_IN_PAGE.EQ.2) .OR. PAGE.EQ.0) THEN        
	      IF (PAGE.GT.0) THEN
	         WRITE(REP_LUN,FMT='(A1)') '1'    !PAGE JUMP 
      	      ENDIF
              PAGE = PAGE + 1
	      GAMS_IN_PAGE = 0
	      DATE(VCDC) = DAYCDC
              CALL LCDATE(DATE)
              WRITE (REP_LUN, 105) PAGE, DIA, MES, ANO, DATE(VDAY), DATE(VMON), DATE(VYEAR)+2000
105           FORMAT(1X, 130('='), /, 
     *        1X,'SCML - Departamento de Jogos',T41,'PRESTACAO DE CONTAS DO PAGAMENTO DE PREMIOS (SAP)',T123,'Pag.:',I4.4,/, 
     *        1X,'Data do Processamento dos Pagamentos : ', I2.2,'/',I2.2,'/',I4.4, T122, I2.2,'/',I2.2,'/',I4.4,/,   
     *        1X, 130('='))
	   ENDIF

	   IF (TEM_VALOR) THEN
	      GAMS_IN_PAGE = GAMS_IN_PAGE + 1
           ENDIF

	   PRINTED_GAME_TITLE = .FALSE.

	   GAM_AMT = 0
	   GAM_CNT = 0

	   DO SEMP = 1,SEMANA_CNT
	      AMT = 0
	      CNT = 0
	      DO BNKP = 1,MAXBANKS
		 AMT = AMT + OP_AMT(GAM,SEMP,BNKP)
		 CNT = CNT + OP_CNT(GAM,SEMP,BNKP)

		 GAM_AMT = GAM_AMT + OP_AMT(GAM,SEMP,BNKP)
	         GAM_CNT = GAM_CNT + OP_CNT(GAM,SEMP,BNKP)
              ENDDO

	      IF (AMT.GT.0) THEN
                 IF (.NOT. PRINTED_GAME_TITLE) THEN
		    WRITE(REP_LUN,77) (GLNAMES(K,GAM),K=1,4)
77		    FORMAT(//,32X,4A4,/,1X,80('-'),/,1X,'CONCURSO  ORDENS PAGAMENTO    IMPORTANCIA',/,
     *                     1X,80('-'))
                    PRINTED_GAME_TITLE = .TRUE.
		 ENDIF
	         WRITE(REP_LUN,33) SEMANA(SEMP)(1:3), SEMANA(SEMP)(4:7), CNT, CSMONY(AMT,11,VALUNIT)
33	         FORMAT(1X,A3,'/',A4, 11X, I5, 6X, A11)
	      ENDIF
	   ENDDO

           TOTAL_SAP = TOTAL_SAP + GAM_AMT

	   IF (PRINTED_GAME_TITLE) THEN
	      WRITE(REP_LUN,34) GAM_CNT, CSMONY(GAM_AMT,11,VALUNIT)
34	      FORMAT(1X,80('-'),/,20X, I5, 6X, A11)
	   ENDIF

	ENDDO

	WRITE(REP_LUN, 258) CMONY(TOTAL_SAP,11,VALUNIT)
258     FORMAT(//,1X, 'TOTAL ENVIADO AO SAP          ', A11)

	WRITE(REP_LUN, 259) CMONY(TOTAL_CLAIM,11,VALUNIT)
259     FORMAT(//,1X, 'TOTAL PAGO DE RECLAMACOES     ', A11)

	WRITE(REP_LUN, 989) CMONY(PAID_MANUALLY,11,VALUNIT)
989     FORMAT(//,1X, 'TOTAL DE PAGAMENTOS MANUAIS   ', A11,//,
     *            1X, 'OBS: O TOTAL DE PAGAMENTOS MANUAIS nao entra no relatorio de totais pagos por Banco.')

	CLOSE (REP_LUN)

	WRITE(6,733) CMONY(TOTAL_SAP,11,VALUNIT)
733	FORMAT(' >>> TOTAL ENVIADO AO SAP              = ', A11)

	WRITE(6,735) CMONY(TOTAL_CLAIM,11,VALUNIT)
735	FORMAT(' >>> TOTAL DE RECLAMAÇÕES              = ', A11)

	WRITE(6,736) CMONY(PAID_MANUALLY,11,VALUNIT)
736	FORMAT(' >>> TOTAL DE ACERTOS (PAGTOS) MANUAIS = ', A11)

	IF (TOTAL_SAP.NE.TOTAL_SAP2) THEN
	   CALL DISPERR (MSG_LUN, 'TOTAL LIDO DO FICHEIRO NAO CORRESPONDE', 0, 'AO TOTAL ENVIADO AO SAP', 0, ' ', 0)
	   WRITE(6,734) CMONY(TOTAL_SAP2,11,BETUNIT)
734	   FORMAT(' >>> TOTAL LIDO DO FICHEIRO OP = ', A11)
        ENDIF
C
C	WRITE WORSAP FILE (TO SAP)
C       --------------------------
C
	CALL OPEN_WORSAP (WORSAP_LUN, CDCPROC, ST)
	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, WORSAP_REC.ERRSTR, 0, 'STATUS = ', ST, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
	ENDIF
C
C	WRITE HEADER
C
	DATE(VCDC) = DAYCDC
	CALL LCDATE(DATE)
	WRITE (WORSAP_REC.DATA_GERACAO,FMT='(I4.4,I2.2,I2.2)') DATE(VYEAR)+2000, DATE(VMON), DATE(VDAY) 
	CALL WRITE_WORSAP('HD',ST)
	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, WORSAP_REC.ERRSTR, 0, 'STATUS = ', ST, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
	ENDIF
C
C	WRITE ALL DETAIL DATA
C
	DO GAM = 1,MAXGAM
	   DO SEMP = 1,SEMANA_CNT
	      DO BNKP = 1,MAXBANKS
		 IF (OP_AMT(GAM,SEMP,BNKP).GT.0) THEN
		    WORSAP_REC.CONCURSO		   = SEMANA(SEMP)
		    WORSAP_REC.JOGO		   = GAM
		    WORSAP_REC.BANCO		   = CTOI(BANK_TAB(BNKP).BANK,SZ)
		    WORSAP_REC.VALOR               = OP_AMT(GAM,SEMP,BNKP)
		    WORSAP_REC.OCORRENCIAS_PREMIOS = OP_CNT(GAM,SEMP,BNKP)
	            CALL WRITE_WORSAP('DT',ST)
	            IF (ST.NE.0) THEN
		       CALL DISPERR (MSG_LUN, WORSAP_REC.ERRSTR, 0, 'STATUS = ', ST, ' ', 0)
      		       CALL GSTOP (GEXIT_FATAL)
		    ENDIF
                 ENDIF
              ENDDO
	   ENDDO
	ENDDO
C
C	WRITE TRAILLER
C
	CALL WRITE_WORSAP('TL',ST)
	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, WORSAP_REC.ERRSTR, 0, 'STATUS = ', ST, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
	ENDIF

	CLOSE (WORSAP_LUN)

      	CALL GSTOP (GEXIT_SUCCESS)
	END


C	**************************************************************
	SUBROUTINE UPD_WEEK (YEARWEEK, SEMANA, SEMANA_CNT, SEMANA_POS)
C	**************************************************************
	IMPLICIT NONE

	CHARACTER*7 YEARWEEK
	CHARACTER*7 SEMANA(*)
	INTEGER*4   SEMANA_CNT
	INTEGER*4   SEMANA_POS
	INTEGER*4   I	

	CHARACTER*7 YW

	YW = YEARWEEK(5:7)//YEARWEEK(1:4)

	SEMANA_POS = 0

	DO I = 1,SEMANA_CNT
	   IF (SEMANA(I).EQ.YW) THEN
C
C	      THIS WEEK IS IN TABLE (RETURN ITS POSITION)
C
	      SEMANA_POS = I
              EXIT	
           ENDIF
	ENDDO

	IF (SEMANA_POS.EQ.0) THEN
C
C	   NEW WEEK TO BE PUT IN TABLE
C
	   SEMANA_CNT = SEMANA_CNT + 1
	   SEMANA_POS = SEMANA_CNT
	   SEMANA(SEMANA_POS) = YW
	ENDIF

	RETURN
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
	INTEGER*4 K, SZ

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

	WRITE(LUN,50) GSNAMES(CTOI(OPS_REC.GAME,SZ)),
     *                OPS_REC.YEARWEEK(5:7), OPS_REC.YEARWEEK(3:4),
     *                OPS_REC.ORDER,
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

50      FORMAT(1X,A4, 2X, A3,'/',A2, 5X, A6, 4X, A14, 2X, A2,'.',A5,
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
20      FORMAT(' GAME  SORTEIO    ORDER       BILHETE       AGENTE    TIPO OP  PRZ   1  2  3  4  5  6    '
     *         'VAL.GAME  JDV   VAL.JOKER  BANK  BALC  PCDC',/,1X,131('-'))
	RETURN
	END
