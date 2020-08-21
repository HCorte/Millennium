C
C PURGE_OPS.FOR                                                                    
C
C
C V07 20-JUN-2012 FRP If report only (no purgue), generate correct report name.
C	V06 21-MAR-2012 ACC Fix Joker/Main game count, amount totals
C V05 16-MAR-2011 FRP Request week draw for each game
C V04 27-DEC-2010 FRP Lotto2 Changes
C V05 19-DIC-2003 FRP Modify for Batch2 Totobola Changes.
C V02 27-JUN-2001 EPH CLAIMS APPEAR IN PURGE REPORT TOO
C V01 25-APR-2001 EPH INITIAL VERSION FOR SCML
C                                                                               
C PURGE A DRAW (SEMANA) FROM OP FILE
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
        PROGRAM PURGE_OPS
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
                                                                               
        INTEGER*4   MSG_LUN /6/
	INTEGER*4   REP_LUN /TMP1_LUN/

        INTEGER*4   ANOSEMANA, ANO, SEMANA
	CHARACTER   YEARWEEK*7
	INTEGER*4   GM, BP
	INTEGER*4   ST, SZ, K
        INTEGER*4   BANK, BANKPOS
	INTEGER*4   OP_CASH /1/, OP_UNCASH /2/
	INTEGER*4   JOGO
	INTEGER*4   PAYST
	INTEGER*4   NUM_JOKER/5/, NUM_LOTO/2/, NUM_LOTO2/4/
	INTEGER*4   NUM_LOTO3/6/, NUM_LOTO4/7/
	LOGICAL	    GAME_TITLE
	CHARACTER   REP_FILE*24
	INTEGER*4   PAGE
	CHARACTER   UNOPS_FILE*23
	INTEGER*4   YESNO
	LOGICAL     SORELAT
	CHARACTER   TITULO*31

	INTEGER*4  OPS_CNT(MAXGAM,MAXBANKS,2), OPS_AMT(MAXGAM,MAXBANKS,2)
	INTEGER*4  TOT_CNT(2), TOT_AMT(2)

	INTEGER*2 DATE(LDATE_LEN)
        INTEGER*4 AA,MM,DD, GAM
        INTEGER*4 GAME_SEL(MAXGAM)
        INTEGER*4 AAAA(MAXGAM),CCC(MAXGAM)
        CHARACTER AAAACCC(MAXGAM)*7

        CALL COPYRITE                                                             
  	                                                                         
        TYPE*,IAM(),' '
        TYPE*,IAM(),'-----------------------------------------------------------'   
        TYPE*,IAM(),'<<<<< PRESCRICAO DE PREMIOS (APENAS OPS)              >>>>>'   
        TYPE*,IAM(),'      GERA UNOPS_aaaammdd.LIS (OPS PRESCR. E NAO PAGAS)'
        TYPE*,IAM(),'      GERA RELPRG_aaaammdd.REP'
	TYPE*,IAM(),'        aaaammdd  =  data geracao'
        TYPE*,IAM(),'-----------------------------------------------------------'   
        TYPE*,IAM(),'<<<<< CERTIFIQUE-SE DE QUE HA UMA COPIA DE SEGURANCA  >>>>>'
        TYPE*,IAM(),'<<<<< DO FICHEIRO DE OPS                              >>>>>'
        TYPE*,IAM(),'-----------------------------------------------------------'
        TYPE*,IAM(),' '
	TYPE*,' '
	TYPE*,' '
C
C Input Games/Sorteios
C
        CALL INPUT_SORTEIO_PER_GAME(GAME_SEL,AAAACCC,AAAA,CCC)
        TYPE*,' '
        TYPE*,' '

	
	CALL PRMYESNO('Deseja apenas um relatório até o momento (Y/N) ? ', YESNO)
	IF (YESNO.EQ.1) THEN
           SORELAT = .TRUE.
           TITULO = 'RESUMO TEMPORÁRIO DE PRESCRICAO'           
           TYPE*,' '
	   TYPE*,'>>> GERANDO APENAS O RELATÓRIO ATÉ O MOMENTO  <<<'
           TYPE*,' '
        ELSE
           SORELAT = .FALSE. 
           TITULO = '   RESUMO FINAL DE PRESCRICAO  '           
           TYPE*,' '
	   TYPE*,'************************************************************************'
	   TYPE*,'>>> ATENÇÃO : SERÁ REALIZADA A PRESCRIÇÃO DAS ORDENS DA SEMANA E SUA <<<'
	   TYPE*,'>>>           CONSEQUENTE ELIMINAÇÃO DO FICHEIRO DE OPS (OPS.FIL)    <<<'
	   TYPE*,'************************************************************************'
           TYPE*,' '
           CALL PRMYESNO('Confirma a PRESCRIÇÃO (Y/N) ? ', YESNO)
           IF (YESNO.NE.1) STOP
        ENDIF

	TYPE*,' '
	TYPE*,'Aguarde...Percorrendo ficheiro de OPS'
	TYPE*,' '
C
C	LOAD BANK TABLE STRUCTURE
C	-------------------------
	CALL LOAD_BANK_TABLE(ST)
        IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error loading BANK TABLE', 0, 'STATUS = ', ST, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
        ENDIF

C
C       OPEN ORDENS DE PAGAMENTO FILE
C       -----------------------------
        CALL OPEN_OPS('SEQUENTIAL',ST)
        IF (ST.NE.0) THEN
           CALL DISPERR (MSG_LUN, 'Error Opening Order file', 0, ' ', 0, ' ', 0)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
C
C       GET TODAY'S DATE
C       ----------------
        DATE(VCDC) = DAYCDC
        CALL LCDATE(DATE)
        DD = DATE(VDAY)
        MM = DATE(VMON)
        AA = DATE(VYEAR) + 2000
	
	IF (.NOT.SORELAT) THEN
C
C       OPEN UNCASHED OP'S LIST FILE
C       -----------------------------
	WRITE (UNOPS_FILE,FMT='(A11,I4.4,I2.2,I2.2,A4)') 'FILE:UNOPS_', AA, MM, DD, '.LIS'
        OPEN (UNIT   = UNOPS_LUN,
     *        FILE   = UNOPS_FILE,
     *	      STATUS = 'NEW',
     *	      IOSTAT = ST)
        IF (ST.NE.0) THEN
           CALL DISPERR (MSG_LUN, 'Error Opening UNOPS file', 0, ' ', 0, ' ', 0)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
	CALL WRITE_OP_TITLE(UNOPS_LUN)

	ENDIF

C
C	LOOP READING OPS FILE
C	=====================

	CALL FASTSET(0,OPS_AMT,MAXGAM*MAXBANKS*2)
	CALL FASTSET(0,OPS_CNT,MAXGAM*MAXBANKS*2)

	DO 100 WHILE (.TRUE.)

	   READ (OPS_LUN, END=300, IOSTAT=ST) OPS_REC
      	   IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Error Reading OPS FILE', 0, 'STATUS = ', ST, ' ', 0)
      	      CALL GSTOP (GEXIT_FATAL)
      	   ENDIF    

           GAM = CTOI(OPS_REC.GAME,SZ)
           IF(GAME_SEL(GAM) .EQ. 0) GOTO 100  !IT'S NOT THE GAME I WANT TO LIST

           ANO = AAAA(GAM)
           SEMANA = CCC(GAM)
           ANOSEMANA = CTOI(AAAACCC(GAM),SZ)

           WRITE(YEARWEEK,FMT='(I7.7)') ANOSEMANA
	   IF (YEARWEEK .NE. OPS_REC.YEARWEEK) GOTO 100   !IT'S NOT THE WEEK I WANT TO PURGE

	   IF (DAYCDC .LE. OPS_REC.PAYABLE_CDC) GOTO 100  !Do not purge if
	             !program is run during maximum payable date or before

	   IF (.NOT.SORELAT) THEN
C
C	   DELETE IT FROM ORIGINAL FILE
C	   ----------------------------
	   DELETE(OPS_LUN,IOSTAT=ST)
      	   IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Error DELETING OPS FILE RECORD', 0, 'STATUS = ', ST, ' ', 0)
      	   ENDIF    

	   IF (OPS_REC.PAID_CDC.EQ.0) THEN
C	      WRITE IT TO UNOPS FILE
C	      ----------------------
	      CALL WRITE_OP_LINE(UNOPS_LUN)	      
	   ENDIF

	   ENDIF


C !V02	   IF (OPS_REC.CLAIM) GOTO 100     ! ** DON'T WANT CLAIMS IN PURGE REPORT **

	   BANK = CTOI(OPS_REC.BANK,SZ)
	   IF (BANK.EQ.0) THEN
	      CALL DISPERR (MSG_LUN, 'Found value 0 for BANK in Order = '//OPS_REC.ORDER, 0, ' ', 0, ' ', 0)
	   ENDIF

	   JOGO = CTOI(OPS_REC.GAME,SZ)
	   IF (JOGO.EQ.0) THEN
	      CALL DISPERR (MSG_LUN, 'Found value 0 for GAME in Order = '//OPS_REC.ORDER, 0, ' ', 0, ' ', 0)
	   ENDIF

           IF (OPS_REC.PAID_CDC.EQ.0) THEN	   
              PAYST = OP_UNCASH
           ELSE
              PAYST = OP_CASH
           ENDIF

	   CALL GET_BANK_POS (BANK,BANKPOS)
	   IF (BANKPOS.EQ.0) THEN
	      CALL DISPERR (MSG_LUN, 'Bank not found in table.', 0, 'Bank= ', BANK, 'Sending to first bank in list', 0)
	      BANKPOS = 1       !PUT IT IN THE FIRST BANK
	   ENDIF

C
C	   SET COUNT AND AMOUNT FOR GAMES AND JOKER
C
C	   if main and joker then cnt=1 with amt = main amt + joker amt
C	   if main only then      cnt=1 with amt = main amt
C	   if joker only then     cnt=1 with amt = joker amt
C          if main IS joker then  cnt=1 with amt = main amt (assumption: jok amt = 0 in this case)
C
	   IF (OPS_REC.TOTAL_GAME.GT.0) THEN
	      OPS_CNT(JOGO,BANKPOS,PAYST) = OPS_CNT(JOGO,BANKPOS,PAYST) + 1
	      OPS_AMT(JOGO,BANKPOS,PAYST) = OPS_AMT(JOGO,BANKPOS,PAYST) + OPS_REC.TOTAL_GAME
	      IF (OPS_REC.TOTAL_JOKER.GT.0) THEN
	         OPS_AMT(JOGO,BANKPOS,PAYST) = OPS_AMT(JOGO,BANKPOS,PAYST) + OPS_REC.TOTAL_JOKER
	      ENDIF
	   ELSEIF (OPS_REC.TOTAL_JOKER.GT.0) THEN
	      OPS_CNT(NUM_JOKER,BANKPOS,PAYST) = OPS_CNT(NUM_JOKER,BANKPOS,PAYST) + 1
	      OPS_AMT(NUM_JOKER,BANKPOS,PAYST) = OPS_AMT(NUM_JOKER,BANKPOS,PAYST) + OPS_REC.TOTAL_JOKER
	   ENDIF

100     CONTINUE
C
C	END OF OP READING
C
300     CLOSE(OPS_LUN)

	IF (.NOT.SORELAT) THEN
 	   CLOSE(UNOPS_LUN)  !CLOSE LIST FILE
        ENDIF
C
C	PRINT PURGE REPORT
C	==================
C
C	OPEN PURGE REPORT
C
	WRITE (REP_FILE,FMT='(A12,I4.4,I2.2,I2.2,A4)') 'FILE:RELPRG_', AA, MM, DD ,'.REP'
	OPEN (UNIT   = REP_LUN,
     *        FILE   = REP_FILE,
     *	      STATUS = 'NEW',
     *        IOSTAT = ST)
      	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error OPENING REPORT FILE', 0, 'STATUS = ', ST, ' ', 0)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF    

	PAGE = 0
	DO GM=1,MAXGAM
	   GAME_TITLE = .FALSE.
           CALL FASTSET (0,TOT_AMT,2)
           CALL FASTSET (0,TOT_CNT,2)
	   DO BP=1,MAXBANKS
	      IF (OPS_AMT(GM,BP,OP_CASH)+OPS_AMT(GM,BP,OP_UNCASH).GT.0) THEN

                 ANO = AAAA(GM)
                 SEMANA = CCC(GM)

	         IF (.NOT.GAME_TITLE) THEN
		    PAGE = PAGE + 1

      		    WRITE (REP_LUN, 102) TITULO, PAGE, (GLNAMES(K,GM),K=1,4), SEMANA, ANO

102                 FORMAT(1X, 130('='), /, 
     *               1X,'SCML - Departamento de Jogos',T48, A31, T123,'Pag.:',I4.4,/, 
     *               1X, 130('='), /,
     *               1X, 'Jogo : ', 4A4, T86, 'Concurso : ', I3.3, '/', I4.4, /,
     *               1X, 130('-'),/,
     *               1X, '          B A N C O                 EMITIDAS  VALOR EMITIDO        PAGAS     VALOR PAGO',
     *                   '     NAO PAGO   VAL.NAO PAGO',/
     *               1X, 130('-')/)

                    GAME_TITLE = .TRUE.
	         ENDIF
	         WRITE (REP_LUN,25) BANK_TAB(BP).LONG_NAME,
     *                              OPS_CNT(GM,BP,OP_CASH)+OPS_CNT(GM,BP,OP_UNCASH), 
     *			            CMONY(OPS_AMT(GM,BP,OP_CASH)+OPS_AMT(GM,BP,OP_UNCASH),11,VALUNIT),
     *			            OPS_CNT(GM,BP,OP_CASH),
     *                              CMONY(OPS_AMT(GM,BP,OP_CASH),11,VALUNIT),
     *			            OPS_CNT(GM,BP,OP_UNCASH),
     *                              CMONY(OPS_AMT(GM,BP,OP_UNCASH),11,VALUNIT)
25		 FORMAT(1X,A33,6X,3(I5,4X,A11,8X))

	         TOT_CNT(OP_CASH) = TOT_CNT(OP_CASH) + OPS_CNT(GM,BP,OP_CASH)
	         TOT_AMT(OP_CASH) = TOT_AMT(OP_CASH) + OPS_AMT(GM,BP,OP_CASH)

	         TOT_CNT(OP_UNCASH) = TOT_CNT(OP_UNCASH) + OPS_CNT(GM,BP,OP_UNCASH)
	         TOT_AMT(OP_UNCASH) = TOT_AMT(OP_UNCASH) + OPS_AMT(GM,BP,OP_UNCASH)

              ENDIF

              IF (BP.EQ.MAXBANKS .AND. GAME_TITLE) THEN
C
C	         WRITE TOTALS AND JUMP PAGE
C
	         WRITE(REP_LUN,765) TOT_CNT(OP_CASH)+TOT_CNT(OP_UNCASH), CMONY(TOT_AMT(OP_CASH)+TOT_AMT(OP_UNCASH),11,VALUNIT),
     *	                            TOT_CNT(OP_CASH)                   , CMONY(TOT_AMT(OP_CASH),11,VALUNIT),
     *                              TOT_CNT(OP_UNCASH)                 , CMONY(TOT_AMT(OP_UNCASH),11,VALUNIT)
765	         FORMAT(/,  1X,130('-'),//,  40X,3(I5,4X,A11,8X),   /,'1')
              ENDIF

	   ENDDO
	ENDDO	         

	CLOSE(REP_LUN)

      	CALL GSTOP (GEXIT_SUCCESS)
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
