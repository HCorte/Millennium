C
C PAGAMENTO_MANUAL.FOR                                                                    
C
C
C V03 27-DEC-2010 FRP Lotto2 Changes
C V02 06-APR-2009 FRP Modify for EM Joker
C V01 25-JUN-2001 EPH INITIAL VERSION FOR SCML
C                                                                               
C ALLOWS MANUAL PAYMENT FOR THOSE OP'S WHO PRESENT ERRORS DURING 
C ODJ%%%%R PROCESSING                                                      
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
        PROGRAM PAGAMENTO_MANUAL
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
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
                                                                               
        INTEGER*4   MSG_LUN /6/

	INTEGER*4   ST, SZ, K

	CHARACTER   CHAVE*15
	CHARACTER   GAME_NAME*16
	CHARACTER   DATAPAY*10

	INTEGER*4   YESNO

	LOGICAL     SPLITTED
	LOGICAL     CANCELPAY

	INTEGER*4   TOTAL_GAME, TOTAL_JOKER, TOTAL_LOTO2

	INTEGER*4   ORDEM, JOGO, ANO, SEMANA

	INTEGER*2   DATE(12)
	INTEGER*4   DIAPAY, ANOPAY, MESPAY, CDCPAY	

        CALL COPYRITE                                                             
  	                                                                         
        TYPE*,IAM(),' '
        TYPE*,IAM(),'----------------------------------------------------------'   
        TYPE*,IAM(),'<<<<< PAGAMENTO MANUAL DE ORDENS ENVIADAS COM ERRO   >>>>>'   
	TYPE*,IAM(),'<<<<< PELA BANCA                                     >>>>>'
        TYPE*,IAM(),'----------------------------------------------------------'
        TYPE*,IAM(),' '
C
C     	OPEN ORDENS DE PAGAMENTO FILE
C     	=============================
     	CALL OPEN_OPS('KEYED',ST)
      	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error Opening Order file', 0, ' ', 0, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
      	ENDIF    

C	
C	LOOP FOR OPS SELECTION
C	======================
C
	DO 777 WHILE (.TRUE.)    
C
C	   ASK FOR SERIAL OR BILHETE TO SELECT
C
	   TYPE*,' '
	   TYPE*,'==============================================================================='
	   TYPE*,' '
	   CALL INPNUM('Digite a SEMANA do SORTEIO (CCC)   : ', SEMANA, 1, 106, ST) 
           IF (ST.NE.0) EXIT
	   CALL INPNUM('Digite o ANO (AAAA)                : ', ANO, 1900, 2100, ST) 
           IF (ST.NE.0) EXIT
	   CALL INPNUM('Digite o numero do JOGO (na ORDEM) : ', JOGO, 1, 10, ST) 
           IF (ST.NE.0) EXIT
	   CALL INPNUM('Digite o numero da ORDEM (OOOOOO)  : ', ORDEM, 1, 999999, ST) 
           IF (ST.NE.0) EXIT

	   IF (JOGO.GT.9) JOGO = JOGO + 1     !IN OP THEY SUBTRACT 1 FROM GAME NUMBER FOR GAME > 9
C
C	   PREPARE ACCESS KEY (GGAAAACCCOOOOOO)
C
	   WRITE (CHAVE, FMT='(I2.2, I4.4, I3.3, I6.6)') JOGO, ANO, SEMANA, ORDEM
C
C	   LOOK FOR IT ON OP FILE
C	
	   READ(OPS_LUN, KEYID=0, KEY=CHAVE, IOSTAT=ST) OPS_REC
      	   IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Ordem não encontrada.', 0, 'STATUS = ', ST, ' ', 0)
      	      GOTO 777
      	   ENDIF

	   WRITE(GAME_NAME,FMT='(4A4)') (GLNAMES(K,CTOI(OPS_REC.GAME,SZ)),K=1,4)
	
	   TOTAL_GAME  = OPS_REC.TOTAL_GAME
           TOTAL_JOKER = OPS_REC.TOTAL_JOKER

	   SPLITTED = OPS_REC.SPLITTED
	   TOTAL_LOTO2 = 0
	   IF (OPS_REC.SPLITTED) THEN
	      WRITE (CHAVE, FMT='(I2.2, I4.4, I3.3, I6.6)') 4, ANO, SEMANA, ORDEM
	      READ(OPS_LUN, KEYID=0, KEY=CHAVE, IOSTAT=ST) OPS_REC
      	      IF (ST.EQ.0) THEN     !OK FOUND
                 TOTAL_LOTO2 = OPS_REC.TOTAL_GAME
      	      ENDIF
	   ENDIF
C
C	   SHOW SOME OP DATA
C
           TYPE*, ' '
	   TYPE*, '   JOGO            : ', GAME_NAME
	   TYPE*, '   CONCURSO        : ', OPS_REC.YEARWEEK(5:7) // '/' // OPS_REC.YEARWEEK(1:4)
	   TYPE*, '   NUMERO DA ORDEM : ', OPS_REC.ORDER
           TYPE*, '   NUMERO BILHETE  : ', OPS_REC.BILHETE
	   TYPE*, '   AGENTE          : ', OPS_REC.AGENT
	   TYPE*, '   PREMIO JOGO     : ', CMONY (TOTAL_GAME, 11,VALUNIT)
	   TYPE*, '   PREMIO JOKER    : ', CMONY (TOTAL_JOKER,11,VALUNIT)
           IF (SPLITTED) THEN
	      TYPE*, '   PREMIO LOTO2    : ', CMONY (TOTAL_LOTO2, 11,VALUNIT)
           ENDIF
	   TYPE*, ' '


           CANCELPAY = .FALSE.

	   IF (OPS_REC.PAID_CDC.NE.0) THEN
              IF (OPS_REC.PAID_SENT_SAP) THEN
	         CALL DISPERR (MSG_LUN, 'Ordem já foi PAGA e ENVIADA para o SAP', 0, 'STATUS = ', ST, ' ', 0)
      	         GOTO 777
              ENDIF
              IF (OPS_REC.PAID_MANUALLY) THEN
                 CALL PRMYESNO('ORDEM já foi paga manualmente. CANCELA o pagamento (Y/N) ? ', YESNO)
	         IF (YESNO.EQ.1) THEN
                    CANCELPAY = .TRUE.
                 ELSE
                    GOTO 777
                 ENDIF
              ELSE
	         CALL DISPERR (MSG_LUN, 'Ordem já foi PAGA através do PROCESSAMENTO', 0, 'dos ficheiros da BANCA', 0, ' ', 0)
      	         GOTO 777
              ENDIF
           ENDIF              

           IF (CANCELPAY) THEN

	      OPS_REC.PAID_CDC = 0
	      OPS_REC.PROC_PAID_CDC = 0 	
	      OPS_REC.PAID_MANUALLY = .FALSE. 	

           ELSE
C
C             GET PAYMENT DATE
C
500           TYPE*,'Qual a DATA DE PAGAMENTO da OP (dd/mm/aaaa) ? '
              READ(5,13) DATAPAY
13            FORMAT(A10)
	      IF (DATAPAY(1:1).EQ.'E') EXIT
              ANOPAY = CTOI(DATAPAY(7:10),SZ)
              IF (ANOPAY.LE.1900 .OR. ANOPAY.GT.2100) GOTO 500
              MESPAY = CTOI(DATAPAY(4:5),SZ)
              IF (MESPAY.LE.0 .OR. MESPAY.GT.12) GOTO 500
              DIAPAY = CTOI(DATAPAY(1:2),SZ)
              IF (DIAPAY.LE.0 .OR. DIAPAY.GT.31) GOTO 500

              DATE(VMON)  = MESPAY
              DATE(VYEAR) = ANOPAY
              DATE(VDAY)  = DIAPAY
              CALL BDATE(DATE)
              CDCPAY = DATE(VCDC)

	      CALL PRMYESNO ('Confirma o pagamento desta OP (Y/N) ?', YESNO)
	      IF (YESNO.NE.1) GOTO 777

	      OPS_REC.PAID_CDC = CDCPAY
	      OPS_REC.PROC_PAID_CDC = DAYCDC 
	      OPS_REC.PAID_MANUALLY = .TRUE.

           ENDIF           

	   REWRITE (OPS_LUN, IOSTAT=ST) OPS_REC
           IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Error Rewriting to OPS file', 0, 'Status = ', ST, ' ', 0)
              CLOSE(OPS_LUN)
      	      CALL GSTOP (GEXIT_FATAL)
           ENDIF

	   IF (SPLITTED) THEN
	      WRITE (CHAVE, FMT='(I2.2, I4.4, I3.3, I6.6)') 2, ANO, SEMANA, ORDEM
	      READ(OPS_LUN, KEYID=0, KEY=CHAVE, IOSTAT=ST) OPS_REC
      	      IF (ST.NE.0) THEN   
	         CALL DISPERR (MSG_LUN, 'Error locating first part of LOTO ORDER', 0, 'Status = ', ST, ' ', 0)
                 CLOSE(OPS_LUN)
      	         CALL GSTOP (GEXIT_FATAL)
      	      ENDIF

              IF (CANCELPAY) THEN
	         OPS_REC.PAID_CDC = 0
	         OPS_REC.PROC_PAID_CDC = 0 	
	         OPS_REC.PAID_MANUALLY = .FALSE. 	
              ELSE
	         OPS_REC.PAID_CDC = CDCPAY 	
	         OPS_REC.PROC_PAID_CDC = DAYCDC 	
	         OPS_REC.PAID_MANUALLY = .TRUE. 	
              ENDIF

	      REWRITE (OPS_LUN, IOSTAT=ST) OPS_REC
              IF (ST.NE.0) THEN
	         CALL DISPERR (MSG_LUN, 'Error Rewriting to OPS file', 0, 'Status = ', ST, ' ', 0)
                 CLOSE(OPS_LUN)
      	         CALL GSTOP (GEXIT_FATAL)
              ENDIF
	   ENDIF

	   TYPE*, '  '
	   TYPE*, '----------------------------------'
	   TYPE*, '>>> OP FOI PAGA COM SUCESSO    <<<'
	   TYPE*, '----------------------------------'
	   TYPE*, '  '

777	CONTINUE

	CLOSE(OPS_LUN)

      	CALL GSTOP (GEXIT_SUCCESS)
	END
