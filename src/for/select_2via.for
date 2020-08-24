C
C SELECT_2VIA.FOR                                                                    
C
C
C V03 24-DEC-2010 FRP Lotto2 Changes
C V02 17-DEC-2003 FRP Modify for Batch2 Totobola Changes.
C V01 21-MAR-2001 EPH INITIAL VERSION FOR SCML
C                                                                               
C SELECTS OPS ON OPS.FIL TO PRINT BASED ON "BILHETE"/SERIAL NUMBER
C OF TICKET                                                                 
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
        PROGRAM SELECT_2VIA
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
                                                                               
        INTEGER*4   OPSGEN_LUN
        INTEGER*4   MSG_LUN /6/

	INTEGER*4   ST, SZ, K

	LOGICAL     OPENED_OPSGEN
	CHARACTER   BILSER*14
	CHARACTER   GAME_NAME*16

	INTEGER*4   YESNO
	
        CALL COPYRITE                                                             
  	                                                                         
        TYPE*,IAM(),' '
        TYPE*,IAM(),'----------------------------------------------------------'   
        TYPE*,IAM(),'<<<<< SELECAO DE SEGUNDA VIA DE ORDEM PARA IMPRESSAO >>>>>'   
	TYPE*,IAM(),'<<<<< GERA OPSGEN_99_9999999.FIL                     >>>>>'
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
	OPENED_OPSGEN = .FALSE.   !OPEN ONLY IF THERE IS SOMETHING TO WRITE TO IT

	DO 777 WHILE (.TRUE.)    
C
C	   ASK FOR SERIAL OR BILHETE TO SELECT
C
	   TYPE*,' '
	   TYPE*,' '
	   TYPE*,'***********************************************************************'
	   TYPE*,' '
	   TYPE*,'DIGITE O NUMERO DO BILHETE OFFLINE ( 7 DIGITOS - ZEROS A ESQUERDA) OU O'
	   TYPE*,'         NUMERO SERIAL DO ONLINE   (14 DIGITOS - ZEROS A ESQUERDA) OU'
	   TYPE*,'         "EXIT" PARA ENCERRAR'
           READ (5,FMT='(A14)') BILSER
	   TYPE*,' '
	   IF (BILSER(1:4).EQ.'EXIT') EXIT
	   IF (BILSER(8:8).LT.'0' .OR. BILSER(8:8).GT.'9') THEN
	      BILSER = BILSER(1:7) // '       '
	   ENDIF
C
C	   LOOK FOR IT ON OP FILE
C	
	   READ(OPS_LUN, KEYID=1, KEY=BILSER, IOSTAT=ST) OPS_REC
      	   IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'SERIAL/BILHETE '//BILSER//' NAO ENCONTRADO', 0, 'STATUS = ', ST, ' ', 0)
      	      GOTO 777
      	   ENDIF
C
C	   SHOW SOME OP DATA
C
	   WRITE(GAME_NAME,FMT='(4A4)') (GLNAMES(K,CTOI(OPS_REC.GAME,SZ)),K=1,4)
	   TYPE*, '   JOGO            : ', GAME_NAME
	   TYPE*, '   CONCURSO        : ', OPS_REC.YEARWEEK(5:7) // '/' // OPS_REC.YEARWEEK(3:4)
	   TYPE*, '   NUMERO DA ORDEM : ', OPS_REC.ORDER
	   TYPE*, '   AGENTE          : ', OPS_REC.AGENT
	   TYPE*, '   PREMIO JOGO     : ', CMONY (OPS_REC.TOTAL_GAME, 11,VALUNIT)
	   TYPE*, '   PREMIO JOKER    : ', CMONY (OPS_REC.TOTAL_JOKER,11,VALUNIT)
	   TYPE*, ' '
C
C	   SOME KIND OF ORDERS MAY NOT BE SELECTED FOR 2 VIA
C
	   IF (OPS_REC.PAID_CDC.NE.0) THEN
	      TYPE*, ' '
	      TYPE*, '--------------------------------------------------------'
	      TYPE*, '>>> ATENCÃO : ESTA ORDEM JÁ FOI  P A G A  PELO BANCO    '
	      TYPE*, '              * NÃO É POSSIVEL GERAR A SEGUNDA VIA *    '
	      TYPE*, '--------------------------------------------------------'
	      TYPE*, ' '
              GOTO 777
           ENDIF


	   IF (OPS_REC.CLAIM .OR. OPS_REC.PRINTED_BY_OFF) THEN
	      TYPE*, ' '
	      TYPE*, '--------------------------------------------------------'
	      TYPE*, '>>> NAO E POSSIVEL GERAR 2 VIA DESTA ORDEM POIS ESTA FOI'
	      TYPE*, '    IMPRESSA PELO SISTEMA OFFLINE OU E UMA RECLAMACAO   '
	      TYPE*, '    ANTERIOR A ENTRADA DO SISTEMA ONLINE                '
	      TYPE*, '--------------------------------------------------------'
	      TYPE*, ' '
	      GOTO 777
           ENDIF

	   CALL PRMYESNO ('Confirma a selecao desta OP (Y/N) ?', YESNO)
	   IF (YESNO.NE.1) GOTO 777

	   IF (.NOT. OPENED_OPSGEN) THEN
C
C	     OPEN OPSGEN FILE
C
             CALL OPEN_OPSGEN (OPSGEN_LUN, 99, 'WRITE', 9999, 999, CTOI(OPS_REC.GAME,SZ), ST)
      	     IF (ST.NE.0) THEN
	        CALL DISPERR (MSG_LUN, 'Error Opening file for Selected Orders (2VIA)', 0, ' ', 0, ' ', 0)
      	        CALL GSTOP (GEXIT_FATAL)
      	     ENDIF    
	     OPENED_OPSGEN = .TRUE.
	   ENDIF
	
	   WRITE (OPSGEN_LUN, IOSTAT=ST) OPS_REC
           IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Error writing to OPSGEN file', 0, 'Status = ', ST, ' ', 0)
      	      CALL GSTOP (GEXIT_FATAL)
           ENDIF

	   TYPE*, '  '
	   TYPE*, '----------------------------------'
	   TYPE*, '>>> OP FOI GRAVADA COM SUCESSO.<<<'
	   TYPE*, '----------------------------------'
	   TYPE*, '  '

777	CONTINUE

	IF (OPENED_OPSGEN) THEN
	   CLOSE(OPSGEN_LUN)
	ENDIF

      	CALL GSTOP (GEXIT_SUCCESS)
	END
