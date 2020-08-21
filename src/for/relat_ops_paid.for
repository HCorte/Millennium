C
C RELAT_OPS_PAID.FOR                                                                       
C
C V01 29-JUN-2001 EPH INITIAL VERSION FOR SCML
C                                                                               
C RELAT FOR OPS PAID (MANUAL AND BANK) 
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
                                                                               
	INTEGER*4   RELPAID_LUN /TMP2_LUN/
        INTEGER*4   MSG_LUN /6/

	INTEGER*4   ST, SZ
	INTEGER*2   DATE(12)
	INTEGER*4   BANK, BANKPOS
	INTEGER*4   TOT_BANK_OP_AMT, TOT_BANK_OP_CNT
	INTEGER*4   BANK_OP_AMT(MAXBANKS), BANK_OP_CNT(MAXBANKS)
	INTEGER*4   TOT_BANK_OP_AMT_M, TOT_BANK_OP_CNT_M
	INTEGER*4   BANK_OP_AMT_M(MAXBANKS), BANK_OP_CNT_M(MAXBANKS)
	INTEGER*4   DIA, MES, ANO

	CHARACTER   RELPAID_FILE*25, DATAPROC*100
	INTEGER*4   PROC_CDC

        CALL COPYRITE                                                             
  	                                                                         
        TYPE*,IAM(),' '
        TYPE*,IAM(),'-------------------------------------------------'
        TYPE*,IAM(),'<<<<<         RELATORIO DE OPS PAGAS        >>>>>'
        TYPE*,IAM(),'-------------------------------------------------'
        TYPE*,IAM(),'  GERA RELPAID_aaaammdd.REP                      '
        TYPE*,IAM(),'-------------------------------------------------'
	TYPE*,' '


C
C       CHECK ABOUT WICH DATE IS WANTED
C
500     TYPE*,'QUAL O DIA DE PROCESSAMENTO DAS OPS PAGAS (dd/mm/aaaa) ? '
        READ(5,13) DATAPROC
13      FORMAT(A10)
        DIA = CTOI(DATAPROC(1:2),SZ)
        MES = CTOI(DATAPROC(4:5),SZ)
        ANO = CTOI(DATAPROC(7:10),SZ)
        IF (DIA.LE.0 .OR. DIA.GT.31) GOTO 500
        IF (MES.LE.0 .OR. MES.GT.12) GOTO 500
        IF (ANO.LE.1900 .OR. ANO.GT.2100) GOTO 500
        TYPE*,' '
C
C       CONVERT TO CDC
C       
        DATE(VDAY)  = DIA
        DATE(VMON)  = MES
        DATE(VYEAR) = ANO
        CALL BDATE(DATE)
        PROC_CDC = DATE(VCDC)

	WRITE(RELPAID_FILE, FMT='(A13,I4.4,I2.2,I2.2,A4)') 'FILE:RELPAID_', ANO, MES, DIA, '.REP'
	
C
C	LOAD BANK TABLE STRUCTURE
C	-------------------------
	CALL LOAD_BANK_TABLE(ST)
        IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error loading BANK TABLE', 0, 'STATUS = ', ST, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
        ENDIF

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
     *         1X, '               BANCO                 ORDENS     PROCESSADO         ORDENS         MANUAL         ORDENS'
     *             '          TOTAL',/,
     *         1X, 130('-'),/)

C
C     	OPEN ORDENS DE PAGAMENTO FILE
C       -----------------------------
        CALL OPEN_OPS('SEQUENTIAL',ST)
        IF (ST.NE.0) THEN
           CALL DISPERR (MSG_LUN, 'Error opening ** OPS.FIL ** file. ', 0, ' ', 0, ' ', 0)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF             

	DO WHILE (.TRUE.)

	   READ (OPS_LUN, END=9000, IOSTAT=ST) OPS_REC
           IF (ST.NE.0) THEN
              CALL DISPERR (MSG_LUN, 'Error reading ** OPS.FIL ** file. ', 0, ' ', 0, ' ', 0)
              EXIT
	   ENDIF

           IF (PROC_CDC.EQ.OPS_REC.PROC_PAID_CDC) THEN
C
C	      STORE VALUES FOR REPORT
C
	      BANK = CTOI(OPS_REC.BANK,SZ)
	      IF (BANK.EQ.0) THEN
                 CALL DISPERR (MSG_LUN, 'Bank = 0 in OP = '//OPS_REC.ORDER, 0, ' ', 0, ' ', 0)
              ENDIF
	      CALL GET_BANK_POS(BANK,BANKPOS)
              IF (BANKPOS.EQ.0) THEN
                 CALL DISPERR (MSG_LUN, 'Invalid value for BANKPOS from GET_BANK_POS in OP = '
     *                         //OPS_REC.ORDER, 0, ' ', 0, ' ', 0)
                 BANKPOS = 1           ! SEND TO BANK 1
              ENDIF

              IF (OPS_REC.PAID_MANUALLY) THEN
	         BANK_OP_AMT_M(BANKPOS) = BANK_OP_AMT_M(BANKPOS) + OPS_REC.TOTAL_GAME + OPS_REC.TOTAL_JOKER
	         TOT_BANK_OP_AMT_M = TOT_BANK_OP_AMT_M + OPS_REC.TOTAL_GAME + OPS_REC.TOTAL_JOKER
              ELSE
	         BANK_OP_AMT(BANKPOS) = BANK_OP_AMT(BANKPOS) + OPS_REC.TOTAL_GAME + OPS_REC.TOTAL_JOKER
	         TOT_BANK_OP_AMT = TOT_BANK_OP_AMT + OPS_REC.TOTAL_GAME + OPS_REC.TOTAL_JOKER
	      ENDIF

              IF (.NOT. (OPS_REC.SPLITTED .AND. OPS_REC.GAME.EQ.'04') ) THEN
                 IF (OPS_REC.PAID_MANUALLY) THEN
	            BANK_OP_CNT_M(BANKPOS) = BANK_OP_CNT_M(BANKPOS) + 1
	            TOT_BANK_OP_CNT_M = TOT_BANK_OP_CNT_M + 1
                 ELSE
	            BANK_OP_CNT(BANKPOS) = BANK_OP_CNT(BANKPOS) + 1
	            TOT_BANK_OP_CNT = TOT_BANK_OP_CNT + 1
                 ENDIF
              ENDIF

	   ENDIF

        ENDDO

9000    CONTINUE

	DO BANKPOS = 1, MAXBANKS
           IF (BANK_OP_CNT(BANKPOS).GT.0) THEN
	      WRITE(RELPAID_LUN,35) BANK_TAB(BANKPOS).LONG_NAME, 
     *                              BANK_OP_CNT(BANKPOS),   CSMONY(BANK_OP_AMT(BANKPOS),11,BETUNIT),
     *                              BANK_OP_CNT_M(BANKPOS), CSMONY(BANK_OP_AMT_M(BANKPOS),11,BETUNIT),
     *                              BANK_OP_CNT(BANKPOS)+BANK_OP_CNT_M(BANKPOS), 
     *                              CSMONY(BANK_OP_AMT(BANKPOS)+BANK_OP_AMT_M(BANKPOS),11,BETUNIT)

35	      FORMAT(1X,A33, T39,I5,T48,A11, T69,I5,T78,A11, T99,I5,T108,A11)
	   ENDIF
	ENDDO   

	WRITE(RELPAID_LUN,36) TOT_BANK_OP_CNT,   CSMONY(TOT_BANK_OP_AMT,11,BETUNIT),
     *                        TOT_BANK_OP_CNT_M, CSMONY(TOT_BANK_OP_AMT_M,11,BETUNIT),
     *                        TOT_BANK_OP_CNT+TOT_BANK_OP_CNT_M, CSMONY(TOT_BANK_OP_AMT+TOT_BANK_OP_AMT_M,11,BETUNIT)
36	FORMAT(/,1X,130('-'),/,/
     *           1X,'TOTAL GERAL                          ', T39,I5,T48,A11, T69,I5,T78,A11, T99,I5,T108,A11)

	CLOSE (OPS_LUN)
	CLOSE (RELPAID_LUN)

      	CALL GSTOP (GEXIT_SUCCESS)
	END

