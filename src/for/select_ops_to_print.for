C
C SELECT_OPS_TO_PRINT.FOR                                                                    
C
C
C V05 14-MAR-2011 FRP Request week draw for each game
C V04 22-DEC-2010 FRP Lotto2 Changes
C V03 20-MAR-2009 MMO BATCH JOKER/EM.
C V02 17-DEC-2003 FRP Modify for Batch2 Totobola Changes.
C V01 21-MAR-2001 EPH INITIAL VERSION FOR SCML
C                                                                               
C SELECTS OPS ON OPS.FIL FROM A CHOOSEN DRAW AND WRITE IT TO OPSGEN00_aaaaccc.FIL 
C THE PROGRAM TO PRINT OPS AND THE ONE TO SEND OP INFO TO THE BANK USE THIS FILE
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
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
                                                                               
        INTEGER*4   OPSGEN_LUN(MAXGAM)
        INTEGER*4   MSG_LUN /6/

        INTEGER*4   ANOSEMANA, ANO, SEMANA
	CHARACTER*9 CHAVE
	INTEGER*4   GAM, ST, SZ, OP_COUNT
        LOGICAL HAVE_PAID, HAVE_SELECTED
	INTEGER*4   TERM, GTYP, GIND
	INTEGER*4   AGBANK, AGBRANCH
        INTEGER*4   BOTGAM, TOPGAM

        INTEGER*4 GAME_SEL(MAXGAM)
        INTEGER*4 AAAA(MAXGAM),CCC(MAXGAM)
        CHARACTER AAAACCC(MAXGAM)*7

        CALL COPYRITE                                                             

        CALL GETSCONF(SCFREC,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
  	                                                                         
        TYPE*,IAM(),' '
        TYPE*,IAM(),'-------------------------------------------------------------------------'   
        TYPE*,IAM(),'<<<<< SELECAO DAS ORDENS DE PAGAMENTO PARA IMPRESSAO E PARA A BANCA >>>>>'   
        TYPE*,IAM(),'-------------------------------------------------------------------------'
        TYPE*,IAM(),' '
	
	TYPE*,' '
	TYPE*,' '
	TYPE*,' '
C
C Input Games/Sorteios for Expedition Report
C
        CALL INPUT_SORTEIO_PER_GAME(GAME_SEL,AAAACCC,AAAA,CCC)
C
	TYPE*,' '
	TYPE*,' '
C
C     	OPEN ORDENS DE PAGAMENTO FILE
C     	=============================
     	CALL OPEN_OPS('KEYED',ST)
      	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error Opening Order file', 0, ' ', 0, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
      	ENDIF    
C
C	OPEN OPSGEN FILE
C	================
        BOTGAM=1
        TOPGAM=MAXGAM

        DO 100 GAM=BOTGAM,TOPGAM
          GIND = SCFGNT(GAMIDX,GAM)
          GTYP = SCFGNT(GAMTYP,GAM)
          IF (GTYP.LE.0 .OR.
     *        GTYP.EQ.TPAS .OR. GTYP.EQ.TTGL) GOTO 100

          IF(GAME_SEL(GAM) .EQ. 0) GOTO 100
          ANO = AAAA(GAM)
          SEMANA = CCC(GAM)

          CALL OPEN_OPSGEN (OPSGEN_LUN(GAM), 0, 'WRITE', ANO, SEMANA, GAM, ST)
100     CONTINUE
      	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error Opening file for Selected Orders', 0, ' ', 0, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
      	ENDIF    
C
C     	OPEN ASF FILE
C     	=============
C
	CALL OPENASF(ASF)
	
C
C	FOR ALL GAMES
C       =============
        DO 300 GAM=BOTGAM,TOPGAM
           GIND = SCFGNT(GAMIDX,GAM)
           GTYP = SCFGNT(GAMTYP,GAM)
           IF (GTYP.LE.0 .OR.
     *         GTYP.EQ.TPAS .OR. GTYP.EQ.TTGL) GOTO 300
C
           IF(GAME_SEL(GAM) .EQ. 0) GOTO 300
           ANO = AAAA(GAM)
           SEMANA = CCC(GAM)
           ANOSEMANA = CTOI(AAAACCC(GAM),SZ)
C
C	   LOCATE FIRST RECORD FOR THE DRAW FOR THE GAME
C	   ---------------------------------------------
           WRITE(CHAVE,FMT='(I2.2,I4.4,I3.3)') GAM, ANO, SEMANA
	   READ(OPS_LUN, KEYID=0, KEY=CHAVE, ERR=300) OPS_REC

	   TYPE*,IAM(),'>>> Selecionando Ordens para Jogo :', GAM,', Sorteio :',ANOSEMANA

	   OP_COUNT = 0
           HAVE_PAID     = .FALSE.
           HAVE_SELECTED = .FALSE.
          
           DO WHILE (CTOI(OPS_REC.GAME,SZ).EQ.GAM .AND. CTOI(OPS_REC.YEARWEEK,SZ).EQ.ANOSEMANA)

              IF (.NOT.OPS_REC.CLAIM .AND. .NOT.OPS_REC.PRINTED_BY_OFF) THEN

		    IF (OPS_REC.PAID_CDC.NE.0) THEN       !OP MAY NOT BE ALREADY PAID
      		       HAVE_PAID = .TRUE.
		       GOTO 310  !SKIP IF ALREADY PAID
      		    ENDIF

		    IF (OPS_REC.GENERATED) THEN       !OP MAY NOT BE ALREADY SELECTED 
      		       HAVE_SELECTED = .TRUE.
                       GOTO 310  !SKIP IF ALREADY SELECTED
      		    ENDIF

                    IF (CTOI(OPS_REC.BANK,SZ).EQ.0) THEN

                       CALL GET_TERM (CTOI(OPS_REC.AGENT,SZ), TERM, ST)
	               IF (ST.NE.0) THEN
	                  CALL DISPERR (MSG_LUN, 'Agent not found in system = ', OPS_REC.AGENT, ' ', 0, ' ', 0)
      	                  CALL GSTOP (GEXIT_FATAL)
                       ENDIF

		       CALL READASF (TERM, ASFREC, ST)
		       IF (ST.NE.0) THEN
	                  CALL DISPERR (MSG_LUN, 'Error reading ASF file', 0, 'REC = ', TERM, ' ', 0)
      	                  CALL GSTOP (GEXIT_FATAL)
	               ENDIF

		       CALL ASCBIN (ASFINF, SBKOP, LBKOP, AGBANK, ST)
		       CALL ASCBIN (ASFINF, SBROP, LBROP, AGBRANCH, ST)

	               WRITE (OPS_REC.BANK, FMT='(I4.4)')   AGBANK
	               WRITE (OPS_REC.BRANCH, FMT='(I4.4)') AGBRANCH
                    ENDIF

	            OPS_REC.GENERATED = .TRUE.

   	            OP_COUNT = OP_COUNT + 1
	            WRITE (OPSGEN_LUN(GAM), IOSTAT=ST) OPS_REC
                    IF (ST.NE.0) THEN
	               CALL DISPERR (MSG_LUN, 'Error writing to OPSGEN file', 0, 'Status = ', ST, ' ', 0)
      	               CALL GSTOP (GEXIT_FATAL)
                    ENDIF

	            REWRITE (OPS_LUN, IOSTAT=ST) OPS_REC
                    IF (ST.NE.0) THEN
	               CALL DISPERR (MSG_LUN, 'Error writing BACK TO OPS file', 0, 'Status = ', ST, ' ', 0)
      	               CALL GSTOP (GEXIT_FATAL)
                    ENDIF

              ENDIF

310           CONTINUE

	      READ (OPS_LUN, END=200, IOSTAT=ST) OPS_REC
              IF (ST.NE.0) THEN
	         CALL DISPERR (MSG_LUN, 'Error reading from OPS file', 0, 'Status = ', ST, ' ', 0)
      	         CALL GSTOP (GEXIT_FATAL)
              ENDIF

           ENDDO	          

200        TYPE*,IAM(),'    Total de OPs transferidas :', OP_COUNT
           IF (HAVE_PAID) THEN
	      CALL DISPERR (MSG_LUN, 'Where found some OPs already PAID ', 0, ' ', 0, ' ', 0)
           ENDIF
           IF (HAVE_SELECTED) THEN
	      CALL DISPERR (MSG_LUN, 'Where found some OPs already SELECTED BEFORE ', 0, ' ', 0, ' ', 0)
           ENDIF

300	CONTINUE

	CLOSE(OPS_LUN)
        DO 400 GAM=BOTGAM,TOPGAM
          GIND = SCFGNT(GAMIDX,GAM)
          GTYP = SCFGNT(GAMTYP,GAM)
          IF (GTYP.LE.0 .OR.
     *        GTYP.EQ.TPAS .OR. GTYP.EQ.TTGL) GOTO 400
C
          IF(GAME_SEL(GAM) .EQ. 0) GOTO 400

	  CLOSE(OPSGEN_LUN(GAM))
400     CONTINUE
	CALL CLOSASF()

      	CALL GSTOP (GEXIT_SUCCESS)
      	END
