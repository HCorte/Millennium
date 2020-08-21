C
C RELAT_OPS_TO_BANK.FOR                                                                    
C
C
C V06 16-MAR-2011 FRP Request week draw for each game
C V05 15-MAR-2010 HXK Fix Joker/Main game count, amount totals
C V04 24-DEC-2010 FRP Lotto2 Changes
C V03 25-MAR-2003 MMO BATCH JOKER/EM.
C V02 13-NOV-2003 FRP Modify for Batch2 Totobola Changes.
C V01 26-APR-2001 EPH INITIAL VERSION FOR SCML
C                                                                               
C REPORT OF THE OP SENT TO BANK (FROM OPSGENtt_yyyywww.FIL FILE)
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
        PROGRAM RELAT_OPS_TO_BANK
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
                                                                               
        INTEGER*4   OPSGEN_LUN /TMP1_LUN/
        INTEGER*4   RELOPS_LUN /TMP2_LUN/
        INTEGER*4   MSG_LUN /6/

        INTEGER*4   ANO, SEMANA, MES, DIA, CDC
	INTEGER*4   GTYP, GIND, BOTGAM, TOPGAM, GAM
	INTEGER*4   ST, SZ, K
	INTEGER*4   TIPO_OP 
	INTEGER*4   BANK, GAME
	INTEGER*4   PAGE
	INTEGER*4   TOTOP_CNT, TOTOP_AMT
	INTEGER*4   TOTGER_CNT, TOTGER_AMT
        INTEGER*4   SEMANACIVIL, CCCWEK
	INTEGER*2   DATE(12)

	INTEGER*4   OPS_CNT(MAXGAM,MAXBANKS)	
	INTEGER*4   OPS_AMT(MAXGAM,MAXBANKS)

	CHARACTER   OPSGEN_FILE*28
	CHARACTER   RELOPS_FILE*31

	CHARACTER   GAME_NAME*16
	CHARACTER   STR_TIPOOP*13

	INTEGER*4   BANKPOS
	INTEGER*4   NUM_JOKER /5/

	LOGICAL     PRINTED_TITLE
	LOGICAL     FIRST_TITLE

        INTEGER*4 AA,MM,DD
        INTEGER*4 GAME_SEL(MAXGAM)
        INTEGER*4 AAAA(MAXGAM),CCC(MAXGAM)
        CHARACTER AAAACCC(MAXGAM)*7

        CALL COPYRITE                                                             

        CALL GETSCONF(SCFREC,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
  	                                                                         
        TYPE*,IAM(),' '
        TYPE*,IAM(),'--------------------------------------------------------------------------'   
        TYPE*,IAM(),'<<<<< RELATORIO DAS OPS SELECIONADAS PARA IMPRESSAO / ENVIO A BANCA >>>>>'   
        TYPE*,IAM(),'--------------------------------------------------------------------------'
	TYPE*,IAM(),'      GERA RELBANKS_tt_aaaammdd.REP '
	TYPE*,IAM(),'  '
	TYPE*,IAM(),'           tt        =   00 (OP normal e OP de doze dias)'
	TYPE*,IAM(),'           aaaammdd  =   data geracao'
        TYPE*,IAM(),'--------------------------------------------------------------------------'
        TYPE*,IAM(),' '
	TYPE*,' '
	TYPE*,' '
C
C	ORDER TYPE
C	-----------------	
        STR_TIPOOP = 'NORMAIS/ALTAS'
C
C Input Games/Sorteios
C
        CALL INPUT_SORTEIO_PER_GAME(GAME_SEL,AAAACCC,AAAA,CCC)
        TYPE*,' '
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
C	NAME INPUT FILE (SELECTED OPS)
C	------------------------------
        TIPO_OP = 0
           
        BOTGAM=1
        TOPGAM=MAXGAM

        DO 200 GAM=BOTGAM,TOPGAM
          GIND = SCFGNT(GAMIDX,GAM)
          GTYP = SCFGNT(GAMTYP,GAM)
          IF (GTYP.LE.0 .OR.
     *        GTYP.EQ.TPAS .OR. GTYP.EQ.TTGL) GOTO 200

          IF(GAME_SEL(GAM) .EQ. 0) GOTO 200
          ANO = AAAA(GAM)
          SEMANA = CCC(GAM)

	WRITE(OPSGEN_FILE, FMT='(A11,I2.2,A1,I2.2,A1,I4.4,I3.3,A4)') 
     *    'FILE:OPSGEN', GAM, '_', TIPO_OP, '_', ANO, SEMANA, '.FIL'

C
C	CREATE A SORTED FILE OF SAME NAME AND HIGHER VERSION FROM INPUT FILE (OPSGENtt_yyyywww.FIL)
C	------------------------------------------------------------------------------------------
	CALL CREATE_OPSGEN_SORTED_GAME_BANK (OPSGEN_FILE, ST) 
	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error opening ' // OPSGEN_FILE//' INPUT file for SORT', 0, ' ', 0, 
     *                            'Or error during SORT.    STATUS = ', ST)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
C
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

	DO 100 WHILE (.TRUE.)

	   READ(OPSGEN_LUN, END=300, IOSTAT=ST) OPS_REC
      	   IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Error Reading '//OPSGEN_FILE//' SORT FILE', 0, 'STATUS = ', ST, ' ', 0)
      	      CALL GSTOP (GEXIT_FATAL)
      	   ENDIF    

	   GAME = CTOI(OPS_REC.GAME,SZ)
	   BANK = CTOI(OPS_REC.BANK,SZ)

           IF(GAME.NE.GAM) GOTO 100  !game in PO must be equal to file OPSGENgg

	   IF (BANK.LE.0 .OR. GAME.LE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Found invalid value for BANK or GAME in OP = '//OPS_REC.ORDER, 0, ' ', 0, ' ', 0)
      	      GOTO 100
	   ENDIF
C
C	   GET POSITION FOR BANK IN BANK_TAB
C	
	   CALL GET_BANK_POS (BANK, BANKPOS)
	   IF (BANKPOS.LE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Invalid value for BANKPOS from GET_BANK_POS = ', BANKPOS, 'Using BANK = '//
     *                      BANK_TAB(1).BANK, 0, ' ', 0)
              BANKPOS = 1           ! SEND TO BANK 1
	   ENDIF
C
C	   SET COUNT AND AMOUNT FOR GAMES AND JOKER
C
C	   if main and joker then cnt=1 with amt = main amt + jok amt
C	   if main only then      cnt=1 with amt = main amt
C	   if joker only then     cnt=1 with amt = jok amt
C          if main IS joker then  cnt=1 with amt = main amt (assumption: jok amt = 0 in this case)
C
           IF (OPS_REC.TOTAL_GAME.GT.0) THEN        ! sum main game amount and count               
	      OPS_AMT(GAME,BANKPOS)      = OPS_AMT(GAME,BANKPOS)      + OPS_REC.TOTAL_GAME
	      OPS_CNT(GAME,BANKPOS)      = OPS_CNT(GAME,BANKPOS)      + 1
	      IF (OPS_REC.TOTAL_JOKER.GT.0) THEN    ! sum "add" joker game amount to main game
                OPS_AMT(GAME,BANKPOS) = OPS_AMT(GAME,BANKPOS) + OPS_REC.TOTAL_JOKER
	      ENDIF
           ELSEIF (OPS_REC.TOTAL_JOKER.GT.0) THEN   ! sum "add" joker game amt and cnt to main game if no main game amt
              OPS_AMT(NUM_JOKER,BANKPOS) = OPS_AMT(NUM_JOKER,BANKPOS) + OPS_REC.TOTAL_JOKER
	      OPS_CNT(NUM_JOKER,BANKPOS) = OPS_CNT(NUM_JOKER,BANKPOS) + 1
	   ENDIF

100     CONTINUE   !READ LOOP

300     CLOSE(OPSGEN_LUN)  !END READ

200     CONTINUE
C
C	PRINT REPORT
C	============
C
C	OPEN REPORT FILE RELBANKS_tt_aaaammdd.REP
C
        DATE(VCDC) = DAYCDC
        CALL LCDATE(DATE)
        DD = DATE(VDAY)
        MM = DATE(VMON)
        AA = DATE(VYEAR) + 2000

	WRITE(RELOPS_FILE, FMT='(A14,I2.2,A1,I4.4,I2.2,I2.2,A4)') 'FILE:RELBANKS_', TIPO_OP, '_', AA, MM, DD, '.REP'

        OPEN (UNIT   = RELOPS_LUN, 
     *	      FILE   = RELOPS_FILE,
     *        STATUS = 'NEW')

	PAGE = 0

        TOTGER_CNT = 0
	TOTGER_AMT = 0

	FIRST_TITLE = .TRUE.

	DO GAME=1,MAXGAM

	   PRINTED_TITLE = .FALSE. 

	   TOTOP_CNT = 0
	   TOTOP_AMT = 0

           IF(GAME_SEL(GAME) .EQ. 0) GOTO 400
           ANO = AAAA(GAME)
           SEMANA = CCC(GAME)
	
	   DO BANK=1,MAXBANKS

	      IF (OPS_AMT(GAME,BANK).GT.0) THEN

	         IF (.NOT.PRINTED_TITLE) THEN
C
C	            PRINT TITLE
C
 		    PAGE = PAGE + 1

                    SEMANACIVIL = SEMANA
	            IF(GAME.EQ.6 .OR. GAME.EQ.7) SEMANACIVIL = CCCWEK(ANO,SEMANA,GAME)

		    CALL SCML_DRAW_DATE (SEMANACIVIL, ANO, MES, DIA, CDC)	      

		    WRITE(GAME_NAME, FMT='(4A4)') (GLNAMES(K,GAME),K=1,4)

		    DATE(VCDC) = OPS_REC.PAYABLE_CDC
		    CALL LCDATE(DATE)
		    IF (DATE(VYEAR).LT.70) THEN
		       DATE(VYEAR) = DATE(VYEAR) + 2000
		    ELSE
		       DATE(VYEAR) = DATE(VYEAR) + 1900
		    ENDIF

	            IF (.NOT. FIRST_TITLE) THEN
                       WRITE(RELOPS_LUN,FMT='(/,A1)') '1'    !JUMP PAGE
                    ENDIF 
	     
		    WRITE (RELOPS_LUN, 102) PAGE, GAME_NAME, SEMANA, ANO, DIA, MES, ANO, STR_TIPOOP, 
     *                                      DATE(VDAY), DATE(VMON), DATE(VYEAR)
102		    FORMAT(1X, 131('='), /, 
     *            1X,'SCML - Departamento de Jogos',T43,'RESUMO DE ORDENS DE PAGAMENTO EMITIDAS POR BANCO',T124,'Pag.:',I4.4,/, 
     *            1X, 131('='), /,
     *            1X, 'Jogo : ', A16, T70, '                 Concurso : ', I3.3, '/', I4.4, 4X, I2.2, '/', I2.2, '/', I4.4, /,
     *            1X, 'OPS ',A13,     T70, 'Fim do Prazo de Pagamento : ', I2.2, '/', I2.2, '/', I4.4, /,
     *            1X, 131('-'),/,
     *            14X, 'B A N C O', T41, 'ORDENS', 5X, 'V A L O R', /,
     *            1X, 131('-'))

                  FIRST_TITLE   = .FALSE.
	          PRINTED_TITLE = .TRUE.

		 ENDIF

	         WRITE(RELOPS_LUN,103) BANK_TAB(BANK).LONG_NAME, OPS_CNT(GAME,BANK), 
     *                                 CSMONY(OPS_AMT(GAME,BANK),11,VALUNIT)
103	         FORMAT(/,1X, A38, T41, I6, 3X, A11)

	         TOTOP_CNT = TOTOP_CNT + OPS_CNT(GAME,BANK)
	         TOTOP_AMT = TOTOP_AMT + OPS_AMT(GAME,BANK)

	         TOTGER_CNT = TOTGER_CNT + OPS_CNT(GAME,BANK)
	         TOTGER_AMT = TOTGER_AMT + OPS_AMT(GAME,BANK)

              ENDIF

	   ENDDO

           IF (PRINTED_TITLE) THEN
	      WRITE (RELOPS_LUN,109) TOTOP_CNT, CSMONY(TOTOP_AMT,11,BETUNIT)
109 	      FORMAT(/,1X, 131('-'), //, 1X,'TOTAIS', T41, I6, 3X, A11)
           ENDIF

400     CONTINUE

	ENDDO

	WRITE (RELOPS_LUN,107) TOTGER_CNT, CSMONY(TOTGER_AMT,11,BETUNIT)
107   	FORMAT(/,1X, 131('-'), //, 1X,'TOTAIS GERAIS', T41, I6, 3X, A11,/,'1')

	CLOSE (RELOPS_LUN)

      	CALL GSTOP (GEXIT_SUCCESS)
      	END


C	**************************************************************
	SUBROUTINE CREATE_OPSGEN_SORTED_GAME_BANK (OPSGEN_FILE, ST)
C	**************************************************************
        CHARACTER  OPSGEN_FILE*(*)   !*24
        INTEGER*4  ISTATUS

	INTEGER*4  ST

	CHARACTER*30	INPUTNAME
	CHARACTER*30	OUTPUTNAME

	INTEGER*2	KEYBUF(9)

	INTEGER*4	SOR$PASS_FILES
	INTEGER*4	SOR$BEGIN_SORT
	INTEGER*4	SOR$SORT_MERGE
	INTEGER*4	SOR$END_SORT

        EXTERNAL	SS$_ENDOFFILE
        EXTERNAL	DSC$K_DTYPE_T
        EXTERNAL	SOR$GK_RECORD
	INTEGER*4       SRTTYPE

	INPUTNAME  = OPSGEN_FILE
	OUTPUTNAME = OPSGEN_FILE

	KEYBUF(1) = 2			    !NUMBER OF KEYS
C
C	KEY 1 = OPS_REC.GAME
C
	KEYBUF(2) = %LOC(DSC$K_DTYPE_T)
	KEYBUF(3) = 0			    !0 = ASCENDING / 1 = DESCENDING
	KEYBUF(4) = 0			    !OFFSET FOR THE KEY  (GAME) -> LOOK AT DUMP UTILITY FOR OFFSET
	KEYBUF(5) = 2			    !KEY SIZE 
C
C	KEY 2 = OPS_REC.BANK
C
	KEYBUF(6) = %LOC(DSC$K_DTYPE_T)
	KEYBUF(7) = 0			    !0 = ASCENDING / 1 = DESCENDING
	KEYBUF(8) = 114			    !OFFSET FOR THE KEY  (GAME) -> LOOK AT DUMP UTILITY FOR OFFSET
	KEYBUF(9) = 4			    !KEY SIZE 

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
