C
C SEND_OPS_TO_BANK.FOR                                                                    
C
C
C V07 09-OCT-2012 FRP Write civil year in the OP bank record
C V06 03-JAN-2012 FRP Fix name when opening temporal sort file
C V05 15-MAR-2011 FRP Request week draw for each game
C V04 24-DEC-2010 FRP Lotto2 Changes
C V03 25-MAR-2009 MMO BATCH JOKER/EM.
C V02 17-DEC-2003 FRP Modify for Batch2 Totobola Changes.
C V01 25-APR-2001 EPH INITIAL VERSION FOR SCML
C                                                                               
C SEND OPS (ODJ FORMAT) TO BANK FROM OPSGENgg_tt_yyyywww.FIL  FILE 
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
        PROGRAM SEND_OPS_TO_BANK
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
                                                                               
        INTEGER*4   OPSGEN_LUN, OPSGAM_LUN
        INTEGER*4   MSG_LUN /6/

        INTEGER*4   ANO, MES, DIA, SEMANA, GTYP, GIND, THISWEEK
	INTEGER*4   BOTGAM, TOPGAM, GAM
	INTEGER*4   ST, SZ
        INTEGER*4   OPS_SENT
	INTEGER*4   TIPO_OP 
	INTEGER*4   LAST_BANK
	INTEGER*4   BANK
	INTEGER*4   BANKPOS
	INTEGER*2   DATE(LDATE_LEN)

	CHARACTER   OPSGEN_FILE*26
	CHARACTER   OPSGAM_FILE*28

        INTEGER*4 GAME_SEL(MAXGAM)
        INTEGER*4 AAAA(MAXGAM),CCC(MAXGAM)
        CHARACTER AAAACCC(MAXGAM)*7

        CALL COPYRITE                                                             

        CALL GETSCONF(SCFREC,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
  	                                                                         
        TYPE*,IAM(),' '
        TYPE*,IAM(),'-------------------------------------------------------'   
        TYPE*,IAM(),'<<<<< ENVIO DAS ORDENS DE PAGAMENTO PARA A BANCA  >>>>>'   
        TYPE*,IAM(),'-------------------------------------------------------'
        TYPE*,IAM(),' '
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

C
C       CREATE A TEMPORARY OPSGEN FILE
C       ------------------------------
C
        DATE(VCDC) = DAYCDC
        CALL LCDATE(DATE)
        DIA = DATE(VDAY)
        MES = DATE(VMON)
        ANO = DATE(VYEAR) + 2000

	WRITE(OPSGEN_FILE, FMT='(A11,I2.2,A1,I4.4,I2.2,I2.2,A4)') 
     *    'FILE:OPSGEN', TIPO_OP, '_', ANO, MES, DIA, '.FIL'

        CALL FIND_AVAILABLE_LUN(OPSGEN_LUN,ST)
        IF (ST.NE.0) THEN
           TYPE*,IAM(),'ERROR GETTING LOGICAL UNIT FOR ',OPSGEN_FILE
           CALL GSTOP(GEXIT_FATAL)
        ENDIF

        OPEN (UNIT           =  OPSGEN_LUN,
     *        FILE           =  OPSGEN_FILE,
     *        STATUS         = 'NEW',
     *        ORGANIZATION   = 'SEQUENTIAL',
     *        ACCESS         = 'SEQUENTIAL',
     *        FORM           = 'UNFORMATTED',
     *        RECL           =  SIZEOF(OPS_REC),
     *        RECORDTYPE     = 'FIXED',
     *        IOSTAT         =  ST)
      	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error Opening '//OPSGEN_FILE//' SORT FILE', 0, 'STATUS = ', ST, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
      	ENDIF    

C
C       COPY ALL POs FROM OPSGENgg FILES INTO THE TEMPORARY OPSGEN FILE
C       ---------------------------------------------------------------
C
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

	  WRITE(OPSGAM_FILE, FMT='(A11,I2.2,A1,I2.2,A1,I4.4,I3.3,A4)') 
     *      'FILE:OPSGEN', GAM, '_', TIPO_OP, '_', ANO, SEMANA, '.FIL'

          CALL FIND_AVAILABLE_LUN(OPSGAM_LUN,ST)
          IF (ST.NE.0) THEN
             TYPE*,IAM(),'ERROR GETTING LOGICAL UNIT FOR ',OPSGAM_FILE
             CALL GSTOP(GEXIT_FATAL)
          ENDIF

          OPEN (UNIT           =  OPSGAM_LUN,
     *          FILE           =  OPSGAM_FILE,
     *          STATUS         = 'OLD',
     *          ORGANIZATION   = 'SEQUENTIAL',
     *          ACCESS         = 'SEQUENTIAL',
     *          FORM           = 'UNFORMATTED',
     *          RECL           =  SIZEOF(OPS_REC),
     *          RECORDTYPE     = 'FIXED',
     *          IOSTAT         =  ST)
      	  IF (ST.NE.0) THEN
	     CALL DISPERR (MSG_LUN, 'Error Opening '//OPSGAM_FILE//' OLD FILE', 0, 'STATUS = ', ST, ' ', 0)
      	     CALL GSTOP (GEXIT_FATAL)
      	  ENDIF    

	  DO 10 WHILE (.TRUE.)

	    READ(OPSGAM_LUN, END=30, IOSTAT=ST) OPS_REC
      	    IF (ST.NE.0) THEN
	       CALL DISPERR (MSG_LUN, 'Error Reading '//OPSGAM_FILE//' OLD FILE', 0, 'STATUS = ', ST, ' ', 0)
      	       CALL GSTOP (GEXIT_FATAL)
      	    ENDIF    

            WRITE(OPSGEN_LUN, IOSTAT=ST) OPS_REC
            IF (ST.NE.0) THEN
	       CALL DISPERR (MSG_LUN, 'Error Writing '//OPSGEN_FILE//' SORT FILE', 0, 'STATUS = ', ST, ' ', 0)
               CALL GSTOP (GEXIT_FATAL)
            ENDIF

10        CONTINUE

30        CLOSE(OPSGAM_LUN)

200     CONTINUE

        CLOSE(OPSGEN_LUN)

C
C	CREATE A SORTED FILE OF SAME NAME AND HIGHER VERSION FROM INPUT FILE (OPSGENtt_yyyywww.FIL)
C	------------------------------------------------------------------------------------------
	CALL CREATE_OPSGEN_SORTED_BY_BANK (OPSGEN_FILE, ST) 
	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error opening ' // OPSGEN_FILE//' INPUT file for SORT', 0, ' ', 0, 
     *                            'Or error during SORT.    STATUS = ', ST)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF

C	OPEN OPSGEN SORTED FILE WITH DISPOSE = DELETE
C	---------------------------------------------		
C	WRITE(OPSGEN_FILE, FMT='(A11,I2.2,A1,I4.4,I2.2,I2.2,A4)')  !Sorted file has same name (V06) 
C    *    'FILE:OPSGEN', TIPO_OP, '_', ANO, MES, DIA, '.FIL'

        CALL FIND_AVAILABLE_LUN(OPSGEN_LUN,ST)
        IF (ST.NE.0) THEN
           TYPE*,IAM(),'ERROR GETTING LOGICAL UNIT FOR ',OPSGEN_FILE
           CALL GSTOP(GEXIT_FATAL)
        ENDIF

        OPEN (UNIT           =  OPSGEN_LUN,
     *        FILE           =  OPSGEN_FILE,
     *        STATUS         = 'OLD',
     *        ORGANIZATION   = 'SEQUENTIAL',
     *        ACCESS         = 'SEQUENTIAL',
     *        FORM           = 'UNFORMATTED',
     *        RECL           =  SIZEOF(OPS_REC),
     *        RECORDTYPE     = 'FIXED',
     *        DISPOSE        = 'DELETE',
     *        IOSTAT         =  ST)
      	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error Opening '//OPSGEN_FILE//' SORT FILE', 0, 'STATUS = ', ST, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
      	ENDIF    

C
C	LOOP READING SORTED FILE
C	=========================

	LAST_BANK = 0

	DO 100 WHILE (.TRUE.)

	   READ(OPSGEN_LUN, END=300, IOSTAT=ST) OPS_REC
      	   IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Error Reading '//OPSGEN_FILE//' SORT FILE', 0, 'STATUS = ', ST, ' ', 0)
      	      CALL GSTOP (GEXIT_FATAL)
      	   ENDIF    

	   BANK = CTOI(OPS_REC.BANK,SZ)
	   IF (BANK.EQ.0) THEN
	      CALL DISPERR (MSG_LUN, 'Found value 0 for BANK in Order = '//OPS_REC.ORDER, 0, 'Skipping record ', 0, ' ', 0)
      	      GOTO 100
	   ENDIF

           IF (LAST_BANK.NE.BANK) THEN
C
C             HAVE TO OPEN A NEW ODJ FILE FOR THE NEW BANK
C
              IF (LAST_BANK.NE.0) THEN
C
C		 WRITE TRAILLER AND CLOSE LAST ODJ BANK FILE OPENED
C		 --------------------------------------------------
		 CALL WRITE_BNK(LAST_BANK,'TL', ST)
      		 IF (ST.NE.0) THEN
		    CALL DISPERR (MSG_LUN, 'Error writing TRAILER to ODJ file for BANK = ', LAST_BANK, 
     *                            BNK_REC.ERRSTR, 0, ' ', 0)
     	   	    CALL GSTOP (GEXIT_FATAL)
      	         ENDIF

                 CALL CLOSE_BNK (LAST_BANK,ST)
    		 IF (ST.NE.0) THEN
	            CALL DISPERR (MSG_LUN, 'Error during close of  ODJ file for BANK = ', LAST_BANK,
     *                            BNK_REC.ERRSTR, 0, ' ', 0)
      	            CALL GSTOP (GEXIT_FATAL)
                 ENDIF
              ENDIF

C	      OPEN ODJ FILE FOR THE NEW BANK
C	      ------------------------------
              CALL OPEN_BNK(BANK,'E','ODJ', ST)
              IF (ST.NE.0) THEN
	         CALL DISPERR (MSG_LUN, 'Error OPENING ODJ file for BANK = ', BANK, ' ', 0, ' ', 0)
      	            CALL GSTOP (GEXIT_FATAL)
              ENDIF
C
C             FILL HEADER AND WRITE IT TO FILE
C	      --------------------------------
	      DATE(VCDC) = OPS_REC.PAYABLE_CDC
	      CALL LCDATE(DATE)
	      WRITE(BNK_REC.DATA_PAYLIMIT, FMT='(I4.4,I2.2,I2.2)') 2000+DATE(VYEAR), DATE(VMON), DATE(VDAY)	      

	      DATE(VCDC) = DAYCDC
	      CALL LCDATE(DATE)
	      WRITE(BNK_REC.DATA_PROC, FMT='(I4.4,I2.2,I2.2)') 2000+DATE(VYEAR), DATE(VMON), DATE(VDAY)	      
              
C	      CALL GET_BANK_INFO (OPS_REC.BANK, BANK_NAME, CONTA_PREMIO, ST)
	      CALL GET_BANK_POS (BANK, BANKPOS)
	      IF (BANKPOS.EQ.0) THEN
	         CALL DISPERR (MSG_LUN, 'BANK not Found : '//ops_rec.bank, 0, ' ', 0, ' ', 0)
      	         CALL GSTOP (GEXIT_FATAL)
              ENDIF
      	      WRITE (BNK_REC.NIB_HD, FMT='(I4.4, A17)') BANK, BANK_TAB(BANKPOS).NIB_MUTUAS(1:17)   

	      WRITE(MSG_LUN,17) BANK_TAB(BANKPOS).LONG_NAME, BANK   
17	      FORMAT ('>>> Gerando ficheiro ODJ para o banco : ', A38, ' (', I4.4, ')')

              CALL WRITE_BNK (BANK,'HD',ST)
              IF (ST.NE.0) THEN
	         CALL DISPERR (MSG_LUN, 'Error WRITING HEADER TO ODJ file for BANK = ', BANK, ' ', 0, ' ', 0)
      	         CALL GSTOP (GEXIT_FATAL)
              ENDIF

              LAST_BANK = BANK

    	   ENDIF

           OPS_SENT = OPS_SENT + 1
C
C     	   FILL DETAIL RECORD
C	   ------------------

           BNK_REC.VALUE         = OPS_REC.TOTAL_GAME + OPS_REC.TOTAL_JOKER
      	   BNK_REC.AGENTE        = CTOI(OPS_REC.AGENT,SZ)
      	   BNK_REC.GAME          = CTOI(OPS_REC.GAME,SZ)
      	   WRITE (BNK_REC.NIB_DT, FMT='(I4.4, A15, A2)') BANK, BANK_TAB(BANKPOS).CONTA_PREMIO(1:15), '00'   
      	   BNK_REC.TIPO_DOC      = 73
      	   BNK_REC.BALCAO_TO_PAY = CTOI(OPS_REC.BRANCH,SZ)
      	   BNK_REC.SEMANA        = CTOI(OPS_REC.CWEEK,SZ)
C
C          BNK_REC.ANO           = CTOI(OPS_REC.YEARWEEK(3:4),SZ)  !Draw year (v07)
C
           CALL FIGWEK(DAYCDC,THISWEEK,BNK_REC.ANO)  !Civil year (v07)
           IF(THISWEEK .LT. BNK_REC.SEMANA) THEN
             BNK_REC.ANO = BNK_REC.ANO - 1
           ENDIF
C
      	   BNK_REC.BILHETE       = OPS_REC.BILHETE(1:7)
      	   BNK_REC.ORDEM         = CTOI(OPS_REC.ORDER,SZ)

           CALL WRITE_BNK (BANK,'DT',ST)
      	   IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Error WRITING DETAIL TO ODJ file for BANK = ', BANK, ' ', 0, ' ', 0)
      	      CALL GSTOP (GEXIT_FATAL)
      	   ENDIF

100     CONTINUE

300     CLOSE(OPSGEN_LUN)

C
C       DELETE THE TEMPORARY OPSGEN FILE
C       --------------------------------
C
        CALL DFILX(OPSGEN_FILE, 0, 0, ST)


	CALL WRITE_BNK(BANK,'TL', ST)
        IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Errorwriting TRAILER to ODJ file for BANK = ', BANK, BNK_REC.ERRSTR, 0, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
        ENDIF
        CALL CLOSE_BNK (BANK,ST)
        IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error closing ODJ file for BANK = ', BANK, BNK_REC.ERRSTR, 0, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
        ENDIF

        TYPE*,IAM(),'--------------------------------------------'
        TYPE*,IAM(),'TOTAL DE ORDENS ENVIADAS : ', OPS_SENT
        TYPE*,IAM(),'--------------------------------------------'

      	CALL GSTOP (GEXIT_SUCCESS)
      	END



C	*********************************************************
	SUBROUTINE CREATE_OPSGEN_SORTED_BY_BANK (OPSGEN_FILE, ST)
C	*********************************************************
        CHARACTER  OPSGEN_FILE*26
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

	KEYBUF(1) = 1			    !NUMBER OF KEYS
C
C	KEY 1 = OPS_REC.BANK
C
	KEYBUF(2) = %LOC(DSC$K_DTYPE_T)
	KEYBUF(3) = 0			    !0 = ASCENDING / 1 = DESCENDING
	KEYBUF(4) = 114			    !OFFSET FOR THE KEY  (BANK) -> LOOK AT DUMP UTILITY FOR OFFSET
	KEYBUF(5) = 4			    !KEY SIZE 

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
