C
C     REL5000_OPS.FOR                                                         
C
C V05 16-MAR-2011 FRP Request week draw for each game
C V04 27-DEC-2010 FRP Lotto2 Changes
C V03 25-MAR-2009 MMO BATCH JOKER/EM.
C v02 13-JAN-2004 CMB Modify for Batch2 Totobola Changes.
C V01 25-MAR-2003 CMB INITIAL VERSION FOR SCML
C                                                                               
C REPORT BIGGER THEN 500 OF THE OP SENT TO BANK (FROM OPSGENgg_00_yyyyww.FIL FILE)
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
        PROGRAM REL5000_OPS
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

        INTEGER*4    NBRGAM
	INTEGER*4    REPLUN/8/    ! REPORT FILE UNIT
        INTEGER*4    OPSLUN/7/    ! INPUT DATA OPS FILE UNIT
        INTEGER*4    TMPLUN/9/    ! TEMPORARY FILE AFTER SORT
        CHARACTER*30 REPNAM       ! REPORT FILE NAME
        CHARACTER*30 OPSNAM       ! OPS FILE NAME
        CHARACTER*30 TMPNAM       ! TEMP. FILE NAME AFTER SORT
        DATA TMPNAM/'TMPOPS.FIL'/
        INTEGER*4    ANO,SEMANA
	INTEGER*4    ANOSEMANA,ST
	INTEGER*4    VALOR        ! OPS TOTAL VALUE
	INTEGER*4    K,SZ        
	INTEGER*4    BANK, BANKPOS  ! BANK IN OPS REC, BANK POSITION
	INTEGER*4    PRVBANK, PRVBKPOS	    ! PREV. BANK
	INTEGER*4    GAM,BOTGAM, TOPGAM
	INTEGER*4    GTYP, GIND
        CHARACTER*16 GNAME         ! GAME LONG NAME 
        CHARACTER*38 BNAME         ! BANK LONG NAME

        LOGICAL     PRINTED_TITLE
        LOGICAL     FIRST_TITLE
        INTEGER*4   PAGE
	INTEGER*4   MES, DIA, CDC
        INTEGER*4   YESNO
        INTEGER*4   SEMANACIVIL, CCCWEK
        
        LOGICAL*1    ISTHERE

	INTEGER*2   DATE(LDATE_LEN)
        INTEGER*4 AA,MM,DD
        INTEGER*4 GAME_SEL(MAXGAM)
        INTEGER*4 AAAA(MAXGAM),CCC(MAXGAM)
        CHARACTER AAAACCC(MAXGAM)*7

C
C  READ SCF.FIL
C
        CALL GETSCONF(SCFREC,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)

        TYPE*,IAM(),' '
        TYPE*,IAM(),'--------------------------------------------------------' 
        TYPE*,IAM(),'<<<<< RELATORIO DAS OPS SELECIONADAS PARA IMPRESSAO / E ' 
        TYPE*,IAM(),'--------------------------------------------------------' 
        TYPE*,IAM(),'      GERA REL5000_aaaammdd.REP '
        TYPE*,IAM(),'  '
        TYPE*,IAM(),'           aaaammdd   =   data geracao'
        TYPE*,IAM(),'-------------------------------------------------------' 
        TYPE*,IAM(),' '

C
C Input Games/Sorteios
C
        CALL INPUT_SORTEIO_PER_GAME(GAME_SEL,AAAACCC,AAAA,CCC)
        TYPE*,' '
        TYPE*,' '
C
        CALL PRMYESNO('Confirma o relatorio (Y/N) ? ', YESNO)
        IF (YESNO.NE.1) STOP
C
        TYPE*,' '
        TYPE*,IAM(),'REL5000_OPS: Aguarde... A Gerar Relatório'
        TYPE*,' '


C       LOAD BANK TABLE STRUCTURE
C       -------------------------
        CALL LOAD_BANK_TABLE(ST)
        IF (ST.NE.0) THEN
             tYPE*, IAM(), 'REL5000_OPS:Erro a carregar a tabela dos Bancos'
           CALL GSTOP (GEXIT_FATAL)
	ENDIF

C
C  CREATE NAME OF OPSGEN FILE
C  --------------------------
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
          ANOSEMANA = CTOI(AAAACCC(GAM),SZ)
       
               WRITE(OPSNAM, FMT='(A11,I2.2,A4,I4.4,I3.3,A4)') 
     *    'FILE:OPSGEN', GAM, '_00_', ANO, SEMANA, '.FIL'
       
C*************************************************
C   If file OPSGEN.FIL doesn't exist, SEND MESSAGE TO CONSOLE
C**************************************************

 	INQUIRE(FILE=OPSNAM, EXIST=ISTHERE)
          IF(.NOT.ISTHERE) THEN
	    TYPE*,IAM(),'Erro:Não encontro o ficheiro ',OPSNAM
	    TYPE*,IAM(),' '
	    TYPE*,IAM(),'Erro:Verifique se já foi corrido o SELECT_OPS_TO_PRINT,'
	    TYPE*,IAM(),'      OPS 12 dias, para o concurso',ANOSEMANA  
            CALL GSTOP(GEXIT_FATAL)
	  ENDIF          

C
CC NAME OF THE REPORT
C
        DATE(VCDC) = DAYCDC
        CALL LCDATE(DATE)
        DD = DATE(VDAY)
        MM = DATE(VMON)
        AA = DATE(VYEAR) + 2000

        WRITE(REPNAM, FMT='(A13,I4.4,I2.2,I2.2,A4)') 'FILE:REL5000_', AA, MM, DD, '.REP'

C   CREATE A SORTED FILE OF SAME NAME AND HIGHER VERSION FROM INPUT FILE 
C  ----------------------------------------------------------------------- 

        CALL SORTFIL (OPSNAM,TMPNAM,ST) 
        IF (ST.NE.0) THEN

               TYPE*,'REL5000_OPS: Error ao ordenar o ficheiro',ST

           CALL GSTOP (GEXIT_FATAL)
        ENDIF
C
C  OPEN TMP FIL SORTED FILE WITH DISPOSE = DELETE
C   ---------------------------------------------           

        CALL OPEN_OPS_DEL(TMPNAM,TMPLUN,ST)
        IF (ST.NE.0) THEN
             TYPE*,'REL5000_OPS: Error ao abrir o ficheiro temporário depois de ordenado'
             CALL GSTOP (GEXIT_FATAL)
        ENDIF    
C
C OPEN REPORT FILE
C

      CALL ROPEN(REPNAM,REPLUN,ST)
        IF(ST.NE.0) THEN
	 TYPE*,'REL5000_OPS: Error ao abrir o relatório ',ST
   	 CALL GSTOP (GEXIT_FATAL) 
        ENDIF	


	PAGE=0
     
	FIRST_TITLE = .TRUE.
 
         IF (.NOT.PRINTED_TITLE) THEN
C
C  PRINT TITLE
C -------------
          PAGE = PAGE + 1

          PRINTED_TITLE = .FALSE.
            
            SEMANACIVIL = SEMANA
	    IF(GAM.EQ.6 .OR. GAM.EQ.7) SEMANACIVIL = CCCWEK(ANO,SEMANA,GAM)

            CALL SCML_DRAW_DATE (SEMANACIVIL, ANO, MES, DIA, CDC)

          IF (.NOT. FIRST_TITLE) THEN
           WRITE(REPLUN,FMT='(/,A1)') '1'    !JUMP PAGE
          ENDIF 
             
           WRITE (REPLUN, 102) PAGE, SEMANA, ANO, DIA, MES

          FIRST_TITLE   = .FALSE.
          PRINTED_TITLE = .TRUE.
                  
         ENDIF 
C
C  READING OPS INPUT FILE
C

	PRVBANK=0
	PRVBKPOS=0

    5 CONTINUE

       READ(TMPLUN,END=300,IOSTAT=ST) OPS_REC
       IF (ST.NE.0) THEN
            TYPE*,'REL5000_OPS: Error ao ler o ficheiro temporário',ST

          CALL GSTOP (GEXIT_FATAL)
       ENDIF    


	 VALOR=OPS_REC.TOTAL_GAME+OPS_REC.TOTAL_JOKER
	 

	 BANK =CTOI(OPS_REC.BANK,SZ)
	  IF (BANK.LE.0) THEN
              tYPE*,IAM(), 'REL5000_OPS: Banco inválido na OP = '//OPS_REC.ORDER
              GOTO 5
           ENDIF

          NBRGAM=CTOI(OPS_REC.GAME,SZ)                
            WRITE(GNAME,123) (SCFLGN(K,NBRGAM),K=1,4)
123         FORMAT(4A4)

	
C          GET POSITION FOR BANK IN BANK_TAB
C       
           CALL GET_BANK_POS (BANK, BANKPOS)
           IF (BANKPOS.LE.0) THEN
           TYPE*, IAM(), 'REL5000_OPS: Posição do Banco inválida ', BANKPOS, 
     *                  'para o banco ',BANK_TAB(1).BANK
     
              BANKPOS = 1           ! SEND TO BANK 1
           ENDIF	 

          BNAME=BANK_TAB(BANKPOS).LONG_NAME

c ONLY OPS WITH VALUE BIGGUER 5000 EUR


	  IF (VALOR.GE.500000) THEN          ! BIGGER THEN 5000

	

	    IF (BANK .EQ. PRVBANK) THEN        ! BANK


C JUST FILL THE DETAIL
C --------------------

	 WRITE(REPLUN,101)GNAME
     *             ,OPS_REC.AGENT
     *             ,OPS_REC.BILHETE(1:3),OPS_REC.BILHETE(4:11)
     *		   ,OPS_REC.BILHETE(12:14)
     *             ,OPS_REC.ORDER
     *             ,(OPS_REC.WINS(K),K=1,6)
     *             ,OPS_REC.JOKER_DIV	
     *             ,CSMONY(VALOR,11,VALUNIT)

	   ELSE

C BANK LONG NAME HAS A HEADER AND FILL DETAIL
C -------------------------------------------
 
            WRITE(REPLUN,124) BNAME

	 WRITE(REPLUN,101) GNAME
     *             ,OPS_REC.AGENT
     *             ,OPS_REC.BILHETE(1:3),OPS_REC.BILHETE(4:11)
     *		   ,OPS_REC.BILHETE(12:14)
     *             ,OPS_REC.ORDER
     *             ,(OPS_REC.WINS(k),k=1,6)
     *             ,OPS_REC.JOKER_DIV 
     *             ,CSMONY(VALOR,11,VALUNIT)

	PRVBANK=BANK
	PRVBKPOS=BANKPOS


          ENDIF      ! BANK
	 ENDIF     ! BIGGER THEN 5000 EUR

	GOTO 5

  300 CONTINUE       !  END OPS FILE
  200 CONTINUE

      CLOSE(1)
      CLOSE(OPSLUN)
      CLOSE(REPLUN)
      CLOSE(TMPLUN)

      STOP
      

101       FORMAT(10X,A16,3X,A7,5X,A3,'-',A8,'-',A3,5X,A6,5X,6I3,6X,I2,4X,A11)      
102   FORMAT(1X, 131('='), /, 
     *       1X,'SCML - Departamento de Jogos',T43,'ORDENS DE PAGAMENTO COM VALOR >= 5000 EUR POR BANCO',T124,'Pag.:',I4.4, /,
     *       1X, 131(' '), /, 
     *       1X, T50,'Concurso : ', I3.3, '/', I4.4, 4X, I2.2, '/', I2.2, /, 
     *       1X, 131('='), /,
     *       10X,'JOGO',16X,'MEDIADOR',8X,'N SERIE',8X, 'ORDENS',
     *       6x, ' 1  2  3  4  5  6 ', 3X, 'JOKER',5X, 'V A L O R', /,
     *       1X, 131('-'))      
124       FORMAT(/,1X,A38)      
      END

C
C+++++++++++++++++++++++++++++++++++++++++++++++++++
C
        SUBROUTINE SORTFIL(INPUTNAME,OUTPUTNAME, ST)
        IMPLICIT NONE
        INTEGER*4  ISTATUS

        INTEGER*4  ST

        CHARACTER*30    INPUTNAME
        CHARACTER*30    OUTPUTNAME

        INTEGER*2       KEYBUF(13)

        INTEGER*4       SOR$PASS_FILES
        INTEGER*4       SOR$BEGIN_SORT
        INTEGER*4       SOR$SORT_MERGE
        INTEGER*4       SOR$END_SORT

        EXTERNAL        SS$_ENDOFFILE
        EXTERNAL        DSC$K_DTYPE_T
        EXTERNAL        SOR$GK_RECORD
        INTEGER*4       SRTTYPE


        KEYBUF(1) = 3                       !NUMBER OF KEYS
C
C       KEY 1 = OPS_REC.BANK
C
        KEYBUF(2) = %LOC(DSC$K_DTYPE_T)
        KEYBUF(3) = 0                       !0 = ASCENDING / 1 = DESCENDING
        KEYBUF(4) = 114                     !OFFSET FOR THE KEY  (BANK) -> LOOK 
        KEYBUF(5) = 4                       !KEY SIZE 

C
C       KEY 2 = OPS_REC.GAME
C
        KEYBUF(6) = %LOC(DSC$K_DTYPE_T)
        KEYBUF(7) = 0                       !0 = ASCENDING / 1 = DESCENDING
        KEYBUF(8) = 0                     !OFFSET FOR THE KEY  (GAME) -> LOOK 
        KEYBUF(9) = 2                       !KEY SIZE 


C       KEY 3 = OPS_REC.AGENT
C
        KEYBUF(10) = %LOC(DSC$K_DTYPE_T)
        KEYBUF(11) = 0                       !0 = ASCENDING / 1 = DESCENDING
        KEYBUF(12) = 35                      !OFFSET FOR THE KEY  (AGENT) -> LOOK 
        KEYBUF(13) = 7                       !KEY SIZE 

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
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE OPEN_OPS_DEL(FILNAM,LUN,ST)
      IMPLICIT NONE
C
      CHARACTER*(*) FILNAM
      INTEGER*4 ST,LUN
C
           OPEN (UNIT           =  LUN,
     *           FILE           =  FILNAM,
     *           STATUS         = 'OLD',
     *           ORGANIZATION   = 'SEQUENTIAL',
     *           ACCESS         = 'SEQUENTIAL',
     *           FORM           = 'UNFORMATTED',
     *           RECORDTYPE     = 'FIXED',
     *           DISPOSE        = 'DELETE',
     *           IOSTAT         =  ST)

      RETURN
      END
