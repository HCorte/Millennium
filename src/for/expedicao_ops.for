C
C EXPEDICAO_OPS.FOR                                                                  
C
C V10 13-MAR-2011 FRP Request week draw for each game
C V09 10-MAR-2011 FRP Allow optional EM file
C V08 27-DEC-2010 FRP Lotto2 Changes
C nova compilacacao para entrada em producao
C V07 18-NOV-2010 FJG Batch2: Fix NOBOUNDS checking errors
c v06 2-JUN-2007  cmb don't print OPs from portal
C V05 11-JAN-2007 CMB RENAME VERBETES TO EXPEDICAO_OPS
c v04 19-DEZ-2006 CMB ORDER BY BANK ID
C V03 26-MAY-2006 CMB NOVO JOGO EM
C V02 20-ABR-2004 CPH OPS ALTAS
C V01 03-ABR-2003 CBM INITIAL VERSION FOR SCML
C                                                                               
C REPORT OPS/AGENT FROM OP FILE
C                                                                               
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C Copyright 2003 SCML-DISI. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C====== OPTIONS/CHECK=NOOVERFLOW/EXT
        PROGRAM EXPEDICAO_OPS
        IMPLICIT NONE                                                  
	                                                
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:OPS_REC.DEF'
        INCLUDE 'INCLIB:BANK_REC.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
                                                                               
        INTEGER*4    MSG_LUN /6/
        INTEGER*4   NBRGAM        
        INTEGER*4   ST,SZ,K
        INTEGER*4   PAGE, LINCNT  
        INTEGER*4   YESNO
        CHARACTER   TITULO*35

        INTEGER*4  TMPLUN1/7/   !TEMPORARY FILE BEFOR SORT (REPORT)
        INTEGER*4  TMPLUN2/8/   !TEMPORARY FILE AFTER SORT (REPORT)
        INTEGER*4  REPLUN/9/    !REPORT (FINAL REPORT)

        CHARACTER*30 REPNAM        ! REPORT FILE NAME  (FINAL)
        CHARACTER*28 TMPNAM1       ! TEMP. FILE NAME  (BEF. SORT)
        CHARACTER*11 TMPNAM2       ! TEMP. FILE NAME  (AFTER SORT)
        CHARACTER*30 FILNAM        ! FILE NAME TO VALIDATE (AFTER SORT)
        CHARACTER*16 GNAME         ! GAME LONG NAME 
        CHARACTER*2  JOGO
        
        CHARACTER*4    BANK  ! BANK IN OPS REC
        CHARACTER*4    BANK_PJMC       !v06 - Bank OP for Portal


        DATA TMPNAM2/'TMPOPS2.REP'/

        CHARACTER*132 RECREP
        INTEGER*4 AGT, PRVAGT        ! AGT, PREVIOUS AGT
        INTEGER*8 VAL,JOK             ! GAME AND JOKER PRICE AMOUNT 
        INTEGER*4 TOTOP_CNT, TOTOP_AMT, TOTOP_JOKER   ! TOTAL AMONT  		

        INTEGER*8    VALOR        ! OPS TOTAL VALUE

        LOGICAL*1    ISTHERE
       	LOGICAL     EOF

        INTEGER*4 GNUM,GTYP,GIND,EM_OPS_EXP
        INTEGER*4 AA,MM,DD
        INTEGER*4 LOGLUN/10/
        INTEGER*2 DATE(12)
        CHARACTER DATAGER*8,EM_DATAGER*8,LOGNAM*40
       	       
        INTEGER*4 GAME_SEL(MAXGAM)
        INTEGER*4 AAAA(MAXGAM),CCC(MAXGAM)
        CHARACTER AAAACCC(MAXGAM)*7

  	                                                                        
        TYPE*,IAM(),' '
        TYPE*,IAM(),'-----------------------------------------------------------'   
        TYPE*,IAM(),'<<<<<          RELATORIO - EXPEDICAO OPS              >>>>>'   
        TYPE*,IAM(),'<<<<<          GERA EXP_OPS_aaaammdd.REP              >>>>>'
        TYPE*,IAM(),'-----------------------------------------------------------'   
        TYPE*,IAM(),' '
	      TYPE*,' '
	      TYPE*,' '
C
C  READ SCF.FIL
C
        CALL GETSCONF(SCFREC,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
C Request for EM Expedition File
C
        EM_OPS_EXP = 0
        CALL PRMYESNO('Quer Integrar Ficheiro com Informacao do EUROMILHOES (Y/N)?',EM_OPS_EXP)
        IF(EM_OPS_EXP .NE. 1) GOTO 200
C
C Input Generation Date of EM Expedition File
C
100     CONTINUE
        CALL WIMG(5,'Entre a Data da Geracao do Ficheiro do EUROMILHOES (AAAAMMDD):')
        READ(5,13) EM_DATAGER
        IF(EM_DATAGER.EQ.'e' .OR. EM_DATAGER.EQ.'E' .OR. EM_DATAGER.EQ.' ') CALL GSTOP(GEXIT_OPABORT)
        AA = CTOI(EM_DATAGER(1:4),SZ)
        MM = CTOI(EM_DATAGER(5:6),SZ)
        DD = CTOI(EM_DATAGER(7:8),SZ)
        IF((AA.LE.1900 .OR. AA.GT.2100) .OR.
     *     (MM.LE.0    .OR. MM.GT.12)   .OR.
     *     (DD.LE.0    .OR. DD.GT.31)) THEN
          CALL DISPERR (MSG_LUN, 'Data Invalida', 0, ' ', 0, ' ', 0)
          GOTO 100
        ENDIF
13      FORMAT(A8)
C
        WRITE (TMPNAM1,FMT='(A16,A8,A4)')  'FILE:EM_OPS_EXP_',EM_DATAGER,'.ASC'
        INQUIRE(FILE=TMPNAM1, EXIST=ISTHERE)
        IF(.NOT.ISTHERE) THEN
          TYPE*,'ERRO: NAO ENCONTRO O FICHEIRO COM INFORMACAO DO EUROMILHOES ',TMPNAM1
          STOP
        ENDIF       
C
C Generation Date of Expedition Report
C
200    CONTINUE 
       DATE(VCDC) = DAYCDC
       CALL LCDATE(DATE)
       DD = DATE(VDAY)
       MM = DATE(VMON)
       AA = DATE(VYEAR) + 2000
       WRITE(DATAGER,FMT='(I4.4,I2.2,I2.2)') AA,MM,DD
C
C Input Games/Sorteios for Expedition Report
C
500    CONTINUE
       CALL INPUT_SORTEIO_PER_GAME(GAME_SEL,AAAACCC,AAAA,CCC)
C
C Generate Log Report with Input Data
C
       WRITE(LOGNAM,FMT='(A26,A8,A4)') 'FILE:EXP_INPUT_VALIDATION_',DATAGER,'.LOG'
       CALL ROPEN(LOGNAM,LOGLUN,ST)
       IF(ST.NE.0) THEN
	 TYPE*,'Erro ao abrir o ficheiro .LOG',ST
         CALL GSTOP (GEXIT_FATAL)
       ENDIF
C
       WRITE(LOGLUN,15) DATAGER
15     FORMAT(1X,'INPUT VALIDATION REPORT FOR EXPEDICAO_OPS',/,
     *        1X,'=========================================',//,
     *        1X,'DATA DE GERACAO: ',A8)
C
       DO 700 GNUM=1,MAXGAM
         GIND = SCFGNT(GAMIDX,GNUM)
         GTYP = SCFGNT(GAMTYP,GNUM)
         IF(GTYP.LE.0 .OR.
     *      GTYP.EQ.TPAS .OR. GTYP.EQ.TTGL) GOTO 700
C
         IF(GAME_SEL(GNUM).EQ.1) THEN
           WRITE(LOGLUN,16) 'JOGO: ',(SCFLGN(K,GNUM),K=1,4),'SORTEIO: ',AAAACCC(GNUM)
16     FORMAT(//,1X,A,4A4,2X,A,A7)
         ENDIF
C
700    CONTINUE
C
       CLOSE(LOGLUN)
C
C
C
       TYPE*,IAM(),'Verifique o Relatorio de Log dos Inputs: ',LOGNAM
       CALL PRMYESNO('Quer continuar (Y/N)?', YESNO)     
       IF (YESNO.NE.1) GOTO 500
C            
C
C
       TYPE*,' '
       TYPE*,'Aguarde... A Gerar Relatorio'
       TYPE*,' '
C
       BANK_PJMC='0099'

C*************************************************
C   If file doesn't exist, SEND MESSAGE TO CONSOLE
C**************************************************

 	INQUIRE(FILE='FILE:OPS.FIL', EXIST=ISTHERE)
          IF(.NOT.ISTHERE) THEN
	    TYPE*,'ERRO: NAO ENCONTRO O FICHEIRO DE OPS'
	    STOP
	  ENDIF


	INQUIRE(FILE='FILE:ASF.FIL', EXIST=ISTHERE)
          IF(.NOT.ISTHERE) THEN
            TYPE*,'ERRO: NAO ENCONTRO O FICHEIRO ASF.FIL'
            STOP
          ENDIF


C++++++++++++++++++++++++++++++++++++++++++
C  OPEN DATA FILE - OPS.FIL
C
        CALL OPEN_OPS('SEQUENTIAL',ST)
        IF (ST.NE.0) THEN
           CALL DISPERR (MSG_LUN, 'Erro ao abrir ficheiro OPS', 0, ' ', 0, ' ', 0)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
        
c**************************************************           
C LOAD BANK TABLE STRUCTURE
C 
        CALL LOAD_BANK_TABLE(ST)
        IF (ST.NE.0) THEN
             tYPE*, IAM(), 'OPS EXPEDICAO:Erro a carregar a tabela dos Bancos'
           CALL GSTOP (GEXIT_FATAL)
	ENDIF

                
C++++++++++++++++++++++++++++++++++++++++++
C       OPEN FIRST TEMPORARY FILE 
C       TO INPUT DATA FROM OPS.FIL 

      IF(EM_OPS_EXP .EQ. 1) THEN
        OPEN (UNIT   = TMPLUN1,
     *        FILE   = TMPNAM1,
     *	      STATUS = 'old',
     *        ACCESS = 'APPEND',
     *	      IOSTAT = ST)
        IF (ST.NE.0) THEN
           CALL DISPERR (MSG_LUN, 'OPS EXPEDICAO Erro: ao abrir Ficheiro do Euromilhoes',
     *               0, ' ', 0, ' ', 0)
	   CLOSE(OPS_LUN)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
      ELSE
        WRITE (TMPNAM1,FMT='(A16,A8,A4)') 'FILE:AM_OPS_EXP_',DATAGER,'.ASC'
        OPEN (UNIT   = TMPLUN1,
     *        FILE   = TMPNAM1,
     *	      STATUS = 'new',
     *	      IOSTAT = ST)
        IF (ST.NE.0) THEN
           CALL DISPERR (MSG_LUN, 'OPS EXPEDICAO Erro: ao abrir Ficheiro para Apostas Mutuas',
     *               0, ' ', 0, ' ', 0)
	   CLOSE(OPS_LUN)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
      ENDIF
C+++++++++++++++++++++++++++++++++++++++++++
C	LOOP READING OPS FILE
C	=====================

    9 CONTINUE

	   READ (OPS_LUN, END=300, IOSTAT=ST) OPS_REC
      	   IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Erro a ler ficheiro de OPS', 
     *              0, 'STATUS = ', ST, ' ', 0)
	      CLOSE(OPS_LUN)
	      CLOSE(TMPLUN1)
      	      CALL GSTOP (GEXIT_FATAL)
      	   ENDIF    

           GNUM = CTOI(OPS_REC.GAME,SZ)
           IF(GAME_SEL(GNUM) .EQ. 0) GOTO 9 !IT'S NOT THE GAME I WANT TO LIST

           IF((GAME_SEL(GNUM) .EQ. 1 .AND.
     *         .NOT. OPS_REC.HI_PRIZE .AND.  !LESS THAN 5000 EUR
     *         AAAACCC(GNUM) .EQ. OPS_REC.YEARWEEK)) THEN

             VALOR=OPS_REC.TOTAL_GAME+OPS_REC.TOTAL_JOKER

C	     WRITE IT TO FIRST TEMPORARY FILE
C	     --------------------------------
             IF(OPS_REC.BANK .NE. BANK_PJMC) THEN    !DONT IMPORT PORTALOPS
               CALL WRITE_OP_LINE(TMPLUN1)	      
	     ENDIF
	   ENDIF

      GO TO 9
C	
C	END OF OP READING
C
300     CLOSE(OPS_LUN)
	CLOSE(TMPLUN1)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C	SORT DATA, BY AGENT NUMBER AND YEARWEEK
C       INPUT: 1ST TMP REPORT, OUTPUT: 2ND TMP REPORT

        TYPE*,' '
        TYPE*,'Aguarde... a ordernar os dados'
        TYPE*,' '

        CALL SORTREP(TMPNAM1,TMPNAM2,ST)

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       OPEN TEMP SORTED FILE 
C       ------------------------           

        CALL OPEN_REPFIL(TMPNAM2,TMPLUN2,ST)
        IF (ST.NE.0) THEN
             TYPE*,'Erro ao abrir o ficheiro temporario depois de ordenado'
             CALL GSTOP (GEXIT_FATAL)
        ENDIF    
C
C      OPEN REPORT FILE
C     ------------------
        WRITE (REPNAM,FMT='(A13,A8,A4)') 'FILE:EXP_OPS_',DATAGER,'.REP'
	CALL ROPEN(REPNAM,REPLUN,ST)
	IF(ST.NE.0) THEN
	    TYPE*,'Erro ao abrir o ficheiro .rep',ST
	    CLOSE(TMPLUN2)
	    CALL GSTOP (GEXIT_FATAL)
        ENDIF
C++++++++++++++++++++++++++++
C OPEN AGENT SALES FILE 

        CALL OPENASF(ASF)

C++++++++++++++++++++++++++++++++++++++++++++++++++
C READ SORTED FILE AN WRITE TO REPORT
C
        PAGE=0
        LINCNT=0
        EOF=.FALSE.

        PRVAGT=0
        TOTOP_CNT = 0
        TOTOP_AMT = 0	
        TOTOP_JOKER = 0

5	CONTINUE

	   READ (TMPLUN2 ,9001, END=600, IOSTAT=ST) RECREP
      	   IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Erro ao ler o ficheiro depois de ordenado', 
     *              0, 'STATUS = ', ST, ' ', 0)
	      CLOSE(ASF)      	     
	      CALL GSTOP (GEXIT_FATAL)
      	   ENDIF    


       	   AGT=CTOI(RECREP(01:07),SZ)
       	   VAL=CTOI(RECREP(28:40),SZ)
       	   JOK=CTOI(RECREP(41:53),SZ)
       	   BANK =RECREP(63:66)

C  DONT PRINT PORTAL document
C*******************************

      
      IF(BANK .NE. BANK_PJMC) THEN

C CALCULATE THE TOTALS AND WRITE THE DETAIL
C+++++++++++++++++++++++++++++++++++++++++

	 IF (AGT .EQ. PRVAGT ) THEN     ! AGENT 

           TOTOP_CNT = TOTOP_CNT + 1
           TOTOP_AMT = TOTOP_AMT + VAL
           TOTOP_JOKER = TOTOP_JOKER + JOK

	 ELSE

C FOR A DIFERENTE AGENT WRITE THE TOTALS JUMP PAGE 
C  START WITH A NEW AGENT 
C++++++++++++++++++++++++++++++++++++++++++++++++


	IF (TOTOP_CNT .NE. 0) THEN 

	  WRITE(REPLUN,120)
     *              ,TOTOP_CNT
     *              ,CMONY(TOTOP_AMT,13,VALUNIT)
     *              ,CMONY(TOTOP_JOKER,13,VALUNIT)


	  WRITE(REPLUN,104)
	
        WRITE(REPLUN,1005)
    
	ENDIF

	   CALL NEWPAGE(REPLUN,LINCNT,TITULO,AGT,BANK,PRVAGT)
        
		
           TOTOP_CNT = 1
           TOTOP_AMT = VAL
           TOTOP_JOKER = JOK
	      	 LINCNT=8


	PRVAGT=AGT


	ENDIF


        JOGO = RECREP(61:62)
        
        IF (JOGO .NE.'11') THEN
	  
             NBRGAM=CTOI(JOGO,SZ)       
             IF(NBRGAM.NE.0) WRITE(GNAME,123) (SCFLGN(K,NBRGAM),K=1,4)
        ELSE
        
           GNAME='EUROMILHOES'
           
        END IF      

          WRITE(REPLUN,150)                                            !V03
     *		,RECREP(54:60)				 !YEARWEEK
     *		,GNAME          			 !JOGO
     *          ,RECREP(8:10),RECREP(11:18),'***'        ! BILHETE
     *          ,RECREP(22:27)                           !ORDEM 
     *          ,CMONY(CTOI(RECREP(28:40),SZ),13,VALUNIT)       !VALOR
     *          ,CMONY(CTOI(RECREP(41:53),SZ),13,VALUNIT)       !VALOR JOKER
 
	LINCNT=LINCNT+1
C
123         FORMAT(4A4)

          END IF       !V06  
	GOTO 5
       
600     CLOSE(TMPLUN2)
	CALL CLOSASF()

C WRITE TOTAL FOR LAST AGENT
C---------------------------

      WRITE(REPLUN,120)
     *              ,TOTOP_CNT
     *              ,CMONY(TOTOP_AMT,13,VALUNIT)
     *              ,CMONY(TOTOP_JOKER,13,VALUNIT)
     
      WRITE(REPLUN,104)
	
        WRITE(REPLUN,1005)



	CLOSE(REPLUN)  !CLOSE REPORT FILE

C rename temp2 to file txt

        WRITE (FILNAM,FMT='(A13,A8,A4)') 'FILE:EXP_OPS_',DATAGER,'.TXT'

        CALL LIB$RENAME_FILE(TMPNAM2,FILNAM) 

C DELETE TEMPORARY FILES
C++++++++++++++++++++++++

        CALL DEL_REPFIL(TMPNAM1,TMPLUN1,ST)
        CALL DEL_REPFIL(TMPNAM2,TMPLUN2,ST)
C
      	CALL GSTOP (GEXIT_SUCCESS)

150     FORMAT(1X,A7,1X,A16,1X, A3,'-',A8,'-', A3, 2X, A6, 1X, A13, 2X, A13)

120     FORMAT(54X, 26('-'), /, 36X,'TOTAIS',2X,I4,3X,A13,2X,A13,/)

104   FORMAT (1X,/,/,1X,'Assinatura (bem legivel): ',45('_'),/)
1005    FORMAT (1X,/,'NOTA:  Se nao for o destinatario da presente correspondencia, por favor devolva'
     *        ,/,7X,'ao remetente: Departamento de Jogos, Unidade de Premios, Rua das Taipas,'
     *        ,/,7X,'n 1, 1250-264 LISBOA.')

9001   FORMAT(A132)

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


	      INTEGER*4    LUN
C	      INTEGER*4    BILH
	      INTEGER*4    GAM,SZ

	   
	     GAM = CTOI(OPS_REC.GAME,SZ)
c	     IF (GAM .GE. 05) GAM=GAM-1

C write tmpfile   

  
           WRITE(LUN,50)
     *          ,OPS_REC.AGENT
     *          ,OPS_REC.BILHETE (1:11)
     *          ,'***'
     *          ,OPS_REC.ORDER
     *          ,OPS_REC.TOTAL_GAME
     *	        ,OPS_REC.TOTAL_JOKER
     *		,OPS_REC.YEARWEEK 		
     *		,GAM
     *		,OPS_REC.BANK

50	FORMAT(A7, A11,A3, A6,I13.13,I13.13 ,A7,I2.2,A4)!

  
	RETURN
	END	


C       **************************************************************
        SUBROUTINE SORTREP(INPUTNAME,OUTPUTNAME,ST)
C       **************************************************************
        IMPLICIT NONE

        INTEGER*4  ISTATUS
        INTEGER*4  ST

        CHARACTER*28    INPUTNAME
        CHARACTER*11    OUTPUTNAME

        INTEGER*2       KEYBUF(17) ! REMOVE WARNING

        INTEGER*4       SOR$PASS_FILES
        INTEGER*4       SOR$BEGIN_SORT
        INTEGER*4       SOR$SORT_MERGE
        INTEGER*4       SOR$END_SORT

        EXTERNAL        SS$_ENDOFFILE
        EXTERNAL        DSC$K_DTYPE_T
        EXTERNAL        SOR$GK_RECORD
        INTEGER*4       SRTTYPE


        KEYBUF(1) = 4                       !NUMBER OF KEYS
C
C       KEY 1 = OPS_REC.BANK
C
        KEYBUF(2) = %LOC(DSC$K_DTYPE_T)
        KEYBUF(3) = 0                       !0 = ASCENDING / 1 = DESCENDING
        KEYBUF(4) = 62                       !OFFSET FOR THE KEY  (BANK) -> LOOK 
        KEYBUF(5) = 4                       !KEY SIZE         
C
C       KEY 2 = AGENT NUMBER
C
        KEYBUF(6) = %LOC(DSC$K_DTYPE_T)
        KEYBUF(7) = 0                     !0 = ASCENDING / 1 = DESCENDING
        KEYBUF(8) = 0                     !OFFSET FOR THE KEY  AGENT NUMBER 
        KEYBUF(9) = 7                     !KEY SIZE 
C
C       KEY 3 = ANOSEMANA
C
        KEYBUF(10) = %LOC(DSC$K_DTYPE_T)
        KEYBUF(11) = 0                       !0 = ASCENDING / 1 = DESCENDING
C        KEYBUF(12) = 87                     !OFFSET FOR THE KEY  (GAME) -> LOOK 
        KEYBUF(12) = 53                    !OFFSET FOR THE KEY  (semana) -> LOOK  !V03
        KEYBUF(13) = 7                       !KEY SIZE 
C
C       KEY 4 = n ordem
C
        KEYBUF(14) = %LOC(DSC$K_DTYPE_T)
        KEYBUF(15) = 0                       !0 = ASCENDING / 1 = DESCENDING
        KEYBUF(16) = 21                    !OFFSET FOR THE KEY  (ordem) -> LOO 
        KEYBUF(17) = 6                       !KEY SIZE 

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
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE OPEN_REPFIL(FILNAM,LUN,ST)
      IMPLICIT NONE

      CHARACTER*(*) FILNAM
      INTEGER*4 ST,LUN
C
      OPEN (UNIT    =  LUN,
     *      FILE    =  FILNAM,
     *      STATUS  =  'OLD',
     *      FORM    =  'FORMATTED',
     *      DISPOSE =  'KEEP',
     *      IOSTAT  =  ST)
C
      RETURN
      END
C
C++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE DEL_REPFIL(FILNAM,LUN,ST)
      CHARACTER*(*) FILNAM
      INTEGER*4 ST,LUN
C
      OPEN (UNIT    =  LUN,
     *      FILE    =  FILNAM,
     *      STATUS  =  'OLD',
     *      FORM    =  'FORMATTED',
     *      DISPOSE =  'DELETE',
     *      IOSTAT  =  ST)
C
      RETURN
      END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE NEWPAGE(REPLUN,LINCNT,TITULO, AGT, BANK, PRVAGT)     
      IMPLICIT NONE
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:AGTINF.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:PRMAGT.DEF'
      INCLUDE 'INCLIB:RECAGT.DEF'

      INTEGER*4 REPLUN, MSG_LUN
      INTEGER*4 LINCNT
      INTEGER*4 PAGE    !,SEMANA,ANO   
      CHARACTER*35 TITULO
      INTEGER*4 TERM, CNT,ST, agt, PRVAGT
      CHARACTER*4    BANK
C
      CHARACTER*8  IAGT_NO    ! FUNCTION FORMATED AGENT NUMBER XX.XXXXX
      INTEGER*4 ZIPCODE       ! AGENT ZIP CODE
      CHARACTER * 30 CITYNAME ! AGENT CITY NAME

	    TITULO = 'EXPEDICAO OPS' 
          PAGE=PAGE+1
       
       IF ( PRVAGT .NE. 0 ) THEN
         WRITE (REPLUN, 101) CHAR(12), BANK ,TITULO  
       ELSE
        WRITE (REPLUN, 101) ' ', BANK ,TITULO  
       END IF 

C FIND TERMINAL NUMBER AND AGENT NAME
C+++++++++++++++++++++++++++++++++++++++

	 CALL FIND_AGENT(AGT,TERM,ST)
	   IF (ST.NE.0) THEN
	   TYPE*, 'NAO ENCONTRO O NUMERO DE TERMINAL', TERM
	    CALL GSTOP (GEXIT_FATAL)
           ENDIF 


	   CALL READASF (TERM, ASFREC, ST)
                       IF (ST.NE.0) THEN
                           CALL DISPERR (MSG_LUN, 'Erro ao ler o ASF', 0, 'REC = ', TERM, ' ', 0)
                          CALL GSTOP (GEXIT_FATAL)
                       ENDIF 


	    WRITE(REPLUN,102) IAGT_NO(AGT)
      WRITE(REPLUN,200)(ASFBYT(CNT), CNT = SNAME, SNAME + 34) ! AGENT NAME ( 1 PART)
      WRITE(REPLUN,250)(ASFBYT(CNT), CNT = SNAME + 35, ENAME) ! AGENT NAME( 2 PART )
      WRITE(REPLUN,300)(ASFBYT(CNT), CNT = SSTRT, SSTRT + 34) ! AGENT ADRESS ( 1 PART)
      WRITE(REPLUN,350)(ASFBYT(CNT), CNT = SSTRT + 35, ESTRT) ! AGENT ADRESS ( 2 PART )
   
C GET AGENT POSTAL CODE
C
      CALL ASCBIN(ASFINF, SZIPC, LZIPC, ZIPCODE, ST)
       IF(ST .NE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'EXPEDICAO OPS,ERRO NO CODIGO POSTAL ', IAGT_NO(AGT)
        TYPE *, IAM()
       ENDIF
      
C GET AGENT CITY NAME
C
      CALL MOVBYT(%REF(ASFBYT), SZIPA, %REF(CITYNAME), 1, EZIPA - SZIPA + 1)
C
C FORMAT LINE 4 ( AGENT POSTAL CODE - LOCALIDADE )
C
      WRITE(REPLUN,400)
     *      ZIPCODE / 1000,                 ! ZIP CODE ( FIRST PART )
     *      MOD(ZIPCODE, 1000),             ! ZIP CODE ( SECOND PART )
     *      CITYNAME(1:26)                  ! AGENT CITY NAME
     
      WRITE(REPLUN,103)   
      WRITE(REPLUN,600)

101             FORMAT(A1,/,
     *          1X,'Jogos Santa Casa',/,    !T68,
     *          1X,'Banco: ', A4,/,/,
     *          1X, A15,/)

103   FORMAT(1X,/,/,/,/,/,/,
     *       1X,'Apos recepcao das Ordens de Pagamento abaixo discriminadas, devolva este',/, 
     *       1X, 'documento ao Departamento de Jogos: Unidade de Premios.')

102       FORMAT(T35,'          MEDIADOR:  ',A8)
200       FORMAT(T35,'          ',<MIN(35, LNAME)>A1)
250       FORMAT(T35,'          ',<LNAME - 35>A1)
300       FORMAT(T35,'          ',<MIN(35, LSTRT)>A1)
350       FORMAT(T35,'          ',<LSTRT - 35>A1)
400       FORMAT(T35,'          ',I4.4, '-', I3.3, X, A26,/,/)     
  
600       FORMAT( 1X, 78('='), /,
     *          1X 'SORTEIO JOGO                  BILHETE       ORDEM      VAL.JOGO     VAL.JOKER ',/,
     *          1X, 78('='))

      LINCNT=LINCNT+10

      RETURN
      END
