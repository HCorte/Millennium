C
C UNPAID_OPS.FOR                                                                    
C
C V03 16-MAR-2011 FRP Request week draw for each game
C V02 27-DEC-2010 FRP Lotto2 Changes
C V01 19-MAR-2003 CPH INITIAL VERSION FOR SCML
C                                                                               
C REPORT UNPAID OPS FROM OP FILE
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
        PROGRAM UNPAID_OPS
        IMPLICIT NONE                                                  
	                                                
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF'
C        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:OPS_REC.DEF'
C        INCLUDE 'INCLIB:AGTCOM.DEF'
C        INCLUDE 'INCLIB:WINCOM.DEF'
C 	 INCLUDE 'INCLIB:PRN_BUFF.DEF'
C 	 INCLUDE 'INCLIB:BANK_REC.DEF'
C 	 INCLUDE 'INCLIB:BRANCH_REC.DEF'
C 	 INCLUDE 'INCLIB:INTERFACES_REC.DEF'
                                                                               
        INTEGER*4   MSG_LUN /6/
        INTEGER*4   ANOSEMANA, ANO, SEMANA
	CHARACTER   YEARWEEK*7
	INTEGER*4   ST
	INTEGER*4   PAGE, LINCNT
        INTEGER*4   YESNO
	CHARACTER   TITULO*35

	INTEGER*4  TMPLUN1/7/   !TEMPORARY FILE BEFOR SORT (REPORT)
	INTEGER*4  TMPLUN2/8/   !TEMPORARY FILE AFTER SORT (REPORT)
        INTEGER*4  REPLUN/9/    !REPORT (FINAL REPORT)

        CHARACTER*30 REPNAM        ! REPORT FILE NAME  (FINAL)
        CHARACTER*30 TMPNAM1       ! TEMP. FILE NAME  (BEF. SORT)
        CHARACTER*30 TMPNAM2       ! TEMP. FILE NAME  (AFTER SORT)
        DATA TMPNAM1/'TMPOPS1.REP'/
        DATA TMPNAM2/'TMPOPS2.REP'/

	CHARACTER*132 RECREP

	INTEGER*2 DATE(LDATE_LEN)
        INTEGER*4 AA,MM,DD, GAM, SZ
        INTEGER*4 GAME_SEL(MAXGAM)
        INTEGER*4 AAAA(MAXGAM),CCC(MAXGAM)
        CHARACTER AAAACCC(MAXGAM)*7


        CALL COPYRITE                                                             
  	                                                                         
        TYPE*,IAM(),' '
        TYPE*,IAM(),'-----------------------------------------------------------'   
        TYPE*,IAM(),'<<<<<          RELATÓRIO DAS OPS NÃO PAGAS            >>>>>'   
        TYPE*,IAM(),'<<<<<           GERA UNPAID_OPS_aaaammdd.REP          >>>>>'
	TYPE*,IAM(),'<<<<<                                                 >>>>>'
	TYPE*,IAM(),'<<<<<            aaaammdd  =   data geracao           >>>>>'
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
C
        CALL PRMYESNO('Confirma o relatorio (Y/N) ? ', YESNO)
        IF (YESNO.NE.1) STOP
C
	TYPE*,' '
	TYPE*,'Aguarde... A Gerar Relatório'
	TYPE*,' '

C++++++++++++++++++++++++++++++++++++++++++
C	LOAD BANK TABLE STRUCTURE
C	-------------------------
C	CALL LOAD_BANK_TABLE(ST)
C        IF (ST.NE.0) THEN
C	   CALL DISPERR (MSG_LUN, 'Error loading BANK TABLE', 0, 'STATUS = ', ST, ' ', 0)
C      	   CALL GSTOP (GEXIT_FATAL)
C        ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++
C  OPEN DATA FILE - OPS.FIL
C
        CALL OPEN_OPS('SEQUENTIAL',ST)
        IF (ST.NE.0) THEN
           CALL DISPERR (MSG_LUN, 'Error Opening Order file', 0, ' ', 0, ' ', 0)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
C++++++++++++++++++++++++++++++++++++++++++
C       OPEN FIRST TEMPORARY FILE 
C       TO INPUT DATA FROM OPS.FIL 

      OPEN (UNIT   = TMPLUN1,
     *      FILE   = TMPNAM1,
     *	    STATUS = 'NEW',
     *	    IOSTAT = ST)
C
        IF (ST.NE.0) THEN
           CALL DISPERR (MSG_LUN, 'Error Opening 1ST TEMPORARY file',
     *               0, ' ', 0, ' ', 0)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
C+++++++++++++++++++++++++++++++++++++++++++
C	LOOP READING OPS FILE
C	=====================

	DO 100 WHILE (.TRUE.)

	   READ (OPS_LUN, END=300, IOSTAT=ST) OPS_REC
      	   IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Error Reading OPS FILE', 
     *              0, 'STATUS = ', ST, ' ', 0)
      	      CALL GSTOP (GEXIT_FATAL)
      	   ENDIF    

           GAM = CTOI(OPS_REC.GAME,SZ)
           IF(GAME_SEL(GAM) .EQ. 0) GOTO 100  !IT'S NOT THE GAME I WANT TO LIST

           ANO = AAAA(GAM)
           SEMANA = CCC(GAM)
           ANOSEMANA = CTOI(AAAACCC(GAM),SZ)

C	   TESTE PARA SABER DADOS DO FICHEIRO
C	   TYPE*,' ANOSEMANA =', OPS_REC.YEARWEEK 
C	   TYPE*,' PAGO CDC = ', OPS_REC.PAID_CDC

           WRITE(YEARWEEK,FMT='(I7.7)') ANOSEMANA
	   IF (YEARWEEK .NE. OPS_REC.YEARWEEK) GOTO 100	 !IT'S NOT THE WEEK I WANT TO LIST
C	   IF (OPS_REC.CLAIM) GOTO 100			 ! ** DON'T WANT CLAIMS IN REPORT **
	
	   IF (OPS_REC.PAID_CDC.EQ.0) THEN

C	  WRITE IT TO FIRST TEMPORARY FILE
C	 -----------------------------------
	      CALL WRITE_OP_LINE(TMPLUN1)	      
	   ENDIF
C
100     CONTINUE 
C	
C	END OF OP READING
C
300     CLOSE(OPS_LUN)
	CLOSE(TMPLUN1)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C	SORT DATA, BY AGENT NUMBER
C       INPUT: 1ST TMP REPORT, OUTPUT: 2ND TMP REPORT

        CALL SORTREP(TMPNAM1,TMPNAM2,ST)

C             TYPE*,'AFTER SORT ST=',ST

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       OPEN TEMP SORTED FILE 
C       ------------------------           

        CALL OPEN_REPFIL(TMPNAM2,TMPLUN2,ST)
        IF (ST.NE.0) THEN
             TYPE*,'****** ERROR OPEN TMP SORTED REP'
             CALL GSTOP (GEXIT_FATAL)
        ENDIF    
C
C      OPEN REPORT FILE
C     ------------------
        DATE(VCDC) = DAYCDC
        CALL LCDATE(DATE)
        DD = DATE(VDAY)
        MM = DATE(VMON)
        AA = DATE(VYEAR) + 2000

	WRITE (REPNAM,FMT='(A16,I4.4,I2.2,I2.2,A4)') 'FILE:UNPAID_OPS_', AA, MM, DD, '.REP'

	CALL ROPEN(REPNAM,REPLUN,ST)
	
	IF(ST.NE.0) TYPE*,'********ERROR OPENING REPORT FILE',ST
        

C++++++++++++++++++++++++++++++++++++++++++++++++++
C READ SORTED FILE AN WRITE TO REPORT
C
        PAGE=0
        LINCNT=0
	
5	CONTINUE

	   READ (TMPLUN2, 9001, END=600, IOSTAT=ST) RECREP
      	   IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Error Reading SORTED FILE', 
     *              0, 'STATUS = ', ST, ' ', 0)
      	      CALL GSTOP (GEXIT_FATAL)
      	   ENDIF    

        IF(MOD(LINCNT,60).EQ.0) CALL NEWPAGE(REPLUN,LINCNT,
     *        TITULO,PAGE,SEMANA,ANO)

	WRITE(REPLUN,9002)RECREP	      
        LINCNT=LINCNT+1
C
	GOTO 5

600     CLOSE(TMPLUN2)
	CLOSE(REPLUN)  !CLOSE REPORT FILE

C DELETE TEMPORARY FILES
        CALL DEL_REPFIL(TMPNAM1,TMPLUN1,ST)
        CALL DEL_REPFIL(TMPNAM2,TMPLUN2,ST)
C
      	CALL GSTOP (GEXIT_SUCCESS)
 9001   FORMAT(A132)
 9002   FORMAT(1X,A131)
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
	CHARACTER*3  TIPOPRZ, TIPO
	INTEGER*4    K
C
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

	WRITE(LUN,50)
     *                OPS_REC.AGENT(1:2), OPS_REC.AGENT(3:7),
     *                OPS_REC.BILHETE,
     *                OPS_REC.ORDER,
     *	              TIPO,
     *		      TIPOPRZ,
     *	              (OPS_REC.WINS(K),K=1,6),
     *	              CMONY(OPS_REC.TOTAL_GAME,11,VALUNIT),
     *	              OPS_REC.JOKER_DIV,
     *	              CMONY(OPS_REC.TOTAL_JOKER,11,VALUNIT),
     *	              OPS_REC.BANK,	
     *		      OPS_REC.BRANCH,
     *                OPS_REC.PAID_CDC

50	FORMAT(1X, A2,'.',A5, 4X, A14, 4X, A6, 3X, A3,
     *         4X, A3, 3X, 6I3, 3X, A11, 5X, 
     *         I2, 2X, A11, 4X, A4, 5X, A4, 4X, I4)

  
	RETURN
	END	


C       **************************************************************
        SUBROUTINE SORTREP(INPUTNAME,OUTPUTNAME,ST)
C       **************************************************************
        IMPLICIT NONE

        INTEGER*4  ISTATUS
        INTEGER*4  ST

        CHARACTER*30    INPUTNAME
        CHARACTER*30    OUTPUTNAME

        INTEGER*2       KEYBUF(9)

        INTEGER*4       SOR$PASS_FILES
        INTEGER*4       SOR$BEGIN_SORT
        INTEGER*4       SOR$SORT_MERGE
        INTEGER*4       SOR$END_SORT

        EXTERNAL        SS$_ENDOFFILE
        EXTERNAL        DSC$K_DTYPE_T
        EXTERNAL        SOR$GK_RECORD
        INTEGER*4       SRTTYPE


        KEYBUF(1) = 1                       !NUMBER OF KEYS
C
C       KEY 1 = AGENT NUMBER
C
        KEYBUF(2) = %LOC(DSC$K_DTYPE_T)
        KEYBUF(3) = 0                     !0 = ASCENDING / 1 = DESCENDING
        KEYBUF(4) = 1                     !OFFSET FOR THE KEY  AGENT NUMBER 
        KEYBUF(5) = 8                     !KEY SIZE 
C
C       KEY 2 = OPS_REC.BANK
C
CCCC        KEYBUF(6) = %LOC(DSC$K_DTYPE_T)
CCCC        KEYBUF(7) = 0                       !0 = ASCENDING / 1 = DESCENDING
CCCC        KEYBUF(8) = 107                     !OFFSET FOR THE KEY  (GAME) -> LOOK 
CCCC        KEYBUF(9) = 4                       !KEY SIZE 

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
      SUBROUTINE NEWPAGE(REPLUN,LINCNT,TITULO, PAGE, SEMANA, ANO)     
      IMPLICIT NONE
C
      INTEGER*4 REPLUN,LINCNT
      INTEGER*4 PAGE,SEMANA,ANO   
      CHARACTER*35 TITULO
   	
C
	  TITULO = 'RELATORIO DAS OPS NAO PAGAS' 
          PAGE=PAGE+1
          WRITE (REPLUN, 102) TITULO, PAGE, SEMANA, ANO

102             FORMAT(1X, 131('='), /, 
     *          1X,'SCML - Departamento de Jogos',T60, A35, T121, 'Pag.:',I4.4,/,
     *          1X,'Concurso : ', I3.3, '/', I4.4, /,
     *          1X, 130('='), /,
     *          1X ' MEDIADOR       BILHETE        ORDEM   TIPO    PRZ     1  2  3  4  5  6     '
     *             ' VAL.JOGO    JDV    VAL.JOKER   BANCO   BALCAO    PCDC',/,
     *          1X, 129('-'))

      LINCNT=LINCNT+6

      RETURN
      END
