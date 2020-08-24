C
C EMITED_OPS.FOR                                                    
C
C V03 16-MAR-2011 FRP Request week draw for each game
C V02 27-DEC-2010 FRP Lotto2 Changes
C V01 03-APR-2003 CPH INITIAL VERSION FOR SCML
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                   
C REPORT EMITED OPS FROM OP FILE BY BANK AND AGENT
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                                               
C                                                                               
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
        PROGRAM EMITED_OPS
        IMPLICIT NONE                                                  
	                                                
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:OPS_REC.DEF'
	INCLUDE 'INCLIB:BANK_REC.DEF'
C	INCLUDE 'INCLIB:BRANCH_REC.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'

                                                                               
        INTEGER*4   MSG_LUN /6/
        INTEGER*4   ANOSEMANA, ANO, SEMANA
	CHARACTER   YEARWEEK*7
	INTEGER*4   ST, SZ
	INTEGER*4   PAGE, LINCNT
        INTEGER*4   YESNO
	CHARACTER   TITULO*35
	INTEGER*4   BANK, PRVBANK, BANKPOS, PRVBKPOS
	INTEGER*4   AGT, PRVAGT, TERM  ! AGT, PREVIOUS AGT
	CHARACTER*38 BNAME, PRVBKNAM
	INTEGER*4   VAL, JOK           !GAME AND JOKER PRICE AMOUNT 
        INTEGER*4   TOTOP_CNT, TOTOP_AMT, TOTOP_JKR  !TOTAL AMONT  
        INTEGER*4   BNK_TOTOP_CNT, BNK_TOTOP_AMT, BNK_TOTOP_JKR !BANK TOTAL  
        INTEGER*4   GRDBNK_TOTOP_CNT, GRDBNK_TOTOP_AMT, GRDBNK_TOTOP_JKR 
        INTEGER*4   CNT

	INTEGER*4  TMPLUN1/7/   !TEMPORARY FILE BEFOR SORT (REPORT)
	INTEGER*4  TMPLUN2/8/   !TEMPORARY FILE AFTER SORT (REPORT)
        INTEGER*4  REPLUN/9/    !REPORT (FINAL REPORT)

        CHARACTER*30 REPNAM        ! REPORT FILE NAME  (FINAL)
        CHARACTER*30 TMPNAM1       ! TEMP. FILE NAME  (BEF. SORT)
        CHARACTER*30 TMPNAM2       ! TEMP. FILE NAME  (AFTER SORT)
        DATA TMPNAM1/'TMPEMTOPS1.REP'/
        DATA TMPNAM2/'TMPEMTOPS2.REP'/

	CHARACTER*132 RECREP

	INTEGER*2 DATE(LDATE_LEN)
        INTEGER*4 AA,MM,DD, GAM
        INTEGER*4 GAME_SEL(MAXGAM)
        INTEGER*4 AAAA(MAXGAM),CCC(MAXGAM)
        CHARACTER AAAACCC(MAXGAM)*7

	LOGICAL   FIRST/.TRUE./


C        CALL COPYRITE                                                             
  	                                                                         
        TYPE*,IAM(),' '
        TYPE*,IAM(),'-----------------------------------------------------------'   
        TYPE*,IAM(),'<<<<<             EMITED OPS BY BANK                 >>>>>'   
        TYPE*,IAM(),'<<<<<      GENERATES EMITED_OPS_aaaammdd.REP         >>>>>'
	TYPE*,IAM(),'<<<<<                                                >>>>>'
	TYPE*,IAM(),'<<<<<            aaaammdd  =  data geracao           >>>>>'
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
        CALL PRMYESNO('Is this CORRECT: (Y/N) ? ', YESNO)
        IF (YESNO.NE.1) STOP
C
	TYPE*,' '
	TYPE*,'WAIT.....GENERATING REPORT...'
	TYPE*,' '

C++++++++++++++++++++++++++++++++++++++++++
C	LOAD BANK TABLE STRUCTURE
C	-------------------------
	CALL LOAD_BANK_TABLE(ST)
        IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error loading BANK TABLE', 0, 'STATUS = ', ST, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
        ENDIF

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
	
	   BANK = CTOI(OPS_REC.BANK,SZ)
	   IF (BANK.LE.0) THEN
C	      CALL DISPERR (MSG_LUN,'Invalid BANK in OP =',OPS_REC.ORDER,
C     *			    0, ' ', 0, ' ', 0)
	      GOTO 100 
	   ENDIF

C          GET POSITION FOR BANK IN BANK_TAB
C       
           CALL GET_BANK_POS (BANK, BANKPOS)
           IF (BANKPOS.LE.0) THEN
           CALL DISPERR (MSG_LUN, 'Invalid BANKPOS= ', BANKPOS, 
     *                  'Using BANK = '//BANK_TAB(1).BANK,
     *                      0, ' ', 0)
              BANKPOS = 1           ! SEND TO BANK 1
           ENDIF   

           BNAME=BANK_TAB(BANKPOS).LONG_NAME

           WRITE(YEARWEEK,FMT='(I7.7)') ANOSEMANA
	   IF (YEARWEEK .NE. OPS_REC.YEARWEEK) GOTO 100	!IT'S NOT THE WEEK I WANT TO LIST

C	   WRITE IT TO FIRST TEMPORARY FILE
C	   -----------------------------------
	   CALL WRITE_OP_LINE(TMPLUN1,BNAME)

100	CONTINUE
	
C	END OF OP READING

300     CLOSE(OPS_LUN)
	CLOSE(TMPLUN1)

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C	SORT DATA BY BANK AND AGENT NUMBER
C       INPUT: 1ST TMP REPORT, OUTPUT: 2ND TMP REPORT

        CALL SORTREP(TMPNAM1,TMPNAM2,ST)


C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       OPEN TEMP SORTED FILE 
C       ------------------------           

        CALL OPEN_REPFIL(TMPNAM2,TMPLUN2,ST)
        IF (ST.NE.0) THEN
             TYPE*,'****** ERROR OPEN TMP SORTED REP'
             CALL GSTOP (GEXIT_FATAL)
        ENDIF    

C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      OPEN REPORT FILE
C     ------------------
        DATE(VCDC) = DAYCDC
        CALL LCDATE(DATE)
        DD = DATE(VDAY)
        MM = DATE(VMON)
        AA = DATE(VYEAR) + 2000

	WRITE (REPNAM,FMT='(A17,I4.4,I2.2,I2.2,A4)') 'FILE:EMITED_OPS_', AA, MM, DD, '.REP'

	CALL ROPEN(REPNAM,REPLUN,ST)
	
	IF(ST.NE.0) TYPE*,'********ERROR OPENING REPORT FILE',ST

C++++++++++++++++++++++++++++++++++++++++++++++++++
C OPEN AGENT SALES FILE 

        CALL OPENASF(ASF)
        
C++++++++++++++++++++++++++++++++++++++++++++++++++
C READ SORTED FILE AND WRITE TO REPORT
C
        PAGE      = 0
        LINCNT    = 70
	PRVAGT    = 0
	TOTOP_CNT = 0
	TOTOP_AMT = 0
	TOTOP_JKR = 0
	BNK_TOTOP_CNT = 0
	BNK_TOTOP_AMT = 0
	BNK_TOTOP_JKR = 0
	GRDBNK_TOTOP_CNT = 0
	GRDBNK_TOTOP_AMT = 0
	GRDBNK_TOTOP_JKR = 0
	PRVBANK   = 0
	PRVBKPOS  = 0
	PRVBKNAM  = '    '

10	CONTINUE

	READ (TMPLUN2, 9001, END=600, IOSTAT=ST) RECREP 
	IF (ST.NE.0) THEN
	  CALL DISPERR (MSG_LUN, 'Error Reading SORTED FILE', 
     *              0, 'STATUS = ', ST, ' ', 0)
      	  CALL GSTOP (GEXIT_FATAL)
      	ENDIF    

	AGT   = CTOI(RECREP(2:8),SZ)
	VAL   = CTOI(RECREP(101:111),SZ)
	JOK   = CTOI(RECREP(113:123),SZ)
	BANK  = CTOI(RECREP(57:60),SZ)
	BNAME = RECREP(62:99)
	
C+++++++++++++++++++++++++++++++++++++++
C FIND TERMINAL NUMBER AND AGENT NAME

	CALL FIND_AGENT(AGT,TERM,ST)
	IF (ST.NE.0) THEN
	  CALL GSTOP (GEXIT_FATAL)
        ENDIF 

	CALL READASF (TERM, ASFREC, ST)
        IF (ST.NE.0) THEN
          CALL DISPERR (MSG_LUN, 'Error reading ASF file', 0, 'REC = ', 
     *			TERM, ' ', 0)
          CALL GSTOP (GEXIT_FATAL)
        ENDIF              

C++++++++++++++++++++++++++++++++++++++++++++++++++++
C CALCULATE TOTALS AND WRITE DETAIL
	
        IF (AGT .EQ. PRVAGT) THEN     ! AGENT 

   	   TOTOP_CNT = TOTOP_CNT + 1
           TOTOP_AMT = TOTOP_AMT + VAL
	   TOTOP_JKR = TOTOP_JKR + JOK
   	   BNK_TOTOP_CNT = BNK_TOTOP_CNT + 1
           BNK_TOTOP_AMT = BNK_TOTOP_AMT + VAL
	   BNK_TOTOP_JKR = BNK_TOTOP_JKR + JOK
   	   GRDBNK_TOTOP_CNT = GRDBNK_TOTOP_CNT + 1
           GRDBNK_TOTOP_AMT = GRDBNK_TOTOP_AMT + VAL
	   GRDBNK_TOTOP_JKR = GRDBNK_TOTOP_JKR + JOK

	   WRITE(REPLUN,150)
     *			,RECREP(11:24)	      !BILHETE
     *			,RECREP(26:31)        !ORDEM
     *			,RECREP(33:43)	      !VALOR JOGO
     *			,RECREP(45:55)	      !VALOR JOKER	 	      
           LINCNT=LINCNT+1

        ELSE

C          WRITE TOTALS
C  	   START WITH A NEW AGENT 
C++++++++++++++++++++++++++++++++++++++++++++++++
C WRITE TOTALS

      	   IF (TOTOP_CNT .NE. 0) THEN
	
		WRITE (REPLUN,101),TOTOP_CNT
     *			       ,CMONY(TOTOP_AMT,11,VALUNIT)
     *			       ,CMONY(TOTOP_JKR,11,VALUNIT)

      	   ENDIF

101	   FORMAT(1X,33X,50('-'),/,2X,'TOTAIS: ',
     *         29X,I4, 8X,A11, 6X,A11,/)

           LINCNT=LINCNT+3

           IF (LINCNT.GE.50.OR.BANK.NE.PRVBANK) THEN
	      IF(FIRST.OR.BANK .NE. PRVBANK) THEN
	        IF(BNK_TOTOP_CNT .NE. 0) THEN
	          WRITE(REPLUN, 5101) BNK_TOTOP_CNT,
     *                        CMONY(BNK_TOTOP_AMT,11,VALUNIT),
     *                        CMONY(BNK_TOTOP_JKR,11,VALUNIT)
	        ENDIF

   	        BNK_TOTOP_CNT = 1
                BNK_TOTOP_AMT = VAL
	        BNK_TOTOP_JKR = JOK
   	        GRDBNK_TOTOP_CNT = GRDBNK_TOTOP_CNT + 1
                GRDBNK_TOTOP_AMT = GRDBNK_TOTOP_AMT + VAL
	        GRDBNK_TOTOP_JKR = GRDBNK_TOTOP_JKR + JOK

5101	        FORMAT(1X,33X,50('-'),/,2X,'TOTAL BANCO: ',
     *               23X,I5, 8X,A11, 6X,A11,/)

	        LINCNT    = LINCNT + 3
	        FIRST=.FALSE.

              ENDIF

              CALL NEWPAGE(REPLUN,
     *	                   LINCNT,TITULO,PAGE, SEMANA, ANO,BNAME)
	   ENDIF

C
C WRITE AGENT HEADER AND AGENT DATA

	   WRITE(REPLUN,102) RECREP(2:3), RECREP(4:8)
     *                  , (ASFBYT(CNT), CNT = SNAME, SNAME+LNAME-1) 

102        FORMAT(1X, 131('='), /,
     *          1X,'   AGENTE            BILHETE          ORDEM   '
     *             '       VAL.JOGO          VAL.JOKER ', /,
     *          1X, 131('-'),/,
     *	        2X, A2,'-',A5, 2X,<LNAME>A1,/)

	   TOTOP_CNT = 1
      	   TOTOP_AMT = VAL    
	   TOTOP_JKR = JOK
	   IF(BANK.EQ.PRVBANK) THEN
   	     BNK_TOTOP_CNT = BNK_TOTOP_CNT + 1
             BNK_TOTOP_AMT = BNK_TOTOP_AMT + VAL
	     BNK_TOTOP_JKR = BNK_TOTOP_JKR + JOK
   	     GRDBNK_TOTOP_CNT = GRDBNK_TOTOP_CNT + 1
             GRDBNK_TOTOP_AMT = GRDBNK_TOTOP_AMT + VAL
	     GRDBNK_TOTOP_JKR = GRDBNK_TOTOP_JKR + JOK
	   ENDIF

	   WRITE(REPLUN,150)
     *			         ,RECREP(11:24)	      !BILHETE
     *				 ,RECREP(26:31)       !ORDEM
     *				 ,RECREP(33:43)	      !VALOR JOGO
     *				 ,RECREP(45:55)	      !VALOR JOKER    
	
C
150	   FORMAT(7X, 8X ,4X, A14, 6X, A6, 6X, A11, 6X, A11)

	   LINCNT    = LINCNT + 6
	   PRVBANK   = BANK
	   PRVBKPOS  = BANKPOS	
	   PRVBKNAM  = BNAME
	   PRVAGT    = AGT

	ENDIF  !AGENT
C
	GOTO 10

	CALL CLOSASF() 
600     CLOSE(TMPLUN2)

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C WRITE  LAST TOTALS
 	
	WRITE (REPLUN,101),TOTOP_CNT
     *			  ,CMONY(TOTOP_AMT,11,VALUNIT)
     *			  ,CMONY(TOTOP_JKR,11,VALUNIT)

	WRITE (REPLUN,5101),BNK_TOTOP_CNT
     *			  ,CMONY(BNK_TOTOP_AMT,11,VALUNIT)
     *			  ,CMONY(BNK_TOTOP_JKR,11,VALUNIT)

	WRITE (REPLUN,5102),GRDBNK_TOTOP_CNT
     *			  ,CMONY(GRDBNK_TOTOP_AMT,12,VALUNIT)
     *			  ,CMONY(GRDBNK_TOTOP_JKR,12,VALUNIT)

5102	  FORMAT(1X,33X,50('-'),/,2X,'TOTAL GERAL: ',
     *         22X,I6, 7X,A12, 5X,A12,/)

	CLOSE(REPLUN)  !CLOSE REPORT FILE

C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C DELETE TEMPORARY FILES
        CALL DEL_REPFIL(TMPNAM1,TMPLUN1,ST)
        CALL DEL_REPFIL(TMPNAM2,TMPLUN2,ST)

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      	CALL GSTOP (GEXIT_SUCCESS)
 9001   FORMAT(A132)

      	END


C	*************************************
	SUBROUTINE WRITE_OP_LINE (LUN,BNAME)
C	*************************************
	IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:OPS_REC.DEF'

	INTEGER*4    LUN
C	INTEGER*4    K
	CHARACTER*38 BNAME
C

	WRITE(LUN,50) 
     *                ,OPS_REC.AGENT
     *                ,OPS_REC.BILHETE
     *                ,OPS_REC.ORDER
     *	              ,CMONY(OPS_REC.TOTAL_GAME,11,VALUNIT)
     *	              ,CMONY(OPS_REC.TOTAL_JOKER,11,VALUNIT)
     *                ,OPS_REC.BANK    
     *		      ,BNAME
     *                ,OPS_REC.TOTAL_GAME
     *		      ,OPS_REC.TOTAL_JOKER

50	FORMAT(1X,A7,2X,A14,1X,A6,1X,A11,1X, A11,1X,A4,1X,A38,1X,I11,1X,I11)

  
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


        KEYBUF(1) = 2                       !NUMBER OF KEYS
C
C       KEY 1 = BANK NUMBER
C
        KEYBUF(2) = %LOC(DSC$K_DTYPE_T)
        KEYBUF(3) = 0                     !0 = ASCENDING / 1 = DESCENDING
        KEYBUF(4) = 56                    !OFFSET FOR THE KEY  BANK NUMBER 
        KEYBUF(5) = 4                     !KEY SIZE 
C
C       KEY 2 = AGENT NUMBER
C
        KEYBUF(6) = %LOC(DSC$K_DTYPE_T)
        KEYBUF(7) = 0                   !0 = ASCENDING / 1 = DESCENDING
        KEYBUF(8) = 1                   !OFFSET FOR THE KEY AGENT NUMBER 
        KEYBUF(9) = 7                   !KEY SIZE 

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
C  *************************************************
      SUBROUTINE OPEN_REPFIL(FILNAM,LUN,ST)
C  *************************************************
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

C  *********************************************
      SUBROUTINE DEL_REPFIL(FILNAM,LUN,ST)
C  *********************************************
      CHARACTER*(*) FILNAM
      INTEGER*4 ST,LUN
C
      OPEN (UNIT    =  LUN,
     *      FILE    =  FILNAM,
     *      STATUS  =  'OLD',
     *      FORM    =  'FORMATTED',
     *      DISPOSE =  'DELETE',
     *      IOSTAT  =  ST)

      RETURN
      END

C  ******************************************************************
      SUBROUTINE NEWPAGE(REPLUN,LINCNT,TITULO,PAGE,SEMANA,ANO,BNAME) 
C  ******************************************************************    
      IMPLICIT NONE
C
      INTEGER*4 REPLUN,LINCNT
      INTEGER*4 PAGE,SEMANA,ANO   
      CHARACTER*35 TITULO
      CHARACTER*38 BNAME	

	  TITULO = 'RELATORIO DE OPS EMITIDAS' 
	  LINCNT=0
          PAGE=PAGE+1

          WRITE (REPLUN, 130) TITULO, PAGE, SEMANA, ANO, BNAME

130             FORMAT('1', 131('='), /, 
     *          1X,'SCML - Departamento de Jogos',T60, A35, T121, 'Pag.:',I4.4,/,
     *          1X,'Concurso : ', I3.3, '/', I4.4, /,
     *		1X,'BANCO    : ',A38 )

      LINCNT=LINCNT+4
      RETURN
      END
