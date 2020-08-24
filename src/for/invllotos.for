C INVLLOTOS.FOR                                                         
C
C V02 12-APR-2011 FJG ACCENTURE MERGE FOR EM2
C V01 04-JUN-2003 CMB INITIAL VERSION FOR SCML
C                                                                               
C PRINT REPORT WITH ALL AGENTS CANCELLED LOTO WITH LOT2 FLAG AND LOT2 WAGERS

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
C    
C Copyright 2003 SCML-DISI. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS    /CHECK=NOOVERFLOW
        SUBROUTINE INVLLOTOS(DDDD)
        IMPLICIT   NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:PRN_BUFF.DEF'
        INCLUDE 'INCLIB:RELCONC.DEF'
C
C LOCAL PARAMETERS
C
C        INTEGER*4 MAXPRTLN
C        PARAMETER (MAXPRTLN=59)
C
C LOCAL FUNCTIONS
C
C
        INTEGER*4       ST,SZ
        INTEGER*4       DDDD
        INTEGER*4       KAMT              
        INTEGER*4       AGT, PRVAGT 
        INTEGER*4       COUNT
        INTEGER*4       WEEK, YEAR 
	INTEGER*4	PAGE
	
        CHARACTER*46    L_REC
        CHARACTER*4     INTN  	   !INTERNAL DRAW NUMBER
        
        INTEGER*4    TMPTAB(8,MAXCAN,MAXGAM)
        INTEGER*4    REPLUN/8/    ! REPORT FILE UNIT

        INTEGER*4    TMPLUN/9/    ! TEMPORARY FILE AFTER SORT
        INTEGER*4    MSGLUN/6/    ! MESSAGE LUN
        CHARACTER*30 REPNAM       ! REPORT FILE NAME
        CHARACTER*59 REPHED       !REPORT TITLE
        CHARACTER*30 LOTOSNAM     ! LOTOS FILE NAME
        CHARACTER*30 TMPNAM       ! TEMP. FILE NAME AFTER SORT
        DATA TMPNAM/'TMPLOTOS.FIL'/


        CHARACTER*8  IAGT_NO    ! FUNCTION FORMATED AGENT NUMBER XX.XXXXX
        CHARACTER*2  CFLGLOT
        INTEGER*4  CDC, GAM, SER, FLGLTO,  DUR 
        INTEGER*4  AMT, JUL, SERIAL, CHK  
	INTEGER*2  DAT(12)
	
	INTEGER*4	TOTCAN, TOTJOK       
	INTEGER*4       TOTVALAMT           
	INTEGER*4       VALAMT               ! VALUE OF CANCEL (AMOUNT*DUR+KAMT) 
	INTEGER*4       TOTVAL               ! VALUE OF CANCEL PER AGENT(AMOUNT)
	INTEGER*4       TOTAMT, TOTKAMT      ! TOTAL AMOUNT PER AGT
	INTEGER*4       TOTCNT               !V02 TOTAL COUNT PER AGT 
	INTEGER*4	LINCNT

        CALL FASTSET(0, TMPTAB,   SIZEOF(TMPTAB)/4)

          IF (DDDD.LT.0 .OR. DDDD.GT.9999) THEN
           CALL DISPERR (MSGLUN, 'Concurso Invalido', 0, ' ', 0, ' ', 0)
           CALL GSTOP (GEXIT_FATAL)
          ENDIF
        WRITE(INTN,FMT='(I4.4)') DDDD

        TYPE*,' '
        TYPE*,'Aguarde... A Gerar Relatório INVLLOTOS'
        TYPE*,' '


C  CREATE NAME OF LOTOS FILE
C  --------------------------
        WRITE(LOTOSNAM,FMT='(A15,I4.4,A4)')
     *    'FILE:INVLLOTOS',DDDD,'.DAT'

        WRITE(REPNAM,FMT='(A15,I4.4,A4)')
     *    'FILE:INVLLOTOS_',DDDD,'.REP'

C   CREATE A SORTED FILE OF SAME NAME AND HIGHER VERSION FROM INPUT FILE  
C   ----------------------------------------------------------------------- 

        CALL SORTFIL (LOTOSNAM,TMPNAM,ST) 
          IF(ST.NE.0) THEN
             CALL DISPERR (MSGLUN, 'INVLLOTOS - Erro ao Ordenar INVLLOTOS.DAT',
     *               0, ' ', 0, ' ', 0)
             CALL GSTOP (GEXIT_FATAL)
	  ENDIF

C  OPEN TMP FIL SORTED FILE WITH DISPOSE = DELETE
C   ---------------------------------------------           

        CALL OPEN_LOTOS_DEL(TMPNAM,TMPLUN,ST)
          IF(ST.NE.0) THEN
             CALL DISPERR (MSGLUN, 'INVLLOTOS -ERRO ao abrir ficheiro temporario TMPLOTOS.FIL',
     *               0, ' ', 0, ' ', 0)
             CALL GSTOP (GEXIT_FATAL)
          ENDIF    

        
C OPEN REPORT FILE

       CALL ROPEN(REPNAM,REPLUN,ST)
	IF(ST.NE.0) THEN
           CALL DISPERR (MSGLUN, 'INVLLOTOS -Erro ',ST, ' na abertura do relatorio INVLLOTOS',
     *               0, ' ', 0, ' ', 0)
           CALL GSTOP (GEXIT_FATAL) 
        ENDIF     

	PRVAGT  = 0            
	TOTCNT  = 0            
	TOTAMT  = 0            
	TOTVAL  = 0            
	TOTKAMT  = 0 
	TOTVALAMT =0 
	LINCNT =0
	COUNT=0
  
     	  
    5 CONTINUE

       READ(TMPLUN,9000,END=300,IOSTAT=ST) L_REC
         IF (ST.NE.0) THEN
       	     CALL DISPERR (MSGLUN, 'INVLLOTOS -ERRO na leitura do ficheiro temporario TMPLOTOS.FIL',
     *               0, ' ', 0, ' ', 0)
             CALL GSTOP (GEXIT_FATAL)
         ENDIF 


C DECODE
        AGT     = CTOI(L_REC(1:7),SZ)
        GAM     = CTOI(L_REC(8:9),SZ)
        CDC     = CTOI(L_REC(10:13),SZ)
        SER     = CTOI(L_REC(14:21),SZ)
        FLGLTO  = CTOI(L_REC(22:23),SZ)
        DUR     = CTOI(L_REC(24:25),SZ)
        AMT     = CTOI(L_REC(26:35),SZ)
        KAMT   =  CTOI(L_REC(36:45),SZ) 
        
        
C REPORT
c-------

  	  CALL FIGWEK(CDC, WEEK, YEAR)
  	         	      	
     	 DAT(VCDC) = CDC
 	 CALL CDATE(DAT)
	 JUL = DAT(VJUL)


	  CALL OUTGEN (CDC,SER,SERIAL,CHK)
	
	
	IF (FLGLTO.GT.0) THEN
		CFLGLOT='**'
	ELSE
		CFLGLOT=' '
	ENDIF	
	
	IF (LINCNT.EQ.0 .OR.(LINCNT+2).GT.MAXPRTLN) THEN
          WRITE (REPHED,9201) WEEK, YEAR
	  CALL TITLE (REPHED,REPNAM(6:15), 1, REPLUN, PAGE, DAYCDC)
	  WRITE (REPLUN,9202)
	  LINCNT = 8
	ENDIF    
		
	VALAMT = DUR*(AMT + KAMT)                
	TOTCAN = TOTCAN + AMT 
	TOTJOK = TOTJOK + KAMT                    
	TOTVALAMT = TOTVALAMT + VALAMT       
	COUNT= COUNT+1      
            
	 IF (AGT .EQ. PRVAGT) THEN               
	 
	WRITE (REPLUN,9003) IAGT_NO(AGT), GAM, DAT(VDAY), DAT(VMON),
     *             DAT(VYEAR), SER, JUL, SERIAL, 
     *             CHK, CFLGLOT, WEEK, YEAR, DUR,
     *             CMONY(AMT,10,BETUNIT), CMONY(KAMT,10,BETUNIT), CMONY(VALAMT,10,BETUNIT)
	 

     
        TOTCNT = TOTCNT + 1          
        TOTAMT = TOTAMT + AMT        
	TOTVAL = TOTVAL + VALAMT     
	TOTKAMT = TOTKAMT + KAMT      
	LINCNT = LINCNT + 1
	
		 ELSE
	
C WRITE TOTAL'S AND BEGIN VARIABLES	
	
	     IF (TOTCNT .NE. 0) THEN	
	           WRITE(REPLUN,102) TOTCNT, CMONY(TOTAMT,12,BETUNIT),CMONY(TOTKAMT,12,BETUNIT),
     *                     CMONY(TOTVAL, 12, BETUNIT)                   
	     ENDIF

	  WRITE (REPLUN,9003) IAGT_NO(AGT), GAM,
     *	           DAT(VDAY),DAT(VMON),DAT(VYEAR),
     *              SER, JUL, SERIAL, CHK, 
     *             CFLGLOT, WEEK, YEAR, DUR,
     *             CMONY(AMT,10,BETUNIT),CMONY(KAMT,10,BETUNIT),CMONY(VALAMT,10,BETUNIT)

	  TOTCNT  = 1                              
	  TOTAMT  = AMT                            
	  TOTKAMT = KAMT
	  PRVAGT  = AGT                            
          TOTVAL  = VALAMT                         

	  LINCNT = LINCNT + 4                    ! TOTALS AND NEW LINE AGT

	 ENDIF 
	 GO TO 5              
	
300	CONTINUE

	 IF (TOTCNT .NE. 0) THEN 

           WRITE(REPLUN,102) TOTCNT, CMONY(TOTAMT, 12, BETUNIT), CMONY(TOTKAMT, 12, BETUNIT),
     *           CMONY(TOTVAL, 12, BETUNIT)                        

	 ENDIF
C
C OVER AND OUT
C
	WRITE (REPLUN,9204) COUNT, CMONY (TOTCAN, 12, BETUNIT),GSNAMES(GAM),
     *         CMONY (TOTJOK, 12, BETUNIT), CMONY (TOTVALAMT, 12, BETUNIT)


      CLOSE (TMPLUN) 
      CLOSE(REPLUN) 
      
Cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx      
102     FORMAT(1X,131('-')/46X,'TOTAL', 17X, I4, 12X, A12,2X,A12,1X,A12/)
C9000   FORMAT(I7.7,I2.2,I4.4,I8.8,I2.2,I2.2,I8.8)
9000   FORMAT(A45)
909    FORMAT(A8,A4,A6,A4,A10 )
9001   FORMAT(1X,A8,3X,4A4)
9002   FORMAT(1X,A8,2X,I2,2X,I2.2,'-',I2.2,'-',I2.2,3X,I4,
     *         5X,I3.3,'-',I8.8,'-',I3.3,A2,8X,I2,'/',I4,7X,I4,5X,A10,5X,A10)
9003	FORMAT(2X,A8,3X,I2.2,3X,I2.2,'-',I2.2,'-',I2.2,3X,I10,
     *         5X,I3.3,'-',I8.8,'-',I3.3,A2,5X,I2,'/',I4,4X,I4,3X,A10,4X,A10,4X,A10)
     
9004    FORMAT(13X,I2.2,3X,I2.2,'-',I2.2,'-',I2.2,3X,I10,
     *         5X,I3.3,'-',I8.8,'-',I3.3,A2,5X,I2,'/',I4,4X,I4,3X,A10,4X,A10,4X,A10) 
     
9201	FORMAT('          CANC. LOTO C/FLAG E LOTO2 CONCURSO ',I2.2,I4.4)      
     
9202	FORMAT(1X,131('-'),/,
     *         1X, 'MEDIADOR', 4X, 'JOGO',3X, 'DATA', 6X, ' SERIAL ',
     *         6X,'    RECIBO      ',5X, 'CONCURSO',5X,
     *         ' QTDE ',5X,' VALOR ',7X,' JOKER',7X,'VALOR'/,
     *         66X,'INICIAL    CONCURSOS',29X,'BILHETE',/,
     *         1X,131('-'))

9204	FORMAT(1X,//,20X,I8,' CANCELADOS TOTALIZAM ',A12, ' EUROS EM APOSTAS DE ',A4,/,
     *               28X,'            TOTALIZAM ',A12, ' EUROS COM JOKER',/,
     *               28X,'            TOTALIZAM ',A12, ' EUROS')        
	END
C
C       **************************************************************
        SUBROUTINE SORTFIL(INPUTNAME,OUTPUTNAME,ST)
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
        
C       KEY 1 = AGENT NUMBER

        KEYBUF(2) = %LOC(DSC$K_DTYPE_T)
        KEYBUF(3) = 0                     !0 = ASCENDING / 1 = DESCENDING
        KEYBUF(4) = 0                     !OFFSET FOR THE KEY  AGENT NUMBER 
        KEYBUF(5) = 7                     !KEY SIZE C

C       KEY 2 = GAM

       KEYBUF(6) = %LOC(DSC$K_DTYPE_T)
       KEYBUF(7) = 0                       !0 = ASCENDING / 1 = DESCENDING
       KEYBUF(8) = 8                      !OFFSET FOR THE KEY  (GAME) -> LOOK 
       KEYBUF(9) = 2                       !KEY SIZE 

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
      SUBROUTINE OPEN_LOTOS_DEL(FILNAM,LUN,ST)
      IMPLICIT NONE
C
      CHARACTER*(*) FILNAM
      INTEGER*4 ST,LUN

     
                OPEN (UNIT           = LUN,
     *           FILE           =  FILNAM,
     *           STATUS         = 'OLD',
C     *           ORGANIZATION   = 'SEQUENTIAL',
C     *           FORM           = 'UNFORMATTED',
     *           DISPOSE        = 'DELETE',
     *           IOSTAT         =  ST)

      RETURN
      END
