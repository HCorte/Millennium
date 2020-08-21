C
C GET_OPS_PAID.FOR                                                                    
C
C V02 28-JAN-2011 FRP Lotto2 Changes
C V01 24-JAN-2001 EPH INITIAL VERSION FOR SCML
C                                                                               
C READ FILE WITH OP PAYMENT INFORMATION (FROM THE BANK) AND
C UPDATE OP FILE                                                                               
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
      PROGRAM GET_OPS_PAID      
      IMPLICIT NONE                                                  
                                                
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF' 
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:RECSCF.DEF'
      INCLUDE 'INCLIB:DATBUF.DEF'
      INCLUDE 'INCLIB:OPS_REC.DEF'
      INCLUDE 'INCLIB:DLTREC.DEF'
      INCLUDE 'INCLIB:DSPREC.DEF'
      INCLUDE 'INCLIB:DBNREC.DEF'
      INCLUDE 'INCLIB:AGTCOM.DEF'
      INCLUDE 'INCLIB:WINCOM.DEF'
                                                                               
      INTEGER*4   INP_LUN /TMP1_LUN/
      INTEGER*4   REP_LUN /TMP2_LUN/      

      INTEGER*4   REC_NBR   

      INTEGER*4   ST, SZ, OPST

      INTEGER*4   WEEK
      INTEGER*4   YEAR

      INTEGER*4   FILNAME(7)            
      CHARACTER   CFILNAME(28)            
      EQUIVALENCE (CFILNAME,FILNAME)

      CHARACTER   CHAVE*15
      CHARACTER   SCMLREC * 255
      CHARACTER   REFERENCIA * 2

      LOGICAL     ERRO
      LOGICAL     FOUND_TRAILLER
     
      INTEGER*4   GAM, CCC
      CHARACTER   DATAHD * 10

      INTEGER*4   WEKCCC   !FUNCTION

      CALL COPYRITE                                                             
  	                                                                         
      TYPE*,IAM(),' '
      TYPE *,'-------------------------------------------------------------------------------'   
      TYPE *,'<<<<< RECEBE E REGISTRA INFORMACAO DE PAGAMENTO DE OPS ENVIADA PELO BANCO >>>>>'   
      TYPE *,'-------------------------------------------------------------------------------'
      TYPE*,IAM(),' '

C
C     OPEN ORDENS DE PAGAMENTO FILE
C     *****************************
      CALL OPEN_OPS('KEYED',ST)
      IF (ST.NE.0) THEN
         TYPE*,IAM(),' '
         TYPE*,IAM(),'=================================================='
         TYPE*,IAM(),'>> ERRO DE ABERTURA NO ARQUIVO  ** FILE:OPS.FIL **'	
         TYPE*,IAM(),'=================================================='
         TYPE*,IAM(),' '
         CALL GSTOP (GEXIT_FATAL)
      ENDIF             

C
C     OPEN REPORT FILE
C     ****************
      OPEN (UNIT   = REP_LUN,
     *      FILE   = 'GET_OPS_PAID.REP',
     *      IOSTAT = ST,
     *      STATUS = 'NEW') 
      IF (ST.NE.0) THEN
         TYPE*,IAM(),' '
         TYPE*,IAM(),'======================================================='
         TYPE*,IAM(),'>> ERRO DE ABERTURA DO RELATORIO ** GET_OPS_PAID.REP **'	
         TYPE*,IAM(),'======================================================='
         TYPE*,IAM(),' '
         CALL GSTOP (GEXIT_FATAL)
      ENDIF
C
C     PRINT REPORT HEADER
C
      WRITE(REP_LUN,13) 
13    FORMAT(1X, 'SEMANA/ANO  JOGO  ORDEM  NRO BILHETE', 30X, 'STATUS')

C                                                                               
C     GET AND OPEN INPUT FILE
C     *********************** 
120   CONTINUE              
      CALL WIMG(5,'Enter file name (VOLN:FILNAME)        ')
      READ(5,901) FILNAME
901   FORMAT(7A4)   
      IF (FILNAME(1).EQ.'    ') GOTO 120
      IF (CFILNAME(1).EQ.'E '.OR.CFILNAME(1).EQ.'e ') THEN
         CALL GSTOP(GEXIT_OPABORT)
      ENDIF

      OPEN (UNIT   = INP_LUN,
     *      FILE   = FILNAME,
     *      IOSTAT = ST,
     *      STATUS = 'OLD') 
      IF (ST.NE.0) THEN
         TYPE*,IAM(),CFILNAME,' Open error,  status =',ST
         GOTO 120
      ENDIF
    
C
C     VALIDATE INTEGRITY OF INPUT FILE 
C     ********************************
      CALL CHECK_OPS_PAID (INP_LUN, ST)
      IF (ST.NE.0) THEN
         TYPE*,IAM(),' '
         TYPE*,IAM(),'==============================================================='
         TYPE*,IAM(),'>> HOUVE ERRO NA CONSISTENCIA DO ARQUIVO DE PAGAMENTO DE ORDENS'
         TYPE*,IAM(),'==============================================================='
         TYPE*,IAM(),' '
         CALL GSTOP (GEXIT_FATAL)
      ENDIF
C
C     GOT TO BEGINNING OF INPUT FILE AGAIN
C
      CLOSE (INP_LUN)

      OPEN (UNIT   = INP_LUN,
     *      FILE   = FILNAME,
     *      IOSTAT = ST,
     *      STATUS = 'OLD') 
      IF (ST.NE.0) THEN
         TYPE*,IAM(),CFILNAME,' Open error,  status =',ST
         CALL GSTOP (GEXIT_FATAL)
      ENDIF

C
C     READ HEADER (bank file)
C     ***********************
      READ (INP_LUN, 6001, IOSTAT=ST) SCMLREC
       IF (ST.NE.0) THEN
          TYPE*,IAM(),' '
          TYPE*,IAM(),'============================================'
	  TYPE*,IAM(),' >> ERRO NA LEITURA DO ARQUIVO DE INTERFACE '
          TYPE*,IAM(),'============================================'
          CALL GSTOP(GEXIT_FATAL)
       ENDIF
       
       DATAHD = SCMLREC(34:41)    
       TYPE*,IAM(),' '
       TYPE*,IAM(),'--------------------------------'
       TYPE*,IAM(),' >> DATA NO HEADER : ',DATAHD
       TYPE*,IAM(),'--------------------------------'
       TYPE*,IAM(),' '
C
C     LOOP READING INPUT FILE
C     ***********************                                                                              
C   
      ERRO           = .FALSE.
      FOUND_TRAILLER = .FALSE.
      REC_NBR = 0

      DO 555 WHILE (ST.NE.144)

         READ (INP_LUN, 6001, IOSTAT=ST) SCMLREC
6001     FORMAT(A500)
         IF (ST.EQ.144) GOTO 555
         IF (ST.NE.0) THEN
            TYPE*,IAM(),' '
            TYPE*,IAM(),'============================================'
	    TYPE*,IAM(),' >> ERRO NA LEITURA DO ARQUIVO DE INTERFACE '
            TYPE*,IAM(),'============================================'
            CALL GSTOP(GEXIT_FATAL)
         ENDIF

         REFERENCIA = SCMLREC(1:4)

         IF (REFERENCIA.EQ.'ODJ9') THEN    !TRAILLER
            ST = 144
            GOTO 555            
         ENDIF

         REC_NBR = REC_NBR + 1

         WEEK = CTOI(SCMLREC(75:76),SZ)
         YEAR = CTOI(SCMLREC(77:78),SZ) + 2000

         GAM  = CTOI(SCMLREC(79:80),SZ)
         IF(GAM.GE.9) GAM = GAM + 1     !SCML USES THIS SHIFT FOR GAME NUMBER

         CCC = WEEK
         IF(GAM.EQ.6 .OR. GAM.EQ.7) CCC = WEKCCC(YEAR,WEEK,GAM)

         WRITE(CHAVE,FMT='(I2.2,I4.4,I3.3,I6.6)') GAM,YEAR,CCC,CTOI(SCMLREC(48:53), SZ)  
C
C	 LOCATE THIS ORDER IN OP FILE
C
	 READ (OPS_LUN, KEYID=0, KEYEQ=CHAVE, IOSTAT=OPST) OPS_REC
         IF (OPST.NE.0) THEN
C      	    WRITE (REP_LUN,17) OPS_REC.YEARWEEK, OPS_REC.GAME, OPS_REC.ORDER, OPS_REC.BILHETE
C17          FORMAT()
            GOTO 555            
         ENDIF

	 OPS_REC.PAID_CDC = DAYCDC 
C
C        WRITE PAYMENT BACK TO ORDERS FILE
C
         WRITE(OPS_LUN, IOSTAT=OPST) OPS_REC
         IF (OPST.NE.0) THEN
            TYPE*,IAM(),' '
            TYPE*,IAM(),'====================================================='
            TYPE*,IAM(),'>> ERRO AO ESCREVER NO ARQUIVO DE ORDENS DE PAGAMENTO'
            TYPE*,IAM(),'   GAME     = ', OPS_REC.GAME
            TYPE*,IAM(),'   AGENTE   = ', OPS_REC.AGENT
            TYPE*,IAM(),'   ANO/MES  = ', OPS_REC.YEARWEEK 
            TYPE*,IAM(),'   ORDEM    = ', OPS_REC.ORDER
            TYPE*,IAM(),'====================================================='
            TYPE*,IAM(),' '
            CALL GSTOP (GEXIT_FATAL)
         ENDIF

555   CONTINUE

C
C     END OF INPUT FILE  (FINISH)
C     ***************************

      CLOSE(INP_LUN)  
      CLOSE(OPS_LUN)
      CLOSE(REP_LUN)

      TYPE*,IAM(),' '
      TYPE*,IAM(),'------------------------------------------------------'
      TYPE*,IAM(),'>> NUMERO DE REGISTROS PROCESSADOS = ', REC_NBR
      TYPE*,IAM(),'------------------------------------------------------'
      TYPE*,IAM(),' '


      CALL GSTOP(GEXIT_SUCCESS)                          

      END   



C	*******************************************
	SUBROUTINE CHECK_OPS_PAID (INP_LUN, STATUS)
C	*******************************************
	IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'

   	INTEGER*4   INP_LUN 
    	INTEGER*4   REC_NBR   

   	INTEGER*4   ST, SZ

  	INTEGER*4   WEEK
  	INTEGER*4   YEAR

        CHARACTER   SCMLREC * 255

        LOGICAL     FOUND_TRAILLER
     
        INTEGER*4   REC_NBR_TRAILLER
        INTEGER*4   DRAW, GAMEON

	INTEGER*4   STATUS

	INTEGER*4   GETDRW     !FUNCTION

	STATUS  = 0
	REC_NBR = 0
C
C       READ FIRST RECORD HERE TO PROCESS THE HEADER
C       ********************************************
        READ (INP_LUN, 6001, IOSTAT=ST) SCMLREC
        IF (ST.EQ.144) THEN
           TYPE*,IAM(),' '
           TYPE*,IAM(),'========================='
	   TYPE*,IAM(),' >> ARQUIVO VAZIO'   
           TYPE*,IAM(),'========================='
           STATUS = 1
           RETURN
    	ENDIF
        IF (ST.NE.0) THEN
           TYPE*,IAM(),' '
           TYPE*,IAM(),'====================================================='
	   TYPE*,IAM(),' >> ERRO NA LEITURA DO HEADER DO ARQUIVO DE INTERFACE'   
           TYPE*,IAM(),'====================================================='
           STATUS = 1
           RETURN
    	ENDIF
        IF (SCMLREC(1:4).NE.'ODJ1') THEN
           TYPE*,IAM(),' '
           TYPE*,IAM(),'================================================='
           TYPE*,IAM(),'>> ERRO NA REFERENCIA DO REGISTRO OU FALTA HEADER'	
           TYPE*,IAM(),'   REFERENCIA ENCONTRADA = ', SCMLREC(1:4)
           TYPE*,IAM(),'   REFERENCIA ESPERADA   =  ODJ1'
           TYPE*,IAM(),'================================================='
           TYPE*,IAM(),' '
           STATUS = 1
           RETURN
        ENDIF
C
C       LOOP READING INPUT FILE
C       ***********************
C  
        FOUND_TRAILLER = .FALSE.
        REC_NBR        = 0

	DO 500 WHILE (ST.NE.144)
	
           READ (INP_LUN, 6001, END=500, IOSTAT=ST) SCMLREC
6001       FORMAT(A500)
           IF (ST.NE.0) THEN
              TYPE*,IAM(),' '
              TYPE*,IAM(),'======================================================='
	      TYPE*,IAM(),' >> ERRO NA LEITURA DO ARQUIVO DE INTERFACE ', REC_NBR
              TYPE*,IAM(),'======================================================='
              STATUS = 1
              RETURN
           ENDIF
C
C	   PROCURA TRAILLER
C
           IF (SCMLREC(1:4).EQ.'ODJ9') THEN
	      REC_NBR_TRAILLER = CTOI(SCMLREC(10:29), SZ)
              IF (REC_NBR_TRAILLER.NE.REC_NBR) THEN
                TYPE*,IAM(),' '
                TYPE*,IAM(),'================================================'
                TYPE*,IAM(),'>> NUMERO DE REGISTROS DIFERE DO TRAILLER'
                TYPE*,IAM(),'   ENCONTRADO = ', REC_NBR
                TYPE*,IAM(),'   DECLARADO  = ', REC_NBR_TRAILLER
                TYPE*,IAM(),'================================================'
                TYPE*,IAM(),' '
                STATUS = 1
                RETURN
              ENDIF

	      FOUND_TRAILLER = .TRUE. 

              ST = 144
	      GOTO 500   ! END OF READING
           ENDIF

           IF (SCMLREC(1:4).NE.'ODJ2') THEN
              TYPE*,IAM(),' '
              TYPE*,IAM(),'================================================='
              TYPE*,IAM(),'>> ERRO NA REFERENCIA DO REGISTRO'	
              TYPE*,IAM(),'   REFERENCIA ENCONTRADA = ', SCMLREC(1:4)
              TYPE*,IAM(),'   REFERENCIA ESPERADA   =  ODJ2'
              TYPE*,IAM(),'================================================='
              TYPE*,IAM(),' '
              STATUS = 1
              RETURN
           ENDIF

           REC_NBR = REC_NBR + 1

	   IF (SCMLREC(62:63).NE.'73' .AND. SCMLREC(62:63).NE.'72') THEN 
              TYPE*,IAM(),' '
              TYPE*,IAM(),'================================================='
              TYPE*,IAM(),'>> TIPO DO DOCUMENTO INVALIDO = ', SCMLREC(62:63)
              TYPE*,IAM(),'================================================='
              TYPE*,IAM(),' '
              STATUS = 1
              RETURN
           ENDIF              

C
C          CHECK FOR VALID WEEK AND YEAR
C
           WEEK = CTOI(SCMLREC(75:76), SZ)      ! SEMANA	
           YEAR = CTOI(SCMLREC(77:78), SZ)      ! ANO 
        
           IF (WEEK.GT.0.AND.WEEK.LE.53) THEN
              GAMEON = CTOI(SCMLREC(79:80),SZ)
              DRAW = GETDRW (YEAR, WEEK, GAMEON)
              IF (DRAW.LE.0) THEN
                 TYPE*,IAM(),' '
                 TYPE*,IAM(),'==================================================='
                 TYPE*,IAM(),'>> NAO ENCONTRADO NUMERO DE CONCURSO INTERNO PARA:'
                 TYPE*,IAM(),'   ANO    = ', YEAR
                 TYPE*,IAM(),'   WEEK   = ', WEEK
                 TYPE*,IAM(),'==================================================='
                 TYPE*,IAM(),' '
                 STATUS = 1
                 RETURN
              ENDIF
           ELSE
              TYPE*,IAM(),' '
              TYPE*,IAM(),'==================================='
              TYPE*,IAM(),'>> SEMANA INVALIDA : ', WEEK
              TYPE*,IAM(),'==================================='
              TYPE*,IAM(),' '
              STATUS = 1
              RETURN
           ENDIF

500     CONTINUE

        IF (.NOT.FOUND_TRAILLER) THEN
           TYPE*,IAM(),' '
           TYPE*,IAM(),'=================================================='
           TYPE*,IAM(),'>> REGISTRO TRAILLER NAO FOI ENCONTRADO NO ARQUIVO'
           TYPE*,IAM(),'=================================================='
           TYPE*,IAM(),' '
           STATUS = 1
           RETURN                   
        ENDIF
	
	RETURN
	END

