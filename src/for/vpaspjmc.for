CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHANGE LOG:
C ---+-------------+----+----------------------------------------------
C VER|DATE         |USER|DESCRIPTION       
C ---+-------------+----+----------------------------------------------
C V03 15-FEB-2013   SCML Bug fix in secant record count      
C V02 01-FEB-2013   SCML Adaptation for processing Stamp Tax in
C                        national lottery
C V01 08-SEP-2003   CMB  INITIAL RELEASE FOR PJMC
C
C PROGRAM PROCESS FILE FROM SCML ORACLE SYSTEM AND 
C GENERATE VALIDATION FILE TO PORTAL-PJMC.
C ORACLE FILE: ORCSNC.asc - CONTAINS PASSIVE TICKETS SOLD TO PORTAL.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Copyright 2003 SCML-DJ. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C PROJECT LOTARIA CLASSICA EM SERIES (PLCS)
C ADD NEW FIELD ON FILE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE VPASPJMC (GNUM,ST)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:HSHCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:VALPASFIL.DEF'
        INCLUDE 'INCLIB:ORCSNC_REC.DEF'
        INCLUDE 'INCLIB:TAXCONFIG.DEF'        
        INCLUDE 'INCLIB:M_ISLN_PJMC.DEF'        


C PARAMETERS
C************
        
        INTEGER*4 TUBSIZ
        PARAMETER (TUBSIZ=I4BUCSIZ*7)

        INTEGER*4    VPFBUF(TUBSIZ)

	CHARACTER*20 CFILNAM
	CHARACTER*25 PFILENAM
	CHARACTER*24 SNCNAM
	INTEGER*4    IFILNAM(5)
	INTEGER*4    SNCLUN, VLUN
	INTEGER*4    PUNIT
	
	CHARACTER*6 EEAAAA,EXTRACCAOANO
	CHARACTER*1 TIPO
	INTEGER*4   EXTRACCAO, ANO
	INTEGER*4   EMIOFF,INDEMIS
	INTEGER*4   IPAS
	INTEGER*4   GNUM, DRAW
	INTEGER*4   GETDRW              !FUNCTION

	INTEGER*4   STO, ST,STATUS, SZ
	INTEGER*4   YESNO
	LOGICAL     ISTHERE

	INTEGER*4   BILHETE, SERIE, DECIMO
	INTEGER*2   DAT(12)
	CHARACTER*2 TPREC
	INTEGER*4   CNTREC,  CNT_F_PRM
C----|--!--------------------------------------------------------------
C V03   ! fixing the way recnum: adding line counter, msg
C----|--!--------------------------------------------------------------
	INTEGER*4   ERRCNT, RECNUM, LINE_NR
        CHARACTER*80 MSG
        INTEGER*8   CNT_PRM, CNT_NPRM
 	
        INTEGER*4    I4TEMP,INDPAS(2)
        INTEGER*2    I2TEMP(2)
        BYTE         I1TEMP(4)
        
        CHARACTER*4 PASPAS
        CHARACTER*20 PASENT
        EQUIVALENCE (PASPAS,PASENT)

        EQUIVALENCE (I2TEMP,I4TEMP,I1TEMP)
	EQUIVALENCE (IFILNAM,CFILNAM)

C----|--!--------------------------------------------------------------
C V02 Record that holds necessary information for net prize amount
C     processing
C----|--!--------------------------------------------------------------
        RECORD /TAX_SHARES/ TX_SHRS

C----|--!--------------------------------------------------------------
C V02 Correcting detected bug that didn't allow processing of two
C     consecutive games
C----|--!--------------------------------------------------------------
       STO = 0
C----|--!--------------------------------------------------------------
C V03   ! fixing the way recnum: setting line counter to 0
C----|--!--------------------------------------------------------------
       RECNUM = 0
       LINE_NR = 0
       
C      CHOOSE DRAW  
C      -----------

       CALL WIMG(5,'DESEJA PROCESSAR QUAL EXTRACÇÃO (EEAAAA) : ')
       READ(5,901) EXTRACCAOANO
       
  
          IF (EXTRACCAOANO .EQ. 'e' .OR. EXTRACCAOANO .EQ. 'E' .OR. EXTRACCAOANO .EQ. '  ') RETURN    

        ANO    = MOD(CTOI(EXTRACCAOANO,SZ),10000)
        EXTRACCAO =INT(CTOI(EXTRACCAOANO,SZ)/10000) 
        
      
        
        IF (ANO.LT.2000 .OR. EXTRACCAO.GT.53 .OR. EXTRACCAO.LE.0) THEN
          TYPE*,'VPASPJMC-Ano/Extracção Inválidos'
             ST=-1
             RETURN
        ENDIF
         	
        WRITE(EEAAAA,FMT='(I2.2,I4.4)') EXTRACCAO,ANO
        
        CALL PRMYESNO('Confirma a EXTRACÇÃO '//EEAAAA//',  (Y/N) ? ', YESNO)
          IF (YESNO.NE.1) THEN
             ST=-1
             RETURN
          ENDIF
        
 	
 	  IF (GNUM .EQ. 8)THEN 
 	   TIPO = 'C'
           IPAS=1 	  
	  ELSEIF (GNUM .EQ. 9) THEN
 	  TIPO = 'P'
          IPAS=2
          ENDIF
        
       
C ESCREVE NOME FICHEIROS
************************
                  	
        WRITE(SNCNAM,FMT='(A12,A1,A1,A6,A4)')
     *    'FILE:ORCSNC_',TIPO,'_',EEAAAA,'.ASC'
     
     
        WRITE (PFILENAM, FMT='(A13,A1,A6,A4)')
     *     'FILE:PJMC_VL_',TIPO,EEAAAA,'.ASC'
     
C     TRY TO DELETE INTERFACE FILE FIRST
C******************************

        CALL DFILX(PFILENAM,0,0,ST)
     

	INQUIRE (FILE=SNCNAM,EXIST=ISTHERE)
	  IF (.NOT.ISTHERE) THEN
	    TYPE*,IAM(),'VPASPJMC-Ficheiro ORCSNC_',TIPO,'_',EEAAAA,'.ASC não foi encontrado'
             ST=-1
             RETURN
	  ENDIF


C VAI BUSCAR O Nº DRAW
c*********************
C            GNUM = GTNTAB(TPAS,IPAS)
            DRAW = GETDRW(ANO,EXTRACCAO,GNUM)
            
            
C VERIFICA SE ESTÁ EM MEMÓRIA
c***************************+
            
            INDEMIS = -1
            DO EMIOFF=1,PAGEMI
              IF (PASEMIS(EMIOFF,IPAS).EQ.DRAW) THEN
                 INDEMIS = EMIOFF
                 EXIT
              ENDIF
            ENDDO
           
	    IF (INDEMIS.LE.0) THEN
	        TYPE*, 'VPASPJMC-Extracção não está em memória'
               ST=-1
             RETURN
C	        CALL GSTOP(GEXIT_FATAL)
	    ENDIF
		     
	     
	    IF (DRAW.LE.0) THEN
	        TYPE*, 'VPASPJMC-Nº do DRAW inválido'
               ST=-100
             RETURN
C	        CALL GSTOP(GEXIT_FATAL)
	    ENDIF

C VERIFICA STATUS
C****************

	    IF (PASSTS(INDEMIS,IPAS).NE.GFINAL) THEN
		TYPE*,IAM(),'VPASPJMC-Resultados deverão estar finalizados '
		TYPE*, IAM(),'   '
		  ST=-1
		RETURN		
C		CALL GSTOP(GEXIT_FATAL)		
	    ENDIF

       TYPE*, IAM(),'Aguarde, ficheiro de interface em processamento'

		    
C ABRE FICHEIRO DE INTERFACE
C*****************************	
	
	CALL OPEN_FILASC (PFILENAM,PUNIT,ST)
	IF (ST.NE.0) THEN
	    TYPE*,IAM(),'VPASPJMC-Erro a abrir o ficheiro PJMC_VL_',TIPO,'_',EEAAAA,'.ASC '
	    TYPE*, IAM(),'   '
	      ST=-1
	    RETURN		
C		CALL GSTOP(GEXIT_FATAL)	
	ENDIF
	
C WRITE HEADER
C******************

	    TPREC='HP'
            DAT(VCDC) = DAYCDC
            CALL CDATE(DAT)

	 WRITE(PUNIT,5) TPREC,2000+DAT(VYEAR),DAT(VMON),DAT(VDAY)
	 
	 CNTREC = 1	   	
   	
	
C OPEN SECANTE FILE
C******************

	CALL OPEN_ORCSNC(SNCNAM,SNCLUN,ST)
	IF (ST.NE.0) THEN
	    TYPE*,IAM(),'VPASPJMC- Erro a abrir o ficheiro de secantes'
	    TYPE*, IAM(),'   '
	     ST=-1
	    RETURN		
C		CALL GSTOP(GEXIT_FATAL)	
	ENDIF

C----|--!--------------------------------------------------------------
C V02 Initializing auxilliary structure with the necessary values
C----|--!--------------------------------------------------------------
        CALL INIT_STRUCT_TAX_SHARES (TX_SHRS)
        TX_SHRS.GNUM = GNUM
        TX_SHRS.EEAAAA = EEAAAA
        TX_SHRS.DRWN = DRAW
               
C----|--!--------------------------------------------------------------
C V02 Reading lottery shares' info files' content into memory
C----|--!--------------------------------------------------------------
        CALL READ_TAX_FIL (TX_SHRS)
        CALL READ_SHR_FIL (TX_SHRS)
        
C        CALL DUMP_STRUCT_TAX_SHARES (TX_SHRS)
        
C OPEN VPF FILE
c*************

       WRITE(CFILNAM,FMT='(A8,I2.2,I4.4,A4)')
     *    'VALX:VPF',IPAS,DRAW,'.FIL'

        CALL FIND_AVAILABLE_LUN(VLUN,ST)
	IF (ST.NE.0) THEN
	    CALL FILERR(IFILNAM,0,ST,0)
	    TYPE*, IAM(),'   '
	      ST=-1
	    RETURN
	ENDIF

	CALL IOPEN(IFILNAM,VLUN,VPFLEN*2,VFSCDC,VFSSER*2-1,ST)
	IF(ST.NE.0) THEN 
            CALL FILERR(IFILNAM,1,ST,0)
        ENDIF
        
	CALL FASTSET(0, V4BUF_PAS, VPFLEN * VPFMAXREC)
        
        
        CNT_NPRM=0
        
        DO WHILE (STO.NE.EOF)
        
300	CONTINUE

	 IF (STO.NE.EOF)	THEN
	
C----|--!--------------------------------------------------------------
C V03   ! fixing the way recnum: adding line nr
C----|--!--------------------------------------------------------------
	 CALL READ_ORCSNC(SNCLUN,INDEMIS,IPAS,ERRCNT,RECNUM
     *          ,ORCSNC_REC,ST,STO, LINE_NR)
	
	
		
	   IF (ST.NE.0) THEN 
	     TYPE*,IAM(),'VPASPJMC- Erro na leitura do ORCSNC_',TIPO,'_',EEAAAA,'.ASC.'
	     TYPE*, IAM(),'   '
	       ST=-1
	     RETURN
	   ENDIF


                BILHETE=ORCSNC_REC.TICNUM
                SERIE=ORCSNC_REC.TICSER
                TIPO=ORCSNC_REC.RECTYP
    
                
	    DO DECIMO=1,PASNOFFRA(INDEMIS,IPAS)

	    
               I4TEMP      = 0                       !KEY2BYTES
               I1TEMP(2)   = 0                       !FREE
               I1TEMP(1)   = DECIMO
               INDPAS(1)   = I4TEMP
               I4TEMP      = ISHFT(SERIE,24)+BILHETE !KEY4BYTES
               INDPAS(2)   = I4TEMP

C  READ FILE
C ************
               CALL IREAD(INDPAS ,V4BUF_PAS, VLUN, STATUS)

C **************************************************
C VERSAO ANTERIOR LIA O HEADER COMO NAO PREMIADO
C **************************************************                  
C	         IF (STATUS.NE.0) THEN
C **************************************************
	
C----|--!--------------------------------------------------------------
C V03   ! old version counted trailer as a record
C----|--!--------------------------------------------------------------
	         IF (     (STATUS.NE.0)
     *               .AND.(TIPO.NE.'H')
     *               .AND.(TIPO.NE.'T')) THEN
		     CNT_NPRM=1 + CNT_NPRM

    	     

		     GOTO 300

	         ELSEIF (STATUS.EQ.0) THEN
	             CNT_F_PRM=1 + CNT_F_PRM

C **************************************************
C NA VERSAO ANTERIOR LIA O TRAILER COMO MAIS UM BILHETE
C **************************************************                  
	                 
	           IF (TIPO.EQ.'T') THEN

        	   GOTO 301
          
	  	   ENDIF
	  	   
C **************************************************                  
	         
	             CALL LOGPAS(VALREC,V4BUF_PAS)
	             CALL DLOGPAS(VALREC,VDETAIL)  
	         
	             CALL WRITEVPORTAL_LN(VALREC,PUNIT,VDETAIL,CNTREC
     *                      , TX_SHRS)
        	         

	        ENDIF
        
	    ENDDO      

	 ENDIF 
	
 	 
	ENDDO
	
301	CONTINUE
	
	
C WRITE TRAILER
C******************

	TPREC='TP'
	CNTREC =CNTREC+1
	
	 WRITE(PUNIT,20)TPREC,CNTREC
	 
C SEND LOG TO CONSOLE
c++++++++++++++++++

	CNT_PRM=CNT_F_PRM/PASNOFFRA(INDEMIS,IPAS)
		
	IF (RECNUM .EQ.(CNT_PRM+CNT_NPRM).OR. ERRCNT.EQ.0) THEN
	
C----|--!--------------------------------------------------------------
C V03   ! fixing the way recnum: showing more detail
C----|--!--------------------------------------------------------------
        WRITE(MSG,FMT='(A,A,I8)') IAM(),
     *      'VPASPJMC - Nº DE NUMEROS PREMIADOS     :',CNT_PRM
	TYPE *, MSG
        WRITE(MSG,FMT='(A,A,I8)') IAM(),
     *      '           Nº DE NUMEROS NÃO PREMIADOS :',CNT_NPRM
	TYPE *, MSG
        WRITE(MSG,FMT='(A,A,I8)') IAM(),
     *      '           Nº DE NUMEROS DA SECANTE    :',RECNUM
	TYPE *, MSG
	TYPE *, IAM(), '                            '


           TYPE *, IAM(), 'VPASPJMC - PROCEDIMENTO OK'
	ELSE
	   TYPE *, IAM(), 'VPASPJMC - PROCEDIMENTO COM ERROS'

	ENDIF
	
	 
	CALL ICLOSE(VLUN,VPFBUF,ST)
	CLOSE (SNCLUN)
	CLOSE (PUNIT)




	 RETURN
	
901    FORMAT(A6)
902    FORMAT(A1)
C----|--!--------------------------------------------------------------
C V02   ! adding new field with effective prize
C----|--!--------------------------------------------------------------
5      FORMAT(A2,I4.4,I2.2,I2.2,25(' '))
C----|--!--------------------------------------------------------------
C V02   ! adding new field with effective prize
C----|--!--------------------------------------------------------------
20     FORMAT(A2,I8.8,25(' '))

C       **************************************************
C       VERSAO ANTERIOR 2 ESPACOS A MAIS
C       **************************************************
C      5      FORMAT(A2,I4.4,I2.2,I2.2,17(' '))
C      20     FORMAT(A2,I8.8,17(' '))
C       **************************************************

   
	END
	
	
C       **************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE OPEN_ORCSNC (SNCNAM,SNCLUN,ST)
        IMPLICIT NONE                
C       **************************************************

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'
        INCLUDE 'INCLIB:ORCSNC_REC.DEF'

        INTEGER*4 ST
        INTEGER*4 LUN, SNCLUN
        CHARACTER*24 SNCNAM

	CALL FIND_AVAILABLE_LUN(LUN,ST)
	  IF (ST.NE.0) THEN
	    TYPE*,IAM(),'VPASPJMC-Erro, não encontro unidade lógica para ORCSCN.ASC'
	    CALL GSTOP(GEXIT_FATAL)
	  ENDIF


        SNCLUN = LUN

        OPEN (UNIT   =  SNCLUN,
     *        FILE   =SNCNAM,
     *        STATUS = 'OLD', 
     *        IOSTAT =  ST)


	ORCSNC_REC.RECTYP = '  '

	RETURN	
	END

C----|--!--------------------------------------------------------------
C V03   ! fixing the way recnum: adding line nr
C----|--!--------------------------------------------------------------
C       **************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE READ_ORCSNC(SNCLUN,INDEMIS,INDPAS,ERRCNT,RECNUM
     *      ,ORCSNC_REC,ST,STO, LINE_NR)
        IMPLICIT NONE                
C       **************************************************

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'
        INCLUDE 'INCLIB:ORCSNC_REC.DEF'

	CHARACTER*100 SNCREC
	CHARACTER*2   REFTYP

        INTEGER*4     ST,SIZ,RECNUM,TOTREC,ERRCNT
	INTEGER*4     INDEMIS,INDPAS,LOGLUN,STO
	INTEGER*4     SNCLUN
C----|--!--------------------------------------------------------------
C V03   ! fixing the way recnum: adding line nr
C----|--!--------------------------------------------------------------
        INTEGER*4     LINE_NR
        
       COMMON /LOGFILE/ LOGLUN

	CALL CLEAR_ORCSNC_REC()
	

        READ (SNCLUN, 100, END=700,IOSTAT=ST) SNCREC ! READ MY SECANTES FILE
C----|--!--------------------------------------------------------------
C V03   ! fixing the way recnum is counted; adding line nr
C----|--!--------------------------------------------------------------
C	RECNUM = RECNUM + 1
        LINE_NR = LINE_NR + 1
        
	REFTYP = SNCREC(1:2)
		    
C TRATA HEADER

	IF (ORCSNC_REC.RECTYP.EQ.'  ') THEN
	   IF (REFTYP.NE.'HP') THEN
	     TYPE *,IAM(),'READ_ORCSNC - Primeira linha inválida, deverá ser HP-HEADER'	
	     ST =-1
	     RETURN	   
	   ENDIF   
	ENDIF   
	
C TRATA DETAIL	

	IF (REFTYP.EQ.'01') THEN
	
	    ORCSNC_REC.RECTYP = '01'    
            ORCSNC_REC.TICNUM = CTOI(SNCREC(03:07),SIZ)
	    ORCSNC_REC.TICSER = CTOI(SNCREC(08:09),SIZ) ! (PLCS) change serie field to 2 chars

C Nº DE BILHETE É VALIDO?
	
	    IF (ORCSNC_REC.TICNUM.LT.0 .OR.
     *          ORCSNC_REC.TICNUM.GT.PASNUMTCK(INDEMIS,INDPAS)-1 )THEN

	     TYPE *,IAM(),'READ_ORCSNC - Número do bilhete inválido para esta Extracção '
		ST = -1
		RETURN
	    ENDIF

C SÉRIE É VÁLIDA?
	    
	    IF (ORCSNC_REC.TICSER.LE.0.OR.
     *          ORCSNC_REC.TICSER.GT.PASNUMSER(INDEMIS,INDPAS))THEN
	     TYPE *,IAM(),'READ_ORCSNC - Nº de série inválido para esta Extracção'   

		ST =-1
		RETURN
	    ENDIF
	    
	    ORCSNC_REC.ST = 0
	    ST  = 0
	    STO = 0
C----|--!--------------------------------------------------------------
C V03   ! fixing the way recnum is counted: only if we have a body line
C----|--!--------------------------------------------------------------
            RECNUM = RECNUM + 1

            RETURN

	    
	ELSEIF (REFTYP.EQ.'HP') THEN

             ORCSNC_REC.RECTYP  = 'HP'
             ORCSNC_REC.DTHD = SNCREC(3:6)//'/'//SNCREC(7:8)//'/'//SNCREC(9:10)
	     ORCSNC_REC.ST = -1     !SKIP HEADER
	     ST  = 0
	     STO = 0
             RETURN
	

C TRATA TRAILER
C*************	

	ELSEIF (REFTYP.EQ.'TP') THEN
	    ORCSNC_REC.RECTYP  = 'TP'
	    TOTREC             = CTOI(SNCREC(3:10),SIZ)
	
C----|--!--------------------------------------------------------------
C V03   ! fixing the way recnum is counted: using line nr
C----|--!--------------------------------------------------------------
	    IF (TOTREC.NE.LINE_NR) THEN
		TYPE *,IAM(),'READ_ORCSNC - Nº de registos não correponde ao trailer ', TOTREC, ' - ',RECNUM
	        ST=-1
	        RETURN
	    ENDIF
	    
	ELSE
	     TYPE *,IAM(),'READ_ORCSNC - Tipo de registo inválido'
             ST=-1	    
	     RETURN
	ENDIF    

C TRATA ERROS
C************
	
           IF (ORCSNC_REC.ST.NE.0) THEN
	     TYPE *,IAM(),'READ_ORCSNC - OUTROS ERROS'
	     ST = -1

            ERRCNT = ERRCNT + 1

	    RETURN
	    ENDIF
	    
	
700	CONTINUE

	IF (ORCSNC_REC.RECTYP.NE.'TP') THEN
	     TYPE *,IAM(),'READ_ORCSNC - Ficheiro sem TRAILER'
	     ST=-1
	    RETURN
	ELSE
	    ST  = 0	  
	    STO = EOF
	ENDIF              
	RETURN             
	     
100	FORMAT (A100)

	END  
             
C       **************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CLEAR_ORCSNC_REC()
        IMPLICIT NONE                
C       **************************************************

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:ORCSNC_REC.DEF'

	ORCSNC_REC.ST      = 0
        ORCSNC_REC.RECTYP  = ' '    
	ORCSNC_REC.TICNUM = 0
	ORCSNC_REC.TICSER = 0

	RETURN
	END
		
C*************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE WRITEVPORTAL_LN(VALREC, PUNIT, VDETAIL,CNTREC
     *              , TX_SHRS)
	IMPLICIT NONE
C**************************************************

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
        INCLUDE 'INCLIB:NAMCMD.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:PRMHSH.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:M_ISLN_PJMC.DEF'

        RECORD /TAX_SHARES/ TX_SHRS
        
C  LOCAL VARIABLES
C
        INTEGER*2   AUX
        INTEGER*4   I
        INTEGER*4   CNTREC
        INTEGER*4   PUNIT
        INTEGER*4   DIV
        INTEGER*4   SHR
        INTEGER*4   GTYP
        INTEGER*4   GIND
        INTEGER*4   DRW
        INTEGER*4   TSHARES(40)

        INTEGER*4   PAYAMT

        CHARACTER*16  STATAUX
        CHARACTER*123 CPRIZES(VMAX)   
        
C ********************************************************************

        AUX   = 1
        CALL FASTSET(0,TSHARES,40)

        GTYP = VALREC(VGTYP)
        GIND = VALREC(VGIND)

        PAYAMT = VALREC(VPAMT)
             

C PRINT VPF INFORMATION 
C**********************

        DO 100 I=1,VALREC(VPZOFF)

               DRW = VDETAIL(VDRW,I)
               DIV = VDETAIL(VDIV,I)
               
               SHR = VDETAIL(VSHR,I)
               TSHARES(DIV) = TSHARES(DIV) + SHR

               WRITE (CPRIZES(AUX),804) DIV,SHR                

            AUX = AUX + 1
100     CONTINUE

C----|--!--------------------------------------------------------------
C V02 Calling CHECKPASTAX
C----|--!--------------------------------------------------------------
        TX_SHRS.HASTAX = .FALSE.       
        CALL STRUCT_TAX_SHARES_CHECKPASTAX (TX_SHRS, VALREC)
        
C  PRINT VPF LINES
C*******************
        WRITE(STATAUX,903) VALST(VALREC(VSTAT))

C----|--!--------------------------------------------------------------
C V02   ! adding new field with effective prize
C----|--!--------------------------------------------------------------
          WRITE(PUNIT,906) VALREC(VTCKT)     ! TICKET NUMBER
     *                   , VALREC(VSERN)     ! SERIE NUMBER
     *                   , VALREC(VPFRAC)    ! TEN
     *                   , STATAUX           ! STATUS
     *                   , TX_SHRS.PRZAMT    ! PAY AMOUNT
     *                   , TX_SHRS.NETPRZAMT ! NET PAY AMOUNT

        CNTREC = CNTREC + 1
        
            DO 1000 I=1,AUX-1
              WRITE(PUNIT,810) CPRIZES(I)
              CNTREC = CNTREC + 1

1000    CONTINUE

C  PRINT DIVISIONS 

903     FORMAT(A4)
        
C----|--!--------------------------------------------------------------
C V02   ! adding new field with effective prize
C----|--!--------------------------------------------------------------
906     FORMAT('01',I5.5,I2.2,I2.2,A4,I10.10,I10.10) 
C----|--!--------------------------------------------------------------
C V02   ! adding new field with effective prize
C----|--!--------------------------------------------------------------
804     FORMAT('02',I2.2,I4.4,27(' ')) 
C----|--!--------------------------------------------------------------
C V02   ! adding new field with effective prize
C----|--!--------------------------------------------------------------
810     FORMAT(A35) 
        
	END
             
