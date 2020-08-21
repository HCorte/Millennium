C VPJMC.FOR
C
C V08 18-OCT-2013 SCML Adding support for OP Generation Flag
C V07 06-JAN-2011 FJG Lotto2 Batch
C     23-JAN-2011 FJG Out of Bounds issue
C     09-FEB-2011 FJG GAMLOG error - MyGod!
C     17-FEB-2011 FJG Harcode per Accenture's request
C     28-FEB-2011 FJG Undo Hardcode
C V06 07-JUL-2006 CMB escreve op desde que exista 
C V05 24-JUN-2006 CMB LIMITE O ANO DO CONCURSO
C v04 09-JAN-2006 CMB  PORTALSAP (passa a ser DJOGOS)
C V03 24-JAN-2005 CMB OPS Greater than 50.00
C V02 18-JUL-2003 CMB INITIAL RELEASE FOR PORTAL
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2003 DJ - SCML. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C======	OPTIONS /CHECK=NOOVERFLOW
	PROGRAM VPJMC
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:TNAMES.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
	INCLUDE 'INCLIB:RECUSE.DEF'
	INCLUDE 'INCLIB:PRMHSH.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VDETAIL.DEF'
	INCLUDE 'INCLIB:VALFIL.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	
	INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'

C        INTEGER*4 TOT_CNT, TOT_AMT
C        PARAMETER(TOT_CNT = 1)               ! TOTAL COUNTER
C        PARAMETER(TOT_AMT = 2)               ! TOTAL AMOUNT

        CHARACTER*8 PASPAS
        CHARACTER*20 PASENT
        EQUIVALENCE (PASPAS,PASENT)

C	CHARACTER*6 CCAAAA, CONCURSOANO, ANOCONCURSO,CONCURSOM,ANOM,AAAACC
	CHARACTER*7 CCCAAAA, CONCURSOANO, AAAACCC
	CHARACTER*9 GGAAAACCC
	INTEGER*4 CONCURSO, ANO
	INTEGER*4 DRWN, GETDRW                       !FUNCTION 	
	INTEGER*4 GNUM, GTYP, GAM, VST,GIND
	INTEGER*4 INDDRW, DRWVDT
	
	INTEGER*4 ST, NOCHECK0
	
	INTEGER*4 TUBSIZ
        PARAMETER (TUBSIZ=I4BUCSIZ*7)		
	INTEGER*4 VLFBUF(TUBSIZ)	
	
	INTEGER*4   YESNO
	INTEGER*2   DAT(12)

	INTEGER*4   CNTREC	
	CHARACTER*2 TPREC        

	INTEGER*4 SZ
	
	INTEGER*4 PUNIT
        CHARACTER * 30 PFILENAM ! INTERFACE FILE NAME

	LOGICAL      VALDRW
	LOGICAL*1    ISTHERE

	INTEGER*4 I4TEMP
	INTEGER*2 I2TEMP(2)
	BYTE      I1TEMP(4)
	EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)
	
        INTEGER*4 FLUN,GAMSTS, GAMSEC
	EQUIVALENCE(DLTREC,DSPREC,DKKREC,GAMSTS)

        INTEGER*4 PORTALSAP                
c        PARAMETER (PORTALSAP=007233) !coritel
        PARAMETER (PORTALSAP=007456)

	DATA GTYP/0/
	DATA PUNIT/7/
	DATA FLUN/1/
	
C COMMON AREA
C
	COMMON /NOCHECK0/ NOCHECK0

C
C SHOW TITLE
C***************

	CALL CLRSCR(5)
	TYPE *, IAM()
	TYPE*,IAM(),'*******************************************************'
 	TYPE*,IAM(),'                INTERFACE COM PORTAL                   '
   	TYPE*,IAM(),'          GERAÇÃO DO FICHEIRO DE PRÉMIOS               '
     	TYPE*,IAM(),' 							    '  
     	TYPE*,IAM(),'          MÚTUAS   (PJMC_VM_GN_CCCAAAA.asc)            '  
     	TYPE*,IAM(),'                 CCC=CONCURSO/AAAA=ANO                 '
     	TYPE*,IAM(),' 							    '     	
     	TYPE*,IAM(),'          LOTARIA  (PJMC_VL_<C/P>_EEAAAA.asc)          '
     	TYPE*,IAM(),'    C=CLÁSSICA /P=POPULAR / EE=EXTRACÇÃO /AAAA=ANO     '
     	TYPE*,IAM(),'*******************************************************'
        TYPE *, IAM()

C
C PROMPT USER TO SIGNON
C**********************

c        CALL PASSWORD(5,PASENT)
c        IF(PASPAS.NE.'EURO2004') THEN
c          TYPE*,' ********* ACESSO NEGADO **********'
c          TYPE*,'    DIGITE A PASSWORD CORRECTA     '
c          CALL GSTOP(GEXIT_SUCCESS)
c        ENDIF
        
C       CHOOSE GAME

100     CONTINUE
		CALL WIMG(5,'DIGITE O JOGO QUE DESEJA PROCESSAR')
		CALL GAME_TYPNDX(GNUM, GTYP, GIND, ST)
		IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
		IF (GNUM .EQ.5 ) THEN
!			TYPE *,IAM(), 'VPJMC-Não existe ficheiro de prémios de Joker, estes estão associados ao jogo matriz.'
!			TYPE *,IAM(), '    '
!			GOTO 100
!		ELSE IF (GNUM .GT.5 .AND. GNUM .LT. 8) THEN
!			TYPE *,IAM(),'VPJMC-Jogo Desactivado'
!			GOTO 100 
		ELSE IF (GNUM .GT.7 .AND. GNUM .LT. 10) THEN
			CALL VPASPJMC (GNUM,ST)
			IF (ST .NE. 0) GOTO 2000
			GOTO 100
		END IF	 		       	
        
	
C       CHOOSE DRAW  
C       -----------

	CALL WIMG(5,'DESEJA PROCESSAR QUAL CONCURSO (CCCAAAA)?')
        READ(5,102) CONCURSOANO
        TYPE*,IAM(),CONCURSOANO

        
        IF (CONCURSOANO .EQ. 'e' .OR. CONCURSOANO .EQ. 'E' .OR. CONCURSOANO .EQ. '  ') GOTO 100  

        ANO    = MOD(CTOI(CONCURSOANO,SZ),10000)
        CONCURSO =INT(CTOI(CONCURSOANO,SZ)/10000) 
        

        IF (ANO.LT.2005 .OR. ANO.GT.2100 .OR. CONCURSO.GT.105 .OR. CONCURSO.LE.0) THEN !V05
          TYPE*,'VPJMC - Ano/Concurso Inválido'
	    goto 100
c           CALL GSTOP (GEXIT_FATAL)
        ENDIF
         	
        WRITE(CCCAAAA,FMT='(I3.3,I4.4)') CONCURSO,ANO
        WRITE(AAAACCC,FMT='(I4.4,I3.3)') ANO, CONCURSO
C        TYPE*,IAM(),'AAAAACC ',AAAACC
        WRITE(GGAAAACCC,FMT='(I2.2,I4.4,I3.3)') GNUM, ANO, CONCURSO
C        TYPE*,IAM(),'GGAAAAACC ',GGAAAACC
        	
        
         CALL PRMYESNO('Confirma o concurso '//CCCAAAA//',  (Y/N) ? ', YESNO)
          IF (YESNO.NE.1) GOTO 100

 	
C        DO I=1,MAXGAM
          DRWN = GETDRW(ANO,CONCURSO,GNUM) 
C        ENDDO


	IF (DRWN.LE.0) THEN
	   TYPE*, 'VPJMC-Nº do DRAW inválido'
	   TYPE*, 'Erro Fatal'
	   goto 100
	ENDIF

	IF (GTYP .EQ. TSPT) GAMSEC = DSPSEC
	IF (GTYP .EQ. TLTO) GAMSEC = DLTSEC
	IF (GTYP .EQ. TKIK) GAMSEC = DKKSEC

C GET DATA FROM COMMON OR DISK
C******************************
C       IF(DRWN.EQ.DAYDRW(GNUM)) THEN
C           CALL GAMLOG(GTYP,GIND,GAMSTS)  ! ERROR ONE ARGUMENT LESS
C           GOTO 100
C       ENDIF	

C READ GAME FILE
Cthis subroutine open, read and close the file
C************************************************
        CALL READGFL(FLUN,GFNAMES(1,GNUM),GAMSEC,DRWN,GAMSTS)

C CHECK IF DRAW HAVE PRIZES NUMBERS
C**************************************
	IF (GAMSTS.LT.GFINAL) THEN
	    TYPE *,IAM(),'VPJMC-Prémios não apurados para o concurso: ',CCCAAAA 
	    TYPE *,IAM(),'       '  
	    GOTO 100
	ENDIF
	
	
C*************************************************
C   If file OPS.FIL doesn't exist, SEND MESSAGE TO CONSOLE
C**************************************************

 	INQUIRE(FILE='FILE:OPS.FIL', EXIST=ISTHERE)
          IF(.NOT.ISTHERE) THEN
	    TYPE*,IAM(),'VPJMC-Não encontro o ficheiro de OPS'
	    TYPE*, IAM(),'       '  
            CALL GSTOP(GEXIT_FATAL)
	  ENDIF        
              

C WRITE INTERFACE FILE NAME
c************************* 

	WRITE (PFILENAM, FMT='(A13,I2.2,A7,A4)')             ! V07
     *     'FILE:PJMC_VM_',GNUM,CCCAAAA,'.ASC'               ! V07
     

C     TRY TO DELETE FILE FIRST
C******************************

        CALL DFILX(PFILENAM,0,0,ST)
        IF (ST.NE.0) CALL GSTOP (GEXIT_FATAL)


C oPEN INTERFACE FILE
C*****************************	
	
	CALL OPEN_FILASC (PFILENAM,PUNIT,ST)
	IF (ST.NE.0) THEN
	    TYPE*,IAM(),'PJMC-Erro a criar/abrir o ficheiro PJMC_VM_',GNUM,CCCAAAA,'.ASC '
	    TYPE*, IAM(),'       '  
	    GOTO 2000
	ENDIF

C WRITE HEADER
C******************

	    TPREC='HP'
            DAT(VCDC) = DAYCDC
            CALL CDATE(DAT)

	 WRITE(PUNIT,10) TPREC,2000+DAT(VYEAR),DAT(VMON),DAT(VDAY)
	 
	 CNTREC = 1	    

C OPEN VALIDATION FILE
C*********************

        CALL IOPEN(SFNAMES(1,VLF),VLF,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
           IF(ST.NE.0) THEN
             CALL FILERR(SFNAMES(1,VLF),1,ST,0)
       	     GOTO 2000
       	   ENDIF  
        CALL ITUBSIZE(VLF,TUBSIZ)

	TYPE*, IAM(),'VPJMC-Aguarde, ficheiro de interface em processamento'


C READ TRANSACTION
C*****************
           
300	CONTINUE
	
	 CALL ISREAD(V4BUF,VLF,VLFBUF,ST)

          IF (ST.EQ.ERREND) THEN
	   GOTO 1000
	  ENDIF

	  IF (ST.NE.0) THEN
           CALL FILERR(SFNAMES(1,VLF),2,ST,0)
           CALL GPAUSE
           GOTO 300
          ENDIF

	  CALL LOGVAL(VALREC,V4BUF)
	  
	  GAM=VALREC(VGAM)
          VST=VALREC(VSTAT)	
          	 	
C CHECK FOR RESULTS NOT IN
C**************************

        IF(VST.EQ.VNOPAY.OR.VST.EQ.VNOPRZ.OR.VST.EQ.VPOST.OR.
     *     VST.EQ.VPPNPZ.OR.VST.EQ.VPRPOST) THEN
	    
	    TYPE*,IAM(),'VPJMC-O valor dos prémios ainda não foi atribuído.'
	    GOTO 300
	ENDIF
	
C CHECK IF TRANSACTION SHOULD BE PRINTED
C****************************************

	IF(VALREC(VGAM).NE.GNUM.AND.GNUM.GT.0)	      GOTO 300
	  
C ONLY PORTAL AGENT , TESTES COM CASA DA SORTE
C****************************************
		
	IF ( AGTSAP(VALREC(VSTER)).NE. PORTALSAP )           GOTO 300
c	IF ( AGTSAP(VALREC(VSTER)).NE. 007233 )           GOTO 300
C	IF ( AGTSAP(VALREC(VSTER)).NE. 5 )           GOTO 300
                           
C
C PRINT TRANSACTION
C*******************

	GTYP = VALREC(VGTYP)

	CALL DLOGVAL(VALREC,VDETAIL)

	  
	  VALDRW = .FALSE.
	  DO INDDRW=1, VALREC(VPZOFF)
	    DRWVDT = VDETAIL(VDRW,INDDRW) 	    
	    IF (DRWVDT .EQ. DRWN) VALDRW = .TRUE.
	  ENDDO


C CHECK DRAW TO PRINT
C*********************
	
	IF (VALDRW)  CALL WRITEVPORTAL_AM(VALREC,PUNIT,VDETAIL,CNTREC,ST,AAAACCC,GGAAAACCC)	
	IF (ST .NE. 0) GOTO 2000 

	GOTO 300
	
1000	CONTINUE	
	
C WRITE TRAILER
C******************

	TPREC='TP'
	CNTREC =CNTREC+1
	

	 WRITE(PUNIT,20)TPREC,CNTREC
	 
	 TYPE *,IAM(),'VPJMC-Procedimento OK'
	 TYPE*, IAM(),'       '  

C CLOSE FILE
C***********
	CALL USRCLOS1(PUNIT)
	CLOSE(OPS_LUN)
	CALL ICLOSE(VLF,VLFBUF,ST)	
        goto 100
        
2000	CONTINUE

C CLOSE FILE
C***********
	CALL USRCLOS1(PUNIT)
	CLOSE(OPS_LUN)
	CALL ICLOSE(VLF,VLFBUF,ST)
C     DELETE FILE WITH ERRORS

        CALL DFILX(PFILENAM,0,0,ST)
        IF (ST.NE.0) CALL GSTOP (GEXIT_FATAL)
        
	TYPE *,IAM(), 'VPJMC-PROCEDIMENTO COM ERROS'
	TYPE *,IAM(), '     '
	        
        GOTO 100
C*********************************************
C              FORMAT MESSAGES
C*********************************************

102    FORMAT(A7)
10     FORMAT(A2,I4.4,I2.2,I2.2,73(' '))
20     FORMAT(A2,I8.8,73(' '))


	END
	
C       **************************************************
C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE OPEN_FILASC (PFILENAM,PUNIT,ST)
        IMPLICIT NONE                
C       **************************************************
 
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'

	INTEGER*4 ST, PUNIT
	CHARACTER*30 PFILENAM
	
	
        OPEN (UNIT       =  PUNIT,
     *        FILE       = PFILENAM,
     *        IOSTAT     = ST,
     *        FORM       = 'FORMATTED',
     *        RECL       = 135,
     *        STATUS     = 'NEW',
     *        RECORDTYPE = 'STREAM_CR')

	IF(ST.NE.0) THEN
          TYPE *, IAM()
	  TYPE *, IAM(), 'VPJMC- Erro ao criar/abrir: ', PFILENAM
          TYPE *, IAM(),'                '
	  CALL USRCLOS1(PUNIT)
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
	RETURN
	END
C*************************************************
C======	OPTIONS /CHECK=NOOVERFLOW
C	SUBROUTINE WRITEVPORTAL_AM(VALREC, PUNIT, VDETAIL, CNTREC, ST, ANOCONCURSO, GANOCONCURSO)
	SUBROUTINE WRITEVPORTAL_AM(VALREC,PUNIT,VDETAIL,CNTREC,ST,AAAACCC,GGAAAACCC)	
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
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'
        INCLUDE 'INCLIB:OPS_REC.DEF'
        

	 INCLUDE 'INCLIB:RECSCF.DEF'

      INCLUDE 'INCLIB:DLTREC.DEF'
      INCLUDE 'INCLIB:DSPREC.DEF'
      INCLUDE 'INCLIB:DBNREC.DEF'
      
      INCLUDE 'INCLIB:WINCOM.DEF'
        
C  LOCAL VARIABLES

        INTEGER*2   AUX
	
        INTEGER*4  AGT, AGTOPS
 
        INTEGER*4   I
        INTEGER*4   LINCNT, CNTREC
        INTEGER*4   PUNIT
        INTEGER*4   DIV
        INTEGER*4   SHR
        INTEGER*4   GTYP
        INTEGER*4   GIND
        INTEGER*4   DRW
        INTEGER*4   TSHARES(40)
        INTEGER*4   SSER
        INTEGER*4   SCHK
        INTEGER*4   KIK
	INTEGER*2   DAT(12)
        INTEGER*4   PAYAMT
        INTEGER*4   WEEK, YEAR, ST
        
        INTEGER*4   VALOR   

C	###I
        CHARACTER*14   BIL
        CHARACTER*16  STATAUX
        CHARACTER*100 CPRIZES(VMAX)      
        CHARACTER*7 AAAACCC         
        INTEGER*4    BILHETEI,BILHETEI1
        INTEGER*4    SZ,NOOPS
        CHARACTER*6 OPSORDER
        INTEGER*4    OPSTOTAL       ! Value in prize just for the main game 
        INTEGER*4    OPSJOKER      ! Value in prize just for joker
C        INTEGER*4    AGENTEI
	CHARACTER*9  GANOCONCURSOCHAVE
        CHARACTER*9  GGAAAACCC
        
C	###F        

C
C  COMMON AREA
C
        COMMON /VPORTAL/ LINCNT
C  ********************************************************************

C  OPEN  OPS.FIL TO GET OP'S NUMBER
C***********************************

        CALL OPEN_OPS('KEYED',ST)
        IF (ST.NE.0) THEN
           TYPE*, IAM(), 'VPJMC-Erro ao abrir ficheiro OPS'
C           RETURN
           STOP
C           CALL GSTOP (GEXIT_FATAL)
        ENDIF
         OPSORDER=' '
        AUX   = 1
        CALL FASTSET(0,TSHARES,40)

        GTYP = VALREC(VGTYP)
        GIND = VALREC(VGIND)
        PAYAMT = VALREC(VPAMT)
        AGT = AGTTAB(AGTNUM,VALREC(VSTER))


C PRINT VLF INFORMATION 
C**********************
        DO 100 I=1,VALREC(VPZOFF)
          
               DRW = VDETAIL(VDRW,I)
               DIV = VDETAIL(VDIV,I)
               SHR = VDETAIL(VSHR,I)
               KIK = VDETAIL(VKIK,I)
               TSHARES(DIV) = TSHARES(DIV) + SHR
	   
	       CALL GETWEK(DRW, VALREC(VGAM),WEEK,YEAR,ST)
               WRITE (CPRIZES(AUX),800) WEEK,YEAR,DIV,SHR,KIK                
  	
            AUX = AUX + 1
            

100     CONTINUE

        DAT(VCDC) = VALREC(VSCDC)
        CALL CDATE(DAT)
        CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),SSER,SCHK)

        WRITE(STATAUX,903) VALST(VALREC(VSTAT))
        
c        VALOR = PAYAMT + VALREC(VKPAMT)
        VALOR = VALREC(VOPSAMT) + VALREC(VKOPSAMT) 

C WRITE LINE WITH ORDER INFORMATION
c**********************************
C	TYPE *, IAM(),'##### valor ', VALOR
              
c      IF (VALOR .GT. 5000) THEN      !v03
C     IF (VALOR .GE. VALORDER) THEN      !v03
C----+------------------------------------------------------------------
C V07| Adding support for OP Generation Flag
C----+------------------------------------------------------------------
C      IF (VALOR .GT. 0) THEN      !v06
        IF (VALOR .GT. 0 .AND. VALREC(VOPSCNT) .GT. 0) THEN      !V08
C----+------------------------------------------------------------------
C V07| Adding support for OP Generation Flag
C----+------------------------------------------------------------------

          WRITE (BIL, 904)  DAT(VJUL), SSER, SCHK
          OPSORDER=' '
          OPSTOTAL=0
          OPSJOKER=0
         
          READ(OPS_LUN, KEYID=1, KEY=BIL, IOSTAT=ST) OPS_REC

          BILHETEI=0
          BILHETEI=CTOI(OPS_REC.BILHETE (4:11),SZ)
         
          IF (((OPS_REC.YEARWEEK) .EQ. AAAACCC).AND.(BILHETEI .EQ. SSER)) THEN
            OPSTOTAL=OPS_REC.TOTAL_GAME
            OPSJOKER=OPS_REC.TOTAL_JOKER
            OPSORDER=OPS_REC.ORDER
          ENDIF
         
          IF ((BILHETEI .EQ. SSER) .AND. ((OPS_REC.YEARWEEK) .NE. AAAACCC)) THEN
            OPSORDER=' '
            OPSTOTAL=0
            OPSJOKER=0
600    CONTINUE

            READ(OPS_LUN, END=700, ERR=900,IOSTAT=ST) OPS_REC
            GANOCONCURSOCHAVE=' '
            BILHETEI1=0
            BILHETEI1=CTOI(OPS_REC.BILHETE (4:11),SZ)
            GANOCONCURSOCHAVE=OPS_REC.GAME // OPS_REC.YEARWEEK
            AGTOPS=CTOI(OPS_REC.AGENT,SZ)
         
            IF (ST.EQ.36) THEN
              TYPE*, IAM(),'VPJMC - Não encontro a OP do Bilhete', BIL
              TYPE*,IAM(), '    '
C              RETURN
            ENDIF
         
            IF (BILHETEI1 .NE. SSER) THEN
              WRITE(PUNIT,1900) VALREC(VGAM),                  ! GAME NUMBER
     *                          DAT(VJUL),                     ! 
     *                          SSER,                          ! EXTERNAL SERIAL
     *                          SCHK,                          !
     *                          2000+DAT(VYEAR),               ! SELLING YEAR
     *                          DAT(VMON),                     ! SELLING MOTH
     *                          DAT(VDAY),                     ! SELLING DAY
     *                          AGT,                           ! AGT NUMBER 
     *                          STATAUX,                       ! STATUS
     *                          PAYAMT,                        ! PAY AMOUNT
     *                          VALREC(VKPAMT)                 ! KIKER AMT
     
              CNTREC = CNTREC + 1
              
              DO 1001 I=1,AUX-1
                WRITE(PUNIT,810) CPRIZES(I)
                CNTREC = CNTREC + 1
1001    CONTINUE
              CLOSE (OPS_LUN)
              RETURN
            ENDIF
            
            IF ((BILHETEI1 .EQ. SSER) .AND. (GGAAAACCC .NE. GANOCONCURSOCHAVE)) THEN
              OPSORDER=' '
              OPSTOTAL=0
              OPSJOKER=0
              GOTO 600
            ENDIF

            OPSTOTAL=OPS_REC.TOTAL_GAME
            OPSJOKER=OPS_REC.TOTAL_JOKER
            OPSORDER=OPS_REC.ORDER

          ENDIF
700       CLOSE (OPS_LUN)

900       CLOSE (OPS_LUN)

          IF (ST.EQ.36) THEN
            TYPE*, IAM(),'VPJMC - Não encontro a OP do Bilhete', BIL
            TYPE*,IAM(), '    '
            NOOPS = 1
            ST = 0 
C            RETURN
          ENDIF
          
          IF (ST.NE.0) THEN  
            TYPE*,IAM(),'VPJMC - Erro ao ler o ficheiro de OPS'
            TYPE*, IAM(),'       '
            RETURN
CC            CALL GSTOP (GEXIT_FATAL)
          ENDIF
       
          IF (NOOPS .EQ. 1) THEN
            WRITE(PUNIT,1900) VALREC(VGAM),                  ! GAME NUMBER
     *                        DAT(VJUL),                     ! JUL
     *                        SSER,                          ! EXTERNAL SERIAL
     *                        SCHK,                          ! CHK NUMBER
     *                        2000+DAT(VYEAR),               ! SELLING YEAR
     *                        DAT(VMON),                     ! SELLING MOTH
     *                        DAT(VDAY),                     ! SELLING DAY
     *                        AGT,                           ! AGT NUMBER
     *                        STATAUX,                       ! STATUS
     *                        PAYAMT,                        ! PAY AMOUNT
     *                        VALREC(VKPAMT)                 ! KIKER AMT
            NOOPS = 0
          ELSE
            WRITE(PUNIT,1901) VALREC(VGAM),                  ! GAME NUMBER
     *                        DAT(VJUL),                     ! JUL
     *                        SSER,                          ! EXTERNAL SERIAL
     *                        SCHK,                          ! CHK NUMBER
     *                        2000+DAT(VYEAR),               ! SELLING YEAR
     *                        DAT(VMON),                     ! SELLING MOTH
     *                        DAT(VDAY),                     ! SELLING DAY
     *                        AGT,                           ! AGT NUMBER 
     *                        STATAUX,                       ! STATUS
     *                        PAYAMT,                        ! PAY AMOUNT
     *                        VALREC(VKPAMT),                ! KIKER AMT
     *                        OPSORDER,                      ! Nº ORDER
     *                        OPSTOTAL,                      ! VALUE OPS/GAME
     *                        OPSJOKER                       ! VALUE OPS/KIK
          ENDIF
       
          OPSORDER=' '
          OPSTOTAL=0
          OPSJOKER=0
       
        ELSEIF (VALOR .GT. 0 .AND. VALREC(VOPSCNT) .EQ. 0) THEN !V08
          
          WRITE(PUNIT,1902) VALREC(VGAM),                  ! GAME NUMBER
     *                      DAT(VJUL),                     ! 
     *                      SSER,                          ! EXTERNAL SERIAL
     *                      SCHK,                          !
     *                      2000+DAT(VYEAR),               ! SELLING YEAR
     *                      DAT(VMON),                     ! SELLING MOTH
     *                      DAT(VDAY),                     ! SELLING DAY
     *                      AGT,                           ! AGT NUMBER 
     *                      STATAUX,                       ! STATUS
     *                      PAYAMT,                        ! PAY AMOUNT
     *                      VALREC(VKPAMT),                ! KIKER AMT
     *                      VALREC(VOPSAMT),               ! NET PRIZE
     *                      VALREC(VKOPSAMT)               ! KIK NET PRIZE

        ELSE 
          WRITE(PUNIT,1900) VALREC(VGAM),                  ! GAME NUMBER
     *                      DAT(VJUL),                     ! 
     *                      SSER,                          ! EXTERNAL SERIAL
     *                      SCHK,                          !
     *                      2000+DAT(VYEAR),               ! SELLING YEAR
     *                      DAT(VMON),                     ! SELLING MOTH
     *                      DAT(VDAY),                     ! SELLING DAY
     *                      AGT,                           ! AGT NUMBER 
     *                      STATAUX,                       ! STATUS
     *                      PAYAMT,                        ! PAY AMOUNT
     *                      VALREC(VKPAMT)                 ! KIKER AMT
C          ENDIF
        ENDIF
       
        CNTREC = CNTREC + 1
       
        DO 1000 I=1,AUX-1
          WRITE(PUNIT,810) CPRIZES(I)
          CNTREC = CNTREC + 1
1000    CONTINUE

        CLOSE(OPS_LUN)
	
800     FORMAT('02',I3.3,I4.4,I2.2,I4.4,I1)
810     FORMAT(A83)
1900    FORMAT('01',I2.2,I3.3,I8.8,I3.3,I4.4,I2.2,I2.2,I7.7,A4,I10.10,I10.10,26(' '))
1901    FORMAT('01',I2.2,I3.3,I8.8,I3.3,I4.4,I2.2,I2.2,I7.7,A4,I10.10,I10.10,A6,I10.10,I10.10)
1902    FORMAT('01',I2.2,I3.3,I8.8,I3.3,I4.4,I2.2,I2.2,I7.7,A4,I10.10,I10.10,6(' '),I10.10,I10.10) !V08
903     FORMAT(A4)
904     FORMAT(I3.3,I8.8,I3.3)

       END
