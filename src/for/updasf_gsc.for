C
C UPDASF.FOR                                                                    
C
C V09 29-NOV-2010 FRP Lotto2 Changes
C V07 06-JUN-2001 EPH DO NOT ZERO START / END DATES FOR PASSIVE WHEN OFFLINE (IT WORKS LIKE ONLINE FOR IT)
C V06 20-APR-2001 CS  INCLUDE STORE
C V05 20-MAR-2001 EPH UPDATE X2X SIZE
C V04 07-MAR-2001 EPH UPDATE AGTTON (ON LINE TERMINAL FLAG) 
C V03 16-FEB-2001 EPH UPDATE X2XADRRESS AND OTHER COMM DATA OR CLEAN IT WHEN REC TYPE = 05
C V02 20-DEC-2000 EPH CHANGE TO BE COMPLYANT WITH SCML (PORTUGAL)
C V01 21-SEP-1989 MGM INITIAL RELEASE FOR FINLAND                                 
C                                                                               
C PROGRAM TO READ AGENT MODIFICATIONS FILE SENT FROM SCML AND
C APPLY CHANGES TO ASF FILE                           
C IF AGENT ALREADY EXISTS MODIFY THE RECORD WITH INFORMATION                    
C SENT FROM THE LOTTERY.  IF AGENT DOESN'T EXIST ADD HIM IN.                    
C                                                                               
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
      PROGRAM UPDASF                                                            
      IMPLICIT NONE                                                  
                                                
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF' 
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:AGTINF.DEF'
      INCLUDE 'INCLIB:PRMAGT.DEF'
      INCLUDE 'INCLIB:RECAGT.DEF'
      INCLUDE 'INCLIB:RECSCF.DEF'
      INCLUDE 'INCLIB:DATBUF.DEF'
      INCLUDE 'INCLIB:INTERFACES_REC.DEF'
                                                                               
      INTEGER*4    REP_LUN /TMP1_LUN/
      INTEGER*4    AGTMIL_LUN /TMP2_LUN/
      INTEGER*4    DISP_LUN /6/
      INTEGER*4    ASFFDB(7)
      CHARACTER    CZERO*1/Z0/                                                  
      INTEGER*4    PAGE /0/

      INTEGER*4    OPNSLOT(NUMAGT)                                             
      INTEGER*4    LOOKUP(NUMAGT)  !contains Agent # only.

      INTEGER*4    ST, AGTMIL_ST
      INTEGER*4    I, J, SZ
      INTEGER*4    SLOT
      INTEGER*4    TERMINAL
      INTEGER*4    ANSWER
      INTEGER*4    TERMTYP
      INTEGER*4    GAM, GTYPE
                                
      INTEGER*4   FILNAME(7)            
      CHARACTER   CFILNAME(28)            
      EQUIVALENCE (CFILNAME,FILNAME)

      LOGICAL     ERRO

      CHARACTER   CASFBYT*760
      EQUIVALENCE (CASFBYT,ASFBYT)    !ASFBYT is equivalent to ASFINF


      CALL COPYRITE                                                             
  	                                                                         
      TYPE *                                                                    
      TYPE *,'----------------------------------------------------------------------------'
      TYPE *,'<<<<< Atualização de dados de Agente a partir da Base de Dados da SCML >>>>>'                  
      TYPE *,'----------------------------------------------------------------------------'
      TYPE *                                                                    
C
C     OPEN REPORT FILE
C     ****************
      CALL ROPEN('UPDASF.REP',REP_LUN,ST)      
      IF(ST.NE.0) THEN
         CALL DISPERR (DISP_LUN, 'UPDASF.REP Open error  st - ', ST, ' ', 0, ' ', 0) 
         CALL USRCLOS1 (REP_LUN)
         CALL GSTOP(GEXIT_FATAL)
      ENDIF
                                                                               
      CALL TITLE('LISTA DE ATUALIZACAO DE AGENTES','UPDASF.REP',1,REP_LUN,PAGE,DAYCDC)             
      WRITE(REP_LUN,9000)              
9000  FORMAT(/,1X,130('='),/)
C9000  FORMAT(/,T2,'AGENTE',T12,'OPERACAO',T39, 'AREA AFETADA',T87,'ERRO',T114,'AGENTE',/)

C                                                                               
C     OPEN INPUT FILE FOR CHECKING
C     ****************************
      CALL OPEN_AGTMIL (AGTMIL_LUN, AGTMIL_ST)
      IF (AGTMIL_ST.NE.0) THEN
         CALL DISPERR (DISP_LUN, AGTMIL_REC.ERRSTR, 0, ' ', 0, ' ', 0)
         CALL GSTOP(GEXIT_FATAL)
      ENDIF          
     
      TYPE *,IAM()                                                                    
      TYPE *,IAM(),'Wait...Checking ** AGTMIL.ASC ** input file for errors...'   
                                                                    
      CALL CHECK_AGTMIL_FOR_ERRORS (REP_LUN, ST)	      
      IF (ST.NE.0) THEN
         CALL DISPERR (DISP_LUN, 'Error consisting AGTMIL file', 0, 'Check UPDASF.REP report', 0, ' ', 0)                 
         CALL USRCLOS1(REP_LUN)
         CALL GSTOP(GEXIT_FATAL)
      ENDIF

C
C     CLOSE AND OPEN INPUT FILE AGAIN
C     *******************************
                                     
      CLOSE(AGTMIL_LUN)
                                          
      CALL OPEN_AGTMIL (AGTMIL_LUN, AGTMIL_ST)
      IF (AGTMIL_ST.NE.0) THEN
         CALL DISPERR (DISP_LUN, AGTMIL_REC.ERRSTR, 0, ' ', 0, ' ', 0)
         CALL GSTOP(GEXIT_FATAL)
      ENDIF          
C
C     READ FIRST RECORD HERE TO PROCESS THE HEADER 
C
      CALL READ_AGTMIL (AGTMIL_ST)
      IF (AGTMIL_ST.NE.0) THEN
         CALL DISPERR (DISP_LUN, AGTMIL_REC.ERRSTR, 0, ' ', 0, ' ', 0)
         CALL GSTOP(GEXIT_FATAL)
      ENDIF          
C
C     PROCESS HEADER, SHOW INFO AND CONFIRM THE FILE
C
      TYPE*,IAM(),'Date of this file generation is : ', AGTMIL_REC.DATAHD
                                                                               
      CALL WIMG (5,'Is this the correct file? (Y/N)         ')                  
      CALL YESNO(ANSWER)                                                        
      IF (ANSWER .NE. 1) THEN   
	 CLOSE(AGTMIL_LUN)
         CALL GSTOP (GEXIT_SUCCESS)
      ENDIF
C
C     OPEN AND READ AGENT SALES FILE                                                         
C     ******************************

      CALL OPENW (ASF,SFNAMES(1,ASF),4,0,0,ST)
      IF (ST.NE.0) THEN
934      CONTINUE
         CALL WIMG(5,'ASF nao encontrado, entre localizacao (VOLN:FILNAME)        ')
         READ(5,901) FILNAME
         IF (FILNAME(1).EQ.'    ') GOTO 934
         IF (CFILNAME(1).EQ.'E '.OR.CFILNAME(1).EQ.'e ') CALL GSTOP(GEXIT_OPABORT)
         CALL OPENW (ASF,FILNAME,4,0,0,ST)
         IF (ST.NE.0) THEN
            CALL FILERR (FILNAME,1,ST,0)
         ENDIF
      ENDIF

      CALL IOINIT(ASFFDB,ASF,ASFSEC*256)  ! in bytes, not in sectors

      TYPE*,IAM(),' '
      TYPE*,IAM(),'Wait...Building agent table from ASF.FIL file...'
      TYPE*,IAM(),' '
      SLOT = 0
      DO 50 I = NUMAGT,1,-1                                                          
         CALL FASTSET (0,ASFREC,ASFLEN)
         CALL READW(ASFFDB,I,ASFREC,ST)                                         
         IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),2,ST,I)

         DO J=SAGNO,EAGNO                                                    
            IF(ASFBYT(J).NE.' '.AND.ASFBYT(J).NE.CZERO) THEN 
               CALL ASCBIN(ASFINF,SAGNO,LAGNO,LOOKUP(I),ST)
	       GOTO 50
	    ENDIF
 	 END DO

         SLOT=SLOT+1                                                            
         OPNSLOT(SLOT)=I                                                        
 50   CONTINUE                                                                  

      TYPE *,IAM(),'Number of available slots :',SLOT
      TYPE *,IAM(),' '
      TYPE *,IAM(),' '
      TYPE *,IAM(),'Wait...Updating Agent file with changes...'
      TYPE *,IAM(),' '
C
C     LOOP READING INPUT FILE
C     ***********************                                                                              
C   
      ERRO    = .FALSE.

      DO 500 WHILE (ABS(AGTMIL_ST).NE.144)

         CALL READ_AGTMIL(AGTMIL_ST)
         IF (ABS(AGTMIL_ST).EQ.144 .OR. AGTMIL_REC.RECTYPE.EQ.'TL') GOTO 500
         IF (AGTMIL_ST.NE.0) THEN
            CALL DISPERR (REP_LUN, AGTMIL_REC.ERRSTR, 0, ' ', 0, ' ', 0)
	    ERRO = .TRUE.
            GOTO 500
         ENDIF    
C
C	 JUMP RECORD IF AGENT NUMBER STARTS WITH 35 AND IT IS A BANK TERMINAL
C
	 IF (INT(AGTMIL_REC.AGENT/100000).EQ.35 .AND. AGTMIL_REC.BANK.EQ.'Y') GOTO 500
C
C        CHECK FOR THE ACTION (01=NEW AGENT / 02=CHANGE AGENT DATA / 05=DELETION)
C        AND FIND OUT TERMINAL NUMBER TO BE USED
C
         TERMINAL = 0
         DO I = 1,NUMAGT
            IF (LOOKUP(I).EQ.AGTMIL_REC.AGENT) THEN
               TERMINAL = I
            ENDIF
         ENDDO

         IF (AGTMIL_REC.OPER_TYPE.EQ.'1') THEN
C
C           NEW AGENT
C
	    IF (TERMINAL.NE.0) THEN
               CALL DISPERR (REP_LUN, 'This new agent is already in ASF file : ', AGTMIL_REC.AGENT, 
     *                                'Record number = ', AGTMIL_REC.RECNUM, ' ', 0)
	       ERRO = .TRUE.
               GOTO 500
            ENDIF

            IF (SLOT.LE.0) THEN           
               CALL DISPERR (REP_LUN, 'No terminal number available to agent : ', AGTMIL_REC.AGENT, 
     *                                'Record number = ', AGTMIL_REC.RECNUM, ' ', 0)
	       ERRO = .TRUE.
               GOTO 500
            ELSE
C
C	       GET A FREE TERMINAL NUMBER
C
               TERMINAL = OPNSLOT(SLOT)
               SLOT = SLOT - 1
            ENDIF

         ELSE

            IF (AGTMIL_REC.OPER_TYPE.EQ.'2') THEN
C
C              CHANGE AGENT DATA
C
	       IF (TERMINAL.EQ.0) THEN
                  CALL DISPERR (REP_LUN, 'Agent not found in Agent file : ', AGTMIL_REC.AGENT, 
     *                                   'Record number = ', AGTMIL_REC.RECNUM, ' ', 0)
	          ERRO = .TRUE.
                  GOTO 500
               ENDIF

            ELSE
         
               IF (AGTMIL_REC.OPER_TYPE.EQ.'5') THEN
C
C	          AGENT DELETION (NOT IMPLEMENTED YET)
C               
	          IF (TERMINAL.EQ.0) THEN
                     CALL DISPERR (REP_LUN, 'Agent to delete not found in Agent file : ', AGTMIL_REC.AGENT, 
     *                                      'Record number = ', AGTMIL_REC.RECNUM, ' ', 0)
	             ERRO = .TRUE.
                     GOTO 500
                  ENDIF

               ENDIF

	    ENDIF

         ENDIF
C
C        NOW LET'S GO TO ACTION ITSELF
C        *****************************

         CALL FASTSET (0,ASFREC,ASFLEN)

         IF (AGTMIL_REC.OPER_TYPE.EQ.'1' .OR. AGTMIL_REC.OPER_TYPE.EQ.'2') THEN

            IF (AGTMIL_REC.OPER_TYPE.EQ.'2') THEN
               CALL READW(ASFFDB,TERMINAL,ASFREC,ST)                                         
               IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),2,ST,TERMINAL)
            ENDIF

            CALL BINASC(ASFINF, SAGNO, LAGNO, AGTMIL_REC.AGENT)
            LOOKUP(TERMINAL) = AGTMIL_REC.AGENT	    

            CASFBYT(EATYP:EATYP) = AGTMIL_REC.AGENT_TYPE   !PUT IN LAST ATYP POSITION
    
            CASFBYT(SNAME:ENAME) = AGTMIL_REC.AGENT_NAME
            CALL BINASC(ASFINF, SBUSC, LBUSC, AGTMIL_REC.BUSINESS_TYPE)   
            CASFBYT(SSTRT:ESTRT) = AGTMIL_REC.ADDRESS

            CALL BINASC(ASFINF, SZIPC, LZIPC, CTOI(AGTMIL_REC.ZIP_CODE,SZ))
            CASFBYT(SZIPA:EZIPA) = AGTMIL_REC.ZIP_CODE_NAME
            CASFBYT(STELE+3:ETELE) = AGTMIL_REC.AGENT_PHONE
            CASFBYT(SCONT:ECONT) = AGTMIL_REC.MANAGER_NAME(1:27)

            CASFBYT(SBKOP:EBKOP) = AGTMIL_REC.BANK_OP
            CASFBYT(SBROP:EBROP) = AGTMIL_REC.BRANCH_OP

            CASFBYT(SWANB:EWANB) = AGTMIL_REC.WAGER_ACCOUNT
            CASFBYT(SPANB:EPANB) = AGTMIL_REC.PASSIVE_ACCOUNT
C
C	    MUTUAS
C
            CASFBYT(SWBSU:EWBSU) = AGTMIL_REC.WAGER_BSUD
            CASFBYT(SWESU:EWESU) = AGTMIL_REC.WAGER_ESUD

            CASFBYT(SWBSA:EWBSA) = AGTMIL_REC.WAGER_BSAD
            CASFBYT(SWESA:EWESA) = AGTMIL_REC.WAGER_ESAD
C
C	    PASSIVE
C
            CASFBYT(SPBSU:EPBSU) = AGTMIL_REC.PASSIVE_BSUD
            CASFBYT(SPESU:EPESU) = AGTMIL_REC.PASSIVE_ESUD

            CASFBYT(SPBSA:EPBSA) = AGTMIL_REC.PASSIVE_BSAD
            CASFBYT(SPESA:EPESA) = AGTMIL_REC.PASSIVE_ESAD
C
C	    FILL OLD START DATE BECAUSE OF X2X TEST IN GETAGT
C           SET STAR AND END DATES TO ZERO IF NOT ONLINE (THESE DATES ARE FOR ONLINE CONTROL ONLY)
C

	    IF (AGTMIL_REC.AGENT_TYPE .EQ. '1' .OR. AGTMIL_REC.X2XADDRESS(1:9).NE.'              ') THEN   
               WRITE (CASFBYT(SSDAT:ESDAT),FMT='(A6)') '010101'    !set any valid date to go on x2x test
	    ELSE
               WRITE (CASFBYT(SSDAT:ESDAT),FMT='(A6)') '000000'    
            ENDIF

            CALL BINASC(ASFINF, SSAPN, LSAPN, AGTMIL_REC.SAP_NUMBER)

            CALL BINASC(ASFINF, SCHAN, LCHAN, AGTMIL_REC.SAP_NUMBER)  ! USE CARTEL "CODIGO DA MORADA"
C
C	    FILL ALL PASSWORDS WITH SAME VALUE
C
            CALL BINASC(ASFINF, SPAS1, LPAS1, AGTMIL_REC.AGENT_PASSWORD)
            CALL BINASC(ASFINF, SPAS2, LPAS2, AGTMIL_REC.AGENT_PASSWORD)
            CALL BINASC(ASFINF, SPAS3, LPAS3, AGTMIL_REC.AGENT_PASSWORD)
            CALL BINASC(ASFINF, SPAS4, LPAS4, AGTMIL_REC.AGENT_PASSWORD)
            CALL BINASC(ASFINF, SPAS5, LPAS5, AGTMIL_REC.AGENT_PASSWORD)
            CALL BINASC(ASFINF, SPAS6, LPAS6, AGTMIL_REC.AGENT_PASSWORD)
            CALL BINASC(ASFINF, SPAS7, LPAS7, AGTMIL_REC.AGENT_PASSWORD)
            CALL BINASC(ASFINF, SPAS8, LPAS8, AGTMIL_REC.AGENT_PASSWORD)
C
C	    UPDATE TERMINAL FLAGS
C           *********************
C
            CALL ASCBIN (ASFINF, STTYP, LTTYP, TERMTYP, ST)

            IF (AGTMIL_REC.BANK.EQ.'Y') THEN
               CALL BSET(TERMTYP,AGTBNK)
            ELSE
               CALL BCLR(TERMTYP,AGTBNK)
            ENDIF

	    IF (AGTMIL_REC.AGENT_TYPE .EQ. '1') THEN
               CALL BSET(TERMTYP,AGTTON)
            ELSE
               CALL BCLR(TERMTYP,AGTTON)
            ENDIF
           
            IF (AGTMIL_REC.PRIVILIGED.EQ.'Y') THEN
               CALL BSET(TERMTYP,AGTPRV)
            ELSE
               CALL BCLR(TERMTYP,AGTPRV)
            ENDIF

            CALL BINASC (ASFINF, STTYP, LTTYP, TERMTYP)
C
C	    UPDATE "SALES" GAME FLAGS FROM PASSIVE AND WAGER STATUS
C           *******************************************************
C
            DO GAM=1,MAXGAM
C
C	       SUSPEND ALL 
C
               CALL BSET(ASFGFL(GAM),AGTVAL)
               CALL BSET(ASFGFL(GAM),AGTCAN)
               CALL BSET(ASFGFL(GAM),AGTWAG)

               GTYPE = GNTTAB(GAMTYP,GAM)

               IF (GTYPE.EQ.TPAS) THEN
C
C		  RELEASE VALIDATIONS 
C
                  CALL BCLR(ASFGFL(GAM),AGTVAL)

                  IF  (AGTMIL_REC.PASSIVE_STATUS.EQ.'V') THEN     !PASSIVE SALES
		      CALL BCLR(ASFGFL(GAM),AGTCAN)		  !RELEASE RETORNO
		  ENDIF
               ENDIF

               IF (GTYPE.EQ.TLTO .OR. GTYPE.EQ.TSPT .OR. GTYPE.EQ.TTGL .OR. GTYPE.EQ.TKIK) THEN
                  IF (AGTMIL_REC.WAGER_STATUS.EQ.'V' .AND. AGTMIL_REC.AGENT_TYPE .EQ. '1') THEN  
                     CALL BCLR(ASFGFL(GAM),AGTWAG)
		     CALL BCLR(ASFGFL(GAM),AGTCAN)
		     CALL BCLR(ASFGFL(GAM),AGTVAL)
                  ENDIF
               ENDIF
            ENDDO
C
C           FILL X2XADRRESS AND OTHER COMM FIELDS (FIXED DATA)
C
	    CASFBYT(SXADR:EXADR) = AGTMIL_REC.X2XADDRESS 
	    
	    CASFBYT(SSCLS:ESCLS) = '03'    !STN CLS
	    WRITE(CASFBYT(SXSTN:EXSTN),FMT='(I5.5)') TERMINAL
	    CASFBYT(SXPRT:EXPRT) = '00001'
	    CASFBYT(SDROP:EDROP) = '@ '  
	    CASFBYT(SXGRP:EXGRP) = '000'
	    CASFBYT(SGPHN:EGPHN) = '000000000000'

C
C	    FILL LINHA DISTRIBUICAO / CENTRAL RECEPCAO / STATUS TRANSPORTE
C	    
            CALL BINASC(ASFINF, SLIND, LLIND, AGTMIL_REC.LINHA_DISTRIBUICAO)
C
C	    SOME CENTRALS DO NOT RECEIVE VALUE, SO DO NOT WRITE THEM TO ASF
C
	    IF (AGTMIL_REC.CENTRAL_RECEPCAO.EQ.9900001 .OR.
     *          AGTMIL_REC.CENTRAL_RECEPCAO.EQ.9900002 .OR.
     *          AGTMIL_REC.CENTRAL_RECEPCAO.EQ.9900999 .OR.
     *          AGTMIL_REC.CENTRAL_RECEPCAO.EQ.9900500 .OR.
     *          AGTMIL_REC.CENTRAL_RECEPCAO.EQ.9900501 .OR.
     *          AGTMIL_REC.CENTRAL_RECEPCAO.EQ.9900503 .OR.
     *          AGTMIL_REC.CENTRAL_RECEPCAO.EQ.9900504 ) THEN
                CALL BINASC(ASFINF, SCENR, LCENR, 0)
            ELSE
                CALL BINASC(ASFINF, SCENR, LCENR, AGTMIL_REC.CENTRAL_RECEPCAO)
            ENDIF

            CALL BINASC(ASFINF, SSTTP, LSTTP, AGTMIL_REC.STATUS_TRANSPORTE)
             
         ELSE
C
C           WRITE CODE FOR DELETION HERE
C           PROBABLY YOU WILL HAVE TO: FILL ASDNM WITH AGENT NUMBER DELETED
C                                      CLEAN AGNO(INF) 
C                                      PUT DELETION DATE IN DELD(INF) 
C           BUT NOW THIS WAS CHANGED TO DELETE (CLEAN) COMMUNICATION DATA (NOT AGENT)
C
	    CASFBYT(SSCLS:ESCLS) = '  '    !STN CLS
	    WRITE(CASFBYT(SXSTN:EXSTN),FMT='(I5.5)') 0
	    CASFBYT(SXPRT:EXPRT) = '     '
	    CASFBYT(SDROP:EDROP) = '  '  
	    CASFBYT(SXGRP:EXGRP) = '   '
	    CASFBYT(SGPHN:EGPHN) = '            '

         ENDIF
C
C        WRITE BACK TO ASF
C        *****************
         CALL WRITEW (ASFFDB,TERMINAL,ASFREC,ST)                         
         IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),3,ST,TERMINAL)


500   CONTINUE     !END OF AGTMIL FILE LOOP *** 

      IF (AGTMIL_ST.EQ.-144) THEN
         CALL DISPERR (REP_LUN, AGTMIL_REC.ERRSTR, 0, ' ', 0, ' ', 0)
      ENDIF          

      CALL CLOSASF
      CLOSE(AGTMIL_LUN)
      CALL USRCLOS1(REP_LUN)
  
      IF (ERRO) THEN
         CALL DISPERR(DISP_LUN, 'There were error during ASF update', 0, 
     *                          'Goto to UPDASF.REP report for it', 0, ' ', 0) 
      ENDIF
                                                                               
      CALL GSTOP(GEXIT_SUCCESS)                          

C     ------------
C     SOME FORMATS
C     ------------
9001  FORMAT(A130)              
901   FORMAT(7A4)   

      END   



C	**********************************************************
	SUBROUTINE CHECK_AGTMIL_FOR_ERRORS (REP_LUN, STATUS)
C	**********************************************************
        IMPLICIT NONE                                                  
                                                
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

	INTEGER*4 REP_LUN, STATUS
        INTEGER*4 AGTMIL_ST

	STATUS = 0
C
C       READ FIRST RECORD HERE FOR THE HEADER
C
   	CALL READ_AGTMIL (AGTMIL_ST)
        IF (AGTMIL_ST.NE.0) THEN
          CALL DISPERR (REP_LUN, AGTMIL_REC.ERRSTR, 0, 'Status = ', AGTMIL_ST, ' ', 0)
          STATUS = -1
          RETURN
        ENDIF          
C
C       LOOP READING INPUT FILE
C       ***********************                                                                              
	   
        DO 500 WHILE (ABS(AGTMIL_ST).NE.144)

           CALL READ_AGTMIL(AGTMIL_ST)
           IF (ABS(AGTMIL_ST).EQ.144 .OR. AGTMIL_REC.RECTYPE.EQ.'TL') GOTO 500
           IF (AGTMIL_ST.NE.0) THEN
              CALL DISPERR (REP_LUN, AGTMIL_REC.ERRSTR, 0, 'Status = ', AGTMIL_ST, ' ', 0)
              STATUS = -1
              GOTO 500
           ENDIF          

500     CONTINUE     !END OF AGTMIL FILE LOOP *** 

        IF (AGTMIL_ST.EQ.-144) THEN
           CALL DISPERR (REP_LUN, AGTMIL_REC.ERRSTR, 0, ' ', 0, ' ', 0)
           STATUS = -1
        ENDIF          
	
	RETURN
	END
