      PROGRAM COMOLM
      IMPLICIT NONE

      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:PROCOM.DEF'
      INCLUDE 'INCLIB:AGTCOM.DEF'
      INCLUDE 'INCLIB:TASKID.DEF'
      INCLUDE 'INCLIB:PRMLOG.DEF'
      INCLUDE 'INCLIB:CHKSUMCM.DEF'
      INCLUDE 'INCLIB:QUECOM.DEF'
      INCLUDE 'INCLIB:APUCOM.DEF'
      INCLUDE 'INCLIB:IGSCON.DEF'
      INCLUDE 'INCLIB:GLIST.DEF' 
      INCLUDE '(LIB$ROUTINES)'      

      INTEGER*4  MESS(EDLEN)       
      INTEGER*4  TASK              
      INTEGER*4  BUF               
      INTEGER*4  STATUS,ST, STAT
      INTEGER*4  MESSERIAL
      INTEGER*4  BUFNUM
      LOGICAL    CONOLM, FIRSTRUN, WTFORMESS
      INTEGER*4   MYCHKSUM
C      INTEGER*4 TIMEOUT_RETRY /0/, TIMEOUT_RETRY_MAX
C      PARAMETER (TIMEOUT_RETRY_MAX = 3)        

      CALL OPSTXT(' Copyright 2014 SCML. All rights reserved. ') 
      CALL SNIF_AND_WRKSET 

      TASK    = OLM
      CALL BUILD_MSG(MESS,1, TASK) 

C      MESSERIAL = 0
      CONOLM = .FALSE.
      FIRSTRUN = .FALSE.

      CALL OPSTXT(' ******************* COMOLM RUNNING ******************* ')     

10      CONTINUE
        WTFORMESS = .FALSE.            

      IF (DAYSTS .EQ. DSCLOS)  THEN 
            IF (P(SYSTYP) .EQ. LIVSYS) THEN 
                  CALL MESSQ_EXIT(%REF(ST)) 
                  CALL OPSTXT('OLM IS DISCONNECTED')
            ENDIF
            CONOLM = .FALSE. 
            P(OLMCONF) = 0 
            CALL GSTOP(GEXIT_SUCCESS) 
      ENDIF
C      CALL OPSTXT(' not DSCLOS')
C      IF (P(SYSTYP) .NE. LIVSYS) THEN
C            CALL XWAIT(5, 2, ST)   
C            FIRSTRUN = .TRUE.
C            GOTO 10
C      ENDIF
C      CALL OPSTXT(' equal to LIVSYS')
C      IF(DAYSTS .EQ. DSSUSP) THEN 
C            CALL HOLD(0,STATUS)
C            IF(DAYSTS .EQ. DSOPEN) GOTO 10 
C            GOTO 10
C      ENDIF
C      CALL OPSTXT(' not DSSUSP')

543     CONTINUE
        IF (P(OLMCONF) .EQ. 0) THEN 
            GOTO 333
        ENDIF

      IF (CONOLM .EQ. .FALSE.) THEN 
            CALL MESSQ_ATTACH(%REF(ST))
      
            IF (ST .NE. PAMS__SUCCESS) THEN 
                  CALL OPS('MESSQ_ATTCH Status:',ST,ST)
                  CALL MESSQ_EXIT(%REF(ST))
                  P(OLMCONF) = 0 
                  CALL OPSTXT('ERROR!!!! CAN NOT ATTACH TO MESSAGEQ!!!!')
                  CALL OPS('MESSQ_EXIT Status:',ST,ST)   
                  GOTO 10           
            ENDIF

            CALL BUILD_MSG(MESS,2, TEOLM) 
            CALL BUILD_MSG(MESS,3, 1)
            CALL QUEMES(MESS) 
            
            CONOLM = .TRUE. 
            P(OLMCONF) = 1 
      
      ENDIF

333   CONTINUE          
      CALL XWAIT(250,1,ST)      

570   CONTINUE        
      CALL LISTTOP(BUFNUM, QUETAB(1, OLM), STAT)

      IF(STAT .EQ. GLIST_STAT_EMPTY) THEN             
            IF(WTFORMESS .EQ. .TRUE.) GOTO 10
            BUFNUM = 0
            GOTO 600
      ENDIF
      ST = PAMS__SUCCESS

15    CONTINUE      
C      CALL OPSTXT(' vai tentar enviar mensagem para olimpo ')
      CALL SENDTOOLM(BUFNUM,ST,.FALSE.) 
      IF ((ST .NE. PAMS__SUCCESS ) .AND. (ST .NE. PAMS__TIMEOUT)) THEN
C         CALL OPSTXT(' estado de envio falhou')
         CALL MESSQ_EXIT(%REF(ST))
         IF (ST .EQ. PAMS__SUCCESS) THEN 
            CONOLM = .FALSE.           
            HPRO(TRCODE,BUFNUM)=TYPERR  
C            CALL ABL (BUFNUM, QUETAB(1, ERR), STAT)              
            CALL QUETRA(ERR,BUFNUM)                  
            CALL OPS('ERROR: MESSAGEQ CAN NOT BE DETACHED!!!',ST,0)
            GOTO 543
         ENDIF
     
         CALL DQUTRA(OLM, BUFNUM)
         CALL RELBUF(BUFNUM)

C         CALL RTL (BUFNUM, QUETAB(1, OLM), STAT)
C         IF (STAT .EQ. GLIST_STAT_EMPTY) THEN
C               BUFNUM = 0
C         ELSE  
C               CALL RELBUF(BUFNUM) 
C         ENDIF
        
         CALL OPS('ERROR: MESSAGEQ CAN NOT BE DETACHED!!!',ST,0)
         GOTO 10
      ENDIF

C     EITHER IS SUCCESS OR TIMEOUT THE MESSAGE IS REMOVED FROM APP QUEUE OLM
C     IN CASE OF TIMEOUT THERE IS NEED TO BE NORMALIZED LATER ON BY A THIRD PROGRAM
C     TO AVOID DESCRIPANCIES BETWEEN MILL AND OLIMPO
      CALL DQUTRA(OLM, BUFNUM)
      CALL RELBUF(BUFNUM)
C      CALL RTL (BUFNUM, QUETAB(1, OLM), STAT)
C      IF (STAT .EQ. GLIST_STAT_EMPTY) THEN
C            BUFNUM = 0
C      ELSE 
C            CALL RELBUF(BUFNUM) 
C            CALL OPSTXT('SUCCESS OR TIMEOUT REMOVED FROM OLM QUEUE....')   
C      ENDIF

      GOTO 570

600   CONTINUE            

      IF (P(OLMCONF) .EQ. 0) THEN 
            GOTO 333
      ENDIF

      ST = PAMS__NOMOREMSG 
      IF (P(OLMCONF) .NE. 0 ) THEN 
            CALL GETFROMOLM(ST) 
      ENDIF
       
      IF(ST .EQ. -1) THEN
            GOTO 570  
      ENDIF   
      IF ((ST .NE. PAMS__SUCCESS) .AND. (ST .NE. PAMS__NOMOREMSG)) THEN
            CALL MESSQ_EXIT(%REF(ST)) 
            IF (ST .EQ. PAMS__SUCCESS) THEN
                  CONOLM = .FALSE.
                  CALL OPS('GETFROMOLM ERROR: not PAMS__SUCCESS and not PAMS__NOMOREMSG but MESSQ_EXIT!!!',ST,0)
                  GOTO 543
            ENDIF
            CALL OPS('GETFROMOLM ERROR: not PAMS__SUCCESS and not PAMS__NOMOREMSG !!!',ST,0)
            GOTO 10 
      ENDIF
    
      IF(ST .EQ. PAMS__NOMOREMSG) THEN
C            CALL OPSTXT('MESSAGEQ EMPTY')
            WTFORMESS = .TRUE.
            GOTO 570 
      ELSE IF(ST .EQ. PAMS__SUCCESS) THEN  
C            CALL OPSTXT('READED A MESSAGE FROM MESSAGEQ')  
            GOTO 570 
      ENDIF

      END      

C   AGENT_NUM -> Agent Number (External)
C   TERMINALNUM -> Terminal Number (Internal)    
C   MESSERIAL -> MESSAGEID generated and sent by Olimpo   
      SUBROUTINE GETFROMOLM(ST)
      IMPLICIT NONE
            INCLUDE 'INCLIB:SYSPARAM.DEF'
            INCLUDE 'INCLIB:SYSEXTRN.DEF'
            INCLUDE 'INCLIB:GLOBAL.DEF'
            INCLUDE 'INCLIB:CONCOM.DEF'
            INCLUDE 'INCLIB:PROCOM.DEF'
            INCLUDE 'INCLIB:AGTCOM.DEF'
            INCLUDE 'INCLIB:TASKID.DEF'
            INCLUDE 'INCLIB:PRMLOG.DEF'
            INCLUDE 'INCLIB:CHKSUMCM.DEF'
            INCLUDE 'INCLIB:QUECOM.DEF'
            INCLUDE 'INCLIB:DESTRA.DEF'
            INCLUDE 'INCLIB:APUCOM.DEF'
            INCLUDE 'INCLIB:EURCON.DEF'
            INCLUDE 'INCLIB:DATBUF.DEF'

            INTEGER*4  values(8)
            INTEGER*4  MESS(EDLEN)

            INTEGER*4 I4TEMP
            INTEGER*2 I2TEMP(2)
            BYTE      I1TEMP(4)
            EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)

            INTEGER*8 I8AUX
            INTEGER*4 I4AUX(2)
            INTEGER*1 I1AUX(8)
            EQUIVALENCE(I8AUX,I4AUX,I1AUX) 
            INTEGER*8 AUX_ID      

            INTEGER*4 ST,STATUS,MTYPE,MSUBTYPE
            INTEGER*4 XRFNUM,AGENTNR   
            
            character*34 XRFNUMSTR, AGENTNRSTR, MTYPESTR, MSUBTYPESTR

            COMMON /FROM_OLM/ MESS_FROM_OLM, MESS_FROM_LEN
            BYTE MESS_FROM_OLM(1024) 
            INTEGER*4 MESS_FROM_LEN           
            INTEGER*4 MESSERIAL, TYPE, SUBTYPE, TERMINALNUM, AGENT_NUM, I, MYCHKSUM, ERRTYP
            INTEGER*8 MESSAGEID
            INTEGER*4 MESSAGEID_POS /1/, AGENT_NUM_POS /6/, TERMINAL_NUM_POS /10/, SERIAL_OLM_POS /12/   
            INTEGER*4 DAYCDC_POS /21/, DAYJUL_POS /23/, TOTAL_AMOUNT_POS /25/, BUFFER_HEADER_LENTH /29/   

            LOGICAL  DMPDBG
            DATA    ERRTYP /Z90/            
            BYTE    ERRMSG(5)
            INTEGER MESS_BODY                        
               
            
            INTEGER*4 APPQUE 
            INTEGER*4 PROBUF /0/          

            MESS_BODY = (OUTTAB*4)-3
            ST = 0
            TYPE = 0 
            SUBTYPE = 0
            TERMINALNUM = 0
20          CONTINUE

            CALL MESSQ_GET(%REF(STATUS))  

            IF (STATUS .EQ. PAMS__SUCCESS) THEN   
C                  CALL OPSTXT('Get Message From MessageQ from Olimpo')
80                CONTINUE

                  CALL GETBUF(PROBUF)
C                 may comment this buffer body reset since with length size info all the garbage at the remaining bytes of the buffer are ignored (so no actual need to reset those bytes)                 
                  CALL FASTSET(0, PRO(1,PROBUF), PROLEN)

C                  CALL OPS('MESS_FROM_LEN:',MESS_FROM_LEN,MESS_FROM_LEN)

C                  I1AUX(1) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  0)) 
C                  I1AUX(2) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  1)) 
C                  I1AUX(3) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  2)) 
C                  I1AUX(4) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  3)) 
C                  I1AUX(5) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  4)) 
C                  I1AUX(6) = 0
C                  I1AUX(7) = 0
C                  I1AUX(8) = 0

C                  AUX_ID = I8AUX
C                  CALL OPS('MESSAGE ID:',AUX_ID,AUX_ID)

                  I1TEMP(1) = ZEXT (MESS_FROM_OLM(AGENT_NUM_POS +  3))!0
                  I1TEMP(2) = ZEXT (MESS_FROM_OLM(AGENT_NUM_POS +  2))!1
                  I1TEMP(3) = ZEXT (MESS_FROM_OLM(AGENT_NUM_POS +  1))!2
                  I1TEMP(4) = ZEXT (MESS_FROM_OLM(AGENT_NUM_POS +  0))!3
                  AGENT_NUM = I4TEMP

C                  CALL OPS('AGENT_NUM:',AGENT_NUM,AGENT_NUM)

                  I1TEMP(1) = ZEXT (MESS_FROM_OLM(TERMINAL_NUM_POS +  0))!0
                  I1TEMP(2) = ZEXT (MESS_FROM_OLM(TERMINAL_NUM_POS +  1))!1
                  I1TEMP(3) = 0
                  I1TEMP(4) = 0
                  TERMINALNUM = I4TEMP
C                  CALL OPS('TERMINALNUM:',TERMINALNUM,TERMINALNUM)

C                 se for diferente de 0000 então está defenido o terminal number no header
                  IF(TERMINALNUM .EQ. 0) THEN                       
                        CALL FIND_AGENT(AGENT_NUM,TERMINALNUM,ST)

                        IF(ST .EQ. -1)THEN
C                              CALL OPS('FIND_AGENT FAILED -- ST:',ST,ST)
                              ST = -8
C                             return a error message to MessageQ do not allow to process anymore                              
                        ENDIF
                  ENDIF

C                 AGTN = AGTTAB(AGTNUM,AGT)  obter o agente number no AGTTAB apartir do terminal number e comparar que esse agent number é igual ao recebido no header caso contrario há uma falha nos dados enviados e é retornado uma mensagem de erro                
                  IF(AGENT_NUM .NE. AGTTAB(AGTNUM,TERMINALNUM) ) THEN
                        TYPE*, ' '                                            
                        TYPE*, 'Received in the header umatched Terminal Number:',TERMINALNUM, 'and Agent Number', AGENT_NUM
                        ST = -8
                  ENDIF
                        
                  IF (ST.NE.0) THEN
                        CALL OPS('FAILED TO RETRIVE TERMINAL NUMBER FOR AGENTNUM:',TERMINALNUM,TERMINALNUM)
                  ENDIF                   

C apos x tentativas secanhar ver se caio alguma mensagem de resposta na queue aplicacional para ser enviado para o MessageQ (ou pouco provavel pois nesse caso também não tinha buffers livres...)                  
C adicionar uma variabel do vision que indique logo que aconteceu no dia xx as hh horas e mm de minutes uma falta de procom buffers
                  IF (PROBUF.LE.0) THEN
C                       remember that while QUEMES subroutine uses GETBUF thats not true for OPS that uses caixa de email                        
C                        CALL OPSTXT('COMOLM - THERE IS NO BUFFER(PROCOM) AVAILABLE IN THE FREE QUEUE')  
                        
C                       MESSAGEID
                        I1AUX(1) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  0))
                        I1AUX(2) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  1))
                        I1AUX(3) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  2))
                        I1AUX(4) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  3))
                        I1AUX(5) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  4))
                        MESSAGEID = I8AUX                      
                         
                        TYPE*, ' '                                            
                        TYPE*, 'Unable to allocate PROCOM buffer for Agent Number ', TERMINALNUM, 
     *                   ' TYPE:' ,TYPE , ' SUBTYPE:' , SUBTYPE, ' MESSAGEID:', MESSAGEID
C        (importante)               SEE BETHER METHOD OF WRITING IN ONE LINE INSTEAD OF WRITING IN FOR CYCLE                        
                        DO I=1, MESS_FROM_LEN  
                              TYPE*, MESS_FROM_OLM(I)                               
                        ENDDO   
                      
                        ST = -10 
                  ENDIF

                  IF(TERMINALNUM .LT.1 .OR. TERMINALNUM .GT. NUMAGT) THEN 
                        ST = -9
                  ENDIF 

                  BPRO(MESSID_OLM + 4,PROBUF) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  0))
                  BPRO(MESSID_OLM + 3,PROBUF) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  1))
                  BPRO(MESSID_OLM + 2,PROBUF) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  2))
                  BPRO(MESSID_OLM + 1,PROBUF) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  3))
                  BPRO(MESSID_OLM + 0,PROBUF) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  4))

                  BPRO(SEROLM_OLM + 8,PROBUF) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  0))
                  BPRO(SEROLM_OLM + 7,PROBUF) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  1))
                  BPRO(SEROLM_OLM + 6,PROBUF) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  2))
                  BPRO(SEROLM_OLM + 5,PROBUF) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  3))
                  BPRO(SEROLM_OLM + 4,PROBUF) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  4))
                  BPRO(SEROLM_OLM + 3,PROBUF) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  5))
                  BPRO(SEROLM_OLM + 2,PROBUF) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  6))
                  BPRO(SEROLM_OLM + 1,PROBUF) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  7))
                  BPRO(SEROLM_OLM + 0,PROBUF) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  8))                   


                  IF (ST .LT. 0) THEN
C                        CALL OPS('ERROR-> STATUS:',ST,ST)
                        ERRMSG(1) = ZEXT(MESS_FROM_OLM(BUFFER_HEADER_LENTH))        
                        ERRMSG(2) = ZEXT(ERRTYP) 
                        IF(ST .EQ. -10) THEN
                              ERRMSG(5) = ZEXT(INVL)    
                        ENDIF
                        IF(ST .EQ. -9 .OR. ST .EQ. -8) THEN
                              ERRMSG(5) = ZEXT(TBAD)                              
                        ELSE 
                              ERRMSG(5) = ZEXT(INVL)
                        ENDIF

                        IF(ST .NE. -9) THEN
                              BASECHKSUM = IAND(DAYCDC,'FFFF'X)
                              I4CCITT = IAND(BASECHKSUM+TERMINALNUM,'FFFF'X)  
C                        I4CCITT =  DAYCDC + TERMINALNUM !Olimpo têm que guardar no lado deles o TERMINALNUM ao fazer o sign-on (ver melhor onde está a ser chamado a validação do checksum no fluxo do registo de uma aposta)
                              CALL GETCCITT(ERRMSG,1,5,MYCHKSUM)
                              I4CCITT = MYCHKSUM
                              ERRMSG(3) = ZEXT(I1CCITT(2))
                              ERRMSG(4) = ZEXT(I1CCITT(1))
                        ELSE 
                              ERRMSG(3) = ZEXT(0)
                              ERRMSG(4) = ZEXT(0)                              
                        ENDIF

                        BPRO(MESS_BODY + 0,PROBUF) =  ERRMSG(1)
                        BPRO(MESS_BODY + 1,PROBUF) =  ERRMSG(2)
                        BPRO(MESS_BODY + 2,PROBUF) =  ERRMSG(3)
                        BPRO(MESS_BODY + 3,PROBUF) =  ERRMSG(4)
                        BPRO(MESS_BODY + 4,PROBUF) =  ERRMSG(5)
                        HPRO(OUTLEN,PROBUF) = 5

                        CALL SENDTOOLM(PROBUF,ST,.TRUE.)
C                       ver mais tarde se trata-se se a mensagem de erro foi enviado com sucesso ou falhou no envio (meter a logica de não ter dado sucesso nem timeout na lógica principal numa subroutina e chamar aqui)
                        ST = - 1
                        RETURN 
                  ENDIF                  
C                  CALL PRINTDATE() 
                  HPRO(PRCSRC,PROBUF)=OLM_COM                
                  HPRO(PRCDST,PROBUF)=0 
                  HPRO(QUENUM,PROBUF)=QIN           
                  HPRO(TRCODE,PROBUF)=TYPREG                                              
                  HPRO(TERNUM,PROBUF)=TERMINALNUM 
                  PRO(LINENO,PROBUF)=0              
                  HPRO(MSGNUM,PROBUF)=0
                  HPRO(INPLEN,PROBUF)=MESS_FROM_LEN-(BUFFER_HEADER_LENTH)+1  
                  BPRO(CHOLM_OLM,PROBUF) = ZEXT (1)

                  CALL GETTIM(P(ACTTIM))
                  PRO(TIMOFF,PROBUF)=P(ACTTIM) 

                  CALL LIB$MOVC3(MESS_FROM_LEN-(BUFFER_HEADER_LENTH)+1, MESS_FROM_OLM(BUFFER_HEADER_LENTH), BPRO(BINPTAB,PROBUF))

C                  CALL LOGBUF(PROBUF,'COMOLM GET:')                  
D                 TYPE *,IAM(),'COMOLM GET'
D                 CALL PRTOUT(PROBUF)


C                  IF(P(XXDEBUG).EQ.0) THEN
C                        DMPDBG=.FALSE.                        
C                        IF(P(XXDTRLN).EQ.0) DMPDBG=.TRUE.
C                        IF(P(XXDTRLN).LT.0) THEN
C                              IF(ABS(P(XXDTRLN)).EQ.HPRO(LINENO,PROBUF)) DMPDBG=.TRUE.
C                        ENDIF
C                        IF(P(XXDTRLN).GT.0) THEN
C                              IF(P(XXDTRLN).EQ.HPRO(TERNUM,PROBUF)) THEN
C                                    DMPDBG=.TRUE.
C                              ENDIF
C                        ENDIF
C                        IF(DMPDBG) THEN
C                              CALL PRTOUT(PROBUF) 
C                        ENDIF
C                  ENDIF

C                  CALL PRINTDATE() 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                  
C Send to DIS  (for now commented to just write to GTECH$DEBUG.DAT)              
C                 CALL QUETRA(DIS,PROBUF) 
C use ABL subroutine instead of QUETRA since QUETRA haves extra validation to see how many tasks are active at one moment of time (most likely to prevent to many running at the same time)
C                  CALL ABL(PROBUF,QUETAB(1,DIS),ST)
CCCCCCCCCCCCCCCCCCCCCCCSend to Encproi instead of DispatcherCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                  
C                  PARAMETER (ENCRYPTION_ON='00000008'X) --> ENCRYPTION ON IN PROCOM BUF
C                  P(DESFLG_TYPE) = 0 <---> Encrypted Mode |||| P(DESFLG_TYPE) = 1 <---> Decrypted Mode
C                  IAND(PRO(INPTAB,BUF_NO),ENCRYPTION_ON) --> 8x and value lower then 8x (decrypted) will alls return 0
                  CALL QUEINP (PROBUF,ST)
C                  CALL OPSTXT('Result Of Send Message to Queue')
C                  CALL OPS('ST: ',ST,ST)
C 	  STATUS = GLIST_STAT_FULL or STATUS = GLIST_STAT_GOOD (retornar erro caso a queue aplicacional do Disptacher estiver cheio algo a considerar)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
                  ST = STATUS
                  RETURN
            ENDIF 
               
        IF (STATUS .EQ. PAMS__NOMOREMSG) THEN
          ST = STATUS
          RETURN
        ENDIF 

        IF (STATUS .NE. PAMS__NOMOREMSG .AND. (STATUS .NE. PAMS__SUCCESS)) THEN
            ST = STATUS

            CALL OPS('ERROR: BAD STATUS WHILE GET FROM MESSAGEQ!!',ST,0)
            CALL OPSTXT('WILL CONNECT TO FAILOVER HOST')

            RETURN
        ENDIF        

      END

        SUBROUTINE BUILD_MSG(MSG, INDEX, INT_VALUE) 
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'

        INTEGER*4 MSG(EDLEN)
        INTEGER*4 INDEX
        INTEGER*4 INT_VALUE
        
        IF(INDEX .LE. 0 .OR. INDEX .GT. EDLEN) THEN
            CALL OPS('BUILD_MSG: ERROR: INDEX OUT OF BOUNDS',INDEX,EDLEN)
            CALL GSTOP(GEXIT_FATAL)
        ENDIF
        
        MSG(INDEX) = INT_VALUE
        
        RETURN
        END


      SUBROUTINE SENDTOOLM(SBUF,ST,ISERR)
      IMPLICIT NONE
            INCLUDE 'INCLIB:SYSPARAM.DEF'
            INCLUDE 'INCLIB:SYSEXTRN.DEF'
            INCLUDE 'INCLIB:GLOBAL.DEF'
            INCLUDE 'INCLIB:CONCOM.DEF'
            INCLUDE 'INCLIB:PROCOM.DEF'
            INCLUDE 'INCLIB:AGTCOM.DEF'
            INCLUDE 'INCLIB:DESTRA.DEF'
            INCLUDE 'INCLIB:TASKID.DEF'
            INCLUDE 'INCLIB:PRMLOG.DEF'
            INCLUDE 'INCLIB:CHKSUMCM.DEF'
            INCLUDE 'INCLIB:QUECOM.DEF'
            INCLUDE 'INCLIB:APUCOM.DEF'
            INCLUDE 'INCLIB:IGSCON.DEF'
            INCLUDE 'INCLIB:IGSDEBUG.DEF'    
      
            INTEGER*4 MESS(EDLEN)
            INTEGER*4 I4TEMP
            INTEGER*2 I2TEMP(2) 
            BYTE      I1TEMP(4)
            EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)     

C            INTEGER*8 I8AUX
C            INTEGER*4 I4AUX(2)
C            INTEGER*1 I1AUX(8)
C            EQUIVALENCE(I8AUX,I4AUX,I1AUX)                 
            
            COMMON /TO_OLM/ MESS_TO_OLM, MESS_TO_LEN 
            BYTE MESS_TO_OLM(1024)
            INTEGER*4 MESS_TO_LEN, MESS_TO_LEN_BODY
            INTEGER*4 MESSAGE_TYPE, LIST_INDEX  
            INTEGER*4 SBUF, I, ST, STATUS
            INTEGER*4 TERMINALNUM
C            INTEGER*4 MESSERIAL, SERIAL_OLM, TERMINALNUM, CDC_DATE, JULIAN_DATE - 8 + 2 + 4 + 9 + 4 + 4 = 31
C for Messageid 8 bytes    INTEGER*4 MSG_OFFSET /33/, MESSAGEID_POS /1/, TERMINAL_NUM_POS /9/, AGENT_NUM_POS /11/, SERIAL_NUM_POS /15/
C for Messageid 8 bytes    INTEGER*4 DAYCDC_POS /24/, DAYJUL_POS /28/
            INTEGER*4 BUFFER_HEADER_LENTH /29/, MESSAGEID_POS /1/, AGENT_NUM_POS /6/, TERMINAL_NUM_POS /10/, SERIAL_NUM_POS /12/       
            INTEGER*4 DAYCDC_POS /21/, DAYJUL_POS /23/, TOTAL_AMOUNT_POS /25/  
            INTEGER*4 BUFFER_OUTPUT  
C            INTEGER*4 WAGER_AMOUT
            LOGICAL ISERR
            
            BUFFER_OUTPUT = (OUTTAB*4)-3
            

C            CALL OPSTXT('!!!!!!!!!!!!!!!!!!!!Processou a mensagem!!!!!!!!!!!!!!!!!!!!!')

            MESS_TO_OLM(MESSAGEID_POS + 0) = ZEXT(BPRO(MESSID_OLM+0, SBUF))
            MESS_TO_OLM(MESSAGEID_POS + 1) = ZEXT(BPRO(MESSID_OLM+1, SBUF))
            MESS_TO_OLM(MESSAGEID_POS + 2) = ZEXT(BPRO(MESSID_OLM+2, SBUF))
            MESS_TO_OLM(MESSAGEID_POS + 3) = ZEXT(BPRO(MESSID_OLM+3, SBUF))
            MESS_TO_OLM(MESSAGEID_POS + 4) = ZEXT(BPRO(MESSID_OLM+4, SBUF))

C            CALL OPS('MESSAGEID::',ZEXT (MESS_TO_OLM(MESSAGEID_POS +  0)),ZEXT (MESS_TO_OLM(MESSAGEID_POS +  0)))
C            CALL OPS('MESSAGEID::',ZEXT (MESS_TO_OLM(MESSAGEID_POS +  1)),ZEXT (MESS_TO_OLM(MESSAGEID_POS +  1)))
C            CALL OPS('MESSAGEID::',ZEXT (MESS_TO_OLM(MESSAGEID_POS +  2)),ZEXT (MESS_TO_OLM(MESSAGEID_POS +  2)))
C            CALL OPS('MESSAGEID::',ZEXT (MESS_TO_OLM(MESSAGEID_POS +  3)),ZEXT (MESS_TO_OLM(MESSAGEID_POS +  3)))
C            CALL OPS('MESSAGEID::',ZEXT (MESS_TO_OLM(MESSAGEID_POS +  4)),ZEXT (MESS_TO_OLM(MESSAGEID_POS +  4)))            

            IF(.NOT. ISERR) THEN
                  TERMINALNUM = HPRO(TERNUM,SBUF) 
                  I4TEMP = TERMINALNUM
C            CALL OPS('HPRO(TERNUM,SBUF):',ZEXT (HPRO(TERNUM,SBUF)),ZEXT (HPRO(TERNUM,SBUF)))
                  MESS_TO_OLM(TERMINAL_NUM_POS + 0) = ZEXT(I1TEMP(1))
                  MESS_TO_OLM(TERMINAL_NUM_POS + 1) = ZEXT(I1TEMP(2)) 
C            CALL OPS('TERMINAL_NUM::',ZEXT (MESS_TO_OLM(TERMINAL_NUM_POS + 0)),ZEXT (MESS_TO_OLM(TERMINAL_NUM_POS + 0)))
C            CALL OPS('TERMINAL_NUM::',ZEXT (MESS_TO_OLM(TERMINAL_NUM_POS + 1)),ZEXT (MESS_TO_OLM(TERMINAL_NUM_POS + 1)))                        

                  I4TEMP = AGTTAB(AGTNUM,TERMINALNUM) !PRO(AGTNUM,SBUF) 
                  MESS_TO_OLM(AGENT_NUM_POS+0) = ZEXT(I1TEMP(1))
                  MESS_TO_OLM(AGENT_NUM_POS+1) = ZEXT(I1TEMP(2))
                  MESS_TO_OLM(AGENT_NUM_POS+2) = ZEXT(I1TEMP(3))
                  MESS_TO_OLM(AGENT_NUM_POS+3) = ZEXT(I1TEMP(4))
            ELSE
                  MESS_TO_OLM(TERMINAL_NUM_POS + 0) = ZEXT(0)
                  MESS_TO_OLM(TERMINAL_NUM_POS + 1) = ZEXT(0) 
                  
                  MESS_TO_OLM(AGENT_NUM_POS+0) = ZEXT(0)
                  MESS_TO_OLM(AGENT_NUM_POS+1) = ZEXT(0)
                  MESS_TO_OLM(AGENT_NUM_POS+2) = ZEXT(0)
                  MESS_TO_OLM(AGENT_NUM_POS+3) = ZEXT(0)                  
            ENDIF

C            CALL OPS('AGENT_NUM::',ZEXT (MESS_TO_OLM(AGENT_NUM_POS+0)),ZEXT (MESS_TO_OLM(AGENT_NUM_POS+0)))
C            CALL OPS('AGENT_NUM::',ZEXT (MESS_TO_OLM(AGENT_NUM_POS+1)),ZEXT (MESS_TO_OLM(AGENT_NUM_POS+1)))
C            CALL OPS('AGENT_NUM::',ZEXT (MESS_TO_OLM(AGENT_NUM_POS+2)),ZEXT (MESS_TO_OLM(AGENT_NUM_POS+2)))
C            CALL OPS('AGENT_NUM::',ZEXT (MESS_TO_OLM(AGENT_NUM_POS+3)),ZEXT (MESS_TO_OLM(AGENT_NUM_POS+3)))            

C            MESS_TO_OLM(SERIAL_NUM_POS+0) = ZEXT(BPRO(SEROLM_OLM+0, SBUF))
C            MESS_TO_OLM(SERIAL_NUM_POS+1) = ZEXT(BPRO(SEROLM_OLM+1, SBUF))            
C            MESS_TO_OLM(SERIAL_NUM_POS+2) = ZEXT(BPRO(SEROLM_OLM+2, SBUF))
C            MESS_TO_OLM(SERIAL_NUM_POS+3) = ZEXT(BPRO(SEROLM_OLM+3, SBUF))
C            MESS_TO_OLM(SERIAL_NUM_POS+4) = ZEXT(BPRO(SEROLM_OLM+4, SBUF))
C            MESS_TO_OLM(SERIAL_NUM_POS+5) = ZEXT(BPRO(SEROLM_OLM+5, SBUF))
C            MESS_TO_OLM(SERIAL_NUM_POS+6) = ZEXT(BPRO(SEROLM_OLM+6, SBUF))
C            MESS_TO_OLM(SERIAL_NUM_POS+7) = ZEXT(BPRO(SEROLM_OLM+7, SBUF))            
C            MESS_TO_OLM(SERIAL_NUM_POS+8) = ZEXT(BPRO(SEROLM_OLM+8, SBUF))           


C            CALL OPS('SERIAL_NUM::',ZEXT (MESS_TO_OLM(SERIAL_NUM_POS+0)),ZEXT (MESS_TO_OLM(SERIAL_NUM_POS+0)))
C            CALL OPS('SERIAL_NUM::',ZEXT (MESS_TO_OLM(SERIAL_NUM_POS+1)),ZEXT (MESS_TO_OLM(SERIAL_NUM_POS+1)))
C            CALL OPS('SERIAL_NUM::',ZEXT (MESS_TO_OLM(SERIAL_NUM_POS+2)),ZEXT (MESS_TO_OLM(SERIAL_NUM_POS+2)))
C            CALL OPS('SERIAL_NUM::',ZEXT (MESS_TO_OLM(SERIAL_NUM_POS+3)),ZEXT (MESS_TO_OLM(SERIAL_NUM_POS+3)))
C            CALL OPS('SERIAL_NUM::',ZEXT (MESS_TO_OLM(SERIAL_NUM_POS+4)),ZEXT (MESS_TO_OLM(SERIAL_NUM_POS+4)))
C            CALL OPS('SERIAL_NUM::',ZEXT (MESS_TO_OLM(SERIAL_NUM_POS+5)),ZEXT (MESS_TO_OLM(SERIAL_NUM_POS+5)))
C            CALL OPS('SERIAL_NUM::',ZEXT (MESS_TO_OLM(SERIAL_NUM_POS+6)),ZEXT (MESS_TO_OLM(SERIAL_NUM_POS+6)))
C            CALL OPS('SERIAL_NUM::',ZEXT (MESS_TO_OLM(SERIAL_NUM_POS+7)),ZEXT (MESS_TO_OLM(SERIAL_NUM_POS+7)))
C            CALL OPS('SERIAL_NUM::',ZEXT (MESS_TO_OLM(SERIAL_NUM_POS+8)),ZEXT (MESS_TO_OLM(SERIAL_NUM_POS+8)))


            I4TEMP = DAYCDC
            MESS_TO_OLM(DAYCDC_POS+0) = ZEXT(I1TEMP(1))
            MESS_TO_OLM(DAYCDC_POS+1) = ZEXT(I1TEMP(2))
            
C            CALL OPS('DAYCDC::',ZEXT (MESS_TO_OLM(DAYCDC_POS+0)),ZEXT (MESS_TO_OLM(DAYCDC_POS+0)))
C            CALL OPS('DAYCDC::',ZEXT (MESS_TO_OLM(DAYCDC_POS+1)),ZEXT (MESS_TO_OLM(DAYCDC_POS+1)))

            I4TEMP = DAYJUL
            MESS_TO_OLM(DAYJUL_POS+0) = ZEXT(I1TEMP(1))
            MESS_TO_OLM(DAYJUL_POS+1) = ZEXT(I1TEMP(2))

C            CALL OPS('DAYJUL_POS::',ZEXT (MESS_TO_OLM(DAYJUL_POS+0)),ZEXT (MESS_TO_OLM(DAYJUL_POS+0)))
C            CALL OPS('DAYJUL_POS::',ZEXT (MESS_TO_OLM(DAYJUL_POS+1)),ZEXT (MESS_TO_OLM(DAYJUL_POS+1)))
      
            MESS_TO_OLM(TOTAL_AMOUNT_POS + 0) = ZEXT(BPRO(TWTOT_OLM+0, SBUF))
            MESS_TO_OLM(TOTAL_AMOUNT_POS + 1) = ZEXT(BPRO(TWTOT_OLM+1, SBUF))
            MESS_TO_OLM(TOTAL_AMOUNT_POS + 2) = ZEXT(BPRO(TWTOT_OLM+2, SBUF))
            MESS_TO_OLM(TOTAL_AMOUNT_POS + 3) = ZEXT(BPRO(TWTOT_OLM+3, SBUF))

C            CALL OPS('1 byte Wager Amount to Pay:',ZEXT(BPRO(TWTOT_OLM+0, SBUF)),ZEXT(BPRO(TWTOT_OLM+0, SBUF)))
C            CALL OPS('2 byte Wager Amount to Pay:',ZEXT(BPRO(TWTOT_OLM+1, SBUF)),ZEXT(BPRO(TWTOT_OLM+1, SBUF)))
C            CALL OPS('3 byte Wager Amount to Pay:',ZEXT(BPRO(TWTOT_OLM+2, SBUF)),ZEXT(BPRO(TWTOT_OLM+2, SBUF)))
C            CALL OPS('4 byte Wager Amount to Pay:',ZEXT(BPRO(TWTOT_OLM+3, SBUF)),ZEXT(BPRO(TWTOT_OLM+3, SBUF)))
C
C            I1TEMP(1) = ZEXT(BPRO(TWTOT_OLM+0, SBUF))
C            I1TEMP(2) = ZEXT(BPRO(TWTOT_OLM+1, SBUF))
C            I1TEMP(3) = ZEXT(BPRO(TWTOT_OLM+2, SBUF))
C            I1TEMP(4) = ZEXT(BPRO(TWTOT_OLM+3, SBUF))
C            WAGER_AMOUT = I4TEMP
C            CALL OPS('wager amout total:',WAGER_AMOUT,WAGER_AMOUT)

            MESS_TO_LEN_BODY  = HPRO(OUTLEN,SBUF)
            MESS_TO_LEN  = HPRO(OUTLEN,SBUF)+BUFFER_HEADER_LENTH
C            CALL OPS('Message Full Size send',MESS_TO_LEN,MESS_TO_LEN)
C            CALL OPS('starting position (129)?->',BUFFER_OUTPUT,BUFFER_OUTPUT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC BUFFER_OUTPUT=(OUTAB*4)-4=129          
C            CALL LIB$MOVC3(MESS_FROM_LEN-(BUFFER_HEADER_LENTH), MESS_FROM_OLM(BUFFER_HEADER_LENTH), BPRO(BINPTAB,PROBUF))
            CALL LIB$MOVC3(MESS_TO_LEN_BODY, BPRO(BUFFER_OUTPUT,SBUF), MESS_TO_OLM(BUFFER_HEADER_LENTH))

C            CALL LOGBUF(SBUF,'COMOLM SEND:')            
C            DO I=0, MESS_TO_LEN_BODY
C                  MESS_TO_OLM(BUFFER_HEADER_LENTH+I) = BPRO(BUFFER_OUTPUT+I,SBUF)
C                  CALL OPS('Message Body send',MESS_TO_OLM(BUFFER_HEADER_LENTH+I),MESS_TO_OLM(BUFFER_HEADER_LENTH+I))
C            ENDDO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C            CALL OPSTXT('!!!!!!!!!!!!!!!!!!!!Vai tentar enviar mensagem para MessageQ!!!!!!!!!!!!!!!!!!!!!')            
            CALL MESSQ_PUT(%REF(STATUS)) 
C            CALL OPSTXT('Message Sent to MessageQ Status:')            
C            CALL OPS('MessageQ Status:',STATUS,STATUS)
            IF (STATUS .NE. PAMS__SUCCESS) THEN
                  CALL OPSTXT('??????????????  ERROR in sending to MessageQ   ????????????????????????')
C ERRLOG   01/12/2021   ERRLOG  INVALID MESSAGE TYPE>  9 NUMBER>   4       --- TEOLM = 9           
                  CALL BUILD_MSG(MESS,1, OLM) 
                  CALL BUILD_MSG(MESS,2, TEOLM)
                  CALL BUILD_MSG(MESS,3, 4) 
                  CALL BUILD_MSG(MESS,4, STATUS)
                  CALL QUEMES(MESS)                                 
C                  CALL OPSTXT('Showed Error Message by Logger')

C                  DO I=1, MESS_TO_LEN_BODY
C                        CALL OPS('Message Body send',MESS_TO_OLM(BUFFER_HEADER_LENTH+I),MESS_TO_OLM(BUFFER_HEADER_LENTH+I))                        
C                  ENDDO  
                  
                  TYPE *,'ERROR: WHILE TRY TO PUT INTO MESSAGEQ, STATUS: ',
     *            STATUS 
                  TYPE *,' '
 
                  DO I=0,MESS_TO_LEN_BODY
                        CALL OPS('Message Body send',MESS_TO_OLM(BUFFER_HEADER_LENTH+I),MESS_TO_OLM(BUFFER_HEADER_LENTH+I))
                        TYPE 9998,I,MESS_TO_OLM(BUFFER_HEADER_LENTH+I) 
                  ENDDO
                  TYPE *,' ' 
          
9998              FORMAT(' MESSWORD SENT: ',I4.3,' - ', Z3.2) 

                  IF (STATUS .NE. PAMS__TIMEOUT) THEN 
                        CALL OPSTXT('WILL CONNECT TO FAILOVER HOST')
                  ENDIF

            ENDIF

            ST = STATUS
            RETURN
      END

      SUBROUTINE PRINTDATE()
      IMPLICIT NONE
            character*8 DATEI
            character*10 TIMEI
            character*28 LOGDATEI

            CALL date_and_time(DATEI,TIMEI) 

            LOGDATEI = DATEI(7:8)//'/'//DATEI(5:6)//'/'//DATEI(1:4)
     &      //'  '//TIMEI(1:2)//':'//TIMEI(3:4)//':'//TIMEI(5:6)//':'//TIMEI(7:10)   
     
            CALL OPSTXT(LOGDATEI)
      END

C      SUBROUTINE LOGBUF(BUF,MESS)
C      IMPLICIT NONE
C
C            INCLUDE '(LIB$ROUTINES)'
C            INCLUDE '($FORIOSDEF)'
C            INCLUDE 'INCLIB:SYSDEFINE.DEF'
C            INCLUDE 'INCLIB:SYSEXTRN.DEF'
C            INCLUDE 'INCLIB:GLOBAL.DEF'
C            INCLUDE 'INCLIB:PROCOM.DEF'
C
C            INTEGER * 4  BUF       ! INPUT: Buffer number
C
C            INTEGER * 4  J, K      ! Loop counters
C            INTEGER * 4  ST        ! I/O status
C            INTEGER * 4  WST
C            INTEGER * 4  LU        ! Logical unit
C            CHARACTER*10 MESS            
C            
C            open(UNIT=1236, FILE='BUFLOG.DAT', ACCESS='append', STATUS='UNKNOWN', IOSTAT=ST)
C            CALL OPS('STATUS OPEN FILE',ST,ST)
C            Write (1236, MESS)
C            Write (1236, *) IAM()
C            Write (1236, '(''   Buffer     = '', I5   )') BUF
C            Write (1236, '(''   Comm       = '', X, I4)') HPRO(PRCSRC, BUF)
C            Write (1236, '(''   TrCode     = '', I5   )') HPRO(TRCODE  , BUF)
C            Write (1236, '(''   Term Nr    = '', I5   )') HPRO(TERNUM  , BUF)
C    
C            Write (1236, '(''   Line Nr    = '', I5)') HPRO(LINENO  , BUF)
C    
C            Write (1236, '(''   Dest.      = '', I5)') HPRO(X2X_DEST, BUF)
C            Write (1236, '(''   Message    = '', I5)') HPRO(MSGNUM  , BUF)
C            Write (1236, '(''   Length     = '', I5)') HPRO(INPLEN  , BUF)
C            Write (1236, *)            
C
C            DO K=0, HPRO(INPLEN,BUF)-1, 20
C                  Write (1236, '(3X, I3, '':'', 20(X, Z2.2))') 
C     *            K+1, (BPRO(BINPTAB+J, BUF), J=K, 
C     *                MIN(K+19, HPRO(INPLEN, BUF)-1))
C                ENDDO            
C
C            close(1236)
C
C      END