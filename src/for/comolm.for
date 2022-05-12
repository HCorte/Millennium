C-----------------------------------------------------------------------
C PROGRAM COMOLM
C-----------------------------------------------------------------------
C COMOLM.FOR
C
C V01 2020-JUL-10 SCML NEW TERMINALS PROJECT - OLM - Creation
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2020 SCML All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
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
      INCLUDE 'INCLIB:OLMCOM.DEF'     
CCCCCCCCCOnly for error symbol that will be comment for prdCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE '($FORDEF)'

      INTEGER*4  MESS(EDLEN)       
      INTEGER*4  TASK              
      INTEGER*4  BUF               
      INTEGER*4  STATUS,ST, STAT
      INTEGER*4  MESSERIAL
      INTEGER*4  BUFNUM
      LOGICAL    CONOLM, FIRSTRUN, WTFORMESS
      INTEGER*4   MYCHKSUM     

	INTEGER*4   OLMERRHANDLER
	EXTERNAL    OLMERRHANDLER        

      CALL OPSTXT(' Copyright 2020 SCML. All rights reserved. ') 
      CALL LIB$ESTABLISH(OLMERRHANDLER)
      CALL SNIF_AND_WRKSET 

      TASK    = OLM
      CALL BUILD_MSG(MESS,1, TASK) 

      CONOLM = .FALSE.
      FIRSTRUN = .FALSE.
      OLMS_ATTACHSTS = 0     !NOT ATTACHED TO MESSAGEQ SERVER
      OLMS_DETACHFLG = 0     !A DETACHED FROM MESSAGEQ SERVER HAS NOT OCCURRED
      OLMS_TOTOKYPUT = 0
      OLMS_TOTOKYGET = 0
      OLMS_TOTERRPUT = 0
      OLMS_TOTERRGET = 0


      CALL CLR_OLMS_ATTACHDATTIM                                              
C      CALL CLR_OLMS_DETACHDATTIM                                                 

      CALL OPSTXT(' ******************* COMOLM RUNNING ******************* ')     

10      CONTINUE
        WTFORMESS = .FALSE. 
        
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Teste fatal error that should lead to killing the current process   C 
C if not treated properly                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      CALL XWAIT(10, 2, ST)    !gives a sleep before triggering a exception
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C condition value 0 correspondes to a Warning see 9.5. Condition Values in documentation   C
C of VSI_PROGRAM_CONCEPTS_VOL_I.pdf, since its LIB$STOP by default its SEVERE the level of C
C of the error severity (so does it revert to 4 even if its passed the value 0???)         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      CALL LIB$STOP(%VAL(FOR$_OPEFAI)) !generates a severe error of severity by default
C      CALL LIB$SIGNAL(%VAL(4)) !generates a error of severity from 1 to 4 being for 4 the most severe
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      CALL LIB$SIGNAL(%VAL(1605868))               
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C insert here the error symbol to test the error handler function OLMERRHANDLER            C
C for example FOR$_FILNAMSPE                                                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC  
C      CALL LIB$SIGNAL(%VAL(FOR$_FILNAMSPE)) ! use this method instead of LIB$STOP                      

      IF (DAYSTS .EQ. DSCLOS)  THEN 
            IF (P(SYSTYP) .EQ. LIVSYS) THEN 
                  CALL MESSQ_EXIT(%REF(ST)) 
                  CALL OPSTXT('OLM IS DISCONNECTED')
                  IF(ST .EQ. PAMS__SUCCESS) THEN
                        CALL GET_OLMS_DETACHDATTIM          !GET DATE DETACHED
                        OLMS_DETACHFLG = 1                  !DETACHED FROM MESSAGEQ SERVER  
                        TYPE *,IAM(),''                                                 !comment D for prd only test purposes only  
                        TYPE *,IAM(),'COMOLM (destach) DISCONNECTED FROM MESSAGEQ IN DAYCLOSE'    !comment D for prd only test purposes only   
                        TYPE *,IAM(),''                                                 !comment D for prd only test purposes only       
                  ELSE                                                                  !comment D for prd only test purposes only                                
                        TYPE *,IAM(),''                                                 !comment D for prd only test purposes only
                        TYPE *,IAM(),'COMOLM (destach) FAILED TO DISCONNECTED FROM MESSAGEQ IN DAYCLOSE, STATUS:',ST
                        TYPE *,IAM(),''                                                 !comment D for prd only test purposes only  
                  ENDIF
            ENDIF
            CONOLM = .FALSE. 
            P(OLMCONF) = 0 
            CALL CLR_OLMS_ATTACHDATTIM      !CLEAR ATTACH TIME STAMP         
            OLMS_ATTACHSTS = 0              !NOT ATTACHED TO MESSAGEQ SERVER 
            OLMS_DETACHFLG = 1              !DETACHED FROM MESSAGEQ SERVER              
            CALL GSTOP(GEXIT_SUCCESS) 
      ENDIF

      IF (P(SYSTYP) .NE. LIVSYS) THEN
            CALL XWAIT(5, 2, ST)   
            FIRSTRUN = .TRUE.
            GOTO 10
      ENDIF

      IF(DAYSTS .EQ. DSSUSP) THEN 
            CALL HOLD(0,STATUS)
            IF(DAYSTS .EQ. DSOPEN) GOTO 10 
            GOTO 10
      ENDIF

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
                  TYPE *,IAM(),''                                                                !comment D for prd only test purposes only                  
                  TYPE *,IAM(),'COMOLM (destach) FAILED TO CONNECT TO MESSAGEQ IN CONOLM TRUE, STATUS:',ST !comment D for prd only test purposes only
                  TYPE *,IAM(),''                                                                !comment D for prd only test purposes only           
                  IF(ST .EQ. PAMS__SUCCESS) THEN
C                       GET DATE AND TIME OF DETACH
                        CALL GET_OLMS_DETACHDATTIM                                      !GET DETCH DATE 
                        OLMS_DETACHFLG = 1                                              !DETACHED FROM MESSAGEQ SERVER 
                        CALL CLR_OLMS_ATTACHDATTIM                                      !CLEAR ATTACH TIME STAMP 
                        OLMS_ATTACHSTS = 0                                              !NOT ATTACHED TO MESSAGEQ SERVER
                  ENDIF      

                  CALL OPSTXT('ERROR!!!! CAN NOT ATTACH TO MESSAGEQ!!!!')
                  CALL OPS('MESSQ_EXIT Status:',ST,ST)   
                  GOTO 10           
            ENDIF

C          GET DATE AND TIME OF ATTACHMENT
           CALL GET_OLMS_ATTACHDATTIM 
           OLMS_ATTACHSTS = 1   
           OLMS_DETACHFLG = 0         

            CALL BUILD_MSG(MESS,2, TEOLM) 
            CALL BUILD_MSG(MESS,3, 1)
            CALL QUEMES(MESS) 
            
            CONOLM = .TRUE. 
            P(OLMCONF) = 1 
      
      ENDIF

333   CONTINUE          
      CALL XWAIT(250,1,ST)      

570   CONTINUE        
C      CALL LISTTOP(BUFNUM, QUETAB(1, OLM), STAT)
       CALL LISTTOP(BUFNUM, COMOLMQUE(1), STAT)

      IF(STAT .EQ. GLIST_STAT_EMPTY) THEN             
            IF(WTFORMESS .EQ. .TRUE.) GOTO 10
            BUFNUM = 0
            GOTO 600
      ENDIF
      ST = PAMS__SUCCESS

15    CONTINUE      
      CALL SENDTOOLM(BUFNUM,ST,.FALSE.) 
      IF ((ST .NE. PAMS__SUCCESS ) .AND. (ST .NE. PAMS__TIMEOUT)) THEN
         TYPE *,IAM(),''                                                !comment D for prd only test purposes only                  
         TYPE *,IAM(),'COMOLM (destach) SEND MESSAGE TO OLIMPO ERROR, STATUS:',ST !comment D for prd only test purposes only
         TYPE *,IAM(),''                                                !comment D for prd only test purposes only             
         CALL MESSQ_EXIT(%REF(ST))
         IF (ST .EQ. PAMS__SUCCESS) THEN 
            CONOLM = .FALSE.           
            HPRO(TRCODE,BUFNUM)=TYPERR              
            CALL QUETRA(ERR,BUFNUM)  
C             GET DATE AND TIME OF DETACH
            CALL GET_OLMS_DETACHDATTIM
            OLMS_DETACHFLG = 1     
            CALL CLR_OLMS_ATTACHDATTIM
            OLMS_ATTACHSTS = 0 
            CALL OPS('ERROR: MESSAGEQ CAN NOT BE DETACHED!!!',ST,0)
            GOTO 543
         ENDIF
     
C         CALL DQUTRA(OLM, BUFNUM)
         CALL OLM_DQUTRA(BUFNUM)
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
C      CALL DQUTRA(OLM, BUFNUM)
      CALL OLM_DQUTRA(BUFNUM)
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
            OLMS_TOTERRGET = OLMS_TOTERRGET + 1
            TYPE *,IAM(),''                                                 !comment D for prd only test purposes only                  
            TYPE *,IAM(),'COMOLM (destach) GET MESSAGE FROM OLIMPO ERROR, STATUS:',ST !comment D for prd only test purposes only
            TYPE *,IAM(),''                                                 !comment D for prd only test purposes only                
            CALL MESSQ_EXIT(%REF(ST)) 
            IF (ST .EQ. PAMS__SUCCESS) THEN
                  CONOLM = .FALSE.
                  CALL GET_OLMS_DETACHDATTIM  
                  OLMS_DETACHFLG = 1
                  CALL CLR_OLMS_ATTACHDATTIM   
                  OLMS_ATTACHSTS = 0                   
                  CALL OPS('GETFROMOLM ERROR: not PAMS__SUCCESS and not PAMS__NOMOREMSG but MESSQ_EXIT!!!',ST,0)
                  GOTO 543
            ENDIF
            CALL OPS('GETFROMOLM ERROR: not PAMS__SUCCESS and not PAMS__NOMOREMSG !!!',ST,0)
            GOTO 10 
      ENDIF
    
      IF(ST .EQ. PAMS__NOMOREMSG) THEN
            WTFORMESS = .TRUE.
C            IF(OLMS_TOTNOMGET .LE. 99) OLMS_TOTNOMGET=OLMS_TOTNOMGET+1
            GOTO 570 
      ELSE IF(ST .EQ. PAMS__SUCCESS) THEN   
            OLMS_TOTOKYGET = OLMS_TOTOKYGET + 1
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
            INCLUDE 'INCLIB:OLMCOM.DEF'            

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
C            INTEGER*8 AUX_ID      

            INTEGER*4 ST,STATUS,MTYPE,MSUBTYPE
            INTEGER*4 XRFNUM,AGENTNR   
            
            character*34 XRFNUMSTR, AGENTNRSTR, MTYPESTR, MSUBTYPESTR
            CHARACTER*5 TERMINALNUM_STR,NUMAGT_STR
            CHARACTER*7 AGENT_NUM_STR
            CHARACTER*12 MESSAGEID_STR

            COMMON /FROM_OLM/ MESS_FROM_OLM, MESS_FROM_LEN
            BYTE MESS_FROM_OLM(1024) 
            INTEGER*4 MESS_FROM_LEN           
            INTEGER*4 MESSERIAL, TYPE, SUBTYPE, TERMINALNUM, AGENT_NUM, I, MYCHKSUM, ERRTYP
            INTEGER*8 MESSAGEID
            INTEGER*4 MESSAGEID_POS /1/, AGENT_NUM_POS /6/, TERMINAL_NUM_POS /10/, SERIAL_OLM_POS /12/   
            INTEGER*4 DAYCDC_POS /21/, DAYJUL_POS /23/, TOTAL_AMOUNT_POS /25/, BUFFER_HEADER_LENTH /29/   

C            LOGICAL  TERFROHEAD /1/
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
80                CONTINUE

                  CALL GETBUF(PROBUF)
C                 may comment this buffer body reset since with length size info all the garbage at the remaining bytes of the buffer are ignored (so no actual need to reset those bytes)                 
                  CALL FASTSET(0, PRO(1,PROBUF), PROLEN)

                  I1TEMP(1) = ZEXT (MESS_FROM_OLM(AGENT_NUM_POS +  3))!0
                  I1TEMP(2) = ZEXT (MESS_FROM_OLM(AGENT_NUM_POS +  2))!1
                  I1TEMP(3) = ZEXT (MESS_FROM_OLM(AGENT_NUM_POS +  1))!2
                  I1TEMP(4) = ZEXT (MESS_FROM_OLM(AGENT_NUM_POS +  0))!3
                  AGENT_NUM = I4TEMP

                  I1TEMP(1) = ZEXT (MESS_FROM_OLM(TERMINAL_NUM_POS +  1))!1
                  I1TEMP(2) = ZEXT (MESS_FROM_OLM(TERMINAL_NUM_POS +  0))!0
                  I1TEMP(3) = 0
                  I1TEMP(4) = 0
                  TERMINALNUM = I4TEMP

                  IF(TERMINALNUM .LT.1 .OR. TERMINALNUM .GT. NUMAGT) THEN 
C                       If its diferent from 0000 then terminal number is defined in the header
                        IF(TERMINALNUM .EQ. 0) THEN                       
                              CALL FIND_AGENT(AGENT_NUM,TERMINALNUM,ST)
                              IF(ST .EQ. -1)THEN                           
                                    TYPE*, ' '                                            
                                    TYPE*, 'FAILED TO RETRIVE TERMINAL NUMBER FOR AGENTNUM:',AGENT_NUM
                                    TYPE*, ' '                        
                                    CALL OPS('FAILED TO RETRIVE TERMINAL NUMBER FOR AGENTNUM:',AGENT_NUM,AGENT_NUM)
                                    I1AUX(1) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  4))
                                    I1AUX(2) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  3))
                                    I1AUX(3) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  2))
                                    I1AUX(4) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  1))
                                    I1AUX(5) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  0))  
                                    OLMS_GETTERFAI = I8AUX
                                    ST = -8
                              ENDIF
                        ELSE
                              write(NUMAGT_STR,200) NUMAGT
                              write(TERMINALNUM_STR,200) TERMINALNUM
                              TYPE*, ' '
                              TYPE*, ' TERMINAL CODE IS INVALID LOWER THEN 1 OR GREATER THEN ',NUMAGT_STR,
     *                  ' THE TERMINAL CODE RECEIVED IS ',TERMINALNUM_STR
                              CALL OPSTXT(' TERMINAL CODE IS INVALID LOWER THEN 1 OR GREATER THEN '//NUMAGT_STR//
     *                  ' THE TERMINAL CODE RECEIVED IS '//TERMINALNUM_STR)  
                              I1AUX(1) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  4))
                              I1AUX(2) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  3))
                              I1AUX(3) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  2))
                              I1AUX(4) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  1))
                              I1AUX(5) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  0))
                              OLMS_GETTERFAI = I8AUX
                              ST = -9
                        ENDIF 
                  ELSE
                        IF(AGENT_NUM .NE. AGTTAB(AGTNUM,TERMINALNUM) ) THEN
                              TYPE*, ' '                                            
                              TYPE*, 'Received in the header unmatched Terminal Number:',TERMINALNUM,' and Agent Number:',AGENT_NUM
                              TYPE*, ' '
                              write(TERMINALNUM_STR,200) TERMINALNUM  
                              write(AGENT_NUM_STR,210) AGENT_NUM                            
                              CALL OPSTXT('Received in the header umatched Terminal Number:'//TERMINALNUM_STR
     *                        //' and Agent Number:'//AGENT_NUM_STR)
                              I1AUX(1) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  4))
                              I1AUX(2) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  3))
                              I1AUX(3) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  2))
                              I1AUX(4) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  1))
                              I1AUX(5) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  0))  
                              OLMS_GETTERFAI = I8AUX   
                              ST = -8
                        ENDIF
                  ENDIF                 
                        
C                  IF (ST.NE.0) THEN
C                        TYPE*, ' '                                            
C                        TYPE*, 'FAILED TO RETRIVE TERMINAL NUMBER FOR AGENTNUM:',TERMINALNUM
C                        TYPE*, ' '                        
C                        CALL OPS('FAILED TO RETRIVE TERMINAL NUMBER FOR AGENTNUM:',TERMINALNUM,TERMINALNUM)
C                  ENDIF                   

C apos x tentativas secanhar ver se caio alguma mensagem de resposta na queue aplicacional para ser enviado para o MessageQ (ou pouco provavel pois nesse caso tamb?m n?o tinha buffers livres...)                  
C adicionar uma variabel do vision que indique logo que aconteceu no dia xx as hh horas e mm de minutes uma falta de procom buffers
                  IF (PROBUF.LE.0) THEN
C                       remember that while QUEMES subroutine uses GETBUF thats not true for OPS that uses caixa de email                        
                        
                        I1AUX(1) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  4))
                        I1AUX(2) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  3))
                        I1AUX(3) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  2))
                        I1AUX(4) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  1))
                        I1AUX(5) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  0))
                        MESSAGEID = I8AUX    
                        OLMS_GETMESFAI = MESSAGEID                  
                         
                        TYPE*, ' '                                            
                        TYPE*, 'Unable to allocate PROCOM buffer for Agent Number ', TERMINALNUM, 
     *                   ' TYPE:' ,TYPE , ' SUBTYPE:' , SUBTYPE, ' MESSAGEID:', MESSAGEID
C        (importante)               SEE BETHER METHOD OF WRITING IN ONE LINE INSTEAD OF WRITING IN FOR CYCLE                        
                        DO I=1, MESS_FROM_LEN  
                              TYPE*, MESS_FROM_OLM(I)                               
                        ENDDO

                        write(TERMINALNUM_STR,200) TERMINALNUM  
                        write(MESSAGEID_STR,220) MESSAGEID
                        CALL OPSTXT('Unable to allocate PROCOM buffer for Agent Number '//TERMINALNUM_STR// 
     *                   ' MESSAGEID:'//MESSAGEID_STR) 
C     *                   ' TYPE:'//TYPE//' SUBTYPE:'//SUBTYPE//' MESSAGEID:'//MESSAGEID) 
                        ST = -10 
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
                        ST = - 1
                        RETURN 
                  ENDIF                  

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
                
D                 TYPE *,IAM(),'COMOLM GET'
D                 CALL PRTOUT(PROBUF)

CCCCCCCCCCCCCCCCCCCCCCC Send to Encproi instead of Dispatcher CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                  
C                  PARAMETER (ENCRYPTION_ON='00000008'X) --> ENCRYPTION ON IN PROCOM BUF
C                  P(DESFLG_TYPE) = 0 <---> Encrypted Mode |||| P(DESFLG_TYPE) = 1 <---> Decrypted Mode
C                  IAND(PRO(INPTAB,BUF_NO),ENCRYPTION_ON) --> 8x and value lower then 8x (decrypted) will alls return 0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                  I1AUX(1) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  4)) !BPRO(MESSID_OLM + 4,PROBUF)
                  I1AUX(2) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  3)) !BPRO(MESSID_OLM + 3,PROBUF)
                  I1AUX(3) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  2)) !BPRO(MESSID_OLM + 2,PROBUF)
                  I1AUX(4) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  1)) !BPRO(MESSID_OLM + 1,PROBUF)
                  I1AUX(5) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  0)) !BPRO(MESSID_OLM + 0,PROBUF)

                  OLMS_GETMESSUC = I8AUX 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
                  CALL QUEINP (PROBUF,ST)   
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

            I1AUX(1) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  4))
            I1AUX(2) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  3))
            I1AUX(3) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  2))
            I1AUX(4) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  1))
            I1AUX(5) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  0))
            OLMS_GETMESFAI = I8AUX    

            RETURN
        ENDIF   
        
200     format(I5)
210     format(I7)
220     format(I0)        

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
            INCLUDE 'INCLIB:OLMCOM.DEF' 
      
            INTEGER*4 MESS(EDLEN)
            INTEGER*4 I4TEMP
            INTEGER*2 I2TEMP(2) 
            BYTE      I1TEMP(4)
            EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)     

            INTEGER*8 I8AUX
            INTEGER*4 I4AUX(2)
            INTEGER*1 I1AUX(8)
            EQUIVALENCE(I8AUX,I4AUX,I1AUX)                 
            
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

            MESS_TO_OLM(MESSAGEID_POS + 0) = ZEXT(BPRO(MESSID_OLM+0, SBUF))
            MESS_TO_OLM(MESSAGEID_POS + 1) = ZEXT(BPRO(MESSID_OLM+1, SBUF))
            MESS_TO_OLM(MESSAGEID_POS + 2) = ZEXT(BPRO(MESSID_OLM+2, SBUF))
            MESS_TO_OLM(MESSAGEID_POS + 3) = ZEXT(BPRO(MESSID_OLM+3, SBUF))
            MESS_TO_OLM(MESSAGEID_POS + 4) = ZEXT(BPRO(MESSID_OLM+4, SBUF))

            IF(.NOT. ISERR) THEN
                  TERMINALNUM = HPRO(TERNUM,SBUF) 
                  I4TEMP = TERMINALNUM
                  MESS_TO_OLM(TERMINAL_NUM_POS + 0) = ZEXT(I1TEMP(1))
                  MESS_TO_OLM(TERMINAL_NUM_POS + 1) = ZEXT(I1TEMP(2)) 

                  I4TEMP = AGTTAB(AGTNUM,TERMINALNUM) 
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

            I4TEMP = DAYCDC
            MESS_TO_OLM(DAYCDC_POS+0) = ZEXT(I1TEMP(1))
            MESS_TO_OLM(DAYCDC_POS+1) = ZEXT(I1TEMP(2))
            
            I4TEMP = DAYJUL
            MESS_TO_OLM(DAYJUL_POS+0) = ZEXT(I1TEMP(1))
            MESS_TO_OLM(DAYJUL_POS+1) = ZEXT(I1TEMP(2))

 
            MESS_TO_OLM(TOTAL_AMOUNT_POS + 0) = ZEXT(BPRO(TWTOT_OLM+0, SBUF))
            MESS_TO_OLM(TOTAL_AMOUNT_POS + 1) = ZEXT(BPRO(TWTOT_OLM+1, SBUF))
            MESS_TO_OLM(TOTAL_AMOUNT_POS + 2) = ZEXT(BPRO(TWTOT_OLM+2, SBUF))
            MESS_TO_OLM(TOTAL_AMOUNT_POS + 3) = ZEXT(BPRO(TWTOT_OLM+3, SBUF))


            MESS_TO_LEN_BODY  = HPRO(OUTLEN,SBUF)
            MESS_TO_LEN  = HPRO(OUTLEN,SBUF)+BUFFER_HEADER_LENTH
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC BUFFER_OUTPUT=(OUTAB*4)-4=129          
C            CALL LIB$MOVC3(MESS_FROM_LEN-(BUFFER_HEADER_LENTH), MESS_FROM_OLM(BUFFER_HEADER_LENTH), BPRO(BINPTAB,PROBUF))
            CALL LIB$MOVC3(MESS_TO_LEN_BODY, BPRO(BUFFER_OUTPUT,SBUF), MESS_TO_OLM(BUFFER_HEADER_LENTH))

C            CALL LOGBUF(SBUF,'COMOLM SEND:')            
C            DO I=0, MESS_TO_LEN_BODY
C                  MESS_TO_OLM(BUFFER_HEADER_LENTH+I) = BPRO(BUFFER_OUTPUT+I,SBUF)
C                  CALL OPS('Message Body send',MESS_TO_OLM(BUFFER_HEADER_LENTH+I),MESS_TO_OLM(BUFFER_HEADER_LENTH+I))
C            ENDDO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC           
            CALL MESSQ_PUT(%REF(STATUS)) 
            IF (STATUS .NE. PAMS__SUCCESS) THEN
                  OLMS_TOTERRPUT = OLMS_TOTERRPUT + 1
                  I1AUX(1) = MESS_TO_OLM(MESSAGEID_POS + 0)
                  I1AUX(2) = MESS_TO_OLM(MESSAGEID_POS + 1)
                  I1AUX(3) = MESS_TO_OLM(MESSAGEID_POS + 2)
                  I1AUX(4) = MESS_TO_OLM(MESSAGEID_POS + 3)
                  I1AUX(5) = MESS_TO_OLM(MESSAGEID_POS + 4)
                  OLMS_PUTMESFAI = I8AUX

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
                  ELSE
                        OLMS_TOTTMOPUT = OLMS_TOTTMOPUT + 1      
                  ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            ELSE      
                  I1AUX(1) = MESS_TO_OLM(MESSAGEID_POS + 0)
                  I1AUX(2) = MESS_TO_OLM(MESSAGEID_POS + 1)
                  I1AUX(3) = MESS_TO_OLM(MESSAGEID_POS + 2)
                  I1AUX(4) = MESS_TO_OLM(MESSAGEID_POS + 3)
                  I1AUX(5) = MESS_TO_OLM(MESSAGEID_POS + 4)
                  OLMS_PUTMESSUC = I8AUX                  
            ENDIF

            OLMS_TOTOKYPUT = OLMS_TOTOKYPUT + 1
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
