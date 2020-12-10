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
 
C      character*8 DATEI
C      character*10 TIMEI
C      character*20 LOGDATEI
C      character*80 PATHI 
            
C      PATHI = 'DKD10:[DMIL.WRK.HMC.EXAMPLES.LOGFILES]logs.dat' 

      CALL OPSTXT(' Copyright 2014 SCML. All rights reserved. ') 
      CALL SNIF_AND_WRKSET 

      TASK    = OLM
      CALL BUILD_MSG(MESS,1, TASK) 

C      MESSERIAL = 0
      CONOLM = .FALSE.
      FIRSTRUN = .FALSE.

      CALL OPSTXT(' ******************* COMOLM ******************* ')

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
            CALL MESSQ_EXIT(%REF(ST))
            P(OLMCONF) = 0 
            CALL OPSTXT('ERROR!!!! CAN NOT ATTACH TO MESSAGEQ!!!!')

C            CALL date_and_time(DATEI,TIMEI) 

C            LOGDATEI = DATEI(7:8)//'/'//DATEI(5:6)//'/'//DATEI(1:4)
C     &            //'  '//TIMEI(1:2)//':'//TIMEI(3:4)//':'//TIMEI(5:6)

C            open(UNIT=1234, FILE=PATHI, ACCESS='append', STATUS='old')
C            write(1234,*) LOGDATEI//' - ERROR!!!! CAN NOT ATTACH TO MESSAGEQ!!!!'
C            close(1234)   
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

C     send in the HEADER Julia Date from Millennium value   
C     DAYJUL   ->    CURRENT JULIAN DATE
C     BUFNUM() = DAYJUL     

C     MESSERIAL = MESS_FROM_OLM(MESSAGEID_POS) !MESSAGEID -> MESSERIAL

      CALL SENDTOOLM(BUFNUM,ST) 
      IF ((ST .NE. PAMS__SUCCESS ) .AND. (ST .NE. PAMS__TIMEOUT)) THEN
         CALL MESSQ_EXIT(%REF(ST))
         IF (ST .EQ. PAMS__SUCCESS) THEN 
            CONOLM = .FALSE.  
C           before removing from the application queue send the queue as error to dispat to save as error in the tmf as error transaction
            HPRO(TRCODE,BUFNUM)=TYPERR    
            CALL QUETRA(ERR,BUFNUM)         

C 4.1	Cancellation Terminal -> Central (so its registered but not possible to send response to Olimpo then cancel to normalize the information)
C     Bytes
C     Start	End	Size	Field Contents
C     1	1	1	Control	Sequence
C     2	2	1	Type = 2	Subtype = n
C     3	4	2	Checksum
C     5	5	1	Statistics (see below)
C     6	7	2	Wager Julian Date
C     8	10	3	Wager Serial Number
C     11	11	1	Wager Check Digits

C     CODE = HPRO(TRCODE,BUF)
C     CODE.EQ.TYPDEL -> INTERNAL CANCELLATION

C     construct a new buffer(message)
C     HPRO(TRCODE,BUFNUM) = TYPDEL
C     CALL ABL(BUFNUM,QUETAB(1,DIS),ST)

C GLOBAL.DEF

C TERR -> NOER (No Error) - SYNT (Synthesis Error) - RETY (Retry Error) - DESMOD (???) - SUPR (Supression Error) - BSTS - NOTON - SDRW - SDOR
C Values:		Description
C 1 =		Invalid
C 2 =		Syntax Error
C 3 =		Function Suppressed
C 4 =		Not Signed On
C 5 =		System Dormant
C 6 =		Drawing Break
C 7 =		Liability Limit Exceeded
C 8 =		Non-drawing Day
C 9 =		DES Encryption Error
C 10 =		Results Not In
C 11 =		Invalid Terminal Number -> TBAD=11
C 12 =		Security Violation (>10 sign-on)
C 13 =		Bad Checksum
C 14 =		Bad Game Status
C 15 =		Retry Transaction
C 18 =		Game Revision Error
C 19 =		Can Not Fraction Ticket
C 20 =		Already Fractioned
C 21 =		Unfractioned
C 22 =		Card Present (Pass # mismatch)
C 23 =		No Card Present at SON (Do not SON)
C 25 =		Sports, Wrong number of marks with an event cancelled
C 26 =		Sports, The full draw has been cancelled.
C 31 =		Combination Closed
C 32 =		Odds exceeded
C 36 =         	Invalid Agent Or Password In Sign On -> (BTOPSN = 36)
C 37 =		Not available passive number found
C 38 =		Passive IO error
C 39 =		Blocked NIF  
C 40 =            PROCOM Buffer Unavailable         

            
C            IF(TRABUF(TSTAT).NE.GOOD) THEN !quer seja sucesso ou erro o 1º byte têm sempre Control e Sequence
C                  OUTTAB(2) = ERRTYP !segundo byte têm o Type = 9 e Subtype = 0
C                  byte 3 e 4 é usado para guardar o checksum  (validado no fim pois tb é campo obrigatorio em caso de sucesso)          
C                  OUTTAB(5) = TRABUF(TERR) !Error Number ou seja mensagem de erro que ficou durante o processamento
C                  OUTTAB(6) = TRABUF(TSUBERR) !não está na documentação, é usado???
C                  OUTLEN=6
C                  GOTO 1000
C              ENDIF

C           move to try a second time only after fails a second time will remove permentaly (see if that makes more sense)
C            CALL RTL (BUFNUM, QUETAB(1, OLM), STAT)
       
            GOTO 543
         ENDIF
         CALL OPS('ERROR: MESSAGEQ CAN NOT BE DETACHED!!!',ST,0)
         GOTO 10
      ENDIF
      CALL RTL (BUFNUM, QUETAB(1, OLM), STAT)

      GOTO 570

600   CONTINUE            

      IF (P(OLMCONF) .EQ. 0) THEN 
            GOTO 333
      ENDIF

      ST = PAMS__NOMOREMSG 
      IF (P(OLMCONF) .NE. 0 ) THEN 
            CALL GETFROMOLM(ST) 
      ENDIF


C meter dentro do getfromolm e obter os valores dos campos control e sequence da própria mensagem recebida      
C adicionar header para enviar messageid (8bytes), agent number (4bytes) = TERMINALNUM, serial (Olimpo), current Julian date 
C     DAYJUL   ->    CURRENT JULIAN DATE ;;;;; BUFNUM() = DAYJUL 
C        
      IF(ST .EQ. -1) THEN
            GOTO 570  
      ENDIF  
        
      IF ((ST .NE. PAMS__SUCCESS) .AND. (ST .NE. PAMS__NOMOREMSG)) THEN
            CALL MESSQ_EXIT(%REF(ST)) 
            IF (ST .EQ. PAMS__SUCCESS) THEN
                  CONOLM = .FALSE.
                  GOTO 543
            ENDIF
            CALL OPS('ERROR: MESSAGEQ CAN NOT BE DETACHED!!!',ST,0)
            GOTO 10 
      ENDIF
    
      IF(ST .EQ. PAMS__NOMOREMSG) THEN
            WTFORMESS = .TRUE.
            GOTO 570 
      ELSE IF(ST .EQ. PAMS__SUCCESS) THEN    
            GOTO 570 
      ENDIF

      END      

C   TERMINALNUM -> Agent Number (External)
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

            INTEGER*4  MESS(EDLEN)

            INTEGER*4 I4TEMP
            INTEGER*2 I2TEMP(2)
            BYTE      I1TEMP(4)
            EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)

            INTEGER*8 I8AUX
            INTEGER*4 I4AUX(2)
            INTEGER*1 I1AUX(8)
            EQUIVALENCE(I8AUX,I4AUX,I1AUX)       

            INTEGER*4 ST,STATUS,MTYPE,MSUBTYPE
            INTEGER*4 XRFNUM,AGENTNR   
            
            character*34 XRFNUMSTR, AGENTNRSTR, MTYPESTR, MSUBTYPESTR

            COMMON /FROM_OLM/ MESS_FROM_OLM, MESS_FROM_LEN
            BYTE MESS_FROM_OLM(1024) 
            INTEGER*4 MESS_FROM_LEN
C           MESSERIAL is MessageID used as identification of the Message           
            INTEGER*4 MESSERIAL, TYPE, SUBTYPE, TERMINALNUM, AGENT_NUM, I, MYCHKSUM, ERRTYP
C            , TEMP1, TEMP2
            INTEGER*8 MESSAGEID

C for messageid 8 bytes            INTEGER*4 MESSAGEID_POS /1/,TERMINAL_NUM_POS /9/, AGENT_NUM_POS /11/, SERIAL_OLM_POS /15/
            INTEGER*4 MESSAGEID_POS /1/,TERMINAL_NUM_POS /6/, AGENT_NUM_POS /8/, SERIAL_OLM_POS /12/            
            LOGICAL  DMPDBG
            DATA    ERRTYP /Z90/            
            BYTE       ERRMSG(5)                       

C            character*8 DATE
C            character*10 TIME
C            character*20 LOGDATE
C            character*80 PATH  
               
            
            INTEGER*4 APPQUE 
            INTEGER*4 PROBUF /0/
            
C            PATH = 'DKD10:[DMIL.WRK.HMC.EXAMPLES.LOGFILES]logs.dat'            

            ST = 0
            TYPE = 0 
            SUBTYPE = 0
            TERMINALNUM = 0
20          CONTINUE


            CALL MESSQ_GET(%REF(STATUS))   

            IF (STATUS .EQ. PAMS__SUCCESS) THEN   
  

80                CONTINUE

                  CALL GETBUF(PROBUF)
                  CALL FASTSET(0, PRO(1,PROBUF), PROLEN)

C                 MESSAGEID
C                  I1AUX(1) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  0))
C                  I1AUX(2) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  1))
C                  I1AUX(3) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  2))
C                  I1AUX(4) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  3))
C                  I1AUX(5) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  4))
C                  I1AUX(6) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  5))
C                  I1AUX(7) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  6))
C                  I1AUX(8) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  7))
C                 MESSAGEID = I8AUX

                  I1TEMP(1) = ZEXT (MESS_FROM_OLM(TERMINAL_NUM_POS +  0))
                  I1TEMP(2) = ZEXT (MESS_FROM_OLM(TERMINAL_NUM_POS +  1))
                  I1TEMP(3) = 0
                  I1TEMP(4) = 0
                  TERMINALNUM = I4TEMP

                  I1TEMP(1) = ZEXT (MESS_FROM_OLM(AGENT_NUM_POS +  0))
                  I1TEMP(2) = ZEXT (MESS_FROM_OLM(AGENT_NUM_POS +  1))
                  I1TEMP(3) = ZEXT (MESS_FROM_OLM(AGENT_NUM_POS +  2))
                  I1TEMP(4) = ZEXT (MESS_FROM_OLM(AGENT_NUM_POS +  3))
                  AGENT_NUM = I4TEMP

C                  I1AUX(1) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  0))
C                  I1AUX(2) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  1))
C                  I1AUX(3) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  2))
C                  I1AUX(4) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  3))
C                  I1AUX(5) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  4))
C                  I1AUX(6) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  5))
C                  I1AUX(7) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  6))
C                  I1AUX(8) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  7))
C                  MESSERIAL = I8AUX                  

C                  AGENT_NUM = MESS_FROM_OLM(AGENT_NUM_POS)
C                 !AGENT NUMBER
C                  AGENT_NUM = MESS_FROM_OLM(AGENT_NUM_POS) 
C                  TERMINALNUM = MESS_FROM_OLM(TERMINAL_NUM_POS) 
C                 !MESSAGEID -> MESSERIAL                  
C                  MESSERIAL = MESS_FROM_OLM(MESSAGEID_POS) 
                  
C                  SERIAL_OLM = MESS_FROM_OLM(SERIAL_OLM_POS)                  

C                 se for diferente de 0000 então está defenido o terminal number no header
                  IF(TERMINALNUM .EQ. 0) THEN
                        CALL FIND_AGENT(AGENT_NUM,TERMINALNUM,ST)
                  ENDIF

C                 AGTN = AGTTAB(AGTNUM,AGT)  obter o agente number no AGTTAB apartir do terminal number e comparar que esse agent number é igual ao recebido no header caso contrario há uma falha nos dados enviados e é retornado uma mensagem de erro                
                  IF(AGENT_NUM .NE. AGTTAB(AGTNUM,TERMINALNUM) ) THEN
                        TYPE*, ' '                                            
                        TYPE*, 'Received in the header umatched Terminal Number:',TERMINALNUM, 'and Agent Number', AGENT_NUM
                        ST = -8
                  ENDIF
                        

                  IF (ST.NE.0) THEN
c                               CALL OPSTXT('FAILED TO RETRIVE TERMINAL NUMBER FOR AGENTNUM:')
                                    CALL OPS('FAILED TO RETRIVE TERMINAL NUMBER FOR AGENTNUM:',TERMINALNUM,TERMINALNUM)
C                              CALL GSTOP (GEXIT_FATAL)
                        ENDIF                   

C apos x tentativas secanhar ver se caio alguma mensagem de resposta na queue aplicacional para ser enviado para o MessageQ (ou pouco provavel pois nesse caso também não tinha buffers livres...)                  
C adicionar uma variabel do vision que indique logo que aconteceu no dia xx as hh horas e mm de minutes uma falta de procom buffers
                  IF (PROBUF.LE.0) THEN
C                       remember that while QUEMES subroutine uses GETBUF thats not true for OPS that uses caixa de email                        
                        CALL OPSTXT('COMOLM - THERE IS NO BUFFER(PROCOM) AVAILABLE IN THE FREE QUEUE')  
                        
C                       MESSAGEID
                        I1AUX(1) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  0))
                        I1AUX(2) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  1))
                        I1AUX(3) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  2))
                        I1AUX(4) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  3))
                        I1AUX(5) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  4))
C                        I1AUX(6) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  5))
C                        I1AUX(7) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  6))
C                        I1AUX(8) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  7))
                        MESSAGEID = I8AUX                        
C saves to program log itself the message
C TERMINALNUM - type - subtype - message id                          
                        TYPE*, ' '                                            
                        TYPE*, 'Unable to allocate PROCOM buffer for Agent Number ', TERMINALNUM, 
     *                   ' TYPE:' ,TYPE , ' SUBTYPE:' , SUBTYPE, ' MESSAGEID:', MESSAGEID
C        (importante)               SEE BETHER METHOD OF WRITING IN ONE LINE INSTEAD OF WRITING IN FOR CYCLE                        
                        DO I=1, MESS_FROM_LEN  
                              TYPE*, MESS_FROM_OLM(I)                               
                        ENDDO   

C                       return error message (case there is no avaible buffer at the moment)                       
                        ST = -10 
C                        RETURN 

C                        CALL XWAIT(250,1,ST) 
C	                  GOTO 80

                  ENDIF

C                 VALIDATE THAT TERMINALNUM IS A VALIDE NUMER (2 bytes max <-> 7 digitos) AND ALSO THAT MESSAGEID IS ALSO VALIDE     
C                 11 =		Invalid Terminal Number -> TBAD=11 

                  IF(TERMINALNUM .LT.1 .OR. TERMINALNUM .GT. NUMAGT) THEN 
C                       TRABUF(TERR) = TBAD
                        ST = -9
C                        RETURN 
                  ENDIF 

                  IF (ST .LT. 0) THEN
C TRABUF(TTRN) !TTRN=TRANSACTION SEQUENCE NUMBER
C                       ERRMSG(1)= '20'X + '00'X !Control + Sequence
C                       Control + Sequence (request) send the same as response in error
                        ERRMSG(1) = MESS_FROM_OLM(1) 
C Type = 9	Subtype = 0 ->  1001 0000 -> 90 (hexadecimal)         
                        ERRMSG(2) = ERRTYP !'90'X <-> !10010000
                        IF(ST .EQ. -10) THEN
                              ERRMSG(5) = INVL !TRABUF(TERR)     
                        ENDIF
                        IF(ST .EQ. -9 .OR. ST .EQ. -8) THEN
                              ERRMSG(5) = TBAD
C !received un unespectade error status                              
                        ELSE 
                              ERRMSG(5) = INVL
                        ENDIF
C                        TEMP1 = ZEXT(MESS_FROM_OLM(3))
C                        TEMP2 = ZEXT(MESS_FROM_OLM(4))    
C !TRABUF(TCHK)                                            
C                        I4CCITT =   ISHFT(TEMP1,8) + TEMP2                        
C                        ERRMSG(3) = I1CCITT(2)
C                        ERRMSG(4) = I1CCITT(1)
C                       Message checksum seed    (TERMINALNUM is the internal of Millennium not the external that is Agent Number)  
                        
C                        TERMINALNUM -> TERMINALNUM
C                        CALL AGT_TO_TERM(INIT_FLAG, TERMINALNUM, TERMINALNUM)
C                        AGTN = AGTTAB(AGTNUM,AGT)

C                        CALL FIND_AGENT(TERMINALNUM,TERMINALNUM,ST)

                        BASECHKSUM = IAND(DAYCDC,'FFFF'X)
                        I4CCITT = IAND(BASECHKSUM+TERMINALNUM,'FFFF'X)  
C                        I4CCITT =  DAYCDC + TERMINALNUM !Olimpo têm que guardar no lado deles o TERMINALNUM ao fazer o sign-on (ver melhor onde está a ser chamado a validação do checksum no fluxo do registo de uma aposta)
                        CALL GETCCITT(ERRMSG,1,5,MYCHKSUM)
                        I4CCITT = MYCHKSUM
                        ERRMSG(3) = I1CCITT(2)
                        ERRMSG(4) = I1CCITT(1)
                        CALL SENDTOOLM(ERRMSG,ST)
C                       ver mais tarde se trata-se se a mensagem de erro foi enviado com sucesso ou falhou no envio (meter a logica de não ter dado sucesso nem timeout na lógica principal numa subroutina e chamar aqui)
                        ST = - 1
                        RETURN 
                  ENDIF                  
 
                  HPRO(PRCSRC,PROBUF)=OLM_COM                
                  HPRO(PRCDST,PROBUF)=0 
                  HPRO(QUENUM,PROBUF)=QIN           
                  HPRO(TRCODE,PROBUF)=TYPREG    
C                 Internimal terminal number of Millennium limited to the (X2X_TERMS=12288) or the same (NUMAGT=12288)                                            
                  HPRO(TERNUM,PROBUF)=TERMINALNUM !TERMINALNUM 
                  PRO(LINENO,PROBUF)=0              
                  HPRO(MSGNUM,PROBUF)=0
                  HPRO(INPLEN,PROBUF)=MESS_FROM_LEN
C new buffer header fields  
C                  PRO(MESSID,PROBUF)=MESSERIAL     
                  BPRO(MESSID_OLM + 0,PROBUF) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  0))
                  BPRO(MESSID_OLM + 1,PROBUF) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  1))
                  BPRO(MESSID_OLM + 2,PROBUF) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  2))
                  BPRO(MESSID_OLM + 3,PROBUF) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  3))
                  BPRO(MESSID_OLM + 4,PROBUF) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  4))
                  BPRO(MESSID_OLM + 5,PROBUF) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  5))
                  BPRO(MESSID_OLM + 6,PROBUF) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  6))
                  BPRO(MESSID_OLM + 7,PROBUF) = ZEXT (MESS_FROM_OLM(MESSAGEID_POS +  7))           

C                  PRO(SEROLM,PROBUF)=SERIAL_OLM
                  BPRO(SEROLM_OLM + 0,PROBUF) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  0))
                  BPRO(SEROLM_OLM + 1,PROBUF) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  1))
                  BPRO(SEROLM_OLM + 2,PROBUF) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  2))
                  BPRO(SEROLM_OLM + 3,PROBUF) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  3))
                  BPRO(SEROLM_OLM + 4,PROBUF) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  4))
                  BPRO(SEROLM_OLM + 5,PROBUF) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  5))
                  BPRO(SEROLM_OLM + 6,PROBUF) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  6))
                  BPRO(SEROLM_OLM + 7,PROBUF) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  7))
                  BPRO(SEROLM_OLM + 8,PROBUF) = ZEXT (MESS_FROM_OLM(SERIAL_OLM_POS +  8))

                  BPRO(CHOLM_OLM) = 1

                  CALL GETTIM(P(ACTTIM))
                  PRO(TIMOFF,PROBUF)=P(ACTTIM) 

                  CALL LIB$MOVC3(MESS_FROM_LEN, MESS_FROM_OLM, BPRO(BINPTAB,PROBUF))

                  IF(P(XXDEBUG).EQ.0) THEN
                        DMPDBG=.FALSE.
                        IF(P(XXDTRLN).EQ.0) DMPDBG=.TRUE.
                        IF(P(XXDTRLN).LT.0) THEN
                              IF(ABS(P(XXDTRLN)).EQ.HPRO(LINENO,PROBUF)) DMPDBG=.TRUE.
                        ENDIF
                        IF(P(XXDTRLN).GT.0) THEN
                              IF(P(XXDTRLN).EQ.HPRO(TERNUM,PROBUF)) DMPDBG=.TRUE.
                        ENDIF
                        IF(DMPDBG) CALL PRTOUT(PROBUF) 
                  ENDIF

C Send to DIS  (for now commented to just write to GTECH$DEBUG.DAT)              
C                 CALL QUETRA(DIS,PROBUF) 
C use ABL subroutine instead of QUETRA since QUETRA haves extra validation to see how many tasks are active at one moment of time (most likely to prevent to many running at the same time)
                  CALL ABL(PROBUF,QUETAB(1,DIS),ST)
C 	  STATUS = GLIST_STAT_FULL or STATUS = GLIST_STAT_GOOD (retornar erro caso a queue aplicacional do Disptacher estiver cheio algo a considerar)
    
C                 faz sentido fazer aqui return...
C                 RETURN
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

C      RETURN
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


      SUBROUTINE SENDTOOLM(SBUF,ST)
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

            INTEGER*8 I8AUX
            INTEGER*4 I4AUX(2)
            INTEGER*1 I1AUX(8)
            EQUIVALENCE(I8AUX,I4AUX,I1AUX)                 
            
            COMMON /TO_OLM/ MESS_TO_OLM, MESS_TO_LEN 
            BYTE MESS_TO_OLM(1024)
            INTEGER*4 MESS_TO_LEN
            INTEGER*4 MESSAGE_TYPE, LIST_INDEX  
            INTEGER*4 SBUF, I, ST, STATUS
C            INTEGER*4 MESSERIAL, SERIAL_OLM, TERMINALNUM, CDC_DATE, JULIAN_DATE - 8 + 2 + 4 + 9 + 4 + 4 = 31
C for Messageid 8 bytes    INTEGER*4 MSG_OFFSET /33/, MESSAGEID_POS /1/, TERMINAL_NUM_POS /9/, AGENT_NUM_POS /11/, SERIAL_NUM_POS /15/
C for Messageid 8 bytes    INTEGER*4 DAYCDC_POS /24/, DAYJUL_POS /28/
            INTEGER*4 MSG_OFFSET /30/, MESSAGEID_POS /1/, TERMINAL_NUM_POS /6/, AGENT_NUM_POS /8/, SERIAL_NUM_POS /12/       
            INTEGER*4 DAYCDC_POS /21/, DAYJUL_POS /25/            
C            BYTE SERIAL_OLM(0:8)
C           MESSAGEID <-> MESSERIAL             

C            TERMINALNUM = PRO(TERNUM,SBUF)
C            MESSERIAL = HPRO(MESSID,SBUF) 
C            SERIAL_OLM = HPRO(SEROLM,SBUF)    
C            CDC_DATE = DAYCDC 
C            JULIAN_DATE = DAYJUL

C            AGTN = AGTTAB(AGTNUM,AGT)
            
C            I8AUX = PRO(MESSID,SBUF)
C            MESS_TO_OLM(MESSAGEID_POS + 0) = I1AUX(1)
C            MESS_TO_OLM(MESSAGEID_POS + 1) = I1AUX(2)
C            MESS_TO_OLM(MESSAGEID_POS + 2) = I1AUX(3)
C            MESS_TO_OLM(MESSAGEID_POS + 3) = I1AUX(4)
C            MESS_TO_OLM(MESSAGEID_POS + 4) = I1AUX(5)
C            MESS_TO_OLM(MESSAGEID_POS + 5) = I1AUX(6)
C            MESS_TO_OLM(MESSAGEID_POS + 6) = I1AUX(7)
C            MESS_TO_OLM(MESSAGEID_POS + 7) = I1AUX(8)
            MESS_TO_OLM(MESSAGEID_POS + 0) = BPRO(MESSID_OLM+0, SBUF)
            MESS_TO_OLM(MESSAGEID_POS + 1) = BPRO(MESSID_OLM+1, SBUF)
            MESS_TO_OLM(MESSAGEID_POS + 2) = BPRO(MESSID_OLM+2, SBUF)
            MESS_TO_OLM(MESSAGEID_POS + 3) = BPRO(MESSID_OLM+3, SBUF)
            MESS_TO_OLM(MESSAGEID_POS + 4) = BPRO(MESSID_OLM+4, SBUF)
C            MESS_TO_OLM(MESSAGEID_POS + 5) = BPRO(MESSID_OLM+5, SBUF)
C            MESS_TO_OLM(MESSAGEID_POS + 6) = BPRO(MESSID_OLM+6, SBUF)
C            MESS_TO_OLM(MESSAGEID_POS + 7) = BPRO(MESSID_OLM+7, SBUF)

            I4TEMP = HPRO(TERNUM,SBUF) 
            MESS_TO_OLM(TERMINAL_NUM_POS + 0) = I1TEMP(1)
            MESS_TO_OLM(TERMINAL_NUM_POS + 1) = I1TEMP(2) 

            I4TEMP = AGTTAB(AGTNUM,TERNUM) !PRO(AGTNUM,SBUF) 
            MESS_TO_OLM(AGENT_NUM_POS+0) = I1TEMP(1)
            MESS_TO_OLM(AGENT_NUM_POS+1) = I1TEMP(2)
            MESS_TO_OLM(AGENT_NUM_POS+2) = I1TEMP(3)
            MESS_TO_OLM(AGENT_NUM_POS+3) = I1TEMP(4)

            MESS_TO_OLM(SERIAL_NUM_POS+0) = BPRO(SEROLM_OLM+0, SBUF)
            MESS_TO_OLM(SERIAL_NUM_POS+1) = BPRO(SEROLM_OLM+1, SBUF)            
            MESS_TO_OLM(SERIAL_NUM_POS+2) = BPRO(SEROLM_OLM+2, SBUF)
            MESS_TO_OLM(SERIAL_NUM_POS+3) = BPRO(SEROLM_OLM+3, SBUF)
            MESS_TO_OLM(SERIAL_NUM_POS+4) = BPRO(SEROLM_OLM+4, SBUF)
            MESS_TO_OLM(SERIAL_NUM_POS+5) = BPRO(SEROLM_OLM+5, SBUF)
            MESS_TO_OLM(SERIAL_NUM_POS+6) = BPRO(SEROLM_OLM+6, SBUF)
            MESS_TO_OLM(SERIAL_NUM_POS+7) = BPRO(SEROLM_OLM+7, SBUF)            
            MESS_TO_OLM(SERIAL_NUM_POS+8) = BPRO(SEROLM_OLM+8, SBUF)

C            DO I=0, 8
C                  SERIAL_OLM(I) = BPRO(SEROLM+I,SBUF) !SEROLM=49
C            ENDDO           
            
C            MESS_TO_OLM(SERIAL_NUM_POS) = SERIAL_OLM !PRO(SEROLM,SBUF)
            I4TEMP = DAYCDC
            MESS_TO_OLM(DAYCDC_POS+0) = I1TEMP(1)
            MESS_TO_OLM(DAYCDC_POS+1) = I1TEMP(2)
            MESS_TO_OLM(DAYCDC_POS+2) = I1TEMP(3)
            MESS_TO_OLM(DAYCDC_POS+3) = I1TEMP(4)            
C            MESS_TO_OLM(DAYCDC_POS) = DAYCDC
            I4TEMP = DAYJUL
            MESS_TO_OLM(DAYJUL_POS+0) = I1TEMP(1)
            MESS_TO_OLM(DAYJUL_POS+1) = I1TEMP(2)
            MESS_TO_OLM(DAYJUL_POS+2) = I1TEMP(3)
            MESS_TO_OLM(DAYJUL_POS+3) = I1TEMP(4)               
C            MESS_TO_OLM(DAYJUL_POS) = DAYJUL
      
            MESS_TO_LEN  = HPRO(INPLEN,SBUF) 
            DO I=1, MESS_TO_LEN
                  MESS_TO_OLM(I+MSG_OFFSET) = BPRO(I,SBUF)
            ENDDO

            CALL MESSQ_PUT(%REF(STATUS)) 

            IF (STATUS .NE. PAMS__SUCCESS) THEN
                  CALL BUILD_MSG(MESS,1, OLM) 
                  CALL BUILD_MSG(MESS,2, TEOLM)
                  CALL BUILD_MSG(MESS,3, 4) 
                  CALL BUILD_MSG(MESS,4, STATUS)
                  CALL QUEMES(MESS)                                 

                  DO I=1, MESS_TO_LEN
                        BPRO(WRKTAB*4-3+I,SBUF) = MESS_TO_OLM(I)
                  ENDDO  
                  
                  TYPE *,'ERROR: WHILE TRY TO PUT INTO MESSAGEQ, STATUS: ',
     *            STATUS 
                  TYPE *,' '
 
                  DO I=1,MESS_TO_LEN
                        TYPE 9998,I,BPRO(WRKTAB*4-3+I,SBUF) 
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