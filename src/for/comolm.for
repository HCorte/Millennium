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
      INCLUDE 'INCLIB:PRMPRO.DEF' !from igscommon
      INCLUDE 'INCLIB:GLIST.DEF' !from igscommon 
      INCLUDE '(LIB$ROUTINES)' 

C     file:///C:/Users/105864/Desktop/MessageQ-Fortran/MessageQ.pdf

C YE -> YES || TS -> TESTS || NO -> NO || VL -> VALIDATE


C pams_attach_q <->  MessageQ return Code
C TS  PAMS__BADARGLIST        OpenVMS     Wrong number of call arguments has been passed to this function (não usar isto, só serveria se tanto para testes iniciais)
C TS  PAMS__BADDECLARE        All         Queue has already been attached to this application
C TS  PAMS__BADNAME           All         Invalid name string was specified.
C YE  PAMS__BADPARAM          All         Invalid argument in the argument list.
C VL  PAMS__BADPROCNUM        All         Queue number out of range.
C TS  PAMS__BADQTYPE          All         Invalid queue type.
C NO  PAMS__BADTMPPROC        OpenVMS     Invalid temporary queue number.
C ??  PAMS__DECLARED          All         The queue number is already attached to another application or process.     -> COMMGR   ERROR!!!! CAN NOT ATTACH TO MESSAGEQ!!!! (@08.01.2020 the Millennium primary system was shutdown abruptly, the backup system assumed the processing) 
C NO  PAMS__DUPLQNAME         OpenVMS     Duplicate queue name.
C YE  PAMS__NETERROR          Clients     Network error resulted in a communications link abort
C NO  PAMS__NOACCESS          All         No access to the resource. The address of the specified name is either 0 or it is in another group.
C TS  PAMS__NOACL             All         The queue access control file could not be found.
C TS  PAMS__NOOBJECT          All         No such queue name. For a global queue reference, this error can be caused by a bad
C                             default pathname in the group configuration file.
C VL  PAMS__NOQUOTA           OpenVMS     Insufficient receive message or byte quota to attach.
C NO  PAMS__NOTBOUND          All         The queue name is not bound to an address.
C NO  PAMS__NOTMRQ            OpenVMS     Attempting to attach to Multi-reader Queue and queue type is not an MRQ.
C VL  PAMS__NOTPRIMARYQ       All         Queue name or number is not a primary queue.
C NO  PAMS__NOTSECONDARYQ     All         Queue name or number is not a secondary queue.
C VL  PAMS__PAMSDOWN          All         The specified BEA MessageQ group is not running. 
C NO  PAMS__PREVCALLBUSY      Clients     The previous call to CLS has not been completed.
C YE  PAMS__PNUMNOEXIST       OpenVMS     Target queue name or number does not exist.
C YE  PAMS__RESRCFAIL         All         Failed to allocate resources.
C YE  PAMS__SUCCESS = 1       All         Successful completion of an action.
C YE  PAMS__TIMEOUT           All         The timeout period specified has expired.

C----------------------------------------------------------------------------------

C pams_put_msg <->  MessageQ return Code

C YE  PAMS__BADPARAM          All         Invalid argument in the argument list.
C NO  PAMS__BADPRIORITY       All         Invalid priority value on send operation.
C NO  PAMS__BADPROCNUM UNIX Windows NT    Invalid target queue address specified.
C VL  PAMS__BADRESPQ          All         Response queue not owned by process.
C NO  PAMS__BADTIME           OpenVMS     Invalid time specified.
C NO  PAMS__BADUMA            All         Undeliverable message action (UMA) is invalid.
C VL  PAMS__EXCEEDQUOTA       All         Target process quota exceeded; message was not sent.
C VL  PAMS__EXHAUSTBLKS       OpenVMS     No more message blocks available.
C NO  PAMS__FMLERROR          All         Problem detected with internal format of FML message; 
C                                         this can be an error in
C                                         processing or data corruption.
C NO  PAMS__LINK_UP           OpenVMS     MRS has reestablished link.
C NO  PAMS__MSGTOBIG          All         Message exceeded the size of the largest
C                                         link list section (LLS).
C NO  PAMS__MSGTOSMALL        OpenVMS     Invalid (negative) msg_size specified in
C                                         the argument list.
C YE  PAMS__NETERROR          Clients     Network error resulted in a  -> COMIGS   ERROR: BAD STATUS WHILE GET FROM MESSAGEQ!!
C                                         communications link abort
C NO  PAMS__NOMRS             OpenVMS     MRS is not available.
C VL  PAMS__NOTACTIVE         All         Target process is not currently active;
C                                         message not sent.
C VL  PAMS__NOTDCL            All         Process has not been attached to BEA MessageQ.
C NO  PAMS__NOTFLD            All         The buffer supplied is not an FML32 buffer.
C NO  PAMS__NOTSUPPORTED      All         The combination of delivery mode and
C TS  PAMS__PNUMNOEXIST       OpenVMS     Target queue number does not exist.                                        uma selected is not supported.
C NO  PAMS__PREVCALLBUSY      Clients     Previous call to CLS has not been completed.
C NO  PAMS__REMQUEFAIL        All         Failed to properly dequeue a message buffer.
C YE  PAMS__STOPPED           All         The requested queue has been stopped.
C YE  PAMS__SUCCESS           All         Successful completion
C YE  PAMS__TIMEOUT           All         Timeout period has expired
C ??  PAMS__UNATTACHEDQ       All         Message successfully sent to unattached
C NO  PAMS__WAKEFAIL          OpenVMS     Failed to wake up the target process.

C------------------List to used and send to Ewatcher---------------------------------------------
C     Error of logic -> PAMS__BADPARAM
C     Error of MessageQ -> PAMS__STOPPED
C     Error of NetWork -> PAMS__NETERROR - PAMS__TIMEOUT
C     Error of Hardware and App resources - 
C------------------------------------------------------------------------------------------------

C----------------------------------------------------------------------------------

C pams_get_msg <->  MessageQ return Code

C NO  PAMS__AREATOSMALL     All           Received message is larger than the user’s message area.
C TS  PAMS__BADARGLIST      All           Wrong number of call arguments have been passed to this function.
C VL  PAMS__BADHANDLE       All           Invalid message handle.
C TS  PAMS__BADPARAM            All       Bad argument value.
C NO  PAMS__BADPRIORITY         All       Invalid priority value used for receive.
C ??  PAMS__BADSELIDX       All           Invalid or undefined selective receive index.
C TS  PAMS__BUFFEROVF       UNIX Windows NT The size of the show_buffer specified is too small.
C VL  PAMS__EXHAUSTBLKS     OpenVMS       No more message blocks available.
C NO  PAMS__FMLERROR        All           Problem detected with internal format of FML message; 
C                                         this can be an error in processing or data corruption.
C YE  PAMS__INSQUEFAIL      All           Failed to properly queue a message buffer.
C TS  PAMS__MSGTOSMALL      All           The msg_area_len argument must be positive or zero.
C VL  PAMS__MSGUNDEL          All         Message returned is undeliverable.
C NO  PAMS__NEED_BUFFER_PTR UNIX Windows NT     FML32 buffer received but msg_area_len argument 
C                                         not set to PSYM_MSG_BUFFER_PTR.    
C YE  PAMS__NETERROR        Clients       Network error resulted in a communications link abort. -> COMIGS   ERROR: BAD STATUS WHILE GET FROM MESSAGEQ!!
C VL  PAMS__NOACCESS        All           No access to resource.
C TS  PAMS__NOACL           All           Queue access control file could not be found.
C YE  PAMS__NOMEMORY        OpenVMS       Insuffucient memory resources to reallocate buffer pointer.
C YE  PAMS__NOMOREMSG       All           No messages available.
C NO  PAMS__NOMRQRESRC      All           Insufficient multireader queue resources to allow access.
C VL  PAMS__NOTDCL          All           Process has not been attached to BEA MessageQ
C NO  PAMS__NOTSUPPORTED   UNIX Windows NT    The supplied delivery mode is not supported.    
C TS  PAMS__PAMSDOWN UNIX Windows NT      The specified BEA MessageQ group is not running.
C NO  PAMS__PREVCALLBUSY      Clients     Previous call to CLS has not been completed
C VL  PAMS__QUECORRUPT        OpenVMS     Message buffer queue corrupt.
C VL  PAMS__REMQUEFAIL        All         Failed to properly read from a message buffer
C NO  PAMS__STALE             All         Resource is no longer valid and must be freed by the user. 
C YE  PAMS__STOPPED           All         The requested queue has been stopped.
C YE  PAMS__SUCCESS           All         Indicates successful completion.

C------------------List to used and send to Ewatcher---------------------------------------------
C     Error of logic -> PAMS__BADARGLIST - PAMS__BADPARAM
C     Error of MessageQ -> PAMS__INSQUEFAIL
C     Error of NetWork -> PAMS__NETERROR
C     Error of Hardware and App resources - PAMS__NOMEMORY
C------------------------------------------------------------------------------------------------


C------------------------------------------------------------------------------------------------------------


C     PAMS__FAILED        All                 There is no translation for the specified return code.
C NA  PAMS__BUSNOTSET     UNIX - Windows NT   DMQ_BUS_ID environment variable not set.
C NA  PAMS__GROUPNOTSET   UNIX - Windows NT   DMQ_GROUP_ID environment variable not set.
C     PAMS__EXHAUSTBLKS   OpenVMS             No more message blocks available.
C     PAMS__QUECORRUPT    OpenVMS             Message buffer queue corrupt.
C     PAMS__REMQUEFAIL    All                 Failed to properly read a message buffer.

C PAMS__BADPARAM = 139758786
C PAMS__NETERROR = 139759722
C PAMS__NOMOREMSG = 139756347

      INTEGER*4  MESS(EDLEN)       ! EDLEN is defined GLOBAL and its a constant INTEGER*4  EDLEN PARAMETER (EDLEN=20) !MESSAGE DATA LENGTH 
      INTEGER*4  TASK              !
      INTEGER*4  BUF               !
      INTEGER*4  STATUS,ST
      INTEGER*4  MESSERIAL
      INTEGER*4  BUFNUM
      LOGICAL    CONOLM, FIRSTRUN, WTFORMESS

      INTEGER*4  TEOLM !temporary this variable will come from global.def
      TEOLM = 9 !temporary this value will come from global.def

      character*8 DATEI
      character*10 TIMEI
      character*20 LOGDATEI!could be 18 caracters with
      character*80 PATHI    
            
      PATHI = 'DKD10:[DMIL.WRK.HMC.EXAMPLES.LOGFILES]logs.dat' 

      CALL OPSTXT(' Copyright 2014 SCML. All rights reserved. ') !um print para command line
      CALL SNIF_AND_WRKSET !SIZE OF THE WORKING SET;;;SETS THE WORKING SET TO THE REQUIRED VALUE Alocar memoria necessaria para o programa

      !PARAMETER (PSV=28)        !PASSIVE VALIDATION TASK # (PASVAL)
      !PARAMETER (PST=29)        !PASSIVE PROCESSING TASK # (PASPRO)
C     Message to Send to Logger that have registration/history of when this process starts
C     QUEUE MESSAGE TO ERRLOG  -> ERR (application queue id)   
      TASK    = OLM!28 !OLM !it will be a constant defined in taskid.def thats PARAMETER (OLM=28) but still in study if its one of this (PSV=28) or (PST=29) or another task id
      CALL BUILD_MSG(MESS,1, TASK) 

      MESSERIAL = 0 !comolm message number
      CONOLM = .FALSE.
      FIRSTRUN = .FALSE.

C      WTFORMESS = .FALSE.

      CALL RESET_TIMERS !é timeout quando faz pedido e não retorna a resposta em x tempo -> vision program

      CALL OPSTXT(' ******************* COMOLM ******************* ')

C----+------------------------------------------------------------------
C    | Entry Point: wait for something to do
C----+------------------------------------------------------------------
10      CONTINUE
        WTFORMESS = .FALSE.            
C----+------------------------------------------------------------------
C    | If day close then send stop... 
C    | If system is live then disconnect from IGS MessageQ
C----+------------------------------------------------------------------
      IF (DAYSTS .EQ. DSCLOS)  THEN ! PARAMETER (DSCLOS=4)  !DAY CLOSED or PARAMETER (DSOPEN=2) or TRABUF(TCMNEW) onde está a ler este valor do TRABUF !DAY OPEN FOR SALES or ||||| está em global.def é PARAMETER (DSCLOS=4) !DAY CLOSED
            !INTEGER*4  NUMPAR <---> PARAMETER (NUMPAR=500)       !NUMBER OF SYSTEM PARAMETERS
            !/CONCOM/ P(NUMPAR) 
            !PARAMETER (SYSTYP=29)  !THIS SYSTEM TYPE
            !sistema primario Millennium é que se liga ao euromilhoes o secundario é o backup que mal seja reposto fica logo "funcional" mas de resto fica neste loop que não faz nada
            IF (P(SYSTYP) .EQ. LIVSYS) THEN ! PARAMETER (SYSTYP=29) !THIS SYSTEM TYPE ||| PARAMETER (LIVSYS=1)  !1=LIVE SYSTEM
                  !INTEGER*4  ST <-----> ST = PAMS__SUCCESS  <---->  PARAMETER (PAMS__SUCCESS = 1)
                  CALL MESSQ_EXIT(%REF(ST)) !exit of message queue if its connected subroutine in messaget.for by returning 1???????
                  CALL OPSTXT('OLM IS DISCONNECTED')!prints the message that exited from MessageQ
            ENDIF
            CONOLM = .FALSE. !the flag that is not connected
            !é definido no arranque do millennium
            !é possivel altero seu valor/estado no programa/task vision 
            P(OLMCONF) = 0 !CONF connection flag array of the system where the IGS is not configurated? that is IGS not connect to MessageQ
            !GEXIT_SUCCESS <----> nrm_gstop.for
            CALL GSTOP(GEXIT_SUCCESS) !the application/task comolm stops running
      ENDIF
C----+------------------------------------------------------------------
C    | If not live system then wait 
C----+------------------------------------------------------------------
      IF (P(SYSTYP) .NE. LIVSYS) THEN
            CALL XWAIT(5, 2, ST)   ! Waiting for 5 seconds (unit type = 2) é um timeout
            !CALL CHECKPROCESS() !no longer used there is no in out only com process/program !This subroutine checks the status of the INIGS and OUTIGS processes. If they are down, restarts them.
            FIRSTRUN = .TRUE.
            GOTO 10
      ENDIF
C----+------------------------------------------------------------------
C    | If day is suspended then stay on hold 
C----+------------------------------------------------------------------
      IF(DAYSTS .EQ. DSSUSP) THEN !PARAMETER (DSSUSP=3) !DAY SUSPENDED
            CALL HOLD(0,STATUS)
C           is there a point doing this validation if it goes to the same label anyway or should add perhaps a new label 
            IF(DAYSTS .EQ. DSOPEN) GOTO 10 
            GOTO 10
      ENDIF

C----+------------------------------------------------------------------
C    | If there is no connection to OLM then try to connect to MessageQ
C    | If an error occurs then send an error message and try again 
C----+------------------------------------------------------------------
543     CONTINUE

C        IF (P(OLMCONF) .EQ. 0) THEN !não devia ser NE?? pois ser 0 quer dizer que não está ligado ao MessageQ
C            GOTO 333
C        ENDIF

C----+------------------------------------------------------------------
C    | If there is no connection to OLM then try to connect to MessageQ
C----+------------------------------------------------------------------
C     ter mais uma flag/variavel para controlar pelo vision se quer fazer ou não attach
C     TYPE *,'could not get a buffer ' -> escreve para o ficheiro de logger do programa que está a correr
C----+---------------      
      IF (CONOLM .EQ. .FALSE.) THEN !or could be (.NOT. CONIGS) thats would be true when CONIGS IS FALSE that it not connected to IGS
      CALL MESSQ_ATTACH(%REF(ST))
      IF (ST .NE. PAMS__SUCCESS) THEN !if not attach successfully then exit 
            CALL MESSQ_EXIT(%REF(ST))
            P(OLMCONF) = 0 !so it indicates that its not connected to the MessageQ
            CALL OPSTXT('ERROR!!!! CAN NOT ATTACH TO MESSAGEQ!!!!')

C----+------------------------------------------------------------------
C    | Begin Extra for Initial testing (remove before going to production)
C----+------------------------------------------------------------------   
            CALL date_and_time(DATEI,TIMEI) 

            LOGDATEI = DATEI(7:8)//'/'//DATEI(5:6)//'/'//DATEI(1:4)
     &            //'  '//TIMEI(1:2)//':'//TIMEI(3:4)//':'//TIMEI(5:6)

            open(UNIT=1234, FILE=PATHI, ACCESS='append', STATUS='old')
            write(1234,*) LOGDATEI//' - ERROR!!!! CAN NOT ATTACH TO MESSAGEQ!!!!'
            close(1234)   
C----+------------------------------------------------------------------
C    | End Extra for Initial testing (remove before going to production)
C----+------------------------------------------------------------------  
            GOTO 10           ! ST = PAMS__SUCCESS : Attach successful.
      ENDIF

C----+------------------------------------------------------------------
C    | Send message - OLM connect
C----+------------------------------------------------------------------
      !global.def -> ERRLOG MESAGE TYPE DEFINITIONS new one !PARAMETER (TEOLM=9)   !OLM - Olimpo Messages
      
      CALL BUILD_MSG(MESS,2, TEOLM) !PARAMETER (TEOLM=9)   !OLM - Olimpo Messages
      CALL BUILD_MSG(MESS,3, 1)
      CALL QUEMES(MESS) !QUEUE MESSAGE TO ERRLOG  -> ERR (application queue id)   to keep record of the process starting
      
      CONOLM = .TRUE. !the flag is true since its connected
      P(OLMCONF) = 1 !this position in P indicates that its connected and configurated IGS
      
      ENDIF

333   CONTINUE          

C----+------------------------------------------------------------------
C    | Get buffer number from queue top.
C    | If there are no wagers queued, then go back to wait state.
C----+------------------------------------------------------------------
C        CALL XWAIT(1,2,ST)      ! Wait for 1 second
      CALL XWAIT(250,1,ST)      ! Wait for 250 milliseconds

570   CONTINUE        
C----+------------------------------------------------------------------
C    | If there is Response (transaction processed) to MessageQ sent to Apllications queues
C----+------------------------------------------------------------------

      CALL LISTTOP (BUFNUM, QUETAB(1, OLM), STAT) !still to be decided if its DIS (application queue id or another that will listen to messages)

C is the XXX aplication queue empty then there is no response pending from      
      IF(STAT .EQ. GLIST_STAT_EMPTY) THEN !BUF .EQ. 0
            IF(WTFORMESS .EQ. .TRUE.) GOTO 10
            BUFNUM = 0
            GOTO 600
      ENDIF

C THERE IS RESPONSE MESSAGE IN THE APPLICATION QUEUE THEN SEND THOSE RESPONSES TO THE MESSAGEQ
C AND SEE IF THERE IS MORE RESPONSE MESSAGE STILL PENDING IN THE APPLICATION QUEUE
C----+------------------------------------------------------------------
C    | The SendToOLM function is used to put into MessageQ the message
C    | from Millennium
C----+------------------------------------------------------------------

      ST = PAMS__SUCCESS

      CALL SENDTOOLM(BUF,MESSERIAL,ST) 
      IF ((ST .NE. PAMS__SUCCESS ) .AND. (ST .NE. PAMS__TIMEOUT)) THEN
         CALL MESSQ_EXIT(%REF(ST))
         IF (ST .EQ. PAMS__SUCCESS) THEN !caso tenha feito com sucesso o exit/detach do MessageQ
            CONOLM = .FALSE.  !flag a indicar que não se econtra conectado com IGS
            !confirmar se falhou o envio devia retornar algum tipo de mensagem de erro????
            !falar com o Mário não faz sentido remover da queue se não conseguio enviar ou é para descartar neste caso
            !nunca envia a resposta do lado do Olimpo acaba por dar um timeout de esperar a resposta
            CALL RTL (BUFNUM, QUETAB(1, OLM), STAT)!retira a transaction/transição da queue
            !CALL IGS_DQUTRA(BUF) !retira a transaction/transição da queue pois falhou o invio e já se fez o detach
            GOTO 543
         ENDIF
         CALL OPS('ERROR: MESSAGEQ CAN NOT BE DETACHED!!!',ST,0)
         GOTO 10
      ENDIF
      CALL RTL (BUFNUM, QUETAB(1, OLM), STAT)!retira a transaction/transição da queue

C sent the message of application queue to the messageq now goes fetch the next one if there is one      
      GOTO 570

600   CONTINUE            

C----+------------------------------------------------------------------
C    | Get from OLM data for Sign On and save it into a file
C----+------------------------------------------------------------------
C333     CONTINUE 

C----+------------------------------------------------------------------
C    | Check if OUTIGS and INIGS are running. If not, start them.
C    | as stated before only exists COMOLM there is no IN or OUT
C----+------------------------------------------------------------------
      !CALL CHECKPROCESS() 

C----+------------------------------------------------------------------
C    | Call GETFROMOLM function is used to get from OLM all requests
C    | messages -- only if status is 'NO MORE MESSAGES'
C----+------------------------------------------------------------------

      IF (P(OLMCONF) .EQ. 0) THEN !não devia ser NE?? pois ser 0 quer dizer que não está ligado ao MessageQ
            GOTO 333
      ENDIF

      ST = PAMS__NOMOREMSG !PARAMETER (PAMS__NOMOREMSG      = 139756347)
      IF (P(OLMCONF) .NE. 0 ) THEN !!that is its connected to MessageQ
            CALL GETFROMOLM(ST,MESSERIAL) !then reads message from MessageQ from OLM
            !READS ONE MESSAGE AND NOW DETACH AND EXITS
            !CALL MESSQ_EXIT(%REF(ST))
      ENDIF
        
      IF ((ST .NE. PAMS__SUCCESS) .AND. (ST .NE. PAMS__NOMOREMSG)) THEN
            !IF(IGSDEBUG(IA_COMIGS)) THEN
            !      CALL OPS('148:MESSQ_EXIT',ST,ST)
            !ENDIF
            CALL MESSQ_EXIT(%REF(ST)) !so basicaly something went wrong and ST != PAMS__SUCCESS and PAMS__SUCCESS != PAMS__NOMOREMSG
            IF (ST .EQ. PAMS__SUCCESS) THEN
                  CONOLM = .FALSE. !foi desconectado do MessageQ com sucesso
                  GOTO 543 !vai para o alias 543 para voltar a connectar-se ao MessageQ
            ENDIF
            CALL OPS('ERROR: MESSAGEQ CAN NOT BE DETACHED!!!',ST,0)
            GOTO 10 !falhou desconectar com sucesso volta para o alias 10 (Entry Point)
      ENDIF

C----+------------------------------------------------------------------
C    | Checks if there is no more messages in messageQ then first validates
C    | that there is also no more messages in the app queue to send to Olimpo
C    | only then makes a wait of 250ms to seed if new messages request comes
C    | from Olimpo in the MessageQ
C----+------------------------------------------------------------------      
      IF(ST .EQ. PAMS__NOMOREMSG) THEN
            WTFORMESS = .TRUE.
            GOTO 570 !Ok não têm mais mensagens do MessageQ então volta a ver se têm mais respostas das queues aplicacionais para enviar para o MessageQ
      ELSE IF(ST .EQ. PAMS__SUCCESS) THEN    
            GOTO 570 !têm mais mensagens mas vai ver se têm entretanto mais respostas das queues aplicacionais se não tiver volta este processo de obter mais mensagens do messageq
      ENDIF

C----+------------------------------------------------------------------
C    | Call CheckTimeout function to see if there are any messages sent
C    | to IGS that do not have response. If so, delete buffer and send
C    | to Altura terminal an error message
C    |
C    | CheckTimeout wont be called since COMOLM will get the message from Olimpo and 
C    | hasn't yet process that message so no need to check for timeout only after
C    | processing the message and doesn't have response will call CheckTimeout
C----+------------------------------------------------------------------
      !CALL CHKTIMEOUT !excedeu o tempo de retornar a resposta ao terminal logo algo correo mal e devolve ao terminal uma mensagem de erro e remove o buffer


C----+------------------------------------------------------------------
C    | If there are no more response messages, then start sending all
C    | messages to IGS until there are no more buffers to process
C----+------------------------------------------------------------------
C20    CONTINUE

      !CALL OLM_TOPQUE(BUF) ! This subroutine returns the top element of the queue, or zero, if none the internal queue from openvms/millennium
!      INTEGER*4       STAT
C
!      CALL LISTTOP (BUF, COMIGSQUE(1), STAT) !second parameter not COMIGSQUE(1) but a queue for OLM messages
C
!      IF (STAT .EQ. GLIST_STAT_EMPTY) THEN
!            BUF = 0
!      ENDIF      
      
      
!      IF(BUF .EQ. 0) THEN !in case the queue is empty returns to alias 10 (Entry Point)
!            GOTO 10
!      ELSE
!            IF(IGSDEBUG(IA_COMIGS)) THEN
!                  CALL OPS('BUF',BUF,BUF)
!                  IF(HPRO(INPLEN,BUF) .LT. 1000) THEN
!                        CALL OPSTXT('DUMP INPTAB:')
!                        CALL DUMP_MESSAGE(0,175,BPRO(BINPTAB,BUF),HPRO(INPLEN,BUF))
!                  ELSE
!                        CALL OPSTXT('DUMP INPTAB:')
!                        CALL OPS('177:MESSAGE LENGTH TOO LARGE!!',HPRO(INPLEN,BUF),HPRO(INPLEN,BUF))
!                  ENDIF
!                  IF(HPRO(OUTLEN,BUF) .LT. 1000) THEN
!                        CALL OPSTXT('DUMP WRKTAB:')
!                        CALL DUMP_MESSAGE(0,180,BPRO(WRKTAB*4-3+1,BUF),HPRO(OUTLEN,BUF))
!                  ELSE
!                        CALL OPSTXT('DUMP WRKTAB:')
!                        CALL OPS('182:MESSAGE LENGTH TOO LARGE!!',HPRO(OUTLEN,BUF),HPRO(OUTLEN,BUF))
!                  ENDIF
!            ENDIF
!      ENDIF

      END      
C----+-----------------------------------------------------------------
C    | SUBROUTINE GETFROMOLM
C    |    This subroutine receives messages from OLM
C    +-----------------------------------------------------------------
C    | OUTPUT PARAMETERS:
C    |    ST         Process status
C    |    MESSERIAL  Received messages' sequence number
C----+-----------------------------------------------------------------
C=======OPTIONS /CHECK=NOOVERFLOW
C
C Melhoramento possivel replicar REMTIMER e alterar para em vez de dar um time out
C dar um aviso (já passou 10 segundos e ainda não retornou a resposta) para um ficheiro de logger ou para o errlog e ser
C lido pelo ewatch a indicar possiveis lentidões nas respostas/processamentos (distinguir até por transições).
C-----+-----------------------------------------------
      SUBROUTINE GETFROMOLM(ST,MESSERIAL)
      IMPLICIT NONE
C**************************************************
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
            !INCLUDE 'INCLIB:IGSDEBUG.DEF'

            INTEGER*4  MESS(EDLEN)
            INTEGER*4 I4TEMP
            INTEGER*2 I2TEMP(2)
            BYTE      I1TEMP(4)
            EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)

            INTEGER*4 ST,STATUS,MTYPE,MSUBTYPE
            INTEGER*4 XRFNUM,AGENTNR   !AGENTNR may not be needed here
            
            character*34 XRFNUMSTR, AGENTNRSTR, MTYPESTR, MSUBTYPESTR

            COMMON /FROM_OLM/ MESS_FROM_OLM, MESS_FROM_LEN
            BYTE MESS_FROM_OLM(1024) !so the message is 1024 bytes of length
            INTEGER*4 MESS_FROM_LEN
            INTEGER*4 MESSERIAL

C----+------------------------------------------------------------------
C    | Begin Extra for Initial testing (remove before going to production)
C----+------------------------------------------------------------------
            character*8 DATE
            character*10 TIME
            character*20 LOGDATE!could be 18 caracters with 
            character*80 PATH  
            
!            INTEGER*4  PAMS__SUCCESS
!            PARAMETER (PAMS__SUCCESS        = 1)      
            
            INTEGER*4 APPQUE
            INTEGER*4 PROBUF /0/
            
            PATH = 'DKD10:[DMIL.WRK.HMC.EXAMPLES.LOGFILES]logs.dat'            
C----+------------------------------------------------------------------
C    | End Extra for Initial testing (remove before going to production)
C----+------------------------------------------------------------------

            !MESS(1) = 28!OLM ! OLM=28 !its a constant defined in taskid.def thats PARAMETER (IGC=42) 
            ST = 0

20          CONTINUE

            CALL MESSQ_GET(%REF(STATUS)) !this is the function in MessageQ library in c not the one messageget.for that one is ignored  

            IF (STATUS .EQ. PAMS__SUCCESS) THEN   
C get information for the buffer header   

80                CONTINUE
C aqui ir buscar um buffer da lista de buffer livres pelo getbuf ou é suposto usar o FASTSET???? 
                  CALL GETBUF(PROBUF)

                  IF (PROBUF.LE.0) THEN
C                        CALL XWAIT(50,1,ST) !x2rcvbuf.for
C                        X2X_LOOP_TIME=X2X_LOOP_TIME+50 !x2rcvbuf.for                        
                        !TYPE *,'could not get a buffer ',PROBUF
                        CALL XWAIT(3,2,ST) !faz um wait de 3 segundos antes de voltar a tentar obter um buffer livre
	                  GOTO 80
                  ENDIF

                  HPRO(PRCSRC,PROBUF)=X2X_COM
C                 Applications processor id  (Na pesquisa é sempre 0 por isso manter 0...)                
                  HPRO(PRCDST,PROBUF)=0 
C                 Communications queue number --- (SUBROUTINE TCPQUEUE(QUENUM,ST)) --- used only in tcp communications protocal mostlikely not gona needed in the new terminal (messageq)                
                  HPRO(QUENUM,PROBUF)=QIN
C                 Transaction code  (defenido em comigs,commgr e comolm)            
                  HPRO(TRCODE,PROBUF)=TYPREG
C                 Terminal number --- (usado no in dos com que são: ings, inmgr, etc) neste caso deve vir ou pode vir do Olimpo em vez do x2rcvbuf.for -- PRO(TERNUM,PROBUF) = TERMINAL_NO                
                  PRO(TERNUM,PROBUF)=TERMINAL_NO
C                 Line number or sap number --- PRO(LINENO,PROBUF)=STATION_NO in x2rcvbuf.for                
                  PRO(LINENO,PROBUF)=STATION_NO
C                 Simulation mode (-999 for SIM)                  
                  !PRO(SIMMOD,PROBUF)=SIMMOD !not used/defined in x2rcvbuf.for não vai ser usado pois não se quer estas simulações
C                 X2X communication subsystem (Não se ve onde se está a ser usado este campo talvez na comunicação de volta se for o caso deixa de ser necessario)
                  HPRO(PRCSRC,PROBUF)=X2X_COM
C                 Message number --- HPRO(MSGNUM,PROBUF)=0 in x2rcvbuf.for                
                  HPRO(MSGNUM,PROBUF)=0
C                 MXSRV                  
                  !HPRO(MXS_COM,PROBUF)=MXS_COM !not used/defined in x2rcvbuf.for
C                 Output/Input message length passa a vir do messageq message
                  HPRO(INPLEN,PROBUF)=MES_LEN
C                 Encryption override flag                  
                  !HPRO(ENCOVR,PROBUF)=ENCOVR !not used/defined in x2rcvbuf.for
C                 Buffer output status --- não existe em lado nenhum deve ter sido descontinuado logo não usar
                  !HPRO(BUFSTA,PROBUF)=BUFSTA !not used/defined in x2rcvbuf.for
C                 Special function -> HPRO(SPCFUN,BUF)=1 --- getenc.for (PROCESS ECRYPTION KEY REQUESTS---spesrvf.for)
                  !HPRO(SPCFUN,PROBUF)=SPCFUN !not used/defined in x2rcvbuf.for
C                 Index terminal list --- não existe em lado nenhum deve ter sido descontinuado logo não usar    
                  !HPRO(TRMNDX,PROBUF)=TRMNDX !not used/defined in x2rcvbuf.for

C                 Number of transactions in buffer -> NBRTRA=41 --> getque.for
C                 HPRO(NBRTRA,BUF) = 1 ---------Default 
C                 HPRO(NBRTRA,BUF) = NUMTKT -------- CHECK FOR FRACTION WAGER (2ND PHASE)
C

C                 Number of log records for the this transaction -> NUMLRC=42 --> getque.for
C                 HPRO(NUMLRC,BUF)=SIZE

C                 Number of log records skipped in TMF -> NUMLSK=43

C                 Time at which transaction has entered the system -> TIMOFF=29 -- x2rcvbuf.for
                  PRO(TIMOFF,PROBUF)=X2X_LOOP_TIME

C                 Transaction serial number  -> SERIAL=30   
C                 getser.for -> CALL GETSER(PRO(SERIAL,COMMAND_BUFFER),COMMAND_SIZE) -- dispat.for             

C                 Time stamp -> TSTAMP=31
C                 PRO(TSTAMP,BUF)=P(ACTTIM) -- dispat.for
C                 PRO(TSTAMP,COMMAND_BUFFER)=P(ACTTIM) -- dispat.for


C                 Remote system serial number --> REMSER=32

                  BPRO(NEWTER,PROBUF)=1 !flag for New terminal so that in dispatch knows that the message comes from Olimpo to reponde to this process 'comolm'

                  !BPRO((WRKTAB*4-3+15)+ I,BUF)
C                 INPTAB=33-> 129 && WRKTAB=81 -> 321                 
C                 max length -> 321-129=192bytes 
C                 Mess_From_len returned from messageq_olm.c -> MESSQ_GET function                 
                  DO I=1, MESS_FROM_LEN    
                        BPRO(BINPTAB+I,RBUF) = MESS_FROM_OLM(I) !BPRO(WRKTAB*4-3+I,RBUF)
                  ENDDO

C Send to DIS or ENC application queue???                  
                   CALL QUETRA(ENC,PROBUF) 
C                  CALL ABL(PROBUF,QUETAB(1,ENC),STATUS)                  

C----+------------------------------------------------------------------
C    | Get AGENTNR from message (bytes 1-4)
C----+------------------------------------------------------------------
C                  I1TEMP(4) = ZEXT (MESS_FROM_OLM(1))
C                  I1TEMP(3) = ZEXT (MESS_FROM_OLM(2))
C                  I1TEMP(2) = ZEXT (MESS_FROM_OLM(3))
C                  I1TEMP(1) = ZEXT (MESS_FROM_OLM(4))  
C                  AGENTNR = I4TEMP    
                  
C                  write(AGENTNRSTR,30) AGENTNR !convert the integer value to string using internal file log-> (write)
C30                format(I4)


                  IF(.TRUE.) THEN!IGSDEBUG(IA_COMIGS)
                        !CALL OPS('517:GETFROMIGS:XRFNUM   ',XRFNUM,XRFNUM)

C----+------------------------------------------------------------------
C    | Begin Extra for Initial testing (remove before going to production)
C----+------------------------------------------------------------------
C                        CALL date_and_time(DATE,TIME) 

C                          LOGDATE = DATE(7:8)//'/'//DATE(5:6)//'/'//DATE(1:4)
C     &                   //'  '//TIME(1:2)//':'//TIME(3:4)//':'//TIME(5:6)

C                        open(UNIT=123456, FILE=PATH, ACCESS='append', STATUS='old')
C                        write(123456,*) LOGDATE//' - XRFNUM:  '//XRFNUMSTR//' AGENTNR: '//AGENTNRSTR//' MSUBTYPE: '//MSUBTYPESTR
C     &                  //' MTYPE: '//MTYPESTR 
C                        close(123456)   
                        
C                        ST = STATUS !confirm if its this value thats returned

C                        CALL MESSQ_EXIT(%REF(ST))
C----+------------------------------------------------------------------
C    | End Extra for Initial testing (remove before going to production)
C----+------------------------------------------------------------------

                        !CALL OPS('517:GETFROMIGS:MESSERIAL',MESSERIAL,MESSERIAL)
                  ENDIF

                  !CALL FASTSET(0,TRABUF,TRALEN)

C----+------------------------------------------------------------------
C    | Create buffer to send to the respective application queue (using QUETRA)
C----+------------------------------------------------------------------      
C      X2RCVBUF.FOR
C      CALL X2RCVMSG(LANBUF(LANDATAF,BUF),PRO(INPTAB,PROBUF),MES_LEN
C      *	  ,TERMINAL_NO,STATION_NO,POINTER,SSAP,DSAP,DATA_LEN,
C      *	   DEST_LAYER,HOST_ID,STATUS)


C     CALL I4TOBUF2(TERMINAL_NO,MESSAGE,X2PRO_TERMINAL-1)

C     CALL I4TOBUF4(SND_LAYER,MESSAGE,X2PRO_MESTYP-1)

C     CALL NMOV4TOI4(HEADER,BUFFER,X2TDBH_BLKCHK-1) !'TDBH'

C     CALL MOV2TOI4(RECV_SEQ,BUFFER,X2TDBH_BLKSEQ-1) !RCV SEQ

C     MESTYP = ZEXT (BUFFER(X2FEMES_MESTYP-1))

C Generic Header Info

C TOTOLOTO
                  !PROBUF(1) = 

CALL GETQUE(PROBUF,APPQUE,SIZE,BYPASS) !obtêm QUE que é a queue aplicational (application task queue number) do PROCOM/PROBUF

C                  IF(MTYPE .EQ. 1) THEN!TLTO needs confirmation if its totoloto then send to INMNG application queue
C                        APPQUE = WAG
C                  ELSE IF (MTYPE .EQ. 2) THEN!TSPT needs confirmation if its totobola then send to INMNG application queue
C                        APPQUE = WAG    
C                  ELSE IF (MTYPE .EQ. 14) THEN!TSSC <-> needs confirmation if its Lotaria Instantânea then send to IPS application queue
C                        APPQUE = INI !CRS
C                  ELSE      
                        !received a message with a invalid type send to error queue process...
C                        APPQUE = ERR 
C                  END IF


C                  CALL QUETRA(APPQUE,PROBUF) !envia o buffer/probuf para a aplicação/processo???

C ON PRIMARY SEND TO NETWORK IF QUEUE EMPTIES OR BATCH DONE
C
C                  IF(P(SYSTYP).EQ.LIVSYS.AND.BUFNET.NE.0) TOSEND=.TRUE.
C                  IF(P(SYSTYP).EQ.LIVSYS) CALL SENDBUF(BUFNET)                  

C----+--------------                  
            ENDIF 
            
C----+------------------------------------------------------------------
C    | If status is 'NO MORE MSG', then return
C----+------------------------------------------------------------------
        IF (STATUS .EQ. PAMS__NOMOREMSG) THEN
          ST = STATUS
          RETURN
        ENDIF 

C----+------------------------------------------------------------------
C    | If status is not 'NO MORE MSG' or 'SUCCESS', then signal error 
C    | and return
C----+------------------------------------------------------------------
        IF ((STATUS .NE. PAMS__NOMOREMSG) .OR. (STATUS .NE. PAMS__SUCCESS)) THEN

C     Error of logic -> PAMS__BADARGLIST - PAMS__BADPARAM
C     Error of MessageQ -> PAMS__INSQUEFAIL
C     Error of NetWork -> PAMS__NETERROR
C     Error of Hardware and App resources - PAMS__NOMEMORY


C----+------------------------------------------------------------------
C    | track down more specific erros for example of network or time out other 
C    | erros will be considered as generic errors
C    | Error of logic -> PAMS__BADARGLIST - PAMS__BADPARAM
C    | Error of MessageQ -> PAMS__INSQUEFAIL
C    | Error of NetWork -> PAMS__NETERROR
C    | Error of Hardware and App resources - PAMS__NOMEMORY
C    | Vaibles defined in the shared memorie olmcom.def
C----+------------------------------------------------------------------           

            IF(STATUS .NE. PAMS__BADARGLIST) THEN
                  OLM_BADARGLIST_GET = OLM_BADARGLIST_GET + 1
            ELSE IF(STATUS .NE. PAMS__BADPARAM) THEN
                  OLM_BADPARAM_GET = OLM_BADPARAM_GET + 1
            ELSE IF(STATUS .NE. PAMS__INSQUEFAIL) THEN
                  OLM_INSQUEFAIL_GET = OLM_INSQUEFAIL_GET + 1
            ELSE IF(STATUS .NE. PAMS__NETERROR) THEN
                  OLM_NETERROR_GET = OLM_NETERROR_GET + 1
            ELSE IF(STATUS .NE. PAMS__NOMEMORY) THEN
                  OLM_NOMEMORY_GET = OLM_NOMEMORY_GET + 1
            ELSE 
                  OLM_TOTERRGET = OLM_TOTERRGET + 1
            END IF 
C----+------------------------------------------------------------------
C V06| Bugfix if there is an error while putting a message
C----+------------------------------------------------------------------
            ST = STATUS
C----+------------------------------------------------------------------
C V06| Bugfix if there is an error while putting a message
C----+------------------------------------------------------------------
            CALL OPS('ERROR: BAD STATUS WHILE GET FROM MESSAGEQ!!',ST,0)
            CALL OPSTXT('WILL CONNECT TO FAILOVER HOST')
C----+------------------------------------------------------------------
C V06| Bugfix if there is an error while putting a message
C----+------------------------------------------------------------------
C         ST = STATUS
C----+------------------------------------------------------------------
C V06| Bugfix if there is an error while putting a message
C----+------------------------------------------------------------------
            RETURN
        ENDIF        

      RETURN
      END

C----+-----------------------------------------------------------------
C    | SUBROUTINE BUILD_MSG
C    |    This subroutine sends messages to IGS
C    +-----------------------------------------------------------------
C    | INPUT PARAMETERS:
C    |    MSG       Message to be build
C    |    INDEX     Index of message to set value
C    |    INT_VALUE Value to be set at the given index
C    +-----------------------------------------------------------------
C    | OUTPUT PARAMETERS:
C    |    MSG       Message to be build
C----+-----------------------------------------------------------------
        SUBROUTINE BUILD_MSG(MSG, INDEX, INT_VALUE) !function/subroutine where validates first if the insertion of value into a valid position of the buffer before inserting into it
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


C----+-----------------------------------------------------------------
C    | SUBROUTINE SENDTOOLM
C    |    This subroutine sends messages to OLM
C    +-----------------------------------------------------------------
C    | INPUT PARAMETERS:
C    |    SBUF       Message to be sent
C    |    MESSERIAL  Sent messages' sequence number
C    +-----------------------------------------------------------------
C    | OUTPUT PARAMETERS:
C    |    ST         Process status
C    |    MESSERIAL  Sent messages' sequence number
C----+-----------------------------------------------------------------
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE SENDTOOLM(SBUF,MESSERIAL,ST)
      IMPLICIT NONE
C**************************************************
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
      
            INTEGER*4 I4TEMP
            INTEGER*2 I2TEMP(2) 
            BYTE      I1TEMP(4)
            EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)     
            
            COMMON /TO_OLM/ MESS_TO_OLM, MESS_TO_LEN !------->ficheiro do message message_put.c
            BYTE MESS_TO_OLM(1024)
            INTEGER*4 MESS_TO_LEN
            INTEGER*4 MESSAGE_TYPE, LIST_INDEX  
      
C COPY ALL THE VALUES AS IS INTO THE MESSAGEQ OR DOES IT NEEDED SOME KIND OF PROCESSING
C BEFORE SENDING THE MESSAGE INTO MESSAGEQ?   
            MESS_TO_LEN  = HPRO(INPLEN,SBUF) !INPLEN expect alls a field with the size of message
            DO I=1, MESS_TO_LEN
                  MESS_TO_OLM(I) = BPRO(I,SBUF)!BPRO(WRKTAB*4-3+I,SBUF)
            ENDDO
C      MESS_TO_IGS(IND+1)

            CALL MESSQ_PUT(%REF(STATUS)) 

            IF (STATUS .NE. PAMS__SUCCESS) THEN
                  CALL BUILD_MSG(MESS,1, OLM) !its a constant defined in taskid.def thats PARAMETER (IGC=42)
                  CALL BUILD_MSG(MESS,2, TEOLM)
                  CALL BUILD_MSG(MESS,3, 4) ! ver se 4 quer dizer erro e 1 é de informação
                  CALL BUILD_MSG(MESS,4, STATUS)
                  CALL QUEMES(MESS) !aqui envia a mensagem para queue aplicacional do logger

C------------------------------------------------------------------------------  
C------------------List to used and send to Ewatcher---------------------------------------------
C     Error of logic -> PAMS__BADPARAM
C     Error of MessageQ -> PAMS__STOPPED
C     Error of NetWork -> PAMS__NETERROR - PAMS__TIMEOUT
C     Error of Hardware and App resources - 
C     ??? Se já fica no ficheiro de logs da App e ter já uma mensagem para o ewatch é necessario mais pamarametros
C     para ter mais dados estatisticos...
C------------------------------------------------------------------------------------------------
C                  IF(STATUS .NE. PAMS__STOPPED) THEN
C                        OLM_STOPPED_PUT = OLM_STOPPED_PUT + 1
C                  ELSE IF(STATUS .NE. PAMS__BADPARAM) THEN
C                        OLM_BADPARAM_PUT = OLM_BADPARAM_PUT + 1
C                  ELSE IF(STATUS .NE. PAMS__INSQUEFAIL) THEN
C                        OLM_INSQUEFAIL_PUT = OLM_INSQUEFAIL_PUT + 1
C                  ELSE IF(STATUS .NE. PAMS__NETERROR) THEN
C                        OLM_NETERROR_PUT = OLM_NETERROR_PUT + 1
C                  ELSE 
C                        OLM_TOTERRPUT = OLM_TOTERRPUT + 1
C                  END IF   
C--------------------------------------------------------------------------------                                  

                  DO I=1, MESS_TO_LEN
                        BPRO(WRKTAB*4-3+I,SBUF) = MESS_TO_OLM(I)
                  ENDDO  
                  
                  !CALL TRALOG(TRABUF,APUBUF(2,SBUF))  
C----+------------------------------------------------------------------
C    | Put status and message into COMOLM.LOG (its implicit with this type writes to the logger of this process thats
C    | why it doesn't explicitly point to a file its how application logic as been implemented somewhere the type call knows
C    | what process is calling and so writes to its corresponding logger)
C----+------------------------------------------------------------------
          TYPE *,'ERROR: WHILE TRY TO PUT INTO MESSAGEQ, STATUS: ',
    &     STATUS 
          TYPE *,' '

C !ok escreve nos logs usando mensagem que se ecnontra agora no buffer na secção WRKTAB            
                  DO I=1,MESS_TO_LEN
                        TYPE 9998,I,BPRO(WRKTAB*4-3+I,SBUF) 
                  ENDDO
                  TYPE *,' ' 
C format of the message saved in the logs in case of failer in sending the message into MessageQ            
      9998        FORMAT(' MESSWORD SENT: ',I4.3,' - ', Z3.2) 

                  IF (STATUS .NE. PAMS__TIMEOUT) THEN 
C se não houver falha de conectividade ao conectar-se ao failover imprimir para a linha de comandos a mensagem de ligar-se ao failover
C que na pratica """NUNCA""" deverá acontecer pois o servidor de failover na pratica é o mesmo do principal pois é assim no ficheiro de configuração
                        CALL OPSTXT('WILL CONNECT TO FAILOVER HOST')
                  ENDIF

            ENDIF
      
            ST = STATUS
            RETURN
      END