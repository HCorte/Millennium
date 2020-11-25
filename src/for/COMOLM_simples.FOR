      PROGRAM COMOLM
      IMPLICIT NONE

      INCLUDE 'INCLIB:SYSPARAM.DEF'
!      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
!      INCLUDE 'INCLIB:CONCOM.DEF'
!      INCLUDE 'INCLIB:PROCOM.DEF'
!      INCLUDE 'INCLIB:AGTCOM.DEF'
!      INCLUDE 'INCLIB:TASKID.DEF'
      INCLUDE 'INCLIB:PRMLOG.DEF'
      INCLUDE 'INCLIB:CHKSUMCM.DEF'
!      INCLUDE 'INCLIB:QUECOM.DEF'
!      INCLUDE 'INCLIB:APUCOM.DEF'
!      INCLUDE 'INCLIB:IGSCON.DEF'
      INCLUDE '(LIB$ROUTINES)' 

      INTEGER*4  MESS(EDLEN)       ! EDLEN is defined GLOBAL and its a constant INTEGER*4  EDLEN PARAMETER (EDLEN=20) !MESSAGE DATA LENGTH 
      INTEGER*4  TASK              !
      INTEGER*4  BUF               !
      INTEGER*4  STATUS,ST
      INTEGER*4  MESSERIAL
      LOGICAL    CONOLM, FIRSTRUN
C      LOGICAL FLUSH,BYPASS,TOSEND,DMPDBG

!      INTEGER*4  TEOLM !temporary this variable will come from global.def
!      TEOLM = 9 !temporary this value will come from global.def

      character*8 DATEI
      character*10 TIMEI
      character*20 LOGDATEI!could be 18 caracters with
      character*80 PATHI    

C
C     Begin Constants defined in INCLIB:IGSCON.DEF
C
      INTEGER*4  PAMS__NOMOREMSG
      PARAMETER (PAMS__NOMOREMSG      = 139756347)
      INTEGER*4  PAMS__SUCCESS
      PARAMETER (PAMS__SUCCESS        = 1)
      INTEGER*4  PAMS__DECLARED
      PARAMETER (PAMS__DECLARED       = 139761972)
      INTEGER*4  PAMS__NOLINK
      PARAMETER (PAMS__NOLINK         = 139759394)
      INTEGER*4  PAMS__LINK_DOWN
      PARAMETER (PAMS__LINK_DOWN      = 139759266)
      INTEGER*4  PAMS__NOTACTIVE
      PARAMETER (PAMS__NOTACTIVE      = 139759474)
      INTEGER*4  PAMS__NETLINKLOST
      PARAMETER (PAMS__NETLINKLOST    = 139759730)
      INTEGER*4  PAMS__STOPPED
      PARAMETER (PAMS__STOPPED        = 139759962)
      INTEGER*4  PAMS__NETNOLINK
      PARAMETER (PAMS__NETNOLINK      = 139762140)
      INTEGER*4  PAMS__FATAL
      PARAMETER (PAMS__FATAL          = 139762156)
      INTEGER*4  PAMS__NETERROR 
      PARAMETER (PAMS__NETERROR       = 139759722)
      INTEGER*4  PAMS__STUB
      PARAMETER (PAMS__STUB           = 139757248)
      INTEGER*4 PAMS__TIMEOUT
      PARAMETER (PAMS__TIMEOUT        = 139757256)
C
C     End Constants defined in INCLIB:IGSCON.DEF
C
            
      PATHI = 'DKD10:[DMIL.WRK.HMC.EXAMPLES.LOGFILES]logs.dat' 

      CALL OPSTXT(' Copyright 2014 SCML. All rights reserved. ') !um print para command line
      CALL SNIF_AND_WRKSET !SIZE OF THE WORKING SET;;;SETS THE WORKING SET TO THE REQUIRED VALUE Alocar memoria necessaria para o programa

      !PARAMETER (PSV=28)        !PASSIVE VALIDATION TASK # (PASVAL)
      !PARAMETER (PST=29)        !PASSIVE PROCESSING TASK # (PASPRO)
      !TASK    = 28 !OLM !it will be a constant defined in taskid.def thats PARAMETER (OLM=28) but still in study if its one of this (PSV=28) or (PST=29) or another task id
      !CALL BUILD_MSG(MESS,1, TASK) !queue interna do openvms/millenium

      MESSERIAL = 0 !comolm message number
      CONOLM = .FALSE.
      CALL OPSTXT(' ******************* COMOLM ******************* ')

C----+------------------------------------------------------------------
C    | Entry Point: wait for something to do
C----+------------------------------------------------------------------
10      CONTINUE

C----+------------------------------------------------------------------
C    | If there is no connection to OLM then try to connect to MessageQ
C----+------------------------------------------------------------------
        IF (CONOLM .EQ. .FALSE.) THEN !or could be (.NOT. CONIGS) thats would be true when CONIGS IS FALSE that it not connected to IGS
            CALL MESSQ_ATTACH(%REF(ST))
            IF (ST .NE. PAMS__SUCCESS) THEN !if not attach successfully then exit 
                  CALL MESSQ_EXIT(%REF(ST))
                  !P(OLMCONF) = 0 !so it indicates that its not connected to the MessageQ
                  !CALL OPSTXT('ERROR!!!! CAN NOT ATTACH TO MESSAGEQ!!!!')
                  !GOTO 10           ! ST = PAMS__SUCCESS : Attach successful.

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

            ENDIF

C----+------------------------------------------------------------------
C    | Send message - OLM connect
C----+------------------------------------------------------------------
            !global.def -> ERRLOG MESAGE TYPE DEFINITIONS new one !PARAMETER (TEOLM=9)   !OLM - Olimpo Messages
            
            !CALL BUILD_MSG(MESS,2, TEOLM) !PARAMETER (TEOLM=9)   !OLM - Olimpo Messages
            !CALL BUILD_MSG(MESS,3, 1)
            !CALL QUEMES(MESS) !writes the message into the queue/buffer of 768 bytes of body size with header of 64 bytes generated/obtain from the list of buffers/queues called FREEQ
            
            CONOLM = .TRUE. !the flag is true since its connected
            !P(IGSCONF) = 1 !this position in P indicates that its connected and configurated IGS
            
        ENDIF

333   CONTINUE            
C----+------------------------------------------------------------------
C    | Get buffer number from queue top.
C    | If there are no wagers queued, then go back to wait state.
C----+------------------------------------------------------------------        
        CALL XWAIT(250,1,ST)      ! Wait for 250 milliseconds


      IF (P(OLMCONF) .EQ. 0) THEN !não devia ser NE?? pois ser 0 quer dizer que não está ligado ao MessageQ
            GOTO 333
      ENDIF   
C----+------------------------------------------------------------------
C    | Call GetFromIGS function is used to get from IGS all response
C    | messages -- only if status is 'NO MORE MESSAGES'
C----+------------------------------------------------------------------
      ST = PAMS__NOMOREMSG !PARAMETER (PAMS__NOMOREMSG      = 139756347)
      IF (P(OLMCONF) .NE. 0 ) THEN !!that is its connected to MessageQ
            CALL GETFROMOLM(ST,MESSERIAL) !then reads message from MessageQ from IGS
            !READS ONE MESSAGE AND NOW DETACH AND EXITS
            !CALL MESSQ_EXIT(%REF(ST))
      ENDIF

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
            LOGICAL  DMPDBG
            INTEGER*4 APPQUE
            INTEGER*4 PROBUF /0/
            INTEGER*4 OLM_COM /15/
            INTEGER*4 TERMINAL_NO /0300006/
C            INTEGER*4 TYPREG /1/
C            INTEGER*4 QIN /-2/
            
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
C aqui ir buscar um buffer da lista de buffer livres pelo getbuf 
                  CALL GETBUF(PROBUF)
C                  CALL FASTSET(0, PROBUF,PROLEN)!192*4=768 bytes
                  CALL FASTSET(0, PRO(1,PROBUF), PROLEN)
C confirmar que o buffer esta zerado ou seja limpo/reset confirmar como já está implementado  
C                  CALL LIB$MOVC5(1,0,0,64000,OUARY(K))   
C                  CALL LIB$MOVC3(BLEN,B_CHKTAB,BPRO(BINSTAB+1,BUF))   
C MOVE INPUT MSG TO PROCOM BUFFER
C
C                  CALL LIB$MOVC3(BLEN, INTAB(1), BPRO(BINSTAB+1,BUF))                            
C                  PROBUF=0   !RESET BUF NR
                  IF (PROBUF.LE.0) THEN
C                        CALL XWAIT(50,1,ST) !x2rcvbuf.for
C                        X2X_LOOP_TIME=X2X_LOOP_TIME+50 !x2rcvbuf.for                        
                        !TYPE *,'could not get a buffer ',PROBUF
                        CALL XWAIT(3,2,ST) !faz um wait de 3 segundos antes de voltar a tentar obter um buffer livre
	                  GOTO 80
                  ENDIF


CCCCCCCCCC                  HPRO(OLM_COM,PROBUF)=1                  
C                 !COMMUNICATIONS PROCESSOR ID                  
                  HPRO(PRCSRC,PROBUF)=OLM_COM !X2X_COM or MXS_COM or 0 ou um novo chamado de OLM_COM
C                 Applications processor id  (Na pesquisa é sempre 0 por isso manter 0...)                
                  HPRO(PRCDST,PROBUF)=0 
C                 Communications queue number --- (SUBROUTINE TCPQUEUE(QUENUM,ST)) --- used only in tcp communications protocal mostlikely not gona needed in the new terminal (messageq)                
C                 PARAMETER (QIN=-2)     !IN INPUT QUEUE
                  HPRO(QUENUM,PROBUF)=QIN !?? confirmar se este campo está a ser usado
C                 Transaction code  (defenido em comigs,commgr e comolm) por default comesa como REGULAR TRANSACTION (queue_to_game.for and x2rcvbuf.for)           
                  HPRO(TRCODE,PROBUF)=TYPREG
C                 Terminal number --- (usado no in dos com que são: inigs, inmgr, etc) neste caso deve vir ou pode vir do Olimpo em vez do x2rcvbuf.for -- PRO(TERNUM,PROBUF) = TERMINAL_NO                
C                 Vêm do Olimpo que sabe qual é o numero do terminal externo AGTNUM
                  PRO(TERNUM,PROBUF)=TERMINAL_NO 
C                 Line number or sap number --- PRO(LINENO,PROBUF)=STATION_NO in x2rcvbuf.for and in queue_to_game PRO(LINENO,PROBUF)=0               
                  PRO(LINENO,PROBUF)=0 !its 0 as in queue_to_game this header seems related to X2X communications routines only
C                 Simulation mode (-999 for SIM)                  
C                 PRO(SIMMOD,PROBUF)=0 !not used/defined in x2rcvbuf.for não vai ser usado pois não se quer estas simulações
C                 X2X communication subsystem (Não se ve onde se está a ser usado este campo talvez na comunicação de volta se for o caso deixa de ser necessario)
C                 HPRO(PRCSRC,PROBUF)=X2X_COM
C                 Message number --- HPRO(MSGNUM,PROBUF)=0 in x2rcvbuf.for                
                  HPRO(MSGNUM,PROBUF)=0
C                 MXSRV                  
                  !HPRO(MXS_COM,PROBUF)=MXS_COM !not used/defined in x2rcvbuf.for
C                 Output/Input message length passa a vir do messageq message
                  HPRO(INPLEN,PROBUF)=MESS_FROM_LEN
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
                  !date_and_time(date,time)!X2X_LOOP_TIME 
                  CALL GETTIM(P(ACTTIM))
                  PRO(TIMOFF,PROBUF)=P(ACTTIM) !TIMIOFF

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
C           ver função mas basicamente escreve para o ficheiro GTECH$DEBUG.DAT esta escrita só acontece quando o programa 
C           gxutl:setdebug estiver activado o modo debug aqui vê se as mensagens na entrada Dispacther e na saida            
C           que é Logger (Passando agora nem que seja só em testes a mensagem enviada para o Dispacther)
                  IF(DMPDBG) CALL PRTOUT(PROBUF) 
            ENDIF

C Send to DIS  (for now commented to just write to GTECH$DEBUG.DAT)              
                   !CALL QUETRA(DIS,PROBUF) 
C                  CALL ABL(PROBUF,QUETAB(1,ENC),STATUS)                  
               
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