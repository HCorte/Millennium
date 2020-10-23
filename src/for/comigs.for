C-----------------------------------------------------------------------
C PROGRAM COMIGS
C-----------------------------------------------------------------------
C COMIGS.FOR
C
C V06 2015-SEP-14 SCML Bugfix if there is an error while putting a message
C V05 2015-JUL-20 SCML Added support for IGS internal cancel flags
C V04 2015-JUN-02 SCML Bugfix with financial reports when IGS returns an
C                      error message
C V03 2015-MAY-14 SCML Bugfix with transaction recording while in Duplex Mode
C V02 2015-APR-24 SCML More info for logging purposes
C V01 2014-FEB-10 SCML PLACARD PROJECT - IGS - Creation
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2014 SCML/Accenture. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM COMIGS
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
        INCLUDE '(LIB$ROUTINES)' !não é um logical name e não é symbol é o que ?? e qual o path final
        INCLUDE 'INCLIB:IGSDEBUG.DEF'

        INTEGER*4  MESS(EDLEN)       ! EDLEN is defined GLOBAL and its a constant INTEGER*4  EDLEN PARAMETER (EDLEN=20) !MESSAGE DATA LENGTH 
        INTEGER*4  TASK              !
        INTEGER*4  BUF               !
        INTEGER*4  STATUS,ST
        INTEGER*4  MESSERIAL
        LOGICAL    CONIGS, FIRSTRUN

        CALL OPSTXT(' Copyright 2014 SCML. All rights reserved. ') !um print para command line
        CALL SNIF_AND_WRKSET !SIZE OF THE WORKING SET;;;SETS THE WORKING SET TO THE REQUIRED VALUE Alocar memoria necessaria para o programa

        TASK    = IGC !its a constant defined in taskid.def thats PARAMETER (IGC=42)        !IGS Connect  -- new

        CALL BUILD_MSG(MESS,1, TASK) !queue interna do openvms/millenium

        MESSERIAL = 0 !comigs message number
        CONIGS = .FALSE.
        FIRSTRUN = .FALSE.

        CALL RESET_TIMERS !é timeout quando faz pedido e não retorna a resposta em x tempo -> vision program

        CALL OPSTXT(' ******************* COMIGS ******************* ')
        
C        P(ODSPACAN) = 0          ! SUPPRESS AUTO CANCELS ODDSET

C----+------------------------------------------------------------------
C    | Entry Point: wait for something to do
C----+------------------------------------------------------------------
10      CONTINUE

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
              CALL OPSTXT('IGS IS DISCONNECTED')!prints the message that exited from MessageQ
           ENDIF
           CONIGS = .FALSE. !the flag that is not connected
           !é definido no arranque do millennium
           !é possivel altero seu valor/estado no programa/task vision 
           P(IGSCONF) = 0 !CONF connection flag array of the system where the IGS is not configurated? that is IGS not connect to MessageQ
           !GEXIT_SUCCESS <----> nrm_gstop.for
           CALL GSTOP(GEXIT_SUCCESS) !the application/task comigs stops running
        ENDIF
C----+------------------------------------------------------------------
C    | If not live system then wait 
C----+------------------------------------------------------------------
        IF (P(SYSTYP) .NE. LIVSYS) THEN
           CALL XWAIT(5, 2, ST)   ! Waiting for 5 seconds (unit type = 2) é um timeout
           CALL CHECKPROCESS() !This subroutine checks the status of the INIGS and OUTIGS processes. If they are down, restarts them.
           FIRSTRUN = .TRUE.
           GOTO 10
        ENDIF
C----+------------------------------------------------------------------
C    | If day is suspended then stay on hold 
C----+------------------------------------------------------------------
        IF(DAYSTS .EQ. DSSUSP) THEN !PARAMETER (DSSUSP=3) !DAY SUSPENDED
          CALL HOLD(0,STATUS)
          IF(DAYSTS .EQ. DSOPEN) GOTO 10
          GOTO 10
        ENDIF

C----+------------------------------------------------------------------
C    | If there is no connection to IGS then try to connect to MessageQ
C    | If an error occurs then send an error message and try again 
C----+------------------------------------------------------------------
543     CONTINUE

        IF (P(IGSCONF) .EQ. 0) THEN !não devia ser NE?? pois ser 0 quer dizer que não está ligado ao MessageQ
            GOTO 333
        ENDIF

C----+------------------------------------------------------------------
C    | If there is no connection to IGS then try to connect to MessageQ
C----+------------------------------------------------------------------
        IF (CONIGS .EQ. .FALSE.) THEN !or could be (.NOT. CONIGS) thats would be true when CONIGS IS FALSE that it not connected to IGS
           CALL MESSQ_ATTACH(%REF(ST))
           IF (ST .NE. PAMS__SUCCESS) THEN !if not attach successfully then exit 
              CALL MESSQ_EXIT(%REF(ST))
              P(IGSCONF) = 0 !so it indicates that its not connected to the MessageQ
              CALL OPSTXT('ERROR!!!! CAN NOT ATTACH TO MESSAGEQ!!!!')
              GOTO 10           ! ST = PAMS__SUCCESS : Attach successful.
           ENDIF

C----+------------------------------------------------------------------
C    | Send message - IGS connect
C----+------------------------------------------------------------------
           CALL BUILD_MSG(MESS,2, TEIGS) !PARAMETER (TEIGS=8)          !IGS - Odds Set Messages
           CALL BUILD_MSG(MESS,3, 1)
           CALL QUEMES(MESS) !writes the message into the queue/buffer of 768 bytes of body size with header of 64 bytes generated/obtain from the list of buffers/queues called FREEQ
           
           CONIGS = .TRUE. !the flag is true since its connected
           P(IGSCONF) = 1 !this position in P indicates that its connected and configurated IGS
        ENDIF

C----+------------------------------------------------------------------
C    | Get from IGS data for Sign On and save it into a file
C----+------------------------------------------------------------------
333     CONTINUE 

C----+------------------------------------------------------------------
C    | Check if OUTIGS and INIGS are running. If not, start them.
C----+------------------------------------------------------------------
        CALL CHECKPROCESS()     

C----+------------------------------------------------------------------
C    | Get buffer number from queue top.
C    | If there are no wagers queued, then go back to wait state.
C----+------------------------------------------------------------------
C        CALL XWAIT(1,2,ST)      ! Wait for 1 second
        CALL XWAIT(250,1,ST)      ! Wait for 250 milliseconds

C----+------------------------------------------------------------------
C    | Call GetFromIGS function is used to get from IGS all response
C    | messages -- only if status is 'NO MORE MESSAGES'
C----+------------------------------------------------------------------
        ST = PAMS__NOMOREMSG !PARAMETER (PAMS__NOMOREMSG      = 139756347)
        IF (P(IGSCONF) .NE. 0) THEN !that is its connected to MessageQ
            CALL GETFROMIGS(ST,MESSERIAL) !then reads message from MessageQ from IGS
        ENDIF
        
        IF ((ST .NE. PAMS__SUCCESS) .AND. (ST .NE. PAMS__NOMOREMSG)) THEN
           IF(IGSDEBUG(IA_COMIGS)) THEN
               CALL OPS('148:MESSQ_EXIT',ST,ST)
           ENDIF
           CALL MESSQ_EXIT(%REF(ST)) !so basicaly something went wrong and ST != PAMS__SUCCESS and PAMS__SUCCESS != PAMS__NOMOREMSG
           IF (ST .EQ. PAMS__SUCCESS) THEN
              CONIGS = .FALSE. !foi desconectado do MessageQ com sucesso
              GOTO 543 !vai para o alias 543 para voltar a connectar-se ao MessageQ
           ENDIF
           CALL OPS('ERROR: MESSAGEQ CAN NOT BE DETACHED!!!',ST,0)
           GOTO 10 !falhou desconectar com sucesso volta para o alias 10 (Entry Point)
        ENDIF

C----+------------------------------------------------------------------
C    | Call CheckTimeout function to see if there are any messages sent
C    | to IGS that do not have response. If so, delete buffer and send
C    | to Altura terminal an error message
C----+------------------------------------------------------------------
        CALL CHKTIMEOUT !excedeu o tempo de retornar a resposta ao terminal logo algo correu mal e devolve ao terminal uma mensagem de erro e remove o buffer

C----+------------------------------------------------------------------
C    | If there are no more response messages, then start sending all
C    | messages to IGS until there are no more buffers to process
C----+------------------------------------------------------------------
20      CONTINUE
        CALL IGS_TOPQUE(BUF) ! This subroutine returns the top element of the queue, or zero, if none the internal queue from openvms/millennium
        IF(BUF .EQ. 0) THEN !in case the queue is empty returns to alias 10 (Entry Point) and waits for 250ms before trying to fetch more messages
            GOTO 10
        ELSE
            IF(IGSDEBUG(IA_COMIGS)) THEN
                CALL OPS('BUF',BUF,BUF)
                IF(HPRO(INPLEN,BUF) .LT. 1000) THEN
                    CALL OPSTXT('DUMP INPTAB:')
                    CALL DUMP_MESSAGE(0,175,BPRO(BINPTAB,BUF),HPRO(INPLEN,BUF))
                ELSE
                    CALL OPSTXT('DUMP INPTAB:')
                    CALL OPS('177:MESSAGE LENGTH TOO LARGE!!',HPRO(INPLEN,BUF),HPRO(INPLEN,BUF))
                ENDIF
                IF(HPRO(OUTLEN,BUF) .LT. 1000) THEN
                    CALL OPSTXT('DUMP WRKTAB:')
                    CALL DUMP_MESSAGE(0,180,BPRO(WRKTAB*4-3+1,BUF),HPRO(OUTLEN,BUF))
                ELSE
                    CALL OPSTXT('DUMP WRKTAB:')
                    CALL OPS('182:MESSAGE LENGTH TOO LARGE!!',HPRO(OUTLEN,BUF),HPRO(OUTLEN,BUF))
                ENDIF
            ENDIF
        ENDIF
C----+------------------------------------------------------------------
C    | The SendToIGS function is used to put into MessageQ the message
C    | from Altura
C----+------------------------------------------------------------------
        ST = PAMS__SUCCESS
        CALL SENDTOIGS(BUF,MESSERIAL,ST) !envia as mensagens/buffer obtidas em IGS_TOPQUE para o IGS
        IF ((ST .NE. PAMS__SUCCESS ) .AND. (ST .NE. PAMS__TIMEOUT)) THEN
           CALL MESSQ_EXIT(%REF(ST))
           IF (ST .EQ. PAMS__SUCCESS) THEN !caso tenha feito com sucesso o exit/detach do MessageQ
              CONIGS = .FALSE. !flag a indicar que não se econtra conectado com IGS
              CALL IGS_DQUTRA(BUF) !retira a transaction/transição da queue pois falhou o invio e já se fez o detach
              GOTO 543
           ENDIF
           CALL OPS('ERROR: MESSAGEQ CAN NOT BE DETACHED!!!',ST,0)
           GOTO 10
        ENDIF
        IF(IGSDEBUG(IA_COMIGS)) THEN
            CALL OPS('191:IGS_DQUTRA',0,0)
        ENDIF
        CALL IGS_DQUTRA(BUF) !retira a transaction/transição da queue 
        IF(IGSDEBUG(IA_COMIGS)) THEN
            CALL OPS('193:IGS_DQUTRA',0,0)
        ENDIF

C----+------------------------------------------------------------------
C    | Call GetFromIGS function is used to get from IGS all response
C    | messages -- only if status is 'NO MORE MESSAGES'
C----+------------------------------------------------------------------
        ST = PAMS__NOMOREMSG
        IF (P(IGSCONF) .NE. 0 ) THEN
              IF(IGSDEBUG(IA_COMIGS)) THEN
                CALL OPS('197:GETFROMIGS',0,0)
            ENDIF
            CALL GETFROMIGS(ST,MESSERIAL) !the second time its called the GETFROMIGS to retrive the messages from the MessageQ sent by IGS
            IF(IGSDEBUG(IA_COMIGS)) THEN
                CALL OPS('197:GETFROMIGS',MESSERIAL,ST)
            ENDIF
        ENDIF
        IF ((ST .NE. PAMS__SUCCESS) .AND. (ST .NE. PAMS__NOMOREMSG))THEN
           CALL MESSQ_EXIT(%REF(ST)) !so there was some kind of error then exit/detach of the MessageQ
           IF (ST .EQ. PAMS__SUCCESS) THEN !in case the exit/detach of the MessageQ was success the update the flag CONIGS (connected IGSs) 
              CONIGS = .FALSE.
              GOTO 543
           ENDIF
           CALL OPS('ERROR: MESSAGEQ CAN NOT BE DETACHED!!!',ST,0)
           GOTO 10
        ENDIF

C----+------------------------------------------------------------------
C    | Call CheckTimeout function to see if there are any messages sent
C    | to IGS that do not have response. If so, delete buffer and send
C    | to Altura terminal an error message
C----+------------------------------------------------------------------
        CALL CHKTIMEOUT !validate that there isn't any pending messages if there is deletes from the buffer and returns a error message to the terminal (timeout)
        GOTO 20
        END
        

c Today's date is 13-Jul-2020 (UTC).
c Today's Julian Date is 20195 .
c 6 meses em dias 6*30=180 mais 13 dias já de julho dá 193 aproximadamente
c mas na realidade 31+29+31+30+31+30+13=195 valor indicado em cima do ano 20 -> 20195


        SUBROUTINE GET_JULIAN_DAY_FOR_YEAR(YYYY,MM,DD,JDAY)!We refer to a yyddd date format (yy = year, ddd=day) as a 'Julian Date' - this is the common term for such a date in mainframe and other circles. where the ddd refers to days from the beginning of the year 
        IMPLICIT NONE
        
        INTEGER*4 YYYY,MM,DD,JDAY
        
        INTEGER*4 REG_YEAR(12)
        INTEGER*4 BIS_YEAR(12)
        INTEGER*4 I
        
        
        I = 1
        REG_YEAR(I) = 31
        BIS_YEAR(I) = 31

        I = 2                          
        REG_YEAR(I) = REG_YEAR(I - 1) + 28
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 29
        
        I = 3                          
        REG_YEAR(I) = REG_YEAR(I - 1) + 31
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 31
        
        I = 4                           
        REG_YEAR(I) = REG_YEAR(I - 1) + 30
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 30
        
        I = 5                           
        REG_YEAR(I) = REG_YEAR(I - 1) + 31
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 31
        
        I = 6                           
        REG_YEAR(I) = REG_YEAR(I - 1) + 30
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 30
        
        I = 7                           
        REG_YEAR(I) = REG_YEAR(I - 1) + 31
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 31
        
        I = 8                           
        REG_YEAR(I) = REG_YEAR(I - 1) + 31
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 31
        
        I = 9                           
        REG_YEAR(I) = REG_YEAR(I - 1) + 30
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 30
        
        I = 10                          
        REG_YEAR(I) = REG_YEAR(I - 1) + 31
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 31
        
        I = 11                          
        REG_YEAR(I) = REG_YEAR(I - 1) + 30
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 30
        
        I = 12                          
        REG_YEAR(I) = REG_YEAR(I - 1) + 31
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 31

        ! Handle bissext years
        IF(  (MOD(YYYY,4)   .EQ. 0
     *  .AND. MOD(YYYY,400) .EQ. 0)
     *  .OR. (MOD(YYYY,4)   .EQ. 0
     *  .AND. MOD(YYYY,100) .NE. 0) ) THEN
            JDAY = DD
            IF(MM .GT. 1) THEN
                JDAY = JDAY + BIS_YEAR(MM - 1)
            ENDIF
        ! Handle regular years
        ELSE
            JDAY = DD
            IF(MM .GT. 1) THEN
                JDAY = JDAY + REG_YEAR(MM - 1)
            ENDIF
        ENDIF
        
        RETURN
        END




        SUBROUTINE GET_UNIX_TIME_MS(UX_TIME_MS)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C ARGUMENTS
        INTEGER*8 UX_TIME_MS

C INTERNAL VARIABLES
        INTEGER*4 TIM(8), I, YYYY,MM,DD, JDAY
        CHARACTER*12 CLOCK(3)
C
C       values (1) is the 4-digit year
C       values (2) is the month of the year
C       values (3) is the day of the year
C       values (4) is the time difference with respect to
C                   Coordinated Universal Time (UTC) in minutes
C       values (5) is the hour of the day (range 0 to 23)
C       values (6) is the minutes of the hour (range 0 to 59).
C       values (7) is the seconds of the minute (range 0 to 59).
C       values (8) is the milliseconds of the second (range 0 to 999).


        CALL DATE_AND_TIME(CLOCK(1),CLOCK(2),CLOCK(3),TIM)

        UX_TIME_MS = TIM(8)
     *             + TIM(7) * 1000
     *             + TIM(6) * 1000 * 60
     *             + TIM(5) * 1000 * 60 * 60
     
        DO I = 1970, TIM(1) - 1
            CALL GET_JULIAN_DAY_FOR_YEAR(I,12,31,JDAY)
            UX_TIME_MS = UX_TIME_MS + JDAY * 1000 * 60 * 60 * 24
        ENDDO
        CALL GET_JULIAN_DAY_FOR_YEAR(TIM(1),TIM(2),TIM(3),JDAY)
        JDAY = JDAY - 1 ! Must remove one day, because of that day's milliseconds
        UX_TIME_MS = UX_TIME_MS + (JDAY * 1000 * 60 * 60 * 24)

        RETURN
        END



        SUBROUTINE CALCULATE_MSG_CHECKSUM(TER,MSG_OFFSET,OUTBUF,OUTLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
        BYTE      OUTBUF(*)
        INTEGER*4 OUTLEN
        
        INTEGER*4 MYCHKSUM, CHKLEN
        INTEGER*4 TER, MSG_OFFSET
        
        CHARACTER*255 LINE
        
        BASECHKSUM = IAND(DAYCDC,'FFFF'X)
        I4CCITT   = IAND(BASECHKSUM+TER,'FFFF'X)
        OUTBUF(MSG_OFFSET + 0) = I1CCITT(2)
        OUTBUF(MSG_OFFSET + 1) = I1CCITT(1)

        I4CCITT = 0
        I1CCITT(2) = OUTBUF(MSG_OFFSET + 0)
        I1CCITT(1) = OUTBUF(MSG_OFFSET + 1)
        
        CHKLEN=OUTLEN-1
        CALL GETCCITT(OUTBUF,1,CHKLEN,MYCHKSUM)
        I4CCITT = MYCHKSUM
        OUTBUF(MSG_OFFSET + 0) = I1CCITT(2)
        OUTBUF(MSG_OFFSET + 1) = I1CCITT(1)

        I4CCITT = 0
        I1CCITT(2) = OUTBUF(MSG_OFFSET + 0)
        I1CCITT(1) = OUTBUF(MSG_OFFSET + 1)
        
        RETURN
        END





        SUBROUTINE INJECT_FIELDS_TO_SEND_MESSAGE(MESS_TO_IGS,MESS_TO_LEN, BUF, TER, TRABUF, GTYP,GIND)
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

        BYTE MESS_TO_IGS(1024)
        INTEGER*4 MESS_TO_LEN
        INTEGER*4 BUF, TER
        
        INTEGER*4 GTYP, GIND
        
        INTEGER*4 IND, MESSLEN, I
        
        INTEGER*4 MTYPE, MSUBTYPE
        
        INTEGER*4 OTYPE, OSUBTYPE
        
        INTEGER*8 I8TSTAMP, UX_TS
        INTEGER*1 I1TSTAMP(8)
        INTEGER*4 I4TSTAMP(2)
        EQUIVALENCE(I8TSTAMP,I1TSTAMP)
        EQUIVALENCE(I8TSTAMP,I4TSTAMP)
        
        INTEGER*8 I8TMP
        INTEGER*1 I1TMP(8)
        INTEGER*4 I4TMP(2)
        EQUIVALENCE(I8TMP,I1TMP)
        EQUIVALENCE(I8TMP,I4TMP)
        
        MTYPE    =     ZEXT(BPRO(BINPTAB + 1,BUF))/16 !ZEXT Elemental Intrinsic Function (Generic): Extends an argument with zeros. This function is used primarily for bit-oriented operations. returns a single byte but requires two byte storage
        MSUBTYPE = MOD(ZEXT(BPRO(BINPTAB + 1,BUF)),16)
        MESSLEN  = HPRO(INPLEN,BUF)
        OTYPE    =     ZEXT(BPRO((WRKTAB*4-3+15) + 1,BUF))/16
        OSUBTYPE = MOD(ZEXT(BPRO((WRKTAB*4-3+15) + 1,BUF)),16)

        IF(IGSDEBUG(IA_COMIGS)) THEN
           CALL OPS('427:INJECT:MTYPE',MTYPE,MTYPE)
           CALL OPS('427:INJECT:MSUBTYPE',MSUBTYPE,MSUBTYPE)
           CALL OPS('427:INJECT:OTYPE',OTYPE,OTYPE)
           CALL OPS('427:INJECT:OSUBTYPE',OSUBTYPE,OSUBTYPE)
           CALL OPS('427:INJECT:MESSLEN',MESSLEN,MESSLEN)
           CALL OPS('427:INJECT:BUF',BUF,BUF)
           CALL OPS('427:INJECT:TRABUF(TSDT1)(C)',TRABUF(TSDT1),TRABUF(TSDT1))
           CALL OPS('427:INJECT:TRABUF(TSDT2)(S)',TRABUF(TSDT2),TRABUF(TSDT2))
           CALL OPS('427:INJECT:TRABUF(TSDT6)(H)',TRABUF(TSDT6),TRABUF(TSDT6))
           CALL OPS('427:INJECT:TRABUF(TSDT5)(L)',TRABUF(TSDT5),TRABUF(TSDT5))
        ENDIF
        
        IF(MTYPE .EQ. 6 .AND. MSUBTYPE .EQ. 3) THEN !10.1	Financial Reports
           IF(MTYPE .EQ. OTYPE .AND. MSUBTYPE .EQ. OSUBTYPE) THEN
           ! Handling financial reports - normal case, copy result from EM
              IND = 14
              DO I=0,3 ! up to checksum ---- 4bytes total comessa no byte 15-18
                 MESS_TO_IGS(IND+1+ I) = BPRO((WRKTAB*4-3+15)+ I,BUF) !there were no error's so WRKTAB is the reports of Millennium and Euromillions
              ENDDO
              ! Inject other fields
              !CALL GET_UNIX_TIME_MS(UX_TS)
              !I8TSTAMP = UX_TS
              I4TSTAMP(2) = TRABUF(TSDT6) ! HIGH
              I4TSTAMP(1) = TRABUF(TSDT5) ! LOW
              
              I8TMP = I8TSTAMP
              DO I = 1,8
                 I1TMP(I) = I1TSTAMP(9 - I)
              ENDDO
              
              ! IMPOSTANTE ---->the combination of this two parameters refer the game
              ! Injecting game type
              MESS_TO_IGS(IND+1 + 4) = TODS ! 1byte total byte 19 the value is TODS=18 by default from global.def
              GTYP = ZEXT(MESS_TO_IGS(IND+1 + 4))
              ! Injecting game index
              MESS_TO_IGS(IND+1 + 5) = 1 ! 1byte total byte 20
              GIND = ZEXT(MESS_TO_IGS(IND+1 + 5))
              
              ! Injecting timestamp
              DO I = 1,8
                 MESS_TO_IGS(IND+1+ I + 5) = I1TMP(I) ! 8bytes total from byte 21 to byte 28 for example YYYYMMDD confirmar é uma suposição
              ENDDO
              
              DO I=4,MESSLEN - 1 ! from checksum onwards 
                 MESS_TO_IGS(IND+1+ I + 10) = BPRO((WRKTAB*4-3+15)+ I,BUF) ! from byte 29 to MESSLEN - 1
              ENDDO
              MESS_TO_LEN = MESSLEN + IND + 10 ! não devia somar 4+1+1+8=14 em vez dos 10???
              CALL CALCULATE_MSG_CHECKSUM(TER,3,MESS_TO_IGS(15),MESSLEN + 10)
              IF(IGSDEBUG(IA_COMIGS)) THEN
                 CALL OPS('427:INJECT:MESS_TO_LEN',MESS_TO_LEN,MESS_TO_LEN)
              ENDIF
              MESSLEN = MESSLEN + 10
              RETURN
           ELSE
           ! Handling error messages
              IND = 14
              DO I=0,3 ! up to checksum 
                 MESS_TO_IGS(IND+1+ I) = BPRO(BINPTAB+ I,BUF) !there were erros so the reports in the BINPTAB are the correct ones thats of Millennium System
              ENDDO
              
              ! Inject other fields
              !CALL GET_UNIX_TIME_MS(UX_TS)
              !I8TSTAMP = UX_TS
              I4TSTAMP(2) = TRABUF(TSDT6) ! HIGH
              I4TSTAMP(1) = TRABUF(TSDT5) ! LOW
              
              I8TMP = I8TSTAMP
              DO I = 1,8
                 I1TMP(I) = I1TSTAMP(9 - I)
              ENDDO
              
              ! Injecting game type
              MESS_TO_IGS(IND+1 + 4) = TODS
              GTYP = ZEXT(MESS_TO_IGS(IND+1 + 4))
              ! Injecting game index
              MESS_TO_IGS(IND+1 + 5) = 1
              GIND = ZEXT(MESS_TO_IGS(IND+1 + 5))
              
              ! Injecting timestamp
              DO I = 1,8
                 MESS_TO_IGS(IND+1+ I + 5) = I1TMP(I)
              ENDDO
              
              DO I=4,MESSLEN - 1 ! from checksum onwards 
                 MESS_TO_IGS(IND+1+ I + 10) = BPRO(BINPTAB+ I,BUF)
              ENDDO
              MESS_TO_LEN = MESSLEN + IND + 10
              CALL CALCULATE_MSG_CHECKSUM(TER,3,MESS_TO_IGS(15),MESSLEN + 10)
              IF(IGSDEBUG(IA_COMIGS)) THEN
                 CALL OPS('427:INJECT:MESS_TO_LEN',MESS_TO_LEN,MESS_TO_LEN)
              ENDIF
              MESSLEN = MESSLEN + 10
              RETURN
           ENDIF
        ENDIF
        
        IF (MTYPE .EQ. 6) THEN !repetição???? não esta a fazer nada
           IF (MSUBTYPE .EQ. 3) THEN
           ENDIF
        ENDIF
        
        ! Handling default case (that are not reports so basicaly all the other cases)
        IND = 14
        DO I=0,MESSLEN - 1 !copy the message from the buffer to the MESS_TO_IGS array
           MESS_TO_IGS(IND+1+ I) = BPRO(BINPTAB+ I,BUF)
        ENDDO
        MESS_TO_LEN = MESSLEN + IND

        RETURN
        END



C----+-----------------------------------------------------------------
C    | SUBROUTINE SENDTOIGS
C    |    This subroutine sends messages to IGS
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
        SUBROUTINE SENDTOIGS(SBUF,MESSERIAL,ST)
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

        INTEGER*4  MESS(EDLEN)
        INTEGER*4 I4TEMP
        INTEGER*2 I2TEMP(2) ! why not BYTE*2 if a INTEGER corresponds to a BYTE
        BYTE      I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
        INTEGER*4 SBUF,ST,IND,I,STATUS,TYP,SUBTYP, GTYP, GIND
        INTEGER*4 MESSERIAL
        INTEGER*2 MESSLEN

        LOGICAL*1 TEMPFUNCRES   !Used in debug only

C----+------------------------------------------------------------------
C    | This common area is equal to the structure used by MESSAGEQ_IGS.C
C    | It is used to pass the message...
C    | MESS_TO_IGS = Message in bytes
C    | MESS_TO_LEN = Message length
C----+------------------------------------------------------------------
        COMMON /TO_IGS/ MESS_TO_IGS, MESS_TO_LEN !------->ficheiro do message message_put.c
        BYTE MESS_TO_IGS(1024)
        INTEGER*4 MESS_TO_LEN
        INTEGER*4 MESSAGE_TYPE, LIST_INDEX

        CALL BUILD_MSG(MESS,1, IGC) !its a constant defined in taskid.def thats PARAMETER (IGC=42)

        ST = 0 !in case of failer its default value is 0
        CALL FASTSET(0,TRABUF,TRALEN)

C----+------------------------------------------------------------------
C    | Get type of message from second byte
C----+------------------------------------------------------------------
        !ZEXT stands for Zero Extend fortran -> The first unused high-order bit is set to zero and extended toward the higher-order end
        !RESULT = ISHFT(I, SHIFT)
        !ISHFT returns a value corresponding to I with all of the bits shifted SHIFT places. A value of SHIFT greater than zero corresponds to a left shift
        !, a value of zero corresponds to no shift, and a value less than zero corresponds to a right shift.
        ! 10 1010 -> BPRO -> 0010 1010 -> ISHFT -> 0000 0010
        TYP = ISHFT(ZEXT(BPRO(BINPTAB+1,SBUF)),-4) !INTEGER*4  BINPTAB,BOUTTAB |||||| PARAMETER (BINPTAB=INPTAB*4-3)-> 33*4-3=132-3=129 porque o -3????
        ! MOD(A,P) computes the remainder of the division of A by P. <------ é o resto da divisão como em c 5%4 o resto é 1
        ! The return value is the result of A - (INT(A/P) * P). The type and kind of the return value is the same as that of the arguments. The returned value has the same sign as A and a magnitude less than the magnitude of P.
        ! 10 1010 -> BPRO -> 0010 1010 -> MOD -> (2+8+32)/16=42/16 resto é 10
        SUBTYP = MOD(ZEXT(BPRO(BINPTAB+1,SBUF)),16) ! 2*2*2*2=2^4=16 transformação para converter nos bits menos signaficativos
        !obtém sempre 4 bits do byte obtido do buffer pois TYP+SUBTYP->1 Byte -> 4bits mais significativos + 4bits menos significativos
        ! o maximo possivel é 16 quer para TYP quer para SUBTYP     
C----+------------------------------------------------------------------
C V04| Bugfix with financial reports when IGS returns an error message:
C    | We must copy result from WRKTAB to INPTAB first, for it has the
C    | latest answer from Euromillions
C----+------------------------------------------------------------------
        IF (TYP .EQ. 6 .AND. SUBTYP .EQ. 3) THEN ! its a financial reports sent to the terminal (9.10.1	Total Summary Report Central -> Terminal || 9.10.2	Total On-Line Sales Report Central -> Terminal || 9.10.3	Total On-Line Validations Report Central -> Terminal || 9.10.4	Total On-Line Remunerations Report Central -> Terminal)
           MESS_TO_LEN = ZEXT(HPRO(INPLEN,SBUF)) !PARAMETER (INPLEN=14)      !INPUT MESSAGE LENGTH ou seja a queue têm nesta pos o valor do tamanho da message
           
           IF(IGSDEBUG(IA_COMIGS)) THEN
               CALL OPS('661:SENDTOIGS',MESS_TO_LEN,MESS_TO_LEN)
               CALL OPSTXT('661:SENDTOIGS DUMP INPTAB')
               CALL DUMP_MESSAGE(661,SBUF,BPRO(BINPTAB,SBUF),MESS_TO_LEN)
               CALL OPSTXT('661:SENDTOIGS DUMP WRKTAB')
               CALL DUMP_MESSAGE(661,SBUF,BPRO(WRKTAB*4-3+1,SBUF),MESS_TO_LEN + 14)
           ENDIF
           
           DO I=1, MESS_TO_LEN
              ! When handling a financial report, we must copy the result
              ! from Euromillions first (spesrv-> special services (task))
              BPRO(BINPTAB + I - 1,SBUF) = BPRO(WRKTAB*4-3+14+I,SBUF) !porque estar a copiar um segmento do buffer para outra posição no mesmo buffer???
           ENDDO
           
           IF(IGSDEBUG(IA_COMIGS)) THEN
               CALL OPS('673:SENDTOIGS',MESS_TO_LEN,MESS_TO_LEN)
               CALL OPSTXT('673:SENDTOIGS DUMP INPTAB')
               CALL DUMP_MESSAGE(673,SBUF,BPRO(BINPTAB,SBUF),MESS_TO_LEN)
               CALL OPSTXT('673:SENDTOIGS DUMP WRKTAB')
               CALL DUMP_MESSAGE(673,SBUF,BPRO(WRKTAB*4-3+1,SBUF),MESS_TO_LEN + 14)
           ENDIF
        ENDIF
C----+------------------------------------------------------------------
C V04| Bugfix with financial reports when IGS returns an error message
C    | We must copy result from WRKTAB to INPTAB first, for it has the
C    | latest answer from Euromillions
C----+------------------------------------------------------------------


C----+------------------------------------------------------------------
C    | If this message is a report, or a reprint, then get TRABUF from WRKTAB in 
C    | PROCOM and put it into APUBUF (See page 33 of Portugal Host-IGS Pass-Thru file in Millennium Folder)
C----+------------------------------------------------------------------
C        IF (TYP .EQ. 6 .OR. TYP .EQ. 8) THEN
        IF (TYP .EQ. 8) THEN ! type = 8 é reprint
           !TRABUF -> buffer de transacção – TRABUF
           CALL LOGTRA(TRABUF,PRO(WRKTAB,SBUF)) !como é PRO corresponde aos 4 bytes na position 97,98,99,100
           CALL TRALOG(TRABUF,APUBUF(2,SBUF)) !C SUBROUTINE TO ENCODE INTERNAL TRANSACTION FORMAT
        ENDIF   
C----+------------------------------------------------------------------
C    | Get TRABUF from APUBUF
C----+------------------------------------------------------------------
        !OUTPUT ->  TRALOG - INTERNAL TRANSACTION FORMAT
        ! what does IAND - Bitwise logical AND. (ver logica de circuitos)
        CALL LOGTRA(TRABUF,APUBUF(2,SBUF)) !converte a estrutura de dados do Buffer SBUF para tipo de transaction/transação do Millennium usando esta função LOGTRA
           
C----+------------------------------------------------------------------
C    | Get message length
C----+------------------------------------------------------------------
        MESSLEN = HPRO(INPLEN,SBUF) !reads in position INPLEN/section of SBUF buffer the value that stands for message length in this case

C----+------------------------------------------------------------------
C    | Put agent number at the beginning (bytes 1-4) ver 2.1	Placard Wager Request Central -> IGS por exemplo
C----+------------------------------------------------------------------
        I4TEMP = 0 !resents the variable
        IND = 0
        I4TEMP    = TRABUF(TAGT) !o TAGT corresponde ao campo do AGENT NUMBER no TRABUF (na posição 7 ou seja devia ser só um byte não???? ou é na posição de 7 que é um inteiro logo composto por 4 bytes)
        MESS_TO_IGS(IND+1) = I1TEMP(4) !HHHH LLLL -> H byte
        MESS_TO_IGS(IND+2) = I1TEMP(3) !HHHH LLLL -> H byte
        MESS_TO_IGS(IND+3) = I1TEMP(2) !HHHH LLLL -> L byte
        MESS_TO_IGS(IND+4) = I1TEMP(1) !HHHH LLLL -> L byte

C----+------------------------------------------------------------------
C    | Put MessageQ sequence number (bytes 5-8) ---> Cross Reference Number
C----+------------------------------------------------------------------
        MESSERIAL = MESSERIAL + 1 !quando é chamado vem a 0 que somando 1 fica 1
        IND = 4
        I4TEMP = 0 !resents the variable
        I4TEMP = MESSERIAL ! 0000 .... 0001
        MESS_TO_IGS(IND+1) = I1TEMP(4) ! 0000 0000
        MESS_TO_IGS(IND+2) = I1TEMP(3) ! 0000 0000
        MESS_TO_IGS(IND+3) = I1TEMP(2) ! 0000 0000
        MESS_TO_IGS(IND+4) = I1TEMP(1) ! 0000 0001
C----+------------------------------------------------------------------
C    | Put Millennium buffer number (bytes 9-10) ----> Host Buffer Number and Host stands for Millennium
C----+------------------------------------------------------------------
        IND = 8
        !so if the SBUF is buffer of 768 bytes length
        ! I1TEMP(1) is the first byte of the buffer and I1TEMP(2) is the second byte that identifyes the buffer used in this case SBUF is pointing
        I4TEMP = SBUF !why now doesnt reset first the variable??? is it because its a pointer???
        MESS_TO_IGS(IND+1) = I1TEMP(2)
        MESS_TO_IGS(IND+2) = I1TEMP(1)
C----+------------------------------------------------------------------
C    | Put Millennium CDC date (bytes 11-12)
C----+------------------------------------------------------------------
        IND = 10
        I4TEMP = TRABUF(TCDC)
        MESS_TO_IGS(IND+1) = I1TEMP(2)
        MESS_TO_IGS(IND+2) = I1TEMP(1)
C----+------------------------------------------------------------------
C    | Put Terminal number (bytes 13-14)
C----+------------------------------------------------------------------
        IND = 12
        I4TEMP = TRABUF(TTER)
        MESS_TO_IGS(IND+1) = I1TEMP(2)
        MESS_TO_IGS(IND+2) = I1TEMP(1)
        
        ! Inject fields (copys the "message body" to the message to send to igs)
        GTYP = 0
        GIND = 0
        CALL INJECT_FIELDS_TO_SEND_MESSAGE(MESS_TO_IGS,MESS_TO_LEN,SBUF,TRABUF(TTER),TRABUF,GTYP,GIND)
CC----+------------------------------------------------------------------
CC    | Put message from Altura ((MESSLEN - 15) bytes )
CC----+------------------------------------------------------------------
C        IND = 14
C        DO I=0,MESSLEN-1
C           MESS_TO_IGS(IND+1+ I) = BPRO(BINPTAB+ I,SBUF)
C        ENDDO
CC----+------------------------------------------------------------------
CC    | Setting message length sent to MessageQ
CC    |    MESS_TO_LEN = MESLEN + 15
CC----+------------------------------------------------------------------
C        MESS_TO_LEN = MESSLEN + IND
        IF(IGSDEBUG(IA_COMIGS)) THEN
            CALL OPS('613:SENDTOIGS',MESS_TO_LEN,MESS_TO_LEN)
            CALL DUMP_MESSAGE(613,SBUF,MESS_TO_IGS,MESS_TO_LEN)
            CALL OPS('TYP: ',TYP,TYP)
            CALL OPS('P(IGSPGFIN): ',P(IGSPGFIN),P(IGSPGFIN))
            CALL OPS('P(IGSPPLA): ',P(IGSPPLA),P(IGSPPLA))
            CALL OPS('GTYP: ',GTYP,GTYP)
            CALL OPS('GIND: ',GIND,GIND)
            CALL OPS('IGS_GAMNUM(GTYP,GIND): ',IGS_GAMNUM(GTYP,GIND),IGS_GAMNUM(GTYP,GIND))
            TEMPFUNCRES = IGSGAMFLG(P(IGSPGFIN),IGS_GAMNUM(GTYP,GIND))
            CALL OPS('IGSGAMFLG(P(IGSPGFIN),IGS_GAMNUM(GTYP,GIND)): ',TEMPFUNCRES,TEMPFUNCRES)
        ENDIF

C----+------------------------------------------------------------------
C    | Calling C function to put message into MessageQ (Queue = 1)
C    | Note: check if function is suppressed first for reports
C----+------------------------------------------------------------------
        IF (   P(IGSCONF)  .NE. 0 ! its connected to the MessageQ
     *  .AND. (   (TYP .NE. 6) !type = 6 AND subtype = 3 ---> financial reports so NE (not equal) its a not a financial report
     *        .OR.(TYP .EQ. 6 .AND. P(IGSPFIN) .EQ. 0 ! some flags indicating that even if type is 6 its not a financial report
     *                        .AND. P(IGSPPLA) .EQ. 0 
     *                        .AND. IGSGAMFLG(P(IGSPGFIN),IGS_GAMNUM(GTYP,GIND)) 
     *                              .EQ. .FALSE.) ! Are financial reports suppressed for game Placard ?
     *        )
     *  ) THEN ! corresponds to all messages except the financial reports
           IF(IGSDEBUG(IA_COMIGS)) THEN
                  CALL OPSTXT('359:SENDTOIGS:MESSQ_PUT ANTE') 
           ENDIF
           !inside the messageq_igs.c there is a struture named MQ_Struct_Put and object of that structer named
           ! TO_IGS that haves the to field one named Mess_To_IGS and the other named Mess_To_Len that
           ! corresponds to global variabel  common name and its variables---->   COMMON /TO_IGS/ MESS_TO_IGS, MESS_TO_LEN
           !so here MESS_TO_IGS array corresponds to the field TO_IGS.MESS_TO_IGS in the messageq_igs.c file have the same value
           ! pointing to the same adddress in memory
          CALL MESSQ_PUT(%REF(STATUS)) !so again its the function in c file messq_put NOT A FUNCTION IN FORTRAN
           IF(IGSDEBUG(IA_COMIGS)) THEN
                  CALL OPS('359:SENDTOIGS:MESSQ_PUT POST ST =',STATUS,STATUS) 
           ENDIF
        ELSE ! here is when to send the reports to IGS (a comunicação com IGS é pela queue do openvms??)
           IF(IGSDEBUG(IA_COMIGS)) THEN
               CALL OPS('361:SENDTOIGS:P(IGSCONF)',P(IGSCONF),P(IGSCONF))
               CALL OPS('361:SENDTOIGS:P(IGSPREP)',P(IGSPREP),P(IGSPREP))
               CALL OPS('361:SENDTOIGS:TYP',TYP,TYP)
               CALL OPS('361:SENDTOIGS:TRABUF(TTYP)',TRABUF(TTYP),TRABUF(TTYP))
               CALL OPS('361:SENDTOIGS:TSPE',TSPE,TSPE)
           ENDIF
           DO I=1, MESS_TO_LEN !why is it copy into WRKTAB in SBUF buffer from the array MESS_TO_IGS????
              BPRO(WRKTAB*4-3+I,SBUF) = MESS_TO_IGS(I) ! remember its in the WRKTAB the message since it the reports of the Millennium and Euromillions already
           ENDDO
           HPRO(OUTLEN,SBUF) = MESS_TO_LEN !new size taking into account the extra bytes of Agent Number+Cross Reference Number+Host Buffer Number+Host CDC Date+Terminal Number
           IF(TYP .NE. 6) THEN !qual é o caso que type é 6 mas não financial report???
              HPRO(REMSTS,SBUF) = RMDOWN ! PARAMETER (RMDOWN=2)   !REMOTE SYSTEM DOWN/DEAD/NOT COFIGURED
           ENDIF
           HPRO(TRCODE,SBUF) = TYPIGS !INTEGER*4  TYPIGS --- PARAMETER (TYPIGS=41) !IGS - Odds Set 
           IF (TRABUF(TTYP) .NE. TSPE) THEN
             TRABUF(TERR) = SDOR ! PARAMETER (SDOR=5)  !SYSTEM DORMANT (TRANSACTION ERROR CODE DEFINITIONS)
             TRABUF(TSTAT) =  REJT! PARAMETER (REJT=6)  !REJECTED (TRANSACTION STATUS DEFINITIONS)
           ENDIF
           !SBUF -> buffer de saida
           !TRABUF -> buffer de transacção (TRABUF).
           !APUBUF -> buffer de ????
           CALL TRALOG(TRABUF,APUBUF(2,SBUF))
C----+------------------------------------------------------------------
C    | Put it into DISPAT to send it to OUTIGS
C----+------------------------------------------------------------------
           CALL ABL(SBUF,QUETAB(1,DIS),ST)
           ST = STATUS
           RETURN
        ENDIF
C----+------------------------------------------------------------------
C    | If status is not success, then send to console an error message
C    | and alter message to RMDOWN (Remote System Down) to send to 
C    | Altura an error message
C----+------------------------------------------------------------------
        IF (STATUS .NE. PAMS__SUCCESS) THEN
           
           CALL OPS ('ERROR: WHILE PUT INTO MESSAGEQ!!',STATUS,0)
           CALL BUILD_MSG(MESS,2, TEIGS)
           CALL BUILD_MSG(MESS,3, 4)
           CALL BUILD_MSG(MESS,4, STATUS)
           CALL QUEMES(MESS) !aqui envia a mensagem para queue que deve ser do terminal Altura
           
           !esta a meter a mensagem outra fez no buffer na secção WRKTAB para meter nos logs
           DO I=1, MESS_TO_LEN
              BPRO(WRKTAB*4-3+I,SBUF) = MESS_TO_IGS(I)
           ENDDO
           HPRO(OUTLEN,SBUF) = MESS_TO_LEN !size of message to send
           HPRO(REMSTS,SBUF) = RMDOWN !RMDOWN (Remote System Down)
           HPRO(TRCODE,SBUF) = TYPIGS 
           IF (TRABUF(TTYP) .NE. TSPE) THEN
             TRABUF(TERR) = SDOR !codigo de erro (NOT signed on )
             TRABUF(TSTAT) = REJT !estado rejeitado da transacção 
           ENDIF

           CALL TRALOG(TRABUF,APUBUF(2,SBUF))      

C----+------------------------------------------------------------------
C    | Put status and message into COMIGS.LOG
C----+------------------------------------------------------------------
           TYPE *,'ERROR: WHILE TRY TO PUT INTO MESSAGEQ, STATUS: ',STATUS 
           TYPE *,' '
           
           DO I=1,MESS_TO_LEN
              TYPE 9998,I,BPRO(WRKTAB*4-3+I,SBUF) !ok escreve nos logs usando mensagem que se ecnontra agora no buffer na secção WRKTAB
           ENDDO
           TYPE *,' ' 
C----+------------------------------------------------------------------
C    | Put it into DISPAT to send it to OUTIGS
C----+------------------------------------------------------------------
           CALL ABL(SBUF,QUETAB(1,DIS),ST)! mete no fim da lista QUETAB
           IF (STATUS .NE. PAMS__TIMEOUT) THEN !se não hover falha de conectividade ao conectar-se ao failover imprimir para a linha de comandos a mensagem de ligar-se ao failover
            ! que na pratica """NUNCA""" deverá acontecer pois o servidor de failover na pratica é o mesmo do principal pois é assim no ficheiro de configuração
               CALL OPSTXT('WILL CONNECT TO FAILOVER HOST')
           ENDIF
           ST = STATUS
           RETURN
        ENDIF
C----+------------------------------------------------------------------
C    | Put sent message into WRKTAB in PROCOM
C----+------------------------------------------------------------------
        DO I=1, MESS_TO_LEN
           BPRO(WRKTAB*4-3+I,SBUF) = MESS_TO_IGS(I)
        ENDDO
        
C----+------------------------------------------------------------------
C    | Add buffer into timer list
C----+------------------------------------------------------------------
        MESSAGE_TYPE = TYP !MessageQ message type 
        !(Type = 14 <-> IGS Wagers,IGS Cancellations,IGS Validations,IGS Payments,IGS Game Programme Reports,IGS Game Commands,IGS Error Messages )
        !(Type = 8 <-> Reprints )
        !(Type = 6 <-> Reports )
        CALL GET_TIMER_LIST_INDEX(MESSAGE_TYPE, LIST_INDEX) !gets the index value based on the type of message with that its infered what is the value of time out given (associated to its type)
        CALL ADDTIMER(LIST_INDEX,MESSERIAL,SBUF) !this is a matrix where the position is another array or only one buffer????
        IF(IGSDEBUG(IA_COMIGS)) THEN
           CALL OPS('482:SENDTOIGS:MESSAGE_TYPE',MESSAGE_TYPE,MESSAGE_TYPE)
           CALL OPS('482:SENDTOIGS:LIST_INDEX',LIST_INDEX,LIST_INDEX)
           CALL OPS('482:SENDTOIGS:MESSERIAL',MESSERIAL,MESSERIAL)
           CALL OPS('482:SENDTOIGS:SBUF',SBUF,SBUF)
        ENDIF
        HPRO(OUTLEN,SBUF) = MESS_TO_LEN 
        ST = STATUS
9998    FORMAT(' MESSWORD SENT: ',I4.3,' - ', Z3.2) !format of the message saved in the logs in case of failer in sending the message into MessageQ 
        END



        SUBROUTINE CONVERT_TO_UNIX_TIME_MS(UX_TIME_MS,YYYY,MM,DD,HH,MI,SS)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C ARGUMENTS
        INTEGER*8 UX_TIME_MS
        INTEGER*4 YYYY,MM,DD,HH,MI,SS

C INTERNAL VARIABLES
        INTEGER*4  I, JDAY

        UX_TIME_MS = 0
     *             + SS * 1000
     *             + MI * 1000 * 60
     *             + HH * 1000 * 60 * 60
     
        DO I = 1970, YYYY - 1
            CALL GET_JULIAN_DAY_FOR_YEAR(I,12,31,JDAY)
            UX_TIME_MS = UX_TIME_MS + JDAY * 1000 * 60 * 60 * 24
        ENDDO
        CALL GET_JULIAN_DAY_FOR_YEAR(YYYY,MM,DD,JDAY)
        JDAY = JDAY - 1 ! Must remove one day, because of that day's milliseconds
        UX_TIME_MS = UX_TIME_MS + (JDAY * 1000 * 60 * 60 * 24)

        RETURN
        END




C----+-----------------------------------------------------------------
C    | SUBROUTINE GETFROMIGS
C    |    This subroutine receives messages from IGS
C    +-----------------------------------------------------------------
C    | OUTPUT PARAMETERS:
C    |    ST         Process status
C    |    MESSERIAL  Received messages' sequence number
C----+-----------------------------------------------------------------
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE GETFROMIGS(ST,MESSERIAL)
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
        INCLUDE 'INCLIB:IGSDEBUG.DEF'
        
        INTEGER*4  MESS(EDLEN)
        INTEGER*4 I4TEMP
        INTEGER*2 I2TEMP(2)
        BYTE      I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
        INTEGER*4 ST,STATUS,I,IND,JUMPINC,MTYPE,MSUBTYPE,ST2
        INTEGER*4 ITYPE,ISUBTYPE, OTYPE,OSUBTYPE
        INTEGER*4 RBUF,CHKDIG
        INTEGER*4 XRFNUM
        INTEGER*4 MESSAGE_TYPE, LIST_INDEX


        INTEGER*8 I8AUX
        INTEGER*4 I4AUX(2)
        INTEGER*1 I1AUX(8)
        EQUIVALENCE(I8AUX,I4AUX,I1AUX)
        
        CHARACTER*255 LINE
C----+------------------------------------------------------------------
C    | This common area is equal to the structure used by MESSAGEQ_IGS.C
C    | It is used to pass the message...
C    | MESS_TO_IGS = Message in bytes
C    | MESS_TO_LEN = Message length
C----+------------------------------------------------------------------
        COMMON /TO_IGS/ MESS_TO_IGS, MESS_TO_LEN
        BYTE MESS_TO_IGS(1024)
        INTEGER*4 MESS_TO_LEN
        INTEGER*4 STATUSTO,TERMINALNUM,JULIANDATE,MYCHKSUM,CHKLEN,ITO,INDTO
        INTEGER*4 OFFSET
        BYTE OUTTABTO(500)
        INTEGER*2 DBUF(12)
        INTEGER*2 I2CCITT(2)
        EQUIVALENCE (I4CCITT,I2CCITT)
        
        COMMON /FROM_IGS/ MESS_FROM_IGS, MESS_FROM_LEN
        BYTE MESS_FROM_IGS(1024) !so the message is 1024 bytes of length
        INTEGER*4 MESS_FROM_LEN
        INTEGER*4 MESSERIAL

        INTEGER*4 BS_YEAR,BS_MONTH,BS_DAY,BS_GAME_ID,BS_CHK_DIG
        INTEGER*8 BS_SERIAL
        INTEGER*4 BC_YEAR,BC_MONTH,BC_DAY,BC_HOUR,BC_MINUTE,BC_SECOND
        LOGICAL   IS_RETRY
        INTEGER*8 MESSAGE_TILL_ID
        INTEGER*4 MSG_GAME_TYPE, MSG_GAME_INDEX
        
        MESS(1) = IGC !its a constant defined in taskid.def thats PARAMETER (IGC=42) 
        ST = 0

20      CONTINUE
C        IF(IGSDEBUG(IA_COMIGS)) THEN
C           CALL OPSTXT('921:GETFROMIGS:ANTE GET')
C        ENDIF
        CALL MESSQ_GET(%REF(STATUS)) !this is the function in MessageQ library in c not the one messageget.for that one is ignored  
        !to read the message from the queue
C        IF(IGSDEBUG(IA_COMIGS)) THEN
C           CALL OPSTXT('921:GETFROMIGS:POST GET')
C        ENDIF
        
        IF (STATUS .EQ. PAMS__SUCCESS) THEN !if its success the return value of MESSQ_GET that its PAMS__SUCCESS = 1 of value

C----+------------------------------------------------------------------
C    | Get XREFNUM from message (bytes 5-8)
C----+------------------------------------------------------------------
            I1TEMP(1) = ZEXT (MESS_FROM_IGS(8))
            I1TEMP(2) = ZEXT (MESS_FROM_IGS(7))
            I1TEMP(3) = ZEXT (MESS_FROM_IGS(6))
            I1TEMP(4) = ZEXT (MESS_FROM_IGS(5))
            XRFNUM = I4TEMP
            IF(IGSDEBUG(IA_COMIGS)) THEN
                CALL OPS('517:GETFROMIGS:XRFNUM   ',XRFNUM,XRFNUM)
                CALL OPS('517:GETFROMIGS:MESSERIAL',MESSERIAL,MESSERIAL)
            ENDIF
C----+------------------------------------------------------------------
C    | Clear variable memory
C----+------------------------------------------------------------------
          I4TEMP = 0
          
C----+------------------------------------------------------------------
C    | Get RBUF from message (bytes 9-10)
C----+------------------------------------------------------------------
          I1TEMP(1) = ZEXT(MESS_FROM_IGS(10))
          I1TEMP(2) = ZEXT(MESS_FROM_IGS(9))
          I1TEMP(3) = 0
          I1TEMP(4) = 0
          
          RBUF = I4TEMP
          IF(IGSDEBUG(IA_COMIGS)) THEN
             CALL OPS('532:GETFROMIGS:RBUF',RBUF,RBUF)
          ENDIF
          
C----+------------------------------------------------------------------
C    | check for unsolicited messages from IGS (check type and subtype)
C----+------------------------------------------------------------------
          MTYPE = ZEXT(MESS_FROM_IGS(16))
          MSUBTYPE = IAND(  MOD(MTYPE, 16), '0F'X)
          MTYPE    = IAND(ISHFT(MTYPE, -4), '0F'X)
C
          IF(MTYPE .EQ. 14 .AND. MSUBTYPE .EQ. 14) THEN ! mas quando type e subtype são 14 não é um pedido de cancelamento do Totobola???
             CALL DECODE_AND_SEND_IGS_COMMAND(MESS_FROM_IGS)
             ST = STATUS
             RETURN            
          ENDIF

C----+------------------------------------------------------------------
C    | Checking for time-outs
C----+------------------------------------------------------------------
          IF (XRFNUM .GT. MESSERIAL) THEN
             MESSERIAL = XRFNUM
          ELSE
             MESSAGE_TYPE = ZEXT(BPRO(BINPTAB + 1, RBUF))
             MESSAGE_TYPE = ISHFT(MESSAGE_TYPE,-4)
             CALL GET_TIMER_LIST_INDEX(MESSAGE_TYPE, LIST_INDEX)
             CALL REMTIMER(LIST_INDEX,XRFNUM,RBUF,ST)
             IF(IGSDEBUG(IA_COMIGS)) THEN
                CALL OPS('603:GETFROMIGS:MESSAGE_TYPE',MESSAGE_TYPE,MESSAGE_TYPE)
                CALL OPS('603:GETFROMIGS:LIST_INDEX',LIST_INDEX,LIST_INDEX)
                CALL OPS('603:GETFROMIGS:HPRO(INPLEN,RBUF)',ZEXT(HPRO(INPLEN,RBUF)),ZEXT(HPRO(INPLEN,RBUF)))
                CALL OPS('603:GETFROMIGS:ST',ST,ST)
             ENDIF
             IF (ST.LT.0) THEN
                 CALL OPS('MESSAGE ALREADY TIME OUT',ST,ST)
C----+------------------------------------------------------------------
C V02| Adding more info for logging purposes
C----+------------------------------------------------------------------
                 TYPE *, IAM()
C----+------------------------------------------------------------------
C V02| Adding more info for logging purposes
C----+------------------------------------------------------------------
                 TYPE *,'ERROR        : MESSAGE ALREADY TIMEOUT '
                 TYPE *,'TRX SERIAL   : ', PRO(SERIAL,RBUF)
                 TYPE *,'BUF          : ', RBUF
C----+------------------------------------------------------------------
C    | Get AGENTNR from message (bytes 1-4)
C----+------------------------------------------------------------------
                 I1TEMP(4) = ZEXT (MESS_FROM_IGS(1))
                 I1TEMP(3) = ZEXT (MESS_FROM_IGS(2))
                 I1TEMP(2) = ZEXT (MESS_FROM_IGS(3))
                 I1TEMP(1) = ZEXT (MESS_FROM_IGS(4))
                 TYPE *,'AGENT NUMBER : ',I4TEMP
 
C----+------------------------------------------------------------------
C    | Get TERMINALNUM from message (bytes 13-14)
C----+------------------------------------------------------------------
                 I1TEMP(4) = 0
                 I1TEMP(3) = 0
                 I1TEMP(2) = ZEXT (MESS_FROM_IGS(13))
                 I1TEMP(1) = ZEXT (MESS_FROM_IGS(14)) 
                 TERMINALNUM = I4TEMP
                 TYPE *,'TERM. NUMBER : ',I4TEMP
                 
                 TYPE *,'CDC DATE     : ',DAYCDC

C----+------------------------------------------------------------------
C    | Check if message is a retry (byte 15)
C----+------------------------------------------------------------------
                 IS_RETRY = (IAND(ZEXT(MESS_FROM_IGS(15)),'40'X) .NE. 0)
          
C----+------------------------------------------------------------------
C    | Get Message Type (MTYPE) from message (byte 16)
C----+------------------------------------------------------------------
                 MTYPE = ZEXT(MESS_FROM_IGS(16))
                 MSUBTYPE = MOD(MTYPE,16)
                 MTYPE = ISHFT(MTYPE,-4)
                 
                 
                 ! Handling only IGS messages
                 IF (MTYPE .EQ. 14) THEN
                     MSG_GAME_TYPE  = 0
                     MSG_GAME_INDEX = 0
C----+------------------------------------------------------------------
C    | Handling wager messages (auto-cancellation enabled)
C----+------------------------------------------------------------------
                     IF(MSUBTYPE .EQ. 0) THEN
                         MSG_GAME_TYPE  = ZEXT(MESS_FROM_IGS(19))
                         MSG_GAME_INDEX = ZEXT(MESS_FROM_IGS(20))
                     ENDIF
                     IF(   MSUBTYPE       .EQ. 0
     *               .AND. .NOT. IS_RETRY
     *               .AND. MSG_GAME_TYPE  .NE. 0
     *               .AND. MSG_GAME_INDEX .NE. 0
     *               .AND. (     (P(SUPCAN)  .NE. 1) 
     *                     .AND. (P(IGSPPLA) .NE. 1)
     *                     .AND. (P(IGSPCAN) .NE. 1)
     *                     .AND. (.NOT. IGSGAMFLG(P(IGSPGCAN),
     *                                      IGS_GAMNUM(MSG_GAME_TYPE,
     *                                                 MSG_GAME_INDEX)
     *                                           )
     *                           )
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
     *                     .AND. (P(IGSPICAN) .NE. 1)
     *                     .AND. (.NOT. IGSGAMFLG(P(IGSPGICAN),
     *                                      IGS_GAMNUM(MSG_GAME_TYPE,
     *                                                 MSG_GAME_INDEX)
     *                                           )
     *                           )
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
     *                     )
     *               ) THEN
                         IF(IGSDEBUG(IA_COMIGS)) THEN
                            CALL OPSTXT('641:GETFROMIGS:AUTO-CANCELLATION')
                         ENDIF
C                        CALL XWAIT(100,1,ST2) ! Wait 100 msec
                         OFFSET = 0
C----+------------------------------------------------------------------
C    | Put AGENTNR at the beginning (bytes 1-4)
C----+------------------------------------------------------------------
                         MESS_TO_IGS(OFFSET + 1) = ZEXT (MESS_FROM_IGS(1))
                         MESS_TO_IGS(OFFSET + 2) = ZEXT (MESS_FROM_IGS(2))
                         MESS_TO_IGS(OFFSET + 3) = ZEXT (MESS_FROM_IGS(3))
                         MESS_TO_IGS(OFFSET + 4) = ZEXT (MESS_FROM_IGS(4))
C----+------------------------------------------------------------------
C    | Put MessageQ sequence number in message (bytes 5-8)
C----+------------------------------------------------------------------
                         MESSERIAL = MESSERIAL + 1
                         I4TEMP = 0
                         I4TEMP = MESSERIAL
                         MESS_TO_IGS(OFFSET + 5) = I1TEMP(4)
                         MESS_TO_IGS(OFFSET + 6) = I1TEMP(3)
                         MESS_TO_IGS(OFFSET + 7) = I1TEMP(2)
                         MESS_TO_IGS(OFFSET + 8) = I1TEMP(1)
C----+------------------------------------------------------------------
C    | Put Millennium Buffer Number in message (bytes 9-10)
C----+------------------------------------------------------------------
                         MESS_TO_IGS(OFFSET + 9) = 'FF'X
                         MESS_TO_IGS(OFFSET +10) = 'FF'X
C----+------------------------------------------------------------------
C    | Put Millennium CDC date in message (bytes 11-12)
C----+------------------------------------------------------------------
                         I4TEMP = DAYCDC
                         MESS_TO_IGS(OFFSET +11) = I1TEMP(2)
                         MESS_TO_IGS(OFFSET +12) = I1TEMP(1)
C----+------------------------------------------------------------------
C    | Put Terminal Number in message (bytes 13-14)
C----+------------------------------------------------------------------
                         MESS_TO_IGS(OFFSET +13) = ZEXT (MESS_FROM_IGS(13))
                         MESS_TO_IGS(OFFSET +14) = ZEXT (MESS_FROM_IGS(14))
C----+------------------------------------------------------------------
C    | Create cancel message to send to IGS
C----+------------------------------------------------------------------
                         OFFSET = 14
                         !----------------------------------------------
                         ! IGS Cancellation Header
                         !----------------------------------------------
                         ! Control/Sequence
                         OUTTABTO( 1) = ZEXT (MESS_FROM_IGS(OFFSET + 1))
                         ! Type/Sub-type
                         OUTTABTO( 2) = 'E1'X
                         ! Message checksum seed 
                         I4TEMP = DAYCDC + TERMINALNUM   
                         OUTTABTO( 3) = I1TEMP(2)
                         OUTTABTO( 4) = I1TEMP(1)
                         ! Statistics 
                         OUTTABTO( 5) = 0
                         ! Game type
                         OUTTABTO( 6) = MSG_GAME_TYPE !ZEXT (MESS_FROM_IGS(OFFSET + 6))
                         ! Game index
                         OUTTABTO( 7) = MSG_GAME_INDEX !ZEXT (MESS_FROM_IGS(OFFSET + 7))
                         !----------------------------------------------
                         ! IGS Oddset Cancellation 
                         !----------------------------------------------
                         ! Agent number
                         OUTTABTO( 8) = ZEXT (MESS_FROM_IGS(1))
                         OUTTABTO( 9) = ZEXT (MESS_FROM_IGS(2))
                         OUTTABTO(10) = ZEXT (MESS_FROM_IGS(3))
                         OUTTABTO(11) = ZEXT (MESS_FROM_IGS(4))
                         !----------------------------------------------
                         ! Message Id
                         ! IMPORTANT: THIS MUST BE CHANGED!!! (messageId
                         ! is not recorded in Millennium)
                         ! (FOR PHASE II)
                         !----------------------------------------------
                         I1TEMP(4)  = 0
                         I1TEMP(3)  = 0
                         I1TEMP(2)  = ZEXT (MESS_FROM_IGS(32))
                         I1TEMP(1)  = ZEXT (MESS_FROM_IGS(33)) 
                         BC_YEAR    = I4TEMP
                         BC_MONTH   = ZEXT (MESS_FROM_IGS(34))
                         BC_DAY     = ZEXT (MESS_FROM_IGS(35))
                         BC_HOUR    = ZEXT (MESS_FROM_IGS(36))
                         BC_MINUTE  = ZEXT (MESS_FROM_IGS(37))
                         BC_SECOND  = ZEXT (MESS_FROM_IGS(38))
                         
                         CALL GET_UNIX_TIME_MS(I8AUX)
C                        CALL CONVERT_TO_UNIX_TIME_MS(I8AUX
C    *                                       , BC_YEAR, BC_MONTH , BC_DAY
C    *                                       , BC_HOUR, BC_MINUTE, BC_SECOND)

C                        I8AUX = I8AUX + 1000 ! Hammering a new message Id (with an extra second)

                         OUTTABTO(12) = ZEXT (I1AUX( 8))
                         OUTTABTO(13) = ZEXT (I1AUX( 7))
                         OUTTABTO(14) = ZEXT (I1AUX( 6))
                         OUTTABTO(15) = ZEXT (I1AUX( 5))
                         OUTTABTO(16) = ZEXT (I1AUX( 4))
                         OUTTABTO(17) = ZEXT (I1AUX( 3))
                         OUTTABTO(18) = ZEXT (I1AUX( 2))
                         OUTTABTO(19) = ZEXT (I1AUX( 1))
                         ! Bet slip
                         OUTTABTO(20) = ZEXT (MESS_FROM_IGS(OFFSET +  7))
                         OUTTABTO(21) = ZEXT (MESS_FROM_IGS(OFFSET +  8))
                         OUTTABTO(22) = ZEXT (MESS_FROM_IGS(OFFSET +  9))
                         OUTTABTO(23) = ZEXT (MESS_FROM_IGS(OFFSET + 10))
                         OUTTABTO(24) = ZEXT (MESS_FROM_IGS(OFFSET + 11))
                         OUTTABTO(25) = ZEXT (MESS_FROM_IGS(OFFSET + 12))
                         OUTTABTO(26) = ZEXT (MESS_FROM_IGS(OFFSET + 13))
                         OUTTABTO(27) = ZEXT (MESS_FROM_IGS(OFFSET + 14))
                         OUTTABTO(28) = ZEXT (MESS_FROM_IGS(OFFSET + 15))
                         OUTTABTO(29) = ZEXT (MESS_FROM_IGS(OFFSET + 16))
                         OUTTABTO(30) = ZEXT (MESS_FROM_IGS(OFFSET + 17))
                         
                         BS_YEAR    = ZEXT (MESS_FROM_IGS(OFFSET +  7))
                         BS_MONTH   = ZEXT (MESS_FROM_IGS(OFFSET +  8))
                         BS_DAY     = ZEXT (MESS_FROM_IGS(OFFSET +  9))
                         BS_GAME_ID = ZEXT (MESS_FROM_IGS(OFFSET + 10))
                         I8AUX = 0
                         I1AUX(5)   = ZEXT (MESS_FROM_IGS(OFFSET + 11))
                         I1AUX(4)   = ZEXT (MESS_FROM_IGS(OFFSET + 12))
                         I1AUX(3)   = ZEXT (MESS_FROM_IGS(OFFSET + 13))
                         I1AUX(2)   = ZEXT (MESS_FROM_IGS(OFFSET + 14))
                         I1AUX(1)   = ZEXT (MESS_FROM_IGS(OFFSET + 15))
                         BS_SERIAL  = I8AUX
                         I8AUX = 0
                         I1AUX(2)   = ZEXT (MESS_FROM_IGS(OFFSET + 16))
                         I1AUX(1)   = ZEXT (MESS_FROM_IGS(OFFSET + 17))
                         BS_CHK_DIG = I4AUX(1)
                         TYPE 10001, BS_YEAR, BS_MONTH, BS_DAY, BS_GAME_ID
     *                             , BS_SERIAL, BS_CHK_DIG
                         IF(IGSDEBUG(IA_COMIGS)) THEN
                            WRITE(LINE,10001) BS_YEAR, BS_MONTH, BS_DAY, BS_GAME_ID
     *                                , BS_SERIAL, BS_CHK_DIG
                            CALL OPSTXT(TRIM(LINE))
                         ENDIF
10001                    FORMAT('Bet External Reference Serial # ',I4.4,'/',I2.2,'/',I2.2,'-',I2.2,'-',I10.10,'-',I3.3)
C----+------------------------------------------------------------------
C    | Calculate checksum
C----+------------------------------------------------------------------
                         I4CCITT = DAYCDC + TERMINALNUM
                         OUTTABTO(3) = I1CCITT(2)
                         OUTTABTO(4) = I1CCITT(1)
                         CHKLEN = 30
                         CALL GETCCITT(OUTTABTO,1,CHKLEN - 1,MYCHKSUM)
                         I4CCITT = MYCHKSUM
                         OUTTABTO(3) = I1CCITT(2)
                         OUTTABTO(4) = I1CCITT(1)   
C----+------------------------------------------------------------------
C    | Put message from Altura ((MESSLEN - 15) bytes )
C----+------------------------------------------------------------------
                         OFFSET = 14
                         DO ITO = 1,CHKLEN
                             MESS_TO_IGS(OFFSET + ITO) = OUTTABTO(ITO)
                         ENDDO
C----+------------------------------------------------------------------
C    | Setting message length sent to MessageQ
C    |    MESS_TO_LEN = MESLEN + 15
C----+------------------------------------------------------------------
                         MESS_TO_LEN = CHKLEN + OFFSET
                         
                         TYPE *,' '
                         DO ITO = 1,MESS_TO_LEN 
                            TYPE 9993,ITO,MESS_TO_IGS(ITO)
                         ENDDO

                         IF(IGSDEBUG(IA_COMIGS)) THEN
                             CALL DUMP_MESSAGE(0,793,MESS_TO_IGS,MESS_TO_LEN)
                         ENDIF
C----+------------------------------------------------------------------
C    | Calling C function to put message into MessageQ (Queue = 1)
C----+------------------------------------------------------------------
                         CALL MESSQ_PUT(%REF(STATUSTO)) ! the name is the same as the function in c
                         IF (STATUSTO .EQ. PAMS__SUCCESS) THEN !if it was successfull in puting the message into the queue the status is equal to integer value in PAMS__SUCCESS
                            TYPE *,' '
                            TYPE *,'IGS WAGER: INTERNAL CANCEL OK'
                            IF(IGSDEBUG(IA_COMIGS)) THEN
                                CALL OPSTXT('IGS WAGER: INTERNAL CANCEL OK')
                            ENDIF
                         ELSE
                            TYPE *,' '
                            TYPE *,'IGS WAGER: INTERNAL CANCEL FAIL'
                            IF(IGSDEBUG(IA_COMIGS)) THEN
                                CALL OPSTXT('IGS WAGER: INTERNAL CANCEL FAIL')
                            ENDIF
                         ENDIF
C----+------------------------------------------------------------------
C    | Handling wager messages : retries
C----+------------------------------------------------------------------
                     ELSEIF(MSUBTYPE      .EQ. 0
     *               .AND. IS_RETRY
     *               .AND. MSG_GAME_TYPE  .NE. 0
     *               .AND. MSG_GAME_INDEX .NE. 0
     *               .AND. (     (P(SUPCAN)  .NE. 1) 
     *                     .AND. (P(IGSPPLA) .NE. 1)
     *                     .AND. (P(IGSPCAN) .NE. 1)
     *                     .AND. (.NOT. IGSGAMFLG(P(IGSPGCAN),
     *                                      IGS_GAMNUM(MSG_GAME_TYPE,
     *                                                 MSG_GAME_INDEX)
     *                                           )
     *                           )
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
     *                     .AND. (P(IGSPICAN) .NE. 1)
     *                     .AND. (.NOT. IGSGAMFLG(P(IGSPGICAN),
     *                                      IGS_GAMNUM(MSG_GAME_TYPE,
     *                                                 MSG_GAME_INDEX)
     *                                           )
     *                           )
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
     *                     )
     *               ) THEN
                         TYPE *,'IGS WAGER: NO INTERNAL CANCEL FOR RETRIES'
                         IF(IGSDEBUG(IA_COMIGS)) THEN
                             CALL OPSTXT('IGS WAGER: NO INTERNAL CANCEL FOR RETRIES')
                         ENDIF
C----+------------------------------------------------------------------
C    | Handling wager messages (auto-cancellation disabled)
C----+------------------------------------------------------------------
                     ELSEIF(MSUBTYPE       .EQ. 0
     *               .AND.  MSG_GAME_TYPE  .NE. 0
     *               .AND.  MSG_GAME_INDEX .NE. 0
     *               .AND.  (     (P(SUPCAN)  .EQ. 1) 
     *                      .OR.  (P(IGSPPLA) .EQ. 1)
     *                      .OR.  (P(IGSPCAN) .EQ. 1)
     *                      .OR.  (IGSGAMFLG(P(IGSPGCAN),
     *                                      IGS_GAMNUM(MSG_GAME_TYPE,
     *                                                 MSG_GAME_INDEX)
     *                                      )
     *                            )
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
     *                      .OR. (P(IGSPICAN) .EQ. 0)
     *                      .OR. (IGSGAMFLG(P(IGSPGICAN),
     *                                      IGS_GAMNUM(MSG_GAME_TYPE,
     *                                                 MSG_GAME_INDEX)
     *                                           )
     *                           )
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
     *                      )
     *               ) THEN
                         TYPE *,'IGS WAGER: INTERNAL CANCEL DISABLED'
                         IF(IGSDEBUG(IA_COMIGS)) THEN
                             CALL OPSTXT('IGS WAGER: INTERNAL CANCEL DISABLED')
                         ENDIF
C----+------------------------------------------------------------------
C    | Handling cancellation messages
C----+------------------------------------------------------------------
                     ELSEIF(MSUBTYPE .EQ. 1) THEN
                         TYPE *,'IGS CANCELLATION: NO INTERNAL CANCEL'
                         IF(IGSDEBUG(IA_COMIGS)) THEN
                             CALL OPSTXT('IGS CANCELLATION: NO INTERNAL CANCEL')
                         ENDIF
C----+------------------------------------------------------------------
C    | Handling validation messages
C----+------------------------------------------------------------------
                     ELSEIF(MSUBTYPE .EQ. 2) THEN
                         TYPE *,'IGS VALIDATION: NO INTERNAL CANCEL'
                         IF(IGSDEBUG(IA_COMIGS)) THEN
                             CALL OPSTXT('IGS VALIDATION: NO INTERNAL CANCEL')
                         ENDIF
C----+------------------------------------------------------------------
C    | Handling payment messages
C----+------------------------------------------------------------------
                     ELSEIF(MSUBTYPE .EQ. 3) THEN
                         TYPE *,'IGS PAYMENT: NO INTERNAL CANCEL'
                         IF(IGSDEBUG(IA_COMIGS)) THEN
                             CALL OPSTXT('IGS PAYMENT: NO INTERNAL CANCEL')
                         ENDIF
C----+------------------------------------------------------------------
C    | Handling game programme report messages
C----+------------------------------------------------------------------
                     ELSEIF(MSUBTYPE .EQ. 4) THEN
                         TYPE *,'IGS GAME PROG REP: NO INTERNAL CANCEL'
                         IF(IGSDEBUG(IA_COMIGS)) THEN
                             CALL OPSTXT('IGS GAME PROG REP: NO INTERNAL CANCEL')
                         ENDIF
C----+------------------------------------------------------------------
C    | Handling other messages
C----+------------------------------------------------------------------
                     ELSE
                         TYPE *,'IGS OTHER MSGS: NO INTERNAL CANCEL'
                         IF(IGSDEBUG(IA_COMIGS)) THEN
                             CALL OPSTXT('IGS OTHER MSGS: NO INTERNAL CANCEL')
                         ENDIF
                     ENDIF
                 ELSE
                     TYPE *,'NOT AN IGS MESSAGE'
                     IF(IGSDEBUG(IA_COMIGS)) THEN
                         CALL OPSTXT('NOT AN IGS MESSAGE')
                     ENDIF
                 ENDIF
                 TYPE *,'LEN          : ', MESS_FROM_LEN
                 TYPE *,' ' 
                 DO I=1,MESS_FROM_LEN
                    TYPE 9995,I,MESS_FROM_IGS(I) 
                 ENDDO
                 TYPE *,' ERROR TIME OUT END'
                 TYPE *,' '          
                 IF(IGSDEBUG(IA_COMIGS)) THEN
                     CALL DUMP_MESSAGE(0,874,MESS_FROM_IGS,MESS_FROM_LEN)
                     CALL OPSTXT(' ERROR TIME OUT END')
                 ENDIF
                 GOTO 20
             ENDIF
          ENDIF
          
          
          DO I=1, MESS_FROM_LEN    
             BPRO(WRKTAB*4-3+I,RBUF) = MESS_FROM_IGS(I)
          ENDDO
          
C----+------------------------------------------------------------------
C    | IMPORTANT: SCML - 2014.05.05
C    |    Here we check the input message type/sub-type to see if it
C    | is a financial report '63'x; we also check the incoming result 
C    | from IGS:
C    |    If it is of type error 'EF'x then we DO NOT UPDATE its length.
C    | This is an EXCEPTION that we open, in order to disturb the LEAST 
C    | possible the usual message flow through MILLENNIUM.
C----+------------------------------------------------------------------
          ITYPE    =     ZEXT(BPRO(BINPTAB + 1, RBUF)) / 16
          ISUBTYPE = MOD(ZEXT(BPRO(BINPTAB + 1, RBUF)) , 16)
          
          OTYPE    =     ZEXT(BPRO((WRKTAB*4-3) + 14 + 2, RBUF)) / 16
          OSUBTYPE = MOD(ZEXT(BPRO((WRKTAB*4-3) + 14 + 2, RBUF)) , 16)
          
          IF(IGSDEBUG(IA_COMIGS)) THEN
             CALL OPS('1202:GETFROMIGS:ITYPE',ITYPE,ITYPE)
             CALL OPS('1202:GETFROMIGS:ISUBTYPE',ISUBTYPE,ISUBTYPE)
             CALL OPS('1202:GETFROMIGS:OTYPE',OTYPE,OTYPE)
             CALL OPS('1202:GETFROMIGS:OSUBTYPE',OSUBTYPE,OSUBTYPE)
             CALL OPS('1202:GETFROMIGS:HPRO(INPLEN,RBUF)',ZEXT(HPRO(INPLEN,RBUF)),ZEXT(HPRO(INPLEN,RBUF)))
          ENDIF
          IF(
     *        .NOT. (
     *              ITYPE .EQ.  6 .AND. ISUBTYPE .EQ.  3
     *        .AND. OTYPE .EQ. 14 .AND. OSUBTYPE .EQ. 15
     *        ) 
     *    ) THEN
              HPRO(OUTLEN,RBUF) = MESS_FROM_LEN
          ENDIF
          IF(IGSDEBUG(IA_COMIGS)) THEN
             CALL OPS('1217:GETFROMIGS:HPRO(INPLEN,RBUF)',ZEXT(HPRO(INPLEN,RBUF)),ZEXT(HPRO(INPLEN,RBUF)))
          ENDIF
C----+------------------------------------------------------------------
C    | IMPORTANT: SCML - 2014.05.05
C    |    Here we check the input message type/sub-type to see if it
C    | is a financial report '63'x; we also check the incoming result 
C    | from IGS:
C    |    If it is of type error 'EF'x then we DO NOT UPDATE its length.
C    | This is an EXCEPTION that we open, in order to disturb the LEAST 
C    | possible the usual message flow through MILLENNIUM.
C----+------------------------------------------------------------------
          
          HPRO(TRCODE,RBUF) = TYPIGS
          HPRO(REMSTS,RBUF) = RMGOOD
          
123       CONTINUE          
C----+------------------------------------------------------------------
C    | Put it into DISPAT to send it to OUTIGS (?)
C----+------------------------------------------------------------------
          CALL ABL(RBUF,QUETAB(1,DIS),ST)
          IF(IGSDEBUG(IA_COMIGS)) THEN
              IF(HPRO(INPLEN,RBUF) .LT. 1000) THEN
                  CALL DUMP_MESSAGE(0,756,BPRO(BINPTAB,RBUF),HPRO(INPLEN,RBUF))
              ELSE
                  CALL OPS('758:MESSAGE LENGTH TOO LARGE!!!',HPRO(INPLEN,RBUF),HPRO(INPLEN,RBUF))
              ENDIF
              IF(MESS_FROM_LEN .LT. 1000) THEN
                  CALL DUMP_MESSAGE(RBUF,761,BPRO(WRKTAB*4-3+1,RBUF),MESS_FROM_LEN)
              ELSE
                  CALL OPS('763:MESSAGE LENGTH TOO LARGE!!!',MESS_FROM_LEN,MESS_FROM_LEN)
              ENDIF
          ENDIF
          GOTO 20
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

9990    FORMAT(' EXTERNAL SERIAL: ',I8.8,'-',I3.3)
9991    FORMAT(' DEBUG: ', I8.8, '-', I3.3, I)     
9995    FORMAT(' ERROR TIME OUT MESSWORD RECB: ',I3.1,' - ', Z3.2)
9993    FORMAT(' CANCEL MESSAGE: ',I2.1,' - ', Z3.2)
9999    FORMAT(' MESSWORD RECB: ',I2.1,' - ', Z3.2)
        END



C----+-----------------------------------------------------------------
C    | SUBROUTINE CHKTIMEOUT
C    |    This subroutine checks if there are any expired transactions
C----+-----------------------------------------------------------------
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CHKTIMEOUT
        IMPLICIT NONE
C**************************************************
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:IGSCON.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:APUCOM.DEF'        

        INTEGER*4   XRFNUM          !Cross reference number
        INTEGER*4   PBUF            !Procom buffer number
        INTEGER*4   ST              !Status

        INTEGER*4   MESS(EDLEN)
        INTEGER*4   I4TEMP
        INTEGER*2   I2TEMP(2)
        BYTE        I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)

        LOGICAL*1   TIMEOUT         !Timeout Flag
        
        LOGICAL    NO_TIMEOUTS
        INTEGER*4 I, LIST_INDEX, TIMEOUT_CNT

        MESS(1) = IGC

C----+------------------------------------------------------------------
C    | Check for any timed-out transactions
C----+------------------------------------------------------------------
        NO_TIMEOUTS = .FALSE.
        DO WHILE ( NO_TIMEOUTS .EQ. .FALSE. )
            TIMEOUT_CNT = 0
            DO I = 1, IGS_MAX_TIMER_LISTS !PARAMETER (IGS_MAX_TIMER_LISTS = 2) -> so there is two possible time outs?? one for reports and another for everything else
                CALL CHKTIMER(I,XRFNUM,PBUF) !PBUF
                IF(PBUF .NE. 0) THEN
C----+------------------------------------------------------------------
C    | A timed-out buffer has been found.
C    | Set status to TIMED OUT, and set the transaction code allowing
C    | DISPAT to queue it to the appropriate processing task.
C----+------------------------------------------------------------------

C----+------------------------------------------------------------------
C    | Get XREFNUM from WRKTAB (bytes 5-8 + offset)
C----+------------------------------------------------------------------
                   I4TEMP = 0
                   I1TEMP(1) = BPRO(WRKTAB*4-3+5,PBUF)
                   I1TEMP(2) = BPRO(WRKTAB*4-3+6,PBUF)
                   I1TEMP(3) = BPRO(WRKTAB*4-3+7,PBUF)
                   I1TEMP(4) = BPRO(WRKTAB*4-3+8,PBUF)
                   XRFNUM = I4TEMP                   !MESSAGE SENT #
        
                   HPRO(TRCODE,PBUF) = TYPIGS
                   IF(I .EQ. IGS_TL_MAIN) THEN
                       CALL OPS('BUFFER EXPIRADO:',PBUF,PBUF)
                   ELSE 
                       CALL OPS('FIN BUFFER EXPIRADO:',PBUF,PBUF)
                   ENDIF
                   HPRO(REMSTS,PBUF) = RMTMOT  

C----+------------------------------------------------------------------
C    | Get TRABUF from APUBUF
C----+------------------------------------------------------------------
                   CALL LOGTRA(TRABUF,APUBUF(2,PBUF))               

C----+------------------------------------------------------------------
C V03| Bugfix with transaction recording while in Duplex Mode
C----+------------------------------------------------------------------
C                  IF (TRABUF(TTYP) .NE. TSPE) THEN
                   IF ( (TRABUF(TTYP) .NE. TSPE)
     *             .OR. (TRABUF(TTYP) .EQ. TSPE .AND. TRABUF(TSFUN).EQ.TREPR) )
     *             THEN
C----+------------------------------------------------------------------
C V03| Bugfix with transaction recording while in Duplex Mode
C----+------------------------------------------------------------------
                      TRABUF(TERR) = BCRS
                      TRABUF(TSTAT) = REJT
                   ENDIF
         
                   CALL TRALOG(TRABUF,APUBUF(2,PBUF))
                   CALL ABL(PBUF,QUETAB(1,DIS),ST)
                   TIMEOUT_CNT = TIMEOUT_CNT + 1
                ENDIF
            ENDDO
C----+------------------------------------------------------------------
C    | If there are no more time-outs, then exit
C----+------------------------------------------------------------------
            IF(TIMEOUT_CNT .EQ. 0) THEN
                NO_TIMEOUTS = .TRUE.
            ENDIF
        ENDDO

        RETURN
        END


C----+-----------------------------------------------------------------
C    | SUBROUTINE CHECKPROCESS
C    |    This subroutine checks the status of the INIGS and OUTIGS 
C    |    processes. If they are down, restarts them.
C----+-----------------------------------------------------------------
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CHECKPROCESS()
        IMPLICIT NONE
C**************************************************
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'

        INTEGER*4  TSKSTS,STATUS
C----+------------------------------------------------------------------
C    | If OUTIGS is down, then restart OUTIGS
C----+------------------------------------------------------------------
        STATUS = 0 
        CALL STTSK(8HOUTIGS  ,TSKSTS,STATUS) !VERIFY IF OUTIGS IS OK
        IF (STATUS .EQ. 4) THEN
           CALL OPSTXT('ERROR!!!!! OUTIGS IS NOT RUNNING')
           CALL OPSTXT('STARTING OUTIGS AND TRY TO PROCESS ALL TRANSACTIONS ')
           CALL START(8HOUTIGS  )
        ENDIF
C----+------------------------------------------------------------------
C    | If INIGS is down, then restart INIGS
C----+------------------------------------------------------------------
        STATUS = 0 
        CALL STTSK(8HINIGS   ,TSKSTS,STATUS) ! VERIFY IF INIGS IS OK
        IF (STATUS .EQ. 4) THEN
           CALL OPSTXT('ERROR!!!!! INIGS IS NOT RUNNING')
           CALL OPSTXT('STARTING INIGS AND TRY TO PROCESS ALL TRANSACTIONS ')
           CALL START(8HINIGS   )
        ENDIF
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
C    | SUBROUTINE GET_TIMER_LIST_INDEX
C    |    This subroutine returns the timer list index to which a 
C    |    buffer must be associated, given its type
C    +-----------------------------------------------------------------
C    | INPUT PARAMETERS:
C    |    MESSAGE_TYPE  Message type
C    +-----------------------------------------------------------------
C    | OUTPUT PARAMETERS:
C    |    LIST_INDEX    Timer list index
C----+-----------------------------------------------------------------
        SUBROUTINE GET_TIMER_LIST_INDEX(MESSAGE_TYPE, LIST_INDEX)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:IGSCON.DEF'

        INTEGER*4 MESSAGE_TYPE
        INTEGER*4 LIST_INDEX
        
C----+------------------------------------------------------------------
C    | By default, assign buffer to main timer list
C----+------------------------------------------------------------------
        LIST_INDEX = IGS_TL_MAIN !PARAMETER (IGS_TL_MAIN = 1) -> by default its the first index of the list 
        
C----+------------------------------------------------------------------
C    | If message type is 6 (reports) then assign buffer to cross timer
C    | list
C----+------------------------------------------------------------------
        IF ( MESSAGE_TYPE .EQ. 6 ) THEN
            LIST_INDEX = IGS_TL_CRS !PARAMETER (IGS_TL_CRS = 2) -> second position of the array
        ENDIF
        RETURN
        END
        
        
        
        
        
        ! esta função faz dump da mensagem para imprimir na linha de comandos?
        SUBROUTINE DUMP_MESSAGE(MESSAGE_ID, LINE_ID, OUTBUF, MESLEN) ! what does this function do?????
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        BYTE OUTBUF(*)
        INTEGER*4 MESLEN
        INTEGER*4 MESSAGE_ID, LINE_ID

        CHARACTER*255 BUF
        CHARACTER*3 ARR(16)
        INTEGER*4 I, J, K, DIV, REMAIN, OFFSET
        
        DO I = 1, 255
            BUF(I:I) = CHAR(0)
        ENDDO
        
        DIV = MESLEN / 16
        REMAIN = MOD(MESLEN,16)
        
        WRITE(BUF, 900) MESSAGE_ID, LINE_ID, MESLEN
        TYPE *, IAM(), '', TRIM(BUF)
        CALL OPSTXT(TRIM(BUF))
        
        DO K = 1, DIV
           DO I = 1, 16
               DO J = 1, 2
                   ARR(I)(J:J) = ' '
               ENDDO
               ARR(I)(3:3) = CHAR(0)
           ENDDO
           DO I = 1, 16
               OFFSET = ((K - 1) * 16) + I
               WRITE(ARR(I), 901) OUTBUF(OFFSET)
           ENDDO
           OFFSET = ((K - 1) * 16)
           WRITE(BUF, 902) OFFSET + 1, OFFSET + 16,( ARR(I), I = 1, 16)
           TYPE *, '', TRIM(BUF)
           CALL OPSTXT(TRIM(BUF))
        ENDDO
        IF(REMAIN .NE. 0) THEN
           DO I = 1, 16
               DO J = 1, 2
                   ARR(I)(J:J) = ' '
               ENDDO
               ARR(I)(3:3) = CHAR(0)
           ENDDO
           DO I = 1, REMAIN
               OFFSET = ((K - 1) * 16) + I
               WRITE(ARR(I), 901) OUTBUF(OFFSET)
           ENDDO
           OFFSET = ((K - 1) * 16)
           WRITE(BUF, 902) OFFSET + 1, OFFSET + REMAIN, (ARR(I), I = 1, 16)
           TYPE *, '', TRIM(BUF)
           CALL OPSTXT(TRIM(BUF))
        ENDIF
        TYPE *, ''

900     FORMAT('PARSED MESSAGE #',I8,' (@ LINE #',I8,') : LEN = ', I8)
901     FORMAT(Z2.2)
902     FORMAT('[',I4,':',I4,'] = ',16(A2,1X))

        RETURN
        END

        
