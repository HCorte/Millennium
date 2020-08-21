C
C PROGRAM COMMGR
C
C COMMGR.FOR
C
C V07 07-OCT-2016 SCML Avoid calling messq_exit when P(EUMILF)=0
C                      Send an error message to error log when COMMGR can not
C                      attach to Euromillions system
C V06 01-APR-2016 SCML M16 PROJECT: added global parameters checking regarding
C                      financial reports (EUSPFIR) and internal cancellations
C                      (EUSPICA and EUSPGICA)
C                      Get next cross reference number of message to be sent to
C                      Euromillions system using new subroutine GETEURXRF
C                      Fixed time to time out game results reports and billing
C                      reports (only financial reports use EUFINTO time out 
C                      value; other reports use EUTIMOUT time out value).
C V05 08-SEP-2015 SCML Bugfix for wrong messages sent when end-of-day 
C                      occurs in Euromillions
C V04 07-APR-2014 SCML Added support to PLACARD Project - IGS
C V03 12-APR-2011 FJG  ACCENTURE MERGE FOR EM2
C V02 26-JAN-2011      Array access revision (access to arrays MESS_TO_EUROMIL and 
C                      MESS_FROM_EUROMIL corrected. In some situations, the access
C                      to first array element was taken using index zero and not one 
C                      as it should).
C V01        2004      INITIAL RELEASE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2004 SCML/Accenture. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM COMMGR
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
C        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:APUCOM.DEF'
        INCLUDE 'INCLIB:EURCON.DEF'
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:IGSDEBUG.DEF'
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:EURCOM.DEF'                                             !V06
C
        INTEGER*4  MESS(EDLEN)       !
        INTEGER*4  TASK              !
        INTEGER*4  BUF              !
        INTEGER*4  STATUS,ST
        INTEGER*4  MESSERIAL
        LOGICAL    CONEURO,FIRSTRUN
        
        CALL OPSTXT(' Copyright 2004 SCML/Accenture. All rights reserved. ')
        CALL SNIF_AND_WRKSET

        TASK    = EUC
        MESS(1) = TASK
CV06        MESSERIAL = 0
        MESSERIAL = EURS_NXTXRF                                                 !V06
        CONEURO = .FALSE.
        FIRSTRUN = .FALSE.
        EURS_ATTACHSTS = 0                                                      !NOT ATTACHED TO MESSAGEQ SERVER !V06
        EURS_DETACHFLG = 0                                                      !A DETACHED FROM MESSAGEQ SERVER HAS NOT OCCURRED !V06

C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        CALL RESET_TIMERS
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
        CALL CLR_EURS_ATTACHDATTIM                                              !V06
        CALL CLR_EURS_DETACHDATTIM                                              !V06
C
        CALL OPSTXT(' ******************* COMMGR ****************')
C
C WAIT FOR SOMETHING TO DO
C IF END OF DAY THEN CALL GSTOP(GEXIT_SUCCESS)
C
10      CONTINUE
C
C IF DAYCLOSE THEN SEND STOP... IF SYSTEM IS LIVE THEN DISCONNECT TO EURO MILHOES MESSAGEQ
C
        IF (DAYSTS .EQ. DSCLOS)  THEN
           IF (P(SYSTYP) .EQ. LIVSYS) THEN 
              CALL MESSQ_EXIT(%REF(ST))
              CALL OPSTXT('EURO MILHOES IS DISCONNECTED')
C             GET DATE AND TIME OF DETACH
              IF(ST .EQ. PAMS__SUCCESS) THEN
                CALL GET_EURS_DETACHDATTIM              !V06
                EURS_DETACHFLG = 1                                              !DETACHED FROM MESSAGEQ SERVER
              ENDIF
           ENDIF
           CONEURO = .FALSE.
           P(EUMILF) = 0
           CALL CLR_EURS_ATTACHDATTIM                                           !CLEAR ATTACH TIME STAMP !V06
           EURS_ATTACHSTS = 0                                                   !NOT ATTACHED TO MESSAGE QSERVER !V06
           EURS_DETACHFLG = 1                                                   !DETACHED FROM MESSAGEQ SERVER !V06
           CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
C
C IF NOT LIVE SYSTEM THEN WAIT 
C
        IF (P(SYSTYP) .NE. LIVSYS) THEN
           CALL XWAIT(5,2,ST)
           CALL CHECKPROCESS()
           FIRSTRUN = .TRUE.
C           IF (FIRSTRUN .EQ. .TRUE.) THEN
C              P(EUTIMOUT) = 20 	! TIME OUT MESSAGES
C              FIRSTRUN = .FALSE.
C           ENDIF
   	   GOTO 10
        ENDIF
C
C IF DAY IS SUSPENDED STAY ON HOLD
C 
        IF(DAYSTS .EQ. DSSUSP) THEN
          CALL HOLD(0,STATUS)
          IF(DAYSTS .EQ. DSOPEN) GOTO 10
          GOTO 10
        ENDIF
C
C IF NO CONNECTION TO EURO MILHOES WAIT
C
C        IF (P(EUMILF) .EQ. 0) THEN 
C           CALL XWAIT(5,2,ST)
C   	   GOTO 10
C        ENDIF
C
C IF NO CONNECTION TO EURO MILHOES THEN TRY TO CONNECT TO MESSAGEQ 
C IF ONE ERROR OCCURS SEND ERROR MESSAGE AND TRY AGAIN
C
543     CONTINUE
        IF (P(EUMILF) .EQ. 0) GOTO 333
        IF (CONEURO .EQ. .FALSE.) THEN
           CALL MESSQ_ATTACH(%REF(ST))
           IF (ST .NE. PAMS__SUCCESS) THEN
              CALL OPS('ERROR!!!! CAN NOT ATTACH TO MESSAGEQ!!!!',ST,0)
              CALL MESSQ_EXIT(%REF(ST))
              P(EUMILF) = 0
C
CV06              CALL OPSTXT('ERROR!!!! CAN NOT ATTACH TO MESSAGEQ!!!!')
C             GET DATE AND TIME OF DETACH
              IF(ST .EQ. PAMS__SUCCESS) THEN
                CALL GET_EURS_DETACHDATTIM                                      !V06
                EURS_DETACHFLG = 1                                              !DETACHED FROM MESSAGEQ SERVER !V06
                CALL CLR_EURS_ATTACHDATTIM                                      !CLEAR ATTACH TIME STAMP !V06
                EURS_ATTACHSTS = 0                                              !NOT ATTACHED TO MESSAGEQ SERVER !V06
C----+---+--------------------------------------------------------------
C V07|BEG| SEND EUROMILLIONS MESSAGE DISCONNECT
C----+---+--------------------------------------------------------------
                MESS(2) = TEEUM
                MESS(3) = 2                                                     !SEND EUROMILLIONS DISCONNECT MESSAGE
                CALL QUEMES(MESS)
C----+---+-------------+------------------------------------------------
C V07|END| SEND EUROMILLIONS MESSAGE DISCONNECT
C----+---+-------------+------------------------------------------------
              ENDIF
C
              GOTO 10 	! ST = 1 ATTACH SUCESS
           ENDIF
C          GET DATE AND TIME OF ATTACHMENT
           CALL GET_EURS_ATTACHDATTIM                                           !V06
           EURS_ATTACHSTS = 1                                                   !ATTACHED SUCCESSFULLY TO MESSAGEQ SERVER !V06
C
C  SEND MESSAGE - EURO MILHOES CONNECT          
           MESS(2) = TEEUM
           MESS(3) = 1
           CALL QUEMES(MESS)
           
           CONEURO = .TRUE.
           P(EUMILF) = 1
        ENDIF
C
C Get from EUROMILHOES data for SIGN ON
C and save it in one file
C
333     CONTINUE 
C
C SEE IF OUTMGR AND INMGR ARE RUNNING IF NOT START THEM
C
        CALL CHECKPROCESS()     
C
C GET BUFFER NUMBER FROM TOP OF QUEUE.
C IF NO WAGERS QUEUED, GO BACK TO WAIT STATE.
C
        CALL XWAIT(1,2,ST) ! WAIT FOR 1 SEC        
C
C
C CALL GET FROM EURO MILHOES FUNCTION IS USED TO GET FROM EURO MIL 
C ALL RESPONSE MESSAGES - ONLY IF STATUS IS NO MORE MESSAGES 
C
        ST = PAMS__NOMOREMSG
        IF (P(EUMILF) .NE. 0) CALL GETFROMEUROMIL(ST,MESSERIAL)
C        IF ((ST .EQ. PAMS__NETERROR) .OR. (ST .EQ. PAMS__STUB)) THEN
        IF ((ST .NE. PAMS__SUCCESS) .AND. (ST .NE. PAMS__NOMOREMSG))THEN	
           CALL MESSQ_EXIT(%REF(ST))
           IF (ST .EQ. PAMS__SUCCESS) THEN
              CONEURO = .FALSE.
C             GET DATE AND TIME OF DETACH
              CALL GET_EURS_DETACHDATTIM                                        !V06
              EURS_DETACHFLG = 1                                                !V06
              CALL CLR_EURS_ATTACHDATTIM                                        !CLEAR ATTACH TIME STAMP !V06
              EURS_ATTACHSTS = 0                                                !V06
C
              GOTO 543
           ENDIF
           CALL OPS('ERROR: MESSAGEQ CAN NOT BE DETACHED!!!',ST,0)
           GOTO 10
        ENDIF
C CALL CHECK TIME OUT FUNCTION TO SEE IF THERE ARE MESSAGES 
C SENT TO EURO MIL AND DO NOT HAVE RESPONSE
C IF SO THEN DELETE BUFFER AND SEND TO ALTURA ONE ERROR MESSAGE
C
        CALL CHKTIMEOUT

C
C IF NO MORE RESPONSE MESSAGES THEN START TO SEND ALL MESSAGES TO EURO MIL 
C UNTIL THERE ARE NO MORE BUFFER TO PROCESS
C
20      CONTINUE
        CALL TOPQUE(TASK,BUF)
        IF(BUF .EQ. 0) GOTO 10
c        CALL LOGTRA(TRABUF,APUBUF(32,BUF))
c        call ops('TRABUF(TAGT)',TRABUF(TAGT))
c        CALL OPS('COMMGR TYPE1: ',TRABUF(TTYP))
C
C THIS FUNCTION IS USED TO PUT INTO MESSAGEQ THE MESSAGE FROM ALTURA 
C        
        ST = PAMS__SUCCESS
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF(IGSDEBUG(IA_COMMGR)) THEN
            CALL OPSTXT('(a)SENDTOEUROMIL:')
        ENDIF
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        CALL SENDTOEUROMIL(BUF,MESSERIAL,ST)
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF(IGSDEBUG(IA_COMMGR)) THEN
            CALL OPSTXT('(p)SENDTOEUROMIL:')
        ENDIF
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C        IF ((ST .EQ. PAMS__NETERROR) .OR. (ST .EQ. PAMS__STUB) .OR. (ST .EQ. PAMS__TIMEOUT)) THEN
        IF ((ST .NE. PAMS__SUCCESS ) .AND. (ST .NE. PAMS__TIMEOUT)) THEN
           CALL MESSQ_EXIT(%REF(ST))
           IF (ST .EQ. PAMS__SUCCESS) THEN
              CONEURO = .FALSE.
              CALL DQUTRA(TASK,BUF)
C             GET DATE AND TIME OF DETACH
              CALL GET_EURS_DETACHDATTIM                                        !V06
              EURS_DETACHFLG = 1                                                !V06
              CALL CLR_EURS_ATTACHDATTIM                                        !CLEAR ATTACH TIME STAMP !V06
              EURS_ATTACHSTS = 0                                                !V06
C
              GOTO 543
           ENDIF
           CALL OPS('ERROR: MESSAGEQ CAN NOT BE DETACHED!!!',ST,0)
           GOTO 10
        ENDIF
        CALL DQUTRA(TASK,BUF)
C        CALL OPS('------------------- BUF: ',BUF)
C        CALL ABL(BUF,QUETAB(1,DIS),ST)
C
C CALL GET FROM EURO MILHOES FUNCTION IS USED TO GET FROM EURO MIL 
C ALL RESPONSE MESSAGES - ONLY IF STATUS IS NO MORE MESSAGES 
C
        ST = PAMS__NOMOREMSG
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF(IGSDEBUG(IA_COMMGR)) THEN
            CALL OPSTXT('(a)GETFROMEUROMIL:')
        ENDIF
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF (P(EUMILF) .NE. 0 ) CALL GETFROMEUROMIL(ST,MESSERIAL)
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF(IGSDEBUG(IA_COMMGR)) THEN
            CALL OPSTXT('(p)GETFROMEUROMIL:')
        ENDIF
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C        IF ((ST .EQ. PAMS__NETERROR) .OR. (ST .EQ. PAMS__STUB)) THEN
        IF ((ST .NE. PAMS__SUCCESS) .AND. (ST .NE. PAMS__NOMOREMSG))THEN
           CALL MESSQ_EXIT(%REF(ST))
           IF (ST .EQ. PAMS__SUCCESS) THEN
              CONEURO = .FALSE.
C             GET DATE AND TIME OF DETACH
              CALL GET_EURS_DETACHDATTIM                                        !V06
              EURS_DETACHFLG = 1                                                !V06
              CALL CLR_EURS_ATTACHDATTIM                                        !CLEAR ATTACH TIME STAMP !V06
              EURS_ATTACHSTS = 0                                                !V06
C
              GOTO 543
           ENDIF
           CALL OPS('ERROR: MESSAGEQ CAN NOT BE DETACHED!!!',ST,0)
           GOTO 10
        ENDIF
C CALL CHECK TIME OUT FUNCTION TO SEE IF THERE ARE MESSAGES 
C SENT TO EURO MIL AND DO NOT HAVE RESPONSE
C IF SO THEN DELETE BUFFER AND SEND TO ALTURA ONE ERROR MESSAGE
C
        CALL CHKTIMEOUT
        GOTO 20
        END
        
C**************************************************
C SUBROUTINE para enviar mensagens para o Euro milhoes 
C INPUT:
C        SBUF - mensagem que vai ser enviada
C        MESSERIAL - numero sequencial de mensagens de envio
C
C OUTPUT:
C        ST - Status do processo...
C        MESSERIAL - numero sequencial de mensagens de envio
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE SENDTOEUROMIL(SBUF,MESSERIAL,ST)
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
        INCLUDE 'INCLIB:EURCON.DEF'
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:IGSDEBUG.DEF'
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:EURCOM.DEF'                                             !V06
C
        INTEGER*4  MESS(EDLEN)
        INTEGER*4 I4TEMP
        INTEGER*2 I2TEMP(2)
        BYTE      I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
        INTEGER*4 SBUF,ST,IND,I,STATUS,TYP
        INTEGER*4 STYP                                                          !V06
        INTEGER*4 MESSERIAL
        INTEGER*2 MESSLEN
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        INTEGER*4 MTYPE, MSUBTYPE
        INTEGER*4 MESSAGE_TYPE, LIST_INDEX
        INTEGER*4 MESSAGE_STYPE                                                 !V06
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
C THIS COMMON AREA IS EQUAL TO THE STRUCTURE USED IN MESSAGEQ.C 
C IS USED TO PASS MESSAGE...
C
C MESS_TO_EUROMIL = MESSAGE IN BYTES
C MESS_TO_LEN = MESSAGE LENGTH 
C        
        COMMON /TO_EUROMIL/MESS_TO_EUROMIL,MESS_TO_LEN
        BYTE MESS_TO_EUROMIL(1024)
        INTEGER*4 MESS_TO_LEN
        
        MESS(1) = EUC
        ST = 0
        CALL FASTSET(0,TRABUF,TRALEN)
c        CALL LOGTRA(TRABUF,APUBUF(2,SBUF))
C
C GET FROM SECOND BYTE TYPE OF MESSAGE
C
        TYP = ISHFT(ZEXT(BPRO(BINPTAB+1,SBUF)),-4)
        STYP = MOD(ZEXT(BPRO(BINPTAB+1,SBUF)),16)                               !V06
C	CALL OPS('TYPE:',TYP,TYP)
C
C IF THIS MESSAGE IS ONE REPORT THEN GET TRABUF FROM 
C WORK TAB IN PROCOM AND PUT IT INTO APUBUF 
C
        IF (TYP .EQ. 6) THEN
           CALL LOGTRA(TRABUF,PRO(WRKTAB,SBUF))
C           CALL OPS('COMMGR TYPE: ',TRABUF(TTYP))
C           CALL OPS('COMMGR AGENT: ',TRABUF(TAGT))
           CALL TRALOG(TRABUF,APUBUF(2,SBUF))
        ENDIF   
C
C GET TRABUF FROM APUBUF
C
        CALL LOGTRA(TRABUF,APUBUF(2,SBUF))
           
C        CALL OPS('COMMGR TYPE: ',TRABUF(TTYP))
C        CALL OPS('COMMGR AGENT: ',TRABUF(TAGT))
C
C GET LENGTH OF THE MESSAGE
C       
        MESSLEN = HPRO(INPLEN,SBUF)
c        call ops('MESSLEN',MESSLEN,0)
C
C PUT IN BEGIN (1-4 BYTES) THE AGENT NUMBER 
C                 
        I4TEMP = 0
        IND = 0
        I4TEMP    = TRABUF(TAGT)
        MESS_TO_EUROMIL(IND+1) = I1TEMP(4)
        MESS_TO_EUROMIL(IND+2) = I1TEMP(3)
        MESS_TO_EUROMIL(IND+3) = I1TEMP(2)
        MESS_TO_EUROMIL(IND+4) = I1TEMP(1)
C
C PUT (5-8 BYTES) THE SEQUENCE NUMBER OF MESSAGEQ 
C                
C        IF (MESSERIAL .GT. 65000) MESSERIAL = 0
CV06        MESSERIAL = MESSERIAL + 1
        CALL GETEURXRF(MESSERIAL)
        IND = 4
        I4TEMP = 0
        I4TEMP = MESSERIAL
        MESS_TO_EUROMIL(IND+1) = I1TEMP(4)
        MESS_TO_EUROMIL(IND+2) = I1TEMP(3)
        MESS_TO_EUROMIL(IND+3) = I1TEMP(2)
        MESS_TO_EUROMIL(IND+4) = I1TEMP(1)
        IF(TYP.EQ.6 .AND. STYP.EQ.3) THEN                                       !V06
          TRABUF(TSDT3) = MESSERIAL
          CALL TRALOG(TRABUF,APUBUF(2,SBUF))
        ENDIF
C
C PUT (9-10 BYTES) THE BUFFER NUMBER OF MILLENNIUM
C        
        IND = 8
        I4TEMP = SBUF
        MESS_TO_EUROMIL(IND+1) = I1TEMP(2)
        MESS_TO_EUROMIL(IND+2) = I1TEMP(1)
C
C PUT (11-12 BYTES) CDC DATE OF MILLENNIUM
C        
        IND = 10
        I4TEMP = TRABUF(TCDC)
        MESS_TO_EUROMIL(IND+1) = I1TEMP(2)
        MESS_TO_EUROMIL(IND+2) = I1TEMP(1)
C
C PUT (13-14 BYTES) TERMINAL NUMBER
C        
        IND = 12
        I4TEMP = TRABUF(TTER)
        MESS_TO_EUROMIL(IND+1) = I1TEMP(2)
        MESS_TO_EUROMIL(IND+2) = I1TEMP(1)
C
C PUT (15-MESSLEN BYTES) THE MESSAGE FROM ALTURA
C        
        IND = 14
C       DO I=0,MESSLEN
        DO I=0,MESSLEN-1 ! V02
           MESS_TO_EUROMIL(IND+1+I) = BPRO(BINPTAB+I,SBUF)
        ENDDO
C
C MESS_TO_LEN = MESLEN + 15 (LENGTH OF MESSAGE SENT TO MESSAGEQ)
C        
        MESS_TO_LEN = MESSLEN + IND
c        call ops('MESS_TO_LEN',MESS_TO_LEN,0)
C
C CALL C FUNCTION TO PUT THE MESSAGE INTO MESSAGEQ (QUEUE = 1)
C          
CV06        IF (P(EUMILF) .NE. 0 ) THEN 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       IF CONNECTION TO EUROMILLIONS SYSTEM IS OPEN AND MESSAGE IS NOT
C       A FINANCIAL REPORT THEN SEND IT TO EUROMILLIONS SYSTEM.
C
C       IF CONNECTION TO EUROMILLIONS SYSTEM IS OPEN AND MESSAGE IS NOT
C       A FINANCIAL REPORT THEN SEND IT TO EUROMILLIONS SYSTEM (THIS
C       INCLUDES GAME RESULTS REPORT AND BILLING REPORT).
C
C       IF CONNECTION TO EUROMILLIONS SYSTEM IS OPEN AND MESSAGE IS A
C       FINANCIAL REPORT AND FINANCIAL REPORTS ARE NOT SUPRESSED THEN 
C       SEND IT TO EUROMILLIONS SYSTEM.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        IF(P(EUMILF).NE. 0                                                      !V06
     *     .AND.(    (TYP.NE.6)                                                 !V06
     *           .OR.(TYP.EQ.6.AND.STYP.NE.3)                                   !V06
     *           .OR.(TYP.EQ.6.AND.STYP.EQ.3.AND.P(EUSPFIR).EQ.0)               !V06
     *          )                                                               !V06
     *    ) THEN                                                                !V06
C          TYPE *,' PUT BEF - ',MESSERIAL,' - TIME: ',P(ACTTIM)
          CALL MESSQ_PUT(%REF(STATUS)) 
C          TYPE *,' PUT AFT- ',MESSERIAL,' - TIME: ',P(ACTTIM)
        ELSE
           DO I=1, MESS_TO_LEN
              BPRO(WRKTAB*4-3+I,SBUF) = MESS_TO_EUROMIL(I)
           ENDDO
           HPRO(OUTLEN,SBUF) = MESS_TO_LEN 
C----+------------------------------------------------------------------
C V04| Bugfix
C----+------------------------------------------------------------------
C           PRO(REMSTS,SBUF) = RMDOWN
CV06            HPRO(REMSTS,SBUF) = RMDOWN
           IF(.NOT.(TYP.EQ.6.AND.STYP.EQ.3)) THEN                               !V06
             HPRO(REMSTS,SBUF) = RMDOWN                                         !V06
           ENDIF                                                                !V06
C----+------------------------------------------------------------------
C V04| Bugfix
C----+------------------------------------------------------------------
           HPRO(TRCODE,SBUF) = TYPEUR 
           IF (TRABUF(TTYP) .NE. TSPE) THEN
             TRABUF(TERR) = SDOR
             TRABUF(TSTAT) = REJT
           ENDIF

           CALL TRALOG(TRABUF,APUBUF(2,SBUF))
           
C----+------------------------------------------------------------------
C V04| Redirecting if there is no connection to EM
C----+------------------------------------------------------------------
           MTYPE    =     ZEXT(BPRO(BINPTAB + 1, SBUF)) / 16
           MSUBTYPE = MOD(ZEXT(BPRO(BINPTAB + 1, SBUF)) , 16)
           IF(MTYPE .EQ. 6 .AND. MSUBTYPE .EQ. 3) THEN
              HPRO(OUTLEN,SBUF) = HPRO(OUTLEN,SBUF) - 14 
              IF(IGSDEBUG(IA_COMMGR)) THEN
                  CALL OPSTXT('SEND MESSAGE TO IGS (SUPR)')
              ENDIF
              HPRO(TRCODE,SBUF) = TYPIGS 
              ! Sends message to IGS
              CALL IGS_QUETRA_ST(SBUF,ST)
              IF(IGSDEBUG(IA_COMMGR)) THEN
                  CALL OPS('SEND MESSAGE TO IGS (SUPR): ST',ST,ST)
                  CALL OPS('SEND MESSAGE TO IGS (SUPR): STATUS',STATUS,STATUS)
                  CALL DUMP_BUF(SBUF,'SEND MESSAGE TO IGS: STATUS')
              ENDIF
              ST = PAMS__SUCCESS                                                !V07 AVOID CALLING MESSQ_EXIT WHEN P(EUMILF)=0
              RETURN
           ENDIF
C----+------------------------------------------------------------------
C V04| Redirecting if there is no connection to EM
C----+------------------------------------------------------------------

C
C PUT IT INTO DISPAT TO SEND IT TO OUTMGR
C           
           CALL ABL(SBUF,QUETAB(1,DIS),ST)
c           CALL OPSTXT('SEM EURO ')
CV07           ST = STATUS
           ST = PAMS__SUCCESS                                                   !V07 AVOID CALLING MESSQ_EXIT WHEN P(EUMILF)=0
           RETURN
        ENDIF
C
C IF STATUS NOT SUCCESS THEN SEND TO CONSOLE ERROR MESSAGE
C AND ALTER MESSAGE TO RMDOWN (REMOTE SYSTEM DOWN) TO 
C SEND TO ALTURA ONE ERROR MESSAGE
C        
        IF (STATUS .NE. PAMS__SUCCESS) THEN

          EURS_TOTERRPUT = EURS_TOTERRPUT + 1                                   !V06
          CALL OPS ('ERROR: WHILE PUT INTO MESSAGEQ!!',STATUS,0)
C           CALL OPS('SEND BUFF', SBUF,0)
          MESS(2) = TEEUM
          MESS(3) = 4
          MESS(4) = STATUS
          CALL QUEMES(MESS)

C----+------------------------------------------------------------------
C V05| Bugfix for wrong messages sent when end-of-day occurs in 
C    | Euromillions
C----+------------------------------------------------------------------
          DO I=1, MESS_TO_LEN
             BPRO(WRKTAB*4-3+I,SBUF) = MESS_TO_EUROMIL(I)
          ENDDO
          HPRO(OUTLEN,SBUF) = MESS_TO_LEN 
CV06          HPRO(REMSTS,SBUF) = RMDOWN
          IF(.NOT.(TYP.EQ.6.AND.STYP.EQ.3)) THEN                                !V06
            HPRO(REMSTS,SBUF) = RMDOWN                                          !V06
          ENDIF                                                                 !V06
          HPRO(TRCODE,SBUF) = TYPEUR 
          IF (TRABUF(TTYP) .NE. TSPE) THEN
              TRABUF(TERR) = SDOR
              TRABUF(TSTAT) = REJT
          ENDIF

          CALL TRALOG(TRABUF,APUBUF(2,SBUF))
C----+------------------------------------------------------------------
C V05| Bugfix for wrong messages sent when end-of-day occurs in 
C    | Euromillions
C----+------------------------------------------------------------------


C----+------------------------------------------------------------------
C V04| Redirecting if there is no connection to EM
C----+------------------------------------------------------------------
           MTYPE    =     ZEXT(BPRO(BINPTAB + 1, SBUF)) / 16
           MSUBTYPE = MOD(ZEXT(BPRO(BINPTAB + 1, SBUF)) , 16)
           IF(MTYPE .EQ. 6 .AND. MSUBTYPE .EQ. 3) THEN
              HPRO(OUTLEN,SBUF) = HPRO(OUTLEN,SBUF) - 14 
              IF(IGSDEBUG(IA_COMMGR)) THEN
                  CALL OPSTXT('SEND MESSAGE TO IGS (WHILE PUT)')
              ENDIF
C
C PUT INTO COMMGR.LOG THE STATUS AND THE MESSAGE
C           
              TYPE *,'ERROR: WHILE TRY TO PUT INTO MESSAGEQ (WP), STATUS: ',STATUS 
              TYPE *,' '
              
              DO I=1,MESS_TO_LEN
                 TYPE 9998,I, MESS_TO_EUROMIL(I) 
              ENDDO
              TYPE *,' ' 
              
              HPRO(TRCODE,SBUF) = TYPIGS 
              ! Sends message to IGS
              CALL IGS_QUETRA_ST(SBUF,ST)
              IF(IGSDEBUG(IA_COMMGR)) THEN
                  CALL OPS('SEND MESSAGE TO IGS (WHILE PUT): ST',ST,ST)
                  CALL OPS('SEND MESSAGE TO IGS (WHILE PUT): STATUS',STATUS,STATUS)
                  CALL DUMP_BUF(SBUF,'SEND MESSAGE TO IGS (WP): STATUS')
              ENDIF
CV06              IF (STATUS .NE. PAMS__TIMEOUT) CALL OPSTXT('WILL CONNECT TO FAILOVER HOST')
              IF (STATUS .NE. PAMS__TIMEOUT) THEN                               !V06
                CALL OPSTXT('WILL CONNECT TO FAILOVER HOST')                    !V06
              ELSE                                                              !V06
                EURS_TOTTMOPUT = EURS_TOTTMOPUT + 1                             !V06
              ENDIF                                                             !V06
              ST = STATUS
              RETURN
           ENDIF

C----+------------------------------------------------------------------
C V05| Bugfix for wrong messages sent when end-of-day occurs in 
C    | Euromillions
C----+------------------------------------------------------------------
CC----+------------------------------------------------------------------
CC V04| Redirecting if there is no connection to EM
CC----+------------------------------------------------------------------
C           
C           DO I=1, MESS_TO_LEN
C              BPRO(WRKTAB*4-3+I,SBUF) = MESS_TO_EUROMIL(I)
CC              BPRO(BOUTTAB+I,SBUF) = MESS_TO_EUROMIL(I)
C           ENDDO
CC           CALL OPS('VALOR DE BPRO(BOUTTAB,SBUF)',BPRO(BOUTTAB,SBUF))
C           HPRO(OUTLEN,SBUF) = MESS_TO_LEN 
CC----+------------------------------------------------------------------
CC V04| Bugfix
CC----+------------------------------------------------------------------
CC           PRO(REMSTS,SBUF) = RMDOWN
C           HPRO(REMSTS,SBUF) = RMDOWN
CC----+------------------------------------------------------------------
CC V04| Bugfix
CC----+------------------------------------------------------------------
C           HPRO(TRCODE,SBUF) = TYPEUR 
C           IF (TRABUF(TTYP) .NE. TSPE) THEN
C             TRABUF(TERR) = SDOR
C             TRABUF(TSTAT) = REJT
C           ENDIF
C
C           CALL TRALOG(TRABUF,APUBUF(2,SBUF))      
C----+------------------------------------------------------------------
C V05| Bugfix for wrong messages sent when end-of-day occurs in 
C    | Euromillions
C----+------------------------------------------------------------------

C
C PUT INTO COMMGR.LOG THE STATUS AND THE MESSAGE
C           
           TYPE *,'ERROR: WHILE TRY TO PUT INTO MESSAGEQ, STATUS: ',STATUS 
           TYPE *,' '
           
           DO I=1,MESS_TO_LEN
              TYPE 9998,I,BPRO(WRKTAB*4-3+I,SBUF) 
           ENDDO
           TYPE *,' ' 
C
C PUT IT INTO DISPAT TO SEND IT TO OUTMGR
C           
           CALL ABL(SBUF,QUETAB(1,DIS),ST)
C           CALL OPSTXT(' ******************* COMMGR - PARA O DISPAT ****************')
CV06           IF (STATUS .NE. PAMS__TIMEOUT) CALL OPSTXT('WILL CONNECT TO FAILOVER HOST')
           IF (STATUS .NE. PAMS__TIMEOUT) THEN                                  !V06
             CALL OPSTXT('WILL CONNECT TO FAILOVER HOST')                       !V06
           ELSE                                                                 !V06
             EURS_TOTTMOPUT = EURS_TOTTMOPUT + 1                                !V06
           ENDIF                                                                !V06
           ST = STATUS
           RETURN
        ENDIF
C
        EURS_TOTOKYPUT = EURS_TOTOKYPUT + 1                                     !V06
C 
C PUT SENT MESSAGE INTO WORK TAB IN PROCOM
C
        DO I=1, MESS_TO_LEN
           BPRO(WRKTAB*4-3+I,SBUF) = MESS_TO_EUROMIL(I)
        ENDDO
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF(IGSDEBUG(IA_COMMGR)) THEN
            CALL DUMP_BUF(SBUF,'SENDTOEUROMIL - MESSAGE DUMP:')
        ENDIF
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        
C        CALL OPS('VALOR DE BUFFER',SBUF,SBUF)
C
C ADD BUFFER INTO TIMER LIST 
C
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C        CALL ADDTIMER(MESSERIAL,SBUF)
        MESSAGE_TYPE = TYP
        MESSAGE_STYPE = STYP                                                    !V06
CV06        CALL GET_TIMER_LIST_INDEX(MESSAGE_TYPE, LIST_INDEX)
        CALL GET_TIMER_LIST_INDEX(MESSAGE_TYPE, MESSAGE_STYPE, LIST_INDEX)
        CALL ADDTIMER(LIST_INDEX,MESSERIAL,SBUF)
        IF(IGSDEBUG(IA_COMMGR)) THEN
           CALL OPS('543:SENDTOEUROMIL:MESSAGE_TYPE',MESSAGE_TYPE,MESSAGE_TYPE)
           CALL OPS('543:SENDTOEUROMIL:LIST_INDEX',LIST_INDEX,LIST_INDEX)
           CALL OPS('543:SENDTOEUROMIL:MESSERIAL',MESSERIAL,MESSERIAL)
           CALL OPS('543:SENDTOEUROMIL:SBUF',SBUF,SBUF)
        ENDIF
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        HPRO(OUTLEN,SBUF) = MESS_TO_LEN 
        ST = STATUS
C        DO I=1,MESS_TO_LEN
C           TYPE 9998,I,BPRO(WRKTAB*4-3+I,SBUF) 
C        ENDDO
C        TYPE *,' ' 
9998    FORMAT(' MESSWORD SENT: ',I4.3,' - ', Z3.2)
        END
C**************************************************
C SUBROUTINE para receber mensagens para o Euro milhoes 
C INPUT:
C
C OUTPUT:
C        ST - Status do processo...
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE GETFROMEUROMIL(ST,MESSERIAL)
        IMPLICIT NONE
C**************************************************
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
C        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:APUCOM.DEF'
        INCLUDE 'INCLIB:EURCOM.DEF'                                             !V06
        INCLUDE 'INCLIB:EURCON.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:IGSDEBUG.DEF'

        INTEGER*4 MSUBTYPE, SYS_AVAIL_FLAG
        INTEGER*4 OTYPE, OSUBTYPE
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        
        INTEGER*4  MESS(EDLEN)
        INTEGER*4 I4TEMP
        INTEGER*2 I2TEMP(2)
        BYTE      I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
        INTEGER*4 ST,STATUS,I,IND,JUMPINC,MTYPE
        INTEGER*4 RBUF,CHKDIG
        INTEGER*4 XRFNUM
C        INTEGER*4 MESSERIAL
C        INTEGER*2 MESSLEN
C MESS_TO_EUROMIL = MESSAGE IN BYTES
C MESS_TO_LEN = MESSAGE LENGTH 
C        
        COMMON /TO_EUROMIL/MESS_TO_EUROMIL,MESS_TO_LEN
        BYTE MESS_TO_EUROMIL(1024)
        INTEGER*4 MESS_TO_LEN
        INTEGER*4 STATUSTO,TERMINALNUM,JULIANDATE,MYCHKSUM,CHKLEN,ITO,INDTO
        BYTE OUTTABTO(500)
        INTEGER*2 DBUF(12)
        INTEGER*2 I2CCITT(2)
        EQUIVALENCE (I4CCITT,I2CCITT)
        
        COMMON /FROM_EUROMIL/MESS_FROM_EUROMIL,MESS_FROM_LEN
        BYTE MESS_FROM_EUROMIL(1024)
        INTEGER*4 MESS_FROM_LEN
        INTEGER*4 MESSERIAL
        INTEGER*4 MESGTYP, MESGIND                                              !V06
        INTEGER*4 EGNUM                                                         !V06
        LOGICAL*4 SENDICA                                                       !V06
        LOGICAL*4 EGNUMOK                                                       !V06 INVALID GAME NUMBER
        LOGICAL*4 EGTYIOK                                                       !V06 INVALID GAME TYPE AND/OR GAME INDEX

C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        INTEGER*4 BUF_MES_LEN
        INTEGER*4 MESSAGE_TYPE, LIST_INDEX
        INTEGER*4 MESSAGE_STYPE                                                 !V06
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        
        MESS(1) = EUC
        ST = 0

20      CONTINUE
        CALL MESSQ_GET(%REF(STATUS))
        
        IF (STATUS .EQ. PAMS__SUCCESS) THEN
C
          EURS_TOTOKYGET = EURS_TOTOKYGET + 1                                   !V06
C          CALL OPSTXT('EXISTE MSG')

C          DO I=0,MESS_FROM_LEN
C            TYPE 9999,I,MESS_FROM_EUROMIL(I) 
C          ENDDO

          I1TEMP(1) = ZEXT (MESS_FROM_EUROMIL(8))
          I1TEMP(2) = ZEXT (MESS_FROM_EUROMIL(7))
          I1TEMP(3) = ZEXT (MESS_FROM_EUROMIL(6))
          I1TEMP(4) = ZEXT (MESS_FROM_EUROMIL(5))
          XRFNUM = I4TEMP
C
C CLEAR VAR MEMORY
C          
          I4TEMP = 0
          
          I1TEMP(1) = ZEXT(MESS_FROM_EUROMIL(10))
          I1TEMP(2) = ZEXT(MESS_FROM_EUROMIL(9))
          I1TEMP(3) = 0
          I1TEMP(4) = 0
          
          RBUF = I4TEMP
          
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
          BUF_MES_LEN = HPRO(INPLEN,RBUF) 
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
          IF(IGSDEBUG(IA_COMMGR)) THEN
              CALL DUMP_BUF(RBUF,'INSIDE GETFROMEUROMIL - LINE 595')
          ENDIF
          
          IF (XRFNUM .GT. MESSERIAL) THEN
             MESSERIAL = XRFNUM
          ELSE
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C            CALL REMTIMER(XRFNUM,RBUF,ST)
             MESSAGE_TYPE = ZEXT(BPRO(BINPTAB + 1, RBUF))
             MESSAGE_TYPE = ISHFT(MESSAGE_TYPE,-4)
             MESSAGE_STYPE = MOD(ZEXT(BPRO(BINPTAB + 1,RBUF)),16)               !V06
CV06             CALL GET_TIMER_LIST_INDEX(MESSAGE_TYPE, LIST_INDEX)
             CALL GET_TIMER_LIST_INDEX(MESSAGE_TYPE, MESSAGE_STYPE, LIST_INDEX)
             CALL REMTIMER(LIST_INDEX,XRFNUM,RBUF,ST)
             IF(IGSDEBUG(IA_COMMGR)) THEN
                CALL OPS('683:GETFROMEUROMIL:MESSAGE_TYPE',MESSAGE_TYPE,MESSAGE_TYPE)
                CALL OPS('683:GETFROMEUROMIL:MESSAGE_STYPE',MESSAGE_STYPE,MESSAGE_STYPE)
                CALL OPS('683:GETFROMEUROMIL:LIST_INDEX',LIST_INDEX,LIST_INDEX)
                CALL OPS('683:GETFROMEUROMIL:HPRO(INPLEN,RBUF)',ZEXT(HPRO(INPLEN,RBUF)),ZEXT(HPRO(INPLEN,RBUF)))
                CALL OPS('683:GETFROMEUROMIL:ST',ST,ST)
             ENDIF
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------

             IF (ST.LT.0) THEN
                IF(MESSAGE_TYPE .EQ. 6 .AND. MESSAGE_STYPE .EQ. 3) THEN         !FINANCIAL REPORTS
                  CALL OPSTXT('FIN MESSAGE ALREADY TIME OUT')
                ELSE
                  CALL OPSTXT('MESSAGE ALREADY TIME OUT')
                ENDIF
C                CALL OPS('RECV ',RBUF,0)
C----+------------------------------------------------------------------
C V04| Bugfix
C----+------------------------------------------------------------------
C                PRO(REMSTS,RBUF) = RMLATE
C                HPRO(REMSTS,RBUF) = RMLATE
C----+------------------------------------------------------------------
C V04| Bugfix
C----+------------------------------------------------------------------
C                HPRO(TRCODE,RBUF) = TYPEUR 
                 TYPE *,' ' 
                 TYPE *,'ERROR: MESSAGE ALREADY TIMEOUT '
                 
                 I1TEMP(4) = ZEXT (MESS_FROM_EUROMIL(1))
                 I1TEMP(3) = ZEXT (MESS_FROM_EUROMIL(2))
                 I1TEMP(2) = ZEXT (MESS_FROM_EUROMIL(3))
                 I1TEMP(1) = ZEXT (MESS_FROM_EUROMIL(4))
                 TYPE *,'AGENT NUMBER: ',I4TEMP
C                 CALL OPS('AGENT NUMBER: ',I4TEMP,0)
 
                 I1TEMP(4) = 0
                 I1TEMP(3) = 0
                 I1TEMP(2) = ZEXT (MESS_FROM_EUROMIL(13))
                 I1TEMP(1) = ZEXT (MESS_FROM_EUROMIL(14)) 
                 TERMINALNUM = I4TEMP
                 TYPE *,'TERMINAL NUMBER: ',I4TEMP
C                 CALL OPS('TERMINAL NUMBER: ',I4TEMP,0)
                 
                 TYPE *,'CDC DATE: ',DAYCDC
          
                 MTYPE = ZEXT(MESS_FROM_EUROMIL(16))
                 MTYPE = ISHFT(MTYPE,-4)
C
                 MSUBTYPE = MOD(MESS_FROM_EUROMIL(16),16)                       !V06
                 CALL UPD_EURS_TOTATO(MTYPE, MSUBTYPE)                          !V06
C
                 IF (MTYPE .EQ. 0) JUMPINC = 0
                 IF (MTYPE .EQ. 1) JUMPINC = 1
                 IF (MTYPE .EQ. 2) JUMPINC = 1
                 IF ((MTYPE .EQ. 0) .OR. (MTYPE .EQ. 1) .OR. (MTYPE .EQ. 2)) THEN 
                    I1TEMP(4) = 0
                    I1TEMP(3) = 0
                    I1TEMP(2) = 0
                    I1TEMP(1) = ZEXT (MESS_FROM_EUROMIL(24+JUMPINC))
                    CHKDIG = I4TEMP
                    
                    I1TEMP(4) = 0
                    I1TEMP(3) = ZEXT (MESS_FROM_EUROMIL(21+JUMPINC))
                    I1TEMP(2) = ZEXT (MESS_FROM_EUROMIL(22+JUMPINC))
                    I1TEMP(1) = ZEXT (MESS_FROM_EUROMIL(23+JUMPINC))

                    TYPE 9990,I4TEMP,CHKDIG
C                    CALL OPS('EXTERNAL SERIAL: ',I4TEMP,0)
C                    CALL OPS('EXTERNAL CHECK DIGITIS: ',CHKDIG,0)
                    
                    IF (MTYPE .EQ. 0) THEN
C----+---+-------------+------------------------------------------------
C V06|BEG| M16 PROJECT | CHECK GLOBAL PARAMETERS REGARDING INTERNAL CAN
C----+---+-------------+------------------------------------------------
                       MESGTYP = ZEXT(MESS_FROM_EUROMIL(19))                    !GAME TYPE
                       MESGIND = ZEXT(MESS_FROM_EUROMIL(20))                    !GAME INDEX
                       EGTYIOK = .FALSE.
                       EGNUMOK = .FALSE.
                       SENDICA = .FALSE.
                       IF(     MESGTYP.GT.0.AND.MESGTYP.LE.MAXTYP
     *                   .AND. MESGIND.GT.0.AND.MESGIND.LE.MAXIND) THEN
                         EGTYIOK = .TRUE.
                         EGNUM = EGTNTAB(MESGTYP,MESGIND)
                         IF(EGNUM.GT.0.AND.EGNUM.LE.EMAXGAM) THEN
                           EGNUMOK = .TRUE.
                           IF(     P(EUSPICA).EQ. 0                             !INTERNAL CANCELLATIONS NOT SUPRESSED
     *                       .AND. .NOT.TSBIT(P(EUSPGICA),EGNUM-1)) THEN        !GAME INTERNAL CANCELLATIONS NOT SUPRESSED (SUPRESSION BIT # STARTS AT ZERO (GAME NUMBER = 1 -> BIT # = 0))
                             SENDICA = .TRUE.                                   !SEND INTERNAL CANCELLATION MESSAGE TO EUROMILLIONS SYSTEM
                           ENDIF
                         ENDIF
                       ENDIF
C
                       IF(SENDICA) THEN
C----+---+-------------+------------------------------------------------
C V06|END| M16 PROJECT | CHECK GLOBAL PARAMETERS REGARDING INTERNAL CAN
C----+---+-------------+------------------------------------------------
C
C PUT IN BEGIN (1-4 BYTES) THE AGENT NUMBER 
C                 
                         INDTO = 0
                         MESS_TO_EUROMIL(INDTO+1) = ZEXT (MESS_FROM_EUROMIL(1))
                         MESS_TO_EUROMIL(INDTO+2) = ZEXT (MESS_FROM_EUROMIL(2))
                         MESS_TO_EUROMIL(INDTO+3) = ZEXT (MESS_FROM_EUROMIL(3))
                         MESS_TO_EUROMIL(INDTO+4) = ZEXT (MESS_FROM_EUROMIL(4))
C
C PUT (5-8 BYTES) THE SEQUENCE NUMBER OF MESSAGEQ 
C                
CV06                         MESSERIAL = MESSERIAL + 1
                         CALL GETEURXRF(MESSERIAL)
                         INDTO = 4
                         I4TEMP = 0
                         I4TEMP = MESSERIAL
                         MESS_TO_EUROMIL(INDTO+1) = I1TEMP(4)
                         MESS_TO_EUROMIL(INDTO+2) = I1TEMP(3)
                         MESS_TO_EUROMIL(INDTO+3) = I1TEMP(2)
                         MESS_TO_EUROMIL(INDTO+4) = I1TEMP(1)
C
C PUT (9-10 BYTES) THE BUFFER NUMBER OF MILLENNIUM
C        
                         INDTO = 8
                         MESS_TO_EUROMIL(INDTO+1) = 'FF'X
                         MESS_TO_EUROMIL(INDTO+2) = 'FF'X
C
C PUT (11-12 BYTES) CDC DATE OF MILLENNIUM
C        
                         INDTO = 10
                         I4TEMP = DAYCDC
                         MESS_TO_EUROMIL(INDTO+1) = I1TEMP(2)
                         MESS_TO_EUROMIL(INDTO+2) = I1TEMP(1)
C
C PUT (13-14 BYTES) TERMINAL NUMBER
C        
                         INDTO = 12
                         MESS_TO_EUROMIL(INDTO+1) = ZEXT (MESS_FROM_EUROMIL(13))
                         MESS_TO_EUROMIL(INDTO+2) = ZEXT (MESS_FROM_EUROMIL(14))
C
C CREATE CANCEL MESSAGE TO SEND TO EURO MILHOES
C
                         OUTTABTO(1) = ZEXT (MESS_FROM_EUROMIL(15)) !CONTROL/SEQ
                         OUTTABTO(2) = '20'X 			  !TYPE/SUBTYPE
                         ! SEED OF CHECKSUM
                         I4TEMP = DAYCDC + TERMINALNUM   
                       
                         OUTTABTO(3) = I1TEMP(2)
                         OUTTABTO(4) = I1TEMP(1)
                         ! STATISTICS
                         OUTTABTO(5) = '00'X
                         
                         ! CALCULATE JULIAN DATE 
                         DBUF(VCDC)=DAYCDC
                         CALL CDATE(DBUF)
                         JULIANDATE=DBUF(VJUL) + 500
                         
                         I4TEMP = JULIANDATE
                         OUTTABTO(6) = I1TEMP(2)
                         OUTTABTO(7) = I1TEMP(1)
                         ! EXTERNAL SERIAL
                         OUTTABTO(8)  = ZEXT (MESS_FROM_EUROMIL(21+JUMPINC))
                         OUTTABTO(9)  = ZEXT (MESS_FROM_EUROMIL(22+JUMPINC))
                         OUTTABTO(10) = ZEXT (MESS_FROM_EUROMIL(23+JUMPINC))
                         ! CHECK DIGITS
                         OUTTABTO(11) = ZEXT (MESS_FROM_EUROMIL(24+JUMPINC))
C
C CALCULATE CHECKSUM
C
                         I4CCITT = DAYCDC + TERMINALNUM
                         OUTTABTO(3) = I1CCITT(2)
                         OUTTABTO(4) = I1CCITT(1)
                         CHKLEN=11-1
                         CALL GETCCITT(OUTTABTO,1,CHKLEN,MYCHKSUM)
                         I4CCITT = MYCHKSUM
                         OUTTABTO(3) = I1CCITT(2)
                         OUTTABTO(4) = I1CCITT(1)   
C
C PUT (15-MESSLEN BYTES) THE MESSAGE FROM ALTURA
C        
                         INDTO = 14
C                        DO ITO=0,11
                         DO ITO=0,10 ! V02
                            MESS_TO_EUROMIL(INDTO+1+ITO) = OUTTABTO(ITO+1)
                         ENDDO
C
C MESS_TO_LEN = MESLEN + 15 (LENGTH OF MESSAGE SENT TO MESSAGEQ)
C        
                         MESS_TO_LEN = 11 + INDTO                         
                         
                         TYPE *,' '
C                         DO ITO = 0,MESS_TO_LEN
                         DO ITO = 1,MESS_TO_LEN ! V02 FIRST INDEX IN A FORTRAN BYTE ARRAY IS 1 (CHANGED IN 2011-01-26)
                            TYPE 9993,ITO,MESS_TO_EUROMIL(ITO)
                         ENDDO
                                               
C                         CALL OPS('VALOR DO TIME ANTES DO PUT',P(ACTTIM),0)
                         CALL MESSQ_PUT(%REF(STATUSTO)) 
C                         CALL OPS('VALOR DO TIME DEPOIS DO PUT',P(ACTTIM),0)
                         IF (STATUSTO .EQ. PAMS__SUCCESS) THEN
C                            CALL OPSTXT('INTERNAL CANCEL OK')
                            EURS_TOTICANOK = EURS_TOTICANOK + 1                 !TOTAL OF INTERNAL CANCEL MESSAGES SENT SUCCESSFULLY TO EUROMILLIONS SYSTEM
                            TYPE *,' '
                            TYPE *,'INTERNAL CANCEL OK'
                         ELSE
C                          CALL OPSTXT('INTERNAL CANCEL FAIL')
                           EURS_TOTICANER = EURS_TOTICANER + 1                  !TOTAL OF INTERNAL CANCEL MESSAGES FAILED TO SEND TO EUROMILLIONS SYSTEM
                           TYPE *,' '
                           TYPE *,'INTERNAL CANCEL FAIL'
                         ENDIF
C----+---+-------------+------------------------------------------------
C V06|BEG| M16 PROJECT | CHECK GLOBAL PARAMETERS REGARDING INTERNAL CAN
C----+---+-------------+------------------------------------------------
                       ELSE                                                     !SENDICA = .FALSE.
                         EURS_TOTICANNS = EURS_TOTICANNS + 1                    !TOTAL OF INTERNAL CANCEL MESSAGES NOT SENT TO EUROMILLIONS SYSTEM
                         TYPE*, ' '
                         IF(.NOT.EGTYIOK) THEN
                           TYPE*, 'INTERNAL CANCEL NOT SENT: INVALID '//
     *                             'GTYP=',MESGTYP,' GIND=',MESGIND
                         ELSEIF(.NOT.EGNUMOK) THEN
                           TYPE*, 'INTERNAL CANCEL NOT SENT: INVALID '//
     *                             'GNUM=',EGNUM, 
     *                             ' (GTYP=',MESGTYP,' GIND=',MESGIND,')'
                         ELSE
C                          CALL OPSTXT('INTERNAL CANCEL NOT SENT: FUNCTION SUPRESSED')
                           TYPE *,'INTERNAL CANCEL NOT SENT: '//
     *                            'FUNCTION SUPRESSED'
                         ENDIF
                       ENDIF
C----+---+-------------+------------------------------------------------
C V06|END| M16 PROJECT | CHECK GLOBAL PARAMETERS REGARDING INTERNAL CAN
C----+---+-------------+------------------------------------------------
                    ELSE IF (MTYPE .EQ. 1) THEN
                       TYPE *,'VALIDATION: NO INTERNAL CANCEL'
                    ELSE IF (MTYPE .EQ. 2) THEN
                       TYPE *,'CANCEL: NO INTERNAL CANCEL'
                    ENDIF
                 ELSE 
                   TYPE *,'MESSAGE NOT WAGER, VALIDATION OR CANCEL'
                 ENDIF
                 
                 TYPE *,' ' 
C                 DO I=0,MESS_FROM_LEN
                 DO I=1,MESS_FROM_LEN ! V02 FIRST INDEX IN A FORTRAN BYTE ARRAY IS ONE, NOT ZERO (CHANGED IN 2011-01-26)
                    TYPE 9995,I,MESS_FROM_EUROMIL(I) 
                 ENDDO
                 TYPE *,' '          
                 TYPE *,' ERROR TIME OUT END'
                GOTO 20
             ENDIF
          ENDIF

!! COMMENTED IN 2011-01-26
!!          IND = 0
!!          DO I=0, MESS_FROM_LEN
!!             BPRO(WRKTAB*4-3+I,RBUF) = MESS_FROM_EUROMIL(I)
!!C             BPRO(BOUTTAB+I,RBUF) = MESS_FROM_EUROMIL(I)
!!             IND = IND + 1
!!          ENDDO
!!C          CALL OPS('tamanho da mensagem',MESS_FROM_LEN,IND)
!!          HPRO(OUTLEN,RBUF) = IND - 1
!!          HPRO(TRCODE,RBUF) = TYPEUR
C----+------------------------------------------------------------------
C V04| Bugfix
C----+------------------------------------------------------------------
!!C          PRO(REMSTS,RBUF) = RMGOOD
!!          HPRO(REMSTS,RBUF) = RMGOOD
C----+------------------------------------------------------------------
C V04| Bugfix
C----+------------------------------------------------------------------
          DO I=1, MESS_FROM_LEN ! V02 FIRST INDEX IN A FORTRAN BYTE ARRAY IS ONE (CHANGED IN 2011-01-26)   
             BPRO(WRKTAB*4-3+I,RBUF) = MESS_FROM_EUROMIL(I)
!             TYPE 9991, I, MESS_FROM_EUROMIL(I), MESS_FROM_LEN ! DEBUG
          ENDDO
          HPRO(OUTLEN,RBUF) = MESS_FROM_LEN
          HPRO(TRCODE,RBUF) = TYPEUR
C----+------------------------------------------------------------------
C V04| Bugfix
C----+------------------------------------------------------------------
C          PRO(REMSTS,RBUF) = RMGOOD
          HPRO(REMSTS,RBUF) = RMGOOD
C----+------------------------------------------------------------------
C V04| Bugfix
C----+------------------------------------------------------------------
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
          ! Clearing first bit of system availability flags
CV06          IF(IGSDEBUG(IA_COMMGR)) THEN
CV06              CALL OPS('BEF:SETTING EUROMIL SYSTEM AVAILABITY FLAG TO 0',
CV06     *          ZEXT(BPRO(WRKTAB*4-3+8,RBUF)),ZEXT(BPRO(WRKTAB*4-3+8,RBUF)))
CV06          ENDIF
CV06          BPRO(WRKTAB*4-3+8,RBUF) = IAND(BPRO(WRKTAB*4-3+ 8,RBUF),'EF'X)
CV06          IF(IGSDEBUG(IA_COMMGR)) THEN
CV06              CALL OPS('AFT:SETTING EUROMIL SYSTEM AVAILABITY FLAG TO 0',
CV06     *          ZEXT(BPRO(WRKTAB*4-3+8,RBUF)),ZEXT(BPRO(WRKTAB*4-3+8,RBUF)))
CV06          ENDIF
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
          
123       CONTINUE
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
          OTYPE    =     ZEXT (MESS_FROM_EUROMIL(16))/ 16
          OSUBTYPE = MOD(ZEXT (MESS_FROM_EUROMIL(16)), 16)
          MTYPE    =     ZEXT (BPRO(BINPTAB + 1,RBUF))/ 16
          MSUBTYPE = MOD(ZEXT (BPRO(BINPTAB + 1,RBUF)), 16)
          SYS_AVAIL_FLAG = IAND(ZEXT(MESS_FROM_EUROMIL(22)),'20'X)
          IF(IGSDEBUG(IA_COMMGR)) THEN
              CALL OPS('MTYPE',MTYPE,MTYPE)
              CALL OPS('MSUBTYPE',MSUBTYPE,MSUBTYPE)
              CALL OPS('OTYPE',OTYPE,OTYPE)
              CALL OPS('OSUBTYPE',OSUBTYPE,OSUBTYPE)
              CALL OPS('SYS_AVAIL_FLAG',SYS_AVAIL_FLAG,SYS_AVAIL_FLAG)
              CALL OPS('RBUF',RBUF,RBUF)
              CALL OPS('MESS_FROM_LEN',MESS_FROM_LEN,MESS_FROM_LEN)
              CALL DUMP_BUF(RBUF,'INSIDE GETFROMEUROMIL - LINE 841')
          ENDIF
          IF(MTYPE .EQ. 6 .AND. MSUBTYPE .EQ. 3) THEN
              IF(OTYPE .EQ. 6 .AND. OSUBTYPE .EQ. 3 .AND. SYS_AVAIL_FLAG .NE. 0) THEN
                  DO I=1, MESS_FROM_LEN - 14
                     BPRO(BINPTAB + I - 1,RBUF) = BPRO(WRKTAB*4-3+I+14,RBUF)
                  ENDDO
                  HPRO(INPLEN,RBUF) = MESS_FROM_LEN - 14
                  IF(IGSDEBUG(IA_COMMGR)) THEN
                      CALL OPSTXT('SEND MESSAGE TO IGS : COPY FROM WRKTAB')
                      CALL OPS('SEND MESSAGE TO IGS: COPY FROM WRKTAB : HPRO(INPLEN,RBUF)'
     *                        ,ZEXT(HPRO(INPLEN,RBUF)),ZEXT(HPRO(INPLEN,RBUF)))
                  ENDIF
              ELSE
                  HPRO(INPLEN,RBUF) = BUF_MES_LEN - 14
                  IF(IGSDEBUG(IA_COMMGR)) THEN
                      CALL OPS('SEND MESSAGE TO IGS: HPRO(INPLEN,RBUF)',ZEXT(HPRO(INPLEN,RBUF)),ZEXT(HPRO(INPLEN,RBUF)))
                      CALL OPSTXT('SEND MESSAGE TO IGS')
                  ENDIF
              ENDIF
              ! Sends message to IGS
              CALL IGS_QUETRA_ST(RBUF,ST)
              IF(IGSDEBUG(IA_COMMGR)) THEN
                  CALL OPS('SEND MESSAGE TO IGS: STATUS',ST,ST)
                  CALL DUMP_BUF(RBUF,'SEND MESSAGE TO IGS: STATUS')
              ENDIF
          ELSE
              IF(IGSDEBUG(IA_COMMGR)) THEN
                  CALL OPSTXT('SEND MESSAGE TO DISP')
              ENDIF
              ! Sends message to DISPatcher
              CALL ABL(RBUF,QUETAB(1,DIS),ST)
              IF(IGSDEBUG(IA_COMMGR)) THEN
                  CALL OPS('SEND MESSAGE TO DISP: STATUS',ST,ST)
              ENDIF
          ENDIF
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C Ricardo
c          CALL OPSTXT(' ******************* COMMGR - PARA O DISPAT ****************')
c          DO I=0,MESS_FROM_LEN
c            TYPE 9999,I,MESS_FROM_EUROMIL(I) 
c          ENDDO
c          TYPE *,' '
c          TYPE *,'NEXT MESSAGE ' 
c          TYPE *,' '
C Ricar
          GOTO 20
        ENDIF 
        IF (STATUS .EQ. PAMS__NOMOREMSG) THEN
C          CALL OPS('NAO EXISTEM MSG NA QUEUE',STATUS)
          ST = STATUS
          RETURN
        ENDIF 
        IF ((STATUS .NE. PAMS__NOMOREMSG) .OR. (STATUS .NE. PAMS__SUCCESS)) THEN
C
          EURS_TOTERRGET = EURS_TOTERRGET + 1                                  !V06
C          IF ((STATUS .NE. PAMS__NETERROR) .OR. (STATUS .NE. PAMS__STUB)) CALL OPS('ERRO NO GET!!!!!!',STATUS,0)
C----+------------------------------------------------------------------
C V05| Bugfix for wrong messages sent when end-of-day occurs in 
C    | Euromillions
C----+------------------------------------------------------------------
          ST = STATUS
C----+------------------------------------------------------------------
C V05| Bugfix for wrong messages sent when end-of-day occurs in 
C    | Euromillions
C----+------------------------------------------------------------------
          CALL OPS('ERROR: BAD STATUS WHILE GET FROM MESSAGEQ!!',ST,0)
          CALL OPSTXT('WILL CONNECT TO FAILOVER HOST')
C----+------------------------------------------------------------------
C V05| Bugfix for wrong messages sent when end-of-day occurs in 
C    | Euromillions
C----+------------------------------------------------------------------
C         ST = STATUS
C----+------------------------------------------------------------------
C V05| Bugfix for wrong messages sent when end-of-day occurs in 
C    | Euromillions
C----+------------------------------------------------------------------
          RETURN
        ENDIF

C        CALL ABL(BUF,QUETAB(1,DIS),ST)
C        CALL DQUTRA(TASK,BUF)
9990    FORMAT(' EXTERNAL SERIAL: ',I8.8,'-',I3.3)
9991    FORMAT(' DEBUG: ', I8.8, '-', I3.3, I)     
9995    FORMAT(' ERROR TIME OUT MESSWORD RECB: ',I2.1,' - ', Z3.2)
9993    FORMAT(' CANCEL MESSAGE: ',I2.1,' - ', Z3.2)
9999    FORMAT(' MESSWORD RECB: ',I2.1,' - ', Z3.2)
        END
CC**************************************************
CC SUBROUTINE: SEE IF EXIST EXPIRED TRANSACTIONS 
CC INPUT:
CC
CC OUTPUT:
CC
CC=======OPTIONS /CHECK=NOOVERFLOW
C        SUBROUTINE CHKTIMEOUT
C        IMPLICIT NONE
CC**************************************************
C        INCLUDE 'INCLIB:SYSPARAM.DEF'
C        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C        INCLUDE 'INCLIB:GLOBAL.DEF'
C        INCLUDE 'INCLIB:CONCOM.DEF'
C        INCLUDE 'INCLIB:PROCOM.DEF'
C        INCLUDE 'INCLIB:TASKID.DEF'
C        INCLUDE 'INCLIB:QUECOM.DEF'
C        INCLUDE 'INCLIB:EURCON.DEF'
C        INCLUDE 'INCLIB:DESTRA.DEF'
C        INCLUDE 'INCLIB:APUCOM.DEF'        
C
C        INTEGER*4   XRFNUM          !Cross reference number
C        INTEGER*4   PBUF            !Procom buffer number
C        INTEGER*4   ST              !Status
Cc        INTEGER*4   MES_LEN
CC
C        INTEGER*4  MESS(EDLEN)
C        INTEGER*4   I4TEMP
C        INTEGER*2   I2TEMP(2)
C        BYTE        I1TEMP(4)
C        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
CC
C        LOGICAL*1   TIMEOUT         !Timeout Flag
CC
C        TIMEOUT=.FALSE.
C        MESS(1) = EUC
CC
CC
CC CHECK FOR ANY TIMED OUT TRANSACTIONS.
CC
C1000    CONTINUE
C        CALL CHKTIMER(XRFNUM,PBUF)
C        IF(PBUF.EQ.0) GOTO 9000
CC
CC A TIMED OUT BUFFER HAS BEEN ENCOUNTERED.
CC SET STATUS TO TIMED OUT, AND SET THE TRCODE ALLOWING
CC DISPAT TO QUEUE TO THE APPROPRIATE PROCESSING TASK.
CC
C	TIMEOUT=.TRUE.
CC
C	I4TEMP=0
C	I1TEMP(1)=BPRO(WRKTAB*4-3+5,PBUF)
C	I1TEMP(2)=BPRO(WRKTAB*4-3+6,PBUF)
C	I1TEMP(3)=BPRO(WRKTAB*4-3+7,PBUF)
C	I1TEMP(4)=BPRO(WRKTAB*4-3+8,PBUF)
C	XRFNUM=I4TEMP			!MESSAGE SENT #
CC        
C        HPRO(TRCODE,PBUF)=TYPEUR
C        CALL OPS('BUFFER EXPIRADO:',PBUF,PBUF)
CC----+------------------------------------------------------------------
CC V04| Bugfix
CC----+------------------------------------------------------------------
CC        PRO(REMSTS,PBUF) = RMTMOT  
C        HPRO(REMSTS,PBUF) = RMTMOT  
CC----+------------------------------------------------------------------
CC V04| Bugfix
CC----+------------------------------------------------------------------
CC
CC GET TRABUF FROM APUBUF
CC
C        CALL LOGTRA(TRABUF,APUBUF(2,PBUF))               
C        IF (TRABUF(TTYP) .NE. TSPE) THEN
C           TRABUF(TERR) = BCRS
C           TRABUF(TSTAT) = REJT
C        ENDIF
C
C        CALL TRALOG(TRABUF,APUBUF(2,PBUF))
C               
C        CALL ABL(PBUF,QUETAB(1,DIS),ST)
C        GOTO 1000
CC
CC ALL BUFFERS HAVE BEEN CHECKED. REMOVE ANY TIMED OUT BUFFERS
CC FROM SNDQUE
CC
C9000    CONTINUE
C        RETURN
C        END

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
        INCLUDE 'INCLIB:EURCON.DEF'
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
C
        INTEGER*4  MTYP, MSUBTYP                                                !V06
C

        MESS(1) = EUC

C----+------------------------------------------------------------------
C    | Check for any timed-out transactions
C----+------------------------------------------------------------------
        NO_TIMEOUTS = .FALSE.
        DO WHILE ( NO_TIMEOUTS .EQ. .FALSE. )
            TIMEOUT_CNT = 0
            DO I = 1, EUR_MAX_TIMER_LISTS
                CALL CHKTIMER(I,XRFNUM,PBUF)
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
        
                   HPRO(TRCODE,PBUF) = TYPEUR
                   IF(I .EQ. EUR_TL_MAIN) THEN
                       CALL OPS('BUFFER EXPIRADO:',PBUF,PBUF)
                   ELSE 
                       CALL OPS('FIN BUFFER EXPIRADO:',PBUF,PBUF)
                   ENDIF
CV06                   HPRO(REMSTS,PBUF) = RMTMOT  
C
                   MTYP = ISHFT(ZEXT(BPRO(BINPTAB+1,PBUF)),-4)                  !V06
                   MSUBTYP = MOD(ZEXT(BPRO(BINPTAB+1,PBUF)),16)                 !V06
                   CALL UPD_EURS_TOTTMO(MTYP, MSUBTYP)                          !V06
                   IF(.NOT.(MTYP.EQ.6.AND.MSUBTYP.EQ.3)) THEN                   !V06
                     HPRO(REMSTS,PBUF) = RMTMOT                                 !V06
                   ENDIF                                                        !V06
C----+------------------------------------------------------------------
C    | Get TRABUF from APUBUF
C----+------------------------------------------------------------------
                   CALL LOGTRA(TRABUF,APUBUF(2,PBUF))               
                   IF (TRABUF(TTYP) .NE. TSPE) THEN
                      TRABUF(TERR) = BCRS
                      TRABUF(TSTAT) = REJT
                   ENDIF
         
                   CALL TRALOG(TRABUF,APUBUF(2,PBUF))
                   
                   IF(TRABUF(TTYP) .EQ. TSPE .AND. TRABUF(TSFUN) .EQ. TSREP) THEN
                       HPRO(TRCODE,PBUF) = TYPIGS
                       CALL IGS_QUETRA_ST(PBUF,ST)
                   ELSE
                       CALL ABL(PBUF,QUETAB(1,DIS),ST)
                   ENDIF
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



C**************************************************
C SUBROUTINE para verificar o estado dos processos COMMGR e OUTMGR
C INPUT:
C
C OUTPUT:
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CHECKPROCESS()
        IMPLICIT NONE
C**************************************************
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INTEGER*4   TSKSTS,STATUS
C
C IF COMMGR DO NOT RUN THEN RESTART COMMGR
C       
        STATUS = 0 
        CALL STTSK(8HOUTMGR  ,TSKSTS,STATUS) !VERIFY IF OUTMGR IS OK
        IF (STATUS .EQ. 4) THEN
           CALL OPSTXT('ERROR!!!!! OUTMGR IS NOT RUNNING')
           CALL OPSTXT('STARTING OUTMGR AND TRY TO PROCESS ALL TRANSACTIONS ')
           CALL START(TSKNAM(31)) ! OUTMGR
        ENDIF
C
C IF INMGR DO NOT RUN THEN RESTART INMGR
C       
        STATUS = 0 
        CALL STTSK(8HINMGR   ,TSKSTS,STATUS) ! VERIFY IF INMGR IS OK
        IF (STATUS .EQ. 4) THEN
           CALL OPSTXT('ERROR!!!!! INMGR IS NOT RUNNING')
           CALL OPSTXT('STARTING INMGR AND TRY TO PROCESS ALL TRANSACTIONS ')
           CALL START(TSKNAM(30)) !INMGR
        ENDIF
        END


C----+-----------------------------------------------------------------
C    | SUBROUTINE GET_TIMER_LIST_INDEX
C    |    This subroutine returns the timer list index to which a 
C    |    buffer must be associated, given its type
C    +-----------------------------------------------------------------
C    | INPUT PARAMETERS:
C    |    MESSAGE_TYPE  Message type
C    |    MESSAGE_STYPE Message subtype                                         !V06
C    +-----------------------------------------------------------------
C    | OUTPUT PARAMETERS:
C    |    LIST_INDEX    Timer list index
C----+-----------------------------------------------------------------
CV06        SUBROUTINE GET_TIMER_LIST_INDEX(MESSAGE_TYPE, LIST_INDEX)
        SUBROUTINE GET_TIMER_LIST_INDEX(MESSAGE_TYPE, MESSAGE_STYPE, 
     *                                  LIST_INDEX)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:EURCON.DEF'

        INTEGER*4 MESSAGE_TYPE
        INTEGER*4 MESSAGE_STYPE                                                 !V06
        INTEGER*4 LIST_INDEX
        
C----+------------------------------------------------------------------
C    | By default, assign buffer to main timer list
C----+------------------------------------------------------------------
        LIST_INDEX = EUR_TL_MAIN
        
C----+------------------------------------------------------------------
C    | If message type is 6 (reports) then assign buffer to cross timer
C    | list
C----+------------------------------------------------------------------
CV06        IF ( MESSAGE_TYPE .EQ. 6 ) THEN
        IF ( MESSAGE_TYPE .EQ. 6 .AND. MESSAGE_STYPE .EQ. 3 ) THEN              !V06 - FIX (ONLY FINANCIAL REPORTS)
            LIST_INDEX = EUR_TL_CRS
        ENDIF
        RETURN
        END


        SUBROUTINE DUMP_BUF(BUF,COMMENT)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'

        INTEGER*4 BUF
        CHARACTER*255 LINE
        CHARACTER*(*) COMMENT
1001    FORMAT('(',I5,'):',A,' # ')
1002    FORMAT('(',I5,'):',A,I10,' ',Z8)
        WRITE(LINE, 1001) BUF, '--------------------------------------------------'
        CALL OPSTXT(TRIM(LINE))
        WRITE(LINE, 1001) BUF, TRIM(COMMENT)
        CALL OPSTXT(TRIM(LINE))
        WRITE(LINE, 1001) BUF, '..................................................'
        CALL OPSTXT(TRIM(LINE))
        WRITE(LINE, 1002) BUF, 'DUMP_BUF:',BUF, BUF
        CALL OPSTXT(TRIM(LINE))
        WRITE(LINE, 1002) BUF, ' HPRO (REMSTS,BUF):',ZEXT(HPRO(REMSTS,BUF)), ZEXT(HPRO(REMSTS,BUF))
        CALL OPSTXT(TRIM(LINE))
        WRITE(LINE, 1002) BUF, '  HPRO(TRCODE,BUF):',ZEXT(HPRO(TRCODE,BUF)), ZEXT(HPRO(TRCODE,BUF))
        CALL OPSTXT(TRIM(LINE))
        WRITE(LINE, 1001) BUF, '== DUMP INPTAB =='
        CALL OPSTXT(TRIM(LINE))
        CALL DUMP_MESSAGE(-1,BUF,BPRO(BINPTAB,BUF),HPRO(INPLEN,BUF))
        WRITE(LINE, 1001) BUF, '== DUMP WRKTAB =='
        CALL OPSTXT(TRIM(LINE))
        CALL DUMP_MESSAGE(-1,BUF,BPRO(WRKTAB*4-3+1,BUF),HPRO(INPLEN,BUF)+14)
        WRITE(LINE, 1001) BUF, '--------------------------------------------------'
        CALL OPSTXT(TRIM(LINE))
        
        
        RETURN
        END



        SUBROUTINE DUMP_MESSAGE(MESSAGE_ID, LINE_ID, OUTBUF, MESLEN)
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
        
        WRITE(BUF, 900) LINE_ID, MESSAGE_ID, LINE_ID, MESLEN
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
           WRITE(BUF, 902) LINE_ID, OFFSET + 1, OFFSET + 16,( ARR(I), I = 1, 16)
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
           WRITE(BUF, 902) LINE_ID, OFFSET + 1, OFFSET + REMAIN, (ARR(I), I = 1, 16)
           TYPE *, '', TRIM(BUF)
           CALL OPSTXT(TRIM(BUF))
        ENDIF
        TYPE *, ''

900     FORMAT('(',I5,'):PARSED MESSAGE #',I8,' (@ LINE #',I8,') : LEN = ', I8)
901     FORMAT(Z2.2)
902     FORMAT('(',I5,'):[',I4,':',I4,'] = ',16(A2,1X))

        RETURN
        END

        
