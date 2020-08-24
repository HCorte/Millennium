CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C File      : INIGS.FOR
C Change Log:
C
C Ver Date       Author  Comment
C --- ---------- ------- ----------------------------------------------
C V01 2013.02.24 SCML    PLACARD PROJECT - Created - handles input
C                        messages for IGS; created for support of the
C                        ADTA PROJECT: "Apostas Desportivas no Terminal
C                        Altura"(ADTA)
C
C
C THIS ROUTINE CAN GENERATE THE FOLLOWING 'SYNTERRCOD'
C         0     NO ERROR
C         3     INVALID MESSAGE SUBTYPE
C         4     GAME TYPE NOT ODDS
C         5     GAME INDEX NOT PLACARD
C        80     INVALID MESSAGE LENGTH
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C                 THIS ITEM IS THE PROPERTY OF SCML.
C
C             COPYRIGHT 2014 SCML. ALL RIGHTS RESERVED.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM INIGS
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

C DEBUG - START /*
        INCLUDE 'INCLIB:IGSDEBUG.DEF'
C DEBUG - END */

        INTEGER*4   MESS(EDLEN),LOGREC(LREC*3),WRKBUF(TRALEN)
        INTEGER*4   TASK
        INTEGER*4   BUF, STRTY
        INTEGER*4   MSTYPE, LSTSER
        INTEGER*4   RETYFLAG          ! RETRY CONTROL FLAG
        INTEGER*4   STATUS,TER,ST

        INTEGER*4   TEMP1, TEMP2
        INTEGER*4   I4TEMP
        INTEGER*2   I2TEMP(2)
        BYTE        I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)

        INTEGER*4   MESLEN            ! message length
        INTEGER*4   TEMP_MIDH, TEMP_MIDL ! temporary message id for synterr message
        INTEGER*4   I                 ! mess iterator count



        CALL OPSTXT(' Copyright 2014 SCML. All rights reserved. ')
        CALL SNIF_AND_WRKSET

        TASK    = IGI
        MESS(1) = TASK

        CALL OPSTXT(' ******************* INIGS ******************* ')

C
C SET DEFAULT VALUES
C
C        P(IGSTOUT) = 20         ! TIME OUT MESSAGES
C        P(IGSPGWAG) = 0         ! SUPPRESS WAGER
C        P(IGSPGCAN) = 0          ! SUPPRESS CANCEL
C        P(IGSPGVAL) = 0          ! SUPPRESS VALIDATIONS AND PAYMENTS
C        P(IGSPGREP) = 0          ! SUPPRESS ODDSET REPORTS

C
C WAIT FOR SOMETHING TO DO
C IF END OF DAY THEN CALL GSTOP(GEXIT_SUCCESS)
C
10      CONTINUE !fica neste ciclo enquanto o DAYSTS não passar para DSOPEN 
        IF(DAYSTS .EQ. DSCLOS) CALL GSTOP(GEXIT_SUCCESS) !para de correr o programa (neste caso inigs)
        IF(DAYSTS .EQ. DSSUSP) THEN
          CALL HOLD(0,STATUS) !setinterval fica x tempo on hold
          IF(DAYSTS .EQ. DSOPEN) GOTO 10
          GOTO 10
        ENDIF
C
C FORCE CALLING KERNEL THREAD TO HALT
C
        CALL HOLD(0,STATUS)

C
C GET BUFFER NUMBER FROM TOP OF QUEUE.
C IF NO WAGERS QUEUED, GO BACK TO WAIT STATE.
C

20      CONTINUE

        CALL TOPQUE(TASK,BUF)
        IF(BUF .EQ. 0) GOTO 10 !the queue is empty so wait to get any message by sending to alias 10 that ends up calling HOLD/timeout

C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('ENTROU NA QUEUE',0,0)
           TYPE*,'ENTROU NA QUEUE!!!'
        ENDIF
C DEBUG - END */

C
C VERIFY STATUS OF OUTIGS AND COMIGS
C
        CALL CHECKPROCESS()!nesta função parece que vai ver no sistema se os ficheiros existem e têm os respectovos prefix a identificar o tipo de jogo

C
C DECODE TERMINAL MESSAGE INTO INTERNAL TRANSACTION FORMAT (TRABUF)
C
C       RESET ERROR CODES
        SYNTERRCOD = 0

C
C TRABUF HEADER
C
C       START SET TRABUF VARIABLES
        TER = HPRO(TERNUM,BUF) ! Descodifica número do terminal (campo TERNUM do PROCOM). 2bytes pois é HPRO
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TER: ',TER,TER)
           TYPE*,'TER!!!',TER
        ENDIF
C DEBUG - END */

C       RESET TRABUF ARRAY
        CALL FASTSET(0,TRABUF,TRALEN)

C       INITIALIZE IGS ERROR DESCRIPTION WITH SPACES
            I1TEMP(1) = ZEXT (32) ! 0010 0000
            I1TEMP(2) = ZEXT (32) ! 0010 0000
            I1TEMP(3) = ZEXT (32) ! 0010 0000
            I1TEMP(4) = ZEXT (32) ! 0010 0000
            !PARAMETER (TIGS_XERR=28)   !IGS Error Code Description (3 x INTEGER*4) 
            TRABUF(TIGS_XERR + 0) = I4TEMP !28 -> 0010 0000
            TRABUF(TIGS_XERR + 1) = I4TEMP !29 -> 0010 0000
            TRABUF(TIGS_XERR + 2) = I4TEMP !30 -> 0010 0000

C SET DEFAULT VALUES
C       SYSTEM ERROR CODE                                           (27)
        TRABUF(TIGS_SERR) = 0

C       TRANSACTION STATUS                                           (1)
        TRABUF(TSTAT) = GOOD

C       TRANSACTION ERROR CODE                                       (2)
        TRABUF(TERR)  = NOER

C       TRANSACTION CDC DATE                                         (3)
        TRABUF(TCDC)  = DAYCDC

C       TERMINAL NUMBER                                              (4)
        TRABUF(TTER)  = TER

C       AGENT NUMBER                                                 (7)
        TRABUF(TAGT)  = AGTTAB(AGTNUM,TER)

C       GET SEQUENCE NUMBER                                          (8)
        TRABUF(TTRN)  = IAND(ZEXT(BPRO(BINPTAB,BUF)),'F'X)

C       TRANSACTION TYPE - TIGS = 12                                 (9)
        TRABUF(TTYP)  = TIGS
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TTYP: ',TRABUF(TTYP),TRABUF(TTYP))
           TYPE*,'TTYP: ',TRABUF(TTYP)
        ENDIF
C DEBUG - END */

C **************************************************
C DECODE IGS TRANSACTION TYPE (Message Subtype):
C     MSTYPE = 0 -> IGSWAG (VALUE = 0) = WAGER
C     MSTYPE = 1 -> IGSCAN (VALUE = 1) = CANCELLATION
C     MSTYPE = 2 -> IGSVAL (VALUE = 2) = VALIDATION
C     MSTYPE = 3 -> IGSPAY (VALUE = 3) = PAYMENT
C     MSTYPE = 4 -> IGSREP (VALUE = 4) = REPORT
C **************************************************
        MSTYPE = IAND(ZEXT(BPRO(BINPTAB+1,BUF)),'F'X) ! F -> 1111 AND 1010 0010 -> 0010 (filtra os bits mais significativos) pois BPRO é um byte e F em hexa corresponde a 0000 1111 bits
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('MSTYPE: ',MSTYPE,MSTYPE)
           TYPE*,'MSTYPE: ',MSTYPE
        ENDIF
C DEBUG - END */

C       GAME NUMBER                                                 (10)
        TRABUF(TGAM)  = 0

C       GET CHECKSUM                                                (17)
        TEMP1 = ZEXT(BPRO(BINPTAB+2,BUF))
        TEMP2 = ZEXT(BPRO(BINPTAB+3,BUF))
        TRABUF(TCHK) = ISHFT(TEMP1,8) + TEMP2
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TCHK: ',TRABUF(TCHK),TRABUF(TCHK))
           TYPE*,'TCHK: ',TRABUF(TCHK)
        ENDIF
C DEBUG - END */


C
C WAGER TRANSACTION
C
        IF(MSTYPE .EQ. 0) THEN
C          GET STATISTICS                                           (13)
           TRABUF(TTSTCS) = ZEXT(BPRO(BINPTAB+4,BUF))

C          GAME TYPE (18 - Oddset)                                  (11)
           TRABUF(TGAMTYP) = ZEXT(BPRO(BINPTAB+5,BUF))!TODS
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TGAMTYP: ',TRABUF(TGAMTYP),TRABUF(TGAMTYP))
              TYPE*,'TGAMTYP: ',TRABUF(TGAMTYP)
           ENDIF
C DEBUG - END */

C          GAME INDEX (1 - Placard)                                 (12)
           TRABUF(TGAMIND) = ZEXT(BPRO(BINPTAB+6,BUF))
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TGAMIND: ',TRABUF(TGAMIND),TRABUF(TGAMIND))
              TYPE*,'TGAMIND: ',TRABUF(TGAMIND)
           ENDIF
C DEBUG - END */

C          TERMINAL MESSAGE ID LOW PART                                (30) ! a posição do trabuf no 30 já é usada para IGS Error Code Description 3º byte
           CALL TERM_TO_HOST(BPRO(BINPTAB+11,BUF), TRABUF(TIGSW_MIDH), 4) !rever
           TEMP_MIDH = TRABUF(TIGSW_MIDH) !rever
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TIGSW_MIDH: ',TRABUF(TIGSW_MIDH),TRABUF(TIGSW_MIDH))
              TYPE*,'TIGSW_MIDH: ',TRABUF(TIGSW_MIDH)
           ENDIF
C DEBUG - END */

C          TERMINAL MESSAGE ID HIGH PART                               (31)
           CALL TERM_TO_HOST(BPRO(BINPTAB+15,BUF), TRABUF(TIGSW_MIDL), 4) !rever
           TEMP_MIDL = TRABUF(TIGSW_MIDL) !rever
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TIGSW_MIDL: ',TRABUF(TIGSW_MIDL),TRABUF(TIGSW_MIDL))
              TYPE*,'TIGSW_MIDL: ',TRABUF(TIGSW_MIDL)
           ENDIF
C DEBUG - END */

C
C CANCELLATION TRANSACTION 
C
        ELSEIF(MSTYPE .EQ. 1) THEN
C          GET STATISTICS                                           (13)
           TRABUF(TTSTCS) = ZEXT(BPRO(BINPTAB+4,BUF))

C          GAME TYPE (18 - Oddset)                                  (11)
           TRABUF(TGAMTYP) = ZEXT(BPRO(BINPTAB+5,BUF))!TODS
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TGAMTYP: ',TRABUF(TGAMTYP),TRABUF(TGAMTYP))
              TYPE*,'TGAMTYP: ',TRABUF(TGAMTYP)
           ENDIF
C DEBUG - END */

C          GAME INDEX (1 - Placard)                                 (12)
           TRABUF(TGAMIND) = ZEXT(BPRO(BINPTAB+6,BUF))
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TGAMIND: ',TRABUF(TGAMIND),TRABUF(TGAMIND))
              TYPE*,'TGAMIND: ',TRABUF(TGAMIND)
           ENDIF
C DEBUG - END */

C          TERMINAL MESSAGE ID LOW PART                                (30)
           CALL TERM_TO_HOST(BPRO(BINPTAB+11,BUF), TRABUF(TIGSC_MIDH), 4)
           TEMP_MIDH = TRABUF(TIGSC_MIDH)
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TIGSC_MIDH: ',TRABUF(TIGSC_MIDH),TRABUF(TIGSC_MIDH))
              TYPE*,'TIGSC_MIDH: ',TRABUF(TIGSC_MIDH)
           ENDIF
C DEBUG - END */

C          TERMINAL MESSAGE ID HIGH PART                               (31)
           CALL TERM_TO_HOST(BPRO(BINPTAB+15,BUF), TRABUF(TIGSC_MIDL), 4)
           TEMP_MIDL = TRABUF(TIGSC_MIDL)
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TIGSC_MIDL: ',TRABUF(TIGSC_MIDL),TRABUF(TIGSC_MIDL))
              TYPE*,'TIGSC_MIDL: ',TRABUF(TIGSC_MIDL)
           ENDIF
C DEBUG - END */

C
C VALIDATION TRANSACTION
C
        ELSEIF(MSTYPE .EQ. 2) THEN
C          GET STATISTICS                                           (13)
           TRABUF(TTSTCS) = ZEXT(BPRO(BINPTAB+4,BUF))

C          GAME TYPE (18 - Oddset)                                  (11)
           TRABUF(TGAMTYP) = ZEXT(BPRO(BINPTAB+5,BUF))!TODS
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TGAMTYP: ',TRABUF(TGAMTYP),TRABUF(TGAMTYP))
              TYPE*,'TGAMTYP: ',TRABUF(TGAMTYP)
           ENDIF
C DEBUG - END */

C          GAME INDEX (1 - Placard)                                 (12)
           TRABUF(TGAMIND) = ZEXT(BPRO(BINPTAB+6,BUF))
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TGAMIND: ',TRABUF(TGAMIND),TRABUF(TGAMIND))
              TYPE*,'TGAMIND: ',TRABUF(TGAMIND)
           ENDIF
C DEBUG - END */

C          TERMINAL MESSAGE ID LOW PART                                (30)
           CALL TERM_TO_HOST(BPRO(BINPTAB+11,BUF), TRABUF(TIGSV_MIDH), 4)
           TEMP_MIDH = TRABUF(TIGSV_MIDH)
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TIGSV_MIDH: ',TRABUF(TIGSV_MIDH),TRABUF(TIGSV_MIDH))
              TYPE*,'TIGSV_MIDH: ',TRABUF(TIGSV_MIDH)
           ENDIF
C DEBUG - END */

C          TERMINAL MESSAGE ID HIGH PART                               (31)
           CALL TERM_TO_HOST(BPRO(BINPTAB+15,BUF), TRABUF(TIGSV_MIDL), 4)
           TEMP_MIDL = TRABUF(TIGSV_MIDL)
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TIGSV_MIDL: ',TRABUF(TIGSV_MIDL),TRABUF(TIGSV_MIDL))
              TYPE*,'TIGSV_MIDL: ',TRABUF(TIGSV_MIDL)
           ENDIF
C DEBUG - END */
C
C PAYMENT TRANSACTION
C
        ELSEIF(MSTYPE .EQ. 3) THEN
C          GET STATISTICS                                           (13)
           TRABUF(TTSTCS) = ZEXT(BPRO(BINPTAB+4,BUF))

C          GAME TYPE (18 - Oddset)                                  (11)
           TRABUF(TGAMTYP) = ZEXT(BPRO(BINPTAB+5,BUF))!TODS
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TGAMTYP: ',TRABUF(TGAMTYP),TRABUF(TGAMTYP))
              TYPE*,'TGAMTYP: ',TRABUF(TGAMTYP)
           ENDIF
C DEBUG - END */

C          GAME INDEX (1 - Placard)                                 (12)
           TRABUF(TGAMIND) = ZEXT(BPRO(BINPTAB+6,BUF))
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TGAMIND: ',TRABUF(TGAMIND),TRABUF(TGAMIND))
              TYPE*,'TGAMIND: ',TRABUF(TGAMIND)
           ENDIF
C DEBUG - END */

C          TERMINAL MESSAGE ID LOW PART                                (30)
           CALL TERM_TO_HOST(BPRO(BINPTAB+11,BUF), TRABUF(TIGSP_MIDH), 4)
           TEMP_MIDH = TRABUF(TIGSP_MIDH)
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TIGSP_MIDH: ',TRABUF(TIGSP_MIDH),TRABUF(TIGSP_MIDH))
              TYPE*,'TIGSP_MIDH: ',TRABUF(TIGSP_MIDH)
           ENDIF
C DEBUG - END */

C          TERMINAL MESSAGE ID HIGH PART                               (31)
           CALL TERM_TO_HOST(BPRO(BINPTAB+15,BUF), TRABUF(TIGSP_MIDL), 4)
           TEMP_MIDL = TRABUF(TIGSP_MIDL)
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TIGSP_MIDL: ',TRABUF(TIGSP_MIDL),TRABUF(TIGSP_MIDL))
              TYPE*,'TIGSP_MIDL: ',TRABUF(TIGSP_MIDL)
           ENDIF
C DEBUG - END */
C
C FOR REPORT TRANSACTIONS
C
        ELSEIF(MSTYPE .EQ. 4) THEN
C          GAME TYPE (18 - Oddset)                                  (11)
           TRABUF(TGAMTYP) = ZEXT(BPRO(BINPTAB+4,BUF))!TODS
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TGAMTYP: ',TRABUF(TGAMTYP),TRABUF(TGAMTYP))
              TYPE*,'TGAMTYP: ',TRABUF(TGAMTYP)
           ENDIF
C DEBUG - END */

C          GAME INDEX (1 - Placard)                                 (12)
           TRABUF(TGAMIND) = ZEXT(BPRO(BINPTAB+5,BUF))
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TGAMIND: ',TRABUF(TGAMIND),TRABUF(TGAMIND))
              TYPE*,'TGAMIND: ',TRABUF(TGAMIND)
           ENDIF
C DEBUG - END */

C          TERMINAL MESSAGE ID LOW PART                                (30)
           CALL TERM_TO_HOST(BPRO(BINPTAB+10,BUF), TRABUF(TIGSR_MIDH), 4)
           TEMP_MIDH = TRABUF(TIGSR_MIDH)
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TIGSR_MIDH: ',TRABUF(TIGSR_MIDH),TRABUF(TIGSR_MIDH))
              TYPE*,'TIGSR_MIDH: ',TRABUF(TIGSR_MIDH)
           ENDIF
C DEBUG - END */

C          TERMINAL MESSAGE ID HIGH PART                               (31)
           CALL TERM_TO_HOST(BPRO(BINPTAB+14,BUF), TRABUF(TIGSR_MIDL), 4)
           TEMP_MIDL = TRABUF(TIGSR_MIDL)
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TIGSR_MIDL: ',TRABUF(TIGSR_MIDL),TRABUF(TIGSR_MIDL))
              TYPE*,'TIGSR_MIDL: ',TRABUF(TIGSR_MIDL)
           ENDIF
C DEBUG - END */

C
C INVALID MESSAGE SUBTYPE (CONSIDER AS WAGER TRANSACTION)
C
        ELSE
C          GAME TYPE (18 - Oddset)                                     (11)
           TRABUF(TGAMTYP) = TODS
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TGAMTYP: ',TRABUF(TGAMTYP),TRABUF(TGAMTYP))
              TYPE*,'TGAMTYP: ',TRABUF(TGAMTYP)
           ENDIF
C DEBUG - END */

C          GAME INDEX (1 - Placard)                                    (12)
           TRABUF(TGAMIND) = 1
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TGAMIND: ',TRABUF(TGAMIND),TRABUF(TGAMIND))
              TYPE*,'TGAMIND: ',TRABUF(TGAMIND)
           ENDIF
C DEBUG - END */
C          SET TRANSACTION TYPE                                        (25)
           TRABUF(TIGS_TTYP) = IGSWAG

C          TERMINAL MESSAGE ID LOW PART                                (30)
           TRABUF(TIGSW_MIDH) = 0 
           TEMP_MIDH = TRABUF(TIGSW_MIDH)
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TIGSW_MIDH: ',TRABUF(TIGSW_MIDH),TRABUF(TIGSW_MIDH))
              TYPE*,'TIGSW_MIDH: ',TRABUF(TIGSW_MIDH)
           ENDIF
C DEBUG - END */

C          TERMINAL MESSAGE ID HIGH PART                               (31)
           TRABUF(TIGSW_MIDL) = 0
           TEMP_MIDL = TRABUF(TIGSW_MIDL)
C DEBUG - START /*
           IF(IGSDEBUG(IA_INIGS)) THEN
              CALL OPS('TIGSW_MIDL: ',TRABUF(TIGSW_MIDL),TRABUF(TIGSW_MIDL))
              TYPE*,'TIGSW_MIDL: ',TRABUF(TIGSW_MIDL)
           ENDIF
C DEBUG - END */

           TRABUF(TSTAT) = REJT
           TRABUF(TERR)  = SYNT
           SYNTERRCOD    = 3
           GOTO 28
        ENDIF

C       FILE STATUS                                                 (15)
        TRABUF(TFIL) = 0

C       TRANSACTION SOLD CDC DATE                                   (21)
        TRABUF(TCDC_SOLD) = DAYCDC


C IGS WAGER TRANSACTION
        IF(MSTYPE .EQ. 0) THEN
C          SET TRANSACTION TYPE                                     (25)
           TRABUF(TIGS_TTYP) = IGSWAG
C          VALIDATE GAME TYPE
           IF(TRABUF(TGAMTYP) .EQ. TODS) THEN
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('CALLING DIGSODSWAG!!!',0,0)
           TYPE*,'CALLING DIGSODSWAG!!!'
        ENDIF
C DEBUG - END */
              CALL DIGSODSWAG(BPRO(BINPTAB,BUF),TRABUF)
           ELSE
              TRABUF(TSTAT) = REJT
              TRABUF(TERR)  = SYNT
              SYNTERRCOD    = 4
           ENDIF
C IGS CANCELATION TRANSACTION
        ELSE IF (MSTYPE .EQ. 1) THEN
C          SET TRANSACTION TYPE                                     (25)
           TRABUF(TIGS_TTYP) = IGSCAN
C          VALIDATE GAME TYPE
           IF(TRABUF(TGAMTYP) .EQ. TODS) THEN
              CALL DIGSODSCAN(BPRO(BINPTAB,BUF),TRABUF)
           ELSE
              TRABUF(TSTAT) = REJT
              TRABUF(TERR)  = SYNT
              SYNTERRCOD    = 4
           ENDIF
C IGS VALIDATION TRANSACTION
        ELSE IF (MSTYPE .EQ. 2) THEN
C          SET TRANSACTION TYPE                                     (25)
           TRABUF(TIGS_TTYP) = IGSVAL
           
C          VALIDATE GAME TYPE
           IF(TRABUF(TGAMTYP) .EQ. TODS) THEN
              CALL DIGSODSVAL(BPRO(BINPTAB,BUF),TRABUF)
           ELSE
              TRABUF(TSTAT) = REJT
              TRABUF(TERR)  = SYNT
              SYNTERRCOD    = 4
           ENDIF
C IGS PAYMENT TRANSACTION
        ELSE IF (MSTYPE .EQ. 3) THEN
C          SET TRANSACTION TYPE                                     (25)
           TRABUF(TIGS_TTYP) = IGSPAY
C          VALIDATE GAME TYPE
           IF(TRABUF(TGAMTYP) .EQ. TODS) THEN
              CALL DIGSODSPAY(BPRO(BINPTAB,BUF),TRABUF)
           ELSE
              TRABUF(TSTAT) = REJT
              TRABUF(TERR)  = SYNT
              SYNTERRCOD    = 4
           ENDIF
C IGS GAME PROGRAMME REPORT TRANSACTION
        ELSE IF (MSTYPE .EQ. 4) THEN
C          SET TRANSACTION TYPE                                     (25)
           TRABUF(TIGS_TTYP) = IGSREP
C          VALIDATE GAME TYPE
           IF(TRABUF(TGAMTYP) .EQ. TODS) THEN
              CALL DIGSODSREP(BPRO(BINPTAB,BUF),TRABUF)
           ELSE
              TRABUF(TSTAT) = REJT
              TRABUF(TERR)  = SYNT
              SYNTERRCOD    = 4
           ENDIF
        ENDIF

28      CONTINUE

C       IF THERE IS A SYNTAX ERROR CODE GOTO QUEMESS
        IF(SYNTERRCOD.NE.0) THEN
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('HAS A SYNTAX ERROR: ',SYNTERRCOD,SYNTERRCOD)
           TYPE*,'HAS A SYNTAX ERROR: ',SYNTERRCOD
        ENDIF
C DEBUG - END */
                GOTO 30
        ENDIF

C       MESSAGE LENGTH
        MESLEN = ZEXT(HPRO(INPLEN,BUF))

C       VALIDATE MESSAGE LENGTH
        IF (MESLEN .LT. 0) THEN            !MESSAGE LENGHT WORST CASE SCENARIO
           TRABUF(TSTAT) = REJT
           TRABUF(TERR)  = SYNT
           SYNTERRCOD    = 80
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('INVALID MESSAGE LENGTH: ',MESLEN,MESLEN)
           TYPE*, 'INVALID MESSAGE LENGTH - MESLEN>',MESLEN,' TER>',TRABUF(TTER)
        ENDIF
C DEBUG - END */

           GOTO 30

        ENDIF

30      CONTINUE

C       AGENT NOT SIGNED ON
        IF(AGTHTB(AOPSTS,TER).NE.SIGNON) THEN
           TRABUF(TSTAT) = REJT
           TRABUF(TERR)  = NOTON
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('AGENT NOT SIGNED ON: ',TER,TER)
           TYPE*,'AGENT NOT SIGNED ON: ',TER
        ENDIF
C DEBUG - END */
        ENDIF

C
C IF SYNTAX ERROR THEN PRINT ERROR CODE ON THE CONSOLE.
C
        IF(P(SUPSYN).EQ.0.AND.SYNTERRCOD.NE.0.AND.
     *      TRABUF(TERR).NE.NOTON) THEN

           TYPE *,' '
           TYPE *,'ERROR: SYNTAX ERROR CODE ',SYNTERRCOD
           TYPE *,'AGENT NUMBER:      ',TRABUF(TAGT)
           TYPE *,'TERMINAL NUMBER:   ',TRABUF(TTER)
           TYPE *,'TRANSACTION TYPE:  ',TRABUF(TIGS_TTYP)
           TYPE *,'CDC DATE:          ',TRABUF(TCDC)
C MESSAGE LENGTH
           MESLEN = ZEXT(HPRO(INPLEN,BUF))
           TYPE *,'MESLEN:            ',MESLEN

           DO I=0,MESLEN-1
              TYPE 9997,I,BPRO(BINPTAB+I,BUF) 
           ENDDO

           TYPE *,' '
            MESS(2) = TEGEN
            MESS(3) = 47
            MESS(4) = SYNTERRCOD
            MESS(5) = TER
            MESS(6) = TRABUF(TGAMTYP)
            MESS(7) = TRABUF(TGAMIND)
            MESS(8) = TEMP_MIDH
            MESS(9) = TEMP_MIDL
            CALL QUEMES(MESS)
        ENDIF

C
C IF TERMINAL RETRY, CONTINUE RETRY PROCESSING, ELSE PROCESS AS NORMAL.
C
        RETYFLAG = IAND(ZEXT(BPRO(BINPTAB,BUF)),'40'X)

        IF(TRABUF(TSTAT).EQ.GOOD .AND. RETYFLAG .NE. 0 .AND.
     *    TRABUF(TTRN) .EQ. ZEXT(AGTHTB(ATRNUM,TER))) THEN
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('VALIDATING A RETRY! ',0,0)
           TYPE*,'VALIDATING A RETRY! '
        ENDIF
C DEBUG - END */
           LSTSER=AGTTAB(ALSTRA,TER)
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('LAST SER: ',LSTSER,LSTSER)
           TYPE*,'LAST SER: ',LSTSER
        ENDIF
C DEBUG - END */
           CALL RLOG(LSTSER,LOGREC,TASK,STRTY)
           IF(STRTY .NE. 0) THEN
              CALL WAIT_APUQUE
              CALL RLOG(LSTSER,LOGREC,TASK,STRTY)
           ENDIF
           IF(STRTY .NE. 0) THEN
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('STRTY ERROR! ',0,0)
           TYPE*,'STRTY ERROR! '
        ENDIF
C DEBUG - END */
                GOTO 80
           ENDIF

C          CREATE WORKBUF FOR THE LAST TRANSACTION
           CALL FASTSET(0,WRKBUF,TRALEN)
           CALL LOGTRA(WRKBUF,LOGREC)

C
C CHECK IF MESSAGE ID MATCHES
C
C WAGER TRANSACTION
          IF(MSTYPE .EQ. 0) THEN
             IF(WRKBUF(TIGSW_MIDH) .NE. TRABUF(TIGSW_MIDH)) THEN
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSW_MIDH NOT EQUAL! ',0,0)
           TYPE*,'TIGSW_MIDH NOT EQUAL! '
        ENDIF
C DEBUG - END */
                GOTO 80
             ENDIF
             IF(WRKBUF(TIGSW_MIDL) .NE. TRABUF(TIGSW_MIDL)) THEN
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSW_MIDL NOT EQUAL! ',0,0)
           TYPE*,'TIGSW_MIDL NOT EQUAL! '
        ENDIF
C DEBUG - END */
                GOTO 80
             ENDIF

C CANCELLATION TRANSACTION
          ELSEIF(MSTYPE .EQ. 1) THEN
             IF(WRKBUF(TIGSC_MIDH) .NE. TRABUF(TIGSC_MIDH)) THEN
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSC_MIDH NOT EQUAL! ',0,0)
           TYPE*,'TIGSC_MIDH NOT EQUAL! '
        ENDIF
C DEBUG - END */
                GOTO 80
             ENDIF
             IF(WRKBUF(TIGSC_MIDL) .NE. TRABUF(TIGSC_MIDL)) THEN
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSC_MIDL NOT EQUAL! ',0,0)
           TYPE*,'TIGSC_MIDL NOT EQUAL! '
        ENDIF
C DEBUG - END */
                GOTO 80
             ENDIF

C VALIDATION TRANSACTION
          ELSEIF(MSTYPE .EQ. 2) THEN
             IF(WRKBUF(TIGSV_MIDH) .NE. TRABUF(TIGSV_MIDH)) THEN
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSV_MIDH NOT EQUAL! ',0,0)
           TYPE*,'TIGSV_MIDH NOT EQUAL! '
        ENDIF
C DEBUG - END */
                GOTO 80
             ENDIF
             IF(WRKBUF(TIGSV_MIDL) .NE. TRABUF(TIGSV_MIDL)) THEN
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSV_MIDL NOT EQUAL! ',0,0)
           TYPE*,'TIGSV_MIDL NOT EQUAL! '
        ENDIF
C DEBUG - END */
                GOTO 80
             ENDIF

C PAYMENT TRANSACTION
          ELSEIF(MSTYPE .EQ. 3) THEN
             IF(WRKBUF(TIGSP_MIDH) .NE. TRABUF(TIGSP_MIDH)) THEN
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSP_MIDH NOT EQUAL! ',0,0)
           TYPE*,'TIGSP_MIDH NOT EQUAL! '
        ENDIF
C DEBUG - END */
                GOTO 80
             ENDIF
             IF(WRKBUF(TIGSP_MIDL) .NE. TRABUF(TIGSP_MIDL)) THEN
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSP_MIDL NOT EQUAL! ',0,0)
           TYPE*,'TIGSP_MIDL NOT EQUAL! '
        ENDIF
C DEBUG - END */
                GOTO 80
             ENDIF

C REPORT TRANSACTION
          ELSEIF(MSTYPE .EQ. 4) THEN
             IF(WRKBUF(TIGSR_MIDH) .NE. TRABUF(TIGSR_MIDH)) THEN
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSR_MIDH NOT EQUAL! ',0,0)
           TYPE*,'TIGSR_MIDH NOT EQUAL! '
        ENDIF
C DEBUG - END */
                GOTO 80
             ENDIF
             IF(WRKBUF(TIGSR_MIDL) .NE. TRABUF(TIGSR_MIDL)) THEN
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSR_MIDL NOT EQUAL! ',0,0)
           TYPE*,'TIGSR_MIDL NOT EQUAL! '
        ENDIF
C DEBUG - END */
                GOTO 80
             ENDIF
          ENDIF

          TRABUF(TSTAT) = REJT
          TRABUF(TERR) = RETY
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('THIS IS A VALID RETRY! ',0,0)
           TYPE*,'THIS IS A VALID RETRY! '
        ENDIF
C DEBUG - END */

C END RETRY
        ENDIF

80      CONTINUE 

C
C IF NO CONNECTION TO IGS THEN CREATE ONE ERROR MESSAGE
C
        IF(TRABUF(TSTAT).EQ.GOOD .AND. P(IGSCONF) .EQ. 0) THEN
           TRABUF(TERR)     = SDOR
           TRABUF(TSTAT)    = REJT
           HPRO(REMSTS,BUF)  = RMDOWN
           HPRO(TRCODE,BUF) = TYPIGS
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('NO CONNECTION TO IGS! ',0,0)
           TYPE*,'NO CONNECTION TO IGS! '
        ENDIF
C DEBUG - END */
        ENDIF

C
C PUT TRABUF INTO APUBUF FOR COMIGS GET IT AND SET TRANSACTION CODE TO TYPIGS (IGS TRANSACTION)
C
        CALL TRALOG(TRABUF,APUBUF(2,BUF))
        HPRO(TRCODE,BUF) = TYPIGS

C
C IF TRANSACTION IS ERROR THEN PUT IT ON DISPAT - TO SEND TO OUTIGS TO CREATE ERROR MESSAGE
C ELSE SEND TO COMIGS TO PUT MESSAGE INTO MESSAGEQ
C
        IF (TRABUF(TSTAT) .EQ. GOOD .OR. (TRABUF(TSTAT) .EQ. REJT .AND. TRABUF(TERR) .EQ. RETY)) THEN
C          PUT BUFFER IN COMIGS QUEUE (this uses a new queue outside the 32 default queues which requires a different lib (IGSCOMMON.FOR)
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('CALLING IGS_QUETRA!!! ',0,0)
           TYPE*,'CALLING IGS_QUETRA!!! '
        ENDIF
C DEBUG - END */
           CALL IGS_QUETRA(BUF)
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('CALLING DQUTRA!!! ',0,0)
           TYPE*,'CALLING DQUTRA!!! '
        ENDIF
C DEBUG - END */
C          RELEASE FIRST BUFFER USED
           CALL DQUTRA(TASK,BUF)  ! Keep this implementation for queues in 32 default queues.

        ELSE
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TSTAT NE GOOD: PUT ON DISPAT! ',0,0)
           TYPE*,'TSTAT NE GOOD: PUT ON DISPAT! '
        ENDIF
C DEBUG - END */
           CALL ABL(BUF,QUETAB(1,DIS),ST)
           CALL DQUTRA(TASK,BUF)
        ENDIF


        GOTO 20

9997    FORMAT(' SYNTAX ERROR MESSWORD RECB: ',I4.3,' - ', Z3.2)

C PROGRAM INIGS ENDED
       END




C***********************************************************************
C SUBROUTINE para verificar o estado dos processos COMIGS e OUTIGS
C     SUBROUTINE:    CHECKPROCESS
C     INPUT:
C     OUTPUT:
C***********************************************************************
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
C IF COMIGS DO NOT RUN THEN RESTART COMIGS
C
        CALL STTSK(8HCOMIGS  ,TSKSTS,STATUS) !VERIFY IF COMIGS IS OK (pergunta aqui a função vai buscar o nome do ficheiro por um path no sistema para confirmar se está a correr???)
        IF (STATUS .EQ. 4) THEN
           CALL OPSTXT('ERROR!!!!! COMIGS IS NOT RUNNING')
           CALL OPSTXT('STARTING COMIGS AND TRY TO PROCESS ALL TRANSACTIONS ')
C           CALL START(TSKNAM(32)) ! COMIGS só COMIGS TASK number is 32
           CALL START(8HCOMIGS  )
        ENDIF
C
C IF OUTIGS DO NOT RUN THEN RESTART OUTIGS
C
        CALL STTSK(8HOUTIGS  ,TSKSTS,STATUS)
        IF (STATUS .EQ. 4) THEN
           CALL OPSTXT('ERROR!!!!! OUTIGS IS NOT RUNNING')
           CALL OPSTXT('STARTING OUTIGS AND TRY TO PROCESS ALL TRANSACTIONS ')
C           CALL START(TSKNAM(31)) !OUTIGS só OUTIGS TASK number is 31 
           CALL START(8HOUTIGS  )
        ENDIF
       END
