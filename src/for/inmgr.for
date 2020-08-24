C INMGR.FOR
C
C V06 06-OCT-2016 SCML Retry fix:
C                       Validation - game type removed from retry logic
C                       Cancellation - game type removed from retry logic
C                       Wager - the byte following the last byte of the message
C                               was being miscopied into TRABUF(TEUWBOARD)
C V05 07-MAR-2016 SCML M16 PROJECT: wager and validation messages modified
C                      Added terminal sign-on check
C V04 24-APR-2014 SCML Added support to PLACARD Project - IGS
C V03 08-OCT-2013 SCML New Validation Messages
C V02 12-APR-2011 FJG  ACCENTURE MERGE FOR EM2
C V01        2004 ---  INITIAL RELEASE
C
C
C PROGRAM CAN GENERATE FOLLOWING 'SYNTERRCOD'
C         0     NO ERROR
C        10     INVALID GAME TYPE IN WAGER REQUESTS (ONLY EUROMILLIONS GAME TYPE ALLOWED)
C        20     INVALID GAME INDEX IN WAGER REQUESTS (ONLY EUROMILLIONS GAME INDEX ALLOWED)
C        30     INVALID EUROMILLIONS GAME NUMBER (OUT OF RANGE)
C        40     INVALID EUROMILLIONS GAME NUMBER REVISION
C        80     INVALID EUROMILLIONS BOARD LENGTH (OUT OF RANGE)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2004 SCML/Accenture. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM INMGR
        IMPLICIT NONE
C
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
        INCLUDE 'INCLIB:EURCOM.DEF'                                             !V05
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:IGSDEBUG.DEF'
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        
        INTEGER*4 MESS(EDLEN),LOGREC(LREC*3),WRKBUF(TRALEN)       !
        INTEGER*4 TASK              !
        INTEGER*4 BUF,STRTY,I         !
        INTEGER*4 MTYPE,LSTSER
        INTEGER*4 STATUS,TER,ST
        INTEGER*4 EGNUM                                                         !V05
C
        INTEGER*4 TEMP              ! temp variable
        INTEGER*4 TEMP1             !  "       "
        INTEGER*4 TEMP2             !  "       "
        INTEGER*4 TEMP3             !  "       "       
C----+------------------------------------------------------------------
C V03| New validation messages
C----+------------------------------------------------------------------
        INTEGER*4 TEMP4             !  "       "       
C----+------------------------------------------------------------------
C V03| New validation messages
C----+------------------------------------------------------------------
        INTEGER*4 IND		    ! offset counter in terminal message
        INTEGER*4 OPTIONS           ! options byte
        INTEGER*2 MESLEN            ! message lenth
        INTEGER*4 LEN               ! calculated message length
        INTEGER*4   I4TEMP
        INTEGER*2   I2TEMP(2)
        BYTE        I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C        INTEGER*4 LOGBUF(LREC*3)

C----+------------------------------------------------------------------
C V03| New validation messages
C----+------------------------------------------------------------------
        INTEGER*8   NIB
        BYTE        I1NIB(8)
        EQUIVALENCE (NIB,I1NIB)    
C----+------------------------------------------------------------------
C V03| New validation messages
C----+------------------------------------------------------------------


        CALL OPSTXT(' Copyright 2004 SCML/Accenture. All rights reserved. ')
        CALL SNIF_AND_WRKSET

        TASK    = EUI
        MESS(1) = TASK

        CALL OPSTXT(' ******************* INMGR ****************')
C
C SET DEFAULT VALUES
C
C        P(EUTIMOUT) = 20 	! TIME OUT MESSAGES
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C        P(EUSPWAG) = 0 		! SUPPRESS WAGER 
C        P(EUSPCAN) = 0 		! SUPPRESS CANCEL 
C        P(EUSPVAL) = 0 		! SUPPRESS VALIDATIONS
C        P(EUSPREP) = 0          ! SUPPRESS EUROMILLIONS' REPORTS
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
C WAIT FOR SOMETHING TO DO
C IF END OF DAY THEN CALL GSTOP(GEXIT_SUCCESS)
C
10      CONTINUE
        IF(DAYSTS .EQ. DSCLOS) CALL GSTOP(GEXIT_SUCCESS)
        IF(DAYSTS .EQ. DSSUSP) THEN
          CALL HOLD(0,STATUS)
          IF(DAYSTS .EQ. DSOPEN) GOTO 10
          GOTO 10
        ENDIF
C
C VERIFY STATUS OF OUTMGR AND COMMGR
C
C        CALL CHECKPROCESS()
        CALL HOLD(0,STATUS)
C
C GET BUFFER NUMBER FROM TOP OF QUEUE.
C IF NO WAGERS QUEUED, GO BACK TO WAIT STATE.
C
20      CONTINUE
C        CALL OPS('VALOR DE GLOBAL VAR',P(EUMILF),P(EUMILF))
        CALL TOPQUE(TASK,BUF)
        IF(BUF .EQ. 0) GOTO 10
C
C VERIFY STATUS OF OUTMGR AND COMMGR
C        
        CALL CHECKPROCESS()
C
C DECODE TERMINAL MESSAGE INTO INTERNAL TRANSACTION FORMAT
C
        SYNTERRCOD = 0                                                          !V05
        TER =HPRO(TERNUM,BUF) 
c        CALL OPSTXT('EURO MIL MESSAGE in')
        CALL FASTSET(0,TRABUF,TRALEN)
        TRABUF(TTYP) = TEUR
        TRABUF(TSTAT) = GOOD
        TRABUF(TERR) = NOER
        TRABUF(TCDC) = DAYCDC
        TRABUF(TCDC_SOLD) = DAYCDC
        TRABUF(TTER) = TER
        TRABUF(TAGT)  = AGTTAB(AGTNUM,TER)
        TRABUF(TFIL) = 0
C        TRABUF(TGAMIND) = 1                                                    !V05
C        TRABUF(TGAMTYP) = TEUM                                                 !V05
        TRABUF(TGAM) = 0
C
C GET SEQUENCE NUMBER
C      
        TRABUF(TTRN) = IAND(ZEXT(BPRO(BINPTAB,BUF)),15)
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF(IGSDEBUG(IA_INMGR)) THEN
            CALL OPSTXT('INMGR - INPTAB - MESSAGE DUMP:')
            CALL DUMP_MESSAGE(144,BUF,BPRO(BINPTAB*4-3+1,BUF),ZEXT(HPRO(INPLEN,BUF)))
        ENDIF
C----+------------------------------------------------------------------
C V04| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
C GET CHECKSUM
C
        TEMP1 = ZEXT(BPRO(BINPTAB+2,BUF))
        TEMP2 = ZEXT(BPRO(BINPTAB+3,BUF))
        TRABUF(TCHK) = ISHFT(TEMP1,8) + TEMP2
C        CALL TERM_TO_HOST(BPRO(BINPTAB+2,BUF), TRABUF(TCHK), 2)
C        CALL OPS('TRABUF(TCHK)',TRABUF(TCHK),TRABUF(TCHK))

C
C GET STATISTICS
C
        TRABUF(TTSTCS)=ZEXT(BPRO(BINPTAB+4,BUF))
C
C DECODE TRANSACTION EURO MILHOES TYPE:
C        MTYPE = 0 -> TWAG (VALUE = 1) = WAGGER
C        MTYPE = 1 -> TCAN (VALUE = 2) = CANCELATION
C        MTYPE = 2 -> TVAL (VALUE = 4) = VALIDATION
C        
        MTYPE = ZEXT(BPRO(BINPTAB+1,BUF))
        MTYPE = ISHFT(MTYPE,-4)
C
C WAGER TYPE
C
        IF (MTYPE .EQ. 0) THEN
           
           TRABUF(TEUTYP)  = TWAG
           TRABUF(TGAMTYP) = BPRO(BINPTAB+5,BUF)                                !V05
           TRABUF(TGAMIND) = ISHFT(ZEXT(BPRO(BINPTAB+6,BUF)),-4)                !V05
           IF(TRABUF(TGAMTYP).NE.TEUM) THEN                                     !V05
             TRABUF(TSTAT) = REJT
             TRABUF(TERR)  = SYNT
             SYNTERRCOD=10
             GOTO 100
           ENDIF
C          CHECK EUROMILLIONS GAME INDEX
           IF(TRABUF(TGAMIND).NE.EUM1GI) THEN                                   !V05
             TRABUF(TSTAT) = REJT
             TRABUF(TERR)  = SYNT
             SYNTERRCOD=20
             GOTO 100
           ENDIF
C          CHECK EUROMILLIONS GAME NUMBER OUT OF RANGE
           EGNUM = EGTNTAB(TRABUF(TGAMTYP),TRABUF(TGAMIND))
           IF(EGNUM.LT.1.OR.EGNUM.GT.ENUMEUM) THEN                              !V05
             TRABUF(TSTAT) = REJT
             TRABUF(TERR)  = SYNT
             SYNTERRCOD=30
             GOTO 100
           ENDIF
C
C GET DURATION AND NUMBER OF BOARDS
C           
           TEMP = ZEXT(BPRO(BINPTAB+7,BUF))
           TRABUF(TEUWNBET) = IAND(TEMP,15)
           TRABUF(TEUWDUR)  = ISHFT(TEMP,-4)
C           CALL OPS('NBET E DUR',TRABUF(TEUWNBET),TRABUF(TEUWDUR))
C
C GET OPTIONS FIELD
C
           TEMP1 = ZEXT(BPRO(BINPTAB+8,BUF))
           TEMP2 = ZEXT(BPRO(BINPTAB+9,BUF))
           OPTIONS = ISHFT(TEMP1,8) + TEMP2
           IND=10           
C
C CHECK OPTIONS
C
           IF(IAND(OPTIONS,'8000'X) .NE. 0) THEN
            TEMP = 0                                                            !V05
            CALL TERM_TO_HOST(BPRO(BINPTAB+IND,BUF), TEMP, 2)                   !V05
            IND = IND + 2
            IF(TEMP.NE.EUMCTRLREV(EGNUM)) THEN                                  !V05
              TRABUF(TSTAT) = REJT                                              !V05
              TRABUF(TERR)  = GREV                                              !V05
              TRABUF(TSUBERR) = TRABUF(TSUBERR) + GREV_GAMCLT                   !V05
              SYNTERRCOD = 40                                                   !V05
            ENDIF                                                               !V05
           ENDIF
           IF(IAND(OPTIONS,'4000'X) .NE. 0) THEN
           	IND = IND + 2
           ENDIF
           IF(IAND(OPTIONS,'2000'X) .NE. 0) THEN
           	IND = IND + 2
           ENDIF
           IF(IAND(OPTIONS,'0800'X) .NE. 0) THEN
           	IND = IND + 2
           ENDIF
           IF(IAND(OPTIONS,'0400'X) .NE. 0) THEN
           	IND = IND + 2
           ENDIF

C
C CHECK FOR QP IF TRUE THEN GET QUICK PICK FLAGS
C
           IF(IAND(OPTIONS,'0200'X) .NE. 0) THEN
             TEMP1 = ZEXT(BPRO(BINPTAB+IND+0,BUF))
             TEMP2 = ZEXT(BPRO(BINPTAB+IND+1,BUF))
             TRABUF(TEUWQP) = ISHFT(TEMP1,8) + TEMP2
             IND=IND+2
C             CALL OPS('QUICK PICK FLAGS',TRABUF(TEUWQP),TRABUF(TEUWQP))
           ENDIF
C
C CHECK FOR SYSTEM BET FLAG IF TRUE GET SYSTEM NUMBER (2 bytes)
C
           IF(IAND(OPTIONS,'0100'X) .NE. 0) THEN
             TEMP1 = ZEXT(BPRO(BINPTAB+IND+0,BUF))
             TEMP2 = ZEXT(BPRO(BINPTAB+IND+1,BUF))
             TRABUF(TEUWNMK) = TEMP1
             TRABUF(TEUWNST) = TEMP2
             IND=IND+2
C             CALL OPS('SYSTEM NUMBER MARK E STARS',TRABUF(TEUWNMK),TRABUF(TEUWNST))
           ENDIF          
C
C CHECK FOR PARTICIPATING IN JOKER 1 FLAG !V05
C
           IF(IAND(OPTIONS,'0080'X) .NE. 0) THEN
             TRABUF(TEUW_KIWFL) = 1
           ENDIF
C
C CHECK OPTIONS
C
           IF(IAND(OPTIONS,'0008'X) .NE. 0) THEN
           	IND = IND + 8
           ENDIF
           IF(IAND(OPTIONS,'0004'X) .NE. 0) THEN
           	IND = IND + 1
           ENDIF
           IF(IAND(OPTIONS,'0002'X) .NE. 0) THEN
           	IND = IND + 4
           ENDIF
           IF(IAND(OPTIONS,'0001'X) .NE. 0) THEN
           	IND = IND + 4
           ENDIF
C
C PLAYER NIF !V05
C
           CALL TERM_TO_HOST(BPRO(BINPTAB+IND,BUF), TRABUF(TEUW_PLNIF), 4)
           IND = IND + 4
C
C SALES CHANNEL !V05
C
           TRABUF(TEUW_EUWCH) = ZEXT(BPRO(BINPTAB+IND,BUF))
           IND = IND + 1
C
C DRAW INDICATOR
C
           TEMP = ZEXT(BPRO(BINPTAB+IND,BUF))
           TRABUF(TEUWDRWIND) = TEMP
C
           IND = IND + 9 
           MESLEN = ZEXT(HPRO(INPLEN,BUF))
CV06           LEN = MESLEN - IND + 1
           LEN = MESLEN - IND                                                   !V06
C
C CHECK LENGTH ADDED ON 2010-06-01
C
C           IF (LEN.LT.0 .OR. LEN.GT.38) THEN ! BOARD LEN WORST CASE SCENARIO
           IF (LEN.LT.0 .OR. LEN.GT.EUWB_MAXBLEN) THEN ! BOARD LEN EXCEEDS MAX SIZE FOR LOG RECORD !V05
           	TRABUF(TSTAT) = REJT
           	TRABUF(TERR)=SYNT
            SYNTERRCOD=80
           	TYPE*, 'INVALID BOARD LENGTH - LEN>',LEN,' TER>',TER
           ELSE
            CALL LIB$MOVC3(LEN, BPRO(BINPTAB+IND,BUF), TRABUF(TEUWBOARD))
           ENDIF           
C
C IF SYNTAX ERROR THEN PRINT ERROR CODE
C ON THE CONSOLE.
C
100     CONTINUE                                                                !V05

        IF(P(SUPSYN).EQ.0.AND.SYNTERRCOD.NE.0.AND.
     *      TRABUF(TERR).NE.NOTON) THEN
            MESS(2) = TEGEN
            MESS(3) = 10
            MESS(4) = SYNTERRCOD
CV05            MESS(5) = TER
            MESS(5) = TRABUF(TTER)
            MESS(6) = TRABUF(TGAMTYP)
            MESS(7) = TRABUF(TGAMIND)
CV05            MESS(8) = TRABUF(TSER)
            MESS(8) = 0                                                         !V05
            CALL QUEMES(MESS)
        ENDIF
C
        IF(SYNTERRCOD.NE.0) GOTO 90                                             !V05
C
C IF FUNCTION SUPRESS THEN SET ERROR MESSAGE
C           
CV05           IF ((P(SUPWAG) .NE. 0) .OR. (P(EUSPWAG) .NE. 0) )THEN
           IF ((P(SUPWAG)  .NE. 0) .OR. 
     *         (P(EUSPWAG) .NE. 0) .OR.
     *         (TSBIT(P(EUSPGWAG), EGNUM-1))                                    !V05 SUPRESSION BIT # STARTS AT ZERO (GAME NUMBER = 1 -> BIT # = 0)
     *        ) THEN
              TRABUF(TERR) = SUPR
              TRABUF(TSTAT) = REJT            
           ENDIF
C
C CHECK IF TERMINAL IS NOT SIGNED ON (V05)
C
          IF(AGTHTB(AOPSTS,TRABUF(TTER)).NE.SIGNON) THEN                        !V05
            TRABUF(TERR)  = NOTON                                               !V05
            TRABUF(TSTAT) = REJT                                                !V05
          ENDIF                                                                 !V05
        ENDIF
C
C CANCEL TYPE
C               
        IF (MTYPE .EQ. 2) THEN 
           TRABUF(TEUTYP) = TCAN
           
C
C GET JULIAN DATE
C
           TEMP1 = ZEXT(BPRO(BINPTAB+5,BUF))
           TEMP2 = ZEXT(BPRO(BINPTAB+6,BUF))
           TRABUF(TEUCWJUL) = ISHFT(TEMP1,8) + TEMP2
        
C
C GET SERIAL NUMBER 
C           
           TEMP1 = ZEXT(BPRO(BINPTAB+7,BUF))
           TEMP2 = ZEXT(BPRO(BINPTAB+8,BUF))
           TEMP3 = ZEXT(BPRO(BINPTAB+9,BUF))
           TRABUF(TEUCWSER) = ISHFT(TEMP1,16) + ISHFT(TEMP2,8) + TEMP3
C
C GET CHECK DIGITS
C
           TRABUF(TEUCWCKD) = ZEXT(BPRO(BINPTAB+10,BUF))
C
C IF FUNCTION SUPRESS THEN SET ERROR MESSAGE
C           
           IF ((P(SUPCAN) .NE. 0) .OR. (P(EUSPCAN) .NE. 0)) THEN
              TRABUF(TERR) = SUPR
              TRABUF(TSTAT) = REJT            
           ENDIF
C
C CHECK IF TERMINAL IS NOT SIGNED ON (V05)
C
          IF(AGTHTB(AOPSTS,TRABUF(TTER)).NE.SIGNON) THEN                        !V05
            TRABUF(TERR)  = NOTON                                               !V05
            TRABUF(TSTAT) = REJT                                                !V05
          ENDIF                                                                 !V05
        ENDIF
C
C VALIDATION TYPE
C        
        IF (MTYPE .EQ. 1) THEN 
           TRABUF(TEUTYP) = TVAL
C
C GET VALIDATION SUBTYPE
C
           TEMP1=ZEXT(BPRO(BINPTAB+1,BUF))
           TRABUF(TEUVSBT)=IAND(TEMP1,'0F'X)
C
C GET JULIAN DATE
C
           TEMP1 = ZEXT(BPRO(BINPTAB+5,BUF))
           TEMP2 = ZEXT(BPRO(BINPTAB+6,BUF))
           TRABUF(TEUVWJUL) = ISHFT(TEMP1,8) + TEMP2
        
C
C GET SERIAL NUMBER 
C           
           TEMP1 = ZEXT(BPRO(BINPTAB+7,BUF))
           TEMP2 = ZEXT(BPRO(BINPTAB+8,BUF))
           TEMP3 = ZEXT(BPRO(BINPTAB+9,BUF))
           TRABUF(TEUVWSER) = ISHFT(TEMP1,16) + ISHFT(TEMP2,8) + TEMP3
C
C GET CHECK DIGITS
C
           TRABUF(TEUVWCKD) = ZEXT(BPRO(BINPTAB+10,BUF))
C
C IF FUNCTION SUPRESS THEN SET ERROR MESSAGE
C           
           IF ((P(SUPVAL) .NE. 0) .OR. (P(EUSPVAL) .NE. 0)) THEN
              TRABUF(TERR) = SUPR
              TRABUF(TSTAT) = REJT            
           ENDIF
C
C CHECK IF TERMINAL IS NOT SIGNED ON (V05)
C
          IF(AGTHTB(AOPSTS,TRABUF(TTER)).NE.SIGNON) THEN                        !V05
            TRABUF(TERR)  = NOTON                                               !V05
            TRABUF(TSTAT) = REJT                                                !V05
          ENDIF                                                                 !V05

C----+------------------------------------------------------------------
C V03| New Validation Messages
C----+------------------------------------------------------------------
           ! Only set values if subtype is:
           IF(TRABUF(TEUVSBT).EQ.VNBNK) THEN ! New validation bank transfer accepted
              ! Setting Id Type: telephone number = 0, player card = 1
              TRABUF(TEUVPLIDTYP) = ZEXT(BPRO(BINPTAB + 11,BUF))
              
              ! Setting Player Card / Telephone Number
              TEMP1 = ZEXT(BPRO(BINPTAB + 12,BUF))
              TEMP2 = ZEXT(BPRO(BINPTAB + 13,BUF))
              TEMP3 = ZEXT(BPRO(BINPTAB + 14,BUF))
              TEMP4 = ZEXT(BPRO(BINPTAB + 15,BUF))
              TRABUF(TEUVPLCARD) = ISHFT(TEMP1,24) + ISHFT(TEMP2,16) +
     *                           ISHFT(TEMP3,8) + TEMP4
   
              ! Setting NIB: Bank Branch - 2 bytes
              TEMP1 = ZEXT(BPRO(BINPTAB + 16,BUF))
              TEMP2 = ZEXT(BPRO(BINPTAB + 17,BUF))
              TRABUF(TEUVNIBBB) = ISHFT(TEMP1,8) + TEMP2
   
              ! Setting NIB: Bank Office - 2 bytes
              TEMP1 = ZEXT(BPRO(BINPTAB + 18,BUF))
              TEMP2 = ZEXT(BPRO(BINPTAB + 19,BUF))
              TRABUF(TEUVNIBBO) = ISHFT(TEMP1,8) + TEMP2
   
              ! Setting NIB: Account Number - 5 bytes
              I1NIB(5) = ZEXT(BPRO(BINPTAB + 20,BUF))
              I1NIB(4) = ZEXT(BPRO(BINPTAB + 21,BUF))
              I1NIB(3) = ZEXT(BPRO(BINPTAB + 22,BUF))
              I1NIB(2) = ZEXT(BPRO(BINPTAB + 23,BUF))
              I1NIB(1) = ZEXT(BPRO(BINPTAB + 24,BUF))
              TRABUF(TEUVNIBBA1) = NIB/100
              TRABUF(TEUVNIBBA2) = MOD(NIB,100)
   
              ! Setting NIB: Check digits - 1 byte
              TRABUF(TEUVNIBCD) = ZEXT(BPRO(BINPTAB + 25,BUF))
           ENDIF
C----+------------------------------------------------------------------
C V03| New Validation Messages
C----+------------------------------------------------------------------
        ENDIF
C
C IF TERMINAL RETRY, AND TRANSACTION STATUS IS GOOD, AND
C TRANSACTION SEQUENCE NUMBER MATCHES THE LAST SEQUENCE
C NUMBER FOR THIS TERMINAL, CONTINUE RETRY PROCESSING, ELSE
C PROCESS AS NORMAL.
C
C        IF(HPRO(SIMMOD,BUF).EQ.-999) GOTO 80    !NO RETRIES FOR SIM
C
        IF(TRABUF(TSTAT) .EQ. GOOD .AND.
     *    TRABUF(TTRN) .EQ. ZEXT(AGTHTB(ATRNUM,TER))) THEN
          LSTSER=AGTTAB(ALSTRA,TER)
c          CALL FASTSET(0,LOGREC,LREC*3)
          CALL RLOG(LSTSER,LOGREC,TASK,STRTY)
	  IF(STRTY .NE. 0) THEN
	     CALL WAIT_APUQUE
	     CALL RLOG(LSTSER,LOGREC,TASK,STRTY)
	  ENDIF
          IF(STRTY .NE. 0) GOTO 80
          CALL FASTSET(0,WRKBUF,TRALEN)
          CALL LOGTRA(WRKBUF,LOGREC)
C
C CHECK FOR MATCHING HEADER INORMATION
C
          IF(WRKBUF(TTYP)   .NE. TRABUF(TTYP))    GOTO 80
          IF(WRKBUF(TEUTYP) .NE. TRABUF(TEUTYP))  GOTO 80          
          IF(WRKBUF(TTRN)   .NE. TRABUF(TTRN))    GOTO 80
CV06          IF(WRKBUF(TGAMTYP).NE. TRABUF(TGAMTYP)) GOTO 80
          IF(WRKBUF(TAGT)   .NE. TRABUF(TAGT))    GOTO 80
          IF(WRKBUF(TCHK)   .NE. TRABUF(TCHK))    GOTO 80
c          IF(WRKBUF(TSTAT)  .NE. TRABUF(TSTAT))      GOTO 80
C
C CHECK THAT ALL WAGER DETAIL MATCHES
C
          IF(TRABUF(TEUTYP) .EQ. TWAG) THEN
!             DO I=TEUWBOARD,120
             DO I=TEUWBOARD,TEUWBEND                                            !V05
                IF(WRKBUF(I) .NE. TRABUF(I)) GOTO 80
             ENDDO
             IF(WRKBUF(TGAMTYP) .NE. TRABUF(TGAMTYP))  GOTO 80                  !V06
             IF(WRKBUF(TGAMIND) .NE. TRABUF(TGAMIND))  GOTO 80                  !V06
             IF(WRKBUF(TEUWDUR) .NE. TRABUF(TEUWDUR))  GOTO 80
             IF(WRKBUF(TEUWNBET).NE. TRABUF(TEUWNBET)) GOTO 80
             IF(WRKBUF(TEUWNMK) .NE. TRABUF(TEUWNMK))  GOTO 80
             IF(WRKBUF(TEUWNST) .NE. TRABUF(TEUWNST))  GOTO 80
             IF(WRKBUF(TEUWDRWIND) .NE. TRABUF(TEUWDRWIND)) GOTO 80             !V05 FIX
             IF(WRKBUF(TEUW_KIWFL) .NE. TRABUF(TEUW_KIWFL)) GOTO 80             !V05
             IF(WRKBUF(TEUW_PLNIF) .NE. TRABUF(TEUW_PLNIF)) GOTO 80             !V05
             IF(WRKBUF(TEUW_EUWCH) .NE. TRABUF(TEUW_EUWCH)) GOTO 80             !V05
          ENDIF  
C
C
C CHECK THAT ALL CANCEL DETAIL MATCHES
C
          IF(TRABUF(TEUTYP) .EQ. TCAN) THEN
             IF(WRKBUF(TEUCWJUL) .NE. TRABUF(TEUCWJUL)) GOTO 80
             IF(WRKBUF(TEUCWSER) .NE. TRABUF(TEUCWSER)) GOTO 80
             IF(WRKBUF(TEUCWCKD) .NE. TRABUF(TEUCWCKD)) GOTO 80
          ENDIF  
C
C CHECK THAT ALL VALIDATIONS DETAIL MATCHES
C
          IF(TRABUF(TEUTYP) .EQ. TVAL) THEN
             IF(WRKBUF(TEUVWJUL) .NE. TRABUF(TEUVWJUL)) GOTO 80
             IF(WRKBUF(TEUVWSER) .NE. TRABUF(TEUVWSER)) GOTO 80
             IF(WRKBUF(TEUVWCKD) .NE. TRABUF(TEUVWCKD)) GOTO 80
          ENDIF  
          
50        CONTINUE
          TRABUF(TSTAT) = REJT
          TRABUF(TERR ) = RETY
C
C PUT ORIGINAL WAGER INTO PROCOM BUFFER FOR RETRY LOGIC IN OUTMGR
C
          CALL TRALOG(WRKBUF,PRO(WRKTAB,BUF))
          GOTO 90	
        ENDIF

80      CONTINUE
C
C IF NO CONNECTION TO EURO MILHOES THEN CREATE ONE ERROR MESSAGE 
C        
        IF (P(EUMILF) .EQ. 0) THEN
           TRABUF(TERR) = INVL
           TRABUF(TSTAT) = REJT
C----+------------------------------------------------------------------
C V04| Bugfix
C----+------------------------------------------------------------------
C           PRO(REMSTS,BUF) = RMDOWN
           HPRO(REMSTS,BUF) = RMDOWN
C----+------------------------------------------------------------------
C V04| Bugfix
C----+------------------------------------------------------------------
           HPRO(TRCODE,BUF) = TYPEUR 
        ENDIF 
           
C        CALL OPSTXT(' ******************* INMGR ****************')


c        CALL OPS('TRABUF(TEUTYP)',TRABUF(TEUTYP))
c        CALL OPS('VALOR de HPRO(INPLEN,BUF): ',HPRO(INPLEN,BUF),HPRO(INPLEN,BUF))  
        
c        CALL OPS('VALOR de BPRO(BINPTAB+5,BUF): ',BPRO(BINPTAB+5,BUF),BPRO(BINPTAB+5,BUF))  
c        DO I=0,48
c          TYPE 9998,I,BPRO(BINPTAB+I,BUF)   
c        ENDDO
C        TYPE 9999, (BPRO(INPTAB+I,BUF),I=0,10)
90      CONTINUE
C   
C PUT TRABUF INTO APUBUF FOR COMMGR GET IT AND SET TRANSACTION CODE TO TYPEUR (TRANSACTION EURO MIL)
C   
        CALL TRALOG(TRABUF,APUBUF(2,BUF))
        HPRO(TRCODE,BUF) = TYPEUR
C
C IF TRANSACTION IS ERROR THEN PUT IT ON DISPAT - TO SEND TO OUTMGR TO CREATE ERROR MESSAGE
C ELSE SEND TO COMMGR TO PUT MESSAGE INTO MESSAGEQ
C
        IF (TRABUF(TSTAT) .NE. GOOD) THEN
C           CALL OPSTXT(' ******************* INMGR PARA O COMMGR 2 ****************')
           CALL ABL(BUF,QUETAB(1,DIS),ST)
           CALL DQUTRA(TASK,BUF)
        ELSE
C           CALL OPSTXT(' ******************* INMGR PARA O COMMGR ****************')
           CALL QUETRA(EUC, BUF)
           CALL DQUTRA(TASK,BUF)
        ENDIF
        GOTO 20	
9998    FORMAT(' MESS: ',I2.1,' - ', Z3.2)
9999    FORMAT(' MES= ',<10>Z8)
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
        CALL STTSK(8HCOMMGR  ,TSKSTS,STATUS) !VERIFY IF COMMGR IS OK
        IF (STATUS .EQ. 4) THEN
           CALL OPSTXT('ERROR!!!!! COMMGR IS NOT RUNNING')
           CALL OPSTXT('STARTING COMMGR AND TRY TO PROCESS ALL TRANSACTIONS ')
           CALL START(TSKNAM(32)) ! COMMGR
        ENDIF
C
C IF OUTMGR DO NOT RUN THEN RESTART OUTMGR
C        
        CALL STTSK(8HOUTMGR  ,TSKSTS,STATUS)
        IF (STATUS .EQ. 4) THEN
           CALL OPSTXT('ERROR!!!!! OUTMGR IS NOT RUNNING')
           CALL OPSTXT('STARTING OUTMGR AND TRY TO PROCESS ALL TRANSACTIONS ')
           CALL START(TSKNAM(31)) !OUTMGR
        ENDIF
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

        
