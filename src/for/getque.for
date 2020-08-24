C
C SUBROUTINE GETQUE
C
C GETQUE.FOR
C
C V53 21-MAR-2016 SCML M16 PROJECT: set log record size of Euromillions wagers
C                      to 3 log records
C V52 06-MAR-2014 SCML Added support to PLACARD Project - IGS
C V51 04-DEC-2013 SCML Added handling for new accounting reports
C V50 06-JAN-2011 FJG  MILLENNIUM MXSRV
C V49 13-APR-2010 RXK  Added check of queue size for Passive
C V48 31-MAR-2010 RXK  Changes for ePassive
C V47 24-JUN-2009 FJG  Add a transaction hex dump to log in case of game not conf.
C V46 03-APR-2009 TRG  Get Joker when played as standalone
C V45 18-MAR-2009 TRG  WHEN ARRIVE A TRANSACTION FOR A GAME NOT CONFIGURED, SEND TO SPESRV
C V44 10-MAR-2009 MMO  KICKER ONLY GAME ARE SET TO 2 RECORDS (SERIAL OF EM)
C V43 07-MAY-2001 JHR/TONY/EPH  VALIDATIONS ARE SET TO 2 RECORDS (BECAUSE OF OP AMOUNT)
C V42 20-MAR-2001 UXN  TYPCMD size changed to 2 log records.
C V41 26-DEC-2000 CS   INCLUDED PASSIVE FOR PORTUGAL
C V40 29-NOV-2000 UXN  TTGL ADDED.
C V39 09-NOV-2000 UXN  GUI added.
C V38 28-SEP-2000 UXN  TYPAGTINF added.
C V37 13-OCT-1999 RXK  World Tour added.
C V36 14-MAY-1999 UXN  Super Triple added.
C V35 18-MAR-1999 RXK  Game type,game index in separate bytes.Removed hack of V5
C V34 29-JAN-1999 UXN  Fractions/unfractions redesigned.
C V33 05-DEC-1996 HXK  Updated for Finland IPS
C V32 21-MAY-1996 HXK  Addition of segmented signon (Rita)
C V31 23-NOV-1995 PXB  Couple and Double games added
C V30 04-AUG-1995 HXK  Batch fix for RAVI v5 install
C V29 17-JUL-1995 HXK  Various bug fixes, etc. for Ravi batch
C V28 26-JUN-1995 HXK  Changes for RAVI modification, mainly QPs, alternatives,
C                      screening and prognosis
C V27 29-MAY-1995 HXK  Popularity list: i) with QP; ii) without QP;
C                      also addition of prognosis maych 4 capability.
C V26 24-APR-1995 HXK  Merge of V5 development with March 10th 1995 bible
C V25 22-FEB-1995 HXK  HACK FOR V5
C V24 11-DEC-1994 HXK  Added Total Key Txn
C V23 01-NOV-1994 HXK  Made minor changes, call BNGSET to set Bingo seed
C V22 21-OCT-1994 HXK  Fixed typo bug
C V21 19-OCT-1994 HXK  Set Bingo record size = 2
C V20 02-OCT-1994 HXK  Added Bingo 
C V19 16-NOV-1993 GXA  Added Opinion Poll to SPESRV.
C V18 05-OCT-1993 GXA  Increased Order report size ti two logrecords.
C V17 10-SEP-1993 GXA  Validation transactions can be longer then one 
C                      logrecord with bank#'s.
C V16 04-SEP-1993 GXA  Commands take two logrecords in some cases.
C V15 23-AUG-1993 GXA  Changed for Toto Select. Could extend to two logrecords
C                      if bank info is used.
C V14 04-AUG-1993 GXA  Corrected Ravi Kicker# check (EQ to NE).
C V13 03-AUG-1993 GXA  Added Fractioning/Unfractioning.
C V12 16-JUL-1993 GXA  Fixed Kicker1 option check for Speden, changed EQ to NE.
C V11 15-JUL-1993 GXA  Added ST parameter on KIKSET calls.
C V10 15-JUL-1993 GXA  Corrected check for Type 3 msg's, changed OR to AND.
C V09 15-JUL-1993 GXA  Added Boundary checks when decoding GIND,GNUM ...., 
C                      the game specific decode routines will pick up the 
C                      syntax error and report it. Moved SON and SOFF to be 
C                      done out of SPESRV instead of SPERVF. SPESRV has the 
C                      correct SPECOM in order to do file access for clearks.
C V08 24-JUN-1993 GXA  Added RAVI (V65), Consolidated Speden with Lotto/Sports,
C                      Combined checking of some types for SPEF,
C                      Added Kicker generation from Central (KIKSET).
C V07 17-MAY-1993 HXK  Spede game has its own task, id =PPP.
C V06 17-MAY-1993 HJK  ADDED OPINION POLLS
C V05 11-MAY-1993 HXK  Added Speden game, Speden cancels and deletes.
C V04 19-AUG-1992 GCAN INCREASED TOTO SELECT TO TWO RECORDS, DUE TO KICKER.
C V03 01-MAY-1992 GCAN ADDED ODDS REQUESTS (PASSED TO UNSPRO).
C V02 01-NOV-1991 MTK  INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C
C SUBROUTINE TO DECODE APPLICATIONS TASK
C QUEUE AND TRANSACTION LOG REOCRD SIZE.
C
C CALLING SEQUENCE:
C     CALL GETQUE(BUF,QUE,SIZE,BYPASS)
C INPUT
C     BUF    - PROCOM BUFFER NUMBER
C OUTPUT
C     QUE    - APPLICATIONS TASK QUEUE NUMBER
C     SIZE   - NUMBER OF LOG RECORDS REQUIRED
C     BYPASS - TRANSACTION ALREADY HAS SERIAL NUMBER (SECOND PHASE)
C
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE GETQUE(BUF,QUE,SIZE,BYPASS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
	INCLUDE 'INCLIB:FRAC.DEF'
C----+------------------------------------------------------------------
C V52| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:IGSDEBUG.DEF'
C----+------------------------------------------------------------------
C V52| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
        INTEGER*4 BUF               !IN:  BUFFER NUMBER
        INTEGER*4 QUE               !OUT: QUEUE NUMBER
        INTEGER*4 SIZE              !OUT: # OF SERIAL #'S TO ASSIGN
        LOGICAL   BYPASS            !OUT: .TRUE. IF NO SRL #'S NEEDED
        LOGICAL   KICKER
        LOGICAL   MULTS
        LOGICAL   DELWAG                        !terminal deletion of a wager
C
        INTEGER*4 INTLEN, NUMBRDS, SUBTYP, TYPE, DEL
        INTEGER*4 CODE, TEMP
C
        INTEGER*4 OPTIONS                       !Game Option Bytes 
        INTEGER*4 GTYP                          !Game Type
        INTEGER*4 GNUM                          !Main Game Number.
        INTEGER*4 GIND                          !Main Game Index.
        INTEGER*4 KGNUM                         !Kicker Game Number.
        INTEGER*4 KGIND                         !Kicker Game Index.
        INTEGER*4 ST                            !Subroutine Return Status.
        INTEGER*4 NUMTKT                        !# Fractions.
        INTEGER*4 SYSBET                        !System bet
        INTEGER*4 IND                           !Temporary index
        INTEGER*4 OPREQ                         !ePassive Operation Request
        INTEGER*4 TIME
        INTEGER*4 LENGTH
        INTEGER*4 QUECOUNT
C
C OPTION FLAGS
C
        INTEGER*2 GCTR_OPT /Z8000/              !Game Control Option.
        INTEGER*2 GTXT_OPT /Z4000/              !Game Text  Option
        INTEGER*2 GTTX_OPT /Z2000/              !Game Ticket Text Option.
        INTEGER*2 KCTR_OPT /Z1000/              !Kicker Control option
        INTEGER*2 KTXT_OPT /Z0800/              !Kicker Text Option
        INTEGER*2 KTTX_OPT /Z0400/              !Kicker Ticket Text Option.
        INTEGER*2 QPFF_OPT /Z0200/              !Quick Pick / Board option.
        INTEGER*2 SYSN_OPT /Z0100/              !System Number option.
        INTEGER*2 KIK1_OPT /Z0020/              !Kicker 1 game requested.
        INTEGER*2 KIK2_OPT /Z0010/              !Kicker 2 game requested.
        INTEGER*2 BNKN_OPT /Z0008/              !Bank Number option.
C
        INTEGER*4 I4TEMP
        INTEGER*2 I2TEMP(2)
        BYTE      I1TEMP(4)
        EQUIVALENCE (I4TEMP, I2TEMP, I1TEMP)
        INTEGER*2 JULIAN_DATE
C
        GNUM  = 0
        GIND  = 0
        KGNUM = 0
        KGIND = 0
C
C
        BYPASS=.FALSE.
C
        SIZE = 0
        QUE  = 0
        CODE = HPRO(TRCODE,BUF)
        HPRO(NBRTRA,BUF) = 1
        HPRO(ENCOVR,BUF) = 0
C----+------------------------------------------------------------------
C V52| Added support to PLACARD Project - IGS - Financial Reports
C----+------------------------------------------------------------------
        IF(IGSDEBUG(IA_DISPAT)) THEN
            CALL OPS('GETQUE:CODE',CODE,CODE)
        ENDIF
C----+------------------------------------------------------------------
C V52| Added support to PLACARD Project - IGS - Financial Reports
C----+------------------------------------------------------------------
C
C Check for the most probable case first
C
        IF(CODE.EQ.TYPREG) GOTO 2000
C
C     CHECK FOR X2X TRANSACTION
C
        IF (CODE.EQ.TYPX2X_PRO) THEN
           QUE  = X2P
           SIZE = 1
           GOTO 9000
        ENDIF
C
        IF (CODE.EQ.TYPX2X_RELAY) THEN
           QUE  = X2R
           SIZE = 1
           GOTO 9000
        ENDIF
C
C CHECK FOR CHECKWRITER TRANSACTION
C
        IF (CODE.EQ.TYPCWT) THEN
           QUE  = SPE
           SIZE = 1
           GOTO 9000
        ENDIF
C
C CHECK FOR INTRASYSTEM SOLICITED TRANSACTIONS
C
        IF (CODE.EQ.TYPSSI) THEN
           QUE  = SPE
           SIZE = 1
           GOTO 9000
        ENDIF
C
C CHECK FOR INTRASYSTEM SOLICITED TRANSACTIONS
C
        IF (CODE.EQ.TYPSSU) THEN
           QUE  = SPE
           SIZE = 1
           GOTO 9000
        ENDIF
C
C CHECK FOR INTRASYSTEM SOLICITED TRANSACTIONS
C
        IF (CODE.EQ.TYPFPT) THEN
           QUE  = SPE
           SIZE = 1
           GOTO 9000
        ENDIF
C
C CHECK FOR CROSS SYSTEM
C
        IF(CODE.EQ.TYPCRS) THEN
          QUE  = INO
          SIZE = PRO(INPTAB,BUF)
          GOTO 9000
        ENDIF
C
C EURO MIL PROJECT - EURO MIL CONNECT
C
        IF(CODE.EQ.TYPEUR) THEN
          QUE  = EUO
          SIZE = 2
C----+---+-------------+------------------------------------------------
C V53|BEG| M16 PROJECT | WAGER OF TEUM GAME TYPE HAS SIZE 3
C----+---+-------------+------------------------------------------------
          TYPE   = ZEXT(BPRO(BINPTAB+1,BUF))
          SUBTYP = IAND(TYPE,15)
          TYPE   = ISHFT(TYPE,-4)
          GTYP   = ZEXT(BPRO(BINPTAB+5,BUF))
          IF(TYPE.EQ.0 .AND. GTYP.EQ.TEUM) SIZE = 3
C----+---+-------------+------------------------------------------------
C V53|END| M16 PROJECT | WAGER OF TEUM GAME TYPE HAS SIZE 3
C----+---+-------------+------------------------------------------------
          GOTO 9000
        ENDIF
C----+------------------------------------------------------------------
C V52| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF(CODE .EQ. TYPIGS) THEN
          QUE  = IGO
          SIZE = 2
          GOTO 9000
        ENDIF
C----+------------------------------------------------------------------
C V52| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
C CHECK FOR INTERNAL CANCELLATION
C
        IF(CODE.EQ.TYPDEL) THEN
          QUE  = INC
          SIZE = 3
          PRO(INPTAB,BUF)=PRO(SERIAL,BUF)   !SAVE SERIAL # IN INPTAB
          GOTO 9000
        ENDIF
C
C CHECK FOR COMMAND
C
        IF(CODE.EQ.TYPCMD) THEN
          QUE  = CMD
          SIZE = 2
          GOTO 9000
        ENDIF
C
C CHECK FOR ERRLOG MESSAGE
C
        IF(CODE.EQ.TYPERR) THEN
          QUE  = ERR
          SIZE = 1
          GOTO 9000
        ENDIF
C
C CHECK FOR FRACTION WAGER (2ND PHASE)
C
        IF(CODE.EQ.TYPFRA) THEN
           QUE  = PRO(FRA_QUE,BUF)
           SIZE = PRO(FRA_SIZ,BUF)
	   IF(QUE.EQ.SPE.OR.SIZE.EQ.0) THEN ! IF ERROR OR RETRY THEN SERIAL
		BYPASS = .TRUE.		    ! NUMBERS ALREADY ASSIGNED
		GOTO 9000
	   ENDIF
           NUMTKT = PRO(FRA_TKT,BUF)
           HPRO(NBRTRA,BUF) = NUMTKT
           GOTO 9000
        ENDIF
C
C CHECK FOR UNFRACTION WAGER (2ND PHASE)
C
        IF(CODE.EQ.TYPUNF) THEN
           QUE = PRO(FRA_QUE,BUF)
	   BYPASS = .TRUE.
           GOTO 9000
        ENDIF
C
C
C CHECK FOR EXCHANGE TICKET
C
        IF(CODE.EQ.TYPECH) THEN
          QUE  = VAL
          SIZE = 3
          GOTO 9000
        ENDIF
C                                     !MXSRV
C CHECK FOR MX LOG TRANSACTIONS       !MXSRV
C                                     !MXSRV
        IF(CODE.EQ.TYPMXL) THEN       !MXSRV
          QUE  = SPE                  !MXSRV
          SIZE = 3                    !MXSRV
          GOTO 9000                   !MXSRV
        ENDIF                         !MXSRV           
C
C CHECK FOR GUI
C
        IF (CODE.EQ.TYPGUI) THEN
          QUE  = GUI
          SIZE = HPRO(INPLEN, BUF) / (LREC*4)
          GOTO 9000
        ENDIF
C
C CHECK FOR LOTTO/SPORTS CANCELLATION (SECOND PHASE)
C
        IF(CODE.EQ.TYPWCN.OR.CODE.EQ.TYPWDL) THEN
          QUE  = WAG
          BYPASS=.TRUE.                 !NO SIZE SET HERE
          GOTO 9000
        ENDIF
C
C CHECK FOR EPASSIVE CANCELLATION (SECOND PHASE)
C
        IF(CODE.EQ.TYPEPC.OR.CODE.EQ.TYPEPD) THEN
          QUE  = PST
          BYPASS=.TRUE.                 !NO SIZE SET HERE
          GOTO 9000
        ENDIF
C
C CHECK FOR NUMBERS CANCELLATION (SECOND PHASE)
C
        IF(CODE.EQ.TYPNCN.OR.CODE.EQ.TYPNDL) THEN
          QUE  = NBR
          BYPASS=.TRUE.                 !NO SIZE SET HERE
          GOTO 9000
        ENDIF
C
C CHECK FOR ODDSET CANCELLATION (SECOND PHASE)
C
        IF(CODE.EQ.TYPOCN.OR.CODE.EQ.TYPODL) THEN
          QUE  = ODD
          BYPASS=.TRUE.                 !NO SIZE SET HERE
          GOTO 9000
        ENDIF
C
C CHECK FOR SPEDEN CANCELLATION (SECOND PHASE)
C
C       IF(CODE.EQ.TYPPCN.OR.CODE.EQ.TYPPDL) THEN                        !MXSRV
C         QUE=PPP                       !PPP = SPEDPRO = SPEDEN task     !MXSRV
C         BYPASS=.TRUE.                 !NO SIZE SET HERE                !MXSRV
C         GOTO 9000                                                      !MXSRV
C       ENDIF                                                            !MXSRV
C
C CHECK FOR TERMINAL ERROR TRANSACTION
C
        IF(CODE.EQ.TYPNON)THEN
          QUE  = SPEF
          SIZE = 1
          GOTO 9000
        ENDIF
C
C CHECK FOR HASF TRANSACTION
C
        IF(CODE.EQ.TYPHSF) THEN
          QUE  = SPE
          SIZE = 1
          GOTO 9000
        ENDIF
C
C CHECK FOR ONLINE AGENT UPDATE
C
        IF(CODE.EQ.TYPAGTINF) THEN
          QUE  = SPE
          SIZE = 3
          GOTO 9000
        ENDIF
C
C
C **** IF WE GET HERE WITHOUT A MATCH, IT MEANS WE HAVE A BAD TRCODE.
C      TYPE IT OUT AND TREAT AS A SPECIAL SERVICE XACTION
C
        TYPE *,'BAD TRCODE RECEIVED IN GETQUE = ',CODE
        QUE  = SPE
        SIZE = 1
        GOTO 9000
C
C
C **** COME HERE WHEN TRCODE = TYPREG (I.E., FROM COMMUNICATIONS)
C
2000    CONTINUE
C
C CHECK IF TERMINAL GENERATED DELETION
CRXK This check moved to wager section
C
CRXK    DEL = ZEXT(BPRO(BINPTAB,BUF))
CRXK    IF(IAND(DEL,'80'X).NE.0) THEN
CRXK      QUE  = INC
CRXK      SIZE = 3
CRXK      GOTO 9000
CRXK    ENDIF
C
C DETERMINE APPROPIATE PROCESSING TASK FROM INPUT MESSAGE
C
        TYPE = ZEXT(BPRO(BINPTAB+1,BUF))
        SUBTYP = IAND(TYPE,15)
        TYPE = ISHFT(TYPE,-4)
C----+------------------------------------------------------------------
C V52| Added support to PLACARD Project - IGS - Financial Reports
C----+------------------------------------------------------------------
        IF(IGSDEBUG(IA_DISPAT)) THEN
            CALL OPSTXT('GETQUE:CODE .EQ. TYPREG')
            CALL OPS('GETQUE:TYPE',TYPE,TYPE)
            CALL OPS('GETQUE:SUBTYP',SUBTYP,SUBTYP)
        ENDIF
C----+------------------------------------------------------------------
C V52| Added support to PLACARD Project - IGS - Financial Reports
C----+------------------------------------------------------------------
C
C
C CHECK TO SEE IF TRANSACTION IS A WAGER.  IF SO, GO DO IT
C
        IF(TYPE.EQ.0) GOTO 3000
C
C
C **** COME HERE IF WE HAVE A NORMAL TRANSACTION (TYPREG) WHICH IS NOT A
C      DELETE AND NOT A WAGER
C
C----+------------------------------------------------------------------
C V52| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF(TYPE .EQ. 14 .AND. (SUBTYP .GE. 0 .AND. SUBTYP .LE. 4)) THEN
C         SUBTYP = 0 -> IGS WAGER
C         SUBTYP = 1 -> IGS CANCELLATION
C         SUBTYP = 2 -> IGS VALIDATION
C         SUBTYP = 3 -> IGS PAYMENT
C         SUBTYP = 4 -> IGS REPORT
          QUE    = IGI         ! Send to INIGS Queue 
          BYPASS = .TRUE.
          GOTO 9000
        ENDIF
C----+------------------------------------------------------------------
C V52| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
C PROJECT EURO MIL - SEND TO EURO MILHOES if Julian date > 500
C
C
        I1TEMP(2) = BPRO(BINPTAB+5,BUF)
        I1TEMP(1) = BPRO(BINPTAB+6,BUF)
        
        JULIAN_DATE = ZEXT(I2TEMP(1))
        
        IF ((JULIAN_DATE .GT. 499) .AND. ((TYPE .EQ. 1) .OR. (TYPE .EQ. 2))) THEN
          QUE = EUI
          BYPASS= .TRUE.
          GOTO 9000
        ENDIF
        
        IF(TYPE.EQ.1)THEN                       !VALIDATION
          QUE  = VAL
          SIZE = 1
          IF(SUBTYP.EQ.VPTB) SIZE = 2           !INCLUDES BANK# TO LOG
          SIZE = 2                              !SIZE IS ALWAYS 2 HERE, 
                                                !BECAUSE I USE SECOND TMF REC TO WRITE OPS AMOUNT  !V43
          GOTO 9000
        ENDIF
C
        IF(TYPE.EQ.2)THEN                       !CANCELLATION
          QUE  = CAN                    
          SIZE = 3
          GOTO 9000
        ENDIF
C
        IF(TYPE.EQ.4)THEN                !PASSIVE VALIDATION
C                                        !OR RETURN OF TICKETS
          SIZE    = 1
          PRO(WRKTAB,BUF) = 0
C
C DETERMINE PROCESSING TASK. IF RETURN THEN CHECK SIZE OF THE QUEUE
C
          IF(SUBTYP.EQ.6) THEN
            SIZE    = 2
            QUE = PST                    !PASPRO (RETURNED TICKETS)
            CALL LISTSIZE(QUETAB(1,PST),QUECOUNT)
            IF(P(PASTHRO).GT.0.AND.QUECOUNT.GT.P(PASTHRO)) THEN
               PRO(WRKTAB,BUF) = 1
               GOTO 9000
            ENDIF 
          ELSE
            QUE = PSV                    !PASVAL (VALIDATIONS)
          ENDIF
C
C GET NUMBER OF TICKETS TO VALIDATE
C
          NUMTKT = ZEXT(BPRO(BINPTAB+10,BUF))
C
C DETERMINE TRANSACTION SIZE ACCORDING TO
C NUMBER OF TICKETS ON IT
C
          IF(NUMTKT.GT.2) SIZE=2
          IF(NUMTKT.GT.6) SIZE=3

          IF(SUBTYP.EQ.VPNREG) SIZE=2
          IF(SUBTYP.EQ.VPNDON) SIZE=2
          IF(SUBTYP.EQ.VPNBNK) SIZE=2
          
          GOTO 9000
        ENDIF
C
C ALL TYPE 3 MESSAGES, EXCEPT SON AND SOFF
C
C STATISTICS,           FAULTS,         TICKET MESSAGE REQUESTS
C CONTROL MESSAGES,     TEXT MESSAGES,  
C
        IF(TYPE.EQ.3.AND.(SUBTYP.NE.0.AND.SUBTYP.NE.15)) THEN
           QUE = SPEF                      
           SIZE = 2
           IF(SUBTYP.EQ.6) SIZE = 1
           GOTO 9000
        ENDIF
C
        IF(TYPE.EQ.3.AND.SUBTYP.EQ.0) THEN              !SON
           QUE = SPE
           SIZE = 2
           GOTO 9000
        ENDIF
C
        IF(TYPE.EQ.5)THEN                               !DOWNLOAD
           QUE = SPEF
           SIZE = 1
           GOTO 9000
        ENDIF
C
        IF(TYPE.EQ.6) THEN
           IF(SUBTYP.EQ.8) THEN                         !PASIIVE ACCOUNTING
              QUE = SPE
              SIZE = 1
              GOTO 9000
           ENDIF
        ENDIF 
C
        IF(TYPE.EQ.7) THEN                              !ENCRYPTION KEY REQUEST
           QUE = SPEF
           SIZE = 1
           GOTO 9000
        ENDIF
C----+------------------------------------------------------------------
C V52| Added support to PLACARD Project - IGS - REPRINTS
C----+------------------------------------------------------------------
        IF (TYPE .EQ. 8) THEN
           QUE = SPE
           BYPASS = .TRUE.
           GOTO 9000
        ENDIF
C----+------------------------------------------------------------------
C V52| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------

        IF(TYPE.EQ.10.AND.SUBTYP.EQ.13) THEN            !LOOPBACK
          QUE = SPEF
          SIZE = 1
          GOTO 9000
        ENDIF
C
        IF(TYPE.EQ.11.AND.SUBTYP.EQ.0) THEN             !ODDS REQUEST MESSAGE
          QUE = UNS
          SIZE = 1
          GOTO 9000
        ENDIF
C
CRXK    IF(TYPE.EQ.12) THEN                     !INSTANT
C
        IF(TYPE.EQ.12.AND.SUBTYP.EQ.1) THEN     !SUPPLY ORDER
          QUE = SPEF
          SIZE = 2
          GOTO 9000
        ENDIF
C
        IF(TYPE.EQ.12.AND.SUBTYP.EQ.3) THEN     !INSTANT/SUPPLY,REPR.SIGNON
          QUE = SPE
          SIZE = 1
          GOTO 9000
        ENDIF
C
        IF(TYPE.EQ.12.AND.SUBTYP.EQ.4) THEN     !INSTANT/GVT PASS # MANAGEMENT
          QUE = SPE
          SIZE = 1
          GOTO 9000
        ENDIF
C
        IF(TYPE.EQ.12.AND.SUBTYP.EQ.14) THEN            !TOTAL KEY
          QUE = SPEF
          SIZE = 1
          GOTO 9000
        ENDIF
C
        IF(TYPE.EQ.12.AND.SUBTYP.EQ.15) THEN            !Opinion Poll
           QUE = SPE
           SIZE = 2
           CALL KIKSET(PRO(1,BUF),1,ST)                 !Kicker Index One 
           GOTO 9000                                    !Hardcode for OPNPOLL !
        ENDIF
C
        IF(TYPE.EQ.13) THEN                     !INSTANT
          QUE = INI
          BYPASS= .TRUE.
          GOTO 9000
        ENDIF
C
        IF(TYPE.EQ.15.AND.SUBTYP.EQ.1) THEN    !FRACTION
           QUE = SPE
           SIZE = 2
           GOTO 9000
        ENDIF
C
C EURO MIL PROJECT - TYPE 14 
C        IF(TYPE.EQ.14) THEN                     !EURO MIL
C          QUE = EUI
C          BYPASS= .TRUE.
C          GOTO 9000
C        ENDIF
C
C EURO MIL PROJECT - IF TRANSACTION IS ONE REPORT OR GAME RESULTS OF EURO MIL THEN BYPASS
C
        IF (TYPE .EQ. 6) THEN
C----+------------------------------------------------------------------
C V51| Added handling for new accounting reports
C----+------------------------------------------------------------------
           IF ( SUBTYP .EQ. 10  ! 0x6A messages - new accounting report 
     *     ) THEN
              QUE = SPE
C             SIZE = 2
              BYPASS= .TRUE.
              GOTO 9000
           ENDIF
C----+------------------------------------------------------------------
C V51| Added handling for new accounting reports
C----+------------------------------------------------------------------
           IF (SUBTYP .EQ. 9) THEN
              QUE = SPE
              BYPASS= .TRUE.
              GOTO 9000
           ENDIF
           IF (SUBTYP .EQ. 3) THEN
              QUE = SPE
              BYPASS= .TRUE.
              GOTO 9000
           ENDIF
           IF (SUBTYP .EQ. 1) THEN
              GTYP  = ZEXT(BPRO(BINPTAB+4,BUF))
CV53              IF (GTYP .EQ. TEUM) THEN
              IF (GTYP .EQ. TEUM .OR. GTYP .EQ. TRAF) THEN                      !V53
                QUE = SPE
                BYPASS= .TRUE.
                GOTO 9000              
              ENDIF
           ENDIF 
        ENDIF 

        IF(TYPE.EQ.15.AND.SUBTYP.EQ.15) THEN   !UNFRACTION
           QUE = SPE
           SIZE = 2
           GOTO 9000
        ENDIF
C
        QUE  = SPE                                      !ALL OTHERS GO TO SPESRV
        SIZE = 1
        GOTO 9000
C
C
C **** COME HERE IF TRANSACTION IS A WAGER
CRXK   OR TERMINAL DELETION OF A WAGER
C
3000    CONTINUE
C
        SUBTYP = ZEXT(BPRO(BINPTAB+5,BUF))     ! game type
C
C EURO MIL PROJECT - GAME TYPE 17 (TEUM)
C It's BYPASS - First time... do not send to others systems
C 
        IF(SUBTYP .EQ. TEUM) THEN                     !EURO MIL
          QUE = EUI
          BYPASS= .TRUE.
          GOTO 9000
        ENDIF
C
        DEL = ZEXT(BPRO(BINPTAB,BUF))
        DELWAG = .FALSE.
        IF(IAND(DEL,'80'X).NE.0) DELWAG = .TRUE.
C
C V45-V47
C IF GAME IS NOT CONFIGURED, GET OUT FROM HERE
C
        IF(SUBTYP .LT. 0 .OR. SUBTYP .GT. MAXTYP) THEN
         QUE  = SPE        !ALL UNKNOWN TO SPESRV
         SIZE = 1
         IF(P(SYSTYP).EQ.SPRSYS) TYPE '(16(8(1X,Z8.8)/))',(PRO(ind,BUF),ind=1,128)
         GOTO 9000        	 
        ENDIF	

        DO TEMP=1, MAXIND
         IF(GTNTAB(SUBTYP,TEMP) .NE. 0) GOTO 3100	
        ENDDO	
C
C IF WE ARE HERE MEANS WE DIDN'T FOUND ANY GAME CONFIGURED FOR THIS
C        
        QUE  = SPE        !ALL UNKNOWN TO SPESRV
        SIZE = 1
        IF(P(SYSTYP).EQ.SPRSYS) TYPE '(16(8(1X,Z8.8)/))',(PRO(ind,BUF),ind=1,128)
        GOTO 9000        	 
C
3100    CONTINUE                
C END V45-V47
C
C
C KICKER ONLY GAME
C
        IF(SUBTYP .EQ. TKIK) THEN
           QUE = WAG
           IF(DELWAG) THEN
              QUE = INC
           ENDIF           
c           TEMP = ZEXT(BPRO(BINPTAB+7,BUF))
c           NUMBRDS=IAND(TEMP,15)
c           TEMP = ZEXT(BPRO(BINPTAB+6,BUF))
c           SYSBET = IAND(TEMP,15)
C           IF(NUMBRDS.GT.2 .OR. SYSBET.NE.NOSYS) SIZE = 2
C
C V46
           KGIND = ZEXT(BPRO(BINPTAB+6,BUF))
           KGIND = ISHFT(KGIND,-4)
           OPTIONS = ZEXT(BPRO(BINPTAB+8,BUF))
           OPTIONS = ISHFT(OPTIONS,8) + ZEXT(BPRO(BINPTAB+9,BUF))
C
C IF KICKER IS REQUESTED BY THE TERMINAL , GENERATE KICKER NUMBER(S)
C FROM SEED AND ATTACH TO END OF MESSAGE.
C
           IF(IAND(OPTIONS,KIK1_OPT).NE.0) THEN
             CALL KIKSET(PRO(1,BUF),KGIND,ST)
           ENDIF
C
           IF(IAND(OPTIONS,KIK2_OPT).NE.0) THEN
             CALL KIKSET(PRO(1,BUF),KGIND,ST)
           ENDIF
C End V46
           SIZE = 2
           GOTO 9000
        ENDIF
C
C LOTTO/SPORTS GAMES
C
        IF(SUBTYP.EQ.TLTO.OR.SUBTYP.EQ.TSPT.OR.SUBTYP.EQ.TBNG.OR.
     *     SUBTYP.EQ.TTGL) THEN
           IF(DELWAG) THEN
              QUE = INC
           ELSE
              QUE = WAG
C             IF(SUBTYP.EQ.TBNG) QUE = PPP      ! MXSRV
           ENDIF
C
           SIZE = 2
           TEMP = ZEXT(BPRO(BINPTAB+7,BUF))
           NUMBRDS=IAND(TEMP,15)
           GIND = ZEXT(BPRO(BINPTAB+6,BUF))
           GIND = ISHFT(GIND,-4)
           IF(GIND.GT.0.AND.GIND.LE.MAXIND) GNUM = GTNTAB(SUBTYP,GIND)
           IF(GNUM.GT.0.AND.GNUM.LE.MAXGAM) KGNUM= KGNTAB(GNUM)
           IF(KGNUM.GT.0.AND.KGNUM.LE.MAXGAM) KGIND = GNTTAB(GAMIDX,KGNUM)
           OPTIONS = ZEXT(BPRO(BINPTAB+8,BUF))
           OPTIONS = ISHFT(OPTIONS,8) + ZEXT(BPRO(BINPTAB+9,BUF))
           INTLEN = HPRO(INPLEN,BUF)-9
           KICKER=.FALSE.
           MULTS=.FALSE.
           IF(IAND(OPTIONS,GCTR_OPT).NE.0) INTLEN=INTLEN-2  !Game Control Rev
           IF(IAND(OPTIONS,GTXT_OPT).NE.0) INTLEN=INTLEN-2  !Game Text Rev
           IF(IAND(OPTIONS,GTTX_OPT).NE.0) INTLEN=INTLEN-2  !Game TKT Text Rev
           IF(IAND(OPTIONS,KCTR_OPT).NE.0) INTLEN=INTLEN-2  !Kicker Control Rev
           IF(IAND(OPTIONS,KTXT_OPT).NE.0) INTLEN=INTLEN-2  !Kicker Text Rev
           IF(IAND(OPTIONS,KTTX_OPT).NE.0) INTLEN=INTLEN-2  !Kicker TKT TextRev
           IF(IAND(OPTIONS,QPFF_OPT).NE.0) INTLEN=INTLEN-2  !Quick Pick 
           IF(IAND(OPTIONS,SYSN_OPT).NE.0) INTLEN=INTLEN-2  !System Bet OPTION
           IF(IAND(OPTIONS,BNKN_OPT).NE.0) INTLEN=INTLEN-8  !Bank #
C
C IF KICKER IS REQUESTED BY THE TERMINAL , GENERATE KICKER NUMBER(S)
C FROM SEED AND ATTACH TO END OF MESSAGE.
C
           IF(IAND(OPTIONS,KIK1_OPT).NE.0) THEN
              CALL KIKSET(PRO(1,BUF),KGIND,ST)
              IF(P(SYSTYP).NE.LIVSYS.AND.ST.EQ.0) INTLEN = INTLEN - 3
           ENDIF
C
           IF(IAND(OPTIONS,KIK2_OPT).NE.0) THEN
              CALL KIKSET(PRO(1,BUF),KGIND,ST)
              IF(P(SYSTYP).NE.LIVSYS.AND.ST.EQ.0) INTLEN = INTLEN - 3
           ENDIF
C
C IF NO KICKER IS REQUESTED WE CAN SAVE SOME SPACE IN THE LOG RECORD
C IN THIS CASE WE CAN SAVE SAPCE FOR TWO KICKER #'S. (4 WORDS)
C
           IF(IAND(OPTIONS,KIK1_OPT).EQ.0.AND. 
     *        IAND(OPTIONS,KIK2_OPT).EQ.0) THEN
              KICKER = .FALSE.
           ELSE
              KICKER = .TRUE.
           ENDIF
C
C IF BINGO WAGER, GET BINGO SEED. 
C IF BINGO CANCELLATION THEN DON'T GET BINGO SEED BUT INCREASE MESSAGE LENGTH 
C
           IF(SUBTYP.EQ.TBNG) THEN
              IF(QUE.NE.INC) THEN
                 CALL BNGSET(PRO(1,BUF),GIND,ST)
                 IF(P(SYSTYP).NE.LIVSYS.AND.ST.EQ.0) INTLEN = INTLEN - 4
              ELSEIF(P(SYSTYP).EQ.LIVSYS) THEN
                 HPRO(INPLEN,BUF) = HPRO(INPLEN,BUF) + 4
              ENDIF
           ENDIF
C
           IF(KICKER.AND.INTLEN.GT.44) SIZE = 3
           IF(SUBTYP.EQ.TSPT.AND.INTLEN.GT.50) SIZE = 3
           IF(INTLEN.GT.60) SIZE = 3    
C
           GOTO 9000
        ENDIF
C
C EPASSIVE
C
        IF(SUBTYP.EQ.TPAS) THEN
           PRO(WRKTAB,BUF) = 0
           TEMP = ZEXT(BPRO(BINPTAB+6,BUF))
           OPREQ = IAND(TEMP,15)
           QUE = PST
           IF(DELWAG) QUE = INC
           IF(OPREQ.EQ.EPASRES) THEN
              IF(P(SYSTYP).EQ.LIVSYS) THEN
                 CALL GETTIM(TIME)
                 LENGTH = HPRO(INPLEN,BUF)
                 HPRO(INPLEN,BUF) = HPRO(INPLEN,BUF) + 4
                 CALL MOVBYT(TIME,1,BPRO(BINPTAB,BUF),LENGTH+1,4)
              ENDIF
           ENDIF 
           IF(OPREQ.EQ.EPASSAL) THEN
              SIZE = 1
           ELSEIF(OPREQ.EQ.EPASREL) THEN
              SIZE = 2
           ELSE
              SIZE = 3      
           ENDIF    
           GOTO 9000  
        ENDIF  
C
C NUMBERS GAMES
C
        IF(SUBTYP.EQ.TNBR) THEN
          TEMP = ZEXT(BPRO(BINPTAB+7,BUF))
          NUMBRDS=IAND(TEMP,15)
          QUE  = NBR
          IF(DELWAG) QUE = INC
          SIZE = 1
          IF(NUMBRDS.GT.3) SIZE=2
          GOTO 9000
        ENDIF
C
C SCORE GAMES
C
        IF(SUBTYP.EQ.TSCR) THEN
          TEMP = ZEXT(BPRO(BINPTAB+7,BUF))
          NUMBRDS = IAND(TEMP,15)
          QUE = ODD
          IF(DELWAG) QUE = INC
          SIZE = 1
          IF(NUMBRDS.GT.3) SIZE = 2
          GOTO 9000
        ENDIF
C
C WINNERS TIP GAMES
C
        IF(SUBTYP.EQ.TWIT) THEN
          TEMP = ZEXT(BPRO(BINPTAB+7,BUF))
          NUMBRDS = IAND(TEMP,15)
          QUE = ODD
          IF(DELWAG) QUE = INC
          SIZE = 1
          IF(NUMBRDS.GT.3) SIZE = 2
          GOTO 9000
        ENDIF
C
C TOTO SELECT GAMES
C
        IF(SUBTYP.EQ.TTSL) THEN
           TEMP = ZEXT(BPRO(BINPTAB+7,BUF))
           NUMBRDS = IAND(TEMP,15)
           OPTIONS = ZEXT(BPRO(BINPTAB+8,BUF))
           OPTIONS = ISHFT(OPTIONS,8) + ZEXT(BPRO(BINPTAB+9,BUF))
           QUE = ODD
           SIZE = 1
           IF(NUMBRDS.GT.1.OR.IAND(OPTIONS,BNKN_OPT).NE.0) SIZE = 2
           GOTO 9000
        ENDIF
C
C SUPER DOUBLE GAMES
C
        IF(SUBTYP.EQ.TDBL) THEN
           TEMP = ZEXT(BPRO(BINPTAB+7,BUF))
           NUMBRDS = IAND(TEMP,15)
           QUE = ODD
           IF(DELWAG) QUE = INC
           SIZE = 1
           IF(NUMBRDS.GT.3) SIZE = 2
           GOTO 9000
        ENDIF
C
C TODAYS COUPLE GAMES
C
        IF(SUBTYP.EQ.TCPL) THEN
           TEMP = ZEXT(BPRO(BINPTAB+7,BUF))
           NUMBRDS = IAND(TEMP,15)
           QUE = ODD
           IF(DELWAG) QUE = INC
           SIZE = 1
           IF(NUMBRDS.GT.3) SIZE = 2
           GOTO 9000
        ENDIF
C
C SUPERSCORE GAMES
C
        IF(SUBTYP.EQ.TSSC) THEN
           TEMP = ZEXT(BPRO(BINPTAB+6,BUF))
           SYSBET = IAND(TEMP,15)
           QUE = ODD
           IF(DELWAG) QUE = INC
           SIZE = 1
           IF(SYSBET.NE.NOSYS) SIZE = 2
           GOTO 9000
        ENDIF
C
C TODAY'S TRIO GAMES
C
        IF(SUBTYP.EQ.TTRP) THEN
           TEMP = ZEXT(BPRO(BINPTAB+6,BUF))
           SYSBET = IAND(TEMP,15)
           QUE = ODD
           IF(DELWAG) QUE = INC
           SIZE = 1
           IF(SYSBET.NE.NOSYS) SIZE = 2
           GOTO 9000
        ENDIF
C
C SUPER TRIPLE GAMES
C
        IF(SUBTYP.EQ.TSTR) THEN
           TEMP = ZEXT(BPRO(BINPTAB+6,BUF))
           SYSBET = IAND(TEMP,15)
           QUE = ODD
           IF(DELWAG) QUE = INC
           SIZE = 1
           IF(SYSBET.NE.NOSYS) SIZE = 2
           GOTO 9000
        ENDIF
C
C ALL OTHER GAME TYPES, SEND TO SPESRV
C
        QUE  = SPE
        SIZE = 1
        GOTO 9000
C
C
C **** COME HERE FOR ALL TRANSACTIONS WITH QUE, SIZE, AND BYPASS SET
C
C
9000    CONTINUE
        IF(.NOT.BYPASS)THEN
          IF(SIZE.EQ.0)THEN
            TYPE *,'GETQUE LOGIC ERROR - BAD SIZE',CODE,TYPE,SUBTYP
            SIZE = 1
          ENDIF
          HPRO(NUMLRC,BUF)=SIZE
        ENDIF
        IF(QUE.EQ.0)THEN
          TYPE *,'GETQUE LOGIC ERROR - NO TASK QUE',CODE,TYPE,SUBTYP
          QUE = SPE
        ENDIF
        RETURN
        END
