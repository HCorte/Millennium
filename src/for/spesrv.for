C
C PROGRAM SPESRV
C  
C SPESRV.FOR
C
C V35 12-DEZ-2020 New Terminals Project - Olimpo
C V34 09-MAR-2016 SCML M16 PROJECT:
C                       Added new game report for SM and SoM games
C                       Loading of Euromillions configuration commented out (the
C                       logic was moved to RESET program)
C                       Global parameter SPBLRP replaced by EUSPBIR
C V33 27-MAR-2014 SCML Added support to PLACARD Project - IGS
C V32 05-FEV-2013 SCML Call ABL to queue to Dispatcher instead of QUETRA
C V31 24-SEP-2013 SCML New accounting platform
C                      SENDOUTMGR replaced by SENDDISPAT
C V30 26-AUG-2013 FJG  CR15 SPORTS NEW TEAMS NAME REPORTS
C V29 18-MAY-2011 RXK  Processing of TPASAC and TPASAD fixed.
C V28 28-JAN-2011 RXK  FDB fixed, GPAUSE replaced with message to OPS
C V27 14-OCT-2010 MAC  SALINV_PAS_OLD ADDED
C V26 19-APR-2010 RXK  SALINV_PAS added for Passive Accounting report
C V25 05-FEB-2001 CS   INCLUDED GETAGTNAM ROUTINE
C V24 05-OCT-2000 UXN  AlphaIPS release. TCHKWRT, TISSACK, TISUBAD added.
C V23 22-SEP-2000 UXN  UPDAGTINF() added.
C V22 29-JAN-1999 UXN  Fractions/unfractions redesigned.
C V21 27-FEB-1997 RXK  Temporarily removed INVMIS,PETCASH are back
C V20 13-FEB-1997 WPW  Logging financial pass-through trans added.
C V19 01-FEB-1997 HXK  Check for valid terminal number
C V18 18-JAN-1997 RXK  By mistake removed call to SALINV is back
C V17 05-DEC-1996 HXK  Updated for Finland IPS
C V16 19-NOV-1993 GXA  Added call to UPDMIS for Opinion Poll processing.
C V15 14-SEP-1993 GXA  Applied correction from Ireland for rejected transaction
C                      handling,GETCCITT called with PRO(OUTTAB,BUF) 
C                      NOT OUTTAB!)
C V14 01-SEP-1993 SXH  Added direct CALL to SALINV
C V13 13-AUG-1993 HXK  FIXED BUG WITH INSTANT CALL
C V12 10-AUG-1993 HXK  PUT IN TVLST FOR TERMINAL PEOPLE
C V11 06-AUG-1993 HXK  CHANGED TOLST TO TGLST
C V10 03-AUG-1993 GXA  Added code for Fractioning/Unfractioning transactions 
C                      and logging of them.
C V09 15-JUL-1993 GXA  Moved SON and SOFF into SPESRV from SPESRVF due to the 
C                      need of a valid SPECOM for file access for clearks.
C V08 05-JUL-1993 HXK  ADDED POPULARITY LIST 
C V07 27-JUN-1993 HXK  ADDED MISCELLANEOUS INVENTORY ITEMS (INVMIS) AND
C                      PETTY CASH (PETCASH) SUBROUTINE CALLS.
C V06 25-JUN-1995 HXK  ADDED INSTANT SALES, VALIDATIONS
C V05 10-JUN-1993 HXK  Changed AGTINF.DEF, AGTCOM.DEF includes.
C V04 17-MAY-1993 HJK  ADDED OPINION POLLING.
C V03 01-NOV-1991 GCAN SPLIT SPESRV INTO TWO TASKS, ONE FOR FREEZE ONE WITHOUT.
C                      SPESRV.EXE WILL HANDLE ALL FUNCTIONS THAT DO NOT
C                      REQUIRE A SYSTEM FREEZE OR HAVE DISK ACCESS (SLOW).
C V02 07-OCT-1991 MTK  INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C
C PROGRAM CAN GENERATE FOLLOWING 'SYNTERRCOD'
C         0     NO ERROR
C        10     INVALID GAME INDEX FOR GAME RESULTS REPORT (OUT OF RANGE)
C        20     INVALID GAME NUMBER FOR GAME RESULTS REPORT (OUT OF RANGE)
C        30     INVALID GAME INDEX FOR GAME RESULTS REPORT (OUT OF RANGE)
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
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM SPESRV
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SPECOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:SLOCOM.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'   
        INCLUDE 'INCLIB:FRAC.DEF'
!        INCLUDE 'INCLIB:EUROCONFIG.DEF'                                        !V34 - COMMENTED OUT
        INCLUDE 'INCLIB:APUCOM.DEF'
        INCLUDE 'INCLIB:EURCOM.DEF'                                             !V34
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:IGSDEBUG.DEF'
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
        INTEGER*4 LOGREC(LREC*3)
        INTEGER*4 MYCHKSUM
        INTEGER*4 CHKLEN
        INTEGER*4 TEMP
        INTEGER*4 TER
        INTEGER*4 BUF
        INTEGER*4 STATUS
        INTEGER*4 ST
        INTEGER*4 TASK
        INTEGER*4 LSTSUP
        INTEGER*4 CONTRL
        INTEGER*4 ERRTYP
        INTEGER*4 INDEX
C
C EURO MIL PROJECT - GTYP AND SENDEUROMIL
C
        INTEGER*4 GTYP
        INTEGER*4 GIND, EGNUM                                                   !V34
        INTEGER*4 MESS(EDLEN)                                                   !V34
!        LOGICAL SENDEUROMIL,SENDOUTMGR !V31
        LOGICAL SENDEUROMIL !V31
        LOGICAL SENDDISPAT
!        COMMON ECFREC                                                          !V34 - COMMENTED OUT
        INTEGER*4 FDB(7)
C
        DATA ERRTYP/Z90/
        DATA CONTRL/Z20/
        DATA LSTSUP/1/
        
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        LOGICAL SENDIGS
        INTEGER*4 I, FR_SYS_AVAIL_FLAG
        
        INTEGER*4  I4TEMP,TEMP1,TEMP2
        INTEGER*2  I2TEMP(2)
        BYTE       I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
C
        CALL COPYRITE
        CALL SNIF_AND_WRKSET
C
C        P(SPBLRP) = 0          ! SUPPRESS Billing Reports
C
C EURO MIL PROJECT - OPEN EURO CONFIG FILE AND LOAD CONFIG
C
!        CALL OPENX(1,'EUROCONF.FIL',4,0,0,ST)                                  !V34 - COMMENTED OUT
!        CALL IOINIT(FDB,1,ECFSEC*256)                                          !V34 - COMMENTED OUT
!        IF(ST .NE. 0) THEN                                                     !V34 - COMMENTED OUT
!          CALL OPS('EUROCONF.FIL open error  ',ST,0)                           !V34 - COMMENTED OUT
!          CALL OPS('CONTINUING WITHOUT EUROCONF.FIL !!!',0,0)                  !V34 - COMMENTED OUT
!        ENDIF                                                                  !V34 - COMMENTED OUT
!                                                                               !V34 - COMMENTED OUT
!        CALL READW(FDB,1,ECFREC,ST)                                            !V34 - COMMENTED OUT
!        IF(ST.NE.0) THEN                                                       !V34 - COMMENTED OUT
!          CALL OPS('EUROCONF.FIL read error ',ST,0)                            !V34 - COMMENTED OUT
!          CALL OPS('CONTINUING WITHOUT EUROCONF.FIL !!!',0,0)                  !V34 - COMMENTED OUT
!        ENDIF                                                                  !V34 - COMMENTED OUT
!        CALL CLOSEFIL(FDB)                                                     !V34 - COMMENTED OUT
C
C
        TASK=SPE
        MESS(1) = TASK                                                          !V34
10      CONTINUE
        BASECHKSUM=IAND(DAYCDC,'FFFF'X)
C
C
20      CONTINUE
        IF(DAYSTS.EQ.DSCLOS) CALL GSTOP(GEXIT_SUCCESS)
        IF(DAYSTS.EQ.DSSUSP) THEN
          CALL CLSSPE
          LSTSUP=1
30      CONTINUE
          CALL HOLD(0,ST)
          IF(DAYSTS.EQ.DSOPEN) GOTO 10
          IF(DAYSTS.EQ.DSCLOS) CALL GSTOP(GEXIT_SUCCESS)
          GOTO 30
        ENDIF
        CALL HOLD(0,STATUS)
C
C
        IF(LSTSUP.NE.P(SUPFIL)) THEN
          LSTSUP=P(SUPFIL)
          IF(LSTSUP.EQ.0) THEN
            CALL OPNSPE
          ELSE
            CALL CLSSPE
          ENDIF
        ENDIF
C
C GET BUFFER NUMBER FROM TOP OF SPECIAL SERVICES QUEUE.
C IF NO BUFFERS QUEUED, GO BACK TO WAIT STATE.
C
40      CONTINUE
        CALL TOPQUE(TASK,BUF)
        IF(BUF.EQ.0) GOTO 20
        CALL FASTSET(0,TRABUF,TRALEN)
C 
C EURO MIL PROJECT - SENDEUROMIL FALSE = DO NOT SEND TO EURO MIL
C EURO MIL PROJECT - SENDOUTMGR TRUE = DO NOT SEND TO EURO MIL BUT SEND TO OUTMGR
        SENDEUROMIL = .FALSE.
C        SENDOUTMGR  = .FALSE. !V31
        SENDDISPAT  = .FALSE. !V31
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        SENDIGS = .FALSE.
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
C BUILD SPECIAL FUNCTION TRANSACTION
C
        TER=HPRO(TERNUM,BUF)
C
C 2ND PHASE FRACTIONS AND UNFRACTION (ERROR MESSAGES ONLY)
C
        IF(HPRO(TRCODE,BUF).EQ.TYPFRA.OR.
     *     HPRO(TRCODE,BUF).EQ.TYPUNF) THEN
          CALL LOGTRA(TRABUF,PRO(FRA_WRKTAB,BUF))
          HPRO(TRCODE,BUF) = TYPREG
          GOTO 45
        ENDIF
C
        TRABUF(TSTAT)  = GOOD
        TRABUF(TERR)   = NOER
        TRABUF(TSUBERR)= NOER
        TRABUF(TTYP)   = TSPE
        TRABUF(TTER)   = TER
        TRABUF(TCDC)   = DAYCDC
        TRABUF(TSIZE)  = HPRO(NUMLRC,BUF)
        TRABUF(TSER)   = PRO(SERIAL,BUF)
        TRABUF(TTIM)   = PRO(TSTAMP,BUF)
        IF(TER.GE.1.AND.TER.LE.NUMAGT) TRABUF(TAGT)=AGTTAB(AGTNUM,TER)
        CALL DSPE(PRO(INPTAB,BUF),TRABUF,HPRO(INPLEN,BUF),
     *            HPRO(TRCODE,BUF))
C
C IF TRANSACTION HAS BEEN REJECTED BUILD ERROR MESSAGE
C BACK TO TERMINAL.
C
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF(IGSDEBUG(IA_SPESRV)) THEN
            CALL OPS('TRABUF(TSFUN)',TRABUF(TSFUN),TRABUF(TSFUN))
            CALL OPS('TYP/SUBTYP',ZEXT(BPRO(BINPTAB+1,BUF)),ZEXT(BPRO(BINPTAB+1,BUF)))
            CALL OPS('TRABUF(TERR)',TRABUF(TERR),TRABUF(TERR))
            CALL OPSTXT('INPTAB')
            CALL DUMP_MESSAGE(231,BUF,BPRO(BINPTAB,BUF),HPRO(OUTLEN,BUF))
            CALL OPSTXT('WRKTAB')
            CALL DUMP_MESSAGE(231,BUF,BPRO(WRKTAB,BUF),HPRO(OUTLEN,BUF))
        ENDIF
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------

45      CONTINUE
        IF(TRABUF(TERR).NE.NOER) THEN
          TRABUF(TSTAT)=REJT
          TEMP=CONTRL+TRABUF(TTRN)
          BPRO(BOUTTAB,BUF) = TEMP
          BPRO(BOUTTAB+1,BUF) = ERRTYP
          I4CCITT   = TRABUF(TCHK)
          BPRO(BOUTTAB+2,BUF) = I1CCITT(2)
          BPRO(BOUTTAB+3,BUF) = I1CCITT(1)
          BPRO(BOUTTAB+4,BUF) = TRABUF(TERR)
          BPRO(BOUTTAB+5,BUF) = TRABUF(TSUBERR)
          HPRO(OUTLEN,BUF)=6
          CHKLEN=HPRO(OUTLEN,BUF)
          CALL GETCCITT(PRO(OUTTAB,BUF),1,CHKLEN,MYCHKSUM)
          I4CCITT   = MYCHKSUM
          BPRO(BOUTTAB+2,BUF) = I1CCITT(2)
          BPRO(BOUTTAB+3,BUF) = I1CCITT(1)
          IF (TRABUF(TSFUN) .EQ. TREPR) GOTO 110 !V33
          GOTO 100
        ENDIF
C
C PROCESS SIGN-ON
C
        IF(TRABUF(TSFUN).EQ.TSON) THEN
C----+------------------------------------------------------------------
C V35| New Terminals Project - Olimpo
C         1 means that comes from channel Olimpo other wise comes from x2x or mxs  
C----+------------------------------------------------------------------                
          IF(BPRO(CHOLM_OLM,BUF) .EQ. 1) THEN
            TRABUF(TSDT1)=PRO(SEROLM_INT_OLM,BUF)
            TRABUF(TSDT2)=PRO(SEROLM_INT_OLM+1,BUF)
            TRABUF(TSDT3)=BPRO(SEROLM_OLM+8,BUF)
            TRABUF(TSDT4)=PRO(MESSID_INT_OLM,BUF)
            TRABUF(TSDT5)=BPRO(MESSID_OLM+1,BUF)
            TRABUF(TSDT6)=BPRO(CHOLM_OLM,BUF)
          ENDIF 
C----+------------------------------------------------------------------
C V35| New Terminals Project - Olimpo
C----+------------------------------------------------------------------                           
          CALL SON(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          GOTO 100
        ENDIF
C
C PROCESS SIGN-OFF
C
        IF(TRABUF(TSFUN).EQ.TSOFF) THEN
          CALL SOFF(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          GOTO 100
        ENDIF
C
C PROCESS SALES REPORTS
C
        IF((TRABUF(TSFUN).EQ.TSREP) .AND. (P(SYSTYP) .EQ. LIVSYS)) THEN
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
          IF(IGSDEBUG(IA_SPESRV)) THEN
              CALL OPSTXT('TSREP (WITH LIVSYS)')
          ENDIF
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
C EURO MIL PROJECT - SALES REPORTS AND LIVE SYSTEM 
C  IF GAME TYPE = 17  THEN SEND TO EUROMILHOES
C     ELSE IF SALES REPORTS AND NOT LIVE SYSTEM THEN FREE BUFFER AND GO TO NEXT BUFFER
C
C          CALL OPSTXT('SAL REP')
C----+------------------------------------------------------------------
C V35| New Terminals Project - Olimpo
C         1 means that comes from channel Olimpo other wise comes from x2x or mxs  
C----+------------------------------------------------------------------
          IF(BPRO(CHOLM_OLM,BUF) .EQ. 1) THEN
            TRABUF(TSDT7)=PRO(SEROLM_OLM,BUF)
            TRABUF(TSDT8)=PRO(SEROLM_OLM+1,BUF)
            TRABUF(TSDT9)=BPRO(SEROLM_OLM+8,BUF)
            TRABUF(TSDT10)=PRO(MESSID_OLM,BUF)
            TRABUF(TSDT11)=PRO(MESSID_OLM+1,BUF)
            TRABUF(TSDT12)=BPRO(CHOLM_OLM,BUF)
          ENDIF        
C----+------------------------------------------------------------------
C V35| New Terminals Project - Olimpo
C----+------------------------------------------------------------------ 
          CALL SALEREP(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          IF(TRABUF(TERR).EQ. NOER) THEN
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
              IF(IGSDEBUG(IA_SPESRV)) THEN
                  CALL OPSTXT('TRABUF(TERR) .EQ. NOER')
              ENDIF
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
              SENDEUROMIL = .TRUE.
              SENDIGS     = .FALSE.
              SENDDISPAT  = .FALSE.
          ENDIF
          IF(TRABUF(TERR).NE. NOER) THEN
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
              IF(IGSDEBUG(IA_SPESRV)) THEN
                  CALL OPSTXT('TRABUF(TERR) .NE. NOER')
              ENDIF
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C             SENDOUTMGR = .TRUE.
              CALL TRALOG(TRABUF,APUBUF(2,BUF))
C              HPRO(TRCODE,BUF) = TYPEUR
              HPRO(TRCODE,BUF) = TYPIGS !V33
              SENDEUROMIL = .FALSE.
              SENDIGS     = .FALSE.
              SENDDISPAT  = .TRUE.
              GOTO 110 !V33
          ENDIF
          GOTO 100
        ELSE IF((TRABUF(TSFUN).EQ.TSREP) .AND. (P(SYSTYP) .NE. LIVSYS)) THEN 
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
          IF(IGSDEBUG(IA_SPESRV)) THEN
              CALL OPSTXT('TSREP (WITHOUT LIVSYS)')
          ENDIF
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
          CALL RELBUF(BUF)
          CALL DQUTRA(TASK,BUF)
          SENDEUROMIL = .FALSE.
          SENDIGS     = .FALSE.
C          SENDOUTMGR  = .FALSE.
          SENDDISPAT  = .FALSE.
          GOTO 40
        ENDIF
C
C PROCESS PASSIVE ACCOUNTING REPORT
C
        IF(TRABUF(TSFUN).EQ.TPASAC) THEN
          CALL SALINV_PAS_OLD(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF)) !V27
          GOTO 100
        ENDIF
C
C PROCESS NEW PASSIVE ACCOUNTING REPORT                                !V27...
C
        IF(TRABUF(TSFUN).EQ.TPASAD) THEN
          CALL SALINV_PAS(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          GOTO 100
        ENDIF                                                          !...V27
C-----------------------------------------------------------------------
C V31| Adding new accounting platform:
C    | Processing new accounting reports
C-----------------------------------------------------------------------
        IF ((TRABUF(TSFUN).EQ.TNAP) .AND. (P(SYSTYP) .EQ. LIVSYS)) THEN
          IF(IGSDEBUG(IA_SPESRV)) THEN
              CALL OPSTXT('411:TRABUF(TSFUN) = TNAP')
              CALL OPSTXT('411:P(SYSTYP) = LIVSYS')
          ENDIF
CV34          IF (TRABUF(TERR).EQ. NOER .AND. P(SPBLRP) .EQ. 0) THEN
          IF (TRABUF(TERR).EQ. NOER .AND. P(EUSPBIR) .EQ. 0) THEN               !V34 - SPBLRP REPLACED BY EUSPBIR
            IF(IGSDEBUG(IA_SPESRV)) THEN
                CALL OPSTXT('416:TRABUF(TERR) = NOER')
CV34                CALL OPSTXT('416:P(SPBLRP) = 0')
                CALL OPSTXT('416:P(EUSPBIR) = 0')                               !V34 - SPBLRP REPLACED BY EUSPBIR
            ENDIF
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS - Bugfix
C----+------------------------------------------------------------------
            TRABUF(TSDT1) = ZEXT(BPRO(BINPTAB+4,BUF)) ! CLASS
            TRABUF(TSDT2) = ZEXT(BPRO(BINPTAB+5,BUF)) ! SUBCLASS
            I4TEMP    = 0
            I1TEMP(4) = ZEXT(BPRO(BINPTAB + 11,BUF))
            I1TEMP(3) = ZEXT(BPRO(BINPTAB + 12,BUF))
            I1TEMP(2) = ZEXT(BPRO(BINPTAB + 13,BUF))
            I1TEMP(1) = ZEXT(BPRO(BINPTAB + 14,BUF))
            IF( I4TEMP .NE. 0 ) THEN
                TRABUF(TSDT3) = I4TEMP
            ELSE
                TRABUF(TSDT3) = TRABUF(TAGT)
            ENDIF
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS - Bugfix
C----+------------------------------------------------------------------
            SENDEUROMIL = .TRUE.
            SENDIGS     = .FALSE.
            SENDDISPAT  = .FALSE.
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS - Bugfix
C----+------------------------------------------------------------------
            IF(IGSDEBUG(IA_SPESRV)) THEN
                CALL OPS('444:TRABUF(TSDT1)',TRABUF(TSDT1),TRABUF(TSDT1))
                CALL OPS('444:TRABUF(TSDT2)',TRABUF(TSDT2),TRABUF(TSDT2))
                CALL OPS('444:TRABUF(TSDT3)',TRABUF(TSDT3),TRABUF(TSDT3))
                CALL OPS('444:SENDEUROMIL',SENDEUROMIL,SENDEUROMIL)
                CALL OPS('444:SENDDISPAT',SENDDISPAT,SENDDISPAT)
                CALL OPS('444:SENDIGS',SENDIGS,SENDIGS)
            ENDIF
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS - Bugfix
C----+------------------------------------------------------------------
            GOTO 100
          ELSE
C          ENDIF
C          IF (TRABUF(TERR).NE. NOER .OR. P(SPBLRP).EQ.1) THEN
C            IF(P(SPBLRP).EQ. 1) TRABUF(TERR) = SUPR
            IF(TRABUF(TERR).EQ. NOER) TRABUF(TERR) = SUPR
            TRABUF(TSTAT) = REJT
            TRABUF(TSDT1) = ZEXT(BPRO(BINPTAB+4,BUF)) ! CLASS
            TRABUF(TSDT2) = ZEXT(BPRO(BINPTAB+5,BUF)) ! SUBCLASS
            CALL TRALOG(TRABUF,APUBUF(2,BUF))
            HPRO(TRCODE,BUF) = TYPEUR
C              SENDOUTMGR = .TRUE.
            SENDEUROMIL = .FALSE.
C            SENDOUTMGR  = .FALSE.
            SENDIGS     = .FALSE.
            SENDDISPAT  = .TRUE.
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS - Bugfix
C----+------------------------------------------------------------------
            IF(IGSDEBUG(IA_SPESRV)) THEN
                CALL OPS('471:TRABUF(TSDT1)',TRABUF(TSDT1),TRABUF(TSDT1))
                CALL OPS('471:TRABUF(TSDT2)',TRABUF(TSDT2),TRABUF(TSDT2))
                CALL OPS('471:TRABUF(TSDT3)',TRABUF(TSDT3),TRABUF(TSDT3))
                CALL OPS('471:SENDDISPAT',SENDDISPAT,SENDDISPAT)
                CALL OPS('471:SENDEUROMIL',SENDEUROMIL,SENDEUROMIL)
                CALL OPS('471:SENDIGS',SENDIGS,SENDIGS)
            ENDIF
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS - Bugfix
C----+------------------------------------------------------------------
C            GOTO 100
            GOTO 110 !V33
          ENDIF
          GOTO 100
        ELSE IF ((TRABUF(TSFUN).EQ.TNAP) .AND. (P(SYSTYP) .NE. LIVSYS)) THEN 
          CALL RELBUF(BUF)
          CALL DQUTRA(TASK,BUF)
          SENDEUROMIL = .FALSE.
C          SENDOUTMGR  = .FALSE.
          SENDIGS     = .FALSE.
          SENDDISPAT  = .FALSE.
          GOTO 40
        ENDIF
C-----------------------------------------------------------------------
C V31| Adding new accounting platform
C-----------------------------------------------------------------------
C
C PROCESS INVOICE REPORTS
C
        IF((TRABUF(TSFUN).EQ.TSINV) .AND. (P(SYSTYP) .EQ. LIVSYS)) THEN
C
C EURO MIL PROJECT - INVOICE REPORTS AND LIVE SYSTEM 
C  IF GAME TYPE = 17  THEN SEND TO EUROMILHOES
C     ELSE IF INVOICE REPORTS AND NOT LIVE SYSTEM THEN FREE BUFFER AND GO TO NEXT BUFFER
C
C          CALL OPSTXT('SAL INV')
          CALL SALINV(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          IF (TRABUF(TERR).EQ. NOER) THEN
            SENDEUROMIL = .TRUE.
            SENDIGS     = .FALSE.
            SENDDISPAT  = .FALSE.
          ENDIF
          IF (TRABUF(TERR).NE. NOER) THEN
            CALL TRALOG(TRABUF,APUBUF(2,BUF))
            HPRO(TRCODE,BUF) = TYPEUR
            SENDEUROMIL = .FALSE.
            SENDIGS     = .FALSE.
            SENDDISPAT  = .TRUE.
C            SENDOUTMGR = .TRUE.
          ENDIF
          GOTO 100
        ELSE IF ((TRABUF(TSFUN).EQ.TSINV) .AND. (P(SYSTYP) .NE. LIVSYS)) THEN 
          CALL RELBUF(BUF)
          CALL DQUTRA(TASK,BUF)
          SENDEUROMIL = .FALSE.
C          SENDOUTMGR  = .FALSE.
          SENDIGS     = .FALSE.
          SENDDISPAT  = .FALSE.
          GOTO 40
        ENDIF
C
C PROCESS GAME REPORTS
C
        IF(TRABUF(TSFUN) .EQ. TGREP) THEN
C
C EURO MIL PROJECT - GAME REPORT AND LIVE SYSTEM 
C  IF GAME TYPE = 17 OR GAME TYPE = 19 THEN SEND TO EUROMILHOES
C     ELSE IF GAME REPORT AND NOT LIVE SYSTEM THEN FREE BUFFER AND GO TO NEXT BUFFER
C         
          GTYP  = ZEXT(BPRO(BINPTAB+4,BUF))
!          IF((GTYP .EQ. TEUM) .AND. (P(SYSTYP) .EQ. LIVSYS)) THEN
          IF((GTYP .EQ. TEUM .OR. GTYP .EQ. TRAF) .AND. 
     *       (P(SYSTYP) .EQ. LIVSYS)) THEN                                      !V34
C----+------------------------------------------------------------------
C V31| Adding new flag to suppress reports
C----+------------------------------------------------------------------
CV34            IF (P(EUSPREP) .EQ. 0) THEN
            SYNTERRCOD = 0
            GIND = ZEXT(BPRO(BINPTAB+5,BUF))
            IF(GIND.GT.0.AND.GIND.LE.MAXIND) THEN
              EGNUM = EGTNTAB(GTYP,GIND)                                        !V34 - GET (EXTERNAL) GAME NUMBER
              IF(EGNUM.GT.0.AND.EGNUM.LE.EMAXGAM) THEN                          !V34
                IF(P(EUSPGRR) .EQ. 0 .AND.                                      !V34 - EUSPREP REPLACED BY EUSPGRR
     *             .NOT.TSBIT(P(EUSPGGRR), EGNUM-1)) THEN                       !V34 - ADDED TEST OF GLAOBAL PARAMETER EUSPGGRR (GAME SPECIFIC)
                  SENDEUROMIL = .TRUE.
                  SENDIGS     = .FALSE.
C                  SENDOUTMGR  = .FALSE.
                  SENDDISPAT  = .FALSE.
                  GOTO 100                                                      !V34
                ELSE
                  TRABUF(TSTAT) = REJT
                  TRABUF(TERR)  = SUPR
                  TRABUF(TSDT1) = GTYP                                          !GAME TYPE
                  TRABUF(TSDT2) = GIND                                          !GAME INDEX
CV24                  IF(TRABUF(TSDT2) .NE. 1) THEN
CV24                    SYNTERRCOD = 10
CV24                    TRABUF(TERR) = SYNT
CV24                  ENDIF
                  CALL TRALOG(TRABUF,APUBUF(2,BUF))
                  HPRO(TRCODE,BUF) = TYPEUR
                  SENDEUROMIL = .FALSE.
                  SENDIGS     = .FALSE.
C                  SENDOUTMGR  = .FALSE.
                  SENDDISPAT  = .TRUE.
CV34                  GOTO 110 !V33
                  GOTO 80                                                       !V34
                ENDIF
              ELSE                                                              !INVALID GAME NUMBER
                SYNTERRCOD    = 20
                TRABUF(TSTAT) = REJT
                TRABUF(TERR)  = SYNT
                TRABUF(TSDT1) = GTYP                                            !GAME TYPE
                TRABUF(TSDT2) = GIND                                            !GAME INDEX
                CALL TRALOG(TRABUF,APUBUF(2,BUF))
                HPRO(TRCODE,BUF) = TYPEUR
                SENDEUROMIL = .FALSE.
                SENDIGS     = .FALSE.
                SENDDISPAT  = .TRUE.
                GOTO 80
              ENDIF
            ELSE                                                                !INVALID GAME INDEX
              SYNTERRCOD    = 30
              TRABUF(TSTAT) = REJT
              TRABUF(TERR)  = SYNT
              TRABUF(TSDT1) = GTYP                                              !GAME TYPE
              TRABUF(TSDT2) = 0                                                 !GAME INDEX
              CALL TRALOG(TRABUF,APUBUF(2,BUF))
              HPRO(TRCODE,BUF) = TYPEUR
              SENDEUROMIL = .FALSE.
              SENDIGS     = .FALSE.
              SENDDISPAT  = .TRUE.
              GOTO 80
            ENDIF
C
80          CONTINUE
            IF(P(SUPSYN).EQ.0.AND.SYNTERRCOD.NE.0.AND.
     *        TRABUF(TERR).NE.NOTON) THEN
              MESS(2) = TEGEN
              MESS(3) = 10
              MESS(4) = SYNTERRCOD
              MESS(5) = TRABUF(TTER)
              MESS(6) = TRABUF(TSDT1)                                           !GAME TYPE
              MESS(7) = TRABUF(TSDT2)                                           !GAME INDEX
              MESS(8) = TRABUF(TSER)
              CALL QUEMES(MESS)
            ENDIF
            GOTO 110
C----+------------------------------------------------------------------
C V31| Adding new flag to suppress reports
C----+------------------------------------------------------------------
!          ELSE IF(GTYP .NE. TEUM) THEN
          ELSE IF(GTYP .NE. TEUM .AND. GTYP .NE. TRAF) THEN                     !V34
C            CALL OPSTXT('GREP GTYP NOT EUROMILLIONS')
            CALL GAMEREP(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
            SENDEUROMIL = .FALSE.
            SENDIGS     = .FALSE.
C            SENDOUTMGR  = .FALSE.
            SENDDISPAT  = .FALSE.
!          ELSE IF((GTYP .EQ. TEUM) .AND. (P(SYSTYP) .NE. LIVSYS)) THEN
          ELSE IF((GTYP .EQ. TEUM .OR. GTYP .EQ. TRAF) .AND.
     *            (P(SYSTYP) .NE. LIVSYS)) THEN                                 !V34
C            CALL OPSTXT('GREP GTYP EUROMILLIONS NOT LIVSYS')
            CALL RELBUF(BUF)
            CALL DQUTRA(TASK,BUF)
            SENDEUROMIL = .FALSE.
            SENDIGS     = .FALSE.
C            SENDOUTMGR  = .FALSE.
            SENDDISPAT  = .FALSE.
            GOTO 40
          ENDIF
          GOTO 100
        ENDIF
C
C PROCESS INFORMATION REPORTS (NEWS AND ODDS)
C
        IF(TRABUF(TSFUN).EQ.TINFO.OR.
     *    TRABUF(TSFUN).EQ.TOLST.OR.
     *    TRABUF(TSFUN).EQ.TVLST) THEN
          CALL INFOREP(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),
     *    HPRO(MSGNUM,BUF))
          GOTO 100
        ENDIF
C
C PROCESS REPRINTS
C
        IF(TRABUF(TSFUN).EQ.TREPR) THEN
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C          CALL REPRINT(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),SENDEUROMIL)
          IF(IGSDEBUG(IA_SPESRV)) THEN
              CALL OPSTXT('435:BEFORE CALL TO REPRINT')
              CALL OPS('BUF         ',BUF,BUF)
              CALL OPS('SENDEUROMIL ',SENDEUROMIL,SENDEUROMIL)
              CALL OPS('SENDIGS     ',SENDIGS,SENDIGS)
              CALL OPS('SENDDISPAT  ',SENDDISPAT,SENDDISPAT)
              CALL OPS('TRABUF(TSER)',TRABUF(TSER),TRABUF(TSER))
              CALL DUMP_MESSAGE(0,439,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ENDIF
          CALL REPRINT(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),SENDEUROMIL,SENDIGS)
          IF(SENDIGS .EQ. .FALSE.) THEN
              CALL TRALOG(TRABUF,APUBUF(2,BUF))
              HPRO(TRCODE,BUF) = TYPIGS
              SENDEUROMIL = .FALSE.
              SENDDISPAT  = .TRUE.
              IF(IGSDEBUG(IA_SPESRV)) THEN
                CALL OPSTXT('442:AFTER CALL TO REPRINT')
                CALL OPS('BUF        ',BUF,BUF)
                CALL OPS('SENDEUROMIL',SENDEUROMIL,SENDEUROMIL)
                CALL OPS('SENDIGS    ',SENDIGS,SENDIGS)
                CALL OPS('SENDDISPAT ',SENDDISPAT,SENDDISPAT)
                CALL OPS('TRABUF(TSER)',TRABUF(TSER),TRABUF(TSER))
                CALL OPS('TRABUF(TAGT)',TRABUF(TAGT),TRABUF(TAGT))
                CALL OPS('TRABUF(TTER)',TRABUF(TTER),TRABUF(TTER))
                CALL OPS('TRABUF(TCDC)',TRABUF(TCDC),TRABUF(TCDC))
                CALL OPS('TRABUF(TSDT1)',TRABUF(TSDT1),TRABUF(TSDT1))
                CALL OPS('TRABUF(TSDT2)',TRABUF(TSDT2),TRABUF(TSDT2))
                CALL DUMP_MESSAGE(0,447,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
              ENDIF
              GOTO 110
          ENDIF
          IF(IGSDEBUG(IA_SPESRV)) THEN
              CALL OPSTXT('442:AFTER CALL TO REPRINT 2')
              CALL OPS('BUF        ',BUF,BUF)
              CALL OPS('SENDEUROMIL',SENDEUROMIL,SENDEUROMIL)
              CALL OPS('SENDIGS    ',SENDIGS,SENDIGS)
              CALL OPS('SENDDISPAT ',SENDDISPAT,SENDDISPAT)
              CALL OPS('TRABUF(TSER)',TRABUF(TSER),TRABUF(TSER))
              CALL OPS('TRABUF(TAGT)',TRABUF(TAGT),TRABUF(TAGT))
              CALL OPS('TRABUF(TTER)',TRABUF(TTER),TRABUF(TTER))
              CALL OPS('TRABUF(TCDC)',TRABUF(TCDC),TRABUF(TCDC))
              CALL OPS('TRABUF(TSDT1)',TRABUF(TSDT1),TRABUF(TSDT1))
              CALL OPS('TRABUF(TSDT2)',TRABUF(TSDT2),TRABUF(TSDT2))
              CALL DUMP_MESSAGE(0,447,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          ENDIF
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
          GOTO 100
        ENDIF
C
C PROCESS AGENT ADJUSTMENTS
C
        IF(TRABUF(TSFUN).EQ.THASF) THEN
            CALL TRAHSF(ASFFDB,SPE,TRABUF)
            GOTO 100
        ENDIF
C
C PROCESS AGTINF UPDATES (ONLINE AGENT UPDATE)
C
        IF(TRABUF(TSFUN).EQ.TAGTINF) THEN
            CALL UPDAGTINF(ASFFDB,SPE,TRABUF)
            GOTO 100
        ENDIF
C
C GET AGENT NAME
C
        IF(TRABUF(TSFUN).EQ.TAGTNAM) THEN
            CALL GETAGTNAM(ASFFDB,SPE,TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
            GOTO 100
        ENDIF
C
C PROCESS JACKPOT REPORT TRANSACTION
C
        IF(TRABUF(TSFUN).EQ.TJACK) THEN
          CALL JACKPOT(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          GOTO 100
        ENDIF
C
C PROCESS OPINION POLL TRANSACTION
C
        IF(TRABUF(TSFUN).EQ.TPOLL) THEN
          CALL OPNPOLL(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          CALL UPDMIS(TRABUF)
          GOTO 100
        ENDIF
C
C PROCESS SUPPLY ORDER AND DELIVERY AND GVT REPRESENTATIVE SIGNON
C
        IF(TRABUF(TSFUN).EQ.TDLV) THEN
          CALL DELIVERY(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          GOTO 100
        ENDIF
C
C PROCESS GVT PASS NUMBER MANAGEMENT
C
        IF(TRABUF(TSFUN).EQ.TPASS) THEN
          CALL GVTPASS(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          GOTO 100
        ENDIF
C
C PROCESS CHECKWRITTER
C
        IF(TRABUF(TSFUN).EQ.TCHKWRT) THEN
          CALL CHKLOG(TRABUF,PRO(INSTAB,BUF))
          GOTO 100
        ENDIF
C                                                                       
C PROCESS INTRASYS ACKS FROM STRATUS                                    
C                                                                       
        IF(TRABUF(TSFUN).EQ.TISSACK) THEN                               
          CALL ISSLOG(TRABUF,BPRO(BINSTAB+1,BUF))                       
          GOTO 100                                                      
        ENDIF
C                                                                       
C PROCESS BAD UNSOLICITED INTRASYS MSGS FROM STRATUS                    
C                                                                       
        IF(TRABUF(TSFUN).EQ.TISUBAD) THEN                               
          CALL ISULOG(TRABUF,BPRO(BINSTAB+1,BUF))                       
          GOTO 100                                                      
        ENDIF                                                           
C
C PROCESS FINANCIAL PASSTHRU TRANS FROM IPS
C
        IF(TRABUF(TSFUN).EQ.TFPT) THEN
          CALL FPTLOG(TRABUF,BPRO(BINSTAB+1,BUF))
          GOTO 100
        ENDIF
C
C PROCESS INSTANT GAMES TRANSACTION
C
CRXK        IF(TRABUF(TSFUN).EQ.TINSS.OR.TRABUF(TSFUN).EQ.TINSV) THEN
CRXK          CALL INSTANT(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
CRXK          GOTO 100
CRXK        ENDIF
C
C PROCESS MISCELLANEOUS SALES ITEMS INVENTORY
C
        IF(TRABUF(TSFUN).EQ.TITEMS) THEN
          CALL INVMIS(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          GOTO 100
        ENDIF
C
C PROCESS PETTY CASH TRANSACTIONS
C
        IF(TRABUF(TSFUN).EQ.TPCSH) THEN
          CALL PETCASH(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          GOTO 100
        ENDIF
C
C PROCESS FRACTION/UNFRACTION WAGER REQUEST (PHASE 1)
C
        IF(TRABUF(TSFUN).EQ.TFRC.OR.
     *     TRABUF(TSFUN).EQ.TUNFRC) THEN

          IF(P(SYSTYP).EQ.LIVSYS) THEN
             IF(TRABUF(TSFUN).EQ.TFRC) THEN
               CALL FRACONE(TRABUF,BUF)
             ELSE 
               CALL UNFRACONE(TRABUF,BUF)
             ENDIF
          ENDIF

          CALL DQUTRA(TASK,BUF)
          IF(P(SYSTYP).EQ.LIVSYS) THEN
            CALL ATL(BUF,QUETAB(1,DIS),ST)
          ELSE
            CALL RELBUF(BUF)
          ENDIF
          GOTO 40
        ENDIF
C
C PROCESS NEW TEAM AND RESULTS REPORT V30 CR15
C
        IF(TRABUF(TSFUN).EQ.TMMES) THEN
          CALL SPTEAMS(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
          GOTO 100
        ENDIF
C
C INVALID FUNCTION FOR THIS SPESRV TASK
C
        TRABUF(TERR)=INVL
        TRABUF(TSTAT)=REJT
        GOTO 45
C
C LOG TRANSACTION AND QUEUE TO THE LOGGER OUTPUT QUEUE
C
100     CONTINUE
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF(IGSDEBUG(IA_SPESRV)) THEN
            CALL OPSTXT('100 - CONTINUE')
            CALL OPS('BUF        ',BUF,BUF)
            CALL OPS('SENDEUROMIL',SENDEUROMIL,SENDEUROMIL)
            CALL OPS('SENDIGS    ',SENDIGS,SENDIGS)
            CALL OPS('SENDDISPAT ',SENDDISPAT,SENDDISPAT)
        ENDIF
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF((SENDEUROMIL .EQ. .TRUE.) .AND. (P(SYSTYP) .EQ. LIVSYS)) THEN
          CALL TRALOG(TRABUF,PRO(WRKTAB,BUF))
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        ELSEIF((SENDIGS .EQ. .TRUE.) .AND. (P(SYSTYP) .EQ. LIVSYS)) THEN
          CALL TRALOG(TRABUF,PRO(WRKTAB,BUF))
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        ELSE 
          CALL TRALOG(TRABUF,LOGREC)
          CALL WLOG(PRO(SERIAL,BUF),LOGREC,TASK)
        ENDIF
C
110     CONTINUE
C
        IF(TRABUF(TERR).EQ.TBAD) HPRO(ENCOVR,BUF)=-1
        IF(TER.GE.1 .AND. TER.LE.NUMAGT) THEN  !check for valid ter num HXK
          IF(TRABUF(TINTRA).NE.1) THEN
            AGTHTB(ATRNUM,TER)=TRABUF(TTRN)
            AGTHTB(ACHKSM,TER)=-1
          ENDIF
        ENDIF
C
C EURO MIL PROJECT - IF SENDEUROMIL = TRUE THEN SEND TO COMMGR ELSE SEND TO LOGGER
C   IF NOT LIVE SYSTEM THEN FREE BUFFER
        
        IF((SENDEUROMIL .EQ. .TRUE.) .AND. (P(SYSTYP) .EQ. LIVSYS)) THEN
          CALL QUETRA(EUC, BUF)
C        ELSE IF((SENDOUTMGR .EQ. .TRUE.) .AND. (P(SYSTYP) .EQ. LIVSYS)) THEN
C          CALL QUETRA(EUO, BUF)
        ELSE IF((SENDDISPAT .EQ. .TRUE.) .AND. (P(SYSTYP) .EQ. LIVSYS)) THEN
C          CALL QUETRA(DIS, BUF)
           CALL ABL(BUF,QUETAB(1,DIS),ST) !V32
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        ELSEIF((SENDIGS .EQ. .TRUE.) .AND. (P(SYSTYP) .EQ. LIVSYS)) THEN
          IF(IGSDEBUG(IA_SPESRV)) THEN
              CALL OPS('620: BEFORE CALL TO IGS_QUETRA',BUF,BUF)
          ENDIF
          CALL IGS_QUETRA(BUF)
          IF(IGSDEBUG(IA_SPESRV)) THEN
              CALL OPS('624: AFTER CALL TO IGS_QUETRA',BUF,BUF)
          ENDIF
C----+------------------------------------------------------------------
C V33| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        ELSE
C          CALL OPSTXT('SENDEUROMIL (ELSE)')
          CALL QUETRA(LOG,BUF)
        ENDIF
        CALL DQUTRA(TASK,BUF)
c
c apagar
c       
C        TYPE *,' ',TRABUF(TSFUN)
C       DO TER=0,HPRO(OUTLEN,BUF)
C              TYPE 9998,TER+1,BPRO(BOUTTAB+TER,BUF) 
C           ENDDO
C           TYPE *,' ' 
        GOTO 40
C9998    FORMAT(' SPESRV: ',I3.1,' - ', Z3.2)
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

        
