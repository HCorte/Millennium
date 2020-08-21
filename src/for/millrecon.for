C
C PROGRAM MILLRECON (MILLENNIUM RECONCILIATION)
C
C THIS PROGRAM GENERATES AN ASCII FILE WITH THE
C FIELDS VALUE OF A DEFINED SET OF TRANSACTIONS.
C THE TRANSACTIONS ARE READ FROM THE MTMF FILE.
C THE FILE IS A SET OF LINES CORRESPONDING TO
C TRANSACTIONS, EACH LINE IS COMPOSED BY THE VALUES
C OF THE TRANSACTION FIELDS AND EACH ONE IS SEPARATED
C BY A DELIMITER CHARACTER.
C
C SET OF TRANSACTIONS CONSIDERED:
C
C  1. WAGER
C  2. CANCELLATION
C  3. PRIZE PAYMENT/VALIDATION
C  4. SIGN ON/OFF
C
C      GAME NUMBER       GAME NAME
C          1          Totobola Normal
C          5          Joker
C          6          Totoloto Sábado
C          7          Totoloto Quarta
C          8          Lotaria Clássica
C          9          Lotaria Popular
C         10          Totobola Extra 1
C         11*         Euromilhões
C         12*         M1lhão
C
C * The game numbers 11 and 12 were the next game numbers available.
C It was defined to identify Euromillions and M1lhão games. These values
C are shared with the source file MDCMNG.FOR.
C
C ** Source - MILLRECON.FOR;1 **
C
C MILLRECON.FOR
C
C V04 21-JUN-2016 SCML M16 PROJECT
C V03 13-JAN-2014 SCML Added support to new validation sub-types
C V02 12-SEP-2011 ACN  IGEST PROJECT
C V01 03-ABR-2009 ACN  INITIAL RELEASE
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM MILLRECON
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
        INCLUDE 'INCLIB:EURCON.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'

        INTEGER*4 ST, LUN
        INTEGER*4 SEROFFSET
        INTEGER*4 FILNAME(7)
        INTEGER*2 VDAT(LDATE_LEN)
        LOGICAL EOT/.FALSE./
        CHARACTER*22 TRANFNAME ! TRANSACTIONS FILE NAME

        SEROFFSET = 0

        VDAT(VCDC)=DAYCDC
        CALL LCDATE(VDAT)

        WRITE (TRANFNAME, 901) DAYCDC ! TRANSACTIONS FILE NAME

        TYPE*, 'Opening transaction file with name ', TRANFNAME

C       OPEN THE OUTPUT FILE
        OPEN(LUN, FILE=TRANFNAME, STATUS='NEW')

        TYPE*, 'File opened, writing HEADER'
        WRITE(LUN,*) 'HEAD'

        FILNAME(1) = 'PRIM'
        FILNAME(2) = ':MTM'
        FILNAME(3) = 'F01.'
        FILNAME(4) = 'FIL'

        TYPE*, 'Opening transaction master file with name PRIM:MTMF01.FIL'

C       Open TMF FILE
        CALL OPENWY(1, FILNAME, 0, 4, 0, ST)
        CALL TOPEN(1)

        IF(ST.NE.0) THEN
          CALL FILERR(FILNAME, 1, ST, 0)
          GOTO 30
        ENDIF

        TYPE*, 'Processing Transactions from TMF... '

20      CONTINUE

        CALL GETTRABUF(TRABUF, SEROFFSET, EOT) ! GETS A TRANSACTION FROM PRIM:MTMF01.FIL

        IF (.NOT. EOT) THEN
          CALL WRITE_TO_FILE(TRABUF, LUN, SEROFFSET)
          GOTO 20
        ELSE
          TYPE *, 'End of File'
        ENDIF

C       CLOSE FILE
30      CONTINUE
        TYPE*, 'Processing finished, writing TAIL'
        WRITE(LUN, *) 'TAIL'
        TYPE*, 'Closing transaction file'
        CLOSE(LUN)
        TYPE*, 'Program Ended'

C
C FORMAT STATEMENTS
C
901     FORMAT('MILLENNIUM_',I4.4,'.ASC')

        END

C**************************************************
C SUBROUTINE: WRITES THE TRANSACTION VALUES TO THE
C             FILE
C INPUT:
C       TRABUF - CONTAINS THE DATA TO BE WRITTEN
C          SER - SERIAL NUMBER OF THE TRANSACTION
C
C OUTPUT:
C        NONE.
C
C**************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE WRITE_TO_FILE(TRABUF, LUN, SEROFFSET)
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
        INCLUDE 'INCLIB:EURCON.DEF'

        INTEGER*4 LEN
        INTEGER*4 I4TEMP
        INTEGER*2 I2TEMP(2)
        BYTE      I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)

        INTEGER*4 LUN, SEROFFSET
        INTEGER*4 CSHAMTL, CSHAMTH
        INTEGER*4 TCKS
        INTEGER*4 GAMEUR
        PARAMETER (GAMEUR = 11)                                                 !EUROMILLIONS GAME NUMBER (THE NEXT GAME NUMBER AVAILABLE: 11)
C
        INTEGER*4 GAMSM                                                         !V04
        PARAMETER (GAMSM = 12)                                                  !M1LHAO GAME NUMBER (THE NEXT GAME NUMBER AVAILABLE AFTER EUROMILLIONS: 12) !V04
C
        INTEGER*4 ITYPE, ISUBTYPE, ISERIAL, IAMNT, IJKAMNT, IAGENT, IHOUR, IMIN, ISEC
        INTEGER*4 IVTYPE, IVSUBTYPE, IVGAM, IVSERIAL, IVAGENT, IVHOUR, IVMIN, IVSEC
        INTEGER*4  IVAMNT(2)
        INTEGER*8  I8VAMNT
        EQUIVALENCE (IVAMNT,I8VAMNT)

        INTEGER*4 ISTYPE, ISSUBTYPE, ISSERIAL, ISAGENT, ISTER, ISCDC, ISHOUR, ISMIN, ISSEC
        
        CSHAMTL = 0
        CSHAMTH = 0

C   TYPE*, ' INFO: WRITE_TO_FILE CALLED.' ! INFO

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                         W A G E R  &  C A N C E L L A T I O N
C                              T R A N S A C T I O N S
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        IF(TRABUF(TTYP) .EQ. TWAG .OR. TRABUF(TTYP) .EQ. TCAN) THEN

C          TYPE*,' INFO: WRITING WAGER/CANCELLATION TRANSACTION...'

          ITYPE = TRABUF(TTYP) ! TRANSACTION TYPE
          ISUBTYPE = TRABUF(TGAM) ! GAME NUMBER
          ISERIAL = SEROFFSET ! SERIAL OFFSET
          IAMNT = TRABUF(TWAMT)*TRABUF(TWDUR) ! TOTAL AMOUNT
          IJKAMNT = TRABUF(TWKAMT)*TRABUF(TWDUR) ! JOKER AMOUNT
          IAGENT = TRABUF(TAGT) ! AGENT NUMBER
          CALL DISTIM(TRABUF(TTIM),IHOUR,IMIN,ISEC)

          WRITE(LUN,902) ITYPE,ISUBTYPE,ISERIAL,IAMNT,IJKAMNT,IAGENT,IHOUR,IMIN,ISEC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                             V A L I D A T I O N
C                               M E S S A G E S
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ELSE IF((TRABUF(TTYP) .EQ. TVAL) .OR. ((TRABUF(TTYP) .EQ. TEUR) .AND. (TRABUF(TEUTYP) .EQ. TVAL)) .OR.
     *          (TRABUF(TTYP) .EQ. TREF)) THEN

C          TYPE*,' INFO: WRITING VALIDATION TRANSACTION...'

          IVTYPE = TVAL ! TRANSACTION TYPE
C
C       VALIDATION SUBTYPE - 0 = PRIZE PAYMENT, 16 = PRIZE VALIDATION
C
          IF((TRABUF(TSTAT) .EQ. GOOD) .AND. (TRABUF(TERR) .EQ. NOER)) THEN
            ! GET THE VALUE OF CASH AMOUNT:
            ! IF VALUE GREATER THAN ZERO THEN
            !    PRIZE PAYMENT
            ! ELSE
            !    PRIZE VALIDATION
            ! ENDIF
CV04            IF(TRABUF(TGAMTYP) .EQ. TEUM) THEN
            IF(TRABUF(TGAMTYP) .EQ. TEUM .OR. TRABUF(TGAMTYP) .EQ. TRAF) THEN   !V04
              IF(TRABUF(TEUVSBT) .NE. 15) THEN
                CSHAMTL = TRABUF(TEUVCAM) ! CASH AMOUNT
                CSHAMTH = TRABUF(TEUVCAMH) ! CASH AMOUNT HIGH HALF WORD
              ELSE
                CSHAMTL = 0
                CSHAMTH = 0
              ENDIF
            ELSE IF(TRABUF(TGAMTYP) .EQ. TPAS) THEN
              CSHAMTL = 0
              CSHAMTH = 0
              DO TCKS = 1, TRABUF(TPTCK) ! TRABUF(TPTCK) NUMBER OF TICKETS TO VALIDATE (cfr. DESTRA.DEF)
                IF(TRABUF(TPSTS1+OFFTRA*(TCKS-1)).EQ.VWINNER) THEN
                  CSHAMTL = CSHAMTL + TRABUF(TPPAY1+OFFTRA*(TCKS-1)) !TPPAY1 - PRIZE PAID 1 (cfr. DESTRA.DEF)
                  !# CALL OPS('CMONY:',CMONY(CSHAMTL,11,VALUNIT),CSHAMTL)
C                  TYPE*,'CMONY: ', CMONY(CSHAMTL,11,VALUNIT)
                ENDIF
              ENDDO
            ELSE
              CSHAMTL = TRABUF(TVPAY) + TRABUF(TVKPAY)
              CSHAMTH = 0
            ENDIF
          ENDIF

          IF(TRABUF(TTYP) .EQ. TEUR) THEN
C----+------------------------------------------------------------------
C V03| Added support to new validation sub-types
C----+------------------------------------------------------------------
C             IF((TRABUF(TEUVSBT) .EQ. 1) .OR. (TRABUF(TEUVSBT) .EQ. 15)) THEN
C                 I4TEMP = 16 ! PRIZE VALIDATION
C                 CSHAMTL = 0
C                 CSHAMTH = 0
C             ELSE
C                 I4TEMP = 0 ! PRIZE PAYMENT
C             ENDIF
             ! Validations
             IF( (TRABUF(TEUVSBT) .EQ. 1) 
     *       .OR.(TRABUF(TEUVSBT) .EQ. 6) 
     *       .OR.(TRABUF(TEUVSBT) .EQ. 7) 
     *       .OR.(TRABUF(TEUVSBT) .EQ. 10) 
     *       .OR.(TRABUF(TEUVSBT) .EQ. 15)
     *       ) THEN
                 I4TEMP = 16 ! PRIZE VALIDATION
                 CSHAMTL = 0
                 CSHAMTH = 0
             ! Prize payments
             ELSEIF(
     *           (TRABUF(TEUVSBT) .EQ. 0) 
     *       .OR.(TRABUF(TEUVSBT) .EQ. 8) 
     *       .OR.(TRABUF(TEUVSBT) .EQ. 9) 
     *       ) THEN
                 I4TEMP = 0 ! PRIZE PAYMENT
             ENDIF
C----+------------------------------------------------------------------
C V03| Added support to new validation sub-types
C----+------------------------------------------------------------------
          ELSE
          	IF((TRABUF(TERR) .EQ. VINQ) .OR. (TRABUF(TERR) .EQ. INVL) .OR. ((CSHAMTL .EQ. 0) .AND. (CSHAMTH .EQ. 0))) THEN
              I4TEMP = 16 ! PRIZE VALIDATION
              CSHAMTL = 0
             	CSHAMTH = 0
            ELSE
          	  I4TEMP = 0 ! PRIZE PAYMENT
          	ENDIF
          ENDIF
          IVSUBTYPE = I4TEMP ! VALIDATION SUBTYPE

          IF(TRABUF(TTYP) .EQ. TEUR) THEN
C----+---+-------------+------------------------------------------------
C V04|BEG| M16 PROJECT | ADD M1LHAO GAME
C----+---+-------------+------------------------------------------------
CV04            I4TEMP = GAMEUR !
            IF(TRABUF(TGAMTYP) .EQ. TEUM) THEN                                  
              I4TEMP = GAMEUR                                                   !EUROMILLIONS GAME NUMBER
            ELSEIF(TRABUF(TGAMTYP) .EQ. TRAF) THEN
              I4TEMP = GAMSM                                                    !M1LHAO GAME NUMBER
            ENDIF
C----+---+-------------+------------------------------------------------
C V04|END| M16 PROJECT | ADD M1LHAO GAME
C----+---+-------------+------------------------------------------------
          ELSE
            I4TEMP = TRABUF(TGAM)
          ENDIF
          IVGAM = I4TEMP ! GAME NUMBER

          IVSERIAL = SEROFFSET ! SERIAL NUMBER

          IVAMNT(2) = CSHAMTH
          IVAMNT(1) = CSHAMTL

          IVAGENT = TRABUF(TAGT) ! AGENT NUMBER
          CALL DISTIM(TRABUF(TTIM),IVHOUR,IVMIN,IVSEC) ! HOUR MIN SEC

          WRITE(LUN,903) IVTYPE, IVSUBTYPE, IVGAM, IVSERIAL, I8VAMNT, IVAGENT, IVHOUR, IVMIN, IVSEC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                             S I G N  O N / OFF
C                               M E S S A G E S
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ELSE IF(TRABUF(TTYP) .EQ. TSPE) THEN

C          TYPE*,' INFO: WRITING SIGN ON/OFF TRANSACTION...'

          ISTYPE = TRABUF(TTYP) ! TRANSACTION TYPE
          ISSUBTYPE = TRABUF(TSFUN) ! TRANSACTION SUBTYPE
          ISSERIAL = SEROFFSET ! SERIAL OFFSET
          ISAGENT = TRABUF(TAGT) ! AGENT NUMBER
          ISTER= TRABUF(TTER) ! TERMINAL NUMBER
          ISCDC = TRABUF(TCDC)
          CALL DISTIM(TRABUF(TTIM), ISHOUR, ISMIN, ISSEC)
          LEN=1
          WRITE(LUN,904) ISTYPE, ISSUBTYPE, ISSERIAL, ISAGENT, ISTER, ISCDC, ISHOUR, ISMIN, ISSEC

        ENDIF
C
C FORMAT STATEMENTS
C
902     FORMAT(I0,'|',I0,'|',I0,'|',I0,'|',I0,'|',I0,'|',I0,'|',I0,'|',I0) ! Wager/Cancellation transaction print format
903     FORMAT(I0,'|',I0,'|',I0,'|',I0,'|',I0,'|',I0,'|',I0,'|',I0,'|',I0) ! Validation/Prize Payment transaction print format
904     FORMAT(I0,'|',I0,'|',I0,'|',I0,'|',I0,'|',I0,'|',I0,'|',I0,'|',I0) ! Sign on/off transaction print format

        END

C**************************************************
C SUBROUTINE: GET WAGER, CANCELLATION, VALIDATION
C             AND SIGN-ON TRANSACTIONS FROM TMF OF
C             THE FOLLOWING GAMES:
C
C      GAME NUMBER       GAME NAME
C          1          Totobola Normal
C          5          Joker
C          6          Totoloto Sábado
C          7          Totoloto Quarta
C          8          Lotaria Clássica
C          9          Lotaria Popular
C         10          Totobola Extra 1
C         11*         Euromilhões
C         12*         M1lhão
C
C * The game numbers 11 and 12 were the next game numbers available.
C It was defined to identify Euromillions and M1lhão games.
C
C INPUT:
C       SEROFFSET - SERIAL NUMBER
C
C OUTPUT:
C       TRABUF - TRANSACTION
C       SEROFFSET - SERIAL OFFSET
C       EOT - END OF FILE FLAG INDICATOR
C**************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE GETTRABUF(TRABUF,SEROFFSET,EOT)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'

        INTEGER*4 LOGREC(LREC*3), ST, DUMMY, EOTCNT, SEROFFSET, SER
        LOGICAL EOT
        DATA EOTCNT/0/

C       GAME NUMBERS
        INTEGER*4 GAMTBLANRML
        PARAMETER (GAMTBLANRML = 1)  ! TOTOBOLA NORMAL
        INTEGER*4 GAMKIK
        PARAMETER (GAMKIK = 5)       ! JOKER
        INTEGER*4 GAMTLTOSAT
        PARAMETER (GAMTLTOSAT = 6)   ! TOTOLOTO SÁBADO
        INTEGER*4 GAMTLTOWED
        PARAMETER (GAMTLTOWED = 7)   ! TOTOLOTO QUARTA
        INTEGER*4 GAMPAS1
        PARAMETER (GAMPAS1 = 8)      ! LOTARIA CLÁSSICA
        INTEGER*4 GAMPAS2
        PARAMETER (GAMPAS2 = 9)      ! LOTARIA POPULAR
        INTEGER*4 GAMTBLAEXT1
        PARAMETER (GAMTBLAEXT1 = 10) ! TOTOBOLA EXTRA 1
        INTEGER*4 GAMEUR
        PARAMETER (GAMEUR = 11)      ! EUROMILLIONS
C
        INTEGER*4 GAMSM                                                         !V04
        PARAMETER (GAMSM = 12)                                                  !M1LHAO !V04
C
        EOT = .FALSE.
        SER = SEROFFSET

10      CONTINUE
        SER = SER + 1
        CALL RLOG(SER, LOGREC, DUMMY, ST)

        IF(ST.GT.0) GOTO 10
        IF(ST.LT.0) THEN
          EOT=.TRUE.
          RETURN
        ENDIF
C
C CHECK FOR END OF FILE
C
        IF(LOGREC(1) .EQ. 0) THEN
          EOTCNT = EOTCNT + 1
          IF(EOTCNT.GT.5000) THEN
            EOT = .TRUE.
CV04            TYPE*, ' INFO: END OF FILE REACHED! WAITING 10 SECS BEFORE CONTINUE...'
            RETURN
          ELSE
            GOTO 10
          ENDIF
        ENDIF

        CALL LOGTRA(TRABUF, LOGREC)
        EOTCNT=0
!        TYPE*,' INFO: NEW TRX READ FROM TMF!'
!        TYPE*,'DEBUG: TRABUF(TTYP) = ',TRABUF(TTYP)

        IF((TRABUF(TTYP) .EQ. TEUR) .AND. ((TRABUF(TEUTYP) .EQ. TWAG) .OR. (TRABUF(TEUTYP) .EQ. TCAN))) THEN
!          TYPE*,' INFO: EUR WAG/CAN SKIPPED!'
          SEROFFSET = SER
          GOTO 10
        ENDIF

        ! WAGER/CANCELLATION
        IF((TRABUF(TTYP) .EQ. TWAG) .OR. (TRABUF(TTYP) .EQ. TCAN)) THEN
          IF(TRABUF(TERR) .EQ. NOER) THEN
            IF(((TRABUF(TGAMTYP) .EQ. TLTO) .AND. ((TRABUF(TGAM) .EQ. GAMTLTOWED) .OR. (TRABUF(TGAM) .EQ. GAMTLTOSAT))) .OR.
     *         ((TRABUF(TGAMTYP) .EQ. TKIK) .AND.  (TRABUF(TGAM) .EQ. GAMKIK)) .OR.
     *         ((TRABUF(TGAMTYP) .EQ. TPAS) .AND. (TRABUF(TWEPOP) .NE. EPASRES) .AND. (TRABUF(TWEPOP) .NE. EPASREL) .AND.
     *         ((TRABUF(TGAM) .EQ. GAMPAS1) .OR. (TRABUF(TGAM) .EQ. GAMPAS2))) .OR.
     *         ((TRABUF(TGAMTYP) .EQ. TSPT) .AND. ((TRABUF(TGAM) .EQ. GAMTBLANRML) .OR. (TRABUF(TGAM) .EQ. GAMTBLAEXT1)))) THEN

              SEROFFSET = SER

C              TYPE*,' INFO: NEW TRX FOUND!'
C              TYPE*,'DEBUG: TERR',TRABUF(TERR)
C              TYPE*,'DEBUG: TTYP',TRABUF(TTYP)
C              TYPE*,'DEBUG: TGAMTYP',TRABUF(TGAMTYP)
C              TYPE*,'DEBUB: TGAM',TRABUF(TGAM)
C              TYPE*,'DEBUG: TSTAT',TRABUF(TSTAT)
C              TYPE*,'DEBUG: SER',SER

              RETURN
            ENDIF
          ENDIF
        ! VALIDATION/PAYMENT
        ELSE IF((TRABUF(TSTAT) .NE. SUPR) .AND. ((TRABUF(TTYP) .EQ. TVAL) .OR. (TRABUF(TTYP) .EQ. TREF))) THEN
          SEROFFSET = SER

C          TYPE*,' INFO: NEW TRX FOUND!'
C          TYPE*,'DEBUG: TERR',TRABUF(TERR)
C          TYPE*,'DEBUG: TTYP',TRABUF(TTYP)
C          TYPE*,'DEBUG: TGAMTYP',TRABUF(TGAMTYP)
C          TYPE*,'DEBUB: TGAM',TRABUF(TGAM)
C          TYPE*,'DEBUG: TSTAT',TRABUF(TSTAT)
C          TYPE*,'DEBUG: SER',SER

          RETURN
        ! EM VALIDATION/PAYMENT
        ELSE IF((TRABUF(TTYP) .EQ. TEUR) .AND. (TRABUF(TEUTYP) .EQ. TVAL)) THEN
C          TYPE*, '  TRABUF(TSTAT): ', TRABUF(TSTAT)
C          TYPE*, '   TRABUF(TERR): ', TRABUF(TERR)
C          TYPE*, 'TRABUF(TGAMTYP): ', TRABUF(TGAMTYP) ! TRABUF(TEUVSBT)
C----+------------------------------------------------------------------
C V03| Added support to new validation sub-types
C----+------------------------------------------------------------------
C         IF((TRABUF(TSTAT) .EQ. GOOD) .AND. ((TRABUF(TEUVSBT) .EQ. 1) .OR. (TRABUF(TERR) .EQ. NOER)) .AND.
C    *        (TRABUF(TGAMTYP) .EQ. TEUM)) THEN
          IF(  (TRABUF(TSTAT)   .EQ. GOOD) 
     *    .AND.    ((TRABUF(TEUVSBT) .EQ. 1) 
     *         .OR. (TRABUF(TEUVSBT) .EQ. 6) 
     *         .OR. (TRABUF(TEUVSBT) .EQ. 7) 
     *         .OR. (TRABUF(TEUVSBT) .EQ. 8) 
     *         .OR. (TRABUF(TEUVSBT) .EQ. 9) 
     *         .OR. (TRABUF(TEUVSBT) .EQ. 10) 
     *         .OR. (TRABUF(TERR) .EQ. NOER)) 
CV04     *    .AND.(TRABUF(TGAMTYP) .EQ. TEUM)) THEN
     *    .AND.    ((TRABUF(TGAMTYP) .EQ. TEUM)                                 !V04 
     *         .OR. (TRABUF(TGAMTYP) .EQ. TRAF))) THEN                          !V04
C----+------------------------------------------------------------------
C V03| Added support to new validation sub-types
C----+------------------------------------------------------------------

C            TYPE*,' INFO: EM VALIDATION FOUND!'
C            TYPE*,'DEBUG: TERR',TRABUF(TERR)
C            TYPE*,'DEBUG: TTYP',TRABUF(TTYP)
C            TYPE*,'DEBUG: TTYP',TRABUF(TEUTYP)
C            TYPE*,'DEBUG: SERIAL',SER

            SEROFFSET = SER
            RETURN
          ENDIF
      ! SIGN ON/OFF
        ELSE IF((TRABUF(TERR) .EQ. NOER) .AND. (TRABUF(TTYP) .EQ. TSPE) .AND.
     *    ((TRABUF(TSFUN) .EQ. TSON) .OR. (TRABUF(TSFUN) .EQ. TSOFF))) THEN
C            TYPE*,' INFO: SIGN OF/OFF FOUND!'
C            TYPE*,'DEBUG: TERR',TRABUF(TERR)
C            TYPE*,'DEBUG: TTYP',TRABUF(TTYP)
C            TYPE*,'DEBUG: TSFUN',TRABUF(TSFUN)
C            TYPE*,'DEBUG: SERIAL',SER
            SEROFFSET = SER
            RETURN
        ELSE
        	SEROFFSET = SER ! SKIP THE TRANSACTION
        ENDIF

        GOTO 10
        END

C**************************************************
C SUBROUTINE: GETS THE HOUR, MIN AND SEC GIVEN THE
C             TOTAL NUMBER OF SECONDS
C INPUT:
C        NUM - NUMBER OF SECONDS TO CONVERT
C
C OUTPUT:
C        HR - HOUR
C        MIN - MINUTE
C        SEC - SECONDS
C**************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE DISTIM(NUM,HR,MIN,SEC)
        IMPLICIT NONE
C
        INTEGER*4   NUM, HR, MIN, SEC
C
        SEC = NUM
        IF(SEC.GT.'40000000'X) SEC = SEC - '40000000'X
        HR = SEC / 3600
        SEC = SEC - (HR * 3600)
        MIN = SEC / 60
        SEC = SEC - (MIN * 60)
        IF(HR.GT.99) HR = 99

        RETURN
        END
