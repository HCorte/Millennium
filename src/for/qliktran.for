C
C      QLIKTRAN.FOR
C
C      PROGRAM QLIKTRAN
C
C      THIS PROGRAM GENERATES A CSV FILE, QLIK_YYYYMMDD.CSV, CONTAINING
C      THE HEADER FIELDS OF ALL TRANSACTIONS THAT ARE RECORDED IN THE FILE 
C      PRIM:MTMF01.FIL. THE HEADER FIELDS ARE SEPARATED BY THE SEMICOLON 
C      CHARACTER (;), WHICH CAN BE CONFIGURED IN THE DEFINITION FILE 
C      QLIKTRAN.DEF.
C
C      SOURCE FILE: PRIM:MTMF01.FIL
C      OUTPUT FILE: GXTSK:QLIK_YYYYMMDD.CSV, WHERE YYYYMMDD IS THE DATE OF
C                   THE DATA PROCESSED.
C
C V02 14-OCT-2016 SCML 1. Consideration as "EM Validation Inquiry" EM 
C                         validation transactions whose value of validation 
C                         type is 15 (Validation Error).
C                      2. Transaction error, TRABUF(TERR), for an "EM 
C                         Validation Inquiry" transaction is mapped to 
C                         VINQ*1000 + TRABUF(TEUVST)
C                      3. Created EURPAY transaction type for "EM Prize payment"
C V01 07-MAR-2016 SCML INITIAL RELEASE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2016 DJ - SCML. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C==== OPTIONS /CHECK = NOOVERFLOW /EXT
       PROGRAM QLIKTRAN
       IMPLICIT NONE
C
C INCLUDES DEFINITION TO RUN FOR ON LINE TRANSACTIONS
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:CONCOM.DEF'
       INCLUDE 'INCLIB:DESTRA.DEF'
       INCLUDE 'INCLIB:PRMLOG.DEF'
       INCLUDE 'INCLIB:GSALES.DEF'
       INCLUDE 'INCLIB:QLIKTRAN.DEF'
C
C CONSTANT PARAMETERS TO RUN FOR ON LINE TRANSACTIONS
C
       INTEGER * 4 VERSION                                                      !TRANSACTION FILE LOADER VERSION
C
C INITIATE CONSTANT PARAMETERS TO RUN  FOR ON LINE TRANSACTIONS
C
       PARAMETER(VERSION = 1)                                                   !TRANSACTION FILE LOADER VERSION
C
C VARIABLES DEFINITION TO RUN FOR ON LINE TRANSACTIONS
C
       INTEGER*4 I
       INTEGER*4 MTMFIDFIL                                                      !MTMF  IDENTIFICATION FILE
       INTEGER*4 MTMSERNUM                                                      !MTMF TRANSACTION SERIAL NUMBER
       INTEGER*4 LOGREC(LREC * 3)                                               !LOGGER REGISTER
       CHARACTER*24 FILENAME                                                    !FILE NAME
       LOGICAL EOF                                                              !END OF FILE
C
C DISPLAY USER INFORMATION ( RUNNING QLIKTRAN TRANSACTION LOADER )
C
       TYPE *, IAM()
       TYPE *, IAM(), 'Copyright 2016 SCML. All rights reserved.'
       TYPE *, IAM()
       TYPE *, IAM(), 'Running QLIKTRAN On Line Transactions Loader'
       TYPE *, IAM()
C
       CALL FASTSET(0, QLIKDATA, SIZEOF(QLIKDATA)/4)                            !CLEAR DATA STRUCTURE
C
C GET FREE IDENTIFICATION FILE NUMBER FOR TRANSACTION FILE ( MTMF01.FIL )
C
       MTMFIDFIL = GET_FREE_IDFIL(1)
C
C OPEN TRANSACTION MASTER FILE NAME ( MTMF01.FIL )
C
       CALL OPEN_SYS_FILE(MTMFIDFIL, MTMFIL, PTMF)
C
C GET QLIK TRANSACTION FILE NAME QLIK_YYYYMMDD.CSV
C
       WRITE(FILENAME, 101) GET_YYYYMMDD_CDC(DAYCDC)
       QLIKDATA.QLIKFILNAM=FILENAME
C
C IF TRANSACTION FILE EXIST, DELETE IT
C
       CALL DELETE_FILE_NAME(FILENAME)
C
C GET FREE IDENTIFICATION FILE NUMBER FOR QLIK FILE
C
       IDFIL = GET_FREE_IDFIL(0)
C
C OPEN QLIK FILE
C
       CALL OPEN_FILE_NAME(IDFIL, FILENAME)
       QLIKDATA.QLIK_LUN=IDFIL
C
C WRITE QLIK HEADER FIELD NAMES INTO QLIK FILE
C
!       DO I = 1, QLIK_HFTOT-1
!         WRITE(QLIKDATA.QLIK_LUN,111,ADVANCE='NO') TRIM(QLIK_HFNAM(I)), FS
!       ENDDO
!       WRITE(QLIKDATA.QLIK_LUN,112,ADVANCE='YES') TRIM(QLIK_HFNAM(I))
C
C READ TRANSACTION FROM MASTER FILE
C
       MTMSERNUM = 1                                                            !READ FILE FROM THE BEGINNING
2000   CONTINUE
       EOF = .FALSE.
       CALL READTMF(LOGREC, MTMSERNUM, EOF)
       IF(EOF) GOTO 1000
C
C CHECK IF TRANSACTION SHOULD BE PRINTED, IF NOT, GO TO READ NEXT TRANSACTION
C
       CALL LOGTRA(TRABUF, LOGREC)
       IF(TRABUF(TTIM) .LT. 0) GOTO 2000
       IF(TRABUF(TSER) .LT. 1) GOTO 2000
C
C SET TRANSACTION TYPE
C
       TRNTYP = TRABUF(TTYP)
C
C WRITE ON LINE TRANSACTION IN QLIK FILE
C
       IF(TRNTYP.EQ.TEUR) THEN
         CALL PRINT_TEUR(QLIKDATA,TRABUF)
         GOTO 2000
       ELSEIF(TRNTYP.EQ.TWAG) THEN
         CALL PRINT_TWCI(QLIKDATA,TRABUF)
         GOTO 2000
       ELSEIF(TRNTYP.EQ.TIGS) THEN
         CALL PRINT_TIGS(QLIKDATA,TRABUF)
         GOTO 2000
       ELSEIF(TRNTYP.EQ.TCAN) THEN
         CALL PRINT_TWCI(QLIKDATA,TRABUF)
         GOTO 2000
       ELSEIF(TRNTYP.EQ.TINC) THEN
         CALL PRINT_TWCI(QLIKDATA,TRABUF)
         GOTO 2000
!       ELSEIF((TRNTYP.EQ.TVAL .OR. TRNTYP.EQ.TREF) .AND.
!     *          TRABUF(TGAMTYP).EQ.TPAS) THEN
!     *          1 .eq.1 ) THEN
!         CALL PRINT_TVAL(QLIKDATA,TRABUF)
!         GOTO 2000
       ELSEIF(TRNTYP.EQ.TVAL .OR. TRNTYP.EQ.TREF) THEN
         CALL PRINT_TVAL(QLIKDATA,TRABUF)
         GOTO 2000
       ELSEIF(TRNTYP.EQ.TSPE) THEN
         CALL PRINT_TSPE(QLIKDATA,TRABUF)
         GOTO 2000
       ELSEIF(TRNTYP.EQ.TCMD) THEN
         CALL PRINT_TCMD(QLIKDATA,TRABUF)
         GOTO 2000
       ELSEIF(TRNTYP.EQ.TCRS) THEN
         CALL PRINT_TCRS(QLIKDATA,TRABUF)
         GOTO 2000
       ELSEIF(TRNTYP.EQ.TRET) THEN
         CALL PRINT_TRET(QLIKDATA,TRABUF)
         GOTO 2000
       ELSE
         CALL PRINT_OTHER(QLIKDATA,TRABUF)
         GOTO 2000 !GO TO READ NEXT TRANSACTION TO MTMF01.FIL FILE
       ENDIF
       GOTO 2000
C
C END FOR READ TRANSACTION MASTER FILE ( MTMF01.FIL )
C
1000   CONTINUE
C
C CLOSE ALL FILES
C
       CALL USRCLOS1(IDFIL)
       CALL USRCLOS1(MTMFIDFIL)
C
C DISPLAY MESSAGE TO USER
C
       TYPE *, IAM()
       TYPE *, IAM(), 'Generated Transaction QLIK File: ', FILENAME
       TYPE *, IAM()
C
C QLIK TRANSACTION LOADER ENDS OK
C
       CALL GSTOP(GEXIT_SUCCESS)
C
C FORMATS DEFINITION TO RUN QLIK LOADER FOR ON LINE TRANSACTIONS
C
101    FORMAT('GXTSK:QLIK_',A8,'.CSV')
111    FORMAT(A,A1)
112    FORMAT(A)
C
C THIS IS THE END TO RUN QLIK LOADER FOR ON LINE TRANSACTIONS
C
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_TEUR (QLIKDATA,TRABUF)
C
C       THIS SUBROUTINE WRITES EUROMILLIONS TRANSACTIONS INTO QLIK FILE.
C
C       INPUTS:
C        QLIKDATA       QLIK DATA STRUCTURE
C        TRABUF         EUR TRANSACTION TO WRITE INTO THE FILE
C
C       OUTPUTS:
C        *NONE*
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE PRINT_TEUR(QLIKDATA,TRABUF)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSPARAM.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:CONCOM.DEF'
       INCLUDE 'INCLIB:DESTRA.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:QLIKTRAN.DEF'
       INCLUDE 'INCLIB:AGTCOM.DEF'
C
       INTEGER*4 LUN, ST
C
       INTEGER*4 I
       INTEGER*4 JUL
       INTEGER*2 DBUF(12)
       CHARACTER*8 CTTIM
       REAL*8      RTTIM
       EQUIVALENCE (CTTIM, RTTIM)
C
       INTEGER*4 PRZAMT(2)                                                      !PRIZE AMOUNT
       INTEGER*8 PRZAMTI8
       REAL*8    PRZAMTR8
       EQUIVALENCE (PRZAMT,PRZAMTI8)
C
       INTEGER*4 NPZAMT(2)                                                      !NET PRIZE AMOUNT
       INTEGER*8 NPZAMTI8
       REAL*8    NPZAMTR8
       EQUIVALENCE (NPZAMT,NPZAMTI8)
C
       INTEGER*8 TRXAMTI8
       REAL*8    TRXAMTR8
C
       INTEGER*4  NIB(6)
       CHARACTER*24 CNIB
       EQUIVALENCE (NIB,CNIB)
       INTEGER*4  ERRNIB(32)
       CHARACTER*128 CERRNIB
       EQUIVALENCE (ERRNIB,CERRNIB)
       INTEGER*4  BLANK
C
       CHARACTER*11 C11MONY
       CHARACTER*8 IAGT_NO                                                      !EXTERNAL FUNCTION
       CHARACTER*8 GET_YYYYMMDD_CDC                                             !EXTERNAL FUNCTION
C
       INTEGER*4 VST_NOEXCHTKT                                                  !NO EXCHANGE TICKET !V02
       PARAMETER(VST_NOEXCHTKT = 10)                                            !V02
       INTEGER*4 VERR                                                           !VALIDATION ERROR SUBTYPE !V02
       PARAMETER(VERR = 15)                                                     !V02
       INTEGER*4 EURVINQ                                                        !EUR VALIDATION INQUIRY TRANSACTION TYPE !V02
       PARAMETER(EURVINQ = TVAL)                                                !V02
       INTEGER*4 EURPAY                                                         !EUR PAYMENT TRANSACTION TYPE !V02
       PARAMETER(EURPAY = EURVINQ + 1)                                          !V02
       INTEGER*4 PORTALSAP                                                      !PORTAL SAP CODE !V02
       PARAMETER (PORTALSAP = 007456)                                           !V02
C
       DBUF(VCDC)=TRABUF(TCDC)
       CALL CDATE(DBUF)
       JUL=DBUF(VJUL) + 500
C
C----+---+--------------------------------------------------------------
C V02|BEG| SEPARATION OF VALIDATION INQUIRY TRX FROM PRIZE PAYMENT TRX
C----+---+--------------------------------------------------------------
CV02       IF(TRABUF(TEUTYP) .EQ. TVAL) THEN
CV02         IF(TRABUF(TEUVSBT) .EQ. VMID .OR.
CV02     *     TRABUF(TEUVSBT) .EQ. VNREG .OR.
CV02     *     TRABUF(TEUVSBT) .EQ. VNINQ .OR.
CV02     *     TRABUF(TEUVSBT) .EQ. VNIBO) THEN
CV02           TRABUF(TERR) = VINQ
CV02         ENDIF
CV02       ENDIF
       IF(TRABUF(TEUTYP) .EQ. TVAL) THEN
         IF(AGTSAP(TRABUF(TTER)) .NE. PORTALSAP) THEN                           !TERMINAL FROM RETAILER NETWORK
           IF(TRABUF(TEUVSBT) .EQ. VERR  .OR.                                   !VALIDATION ERROR SUBTYPE (IT IS BEING ASSUMED THAT ALL VERR OCCURS ONLY WITH VALIDATION INQUIRIES)
     *       TRABUF(TEUVSBT)  .EQ. VNREG .OR.                                   !NEW VALIDATION INQUIRY CASH PAYMENT RESPONSE (VIRTUAL/PHYSICAL PRIVILEGED AND NON-PRIVILEGED TERMINALS)
     *       TRABUF(TEUVSBT)  .EQ. VNINQ .OR.                                   !NEW VALIDATION INQUIRY CASH PAYMENT OR BANK TRANSFER RESPONSE (VIRTUAL/PHYSICAL PRIVILEGED TERMINALS)
     *       TRABUF(TEUVSBT)  .EQ. VNIBO .OR.                                   !NEW VALIDATION INQUIRY BANK TRANSFER ONLY RESPONSE (VIRTUAL/PHYSICAL NON-PRIVILEGED TERMINALS)
     *       TRABUF(TEUVSBT)  .EQ. VREG) THEN                                   !IT OCCURS, E.G., WHEN VALIDATION FUNCTION IS SUPRESSED IN MILLENNIUM
             TRABUF(TERR)   = VINQ*1000 + TRABUF(TEUVST)
             TRABUF(TEUTYP) = EURVINQ                                           !EUR VALIDATION INQUIRY TRANSACTION TYPE (RETAILER)
           ELSE
C            VNDON !NEW VALIDATION CASH PAYMENT RESPONSE/REQUEST (PHYSICAL TERMINALS ONLY)
C            VNBNK !NEW VALIDATION BANK TRANSFER RESPONSE/REQUEST (PHYSICAL TERMINALS ONLY)
             TRABUF(TEUTYP) = EURPAY                                            !EUR PAYMENT TRANSACTION TYPE (RETAILER)
           ENDIF
         ELSE                                                                   !PORTAL TERMINAL
           IF(TRABUF(TEUVSBT) .EQ. VNREG .OR.                                   !NEW VALIDATION INQUIRY CASH PAYMENT RESPONSE (VIRTUAL/PHYSICAL PRIVILEGED AND NON-PRIVILEGED TERMINALS)
     *       TRABUF(TEUVSBT)  .EQ. VNINQ .OR.                                   !NEW VALIDATION INQUIRY CASH PAYMENT OR BANK TRANSFER RESPONSE (VIRTUAL/PHYSICAL PRIVILEGED TERMINALS)
     *       TRABUF(TEUVSBT)  .EQ. VNIBO .OR.                                   !NEW VALIDATION INQUIRY BANK TRANSFER ONLY RESPONSE (VIRTUAL/PHYSICAL NON-PRIVILEGED TERMINALS)
     *       (TRABUF(TEUVSBT) .EQ. VREG .AND.                                   !IT OCCURS, E.G., WITH VALIDATION FUNCTION SUPRESSED IN MILLENNIUM
     *        TRABUF(TEUVST)  .NE. VST_NOEXCHTKT) .OR.
     *       TRABUF(TEUVSBT)  .EQ. VERR) THEN                                   !VALIDATION ERROR SUBTYPE (IT IS BEING ASSUMED THAT ALL VERR OCCURS ONLY WITH VALIDATION INQUIRIES)
             TRABUF(TERR)   = VINQ*1000 + TRABUF(TEUVST)
             TRABUF(TEUTYP) = EURVINQ                                           !EUR VALIDATION INQUIRY TRANSACTION TYPE (PORTAL)
           ELSE
C            VMID !OLD VALIDATION CASH PAYMENT/BANK TRANSFER REQUEST, WHICH OCCURS, E.G., WITH VALIDATION FUNCTION SUPRESSED IN MILLENNIUM (VIRTUAL TERMINALS ONLY)
C            VREG AND TEUVST = VST_NOEXCHTKT  !OLD VALIDATION CASH PAYMENT BANK TRANSFER RESPONSE (VIRTUAL TERMINALS ONLY)
             TRABUF(TEUTYP) = EURPAY                                            !EUR PAYMENT TRANSACTION TYPE (PORTAL)
           ENDIF
         ENDIF
       ENDIF
C----+---+--------------------------------------------------------------
C V02|END| SEPARATION OF VALIDATION INQUIRY TRX FROM PRIZE PAYMENT TRX
C----+---+--------------------------------------------------------------
C
C EUROMILLIONS TRANSACTION HEADER
       WRITE(QLIKDATA.QLIK_LUN,1000,ADVANCE=WRITE__ADVANCE_VALUE)
     *           TRABUF(TSTAT),                                                 !TEUR.1     STATUS
     *       FS, TRABUF(TERR),                                                  !TEUR.2     ERROR CODE
     *       FS, GET_YYYYMMDD_CDC(TRABUF(TCDC)),                                !TEUR.3     TRANSACTION DATE
     *       FS, TRABUF(TSER),                                                  !TEUR.4     INTERNAL SERIAL NUMBER
     *       FS, DISTIM(TRABUF(TTIM)),                                          !TEUR.5     TIME STAMP HH:MI:SS
     *       FS, TRABUF(TTER),                                                  !TEUR.6     TERMINAL NUMBER
     *       FS, IAGT_NO(TRABUF(TAGT)),                                         !TEUR.7     AGENT NUMBER
     *       FS, AGTSAP(TRABUF(TTER)),                                          !TEUR.8     SAP NUMBER
     *       FS, TRABUF(TTRN),                                                  !TEUR.9     TRANSACTION SEQUENCE NUMBER
     *       FS, TRABUF(TTYP),                                                  !TEUR.10    TRANSACTION TYPE
     *       FS, TRABUF(TGAM),                                                  !TEUR.11    GAME NUMBER
     *       FS, TRABUF(TGAMTYP),                                               !TEUR.12    GAME TYPE
     *       FS, TRABUF(TGAMIND),                                               !TEUR.13    GAME INDEX
     *       FS, TRABUF(TTSTCS),                                                !TEUR.14    TERMINAL STATISTICS
     *       FS, TRABUF(TINTRA),                                                !TEUR.15    INTERNAL TRANSACTION FLAG
     *       FS, TRABUF(TFIL),                                                  !TEUR.16    FILE STATUS
     *       FS, TRABUF(TTKID),                                                 !TEUR.17    TICKET ID
     *       FS, TRABUF(TCHK),                                                  !TEUR.18    MESSAGE CHECKSUM
     *       FS, TRABUF(TFRAC),                                                 !TEUR.19    # OF FRACTIONS
     *       FS, TRABUF(TSIZE),                                                 !TEUR.20    TRANSACTION SIZE (# LOG RECS)
     *       FS, TRABUF(TSUBERR),                                               !TEUR.21    ERROR SUB CODE
     *       FS, TRABUF(TCDC_SOLD),                                             !TEUR.22    CDC TRANS. WAS SOLD (NEVER CHANGES)
     *       FS, TRABUF(TFAMTFLG),                                              !TEUR.23    BET AMOUNT FLAG (FOR FRACTIONS)
     *       FS, TRABUF(TNFRAC),                                                !TEUR.24    REAL NUMBER OF FRACTIONS
     *       FS, TRABUF(TWADDFW),                                               !TEUR.25    PROMOTION - ADD 1 FREE WEEK
     *       FS, TRABUF(TEUTYP)                                                 !TEUR.26    EUROMILLIONS TRANSACTION TYPE
       IF(TDETAIL) THEN
         IF(TRABUF(TEUTYP) .EQ. TWAG) THEN
C EUROMILLIONS WAGER DETAIL
           IF(TRABUF(TSTAT) .EQ. REJT .AND. TRABUF(TERR) .EQ. INVL) THEN        !CLEAN VALUES
             TRABUF(TEUSER)    = 0
             TRABUF(TEUCHK)    = 0
             TRABUF(TEUWTIMEH) = 0
             TRABUF(TEUWTIMEM) = 0
             TRABUF(TEUWTIMES) = 0
             TRABUF(TEUMESSQ)  = 0
           ENDIF
C
           WRITE(QLIKDATA.QLIK_LUN,1100,ERR=1110,IOSTAT=ST)
     *           FS, JUL,                                                       !TEUR.27    TRANSACTION EXTERNAL JULIAN DATE
     *               TRABUF(TEUSER),                                            !TEUR.27    TRANSACION EXTERNAL SERIAL FROM EUROMILLIONS
     *               TRABUF(TEUCHK),                                            !TEUR.27    TRANSACTION CHECK DIGITS FROM EUROMILLIONS
     *           FS, JUL,                                                       !TEUR.28    WAGER EXTERNAL JULIAN DATE
     *               TRABUF(TEUSER),                                            !TEUR.28    WAGER EXTERNAL SERIAL FROM EUROMILLIONS
     *               TRABUF(TEUCHK),                                            !TEUR.28    WAGER CHECK DIGITS FROM EUROMILLIONS
     *          (FS, I=29,29),                                                  !TEUR.29-29 FILLER FIELD (TRANSACTION AMOUNT = WAGER AMOUNT)
     *           FS, TRABUF(TEUWTIMEH),                                         !TEUR.30    TIME OF WAGER : HOUR (OUT)
     *               TRABUF(TEUWTIMEM),                                         !TEUR.30    TIME OF WAGER : MIN  (OUT)
     *               TRABUF(TEUWTIMES),                                         !TEUR.30    TIME OF WAGER : SS   (OUT)
     *          (FS, I=31,32),                                                  !TEUR.31-32 FILLER FIELDS (TRANSACTION ERROR CODE AND NIF)
     *           FS, TRABUF(TEUMESSQ),                                          !TEUR.33    MESSAGE QUEUE NUMBER FOR THIS TRANSACTION
     *          (FS, I=34,34),                                                  !TEUR.34-34 FILLER FIELDS
     *           FS, TRABUF(TEUWBEGW),                                          !TEUR.35    BEGIN WEEK DRAW (OUT)
     *               2000+TRABUF(TEUWBEGY),                                     !TEUR.35    BEGIN YEAR DRAW(OUT)
!     *           FS, TRABUF(TEUWENDW),                                          !END WEEK DRAW (OUT)
!     *               2000+TRABUF(TEUWENDY),                                     !END YEAR DRAW (OUT)
     *          (FS, I=36,38),                                                  !TEUR.36-38 FILLER FIELDS
!     *           FS, TRABUF(TEUWDUR),                                           !DURATION OF BET (NUMBER OF DRAWS) (IN)
     *           FS, TRABUF(TEUWNBET),                                          !TEUR.39    NUMBER OF BOARDS (IN)
     *          (FS, I=40,40),                                                  !TEUR.40-40 FILLER FIELDS
     *           FS, TRABUF(TEUWQP),                                            !TEUR.41    QUICK PICK FLAG   (IN)
     *           FS, TRABUF(TEUWNMK),                                           !TEUR.42    NUMBER OF MARKS  (IN)
     *           FS, TRABUF(TEUWNST),                                           !TEUR.43    NUMBER OF STARS   (IN)
!     *           FS, TRABUF(TEUWOFS1),                                          !OFFSET TO FIRST CDC DATE  (OUT)
!     *           FS, TRABUF(TEUWOFS2),                                          !OFFSET TO SECOND CDC DATE (OUT)
     *           FS, TRABUF(TEUWDRWIND)                                         !TEUR.44    DRAW INDICATOR (IN)
           RETURN
1110       CONTINUE
           RTTIM=DISTIM(TRABUF(TTIM))
           TYPE*,IAM(),'EUR WAG WRITE ERROR: IO STAT > ',ST
           TYPE*,'                  TSTAT......=',TRABUF(TSTAT)
           TYPE*,'                  TERR.......=',TRABUF(TERR)
           TYPE*,'                  TTER.......=',TRABUF(TTER)
           TYPE*,'                  TSER.......=',TRABUF(TSER)
           TYPE*,'                  TTYPE......=',TRABUF(TTYP)
           TYPE*,'                  TTIM.......=    '//CTTIM
           TYPE*,'                  TEUTYP.....=',TRABUF(TEUTYP)
           TYPE*,'                  JUL........=',JUL
           TYPE*,'                  TEUSER.....=',TRABUF(TEUSER)
           TYPE*,'                  TEUCHK.....=',TRABUF(TEUCHK)
           TYPE*,'                  JUL(W).....=',JUL
           TYPE*,'                  TEUSER(W)..=',TRABUF(TEUSER)
           TYPE*,'                  TEUCHK(W)..=',TRABUF(TEUCHK)
           TYPE*,'                  TEUWTIMEH..=',TRABUF(TEUWTIMEH)
           TYPE*,'                  TEUWTIMEM..=',TRABUF(TEUWTIMEM)
           TYPE*,'                  TEUWTIMES..=',TRABUF(TEUWTIMES)
           TYPE*,'                  TEUMESSQ...=',TRABUF(TEUMESSQ)
           TYPE*,'                  TEUWBEGW...=',TRABUF(TEUWBEGW)
           TYPE*,'                  TEUWBEGY...=',TRABUF(TEUWBEGY)
           TYPE*,'                  TEUWNBET...=',TRABUF(TEUWNBET)
           TYPE*,'                  TEUWQP.....=',TRABUF(TEUWQP)
           TYPE*,'                  TEUWNMK....=',TRABUF(TEUWNMK)
           TYPE*,'                  TEUWNST....=',TRABUF(TEUWNST)
           TYPE*,'                  TEUWDRWIND.=',TRABUF(TEUWDRWIND)
           WRITE(QLIKDATA.QLIK_LUN,*)                                           !PRINT NEW LINE
CV02         ELSEIF(TRABUF(TEUTYP) .EQ. TVAL) THEN
         ELSEIF(TRABUF(TEUTYP) .EQ. EURVINQ .OR. 
     *          TRABUF(TEUTYP) .EQ. EURPAY) THEN                                !V02
C EUROMILLIONS VALIDATION DETAIL
           IF(TRABUF(TSTAT) .EQ. REJT .AND.
     *       (TRABUF(TERR) .EQ. INVL .OR. TRABUF(TERR) .EQ. VINQ)) THEN         !CLEAN VARIABLES
             TRABUF(TEUMESSQ)  = 0
             TRABUF(TEUVTIMEH) = 0
             TRABUF(TEUVTIMEM) = 0
             TRABUF(TEUVTIMES) = 0
             TRABUF(TEUVCAMH)  = 0
             TRABUF(TEUVCAM)   = 0
             TRABUF(TEUVRAMH)  = 0
             TRABUF(TEUVRAM)   = 0
           ENDIF
           IF(TRABUF(TEUVSBT) .EQ. 15) THEN                                     !IF VALIDATION ERROR DO NOT PRINT THE CASH AMOUNT VALUE
             TRABUF(TEUVCAMH) = 0
             TRABUF(TEUVCAM)  = 0
             TRABUF(TEUVRAMH) = 0
             TRABUF(TEUVRAM)  = 0
           ENDIF
           PRZAMT(2) = TRABUF(TEUVCAMH)                                         !PRIZE AMOUNT
           PRZAMT(1) = TRABUF(TEUVCAM)
           PRZAMTR8  = DFLOAT(PRZAMTI8)/100.0D0
C
           NPZAMT(2) = TRABUF(TEUVRAMH)                                         !NET PRIZE AMOUNT
           NPZAMT(1) = TRABUF(TEUVRAM)
           NPZAMTR8  = DFLOAT(NPZAMTI8)/100.0D0
C
           TRXAMTI8 = 0
           TRXAMTR8 = 0.0D0
           IF(PRZAMTI8 .LE. P(VALPRZHI)) NPZAMTR8 = PRZAMTR8
!           IF(NPZAMTI8 .EQ. 0) NPZAMTR8 = PRZAMTR8
           IF(TRABUF(TEUVSBT) .EQ. VNDON .OR.
     *       TRABUF(TEUVSBT) .EQ. VNBNK) THEN
             TRXAMTI8=NPZAMTI8
             TRXAMTR8=NPZAMTR8
           ENDIF
           IF(TRABUF(TERR) .EQ. VINQ) TRXAMTR8 = 0.0D0                          !VALIDATION (INQUIRY)
C
           IF(TRABUF(TEUVSBT) .NE. VNBNK) THEN ! SET THIS FIELDS TO ITS DEFAULT VALUES
             TRABUF(TEUVNIBBB)   = 0
             TRABUF(TEUVNIBBO)   = 0
             TRABUF(TEUVNIBBA1)  = 0
             TRABUF(TEUVNIBBA2)  = 0
             TRABUF(TEUVNIBCD)   = 0
             TRABUF(TEUVPLIDTYP) = -1
             TRABUF(TEUVPLCARD)  = 0
           ENDIF
           CALL FASTSET(BLANK,ERRNIB,32)
           CALL FASTSET(BLANK,NIB,6)
           WRITE(CNIB,'(I4.4,1X,I4.4,I9.9,I2.2,1X,I2.2)',ERR=1210,
     *           IOSTAT=ST)                       !NIB
     *           TRABUF(TEUVNIBBB),
     *           TRABUF(TEUVNIBBO),
     *           TRABUF(TEUVNIBBA1),
     *           TRABUF(TEUVNIBBA2),
     *           TRABUF(TEUVNIBCD)
C
           WRITE(QLIKDATA.QLIK_LUN,1200,ERR=1220,IOSTAT=ST)
     *           FS, JUL,                                                       !TEUR.27    VALIDATION TRANSACTION EXTERNAL JULIAN DATE
     *               TRABUF(TEUSER),                                            !TEUR.27    VALIDATION TRANSACTION EXTERNAL SERIAL FROM EUROMILLIONS
     *               TRABUF(TEUCHK),                                            !TEUR.27    TRANSACTION CHECK DIGITS FROM EUROMILLIONS
     *           FS, TRABUF(TEUVWJUL),                                          !TEUR.28    WAGER JULIAN VALIDATION        (IN)
     *               TRABUF(TEUVWSER),                                          !TEUR.28    WAGER EXTERNAL SERIAL VALIDATION (IN)
     *               TRABUF(TEUVWCKD),                                          !TEUR.28    WAGER CHECK DIGITS VALIDATION  (IN)
     *           FS, TRXAMTR8,                                                  !TEUR.29    TRANSACTION AMOUNT (CASHED PRIZE AMOUNT)
     *           FS, TRABUF(TEUVTIMEH),                                         !TEUR.30    VALIDATION TIME : HOUR         (OUT)
     *               TRABUF(TEUVTIMEM),                                         !TEUR.30    VALIDATION TIME : MIN          (OUT)
     *               TRABUF(TEUVTIMES),                                         !TEUR.30    VALIDATION TIME : SS           (OUT)
     *           FS, TRABUF(TEUVST),                                            !TEUR.31    VALIDATION STATUS              (OUT)
     *          (FS, I=32,32),                                                  !TEUR.32-32 FILLER FIELDS (PORTUGUESE VAT)
     *           FS, TRABUF(TEUMESSQ),                                          !TEUR.33    MESSAGE QUEUE NUMBER FOR THIS TRANSACTION
     *          (FS, I=34,57),                                                  !TEUR.34-57 FILLER FIELDS
     *           FS, TRABUF(TEUVSBT),                                           !TEUR.58    VALIDATION SUBTYPE             (IN & OUT)
     *           FS, PRZAMTR8,                                                  !TEUR.59    PRIZE AMOUNT                   (OUT)
     *          (FS, I=61,61),                                                  !TEUR.60-60 FILLER FIELDS
!     *           FS, NPZAMTR8,                                                  !NET PRIZE  AMOUNT              (OUT)
     *           FS, TRABUF(TEUVPLIDTYP),                                       !TEUR.61    PLAYER ID TYPE
     *           FS, TRABUF(TEUVPLCARD),                                        !TEUR.62    PLAYER ID
     *           FS, (NIB(I),I=1,6)                                             !TEUR.63    PLAYER NIB
           RETURN
1210       CONTINUE
           TYPE*, IAM(), '       '
           WRITE(CERRNIB,'(I0,1X,I0,I0,I0,1X,I0)')
     *           TRABUF(TEUVNIBBB),
     *           TRABUF(TEUVNIBBO),
     *           TRABUF(TEUVNIBBA1),
     *           TRABUF(TEUVNIBBA2),
     *           TRABUF(TEUVNIBCD)
           TYPE*, IAM(), 'ERROR FORMATTING NIB: WRITE STATUS=',ST,
     *                   ',TER=',TRABUF(TTER),
     *                   ',ISER=',TRABUF(TSER),
     *                   ',NIB='//TRIM(CERRNIB)
1220       CONTINUE
           RTTIM=DISTIM(TRABUF(TTIM))
           TYPE*,IAM(),'EUR VAL WRITE ERROR: IO STAT > ',ST
           TYPE*,'                  TSTAT......=',TRABUF(TSTAT)
           TYPE*,'                  TERR.......=',TRABUF(TERR)
           TYPE*,'                  TTER.......=',TRABUF(TTER)
           TYPE*,'                  TSER.......=',TRABUF(TSER)
           TYPE*,'                  TTYPE......=',TRABUF(TTYP)
           TYPE*,'                  TTIM.......=    '//CTTIM
           TYPE*,'                  TEUTYP.....=',TRABUF(TEUTYP)
           TYPE*,'                  JUL........=',JUL
           TYPE*,'                  TEUSER.....=',TRABUF(TEUSER)
           TYPE*,'                  TEUCHK.....=',TRABUF(TEUCHK)
           TYPE*,'                  TEUVWJUL...=',TRABUF(TEUVWJUL)
           TYPE*,'                  TEUVWSER...=',TRABUF(TEUVWSER)
           TYPE*,'                  TEUVWCKD...=',TRABUF(TEUVWCKD)
           TYPE*,'                  TRXAMTI8...=',TRXAMTI8
           TYPE*,'                  TEUVTIMEH..=',TRABUF(TEUVTIMEH)
           TYPE*,'                  TEUVTIMEM..=',TRABUF(TEUVTIMEM)
           TYPE*,'                  TEUVTIMES..=',TRABUF(TEUVTIMES)
           TYPE*,'                  TEUVST.....=',TRABUF(TEUVST)
           TYPE*,'                  TEUMESSQ...=',TRABUF(TEUMESSQ)
           TYPE*,'                  TEUVSBT....=',TRABUF(TEUVSBT)
           TYPE*,'                  PRZAMTI8...=',PRZAMTI8
           TYPE*,'                  TEUVPLIDTYP=',TRABUF(TEUVPLIDTYP)
           TYPE*,'                  TEUVPLCARD.=',TRABUF(TEUVPLCARD)
           TYPE*,'                  TEUVNIBBB..=',TRABUF(TEUVNIBBB)
           TYPE*,'                  TEUVNIBBO..=',TRABUF(TEUVNIBBO)
           TYPE*,'                  TEUVNIBBA1.=',TRABUF(TEUVNIBBA1)
           TYPE*,'                  TEUVNIBBA2.=',TRABUF(TEUVNIBBA2)
           TYPE*,'                  TEUVNIBCD..=',TRABUF(TEUVNIBCD)
           TYPE*,'                  NIB........= '//CNIB
           TYPE*,'                  ERRNIB.....= '//CERRNIB
           WRITE(QLIKDATA.QLIK_LUN,*)                                           !PRINT NEW LINE
           RETURN
         ELSEIF(TRABUF(TEUTYP) .EQ. TCAN) THEN
C EUROMILLIONS WAGER DETAIL
!          THE FIELDS TRABUF(TEUCTIMEM) AND TRABUF(TEUCTIMES) ARE OVERWRITTEN BY TRABUF(TEUCST) AND
!          TRABUF(TEUCAM) RESPECTIVELY. BECAUSE OF THIS THERE IS NO CANCELLATION TIME REGISTERED IN MTMF.
           IF (TRABUF(TEUCST) .NE. 0) TRABUF(TEUCAM) = 0
           WRITE(C11MONY,'(A11)') CMONY(TRABUF(TEUCAM),11,VALUNIT)
           WRITE(QLIKDATA.QLIK_LUN,1300)
     *           FS, JUL,                                                       !TEUR.27    CANCEL TRANSACTION EXTERNAL JULIAN DATE (OUT)
     *               TRABUF(TEUSER),                                            !TEUR.27    CANCEL TRANSACTION EXTERNAL SERIAL FROM EUROMILLIONS (OUT)
     *               TRABUF(TEUCHK),                                            !TEUR.27    CANCEL TRANSACTION CHECK DIGITS FROM EUROMILLIONS (OUT)
     *           FS, TRABUF(TEUCWJUL),                                          !TEUR.28    WAGER JULIAN CANCEL        (IN)
     *               TRABUF(TEUCWSER),                                          !TEUR.28    WAGER EXTERN SERIAL CANCEL (IN)
     *               TRABUF(TEUCWCKD),                                          !TEUR.28    WAGER CHECK DIGITS CANCEL  (IN)
     *           FS, TRIM(ADJUSTL(C11MONY)),                                    !TEUR.29    CANCEL AMOUNT (OUT)
     *          (FS, I=30,30),                                                  !TEUR.30-30 FILLER FIELDS (CANCEL TIME)
     *           FS, TRABUF(TEUCST),                                            !TEUR.31    CANCEL STATUS (OUT)
     *          (FS, I=32,32),                                                  !TEUR.32-32 FILLER FIELD (PORTUGUESE VAT)
     *           FS, TRABUF(TEUMESSQ)                                           !TEUR.33    MESSAGE QUEUE NUMBER FOR THIS TRANSACTION
         ELSE ! IT SHOULD NOT HAPPEN
!           WRITE(QLIKDATA.QLIK_LUN,'(A<FS_LEN>)') FS
           WRITE(QLIKDATA.QLIK_LUN,*)                                           !PRINT NEW LINE
         ENDIF
       ENDIF
C
1000   FORMAT(           I0,                                                    !TEUR.1     STATUS
     *        A<FS_LEN>, I0,                                                    !TEUR.2     ERROR CODE
     *        A<FS_LEN>, A8,                                                    !TEUR.3     TRANSACTION DATE
     *        A<FS_LEN>, I0,                                                    !TEUR.4     INTERNAL SERIAL NUMBER
     *        A<FS_LEN>, A8,                                                    !TEUR.5     TIME STAMP HH:MI:SS
     *        A<FS_LEN>, I0,                                                    !TEUR.6     TERMINAL NUMBER
     *        A<FS_LEN>, A8,                                                    !TEUR.7     AGENT NUMBER
     *        A<FS_LEN>, I0,                                                    !TEUR.8     SAP NUMBER
     *        A<FS_LEN>, I0,                                                    !TEUR.9     TRANSACTION SEQUENCE NUMBER
     *        A<FS_LEN>, I0,                                                    !TEUR.10    TRANSACTION TYPE
     *        A<FS_LEN>, I0,                                                    !TEUR.11    GAME NUMBER
     *        A<FS_LEN>, I0,                                                    !TEUR.12    GAME TYPE
     *        A<FS_LEN>, I0,                                                    !TEUR.13    GAME INDEX
     *        A<FS_LEN>, I0,                                                    !TEUR.14    TERMINAL STATISTICS
     *        A<FS_LEN>, I0,                                                    !TEUR.15    INTERNAL TRANSACTION FLAG
     *        A<FS_LEN>, I0,                                                    !TEUR.16    FILE STATUS
     *        A<FS_LEN>, I0,                                                    !TEUR.17    TICKET ID
     *        A<FS_LEN>, I0,                                                    !TEUR.18    MESSAGE CHECKSUM
     *        A<FS_LEN>, I0,                                                    !TEUR.19    # OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TEUR.20    TRANSACTION SIZE (# LOG RECS)
     *        A<FS_LEN>, I0,                                                    !TEUR.21    ERROR SUB CODE
     *        A<FS_LEN>, I0,                                                    !TEUR.22    CDC TRANS. WAS SOLD (NEVER CHANGES)
     *        A<FS_LEN>, I0,                                                    !TEUR.23    BET AMOUNT FLAG (FOR FRACTIONS)
     *        A<FS_LEN>, I0,                                                    !TEUR.24    REAL NUMBER OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TEUR.25    PROMOTION - ADD 1 FREE WEEK
     *        A<FS_LEN>, I0)                                                    !TEUR.26    EUROMILLIONS TRANSACTION TYPE
C EUROMILLIONS WAGER DETAIL
1100   FORMAT(A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TEUR.27    TRANSACTION EXTERNAL SERIAL NUMBER FROM EUROMILLIONS
     *        A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TEUR.28    WAGER EXTERNAL SERIAL NUMBER FROM EUROMILLIONS
     *        A<FS_LEN>,                                                        !TEUR.29-29 FILLER FIELD (TRANSACTION AMOUNT = WAGER AMOUNT)
     *        A<FS_LEN>, I2.2,':',I2.2,':',I2.2,                                !TEUR.30    TIME OF WAGER : HH:MI:SS (OUT)
     *       2A<FS_LEN>,                                                        !TEUR.31-32 FILLER FIELDS (TRANSACTION ERROR CODE AND NIF)
     *        A<FS_LEN>, I0,                                                    !TEUR.33    MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
     *        A<FS_LEN>,                                                        !TEUR.34    FILLER FIELDS
     *        A<FS_LEN>, I3.3,'/',I4.4,                                         !TEUR.35    BEGIN WEEK DRAW/BEGIN YEAR DRAW (OUT)
!     *        A<FS_LEN>, I3.3,'/',I4.4,                                         !END WEEK DRAW/BEGIN YEAR DRAW (OUT)
     *       3A<FS_LEN>,                                                        !TEUR.36-38 FILLER FIELDS
!     *        A<FS_LEN>, I0,                                                    !DURATION OF BET (NUMBER OF DRAWS) (IN)
     *        A<FS_LEN>, I0,                                                    !TEUR.39    NUMBER OF BOARDS (IN)
     *        A<FS_LEN>,                                                        !TEUR.40-40 FILLER FIELDS
     *        A<FS_LEN>, I0,                                                    !TEUR.41    QUICK PICK FLAG   (IN)
     *        A<FS_LEN>, I0,                                                    !TEUR.42    NUMBER OF MARKS  (IN)
     *        A<FS_LEN>, I0,                                                    !TEUR.43    NUMBER OF STARS   (IN)
!     *        A<FS_LEN>, I0,                                                    !OFFSET TO FIRST CDC DATE  (OUT)
!     *        A<FS_LEN>, I0,                                                    !OFFSET TO SECOND CDC DATE (OUT)
     *        A<FS_LEN>, I0)                                                    !TEUR.44    DRAW INDICATOR (IN)
C EUROMILLIONS VALIDATION DETAIL
1200   FORMAT(A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TEUR.27    VALIDATION TRANSACTION EXTERNAL SERIAL NUMBER FROM EUROMILLIONS
     *        A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TEUR.28    WAGER EXTERNAL SERIAL NUMBER TO VALIDATE (IN)
     *        A<FS_LEN>, F14.2,                                                 !TEUR.29    TRANSACTION AMOUNT (CASHED PRIZE AMOUNT)
     *        A<FS_LEN>, I2.2,':',I2.2,':',I2.2,                                !TEUR.30    VALIDATION TIME : HH:MI:SS (OUT)
     *        A<FS_LEN>, I0,                                                    !TEUR.31    VALIDATION STATUS        (OUT)
     *        A<FS_LEN>,                                                        !TEUR.32-32 FILLER FIELDS (PORTUGUESE VAT)
     *        A<FS_LEN>, I0,                                                    !TEUR.33    MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
     *      24A<FS_LEN>,                                                        !TEUR.34-57 FILLER FIELDS
     *        A<FS_LEN>, I0,                                                    !TEUR.58    VALIDATION SUBTYPE (IN & OUT)
     *        A<FS_LEN>, F14.2,                                                 !TEUR.59    VALIDATION AMOUNT              (OUT)
     *        A<FS_LEN>,                                                        !TEUR.60-60 FILLER FIELDS
!     *        A<FS_LEN>, F14.2,                                                 !NET PRIZE AMOUNT   (OUT)
     *        A<FS_LEN>, I0,                                                    !TEUR.61    PLAYER ID TYPE
     *        A<FS_LEN>, I0,                                                    !TEUR.62    PLAYER ID
     *        A<FS_LEN>, 6A4)                                                   !TEUR.63    PLAYER NIB
C EUROMILLIONS CANCELLATION DETAIL
1300   FORMAT(A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TEUR.27    CANCEL TRANSACTION EXTERNAL SERIAL NUMBER FROM EUROMILLIONS (OUT)
     *        A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TEUR.28    WAGER EXTERNAL SERIAL NUMBER TO CANCEL  (IN)
     *        A<FS_LEN>, A10,                                                   !TEUR.29    CANCEL AMOUNT
     *        A<FS_LEN>,                                                        !TEUR.30-30 FILLER FIELDS (CANCEL TIME)
     *        A<FS_LEN>, I0,                                                    !TEUR.31    CANCEL STATUS (OUT)
     *        A<FS_LEN>,                                                        !TEUR.32-32 FILLER FIELD (PORTUGUESE VAT)
     *        A<FS_LEN>, I0)                                                    !TEUR.33    MESSAGE QUEUE NUMBER FOR THIS TRANSACTION
C
       RETURN
       END
CCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC       SUBROUTINE PRINT_TWAG (QLIKDATA,TRABUF)
CCC
CCC       THIS SUBROUTINE WRITES WAG TRANSACTIONS INTO QLIK FILE.
CCC
CCC       INPUTS:
CCC        QLIKDATA       QLIK DATA STRUCTURE
CCC        TRABUF         WAG TRANSACTION TO WRITE INTO THE FILE
CCC
CCC       OUTPUTS:
CCC        *NONE*
CCC
CCC=======================================================================
CCC====== OPTIONS /CHECK=NOOVERFLOW
CC       SUBROUTINE PRINT_TWAG(QLIKDATA,TRABUF)
CC       IMPLICIT NONE
CCC
CC       INCLUDE 'INCLIB:SYSEXTRN.DEF'
CC       INCLUDE 'INCLIB:GLOBAL.DEF'
CC       INCLUDE 'INCLIB:DESTRA.DEF'
CC       INCLUDE 'INCLIB:DATBUF.DEF'
CC       INCLUDE 'INCLIB:QLIKTRAN.DEF'
CC       INCLUDE 'INCLIB:AGTCOM.DEF'
CCC
CC       INTEGER*4 I
CC       INTEGER*4 JUL
CC       INTEGER*2 DBUF(12)
CC       CHARACTER*11 C11MONY
CC       CHARACTER*8 IAGT_NO                                                      !EXTERNAL FUNCTION
CC       CHARACTER*8 GET_YYYYMMDD_CDC                                             !EXTERNAL FUNCTION
CC       CHARACTER*8 CDRWNAM                                                      !LOCAL FUNCTION
CC       INTEGER*4 EPTCKT,TSTYP
CCC
CC       INTEGER*4 WXSER, WXCHK, CXSER, CXCHK
CC       CHARACTER*16 C16XSER
CC       CHARACTER*8 DRWNAM, DRWKNAM
CCC
CC       DRWNAM  = '        '
CC       DRWKNAM = '        '
CC       C16XSER  = '                '
CC       WXSER   = 0
CC       WXCHK   = 0
CC       CXSER   = 0
CC       CXCHK   = 0
CCC
CC       DBUF(VCDC)=TRABUF(TCDC)
CC       CALL CDATE(DBUF)
CC       JUL=DBUF(VJUL)
CCC
CCC WAGER TRANSACTION HEADER
CC       WRITE(QLIKDATA.QLIK_LUN,1000,ADVANCE='NO')
CC     *           TRABUF(TSTAT),                                                 !TWAG.1     STATUS
CC     *       FS, TRABUF(TERR),                                                  !TWAG.2     ERROR CODE
CC     *       FS, GET_YYYYMMDD_CDC(TRABUF(TCDC)),                                !TWAG.3     TRANSACTION DATE
CC     *       FS, TRABUF(TSER),                                                  !TWAG.4     INTERNAL SERIAL NUMBER
CC     *       FS, DISTIM(TRABUF(TTIM)),                                          !TWAG.5     TIME STAMP HH:MI:SS
CC     *       FS, TRABUF(TTER),                                                  !TWAG.6     TERMINAL NUMBER
CC     *       FS, IAGT_NO(TRABUF(TAGT)),                                         !TWAG.7     AGENT NUMBER
CC     *       FS, AGTSAP(TRABUF(TTER)),                                          !TWAG.8     SAP NUMBER
CC     *       FS, TRABUF(TTRN),                                                  !TWAG.9     TRANSACTION SEQUENCE NUMBER
CC     *       FS, TRABUF(TTYP),                                                  !TWAG.10    TRANSACTION TYPE
CC     *       FS, TRABUF(TGAM),                                                  !TWAG.11    GAME NUMBER
CC     *       FS, TRABUF(TGAMTYP),                                               !TWAG.12    GAME TYPE
CC     *       FS, TRABUF(TGAMIND),                                               !TWAG.13    GAME INDEX
CC     *       FS, TRABUF(TTSTCS),                                                !TWAG.14    TERMINAL STATISTICS
CC     *       FS, TRABUF(TINTRA),                                                !TWAG.15    INTERNAL TRANSACTION FLAG
CC     *       FS, TRABUF(TFIL),                                                  !TWAG.16    FILE STATUS
CC     *       FS, TRABUF(TTKID),                                                 !TWAG.17    TICKET ID
CC     *       FS, TRABUF(TCHK),                                                  !TWAG.18    MESSAGE CHECKSUM
CC     *       FS, TRABUF(TFRAC),                                                 !TWAG.19    # OF FRACTIONS
CC     *       FS, TRABUF(TSIZE),                                                 !TWAG.20    TRANSACTION SIZE (# LOG RECS)
CC     *       FS, TRABUF(TSUBERR),                                               !TWAG.21    ERROR SUB CODE
CC     *       FS, TRABUF(TCDC_SOLD),                                             !TWAG.22    CDC TRANS. WAS SOLD (NEVER CHANGES)
CC     *       FS, TRABUF(TFAMTFLG),                                              !TWAG.23    BET AMOUNT FLAG (FOR FRACTIONS)
CC     *       FS, TRABUF(TNFRAC),                                                !TWAG.24    REAL NUMBER OF FRACTIONS
CC     *       FS, TRABUF(TWADDFW)                                                !TWAG.25    PROMOTION - ADD 1 FREE WEEK
CC       IF(TRABUF(TGAMTYP) .NE. TPAS) THEN
CC         WRITE(QLIKDATA.QLIK_LUN,1001,ADVANCE=WRITE__ADVANCE_VALUE)
CC     *      (FS, I=26,26)                                                       !TWAG.26-26 FILLER FIELD (TRANSACTION SUBTYPE N/A)
CC       ELSE
CC         WRITE(QLIKDATA.QLIK_LUN,1002,ADVANCE=WRITE__ADVANCE_VALUE)
CC     *       FS, TRABUF(TWEPOP)                                                 !TWAG.26    EPASSIVE OPERATION REQUESTED
CC       ENDIF
CCC
CC       IF(TDETAIL) THEN
CCC      WAGER DETAIL
CC         CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),WXSER,WXCHK)
CC         DRWNAM = CDRWNAM(TRABUF)
CC         IF(TRABUF(TWKFLG)) THEN
CC           DRWKNAM = CDRWNAM(TRABUF)
CC         ENDIF
CC         WRITE(QLIKDATA.QLIK_LUN,1010,ADVANCE='NO')
CC     *         FS, JUL,                                                         !TWAG.27    TRANSACTION EXTERNAL JULIAN DATE
CC     *             WXSER,                                                       !TWAG.27....TRANSACTION EXTERNAL WAGER SERIAL NUMBER
CC     *             WXCHK,                                                       !TWAG.27    TRANSACTION EXTERNAL WAGER CHECK DIGITS
CC     *         FS, JUL,                                                         !TWAG.28    EXTERNAL WAGER JULIAN DATE
CC     *             WXSER,                                                       !TWAG.28    EXTERNAL WAGER SERIAL NUMBER
CC     *             WXCHK,                                                       !TWAG.28    EXTERNAL WAGER CHECK DIGITS
CC     *         FS, CMONY(TRABUF(TWTOT),10,BETUNIT),                             !TWAG.29    TOTAL AMOUNT
CC     *         FS, DISTIM(TRABUF(TTIM)),                                        !TWAG.30    TIME STAMP HH:MI:SS
CC     *        (FS, I=31,34),                                                    !TWAG.31-34 FILLER FIELDS
CC     *         FS, DRWNAM,                                                      !TWAG.35    (STARTING) DRAW NAME
CC     *         FS, CMONY(TRABUF(TWAMT),8,BETUNIT),                              !TWAG.36    BASE AMOUNT (MAIN GAME)
CC     *         FS, DRWKNAM,                                                     !TWAG.37    KICKER (START) DRAW NAME
CC     *         FS, CMONY(TRABUF(TWKAMT),8,BETUNIT),                             !TWAG.38    KICKER AMOUNT
CC     *         FS, TRABUF(TWNBET),                                              !TWAG.39    NUMBER OF BETS/BOARDS
CC     *        (FS, I=40,40),                                                    !TWAG.40-40 FILLER FIELDS
CC     *         FS, TRABUF(TWQPF)                                                !TWAG.41    QUICK PICK FLAGS
CC!     *         FS, TRABUF(TWBEG),                                               !STARTING DRAW #
CC!     *         FS, TRABUF(TWEND),                                               !ENDING DRAW #
CC!     *         FS, CMONY(TRABUF(TWTKC),8,BETUNIT),                              !TICKET CHARGE
CC!     *         FS, CMONY(TRABUF(TWDAMT),8,BETUNIT),                             !DISCOUNT AMOUNT
CC!     *         FS, DRWNUM,                                                      !TWAG.35    (STARTING) DRAW #
CC!     *         FS, DRWYEAR,                                                     !TWAG.35    (STARTING) DRAW YEAR
CC!     *         FS, TRABUF(TWDUR),                                               !DURATION (NUMBER OF DRAWS)
CC!     *         FS, TRABUF(TWKICK),                                              !KICKER NUMBER
CC!     *         FS, TRABUF(TWKICK2),                                             !KICKER NUMBER    (SECOND)
CC!     *         FS, TRABUF(TWKFLG),                                              !KICKER SOLD FLAG
CC!     *         FS, TRABUF(TWKFLG2),                                             !KICKER SOLD FLAG (SECOND)
CC!     *         FS, TRABUF(TWKSEED),                                             !KICKER SEED
CC!     *         FS, TRABUF(TWKSEED2),                                            !KICKER SEED      (SECOND)
CC!     *         FS, TRABUF(TWKGME),                                              !KICKER GAME #
CC!     *         FS, TRABUF(TWKBEG),                                              !KICKER START DRAW
CC!     *         FS, DRWKNUM,                                                     !TWAG.37    KICKER (START) DRAW #
CC!     *             DRWKYEAR,                                                    !TWAG.37    KICKER (START) DRAW YEAR
CC!     *         FS, TRABUF(TWKEND),                                              !KICKER END DRAW
CC!     *         FS, TRABUF(TWKDUR),                                              !KICKER DURATION
CC!     *         FS, TRABUF(TWCSER),                                              !CASH/CANCEL SERIAL #
CC!     *         FS, TRABUF(TWCTER),                                              !CASH/CANCEL TERMINAL #
CC!     *         FS, TRABUF(TWVSTS),                                              !VALIDATION STATUS
CC!     *         FS, TRABUF(TWNBET),                                              !NUMBER OF BETS/BOARDS
CC!     *         FS, TRABUF(TWSYST),                                              !TWAG.45    SYSTEM TYPE
CC!     *         FS, TRABUF(TWSYSN),                                              !SYSTEM NUMBER
CC!     *         FS, TRABUF(TWBNKID),                                             !BANK ID NUMBER
CC!     *         FS, TRABUF(TWBNKNM),                                             !BANK ACCOUNT NUMBER
CC!     *         FS, TRABUF(TWFFLG),                                              !FRACTIONED WAGER FLAG
CC!     *         FS, TRABUF(TWMFRAC),                                             !INITIAL DEFINED FRACTIONS
CC!     *         FS, TRABUF(TWLMFI),                                              !LOTTO MONDAY FLAG INDICATOR
CC!     *         FS, TRABUF(TWSPFRG),                                             !NUMBER OF RESULTS GAMES (SPORTS)
CC!     *         FS, TRABUF(TWLNKSER),                                            !SERIAL NUMBER OF ASSOCIATED LINKED WAGER, OLD EM
CC!     *             TRABUF(TWLNKCHK)                                             !CHECK DIGITS OF ASSOCIATED LINKED WAGER, OLD EM
CC!     *         FS, TRABUF(TWORK1)                                               !WORKING ENTRY THROUGH PROCESS (NOT USED)
CC!     *         FS, TRABUF(TWORK2)                                               !WORKING ENTRY THROUHG PROCESS (NOT USED)
CC!     *         FS, TRABUF(TWBDTA)                                               !START OF BET DATA
CC         CALL OUTGEN(TRABUF(TCDC),TRABUF(TWCSER),CXSER,CXCHK)
CC         IF(TRABUF(TWCSER) .NE. 0) THEN
CC           WRITE (C16XSER,'(I3.3,A1,I8.8,A1,I3.3)') JUL,'-',CXSER,'-',CXCHK
CC         ENDIF
CC         IF(TRABUF(TGAMTYP) .EQ. TSPT) THEN
CC           WRITE(QLIKDATA.QLIK_LUN,1020,ADVANCE='NO')
CC     *          (FS, I=42,44),                                                  !TWAG.42-44 FILLER FIELDS
CC     *           FS, TRABUF(TWSYST),                                            !TWAG.45    SYSTEM TYPE
CC     *          (FS, I=46,46),                                                  !TWAG.46-46 FILLER FIELDS
CC     *           FS, TRABUF(TWKFLG),                                            !TWAG.47    KICKER SOLD FLAG
CC     *           FS, TRABUF(TWKICK),                                            !TWAG.48    KICKER NUMBER
CC     *           FS, TRIM(C16XSER),                                              !TWAG.49    EXTERNAL CANCEL SERIAL #
CC!     *           FS, JUL,                                                       !TWAG.49    EXTERNAL CANCEL JULIAN #
CC!     *               CXSER,                                                     !TWAG.49    EXTERNAL CANCEL SERIAL #
CC!     *               CXCHK,                                                     !TWAG.49    EXTERNAL CANCEL CHECK DIGITS
CC     *           FS, TRABUF(TWCTER),                                            !TWAG.50    CANCEL TERMINAL #
CC     *           FS, TRABUF(TWSIMP)                                             !TWAG.51    # OF SIMPLE BETS
CC         ELSEIF(TRABUF(TGAMTYP) .EQ. TKIK) THEN
CC           WRITE(QLIKDATA.QLIK_LUN,1030,ADVANCE='NO')
CC     *          (FS, I=42,44),                                                  !TWAG.42-44 FILLER FIELDS
CC     *           FS, TRABUF(TWSYST),                                            !TWAG.45    SYSTEM TYPE
CC     *           FS, JUL,                                                       !TWAG.46    EXTERNAL WAGER JULIAN DATE
CC     *               TRABUF(TWLNKSER),                                          !TWAG.46    EXTERNAL SERIAL NUMBER OF ASSOCIATED LINKED WAGER
CC     *               TRABUF(TWLNKCHK),                                          !TWAG.46    EXTERNAL CHECK DIGITS OF ASSOCIATED LINKED WAGER
CC     *           FS, TRABUF(TWKFLG),                                            !TWAG.47    KICKER SOLD FLAG
CC     *           FS, TRABUF(TWKICK),                                            !TWAG.48    KICKER NUMBER
CC     *           FS, TRIM(C16XSER),                                              !TWAG.49    EXTERNAL CANCEL SERIAL #
CC!     *           FS, JUL,                                                       !TWAG.49    EXTERNAL CANCEL JULIAN #
CC!     *               CXSER,                                                     !TWAG.49    EXTERNAL CANCEL SERIAL #
CC!     *               CXCHK,                                                     !TWAG.49    EXTERNAL CANCEL CHECK DIGITS
CC     *           FS, TRABUF(TWCTER),                                            !TWAG.50    CANCEL TERMINAL #
CC     *           FS, TRABUF(TWSIMP)                                             !TWAG.51    # OF SIMPLE BETS
CC         ELSEIF(TRABUF(TGAMTYP) .EQ. TLTO) THEN
CC           WRITE(QLIKDATA.QLIK_LUN,1040,ADVANCE='NO')
CC     *           FS, TRABUF(TWNMRK),                                            !TWAG.42    # OF LOTTO MARKS/BOARD
CC     *          (FS, I=43,44),                                                  !TWAG.43-44 FILLER FIELDS
CC     *           FS, TRABUF(TWSYST),                                            !TWAG.45    SYSTEM TYPE
CC     *          (FS, I=46,46),                                                  !TWAG.46-46 FILLER FIELDS
CC     *           FS, TRABUF(TWKFLG),                                            !TWAG.47    KICKER SOLD FLAG
CC     *           FS, TRABUF(TWKICK),                                            !TWAG.48    KICKER NUMBER
CC     *           FS, TRIM(C16XSER),                                               !TWAG.49    EXTERNAL CANCEL SERIAL #
CC!     *           FS, JUL,                                                       !TWAG.49    EXTERNAL CANCEL JULIAN #
CC!     *               CXSER,                                                     !TWAG.49    EXTERNAL CANCEL SERIAL #
CC!     *               CXCHK,                                                     !TWAG.49    EXTERNAL CANCEL CHECK DIGITS
CC     *           FS, TRABUF(TWCTER),                                            !TWAG.50    CANCEL TERMINAL #
CC     *           FS, TRABUF(TWSIMP),                                            !TWAG.51    # OF SIMPLE BETS
CC     *           FS, TRABUF(TWLUCK)                                             !TWAG.52    LUCKY #
CC!     *           FS, TRABUF(TWMFLG),                                            !BET MULTIPLIER FLAG
CC!     *           FS, TRABUF(TWCDCOFF),                                          !CDC OFFSET FOR EXCH TKT. (FOR STATE)
CC!     *           FS, TRABUF(TWMULT),                                            !START OF BET MULTIPLIERS
CC!     *           FS, TRABUF(TWBORD),                                            !START OF BOARD DATA
CC!     *           FS, TRABUF(TWWEQP),                                            !WEIGHTED QP
CC         ELSEIF(TRABUF(TGAMTYP) .EQ. TPAS) THEN
CC           EPTCKT = 0
CC           IF(TRABUF(TWEPOP) .EQ. EPASSAL) THEN
CC             EPTCKT = TRABUF(TWEPSN)
CC             WRITE(QLIKDATA.QLIK_LUN,1050,ADVANCE='NO')
CC     *            (FS, I=42,48),                                                !TWAG.42-48 FILLER FIELDS
CC     *             FS, TRIM(C16XSER),                                            !TWAG.49    EXTERNAL CANCEL SERIAL #
CC!     *             FS, JUL,                                                     !TWAG.49    EXTERNAL CANCEL JULIAN #
CC!     *                 CXSER,                                                   !TWAG.49    EXTERNAL CANCEL SERIAL #
CC!     *                 CXCHK,                                                   !TWAG.49    EXTERNAL CANCEL CHECK DIGITS
CC     *             FS, TRABUF(TWCTER),                                          !TWAG.50    CANCEL TERMINAL #
CC     *            (FS, I=51,52),                                                !TWAG.51-52 FILLER FIELDS
CC     *             FS, TRABUF(TWEPWK),                                          !TWAG.53    WEEK
CC     *                 2000+TRABUF(TWEPYR),                                     !TWAG.53    YEAR
CC     *             FS, EPTCKT ,                                                 !TWAG.54    SALE FOR NUMBER
CC     *             FS, TRABUF(TWEPSS),                                          !TWAG.55    SALE FOR SERIE
CC     *             FS, TRABUF(TWEPSF),                                          !TWAG.56    SALE FOR FRACTION
CC     *             FS, TRABUF(TWEPRM),                                          !TWAG.57    RESERVE MASK
CC     *             FS, TRABUF(TWEPNE),                                          !TWAG.58    NUMBER OF GIVEN ENDING NUMBERS
CC     *             FS, TRABUF(TWEPNF),                                          !TWAG.59    NUMBER OF REQUESTED FRACTIONS
CC     *             FS, TRABUF(TWEPNR)                                           !TWAG.60    NUMBER OF RESERVATIONS/RELEASES
CC           ELSEIF(TRABUF(TWEPOP).EQ. EPASREL .OR.
CC     *       TRABUF(TWEPOP).EQ. EPASRES) THEN
CC             EPTCKT = TRABUF(TWEPRES1)
CC             WRITE(QLIKDATA.QLIK_LUN,1050,ADVANCE='NO')
CC     *            (FS, I=42,48),                                                !TWAG.42-48 FILLER FIELDS
CC     *             FS, TRIM(C16XSER),                                            !TWAG.49    EXTERNAL CANCEL SERIAL #
CC!     *             FS, JUL,                                                     !TWAG.49    EXTERNAL CANCEL JULIAN #
CC!     *                 CXSER,                                                   !TWAG.49    EXTERNAL CANCEL SERIAL #
CC!     *                 CXCHK,                                                   !TWAG.49    EXTERNAL CANCEL CHECK DIGITS
CC     *             FS, TRABUF(TWCTER),                                          !TWAG.50    CANCEL TERMINAL #
CC     *            (FS, I=51,52),                                                !TWAG.51-52 FILLER FIELDS
CC     *             FS, TRABUF(TWEPWK),                                          !TWAG.53    WEEK
CC     *                 2000+TRABUF(TWEPYR),                                     !TWAG.53    YEAR
CC     *             FS, EPTCKT ,                                                 !TWAG.54    SALE FOR NUMBER
CC     *             FS, TRABUF(TWEPSS),                                          !TWAG.55    SALE FOR SERIE
CC     *             FS, TRABUF(TWEPSF),                                          !TWAG.56    SALE FOR FRACTION
CC     *             FS, TRABUF(TWEPRM),                                          !TWAG.57    RESERVE MASK
CC     *             FS, TRABUF(TWEPNE),                                          !TWAG.58    NUMBER OF GIVEN ENDING NUMBERS
CC     *             FS, TRABUF(TWEPNF),                                          !TWAG.59    NUMBER OF REQUESTED FRACTIONS
CC     *             FS, TRABUF(TWEPNR)                                           !TWAG.60    NUMBER OF RESERVATIONS/RELEASES
CC           ENDIF
CC         ENDIF
CC         WRITE(QLIKDATA.QLIK_LUN,*)                                             ! PRINT NEW LINE
CC       ENDIF
CCC
CC1000   FORMAT(           I0,                                                    !TWAG.1     STATUS
CC     *        A<FS_LEN>, I0,                                                    !TWAG.2     ERROR CODE
CC     *        A<FS_LEN>, A8,                                                    !TWAG.3     TRANSACTION DATE
CC     *        A<FS_LEN>, I0,                                                    !TWAG.4     INTERNAL SERIAL NUMBER
CC     *        A<FS_LEN>, A8,                                                    !TWAG.5     TIME STAMP HH:MI:SS
CC     *        A<FS_LEN>, I0,                                                    !TWAG.6     TERMINAL NUMBER
CC     *        A<FS_LEN>, A8,                                                    !TWAG.7     AGENT NUMBER
CC     *        A<FS_LEN>, I0,                                                    !TWAG.8     SAP NUMBER
CC     *        A<FS_LEN>, I0,                                                    !TWAG.9     TRANSACTION SEQUENCE NUMBER
CC     *        A<FS_LEN>, I0,                                                    !TWAG.10    TRANSACTION TYPE
CC     *        A<FS_LEN>, I0,                                                    !TWAG.11    GAME NUMBER
CC     *        A<FS_LEN>, I0,                                                    !TWAG.12    GAME TYPE
CC     *        A<FS_LEN>, I0,                                                    !TWAG.13    GAME INDEX
CC     *        A<FS_LEN>, I0,                                                    !TWAG.14    TERMINAL STATISTICS
CC     *        A<FS_LEN>, I0,                                                    !TWAG.15    INTERNAL TRANSACTION FLAG
CC     *        A<FS_LEN>, I0,                                                    !TWAG.16    FILE STATUS
CC     *        A<FS_LEN>, I0,                                                    !TWAG.17    TICKET ID
CC     *        A<FS_LEN>, I0,                                                    !TWAG.18    MESSAGE CHECKSUM
CC     *        A<FS_LEN>, I0,                                                    !TWAG.19    # OF FRACTIONS
CC     *        A<FS_LEN>, I0,                                                    !TWAG.20    TRANSACTION SIZE (# LOG RECS)
CC     *        A<FS_LEN>, I0,                                                    !TWAG.21    ERROR SUB CODE
CC     *        A<FS_LEN>, I0,                                                    !TWAG.22    CDC TRANS. WAS SOLD (NEVER CHANGES)
CC     *        A<FS_LEN>, I0,                                                    !TWAG.23    BET AMOUNT FLAG (FOR FRACTIONS)
CC     *        A<FS_LEN>, I0,                                                    !TWAG.24    REAL NUMBER OF FRACTIONS
CC     *        A<FS_LEN>, I0)                                                    !TWAG.25    PROMOTION - ADD 1 FREE WEEK
CC1001   FORMAT(A<FS_LEN>)                                                        !TWAG.26    FILLER FIELD (TRANSACTION SUBTYPE N/A))
CC1002   FORMAT(A<FS_LEN>, I0)                                                    !TWAG.26    EPASSIVE OPERATION REQUESTED
CCC WAGER DETAIL
CC1010   FORMAT(A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TWAG.27    TRANSACTION EXTERNAL SERIAL NUMBER
CC     *        A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TWAG.28    WAGER EXTERNAL SERIAL NUMBER
CC     *        A<FS_LEN>, A10,                                                   !TWAG.29    TOTAL AMOUNT
CC     *        A<FS_LEN>, A8,                                                    !TWAG.30    TIME STAMP HH:MI:SS
CC     *       4A<FS_LEN>,                                                        !TWAG.31-34 FILLER FIELDS
CC     *        A<FS_LEN>, A,                                                     !TWAG.35    DRAW NAME (MAIN GAME)
CC     *        A<FS_LEN>, A8,                                                    !TWAG.36    BASE AMOUNT (MAIN GAME)
CC     *        A<FS_LEN>, A,                                                     !TWAG.37    KICKER DRAW NAME
CC     *        A<FS_LEN>, A8,                                                    !TWAG.38    KICKER AMOUNT
CC     *        A<FS_LEN>, I0,                                                    !TWAG.39    NUMBER OF BETS/BOARDS
CC     *        A<FS_LEN>,                                                        !TWAG.40-40 FILLER FIELDS
CC     *        A<FS_LEN>, I0)                                                    !TWAG.41    QUICK PICK FLAGS
CCC SPORTS WAGER BODY
CC1020   FORMAT(3A<FS_LEN>,                                                       !TWAG.42-44 FILLER FIELDSA
CC     *        A<FS_LEN>, I0,                                                    !TWAG.45    SYSTEM TYPE
CC     *        A<FS_LEN>,                                                        !TWAG.46-46 FILLER FIELDS
CC     *        A<FS_LEN>, I0,                                                    !TWAG.47    KICKER SOLD FLAG
CC     *        A<FS_LEN>, I7.7,                                                  !TWAG.48    KICKER NUMBER
CC     *        A<FS_LEN>, A,                                                     !TWAG.49    EXTERNAL CANCEL SERIAL NUMBER
CC!     *        A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TWAG.49    EXTERNAL CANCEL SERIAL NUMBER
CC     *        A<FS_LEN>, I0,                                                    !TWAG.50    CANCEL TERMINAL #
CC     *        A<FS_LEN>, I0)                                                    !TWAG.51    # OF SIMPLE BETS
CCC KICKER WAGER BODY
CC1030   FORMAT(3A<FS_LEN>,                                                       !TWAG.42-44 FILLER FIELDSA
CC     *        A<FS_LEN>, I0,                                                    !TWAG.45    SYSTEM TYPE
CC     *        A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TWAG.46    EXTERNAL SERIAL EXTERNAL SERIAL NUMBER OF ASSOCIATED LINKED WAGER
CC     *        A<FS_LEN>, I0,                                                    !TWAG.47    KICKER SOLD FLAG
CC     *        A<FS_LEN>, I7.7,                                                  !TWAG.48    KICKER NUMBER
CC     *        A<FS_LEN>, A,                                                     !TWAG.49    EXTERNAL CANCEL SERIAL NUMBER
CC!     *        A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TWAG.49    EXTERNAL CANCEL SERIAL NUMBER
CC     *        A<FS_LEN>, I0,                                                    !TWAG.50    CANCEL TERMINAL #
CC     *        A<FS_LEN>, I0)                                                    !TWAG.51    # OF SIMPLE BETS
CCC LOTTO WAGER BODY
CC1040   FORMAT(A<FS_LEN>, I0,                                                    !TWAG.42    # OF LOTTO MARKS/BOARD
CC     *       2A<FS_LEN>,                                                        !TWAG.43-44
CC     *        A<FS_LEN>, I0,                                                    !TWAG.45    SYSTEM TYPE
CC     *        A<FS_LEN>,                                                        !TWAG.46-46 FILLER FIELDS
CC     *        A<FS_LEN>, I0,                                                    !TWAG.47    KICKER SOLD FLAG
CC     *        A<FS_LEN>, I7.7,                                                  !TWAG.48    KICKER NUMBER
CC     *        A<FS_LEN>, A,                                                     !TWAG.49    EXTERNAL CANCEL SERIAL NUMBER
CC!     *        A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TWAG.49    EXTERNAL CANCEL SERIAL NUMBER
CC     *        A<FS_LEN>, I0,                                                    !TWAG.50    CANCEL TERMINAL #
CC     *        A<FS_LEN>, I0,                                                    !TWAG.51    # OF SIMPLE BETS
CC     *        A<FS_LEN>, I0)                                                    !TWAG.52    LUCKY #
CCC EPASSIVE WAGER BODY
CC1050   FORMAT(7A<FS_LEN>,                                                       !TWAG.42-48 FILLER FIELDS
CC     *        A<FS_LEN>, A,                                                     !TWAG.49    EXTERNAL CANCEL SERIAL NUMBER
CC!     *        A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TWAG.49    EXTERNAL CANCEL SERIAL NUMBER
CC     *        A<FS_LEN>, I0,                                                    !TWAG.50    CANCEL TERMINAL #
CC     *       2A<FS_LEN>,                                                        !TWAG.51-52 FILLER FIELDS
CC     *        A<FS_LEN>, I2.2,'/',I4.4,                                         !TWAG.53    WEEK/YEAR
CC     *        A<FS_LEN>, I5.5,                                                  !TWAG.54    SALE FOR NUMBER
CC     *        A<FS_LEN>, I2.2,                                                  !TWAG.55    SALE FOR SERIE
CC     *        A<FS_LEN>, I2.2,                                                  !TWAG.56    SALE FOR FRACTION
CC     *        A<FS_LEN>, I<TRABUF(TWEPNE)>.<TRABUF(TWEPNE)>,                    !TWAG.57    RESERVE MASK
CC     *        A<FS_LEN>, I0,                                                    !TWAG.58    NUMBER OF GIVEN ENDING NUMBERS
CC     *        A<FS_LEN>, I0,                                                    !TWAG.59    NUMBER OF REQUESTED FRACTIONS
CC     *        A<FS_LEN>, I0)                                                    !TWAG.60    NUMBER OF RESERVATIONS/RELEASES
CC       RETURN
CC       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_TWCI (QLIKDATA,TRABUF)
C
C       THIS SUBROUTINE WRITES WAG,CAN AND INC TRANSACTIONS INTO QLIK FILE.
C
C       INPUTS:
C        QLIKDATA       QLIK DATA STRUCTURE
C        TRABUF         WAG/CAN/INCA TRANSACTION TO WRITE INTO THE FILE
C
C       OUTPUTS:
C        *NONE*
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE PRINT_TWCI(QLIKDATA,TRABUF)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:DESTRA.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:QLIKTRAN.DEF'
       INCLUDE 'INCLIB:AGTCOM.DEF'
C
       INTEGER*4 I
       INTEGER*4 JUL
       INTEGER*2 DBUF(12)
       CHARACTER*11 C11MONY
       CHARACTER*8  IAGT_NO                                                     !EXTERNAL FUNCTION
       CHARACTER*8  GET_YYYYMMDD_CDC                                            !EXTERNAL FUNCTION
       CHARACTER*19 PASTCKCODE                                                  !LOCAL FUNCTION
       CHARACTER*8  CDRWNAM                                                     !LOCAL FUNCTION
       CHARACTER*8  CAGTN                                                       !LOCAL FUNCTION
       INTEGER*4 EPTCKT
C
       INTEGER*4 XTSER, XTCHK, XWSER, XWCHK
       CHARACTER*16 C16XTCOD, C16XWCOD                                          !TRANSACTION/WAGER EXTERNAL CODE
       CHARACTER*8 DRWNAM, DRWKNAM
       CHARACTER*7 C7KICK                                                       !KIKCER
C
       DRWNAM  = '        '
       DRWKNAM = '        '
       C16XTCOD = '                '
       C16XWCOD = '                '
       C32TMP   = BLNK_SPCS_32
       C32TMP2  = BLNK_SPCS_32
       C32TMP3  = BLNK_SPCS_32
       C32TMP4  = BLNK_SPCS_32
       C32TMP5  = BLNK_SPCS_32
C
       XTSER   = 0
       XTCHK   = 0
       XWSER   = 0
       XWCHK   = 0
C
       DBUF(VCDC)=TRABUF(TCDC)
       CALL CDATE(DBUF)
       JUL=DBUF(VJUL)
C
C WAG/CAN/INCA TRANSACTION HEADER
       WRITE(QLIKDATA.QLIK_LUN,1000,ADVANCE='NO')
     *           TRABUF(TSTAT),                                                 !TWCI.1     STATUS
     *       FS, TRABUF(TERR),                                                  !TWCI.2     ERROR CODE
     *       FS, GET_YYYYMMDD_CDC(TRABUF(TCDC)),                                !TWCI.3     TRANSACTION DATE
     *       FS, TRABUF(TSER),                                                  !TWCI.4     INTERNAL SERIAL NUMBER
     *       FS, DISTIM(TRABUF(TTIM)),                                          !TWCI.5     TIME STAMP HH:MI:SS
     *       FS, TRABUF(TTER),                                                  !TWCI.6     TERMINAL NUMBER
     *       FS, IAGT_NO(TRABUF(TAGT)),                                         !TWCI.7     AGENT NUMBER
     *       FS, AGTSAP(TRABUF(TTER)),                                          !TWCI.8     SAP NUMBER
     *       FS, TRABUF(TTRN),                                                  !TWCI.9     TRANSACTION SEQUENCE NUMBER
     *       FS, TRABUF(TTYP),                                                  !TWCI.10    TRANSACTION TYPE
     *       FS, TRABUF(TGAM),                                                  !TWCI.11    GAME NUMBER
     *       FS, TRABUF(TGAMTYP),                                               !TWCI.12    GAME TYPE
     *       FS, TRABUF(TGAMIND),                                               !TWCI.13    GAME INDEX
     *       FS, TRABUF(TTSTCS),                                                !TWCI.14    TERMINAL STATISTICS
     *       FS, TRABUF(TINTRA),                                                !TWCI.15    INTERNAL TRANSACTION FLAG
     *       FS, TRABUF(TFIL),                                                  !TWCI.16    FILE STATUS
     *       FS, TRABUF(TTKID),                                                 !TWCI.17    TICKET ID
     *       FS, TRABUF(TCHK),                                                  !TWCI.18    MESSAGE CHECKSUM
     *       FS, TRABUF(TFRAC),                                                 !TWCI.19    # OF FRACTIONS
     *       FS, TRABUF(TSIZE),                                                 !TWCI.20    TRANSACTION SIZE (# LOG RECS)
     *       FS, TRABUF(TSUBERR),                                               !TWCI.21    ERROR SUB CODE
     *       FS, TRABUF(TCDC_SOLD),                                             !TWCI.22    CDC TRANS. WAS SOLD (NEVER CHANGES)
     *       FS, TRABUF(TFAMTFLG),                                              !TWCI.23    BET AMOUNT FLAG (FOR FRACTIONS)
     *       FS, TRABUF(TNFRAC),                                                !TWCI.24    REAL NUMBER OF FRACTIONS
     *       FS, TRABUF(TWADDFW)                                                !TWCI.25    PROMOTION - ADD 1 FREE WEEK
       IF(TRABUF(TGAMTYP) .NE. TPAS) THEN
         WRITE(QLIKDATA.QLIK_LUN,1001,ADVANCE=WRITE__ADVANCE_VALUE)
     *      (FS, I=26,26)                                                       !TWCI.26-26 FILLER FIELD (TRANSACTION SUBTYPE N/A)
       ELSE
         WRITE(QLIKDATA.QLIK_LUN,1002,ADVANCE=WRITE__ADVANCE_VALUE)
     *       FS, TRABUF(TWEPOP)                                                 !TWCI.26    EPASSIVE OPERATION REQUESTED
       ENDIF
C
       IF(TDETAIL) THEN                                                         !WAG/CAN/INCA DETAIL
         CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),XTSER,XTCHK)
         WRITE(C16XTCOD,'(I3.3,A1,I8.8,A1,I3.3)') JUL,'-',XTSER,'-',XTCHK       !EXTERNAL TRANSACTION SERIAL NUMBER
         IF(TRABUF(TTYP).EQ.TWAG) THEN
           IF(.NOT. (TRABUF(TGAMTYP).EQ.TPAS .AND.
     *               TRABUF(TWEPOP).NE.EPASSAL)) THEN
             C16XWCOD = C16XTCOD                                                !EXTERNAL WAGER SERIAL NUMBER
             IF(TRABUF(TWCSER).GT.0) THEN                                       !THIS WAGER HAS BEEN CANCELLED
                C32TMP5 = CAGTN(TRABUF(TWCTER))
             ENDIF
           ENDIF
         ELSE                                                                   !TCAN/TINC TRANSACTION TYPE
           CALL OUTGEN(TRABUF(TCDC),TRABUF(TWCSER),XWSER,XWCHK)
           WRITE(C16XWCOD,'(I3.3,A1,I8.8,A1,I3.3)') JUL,'-',XWSER,'-',XWCHK     !EXTERNAL WAGER SERIAL NUMBER CANCELLED
           C32TMP5 = CAGTN(TRABUF(TWCTER))
         ENDIF
C
         DRWNAM = CDRWNAM(TRABUF)
         IF(TRABUF(TWKFLG)) THEN
           DRWKNAM = CDRWNAM(TRABUF)
           WRITE(C32TMP3,'(A8)') CSMONY(TRABUF(TWKAMT),8,BETUNIT)               !KICKER AMOUNT
           WRITE(C32TMP4,'(I7.7)') TRABUF(TWKICK)                               !KICKER NUMBER
         ENDIF
         WRITE(C32TMP,'(A10)') CSMONY(TRABUF(TWTOT),10,BETUNIT)                 !TOTAL AMOUNT
         WRITE(C32TMP2,'(A8)') CSMONY(TRABUF(TWAMT),8,BETUNIT)                  !BASE AMOUNT
         WRITE(QLIKDATA.QLIK_LUN,1010,ADVANCE='NO')
     *         FS, TRIM(C16XTCOD),                                              !TWCI.27    EXTERNAL TRANSACTION SERIAL NUMBER
     *         FS, TRIM(C16XWCOD),                                              !TWCI.28    EXTERNAL WAGER SERIAL NUMBER
     *         FS, TRIM(ADJUSTL(C32TMP)),                                       !TWCI.29    TOTAL AMOUNT
     *         FS, DISTIM(TRABUF(TTIM)),                                        !TWCI.30    TIME STAMP HH:MI:SS
     *        (FS, I=31,34),                                                    !TWCI.31-34 FILLER FIELDS
     *         FS, TRIM(DRWNAM),                                                !TWCI.35    (STARTING) DRAW NAME
     *         FS, TRIM(ADJUSTL(C32TMP2)),                                      !TWCI.36    BASE AMOUNT (MAIN GAME)
     *         FS, TRIM(DRWKNAM),                                               !TWCI.37    KICKER (START) DRAW NAME
     *         FS, TRIM(ADJUSTL(C32TMP3)),                                      !TWCI.38    KICKER AMOUNT
     *         FS, TRABUF(TWNBET),                                              !TWCI.39    NUMBER OF BETS/BOARDS
     *        (FS, I=40,40),                                                    !TWCI.40-40 FILLER FIELDS
     *         FS, TRABUF(TWQPF)                                                !TWCI.41    QUICK PICK FLAGS
         IF(TRABUF(TGAMTYP) .EQ. TLTO) THEN
           WRITE(QLIKDATA.QLIK_LUN,1040,ADVANCE='NO')
     *           FS, TRABUF(TWNMRK),                                            !TWCI.42    # OF LOTTO MARKS/BOARD
     *          (FS, I=43,44),                                                  !TWCI.43-44 FILLER FIELDS
     *           FS, TRABUF(TWSYST),                                            !TWCI.45    SYSTEM TYPE
     *          (FS, I=46,46),                                                  !TWCI.46-46 FILLER FIELDS
     *           FS, TRABUF(TWKFLG),                                            !TWCI.47    KICKER SOLD FLAG
     *           FS, TRIM(C32TMP4),                                             !TWCI.48    KICKER NUMBER
     *           FS, TRIM(C32TMP5),                                             !TWCI.49    CANCEL TERMINAL #
     *           FS, TRABUF(TWSIMP),                                            !TWCI.50    # OF SIMPLE BETS
     *           FS, TRABUF(TWLUCK)                                             !TWCI.51    LUCKY #
         ELSEIF(TRABUF(TGAMTYP) .EQ. TKIK) THEN
           WRITE(QLIKDATA.QLIK_LUN,1030,ADVANCE='NO')
     *          (FS, I=42,44),                                                  !TWCI.42-44 FILLER FIELDS
     *           FS, TRABUF(TWSYST),                                            !TWCI.45    SYSTEM TYPE
     *           FS, JUL,                                                       !TWCI.46    EXTERNAL WAGER JULIAN DATE
     *               TRABUF(TWLNKSER),                                          !TWCI.46    EXTERNAL SERIAL NUMBER OF ASSOCIATED LINKED WAGER
     *               TRABUF(TWLNKCHK),                                          !TWCI.46    EXTERNAL CHECK DIGITS OF ASSOCIATED LINKED WAGER
     *           FS, TRABUF(TWKFLG),                                            !TWCI.47    KICKER SOLD FLAG
     *           FS, TRIM(C32TMP4),                                             !TWCI.48    KICKER NUMBER
     *           FS, TRIM(C32TMP5)                                              !TWCI.49    CANCEL TERMINAL #
         ELSEIF(TRABUF(TGAMTYP) .EQ. TSPT) THEN
           WRITE(QLIKDATA.QLIK_LUN,1020,ADVANCE='NO')
     *          (FS, I=42,44),                                                  !TWCI.42-44 FILLER FIELDS
     *           FS, TRABUF(TWSYST),                                            !TWCI.45    SYSTEM TYPE
     *          (FS, I=46,46),                                                  !TWCI.46-46 FILLER FIELDS
     *           FS, TRABUF(TWKFLG),                                            !TWCI.47    KICKER SOLD FLAG
     *           FS, TRIM(C32TMP4),                                             !TWCI.48    KICKER NUMBER
     *           FS, TRIM(C32TMP5),                                             !TWCI.49    CANCEL TERMINAL #
     *           FS, TRABUF(TWSIMP)                                             !TWCI.50    # OF SIMPLE BETS
         ELSEIF(TRABUF(TGAMTYP) .EQ. TPAS) THEN
           EPTCKT = 0
           IF(TRABUF(TWEPOP).EQ. EPASRES) THEN                                  !EPASSIVE RESERVATION
             EPTCKT = TRABUF(TWEPRES1)                                          !NUMBER RESERVED 1
             WRITE(QLIKDATA.QLIK_LUN,1050,ADVANCE='NO')
     *            (FS, I=42,48),                                                !TWCI.42-48 FILLER FIELDS
     *             FS, TRIM(C32TMP5),                                           !TWCI.49    CANCEL TERMINAL #
     *            (FS, I=50,52),                                                !TWCI.50-52 FILLER FIELDS
     *             FS, EPTCKT,                                                  !TWCI.53    NUMBER RESERVED
     *            (FS, I=54,55),                                                !TWCI.54-55 FILLER FIELDS
     *             FS, TRABUF(TWEPRM),                                          !TWCI.56    RESERVE MASK
     *             FS, TRABUF(TWEPNF),                                          !TWCI.57    NUMBER OF REQUESTED FRACTIONS
     *             FS, TRABUF(TWEPNR)                                           !TWCI.58    NUMBER OF RESERVATIONS
           ELSEIF(TRABUF(TWEPOP) .EQ. EPASSAL) THEN                             !EPASSIVE SALE
             EPTCKT = TRABUF(TWEPSN)                                            !SALE FOR NUMBER
             WRITE(QLIKDATA.QLIK_LUN,1060,ADVANCE='NO')
     *            (FS, I=42,48),                                                !TWCI.42-48 FILLER FIELDS
     *             FS, TRIM(C32TMP5),                                           !TWCI.49    CANCEL TERMINAL #
     *            (FS, I=50,51),                                                !TWCI.50-51 FILLER FIELDS
     *             FS, PASTCKCODE(TRABUF),                                      !TWCI.52    ETICKET EXTERNAL CODE
!     *             FS, TRABUF(TGAMIND),                                         !TWCI.52    PASSIVE GAME INDEX
!     *                 TRABUF(TWEPWK),                                          !TWCI.52    WEEK
!     *                 TRABUF(TWEPYR),                                          !TWCI.52    YEAR
!     *                 EPTCKT,                                                  !TWCI.52    SALE FOR NUMBER
!     *                 TRABUF(TWEPSS),                                          !TWCI.52    SALE FOR SERIE
!     *                 TRABUF(TWEPSF),                                          !TWCI.52    SALE FOR FRACTION
     *             FS, EPTCKT,                                                  !TWCI.53    SALE FOR NUMBER
     *             FS, TRABUF(TWEPSS),                                          !TWCI.54    SALE FOR SERIE
     *             FS, TRABUF(TWEPSF)                                           !TWCI.55    SALE FOR FRACTION
           ELSEIF(TRABUF(TWEPOP).EQ. EPASREL) THEN                              !EPASSIVE RELEASE
             EPTCKT = TRABUF(TWEPRES1)                                          !NUMBER RESERVED 1 RELEASED
             WRITE(QLIKDATA.QLIK_LUN,1070,ADVANCE='NO')
     *            (FS, I=42,48),                                                !TWCI.42-48 FILLER FIELDS
     *             FS, TRIM(C32TMP5),                                           !TWCI.49    CANCEL TERMINAL #
     *            (FS, I=50,52),                                                !TWCI.50-52 FILLER FIELDS
     *             FS, EPTCKT,                                                  !TWCI.53    NUMBER RELEASED
     *            (FS, I=54,57),                                                !TWCI.54-57 FILLER FIELDS
     *             FS, TRABUF(TWEPNR)                                           !TWCI.58    NUMBER OF RELEASES
           ENDIF
         ENDIF
         WRITE(QLIKDATA.QLIK_LUN,*)                                             !PRINT NEW LINE
       ENDIF
C WAG/CAN/INCA TRANSACTION HEADER
1000   FORMAT(           I0,                                                    !TWCI.1     STATUS
     *        A<FS_LEN>, I0,                                                    !TWCI.2     ERROR CODE
     *        A<FS_LEN>, A8,                                                    !TWCI.3     TRANSACTION DATE
     *        A<FS_LEN>, I0,                                                    !TWCI.4     INTERNAL SERIAL NUMBER
     *        A<FS_LEN>, A8,                                                    !TWCI.5     TIME STAMP HH:MI:SS
     *        A<FS_LEN>, I0,                                                    !TWCI.6     TERMINAL NUMBER
     *        A<FS_LEN>, A8,                                                    !TWCI.7     AGENT NUMBER
     *        A<FS_LEN>, I0,                                                    !TWCI.8     SAP NUMBER
     *        A<FS_LEN>, I0,                                                    !TWCI.9     TRANSACTION SEQUENCE NUMBER
     *        A<FS_LEN>, I0,                                                    !TWCI.10    TRANSACTION TYPE
     *        A<FS_LEN>, I0,                                                    !TWCI.11    GAME NUMBER
     *        A<FS_LEN>, I0,                                                    !TWCI.12    GAME TYPE
     *        A<FS_LEN>, I0,                                                    !TWCI.13    GAME INDEX
     *        A<FS_LEN>, I0,                                                    !TWCI.14    TERMINAL STATISTICS
     *        A<FS_LEN>, I0,                                                    !TWCI.15    INTERNAL TRANSACTION FLAG
     *        A<FS_LEN>, I0,                                                    !TWCI.16    FILE STATUS
     *        A<FS_LEN>, I0,                                                    !TWCI.17    TICKET ID
     *        A<FS_LEN>, I0,                                                    !TWCI.18    MESSAGE CHECKSUM
     *        A<FS_LEN>, I0,                                                    !TWCI.19    # OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TWCI.20    TRANSACTION SIZE (# LOG RECS)
     *        A<FS_LEN>, I0,                                                    !TWCI.21    ERROR SUB CODE
     *        A<FS_LEN>, I0,                                                    !TWCI.22    CDC TRANS. WAS SOLD (NEVER CHANGES)
     *        A<FS_LEN>, I0,                                                    !TWCI.23    BET AMOUNT FLAG (FOR FRACTIONS)
     *        A<FS_LEN>, I0,                                                    !TWCI.24    REAL NUMBER OF FRACTIONS
     *        A<FS_LEN>, I0)                                                    !TWCI.25    PROMOTION - ADD 1 FREE WEEK
1001   FORMAT(A<FS_LEN>)                                                        !TWCI.26    FILLER FIELD (TRANSACTION SUBTYPE N/A))
1002   FORMAT(A<FS_LEN>, I0)                                                    !TWCI.26    EPASSIVE OPERATION REQUESTED
C WAG/CAN/INCA DETAIL
1010   FORMAT(A<FS_LEN>, A,                                                     !TWCI.27    TRANSACTION EXTERNAL SERIAL NUMBER
     *        A<FS_LEN>, A,                                                     !TWCI.28    WAGER EXTERNAL SERIAL NUMBER
     *        A<FS_LEN>, A,                                                     !TWCI.29    TOTAL AMOUNT
     *        A<FS_LEN>, A8,                                                    !TWCI.30    TIME STAMP HH:MI:SS
     *       4A<FS_LEN>,                                                        !TWCI.31-34 FILLER FIELDS
     *        A<FS_LEN>, A,                                                     !TWCI.35    DRAW NAME (MAIN GAME)
     *        A<FS_LEN>, A,                                                     !TWCI.36    BASE AMOUNT (MAIN GAME)
     *        A<FS_LEN>, A,                                                     !TWCI.37    KICKER DRAW NAME
     *        A<FS_LEN>, A,                                                     !TWCI.38    KICKER AMOUNT
     *        A<FS_LEN>, I0,                                                    !TWCI.39    NUMBER OF BETS/BOARDS
     *        A<FS_LEN>,                                                        !TWCI.40-40 FILLER FIELDS
     *        A<FS_LEN>, I0)                                                    !TWCI.41    QUICK PICK FLAGS
C SPORTS WAGER BODY
1020   FORMAT(3A<FS_LEN>,                                                       !TWCI.42-44 FILLER FIELDSA
     *        A<FS_LEN>, I0,                                                    !TWCI.45    SYSTEM TYPE
     *        A<FS_LEN>,                                                        !TWCI.46-46 FILLER FIELDS
     *        A<FS_LEN>, I0,                                                    !TWCI.47    KICKER SOLD FLAG
     *        A<FS_LEN>, A,                                                     !TWCI.48    KICKER NUMBER
     *        A<FS_LEN>, A,                                                     !TWCI.49    CANCEL TERMINAL #
     *        A<FS_LEN>, I0)                                                    !TWCI.50    # OF SIMPLE BETS
C KICKER WAGER BODY
1030   FORMAT(3A<FS_LEN>,                                                       !TWCI.42-44 FILLER FIELDSA
     *        A<FS_LEN>, I0,                                                    !TWCI.45    SYSTEM TYPE
     *        A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TWCI.46    EXTERNAL SERIAL EXTERNAL SERIAL NUMBER OF ASSOCIATED LINKED WAGER
     *        A<FS_LEN>, I0,                                                    !TWCI.47    KICKER SOLD FLAG
     *        A<FS_LEN>, A,                                                     !TWCI.48    KICKER NUMBER
     *        A<FS_LEN>, A)                                                     !TWCI.49    CANCEL TERMINAL #
C LOTTO WAGER BODY
1040   FORMAT(A<FS_LEN>, I0,                                                    !TWCI.42    # OF LOTTO MARKS/BOARD
     *       2A<FS_LEN>,                                                        !TWCI.43-44
     *        A<FS_LEN>, I0,                                                    !TWCI.45    SYSTEM TYPE
     *        A<FS_LEN>,                                                        !TWCI.46-46 FILLER FIELDS
     *        A<FS_LEN>, I0,                                                    !TWCI.47    KICKER SOLD FLAG
     *        A<FS_LEN>, A,                                                     !TWCI.48    KICKER NUMBER
     *        A<FS_LEN>, A,                                                     !TWCI.49    CANCEL TERMINAL #
     *        A<FS_LEN>, I0,                                                    !TWCI.50    # OF SIMPLE BETS
     *        A<FS_LEN>, I0)                                                    !TWCI.51    LUCKY #
C EPASSIVE RESERVATION BODY
1050   FORMAT(7A<FS_LEN>,                                                       !TWCI.42-48 FILLER FIELDS
     *        A<FS_LEN>, A,                                                     !TWCI.49    CANCEL TERMINAL #
     *       3A<FS_LEN>,                                                        !TWCI.50-52 FILLER FIELDS
     *        A<FS_LEN>, I5.5,                                                  !TWCI.53    NUMBER RESERVED
     *       2A<FS_LEN>,                                                        !TWCI.54-55 FILLER FIELDS
     *        A<FS_LEN>, I<TRABUF(TWEPNE)>.<TRABUF(TWEPNE)>,                    !TWCI.56    RESERVE MASK
     *        A<FS_LEN>, I0,                                                    !TWCI.57    NUMBER OF REQUESTED FRACTIONS
     *        A<FS_LEN>, I0)                                                    !TWCI.58    NUMBER OF RESERVATIONS
C EPASSIVE WAGER BODY
1060   FORMAT(7A<FS_LEN>,                                                       !TWCI.42-48 FILLER FIELDS
     *        A<FS_LEN>, A,                                                     !TWCI.49    CANCEL TERMINAL #
     *       2A<FS_LEN>,                                                        !TWCI.50-51 FILLER FIELDS
!     *        A<FS_LEN>, I0,'-',I2.2,'-',I2.2,'-',I5.5,'-',I2.2,'-',I2.2,       !TWCI.52    ETICKET EXTERNAL CODE
     *        A<FS_LEN>, A,                                                     !TWCI.52    ETICKET EXTERNAL CODE
     *        A<FS_LEN>, I5.5,                                                  !TWCI.53    SALE FOR NUMBER
     *        A<FS_LEN>, I2.2,                                                  !TWCI.54    SALE FOR SERIE
     *        A<FS_LEN>, I2.2)                                                  !TWCI.55    SALE FOR FRACTION
C EPASSIVE RELEASE BODY
1070   FORMAT(7A<FS_LEN>,                                                       !TWCI.42-48 FILLER FIELDS
     *        A<FS_LEN>, A,                                                     !TWCI.49    CANCEL TERMINAL #
     *       3A<FS_LEN>,                                                        !TWCI.50-52 FILLER FIELDS
     *        A<FS_LEN>, I5.5,                                                  !TWCI.53    NUMBER RELEASED
     *       4A<FS_LEN>,                                                        !TWCI.54-57 FILLER FIELDS
     *        A<FS_LEN>, I0)                                                    !TWCI.58    NUMBER OF RELEASES
C
       RETURN
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_TIGS (QLIKDATA,TRABUF)
C
C       THIS SUBROUTINE WRITES IGS TRANSACTIONS INTO QLIK FILE.
C
C       INPUTS:
C        QLIKDATA       QLIK DATA STRUCTURE
C        TRABUF         IGS TRANSACTION TO WRITE INTO THE FILE
C
C       OUTPUTS:
C        *NONE*
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE PRINT_TIGS(QLIKDATA,TRABUF)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:DESTRA.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:IGSTNAMES.DEF'
       INCLUDE 'INCLIB:QLIKTRAN.DEF'
       INCLUDE 'INCLIB:AGTCOM.DEF'
C
       INTEGER*4 LUN
C
       CHARACTER*11 C11MONY
       CHARACTER*8 IAGT_NO                                                      !EXTERNAL FUNCTION
       CHARACTER*8 GET_YYYYMMDD_CDC                                             !EXTERNAL FUNCTION
       INTEGER*4   ODS_BETTYP                                                   !LOCAL FUNCTION
C
       INTEGER*4 I
       INTEGER*4 CXERR_LEN
       PARAMETER (CXERR_LEN = 9)                                                !XERR HAS 9 CHARACTERS
       CHARACTER*12 CXERR                                                       !XERR_I4LEN * 4
C
       INTEGER*8  I8TMP
       INTEGER*4  I4TMP(2)
       EQUIVALENCE (I8TMP,I4TMP)
C
       INTEGER*8  WEXSER                                                        !WAGER EXTERNAL SERIAL
       INTEGER*8  CEXSER                                                        !CANCEL EXTERNAL SERIAL
       INTEGER*8  PEXSER                                                        !PAYMENT EXTERNAL SERIAL
       INTEGER*8  TMSGID                                                        !TERMINAL MESSAGE ID
       INTEGER*8  MEDVER                                                        !MEDIA VERSION
C
       INTEGER*4  BETTYP                                                        !BET TYPE (SIMPLE, MULTIPLE OR COMBINED)
C
       INTEGER*4  NIB(6)
       CHARACTER*24 CNIB
       EQUIVALENCE (NIB,CNIB)
       INTEGER*4  BLANK
C
C IGS TRANSACTION HEADER
       WRITE(CXERR,'(<XERR_I4LEN>A4)') (TRABUF(TIGS_XERR+I),I=0,XERR_I4LEN-1)
       WRITE(QLIKDATA.QLIK_LUN,1000,ADVANCE=WRITE__ADVANCE_VALUE)
     *           TRABUF(TSTAT),                                                 !TIGS.1     STATUS
     *       FS, TRABUF(TERR),                                                  !TIGS.2     ERROR CODE
     *       FS, GET_YYYYMMDD_CDC(TRABUF(TCDC)),                                !TIGS.3     TRANSACTION DATE
     *       FS, TRABUF(TSER),                                                  !TIGS.4     INTERNAL SERIAL NUMBER
     *       FS, DISTIM(TRABUF(TTIM)),                                          !TIGS.5     TIME STAMP HH:MI:SS
     *       FS, TRABUF(TTER),                                                  !TIGS.6     TERMINAL NUMBER
     *       FS, IAGT_NO(TRABUF(TAGT)),                                         !TIGS.7     AGENT NUMBER
     *       FS, AGTSAP(TRABUF(TTER)),                                          !TIGS.8     SAP NUMBER
     *       FS, TRABUF(TTRN),                                                  !TIGS.9     TRANSACTION SEQUENCE NUMBER
     *       FS, TRABUF(TTYP),                                                  !TIGS.10    TRANSACTION TYPE
     *       FS, TRABUF(TGAM),                                                  !TIGS.11    GAME NUMBER
     *       FS, TRABUF(TGAMTYP),                                               !TIGS.12    GAME TYPE
     *       FS, TRABUF(TGAMIND),                                               !TIGS.13    GAME INDEX
     *       FS, TRABUF(TTSTCS),                                                !TIGS.14    TERMINAL STATISTICS
     *       FS, TRABUF(TINTRA),                                                !TIGS.15    INTERNAL TRANSACTION FLAG
     *       FS, TRABUF(TFIL),                                                  !TIGS.16    FILE STATUS
     *       FS, TRABUF(TTKID),                                                 !TIGS.17    TICKET ID
     *       FS, TRABUF(TCHK),                                                  !TIGS.18    MESSAGE CHECKSUM
     *       FS, TRABUF(TFRAC),                                                 !TIGS.19    # OF FRACTIONS
     *       FS, TRABUF(TSIZE),                                                 !TIGS.20    TRANSACTION SIZE (# LOG RECS)
     *       FS, TRABUF(TSUBERR),                                               !TIGS.21    ERROR SUB CODE
     *       FS, TRABUF(TCDC_SOLD),                                             !TIGS.22    CDC TRANS. WAS SOLD (NEVER CHANGES)
     *       FS, TRABUF(TFAMTFLG),                                              !TIGS.23    BET AMOUNT FLAG (FOR FRACTIONS)
     *       FS, TRABUF(TNFRAC),                                                !TIGS.24    REAL NUMBER OF FRACTIONS
     *       FS, TRABUF(TWADDFW),                                               !TIGS.25    PROMOTION - ADD 1 FREE WEEK
     *       FS, TRABUF(TIGS_TTYP)                                              !TIGS.26    IGS TRANSACTION TYPE
       IF(TDETAIL) THEN
         IF(TRABUF(TIGS_TTYP) .EQ. IGSWAG) THEN
           I4TMP(2) = TRABUF(TIGSW_MIDH)
           I4TMP(1) = TRABUF(TIGSW_MIDL)
           TMSGID = I8TMP                                                       !TERMINAL MESSAGE ID (WAGER REQUEST)
           I4TMP(2) = TRABUF(TIGSW_WRSH)
           I4TMP(1) = TRABUF(TIGSW_WRSL)
           WEXSER = I8TMP                                                       !BET REFERENCE SERIAL NUMBER
C
           WRITE(QLIKDATA.QLIK_LUN,1100)
     *           FS, TRABUF(TIGSW_WRDY),                                        !TIGS.27    BET REFERENCE DATE YEAR (YY, LAST TWO DIGITS ONLY) (OUT)
     *               TRABUF(TIGSW_WRDM),                                        !TIGS.27    BET REFERENCE DATE MONTH (MM) (OUT)
     *               TRABUF(TIGSW_WRDD),                                        !TIGS.27    BET REFERENCE DATE DAY (DD)   (OUT)
     *               TRABUF(TIGSW_WRGM),                                        !TIGS.27    BET REFERENCE GAME            (OUT)
     *               WEXSER,                                                    !TIGS.27    BET REFERENCE SERIAL NUMBER   (OUT)
     *               TRABUF(TIGSW_WRCD),                                        !TIGS.27    BET REFERENCE CHECK DIGITS    (OUT)
     *           FS, TRABUF(TIGSW_WRDY),                                        !TIGS.28    BET REFERENCE DATE YEAR (YY, LAST TWO DIGITS ONLY) (OUT)
     *               TRABUF(TIGSW_WRDM),                                        !TIGS.28    BET REFERENCE DATE MONTH (MM) (OUT)
     *               TRABUF(TIGSW_WRDD),                                        !TIGS.28    BET REFERENCE DATE DAY (DD)   (OUT)
     *               TRABUF(TIGSW_WRGM),                                        !TIGS.28    BET REFERENCE GAME            (OUT)
     *               WEXSER,                                                    !TIGS.28    BET REFERENCE SERIAL NUMBER   (OUT)
     *               TRABUF(TIGSW_WRCD),                                        !TIGS.28    BET REFERENCE CHECK DIGITS    (OUT)
     *           FS, CMONY(TRABUF(TIGSW_TSTK),10,BETUNIT),                      !TIGS.29    BET TOTAL STAKE/TRANSACTION AMOUNT (OUT)
     *           FS, TRABUF(TIGSW_WCDY),                                        !TIGS.30    BET CREATION DATE YEAR (YYYY) (OUT)
     *               TRABUF(TIGSW_WCDM),                                        !TIGS.30    BET CREATION DATE MONTH (MM)  (OUT)
     *               TRABUF(TIGSW_WCDD),                                        !TIGS.30    BET CREATION DATE DAY (DD)    (OUT)
     *               TRABUF(TIGSW_WCTH),                                        !TIGS.30    BET CREATION TIME HOUR (HH)   (OUT)
     *               TRABUF(TIGSW_WCTM),                                        !TIGS.30    BET CREATION TIME MINUTE (MI) (OUT)
     *               TRABUF(TIGSW_WCTS),                                        !TIGS.30    BET CREATION TIME SECOND (SS) (OUT)
     *           FS, SYSTEMERRORCODE(TRABUF(TIGS_SERR)),                        !TIGS.31    SYSTEM CODE WHERE ERROR OCCURRED (OUT)
     *               CXERR,                                                     !TIGS.31    IGS ERROR CODE DESCRIPTION (OUT)
     *           FS, TRABUF(TIGSW_PNIF),                                        !TIGS.32    PORTUGUESE PLAYER VAT IDENTIFICATION NUMBER (IN)
     *           FS, TRABUF(TIGS_XREF),                                         !TIGS.33    MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
     *           FS, TMSGID,                                                    !TIGS.34    TERMINAL MESSAGE ID (IN)
     *          (FS, I=35,35),                                                  !TIGS.35-35 FILLER FIELD
     *           FS, CMONY(TRABUF(TIGSW_USTK),10,BETUNIT),                      !TIGS.36    UNIT STAKE OF THE BET (IN/OUT)
     *          (FS, I=37,38),                                                  !TIGS.37-38 FILLER FIELDS
     *           FS, TRABUF(TIGSW_TBET),                                        !TIGS.39    TOTAL BETS/NUMBER OF SELECTIONS (MAX = 8) (IN/OUT)
     *           FS, CMONY(TRABUF(TIGSW_MAXR),10,BETUNIT),                      !TIGS.40    BET MAXIMUM POSSIBLE RETURNS    (OUT)
     *          (FS, I=41,44),                                                  !TIGS.41-44 FILLER FIELDS
     *           FS, ODS_BETTYP(TRABUF)                                         !TIGS.45    PLACARD BET TYPE
!     *           FS, TRABUF(TIGSW_XGID),                                        ! ABP GAME ID (IN/OUT)
!     *           FS, TRABUF(TIGSW_STID),                                        ! SUBTYPE ID (IN/OUT)
!     *           FS, TRABUF(TIGSW_LEDY),                                        ! BET LAST EVENT DATE YEAR (YYYY) (OUT)
!     *               TRABUF(TIGSW_LEDM),                                        ! BET LAST EVENT DATE MONTH (MM)  (OUT)
!     *               TRABUF(TIGSW_LEDD),                                        ! BET LAST EVENT DATE DAY (DD)    (OUT)

         ELSEIF(TRABUF(TIGS_TTYP) .EQ. IGSVAL) THEN
           I4TMP(2) = TRABUF(TIGSV_MIDH)
           I4TMP(1) = TRABUF(TIGSV_MIDL)
           TMSGID = I8TMP                                                       !TERMINAL MESSAGE ID (VALIDATION REQUEST)
           I4TMP(2) = TRABUF(TIGSV_WRSH)
           I4TMP(1) = TRABUF(TIGSV_WRSL)
           WEXSER = I8TMP
           WRITE(QLIKDATA.QLIK_LUN,1200)
     *          (FS, I=27,27),                                                  !TIGS.27    FILLER FIELD
     *           FS, TRABUF(TIGSV_WRDY),                                        !TIGS.28    BET REFERENCE DATE YEAR (YY, LAST TWO DIGITS ONLY) (IN)
     *               TRABUF(TIGSV_WRDM),                                        !TIGS.28    BET REFERENCE DATE MONTH (MM) (IN)
     *               TRABUF(TIGSV_WRDD),                                        !TIGS.28    BET REFERENCE DATE DAY (DD)   (IN)
     *               TRABUF(TIGSV_WRGM),                                        !TIGS.28    BET REFERENCE GAME            (IN)
     *               WEXSER,                                                    !TIGS.28    BET REFERENCE SERIAL NUMBER   (IN)
     *               TRABUF(TIGSV_WRCD),                                        !TIGS.28    BET REFERENCE CHECK DIGITS    (IN)
     *          (FS, I=29,29),                                                  !TIGS.29-29 FILLER FIELD
     *           FS, TRABUF(TIGSV_WVDY),                                        !TIGS.30    WAGER VALIDATION DATE YEAR (YYYY) (OUT)
     *               TRABUF(TIGSV_WVDM),                                        !TIGS.30    WAGER VALIDATION DATE MONTH (MM)  (OUT)
     *               TRABUF(TIGSV_WVDD),                                        !TIGS.30    WAGER VALIDATION DATE YEAR (DD)   (OUT)
     *               TRABUF(TIGSV_WVTH),                                        !TIGS.30    WAGER VALIDATION TIME HOUR (HH)   (OUT)
     *               TRABUF(TIGSV_WVTM),                                        !TIGS.30    WAGER VALIDATION TIME MINUTE (MI) (OUT)
     *               TRABUF(TIGSV_WVTS),                                        !TIGS.30    WAGER VALIDATION TIME SECOND (SS) (OUT)
     *           FS, SYSTEMERRORCODE(TRABUF(TIGS_SERR)),                        !TIGS.31    SYSTEM CODE WHERE ERROR OCCURRED  (OUT)
     *               CXERR,                                                     !TIGS.31    IGS ERROR CODE DESCRIPTION (OUT)
     *           FS, TRABUF(TIGSV_PNIF),                                        !TIGS.32    PORTUGUESE PLAYER VAT IDENTIFICATION NUMBER (9 DIGITS) (OUT)
     *           FS, TRABUF(TIGS_XREF),                                         !TIGS.33    MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
     *           FS, TMSGID,                                                    !TIGS.34    TERMINAL MESSAGE ID (IN)
     *          (FS, I=35,58),                                                  !TIGS.35-58 FILLER FIELDS
     *           FS, TRABUF(TIGSV_PMOD),                                        !TIGS.59    PAYMENT MODE (OUT)
     *           FS, CMONY(TRABUF(TIGSV_TPRZ),10,VALUNIT),                      !TIGS.60    TOTAL PRIZE AMOUNT (VALIDATION UNITS) (OUT)
!     *           FS, CMONY(TRABUF(TIGSV_TTAX),10,VALUNIT),                      ! TOTAL TAX AMOUNT (VALIDATION UNITS)   (OUT)
!     *           FS, CMONY(TRABUF(TIGSV_NPRZ),10,VALUNIT),                      ! NET PRIZE AMOUNT (VALIDATION UNITS)   (OUT)
     *          (FS, I=61,64),                                                  !TIGS.61-64 FILLER FIELDS
     *           FS, TRABUF(TIGSV_FNIF)                                         !TIGS.65    PORTUGUESE PLAYER VAT IDENTIFICATION CONFIRMATION NEEDED FLAG (OUT)
         ELSEIF(TRABUF(TIGS_TTYP) .EQ. IGSPAY) THEN
           I4TMP(2) = TRABUF(TIGSP_MIDH)
           I4TMP(1) = TRABUF(TIGSP_MIDL)
           TMSGID = I8TMP                                                       !TERMINAL MESSAGE ID (PAYMENT REQUEST)
C
           I4TMP(2) = TRABUF(TIGSP_WRSH)
           I4TMP(1) = TRABUF(TIGSP_WRSL)
           WEXSER = I8TMP                                                       !BET REFERENCE SERIAL NUMBER
C
           I4TMP(2) = TRABUF(TIGSP_PRSH)
           I4TMP(1) = TRABUF(TIGSP_PRSL)
           PEXSER = I8TMP                                                       !PAYMENT REFERENCE SERIAL NUMBER
C
           CALL FASTSET(BLANK,NIB,6)
           WRITE(CNIB,'(I4.4,1X,I4.4,I9.9,I2.2,1X,I2.2)')
     *           TRABUF(TIGSP_NIBB),
     *           TRABUF(TIGSP_NIBO),
     *           TRABUF(TIGSP_NIA1),
     *           TRABUF(TIGSP_NIA2),
     *           TRABUF(TIGSP_NICD)
C
           WRITE(QLIKDATA.QLIK_LUN,1300)
     *           FS, TRABUF(TIGSP_PRDY),                                        !TIGS.27    PAYMENT REFERENCE DATE YEAR (YY, LAST TWO DIGITS ONLY) (OUT)
     *               TRABUF(TIGSP_PRDM),                                        !TIGS.27    PAYMENT REFERENCE DATE MONTH (MM) (OUT)
     *               TRABUF(TIGSP_PRDD),                                        !TIGS.27    PAYMENT REFERENCE DATE DAY (DD)   (OUT)
     *               TRABUF(TIGSP_PRGM),                                        !TIGS.27    PAYMENT REFERENCE GAME            (OUT)
     *               PEXSER,                                                    !TIGS.27    PAYMENT REFERENCE SERIAL NUMBER   (OUT)
     *               TRABUF(TIGSP_PRCD),                                        !TIGS.27    PAYMENT REFERENCE CHECK DIGITS    (OUT)
     *           FS, TRABUF(TIGSP_WRDY),                                        !TIGS.28    BET REFERENCE DATE YEAR (YY, LAST TWO DIGITS ONLY) (IN)
     *               TRABUF(TIGSP_WRDM),                                        !TIGS.28    BET REFERENCE DATE MONTH (MM) (IN)
     *               TRABUF(TIGSP_WRDD),                                        !TIGS.28    BET REFERENCE DATE DAY (DD)   (IN)
     *               TRABUF(TIGSP_WRGM),                                        !TIGS.28    BET REFERENCE GAME            (IN)
     *               WEXSER,                                                    !TIGS.28    BET REFERENCE SERIAL NUMBER   (IN)
     *               TRABUF(TIGSP_WRCD),                                        !TIGS.28    BET REFERENCE CHECK DIGITS    (IN)
     *           FS, CMONY(TRABUF(TIGSP_NPRZ),10,VALUNIT),                      !TIGS.29    NET PRIZE AMOUNT/TRANSACTION AMOUNT (VALIDATION UNITS) (OUT)
     *           FS, TRABUF(TIGSP_PPDY),                                        !TIGS.30    PRIZE PAYMENT DATE YEAR (YYYY) (OUT)
     *               TRABUF(TIGSP_PPDM),                                        !TIGS.30    PRIZE PAYMENT DATE MONTH (MM)  (OUT)
     *               TRABUF(TIGSP_PPDD),                                        !TIGS.30    PRIZE PAYMENT DATE YEAR (DD)   (OUT)
     *               TRABUF(TIGSP_PPTH),                                        !TIGS.30    PRIZE PAYMENT TIME HOUR (HH)   (OUT)
     *               TRABUF(TIGSP_PPTM),                                        !TIGS.30    PRIZE PAYMENT TIME MINUTE (MI) (OUT)
     *               TRABUF(TIGSP_PPTS),                                        !TIGS.30    PRIZE PAYMENT TIME SECOND (SS) (OUT)
     *           FS, SYSTEMERRORCODE(TRABUF(TIGS_SERR)),                        !TIGS.31    SYSTEM CODE WHERE ERROR OCCURRED (OUT)
     *               CXERR,                                                     !TIGS.31    IGS ERROR CODE DESCRIPTION (OUT)
     *           FS, TRABUF(TIGSP_PNIF),                                        !TIGS.32    PORTUGUESE PLAYER VAT IDENTIFICATION NUMBER (9 DIGITS) (OUT)
     *           FS, TRABUF(TIGS_XREF),                                         !TIGS.33    MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
     *           FS, TMSGID,                                                    !TIGS.34    TERMINAL MESSAGE ID (IN)
     *          (FS, I=35,58),                                                  !TIGS.35-58 FILLER FIELDS
     *           FS, TRABUF(TIGSP_PMOD),                                        !TIGS.59    PAYMENT MODE (IN)
     *           FS, CMONY(TRABUF(TIGSP_TPRZ),10,VALUNIT),                      !TIGS.60    TOTAL PRIZE AMOUNT (VALIDATION UNITS) (OUT)
!     *           FS, CMONY(TRABUF(TIGSP_TTAX),10,VALUNIT),                      !TOTAL TAX AMOUNT (VALIDATION UNITS)   (OUT)
!     *           FS, CMONY(TRABUF(TIGSP_NPRZ),10,VALUNIT),                      !NET PRIZE AMOUNT (VALIDATION UNITS) (OUT)
     *          (FS, I=61,61),                                                  !TIGS.61-61 FILLER FIELDS
     *           FS, TRABUF(TIGSP_IDTY),                                        !TIGS.62    PLAYER ID TYPE (IN)
     *           FS, TRABUF(TIGSP_PYID),                                        !TIGS.63    PLAYER ID (IN)
     *           FS, (NIB(I),I=1,6)                                             !TIGS.64    PLAYER NIB (IN)
         ELSEIF(TRABUF(TIGS_TTYP) .EQ. IGSCAN) THEN
           I4TMP(2) = TRABUF(TIGSC_MIDH)
           I4TMP(1) = TRABUF(TIGSC_MIDL)
           TMSGID = I8TMP                                                       !TERMINAL MESSAGE ID (CANCEL REQUEST)
           I4TMP(2) = TRABUF(TIGSC_WRSH)
           I4TMP(1) = TRABUF(TIGSC_WRSL)
           WEXSER = I8TMP                                                       !BET REFERENCE SERIAL NUMBER
           I4TMP(2) = TRABUF(TIGSC_CRSH)
           I4TMP(1) = TRABUF(TIGSC_CRSL)
           CEXSER = I8TMP                                                       !CANCEL REFERENCE SERIAL NUMBER
           WRITE(QLIKDATA.QLIK_LUN,1400)
     *           FS, TRABUF(TIGSC_CRDY),                                        !TIGS.27    CANCEL REFERENCE DATE YEAR (YY, LAST TWO DIGITS ONLY) (OUT)
     *               TRABUF(TIGSC_CRDM),                                        !TIGS.27    CANCEL REFERENCE DATE MONTH (MM) (OUT)
     *               TRABUF(TIGSC_CRDD),                                        !TIGS.27    CANCEL REFERENCE DATE DAY (DD)   (OUT)
     *               TRABUF(TIGSC_CRGM),                                        !TIGS.27    CANCEL REFERENCE GAME            (OUT)
     *               CEXSER,                                                    !TIGS.27    CANCEL REFERENCE SERIAL NUMBER   (OUT)
     *               TRABUF(TIGSC_CRCD),                                        !TIGS.27    CANCEL REFERENCE CHECK DIGITS    (OUT)
     *           FS, TRABUF(TIGSC_WRDY),                                        !TIGS.28    BET REFERENCE DATE YEAR (YY, LAST TWO DIGITS ONLY) (IN)
     *               TRABUF(TIGSC_WRDM),                                        !TIGS.28    BET REFERENCE DATE MONTH (MM) (IN)
     *               TRABUF(TIGSC_WRDD),                                        !TIGS.28    BET REFERENCE DATE DAY (DD)   (IN)
     *               TRABUF(TIGSC_WRGM),                                        !TIGS.28    BET REFERENCE GAME            (IN)
     *               WEXSER,                                                    !TIGS.28    BET REFERENCE SERIAL NUMBER   (IN)
     *               TRABUF(TIGSC_WRCD),                                        !TIGS.28    BET REFERENCE CHECK DIGITS    (IN)
     *           FS, CMONY(TRABUF(TIGSC_CAMT),10,BETUNIT),                      !TIGS.29    CANCEL AMOUNT/TRANSACTION AMOUNT (WAGER UNITS) (OUT)
     *           FS, TRABUF(TIGSC_WCDY),                                        !TIGS.30    WAGER CANCEL DATE YEAR (YYYY) (OUT)
     *               TRABUF(TIGSC_WCDM),                                        !TIGS.30    WAGER CANCEL DATE MONTH (MM)  (OUT)
     *               TRABUF(TIGSC_WCDD),                                        !TIGS.30    WAGER CANCEL DATE YEAR (DD)   (OUT)
     *               TRABUF(TIGSC_WCTH),                                        !TIGS.30    WAGER CANCEL TIME HOUR (HH)   (OUT)
     *               TRABUF(TIGSC_WCTM),                                        !TIGS.30    WAGER CANCEL TIME MINUTE (MI) (OUT)
     *               TRABUF(TIGSC_WCTS),                                        !TIGS.30    WAGER CANCEL TIME SECOND (SS) (OUT)
     *           FS, SYSTEMERRORCODE(TRABUF(TIGS_SERR)),                        !TIGS.31    SYSTEM CODE WHERE ERROR OCCURRED (OUT)
     *               CXERR,                                                     !TIGS.31    IGS ERROR CODE DESCRIPTION (OUT)
     *          (FS, I=32,32),                                                  !TIGS.32-32 FILLER FIELDS
     *           FS, TRABUF(TIGS_XREF),                                         !TIGS.33    MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
     *           FS, TMSGID                                                     !TIGS.34    TERMINAL MESSAGE ID (IN)
         ELSEIF(TRABUF(TIGS_TTYP) .EQ. IGSREP) THEN
           I4TMP(2) = TRABUF(TIGSR_MIDH)
           I4TMP(1) = TRABUF(TIGSR_MIDL)
           TMSGID = I8TMP                                                       !TERMINAL MESSAGE ID (REPORT REQUEST)
C
           I4TMP(2) = TRABUF(TIGSR_MVRH)
           I4TMP(1) = TRABUF(TIGSR_MVRL)
           MEDVER = I8TMP                                                       !MEDIA VERSION
           WRITE(QLIKDATA.QLIK_LUN,1500)
     *          (FS, I=27,29),                                                  !TIGS.27-29 FILLER FIELDS
     *           FS, TRABUF(TIGSR_PRDY),                                        !TIGS.30    PROGRAMME REPORT DATE YEAR (YYYY) (OUT)
     *               TRABUF(TIGSR_PRDM),                                        !TIGS.30    PROGRAMME REPORT DATE MONTH (MM)  (OUT)
     *               TRABUF(TIGSR_PRDD),                                        !TIGS.30    PROGRAMME REPORT DATE YEAR (DD)   (OUT)
     *               TRABUF(TIGSR_PRTH),                                        !TIGS.30    PROGRAMME REPORT TIME HOUR (HH)   (OUT)
     *               TRABUF(TIGSR_PRTM),                                        !TIGS.30    PROGRAMME REPORT TIME MINUTE (MI) (OUT)
     *               TRABUF(TIGSR_PRTS),                                        !TIGS.30    PROGRAMME REPORT TIME SECOND (SS) (OUT)
     *           FS, SYSTEMERRORCODE(TRABUF(TIGS_SERR)),                        !TIGS.31    SYSTEM CODE WHERE ERROR OCCURRED
     *               CXERR,                                                     !TIGS.31    IGS ERROR CODE DESCRIPTION
     *          (FS, I=32,32),                                                  !TIGS.32-32 FILLER FIELDS
     *           FS, TRABUF(TIGS_XREF),                                         !TIGS.33    MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
     *           FS, TMSGID,                                                    !TIGS.34    TERMINAL MESSAGE ID (IN)
     *          (FS, I=35,69),                                                  !TIGS.35-69 FILLER FIELDS
     *           FS, TRABUF(TIGSR_SEGN),                                        !TIGS.70    SEGMENT NUMBER REQUESTED (IN & OUT)
     *           FS, TRABUF(TIGSR_TSEG),                                        !TIGS.71    TOTAL SEGMENTS (IN & OUT)
     *           FS, TRABUF(TIGSR_MEID),                                        !TIGS.72    MEDIA ID (IN)
     *           FS, TRABUF(TIGSR_PTID),                                        !TIGS.72    PROGRAMME TEMPLATE ID (IN)
     *           FS, MEDVER                                                     !TIGS.74    MEDIA VERSION (IN/OUT)

         ELSE ! THIS MUST NOT HAPPEN
           WRITE(QLIKDATA.QLIK_LUN,1600) FS
         ENDIF
       ENDIF
C IGS TRANSACTION HEADER
1000   FORMAT(           I0,                                                    !TIGS.1     STATUS
     *        A<FS_LEN>, I0,                                                    !TIGS.2     ERROR CODE
     *        A<FS_LEN>, A8,                                                    !TIGS.3     TRANSACTION DATE
     *        A<FS_LEN>, I0,                                                    !TIGS.4     INTERNAL SERIAL NUMBER
     *        A<FS_LEN>, A8,                                                    !TIGS.5     TIME STAMP HH:MI:SS
     *        A<FS_LEN>, I0,                                                    !TIGS.6     TERMINAL NUMBER
     *        A<FS_LEN>, A8,                                                    !TIGS.7     AGENT NUMBER
     *        A<FS_LEN>, I0,                                                    !TIGS.8     SAP NUMBER
     *        A<FS_LEN>, I0,                                                    !TIGS.9     TRANSACTION SEQUENCE NUMBER
     *        A<FS_LEN>, I0,                                                    !TIGS.10    TRANSACTION TYPE
     *        A<FS_LEN>, I0,                                                    !TIGS.11    GAME NUMBER
     *        A<FS_LEN>, I0,                                                    !TIGS.12    GAME TYPE
     *        A<FS_LEN>, I0,                                                    !TIGS.13    GAME INDEX
     *        A<FS_LEN>, I0,                                                    !TIGS.14    TERMINAL STATISTICS
     *        A<FS_LEN>, I0,                                                    !TIGS.15    INTERNAL TRANSACTION FLAG
     *        A<FS_LEN>, I0,                                                    !TIGS.16    FILE STATUS
     *        A<FS_LEN>, I0,                                                    !TIGS.17    TICKET ID
     *        A<FS_LEN>, I0,                                                    !TIGS.18    MESSAGE CHECKSUM
     *        A<FS_LEN>, I0,                                                    !TIGS.19    # OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TIGS.20    TRANSACTION SIZE (# LOG RECS)
     *        A<FS_LEN>, I0,                                                    !TIGS.21    ERROR SUB CODE
     *        A<FS_LEN>, I0,                                                    !TIGS.22    CDC TRANS. WAS SOLD (NEVER CHANGES)
     *        A<FS_LEN>, I0,                                                    !TIGS.23    BET AMOUNT FLAG (FOR FRACTIONS)
     *        A<FS_LEN>, I0,                                                    !TIGS.24    REAL NUMBER OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TIGS.25    PROMOTION - ADD 1 FREE WEEK
     *        A<FS_LEN>, I0)                                                    !TIGS.26    IGS TRANSACTION TYPE
C IGS WAGER DETAIL
1100   FORMAT(A<FS_LEN>, I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',I3.3,           !TIGS.27    TRANSACTION BET EXTERNAL REFERENCE SERIAL NUMBER (YYMMDD-GG-SSSSSSSSSS-XXX) (OUT)
     *        A<FS_LEN>, I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',I3.3,           !TIGS.28    BET EXTERNAL REFERENCE SERIAL NUMBER (YYMMDD-GG-SSSSSSSSSS-XXX) (OUT)
     *        A<FS_LEN>, A,                                                     !TIGS.29    BET TOTAL STAKE/TRANSACTION AMOUNT (OUT)
     *        A<FS_LEN>, I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2,     !TIGS.30    BET CREATION DATETIME (YYYY/MM/DD HH:MI:SS) (OUT)
     *        A<FS_LEN>, A, A<CXERR_LEN>,                                       !TIGS.31    SYSTEM CODE WHERE ERROR OCCURRED/IGS ERROR CODE DESCRIPTION
     *        A<FS_LEN>, I0,                                                    !TIGS.32    PORTUGUESE PLAYER VAT IDENTIFICATION NUMBER (IN)
     *        A<FS_LEN>, I0,                                                    !TIGS.33    MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
     *        A<FS_LEN>, I0,                                                    !TIGS.34    TERMINAL MESSAGE ID (IN)
     *        A<FS_LEN>,                                                        !TIGS.35-35 FILLER FIELDS
     *        A<FS_LEN>, A,                                                     !TIGS.36    UNIT STAKE OF THE BET (IN/OUT)
     *       2A<FS_LEN>,                                                        !TIGS.37-38 FILLER FIELDS
     *        A<FS_LEN>, I0,                                                    !TIGS.39    TOTAL BETS/NUMBER OF SELECTIONS (MAX = 8) (IN/OUT)
     *        A<FS_LEN>, A,                                                     !TIGS.40    BET MAXIMUM POSSIBLE RETURNS  (OUT)
     *       4A<FS_LEN>,                                                        !TIGS.41-44 FILLER FIELDS
     *        A<FS_LEN>, I0)                                                    !TIGS.45    PLACARD BET TYPE
!     *        A<FS_LEN>, I0,                                                    ! ABP GAME ID (IN/OUT)
!     *        A<FS_LEN>, I0,                                                    ! SUBTYPE ID (IN/OUT)
!     *        A<FS_LEN>, I4.4,'/',I2.2,'/',I2.2,                                ! BET LAST EVENT DATE (YYYY/MM/DD) (OUT)

C IGS VALIDATION DETAIL
1200   FORMAT(  A<FS_LEN>,                                                      !TIGS.27-27 FILLER FIELDS
     *          A<FS_LEN>, I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',I3.3,         !TIGS.28    BET EXTERNAL REFERENCE SERIAL NUMBER (YYMMDD-GG-SSSSSSSSSS-XXX) (OUT)
     *          A<FS_LEN>,                                                      !TIGS.29-29 FILLER FIELDS
     *          A<FS_LEN>, I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2,   !TIGS.30    BET VALIDATION DATETIME (YYYY/MM/DD HH:MI:SS) (OUT)
     *          A<FS_LEN>, A, A<CXERR_LEN>,                                     !TIGS.31    SYSTEM CODE WHERE ERROR OCCURRED/IGS ERROR CODE DESCRIPTION (OUT)
     *          A<FS_LEN>, I0,                                                  !TIGS.32    PORTUGUESE PLAYER VAT IDENTIFICATION NUMBER (9 DIGITS) (OUT)
     *          A<FS_LEN>, I0,                                                  !TIGS.33    MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
     *          A<FS_LEN>, I0,                                                  !TIGS.34    TERMINAL MESSAGE ID (IN)
     *        24A<FS_LEN>,                                                      !TIGS.35-58 FILLER FIELDS
     *          A<FS_LEN>, I0,                                                  !TIGS.59    PAYMENT MODE (OUT)
     *          A<FS_LEN>, A,                                                   !TIGS.60    TOTAL PRIZE AMOUNT (VALIDATION UNITS) (OUT)
!     *          A<FS_LEN>, A,                                                   ! TOTAL TAX AMOUNT (VALIDATION UNITS)   (OUT)
!     *          A<FS_LEN>, A,                                                   ! NET PRIZE AMOUNT (VALIDATION UNITS)   (OUT)
     *         4A<FS_LEN>,                                                      !TIGS.61-64 FILLER FIELDS
     *          A<FS_LEN>, I0)                                                  !TIGS.65    PORTUGUESE PLAYER VAT IDENTIFICATION CONFIRMATION NEEDED FLAG (OUT)
C IGS PAYMENT DETAIL
1300   FORMAT(  A<FS_LEN>, I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',I3.3,         !TIGS.27    PAYMENT EXTERNAL REFERENCE SERIAL NUMBER (YYMMDD-GG-SSSSSSSSSS-XXX) (OUT)
     *          A<FS_LEN>, I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',I3.3,         !TIGS.28    BET EXTERNAL REFERENCE SERIAL NUMBER (YYMMDD-GG-SSSSSSSSSS-XXX) (OUT)
     *          A<FS_LEN>, A,                                                   !TIGS.29    NET PRIZE AMOUNT/TRANSACTION AMOUNT (VALIDATION UNITS)   (OUT)
     *          A<FS_LEN>, I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2,   !TIGS.30    PRIZE PAYMENT DATETIME (YYYY/MM/DD HH:MI:SS) (OUT)
     *          A<FS_LEN>, A, A<CXERR_LEN>,                                     !TIGS.31    SYSTEM CODE WHERE ERROR OCCURRED/IGS ERROR CODE DESCRIPTION (OUT)
     *          A<FS_LEN>, I0,                                                  !TIGS.32    PORTUGUESE PLAYER VAT IDENTIFICATION NUMBER (9 DIGITS) (OUT)
     *          A<FS_LEN>, I0,                                                  !TIGS.33    TERMINAL MESSAGE ID (IN)
     *          A<FS_LEN>, I0,                                                  !TIGS.34    MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
     *        24A<FS_LEN>,                                                      !TIGS.35-58 FILLER FIELDS
     *          A<FS_LEN>, I0,                                                  !TIGS.59    PAYMENT MODE (IN)
     *          A<FS_LEN>, A,                                                   !TIGS.60    TOTAL PRIZE AMOUNT (VALIDATION UNITS) (OUT)
!     *          A<FS_LEN>, A,                                                   ! TOTAL TAX AMOUNT (VALIDATION UNITS)   (OUT)
!     *          A<FS_LEN>, A,                                                   ! NET PRIZE AMOUNT (VALIDATION UNITS)   (OUT)
     *          A<FS_LEN>,                                                      !TIGS.61-61 FILLER FIELDS
     *          A<FS_LEN>, I0,                                                  !TIGS.62    PLAYER ID TYPE (IN)
     *          A<FS_LEN>, I0,                                                  !TIGS.63    PLAYER ID (IN)
     *          A<FS_LEN>, 6A4)                                                 !TIGS.64    PLAYER NIB (IN)
C IGS CANCEL DETAIL
1400   FORMAT(  A<FS_LEN>, I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',I3.3,         !TIGS.27    CANCEL EXTERNAL REFERENCE SERIAL NUMBER (YYMMDD-GG-SSSSSSSSSS-XXX) (OUT)
     *          A<FS_LEN>, I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',I3.3,         !TIGS.28    BET EXTERNAL REFERENCE SERIAL NUMBER (YYMMDD-GG-SSSSSSSSSS-XXX) (IN)
     *          A<FS_LEN>, A,                                                   !TIGS.29    CANCEL AMOUNT (OUT)
     *          A<FS_LEN>, I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2,   !TIGS.30    WAGER CANCEL DATETIME (YYYY/MM/DD HH:MI:SS) (OUT)
     *          A<FS_LEN>, A, A<CXERR_LEN>,                                     !TIGS.31    SYSTEM CODE WHERE ERROR OCCURRED/IGS ERROR CODE DESCRIPTION
     *          A<FS_LEN>,                                                      !TIGS.32-32 FILLER FIELD
     *          A<FS_LEN>, I0,                                                  !TIGS.33    MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
     *          A<FS_LEN>, I0)                                                  !TIGS.34    TERMINAL MESSAGE ID (IN)
C IGS REPORT DETAIL
1500   FORMAT( 3A<FS_LEN>,                                                      !TIGS.27-29 FILLER FIELDS
     *          A<FS_LEN>, I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2,   !TIGS.30    PROGRAMME REPORT DATETIME (YYYY/MM/DD HH:MI:SS) (OUT)
     *          A<FS_LEN>, A, A<CXERR_LEN>,                                     !TIGS.31    SYSTEM CODE WHERE ERROR OCCURRED/IGS ERROR CODE DESCRIPTION
     *          A<FS_LEN>,                                                      !TIGS.32-32 FILLER FIELDS
     *          A<FS_LEN>, I0,                                                  !TIGS.33    MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
     *          A<FS_LEN>, I0,                                                  !TIGS.34    TERMINAL MESSAGE ID (IN)
     *        35A<FS_LEN>,                                                      !TIGS.35-69 FILLER FIELDS
     *          A<FS_LEN>, I0,                                                  !TIGS.70    SEGMENT NUMBER REQUESTED (IN & OUT)
     *          A<FS_LEN>, I0,                                                  !TIGS.71    TOTAL SEGMENTS (IN & OUT)
     *          A<FS_LEN>, I0,                                                  !TIGS.72    MEDIA ID (IN)
     *          A<FS_LEN>, I0,                                                  !TIGS.73    PROGRAMME TEMPLATE ID (IN)
     *          A<FS_LEN>, I0)                                                  !TIGS.74    MEDIA VERSION (IN/OUT)

C IGS OTHER TRANSACTION DETAIL
1600   FORMAT(A<FS_LEN>)
       RETURN
       END
CCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC       SUBROUTINE PRINT_TCAN (QLIKDATA,TRABUF)
CCC
CCC       THIS SUBROUTINE WRITES CANCELLATION TRANSACTIONS INTO
CCC       QLIK FILE.
CCC
CCC       INPUTS:
CCC        QLIKDATA       QLIK DATA STRUCTURE
CCC        TRABUF         CAN TRANSACTION TO WRITE INTO THE FILE
CCC
CCC       OUTPUTS:
CCC        *NONE*
CCC
CCC=======================================================================
CCC====== OPTIONS /CHECK=NOOVERFLOW
CC       SUBROUTINE PRINT_TCAN(QLIKDATA,TRABUF)
CC       IMPLICIT NONE
CCC
CC       INCLUDE 'INCLIB:SYSEXTRN.DEF'
CC       INCLUDE 'INCLIB:GLOBAL.DEF'
CC       INCLUDE 'INCLIB:DESTRA.DEF'
CC       INCLUDE 'INCLIB:DATBUF.DEF'
CC       INCLUDE 'INCLIB:QLIKTRAN.DEF'
CC       INCLUDE 'INCLIB:AGTCOM.DEF'
CCC
CC       INTEGER*4 I
CC       INTEGER*4 JUL
CC       INTEGER*2 DBUF(12)
CC       CHARACTER*11 C11MONY
CC       CHARACTER*8 IAGT_NO                                                      !EXTERNAL FUNCTION
CC       CHARACTER*8 GET_YYYYMMDD_CDC                                             !EXTERNAL FUNCTION
CC       CHARACTER*8 CDRWNAM                                                      !LOCAL FUNCTION
CC       INTEGER*4 EPTCKT,TSTYP
CCC
CC       INTEGER*4 WXSER, WXCHK, CXSER, CXCHK
CC       CHARACTER*16 C16XSER
CC       CHARACTER*8 DRWNAM, DRWKNAM
CCC
CC       DRWNAM  = '        '
CC       DRWKNAM = '        '
CC       C16XSER  = '                '
CC       WXSER   = 0
CC       WXCHK   = 0
CC       CXSER   = 0
CC       CXCHK   = 0
CCC
CC       DBUF(VCDC)=TRABUF(TCDC)
CC       CALL CDATE(DBUF)
CC       JUL=DBUF(VJUL)
CCC
CCC CANCEL TRANSACTION HEADER
CC       WRITE(QLIKDATA.QLIK_LUN,1000,ADVANCE='NO')
CC     *           TRABUF(TSTAT),                                                 !TCAN.1     STATUS
CC     *       FS, TRABUF(TERR),                                                  !TCAN.2     ERROR CODE
CC     *       FS, GET_YYYYMMDD_CDC(TRABUF(TCDC)),                                !TCAN.3     TRANSACTION DATE
CC     *       FS, TRABUF(TSER),                                                  !TCAN.4     INTERNAL SERIAL NUMBER
CC     *       FS, DISTIM(TRABUF(TTIM)),                                          !TCAN.5     TIME STAMP HH:MI:SS
CC     *       FS, TRABUF(TTER),                                                  !TCAN.6     TERMINAL NUMBER
CC     *       FS, IAGT_NO(TRABUF(TAGT)),                                         !TCAN.7     AGENT NUMBER
CC     *       FS, AGTSAP(TRABUF(TTER)),                                          !TCAN.8     SAP NUMBER
CC     *       FS, TRABUF(TTRN),                                                  !TCAN.9     TRANSACTION SEQUENCE NUMBER
CC     *       FS, TRABUF(TTYP),                                                  !TCAN.10    TRANSACTION TYPE
CC     *       FS, TRABUF(TGAM),                                                  !TCAN.11    GAME NUMBER
CC     *       FS, TRABUF(TGAMTYP),                                               !TCAN.12    GAME TYPE
CC     *       FS, TRABUF(TGAMIND),                                               !TCAN.13    GAME INDEX
CC     *       FS, TRABUF(TTSTCS),                                                !TCAN.14    TERMINAL STATISTICS
CC     *       FS, TRABUF(TINTRA),                                                !TCAN.15    INTERNAL TRANSACTION FLAG
CC     *       FS, TRABUF(TFIL),                                                  !TCAN.16    FILE STATUS
CC     *       FS, TRABUF(TTKID),                                                 !TCAN.17    TICKET ID
CC     *       FS, TRABUF(TCHK),                                                  !TCAN.18    MESSAGE CHECKSUM
CC     *       FS, TRABUF(TFRAC),                                                 !TCAN.19    # OF FRACTIONS
CC     *       FS, TRABUF(TSIZE),                                                 !TCAN.20    TRANSACTION SIZE (# LOG RECS)
CC     *       FS, TRABUF(TSUBERR),                                               !TCAN.21    ERROR SUB CODE
CC     *       FS, TRABUF(TCDC_SOLD),                                             !TCAN.22    CDC TRANS. WAS SOLD (NEVER CHANGES)
CC     *       FS, TRABUF(TFAMTFLG),                                              !TCAN.23    BET AMOUNT FLAG (FOR FRACTIONS)
CC     *       FS, TRABUF(TNFRAC),                                                !TCAN.24    REAL NUMBER OF FRACTIONS
CC     *       FS, TRABUF(TWADDFW)                                                !TCAN.25    PROMOTION - ADD 1 FREE WEEK
CC       IF(TRABUF(TGAMTYP) .NE. TPAS) THEN
CC         WRITE(QLIKDATA.QLIK_LUN,1001,ADVANCE=WRITE__ADVANCE_VALUE)
CC     *      (FS, I=26,26)                                                       !TWAG.26-26 FILLER FIELD (TRANSACTION SUBTYPE N/A)
CC       ELSE
CC         WRITE(QLIKDATA.QLIK_LUN,1002,ADVANCE=WRITE__ADVANCE_VALUE)
CC     *       FS, TRABUF(TWEPOP)                                                 !TWAG.26    EPASSIVE OPERATION REQUESTED
CC       ENDIF
CCC
CC       IF(TDETAIL) THEN
CCC      CANCEL DETAIL
CC         CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),WXSER,WXCHK)
CC         DRWNAM = CDRWNAM(TRABUF)
CC         IF(TRABUF(TWKFLG)) THEN
CC           DRWKNAM = CDRWNAM(TRABUF)
CC         ENDIF
CC         WRITE(QLIKDATA.QLIK_LUN,1010,ADVANCE='NO')
CC     *         FS, JUL,                                                         !TWAG.27    TRANSACTION EXTERNAL JULIAN DATE
CC     *             WXSER,                                                       !TWAG.27....TRANSACTION EXTERNAL WAGER SERIAL NUMBER
CC     *             WXCHK,                                                       !TWAG.27    TRANSACTION EXTERNAL WAGER CHECK DIGITS
CC     *         FS, JUL,                                                         !TWAG.28    EXTERNAL WAGER JULIAN DATE
CC     *             WXSER,                                                       !TWAG.28    EXTERNAL WAGER SERIAL NUMBER
CC     *             WXCHK,                                                       !TWAG.28    EXTERNAL WAGER CHECK DIGITS
CC     *         FS, CMONY(TRABUF(TWTOT),10,BETUNIT),                             !TWAG.29    TOTAL AMOUNT
CC     *         FS, DISTIM(TRABUF(TTIM)),                                        !TWAG.30    TIME STAMP HH:MI:SS
CC     *        (FS, I=31,34),                                                    !TWAG.31-34 FILLER FIELDS
CC     *         FS, DRWNAM,                                                      !TWAG.35    (STARTING) DRAW NAME
CC     *         FS, CMONY(TRABUF(TWAMT),8,BETUNIT),                              !TWAG.36    BASE AMOUNT (MAIN GAME)
CC     *         FS, DRWKNAM,                                                     !TWAG.37    KICKER (START) DRAW NAME
CC     *         FS, CMONY(TRABUF(TWKAMT),8,BETUNIT),                             !TWAG.38    KICKER AMOUNT
CC     *         FS, TRABUF(TWNBET),                                              !TWAG.39    NUMBER OF BETS/BOARDS
CC     *        (FS, I=40,40),                                                    !TWAG.40-40 FILLER FIELDS
CC     *         FS, TRABUF(TWQPF)                                                !TWAG.41    QUICK PICK FLAGS
CC         CALL OUTGEN(TRABUF(TCDC),TRABUF(TWCSER),CXSER,CXCHK)
CC         IF(TRABUF(TWCSER) .NE. 0) THEN
CC           WRITE (C16XSER,'(I3.3,A1,I8.8,A1,I3.3)') JUL,'-',CXSER,'-',CXCHK
CC         ENDIF
CC         IF(TRABUF(TGAMTYP) .EQ. TSPT) THEN
CC           WRITE(QLIKDATA.QLIK_LUN,1020,ADVANCE='NO')
CC     *          (FS, I=42,44),                                                  !TWAG.42-44 FILLER FIELDS
CC     *           FS, TRABUF(TWSYST),                                            !TWAG.45    SYSTEM TYPE
CC     *          (FS, I=46,46),                                                  !TWAG.46-46 FILLER FIELDS
CC     *           FS, TRABUF(TWKFLG),                                            !TWAG.47    KICKER SOLD FLAG
CC     *           FS, TRABUF(TWKICK),                                            !TWAG.48    KICKER NUMBER
CC     *           FS, TRIM(C16XSER),                                              !TWAG.49    EXTERNAL CANCEL SERIAL #
CC     *           FS, TRABUF(TWCTER),                                            !TWAG.50    CANCEL TERMINAL #
CC     *           FS, TRABUF(TWSIMP)                                             !TWAG.51    # OF SIMPLE BETS
CC         ELSEIF(TRABUF(TGAMTYP) .EQ. TKIK) THEN
CC           WRITE(QLIKDATA.QLIK_LUN,1030,ADVANCE='NO')
CC     *          (FS, I=42,44),                                                  !TWAG.42-44 FILLER FIELDS
CC     *           FS, TRABUF(TWSYST),                                            !TWAG.45    SYSTEM TYPE
CC     *           FS, JUL,                                                       !TWAG.46    EXTERNAL WAGER JULIAN DATE
CC     *               TRABUF(TWLNKSER),                                          !TWAG.46    EXTERNAL SERIAL NUMBER OF ASSOCIATED LINKED WAGER
CC     *               TRABUF(TWLNKCHK),                                          !TWAG.46    EXTERNAL CHECK DIGITS OF ASSOCIATED LINKED WAGER
CC     *           FS, TRABUF(TWKFLG),                                            !TWAG.47    KICKER SOLD FLAG
CC     *           FS, TRABUF(TWKICK),                                            !TWAG.48    KICKER NUMBER
CC     *           FS, TRIM(C16XSER),                                              !TWAG.49    EXTERNAL CANCEL SERIAL #
CC     *           FS, TRABUF(TWCTER),                                            !TWAG.50    CANCEL TERMINAL #
CC     *           FS, TRABUF(TWSIMP)                                             !TWAG.51    # OF SIMPLE BETS
CC         ELSEIF(TRABUF(TGAMTYP) .EQ. TLTO) THEN
CC           WRITE(QLIKDATA.QLIK_LUN,1040,ADVANCE='NO')
CC     *           FS, TRABUF(TWNMRK),                                            !TWAG.42    # OF LOTTO MARKS/BOARD
CC     *          (FS, I=43,44),                                                  !TWAG.43-44 FILLER FIELDS
CC     *           FS, TRABUF(TWSYST),                                            !TWAG.45    SYSTEM TYPE
CC     *          (FS, I=46,46),                                                  !TWAG.46-46 FILLER FIELDS
CC     *           FS, TRABUF(TWKFLG),                                            !TWAG.47    KICKER SOLD FLAG
CC     *           FS, TRABUF(TWKICK),                                            !TWAG.48    KICKER NUMBER
CC     *           FS, TRIM(C16XSER),                                               !TWAG.49    EXTERNAL CANCEL SERIAL #
CC     *           FS, TRABUF(TWCTER),                                            !TWAG.50    CANCEL TERMINAL #
CC     *           FS, TRABUF(TWSIMP),                                            !TWAG.51    # OF SIMPLE BETS
CC     *           FS, TRABUF(TWLUCK)                                             !TWAG.52    LUCKY #
CC         ELSEIF(TRABUF(TGAMTYP) .EQ. TPAS) THEN
CC           EPTCKT = 0
CC           IF(TRABUF(TWEPOP) .EQ. EPASSAL) THEN
CC             EPTCKT = TRABUF(TWEPSN)
CC             WRITE(QLIKDATA.QLIK_LUN,1050,ADVANCE='NO')
CC     *            (FS, I=42,48),                                                !TWAG.42-48 FILLER FIELDS
CC     *             FS, TRIM(C16XSER),                                            !TWAG.49    EXTERNAL CANCEL SERIAL #
CC     *             FS, TRABUF(TWCTER),                                          !TWAG.50    CANCEL TERMINAL #
CC     *            (FS, I=51,52),                                                !TWAG.51-52 FILLER FIELDS
CC     *             FS, TRABUF(TWEPWK),                                          !TWAG.53    WEEK
CC     *                 2000+TRABUF(TWEPYR),                                     !TWAG.53    YEAR
CC     *             FS, EPTCKT ,                                                 !TWAG.54    SALE FOR NUMBER
CC     *             FS, TRABUF(TWEPSS),                                          !TWAG.55    SALE FOR SERIE
CC     *             FS, TRABUF(TWEPSF),                                          !TWAG.56    SALE FOR FRACTION
CC     *             FS, TRABUF(TWEPRM),                                          !TWAG.57    RESERVE MASK
CC     *             FS, TRABUF(TWEPNE),                                          !TWAG.58    NUMBER OF GIVEN ENDING NUMBERS
CC     *             FS, TRABUF(TWEPNF),                                          !TWAG.59    NUMBER OF REQUESTED FRACTIONS
CC     *             FS, TRABUF(TWEPNR)                                           !TWAG.60    NUMBER OF RESERVATIONS/RELEASES
CC           ELSEIF(TRABUF(TWEPOP).EQ. EPASREL .OR.
CC     *       TRABUF(TWEPOP).EQ. EPASRES) THEN
CC             EPTCKT = TRABUF(TWEPRES1)
CC             WRITE(QLIKDATA.QLIK_LUN,1050,ADVANCE='NO')
CC     *            (FS, I=42,48),                                                !TWAG.42-48 FILLER FIELDS
CC     *             FS, TRIM(C16XSER),                                            !TWAG.49    EXTERNAL CANCEL SERIAL #
CC     *             FS, TRABUF(TWCTER),                                          !TWAG.50    CANCEL TERMINAL #
CC     *            (FS, I=51,52),                                                !TWAG.51-52 FILLER FIELDS
CC     *             FS, TRABUF(TWEPWK),                                          !TWAG.53    WEEK
CC     *                 2000+TRABUF(TWEPYR),                                     !TWAG.53    YEAR
CC     *             FS, EPTCKT ,                                                 !TWAG.54    SALE FOR NUMBER
CC     *             FS, TRABUF(TWEPSS),                                          !TWAG.55    SALE FOR SERIE
CC     *             FS, TRABUF(TWEPSF),                                          !TWAG.56    SALE FOR FRACTION
CC     *             FS, TRABUF(TWEPRM),                                          !TWAG.57    RESERVE MASK
CC     *             FS, TRABUF(TWEPNE),                                          !TWAG.58    NUMBER OF GIVEN ENDING NUMBERS
CC     *             FS, TRABUF(TWEPNF),                                          !TWAG.59    NUMBER OF REQUESTED FRACTIONS
CC     *             FS, TRABUF(TWEPNR)                                           !TWAG.60    NUMBER OF RESERVATIONS/RELEASES
CC           ENDIF
CC         ENDIF
CC         WRITE(QLIKDATA.QLIK_LUN,*)                                             ! PRINT NEW LINE
CC       ENDIF
CCC
CC1000   FORMAT(           I0,                                                    !TWAG.1     STATUS
CC     *        A<FS_LEN>, I0,                                                    !TWAG.2     ERROR CODE
CC     *        A<FS_LEN>, A8,                                                    !TWAG.3     TRANSACTION DATE
CC     *        A<FS_LEN>, I0,                                                    !TWAG.4     INTERNAL SERIAL NUMBER
CC     *        A<FS_LEN>, A8,                                                    !TWAG.5     TIME STAMP HH:MI:SS
CC     *        A<FS_LEN>, I0,                                                    !TWAG.6     TERMINAL NUMBER
CC     *        A<FS_LEN>, A8,                                                    !TWAG.7     AGENT NUMBER
CC     *        A<FS_LEN>, I0,                                                    !TWAG.8     SAP NUMBER
CC     *        A<FS_LEN>, I0,                                                    !TWAG.9     TRANSACTION SEQUENCE NUMBER
CC     *        A<FS_LEN>, I0,                                                    !TWAG.10    TRANSACTION TYPE
CC     *        A<FS_LEN>, I0,                                                    !TWAG.11    GAME NUMBER
CC     *        A<FS_LEN>, I0,                                                    !TWAG.12    GAME TYPE
CC     *        A<FS_LEN>, I0,                                                    !TWAG.13    GAME INDEX
CC     *        A<FS_LEN>, I0,                                                    !TWAG.14    TERMINAL STATISTICS
CC     *        A<FS_LEN>, I0,                                                    !TWAG.15    INTERNAL TRANSACTION FLAG
CC     *        A<FS_LEN>, I0,                                                    !TWAG.16    FILE STATUS
CC     *        A<FS_LEN>, I0,                                                    !TWAG.17    TICKET ID
CC     *        A<FS_LEN>, I0,                                                    !TWAG.18    MESSAGE CHECKSUM
CC     *        A<FS_LEN>, I0,                                                    !TWAG.19    # OF FRACTIONS
CC     *        A<FS_LEN>, I0,                                                    !TWAG.20    TRANSACTION SIZE (# LOG RECS)
CC     *        A<FS_LEN>, I0,                                                    !TWAG.21    ERROR SUB CODE
CC     *        A<FS_LEN>, I0,                                                    !TWAG.22    CDC TRANS. WAS SOLD (NEVER CHANGES)
CC     *        A<FS_LEN>, I0,                                                    !TWAG.23    BET AMOUNT FLAG (FOR FRACTIONS)
CC     *        A<FS_LEN>, I0,                                                    !TWAG.24    REAL NUMBER OF FRACTIONS
CC     *        A<FS_LEN>, I0)                                                    !TWAG.25    PROMOTION - ADD 1 FREE WEEK
CC1001   FORMAT(A<FS_LEN>)                                                        !TWAG.26    FILLER FIELD (TRANSACTION SUBTYPE N/A))
CC1002   FORMAT(A<FS_LEN>, I0)                                                    !TWAG.26    EPASSIVE OPERATION REQUESTED
CCC WAGER DETAIL
CC1010   FORMAT(A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TWAG.27    TRANSACTION EXTERNAL SERIAL NUMBER
CC     *        A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TWAG.28    WAGER EXTERNAL SERIAL NUMBER
CC     *        A<FS_LEN>, A10,                                                   !TWAG.29    TOTAL AMOUNT
CC     *        A<FS_LEN>, A8,                                                    !TWAG.30    TIME STAMP HH:MI:SS
CC     *       4A<FS_LEN>,                                                        !TWAG.31-34 FILLER FIELDS
CC     *        A<FS_LEN>, A,                                                     !TWAG.35    DRAW NAME (MAIN GAME)
CC     *        A<FS_LEN>, A8,                                                    !TWAG.36    BASE AMOUNT (MAIN GAME)
CC     *        A<FS_LEN>, A,                                                     !TWAG.37    KICKER DRAW NAME
CC     *        A<FS_LEN>, A8,                                                    !TWAG.38    KICKER AMOUNT
CC     *        A<FS_LEN>, I0,                                                    !TWAG.39    NUMBER OF BETS/BOARDS
CC     *        A<FS_LEN>,                                                        !TWAG.40-40 FILLER FIELDS
CC     *        A<FS_LEN>, I0)                                                    !TWAG.41    QUICK PICK FLAGS
CCC SPORTS WAGER BODY
CC1020   FORMAT(3A<FS_LEN>,                                                       !TWAG.42-44 FILLER FIELDSA
CC     *        A<FS_LEN>, I0,                                                    !TWAG.45    SYSTEM TYPE
CC     *        A<FS_LEN>,                                                        !TWAG.46-46 FILLER FIELDS
CC     *        A<FS_LEN>, I0,                                                    !TWAG.47    KICKER SOLD FLAG
CC     *        A<FS_LEN>, I7.7,                                                  !TWAG.48    KICKER NUMBER
CC     *        A<FS_LEN>, A,                                                     !TWAG.49    EXTERNAL CANCEL SERIAL NUMBER
CC!     *        A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TWAG.49    EXTERNAL CANCEL SERIAL NUMBER
CC     *        A<FS_LEN>, I0,                                                    !TWAG.50    CANCEL TERMINAL #
CC     *        A<FS_LEN>, I0)                                                    !TWAG.51    # OF SIMPLE BETS
CCC KICKER WAGER BODY
CC1030   FORMAT(3A<FS_LEN>,                                                       !TWAG.42-44 FILLER FIELDSA
CC     *        A<FS_LEN>, I0,                                                    !TWAG.45    SYSTEM TYPE
CC     *        A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TWAG.46    EXTERNAL SERIAL EXTERNAL SERIAL NUMBER OF ASSOCIATED LINKED WAGER
CC     *        A<FS_LEN>, I0,                                                    !TWAG.47    KICKER SOLD FLAG
CC     *        A<FS_LEN>, I7.7,                                                  !TWAG.48    KICKER NUMBER
CC     *        A<FS_LEN>, A,                                                     !TWAG.49    EXTERNAL CANCEL SERIAL NUMBER
CC!     *        A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TWAG.49    EXTERNAL CANCEL SERIAL NUMBER
CC     *        A<FS_LEN>, I0,                                                    !TWAG.50    CANCEL TERMINAL #
CC     *        A<FS_LEN>, I0)                                                    !TWAG.51    # OF SIMPLE BETS
CCC LOTTO WAGER BODY
CC1040   FORMAT(A<FS_LEN>, I0,                                                    !TWAG.42    # OF LOTTO MARKS/BOARD
CC     *       2A<FS_LEN>,                                                        !TWAG.43-44
CC     *        A<FS_LEN>, I0,                                                    !TWAG.45    SYSTEM TYPE
CC     *        A<FS_LEN>,                                                        !TWAG.46-46 FILLER FIELDS
CC     *        A<FS_LEN>, I0,                                                    !TWAG.47    KICKER SOLD FLAG
CC     *        A<FS_LEN>, I7.7,                                                  !TWAG.48    KICKER NUMBER
CC     *        A<FS_LEN>, A,                                                     !TWAG.49    EXTERNAL CANCEL SERIAL NUMBER
CC!     *        A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TWAG.49    EXTERNAL CANCEL SERIAL NUMBER
CC     *        A<FS_LEN>, I0,                                                    !TWAG.50    CANCEL TERMINAL #
CC     *        A<FS_LEN>, I0,                                                    !TWAG.51    # OF SIMPLE BETS
CC     *        A<FS_LEN>, I0)                                                    !TWAG.52    LUCKY #
CCC EPASSIVE WAGER BODY
CC1050   FORMAT(7A<FS_LEN>,                                                       !TWAG.42-48 FILLER FIELDS
CC     *        A<FS_LEN>, A,                                                     !TWAG.49    EXTERNAL CANCEL SERIAL NUMBER
CC!     *        A<FS_LEN>, I3.3,'-',I8.8,'-',I3.3,                                !TWAG.49    EXTERNAL CANCEL SERIAL NUMBER
CC     *        A<FS_LEN>, I0,                                                    !TWAG.50    CANCEL TERMINAL #
CC     *       2A<FS_LEN>,                                                        !TWAG.51-52 FILLER FIELDS
CC     *        A<FS_LEN>, I2.2,'/',I4.4,                                         !TWAG.53    WEEK/YEAR
CC     *        A<FS_LEN>, I5.5,                                                  !TWAG.54    SALE FOR NUMBER
CC     *        A<FS_LEN>, I2.2,                                                  !TWAG.55    SALE FOR SERIE
CC     *        A<FS_LEN>, I2.2,                                                  !TWAG.56    SALE FOR FRACTION
CC     *        A<FS_LEN>, I<TRABUF(TWEPNE)>.<TRABUF(TWEPNE)>,                    !TWAG.57    RESERVE MASK
CC     *        A<FS_LEN>, I0,                                                    !TWAG.58    NUMBER OF GIVEN ENDING NUMBERS
CC     *        A<FS_LEN>, I0,                                                    !TWAG.59    NUMBER OF REQUESTED FRACTIONS
CC     *        A<FS_LEN>, I0)                                                    !TWAG.60    NUMBER OF RESERVATIONS/RELEASES
CC       RETURN
CC       END
CCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC       SUBROUTINE PRINT_TINC (QLIKDATA,TRABUF)
CCC
CCC       THIS SUBROUTINE WRITES INTERNAL CANCELLATION TRANSACTIONS INTO
CCC       QLIK FILE.
CCC
CCC       INPUTS:
CCC        QLIKDATA       QLIK DATA STRUCTURE
CCC        TRABUF         INC TRANSACTION TO WRITE INTO THE FILE
CCC
CCC       OUTPUTS:
CCC        *NONE*
CCC
CCC=======================================================================
CCC====== OPTIONS /CHECK=NOOVERFLOW
CC       SUBROUTINE PRINT_TINC(QLIKDATA,TRABUF)
CC       IMPLICIT NONE
CCC
CC       INCLUDE 'INCLIB:SYSEXTRN.DEF'
CC       INCLUDE 'INCLIB:GLOBAL.DEF'
CC       INCLUDE 'INCLIB:DESTRA.DEF'
CC       INCLUDE 'INCLIB:DATBUF.DEF'
CC       INCLUDE 'INCLIB:QLIKTRAN.DEF'
CC       INCLUDE 'INCLIB:AGTCOM.DEF'
CC
CCC
CC       CHARACTER*8 IAGT_NO                                              ! EXTERNAL FUNCTION
CC       CHARACTER*8 GET_YYYYMMDD_CDC                                     ! EXTERNAL FUNCTION
CCC
CC       WRITE(QLIKDATA.QLIK_LUN,1000) TRABUF(TSTAT),                     ! STATUS
CC     *                 FS, TRABUF(TERR),                                ! ERROR CODE
CC     *                 FS, GET_YYYYMMDD_CDC(TRABUF(TCDC)),              ! TRANSACTION DATE
CC     *                 FS, TRABUF(TSER),                                ! INTERNAL SERIAL NUMBER
CC     *                 FS, DISTIM(TRABUF(TTIM)),                        ! TIME STAMP HH:MI:SS
CC     *                 FS, TRABUF(TTER),                                ! TERMINAL NUMBER
CC     *                 FS, IAGT_NO(TRABUF(TAGT)),                       ! AGENT NUMBER
CC     *                 FS, AGTSAP(TRABUF(TTER)),                                ! SAP NUMBER
CC     *                 FS, TRABUF(TTRN),                                ! TRANSACTION SEQUENCE NUMBER
CC     *                 FS, TRABUF(TTYP),                                ! TRANSACTION TYPE
CC     *                 FS, TRABUF(TGAM),                                ! GAME NUMBER
CC     *                 FS, TRABUF(TGAMTYP),                             ! GAME TYPE
CC     *                 FS, TRABUF(TGAMIND),                             ! GAME INDEX
CC     *                 FS, TRABUF(TTSTCS),                              ! TERMINAL STATISTICS
CC     *                 FS, TRABUF(TINTRA),                              ! INTERNAL TRANSACTION FLAG
CC     *                 FS, TRABUF(TFIL),                                ! FILE STATUS
CC     *                 FS, TRABUF(TTKID),                               ! TICKET ID
CC     *                 FS, TRABUF(TCHK),                                ! MESSAGE CHECKSUM
CC     *                 FS, TRABUF(TFRAC),                               ! # OF FRACTIONS
CC     *                 FS, TRABUF(TSIZE),                               ! TRANSACTION SIZE (# LOG RECS)
CC     *                 FS, TRABUF(TSUBERR),                             ! ERROR SUB CODE
CC     *                 FS, TRABUF(TCDC_SOLD),                           ! CDC TRANS. WAS SOLD (NEVER CHANGES)
CC     *                 FS, TRABUF(TFAMTFLG),                            ! BET AMOUNT FLAG (FOR FRACTIONS)
CC     *                 FS, TRABUF(TNFRAC),                              ! REAL NUMBER OF FRACTIONS
CC     *                 FS, TRABUF(TWADDFW),                             ! PROMOTION - ADD 1 FREE WEEK
CCC       INCA DETAIL
CC     *                 FS
CC
CCC
CC1000   FORMAT(           I0,                                            ! STATUS
CC     *        A<FS_LEN>, I0,                                            ! ERROR CODE
CC     *        A<FS_LEN>, A8,                                            ! TRANSACTION DATE
CC     *        A<FS_LEN>, I0,                                            ! INTERNAL SERIAL NUMBER
CC     *        A<FS_LEN>, A8,                                            ! TIME STAMP HH:MI:SS
CC     *        A<FS_LEN>, I0,                                            ! TERMINAL NUMBER
CC     *        A<FS_LEN>, A8,                                            ! AGENT NUMBER
CC     *        A<FS_LEN>, I0,                                            ! SAP NUMBER
CC     *        A<FS_LEN>, I0,                                            ! TRANSACTION SEQUENCE NUMBER
CC     *        A<FS_LEN>, I0,                                            ! TRANSACTION TYPE
CC     *        A<FS_LEN>, I0,                                            ! GAME NUMBER
CC     *        A<FS_LEN>, I0,                                            ! GAME TYPE
CC     *        A<FS_LEN>, I0,                                            ! GAME INDEX
CC     *        A<FS_LEN>, I0,                                            ! TERMINAL STATISTICS
CC     *        A<FS_LEN>, I0,                                            ! INTERNAL TRANSACTION FLAG
CC     *        A<FS_LEN>, I0,                                            ! FILE STATUS
CC     *        A<FS_LEN>, I0,                                            ! TICKET ID
CC     *        A<FS_LEN>, I0,                                            ! MESSAGE CHECKSUM
CC     *        A<FS_LEN>, I0,                                            ! # OF FRACTIONS
CC     *        A<FS_LEN>, I0,                                            ! TRANSACTION SIZE (# LOG RECS)
CC     *        A<FS_LEN>, I0,                                            ! ERROR SUB CODE
CC     *        A<FS_LEN>, I0,                                            ! CDC TRANS. WAS SOLD (NEVER CHANGES)
CC     *        A<FS_LEN>, I0,                                            ! BET AMOUNT FLAG (FOR FRACTIONS)
CC     *        A<FS_LEN>, I0,                                            ! REAL NUMBER OF FRACTIONS
CC     *        A<FS_LEN>, I0,                                            ! PROMOTION - ADD 1 FREE WEEK
CCC INCA DETAIL
CC     *        A<FS_LEN>)
CC       RETURN
CC       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_TVAL (QLIKDATA,TRABUF)
C
C       THIS SUBROUTINE WRITES VALIDATION TRANSACTIONS INTO
C       QLIK FILE.
C
C       INPUTS:
C        QLIKDATA       QLIK DATA STRUCTURE
C        TRABUF         VAL TRANSACTION TO WRITE INTO THE FILE
C
C       OUTPUTS:
C        *NONE*
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE PRINT_TVAL(QLIKDATA,TRABUF)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:DESTRA.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:QLIKTRAN.DEF'
       INCLUDE 'INCLIB:AGTCOM.DEF'
C
       INTEGER*2 DBUF(12)
C
       INTEGER*4 XTSER, XTCHK, XWSER, XWCHK
       CHARACTER*16 C16XTCOD, C16XWCOD                                          !TRANSACTION/WAGER EXTERNAL CODE
       CHARACTER*8 DRWNAM, DRWKNAM
C
       CHARACTER*8  CDRWNAM                                                     !LOCAL FUNCTION
       CHARACTER*8  CAGTN                                                       !LOCAL FUNCTION
       CHARACTER*19 PASTCKCODE                                                  !LOCAL FUNCTION
       CHARACTER*8  IAGT_NO                                                     !EXTERNAL FUNCTION
       CHARACTER*8  GET_YYYYMMDD_CDC                                            !EXTERNAL FUNCTION
C
       INTEGER*4 I, TCKS, TOTPRZAMT
C
       INTEGER*4  NIB(6)
       CHARACTER*24 CNIB
       EQUIVALENCE (NIB,CNIB)
       INTEGER*4  BLANK
C
       C32TMP  = BLNK_SPCS_32
       C32TMP2 = BLNK_SPCS_32
       C32TMP3 = BLNK_SPCS_32
       C32TMP4 = BLNK_SPCS_32
       C32TMP5 = BLNK_SPCS_32
       C32TMP6 = BLNK_SPCS_32
       C32TMP7 = BLNK_SPCS_32
       C32TMP8 = BLNK_SPCS_32
       C32TMP9 = BLNK_SPCS_32
       C32TMP10= BLNK_SPCS_32
C
       C16XTCOD = BLNK_SPCS_16
       C16XWCOD = BLNK_SPCS_16
C
       XTSER   = 0
       XTCHK   = 0
       XWSER   = 0
       XWCHK   = 0
C
C VAL TRANSACTION HEADER
       WRITE(C32TMP6,'(A8)') DISTIM(TRABUF(TTIM))                               !TIME STAMP HH:MI:SS
       WRITE(QLIKDATA.QLIK_LUN,1000,ADVANCE=WRITE__ADVANCE_VALUE)
     *           TRABUF(TSTAT),                                                 !TVAL.1     STATUS
     *       FS, TRABUF(TERR),                                                  !TVAL.2     ERROR CODE
     *       FS, GET_YYYYMMDD_CDC(TRABUF(TCDC)),                                !TVAL.3     TRANSACTION DATE
     *       FS, TRABUF(TSER),                                                  !TVAL.4     INTERNAL SERIAL NUMBER
     *       FS, TRIM(C32TMP6),                                                 !TVAL.5     TIME STAMP HH:MI:SS
     *       FS, TRABUF(TTER),                                                  !TVAL.6     TERMINAL NUMBER
     *       FS, IAGT_NO(TRABUF(TAGT)),                                         !TVAL.7     AGENT NUMBER
     *       FS, AGTSAP(TRABUF(TTER)),                                          !TVAL.8     SAP NUMBER
     *       FS, TRABUF(TTRN),                                                  !TVAL.9     TRANSACTION SEQUENCE NUMBER
     *       FS, TRABUF(TTYP),                                                  !TVAL.10    TRANSACTION TYPE
     *       FS, TRABUF(TGAM),                                                  !TVAL.11    GAME NUMBER
     *       FS, TRABUF(TGAMTYP),                                               !TVAL.12    GAME TYPE
     *       FS, TRABUF(TGAMIND),                                               !TVAL.13    GAME INDEX
     *       FS, TRABUF(TTSTCS),                                                !TVAL.14    TERMINAL STATISTICS
     *       FS, TRABUF(TINTRA),                                                !TVAL.15    INTERNAL TRANSACTION FLAG
     *       FS, TRABUF(TFIL),                                                  !TVAL.16    FILE STATUS
     *       FS, TRABUF(TTKID),                                                 !TVAL.17    TICKET ID
     *       FS, TRABUF(TCHK),                                                  !TVAL.18    MESSAGE CHECKSUM
     *       FS, TRABUF(TFRAC),                                                 !TVAL.19    # OF FRACTIONS
     *       FS, TRABUF(TSIZE),                                                 !TVAL.20    TRANSACTION SIZE (# LOG RECS)
     *       FS, TRABUF(TSUBERR),                                               !TVAL.21    ERROR SUB CODE
     *      (FS, I=22,22),                                                      !TVAL.22    FILLER FIELD (CDC TRANS. WAS SOLD (NEVER CHANGES))
     *       FS, TRABUF(TFAMTFLG),                                              !TVAL.23    BET AMOUNT FLAG (FOR FRACTIONS)
     *       FS, TRABUF(TNFRAC),                                                !TVAL.24    REAL NUMBER OF FRACTIONS
     *       FS, TRABUF(TWADDFW),                                               !TVAL.25    PROMOTION - ADD 1 FREE WEEK
     *      (FS, I=26,26)                                                       !TVAL.26-26 FILLER FIELDS
       IF(TDETAIL) THEN
C        VAL DETAIL
         CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),XTSER,XTCHK)
         DBUF(VCDC)=TRABUF(TCDC)
         CALL CDATE(DBUF)
         WRITE(C16XTCOD,'(I3.3,A1,I8.8,A1,I3.3)')
     *          DBUF(VJUL),'-',XTSER,'-',XTCHK                                  !EXTERNAL TRANSACTION SERIAL NUMBER
         IF(TRABUF(TVSER).GT.0) THEN
           CALL OUTGEN(TRABUF(TVCDC),TRABUF(TVSER),XWSER,XWCHK)
           DBUF(VCDC)=TRABUF(TVCDC)                                             !WAGER CDC
           CALL CDATE(DBUF)
           WRITE(C16XWCOD,'(I3.3,A1,I8.8,A1,I3.3)')
     *           DBUF(VJUL),'-',XWSER,'-',XWCHK                                 !EXTERNAL WAGER SERIAL NUMBER VALIDATED
         ENDIF
         IF(TRABUF(TVSTER).GT.0) THEN
           C32TMP5 = CAGTN(TRABUF(TVSTER))                                      !WAGER SELL TERMINAL
         ENDIF
C
         IF(TRABUF(TGAMTYP).NE.TPAS) THEN                                       !LOTTO, KICKER AND SPORT GAME TYPES
           WRITE(C32TMP,'(A11)')
     *           CSMONY(TRABUF(TVOPPAY)+TRABUF(TVKOPPAY),11,VALUNIT)            !TOTAL AMOUNT PAID (NET VALUE)
           WRITE(C32TMP2,'(A11)')
     *           CSMONY(TRABUF(TVPAY),11,VALUNIT)                               !TOTAL AMOUNT PAID (NET VALUE)
           WRITE(C32TMP3,'(A11)')
     *           CSMONY(TRABUF(TVKPAY),11,VALUNIT)                              !TOTAL AMOUNT PAID (NET VALUE)
           WRITE(QLIKDATA.QLIK_LUN,1100,ADVANCE='NO')
     *           FS, TRIM(C16XTCOD),                                            !TVAL.27    EXTERNAL TRANSACTION SERIAL NUMBER
     *           FS, TRIM(C16XWCOD),                                            !TVAL.28    EXTERNAL WAGER SERIAL NUMBER
     *           FS, TRIM(ADJUSTL(C32TMP)),                                     !TVAL.29    TOTAL AMOUNT
     *           FS, TRIM(C32TMP6),                                             !TVAL.30    TIME STAMP HH:MI:SS
     *           FS, TRABUF(TVCODE),                                            !TVAL.31    VALIDATION CODE
     *          (FS, I=32,47),                                                  !TVAL.32-47 FILLER FIELDS
     *           FS, TRABUF(TVKGME),                                            !TVAL.48    KICKER GAME #
     *           FS, TRIM(C32TMP5),                                             !TVAL.49    WAGER SELL AGENT NUMBER
     *          (FS, I=50,58),                                                  !TVAL.50-58 FILLER FIELDS
     *           FS, TRABUF(TVTYPE),                                            !TVAL.59    VALIDATION TYPE (PRIZE PAYMENT MODE)
     *           FS, TRIM(ADJUSTL(C32TMP2)),                                    !TVAL.60    MAIN GAME PRIZE VALUE
     *           FS, TRIM(ADJUSTL(C32TMP3))                                     !TVAL.61    KICKER PRIZE VALUE
           IF(TRABUF(TVTYPE).EQ.VNBNK) THEN
             WRITE(QLIKDATA.QLIK_LUN,1110,ADVANCE='NO')
     *             FS, TRABUF(TVPLIDTYP),                                       !TVAL.62    PLAYER ID TYPE (IN)
     *             FS, TRABUF(TVPLCARD),                                        !TVAL.63    PLAYER CARD/TELEPHONE CONTACT NUMBER (IN)
     *             FS, TRABUF(TVNIBBB),                                         !TVAL.64    PLAYER NIB - BANK BRANCH
     *                 TRABUF(TVNIBBO),                                         !TVAL.64    PLAYER NIB - BANK OFFICE
     *                 TRABUF(TVNIBBA1),                                        !TVAL.64    PLAYER NIB - BANK ACCOUNT PART 1
     *                 TRABUF(TVNIBBA2),                                        !TVAL.64    PLAYER NIB - BANK ACCOUNT PART 2
     *                 TRABUF(TVNIBCD)                                          !TVAL.64    PLAYER NIB - CHECK DIGITS
           ENDIF
         ELSE                                                                   !PASSIVE GAME TYPE
           TOTPRZAMT = 0
           DO TCKS = 1, TRABUF(TPTCK)
             IF(TRABUF(TPSTS1+OFFTRA*(TCKS-1)).EQ.VWINNER) THEN
               TOTPRZAMT = TOTPRZAMT + TRABUF(TPPAY1+OFFTRA*(TCKS-1))
             ENDIF
           ENDDO
           CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),XTSER,XTCHK)
           DBUF(VCDC)=TRABUF(TCDC)
           CALL CDATE(DBUF)
           WRITE(C16XTCOD,'(I3.3,A1,I8.8,A1,I3.3)')
     *            DBUF(VJUL),'-',XTSER,'-',XTCHK                                !EXTERNAL TRANSACTION SERIAL NUMBER
           IF(TRABUF(TVEPTYP) .EQ.1 .AND. TRABUF(TVSER).GT.0) THEN              !ONLY AN ETICKET HAS A WAGER SERIAL NUMBER
             CALL OUTGEN(TRABUF(TVCDC),TRABUF(TVSER),XWSER,XWCHK)
             DBUF(VCDC)=TRABUF(TVCDC)                                           !WAGER CDC
             CALL CDATE(DBUF)
             WRITE(C16XWCOD,'(I3.3,A1,I8.8,A1,I3.3)')
     *             DBUF(VJUL),'-',XWSER,'-',XWCHK                               !EXTERNAL WAGER SERIAL NUMBER VALIDATED
           ENDIF
C
           IF(TRABUF(TPTCK).EQ.1) THEN                                          !CONSIDER ONLY ONE TICKET VALIDATION (NOT BATCH VALIDATION)
             WRITE(C32TMP4,'(I0)')   TRABUF(TPSTS1)                             !TICKET STATUS (PTICKET AND ETICKET)
             WRITE(C32TMP5,'(I5.5)') TRABUF(TPNUM1)                             !TICKET NUMBER
             WRITE(C32TMP7,'(I2.2)') TRABUF(TPSER1)                             !TICKET SERIE
             WRITE(C32TMP8,'(I2.2)') TRABUF(TPTEN1)                             !TICKET FRACTION
             IF(TRABUF(TVEPTYP).EQ.1) THEN
               WRITE(C32TMP2,'(A19)') PASTCKCODE(TRABUF)                        !ETICKET CODE I-WW-YY-NNNNN-SS-FF
             ENDIF
           ENDIF
           WRITE(C32TMP,'(A11)') CSMONY(TRABUF(TVOPPAY),11,VALUNIT)             !TOTAL AMOUNT PAID (NET VALUE)
           WRITE(C32TMP9,'(A11)') CSMONY(TOTPRZAMT,11,VALUNIT)                  !PRIZE VALUE 
           WRITE(QLIKDATA.QLIK_LUN,1200,ADVANCE='NO')
     *           FS, TRIM(C16XTCOD),                                            !TVAL.27    EXTERNAL TRANSACTION SERIAL NUMBER
     *           FS, TRIM(C16XWCOD),                                            !TVAL.28    EXTERNAL WAGER SERIAL NUMBER
     *           FS, TRIM(ADJUSTL(C32TMP)),                                     !TVAL.29    TOTAL AMOUNT
     *           FS, TRIM(C32TMP6),                                             !TVAL.30    TIME STAMP HH:MI:SS
     *           FS, TRIM(C32TMP4),                                             !TVAL.31    TICKET VALIDATION STATUS
     *          (FS, I=32,34),                                                  !TVAL.32-34 FILLER FIELDS
     *           FS, TRIM(CDRWNAM(TRABUF)),                                     !TVAL.35    EXTRACTION NAME
     *          (FS, I=36,51),                                                  !TVAL.36-51 FILLER FIELDS
     *           FS, TRIM(C32TMP2),                                             !TVAL.52    TICKET EXTERNAL CODE
     *           FS, TRIM(C32TMP5),                                             !TVAL.53    TICKET NUMBER
     *           FS, TRIM(C32TMP7),                                             !TVAL.54    TICKET SERIE
     *           FS, TRIM(C32TMP8),                                             !TVAL.55    TICKET FRACTION
     *          (FS, I=56,58),                                                  !TVAL.56-58 FILLER FIELDS
     *           FS, TRABUF(TVTYPE),                                            !TVAL.59    VALIDATION TYPE (PRIZE PAYMENT MODE)
     *           FS, TRIM(ADJUSTL(C32TMP9)),                                    !TVAL.60    MAIN GAME PRIZE VALUE
     *          (FS, I=61,61)                                                   !TVAL.61-61 FILLER FIELDS
           IF(TRABUF(TVTYPE).EQ.VPNBNK) THEN
             WRITE(QLIKDATA.QLIK_LUN,1110,ADVANCE='NO')
     *             FS, TRABUF(TVPLIDTYP),                                       !TVAL.62    PLAYER ID TYPE (IN)
     *             FS, TRABUF(TVPLCARD),                                        !TVAL.63    PLAYER CARD/TELEPHONE CONTACT NUMBER (IN)
     *             FS, TRABUF(TVNIBBB),                                         !TVAL.64    PLAYER NIB - BANK BRANCH
     *                 TRABUF(TVNIBBO),                                         !TVAL.64    PLAYER NIB - BANK OFFICE
     *                 TRABUF(TVNIBBA1),                                        !TVAL.64    PLAYER NIB - BANK ACCOUNT PART 1
     *                 TRABUF(TVNIBBA2),                                        !TVAL.64    PLAYER NIB - BANK ACCOUNT PART 2
     *                 TRABUF(TVNIBCD)                                          !TVAL.64    PLAYER NIB - CHECK DIGITS
           ELSE
             WRITE(QLIKDATA.QLIK_LUN,'(3A<FS_LEN>)',ADVANCE='NO')
     *            (FS, I=62,65)                                                 !TVAL.62-65 FILLER FIELDS
           ENDIF
           IF(TRABUF(TPOFFTER).GT.0) C32TMP10 = CAGTN(TRABUF(TPOFFTER))         !OFFLINE AGENT NUMBER
           WRITE(QLIKDATA.QLIK_LUN,1210,ADVANCE='NO')
     *           FS, TRABUF(TVEPVAL),                                           !TVAL.66    NEW PASSIVE VALIDATION LAYOUT
     *           FS, TRABUF(TVEPTYP),                                           !TVAL.67    ETICKET FLAG (IF SET IS AN ETICKET)
     *           FS, TRABUF(TPTCK),                                             !TVAL.68    NUMBER OF TICKETS TO VALIDATE
     *           FS, TRIM(C32TMP10)                                             !TVAL.69    OFFLINE TERMINAL
!           DO TCKS = 1,TRABUF(TPTCK)
!             C32TMP = BLNK_SPCS_32
!             WRITE(C32TMP,'(A11)')
!     *             CSMONY(TRABUF(TPPAY1+OFFTRA*(TCKS-1)),11,VALUNIT)
!             WRITE(QLIKDATA.QLIK_LUN,1220,ADVANCE='NO')
!     *             FS, TRABUF(TPKEY1  + OFFTRA*(TCKS-1)),                       !TVAL.74    TICKET KEY
!     *             FS, TRABUF(TPEMIS1 + OFFTRA*(TCKS-1)),                       !TVAL.75    TICKET EMISSION NUMBER
!     *                 TRABUF(TPNUM1  + OFFTRA*(TCKS-1)),                       !TVAL.75    TICKET NUMBER
!     *                 TRABUF(TPSER1  + OFFTRA*(TCKS-1)),                       !TVAL.75    TICKET SERIE NUMBER
!     *                 TRABUF(TPTEN1  + OFFTRA*(TCKS-1)),                       !TVAL.75    TICKET FRACTION NUMBER
!     *             FS, TRABUF(TPSTS1  + OFFTRA*(TCKS-1)),                       !TVAL.76    TICKET VALIDATION STATUS
!     *             FS, TRIM(ADJUSTL(C32TMP))                                    !TVAL.77    TICKET PRIZE PAID
!           ENDDO
         ENDIF
         WRITE(QLIKDATA.QLIK_LUN,*)                                             !TVAL.      PRINT NEW LINE
       ENDIF
C VAL TRANSACTION HEADER
1000   FORMAT(           I0,                                                    !TVAL.1     STATUS
     *        A<FS_LEN>, I0,                                                    !TVAL.2     ERROR CODE
     *        A<FS_LEN>, A8,                                                    !TVAL.3     TRANSACTION DATE
     *        A<FS_LEN>, I0,                                                    !TVAL.4     INTERNAL SERIAL NUMBER
     *        A<FS_LEN>, A,                                                     !TVAL.5     TIME STAMP HH:MI:SS
     *        A<FS_LEN>, I0,                                                    !TVAL.6     TERMINAL NUMBER
     *        A<FS_LEN>, A8,                                                    !TVAL.7     AGENT NUMBER
     *        A<FS_LEN>, I0,                                                    !TVAL.8     SAP NUMBER
     *        A<FS_LEN>, I0,                                                    !TVAL.9     TRANSACTION SEQUENCE NUMBER
     *        A<FS_LEN>, I0,                                                    !TVAL.10    TRANSACTION TYPE
     *        A<FS_LEN>, I0,                                                    !TVAL.11    GAME NUMBER
     *        A<FS_LEN>, I0,                                                    !TVAL.12    GAME TYPE
     *        A<FS_LEN>, I0,                                                    !TVAL.13    GAME INDEX
     *        A<FS_LEN>, I0,                                                    !TVAL.14    TERMINAL STATISTICS
     *        A<FS_LEN>, I0,                                                    !TVAL.15    INTERNAL TRANSACTION FLAG
     *        A<FS_LEN>, I0,                                                    !TVAL.16    FILE STATUS
     *        A<FS_LEN>, I0,                                                    !TVAL.17    TICKET ID
     *        A<FS_LEN>, I0,                                                    !TVAL.18    MESSAGE CHECKSUM
     *        A<FS_LEN>, I0,                                                    !TVAL.19    # OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TVAL.20    TRANSACTION SIZE (# LOG RECS)
     *        A<FS_LEN>, I0,                                                    !TVAL.21    ERROR SUB CODE
     *        A<FS_LEN>,                                                        !TVAL.22    FILLER FIELD (CDC TRANS. WAS SOLD (NEVER CHANGES))
     *        A<FS_LEN>, I0,                                                    !TVAL.23    BET AMOUNT FLAG (FOR FRACTIONS)
     *        A<FS_LEN>, I0,                                                    !TVAL.24    REAL NUMBER OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TVAL.25    PROMOTION - ADD 1 FREE WEEK
     *        A<FS_LEN>)                                                        !TVAL.26-26 FILLER FIELD FILLER FIELD
C VAL DETAIL FOR LOTTO, KICKER AND SPORT GAMES
1100   FORMAT(A<FS_LEN>, A,                                                     !TVAL.27    TRANSACTION EXTERNAL SERIAL NUMBER
     *        A<FS_LEN>, A,                                                     !TVAL.28    WAGER EXTERNAL SERIAL NUMBER
     *        A<FS_LEN>, A,                                                     !TVAL.29    TOTAL AMOUNT
     *        A<FS_LEN>, A,                                                     !TVAL.30    TIME STAMP HH:MI:SS
     *        A<FS_LEN>, I0,                                                    !TVAL.31    VALIDATION CODE
     *      16A<FS_LEN>,                                                        !TVAL.32-47 FILLER FIELDS
     *        A<FS_LEN>, I0,                                                    !TVAL.48    KICKER GAME #
     *        A<FS_LEN>, A,                                                     !TVAL.49    WAGER SELL AGENT NUMBER
     *       9A<FS_LEN>,                                                        !TVAL.50-58 FILLER FIELDS
     *        A<FS_LEN>, I0,                                                    !TVAL.59    VALIDATION TYPE
     *        A<FS_LEN>, A,                                                     !TVAL.60    MAIN GAME PRIZE AMOUNT
     *        A<FS_LEN>, A)                                                     !TVAL.61    KICKER PRIZE AMOUNT
C BANK TRANSFER DATA
1110   FORMAT(A<FS_LEN>, I0,                                                    !TVAL.62    PLAYER ID TYPE (IN)
     *        A<FS_LEN>, I0,                                                    !TVAL.63    PLAYER CARD/TELEPHONE CONTACT NUMBER (IN)
     *        A<FS_LEN>  I4.4,1X,I4.4,I9.9,I2.2,1X,I2.2)                        !TVAL.64    PLAYER NIB (IN)
C VAL DETAIL FOR PASSIVE GAME
1200   FORMAT(A<FS_LEN>, A,                                                     !TVAL.27    TRANSACTION EXTERNAL SERIAL NUMBER
     *        A<FS_LEN>, A,                                                     !TVAL.28    WAGER EXTERNAL SERIAL NUMBER
     *        A<FS_LEN>, A,                                                     !TVAL.29    TOTAL AMOUNT
     *        A<FS_LEN>, A,                                                     !TVAL.30    TIME STAMP HH:MI:SS
     *        A<FS_LEN>, A,                                                     !TVAL.31    TICKET VALIDATION STATUS
     *       3A<FS_LEN>,                                                        !TVAL.32-34 FILLER FIELDS
     *        A<FS_LEN>, A,                                                     !TVAL.35    EXTRACTION NAME
     *      16A<FS_LEN>,                                                        !TVAL.36-51 FILLER FIELDS
     *        A<FS_LEN>, A,                                                     !TVAL.52    ETICKET EXTERNAL CODE
     *        A<FS_LEN>, A,                                                     !TVAL.53    TICKET NUMBER
     *        A<FS_LEN>, A,                                                     !TVAL.54    TICKET SERIE
     *        A<FS_LEN>, A,                                                     !TVAL.55    TICKET FRACTION
     *       3A<FS_LEN>,                                                        !TVAL.56-58 FILLER FIELDS
     *        A<FS_LEN>, I0,                                                    !TVAL.59    VALIDATION TYPE
     *        A<FS_LEN>, A,                                                     !TVAL.60    MAIN GAME PRIZE AMOUNT
     *        A<FS_LEN>)                                                        !TVAL.61-61 FILLER FIELDS
C
1210   FORMAT(A<FS_LEN>, I0,                                                    !TVAL.65    NEW PASSIVE VALIDATION LAYOUT
     *        A<FS_LEN>, I0,                                                    !TVAL.66    ETICKET FLAG (IF SET IS AN ETICKET)
     *        A<FS_LEN>, I0,                                                    !TVAL.67    NUMBER OF TICKETS TO VALIDATE
     *        A<FS_LEN>, A)                                                     !TVAL.68    OFFLINE AGENT NUMBER
C
!1220   FORMAT(A<FS_LEN>, I0,                                                    ! TICKET TICKET KEY
!     *        A<FS_LEN>, I0,'-',I5.5,'-',I2.2,'-',I2.2,                         ! TICKET EMISSION-TICKET NUMBER-SERIE-FRACTION
!     *        A<FS_LEN>, I0,                                                    ! TICKET VALIDATION STATUS
!     *        A<FS_LEN>, A)                                                     ! TICKET PRIZE PAID
       RETURN
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_TSPE (QLIKDATA,TRABUF)
C
C       THIS SUBROUTINE WRITES SPECIAL TRANSACTIONS INTO
C       QLIK FILE (TRANSACTION DETAIL NO IMPLEMENTED YET).
C
C       INPUTS:
C        QLIKDATA       QLIK DATA STRUCTURE
C        TRABUF         SPE TRANSACTION TO WRITE INTO THE FILE
C
C       OUTPUTS:
C        *NONE*
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE PRINT_TSPE(QLIKDATA,TRABUF)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:DESTRA.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:QLIKTRAN.DEF'
       INCLUDE 'INCLIB:AGTCOM.DEF'
C
       CHARACTER*8 IAGT_NO                                                      !EXTERNAL FUNCTION
       CHARACTER*8 GET_YYYYMMDD_CDC                                             !EXTERNAL FUNCTION
C
C SPE TRANSACTION HEADER
       WRITE(C32TMP6,'(A8)') DISTIM(TRABUF(TTIM))                               !TIME STAMP HH:MI:SS
       WRITE(QLIKDATA.QLIK_LUN,1000,ADVANCE=WRITE__ADVANCE_VALUE)
     *           TRABUF(TSTAT),                                                 !TSPE.1     STATUS
     *       FS, TRABUF(TERR),                                                  !TSPE.2     ERROR CODE
     *       FS, GET_YYYYMMDD_CDC(TRABUF(TCDC)),                                !TSPE.3     TRANSACTION DATE
     *       FS, TRABUF(TSER),                                                  !TSPE.4     INTERNAL SERIAL NUMBER
     *       FS, TRIM(C32TMP6),                                                 !TSPE.5     TIME STAMP HH:MI:SS
     *       FS, TRABUF(TTER),                                                  !TSPE.6     TERMINAL NUMBER
     *       FS, IAGT_NO(TRABUF(TAGT)),                                         !TSPE.7     AGENT NUMBER
     *       FS, AGTSAP(TRABUF(TTER)),                                          !TSPE.8     SAP NUMBER
     *       FS, TRABUF(TTRN),                                                  !TSPE.9     TRANSACTION SEQUENCE NUMBER
     *       FS, TRABUF(TTYP),                                                  !TSPE.10    TRANSACTION TYPE
     *       FS, TRABUF(TGAM),                                                  !TSPE.11    GAME NUMBER
     *       FS, TRABUF(TGAMTYP),                                               !TSPE.12    GAME TYPE
     *       FS, TRABUF(TGAMIND),                                               !TSPE.13    GAME INDEX
     *       FS, TRABUF(TTSTCS),                                                !TSPE.14    TERMINAL STATISTICS
     *       FS, TRABUF(TINTRA),                                                !TSPE.15    INTERNAL TRANSACTION FLAG
     *       FS, TRABUF(TFIL),                                                  !TSPE.16    FILE STATUS
     *       FS, TRABUF(TTKID),                                                 !TSPE.17    TICKET ID
     *       FS, TRABUF(TCHK),                                                  !TSPE.18    MESSAGE CHECKSUM
     *       FS, TRABUF(TFRAC),                                                 !TSPE.19    # OF FRACTIONS
     *       FS, TRABUF(TSIZE),                                                 !TSPE.20    TRANSACTION SIZE (# LOG RECS)
     *       FS, TRABUF(TSUBERR),                                               !TSPE.21    ERROR SUB CODE
     *       FS, TRABUF(TCDC_SOLD),                                             !TSPE.22    CDC TRANS. WAS SOLD (NEVER CHANGES)
     *       FS, TRABUF(TFAMTFLG),                                              !TSPE.23    BET AMOUNT FLAG (FOR FRACTIONS)
     *       FS, TRABUF(TNFRAC),                                                !TSPE.24    REAL NUMBER OF FRACTIONS
     *       FS, TRABUF(TWADDFW),                                               !TSPE.25    PROMOTION - ADD 1 FREE WEEK
     *       FS, TRABUF(TSFUN)                                                  !TSPE.26    SPECIAL FUNCTION
       IF(TDETAIL) THEN
C       SPE DETAIL
         WRITE(QLIKDATA.QLIK_LUN,1100) FS
       ENDIF
C
1000   FORMAT(           I0,                                                    !TSPE.1     STATUS
     *        A<FS_LEN>, I0,                                                    !TSPE.2     ERROR CODE
     *        A<FS_LEN>, A8,                                                    !TSPE.3     TRANSACTION DATE
     *        A<FS_LEN>, I0,                                                    !TSPE.4     INTERNAL SERIAL NUMBER
     *        A<FS_LEN>, A,                                                     !TSPE.5     TIME STAMP HH:MI:SS
     *        A<FS_LEN>, I0,                                                    !TSPE.6     TERMINAL NUMBER
     *        A<FS_LEN>, A8,                                                    !TSPE.7     AGENT NUMBER
     *        A<FS_LEN>, I0,                                                    !TSPE.8     SAP NUMBER
     *        A<FS_LEN>, I0,                                                    !TSPE.9     TRANSACTION SEQUENCE NUMBER
     *        A<FS_LEN>, I0,                                                    !TSPE.10    TRANSACTION TYPE
     *        A<FS_LEN>, I0,                                                    !TSPE.11    GAME NUMBER
     *        A<FS_LEN>, I0,                                                    !TSPE.12    GAME TYPE
     *        A<FS_LEN>, I0,                                                    !TSPE.13    GAME INDEX
     *        A<FS_LEN>, I0,                                                    !TSPE.14    TERMINAL STATISTICS
     *        A<FS_LEN>, I0,                                                    !TSPE.15    INTERNAL TRANSACTION FLAG
     *        A<FS_LEN>, I0,                                                    !TSPE.16    FILE STATUS
     *        A<FS_LEN>, I0,                                                    !TSPE.17    TICKET ID
     *        A<FS_LEN>, I0,                                                    !TSPE.18    MESSAGE CHECKSUM
     *        A<FS_LEN>, I0,                                                    !TSPE.19    # OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TSPE.20    TRANSACTION SIZE (# LOG RECS)
     *        A<FS_LEN>, I0,                                                    !TSPE.21    ERROR SUB CODE
     *        A<FS_LEN>, I0,                                                    !TSPE.22    CDC TRANS. WAS SOLD (NEVER CHANGES)
     *        A<FS_LEN>, I0,                                                    !TSPE.23    BET AMOUNT FLAG (FOR FRACTIONS)
     *        A<FS_LEN>, I0,                                                    !TSPE.24    REAL NUMBER OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TSPE.25    PROMOTION - ADD 1 FREE WEEK
     *        A<FS_LEN>, I0)                                                    !TSPE.26    SPECIAL FUNCTION
C SPE DETAIL
1100   FORMAT(A<FS_LEN>)                                                        !
       RETURN
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_TCMD (QLIKDATA,TRABUF)
C
C       THIS SUBROUTINE WRITES COMMAND TRANSACTIONS INTO
C       QLIK FILE (TRANSACTION DETAIL NO IMPLEMENTED YET).
C
C       INPUTS:
C        QLIKDATA       QLIK DATA STRUCTURE
C        TRABUF         CMD TRANSACTION TO WRITE INTO THE FILE
C
C       OUTPUTS:
C        *NONE*
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE PRINT_TCMD(QLIKDATA,TRABUF)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:DESTRA.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:QLIKTRAN.DEF'
       INCLUDE 'INCLIB:AGTCOM.DEF'
C
       INTEGER*4 I
       CHARACTER*8 IAGT_NO                                                      !EXTERNAL FUNCTION
       CHARACTER*8 GET_YYYYMMDD_CDC                                             !EXTERNAL FUNCTION
C
C CMD TRANSACTION HEADER
       WRITE(C32TMP6,'(A8)') DISTIM(TRABUF(TTIM))                               !TIME STAMP HH:MI:SS
       IF(TRABUF(TTER) .LE. 0) THEN
         WRITE(QLIKDATA.QLIK_LUN,1000)
     *             TRABUF(TSTAT),                                               !TCMD.1     STATUS
     *         FS, TRABUF(TERR),                                                !TCMD.2     ERROR CODE
     *         FS, GET_YYYYMMDD_CDC(TRABUF(TCDC)),                              !TCMD.3     TRANSACTION DATE
     *         FS, TRABUF(TSER),                                                !TCMD.4     INTERNAL SERIAL NUMBER
     *         FS, TRIM(C32TMP6),                                               !TCMD.5     TIME STAMP HH:MI:SS
     *         FS,                                                              !TCMD.6     TERMINAL NUMBER IS EMPTY
     *         FS,                                                              !TCMD.7     AGENT NUMBER IS EMPTY
     *         FS,                                                              !TCMD.8     SAP NUMBER IS EMPTY
     *         FS, TRABUF(TTRN),                                                !TCMD.9     TRANSACTION SEQUENCE NUMBER
     *         FS, TRABUF(TTYP),                                                !TCMD.10    TRANSACTION TYPE
     *         FS, TRABUF(TGAM),                                                !TCMD.11    GAME NUMBER
     *         FS, TRABUF(TGAMTYP),                                             !TCMD.12    GAME TYPE
     *         FS, TRABUF(TGAMIND),                                             !TCMD.13    GAME INDEX
     *         FS, TRABUF(TTSTCS),                                              !TCMD.14    TERMINAL STATISTICS
     *         FS, TRABUF(TINTRA),                                              !TCMD.15    INTERNAL TRANSACTION FLAG
     *         FS, TRABUF(TFIL),                                                !TCMD.16    FILE STATUS
     *         FS, TRABUF(TTKID),                                               !TCMD.17    TICKET ID
     *         FS, TRABUF(TCHK),                                                !TCMD.18    MESSAGE CHECKSUM
     *         FS, TRABUF(TFRAC),                                               !TCMD.19    # OF FRACTIONS
     *         FS, TRABUF(TSIZE),                                               !TCMD.20    TRANSACTION SIZE (# LOG RECS)
     *         FS, TRABUF(TSUBERR),                                             !TCMD.21    ERROR SUB CODE
     *         FS, TRABUF(TCDC_SOLD),                                           !TCMD.22    CDC TRANS. WAS SOLD (NEVER CHANGES)
     *         FS, TRABUF(TFAMTFLG),                                            !TCMD.23    BET AMOUNT FLAG (FOR FRACTIONS)
     *         FS, TRABUF(TNFRAC),                                              !TCMD.24    REAL NUMBER OF FRACTIONS
     *         FS, TRABUF(TWADDFW),                                             !TCMD.25    PROMOTION - ADD 1 FREE WEEK
     *        (FS, I=26,26)                                                     !TCMD.26-26 FILLER FIELDS
        ELSE
         WRITE(QLIKDATA.QLIK_LUN,1100) 
     *             TRABUF(TSTAT),                                               !TCMD.1     STATUS
     *         FS, TRABUF(TERR),                                                !TCMD.2     ERROR CODE
     *         FS, GET_YYYYMMDD_CDC(TRABUF(TCDC)),                              !TCMD.3     TRANSACTION DATE
     *         FS, TRABUF(TSER),                                                !TCMD.4     INTERNAL SERIAL NUMBER
     *         FS, TRIM(C32TMP6),                                               !TCMD.5     TIME STAMP HH:MI:SS
     *         FS, TRABUF(TTER),                                                !TCMD.6     TERMINAL NUMBER
     *         FS, IAGT_NO(TRABUF(TAGT)),                                       !TCMD.7     AGENT NUMBER
     *         FS, AGTSAP(TRABUF(TTER)),                                        !TCMD.8     SAP NUMBER
     *         FS, TRABUF(TTRN),                                                !TCMD.9     TRANSACTION SEQUENCE NUMBER
     *         FS, TRABUF(TTYP),                                                !TCMD.10    TRANSACTION TYPE
     *         FS, TRABUF(TGAM),                                                !TCMD.11    GAME NUMBER
     *         FS, TRABUF(TGAMTYP),                                             !TCMD.12    GAME TYPE
     *         FS, TRABUF(TGAMIND),                                             !TCMD.13    GAME INDEX
     *         FS, TRABUF(TTSTCS),                                              !TCMD.14    TERMINAL STATISTICS
     *         FS, TRABUF(TINTRA),                                              !TCMD.15    INTERNAL TRANSACTION FLAG
     *         FS, TRABUF(TFIL),                                                !TCMD.16    FILE STATUS
     *         FS, TRABUF(TTKID),                                               !TCMD.17    TICKET ID
     *         FS, TRABUF(TCHK),                                                !TCMD.18    MESSAGE CHECKSUM
     *         FS, TRABUF(TFRAC),                                               !TCMD.19    # OF FRACTIONS
     *         FS, TRABUF(TSIZE),                                               !TCMD.20    TRANSACTION SIZE (# LOG RECS)
     *         FS, TRABUF(TSUBERR),                                             !TCMD.21    ERROR SUB CODE
     *         FS, TRABUF(TCDC_SOLD),                                           !TCMD.22    CDC TRANS. WAS SOLD (NEVER CHANGES)
     *         FS, TRABUF(TFAMTFLG),                                            !TCMD.23    BET AMOUNT FLAG (FOR FRACTIONS)
     *         FS, TRABUF(TNFRAC),                                              !TCMD.24    REAL NUMBER OF FRACTIONS
     *         FS, TRABUF(TWADDFW),                                             !TCMD.25    PROMOTION - ADD 1 FREE WEEK
     *        (FS, I=26,26)                                                     !TCMD.26-26 FILLER FIELDS
        ENDIF
C
1000   FORMAT(           I0,                                                    !TCMD.1     !STATUS
     *        A<FS_LEN>, I0,                                                    !TCMD.2     !ERROR CODE
     *        A<FS_LEN>, A8,                                                    !TCMD.3     !TRANSACTION DATE
     *        A<FS_LEN>, I0,                                                    !TCMD.4     !INTERNAL SERIAL NUMBER
     *        A<FS_LEN>, A,                                                     !TCMD.5     !TIME STAMP HH:MI:SS
     *        A<FS_LEN>,                                                        !TCMD.6     !TERMINAL NUMBER
     *        A<FS_LEN>,                                                        !TCMD.7     !AGENT NUMBER
     *        A<FS_LEN>,                                                        !TCMD.8     !SAP NUMBER
     *        A<FS_LEN>, I0,                                                    !TCMD.9     !TRANSACTION SEQUENCE NUMBER
     *        A<FS_LEN>, I0,                                                    !TCMD.10    !TRANSACTION TYPE
     *        A<FS_LEN>, I0,                                                    !TCMD.11    !GAME NUMBER
     *        A<FS_LEN>, I0,                                                    !TCMD.12    !GAME TYPE
     *        A<FS_LEN>, I0,                                                    !TCMD.13    !GAME INDEX
     *        A<FS_LEN>, I0,                                                    !TCMD.14    !TERMINAL STATISTICS
     *        A<FS_LEN>, I0,                                                    !TCMD.15    !INTERNAL TRANSACTION FLAG
     *        A<FS_LEN>, I0,                                                    !TCMD.16    !FILE STATUS
     *        A<FS_LEN>, I0,                                                    !TCMD.17    !TICKET ID
     *        A<FS_LEN>, I0,                                                    !TCMD.18    !MESSAGE CHECKSUM
     *        A<FS_LEN>, I0,                                                    !TCMD.19    !# OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TCMD.20    !TRANSACTION SIZE (# LOG RECS)
     *        A<FS_LEN>, I0,                                                    !TCMD.21    !ERROR SUB CODE
     *        A<FS_LEN>, I0,                                                    !TCMD.22    !CDC TRANS. WAS SOLD (NEVER CHANGES)
     *        A<FS_LEN>, I0,                                                    !TCMD.23    !BET AMOUNT FLAG (FOR FRACTIONS)
     *        A<FS_LEN>, I0,                                                    !TCMD.24    !REAL NUMBER OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TCMD.25    !PROMOTION - ADD 1 FREE WEEK
     *        A<FS_LEN>)                                                        !TCMD.26-26 !FILLER FIELDS
C
1100   FORMAT(           I0,                                                    !TCMD.1     !STATUS
     *        A<FS_LEN>, I0,                                                    !TCMD.2     !ERROR CODE
     *        A<FS_LEN>, A8,                                                    !TCMD.3     !TRANSACTION DATE
     *        A<FS_LEN>, I0,                                                    !TCMD.4     !INTERNAL SERIAL NUMBER
     *        A<FS_LEN>, A,                                                     !TCMD.5     !TIME STAMP HH:MI:SS
     *        A<FS_LEN>, I0,                                                    !TCMD.6     !TERMINAL NUMBER
     *        A<FS_LEN>, A8,                                                    !TCMD.7     !AGENT NUMBER
     *        A<FS_LEN>, I0,                                                    !TCMD.8     ! SAP NUMBER
     *        A<FS_LEN>, I0,                                                    !TCMD.9     !TRANSACTION SEQUENCE NUMBER
     *        A<FS_LEN>, I0,                                                    !TCMD.10    !TRANSACTION TYPE
     *        A<FS_LEN>, I0,                                                    !TCMD.11    !GAME NUMBER
     *        A<FS_LEN>, I0,                                                    !TCMD.12    !GAME TYPE
     *        A<FS_LEN>, I0,                                                    !TCMD.13    !GAME INDEX
     *        A<FS_LEN>, I0,                                                    !TCMD.14    !TERMINAL STATISTICS
     *        A<FS_LEN>, I0,                                                    !TCMD.15    !INTERNAL TRANSACTION FLAG
     *        A<FS_LEN>, I0,                                                    !TCMD.16    !FILE STATUS
     *        A<FS_LEN>, I0,                                                    !TCMD.17    !TICKET ID
     *        A<FS_LEN>, I0,                                                    !TCMD.18    !MESSAGE CHECKSUM
     *        A<FS_LEN>, I0,                                                    !TCMD.19    !# OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TCMD.20    !TRANSACTION SIZE (# LOG RECS)
     *        A<FS_LEN>, I0,                                                    !TCMD.21    !ERROR SUB CODE
     *        A<FS_LEN>, I0,                                                    !TCMD.22    !CDC TRANS. WAS SOLD (NEVER CHANGES)
     *        A<FS_LEN>, I0,                                                    !TCMD.23    !BET AMOUNT FLAG (FOR FRACTIONS)
     *        A<FS_LEN>, I0,                                                    !TCMD.24    !REAL NUMBER OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TCMD.25    !PROMOTION - ADD 1 FREE WEEK
     *        A<FS_LEN>)                                                        !TCMD.26-26 !FILLER FIELDS
       RETURN
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_TCRS (QLIKDATA,TRABUF)
C
C       THIS SUBROUTINE WRITES CROSS SYSTEM TRANSACTIONS INTO
C       QLIK FILE (TRANSACTION DETAIL NO IMPLEMENTED YET).
C
C       INPUTS:
C        QLIKDATA       QLIK DATA STRUCTURE
C        TRABUF         CRS TRANSACTION TO WRITE INTO THE FILE
C
C       OUTPUTS:
C        *NONE*
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE PRINT_TCRS(QLIKDATA,TRABUF)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:DESTRA.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:QLIKTRAN.DEF'
       INCLUDE 'INCLIB:AGTCOM.DEF'
C
       INTEGER*4 I, TCKS, TOTPRZAMT
C
       INTEGER*4  NIB(6)
       CHARACTER*24 CNIB
       EQUIVALENCE (NIB,CNIB)
       INTEGER*4  BLANK
C
       CHARACTER*8 IAGT_NO                                                      !EXTERNAL FUNCTION
       CHARACTER*8 GET_YYYYMMDD_CDC                                             !EXTERNAL FUNCTION
C
C CRS TRANSACTION HEADER
       WRITE(C32TMP6,'(A8)') DISTIM(TRABUF(TTIM))                               !TIME STAMP HH:MI:SS
       WRITE(QLIKDATA.QLIK_LUN,1000,ADVANCE=WRITE__ADVANCE_VALUE)
     *           TRABUF(TSTAT),                                                 !TCRS.1     STATUS
     *       FS, TRABUF(TERR),                                                  !TCRS.2     ERROR CODE
     *       FS, GET_YYYYMMDD_CDC(TRABUF(TCDC)),                                !TCRS.3     TRANSACTION DATE
     *       FS, TRABUF(TSER),                                                  !TCRS.4     INTERNAL SERIAL NUMBER
     *       FS, TRIM(C32TMP6),                                                 !TCRS.5     TIME STAMP HH:MI:SS
     *       FS, TRABUF(TTER),                                                  !TCRS.6     TERMINAL NUMBER
     *       FS, IAGT_NO(TRABUF(TAGT)),                                         !TCRS.7     AGENT NUMBER
     *       FS, AGTSAP(TRABUF(TTER)),                                          !TCRS.8     SAP NUMBER
     *       FS, TRABUF(TTRN),                                                  !TCRS.9     TRANSACTION SEQUENCE NUMBER
     *       FS, TRABUF(TTYP),                                                  !TCRS.10    TRANSACTION TYPE
     *       FS, TRABUF(TGAM),                                                  !TCRS.11    GAME NUMBER
     *       FS, TRABUF(TGAMTYP),                                               !TCRS.12    GAME TYPE
     *       FS, TRABUF(TGAMIND),                                               !TCRS.13    GAME INDEX
     *       FS, TRABUF(TTSTCS),                                                !TCRS.14    TERMINAL STATISTICS
     *       FS, TRABUF(TINTRA),                                                !TCRS.15    INTERNAL TRANSACTION FLAG
     *       FS, TRABUF(TFIL),                                                  !TCRS.16    FILE STATUS
     *       FS, TRABUF(TTKID),                                                 !TCRS.17    TICKET ID
     *       FS, TRABUF(TCHK),                                                  !TCRS.18    MESSAGE CHECKSUM
     *       FS, TRABUF(TFRAC),                                                 !TCRS.19    # OF FRACTIONS
     *       FS, TRABUF(TSIZE),                                                 !TCRS.20    TRANSACTION SIZE (# LOG RECS)
     *       FS, TRABUF(TSUBERR),                                               !TCRS.21    ERROR SUB CODE
     *       FS, TRABUF(TCDC_SOLD),                                             !TCRS.22    CDC TRANS. WAS SOLD (NEVER CHANGES)
     *       FS, TRABUF(TFAMTFLG),                                              !TCRS.23    BET AMOUNT FLAG (FOR FRACTIONS)
     *       FS, TRABUF(TNFRAC),                                                !TCRS.24    REAL NUMBER OF FRACTIONS
     *       FS, TRABUF(TWADDFW),                                               !TCRS.25    PROMOTION - ADD 1 FREE WEEK
     *       FS, TRABUF(TITYP)                                                  !TCRS.26-26 INSTANT TRANSACTION TYPE
C       CRS DETAIL - NOT FINISHED YET !!!!!!!
       IF(TDETAIL) THEN
         IF(TRABUF(TITYP) .EQ. IVAL) THEN
           CALL FASTSET(BLANK,NIB,6)
           WRITE(CNIB,'(I4.4,1X,I4.4,I9.9,I2.2,1X,I2.2)')
!         WRITE(CNIB,'(I0,1X,I0,I0,I0,1X,I0)')
     *                    TRABUF(TINIBBB)
     *                  , TRABUF(TINIBBO)
     *                  , TRABUF(TINIBBA1)
     *                  , TRABUF(TINIBBA2)
     *                  , TRABUF(TINIBCD)
C
!           TOTPRZAMT = 0
!           DO TCKS = 1, TRABUF(TPTCK)
!             IF(TRABUF(TPSTS1+OFFTRA*(TCKS-1)).EQ.VWINNER) THEN
!               TOTPRZAMT = TOTPRZAMT + TRABUF(TPPAY1+OFFTRA*(TCKS-1))
!             ENDIF
!           ENDDO
C
           WRITE(QLIKDATA.QLIK_LUN,1010,ADVANCE='NO')
     *                   FS, TRABUF(TIBCH),                                     ! # TICKETS TO VALIDATE
     *                   FS, TRABUF(TIVMT),                                     ! INSTANT VALIDATIOM MODE TYPE
     *                   FS, TRABUF(TIVALM),                                    ! VALIDATION MODE
     *                   FS, TRABUF(TIVALT),                                    ! PAYMENT MODE
     *                   FS, TRABUF(TIVENV),                                    ! ENVELOPE ID #
     *                   FS, TRABUF(TIVAGT),                                    ! VALIDATING RETAILER
     *                   FS, TRABUF(TIPLIDTYP),                                 ! PLAYER ID TYPE
     *                   FS, TRABUF(TIPLCARD),                                  ! PLAYER ID
     *                   FS, (NIB(I),I=1,6),                                    ! PLAYER NIB (IN)
     *                   FS, CMONY(TRABUF(TINETPRZ),11,VALUNIT),                ! NET PRIZE AMOUNT (VALIDATION UNITS)
     *                   FS, TRABUF(TIIND)                                      ! TICKET INDICATOR
C
           IF(TRABUF(TIBCH) .GT. 0) THEN
             DO TCKS = 0,TRABUF(TIBCH)-1
               WRITE(QLIKDATA.QLIK_LUN,1020,ADVANCE='NO')
     *           FS, TRABUF(TIGAM1    + TCKS),                                  ! INSTANT GAME NUMBER
     *               TRABUF(TIPCK1    + TCKS),                                  ! INSTANT PACK NUMBER
     *               TRABUF(TIVRN1    + TCKS),                                  ! INSTANT VIRN NUMBER
     *               TRABUF(TILTX1    + TCKS),                                  ! INSTANT LATEX NUMBER
     *           FS, TRABUF(TIPCKSTS1 + TCKS),                                  ! INSTANT PACK STATUS
     *           FS, TRABUF(TITIM1    + TCKS),                                  ! INSTANT TIME CASHED AT GVT
     *           FS, TRABUF(TICDC1    + TCKS),                                  ! INSTANT DATE CASHED AT GVT
     *           FS, TRABUF(TISTS1    + TCKS),                                  ! INSTANT VALIDATION STATUS
     *           FS, CMONY(TRABUF(TIPRZ1 + TCKS),11,VALUNIT),                   ! INSTANT PRIZE FROM GAME PLAN
     *           FS, TRABUF(TIVTYP    + TCKS),                                  ! INSTANT VALIDATION TYPE
     *           FS, TRABUF(TIVDESCR)                                           ! NON-CASH PRIZE DESCRIPTION
             ENDDO
           ENDIF
           WRITE(QLIKDATA.QLIK_LUN,*)                                           ! PRINT NEW LINE
!         ELSEIF(TRABUF(TITYP) .EQ. IISS) THEN
!
         ELSE
           WRITE(QLIKDATA.QLIK_LUN,*)                                           ! PRINT NEW LINE
         ENDIF
       ENDIF
C
1000   FORMAT(           I0,                                                    !TCRS.1     STATUS
     *        A<FS_LEN>, I0,                                                    !TCRS.2     ERROR CODE
     *        A<FS_LEN>, A8,                                                    !TCRS.3     TRANSACTION DATE
     *        A<FS_LEN>, I0,                                                    !TCRS.4     INTERNAL SERIAL NUMBER
     *        A<FS_LEN>, A,                                                     !TCRS.5     TIME STAMP HH:MI:SS
     *        A<FS_LEN>, I0,                                                    !TCRS.6     TERMINAL NUMBER
     *        A<FS_LEN>, A8,                                                    !TCRS.7     AGENT NUMBER
     *        A<FS_LEN>, I0,                                                    !TCRS.8     SAP NUMBER
     *        A<FS_LEN>, I0,                                                    !TCRS.9     TRANSACTION SEQUENCE NUMBER
     *        A<FS_LEN>, I0,                                                    !TCRS.10    TRANSACTION TYPE
     *        A<FS_LEN>, I0,                                                    !TCRS.11    GAME NUMBER
     *        A<FS_LEN>, I0,                                                    !TCRS.12    GAME TYPE
     *        A<FS_LEN>, I0,                                                    !TCRS.13    GAME INDEX
     *        A<FS_LEN>, I0,                                                    !TCRS.14    TERMINAL STATISTICS
     *        A<FS_LEN>, I0,                                                    !TCRS.15    INTERNAL TRANSACTION FLAG
     *        A<FS_LEN>, I0,                                                    !TCRS.16    FILE STATUS
     *        A<FS_LEN>, I0,                                                    !TCRS.17    TICKET ID
     *        A<FS_LEN>, I0,                                                    !TCRS.18    MESSAGE CHECKSUM
     *        A<FS_LEN>, I0,                                                    !TCRS.19    # OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TCRS.20    TRANSACTION SIZE (# LOG RECS)
     *        A<FS_LEN>, I0,                                                    !TCRS.21    ERROR SUB CODE
     *        A<FS_LEN>, I0,                                                    !TCRS.22    CDC TRANS. WAS SOLD (NEVER CHANGES)
     *        A<FS_LEN>, I0,                                                    !TCRS.23    BET AMOUNT FLAG (FOR FRACTIONS)
     *        A<FS_LEN>, I0,                                                    !TCRS.24    REAL NUMBER OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TCRS.25    PROMOTION - ADD 1 FREE WEEK
     *        A<FS_LEN>, I0)                                                    !TCRS.26-26 INSTANT TRANSACTION TYPE
C CRS DETAIL
1010   FORMAT(A<FS_LEN>, I0,                                                    ! # TICKETS TO VALIDATE
     *        A<FS_LEN>, I0,                                                    ! INSTANT VALIDATIOM MODE TYPE
     *        A<FS_LEN>, I0,                                                    ! VALIDATION MODE
     *        A<FS_LEN>, I0,                                                    ! PAYMENT MODE
     *        A<FS_LEN>, I0,                                                    ! ENVELOPE ID #
     *        A<FS_LEN>, I0,                                                    ! VALIDATING RETAILER
     *        A<FS_LEN>, I0,                                                    ! PLAYER ID TYPE
     *        A<FS_LEN>, I0,                                                    ! PLAYER ID
     *        A<FS_LEN>, 6A4,                                                   ! PLAYER NIB (IN)
     *        A<FS_LEN>, A11,                                                   ! NET PRIZE AMOUNT (VALIDATION UNITS)
     *        A<FS_LEN>, I0)                                                    ! TICKET INDICATOR
C
1020   FORMAT(A<FS_LEN>, I4.4,'-',I7.7,'-',I9.9,'-',I4.4,                       ! TICKET INFO
     *        A<FS_LEN>, I0,                                                    ! INSTANT PACK STATUS
     *        A<FS_LEN>, I0,                                                    ! INSTANT TIME CASHED AT GVT
     *        A<FS_LEN>, I0,                                                    ! INSTANT DATE CASHED AT GVT
     *        A<FS_LEN>, I0,                                                    ! INSTANT VALIDATION STATUS
     *        A<FS_LEN>, A11,                                                   ! INSTANT PRIZE FROM GAME PLAN
     *        A<FS_LEN>, I0,                                                    ! INSTANT VALIDATION TYPE
     *        A<FS_LEN>, I0)                                                    ! NON-CASH PRIZE DESCRIPTION
       RETURN
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_TRET (QLIKDATA,TRABUF)
C
C       THIS SUBROUTINE WRITES RETURN TRANSACTIONS INTO
C       QLIK FILE.
C
C       INPUTS:
C        QLIKDATA       QLIK DATA STRUCTURE
C        TRABUF         RET TRANSACTION TO WRITE INTO THE FILE
C
C       OUTPUTS:
C        *NONE*
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE PRINT_TRET(QLIKDATA,TRABUF)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:DESTRA.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:QLIKTRAN.DEF'
       INCLUDE 'INCLIB:AGTCOM.DEF'
C
       INTEGER*4 I
       CHARACTER*8  IAGT_NO                                                     !EXTERNAL FUNCTION
       CHARACTER*8  GET_YYYYMMDD_CDC                                            !EXTERNAL FUNCTION
C
C RET TRANSACTION HEADER
       WRITE(C32TMP6,'(A8)') DISTIM(TRABUF(TTIM))                               !TIME STAMP HH:MI:SS
       WRITE(QLIKDATA.QLIK_LUN,1000,ADVANCE=WRITE__ADVANCE_VALUE)
     *          TRABUF(TSTAT),                                                  !TRET.1     STATUS
     *      FS, TRABUF(TERR),                                                   !TRET.2     ERROR CODE
     *      FS, GET_YYYYMMDD_CDC(TRABUF(TCDC)),                                 !TRET.3     TRANSACTION DATE
     *      FS, TRABUF(TSER),                                                   !TRET.4     INTERNAL SERIAL NUMBER
     *      FS, TRIM(C32TMP6),                                                  !TRET.5     TIME STAMP HH:MI:SS
     *      FS, TRABUF(TTER),                                                   !TRET.6     TERMINAL NUMBER
     *      FS, IAGT_NO(TRABUF(TAGT)),                                          !TRET.7     AGENT NUMBER
     *      FS, AGTSAP(TRABUF(TTER)),                                           !TRET.8     SAP NUMBER
     *      FS, TRABUF(TTRN),                                                   !TRET.9     TRANSACTION SEQUENCE NUMBER
     *      FS, TRABUF(TTYP),                                                   !TRET.10    TRANSACTION TYPE
     *      FS, TRABUF(TGAM),                                                   !TRET.11    GAME NUMBER
     *      FS, TRABUF(TGAMTYP),                                                !TRET.12    GAME TYPE
     *      FS, TRABUF(TGAMIND),                                                !TRET.13    GAME INDEX
     *      FS, TRABUF(TTSTCS),                                                 !TRET.14    TERMINAL STATISTICS
     *      FS, TRABUF(TINTRA),                                                 !TRET.15    INTERNAL TRANSACTION FLAG
     *      FS, TRABUF(TFIL),                                                   !TRET.16    FILE STATUS
     *      FS, TRABUF(TTKID),                                                  !TRET.17    TICKET ID
     *      FS, TRABUF(TCHK),                                                   !TRET.18    MESSAGE CHECKSUM
     *      FS, TRABUF(TFRAC),                                                  !TRET.19    # OF FRACTIONS
     *      FS, TRABUF(TSIZE),                                                  !TRET.20    TRANSACTION SIZE (# LOG RECS)
     *      FS, TRABUF(TSUBERR),                                                !TRET.21    ERROR SUB CODE
     *     (FS, I=22,22),                                                       !TRET.22    FILLER FIELD (CDC TRANS. WAS SOLD (NEVER CHANGES))
     *      FS, TRABUF(TFAMTFLG),                                               !TRET.23    BET AMOUNT FLAG (FOR FRACTIONS)
     *      FS, TRABUF(TNFRAC),                                                 !TRET.24    REAL NUMBER OF FRACTIONS
     *      FS, TRABUF(TWADDFW),                                                !TRET.25    PROMOTION - ADD 1 FREE WEEK
     *     (FS, I=26,26)                                                        !TRET.26-26 FILLER FIELDS
C RET TRANSACTION HEADER
1000   FORMAT(           I0,                                                    !TRET.1     STATUS
     *        A<FS_LEN>, I0,                                                    !TRET.2     ERROR CODE
     *        A<FS_LEN>, A8,                                                    !TRET.3     TRANSACTION DATE
     *        A<FS_LEN>, I0,                                                    !TRET.4     INTERNAL SERIAL NUMBER
     *        A<FS_LEN>, A,                                                     !TRET.5     TIME STAMP HH:MI:SS
     *        A<FS_LEN>, I0,                                                    !TRET.6     TERMINAL NUMBER
     *        A<FS_LEN>, A8,                                                    !TRET.7     AGENT NUMBER
     *        A<FS_LEN>, I0,                                                    !TRET.8     SAP NUMBER
     *        A<FS_LEN>, I0,                                                    !TRET.9     TRANSACTION SEQUENCE NUMBER
     *        A<FS_LEN>, I0,                                                    !TRET.10    TRANSACTION TYPE
     *        A<FS_LEN>, I0,                                                    !TRET.11    GAME NUMBER
     *        A<FS_LEN>, I0,                                                    !TRET.12    GAME TYPE
     *        A<FS_LEN>, I0,                                                    !TRET.13    GAME INDEX
     *        A<FS_LEN>, I0,                                                    !TRET.14    TERMINAL STATISTICS
     *        A<FS_LEN>, I0,                                                    !TRET.15    INTERNAL TRANSACTION FLAG
     *        A<FS_LEN>, I0,                                                    !TRET.16    FILE STATUS
     *        A<FS_LEN>, I0,                                                    !TRET.17    TICKET ID
     *        A<FS_LEN>, I0,                                                    !TRET.18    MESSAGE CHECKSUM
     *        A<FS_LEN>, I0,                                                    !TRET.19    # OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TRET.20    TRANSACTION SIZE (# LOG RECS)
     *        A<FS_LEN>, I0,                                                    !TRET.21    ERROR SUB CODE
     *        A<FS_LEN>,                                                        !TRET.22    FILLER FIELD (CDC TRANS. WAS SOLD (NEVER CHANGES))
     *        A<FS_LEN>, I0,                                                    !TRET.23    BET AMOUNT FLAG (FOR FRACTIONS)
     *        A<FS_LEN>, I0,                                                    !TRET.24    REAL NUMBER OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TRET.25    PROMOTION - ADD 1 FREE WEEK
     *        A<FS_LEN>)                                                        !TRET.26-26 FILLER FIELDS
       RETURN
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE PRINT_OTHER (QLIKDATA,TRABUF)
C
C       THIS SUBROUTINE WRITES OTHER TRANSACTIONS INTO
C       QLIK FILE.
C
C       INPUTS:
C        QLIKDATA       QLIK DATA STRUCTURE
C        TRABUF         OTHER TRANSACTION TO WRITE INTO THE FILE
C
C       OUTPUTS:
C        *NONE*
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       SUBROUTINE PRINT_OTHER(QLIKDATA,TRABUF)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:DESTRA.DEF'
       INCLUDE 'INCLIB:DATBUF.DEF'
       INCLUDE 'INCLIB:QLIKTRAN.DEF'
       INCLUDE 'INCLIB:AGTCOM.DEF'
C
       INTEGER*4 I
       CHARACTER*8 IAGT_NO                                                      !EXTERNAL FUNCTION
       CHARACTER*8 GET_YYYYMMDD_CDC                                             !EXTERNAL FUNCTION
C
C OTHER TRANSACTION HEADER
       WRITE(C32TMP6,'(A8)') DISTIM(TRABUF(TTIM))                               !TIME STAMP HH:MI:SS
       WRITE(QLIKDATA.QLIK_LUN,1000) 
     *          TRABUF(TSTAT),                                                  !TOTH.1     STATUS
     *      FS, TRABUF(TERR),                                                   !TOTH.2     ERROR CODE
     *      FS, GET_YYYYMMDD_CDC(TRABUF(TCDC)),                                 !TOTH.3     TRANSACTION DATE
     *      FS, TRABUF(TSER),                                                   !TOTH.4     INTERNAL SERIAL NUMBER
     *      FS, TRIM(C32TMP6),                                                  !TOTH.5     TIME STAMP HH:MI:SS
     *      FS, TRABUF(TTER),                                                   !TOTH.6     TERMINAL NUMBER
     *      FS, IAGT_NO(TRABUF(TAGT)),                                          !TOTH.7     AGENT NUMBER
     *      FS, AGTSAP(TRABUF(TTER)),                                           !TOTH.8     SAP NUMBER
     *      FS, TRABUF(TTRN),                                                   !TOTH.9     TRANSACTION SEQUENCE NUMBER
     *      FS, TRABUF(TTYP),                                                   !TOTH.10    TRANSACTION TYPE
     *      FS, TRABUF(TGAM),                                                   !TOTH.11    GAME NUMBER
     *      FS, TRABUF(TGAMTYP),                                                !TOTH.12    GAME TYPE
     *      FS, TRABUF(TGAMIND),                                                !TOTH.13    GAME INDEX
     *      FS, TRABUF(TTSTCS),                                                 !TOTH.14    TERMINAL STATISTICS
     *      FS, TRABUF(TINTRA),                                                 !TOTH.15    INTERNAL TRANSACTION FLAG
     *      FS, TRABUF(TFIL),                                                   !TOTH.16    FILE STATUS
     *      FS, TRABUF(TTKID),                                                  !TOTH.17    TICKET ID
     *      FS, TRABUF(TCHK),                                                   !TOTH.18    MESSAGE CHECKSUM
     *      FS, TRABUF(TFRAC),                                                  !TOTH.19    # OF FRACTIONS
     *      FS, TRABUF(TSIZE),                                                  !TOTH.20    TRANSACTION SIZE (# LOG RECS)
     *      FS, TRABUF(TSUBERR),                                                !TOTH.21    ERROR SUB CODE
     *      FS, TRABUF(TCDC_SOLD),                                              !TOTH.22    CDC TRANS. WAS SOLD (NEVER CHANGES)
     *      FS, TRABUF(TFAMTFLG),                                               !TOTH.23    BET AMOUNT FLAG (FOR FRACTIONS)
     *      FS, TRABUF(TNFRAC),                                                 !TOTH.24    REAL NUMBER OF FRACTIONS
     *      FS, TRABUF(TWADDFW),                                                !TOTH.25    PROMOTION - ADD 1 FREE WEEK
     *     (FS, I=26,26)                                                        !TOTH.26-26 FILLER FIELDS
C
1000   FORMAT(           I0,                                                    !TOTH.1     STATUS
     *        A<FS_LEN>, I0,                                                    !TOTH.2     ERROR CODE
     *        A<FS_LEN>, A8,                                                    !TOTH.3     TRANSACTION DATE
     *        A<FS_LEN>, I0,                                                    !TOTH.4     INTERNAL SERIAL NUMBER
     *        A<FS_LEN>, A,                                                     !TOTH.5     TIME STAMP HH:MI:SS
     *        A<FS_LEN>, I0,                                                    !TOTH.6     TERMINAL NUMBER
     *        A<FS_LEN>, A8,                                                    !TOTH.7     AGENT NUMBER
     *        A<FS_LEN>, I0,                                                    !TOTH.8     SAP NUMBER
     *        A<FS_LEN>, I0,                                                    !TOTH.9     TRANSACTION SEQUENCE NUMBER
     *        A<FS_LEN>, I0,                                                    !TOTH.10    TRANSACTION TYPE
     *        A<FS_LEN>, I0,                                                    !TOTH.11    GAME NUMBER
     *        A<FS_LEN>, I0,                                                    !TOTH.12    GAME TYPE
     *        A<FS_LEN>, I0,                                                    !TOTH.13    GAME INDEX
     *        A<FS_LEN>, I0,                                                    !TOTH.14    TERMINAL STATISTICS
     *        A<FS_LEN>, I0,                                                    !TOTH.15    INTERNAL TRANSACTION FLAG
     *        A<FS_LEN>, I0,                                                    !TOTH.16    FILE STATUS
     *        A<FS_LEN>, I0,                                                    !TOTH.17    TICKET ID
     *        A<FS_LEN>, I0,                                                    !TOTH.18    MESSAGE CHECKSUM
     *        A<FS_LEN>, I0,                                                    !TOTH.19    # OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TOTH.20    TRANSACTION SIZE (# LOG RECS)
     *        A<FS_LEN>, I0,                                                    !TOTH.21    ERROR SUB CODE
     *        A<FS_LEN>, I0,                                                    !TOTH.22    CDC TRANS. WAS SOLD (NEVER CHANGES)
     *        A<FS_LEN>, I0,                                                    !TOTH.23    BET AMOUNT FLAG (FOR FRACTIONS)
     *        A<FS_LEN>, I0,                                                    !TOTH.24    REAL NUMBER OF FRACTIONS
     *        A<FS_LEN>, I0,                                                    !TOTH.25    PROMOTION - ADD 1 FREE WEEK
     *        A<FS_LEN>)                                                        !TOTH.26-26 FILLER FIELDS
       RETURN
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       FUNCTION ODS_BETTYP(TBET,STID)
C
C       THIS FUNCTION RETURNS THE PLACARD BET TYPE GIVEN THE IGS WAGER
C       TRANSACTION.
C
C       INPUTS:
C        TRABUF         IGS WAGER TRANSACTION
C
C       OUTPUTS:
C        ODS_BETTYP     PLACARD BET TYPE
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       INTEGER*4 FUNCTION ODS_BETTYP(TRABUF)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:DESTRA.DEF'
       INCLUDE 'INCLIB:QLIKTRAN.DEF'
C
       ODS_BETTYP = UNDF_BETTYP                                                 !UNDEFINED BET TYPE (FUNCTION DEFAULT RETURN VALUE)
       IF(TRABUF(TGAMTYP) .EQ. TODS .AND. TRABUF(TGAMIND) .EQ. 1) THEN          !PLACARD GAME
         IF(TRABUF(TIGSW_STID) .EQ. 1) THEN
           ODS_BETTYP = SIMP_BETTYP                                             !SIMPLE BET
         ELSEIF(TRABUF(TIGSW_STID) .GT. 1) THEN
           IF(TRABUF(TIGSW_TBET) .GT. TRABUF(TIGSW_STID)) THEN
             ODS_BETTYP = MULT_BETTYP                                           !MULTIPLE BET
           ELSEIF(TRABUF(TIGSW_TBET) .EQ. TRABUF(TIGSW_STID)) THEN
             ODS_BETTYP = COMB_BETTYP                                           !COMBINED BET
           ENDIF
         ENDIF
       ENDIF
C
       RETURN
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       FUNCTION CDRWNAM(TRABUF)
C
C       THIS FUNCTION RETURNS THE DRAW NAME GIVEN THE TRANSACTION.
C
C       INPUTS:
C        TRABUF         TWAG TRANSACTION
C
C       OUTPUTS:
C        CDRWNAM        DRAW NAME
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       CHARACTER FUNCTION CDRWNAM*8(TRABUF)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:DESTRA.DEF'
       INCLUDE 'INCLIB:QLIKTRAN.DEF'
C
       INTEGER*4   DRWNUM, DRWYEAR, ST
       CHARACTER*8 CTEMP
C
       CTEMP = '        '
       IF(TRABUF(TTYP).EQ.TWAG) THEN
         IF(TRABUF(TGAMTYP).EQ.TLTO) THEN                                       !
!           CALL FIGCCC(TRABUF(TCDC),DRWNUM,DRWYEAR)
           CALL GETWEK(TRABUF(TWBEG),TRABUF(TGAM),DRWNUM,DRWYEAR,ST)
           WRITE(CTEMP,'(I3.3,A1,I4.4)') DRWNUM,'/',DRWYEAR
         ELSEIF(TRABUF(TGAMTYP) .EQ. TKIK) THEN
           CALL GETWEK(TRABUF(TWBEG),TRABUF(TGAM),DRWNUM,DRWYEAR,ST)
           WRITE(CTEMP,'(I2.2,A1,I4.4)') DRWNUM,'/',DRWYEAR
         ELSEIF(TRABUF(TGAMTYP).EQ.TSPT) THEN
           CALL GETWEK(TRABUF(TWBEG),TRABUF(TGAM),DRWNUM,DRWYEAR,ST)
           WRITE(CTEMP,'(I2.2,A1,I4.4)') DRWNUM,'/',DRWYEAR
         ELSEIF(TRABUF(TGAMTYP).EQ.TPAS) THEN
           CALL GETWEK(TRABUF(TWBEG),TRABUF(TGAM),DRWNUM,DRWYEAR,ST)
           WRITE(CTEMP,'(I2.2,A1,I4.4)') DRWNUM,'/',DRWYEAR
         ENDIF
       ELSEIF(TRABUF(TTYP).EQ.TVAL) THEN
         IF(TRABUF(TGAMTYP).EQ.TPAS) THEN
           IF(TRABUF(TVEPVAL).EQ.1) THEN                                        !NEW VALIDATION LAYOUT (ONE TICKET ONLY)
             IF(TRABUF(TVEPTYP).EQ.1) THEN                                      !EPASSIVE TICKET
               WRITE(CTEMP,'(I2.2,A1,I4.4)')
     *               TRABUF(TVEPWK),'/',2000+TRABUF(TVEPYR)
             ELSEIF(TRABUF(TVEPTYP).EQ.0 .AND. TRABUF(TPTCK).EQ.1) THEN         !PPASSIVE TICKET (ONE TICKET ONLY)
               CALL GETWEK(TRABUF(TPEMIS1),TRABUF(TGAM),DRWNUM,DRWYEAR,ST)
               WRITE(CTEMP,'(I2.2,A1,I4.4)') DRWNUM,'/',DRWYEAR
             ENDIF
           ENDIF
         ENDIF
       ENDIF
C
       CDRWNAM = CTEMP
C
       RETURN
       END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       FUNCTION CAGTN(TERN)
C
C       THIS FUNCTION RETURNS THE AGENT NUMBER GIVEN THE TERMINAL NUMBER
C       IN THE FORMAT AA-AAAAA.
C
C       INPUTS:
C        TERN           THE TERMINAL NUMBER
C
C       OUTPUTS:
C        CAGTN          THE AGENT NUMBER
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       CHARACTER FUNCTION CAGTN*8(TERN)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:AGTCOM.DEF'
C
       CHARACTER*8 CTEMP                                                        !TEMPORARY CHAR VARIABLE
       CHARACTER*8 IAGT_NO                                                      !EXTERNAL FUNCTION
       INTEGER*4 I, TERN

       CTEMP = '        '
       DO I=1,NUMAGT
         IF(AGT_LOOKUP_TER(I).EQ.TERN) THEN                                     !LOOK FOR THE AGENT NUMBER
           WRITE(CTEMP,'(A8)') IAGT_NO(AGT_LOOKUP_AGT(I))
           EXIT
         ENDIF
       ENDDO
C
       CAGTN = CTEMP
       RETURN
       END
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       FUNCTION PASTCKCODE(TRABUF)
C
C       THIS FUNCTION RETURNS THE PASSIVE TICKET CODE
C
C       INPUTS:
C        TRABUF         TWAG/TVAL TRANSACTION
C
C       OUTPUTS:
C        PASTCKCODE     PASSIVE TICKET IN THE FORMAT I-WW-YY-NNNNN-SS-FF
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
       CHARACTER FUNCTION PASTCKCODE*19(TRABUF)
       IMPLICIT NONE
C
       INCLUDE 'INCLIB:GLOBAL.DEF'
       INCLUDE 'INCLIB:DESTRA.DEF'
       INCLUDE 'INCLIB:QLIKTRAN.DEF'
C
       INTEGER*4    DRWNUM, DRWYEAR, ST
       CHARACTER*19 CTEMP
C
       CTEMP = '                   '
       IF(TRABUF(TTYP).EQ.TWAG) THEN
         IF(TRABUF(TGAMTYP).EQ.TPAS .AND. TRABUF(TWEPOP).EQ.EPASSAL) THEN
           CALL GETWEK(TRABUF(TWBEG),TRABUF(TGAM),DRWNUM,DRWYEAR,ST)
           WRITE(CTEMP,10)
     *           TRABUF(TGAMIND),
     *           DRWNUM,
     *           MOD(DRWYEAR,100),
     *           TRABUF(TVEPYR),
     *           TRABUF(TWEPSN),
     *           TRABUF(TWEPSS),
     *           TRABUF(TWEPSF)
         ENDIF
       ELSEIF(TRABUF(TTYP).EQ.TVAL) THEN
         IF(TRABUF(TGAMTYP).EQ.TPAS) THEN
           IF(TRABUF(TVEPVAL).EQ.1) THEN                                        !NEW VALIDATION LAYOUT (ONE TICKET ONLY)
             IF(TRABUF(TVEPTYP).EQ.1) THEN                                      !EPASSIVE TICKET
               WRITE(CTEMP,10)
     *           TRABUF(TGAMIND),
     *           TRABUF(TVEPWK),
     *           MOD(TRABUF(TVEPYR),100),
     *           TRABUF(TPNUM1),
     *           TRABUF(TPSER1),
     *           TRABUF(TPTEN1)
             ELSEIF(TRABUF(TVEPTYP).EQ.0 .AND. TRABUF(TPTCK).EQ.1) THEN         !PPASSIVE TICKET (ONE TICKET ONLY)
               CALL GETWEK(TRABUF(TPEMIS1),TRABUF(TGAM),DRWNUM,DRWYEAR,ST)
               WRITE(CTEMP,10)
     *           TRABUF(TGAMIND),
     *           DRWNUM,
     *           MOD(DRWYEAR,100),
     *           TRABUF(TPNUM1),
     *           TRABUF(TPSER1),
     *           TRABUF(TPTEN1)
             ENDIF
           ENDIF
         ENDIF
       ENDIF
C
       PASTCKCODE = CTEMP
C
C PASSIVE TICKET CODE FORMAT
10     FORMAT(I0,'-',I2.2,'-',I2.2,'-',I5.5,'-',I2.2,'-',I2.2)
       RETURN
       END
