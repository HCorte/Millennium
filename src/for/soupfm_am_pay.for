C
C Module SOUPFM_AM_PAY - SOUP File Management
C
C This program will manage the generation of the SOUP files
C for payed prizes of Apostas Mutuas (AM) 
C
C V01 19-DEC-2013 SCML Program creation
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE OPEN_FILE_SOUPFM_AM_PAY(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        
        INTEGER*4  ST
        INTEGER*4  RECORD_LEN
        PARAMETER (RECORD_LEN = 132)
        
        INTEGER*4 DATE_ARR(3)
        INTEGER*4 YEAR,MONTH,DAY
        EQUIVALENCE(DATE_ARR(DATE_ARR_YEAR),YEAR)
        EQUIVALENCE(DATE_ARR(DATE_ARR_MONTH),MONTH)
        EQUIVALENCE(DATE_ARR(DATE_ARR_DAY),DAY)
        
        ST = 0

        IF(SOUPFM_REC.HANDLE_FILE(IDX_AM_PAY_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        
C        SOUPFM_REC.FILE_GEN_CDC(IDX_AM_PAY_FILE) = DAYCDC
        SOUPFM_REC.FILE_DATA_CDC(IDX_AM_PAY_FILE) = 
     *         SOUPFM_REC.SRC_CDC_DATE
        
        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_DATA_CDC(IDX_AM_PAY_FILE)
     *        , DATE_ARR)
        
        CALL PRINT_FILE_NAME(INT_SRC_FILE_NAME
     *        , 'SOUP', 'AM', 'PP', YEAR, MONTH, DAY)
     
        CALL COPY_FROM_FNAME(SOUPFM_REC
     *        , IDX_AM_PAY_FILE,INT_SRC_FILE_NAME)
        
        IF(SOUPFM_REC.LUN_TABLE(IDX_AM_PAY_FILE) .EQ. 0) THEN
            CALL RESERVE_AVAILABLE_LUN(SOUPFM_REC,IDX_AM_PAY_FILE,ST)
            IF(ST .NE. 0) RETURN
        ENDIF
        
        ST = 0
        CALL ROPEN1(CHR_SRC_FILE_NAME
     *            , SOUPFM_REC.LUN_TABLE(IDX_AM_PAY_FILE)
     *            , RECORD_LEN, ST)
        
        IF(ST .NE. 0) THEN
            CALL FILERR(INT_SRC_FILE_NAME
     *           , FUN_OPEN_FILE,ST,0)
            CALL USRCLOS1(SOUPFM_REC.LUN_TABLE(IDX_AM_PAY_FILE))
            RETURN
        ENDIF
        
        CALL PRINT_HEADER_SOUPFM_AM_PAY(SOUPFM_REC, ST)
        IF(ST .NE. 0) THEN
            CALL FILERR(INT_SRC_FILE_NAME
     *           , FUN_WRITE_FILE,ST,0)
            RETURN
        ENDIF

        TYPE *, IAM(), 'Aberto ficheiro ',CHR_SRC_FILE_NAME

        ST = 0
        SOUPFM_REC.PAS_DEBUG = .TRUE.
        CALL OPEN_KEY_FILE_SOUPFM_VLF(SOUPFM_REC, ST)
        SOUPFM_REC.PAS_DEBUG = .FALSE.
        
        RETURN
        END



        SUBROUTINE PRINT_HEADER_SOUPFM_AM_PAY(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4 DATE_ARR_FILE_GEN(3)
        INTEGER*4 DATE_ARR_FILE_DATA(3)
        INTEGER*4 ST
        
        ST = 0
        
        CALL GET_SYS_DATE(DATE_ARR_FILE_GEN)
C        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_GEN_CDC(IDX_AM_PAY_FILE)
C     *        , DATE_ARR_FILE_GEN)

        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_DATA_CDC(IDX_AM_PAY_FILE)
     *        , DATE_ARR_FILE_DATA)

        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_PAY_FILE),20001) 
     *            'HP'
     *          ,   DATE_ARR_FILE_DATA(DATE_ARR_YEAR)  * 10000 
     *            + DATE_ARR_FILE_DATA(DATE_ARR_MONTH) * 100
     *            + DATE_ARR_FILE_DATA(DATE_ARR_DAY)   * 1
     *          ,   DATE_ARR_FILE_GEN(DATE_ARR_YEAR)   * 10000 
     *            + DATE_ARR_FILE_GEN(DATE_ARR_MONTH)  * 100
     *            + DATE_ARR_FILE_GEN(DATE_ARR_DAY)    * 1
        
        RETURN
20001   FORMAT(A2,I8.8,I8.8,114(' '))
        END





        SUBROUTINE HANDLE_TRANSACTION_SOUPFM_AM_PAY(TRABUF, SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4    ST, AUX
        INTEGER*4    DATE_ARR_TRX_GEN(3)

        ! Required variables to print
        INTEGER*4    REC_GAME_CODE
        INTEGER*8    REC_INT_BET_REF
        INTEGER*4    REC_EXT_BET_REF(3)
        CHARACTER*1  REC_PRIZE_PAY_MODE_ID
        INTEGER*4    REC_PRIZE_PAY_CHANNEL_ID
        INTEGER*4    REC_PRIZE_PAY_MEDIATOR
        INTEGER*8    REC_PRIZE_PAY_REF
        INTEGER*4    REC_PRIZE_PAY_DATE(6)
        INTEGER*8    REC_PRIZE_PAY_AMOUNT         !in cents
        INTEGER*8    REC_NET_PRIZE_PAY_AMOUNT     !in cents
        CHARACTER*9  REC_PLAYER_PHONE_NR
        CHARACTER*9  REC_PLAYER_CARD_NR
        CHARACTER*25 REC_PLAYER_IBAN
        INTEGER*4    REC_PLAYER_NIB(5)

        ST = 0
        
        IF(SOUPFM_REC.HANDLE_FILE(IDX_AM_PAY_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        

        !-----------
        ! Handling :
        !-----------
        ! 0) only good transactions 
        IF(   TRABUF(TSTAT)   .NE. GOOD) RETURN
        ! 1) only validation type transactions 
        IF(   TRABUF(TTYP)    .NE. TVAL) RETURN
        ! 2) only mutual games' bets transactions  
        IF(   TRABUF(TGAMTYP) .NE. TLTO
     *  .AND. TRABUF(TGAMTYP) .NE. TKIK
     *  .AND. TRABUF(TGAMTYP) .NE. TSPT) RETURN
        ! 3) only transactions without error  
        IF( TRABUF(TERR)    .NE. NOER) RETURN

        IF(TRABUF(TCDC) .NE. SOUPFM_REC.SRC_CDC_DATE) THEN
            TYPE *, IAM(), 'ERRO:'
            TYPE *, IAM(), ' Foi encontrada uma transaccao com data CDC'
     *                   , '(',TRABUF(TCDC),') diferente da fornecida ('
     *                   , SOUPFM_REC.SRC_CDC_DATE, ')!'
            ST = -1
        ENDIF

        
        IF(ST .NE. 0) RETURN

        SOUPFM_REC.VLF_CDC = TRABUF(TVCDC)
        SOUPFM_REC.VLF_SERIAL = TRABUF(TVSER)
        ! Check if transaction has payment order
        CALL READ_KEY_FILE_SOUPFM_VLF(SOUPFM_REC, VALREC, ST)
        IF(ST .NE. 0) THEN
            TYPE *, IAM(), 'ERRO: Nao foi possivel encontrar o registo de'
     *            , ' validacao com a chave '
     *            , SOUPFM_REC.VLF_CDC
     *            , SOUPFM_REC.VLF_SERIAL
     *            , ' no ficheiro VLF.'
            CALL GPAUSE
            RETURN
        ENDIF
        
        ! 4) only transactions without payment orders  
        IF(VALREC(VOPSCNT) .NE. 0) RETURN
        
        ! Record initialization
        REC_GAME_CODE            = 0
        REC_INT_BET_REF          = 0
        
        REC_EXT_BET_REF(1)       = 0
        REC_EXT_BET_REF(2)       = 0
        REC_EXT_BET_REF(3)       = 0
        
        REC_PRIZE_PAY_MODE_ID    = ' '
        REC_PRIZE_PAY_CHANNEL_ID = 0
        REC_PRIZE_PAY_MEDIATOR   = 0
        REC_PRIZE_PAY_REF        = 0
        
        REC_PRIZE_PAY_DATE(1)    = 0
        REC_PRIZE_PAY_DATE(2)    = 0
        REC_PRIZE_PAY_DATE(3)    = 0
        REC_PRIZE_PAY_DATE(4)    = 0
        REC_PRIZE_PAY_DATE(5)    = 0
        REC_PRIZE_PAY_DATE(6)    = 0
        
        REC_PRIZE_PAY_AMOUNT     = 0
        REC_NET_PRIZE_PAY_AMOUNT = 0
        REC_PLAYER_PHONE_NR      = '         '
        REC_PLAYER_CARD_NR       = '         '
        
        REC_PLAYER_IBAN          = '                         '
        REC_PLAYER_NIB(1)        = 0
        REC_PLAYER_NIB(2)        = 0
        REC_PLAYER_NIB(3)        = 0
        REC_PLAYER_NIB(4)        = 0
        REC_PLAYER_NIB(5)        = 0
        
        ST                       = 0

        !-----------------------------------------------------------
        ! Record fill
        !-----------------------------------------------------------
        ! Game code:
        REC_GAME_CODE            = TRABUF(TGAM)
            
        !-----------------------------------------------------------
        ! Internal bet ref:
        REC_INT_BET_REF          = GET_BET_REF(TRABUF)
        
        !-----------------------------------------------------------
        ! External bet ref:
        CALL GET_BET_EXTERNAL_REF(TRABUF, REC_EXT_BET_REF)

        !-----------------------------------------------------------
        ! Prize pay mode: (in Millennium, we only handle two kinds)
        IF(    TRABUF(TVTYPE)   .EQ. VNBNK
     *  .AND. (TRABUF(TVNIBBB)  .NE. 0
     *  .OR.   TRABUF(TVNIBBO)  .NE. 0
     *  .OR.   TRABUF(TVNIBBA1) .NE. 0
     *  .OR.   TRABUF(TVNIBBA2) .NE. 0
     *  .OR.   TRABUF(TVNIBCD)  .NE. 0)
     *  ) THEN
            REC_PRIZE_PAY_MODE_ID    = PRIZE_PAY_MODE_BANK_TRF
        ELSE
            IF(GET_PRIZE_PAY_CHANNEL_ID(TRABUF) .NE. PRIZE_PAY_CHANNEL_ID_GAME_PORTAL) THEN
                REC_PRIZE_PAY_MODE_ID    = PRIZE_PAY_MODE_CASH_CARD
            ELSE
                REC_PRIZE_PAY_MODE_ID    = PRIZE_PAY_MODE_PORTAL_UNDEF
            ENDIF
        ENDIF
        
        !-----------------------------------------------------------
        ! Prize pay channel id:
        REC_PRIZE_PAY_CHANNEL_ID = GET_PRIZE_PAY_CHANNEL_ID(TRABUF)
        !-----------------------------------------------------------
        ! Prize pay mediator:
        ! We were getting SAP agent code, but it exceeds 7 digits;
        ! Likely is just agent code
        !REC_PRIZE_PAY_MEDIATOR   = GET_SAP_AGENT_CODE(TRABUF)
        REC_PRIZE_PAY_MEDIATOR   = GET_AGENT_CODE(TRABUF)
        !-----------------------------------------------------------
        ! Prize pay ref:
        REC_PRIZE_PAY_REF        = GET_PRIZE_PAY_REF(TRABUF)
        
        !-----------------------------------------------------------
        ! Prize pay date:
        CALL CDC_TO_DATE_ARR(TRABUF(TCDC)
     *        , DATE_ARR_TRX_GEN)
        
        REC_PRIZE_PAY_DATE(1)    = DATE_ARR_TRX_GEN(DATE_ARR_YEAR)
        REC_PRIZE_PAY_DATE(2)    = DATE_ARR_TRX_GEN(DATE_ARR_MONTH)
        REC_PRIZE_PAY_DATE(3)    = DATE_ARR_TRX_GEN(DATE_ARR_DAY)
        
        AUX = TRABUF(TTIM) 
        REC_PRIZE_PAY_DATE(4)    = AUX / 3600        ! Hour
        AUX = AUX - (REC_PRIZE_PAY_DATE(4) * 3600)
        REC_PRIZE_PAY_DATE(5)    = AUX / 60          ! Minute
        AUX = AUX - (REC_PRIZE_PAY_DATE(5) * 60)
        REC_PRIZE_PAY_DATE(6)    = AUX               ! Second
        
        !-----------------------------------------------------------
        ! Prize pay amount:
        REC_PRIZE_PAY_AMOUNT     = TRABUF(TVPAY) + TRABUF(TVKPAY)  
        
        !-----------------------------------------------------------
        ! Net prize pay amount:
        REC_NET_PRIZE_PAY_AMOUNT = TRABUF(TVOPPAY) + TRABUF(TVKOPPAY) 
        
        IF(REC_NET_PRIZE_PAY_AMOUNT .EQ. 0) THEN
            REC_NET_PRIZE_PAY_AMOUNT = REC_PRIZE_PAY_AMOUNT
        ENDIF
        !-----------------------------------------------------------
        ! Player phone nr: / Player card nr:
        IF(TRABUF(TVTYPE)   .EQ. VNBNK) THEN
            IF(TRABUF(TVPLCARD) .NE. 0) THEN
                IF(TRABUF(TVPLIDTYP) .EQ. 0) THEN
                    WRITE(REC_PLAYER_PHONE_NR, 19001) TRABUF(TVPLCARD)
                ELSE
                    WRITE(REC_PLAYER_CARD_NR, 19001) TRABUF(TVPLCARD)
                ENDIF
            ENDIF
        !-----------------------------------------------------------
        ! Player NIB:
            REC_PLAYER_NIB(1)        = TRABUF(TVNIBBB)
            REC_PLAYER_NIB(2)        = TRABUF(TVNIBBO)
            REC_PLAYER_NIB(3)        = TRABUF(TVNIBBA1)
            REC_PLAYER_NIB(4)        = TRABUF(TVNIBBA2)
            REC_PLAYER_NIB(5)        = TRABUF(TVNIBCD)
        ENDIF
        
        CALL PRINT_IBAN(REC_PLAYER_IBAN, 'PT50', REC_PLAYER_NIB)
            
        !-----------------------------------------------------------
        ! Record write
        !-----------------------------------------------------------
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_PAY_FILE),20101)
     *            '01'
     *           , REC_GAME_CODE
     *           , REC_INT_BET_REF
     *           , REC_EXT_BET_REF(1)
     *           , REC_EXT_BET_REF(2)
     *           , REC_EXT_BET_REF(3)
     *           , REC_PRIZE_PAY_MODE_ID
     *           , REC_PRIZE_PAY_CHANNEL_ID
     *           , REC_PRIZE_PAY_MEDIATOR
     *           , REC_PRIZE_PAY_REF
     *           , REC_PRIZE_PAY_DATE(1)
     *           , REC_PRIZE_PAY_DATE(2)
     *           , REC_PRIZE_PAY_DATE(3)
     *           , REC_PRIZE_PAY_DATE(4)
     *           , REC_PRIZE_PAY_DATE(5)
     *           , REC_PRIZE_PAY_DATE(6)
     *           , REC_PRIZE_PAY_AMOUNT
     *           , REC_NET_PRIZE_PAY_AMOUNT
     *           , REC_PLAYER_PHONE_NR
     *           , REC_PLAYER_CARD_NR
     *           , REC_PLAYER_IBAN
        
        !-----------------------------------------------------------
        ! Update counters
        !-----------------------------------------------------------
        SOUPFM_REC.LINE_CNT(IDX_AM_PAY_FILE) = 
     *          SOUPFM_REC.LINE_CNT(IDX_AM_PAY_FILE) + 1 

        !-----------------------------------------------------------
        ! Update counters
        !-----------------------------------------------------------
        SOUPFM_REC.TRX_CNT(IDX_AM_PAY_FILE) = 
     *          SOUPFM_REC.TRX_CNT(IDX_AM_PAY_FILE) + 1 

        RETURN
19001   FORMAT(I9.9)
20101   FORMAT(
     *            A2                    ! Record type
     *          , I2.2                  ! Game code
     *          , I13.13                ! Bet ref: Prize pay ref - Internal ref ?
     *          , I3.3                  ! Bet ref (1): Julian date
     *          , I8.8                  ! Bet ref (2): Serial Number
     *          , I3.3                  ! Bet ref (3): Control digits
     *          , A1                    ! Prize pay mode id
     *          , I1.1                  ! Prize pay channel id
     *          , I7.7                  ! Prize pay mediator
     *          , I13.13                ! Prize pay ref - Internal ref
     *          , I4.4                  ! Prize pay date (1): year
     *          , I2.2                  ! Prize pay date (2): month
     *          , I2.2                  ! Prize pay date (3): day
     *          , I2.2                  ! Prize pay date (4): hour
     *          , I2.2                  ! Prize pay date (5): minute
     *          , I2.2                  ! Prize pay date (6): second
     *          , I11.11                ! Prize pay amount (in cents)
     *          , I11.11                ! Net prize pay amount (in cents)
     *          , A9                    ! Player phone nr
     *          , A9                    ! Player card nr
     *          , A25                   ! IBAN
     *          )
20102   FORMAT(A132)
        END







        SUBROUTINE CLOSE_FILE_SOUPFM_AM_PAY(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4 ST
        
        INTEGER*4 YEAR,MONTH,DAY
        
        ST = 0
        
        IF(SOUPFM_REC.HANDLE_FILE(IDX_AM_PAY_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        
        CALL PRINT_TRAILER_SOUPFM_AM_PAY(SOUPFM_REC, ST)
        
        CALL USRCLOS1(SOUPFM_REC.LUN_TABLE(IDX_AM_PAY_FILE))
        
        CALL PRINT_REPORT_LINE(SOUPFM_REC, IDX_AM_PAY_FILE)

        ST = 0
        CALL CLOSE_KEY_FILE_SOUPFM_VLF(SOUPFM_REC, ST)

        RETURN
        END
        






        SUBROUTINE PRINT_TRAILER_SOUPFM_AM_PAY(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4 ST
        
        ST = 0
        
        ! Writing total number of records, including header and trailer
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_PAY_FILE),20201) 
     *            'TP'
     *          , SOUPFM_REC.LINE_CNT(IDX_AM_PAY_FILE) + 2 
        
        RETURN
20201   FORMAT(A2,I8.8,122(' '))
        END



