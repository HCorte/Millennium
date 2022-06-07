C
C Module SOUPFM_REL_AM_EXPIRED - SOUP File Management
C
C This program will manage the generation of the SOUP files
C for expired prizes report of Apostas Mutuas (AM) 
C
C V02 08-APR-2022 SCML Bug fix for SPORTS (GAME NUM=10) not generating reports with the 
C                      data of prizes purged (uncashed) - ticket(P220125_0011)  
C V01 09-APR-2014 SCML Program creation
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE OPEN_FILE_SOUPFM_REL_AM_EXPIRED(SOUPFM_REC, ST)
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

        IF(SOUPFM_REC.HANDLE_FILE(IDX_AM_EXPIRED_REP_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        
C        SOUPFM_REC.FILE_GEN_CDC(IDX_AM_EXPIRED_REP_FILE) = DAYCDC
        SOUPFM_REC.FILE_DATA_CDC(IDX_AM_EXPIRED_REP_FILE) = 
     *         SOUPFM_REC.SRC_CDC_DATE
        
        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_DATA_CDC(IDX_AM_EXPIRED_REP_FILE)
     *        , DATE_ARR)
        
        CALL PRINT_FILE_NAME_GEN(INT_SRC_FILE_NAME
     *        , 'REL', 'AM', 'PC', YEAR, MONTH, DAY, 'REP')
     
        CALL COPY_FROM_FNAME(SOUPFM_REC
     *        , IDX_AM_EXPIRED_REP_FILE,INT_SRC_FILE_NAME)
        
        IF(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE) .EQ. 0) THEN
            CALL RESERVE_AVAILABLE_LUN(SOUPFM_REC,IDX_AM_EXPIRED_REP_FILE,ST)
            IF(ST .NE. 0) RETURN
        ENDIF
        
        ST = 0
        CALL ROPEN1(CHR_SRC_FILE_NAME
     *            , SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE)
     *            , RECORD_LEN, ST)
        
        IF(ST .NE. 0) THEN
            CALL FILERR(INT_SRC_FILE_NAME
     *           , FUN_OPEN_FILE,ST,0)
            CALL USRCLOS1(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE))
            RETURN
        ENDIF
        
        CALL PRINT_HEADER_SOUPFM_REL_AM_EXPIRED(SOUPFM_REC, ST)
        IF(ST .NE. 0) THEN
            CALL FILERR(INT_SRC_FILE_NAME
     *           , FUN_WRITE_FILE,ST,0)
            RETURN
        ENDIF

        TYPE *, IAM(), 'Aberto ficheiro ',CHR_SRC_FILE_NAME
        
        RETURN
        END



        SUBROUTINE PRINT_HEADER_SOUPFM_REL_AM_EXPIRED(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4 DATE_ARR_FILE_GEN(3)
        INTEGER*4 DATE_ARR_FILE_DATA(3)
        INTEGER*4 ST
        
        ST = 0
        
        CALL GET_SYS_DATE(DATE_ARR_FILE_GEN)
C        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_GEN_CDC(IDX_AM_EXPIRED_REP_FILE)
C     *        , DATE_ARR_FILE_GEN)

        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_DATA_CDC(IDX_AM_EXPIRED_REP_FILE)
     *        , DATE_ARR_FILE_DATA)
C
C Format statements for header
C 
20000   FORMAT(' ',130('='))
20001   FORMAT(' SCML - Departamento de Jogos              '
     *       , 'RELATORIO DE PREMIOS CADUCADOS DE APOSTAS MUTUAS'
     *       , '                        Data: ',I2.2,'.',I2.2,'.',I4.4)
20002   FORMAT('     CODIGO APOSTA    N. MAQUINA          NOME JOGO'
     *       , '      SORTEIO      DATA SORT   DATA CADUC'
     *       , '                         VALOR PREMIO')
20003   FORMAT(' ',130('-'))
20004   FORMAT(\)

        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE),20000)

        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE),20001)
     *              DATE_ARR_FILE_GEN(DATE_ARR_DAY)
     *          ,   DATE_ARR_FILE_GEN(DATE_ARR_MONTH)
     *          ,   DATE_ARR_FILE_GEN(DATE_ARR_YEAR)
C    *              DATE_ARR_FILE_GEN(DATE_ARR_DAY)
C    *          ,   DATE_ARR_FILE_GEN(DATE_ARR_MONTH)
C    *          ,   DATE_ARR_FILE_GEN(DATE_ARR_YEAR)

        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE),20000)

        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE),20002)

        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE),20003)
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE),20004)

        RETURN
        END





        SUBROUTINE HANDLE_VALIDATION_SOUPFM_REL_AM_EXPIRED(VALREC, SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:PRMVPF.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'

        INTEGER*4    ST, AUX, K, I
        INTEGER*4    GAME, NUMDAY
        INTEGER*4    DATE_ARR_TRX_GEN(3)

        ! Required variables to print
        INTEGER*4    REC_GAME_CODE
        INTEGER*8    REC_INT_BET_REF
        INTEGER*4    REC_EXT_BET_REF(3)
        INTEGER*4    REC_PRIZE_PAY_MODE_ID
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
        CHARACTER*8  REC_OFFLINE_MEDIATOR_ID
        CHARACTER*16 REC_EXT_BET_REF_LINE
        CHARACTER*16 REC_GAME_NAME
        INTEGER*4    DRAW_NR
        INTEGER*4    REC_DRAW_NR
        INTEGER*4    REC_DRAW_WEEK
        INTEGER*4    REC_DRAW_YEAR
        INTEGER*4    REC_DRAW_DATE
        INTEGER*4    REC_DRAW_DATE_YEAR
        INTEGER*4    REC_DRAW_DATE_MONTH
        INTEGER*4    REC_DRAW_DATE_DAY
        INTEGER*4    REC_DRAW_PURGE_DATE
        INTEGER*4    REC_DRAW_PURGE_DATE_YEAR
        INTEGER*4    REC_DRAW_PURGE_DATE_MONTH
        INTEGER*4    REC_DRAW_PURGE_DATE_DAY
        CHARACTER*32 REC_PRIZE_AMOUNT_FORMATTED
        LOGICAL      VALID_DRAW

        INTEGER*4 DATE_ARR(3)
        INTEGER*4 YEAR,MONTH,DAY
        EQUIVALENCE(DATE_ARR(DATE_ARR_YEAR),YEAR)
        EQUIVALENCE(DATE_ARR(DATE_ARR_MONTH),MONTH)
        EQUIVALENCE(DATE_ARR(DATE_ARR_DAY),DAY)

        ST = 0
        
        IF(SOUPFM_REC.HANDLE_FILE(IDX_AM_EXPIRED_REP_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        

C        CALL GET_BET_EXTERNAL_REF_FROM_VALREC(VALREC,REC_EXT_BET_REF)
C        TYPE *, '=============================================='
C        TYPE *, SOUPFM_REC.TOTAL_VAL_CNT, ': IBREF = ', GET_BET_REF_FROM_VALREC(VALREC)
C        TYPE *, SOUPFM_REC.TOTAL_VAL_CNT, ': EBREF = ', REC_EXT_BET_REF(1),REC_EXT_BET_REF(2),REC_EXT_BET_REF(3)
C        TYPE *, '----------------------------------------------'
C        TYPE *, 'VALREC(VSTAT)   = ',VALREC(VSTAT) 
C        TYPE *, '         VUNCSH = ',VUNCSH 
C        TYPE *, '         VPRPAY = ',VPRPAY
C        TYPE *, '----------------------------------------------'
C        TYPE *, 'VALREC(VGTYP)   = ',VALREC(VGTYP) 
C        TYPE *, '           TLTO = ',TLTO 
C        TYPE *, '           TSPT = ',TSPT 
C        TYPE *, '           TKIK = ',TKIK
C        TYPE *, '----------------------------------------------'
C        TYPE *, 'VALREC(VGAM)    = ',VALREC(VGAM) 
C        TYPE *, '----------------------------------------------'
C        TYPE *, 'VALREC(VSSER)   = ',VALREC(VSSER) 
C        TYPE *, 'VALREC(VPAMT)   = ',VALREC(VPAMT) 
C        TYPE *, 'VALREC(VKPAMT)  = ',VALREC(VKPAMT) 
C        TYPE *, 'VALREC(VRAMT)   = ',VALREC(VRAMT) 
C        TYPE *, 'VALREC(VOPSCNT) = ',VALREC(VOPSCNT) 
C        TYPE *, '----------------------------------------------'
C        TYPE *, 'VALREC(VWCDC)   = ',VALREC(VWCDC) 
C        TYPE *, '   PRGDAY(GAME) = ',PRGDAY(VALREC(VGAM)) 
C        TYPE *, '        SRC_CDC = ',SOUPFM_REC.SRC_CDC_DATE 
C        TYPE *, '----------------------------------------------'
        !-----------
        ! Handling :
        !-----------
        ! 1) only uncashed and privileged pay validations
        IF(   VALREC(VSTAT)   .NE. VUNCSH
     *  .AND. VALREC(VSTAT)   .NE. VPRPAY) RETURN

        ! 2) only mutual games' bets validations
        IF(   VALREC(VGTYP)   .NE. TLTO
     *  .AND. VALREC(VGTYP)   .NE. TKIK
     *  .AND. VALREC(VGTYP)   .NE. TSPT) RETURN

        ! 3) only validations with serial number
        IF(   VALREC(VSSER)   .EQ. 0) RETURN

        ! 4) only validations with some prize or refund amount
        IF(   VALREC(VPAMT)   .EQ. 0
     *  .AND. VALREC(VKPAMT)  .EQ. 0
     *  .AND. VALREC(VRAMT)   .EQ. 0) RETURN

        ! 5) only validations that have been purged at the provided cdc date
        GAME    = VALREC(VGAM)
        NUMDAY  = PRGDAY(GAME)
C        IF(   VALREC(VWCDC)   .GT. SOUPFM_REC.SRC_CDC_DATE - NUMDAY !V02
C     *  .OR.  NUMDAY          .EQ. 0) RETURN                        !V02
        IF( (GAME .NE. 10) .AND. (VALREC(VWCDC)   .GT. SOUPFM_REC.SRC_CDC_DATE - NUMDAY !V02
     *  .OR.  NUMDAY          .EQ. 0) ) RETURN                                          !V02

        ! 6) only validations that do not have payment orders
        IF(   VALREC(VOPSCNT) .NE. 0) RETURN
        
C       ! 7) only prizes above EUR 150
C       IF((VALREC(VPAMT)+VALREC(VKPAMT)-VALREC(VRAMT)) .LT. P(VALORDER)) THEN
C           RETURN
C       ENDIF
        
        IF(ST .NE. 0) RETURN
C        TYPE *, '**** ACCEPTED! *****'

        ! Getting detail
        CALL DLOGVAL(VALREC,VDETAIL)
        
        ! Record initialization
        REC_GAME_CODE            = 0
        REC_INT_BET_REF          = 0
        
        REC_EXT_BET_REF(1)       = 0
        REC_EXT_BET_REF(2)       = 0
        REC_EXT_BET_REF(3)       = 0
        
        REC_OFFLINE_MEDIATOR_ID  = '       '
        REC_GAME_NAME            = '                '
        REC_DRAW_NR              = 0
        REC_DRAW_WEEK            = 0
        REC_DRAW_YEAR            = 0
        ST                       = 0

        !-----------------------------------------------------------
        ! Record fill
        !-----------------------------------------------------------
        ! Game code:
        REC_GAME_CODE            = VALREC(VGAM)
        !-----------------------------------------------------------
        ! External bet ref:
        CALL GET_BET_EXTERNAL_REF_FROM_VALREC(VALREC,REC_EXT_BET_REF)
        CALL PRINT_EXTERNAL_BET_REF(REC_EXT_BET_REF_LINE,REC_EXT_BET_REF,.TRUE.)
        
        !-----------------------------------------------------------
        ! Agent nr:
        CALL PRINT_MEDIATOR_REP(REC_OFFLINE_MEDIATOR_ID
     *          , AGTTAB(AGTNUM,VALREC(VSTER)))
        
        !-----------------------------------------------------------
        ! Game name:
20100   FORMAT(4A4)
        WRITE(REC_GAME_NAME,20100)
     *            (GLNAMES(K,REC_GAME_CODE),K=1,4)
        !-----------------------------------------------------------
        ! Draw number:
        VALID_DRAW = .TRUE.
        I = 1
        DRAW_NR = VDETAIL(VDRW,1)
        DO WHILE(I .LE. VALREC(VPZOFF) .AND. VALID_DRAW .EQ. .TRUE.)
            REC_DRAW_NR = VDETAIL(VDRW,I)
            IF(REC_DRAW_NR .NE.
     *          DRAW_NR) THEN
               VALID_DRAW = .FALSE.
            ENDIF
            I = I + 1
        ENDDO
        
        IF(VALID_DRAW) THEN
            ! Here we only have draw nr
            CALL GETWEK(
     *                 DRAW_NR
     *               , REC_GAME_CODE
     *               , REC_DRAW_WEEK
     *               , REC_DRAW_YEAR
     *               , ST
     *           )
            IF(ST .NE. 0) THEN
                TYPE *, IAM(), 'ERROR WHILE CONVERTING DRAW #' 
     *                , REC_DRAW_NR
     *                , ' TO WEEK/YEAR'
                CALL GPAUSE
                ST = 0
            ENDIF
        ENDIF
        
        ST = 0
        
        SOUPFM_REC.GAME_DATA_REC.IN_GAME_NUM = REC_GAME_CODE
        SOUPFM_REC.GAME_DATA_REC.IN_DRAW_NUM = REC_DRAW_NR
        
        CALL READ_GAME_DATA(SOUPFM_REC, ST)
        IF(ST .NE. 0) THEN
            TYPE *, IAM(), 'ERROR WHILE READING DATA FOR GAME '
     *            , REC_GAME_CODE, ' DRAW #' 
     *            , REC_DRAW_NR
            CALL GPAUSE
            ST = 0
        ENDIF
        
        CALL CDC_TO_DATE_ARR(SOUPFM_REC.GAME_DATA_REC.OUT_DRAW_DATE_CDC
     *        , DATE_ARR)
        
        REC_DRAW_DATE_YEAR  = YEAR
        REC_DRAW_DATE_MONTH = MONTH
        REC_DRAW_DATE_DAY   = DAY
        
        REC_DRAW_PURGE_DATE = VALREC(VWCDC) + NUMDAY
        CALL CDC_TO_DATE_ARR(REC_DRAW_PURGE_DATE
     *        , DATE_ARR)
        REC_DRAW_PURGE_DATE_YEAR  = YEAR
        REC_DRAW_PURGE_DATE_MONTH = MONTH
        REC_DRAW_PURGE_DATE_DAY   = DAY

        REC_PRIZE_PAY_AMOUNT = KZEXT(VALREC(VPAMT)+VALREC(VKPAMT)-VALREC(VRAMT))
        
        REC_PRIZE_AMOUNT_FORMATTED = SOUPFM_DMONYI8(REC_PRIZE_PAY_AMOUNT)
        
        !-----------------------------------------------------------
        ! Record write
        !-----------------------------------------------------------
20101   FORMAT(
     *            '  '
     *          , A16                          ! Bet code
     *          , '      '
     *          , A8                           ! Agent code
     *          , '    '
     *          , A16                          ! Game name
     *          , '    '
     *          , I3.3, '/', I4.4              ! Draw name
     *          , '     '
     *          , I2.2, '.', I2.2, '.', I4.4   ! Draw date
     *          , '   '
     *          , I2.2, '.', I2.2, '.', I4.4   ! Draw purge date
     *          , 24(' ')
     *          , A13                          ! Prize amount
     *          )
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE),20101)
     *        REC_EXT_BET_REF_LINE
     *    ,   REC_OFFLINE_MEDIATOR_ID
     *    ,   REC_GAME_NAME
     *    ,   REC_DRAW_WEEK, REC_DRAW_YEAR
     *    ,   REC_DRAW_DATE_DAY, REC_DRAW_DATE_MONTH, REC_DRAW_DATE_YEAR
     *    ,   REC_DRAW_PURGE_DATE_DAY, REC_DRAW_PURGE_DATE_MONTH, REC_DRAW_PURGE_DATE_YEAR
     *    ,   REC_PRIZE_AMOUNT_FORMATTED
        !-----------------------------------------------------------
        ! Update counters
        !-----------------------------------------------------------
        SOUPFM_REC.EXP_STAT_REC.NR_BETS(
     *      IDX_AM_EXPIRED_REP_FILE
     *    , REC_GAME_CODE) = 
     *      SOUPFM_REC.EXP_STAT_REC.NR_BETS(
     *          IDX_AM_EXPIRED_REP_FILE
     *        , REC_GAME_CODE) + 1

        SOUPFM_REC.EXP_STAT_REC.AMOUNT(
     *      IDX_AM_EXPIRED_REP_FILE
     *    , REC_GAME_CODE) = 
     *      SOUPFM_REC.EXP_STAT_REC.AMOUNT(
     *          IDX_AM_EXPIRED_REP_FILE
     *        , REC_GAME_CODE) + REC_PRIZE_PAY_AMOUNT

        SOUPFM_REC.EXP_STAT_REC.TOTAL_NR_BETS(IDX_AM_EXPIRED_REP_FILE) = 
     *      SOUPFM_REC.EXP_STAT_REC.TOTAL_NR_BETS(IDX_AM_EXPIRED_REP_FILE) + 1

        SOUPFM_REC.EXP_STAT_REC.TOTAL_AMOUNT(IDX_AM_EXPIRED_REP_FILE) = 
     *      SOUPFM_REC.EXP_STAT_REC.TOTAL_AMOUNT(IDX_AM_EXPIRED_REP_FILE) + 
     *      REC_PRIZE_PAY_AMOUNT
        !-----------------------------------------------------------
        ! Update counters
        !-----------------------------------------------------------
        SOUPFM_REC.LINE_CNT(IDX_AM_EXPIRED_REP_FILE) = 
     *          SOUPFM_REC.LINE_CNT(IDX_AM_EXPIRED_REP_FILE) + 1 

        !-----------------------------------------------------------
        ! Update counters
        !-----------------------------------------------------------
        SOUPFM_REC.TRX_CNT(IDX_AM_EXPIRED_REP_FILE) = 
     *          SOUPFM_REC.TRX_CNT(IDX_AM_EXPIRED_REP_FILE) + 1 

        RETURN
        END







        SUBROUTINE CLOSE_FILE_SOUPFM_REL_AM_EXPIRED(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4 ST
        
        INTEGER*4 YEAR,MONTH,DAY
        
        ST = 0
        
        IF(SOUPFM_REC.HANDLE_FILE(IDX_AM_EXPIRED_REP_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        
        CALL PRINT_TRAILER_SOUPFM_REL_AM_EXPIRED(SOUPFM_REC, ST)
        
        CALL USRCLOS1(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE))
        
        CALL PRINT_REPORT_LINE(SOUPFM_REC, IDX_AM_EXPIRED_REP_FILE)
        
        RETURN
        END
        






        SUBROUTINE PRINT_TRAILER_SOUPFM_REL_AM_EXPIRED(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4 ST, I, K, GTYP
        CHARACTER*16 REC_GAME_NAME
        
        ST = 0

20200   FORMAT(\)
20201   FORMAT(' ',130('-'))
20202   FORMAT(80(' ')
     *       , 'JOGO                     APOSTAS          PREMIOS')
20203   FORMAT(78(' '),53('-'))
20204   FORMAT(80(' ')    ! Line
     *       , A16, 7(' '), I9, 4(' '), A13)
20205   FORMAT(80(' ')    ! Total
     *       , 'TOTAL           ', 7(' '), I9, 4(' '), A13)

        ! Writing trailer and final digest report
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE),20200)
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE),20201)
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE),20200)
        
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE),20202)
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE),20203)

        DO I = 1, MAXGAM
            GTYP = GNTTAB(GAMTYP,I)
            
            IF(  GTYP .EQ. TLTO
     *      .OR. GTYP .EQ. TKIK
     *      .OR. GTYP .EQ. TSPT ) THEN

                IF(   SOUPFM_REC.EXP_STAT_REC.NR_BETS(IDX_AM_EXPIRED_REP_FILE,I) .NE. 0
     *          .AND. SOUPFM_REC.EXP_STAT_REC.AMOUNT(IDX_AM_EXPIRED_REP_FILE,I) .NE. KZEXT(0)) THEN
20100               FORMAT(4A4)
                    WRITE(REC_GAME_NAME,20100)
     *                        (GLNAMES(K,I),K=1,4)
                    WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE),20204)
     *                  , REC_GAME_NAME
     *                  , SOUPFM_REC.EXP_STAT_REC.NR_BETS(IDX_AM_EXPIRED_REP_FILE,I)
     *                  , SOUPFM_DMONYI8(SOUPFM_REC.EXP_STAT_REC.AMOUNT(IDX_AM_EXPIRED_REP_FILE,I))
                ENDIF

            ENDIF
        ENDDO

        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE),20203)
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_REP_FILE),20205)
     *        SOUPFM_REC.EXP_STAT_REC.TOTAL_NR_BETS(IDX_AM_EXPIRED_REP_FILE)
     *      , SOUPFM_DMONYI8(SOUPFM_REC.EXP_STAT_REC.TOTAL_AMOUNT(IDX_AM_EXPIRED_REP_FILE))
        
        RETURN
        END



