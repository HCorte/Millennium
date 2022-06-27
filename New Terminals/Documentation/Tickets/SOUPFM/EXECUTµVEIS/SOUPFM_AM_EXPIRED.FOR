C
C Module SOUPFM_AM_EXPIRED - SOUP File Management
C
C This program will manage the generation of the SOUP files
C for expired prizes of Apostas Mutuas (AM) 
C
C V02 23-MAR-2022 SCML Bug fix for SPORTS (GAME NUM=10) not generating reports with the 
C                      data of prizes purged (uncashed) - ticket(P220125_0011)
C V01 20-DEC-2013 SCML Program creation
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE OPEN_FILE_SOUPFM_AM_EXPIRED(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        
        INTEGER*4  ST
        INTEGER*4  RECORD_LEN
        PARAMETER (RECORD_LEN = 47)
        
        INTEGER*4 DATE_ARR(3)
        INTEGER*4 YEAR,MONTH,DAY
        EQUIVALENCE(DATE_ARR(DATE_ARR_YEAR),YEAR)
        EQUIVALENCE(DATE_ARR(DATE_ARR_MONTH),MONTH)
        EQUIVALENCE(DATE_ARR(DATE_ARR_DAY),DAY)
        
        ST = 0

        IF(SOUPFM_REC.HANDLE_FILE(IDX_AM_EXPIRED_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        
C        SOUPFM_REC.FILE_GEN_CDC(IDX_AM_EXPIRED_FILE) = DAYCDC
        SOUPFM_REC.FILE_DATA_CDC(IDX_AM_EXPIRED_FILE) = 
     *         SOUPFM_REC.SRC_CDC_DATE
        
        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_DATA_CDC(IDX_AM_EXPIRED_FILE)
     *        , DATE_ARR)
        
        CALL PRINT_FILE_NAME(INT_SRC_FILE_NAME
     *        , 'SOUP', 'AM', 'PC', YEAR, MONTH, DAY)
     
        CALL COPY_FROM_FNAME(SOUPFM_REC
     *        , IDX_AM_EXPIRED_FILE,INT_SRC_FILE_NAME)
        
        IF(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_FILE) .EQ. 0) THEN
            CALL RESERVE_AVAILABLE_LUN(SOUPFM_REC,IDX_AM_EXPIRED_FILE,ST)
            IF(ST .NE. 0) RETURN
        ENDIF
        
        ST = 0
        CALL ROPEN1(CHR_SRC_FILE_NAME
     *            , SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_FILE)
     *            , RECORD_LEN, ST)
        
        IF(ST .NE. 0) THEN
            CALL FILERR(INT_SRC_FILE_NAME
     *           , FUN_OPEN_FILE,ST,0)
            CALL USRCLOS1(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_FILE))
            RETURN
        ENDIF
        
        CALL PRINT_HEADER_SOUPFM_AM_EXPIRED(SOUPFM_REC, ST)
        IF(ST .NE. 0) THEN
            CALL FILERR(INT_SRC_FILE_NAME
     *           , FUN_WRITE_FILE,ST,0)
            RETURN
        ENDIF

        TYPE *, IAM(), 'Aberto ficheiro ',CHR_SRC_FILE_NAME
        
        RETURN
        END



        SUBROUTINE PRINT_HEADER_SOUPFM_AM_EXPIRED(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4 DATE_ARR_FILE_GEN(3)
        INTEGER*4 DATE_ARR_FILE_DATA(3)
        INTEGER*4 ST
        
        ST = 0
        
        CALL GET_SYS_DATE(DATE_ARR_FILE_GEN)
C        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_GEN_CDC(IDX_AM_EXPIRED_FILE)
C     *        , DATE_ARR_FILE_GEN)

        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_DATA_CDC(IDX_AM_EXPIRED_FILE)
     *        , DATE_ARR_FILE_DATA)

        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_FILE),20001) 
     *            'HP'
     *          ,   DATE_ARR_FILE_DATA(DATE_ARR_YEAR)  * 10000 
     *            + DATE_ARR_FILE_DATA(DATE_ARR_MONTH) * 100
     *            + DATE_ARR_FILE_DATA(DATE_ARR_DAY)   * 1
     *          ,   DATE_ARR_FILE_GEN(DATE_ARR_YEAR)   * 10000 
     *            + DATE_ARR_FILE_GEN(DATE_ARR_MONTH)  * 100
     *            + DATE_ARR_FILE_GEN(DATE_ARR_DAY)    * 1
        
        RETURN
20001   FORMAT(A2,I8.8,I8.8)
        END





        SUBROUTINE HANDLE_VALIDATION_SOUPFM_AM_EXPIRED(VALREC, SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:PRMVPF.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'

        INTEGER*4    ST, AUX
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


        ST = 0
        
        IF(SOUPFM_REC.HANDLE_FILE(IDX_AM_EXPIRED_FILE) .EQ. .FALSE.) THEN
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
        IF( (GAME .NE. 10) .AND. (VALREC(VWCDC)   .GT. SOUPFM_REC.SRC_CDC_DATE - NUMDAY
     *  .OR.  NUMDAY          .EQ. 0) ) RETURN

        ! 6) only validations that do not have payment orders
        IF(   VALREC(VOPSCNT) .NE. 0) RETURN
        
        IF(ST .NE. 0) RETURN
C        TYPE *, '**** ACCEPTED! *****'

        ! Record initialization
        REC_GAME_CODE            = 0
        REC_INT_BET_REF          = 0
        
        REC_EXT_BET_REF(1)       = 0
        REC_EXT_BET_REF(2)       = 0
        REC_EXT_BET_REF(3)       = 0
        
        ST                       = 0

        !-----------------------------------------------------------
        ! Record fill
        !-----------------------------------------------------------
        ! Game code:
        REC_GAME_CODE            = VALREC(VGAM)
            
        !-----------------------------------------------------------
        ! Internal bet ref:
        REC_INT_BET_REF          = GET_BET_REF_FROM_VALREC(VALREC)
        
        !-----------------------------------------------------------
        ! Record write
        !-----------------------------------------------------------
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_FILE),20101)
     *            '01'
     *           , REC_GAME_CODE
     *           , REC_INT_BET_REF
        
        !-----------------------------------------------------------
        ! Update counters
        !-----------------------------------------------------------
        SOUPFM_REC.LINE_CNT(IDX_AM_EXPIRED_FILE) = 
     *          SOUPFM_REC.LINE_CNT(IDX_AM_EXPIRED_FILE) + 1 

        !-----------------------------------------------------------
        ! Update counters
        !-----------------------------------------------------------
        SOUPFM_REC.TRX_CNT(IDX_AM_EXPIRED_FILE) = 
     *          SOUPFM_REC.TRX_CNT(IDX_AM_EXPIRED_FILE) + 1 

        RETURN
19001   FORMAT(I9.9)
20101   FORMAT(
     *            A2                    ! Record type
     *          , I2.2                  ! Game code
     *          , I13.13                ! Bet ref: Prize pay ref - Internal ref ?
     *          , 1(' ')                ! Reserved to the future
     *          )
20102   FORMAT(A132)
        END







        SUBROUTINE CLOSE_FILE_SOUPFM_AM_EXPIRED(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4 ST
        
        INTEGER*4 YEAR,MONTH,DAY
        
        ST = 0
        
        IF(SOUPFM_REC.HANDLE_FILE(IDX_AM_EXPIRED_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        
        CALL PRINT_TRAILER_SOUPFM_AM_EXPIRED(SOUPFM_REC, ST)
        
        CALL USRCLOS1(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_FILE))
        
        CALL PRINT_REPORT_LINE(SOUPFM_REC, IDX_AM_EXPIRED_FILE)
        
        RETURN
        END
        






        SUBROUTINE PRINT_TRAILER_SOUPFM_AM_EXPIRED(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4 ST
        
        ST = 0
        
        ! Writing total number of records, including header and trailer
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_AM_EXPIRED_FILE),20201) 
     *            'TP'
     *          , SOUPFM_REC.LINE_CNT(IDX_AM_EXPIRED_FILE) + 2 
        
        RETURN
20201   FORMAT(A2,I8.8,8(' '))
        END



