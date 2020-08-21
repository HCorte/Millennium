C
C Module SOUPFM_LN_PAY - SOUP File Management
C
C This program will manage the generation of the SOUP files
C for payed prizes of Lotaria Nacional (LN) 
C
C V01 12-DEC-2013 SCML Program creation
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE OPEN_FILE_SOUPFM_LN_PAY(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        
        INTEGER*4  ST
        INTEGER*4  RECORD_LEN
C       PARAMETER (RECORD_LEN = 135) ! Print draw nr
        PARAMETER (RECORD_LEN = 131)
        
        INTEGER*4 DATE_ARR(3)
        INTEGER*4 YEAR,MONTH,DAY
        EQUIVALENCE(DATE_ARR(DATE_ARR_YEAR),YEAR)
        EQUIVALENCE(DATE_ARR(DATE_ARR_MONTH),MONTH)
        EQUIVALENCE(DATE_ARR(DATE_ARR_DAY),DAY)
        
        ST = 0

        IF(SOUPFM_REC.HANDLE_FILE(IDX_LN_PAY_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        
C        SOUPFM_REC.FILE_GEN_CDC(IDX_LN_PAY_FILE) = DAYCDC
        SOUPFM_REC.FILE_DATA_CDC(IDX_LN_PAY_FILE) = 
     *         SOUPFM_REC.SRC_CDC_DATE
        
        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_DATA_CDC(IDX_LN_PAY_FILE)
     *        , DATE_ARR)
        
        CALL PRINT_FILE_NAME(INT_SRC_FILE_NAME
     *        , 'SOUP', 'LN', 'PP', YEAR, MONTH, DAY)
     
        CALL COPY_FROM_FNAME(SOUPFM_REC
     *        , IDX_LN_PAY_FILE,INT_SRC_FILE_NAME)
        
        IF(SOUPFM_REC.LUN_TABLE(IDX_LN_PAY_FILE) .EQ. 0) THEN
            CALL RESERVE_AVAILABLE_LUN(SOUPFM_REC,IDX_LN_PAY_FILE,ST)
            IF(ST .NE. 0) RETURN
        ENDIF
        
        ST = 0
        CALL ROPEN1(CHR_SRC_FILE_NAME
     *            , SOUPFM_REC.LUN_TABLE(IDX_LN_PAY_FILE)
     *            , RECORD_LEN, ST)
        
        IF(ST .NE. 0) THEN
            CALL FILERR(INT_SRC_FILE_NAME
     *           , FUN_OPEN_FILE,ST,0)
            CALL USRCLOS1(SOUPFM_REC.LUN_TABLE(IDX_LN_PAY_FILE))
            RETURN
        ENDIF
        
        CALL PRINT_HEADER_SOUPFM_LN_PAY(SOUPFM_REC, ST)
        IF(ST .NE. 0) THEN
            CALL FILERR(INT_SRC_FILE_NAME
     *           , FUN_WRITE_FILE,ST,0)
            RETURN
        ENDIF

        TYPE *, IAM(), 'Aberto ficheiro ',CHR_SRC_FILE_NAME
        
        RETURN
        END



        SUBROUTINE PRINT_HEADER_SOUPFM_LN_PAY(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4 DATE_ARR_FILE_GEN(3)
        INTEGER*4 DATE_ARR_FILE_DATA(3)
        INTEGER*4 ST
        
        ST = 0
        
        CALL GET_SYS_DATE(DATE_ARR_FILE_GEN)
C        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_GEN_CDC(IDX_LN_PAY_FILE)
C     *        , DATE_ARR_FILE_GEN)

        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_DATA_CDC(IDX_LN_PAY_FILE)
     *        , DATE_ARR_FILE_DATA)

        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_PAY_FILE),20001) 
     *            'HP'
     *          ,   DATE_ARR_FILE_DATA(DATE_ARR_YEAR)  * 10000 
     *            + DATE_ARR_FILE_DATA(DATE_ARR_MONTH) * 100
     *            + DATE_ARR_FILE_DATA(DATE_ARR_DAY)   * 1
     *          ,   DATE_ARR_FILE_GEN(DATE_ARR_YEAR)   * 10000 
     *            + DATE_ARR_FILE_GEN(DATE_ARR_MONTH)  * 100
     *            + DATE_ARR_FILE_GEN(DATE_ARR_DAY)    * 1
        
        RETURN
20001   FORMAT(A2,I8.8,I8.8,113(' '))
C20001   FORMAT(A2,I8.8,I8.8,117(' ')) ! Print draw nr
        END





        SUBROUTINE HANDLE_TRANSACTION_SOUPFM_LN_PAY(TRABUF, SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'

        INTEGER*4    ST, AUX, TCKS
        INTEGER*4    DATE_ARR_TRX_GEN(3)
        INTEGER*4    WEEK, YEAR
        
        ! Required variables to print
        INTEGER*4    REC_GAME_CODE
        INTEGER*4    REC_EXTRACTION_NAME
        INTEGER*4    REC_TICKET
        INTEGER*4    REC_TICKET_SERIES
        INTEGER*4    REC_TICKET_FRACTION
        CHARACTER*1  REC_PRIZE_PAY_MODE_ID
        INTEGER*4    REC_PRIZE_PAY_CHANNEL_ID
        CHARACTER*4  REC_TICKET_STATUS
        INTEGER*4    REC_PRIZE_PAY_MEDIATOR
        INTEGER*8    REC_PRIZE_PAY_REF
        INTEGER*4    REC_PRIZE_PAY_DATE(6)
        INTEGER*8    REC_PRIZE_PAY_AMOUNT         !in cents
        INTEGER*8    REC_NET_PRIZE_PAY_AMOUNT     !in cents
        CHARACTER*7  REC_OFFLINE_MEDIATOR_ID
        CHARACTER*9  REC_PLAYER_PHONE_NR
        CHARACTER*9  REC_PLAYER_CARD_NR
        CHARACTER*25 REC_PLAYER_IBAN
        INTEGER*4    REC_PLAYER_NIB(5)

        ST = 0
        
        IF(SOUPFM_REC.HANDLE_FILE(IDX_LN_PAY_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        

        !-----------
        ! Handling :
        !-----------
        ! 0) only good transactions 
        IF(TRABUF(TSTAT)   .NE. GOOD) RETURN
        ! 1) only validation type transactions 
        IF(TRABUF(TTYP)    .NE. TVAL) RETURN
        ! 2) only passive game transactions  
        IF(TRABUF(TGAMTYP) .NE. TPAS) RETURN
        ! 3) only transactions without error  
        IF(TRABUF(TERR)    .NE. NOER) RETURN

        IF(ST .NE. 0) RETURN


        DO TCKS = 1, TRABUF(TPTCK)
            IF(TRABUF(TPSTS1+OFFTRA*(TCKS-1)) .EQ. VWINNER     ) THEN

            ! Record initialization
            REC_GAME_CODE            = 0
            REC_EXTRACTION_NAME      = 0
            REC_TICKET               = 0
            REC_TICKET_SERIES        = 0
            REC_TICKET_FRACTION      = 0
            REC_PRIZE_PAY_MODE_ID    = ' '
            REC_PRIZE_PAY_CHANNEL_ID = 0
            REC_TICKET_STATUS        = '    '
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
            REC_OFFLINE_MEDIATOR_ID  = '       '
            REC_PLAYER_PHONE_NR      = '         '
            REC_PLAYER_CARD_NR       = '         '
            
            REC_PLAYER_IBAN          = '                         '
            REC_PLAYER_NIB(1)        = 0
            REC_PLAYER_NIB(2)        = 0
            REC_PLAYER_NIB(3)        = 0
            REC_PLAYER_NIB(4)        = 0
            REC_PLAYER_NIB(5)        = 0
            
            WEEK                     = 0
            YEAR                     = 0
            ST                       = 0
            !-----------------------------------------------------------
            ! Record fill
            !-----------------------------------------------------------
            ! Game code:
            REC_GAME_CODE            = TRABUF(TGAM)
            ! If new passive validation layout
            !-----------------------------------------------------------
            ! Extraction name:
C            IF(TRABUF(TVEPVAL).EQ.1) THEN
C               WEEK = MOD(TRABUF(TVEPWK),100)
C               YEAR = MOD(TRABUF(TVEPYR),10000)
C            ELSE
               ! Here we only have draw nr
               CALL GETWEK(
     *                    TRABUF(TPEMIS1 + OFFTRA*(TCKS-1))
     *                  , REC_GAME_CODE
     *                  , WEEK
     *                  , YEAR
     *                  , ST
     *              )
               IF(ST .NE. 0) THEN
                   TYPE *, IAM(), 'ERROR WHILE CONVERTING DRAW #' 
     *                   , TRABUF(TPEMIS1 + OFFTRA*(TCKS-1))
     *                   , ' TO WEEK/YEAR'
                   ST = 0
                   CALL GPAUSE
               ENDIF

               WEEK = MOD(WEEK,100)
               YEAR = MOD(YEAR,10000)
C            ENDIF           
               
            ! Convert this to EEAAAA
            REC_EXTRACTION_NAME      =
     *         WEEK * 10000
     *       + YEAR 
            !-----------------------------------------------------------
            ! Ticket:
            REC_TICKET               = TRABUF(TPNUM1  + OFFTRA*(TCKS-1))
            !-----------------------------------------------------------
            ! Series:
            REC_TICKET_SERIES        = TRABUF(TPSER1  + OFFTRA*(TCKS-1))
            !-----------------------------------------------------------
            ! Fraction:
            REC_TICKET_FRACTION      = TRABUF(TPTEN1  + OFFTRA*(TCKS-1))
            
            !-----------------------------------------------------------
            ! Prize pay mode: (in Millennium, we only handle two kinds)
            IF(    TRABUF(TVTYPE)   .EQ. VPNBNK
     *      .AND. (TRABUF(TVNIBBB)  .NE. 0
     *      .OR.   TRABUF(TVNIBBO)  .NE. 0
     *      .OR.   TRABUF(TVNIBBA1) .NE. 0
     *      .OR.   TRABUF(TVNIBBA2) .NE. 0
     *      .OR.   TRABUF(TVNIBCD)  .NE. 0)
     *      ) THEN
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
            ! Prize pay amount:
            REC_PRIZE_PAY_AMOUNT     = TRABUF(TPPAY1+OFFTRA*(TCKS-1))  
            !-----------------------------------------------------------
            ! Ticket status:
            IF(REC_PRIZE_PAY_MODE_ID .EQ. PRIZE_PAY_MODE_CASH_CARD) THEN
                REC_TICKET_STATUS        = 'CASH'
            ELSE IF(REC_PRIZE_PAY_MODE_ID .EQ. PRIZE_PAY_MODE_PORTAL_UNDEF) THEN
                REC_TICKET_STATUS        = 'CASH'
            ELSE
                REC_TICKET_STATUS        = 'BANK'
            ENDIF
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
     *            , DATE_ARR_TRX_GEN)
            
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
            ! Net prize pay amount:
            IF(TRABUF(TVOPPAY) .EQ. 0) THEN
                REC_NET_PRIZE_PAY_AMOUNT = REC_PRIZE_PAY_AMOUNT
            ELSE
                REC_NET_PRIZE_PAY_AMOUNT = TRABUF(TVOPPAY)
            ENDIF
            !-----------------------------------------------------------
            ! Offline mediator id:
            !REC_OFFLINE_MEDIATOR_ID  = TRABUF(TPOFFTER)
            IF(TRABUF(TPOFFTER) .GT. 0) THEN
                CALL PRINT_MEDIATOR(REC_OFFLINE_MEDIATOR_ID
     *                  ,AGTTAB(AGTNUM,TRABUF(TPOFFTER)))
            ENDIF
            
            !-----------------------------------------------------------
            ! Player phone nr: / Player card nr:
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
            
            CALL PRINT_IBAN(REC_PLAYER_IBAN, 'PT50', REC_PLAYER_NIB)
            
            !-----------------------------------------------------------
            ! Record write
            !-----------------------------------------------------------
            WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_PAY_FILE),20101) 
     *                '01'
     *              , REC_GAME_CODE            
     *              , REC_EXTRACTION_NAME      
     *              , REC_TICKET               
     *              , REC_TICKET_SERIES        
     *              , REC_TICKET_FRACTION      
     *              , REC_PRIZE_PAY_MODE_ID    
     *              , REC_PRIZE_PAY_CHANNEL_ID 
     *              , REC_TICKET_STATUS        
     *              , REC_PRIZE_PAY_MEDIATOR   
     *              , REC_PRIZE_PAY_REF        
     *              , REC_PRIZE_PAY_DATE(1)    
     *              , REC_PRIZE_PAY_DATE(2)    
     *              , REC_PRIZE_PAY_DATE(3)    
     *              , REC_PRIZE_PAY_DATE(4)    
     *              , REC_PRIZE_PAY_DATE(5)    
     *              , REC_PRIZE_PAY_DATE(6)    
     *              , REC_PRIZE_PAY_AMOUNT     
     *              , REC_NET_PRIZE_PAY_AMOUNT 
     *              , REC_OFFLINE_MEDIATOR_ID  
     *              , REC_PLAYER_PHONE_NR
     *              , REC_PLAYER_CARD_NR
     *              , REC_PLAYER_IBAN
C     *              , TRABUF(TPEMIS1 + OFFTRA*(TCKS-1)) ! Print draw nr
     
            !-----------------------------------------------------------
            ! Update counters
            !-----------------------------------------------------------
            SOUPFM_REC.LINE_CNT(IDX_LN_PAY_FILE) = 
     *              SOUPFM_REC.LINE_CNT(IDX_LN_PAY_FILE) + 1 

            ENDIF
        ENDDO
        !-----------------------------------------------------------
        ! Update counters
        !-----------------------------------------------------------
        SOUPFM_REC.TRX_CNT(IDX_LN_PAY_FILE) = 
     *          SOUPFM_REC.TRX_CNT(IDX_LN_PAY_FILE) + 1 

        RETURN
19001   FORMAT(I9.9)
20101   FORMAT(
     *            A2                    ! Record type
     *          , I2.2                  ! Game code
     *          , I6.6                  ! Extraction name
     *          , I5.5                  ! Ticket
     *          , I2.2                  ! Ticket series
     *          , I2.2                  ! Ticket fraction
     *          , A1                    ! Prize pay mode id
     *          , I1.1                  ! Prize pay channel id
     *          , A4                    ! Ticket status
     *          , I7.7                  ! Prize pay mediator
     *          , I13.13                ! Prize pay ref
     *          , I4.4                  ! Prize pay date (1): year
     *          , I2.2                  ! Prize pay date (2): month
     *          , I2.2                  ! Prize pay date (3): day
     *          , I2.2                  ! Prize pay date (4): hour
     *          , I2.2                  ! Prize pay date (5): minute
     *          , I2.2                  ! Prize pay date (6): second
     *          , I11.11                ! Prize pay amount (in cents)
     *          , I11.11                ! Net prize pay amount (in cents)
     *          , A7                    ! Offline mediator id
     *          , A9                    ! Player phone nr
     *          , A9                    ! Player card nr
     *          , A25                   ! IBAN
C     *          , I4.4                  ! Print draw nr
     *          )
        END







        SUBROUTINE CLOSE_FILE_SOUPFM_LN_PAY(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4 ST
        
        INTEGER*4 YEAR,MONTH,DAY
        
        ST = 0
        
        IF(SOUPFM_REC.HANDLE_FILE(IDX_LN_PAY_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        
        CALL PRINT_TRAILER_SOUPFM_LN_PAY(SOUPFM_REC, ST)
        
        CALL USRCLOS1(SOUPFM_REC.LUN_TABLE(IDX_LN_PAY_FILE))

        CALL PRINT_REPORT_LINE(SOUPFM_REC, IDX_LN_PAY_FILE)
        
        RETURN
        END
        






        SUBROUTINE PRINT_TRAILER_SOUPFM_LN_PAY(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4 ST
        
        ST = 0
        
        ! Writing total number of records, including header and trailer
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_PAY_FILE),20201) 
     *            'TP'
     *          , SOUPFM_REC.LINE_CNT(IDX_LN_PAY_FILE) + 2 
        
        RETURN
20201   FORMAT(A2,I8.8,121(' ')) 
C20201   FORMAT(A2,I8.8,125(' ')) ! Print draw nr
        END



