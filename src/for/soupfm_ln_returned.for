C
C Module SOUPFM_LN_RETURNED - SOUP File Management
C
C This program will manage the generation of the SOUP files
C for returned prizes of Lotaria Nacional (LN) 
C
C V01 20-DEC-2013 SCML Program creation
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE OPEN_FILE_SOUPFM_LN_RETURNED(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        
        INTEGER*4  ST
        INTEGER*4  RECORD_LEN
        PARAMETER (RECORD_LEN = 49)
        
        INTEGER*4 DATE_ARR(3)
        INTEGER*4 YEAR,MONTH,DAY
        EQUIVALENCE(DATE_ARR(DATE_ARR_YEAR),YEAR)
        EQUIVALENCE(DATE_ARR(DATE_ARR_MONTH),MONTH)
        EQUIVALENCE(DATE_ARR(DATE_ARR_DAY),DAY)
        
        ST = 0

        IF(SOUPFM_REC.HANDLE_FILE(IDX_LN_RETURNED_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        
C        SOUPFM_REC.FILE_GEN_CDC(IDX_LN_RETURNED_FILE) = DAYCDC
        SOUPFM_REC.FILE_DATA_CDC(IDX_LN_RETURNED_FILE) = 
     *         SOUPFM_REC.SRC_CDC_DATE
        
        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_DATA_CDC(IDX_LN_RETURNED_FILE)
     *        , DATE_ARR)
        
        CALL PRINT_FILE_NAME(INT_SRC_FILE_NAME
     *        , 'SOUP', 'LN', 'PD', YEAR, MONTH, DAY)
     
        CALL COPY_FROM_FNAME(SOUPFM_REC
     *        , IDX_LN_RETURNED_FILE,INT_SRC_FILE_NAME)
        
        IF(SOUPFM_REC.LUN_TABLE(IDX_LN_RETURNED_FILE) .EQ. 0) THEN
            CALL RESERVE_AVAILABLE_LUN(SOUPFM_REC,IDX_LN_RETURNED_FILE,ST)
            IF(ST .NE. 0) RETURN
        ENDIF
        
        ST = 0
        CALL ROPEN1(CHR_SRC_FILE_NAME
     *            , SOUPFM_REC.LUN_TABLE(IDX_LN_RETURNED_FILE)
     *            , RECORD_LEN, ST)
        
        IF(ST .NE. 0) THEN
            CALL FILERR(INT_SRC_FILE_NAME
     *           , FUN_OPEN_FILE,ST,0)
            CALL USRCLOS1(SOUPFM_REC.LUN_TABLE(IDX_LN_RETURNED_FILE))
            RETURN
        ENDIF
        
        CALL PRINT_HEADER_SOUPFM_LN_RETURNED(SOUPFM_REC, ST)
        IF(ST .NE. 0) THEN
            CALL FILERR(INT_SRC_FILE_NAME
     *           , FUN_WRITE_FILE,ST,0)
            RETURN
        ENDIF

        TYPE *, IAM(), 'Aberto ficheiro ',CHR_SRC_FILE_NAME
        SOUPFM_REC.AUX_CNT(IDX_LN_RETURNED_FILE) = 0
        
        RETURN
        END



        SUBROUTINE PRINT_HEADER_SOUPFM_LN_RETURNED(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4 DATE_ARR_FILE_GEN(3)
        INTEGER*4 DATE_ARR_FILE_DATA(3)
        INTEGER*4 ST
        
        ST = 0
        
        CALL GET_SYS_DATE(DATE_ARR_FILE_GEN)
C        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_GEN_CDC(IDX_LN_RETURNED_FILE)
C     *        , DATE_ARR_FILE_GEN)

        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_DATA_CDC(IDX_LN_RETURNED_FILE)
     *        , DATE_ARR_FILE_DATA)

        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_RETURNED_FILE),20001) 
     *            'HP'
     *          ,   DATE_ARR_FILE_DATA(DATE_ARR_YEAR)  * 10000 
     *            + DATE_ARR_FILE_DATA(DATE_ARR_MONTH) * 100
     *            + DATE_ARR_FILE_DATA(DATE_ARR_DAY)   * 1
     *          ,   DATE_ARR_FILE_GEN(DATE_ARR_YEAR)   * 10000 
     *            + DATE_ARR_FILE_GEN(DATE_ARR_MONTH)  * 100
     *            + DATE_ARR_FILE_GEN(DATE_ARR_DAY)    * 1
        
        RETURN
20001   FORMAT(A2,I8.8,I8.8,31(' '))
        END





        SUBROUTINE HANDLE_TRANSACTION_SOUPFM_LN_RETURNED(TRABUF, SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        
        INTEGER*4    ST, AUX, TCKS
        INTEGER*4    DATE_ARR_TRX_GEN(3)
        INTEGER*4    WEEK, YEAR

        ! Required variables to print
        INTEGER*4    REC_GAME_CODE
        INTEGER*4    REC_EXTRACTION_NAME
        INTEGER*4    REC_TICKET
        INTEGER*4    REC_TICKET_SERIES
        INTEGER*4    REC_TICKET_FRACTION
        INTEGER*4    REC_PRIZE_RETURN_MEDIATOR
        INTEGER*4    REC_PRIZE_RETURN_DATE(6)
        CHARACTER*7  REC_OFFLINE_MEDIATOR_ID
        INTEGER*4    REC_PRIZE_RETURN_TYPE_ID
        
        LOGICAL      RETURNED_WINNER
        INTEGER*4    DRAW_NR, GAME_NR, GAME_IND, PAS_EMI
        INTEGER*4    EXTRACTION_CDC


C        INTEGER*4     CNT
        
C        COMMON /TEST/ CNT
        
        ST = 0
        
        IF(SOUPFM_REC.HANDLE_FILE(IDX_LN_RETURNED_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        
        !-----------
        ! Handling :
        !-----------
        ! 0) only good transactions 
        IF(TRABUF(TSTAT)   .NE. GOOD) RETURN
        ! 1) only returned type transactions 
        IF(TRABUF(TTYP)    .NE. TRET) RETURN
        ! 2) only passive game transactions  
        IF(TRABUF(TGAMTYP) .NE. TPAS) RETURN
        ! 3) only transactions without error  
        IF(TRABUF(TERR)    .NE. NOER) RETURN

        IF(TRABUF(TCDC) .NE. SOUPFM_REC.SRC_CDC_DATE) THEN
            TYPE *, IAM(), 'ERRO:'
            TYPE *, IAM(), ' Foi encontrada uma transaccao com data CDC'
     *                   , '(',TRABUF(TCDC),') diferente da fornecida ('
     *                   , SOUPFM_REC.SRC_CDC_DATE, ')!'
            ST = -1
        ENDIF

        IF(ST .NE. 0) RETURN

        SOUPFM_REC.AUX_CNT(IDX_LN_RETURNED_FILE) = SOUPFM_REC.AUX_CNT(IDX_LN_RETURNED_FILE) + 1

C        TYPE *, '=============================================='
C        SOUPFM_REC.PAS_DEBUG = .TRUE.
C        TYPE *, SOUPFM_REC.AUX_CNT(IDX_LN_RETURNED_FILE), ': IBREF = ', GET_BET_REF(TRABUF)
C        TYPE *, '----------------------------------------------'
C        TYPE *, 'TRABUF(TSTAT)   = ',TRABUF(TSTAT) 
C        TYPE *, '         GOOD   = ',GOOD 
C        TYPE *, '----------------------------------------------'
C        TYPE *, 'TRABUF(TTYP)    = ',TRABUF(TTYP) 
C        TYPE *, '           TRET = ',TRET 
C        TYPE *, '----------------------------------------------'
C        TYPE *, 'TRABUF(TGAMTYP) = ',TRABUF(TGAMTYP) 
C        TYPE *, '           TPAS = ',TPAS 
C        TYPE *, '----------------------------------------------'
C        TYPE *, 'TRABUF(TERR)    = ',TRABUF(TERR) 
C        TYPE *, '----------------------------------------------'
C        TYPE *, 'TRABUF(TCDC)    = ',TRABUF(TCDC)
C        TYPE *, '   SRC_CDC_DATE = ',SOUPFM_REC.SRC_CDC_DATE
C        TYPE *, '----------------------------------------------'

        DO TCKS = 1, TRABUF(TPTCK)
            RETURNED_WINNER  = .FALSE.

C        TYPE *, '+++++++++++++++++++++++++++++++++++++++++++++++++' 
C        TYPE *, 'TRABUF(TPSTS[x])= ',TRABUF(TPSTS1+OFFTRA*(TCKS-1)), '[x=', TCKS, ']'
C        TYPE *, '        RETAFDR = ', RETAFDR 
C        TYPE *, '         NUM[x] = ', TRABUF(TPNUM1+OFFTRA*(TCKS-1))
C        TYPE *, '         SER[x] = ', TRABUF(TPSER1+OFFTRA*(TCKS-1))
C        TYPE *, '        FRAC[x] = ', TRABUF(TPTEN1+OFFTRA*(TCKS-1))
C        TYPE *, '.................................................' 

            
            ! Only prizes returned after draw
            IF(TRABUF(TPSTS1+OFFTRA*(TCKS-1)).EQ.RETAFDR) THEN
C        TYPE *, IAM(), '---------------------------------------'
C        CNT = CNT + 1
C        SOUPFM_REC.PAS_DEBUG = .TRUE.
C            !-----------------------------------------------------------
C            ! Ticket:
C            REC_TICKET               = TRABUF(TPNUM1  + OFFTRA*(TCKS-1))
C            !-----------------------------------------------------------
C            ! Series:
C            REC_TICKET_SERIES        = TRABUF(TPSER1  + OFFTRA*(TCKS-1))
C            !-----------------------------------------------------------
C            ! Fraction:
C            REC_TICKET_FRACTION      = TRABUF(TPTEN1  + OFFTRA*(TCKS-1))
C        TYPE *, IAM(), 'ID = ',  REC_TICKET, ' - ', REC_TICKET_SERIES, ' - ', REC_TICKET_FRACTION
C        TYPE *, IAM(), CNT
C
                DRAW_NR = TRABUF(TPEMIS1 + OFFTRA*(TCKS-1))
                GAME_NR = TRABUF(TGAM)
                GAME_IND = TRABUF(TGAMIND)
                CALL GET_PAS_EMI(DRAW_NR, GAME_IND, PAS_EMI)
                EXTRACTION_CDC = PASESD(PAS_EMI,GAME_IND)
C                
C                TYPE *, IAM(), 'DRAW NR        = ', DRAW_NR
C                TYPE *, IAM(), 'GAME NR        = ', GAME_NR
C                TYPE *, IAM(), 'GAME IND       = ', GAME_IND
C                TYPE *, IAM(), 'PAS EMI        = ', PAS_EMI
C                TYPE *, IAM(), 'EXTRACTION_CDC = ', EXTRACTION_CDC
                
C        TYPE *, '        DRAW NR = ', DRAW_NR
C        TYPE *, '        GAME NR = ', GAME_NR
C        TYPE *, '       GAME IND = ', GAME_IND
C        TYPE *, '        PAS EMI = ', PAS_EMI
C        TYPE *, ' EXTRACTION_CDC = ', EXTRACTION_CDC
C        TYPE *, '        SRC_CDC = ', SOUPFM_REC.SRC_CDC_DATE
C        TYPE *, '.................................................' 
                ! Check if extraction cdc is equal to supplied one
                IF(EXTRACTION_CDC .LE. SOUPFM_REC.SRC_CDC_DATE) THEN
                    
                    SOUPFM_REC.PAS_EMIS = PAS_EMI
                    SOUPFM_REC.PAS_GNUM = GAME_NR
                    SOUPFM_REC.PAS_GIND = GAME_IND
                    
                    ST = 0
                    CALL OPEN_KEY_FILE_SOUPFM_PRG_PAS(SOUPFM_REC, ST)
                    IF(ST .NE. 0) THEN
                        CALL GSTOP(GEXIT_FATAL)
                    ENDIF
                    
                    SOUPFM_REC.PAS_SERIAL = TRABUF(TPSER1+OFFTRA*(TCKS-1))
                    SOUPFM_REC.PAS_NUM    = TRABUF(TPNUM1+OFFTRA*(TCKS-1))
                    SOUPFM_REC.PAS_FRAC   = TRABUF(TPTEN1+OFFTRA*(TCKS-1))
                    
                    ST = 0
                    CALL READ_KEY_PRGREC_PAS(SOUPFM_REC, VALREC, ST)
                    IF(ST .EQ. 0) THEN
C        TYPE *, 'VALREC(VSTAT)   = ',VALREC(VSTAT) 
C        TYPE *, '         VUNCSH = ',VUNCSH 
C        TYPE *, '         VPRPAY = ',VPRPAY
C        TYPE *, '         VNOPAY = ',VNOPAY
C        TYPE *, '###############################################' 
                        ! Only consider prized tickets
                        IF(  VALREC(VSTAT) .EQ. VUNCSH 
     *                  .OR. VALREC(VSTAT) .EQ. VPRPAY
     *                  .OR. VALREC(VSTAT) .EQ. VNOPAY ) THEN ! Must include this for returned after draw
C        TYPE *, '** RETURNED WINNER FOUND! **' 
                             RETURNED_WINNER = .TRUE.
                        ENDIF
C                        TYPE *, IAM(), '* VALREC(VSTAT) = ', VALREC(VSTAT)
C                        TYPE *, IAM(), '*        VUNCSH = ', VUNCSH
C                        TYPE *, IAM(), '*        VPRPAY = ', VPRPAY
                    ENDIF

                    ST = 0
                    CALL CLOSE_KEY_FILE_SOUPFM_PRG_PAS(SOUPFM_REC, ST)
                    IF(ST .NE. 0) THEN
                        CALL GSTOP(GEXIT_FATAL)
                    ENDIF                    

                ENDIF
            ENDIF
            
C            IF(  RETURNED_AFTER_DRAW  .EQ. .TRUE. 
C     *      .OR. RETURNED_ON_DRAW_DAY .EQ. .TRUE.) THEN
            IF(  RETURNED_WINNER  .EQ. .TRUE.) THEN 

            ! Record initialization
            REC_GAME_CODE            = 0
            REC_EXTRACTION_NAME      = 0
            REC_TICKET               = 0
            REC_TICKET_SERIES        = 0
            REC_TICKET_FRACTION      = 0
            REC_PRIZE_RETURN_MEDIATOR= 0
            
            REC_PRIZE_RETURN_DATE(1)    = 0
            REC_PRIZE_RETURN_DATE(2)    = 0
            REC_PRIZE_RETURN_DATE(3)    = 0
            REC_PRIZE_RETURN_DATE(4)    = 0
            REC_PRIZE_RETURN_DATE(5)    = 0
            REC_PRIZE_RETURN_DATE(6)    = 0
            
            REC_OFFLINE_MEDIATOR_ID  = '       '
            REC_PRIZE_RETURN_TYPE_ID = 0
            
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
C               ! Convert this to EEAAAA
C               REC_EXTRACTION_NAME      =
C     *            MOD(TRABUF(TVEPWK),100) * 10000
C     *          + MOD(TRABUF(TVEPYR),10000) 
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
               ! Convert this to EEAAAA
               REC_EXTRACTION_NAME      =
     *            MOD(WEEK,100) * 10000
     *          + MOD(YEAR,10000) 
C            ENDIF           
            
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
            ! Prize return mediator:
            ! We were getting SAP agent code, but it exceeds 7 digits;
            ! Likely is just agent code
            !REC_PRIZE_RETURN_MEDIATOR   = GET_SAP_AGENT_CODE(TRABUF)
            REC_PRIZE_RETURN_MEDIATOR   = GET_AGENT_CODE(TRABUF)
            
            !-----------------------------------------------------------
            ! Prize return date:
            CALL CDC_TO_DATE_ARR(TRABUF(TCDC)
     *            , DATE_ARR_TRX_GEN)
            
            REC_PRIZE_RETURN_DATE(1) = DATE_ARR_TRX_GEN(DATE_ARR_YEAR)
            REC_PRIZE_RETURN_DATE(2) = DATE_ARR_TRX_GEN(DATE_ARR_MONTH)
            REC_PRIZE_RETURN_DATE(3) = DATE_ARR_TRX_GEN(DATE_ARR_DAY)
            
            AUX = TRABUF(TTIM) 
            REC_PRIZE_RETURN_DATE(4) = AUX / 3600        ! Hour
            AUX = AUX - (REC_PRIZE_RETURN_DATE(4) * 3600)
            REC_PRIZE_RETURN_DATE(5) = AUX / 60          ! Minute
            AUX = AUX - (REC_PRIZE_RETURN_DATE(5) * 60)
            REC_PRIZE_RETURN_DATE(6) = AUX               ! Second
            
            !-----------------------------------------------------------
            ! Offline mediator id:
            !REC_OFFLINE_MEDIATOR_ID  = TRABUF(TPOFFTER)
            IF(TRABUF(TPOFFTER) .GT. 0) THEN
                CALL PRINT_MEDIATOR(REC_OFFLINE_MEDIATOR_ID
     *                  ,AGTTAB(AGTNUM,TRABUF(TPOFFTER)))
            ENDIF
            
            !-----------------------------------------------------------
            ! Prize return type id:
            IF     ( TRABUF(TPRETYP) .EQ. ALLTCK  ) THEN
                REC_PRIZE_RETURN_TYPE_ID  = PRIZE_RETURNED_ALL_TICKET
            ELSEIF ( TRABUF(TPRETYP) .EQ. BYFRAC  ) THEN
                REC_PRIZE_RETURN_TYPE_ID  = PRIZE_RETURNED_FRACTION
            ELSEIF ( TRABUF(TPRETYP) .EQ. HALFTCK ) THEN
                REC_PRIZE_RETURN_TYPE_ID  = PRIZE_RETURNED_HALF_TICKET
            ENDIF
            
            !-----------------------------------------------------------
            ! Record write
            !-----------------------------------------------------------
            WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_RETURNED_FILE),20101) 
     *                '01'
     *              , REC_GAME_CODE            
     *              , REC_EXTRACTION_NAME      
     *              , REC_TICKET               
     *              , REC_TICKET_SERIES        
     *              , REC_TICKET_FRACTION      
     *              , REC_PRIZE_RETURN_MEDIATOR
     *              , REC_PRIZE_RETURN_DATE(1) 
     *              , REC_PRIZE_RETURN_DATE(2) 
     *              , REC_PRIZE_RETURN_DATE(3) 
     *              , REC_PRIZE_RETURN_DATE(4) 
     *              , REC_PRIZE_RETURN_DATE(5) 
     *              , REC_PRIZE_RETURN_DATE(6) 
     *              , REC_OFFLINE_MEDIATOR_ID  
     *              , REC_PRIZE_RETURN_TYPE_ID 
            
            !-----------------------------------------------------------
            ! Update counters
            !-----------------------------------------------------------
            SOUPFM_REC.LINE_CNT(IDX_LN_RETURNED_FILE) = 
     *              SOUPFM_REC.LINE_CNT(IDX_LN_RETURNED_FILE) + 1 

            ENDIF
        ENDDO
        !-----------------------------------------------------------
        ! Update counters
        !-----------------------------------------------------------
        SOUPFM_REC.TRX_CNT(IDX_LN_RETURNED_FILE) = 
     *          SOUPFM_REC.TRX_CNT(IDX_LN_RETURNED_FILE) + 1 

        RETURN
19001   FORMAT(I9.9)
20101   FORMAT(
     *            A2                    ! Record type
     *          , I2.2                  ! Game code
     *          , I6.6                  ! Extraction name
     *          , I5.5                  ! Ticket
     *          , I2.2                  ! Ticket series
     *          , I2.2                  ! Ticket fraction
     *          , I7.7                  ! Prize return mediator
     *          , I4.4                  ! Prize return date (1): year
     *          , I2.2                  ! Prize return date (2): month
     *          , I2.2                  ! Prize return date (3): day
     *          , I2.2                  ! Prize return date (4): hour
     *          , I2.2                  ! Prize return date (5): minute
     *          , I2.2                  ! Prize return date (6): second
     *          , A7                    ! Offline mediator id
     *          , I2.2                  ! Prize return type id
     *          )
        END







        SUBROUTINE CLOSE_FILE_SOUPFM_LN_RETURNED(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4 ST
        
        INTEGER*4 YEAR,MONTH,DAY
        
        ST = 0
        
        IF(SOUPFM_REC.HANDLE_FILE(IDX_LN_RETURNED_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        
        CALL PRINT_TRAILER_SOUPFM_LN_RETURNED(SOUPFM_REC, ST)
        
        CALL USRCLOS1(SOUPFM_REC.LUN_TABLE(IDX_LN_RETURNED_FILE))

        CALL PRINT_REPORT_LINE(SOUPFM_REC, IDX_LN_RETURNED_FILE)
        
        RETURN
        END
        






        SUBROUTINE PRINT_TRAILER_SOUPFM_LN_RETURNED(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4 ST
        
        ST = 0
        
        ! Writing total number of records, including header and trailer
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_RETURNED_FILE),20201) 
     *            'TP'
     *          , SOUPFM_REC.LINE_CNT(IDX_LN_RETURNED_FILE) + 2 
        
        RETURN
20201   FORMAT(A2,I8.8,39(' '))
        END



