C
C Module SOUPFM_REL_LN_EXPIRED - SOUP File Management
C
C This program will manage the generation of the SOUP files
C for expired prizes of Lotaria Nacional (LN) 
C
C V01 09-APR-2014 SCML Program creation
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE OPEN_FILE_SOUPFM_REL_LN_EXPIRED(SOUPFM_REC, ST)
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

        IF(SOUPFM_REC.HANDLE_FILE(IDX_LN_EXPIRED_REP_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        
C        SOUPFM_REC.FILE_GEN_CDC(IDX_LN_EXPIRED_REP_FILE) = DAYCDC
        SOUPFM_REC.FILE_DATA_CDC(IDX_LN_EXPIRED_REP_FILE) = 
     *         SOUPFM_REC.SRC_CDC_DATE
        
        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_DATA_CDC(IDX_LN_EXPIRED_REP_FILE)
     *        , DATE_ARR)
        
        CALL PRINT_FILE_NAME_GEN(INT_SRC_FILE_NAME
     *        , 'REL', 'LN', 'PC', YEAR, MONTH, DAY, 'REP')
     
        CALL COPY_FROM_FNAME(SOUPFM_REC
     *        , IDX_LN_EXPIRED_REP_FILE,INT_SRC_FILE_NAME)
        
        IF(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE) .EQ. 0) THEN
            CALL RESERVE_AVAILABLE_LUN(SOUPFM_REC,IDX_LN_EXPIRED_REP_FILE,ST)
            IF(ST .NE. 0) RETURN
        ENDIF
        
        ST = 0
        CALL ROPEN1(CHR_SRC_FILE_NAME
     *            , SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE)
     *            , RECORD_LEN, ST)
        
        IF(ST .NE. 0) THEN
            CALL FILERR(INT_SRC_FILE_NAME
     *           , FUN_OPEN_FILE,ST,0)
            CALL USRCLOS1(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE))
            RETURN
        ENDIF
        
        CALL PRINT_HEADER_SOUPFM_REL_LN_EXPIRED(SOUPFM_REC, ST)
        IF(ST .NE. 0) THEN
            CALL FILERR(INT_SRC_FILE_NAME
     *           , FUN_WRITE_FILE,ST,0)
            RETURN
        ENDIF

        TYPE *, IAM(), 'Aberto ficheiro ',CHR_SRC_FILE_NAME
        
        RETURN
        END



        SUBROUTINE PRINT_HEADER_SOUPFM_REL_LN_EXPIRED(SOUPFM_REC, ST)
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
20001   FORMAT(' SCML - Departamento de Jogos            '
     *       , 'RELATORIO DE PREMIOS CADUCADOS DE LOTARIA NACIONAL'
     *       , '                        Data: ',I2.2,'.',I2.2,'.',I4.4)
20002   FORMAT('  BILHETE-SER-FRA       CODIGO APOSTA    N. MAQUINA    '
     *       , 'NOME JOGO    EXTRACCAO   DATA EXTR   DATA CADUC'
     *       , '               VALOR PREMIO')
20003   FORMAT(' ',130('-'))
20004   FORMAT(\)

        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE),20000)

        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE),20001)
     *              DATE_ARR_FILE_GEN(DATE_ARR_DAY)
     *          ,   DATE_ARR_FILE_GEN(DATE_ARR_MONTH)
     *          ,   DATE_ARR_FILE_GEN(DATE_ARR_YEAR)
C    *              DATE_ARR_FILE_GEN(DATE_ARR_DAY)
C    *          ,   DATE_ARR_FILE_GEN(DATE_ARR_MONTH)
C    *          ,   DATE_ARR_FILE_GEN(DATE_ARR_YEAR)

        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE),20000)

        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE),20002)

        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE),20003)
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE),20004)

        
        RETURN
        END





        SUBROUTINE HANDLE_VALIDATION_SOUPFM_REL_LN_EXPIRED(VALREC, SOUPFM_REC, PASFDB, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:PRMVPF.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:PASIOSUBS.DEF'

        INTEGER*4    ST, AUX, TCKS
        INTEGER*4    DATE_ARR_TRX_GEN(3)
        INTEGER*4    WEEK, YEAR
        INTEGER*4    DRAW_NR, I, J, K
        LOGICAL      VALID_DRAW

        ! Required variables to print
        INTEGER*4    REC_GAME_CODE
        INTEGER*4    REC_EXTRACTION_NAME
        INTEGER*4    REC_TICKET
        INTEGER*4    REC_TICKET_SERIES
        INTEGER*4    REC_TICKET_FRACTION
        CHARACTER*8  REC_OFFLINE_MEDIATOR_ID
        CHARACTER*16 REC_EXT_BET_REF_LINE
        CHARACTER*16 REC_GAME_NAME
        INTEGER*4    REC_EXT_BET_REF(3)
        INTEGER*4    REC_DRAW_DATE
        INTEGER*4    REC_DRAW_DATE_YEAR
        INTEGER*4    REC_DRAW_DATE_MONTH
        INTEGER*4    REC_DRAW_DATE_DAY
        INTEGER*4    REC_DRAW_PURGE_DATE
        INTEGER*4    REC_DRAW_PURGE_DATE_YEAR
        INTEGER*4    REC_DRAW_PURGE_DATE_MONTH
        INTEGER*4    REC_DRAW_PURGE_DATE_DAY
        INTEGER*8    REC_PRIZE_PAY_AMOUNT         !in cents
        CHARACTER*32 REC_PRIZE_AMOUNT_FORMATTED

        INTEGER*4    REC_DRAW_WEEK
        INTEGER*4    REC_DRAW_YEAR


        INTEGER*4 DATE_ARR(3)
        INTEGER*4 DYEAR,DMONTH,DDAY
        EQUIVALENCE(DATE_ARR(DATE_ARR_YEAR),DYEAR)
        EQUIVALENCE(DATE_ARR(DATE_ARR_MONTH),DMONTH)
        EQUIVALENCE(DATE_ARR(DATE_ARR_DAY),DDAY)
        
        CHARACTER*11   CKEY

        RECORD /STPASFDB/  PASFDB
        RECORD /STPASREC/  PASREC

        ST = 0
        
        IF(SOUPFM_REC.HANDLE_FILE(IDX_LN_EXPIRED_REP_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        

        !-----------
        ! Handling :
        !-----------
        ! 1) only uncashed, canceled(?) and privileged pay validations
        IF(   VALREC(VSTAT)   .NE. VUNCSH
C     *  .AND. VALREC(VSTAT)   .NE. VCXL
     *  .AND. VALREC(VSTAT)   .NE. VPRPAY) RETURN
        ! 2) only passive games' bets validations  
        IF(   VALREC(VGTYP)   .NE. TPAS) RETURN
C Mis-placed conditions
C        ! 3) only validations with serial number
C        IF(   VALREC(VSSER)   .EQ. 0) RETURN
C        
C        ! 4) only validations with some prize or refund amount
C        IF(   VALREC(VPAMT)   .EQ. 0
C     *  .AND. VALREC(VKPAMT)  .EQ. 0
C     *  .AND. VALREC(VRAMT)   .EQ. 0) RETURN

        IF(ST .NE. 0) RETURN

        CALL DLOGPAS(VALREC,VDETAIL)

        ! Record initialization
        REC_GAME_CODE            = 0
        REC_EXTRACTION_NAME      = 0
        REC_TICKET               = 0
        REC_TICKET_SERIES        = 0
        REC_TICKET_FRACTION      = 0
        
        WEEK                     = 0
        YEAR                     = 0
        ST                       = 0

        REC_OFFLINE_MEDIATOR_ID  = '       '
        REC_GAME_NAME            = '                '
        !-----------------------------------------------------------
        ! Record fill
        !-----------------------------------------------------------
        ! Game code:
        REC_GAME_CODE            = VALREC(VGAM)
        ! If new passive validation layout
        !-----------------------------------------------------------
        ! Extraction name:
        VALID_DRAW = .TRUE.
        I = 1
        DO WHILE(I .LE. VALREC(VPZOFF) .AND. VALID_DRAW .EQ. .TRUE.)
            DRAW_NR = VDETAIL(VDRW,I)
            IF(PASEMIS(SOUPFM_REC.PAS_EMIS,SOUPFM_REC.PAS_GIND) .NE.
     *          DRAW_NR) THEN
               VALID_DRAW = .FALSE.
            ENDIF
            I = I + 1
        ENDDO

        ! 5) only validations with match this particular purge date
        IF(VALID_DRAW .EQ. .FALSE.) THEN
            TYPE *, IAM(), 'INVALID DRAW NR!'
            ST = -1
            RETURN
        ENDIF
        
C       ! 6) only prizes above EUR 150
C       IF(VALREC(VPAMT) .LT. P(VALORDER)) THEN
C           RETURN
C       ENDIF

       CALL PASIO_READ(PASFDB,VALREC(VTCKT),VALREC(VSERN),
     *                 VALREC(VPFRAC),PASREC)
       IF (PASFDB.ERR .NE. IOE_NOERR) THEN
            TYPE*, IAM(), '       '
9000        FORMAT(I5.5,'S',I2.2,'F',I2.2)
            WRITE(CKEY,9000) VALREC(VTCKT),VALREC(VSERN),VALREC(VPFRAC)
            TYPE*, IAM(), 'SOUPFM - Erro: ', PASFDB.ERR, 
     *                    ' a ler o registo: ', CKEY
            TYPE*, IAM(), '       '
            TYPE*, IAM(), 'DUMP:'
            CALL PASIO_DUMP(PASFDB)
            TYPE*, IAM(), '       '
            CALL GSTOP(GEXIT_FATAL)
       ENDIF
        
        ! Here we only have draw nr
        CALL GETWEK(
     *             DRAW_NR
     *           , REC_GAME_CODE
     *           , WEEK
     *           , YEAR
     *           , ST
     *       )
        IF(ST .NE. 0) THEN
            TYPE *, IAM(), 'ERROR WHILE CONVERTING DRAW #' 
     *            , DRAW_NR
     *            , ' TO WEEK/YEAR'
            CALL GPAUSE
            ST = 0
        ENDIF
        ! Convert this to EEAAAA
        REC_EXTRACTION_NAME      =
     *             MOD(WEEK,100) * 10000
     *           + MOD(YEAR,10000) 
        REC_DRAW_WEEK = WEEK
        REC_DRAW_YEAR = YEAR
        !-----------------------------------------------------------
        ! Ticket:
        REC_TICKET               = VALREC(VTCKT)
        !-----------------------------------------------------------
        ! Series:
        REC_TICKET_SERIES        = VALREC(VSERN)
        !-----------------------------------------------------------
        ! Fraction:
        REC_TICKET_FRACTION      = VALREC(VPFRAC)
        !-----------------------------------------------------------
        ! External bet ref:
        CALL GET_BET_EXTERNAL_REF_FROM_VALREC(VALREC,REC_EXT_BET_REF)
        IF(VALREC(VPASTYP) .EQ. VPASOFF) THEN
            REC_EXT_BET_REF(1) = 0
            REC_EXT_BET_REF(2) = 0
            REC_EXT_BET_REF(3) = 0
        ENDIF
        CALL PRINT_EXTERNAL_BET_REF(REC_EXT_BET_REF_LINE,REC_EXT_BET_REF,.TRUE.)
        
        !-----------------------------------------------------------
        ! Agent nr:
        ! Check which index is the correct one:
        ! VALREC(VOFFTER) - OFFLINE TERMINAL (?)
        ! VALREC(VSTER)   - SELLING TERMINAL
        ! VALREC(VCTER)   - CASHING TERMINAL
        ! VALREC(VLTER)   - CLAIM TERMINAL (only one that has any information)
        CALL PRINT_MEDIATOR_REP(REC_OFFLINE_MEDIATOR_ID
     *          , AGTTAB(AGTNUM,PASREC.AGT))
        
C       TYPE *,IAM(),'1-', VALREC(VOFFTER), VALREC(VSTER), VALREC(VPASTYP)
C       TYPE *,IAM(),'2-', VALREC(VCTER), VALREC(VLTER),AGTTAB(AGTNUM,VALREC(VLTER))
        !-----------------------------------------------------------
        ! Game name:
20100   FORMAT(4A4)
        WRITE(REC_GAME_NAME,20100)
     *            (GLNAMES(K,REC_GAME_CODE),K=1,4)
        
        
        CALL CDC_TO_DATE_ARR(SOUPFM_REC.GAME_DATA_REC.OUT_DRAW_DATE_CDC
     *        , DATE_ARR)
        
        REC_DRAW_DATE_YEAR  = DYEAR
        REC_DRAW_DATE_MONTH = DMONTH
        REC_DRAW_DATE_DAY   = DDAY

        CALL CDC_TO_DATE_ARR(SOUPFM_REC.GAME_DATA_REC.OUT_PURGE_DATE_CDC
     *        , DATE_ARR)

        REC_DRAW_PURGE_DATE_YEAR  = DYEAR
        REC_DRAW_PURGE_DATE_MONTH = DMONTH
        REC_DRAW_PURGE_DATE_DAY   = DDAY
        
        
        REC_PRIZE_PAY_AMOUNT = KZEXT(VALREC(VPAMT))
        
        REC_PRIZE_AMOUNT_FORMATTED = SOUPFM_DMONYI8(REC_PRIZE_PAY_AMOUNT)
        !-----------------------------------------------------------
        ! Record write
        !-----------------------------------------------------------
        !-----------------------------------------------------------
        ! Record write
        !-----------------------------------------------------------
20101   FORMAT(
     *            '      '
     *          , I5.5, '-', I2.2, '-', I2.2   ! Ticket-Series-Fraction
     *          , '    '
     *          , A16                          ! Bet code
     *          , '      '
     *          , A8                           ! Agent code
     *          , '     '
     *          , A11                          ! Game name
     *          , '   '
     *          , I2.2, '/', I4.4              ! Draw name
     *          , '  '
     *          , I2.2, '.', I2.2, '.', I4.4   ! Draw date
     *          , '   '
     *          , I2.2, '.', I2.2, '.', I4.4   ! Draw purge date
     *          , 14(' ')
     *          , A13                          ! Prize amount
     *          )
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE),20101)
     *        REC_TICKET, REC_TICKET_SERIES, REC_TICKET_FRACTION
     *    ,   REC_EXT_BET_REF_LINE
     *    ,   REC_OFFLINE_MEDIATOR_ID
     *    ,   REC_GAME_NAME(1:11)
     *    ,   REC_DRAW_WEEK, REC_DRAW_YEAR
     *    ,   REC_DRAW_DATE_DAY, REC_DRAW_DATE_MONTH, REC_DRAW_DATE_YEAR
     *    ,   REC_DRAW_PURGE_DATE_DAY, REC_DRAW_PURGE_DATE_MONTH, REC_DRAW_PURGE_DATE_YEAR
     *    ,   REC_PRIZE_AMOUNT_FORMATTED
        !-----------------------------------------------------------
        ! Update counters
        !-----------------------------------------------------------
         SOUPFM_REC.EXP_STAT_REC.NR_BETS(
     *      IDX_LN_EXPIRED_REP_FILE
     *    , REC_GAME_CODE) = 
     *      SOUPFM_REC.EXP_STAT_REC.NR_BETS(
     *          IDX_LN_EXPIRED_REP_FILE
     *        , REC_GAME_CODE) + 1

        SOUPFM_REC.EXP_STAT_REC.AMOUNT(
     *      IDX_LN_EXPIRED_REP_FILE
     *    , REC_GAME_CODE) = 
     *      SOUPFM_REC.EXP_STAT_REC.AMOUNT(
     *          IDX_LN_EXPIRED_REP_FILE
     *        , REC_GAME_CODE) + REC_PRIZE_PAY_AMOUNT

        SOUPFM_REC.EXP_STAT_REC.TOTAL_NR_BETS(IDX_LN_EXPIRED_REP_FILE) = 
     *      SOUPFM_REC.EXP_STAT_REC.TOTAL_NR_BETS(IDX_LN_EXPIRED_REP_FILE) + 1

        SOUPFM_REC.EXP_STAT_REC.TOTAL_AMOUNT(IDX_LN_EXPIRED_REP_FILE) = 
     *      SOUPFM_REC.EXP_STAT_REC.TOTAL_AMOUNT(IDX_LN_EXPIRED_REP_FILE) + 
     *      REC_PRIZE_PAY_AMOUNT
        !-----------------------------------------------------------
        ! Update counters
        !-----------------------------------------------------------
        SOUPFM_REC.LINE_CNT(IDX_LN_EXPIRED_REP_FILE) = 
     *         SOUPFM_REC.LINE_CNT(IDX_LN_EXPIRED_REP_FILE) + 1 

        !-----------------------------------------------------------
        ! Update counters
        !-----------------------------------------------------------
        SOUPFM_REC.TRX_CNT(IDX_LN_EXPIRED_REP_FILE) = 
     *          SOUPFM_REC.TRX_CNT(IDX_LN_EXPIRED_REP_FILE) + 1 

        RETURN
        END







        SUBROUTINE CLOSE_FILE_SOUPFM_REL_LN_EXPIRED(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4 ST
        
        INTEGER*4 YEAR,MONTH,DAY
        
        ST = 0
        
        IF(SOUPFM_REC.HANDLE_FILE(IDX_LN_EXPIRED_REP_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        
        CALL PRINT_TRAILER_SOUPFM_REL_LN_EXPIRED(SOUPFM_REC, ST)
        
        CALL USRCLOS1(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE))

        CALL PRINT_REPORT_LINE(SOUPFM_REC, IDX_LN_EXPIRED_REP_FILE)
        
        RETURN
        END
        






        SUBROUTINE PRINT_TRAILER_SOUPFM_REL_LN_EXPIRED(SOUPFM_REC, ST)
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
     *       , 'JOGO                    FRACCOES          PREMIOS')
20203   FORMAT(78(' '),53('-'))
20204   FORMAT(80(' ')    ! Line
     *       , A16, 7(' '), I9, 4(' '), A13)
20205   FORMAT(80(' ')    ! Total
     *       , 'TOTAL           ', 7(' '), I9, 4(' '), A13)

        ! Writing trailer and final digest report
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE),20200)
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE),20201)
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE),20200)
        
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE),20202)
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE),20203)

        DO I = 1, MAXGAM
            GTYP = GNTTAB(GAMTYP,I)
            
            IF(  GTYP .EQ. TPAS ) THEN

                IF(   SOUPFM_REC.EXP_STAT_REC.NR_BETS(IDX_LN_EXPIRED_REP_FILE,I) .NE. 0
     *          .AND. SOUPFM_REC.EXP_STAT_REC.AMOUNT(IDX_LN_EXPIRED_REP_FILE,I) .NE. KZEXT(0) ) THEN 
20100               FORMAT(4A4)
                    WRITE(REC_GAME_NAME,20100)
     *                        (GLNAMES(K,I),K=1,4)
                    WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE),20204)
     *                  , REC_GAME_NAME
     *                  , SOUPFM_REC.EXP_STAT_REC.NR_BETS(IDX_LN_EXPIRED_REP_FILE,I)
     *                  , SOUPFM_DMONYI8(SOUPFM_REC.EXP_STAT_REC.AMOUNT(IDX_LN_EXPIRED_REP_FILE,I))
                ENDIF
            ENDIF
        ENDDO

        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE),20203)
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_REP_FILE),20205)
     *        SOUPFM_REC.EXP_STAT_REC.TOTAL_NR_BETS(IDX_LN_EXPIRED_REP_FILE)
     *      , SOUPFM_DMONYI8(SOUPFM_REC.EXP_STAT_REC.TOTAL_AMOUNT(IDX_LN_EXPIRED_REP_FILE))

        RETURN
        END



