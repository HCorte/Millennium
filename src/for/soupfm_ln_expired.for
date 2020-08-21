C
C Module SOUPFM_LN_EXPIRED - SOUP File Management
C
C This program will manage the generation of the SOUP files
C for expired prizes of Lotaria Nacional (LN) 
C
C V01 20-DEC-2013 SCML Program creation
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE OPEN_FILE_SOUPFM_LN_EXPIRED(SOUPFM_REC, ST)
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

        IF(SOUPFM_REC.HANDLE_FILE(IDX_LN_EXPIRED_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        
C        SOUPFM_REC.FILE_GEN_CDC(IDX_LN_EXPIRED_FILE) = DAYCDC
        SOUPFM_REC.FILE_DATA_CDC(IDX_LN_EXPIRED_FILE) = 
     *         SOUPFM_REC.SRC_CDC_DATE
        
        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_DATA_CDC(IDX_LN_EXPIRED_FILE)
     *        , DATE_ARR)
        
        CALL PRINT_FILE_NAME(INT_SRC_FILE_NAME
     *        , 'SOUP', 'LN', 'PC', YEAR, MONTH, DAY)
     
        CALL COPY_FROM_FNAME(SOUPFM_REC
     *        , IDX_LN_EXPIRED_FILE,INT_SRC_FILE_NAME)
        
        IF(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_FILE) .EQ. 0) THEN
            CALL RESERVE_AVAILABLE_LUN(SOUPFM_REC,IDX_LN_EXPIRED_FILE,ST)
            IF(ST .NE. 0) RETURN
        ENDIF
        
        ST = 0
        CALL ROPEN1(CHR_SRC_FILE_NAME
     *            , SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_FILE)
     *            , RECORD_LEN, ST)
        
        IF(ST .NE. 0) THEN
            CALL FILERR(INT_SRC_FILE_NAME
     *           , FUN_OPEN_FILE,ST,0)
            CALL USRCLOS1(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_FILE))
            RETURN
        ENDIF
        
        CALL PRINT_HEADER_SOUPFM_LN_EXPIRED(SOUPFM_REC, ST)
        IF(ST .NE. 0) THEN
            CALL FILERR(INT_SRC_FILE_NAME
     *           , FUN_WRITE_FILE,ST,0)
            RETURN
        ENDIF

        TYPE *, IAM(), 'Aberto ficheiro ',CHR_SRC_FILE_NAME
        
        RETURN
        END



        SUBROUTINE PRINT_HEADER_SOUPFM_LN_EXPIRED(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4 DATE_ARR_FILE_GEN(3)
        INTEGER*4 DATE_ARR_FILE_DATA(3)
        INTEGER*4 ST
        
        ST = 0
        
        CALL GET_SYS_DATE(DATE_ARR_FILE_GEN)
C        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_GEN_CDC(IDX_LN_EXPIRED_FILE)
C     *        , DATE_ARR_FILE_GEN)

        CALL CDC_TO_DATE_ARR(SOUPFM_REC.FILE_DATA_CDC(IDX_LN_EXPIRED_FILE)
     *        , DATE_ARR_FILE_DATA)

        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_FILE),20001) 
     *            'HP'
     *          ,   DATE_ARR_FILE_DATA(DATE_ARR_YEAR)  * 10000 
     *            + DATE_ARR_FILE_DATA(DATE_ARR_MONTH) * 100
     *            + DATE_ARR_FILE_DATA(DATE_ARR_DAY)   * 1
     *          ,   DATE_ARR_FILE_GEN(DATE_ARR_YEAR)   * 10000 
     *            + DATE_ARR_FILE_GEN(DATE_ARR_MONTH)  * 100
     *            + DATE_ARR_FILE_GEN(DATE_ARR_DAY)    * 1
        
        RETURN
20001   FORMAT(A2,I8.8,I8.8,1(' '))
        END





        SUBROUTINE HANDLE_VALIDATION_SOUPFM_LN_EXPIRED(VALREC, SOUPFM_REC, ST)
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

        INTEGER*4    ST, AUX, TCKS
        INTEGER*4    DATE_ARR_TRX_GEN(3)
        INTEGER*4    WEEK, YEAR
        INTEGER*4    DRAW_NR, I
        LOGICAL      VALID_DRAW

        ! Required variables to print
        INTEGER*4    REC_GAME_CODE
        INTEGER*4    REC_EXTRACTION_NAME
        INTEGER*4    REC_TICKET
        INTEGER*4    REC_TICKET_SERIES
        INTEGER*4    REC_TICKET_FRACTION

        ST = 0
        
        IF(SOUPFM_REC.HANDLE_FILE(IDX_LN_EXPIRED_FILE) .EQ. .FALSE.) THEN
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
        ! Record write
        !-----------------------------------------------------------
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_FILE),20101) 
     *         '01'
     *       , REC_GAME_CODE            
     *       , REC_EXTRACTION_NAME      
     *       , REC_TICKET               
     *       , REC_TICKET_SERIES        
     *       , REC_TICKET_FRACTION      
        
        !-----------------------------------------------------------
        ! Update counters
        !-----------------------------------------------------------
        SOUPFM_REC.LINE_CNT(IDX_LN_EXPIRED_FILE) = 
     *         SOUPFM_REC.LINE_CNT(IDX_LN_EXPIRED_FILE) + 1 

        !-----------------------------------------------------------
        ! Update counters
        !-----------------------------------------------------------
        SOUPFM_REC.TRX_CNT(IDX_LN_EXPIRED_FILE) = 
     *          SOUPFM_REC.TRX_CNT(IDX_LN_EXPIRED_FILE) + 1 

        RETURN
19001   FORMAT(I9.9)
20101   FORMAT(
     *            A2                    ! Record type
     *          , I2.2                  ! Game code
     *          , I6.6                  ! Extraction name
     *          , I5.5                  ! Ticket
     *          , I2.2                  ! Ticket series
     *          , I2.2                  ! Ticket fraction
     *          )
        END







        SUBROUTINE CLOSE_FILE_SOUPFM_LN_EXPIRED(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4 ST
        
        INTEGER*4 YEAR,MONTH,DAY
        
        ST = 0
        
        IF(SOUPFM_REC.HANDLE_FILE(IDX_LN_EXPIRED_FILE) .EQ. .FALSE.) THEN
            RETURN
        ENDIF
        
        CALL PRINT_TRAILER_SOUPFM_LN_EXPIRED(SOUPFM_REC, ST)
        
        CALL USRCLOS1(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_FILE))

        CALL PRINT_REPORT_LINE(SOUPFM_REC, IDX_LN_EXPIRED_FILE)
        
        RETURN
        END
        






        SUBROUTINE PRINT_TRAILER_SOUPFM_LN_EXPIRED(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4 ST
        
        ST = 0
        
        ! Writing total number of records, including header and trailer
        WRITE(SOUPFM_REC.LUN_TABLE(IDX_LN_EXPIRED_FILE),20201) 
     *            'TP'
     *          , SOUPFM_REC.LINE_CNT(IDX_LN_EXPIRED_FILE) + 2 
        
        RETURN
20201   FORMAT(A2,I8.8,9(' '))
        END



