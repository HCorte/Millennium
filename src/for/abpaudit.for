C
C  GXSRC:ABPAUDIT.FOR
C
C V01 2015.06.11 SCML PLACARD PROJECT - IGS - Creation
C ----------------------------------------------------------------------
C ABP - Millennium audit program.
C ----------------------------------------------------------------------
C     This program will traverse the TMF file from time to time, looking
C for IGS wager and cancellation transactions, and will append them into
C a file to be exported for external auditing.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, West Greenwich, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM ABPAUDIT
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        INTEGER*4 I,CNT
        INTEGER*8 I8AUX,I8AUX2
        INTEGER*4 YYYY,MM,DD,HH,MI,SS
         
        RECORD /ABP_AUDIT_CONF/ CONF
        
        CALL SNIF_AND_WRKSET
        CALL OPSTXT(' Copyright 2015 SCML. All rights reserved. ')
        CALL OPSTXT(' ******************* ABPAUDIT ******************* ')
        
        ! Initializing structures
        CALL INIT_CONF(CONF)
        ! Loading data from configuration file
        CALL LOAD_CONFIG_FILE(CONF)

        ! Initializing data for start of day :
        !-------------------------------------
        ! Check if end-of-day or kill system have been performed
        ! if so, run in contingency mode (i.e. scan the entire TMF file)
        ! else run in normal mode until kilsys or end-of-day have been
        ! performed
        CALL SET_START_OF_DAY_CONF(CONF)
        ! Saving current configuration into file
        CALL SAVE_CONFIG_FILE(CONF)
        
        IF(CONF.FORCE_CONTINGENCY) THEN
            CALL RUN_IN_CONTINGENCY_MODE(CONF)
        ELSE
            CALL RUN_IN_NORMAL_MODE(CONF)
        ENDIF
        
        CALL GSTOP(GEXIT_SUCCESS)
        END


        SUBROUTINE RUN_IN_CONTINGENCY_MODE(CONF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        INTEGER*4 I,CNT 
        RECORD /ABP_AUDIT_CONF/ CONF

        CALL L_ALL(CONF,'[ABPAUDIT]: CONTINGENCY MODE - Start')

        CALL EVALUATE_CONDITIONS(CONF)

        CONF.TRX_SER = CONF.P_LAST_TRX_NR
        CNT = 0
        CALL OPEN_TMF(CONF)
        CALL OPEN_REPORT_FILE(CONF)
        CALL WRITE_REPORT_HEADER(CONF)
        
        CALL L_DEBUG(CONF,'[ABPAUDIT]:[CTG]: Begin TMF read cycle')
        DO WHILE(.NOT. CONF.EOT)
            CALL READ_TMF_TRX(CONF)
            CNT = CNT + 1
            IF(.NOT. CONF.EOT) THEN
                CALL WRITE_REPORT_TRX(CONF)
                CONF.P_LAST_TRX_NR = CONF.TRX_SER
            ENDIF
        ENDDO
        CALL L_DEBUG(CONF,'[ABPAUDIT]:[CTG]: End TMF read cycle')
        CALL GET_TIME_MS(CONF.P_LAST_TIME)
        
        CALL UPDATE_CHECKPOINT_TIME(CONF)
        
        CALL WRITE_REPORT_TRAILER(CONF)
        CALL CLOSE_TMF(CONF)
        CALL CLOSE_REPORT_FILE(CONF)
        CALL SAVE_CONFIG_FILE(CONF)
        
        CALL L_ALL(CONF,'[ABPAUDIT]: CONTINGENCY MODE - Exit')
        RETURN
        END




        SUBROUTINE RUN_IN_NORMAL_MODE(CONF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        INTEGER*4 I,CNT 
        RECORD /ABP_AUDIT_CONF/ CONF

        CALL L_ALL(CONF,'[ABPAUDIT]: NORMAL MODE - Start')

        CALL EVALUATE_CONDITIONS(CONF)

        CALL L_DEBUG(CONF,'[ABPAUDIT]:[NRM]: Begin normal read cycle: while not end-of-day')
        DO WHILE(.NOT. CONF.END_OF_DAY)
            ! Loading data from configuration file
            CALL LOAD_CONFIG_FILE(CONF)
            CONF.TRX_SER = CONF.P_LAST_TRX_NR
            CONF.REMAINING_CYCLE_TIME  = CONF.P_CYCLE_WAIT_TIME

            CNT = 0
            CALL OPEN_TMF(CONF)
            CALL OPEN_REPORT_FILE(CONF)
            CALL WRITE_REPORT_HEADER(CONF)
            
            CALL L_DEBUG(CONF,'[ABPAUDIT]:[NRM]: Begin while not end-of-turn and not EOF')
            DO WHILE(
     *            .NOT. CONF.EOT
     *      .AND. .NOT. CONF.END_OF_TURN
     *      )
                CALL READ_TMF_TRX(CONF)
                CALL EVALUATE_CONDITIONS(CONF)
                CNT = CNT + 1
                IF(
     *                .NOT. CONF.EOT
     *          .AND. .NOT. CONF.END_OF_TURN
     *          ) THEN
                    IF(   CONF.TMF_READ_SUCCESS
     *              .AND. CONF.TMF_REC_IN_RANGE .LE. 0
     *              ) THEN
                        ! Advances checkpoint if transactions have been made more than MIN_RANGE_TRX_TIME seconds ago
                        CONF.P_LAST_TRX_NR = CONF.TRX_SER
                        IF(CONF.TMF_REC_IN_RANGE .EQ. 0) THEN
                            CALL WRITE_REPORT_TRX(CONF)
                        ENDIF
                    ENDIF
                ENDIF
            ENDDO
            CALL L_DEBUG(CONF,'[ABPAUDIT]:[NRM]: End while not end-of-turn and not EOF')
            CALL GET_TIME_MS(CONF.P_LAST_TIME)

            CALL UPDATE_CHECKPOINT_TIME(CONF)

            CALL WRITE_REPORT_TRAILER(CONF)
            CALL CLOSE_TMF(CONF)
            CALL CLOSE_REPORT_FILE(CONF)
            CALL SAVE_CONFIG_FILE(CONF)
            
            DO WHILE(CONF.REMAINING_CYCLE_TIME .GT. 0 .AND. .NOT. CONF.END_OF_DAY)
                CALL LOG_I4(CONF,AA_LOG_LEVEL_DEBUG,'[NRM]:Seconds to next cycle',CONF.REMAINING_CYCLE_TIME)
                CALL L_DEBUG(CONF,'[NRM]: Waiting '// 
     *                       TRIM(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_WAIT_TIME)) //
     *                       ' seconds')
                ! Waiting P_WAIT_TIME seconds
                CALL XWAIT(CONF.P_WAIT_TIME,2,CONF.ST)
                CALL EVALUATE_CONDITIONS(CONF)
                CONF.REMAINING_CYCLE_TIME = CONF.REMAINING_CYCLE_TIME - CONF.P_WAIT_TIME
            ENDDO
        ENDDO
        CALL L_INFO(CONF,'[ABPAUDIT]: NORMAL MODE - After END-OF-DAY')
        CALL LOG_L(CONF,AA_LOG_LEVEL_TRACE,'CONF.END_OF_DAY',CONF.END_OF_DAY)
        IF(CONF.END_OF_DAY) THEN
            CALL L_ALL(CONF,'[ABPAUDIT]: NORMAL MODE - END-OF-DAY processing')
            ! Loading data from configuration file
            CALL LOAD_CONFIG_FILE(CONF)
            CONF.TRX_SER = CONF.P_LAST_TRX_NR
            CNT = 0
            CALL OPEN_TMF(CONF)
            CALL OPEN_REPORT_FILE(CONF)
            CALL WRITE_REPORT_HEADER(CONF)
            
            CALL L_DEBUG(CONF,'[ABPAUDIT]:[NRM]: Begin end-of-day processing')
            DO WHILE(
     *            .NOT. CONF.EOT
     *      )
                CALL READ_TMF_TRX(CONF)
                CALL EVALUATE_CONDITIONS(CONF)
                CNT = CNT + 1
                IF(
     *                .NOT. CONF.EOT
     *          ) THEN
                    IF(   CONF.TMF_READ_SUCCESS
     *              ) THEN
                        CONF.P_LAST_TRX_NR = CONF.TRX_SER
                        CALL WRITE_REPORT_TRX(CONF)
                    ENDIF
                ENDIF
            ENDDO
            CALL L_DEBUG(CONF,'[ABPAUDIT]:[NRM]: End end-of-day processing')
            CALL GET_TIME_MS(CONF.P_LAST_TIME)

            CALL UPDATE_CHECKPOINT_TIME(CONF)

            CALL WRITE_REPORT_TRAILER(CONF)
            CALL CLOSE_TMF(CONF)
            CALL CLOSE_REPORT_FILE(CONF)
            CALL SAVE_CONFIG_FILE(CONF)
        ENDIF

        CALL L_ALL(CONF,'[ABPAUDIT]: NORMAL MODE - Exit')
        RETURN
        END


        SUBROUTINE UPDATE_CHECKPOINT_TIME(CONF)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        INTEGER*8 I8LAST_SECOND_OF_DAY 

        CALL GET_UNIX_MS_FROM_CDC_TTIM(I8LAST_SECOND_OF_DAY, DAYCDC + 1, 0)
        I8LAST_SECOND_OF_DAY = I8LAST_SECOND_OF_DAY - KZEXT(1000) ! 23:59:59

        IF (CONF.STOPSYS) THEN
            CONF.CHECKPOINT_TIME_UX = I8LAST_SECOND_OF_DAY
        ELSEIF (CONF.NR_TOT_SUC_TRX .EQ. 0) THEN
            CONF.CHECKPOINT_TIME_UX = CONF.CHECKPOINT_TIME_UX  
     *          +   KZEXT(CONF.P_MIN_RANGE_TRX_TIME    * 1000)
     *          -   KZEXT(CONF.P_DELTA_CHECKPOINT_TIME * 1000)
            IF( CONF.CHECKPOINT_TIME_UX .GT. I8LAST_SECOND_OF_DAY ) THEN
                CONF.CHECKPOINT_TIME_UX = I8LAST_SECOND_OF_DAY
            ENDIF
        ENDIF

        CALL UNIX_TO_FORMATTED( CONF.CHECKPOINT_TIME_UX, CONF.P_CHECKPOINT_TIME )

        RETURN
        END

        SUBROUTINE CONVERT_FILENAME(CONF, INDEX)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        INTEGER*4 I, INDEX
        
        CHARACTER*128 LINE
        INTEGER*4 ILINE(32)
        EQUIVALENCE(LINE,ILINE)

        LINE = TRIM(ADJUSTL(CONF.CONF_PARAMS(AA_I_VALUE, INDEX)))

        DO I = 1, 32
            CONF.ICONV(I) = ILINE(I)
        ENDDO
        
        END

        SUBROUTINE OPEN_TMF(CONF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        INTEGER*4 I

        IF(CONF.LUN_TMF .EQ. 0) THEN
            CALL FIND_AVAILABLE_LUN(CONF.LUN_TMF,CONF.ST)
        ENDIF
        CALL CONVERT_FILENAME(CONF, AA_CI_SRC_TMF_FILE)
        CALL OPENWY(CONF.LUN_TMF
     *             ,CONF.ICONV
     *             ,0,4,0,CONF.ST)
        CALL TOPEN(CONF.LUN_TMF)

        IF(CONF.ST .NE. 0) THEN
            CALL FILERR(CONF.ICONV,1,CONF.ST,0)
            CALL GSTOP(GEXIT_FATAL)
        ENDIF
        CONF.EOT = .FALSE.

        END



        SUBROUTINE READ_TMF_TRX(CONF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        INTEGER*4 I


        CONF.TMF_READ_SUCCESS      = .FALSE.

        CALL FASTSET(0,CONF.TRABUF,TRALEN)
        CALL FASTSET(0,CONF.LOGREC,LREC*3)

        CALL LOG_I4(CONF,AA_LOG_LEVEL_TRACE,'ReadTmfTrx:CONF.TRX_SER',CONF.TRX_SER)
        CALL READTMF(CONF.LOGREC,CONF.TRX_SER,CONF.EOT)
        CONF.ST = 0
        CALL LOG_L(CONF,AA_LOG_LEVEL_TRACE,'ReadTmfTrx:CONF.EOT',CONF.EOT)
        IF( CONF.EOT ) THEN
            CONF.ST = 1 
            RETURN
        ENDIF
        CALL LOGTRA(CONF.TRABUF,CONF.LOGREC)
        CONF.TMF_READ_SUCCESS      = .TRUE.
        CALL LOG_I4(CONF,AA_LOG_LEVEL_TRACE,'ReadTmfTrx:CONF.TRABUF(TSER)',CONF.TRABUF(TSER))
        END


        SUBROUTINE CLOSE_TMF(CONF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF

        CLOSE(CONF.LUN_TMF, IOSTAT = CONF.ST)
        IF(CONF.ST .NE. 0) THEN
            CALL L_ERROR(CONF,TRIM(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_SRC_TMF_FILE)) //
     *                   ' close error')
            RETURN
        ENDIF
        
        END


        SUBROUTINE INIT_CONF (CONF)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        INTEGER*4 I, LEN
        
        CALL L_DEBUG(CONF,'[ABPAUDIT]:Initializing CONF structure')
        DO I = 1, AA_MAX_NR_PARAMS
            CONF.CONF_PARAMS(AA_I_NAME ,I) = TRIM(AA_CONF_NAME(I))
            CONF.CONF_PARAMS(AA_I_VALUE,I) = TRIM(AA_CONF_DEF_VALUE(I))
        ENDDO

        CONF.NR_SUC_WAGER_TRX      = 0
        CONF.NR_SUC_CANCEL_TRX     = 0
        CONF.NR_TOT_WAGER_TRX      = 0
        CONF.NR_TOT_CANCEL_TRX     = 0
        CONF.NR_ANALYZED_TRX       = 0
        CONF.NR_TOT_SUC_TRX        = 0
        CONF.FORCE_CONTINGENCY     = .FALSE.
        CONF.STOPSYS               = .FALSE.
        CONF.START_OF_DAY          = .FALSE.
        CONF.END_OF_DAY            = .FALSE.
        CONF.END_OF_TURN           = .FALSE.
        CONF.LAST_TIME_TODAY       = .FALSE.
        CONF.TMF_READ_SUCCESS      = .FALSE.
        CONF.TMF_REC_IN_RANGE      = 0
        CONF.LUN_TMF               = 0
        CONF.LUN_OUTPUT            = 0
        CONF.LUN_CONF              = 0
        CONF.ST                    = 0
        CONF.EOT                   = .FALSE.
        CONF.TRX_SER               = 0
        CONF.REMAINING_CYCLE_TIME  = 0
        CALL FASTSET(0,CONF.TRABUF,TRALEN)
        CALL FASTSET(0,CONF.ICONV,32)
        CALL FASTSET(0,CONF.LOGREC,LREC*3)
        CALL FASTSET(0,CONF.OUTPUT_FILENAME,32)

        CALL UPDATE_CONF(CONF)

        CONF.REMAINING_CYCLE_TIME  = CONF.P_CYCLE_WAIT_TIME
        
        CALL FORMATTED_TO_UNIX(CONF.P_CHECKPOINT_TIME,CONF.CHECKPOINT_TIME_UX)
        RETURN
        
        END


        SUBROUTINE READ_I8_FROM_CONSOLE(PROMPT, I8NUM)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'
        
        CHARACTER*64 LINE
        CHARACTER*(*) PROMPT
        INTEGER*8 I8NUM
        INTEGER*4 LEN
        
        TYPE *, IAM(), PROMPT
        READ(5, 900) LINE
900     FORMAT(A)
        I8NUM = CTOI8(TRIM(LINE),LEN)
        RETURN
        END



        SUBROUTINE EVALUATE_CONDITIONS (CONF)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        
        INTEGER*4 CURRENT_TIME_S, DELTA_S
        CHARACTER*65 LINE

        CALL L_TRACE(CONF,'Start EVALUATE_CONDITIONS:')
        CONF.STOPSYS          = .FALSE.
        CONF.START_OF_DAY     = .FALSE.
        CONF.END_OF_DAY       = .FALSE.
        CONF.END_OF_TURN      = .FALSE.
        CONF.LAST_TIME_TODAY  = .TRUE.
        CONF.TMF_REC_IN_RANGE = 0

        CALL L_TRACE(CONF, 'Before evaluating conditions :')
        CALL LOG_L(CONF,AA_LOG_LEVEL_TRACE,'STOPSYS',CONF.STOPSYS)
        CALL LOG_L(CONF,AA_LOG_LEVEL_TRACE,'START_OF_DAY',CONF.START_OF_DAY)
        CALL LOG_L(CONF,AA_LOG_LEVEL_TRACE,'END_OF_DAY',CONF.END_OF_DAY)
        CALL LOG_L(CONF,AA_LOG_LEVEL_TRACE,'END_OF_TURN',CONF.END_OF_TURN)
        CALL LOG_L(CONF,AA_LOG_LEVEL_TRACE,'LAST_TIME_TODAY',CONF.LAST_TIME_TODAY)
        CALL LOG_L(CONF,AA_LOG_LEVEL_TRACE,'TMF_REC_IN_RANGE',CONF.TMF_REC_IN_RANGE)

        CALL IS_LAST_TIME_TODAY(CONF)
        CALL LOG_I4(CONF,AA_LOG_LEVEL_TRACE,'DAYSTS',DAYSTS)
        CALL LOG_I4(CONF,AA_LOG_LEVEL_TRACE,':DSOPEN',DSOPEN)
        CALL LOG_I4(CONF,AA_LOG_LEVEL_TRACE,':DSSUSP',DSSUSP)
        CALL LOG_I4(CONF,AA_LOG_LEVEL_TRACE,':DSCLOS',DSCLOS)
        CALL LOG_I4(CONF,AA_LOG_LEVEL_TRACE,'DAYCDC',DAYCDC)
        
        IF(   DAYCDC .NE. CONF.P_LAST_CDC
     *  .OR.  DAYCDC .EQ. 0
     *  ) THEN
            CONF.START_OF_DAY = .TRUE.
        ENDIF

        
        IF(   DAYSTS .NE. DSOPEN
     *  .AND. DAYSTS .NE. DSSUSP
     *  ) THEN
            CONF.END_OF_DAY = .TRUE.
        ENDIF

        IF(   DAYSTS .EQ. DSCLOS
     *  ) THEN
            CONF.STOPSYS = .TRUE.
        ENDIF


        CALL GET_TIME_SEC(CURRENT_TIME_S)

        IF(   CONF.TRX_SER .NE. 0 
     *  .AND. CONF.TMF_READ_SUCCESS 
     *  .AND. .NOT. CONF.EOT
     *  ) THEN
            DELTA_S = CURRENT_TIME_S - CONF.TRABUF(TTIM)
            CALL LOG_I4(CONF,AA_LOG_LEVEL_TRACE,'CONF.TRX_SER',CONF.TRX_SER)
            CALL LOG_I4(CONF,AA_LOG_LEVEL_TRACE,'CONF.TRABUF(TSER)',CONF.TRABUF(TSER))
            CALL LOG_I4(CONF,AA_LOG_LEVEL_TRACE,'CURRENT_TIME_S',CURRENT_TIME_S)
            CALL LOG_I4(CONF,AA_LOG_LEVEL_TRACE,'CONF.TRABUF(TTIM)',CONF.TRABUF(TTIM))
            CALL LOG_I4(CONF,AA_LOG_LEVEL_TRACE,'DELTA_S',DELTA_S)
            CALL LOG_I4(CONF,AA_LOG_LEVEL_TRACE,'CONF.P_MIN_RANGE_TRX_TIME',CONF.P_MIN_RANGE_TRX_TIME)
            IF(   DELTA_S .LT. CONF.P_MIN_RANGE_TRX_TIME ) THEN
                CONF.TMF_REC_IN_RANGE = 1
            ENDIF
            IF(   DELTA_S .LT. CONF.P_MIN_RANGE_TRX_TIME
     *      .AND. DELTA_S .GE. 0 ) THEN ! Only transactions started today
                CONF.END_OF_TURN = .TRUE.
            ENDIF
        ENDIF

        CALL L_TRACE(CONF, 'After evaluating conditions :')
        CALL LOG_L(CONF,AA_LOG_LEVEL_TRACE,'STOPSYS',CONF.STOPSYS)
        CALL LOG_L(CONF,AA_LOG_LEVEL_TRACE,'START_OF_DAY',CONF.START_OF_DAY)
        CALL LOG_L(CONF,AA_LOG_LEVEL_TRACE,'END_OF_DAY',CONF.END_OF_DAY)
        CALL LOG_L(CONF,AA_LOG_LEVEL_TRACE,'END_OF_TURN',CONF.END_OF_TURN)
        CALL LOG_L(CONF,AA_LOG_LEVEL_TRACE,'LAST_TIME_TODAY',CONF.LAST_TIME_TODAY)
        CALL LOG_I4(CONF,AA_LOG_LEVEL_TRACE,'TMF_REC_IN_RANGE',CONF.TMF_REC_IN_RANGE)

        CALL L_TRACE(CONF,'End EVALUATE_CONDITIONS!')
        RETURN
        END
        
        SUBROUTINE SET_START_OF_DAY_CONF (CONF)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        
        CALL EVALUATE_CONDITIONS(CONF)
        
        IF( CONF.START_OF_DAY .OR. .NOT. CONF.LAST_TIME_TODAY ) THEN
            CONF.P_LAST_TRX_NR = 0
            WRITE(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_LAST_TRX_NR),901) CONF.P_LAST_TRX_NR
            
            CONF.P_LAST_CDC = DAYCDC
            WRITE(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_LAST_CDC),901) CONF.P_LAST_CDC

            CALL GET_UNIX_MS_FROM_CDC_TTIM(CONF.CHECKPOINT_TIME_UX, DAYCDC, 0)
            CALL UNIX_TO_FORMATTED(CONF.CHECKPOINT_TIME_UX,CONF.P_CHECKPOINT_TIME)
            WRITE(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CHECKPOINT_TIME),901) CONF.P_CHECKPOINT_TIME
            
901         FORMAT(I0)
        ENDIF
        
        IF(  CONF.P_FORCE_CONTINGENCY
     *  .OR. CONF.END_OF_DAY
     *  ) THEN
            CONF.FORCE_CONTINGENCY = .TRUE.
        ENDIF
        
        END


        SUBROUTINE DUMP_CONF_PARAMS(CONF)
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF

        CHARACTER*65 LINE

        CALL L_DEBUG(CONF,'Dumping configuration parameters:')
        CALL LOG_I4(CONF,AA_LOG_LEVEL_DEBUG,' Min.Range Trx Time (s)',CONF.P_MIN_RANGE_TRX_TIME)
        CALL LOG_I4(CONF,AA_LOG_LEVEL_DEBUG,'          Wait Time (s)',CONF.P_WAIT_TIME)
        CALL LOG_I4(CONF,AA_LOG_LEVEL_DEBUG,'    Cycle Wait Time (s)',CONF.P_CYCLE_WAIT_TIME)
        CALL LOG_I4(CONF,AA_LOG_LEVEL_DEBUG,'            Last Trx Nr',CONF.P_LAST_TRX_NR)
        CALL LOG_L (CONF,AA_LOG_LEVEL_DEBUG,'      Force Contingency',CONF.P_FORCE_CONTINGENCY)
        CALL LOG_I8(CONF,AA_LOG_LEVEL_DEBUG,'         Last Time (ms)',CONF.P_LAST_TIME)
        CALL LOG_I4(CONF,AA_LOG_LEVEL_DEBUG,'              Log Level',CONF.P_LOG_LEVEL)
        CALL LOG_L (CONF,AA_LOG_LEVEL_DEBUG,'  Print Only Good Trxs.',CONF.P_PRINT_ONLY_GOOD_TRX)
        CALL LOG_L (CONF,AA_LOG_LEVEL_DEBUG,'     Single Output File',CONF.P_SINGLE_OUTPUT_FILE)
        CALL LOG_S (CONF,AA_LOG_LEVEL_DEBUG,'        Output Filename',CONF.OUTPUT_FILENAME)
        CALL LOG_I8(CONF,AA_LOG_LEVEL_DEBUG,'    Checkpoint time (s)',CONF.P_CHECKPOINT_TIME)
        CALL LOG_I4(CONF,AA_LOG_LEVEL_DEBUG,'               Last CDC',CONF.P_LAST_CDC)

        RETURN 
        END


        SUBROUTINE UPDATE_CONF (CONF)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        INTEGER*4 I, LEN
        INTEGER*8 TS
        
        CONF.NR_SUC_WAGER_TRX      = 0
        CONF.NR_SUC_CANCEL_TRX     = 0
        CONF.NR_TOT_WAGER_TRX      = 0
        CONF.NR_TOT_CANCEL_TRX     = 0
        CONF.NR_ANALYZED_TRX       = 0
        CONF.NR_TOT_SUC_TRX        = 0
        
        CONF.P_MIN_RANGE_TRX_TIME = CTOI(TRIM(
     *    CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_MIN_RANGE_TRX_TIME)),LEN)

        CONF.P_WAIT_TIME           = CTOI(TRIM(
     *    CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_WAIT_TIME)),LEN)

        CONF.P_CYCLE_WAIT_TIME           = CTOI(TRIM(
     *    CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CYCLE_WAIT_TIME)),LEN)

        CONF.P_LAST_TRX_NR         = CTOI(TRIM(
     *    CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_LAST_TRX_NR)),LEN)

        IF(  TRIM(ADJUSTL(CONF.CONF_PARAMS(AA_I_VALUE
     *                   ,AA_CI_FORCE_CONTINGENCY))) .EQ. 'Y'
     *  .OR. TRIM(ADJUSTL(CONF.CONF_PARAMS(AA_I_VALUE
     *                   ,AA_CI_FORCE_CONTINGENCY))) .EQ. 'y') THEN
            CONF.P_FORCE_CONTINGENCY   = .TRUE. 
        ELSE
            CONF.P_FORCE_CONTINGENCY   = .FALSE. 
        ENDIF

        CONF.P_LAST_TIME           = CTOI8(TRIM(
     *    CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_LAST_TIME)),LEN)

        CONF.P_LOG_LEVEL           = CTOI(TRIM(
     *    CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_LOG_LEVEL)),LEN)

        IF(  TRIM(ADJUSTL(CONF.CONF_PARAMS(AA_I_VALUE
     *                   ,AA_CI_PRINT_ONLY_GOOD_TRX))) .EQ. 'N'
     *  .OR. TRIM(ADJUSTL(CONF.CONF_PARAMS(AA_I_VALUE
     *                   ,AA_CI_PRINT_ONLY_GOOD_TRX))) .EQ. 'n') THEN
            CONF.P_PRINT_ONLY_GOOD_TRX = .FALSE. 
        ELSE
            CONF.P_PRINT_ONLY_GOOD_TRX = .TRUE. 
        ENDIF

        IF(  TRIM(ADJUSTL(CONF.CONF_PARAMS(AA_I_VALUE
     *                   ,AA_CI_SINGLE_OUTPUT_FILE))) .EQ. 'N'
     *  .OR. TRIM(ADJUSTL(CONF.CONF_PARAMS(AA_I_VALUE
     *                   ,AA_CI_SINGLE_OUTPUT_FILE))) .EQ. 'n') THEN
            CONF.P_SINGLE_OUTPUT_FILE = .FALSE. 
        ELSE
            CONF.P_SINGLE_OUTPUT_FILE = .TRUE. 
        ENDIF


        IF(.NOT. CONF.P_SINGLE_OUTPUT_FILE) THEN
            CALL GET_TIME_MS(TS)
            TS = TS / KZEXT(1000)
            
            WRITE(CONF.OUTPUT_FILENAME,901) TRIM(ADJUSTL(CONF.CONF_PARAMS(AA_I_VALUE
     *                                          ,AA_CI_OUTPUT_FILE_PREFIX)))
     *                                    , TS
     *                                    , TRIM(ADJUSTL(CONF.CONF_PARAMS(AA_I_VALUE
     *                                          ,AA_CI_OUTPUT_FILE_SUFFIX)))
901         FORMAT(A,'_',I0,'.',A)
        ELSE
            WRITE(CONF.OUTPUT_FILENAME,902) TRIM(ADJUSTL(CONF.CONF_PARAMS(AA_I_VALUE
     *                                          ,AA_CI_OUTPUT_FILE_PREFIX)))
     *                                    , TRIM(ADJUSTL(CONF.CONF_PARAMS(AA_I_VALUE
     *                                          ,AA_CI_OUTPUT_FILE_SUFFIX)))
902         FORMAT(A,'.',A)
        ENDIF

        CONF.P_CHECKPOINT_TIME         = CTOI8(TRIM(
     *    CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CHECKPOINT_TIME)),LEN)
     
        CALL FORMATTED_TO_UNIX(CONF.P_CHECKPOINT_TIME, CONF.CHECKPOINT_TIME_UX)
        CONF.P_DELTA_CHECKPOINT_TIME   = CTOI(TRIM(
     *    CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_DELTA_CHECKPOINT_TIME)),LEN)

        CONF.P_LAST_CDC                = CTOI(TRIM(
     *    CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_LAST_CDC)),LEN)
        
        CALL DUMP_CONF_PARAMS(CONF)

        END



        SUBROUTINE UPDATE_TO_WRITE_CONF (CONF)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF

        WRITE(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_LAST_TRX_NR),901) CONF.P_LAST_TRX_NR
        WRITE(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_LAST_TIME),901) CONF.P_LAST_TIME
        CALL UNIX_TO_FORMATTED(CONF.CHECKPOINT_TIME_UX,CONF.P_CHECKPOINT_TIME)
        WRITE(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CHECKPOINT_TIME),901) CONF.P_CHECKPOINT_TIME
901     FORMAT(I0)

        END

        SUBROUTINE OPEN_REPORT_FILE(CONF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        
        CHARACTER*1024 LINE
        
        LOGICAL FILE_EXISTS, EOF
        INTEGER*4 I

        CONF.ST = 0
        IF(CONF.LUN_OUTPUT .EQ. 0) THEN
            CALL FIND_AVAILABLE_LUN(CONF.LUN_OUTPUT,CONF.ST)
            IF(CONF.ST .NE. 0) CALL GSTOP(GEXIT_FATAL)
        ENDIF

        OPEN(UNIT = CONF.LUN_OUTPUT, 
     *       FILE = TRIM(CONF.OUTPUT_FILENAME), 
     *       ACCESS = 'APPEND',
     *       STATUS = 'UNKNOWN',
     *       RECL = 1024, 
     *       IOSTAT = CONF.ST)

        IF(CONF.ST .NE. 0) THEN
            CALL L_ERROR(CONF,TRIM(CONF.OUTPUT_FILENAME) //
     *                   ' open error')
            RETURN
        ENDIF
        
        CALL L_ALL(CONF,TRIM('Writing data to file ' // TRIM(CONF.OUTPUT_FILENAME)))
        END

        SUBROUTINE WRITE_REPORT_HEADER(CONF)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        
        CHARACTER*197 LINE
        INTEGER*4 I
        CHARACTER*32 TIMESTAMP
        CHARACTER*19 CHECKPOINT_TS
        
        WRITE(CONF.LUN_OUTPUT, 901) 
901     FORMAT(201('#'))
902     FORMAT('# ',A,' #')
913     FORMAT(A)
914     FORMAT(A,I19,A)
9141    FORMAT(A,A19,A)
915     FORMAT('#TType|Tr. Ser.|Tr.TSt|TCDC|TTerm|Tr.Agent'
     *       , '|Tr. Msg. ID  |BRDate|BG|BR Serial |BCh|Bet Un.St'
     *       , '|TBt|Tot.Stake|MaxPosRet|LastEvDt'
     *       , '|Bt.Cr.Dt|BtCrTm'
     *       , '|NIF      '
     *       , '|CRDate|CG|CR Serial |CCh'
     *       , '|Cn.Cr.Dt|CnCrTm|Can.Amnt#')
        CALL GET_TIMESTAMP(TIMESTAMP)

        WRITE(LINE,914)  '[START] : Writing transactions [FROM] checkpoint number [', CONF.P_LAST_TRX_NR, ']'
        WRITE(CONF.LUN_OUTPUT, 902) LINE
        WRITE(LINE,913) TRIM('[START] : By ABPAUDIT on ' // TRIM(TIMESTAMP))
        WRITE(CONF.LUN_OUTPUT, 902) LINE
        CALL FORMATTED_TO_CHAR( CONF.P_CHECKPOINT_TIME, CHECKPOINT_TS )
        WRITE(LINE,9141) '[START] : Writing transactions [FROM] checkpoint time   [', CHECKPOINT_TS, ']'
        WRITE(CONF.LUN_OUTPUT, 902) LINE
        WRITE(CONF.LUN_OUTPUT, 901) 
        WRITE(CONF.LUN_OUTPUT, 915) 
        WRITE(CONF.LUN_OUTPUT, 901) 

        RETURN

        END


        SUBROUTINE WRITE_REPORT_TRX(CONF)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        
        CALL WRITE_IF_TRX_IS_WAGER(CONF)
        CALL WRITE_IF_TRX_IS_CANCEL(CONF)

        CONF.NR_ANALYZED_TRX = CONF.NR_ANALYZED_TRX + 1
        
        RETURN

        END


        SUBROUTINE WRITE_IF_TRX_IS_WAGER(CONF)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        
        CHARACTER*201 LINE

        INTEGER*8 EXTSER,MSGID
        INTEGER*8 I8TEMP
        INTEGER*4 I4TEMP(2)
        INTEGER*2 I2TEMP(4)
        INTEGER*1 I1TEMP(8)
        EQUIVALENCE(I8TEMP,I4TEMP,I2TEMP,I1TEMP)
        
        INTEGER*4 I4AUX
        
        LOGICAL TO_PRINT, IS_TYPE_OK, IS_GOOD
        
        TO_PRINT = .FALSE.
        IS_TYPE_OK = .FALSE.
        IS_GOOD = .FALSE.
        
        ! Only print transactions that are:
        IF(   CONF.TRABUF(TTYP)      .EQ. TIGS   ! Of IGS type
     *  .AND. CONF.TRABUF(TIGS_TTYP) .EQ. IGSWAG ! Wagers
     *  ) THEN
            CONF.NR_TOT_WAGER_TRX = CONF.NR_TOT_WAGER_TRX + 1  
            IS_TYPE_OK = .TRUE.
            
            IF(   CONF.TRABUF(TSTAT)     .EQ. GOOD   ! Good
     *      .AND. CONF.TRABUF(TERR)      .EQ. NOER   ! Without errors
     *      ) THEN
                CONF.NR_SUC_WAGER_TRX = CONF.NR_SUC_WAGER_TRX + 1  
                IS_GOOD = .TRUE.
            ENDIF

            IF(.NOT. CONF.P_PRINT_ONLY_GOOD_TRX) THEN
                IS_GOOD = .TRUE.
            ENDIF

        ENDIF

        IF(   IS_TYPE_OK 
     *  .AND. IS_GOOD
     *  ) THEN
            TO_PRINT = .TRUE.
        ENDIF
        
        IF(.NOT. TO_PRINT) THEN
            RETURN
        ENDIF
        
        CONF.NR_TOT_SUC_TRX = CONF.NR_TOT_SUC_TRX + 1
        
        CALL LOG_I4(CONF,AA_LOG_LEVEL_INFO,'Printing [WAGER ] trx',CONF.TRABUF(TSER))

        CALL GET_UNIX_MS_FROM_CDC_TTIM(CONF.CHECKPOINT_TIME_UX, CONF.TRABUF(TCDC), CONF.TRABUF(TTIM))
        CALL UNIX_TO_FORMATTED( CONF.CHECKPOINT_TIME_UX, CONF.P_CHECKPOINT_TIME )
        
        I4TEMP(2) = CONF.TRABUF(TIGSW_MIDH)
        I4TEMP(1) = CONF.TRABUF(TIGSW_MIDL)
        MSGID = I8TEMP
        I4TEMP(2) = CONF.TRABUF(TIGSW_WRSH)
        I4TEMP(1) = CONF.TRABUF(TIGSW_WRSL)
        EXTSER = I8TEMP
        CALL FORMAT_TIME_SEC(I4AUX,CONF.TRABUF(TTIM))
                                                                 ! Type: Wager                                
        WRITE(LINE,901)             CONF.TRABUF(TSER)            ! Trx Serial                                 
     *                            , I4AUX                        ! Trx Timestamp (HHMISS)                   
     *                            , CONF.TRABUF(TCDC)            ! Trx CDC                                    
     *                            , CONF.TRABUF(TTER)            ! Trx Terminal Number                        
     *                            , CONF.TRABUF(TAGT)            ! Trx Agent Number                           
     *                            , MSGID                        ! Trx Message ID                             
     *                            , CONF.TRABUF(TIGSW_WRDY)      ! Trx Betslip: Bet reference date (yymmdd)   
     *                            , CONF.TRABUF(TIGSW_WRDM)      ! Trx Betslip: Bet reference date (yymmdd)   
     *                            , CONF.TRABUF(TIGSW_WRDD)      ! Trx Betslip: Bet reference date (yymmdd)   
     *                            , CONF.TRABUF(TIGSW_WRGM)      ! Trx Betslip: Bet reference game            
     *                            , EXTSER                       ! Trx Betslip: Bet reference serial number   
     *                            , CONF.TRABUF(TIGSW_WRCD)      ! Trx Betslip: Bet reference check-digits    
     *                            , CONF.TRABUF(TIGSW_USTK)      ! Trx Bet Unit Stake                         
     *                            , CONF.TRABUF(TIGSW_TBET)      ! Trx Total Bets                             
     *                            , CONF.TRABUF(TIGSW_TSTK)      ! Trx Bet Total Stake                        
     *                            , CONF.TRABUF(TIGSW_MAXR)      ! Trx Bet Maximum Possible Returns           
     *                            , CONF.TRABUF(TIGSW_LEDY)      ! Trx Bet last event date (yymmdd)           
     *                            , CONF.TRABUF(TIGSW_LEDM)      ! Trx Bet last event date (yymmdd)           
     *                            , CONF.TRABUF(TIGSW_LEDD)      ! Trx Bet last event date (yymmdd)           
     *                            , CONF.TRABUF(TIGSW_WCDY)      ! Trx Bet creation date (yyyymmdd)           
     *                            , CONF.TRABUF(TIGSW_WCDM)      ! Trx Bet creation date (yyyymmdd)           
     *                            , CONF.TRABUF(TIGSW_WCDD)      ! Trx Bet creation date (yyyymmdd)           
     *                            , CONF.TRABUF(TIGSW_WCTH)      ! Trx Bet creation time (hhmiss)             
     *                            , CONF.TRABUF(TIGSW_WCTM)      ! Trx Bet creation time (hhmiss)             
     *                            , CONF.TRABUF(TIGSW_WCTS)      ! Trx Bet creation time (hhmiss)             
     *                            , CONF.TRABUF(TIGSW_PNIF)      ! Trx Bet: Player NIF                        
     *                            , 0                            ! Trx Betslip: Cancel reference date (yymmdd)
     *                            , 0                            ! Trx Betslip: Cancel reference date (yymmdd)
     *                            , 0                            ! Trx Betslip: Cancel reference date (yymmdd)
     *                            , 0                            ! Trx Betslip: Cancel reference game         
     *                            , 0                            ! Trx Betslip: Cancel reference serial number
     *                            , 0                            ! Trx Betslip: Cancel reference check-digits 
     *                            , 0                            ! Trx Cancel Date (yyyymmdd)                 
     *                            , 0                            ! Trx Cancel Date (yyyymmdd)                 
     *                            , 0                            ! Trx Cancel Date (yyyymmdd)                 
     *                            , 0                            ! Trx Cancel Time (hhmiss)                   
     *                            , 0                            ! Trx Cancel Time (hhmiss)                   
     *                            , 0                            ! Trx Cancel Time (hhmiss)                   
     *                            , 0                            ! Trx Cancel Amount (wager units)            
        WRITE(CONF.LUN_OUTPUT,900) LINE
900     FORMAT(A)
901     FORMAT(     'WAGER '               ! Trx Type
     *         ,'|',I8.8                   ! Trx Serial
     *         ,'|',I6.6                   ! Trx Timestamp (HHMISS)
     *         ,'|',I4.4                   ! Trx CDC
     *         ,'|',I5.5                   ! Trx Terminal Number
     *         ,'|',I8.8                   ! Trx Agent Number
     *         ,'|',I13.13                 ! Trx Message ID
     *         ,'|',I2.2,I2.2,I2.2         ! Trx Betslip: Bet reference date (yymmdd)
     *         ,'|',I2.2                   ! Trx Betslip: Bet reference game
     *         ,'|',I10.10                 ! Trx Betslip: Bet reference serial number
     *         ,'|',I3.3                   ! Trx Betslip: Bet reference check-digits
     *         ,'|',I9.9                   ! Trx Bet Unit Stake
     *         ,'|',I3.3                   ! Trx Total Bets
     *         ,'|',I9.9                   ! Trx Bet Total Stake
     *         ,'|',I9.9                   ! Trx Bet Maximum Possible Returns
     *         ,'|',I4.4,I2.2,I2.2         ! Trx Bet last event date (yymmdd)
     *         ,'|',I4.4,I2.2,I2.2         ! Trx Bet creation date (yyyymmdd)
     *         ,'|',I2.2,I2.2,I2.2         ! Trx Bet creation time (hhmiss)
     *         ,'|',I9.9                   ! Trx Bet: Player NIF
     *         ,'|',I2.2,I2.2,I2.2         ! Trx Betslip: Cancel reference date (yymmdd)
     *         ,'|',I2.2                   ! Trx Betslip: Cancel reference game
     *         ,'|',I10.10                 ! Trx Betslip: Cancel reference serial number
     *         ,'|',I3.3                   ! Trx Betslip: Cancel reference check-digits
     *         ,'|',I4.4,I2.2,I2.2         ! Trx Wager Cancel Date (yyyymmdd)
     *         ,'|',I2.2,I2.2,I2.2         ! Trx Wager Cancel Time (hhmiss)
     *         ,'|',I9.9                   ! Trx Cancel Amount (wager units)
     *  )

        RETURN
        END


        SUBROUTINE WRITE_IF_TRX_IS_CANCEL(CONF)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        
        CHARACTER*201 LINE

        INTEGER*8 EXTSER,WAGEXTSER,MSGID
        INTEGER*8 I8TEMP
        INTEGER*4 I4TEMP(2)
        INTEGER*2 I2TEMP(4)
        INTEGER*1 I1TEMP(8)
        EQUIVALENCE(I8TEMP,I4TEMP,I2TEMP,I1TEMP)

        INTEGER*4 I4AUX

        LOGICAL TO_PRINT, IS_TYPE_OK, IS_GOOD
        
        TO_PRINT = .FALSE.
        IS_TYPE_OK = .FALSE.
        IS_GOOD = .FALSE.

        ! Only print transactions that are:
        IF(   CONF.TRABUF(TTYP)      .EQ. TIGS   ! Of IGS type
     *  .AND. CONF.TRABUF(TIGS_TTYP) .EQ. IGSCAN ! Cancels
     *  ) THEN
            CONF.NR_TOT_CANCEL_TRX = CONF.NR_TOT_CANCEL_TRX + 1  
            IS_TYPE_OK = .TRUE.

            IF(   CONF.TRABUF(TSTAT)     .EQ. GOOD   ! Good
     *      .AND. CONF.TRABUF(TERR)      .EQ. NOER   ! Without errors
     *      ) THEN
                CONF.NR_SUC_CANCEL_TRX = CONF.NR_SUC_CANCEL_TRX + 1  
                IS_GOOD = .TRUE.
            ENDIF
            
            IF(.NOT. CONF.P_PRINT_ONLY_GOOD_TRX) THEN
                IS_GOOD = .TRUE.
            ENDIF
        ENDIF
        
        IF(   IS_TYPE_OK 
     *  .AND. IS_GOOD
     *  ) THEN
            TO_PRINT = .TRUE.
        ENDIF
        
        IF(.NOT. TO_PRINT) THEN
            RETURN
        ENDIF

        CONF.NR_TOT_SUC_TRX = CONF.NR_TOT_SUC_TRX + 1

        CALL LOG_I4(CONF,AA_LOG_LEVEL_INFO,'Printing [CANCEL] trx',CONF.TRABUF(TSER))

        CALL GET_UNIX_MS_FROM_CDC_TTIM(CONF.CHECKPOINT_TIME_UX, CONF.TRABUF(TCDC), CONF.TRABUF(TTIM))
        CALL UNIX_TO_FORMATTED( CONF.CHECKPOINT_TIME_UX, CONF.P_CHECKPOINT_TIME )

        I4TEMP(2) = CONF.TRABUF(TIGSC_MIDH)
        I4TEMP(1) = CONF.TRABUF(TIGSC_MIDL)
        MSGID = I8TEMP
        I4TEMP(2) = CONF.TRABUF(TIGSC_CRSH)
        I4TEMP(1) = CONF.TRABUF(TIGSC_CRSL)
        EXTSER = I8TEMP
        I4TEMP(2) = CONF.TRABUF(TIGSC_WRSH)
        I4TEMP(1) = CONF.TRABUF(TIGSC_WRSL)
        WAGEXTSER = I8TEMP
        CALL FORMAT_TIME_SEC(I4AUX,CONF.TRABUF(TTIM))
                                                                ! Type: Cancel                            
        WRITE(LINE, 901)            CONF.TRABUF(TSER)           ! Trx Serial                              
     *                            , I4AUX                       ! Trx Timestamp (HHMISS)                
     *                            , CONF.TRABUF(TCDC)           ! Trx CDC                                 
     *                            , CONF.TRABUF(TTER)           ! Trx Terminal Number                     
     *                            , CONF.TRABUF(TAGT)           ! Trx Agent Number                        
     *                            , MSGID                       ! Trx Message ID                          
     *                            , CONF.TRABUF(TIGSC_WRDY)     ! Trx Betslip: Bet reference date (yymmdd)
     *                            , CONF.TRABUF(TIGSC_WRDM)     ! Trx Betslip: Bet reference date (yymmdd)
     *                            , CONF.TRABUF(TIGSC_WRDD)     ! Trx Betslip: Bet reference date (yymmdd)
     *                            , CONF.TRABUF(TIGSC_WRGM)     ! Trx Betslip: Bet reference game         
     *                            , WAGEXTSER                   ! Trx Betslip: Bet reference serial number
     *                            , CONF.TRABUF(TIGSC_WRCD)     ! Trx Betslip: Bet reference check-digits 
     *                            , 0                           ! Trx Bet Unit Stake                      
     *                            , 0                           ! Trx Total Bets                          
     *                            , 0                           ! Trx Bet Total Stake                     
     *                            , 0                           ! Trx Bet Maximum Possible Returns        
     *                            , 0                           ! Trx Bet last event date (yymmdd)        
     *                            , 0                           ! Trx Bet last event date (yymmdd)        
     *                            , 0                           ! Trx Bet last event date (yymmdd)        
     *                            , 0                           ! Trx Bet creation date (yyyymmdd)        
     *                            , 0                           ! Trx Bet creation date (yyyymmdd)        
     *                            , 0                           ! Trx Bet creation date (yyyymmdd)        
     *                            , 0                           ! Trx Bet creation time (hhmiss)          
     *                            , 0                           ! Trx Bet creation time (hhmiss)          
     *                            , 0                           ! Trx Bet creation time (hhmiss)          
     *                            , 0                           ! Trx Bet: Player NIF                     
     *                            , CONF.TRABUF(TIGSC_CRDY)     ! Trx Betslip: Cancel reference date (yymmdd)
     *                            , CONF.TRABUF(TIGSC_CRDM)     ! Trx Betslip: Cancel reference date (yymmdd)
     *                            , CONF.TRABUF(TIGSC_CRDD)     ! Trx Betslip: Cancel reference date (yymmdd)
     *                            , CONF.TRABUF(TIGSC_CRGM)     ! Trx Betslip: Cancel reference game         
     *                            , EXTSER                      ! Trx Betslip: Cancel reference serial number
     *                            , CONF.TRABUF(TIGSC_CRCD)     ! Trx Betslip: Cancel reference check-digits 
     *                            , CONF.TRABUF(TIGSC_WCDY)     ! Trx Cancel Date (yyyymmdd)                 
     *                            , CONF.TRABUF(TIGSC_WCDM)     ! Trx Cancel Date (yyyymmdd)                 
     *                            , CONF.TRABUF(TIGSC_WCDD)     ! Trx Cancel Date (yyyymmdd)                 
     *                            , CONF.TRABUF(TIGSC_WCTH)     ! Trx Cancel Time (hhmiss)                   
     *                            , CONF.TRABUF(TIGSC_WCTM)     ! Trx Cancel Time (hhmiss)                   
     *                            , CONF.TRABUF(TIGSC_WCTS)     ! Trx Cancel Time (hhmiss)                   
     *                            , CONF.TRABUF(TIGSC_CAMT)     ! Trx Cancel Amount (wager units)         

        WRITE(CONF.LUN_OUTPUT,900) LINE
900     FORMAT(A)
901     FORMAT(     'CANCEL'               ! Trx Type
     *         ,'|',I8.8                   ! Trx Serial
     *         ,'|',I6.6                   ! Trx Timestamp (HHMISS)
     *         ,'|',I4.4                   ! Trx CDC
     *         ,'|',I5.5                   ! Trx Terminal Number
     *         ,'|',I8.8                   ! Trx Agent Number
     *         ,'|',I13.13                 ! Trx Message ID
     *         ,'|',I2.2,I2.2,I2.2         ! Trx Betslip: Bet reference date (yymmdd)
     *         ,'|',I2.2                   ! Trx Betslip: Bet reference game
     *         ,'|',I10.10                 ! Trx Betslip: Bet reference serial number
     *         ,'|',I3.3                   ! Trx Betslip: Bet reference check-digits
     *         ,'|',I9.9                   ! Trx Bet Unit Stake
     *         ,'|',I3.3                   ! Trx Total Bets
     *         ,'|',I9.9                   ! Trx Bet Total Stake
     *         ,'|',I9.9                   ! Trx Bet Maximum Possible Returns
     *         ,'|',I4.4,I2.2,I2.2         ! Trx Bet last event date (yymmdd)
     *         ,'|',I4.4,I2.2,I2.2         ! Trx Bet creation date (yyyymmdd)
     *         ,'|',I2.2,I2.2,I2.2         ! Trx Bet creation time (hhmiss)
     *         ,'|',I9.9                   ! Trx Bet: Player NIF
     *         ,'|',I2.2,I2.2,I2.2         ! Trx Betslip: Cancel reference date (yymmdd)
     *         ,'|',I2.2                   ! Trx Betslip: Cancel reference game
     *         ,'|',I10.10                 ! Trx Betslip: Cancel reference serial number
     *         ,'|',I3.3                   ! Trx Betslip: Cancel reference check-digits
     *         ,'|',I4.4,I2.2,I2.2         ! Trx Wager Cancel Date (yyyymmdd)
     *         ,'|',I2.2,I2.2,I2.2         ! Trx Wager Cancel Time (hhmiss)
     *         ,'|',I9.9                   ! Trx Cancel Amount (wager units)
     *  )

        RETURN
        END


        SUBROUTINE WRITE_REPORT_TRAILER(CONF)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        
        CHARACTER*197 LINE
        CHARACTER*197 LINE2
        INTEGER*4 I
        CHARACTER*32 TIMESTAMP
        CHARACTER*19 CHECKPOINT_TS
        
        WRITE(CONF.LUN_OUTPUT, 901) 
901     FORMAT(201('#'))
902     FORMAT('# ',A,' #')
913     FORMAT(A)
914     FORMAT(A,I19,A)
9141    FORMAT(A,A19,A)
921     FORMAT(A45,' [', I19,']')

        CALL GET_TIMESTAMP(TIMESTAMP)

        WRITE(LINE,914)  '[STOP ] : Writing transactions [TO  ] checkpoint number [', CONF.P_LAST_TRX_NR, ']'
        WRITE(CONF.LUN_OUTPUT, 902) LINE
        WRITE(LINE,913) TRIM('[STOP ] : By ABPAUDIT on ' // TRIM(TIMESTAMP))
        WRITE(CONF.LUN_OUTPUT, 902) LINE
        CALL FORMATTED_TO_CHAR( CONF.P_CHECKPOINT_TIME, CHECKPOINT_TS )
        WRITE(LINE,9141) '[STOP ] : Writing transactions [TO  ] checkpoint time   [', CHECKPOINT_TS, ']'
        WRITE(CONF.LUN_OUTPUT, 902) LINE
        WRITE(LINE,913) TRIM('[STOP ] : Statistics :')
        WRITE(CONF.LUN_OUTPUT, 902) LINE 
        WRITE(LINE2,921) '      Successful wager transactions processed', CONF.NR_SUC_WAGER_TRX
        WRITE(LINE,913) TRIM('[STOP ] : ' // TRIM(LINE2))
        WRITE(CONF.LUN_OUTPUT, 902) LINE 
        WRITE(LINE2,921) '           Total wager transactions processed', CONF.NR_TOT_WAGER_TRX
        WRITE(LINE,913) TRIM('[STOP ] : ' // TRIM(LINE2))
        WRITE(CONF.LUN_OUTPUT, 902) LINE 
        WRITE(LINE2,921) '     Successful cancel transactions processed', CONF.NR_SUC_CANCEL_TRX
        WRITE(LINE,913) TRIM('[STOP ] : ' // TRIM(LINE2))
        WRITE(CONF.LUN_OUTPUT, 902) LINE 
        WRITE(LINE2,921) '          Total cancel transactions processed', CONF.NR_TOT_CANCEL_TRX
        WRITE(LINE,913) TRIM('[STOP ] : ' // TRIM(LINE2))
        WRITE(CONF.LUN_OUTPUT, 902) LINE 
        WRITE(LINE2,921) '                  Total analyzed transactions', CONF.NR_ANALYZED_TRX
        WRITE(LINE,913) TRIM('[STOP ] : ' // TRIM(LINE2))
        WRITE(CONF.LUN_OUTPUT, 902) LINE 
        WRITE(LINE2,921) '       Total analyzed successful transactions', CONF.NR_TOT_SUC_TRX
        WRITE(LINE,913) TRIM('[STOP ] : ' // TRIM(LINE2))
        WRITE(CONF.LUN_OUTPUT, 902) LINE 
        WRITE(CONF.LUN_OUTPUT, 901) 


        RETURN

        END


        SUBROUTINE FLUSH_REPORT_TRX(CONF)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        
        CHARACTER*1024 LINE

        ! Effective flush of file
        ENDFILE(UNIT = CONF.LUN_OUTPUT, IOSTAT = CONF.ST)

        END


        SUBROUTINE CLOSE_REPORT_FILE(CONF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        
        CHARACTER*1024 LINE

        CLOSE(CONF.LUN_OUTPUT, IOSTAT = CONF.ST)
        IF(CONF.ST .NE. 0) THEN
            CALL L_ERROR(CONF,TRIM(CONF.OUTPUT_FILENAME) //
     *                   ' close error')
            RETURN
        ENDIF
        
        END



        SUBROUTINE LOAD_CONFIG_FILE(CONF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        
        CHARACTER*1024 LINE
        
        LOGICAL CFG_FILE_EXISTS, EOF
        INTEGER*4 I

        INQUIRE(FILE = TRIM(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CONF_FILE)), 
     *          EXIST = CFG_FILE_EXISTS)
        IF(CFG_FILE_EXISTS) THEN
            CONF.ST = 0
            IF(CONF.LUN_CONF .EQ. 0) THEN
                CALL FIND_AVAILABLE_LUN(CONF.LUN_CONF,CONF.ST)
            ENDIF
            IF(CONF.ST .NE. 0) CALL GSTOP(GEXIT_FATAL)

            OPEN(UNIT = CONF.LUN_CONF
     *         , FILE = TRIM(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CONF_FILE))
     *         , IOSTAT = CONF.ST, STATUS = 'OLD')
            IF(CONF.ST .NE. 0) THEN
                CALL L_ERROR(CONF,TRIM(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CONF_FILE)) //
     *                       ' open error')
                RETURN
            ENDIF
            
            EOF = .FALSE.
            DO WHILE(.NOT. EOF)
                CALL GET_TXT_LINE_FROM_FILE(CONF.LUN_CONF,LINE,EOF)
                IF(.NOT. EOF) THEN
                    CALL PARSE_TXT_LINE(LINE, CONF)
                ENDIF
            ENDDO
            
            CALL UPDATE_CONF (CONF)
            
            CLOSE(CONF.LUN_CONF, IOSTAT = CONF.ST)
            IF(CONF.ST .NE. 0) THEN
                CALL L_ERROR(CONF,TRIM(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CONF_FILE)) //
     *                       ' close error')
                RETURN
            ENDIF
            CALL L_DEBUG(CONF,'Configuration load succeeded!')
            CALL L_DEBUG(CONF,'-  Loaded file '// 
     *            TRIM(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CONF_FILE)) // ' !')
            CALL L_DEBUG(CONF,'-  Configurations read:')
            DO I = 1, AA_MAX_NR_PARAMS
                IF(TRIM(CONF.CONF_PARAMS(AA_I_NAME ,I)) .NE. '') THEN
                    CALL L_DEBUG(CONF,'   -  ' //
     *                    TRIM(CONF.CONF_PARAMS(AA_I_NAME ,I)) // ' = ' //
     *                    TRIM(ADJUSTL(CONF.CONF_PARAMS(AA_I_VALUE ,I))))
                ENDIF
            ENDDO
        ELSE
            CALL L_ERROR(CONF,'Configuration load failed!')
            CALL L_ERROR(CONF,'-  File ' // 
     *            TRIM(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CONF_FILE)) // ' does not exist!')
        ENDIF

        RETURN
        END


        SUBROUTINE PARSE_TXT_LINE(LINE, CONF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSDEFINE.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF

        CHARACTER*1024 LINE

        CHARACTER*128 NAME
        CHARACTER*1024 VAL
        
        INTEGER*4 INDEX, I, I4VAL, J, K
        
        INTEGER*4 CTOI
        
        DO I = 1,128
            NAME(I:I) = CHAR(0)
        ENDDO
        
        DO I = 1,1024
            VAL(I:I) = CHAR(0)
        ENDDO
        
        I = 1
        INDEX = 0
        
        IF(LINE(1:1) .EQ. '!' .OR. LINE(1:1) .EQ. '#') THEN
            RETURN
        ENDIF
        
        DO WHILE(I .LE. 1024 .AND. INDEX .EQ. 0)
            IF(LINE(I:I) .EQ. '=') THEN
                NAME = TRIM(LINE(1:I-1))
                VAL = TRIM(LINE(I+1:1024))
                INDEX = I
            ENDIF
            I = I + 1
        ENDDO 

        IF(INDEX .EQ. 0) THEN
            RETURN
        ENDIF
        
        J = 1
        K = 0
        DO WHILE(J .LE. AA_MAX_NR_PARAMS .AND. K .EQ. 0)
            IF(NAME .EQ. TRIM(AA_CONF_NAME(J))) THEN
                K = J
            ENDIF
            J = J + 1
        ENDDO
        
        IF(K .NE. 0) THEN
            CONF.CONF_PARAMS(AA_I_VALUE,K) = TRIM(VAL)
        ENDIF
        
        RETURN
        END


        SUBROUTINE GET_TXT_LINE_FROM_FILE(LUN,LINE,EOF)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSDEFINE.DEF'

        LOGICAL   EOF
        INTEGER*4 LUN

        CHARACTER*1024 LINE

        READ(UNIT = LUN,FMT='(A)',END = 100) LINE
        RETURN

100     CONTINUE
        EOF = .TRUE.
        RETURN
        END


        SUBROUTINE WRITE_CFG_FILE(CONF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSDEFINE.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF

        INTEGER*4 I
        CHARACTER*32 TIMESTAMP
        CHARACTER*124 LINE
        
        WRITE(CONF.LUN_CONF, 901) 
901     FORMAT(128('#'))
902     FORMAT('# ',A,' #')
913     FORMAT(A)

        CALL GET_TIMESTAMP(TIMESTAMP)

        WRITE(LINE,913) 'Configuration file for module ABPAUDIT'
        WRITE(CONF.LUN_CONF, 902) LINE
        WRITE(LINE,913) TRIM('Written by ABPAUDIT on ' // TRIM(TIMESTAMP))
        WRITE(CONF.LUN_CONF, 902) LINE 
        WRITE(CONF.LUN_CONF, 901) 

        DO I = 1, AA_MAX_NR_PARAMS
            IF(TRIM(CONF.CONF_PARAMS(AA_I_NAME,I)) .NE. '') THEN 
                WRITE(CONF.LUN_CONF, 903) TRIM(CONF.CONF_PARAMS(AA_I_NAME,I))
     *              , TRIM(ADJUSTL(CONF.CONF_PARAMS(AA_I_VALUE,I)))
903             FORMAT(A,' = ',A)
            ENDIF
        ENDDO
        
        RETURN
        END



        SUBROUTINE PUT_TXT_LINE_TO_FILE(LUN,LINE,EOF)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSDEFINE.DEF'

        LOGICAL   EOF
        INTEGER*4 LUN

        CHARACTER*1024 LINE

        WRITE(UNIT = LUN,FMT='(A)') LINE
        RETURN

        END



        


        SUBROUTINE SAVE_CONFIG_FILE(CONF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF

        INTEGER*4 I
        
        LOGICAL CFG_FILE_EXISTS
        
        INQUIRE(FILE = TRIM(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CONF_FILE))
     *        , EXIST = CFG_FILE_EXISTS)
        IF(CFG_FILE_EXISTS) THEN
            CALL DFILX(TRIM(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CONF_FILE))
     *                ,0,0,CONF.ST)
            IF (CONF.ST .NE. 0) THEN
                TYPE*,IAM(),'Configuration save failed!'
                TYPE*,IAM(),'-  Could not delete file '// 
     *               TRIM(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CONF_FILE)) // '!'
                TYPE*,IAM()
            ENDIF
        ENDIF
        OPEN(UNIT = CONF.LUN_CONF, 
     *       FILE = TRIM(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CONF_FILE)), 
     *       ACCESS = 'SEQUENTIAL',
     *       STATUS = 'NEW',
     *       RECL = 1024, 
     *       IOSTAT = CONF.ST)
        IF(CONF.ST .NE. 0) THEN
            CALL L_ERROR(CONF,TRIM(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CONF_FILE)) //
     *                   ' open error')
            RETURN
        ENDIF

        CALL UPDATE_TO_WRITE_CONF(CONF)
        CALL WRITE_CFG_FILE(CONF)

        CLOSE(CONF.LUN_CONF, IOSTAT = CONF.ST)
        IF(CONF.ST .NE. 0) THEN
            CALL L_ERROR(CONF,TRIM(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CONF_FILE)) //
     *                   ' close error')
            RETURN
        ENDIF
        CALL L_DEBUG(CONF,'Configuration save succeeded!')
        CALL L_DEBUG(CONF,'-  Saved file '// 
     *       TRIM(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CONF_FILE)) // '!')
        CALL L_DEBUG(CONF,'-  Configurations saved:')
        DO I = 1, AA_MAX_NR_PARAMS
            IF(TRIM(CONF.CONF_PARAMS(AA_I_NAME ,I)) .NE. '') THEN
                CALL L_DEBUG(CONF,'   -  ' //
     *                TRIM(CONF.CONF_PARAMS(AA_I_NAME ,I)) // ' = ' //
     *                TRIM(ADJUSTL(CONF.CONF_PARAMS(AA_I_VALUE ,I))))
            ENDIF
        ENDDO

        RETURN
        END


        SUBROUTINE GET_TIMESTAMP(LINE)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C ARGUMENTS: output format: YYYY.MM.DD-HH:MI:SS,sss
        CHARACTER*32 LINE

C INTERNAL VARIABLES
        INTEGER*4 TIM(8), I, YYYY,MM,DD, JDAY
        CHARACTER*12 CLOCK(3)
C
C       values (1) is the 4-digit year
C       values (2) is the month of the year
C       values (3) is the day of the year
C       values (4) is the time difference with respect to
C                   Coordinated Universal Time (UTC) in minutes
C       values (5) is the hour of the day (range 0 to 23)
C       values (6) is the minutes of the hour (range 0 to 59).
C       values (7) is the seconds of the minute (range 0 to 59).
C       values (8) is the milliseconds of the second (range 0 to 999).


        CALL DATE_AND_TIME(CLOCK(1),CLOCK(2),CLOCK(3),TIM)

        WRITE(LINE,901) TIM(1)  ! YYYY
     *                , TIM(2)  ! MM
     *                , TIM(3)  ! DD
     *                , TIM(5)  ! HH
     *                , TIM(6)  ! MI
     *                , TIM(7)  ! SS
     *                , TIM(8)  ! sss
901     FORMAT(I4.4,'.',I2.2,'.',I2.2,'-',I2.2,':',I2.2,':',I2.2,',',I3.3)

        RETURN
        END


        SUBROUTINE GET_UNIX_MS_FROM_CDC_TTIM(I8UNIX_MS, I4CDC, I4TTIM)
        IMPLICIT NONE
        
        INTEGER*8 I8UNIX_MS
        INTEGER*4 I4CDC, I4TTIM
        INTEGER*8 I8ZERO_CDC_MOMENT
        
        INTEGER*8 MS_IN_A_DAY
        PARAMETER(MS_IN_A_DAY = KZEXT(24 * 60 * 60 * 1000))
        
        I8ZERO_CDC_MOMENT = KZEXT(0)
        CALL CONVERT_TO_UNIX_TIME_MS(I8ZERO_CDC_MOMENT
     *          , 2001, 04, 30,   00, 00, 00)
     
        I8UNIX_MS = I8ZERO_CDC_MOMENT + (KZEXT(I4CDC)  * MS_IN_A_DAY)
     *                                + (KZEXT(I4TTIM) * KZEXT(1000))
        
        RETURN
        END


        SUBROUTINE GET_JULIAN_DAY_FOR_YEAR(YYYY,MM,DD,JDAY)
        IMPLICIT NONE
        
        INTEGER*4 YYYY,MM,DD,JDAY
        
        INTEGER*4 REG_YEAR(12)
        INTEGER*4 BIS_YEAR(12)
        INTEGER*4 I
        
        
        I = 1
        REG_YEAR(I) = 31
        BIS_YEAR(I) = 31

        I = 2                          
        REG_YEAR(I) = REG_YEAR(I - 1) + 28
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 29
        
        I = 3                          
        REG_YEAR(I) = REG_YEAR(I - 1) + 31
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 31
        
        I = 4                           
        REG_YEAR(I) = REG_YEAR(I - 1) + 30
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 30
        
        I = 5                           
        REG_YEAR(I) = REG_YEAR(I - 1) + 31
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 31
        
        I = 6                           
        REG_YEAR(I) = REG_YEAR(I - 1) + 30
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 30
        
        I = 7                           
        REG_YEAR(I) = REG_YEAR(I - 1) + 31
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 31
        
        I = 8                           
        REG_YEAR(I) = REG_YEAR(I - 1) + 31
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 31
        
        I = 9                           
        REG_YEAR(I) = REG_YEAR(I - 1) + 30
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 30
        
        I = 10                          
        REG_YEAR(I) = REG_YEAR(I - 1) + 31
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 31
        
        I = 11                          
        REG_YEAR(I) = REG_YEAR(I - 1) + 30
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 30
        
        I = 12                          
        REG_YEAR(I) = REG_YEAR(I - 1) + 31
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 31

        ! Handle bissext years
        IF(  (MOD(YYYY,4)   .EQ. 0
     *  .AND. MOD(YYYY,400) .EQ. 0)
     *  .OR. (MOD(YYYY,4)   .EQ. 0
     *  .AND. MOD(YYYY,100) .NE. 0) ) THEN
            JDAY = DD
            IF(MM .GT. 1) THEN
                JDAY = JDAY + BIS_YEAR(MM - 1)
            ENDIF
        ! Handle regular years
        ELSE
            JDAY = DD
            IF(MM .GT. 1) THEN
                JDAY = JDAY + REG_YEAR(MM - 1)
            ENDIF
        ENDIF
        
        RETURN
        END

        SUBROUTINE GET_DATE_FROM_JULIAN_DAY_AND_YEAR(JDAY,YYYY,MM,DD)
        IMPLICIT NONE
        
        INTEGER*4 YYYY,MM,DD,JDAY,AUX
        
        INTEGER*4 REG_YEAR(12)
        INTEGER*4 BIS_YEAR(12)
        INTEGER*4 I
        
        I = 1
        REG_YEAR(I) = 31
        BIS_YEAR(I) = 31

        I = 2                          
        REG_YEAR(I) = 28
        BIS_YEAR(I) = 29
        
        I = 3                          
        REG_YEAR(I) = 31
        BIS_YEAR(I) = 31
        
        I = 4         
        REG_YEAR(I) = 30
        BIS_YEAR(I) = 30
        
        I = 5         
        REG_YEAR(I) = 31
        BIS_YEAR(I) = 31
        
        I = 6         
        REG_YEAR(I) = 30
        BIS_YEAR(I) = 30
        
        I = 7         
        REG_YEAR(I) = 31
        BIS_YEAR(I) = 31
        
        I = 8         
        REG_YEAR(I) = 31
        BIS_YEAR(I) = 31
        
        I = 9         
        REG_YEAR(I) = 30
        BIS_YEAR(I) = 30
        
        I = 10        
        REG_YEAR(I) = 31
        BIS_YEAR(I) = 31
        
        I = 11        
        REG_YEAR(I) = 30
        BIS_YEAR(I) = 30
        
        I = 12        
        REG_YEAR(I) = 31
        BIS_YEAR(I) = 31

        AUX = JDAY
        ! Handle bissext years
        IF(  (MOD(YYYY,4)   .EQ. 0
     *  .AND. MOD(YYYY,400) .EQ. 0)
     *  .OR. (MOD(YYYY,4)   .EQ. 0
     *  .AND. MOD(YYYY,100) .NE. 0) ) THEN
            MM = 1
            DO WHILE(AUX .GT. 0)
               AUX = AUX - BIS_YEAR(MM)
               MM = MM + 1
            ENDDO
            IF(MM .GT. 1) THEN
                MM = MM - 1
                DD = AUX + BIS_YEAR(MM)
            ELSE
                DD = JDAY
            ENDIF
        ! Handle regular years
        ELSE
            MM = 1
            DO WHILE(AUX .GT. 0)
               AUX = AUX - REG_YEAR(MM)
               MM = MM + 1
            ENDDO
            IF(MM .GT. 1) THEN
                MM = MM - 1
                DD = AUX + REG_YEAR(MM)
            ELSE
                DD = JDAY
            ENDIF
        ENDIF
        
        RETURN
        END





        SUBROUTINE CONVERT_TO_UNIX_TIME_MS(UX_TIME_MS,YYYY,MM,DD,HH,MI,SS)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C ARGUMENTS
        INTEGER*8 UX_TIME_MS
        INTEGER*4 YYYY,MM,DD,HH,MI,SS

C INTERNAL VARIABLES
        INTEGER*4  I, JDAY

        UX_TIME_MS = KZEXT(0)
     *             + KZEXT(SS) * KZEXT(1000)
     *             + KZEXT(MI) * KZEXT(1000 * 60)
     *             + KZEXT(HH) * KZEXT(1000 * 60 * 60)
     
        DO I = 1970, YYYY - 1
            CALL GET_JULIAN_DAY_FOR_YEAR(I,12,31,JDAY)
            UX_TIME_MS = UX_TIME_MS + (KZEXT(JDAY) * KZEXT(1000 * 60 * 60 * 24))
        ENDDO
        CALL GET_JULIAN_DAY_FOR_YEAR(YYYY,MM,DD,JDAY)
        JDAY = JDAY - 1 ! Must remove one day, because of that day's milliseconds
        UX_TIME_MS = UX_TIME_MS + (KZEXT(JDAY) * KZEXT(1000 * 60 * 60 * 24))

        RETURN
        END

        SUBROUTINE I8TOI4(I8VAL, I4VAL)
        IMPLICIT NONE
        
        INTEGER*4 I4VAL
        INTEGER*8 I8VAL
        
        INTEGER*8 I8TMP
        
        INTEGER*4 SIGN
        
        SIGN = 1
        IF(I8VAL .LT. 0) THEN
            SIGN = -1
        ENDIF
        
        I8TMP = KIABS(I8VAL)
        I4VAL = INT4(KMOD(I8TMP,KZEXT(2147483647))) ! MOD(I8TMP,2^31) - 
                ! remember, most significant bit is a sign bit
        I4VAL = SIGN * I4VAL
        RETURN
        END

        SUBROUTINE I4TOI8(I4VAL, I8VAL)
        IMPLICIT NONE
        
        INTEGER*8 I8VAL
        INTEGER*4 I4VAL
        
        INTEGER*8 I8TMP
        
        INTEGER*8 SIGN
        
        SIGN = 1
        IF(I4VAL .LT. 0) THEN
            SIGN = -1
        ENDIF
        
        I8TMP = INT8(IABS(I4VAL))
        I8VAL = SIGN * I8TMP
        RETURN
        END


        SUBROUTINE CONVERT_FROM_UNIX_TIME_MS(UX_TIME_MS,YYYY,MM,DD,HH,MI,SS)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C ARGUMENTS
        INTEGER*8 UX_TIME_MS
        INTEGER*4 YYYY,MM,DD,HH,MI,SS

C INTERNAL VARIABLES
        INTEGER*4  I, JDAY, I4UX_TIME_MS, DAYS, AUX

        INTEGER*4 MS_IN_A_DAY
        PARAMETER(MS_IN_A_DAY    = 24 * 60 * 60 * 1000)
        
        INTEGER*4 MS_IN_AN_HOUR
        PARAMETER(MS_IN_AN_HOUR  =      60 * 60 * 1000)
        
        INTEGER*4 MS_IN_A_MINUTE
        PARAMETER(MS_IN_A_MINUTE =           60 * 1000)
        
        INTEGER*4 MS_IN_A_SECOND
        PARAMETER(MS_IN_A_SECOND =                1000)
        
        CALL I8TOI4(KMOD(UX_TIME_MS,KZEXT(MS_IN_A_DAY)),I4UX_TIME_MS)
        CALL I8TOI4(     UX_TIME_MS/KZEXT(MS_IN_A_DAY) ,DAYS)

        AUX = DAYS
        IF(I4UX_TIME_MS .LT. 0) THEN
            AUX = AUX - 1
            I4UX_TIME_MS = I4UX_TIME_MS + MS_IN_A_DAY
        ENDIF
        
        HH =     I4UX_TIME_MS / MS_IN_AN_HOUR
        MI = MOD(I4UX_TIME_MS , MS_IN_AN_HOUR ) / MS_IN_A_MINUTE
        SS = MOD(I4UX_TIME_MS , MS_IN_A_MINUTE) / MS_IN_A_SECOND

        YYYY = 1970
        IF(AUX .GT. 0) THEN
            DO WHILE(AUX .GT. 0)
                CALL GET_JULIAN_DAY_FOR_YEAR(YYYY,12,31,JDAY)
                AUX = AUX - JDAY
                YYYY = YYYY + 1
            ENDDO
            IF(AUX .EQ. 0) THEN
                MM = 1
                DD = 1
            ELSE
                YYYY = YYYY - 1
                AUX = AUX + JDAY + 1
                CALL GET_DATE_FROM_JULIAN_DAY_AND_YEAR(AUX,YYYY,MM,DD)
            ENDIF
        ELSEIF(AUX .LT. 0) THEN
            DO WHILE(AUX .LT. 0)
                CALL GET_JULIAN_DAY_FOR_YEAR(YYYY,12,31,JDAY)
                AUX = AUX + JDAY
                YYYY = YYYY - 1
            ENDDO
            IF(AUX .EQ. 0) THEN
                MM = 1
                DD = 1
            ELSE
                AUX = JDAY 
                CALL GET_DATE_FROM_JULIAN_DAY_AND_YEAR(AUX,YYYY,MM,DD)
            ENDIF
        ELSE
            YYYY = 1970
            MM   =    1
            DD   =    1 
        ENDIF

        RETURN
        END



        SUBROUTINE FORMATTED_TO_UNIX(I8IN_YYYYMMDDHHMISS,I8OUT_UNIX_MS)
        IMPLICIT NONE
        
        INTEGER*8 I8IN_YYYYMMDDHHMISS,I8OUT_UNIX_MS
        INTEGER*4 YYYY,MM,DD,HH,MI,SS
        !                                            YYYYMMDDHHMISS
        YYYY = INT4(KMOD(I8IN_YYYYMMDDHHMISS, KZEXT(100000000000000)) /
     *                                        KZEXT(    10000000000))
        
        !                                            YYYYMMDDHHMISS
        MM   = INT4(KMOD(I8IN_YYYYMMDDHHMISS, KZEXT(    10000000000)) /
     *                                        KZEXT(      100000000))
        
        !                                            YYYYMMDDHHMISS
        DD   = INT4(KMOD(I8IN_YYYYMMDDHHMISS, KZEXT(      100000000)) /
     *                                        KZEXT(        1000000))
        
        !                                            YYYYMMDDHHMISS
        HH   = INT4(KMOD(I8IN_YYYYMMDDHHMISS, KZEXT(        1000000)) /
     *                                        KZEXT(          10000))

        !                                            YYYYMMDDHHMISS
        MI   = INT4(KMOD(I8IN_YYYYMMDDHHMISS, KZEXT(          10000)) /
     *                                        KZEXT(            100))

        !                                            YYYYMMDDHHMISS
        SS   = INT4(KMOD(I8IN_YYYYMMDDHHMISS, KZEXT(            100)) /
     *                                        KZEXT(              1))
        CALL CONVERT_TO_UNIX_TIME_MS(I8OUT_UNIX_MS,
     *        YYYY,MM,DD,HH,MI,SS)
        RETURN
        END



        SUBROUTINE UNIX_TO_FORMATTED(I8IN_UNIX_MS,I8OUT_YYYYMMDDHHMISS)
        IMPLICIT NONE
        
        INTEGER*8 I8IN_UNIX_MS,I8OUT_YYYYMMDDHHMISS
        INTEGER*4 YYYY,MM,DD,HH,MI,SS
        
        CALL CONVERT_FROM_UNIX_TIME_MS(I8IN_UNIX_MS,
     *        YYYY,MM,DD,HH,MI,SS)
     
        I8OUT_YYYYMMDDHHMISS =                     !YYYYMMDDHHMISS
     *                       + (KZEXT(SS  ) * KZEXT(             1))
     *                       + (KZEXT(MI  ) * KZEXT(           100))
     *                       + (KZEXT(HH  ) * KZEXT(         10000))
     *                       + (KZEXT(DD  ) * KZEXT(       1000000))
     *                       + (KZEXT(MM  ) * KZEXT(     100000000))
     *                       + (KZEXT(YYYY) * KZEXT(   10000000000))
        RETURN
        END



        SUBROUTINE FORMATTED_TO_CHAR(I8IN_YYYYMMDDHHMISS,CHAROUT_TS)
        IMPLICIT NONE
        
        INTEGER*8 I8IN_YYYYMMDDHHMISS
        !         1         2         3
        !1---5----0----5----0----5----0
        !YYYY/MM/DD HH:MI:SS
        CHARACTER*19 CHAROUT_TS
        INTEGER*4 YYYY,MM,DD,HH,MI,SS
        !                                            YYYYMMDDHHMISS
        YYYY = INT4(KMOD(I8IN_YYYYMMDDHHMISS, KZEXT(100000000000000)) /
     *                                        KZEXT(    10000000000))
        
        !                                            YYYYMMDDHHMISS
        MM   = INT4(KMOD(I8IN_YYYYMMDDHHMISS, KZEXT(    10000000000)) /
     *                                        KZEXT(      100000000))
        
        !                                            YYYYMMDDHHMISS
        DD   = INT4(KMOD(I8IN_YYYYMMDDHHMISS, KZEXT(      100000000)) /
     *                                        KZEXT(        1000000))
        
        !                                            YYYYMMDDHHMISS
        HH   = INT4(KMOD(I8IN_YYYYMMDDHHMISS, KZEXT(        1000000)) /
     *                                        KZEXT(          10000))

        !                                            YYYYMMDDHHMISS
        MI   = INT4(KMOD(I8IN_YYYYMMDDHHMISS, KZEXT(          10000)) /
     *                                        KZEXT(            100))

        !                                            YYYYMMDDHHMISS
        SS   = INT4(KMOD(I8IN_YYYYMMDDHHMISS, KZEXT(            100)) /
     *                                        KZEXT(              1))
     
        WRITE(CHAROUT_TS, 901) YYYY,MM,DD,HH,MI,SS
901     FORMAT(I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2)
        RETURN
        END


        SUBROUTINE FORMAT_TIME_SEC(OUT_HHMMSS,IN_SECS)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C ARGUMENTS
        INTEGER*4 IN_SECS, OUT_HHMMSS

        OUT_HHMMSS = 
     *    (MOD(IN_SECS / 3600,  100)      ) * 10000
     *  + (MOD(IN_SECS       , 3600) / 60 ) *   100
     *  + (MOD(IN_SECS       ,   60)      )
        RETURN
        END


        SUBROUTINE GET_TIME_SEC(TIME_SEC_TODAY)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C ARGUMENTS
        INTEGER*4 TIME_SEC_TODAY

C INTERNAL VARIABLES
        INTEGER*4 TIM(8), I, YYYY,MM,DD, JDAY
        CHARACTER*12 CLOCK(3)
C
C       values (1) is the 4-digit year
C       values (2) is the month of the year
C       values (3) is the day of the year
C       values (4) is the time difference with respect to
C                   Coordinated Universal Time (UTC) in minutes
C       values (5) is the hour of the day (range 0 to 23)
C       values (6) is the minutes of the hour (range 0 to 59).
C       values (7) is the seconds of the minute (range 0 to 59).
C       values (8) is the milliseconds of the second (range 0 to 999).


        CALL DATE_AND_TIME(CLOCK(1),CLOCK(2),CLOCK(3),TIM)

        TIME_SEC_TODAY = 
     *    ZEXT(TIM(7)) 
     *  + ZEXT(TIM(6)) * ZEXT(60) 
     *  + ZEXT(TIM(5)) * ZEXT(60) * ZEXT(60)

        RETURN
        END

        SUBROUTINE GET_TIME_MS(TIME_MS_YYYYMMDDHHMISSMLS)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C ARGUMENTS
        INTEGER*8 TIME_MS, TIME_MS_YYYYMMDDHHMISSMLS

C INTERNAL VARIABLES
        INTEGER*4 TIM(8), I, YYYY,MM,DD, JDAY
        CHARACTER*12 CLOCK(3)
C
C       values (1) is the 4-digit year
C       values (2) is the month of the year
C       values (3) is the day of the year
C       values (4) is the time difference with respect to
C                   Coordinated Universal Time (UTC) in minutes
C       values (5) is the hour of the day (range 0 to 23)
C       values (6) is the minutes of the hour (range 0 to 59).
C       values (7) is the seconds of the minute (range 0 to 59).
C       values (8) is the milliseconds of the second (range 0 to 999).


        CALL DATE_AND_TIME(CLOCK(1),CLOCK(2),CLOCK(3),TIM)

        TIME_MS_YYYYMMDDHHMISSMLS = 
     *    KZEXT(TIM(8))
     *  + KZEXT(TIM(7)) * KZEXT(1000)
     *  + KZEXT(TIM(6)) * KZEXT(1000) * KZEXT(100)
     *  + KZEXT(TIM(5)) * KZEXT(1000) * KZEXT(100) * KZEXT(100)
     *  + KZEXT(TIM(3)) * KZEXT(1000) * KZEXT(100) * KZEXT(100) * KZEXT(100)
     *  + KZEXT(TIM(2)) * KZEXT(1000) * KZEXT(100) * KZEXT(100) * KZEXT(100) * KZEXT(100)
     *  + KZEXT(TIM(1)) * KZEXT(1000) * KZEXT(100) * KZEXT(100) * KZEXT(100) * KZEXT(100) * KZEXT(100)

        RETURN
        END

        
        SUBROUTINE IS_LAST_TIME_TODAY(CONF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        INTEGER*8 NOW,LAST
        
        CONF.LAST_TIME_TODAY = .TRUE.
        
        IF(CONF.P_LAST_TIME .EQ. 0) THEN
            CONF.LAST_TIME_TODAY = .FALSE.
            RETURN
        ENDIF
        
        CALL GET_TIME_MS(NOW)
        LAST = CONF.P_LAST_TIME
        LAST = LAST / KZEXT(1000000000)
        NOW  = NOW  / KZEXT(1000000000)
        
        IF(LAST .NE. NOW) THEN
            CONF.LAST_TIME_TODAY = .FALSE.
            RETURN
        ENDIF
        
        RETURN
        END
        
        
        
        SUBROUTINE LOG(CONF,LOG_LEVEL,STRING)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        INTEGER*4 LOG_LEVEL
        CHARACTER*(*) STRING
        
        IF(LOG_LEVEL .LE. CONF.P_LOG_LEVEL .AND. LOG_LEVEL .NE. AA_LOG_LEVEL_NONE) THEN
            CALL OPSTXT(TRIM(STRING))
            TYPE *, IAM(), AA_LOG_LEVEL_DESC(LOG_LEVEL),' ',TRIM(STRING)
        ENDIF
        
        RETURN
        END

        SUBROUTINE LOG_I4(CONF,LOG_LEVEL,STRING,I4)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        INTEGER*4 LOG_LEVEL
        INTEGER*4 I4
        CHARACTER*(*) STRING
        CHARACTER*64 LINE
        
        IF(LOG_LEVEL .LE. CONF.P_LOG_LEVEL .AND. LOG_LEVEL .NE. AA_LOG_LEVEL_NONE) THEN
            WRITE(LINE,901) TRIM(STRING),I4
901         FORMAT(A32,' : ',I)
            CALL OPSTXT(TRIM(LINE))
            TYPE *, IAM(), AA_LOG_LEVEL_DESC(LOG_LEVEL),' ',TRIM(LINE)
        ENDIF
        
        RETURN
        END

        SUBROUTINE LOG_I8(CONF,LOG_LEVEL,STRING,I8)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        INTEGER*4 LOG_LEVEL
        INTEGER*8 I8
        CHARACTER*(*) STRING
        CHARACTER*64 LINE
        
        IF(LOG_LEVEL .LE. CONF.P_LOG_LEVEL .AND. LOG_LEVEL .NE. AA_LOG_LEVEL_NONE) THEN
            WRITE(LINE,901) TRIM(STRING),I8
901         FORMAT(A32,' : ',I)
            CALL OPSTXT(TRIM(LINE))
            TYPE *, IAM(), AA_LOG_LEVEL_DESC(LOG_LEVEL),' ',TRIM(LINE)
        ENDIF
        
        RETURN
        END


        SUBROUTINE LOG_L(CONF,LOG_LEVEL,STRING,L)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        INTEGER*4 LOG_LEVEL
        LOGICAL L
        CHARACTER*(*) STRING
        CHARACTER*64 LINE
        
        IF(LOG_LEVEL .LE. CONF.P_LOG_LEVEL .AND. LOG_LEVEL .NE. AA_LOG_LEVEL_NONE) THEN
            WRITE(LINE,901) TRIM(STRING),L
901         FORMAT(A32,' : ',L)
            CALL OPSTXT(TRIM(LINE))
            TYPE *, IAM(), AA_LOG_LEVEL_DESC(LOG_LEVEL),' ',TRIM(LINE)
        ENDIF
        
        RETURN
        END

        SUBROUTINE LOG_S(CONF,LOG_LEVEL,STRING,S)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        INTEGER*4 LOG_LEVEL
        LOGICAL L
        CHARACTER*(*) STRING,S
        CHARACTER*132 LINE
        
        IF(LOG_LEVEL .LE. CONF.P_LOG_LEVEL .AND. LOG_LEVEL .NE. AA_LOG_LEVEL_NONE) THEN
            WRITE(LINE,901) TRIM(STRING),TRIM(S)
901         FORMAT(A32,' : ',A)
            CALL OPSTXT(TRIM(LINE))
            TYPE *, IAM(), AA_LOG_LEVEL_DESC(LOG_LEVEL),' ',TRIM(LINE)
        ENDIF
        
        RETURN
        END


        
        SUBROUTINE L_ALL(CONF,STRING)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        CHARACTER*(*) STRING
        CALL LOG(CONF,AA_LOG_LEVEL_ALL,STRING)
        RETURN
        END

        
        SUBROUTINE L_ERROR(CONF,STRING)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        CHARACTER*(*) STRING
        CALL LOG(CONF,AA_LOG_LEVEL_ERROR,STRING)
        RETURN
        END

        SUBROUTINE L_WARN(CONF,STRING)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        CHARACTER*(*) STRING
        CALL LOG(CONF,AA_LOG_LEVEL_WARN,STRING)
        RETURN
        END

        SUBROUTINE L_INFO(CONF,STRING)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        CHARACTER*(*) STRING
        CALL LOG(CONF,AA_LOG_LEVEL_INFO,STRING)
        RETURN
        END

        SUBROUTINE L_DEBUG(CONF,STRING)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        CHARACTER*(*) STRING
        CALL LOG(CONF,AA_LOG_LEVEL_DEBUG,STRING)
        RETURN
        END
        
        SUBROUTINE L_TRACE(CONF,STRING)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:ABPAUDIT.DEF'

        RECORD /ABP_AUDIT_CONF/ CONF
        CHARACTER*(*) STRING
        CALL LOG(CONF,AA_LOG_LEVEL_TRACE,STRING)
        RETURN
        END
        
