C
C SOUPFM_COMMON - SOUP File Management
C
C This file has common functions used by SOUPFM
C
C V02 08-APR-2014 SCML Adding support for new expired prizes reports
C V01 12-DEC-2013 SCML Program creation
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
C
C SUBROUTINE RESERVE_AVAILABLE_LUN(LUN,ST)
C
C FINDS AND RESERVES AN AVAILABLE LOGICAL UNIT
C
C INPUTS:
C  SOUPFM_REC.START_LUN  STARTS SEARCH FROM LUN X
C  INDEX                 INDEX OF LUN_TABLE
C 
C OUTPUTS:
C  SOUPFM_REC.START_LUN  STARTING SEARCH LUN FROM              INT*4
C  SOUPFM_REC.LUN        RESERVED LOGICAL UNIT                 INT*4
C  ST         STATUS (0 -> OK, ELSE NO LUN, ERROR)  INT*4
C
        SUBROUTINE RESERVE_AVAILABLE_LUN(SOUPFM_REC,INDEX,ST)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        LOGICAL OPNFLG
C----+------------------------------------------------------------------
C V01| Input parameters
C----+------------------------------------------------------------------
C----+------------------------------------------------------------------
C V01| Input/Output parameters
C----+------------------------------------------------------------------
C----+------------------------------------------------------------------
C V01| Output parameters
C----+------------------------------------------------------------------
        INTEGER*4 INDEX
        INTEGER*4 ST
C----+------------------------------------------------------------------
C V01| Auxiliary variables
C----+------------------------------------------------------------------
        INTEGER*4 IOS
        
        INTEGER*4 MAXLUN
        PARAMETER(MAXLUN=500)           !MAXIMUM LUN FOR SEARCH

C----+------------------------------------------------------------------
C V01| Start searching from (if start_lun <= 7) 
C----+------------------------------------------------------------------
        IF(SOUPFM_REC.START_LUN .LE. 7) THEN
            SOUPFM_REC.LUN_TABLE(INDEX) = 7                    !ASSUME LUN 7 IS AVAILABLE
        ELSE
            SOUPFM_REC.LUN_TABLE(INDEX) = SOUPFM_REC.START_LUN !ASSUME SEARCH FROM START_LUN
        ENDIF
        ST     = 0    
        OPNFLG = .TRUE.

C
C KEEP INCREMENTING LUN UNTIL AVAILABLE OR ERROR OR MAXLUN
C
        DO WHILE( (IOS .NE. 0 .OR. OPNFLG) 
     *      .AND. (SOUPFM_REC.LUN_TABLE(INDEX) .LE. MAXLUN) )
          INQUIRE(UNIT = SOUPFM_REC.LUN_TABLE(INDEX)
     *          , OPENED = OPNFLG, IOSTAT = IOS)
          IF(IOS .NE. 0 .OR. OPNFLG) THEN
              SOUPFM_REC.LUN_TABLE(INDEX) = 
     *          SOUPFM_REC.LUN_TABLE(INDEX)+1
          END IF
        END DO
C
C REPORT ERROR STATUS
C
        IF(SOUPFM_REC.LUN_TABLE(INDEX) .GT. MAXLUN) THEN
            TYPE*,IAM(),'NAO HA UNIDADE LOGICA (LUN) DISPONIVEL'
            ST = -1
        ELSE IF(IOS .NE. 0) THEN  !INQUIRE ERROR
            TYPE*,IAM(),'ERRO DE ENTRADA DE DADOS'
            ST = IOS
        ELSE
            SOUPFM_REC.START_LUN = SOUPFM_REC.LUN_TABLE(INDEX)
        ENDIF  

        RETURN 
        END



C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        SUBROUTINE RESERVE_AVAILABLE_LUN_GAME_FILE(SOUPFM_REC,INDEX,ST)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        LOGICAL OPNFLG
        INTEGER*4 INDEX
        INTEGER*4 ST
        INTEGER*4 IOS
        
        INTEGER*4 MAXLUN
        PARAMETER(MAXLUN=500)           !MAXIMUM LUN FOR SEARCH

        IF(SOUPFM_REC.START_LUN .LE. 7) THEN
            SOUPFM_REC.GAME_DATA_REC.GLUN(INDEX) = 7                    !ASSUME LUN 7 IS AVAILABLE
        ELSE
            SOUPFM_REC.GAME_DATA_REC.GLUN(INDEX) = SOUPFM_REC.START_LUN !ASSUME SEARCH FROM START_LUN
        ENDIF
        ST     = 0    
        OPNFLG = .TRUE.

C
C KEEP INCREMENTING LUN UNTIL AVAILABLE OR ERROR OR MAXLUN
C
        DO WHILE( (IOS .NE. 0 .OR. OPNFLG) 
     *      .AND. (SOUPFM_REC.GAME_DATA_REC.GLUN(INDEX) .LE. MAXLUN) )
          INQUIRE(UNIT = SOUPFM_REC.GAME_DATA_REC.GLUN(INDEX)
     *          , OPENED = OPNFLG, IOSTAT = IOS)
          IF(IOS .NE. 0 .OR. OPNFLG) THEN
              SOUPFM_REC.GAME_DATA_REC.GLUN(INDEX) = 
     *          SOUPFM_REC.GAME_DATA_REC.GLUN(INDEX)+1
          END IF
        END DO
C
C REPORT ERROR STATUS
C
        IF(SOUPFM_REC.GAME_DATA_REC.GLUN(INDEX) .GT. MAXLUN) THEN
            TYPE*,IAM(),'NAO HA UNIDADE LOGICA (LUN) DISPONIVEL'
            ST = -1
        ELSE IF(IOS .NE. 0) THEN  !INQUIRE ERROR
            TYPE*,IAM(),'ERRO DE ENTRADA DE DADOS'
            ST = IOS
        ELSE
            SOUPFM_REC.START_LUN = SOUPFM_REC.GAME_DATA_REC.GLUN(INDEX)
        ENDIF  

        RETURN 
        END
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------

        SUBROUTINE STRING_COPY(ORIG, DEST, LEN)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        CHARACTER ORIG(*)
        CHARACTER DEST(*)
        INTEGER*4 LEN, I
        
        DO I = 1, LEN
            DEST(I) = ORIG(I)
        ENDDO
        
        RETURN
        END

        SUBROUTINE COPY_TO_FNAME(SOUPFM_REC, INDEX, CURR_SRC_FILE_NAME)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER *4 I, INDEX
        
        DO I = 1, 7
            CURR_SRC_FILE_NAME(I) = SOUPFM_REC.SRC_FILE_NAME(INDEX,I)
        ENDDO
        
        RETURN
        END

        SUBROUTINE COPY_FROM_FNAME(SOUPFM_REC, INDEX, CURR_SRC_FILE_NAME)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER *4 I, INDEX
        
        DO I = 1, 7
            SOUPFM_REC.SRC_FILE_NAME(INDEX,I) = CURR_SRC_FILE_NAME(I) 
        ENDDO
        
        RETURN
        END

        SUBROUTINE TO_CONSOLE(MESSAGE)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        CHARACTER*(*) MESSAGE
        CHARACTER*132 BUFFER
        BUFFER = MESSAGE
        
        IF(ISDETACHED()) THEN
            CALL OPSTXT(MESSAGE)
        ELSE
            TYPE *,IAM(),MESSAGE
        ENDIF
        
        RETURN
        END



        SUBROUTINE PRINT_FILE_NAME(CURR_SRC_FILE_NAME
     *          ,PREFIX,QUALIFIER,QUALIFIER2,YEAR,MONTH,DAY)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        CHARACTER*4 PREFIX
        CHARACTER*2 QUALIFIER, QUALIFIER2
        INTEGER*4 YEAR,MONTH,DAY
        INTEGER*4 I
        
        CALL FASTSET(0,CURR_SRC_FILE_NAME,28)
        WRITE(CHR_SRC_FILE_NAME,901) 
     *            PREFIX
     *          , QUALIFIER
     *          , QUALIFIER2
     *          , YEAR
     *          , MONTH
     *          , DAY
        DO I = 1,7
            CURR_SRC_FILE_NAME(I) = INT_SRC_FILE_NAME(I)
        ENDDO
        RETURN
901     FORMAT(A4,'_',A2,'_',A2,'_',I4.4,I2.2,I2.2,'.ASC')
        END



C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        SUBROUTINE PRINT_FILE_NAME_GEN(CURR_SRC_FILE_NAME
     *          ,PREFIX,QUALIFIER,QUALIFIER2,YEAR,MONTH,DAY,EXT)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        CHARACTER*4 PREFIX
        CHARACTER*2 QUALIFIER, QUALIFIER2
        CHARACTER*3 EXT
        INTEGER*4 YEAR,MONTH,DAY
        INTEGER*4 I
        
        CALL FASTSET(0,CURR_SRC_FILE_NAME,28)
        WRITE(CHR_SRC_FILE_NAME,901) 
     *            TRIM(PREFIX)
     *          , QUALIFIER
     *          , QUALIFIER2
     *          , YEAR
     *          , MONTH
     *          , DAY
     *          , EXT
        DO I = 1,7
            CURR_SRC_FILE_NAME(I) = INT_SRC_FILE_NAME(I)
        ENDDO
        RETURN
901     FORMAT(A,'_',A2,'_',A2,'_',I4.4,I2.2,I2.2,'.',A)
        END
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------





        SUBROUTINE INIT_SOUPFM_REC(SOUPFM_REC)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INTEGER*4 J
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------

        INTEGER*4 I

        CALL FASTSET(0, SOUPFM_REC.LUN_TABLE, MAX_NR_FILES)
        CALL FASTSET(0, CURR_SRC_FILE_NAME, 7)
        DO I = 1, MAX_NR_FILES
            CALL FASTSET(0, SOUPFM_REC.SRC_FILE_NAME(I,1), 7)
            SOUPFM_REC.HANDLE_FILE(I) = .FALSE.
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
            DO J = 1, MAXGAM
                SOUPFM_REC.EXP_STAT_REC.NR_BETS(I,J) = KZEXT(0)
                SOUPFM_REC.EXP_STAT_REC.AMOUNT(I,J) = KZEXT(0)
            ENDDO
            SOUPFM_REC.EXP_STAT_REC.TOTAL_NR_BETS(I) = KZEXT(0)
            SOUPFM_REC.EXP_STAT_REC.TOTAL_AMOUNT(I) = KZEXT(0)
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        ENDDO
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        DO J = 1, MAXGAM
            SOUPFM_REC.GAME_DATA_REC.GLUN(J) = 0
            DO I = 1, 7
                SOUPFM_REC.GAME_DATA_REC.GFDB(I,J) = 0
            ENDDO
        ENDDO
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        SOUPFM_REC.START_LUN = 0
        SOUPFM_REC.SRC_CDC_DATE = 0
        SOUPFM_REC.TOTAL_TRX_CNT = 0
        SOUPFM_REC.TOTAL_VAL_CNT = 0
        SOUPFM_REC.PAS_CURR_REC_CNT = 0
        SOUPFM_REC.PAS_VPF_FILE_CNT = 0
        CALL FASTSET(0, SOUPFM_REC.LINE_CNT, MAX_NR_FILES)
        CALL FASTSET(0, SOUPFM_REC.TRX_CNT, MAX_NR_FILES)
        CALL FASTSET(0, SOUPFM_REC.FILE_DATA_CDC, MAX_NR_FILES)
        CALL FASTSET(0, SOUPFM_REC.FILE_GEN_CDC, MAX_NR_FILES)
        CALL FASTSET(0, SOUPFM_REC.PASSIVE_FDB, 7)
        CALL FASTSET(0, SOUPFM_REC.PRG_FDB, 7)
        SOUPFM_REC.PRG_BLK_CNT = 0
        SOUPFM_REC.PRG_IND    = 0
        SOUPFM_REC.PAS_EMIS   = 0
        SOUPFM_REC.PAS_GNUM   = 0
        SOUPFM_REC.PAS_GIND   = 0
        SOUPFM_REC.PAS_SERIAL = 0
        SOUPFM_REC.PAS_NUM    = 0
        SOUPFM_REC.PAS_FRAC   = 0
        SOUPFM_REC.PAS_DEBUG  = .FALSE.
        
        RETURN
        END


        SUBROUTINE INIT_SOUPFM_REC_AUX(SOUPFM_REC)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INTEGER*4 J
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------

        INTEGER*4 I

        CALL FASTSET(0, SOUPFM_REC.LUN_TABLE, MAX_NR_FILES)
        CALL FASTSET(0, CURR_SRC_FILE_NAME, 7)
        DO I = 1, MAX_NR_FILES 
            SOUPFM_REC.HANDLE_FILE(I) = .FALSE.
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
            DO J = 1, MAXGAM
                SOUPFM_REC.EXP_STAT_REC.NR_BETS(I,J) = KZEXT(0)
                SOUPFM_REC.EXP_STAT_REC.AMOUNT(I,J) = KZEXT(0)
            ENDDO
            SOUPFM_REC.EXP_STAT_REC.TOTAL_NR_BETS(I) = KZEXT(0)
            SOUPFM_REC.EXP_STAT_REC.TOTAL_AMOUNT(I) = KZEXT(0)
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        ENDDO
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        DO J = 1, MAXGAM
            SOUPFM_REC.GAME_DATA_REC.GLUN(J) = 0
            DO I = 1, 7
                SOUPFM_REC.GAME_DATA_REC.GFDB(I,J) = 0
            ENDDO
        ENDDO
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        SOUPFM_REC.TOTAL_TRX_CNT = 0
        SOUPFM_REC.TOTAL_VAL_CNT = 0
        SOUPFM_REC.PAS_CURR_REC_CNT = 0
        SOUPFM_REC.PAS_VPF_FILE_CNT = 0
        CALL FASTSET(0, SOUPFM_REC.LINE_CNT, MAX_NR_FILES)
        CALL FASTSET(0, SOUPFM_REC.TRX_CNT, MAX_NR_FILES)
        CALL FASTSET(0, SOUPFM_REC.FILE_DATA_CDC, MAX_NR_FILES)
        CALL FASTSET(0, SOUPFM_REC.FILE_GEN_CDC, MAX_NR_FILES)
        CALL FASTSET(0, SOUPFM_REC.PASSIVE_FDB, 7)
        CALL FASTSET(0, SOUPFM_REC.PRG_FDB, 7)
        SOUPFM_REC.PRG_BLK_CNT = 0
        SOUPFM_REC.PRG_IND    = 0
        SOUPFM_REC.PAS_EMIS   = 0
        SOUPFM_REC.PAS_GNUM   = 0
        SOUPFM_REC.PAS_GIND   = 0
        SOUPFM_REC.PAS_SERIAL = 0
        SOUPFM_REC.PAS_NUM    = 0
        SOUPFM_REC.PAS_FRAC   = 0
        SOUPFM_REC.PAS_DEBUG  = .FALSE.
        
        RETURN
        END

      SUBROUTINE CDC_TO_DATE_ARR(CDC,DATE_ARR)
      IMPLICIT NONE
C
C INCLUDES DEFINITION TO GET CDC DATE IN YYYYMMDD FORMAT
C
      INCLUDE 'INCLIB:DATBUF.DEF'
      INCLUDE 'INCLIB:SOUPFM.DEF'
C
C PARAMETERS DEFINION TO GET CDC DATE IN YYYYMMDD FORMAT
C
      INTEGER*4  CDC
      INTEGER*4  DATE_ARR(3)
C
C VARIABLES DEFINITION TO GET CDC DATE IN YYYYMMDD FORMAT
C
      INTEGER * 2 DATE(12)          ! DATE ARRAY

C
C SET CDC DATE IN DAY, MONTH, YEAR VALUES
C
      DATE(VCDC) = CDC
      CALL CDATE(DATE)
      IF(DATE(VYEAR) .GT. 77) THEN
        DATE(VYEAR) = DATE(VYEAR) + 1900
      ELSE
        DATE(VYEAR) = DATE(VYEAR) + 2000
      ENDIF
      
      DATE_ARR(DATE_ARR_YEAR) = DATE(VYEAR)
      DATE_ARR(DATE_ARR_MONTH) = DATE(VMON)
      DATE_ARR(DATE_ARR_DAY) = DATE(VDAY)

      END






        INTEGER*4 FUNCTION GET_SAP_AGENT_CODE(TRABUF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
         
        GET_SAP_AGENT_CODE = AGTSAP(TRABUF(TTER))
        
        RETURN
        END



        INTEGER*4 FUNCTION GET_AGENT_CODE(TRABUF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
         
        !GET_AGENT_CODE = TRABUF(TAGT)
        GET_AGENT_CODE = AGTTAB(AGTNUM,TRABUF(TTER))
        
        RETURN
        END



        INTEGER*4 FUNCTION GET_PRIZE_PAY_CHANNEL_ID(TRABUF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER *4 SAP_AGENT_CODE
        
        SAP_AGENT_CODE = GET_SAP_AGENT_CODE(TRABUF)
        
        IF(SAP_AGENT_CODE .EQ. PORTAL_SAP_CODE) THEN
            GET_PRIZE_PAY_CHANNEL_ID = PRIZE_PAY_CHANNEL_ID_GAME_PORTAL
        ELSE
            GET_PRIZE_PAY_CHANNEL_ID = PRIZE_PAY_CHANNEL_ID_MED_NETWORK
        ENDIF
        
        RETURN
        END





        INTEGER*8 FUNCTION GET_PRIZE_PAY_REF(TRABUF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*8 L_CDC, L_SER
        
        GET_PRIZE_PAY_REF = 0
        L_CDC = KZEXT(TRABUF(TCDC))
        L_SER = KZEXT(TRABUF(TSER))
        
        GET_PRIZE_PAY_REF = 
     *      KMOD(L_CDC,KZEXT(10000)) * KZEXT(1000000000)
     *    + KMOD(L_SER,KZEXT(1000000000))
        RETURN
        END



        INTEGER*8 FUNCTION GET_BET_REF(TRABUF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*8 L_CDC, L_SER
        
        GET_BET_REF = 0
        L_CDC = KZEXT(TRABUF(TVCDC))
        L_SER = KZEXT(TRABUF(TVSER))
        
        GET_BET_REF = 
     *      KMOD(L_CDC,KZEXT(10000)) * KZEXT(1000000000)
     *    + KMOD(L_SER,KZEXT(1000000000))
        RETURN
        END



        INTEGER*8 FUNCTION GET_BET_REF_FROM_VALREC(VALREC)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:PRMVPF.DEF'
        
        INTEGER*8 L_CDC, L_SER
        
        GET_BET_REF_FROM_VALREC = 0
        L_CDC = KZEXT(VALREC(VSCDC))
        L_SER = KZEXT(VALREC(VSSER))
        
        GET_BET_REF_FROM_VALREC = 
     *      KMOD(L_CDC,KZEXT(10000)) * KZEXT(1000000000)
     *    + KMOD(L_SER,KZEXT(1000000000))
        RETURN
        END





        SUBROUTINE GET_BET_EXTERNAL_REF(TRABUF, EXT_SER_ARR)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        
        INTEGER*4 EXT_SER_ARR(3)
        INTEGER*2 DBUF(LDATE_LEN)

        EXT_SER_ARR(1) = TRABUF(TVCDC)
        EXT_SER_ARR(2) = 0
        EXT_SER_ARR(3) = 0
        CALL OUTGEN(
     *            TRABUF(TVCDC)
     *          , TRABUF(TVSER)
     *          , EXT_SER_ARR(2)
     *          , EXT_SER_ARR(3)
     *  )
     
        DBUF(VCDC) = TRABUF(TVCDC)
        CALL LCDATE(DBUF)
        EXT_SER_ARR(1) = DBUF(VJUL)
        RETURN
        END
        

        SUBROUTINE GET_BET_EXTERNAL_REF_FROM_VALREC(VALREC, EXT_SER_ARR)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:PRMVPF.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        
        INTEGER*4 EXT_SER_ARR(3)
        INTEGER*2 DBUF(LDATE_LEN)

        EXT_SER_ARR(1) = VALREC(VSCDC)
        EXT_SER_ARR(2) = 0
        EXT_SER_ARR(3) = 0
        CALL OUTGEN(
     *            VALREC(VSCDC)
     *          , VALREC(VSSER)
     *          , EXT_SER_ARR(2)
     *          , EXT_SER_ARR(3)
     *  )
     
        DBUF(VCDC) = VALREC(VSCDC)
        CALL LCDATE(DBUF)
        EXT_SER_ARR(1) = DBUF(VJUL)
        RETURN
        END
        
        
        
        SUBROUTINE OPEN_FILE_SOUPFM_TMF(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4  ST
        
        ST = 0

        CALL COPY_TO_FNAME(SOUPFM_REC
     *        , IDX_TMF_FILE,INT_SRC_FILE_NAME)
        
        IF(SOUPFM_REC.LUN_TABLE(IDX_TMF_FILE) .EQ. 0) THEN
            CALL RESERVE_AVAILABLE_LUN(SOUPFM_REC,IDX_TMF_FILE,ST)
            IF(ST .NE. 0) RETURN
        ENDIF
        
        CALL OPENWY(SOUPFM_REC.LUN_TABLE(IDX_TMF_FILE)
     *       , INT_SRC_FILE_NAME,0,4,0,ST)
        REWIND(SOUPFM_REC.LUN_TABLE(IDX_TMF_FILE))
        CALL TOPEN(SOUPFM_REC.LUN_TABLE(IDX_TMF_FILE))

        
        IF(ST .NE. 0) THEN
            CALL FILERR(INT_SRC_FILE_NAME
     *           , FUN_OPEN_FILE,ST,0)
            RETURN
        ENDIF

        
        RETURN
        END




        SUBROUTINE CLOSE_FILE_SOUPFM_TMF(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4 ST

        ST = 0
        
        CALL USRCLOS2(SOUPFM_REC.LUN_TABLE(IDX_TMF_FILE),ST)
        IF(ST .NE. 0) THEN
            CALL COPY_TO_FNAME(SOUPFM_REC,IDX_TMF_FILE
     *           , CURR_SRC_FILE_NAME)
            CALL FILERR(CURR_SRC_FILE_NAME
     *           , FUN_CLOSE_FILE,ST,0)
        ENDIF
        
        RETURN
        END



        SUBROUTINE OPEN_FILE_SOUPFM_PRG_BLK(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:PRGREC.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        
        INTEGER*4  ST,I
CCCCCCCC File name CCCCCCCCCCCCCCCC
        CHARACTER*28 FILENAME
        
        ST = 0

        CALL COPY_TO_FNAME(SOUPFM_REC
     *        , IDX_PRG_FILE,INT_SRC_FILE_NAME)
        
        IF(SOUPFM_REC.LUN_TABLE(IDX_PRG_FILE) .EQ. 0) THEN
            CALL RESERVE_AVAILABLE_LUN(SOUPFM_REC,IDX_PRG_FILE,ST)
            IF(ST .NE. 0) RETURN
        ENDIF

        SOUPFM_REC.PRG_BLK_CNT = 0
        
        CALL OPENW( SOUPFM_REC.LUN_TABLE(IDX_PRG_FILE)
     *            , INT_SRC_FILE_NAME
     *            , 4, 0, 0
     *            , ST)

C        WRITE(*,*) ""
C        WRITE(*,*) "FILE PURGE IS: "
C        WRITE(FILENAME,10) (INT_SRC_FILE_NAME(I), I=1, 7)
C10      FORMAT('28A')        
C        WRITE(*,*) "--- ",FILENAME

        IF(ST .NE. 0) THEN
            CALL FILERR(INT_SRC_FILE_NAME
     *           , FUN_OPEN_FILE,ST,0)
            RETURN
        ENDIF
     
        CALL IOINIT(SOUPFM_REC.PRG_FDB, SOUPFM_REC.LUN_TABLE(IDX_PRG_FILE), PRGSEC*256)
        SOUPFM_REC.PRG_IND = 1
        SOUPFM_REC.PRG_BLK_CNT = 0
        SOUPFM_REC.PRG_BLK_FIRST_RUN = .TRUE.
        
        RETURN
        END




        SUBROUTINE READ_PRGREC_BLK(SOUPFM_REC, VALREC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:PRGREC.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:PRMVPF.DEF'
        
        INTEGER*4  ST, LENGTH, I
        INTEGER*4  VLEN(0:3)
        DATA       VLEN /1,2,3,4/
        
        ST = 0

        

        IF(  SOUPFM_REC.PRG_IND .GT. PRGBLC
     *  .OR. SOUPFM_REC.PRG_BLK_FIRST_RUN .EQ. .TRUE.) THEN
            SOUPFM_REC.PRG_IND = 1
            SOUPFM_REC.PRG_BLK_CNT = SOUPFM_REC.PRG_BLK_CNT + 1
            
            CALL READW(SOUPFM_REC.PRG_FDB,SOUPFM_REC.PRG_BLK_CNT,UPREC,ST)

C           (PRGRECLEN=PRGSEC*64) -- PRGSEC=128
C            WRITE(*,*) ""
C            DO I=1, PRGRECLEN  
C                WRITE(*,*) "Index:", I, " Value: ",UPREC(I)                               
C            ENDDO
C            WRITE(*,*) ""
            
            IF(ST .EQ. READW_EOF_REACHED) THEN ! EOF reached
               RETURN
            ENDIF
        
            IF(ST .NE. 0) THEN
               CALL COPY_TO_FNAME(SOUPFM_REC
     *              , IDX_PRG_FILE,INT_SRC_FILE_NAME)
            
               CALL FILERR(INT_SRC_FILE_NAME
     *              , FUN_READ_FILE,ST,0)
               RETURN
            ENDIF
            
            IF(SOUPFM_REC.PRG_BLK_FIRST_RUN .EQ. .TRUE.) THEN
               SOUPFM_REC.PRG_BLK_FIRST_RUN = .FALSE.
            ENDIF
        ENDIF
C
        LENGTH = ISHFT(UPBUF(VFSSER,SOUPFM_REC.PRG_IND),-30)
        LENGTH = VLEN(LENGTH)

        CALL LOGVAL(VALREC,UPBUF(1,SOUPFM_REC.PRG_IND))
        
        SOUPFM_REC.PRG_IND = SOUPFM_REC.PRG_IND + LENGTH
        
        IF(VALREC(VSSER) .EQ. 0) THEN
           ST = READW_EOF_REACHED
        ENDIF

        RETURN
        END



        SUBROUTINE CLOSE_FILE_SOUPFM_PRG_BLK(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4 ST

        ST = 0
        
        CALL CLOSEFIL(SOUPFM_REC.PRG_FDB)
        
        RETURN
        END



        SUBROUTINE OPEN_FILE_SOUPFM_PRG(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:PRGREC.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        
        INTEGER*4  ST
        
        ST = 0

        CALL COPY_TO_FNAME(SOUPFM_REC
     *        , IDX_PRG_FILE,INT_SRC_FILE_NAME)
        
        IF(SOUPFM_REC.LUN_TABLE(IDX_PRG_FILE) .EQ. 0) THEN
            CALL RESERVE_AVAILABLE_LUN(SOUPFM_REC,IDX_PRG_FILE,ST)
            IF(ST .NE. 0) RETURN
        ENDIF

        SOUPFM_REC.PRG_BLK_CNT = 0
        
        CALL IOPEN(INT_SRC_FILE_NAME,SOUPFM_REC.LUN_TABLE(IDX_PRG_FILE),VFLEN*2,VFSCDC,VFSSER*2-1,ST)

        IF(ST .NE. 0) THEN
            CALL FILERR(INT_SRC_FILE_NAME
     *           , FUN_OPEN_FILE,ST,0)
            RETURN
        ENDIF
     
        CALL ITUBSIZE(SOUPFM_REC.LUN_TABLE(IDX_PRG_FILE),SOUPFM_VLF_TUBSIZ)
        RETURN
        END


        SUBROUTINE READ_PRGREC(SOUPFM_REC, VALREC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:PRGREC.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:HSHCOM.DEF'
        
        INTEGER*4  ST
        
        ST = 0

        CALL ISREAD(V4BUF,SOUPFM_REC.LUN_TABLE(IDX_PRG_FILE),SOUPFM_REC.VLFBUF,ST)
        

        IF(ST .EQ. ERREND) THEN ! EOF reached
           RETURN
        ENDIF
        
        IF(ST .NE. 0) THEN
           CALL COPY_TO_FNAME(SOUPFM_REC
     *          , IDX_PRG_FILE,INT_SRC_FILE_NAME)
        
           CALL FILERR(INT_SRC_FILE_NAME
     *          , FUN_READ_FILE,ST,0)
           RETURN
        ENDIF

        CALL LOGVAL(VALREC,V4BUF)
        

        RETURN
        END



        SUBROUTINE CLOSE_FILE_SOUPFM_PRG(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4 ST

        ST = 0
        
        CALL ICLOSE(SOUPFM_REC.LUN_TABLE(IDX_PRG_FILE),SOUPFM_REC.VLFBUF,ST)
        IF(ST .NE. 0) THEN
            CALL COPY_TO_FNAME(SOUPFM_REC,IDX_PRG_FILE
     *           , CURR_SRC_FILE_NAME)
            CALL FILERR(CURR_SRC_FILE_NAME
     *           , FUN_CLOSE_FILE,ST,0)
            RETURN
        ENDIF
        
        RETURN
        END



        SUBROUTINE OPEN_FILE_SOUPFM_PRG_PAS(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VALPASFIL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'    
        INCLUDE 'INCLIB:HSHCOM.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DPAREC.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:PASPURGE.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4  ST, I
        
        CHARACTER*20 FILENAME
        INTEGER*4 I4FILENAME(5)
        EQUIVALENCE(I4FILENAME,FILENAME)
        
        ST = 0

        IF(SOUPFM_REC.LUN_TABLE(IDX_CURR_PRG_PAS_FILE) .EQ. 0) THEN
            CALL RESERVE_AVAILABLE_LUN(SOUPFM_REC,IDX_CURR_PRG_PAS_FILE,ST)
            IF(ST .NE. 0) RETURN
        ENDIF

        CALL IOPEN(PASVPFFIL(1,SOUPFM_REC.PAS_EMIS,SOUPFM_REC.PAS_GIND)
     *          , SOUPFM_REC.LUN_TABLE(IDX_CURR_PRG_PAS_FILE)
     *          , VPFLEN*2 
     *          , VFSCDC
     *          , VFSSER*2-1
     *          , ST)
        IF(ST .NE. 0) THEN
            CALL FILERR(PASVPFFIL(1,SOUPFM_REC.PAS_EMIS,SOUPFM_REC.PAS_GIND)
     *          , FUN_OPEN_FILE, ST, 0)
            RETURN
        ENDIF
        
        CALL ITUBSIZE(SOUPFM_REC.LUN_TABLE(IDX_CURR_PRG_PAS_FILE),SOUPFM_TUBSIZ)

        SOUPFM_REC.PAS_VPF_FILE_CNT = SOUPFM_REC.PAS_VPF_FILE_CNT + 1
        
        DO I = 1, 5
            I4FILENAME(I) = PASVPFFIL(I,SOUPFM_REC.PAS_EMIS,SOUPFM_REC.PAS_GIND)
        ENDDO
        
        TYPE *, IAM(), 'Aberto o ficheiro ' // FILENAME 
        RETURN
        END



        SUBROUTINE READ_PRGREC_PAS(SOUPFM_REC, VALREC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VALPASFIL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'    
        INCLUDE 'INCLIB:HSHCOM.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DPAREC.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:PASPURGE.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4  ST
        
        ST = 0

        CALL ISREAD(V4BUF_PAS, SOUPFM_REC.LUN_TABLE(IDX_CURR_PRG_PAS_FILE)
     *          , SOUPFM_REC.PAS_VPFBUF, ST)
        IF(ST .EQ. 0) THEN
            CALL LOGPAS(VALREC,V4BUF_PAS)
            SOUPFM_REC.PAS_CURR_REC_CNT = SOUPFM_REC.PAS_CURR_REC_CNT + 1
        ELSE IF(ST .EQ. ERREND) THEN ! EOF reached
            RETURN
        ELSE
            CALL FILERR(PASVPFFIL(1,SOUPFM_REC.PAS_EMIS,SOUPFM_REC.PAS_GIND)
     *          , FUN_READ_FILE, ST, SOUPFM_REC.PAS_CURR_REC_CNT)
        ENDIF
        RETURN
        END


        SUBROUTINE CLOSE_FILE_SOUPFM_PRG_PAS(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VALPASFIL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'    
        INCLUDE 'INCLIB:HSHCOM.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DPAREC.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:PASPURGE.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4 ST
        
        ST = 0
        CALL ICLOSE(SOUPFM_REC.LUN_TABLE(IDX_CURR_PRG_PAS_FILE)
     *          ,SOUPFM_REC.PAS_VPFBUF,ST)
        IF(ST .NE. 0) THEN
            CALL FILERR(PASVPFFIL(1,SOUPFM_REC.PAS_EMIS,SOUPFM_REC.PAS_GIND)
     *          , FUN_CLOSE_FILE, ST, 0)
            RETURN
        ENDIF
        END


        SUBROUTINE OPEN_KEY_FILE_SOUPFM_VLF(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INTEGER*4  ST, I

        CHARACTER*20 FILENAME
        INTEGER*4 I4FILENAME(5)
        EQUIVALENCE(I4FILENAME,FILENAME)
        
        ST = 0

        IF(SOUPFM_REC.LUN_TABLE(IDX_CURR_VLF_FILE) .EQ. 0) THEN
            CALL RESERVE_AVAILABLE_LUN(SOUPFM_REC,IDX_CURR_VLF_FILE,ST)
            IF(ST .NE. 0) RETURN
        ENDIF

        CALL IOPEN(SFNAMES(1,VLF)
     *          , SOUPFM_REC.LUN_TABLE(IDX_CURR_VLF_FILE)
     *          , VFLEN*2 
     *          , VFSCDC
     *          , VFSSER*2-1
     *          , ST)

        IF(ST .NE. 0) THEN
            CALL FILERR(SFNAMES(1,VLF)
     *          , FUN_OPEN_FILE, ST, 0)
            RETURN
        ENDIF

        DO I = 1, 5
            I4FILENAME(I) = SFNAMES(I,VLF)
        ENDDO
        
        IF(SOUPFM_REC.PAS_DEBUG .EQ. .TRUE.) THEN
            TYPE *, IAM(), 'Aberto o ficheiro ' // FILENAME
        ENDIF 
        RETURN

        END
        



        SUBROUTINE READ_KEY_FILE_SOUPFM_VLF(SOUPFM_REC, VALREC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:HSHCOM.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4  ST
        INTEGER*4  VKEY(2)
        
        ST = 0

        VKEY(1) = SOUPFM_REC.VLF_CDC
        VKEY(2) = SOUPFM_REC.VLF_SERIAL

        CALL IREAD(VKEY,V4BUF,
     *     SOUPFM_REC.LUN_TABLE(IDX_CURR_VLF_FILE),ST)

        IF(ST .EQ. 0) THEN
            CALL LOGVAL(VALREC,V4BUF)
        ELSE IF(ST .EQ. ERREND) THEN ! EOF reached
            RETURN
        ELSE IF(ST .EQ. ERRRNF) THEN ! Not found
            RETURN
        ELSE
            CALL FILERR(SFNAMES(1,VLF)
     *          , FUN_READ_FILE, ST, 0)
        ENDIF
        RETURN
        END


        
        
        SUBROUTINE CLOSE_KEY_FILE_SOUPFM_VLF(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4 ST
        
        ST = 0
        CALL ICLOSE(SOUPFM_REC.LUN_TABLE(IDX_CURR_VLF_FILE)
     *          ,SOUPFM_REC.VLFBUF,ST)
        IF(ST .NE. 0) THEN
            CALL FILERR(SFNAMES(1,VLF)
     *          , FUN_CLOSE_FILE, ST, 0)
            RETURN
        ENDIF
        END




        
        SUBROUTINE OPEN_KEY_FILE_SOUPFM_PRG_PAS(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VALPASFIL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'    
        INCLUDE 'INCLIB:HSHCOM.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DPAREC.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:PASPURGE.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4  ST, I
        
        CHARACTER*20 FILENAME
        INTEGER*4 I4FILENAME(5)
        EQUIVALENCE(I4FILENAME,FILENAME)
        
        ST = 0

        IF(SOUPFM_REC.LUN_TABLE(IDX_CURR_PRG_PAS_FILE) .EQ. 0) THEN
            CALL RESERVE_AVAILABLE_LUN(SOUPFM_REC,IDX_CURR_PRG_PAS_FILE,ST)
            IF(ST .NE. 0) RETURN
        ENDIF

        CALL IOPEN(PASVPFFIL(1,SOUPFM_REC.PAS_EMIS,SOUPFM_REC.PAS_GIND)
     *          , SOUPFM_REC.LUN_TABLE(IDX_CURR_PRG_PAS_FILE)
     *          , VPFLEN*2 
     *          , VFSCDC
     *          , VFSSER*2-1
     *          , ST)
        IF(ST .NE. 0) THEN
            CALL FILERR(PASVPFFIL(1,SOUPFM_REC.PAS_EMIS,SOUPFM_REC.PAS_GIND)
     *          , FUN_OPEN_FILE, ST, 0)
            RETURN
        ENDIF
        
        DO I = 1, 5
            I4FILENAME(I) = PASVPFFIL(I,SOUPFM_REC.PAS_EMIS,SOUPFM_REC.PAS_GIND)
        ENDDO
        
        IF(SOUPFM_REC.PAS_DEBUG .EQ. .TRUE.) THEN
            TYPE *, IAM(), 'Aberto o ficheiro ' // FILENAME
        ENDIF 
        RETURN
        END

        SUBROUTINE READ_KEY_PRGREC_PAS(SOUPFM_REC, VALREC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VALPASFIL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'    
        INCLUDE 'INCLIB:HSHCOM.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DPAREC.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:PASPURGE.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        
        INTEGER*4  ST
        INTEGER*4  VKEY(2)
        
        ST = 0

        VKEY(1) = SOUPFM_REC.PAS_FRAC
        VKEY(2) = ISHFT(SOUPFM_REC.PAS_SERIAL,24) + SOUPFM_REC.PAS_NUM

        CALL IREAD(VKEY,V4BUF_PAS,
     *     SOUPFM_REC.LUN_TABLE(IDX_CURR_PRG_PAS_FILE),ST)

        IF(ST .EQ. 0) THEN
            CALL LOGPAS(VALREC,V4BUF_PAS)
        ELSE IF(ST .EQ. ERREND) THEN ! EOF reached
            RETURN
        ELSE IF(ST .EQ. ERRRNF) THEN ! Not found
            RETURN
        ELSE
            CALL FILERR(PASVPFFIL(1,SOUPFM_REC.PAS_EMIS,SOUPFM_REC.PAS_GIND)
     *          , FUN_READ_FILE, ST, SOUPFM_REC.PAS_CURR_REC_CNT)
        ENDIF
        RETURN
        END

        SUBROUTINE CLOSE_KEY_FILE_SOUPFM_PRG_PAS(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VALPASFIL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'    
        INCLUDE 'INCLIB:HSHCOM.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DPAREC.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:PASPURGE.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4 ST
        
        ST = 0
        CALL ICLOSE(SOUPFM_REC.LUN_TABLE(IDX_CURR_PRG_PAS_FILE)
     *          ,SOUPFM_REC.PAS_VPFBUF,ST)
        IF(ST .NE. 0) THEN
            CALL FILERR(PASVPFFIL(1,SOUPFM_REC.PAS_EMIS,SOUPFM_REC.PAS_GIND)
     *          , FUN_CLOSE_FILE, ST, 0)
            RETURN
        ENDIF
        END







        SUBROUTINE PRINT_REPORT_HEADER(SOUPFM_REC)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        
        CALL COPY_TO_FNAME(SOUPFM_REC, IDX_TMF_FILE, INT_SRC_FILE_NAME)

        TYPE *, IAM()
        TYPE *, '-'
     *        , '--------------------------------'
     *        , '-'
     *        , '--------------'
     *        , '-'
     *        , '---------------------'
     *        , '-'
        TYPE *, ' Relatorio de geracao de ficheiros'
        TYPE *, '-'
     *        , '--------------------------------'
     *        , '-'
     *        , '--------------'
     *        , '-'
     *        , '---------------------'
     *        , '-'
        TYPE *, ' Ficheiro de transaccoes lido          : ', CHR_SRC_FILE_NAME
        TYPE *, ' Nr. transaccoes lidas                 : ', SOUPFM_REC.TOTAL_TRX_CNT
        CALL COPY_TO_FNAME(SOUPFM_REC, IDX_PRG_FILE, INT_SRC_FILE_NAME)
        TYPE *, ' Ficheiro de validacoes lido           : ', CHR_SRC_FILE_NAME
        TYPE *, ' Nr. validacoes lidas                  : ', SOUPFM_REC.TOTAL_VAL_CNT
        TYPE *, ' Nr. ficheiros validacao passiva lidos : ', SOUPFM_REC.PAS_VPF_FILE_CNT
        TYPE *, ' Nr. validacoes passiva lidas          : ', SOUPFM_REC.PAS_CURR_REC_CNT
        TYPE *, '-'
     *        , '--------------------------------'
     *        , '-'
     *        , '--------------'
     *        , '-'
     *        , '---------------------'
     *        , '-'
        TYPE *, ' '
     *        , 'Nome Ficheiro                   '
     *        , ' '
     *        , 'Nr. Linhas    '
     *        , ' '
     *        , 'Nr. Trans. Proc.     '
     *        , ' '
        TYPE *, '-'
     *        , '--------------------------------'
     *        , '-'
     *        , '--------------'
     *        , '-'
     *        , '---------------------'
     *        , '-'

        RETURN
        END


        SUBROUTINE PRINT_REPORT_LINE(SOUPFM_REC, INDEX)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4 INDEX
        CALL COPY_TO_FNAME(SOUPFM_REC, INDEX, INT_SRC_FILE_NAME)

        TYPE *, ' '
     *        , GENERATED_FILES_LIST(INDEX)
     *        , ' '
     *        , SOUPFM_REC.LINE_CNT(INDEX) + 2
     *        , '     '
     *        , SOUPFM_REC.TRX_CNT(INDEX)
     *        , ' '
        TYPE *, '     '
     *        , CHR_SRC_FILE_NAME
        TYPE *

        RETURN
        END


        SUBROUTINE PRINT_IBAN(IBAN_BUF, COUNTRY_PREFIX, NIB_ARR)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        CHARACTER*25 IBAN_BUF
        CHARACTER*4  COUNTRY_PREFIX
        INTEGER*4    NIB_ARR(5)

        IF(   NIB_ARR(1) .EQ. 0
     *  .AND. NIB_ARR(2) .EQ. 0
     *  .AND. NIB_ARR(3) .EQ. 0
     *  .AND. NIB_ARR(4) .EQ. 0
     *  .AND. NIB_ARR(5) .EQ. 0 ) THEN
            WRITE (IBAN_BUF, 10002)
        ELSE
            WRITE (IBAN_BUF, 10001)
     *                COUNTRY_PREFIX
     *              , NIB_ARR(1)
     *              , NIB_ARR(2)
     *              , NIB_ARR(3)
     *              , NIB_ARR(4)
     *              , NIB_ARR(5)
        ENDIF
        RETURN
10001   FORMAT(
     *            A4
     *          , I4.4
     *          , I4.4
     *          , I9.9
     *          , I2.2
     *          , I2.2
     *  )
10002   FORMAT(25(' '))
        END
        
        
        
        SUBROUTINE PRINT_MEDIATOR(MEDIATOR_BUF, MEDIATOR_ID)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        CHARACTER*7  MEDIATOR_BUF
        INTEGER*4    MEDIATOR_ID

        IF(   MEDIATOR_ID .EQ. 0 ) THEN
            WRITE (MEDIATOR_BUF, 10002)
        ELSE
            WRITE (MEDIATOR_BUF, 10001)
     *                MEDIATOR_ID
        ENDIF
        RETURN
10001   FORMAT(
     *            I7.7
     *  )
10002   FORMAT(7(' '))
        END


C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        SUBROUTINE PRINT_MEDIATOR_REP(MEDIATOR_BUF, MEDIATOR_ID)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        CHARACTER*8  MEDIATOR_BUF
        INTEGER*4    MEDIATOR_ID

        IF(   MEDIATOR_ID .EQ. 0 ) THEN
            WRITE (MEDIATOR_BUF, 10002)
        ELSE
            WRITE (MEDIATOR_BUF, 10001)
     *                MEDIATOR_ID / 100000
     *              , MOD(MEDIATOR_ID, 100000)
        ENDIF
        RETURN
10001   FORMAT(
     *            I2.2
     *          , '-'
     *          , I5.5
     *  )
10002   FORMAT(8(' '))
        END


        SUBROUTINE PRINT_EXTERNAL_BET_REF(EXT_BET_BUF, REC_EXT_BET_REF, HIDE_CHK_DIGITS)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        CHARACTER*16 EXT_BET_BUF
        INTEGER*4    REC_EXT_BET_REF(3), I
        LOGICAL      HIDE_CHK_DIGITS
        LOGICAL      ALL_ZEROS
        
        ALL_ZEROS = .TRUE.
        DO I = 1,3
            IF(REC_EXT_BET_REF(I) .NE. 0) THEN
                ALL_ZEROS = .FALSE.
            ENDIF
        ENDDO
        
        IF( ALL_ZEROS ) THEN
            WRITE (EXT_BET_BUF, 10003)
C    *                REC_EXT_BET_REF(1)
C    *              , REC_EXT_BET_REF(2)
C    *              , REC_EXT_BET_REF(3)
            RETURN
        ENDIF

        IF( HIDE_CHK_DIGITS ) THEN
            WRITE (EXT_BET_BUF, 10002)
     *                REC_EXT_BET_REF(1)
     *              , REC_EXT_BET_REF(2)
C    *              , REC_EXT_BET_REF(3)
        ELSE
            WRITE (EXT_BET_BUF, 10001)
     *                REC_EXT_BET_REF(1)
     *              , REC_EXT_BET_REF(2)
     *              , REC_EXT_BET_REF(3)
        ENDIF
        RETURN
10001   FORMAT(
     *            I3.3
     *          , '-'
     *          , I8.8
     *          , '-'
     *          , I3.3
     *  )
10002   FORMAT(
     *            I3.3
     *          , '-'
     *          , I8.8
     *          , '-'
     *          , '***'
     *  )
10003   FORMAT(
     *            '   '
     *          , ' '
     *          , '        '
     *          , ' '
     *          , '   '
     *  )
        END
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------

        
        SUBROUTINE GET_PAS_EMI(INP_DRAW_NR, INP_GAME_IND, OUT_PAS_EMI)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'

        INTEGER*4  INP_DRAW_NR, INP_GAME_IND, OUT_PAS_EMI
        INTEGER*4  I

        OUT_PAS_EMI = 0
        DO I = 1, PAGEMI
            IF(PASEMIS(I,INP_GAME_IND) .EQ. INP_DRAW_NR) THEN
                OUT_PAS_EMI = I
                RETURN 
            ENDIF 
        ENDDO
        END
        
        
        
        
        
        SUBROUTINE GET_SYS_DATE(OUT_DATE)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        
        INTEGER*4 CDAT(8)
        INTEGER*4 OUT_DATE(3)

        CALL GDATE(CDAT(2),CDAT(3),CDAT(1))
        IF(CDAT(1).LT.77) THEN
          CDAT(1) = CDAT(1) + 2000
        ELSE
          CDAT(1) = CDAT(1) + 1900
        ENDIF
        
        OUT_DATE(1) = CDAT(1) ! Year
        OUT_DATE(2) = CDAT(2) ! Month
        OUT_DATE(3) = CDAT(3) ! Day
        
        END

        
        
        
        SUBROUTINE DUMP_TRABUF(CNT, TRABUF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'

        INTEGER*4 CNT
     
        CHARACTER*80 BUFFER
        
        CHARACTER*4  STR_TGAMTYP(0:17)
        
        DATA  STR_TGAMTYP /
     *            'TIN '
     *          , 'TLTO'
     *          , 'TSPT'
     *          , 'TNBR'
     *          , 'TKIK'
     *          , 'TBNG'
     *          , 'TWIT'
     *          , 'TCPL'
     *          , 'TTRP'
     *          , 'TDBL'
     *          , 'TSTR'
     *          , 'TTSL'
     *          , 'TSCR'
     *          , 'TSSC'
     *          , 'TINS'
     *          , 'TTGL'
     *          , 'TPAS'
     *          , 'TEUM'
     *  /
        
        TYPE *, '======================================================'
        WRITE (BUFFER, 10000) CNT, TRABUF(TCDC), TRABUF(TSER)
        TYPE *, BUFFER
        TYPE *, '------------------------------------------------------'
        WRITE (BUFFER, 10001) STAT(TRABUF(TSTAT)), TRABUF(TSTAT)
        TYPE *, BUFFER
        WRITE (BUFFER, 10002) TTYPE(TRABUF(TTYP)), TRABUF(TTYP)
        TYPE *, BUFFER
        WRITE (BUFFER, 10003) ERROR(TRABUF(TERR)), TRABUF(TERR)
        TYPE *, BUFFER
        WRITE (BUFFER, 10004) STR_TGAMTYP(TRABUF(TGAMTYP)), TRABUF(TGAMTYP)
        TYPE *, BUFFER
        RETURN
10000   FORMAT(' COUNT      = ', I9, ' : ID = ', I4.4,I9.9)
10001   FORMAT('    TSTAT   = ', A4, ' [', I9, ']')
10002   FORMAT('    TTYP    = ', A4, ' [', I9, ']')
10003   FORMAT('    TERR    = ', A4, ' [', I9, ']')
10004   FORMAT('    TGAMTYP = ', A4, ' [', I9, ']')
        END


C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        SUBROUTINE OPEN_GAME_DATA_FILES(SOUPFM_REC, ST)
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
        INCLUDE 'INCLIB:DPAREC.DEF'
        
        INTEGER*4 ST, GTYP, GNUM, LUN, DRAW, I
        INTEGER*4  FDB(7)
        
        DO I = 1, MAXGAM
            GTYP = GNTTAB(GAMTYP,I)
            IF ( GTYP .EQ. TLTO
     *      .OR. GTYP .EQ. TKIK
     *      .OR. GTYP .EQ. TSPT
     *      ) THEN
                ST = 0
                CALL RESERVE_AVAILABLE_LUN_GAME_FILE(SOUPFM_REC,I,ST)
                IF(ST .NE. 0) RETURN
                LUN = SOUPFM_REC.GAME_DATA_REC.GLUN(I)
                CALL OPENW(LUN
     *                   , GFNAMES(1,I)
     *                   , 4, 0, 0, ST)
                IF( GTYP .EQ. TLTO ) THEN
                    CALL IOINIT(SOUPFM_REC.GAME_DATA_REC.GFDB(1,I),LUN,DLTSEC*256)
                ENDIF
                IF( GTYP .EQ. TKIK ) THEN
                    CALL IOINIT(SOUPFM_REC.GAME_DATA_REC.GFDB(1,I),LUN,DKKSEC*256)
                ENDIF
                IF( GTYP .EQ. TSPT ) THEN
                    CALL IOINIT(SOUPFM_REC.GAME_DATA_REC.GFDB(1,I),LUN,DSPSEC*256)
                ENDIF
                IF(ST .NE. 0) THEN
                    CALL USRCLOS1(LUN)
                    RETURN
                ENDIF
            ENDIF
        ENDDO

        RETURN
        END



        SUBROUTINE CLOSE_GAME_DATA_FILES(SOUPFM_REC, ST)
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
        
        INTEGER*4 ST, GTYP, GNUM, LUN, DRAW, I
        INTEGER*4  FDB(7)

        ST = 0
        DO I = 1, MAXGAM
            GTYP = GNTTAB(GAMTYP,I)
            IF ( GTYP .EQ. TLTO
     *      .OR. GTYP .EQ. TKIK
     *      .OR. GTYP .EQ. TSPT
     *      ) THEN
                LUN = SOUPFM_REC.GAME_DATA_REC.GLUN(I)
                CALL USRCLOS1(LUN)
            ENDIF
        ENDDO

        RETURN
        END




        SUBROUTINE READ_GAME_DATA(SOUPFM_REC, ST)
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
        
        INTEGER*4 ST, GTYP, GNUM, LUN, DRAW
        INTEGER*4  FDB(7)
        
        ST = 0
        ! input : game number
        GNUM = SOUPFM_REC.GAME_DATA_REC.IN_GAME_NUM
        ! input : draw number
        DRAW = SOUPFM_REC.GAME_DATA_REC.IN_DRAW_NUM
        
        GTYP = GNTTAB(GAMTYP,GNUM)
        
        IF (GTYP .EQ. TLTO) THEN
            CALL READW(SOUPFM_REC.GAME_DATA_REC.GFDB(1,GNUM),DRAW,DLTREC,ST)
            IF(ST .NE. 0) THEN
                CALL USRCLOS1(LUN)
                RETURN
            ENDIF
            !output
            SOUPFM_REC.GAME_DATA_REC.OUT_DRAW_DATE_CDC = DLTDAT(CURDRW)
            RETURN 
        ENDIF
        IF (GTYP .EQ. TKIK) THEN
            CALL READW(SOUPFM_REC.GAME_DATA_REC.GFDB(1,GNUM),DRAW,DKKREC,ST)
            IF(ST .NE. 0) THEN
                CALL USRCLOS1(LUN)
                RETURN
            ENDIF
            !output
            SOUPFM_REC.GAME_DATA_REC.OUT_DRAW_DATE_CDC = DKKDAT(CURDRW)
            RETURN 
        ENDIF
        IF (GTYP .EQ. TSPT) THEN
            CALL READW(SOUPFM_REC.GAME_DATA_REC.GFDB(1,GNUM),DRAW,DSPREC,ST)
            IF(ST .NE. 0) THEN
                CALL USRCLOS1(LUN)
                RETURN
            ENDIF
            !output
            SOUPFM_REC.GAME_DATA_REC.OUT_DRAW_DATE_CDC = DSPDAT(CURDRW)
            RETURN 
        ENDIF
        ST = -1
        RETURN
        END


        CHARACTER FUNCTION SOUPFM_DMONYI8*32(IN_VALUE)
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'


        INTEGER*8 IN_VALUE
        CHARACTER*32 OUT_BUF
        CHARACTER*32 AUX_OUT

        INTEGER*4 I,J,DIG_CNT
        INTEGER*8 AUX
        INTEGER*8 REMAIN
        INTEGER*8 DIGIT
        CHARACTER*1 CH

        DO I = 1, 32
            AUX_OUT(I:I) = ' '        ! reset to SPACES
            OUT_BUF(I:I) = ' '        ! reset to SPACES
        ENDDO

        AUX = KIABS(IN_VALUE)
        I = 1                        !!iterator counter
        J = 1                        !!struct position
        DIG_CNT = 1
        DO WHILE(AUX .NE. 0)
            REMAIN = AUX / KZEXT(10)
            DIGIT = KMOD(AUX,KZEXT(10))
            CH = CHAR(KZEXT(48) + DIGIT)
C           SET ',' CHAR AT 3RD POSITION
            IF(I .EQ. 2) THEN
               DIG_CNT = 1
               AUX_OUT(J:J) = CH
               J = J + 1
               CH = ','
               AUX_OUT(J:J) = CH
               J = J + 1
C           SET '.' AT 7TH AND 11TH POSITION
            ELSEIF(MOD(DIG_CNT-1,3) .EQ. 0 .AND. DIG_CNT .NE. 1) THEN
               AUX_OUT(J:J) = '.'
               J = J + 1
               AUX_OUT(J:J) = CH
               DIG_CNT = DIG_CNT + 1
               J = J + 1
            ELSE
               AUX_OUT(J:J) = CH
               J = J + 1
               DIG_CNT = DIG_CNT + 1
            ENDIF
            I = I + 1
            AUX = REMAIN
        ENDDO

C       IF ONLY ONE DIGIT FORCE LEFT ZERO(S)
        IF(I .EQ. 1) THEN
           AUX_OUT(1:4) = '00,0'
        ENDIF

C       IF ONLY ONE DIGIT FORCE LEFT ZERO(S)
        IF(I .EQ. 2) THEN
           AUX_OUT(2:4) = '0,0'
        ENDIF
C       IF ONLY TWO DIGITS FORCE LEFT ZERO(S)
        IF(I .EQ. 3) THEN
           AUX_OUT(4:4) = '0'
        ENDIF

C       INVERT STRING
        J=13
        DO I = 1, J
            OUT_BUF(I:I) = AUX_OUT(J+1-I:J+1-I)
        ENDDO

C       RETURN OUT_BUF
        SOUPFM_DMONYI8=OUT_BUF

        RETURN

        END
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------




