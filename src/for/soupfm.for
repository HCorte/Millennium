C
C PROGRAM SOUPFM - SOUP File Management
C
C This program will manage the generation of the SOUP files
C
C V02 08-APR-2014 SCML Adding support for new expired prizes reports
C V01 12-DEC-2013 SCML Program creation
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM SOUPFM
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:TNAMES.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:FTNAMES.DEF'
	INCLUDE 'INCLIB:ITNAMES.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
C----+------------------------------------------------------------------
C V01| New variables and constants
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:PRMHSH.DEF'
C----+------------------------------------------------------------------
C V01| New variables and constants
C----+------------------------------------------------------------------
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:PASIOSUBS.DEF'
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------

        INTEGER*4   LOGREC(LREC*3)          !
        INTEGER*4   I                       !
        INTEGER*4   SER                     !
        INTEGER*4   TER                     !
        INTEGER*4   ST                      !
        INTEGER*4   NOCHECK0                !

        LOGICAL  EOT/.FALSE./               !

        INTEGER*4  NOCONSIG
        EXTERNAL   NOCONSIG

C----+------------------------------------------------------------------
C V01| New variables and constants
C----+------------------------------------------------------------------
        INTEGER*4  INDEX
        INTEGER*4  MAXCDC
        LOGICAL    EXIT_CONDITION
        INTEGER*4  PAS_GIND
        INTEGER*4  PAS_EMIS
        
C----+------------------------------------------------------------------
C V01| New variables
C----+------------------------------------------------------------------
        RECORD /STPASFDB/  PASFDB
        RECORD /STPASREC/  PASREC

        INTEGER*4  GAM

        COMMON /NOCHECK0/ NOCHECK0
         
        CALL TO_CONSOLE('Copyright 2013 SCML. All rights reserved.')

        CALL LIB$ESTABLISH(NOCONSIG)

        NOCHECK0 = -1
        
C----+------------------------------------------------------------------
C V01| Initializing structures
C----+------------------------------------------------------------------
        CALL INIT_SOUPFM_REC(SOUPFM_REC)
        
C----+------------------------------------------------------------------
C V01| Menu
C----+------------------------------------------------------------------
        TYPE *
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
C       TYPE *,'<<<<< SOUP File Management   V02 >>>>>'
        TYPE *,'<<<<< SOUP File Management   V02 >>>>>'
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        TYPE *


C----+------------------------------------------------------------------
C V01| Getting cdc date
C----+------------------------------------------------------------------
        ST = 0
        CALL GET_CDC_DATE(SOUPFM_REC, ST)
        IF(ST .NE. 0) THEN
           CALL GSTOP(GEXIT_FATAL)
        ENDIF

C----+------------------------------------------------------------------
C V01| Getting TMF file name; open file
C----+------------------------------------------------------------------
        EXIT_CONDITION = .FALSE.

        ST = 0
        CALL GET_TMF_FILENAME(SOUPFM_REC, ST)
        IF(ST .NE. 0) THEN
           CALL GSTOP(GEXIT_FATAL)
        ENDIF

C----+------------------------------------------------------------------
C V01| Getting PRG file name
C----+------------------------------------------------------------------
        ST = 0
        CALL GET_PRG_FILENAME(SOUPFM_REC, ST)
        IF(ST .NE. 0) THEN
           CALL GSTOP(GEXIT_FATAL)
        ENDIF

C----+------------------------------------------------------------------
C V01| Main loop
C----+------------------------------------------------------------------
        DO WHILE( .TRUE. )

C----+------------------------------------------------------------------
C V01| Picking files to generate
C----+------------------------------------------------------------------
            CALL INIT_SOUPFM_REC_AUX(SOUPFM_REC)
            ST = 0
            CALL PICK_FILES_TO_GENERATE(SOUPFM_REC, ST)
            IF(ST .NE. 0) THEN
               CALL GSTOP(GEXIT_FATAL)
            ENDIF
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
            CALL OPEN_GAME_DATA_FILES(SOUPFM_REC, ST)
            IF(ST .NE. 0) THEN
               CALL GSTOP(GEXIT_FATAL)
            ENDIF
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------

C----+------------------------------------------------------------------
C V01| Opening TMF file
C----+------------------------------------------------------------------
            ST = 0
            CALL OPEN_FILE_SOUPFM_TMF(SOUPFM_REC, ST)
            IF(ST .NE. 0) THEN
               CALL GSTOP(GEXIT_FATAL)
            ENDIF

C----+------------------------------------------------------------------
C V01| Opening PRG file
C----+------------------------------------------------------------------
C            ST = 0
C            CALL OPEN_FILE_SOUPFM_PRG(SOUPFM_REC, ST)
C            IF(ST .NE. 0) THEN
C               CALL GSTOP(GEXIT_FATAL)
C            ENDIF
            ST = 0
            CALL OPEN_FILE_SOUPFM_PRG_BLK(SOUPFM_REC, ST)
            IF(ST .NE. 0) THEN
               CALL GSTOP(GEXIT_FATAL)
            ENDIF

C----+------------------------------------------------------------------
C V01| Opening output file
C----+------------------------------------------------------------------
           ST = 0
           CALL OPEN_SOUPFM_FILES(SOUPFM_REC, ST)
           IF(ST .NE. 0) THEN
              CALL GSTOP(GEXIT_FATAL)
           ENDIF

C----+------------------------------------------------------------------
C V01| Start TMF File Read cycle
C----+------------------------------------------------------------------
           EXIT_CONDITION = .FALSE.
           SOUPFM_REC.TOTAL_TRX_CNT = 0
           SER = 0
           EOT = .FALSE.
           DO WHILE (EXIT_CONDITION .EQ. .FALSE.)
               CALL READTMF(LOGREC,SER,EOT)
               IF(EOT) THEN
                   EXIT_CONDITION = .TRUE.
               ELSE
                   SOUPFM_REC.TOTAL_TRX_CNT = SOUPFM_REC.TOTAL_TRX_CNT + 1
                   CALL HANDLE_TRANSACTION(LOGREC, SOUPFM_REC, ST)
                   IF(ST .NE. 0) THEN
                       CALL GSTOP(GEXIT_FATAL)
                   ENDIF
               ENDIF
           ENDDO

C----+------------------------------------------------------------------
C V01| Start PRG File Read cycle
C----+------------------------------------------------------------------
C           EXIT_CONDITION = .FALSE.
C           SOUPFM_REC.TOTAL_VAL_CNT = 0
C           DO WHILE (EXIT_CONDITION .EQ. .FALSE.)
C               ST = 0
C               CALL READ_PRGREC(SOUPFM_REC,VALREC,ST)
C               
C               IF(ST .EQ. ERREND .OR. ST .NE. 0) THEN
C                   EXIT_CONDITION = .TRUE.
C               ELSE
C                   SOUPFM_REC.TOTAL_VAL_CNT = SOUPFM_REC.TOTAL_VAL_CNT + 1
C                   ST = 0
C                   CALL HANDLE_VALIDATION(VALREC, SOUPFM_REC, ST)
C                   IF(ST .NE. 0) THEN
C                       CALL GSTOP(GEXIT_FATAL)
C                   ENDIF
C               ENDIF
C           ENDDO
           EXIT_CONDITION = .FALSE.
           SOUPFM_REC.TOTAL_VAL_CNT = 0
           DO WHILE (EXIT_CONDITION .EQ. .FALSE.)
               ST = 0
               CALL READ_PRGREC_BLK(SOUPFM_REC,VALREC,ST)
               
               IF(ST .EQ. READW_EOF_REACHED .OR. ST .NE. 0) THEN
                   EXIT_CONDITION = .TRUE.
               ELSE
                   SOUPFM_REC.TOTAL_VAL_CNT = SOUPFM_REC.TOTAL_VAL_CNT + 1
                   ST = 0
                   CALL HANDLE_VALIDATION(VALREC, SOUPFM_REC, ST)
                   IF(ST .NE. 0) THEN
                       CALL GSTOP(GEXIT_FATAL)
                   ENDIF
               ENDIF
           ENDDO

C----+------------------------------------------------------------------
C V01| Start Passive PRG File Read cycle
C----+------------------------------------------------------------------
           ST = 0
           DO PAS_GIND = 1, NUMPAS
             SOUPFM_REC.PAS_GIND = PAS_GIND
             SOUPFM_REC.PAS_GNUM = GTNTAB(TPAS,SOUPFM_REC.PAS_GIND)
             IF (SOUPFM_REC.PAS_GNUM.GT.0) THEN
               DO PAS_EMIS = 1, PAGEMI
                 SOUPFM_REC.PAS_EMIS = PAS_EMIS
                 IF(   PASPRGCDC(
     *                   SOUPFM_REC.PAS_EMIS
     *                 , SOUPFM_REC.PAS_GIND) .EQ. SOUPFM_REC.SRC_CDC_DATE
     *           .AND. PASPRGCDC(
     *                   SOUPFM_REC.PAS_EMIS
     *                 , SOUPFM_REC.PAS_GIND) .GT. 0
     *           .AND.(PASSUBSTS(
     *                   SOUPFM_REC.PAS_EMIS
     *                 , SOUPFM_REC.PAS_GIND) .EQ. PDRWPUR
C----+------------------------------------------------------------------
C V02| Adding support for contingency mode
C----+------------------------------------------------------------------
     *                 .OR.(PASSUBSTS(
     *                        SOUPFM_REC.PAS_EMIS
     *                      , SOUPFM_REC.PAS_GIND) .EQ. PDRWCLO
     *                      .AND. SOUPFM_REC.SRC_CDC_DATE .LT. DAYCDC
     *                 )
     *           ) 
     *       ) THEN 

C----+------------------------------------------------------------------
C V01| Opening PRG Passive file
C----+------------------------------------------------------------------
                   ST = 0
                   CALL OPEN_FILE_SOUPFM_PRG_PAS(SOUPFM_REC, ST)
                   IF(ST .NE. 0) THEN
                      CALL GSTOP(GEXIT_FATAL)
                   ENDIF
                    
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
                   CALL PASIO_INIT(PASFDB,SOUPFM_REC.PAS_GIND,
     *                       PASEMIS(SOUPFM_REC.PAS_EMIS,SOUPFM_REC.PAS_GIND),
     *                       PASNUMTCK(SOUPFM_REC.PAS_EMIS,SOUPFM_REC.PAS_GIND)-1,
     *                       PASNUMSER(SOUPFM_REC.PAS_EMIS,SOUPFM_REC.PAS_GIND),
     *                       PASNOFFRA(SOUPFM_REC.PAS_EMIS,SOUPFM_REC.PAS_GIND),
     *                       CPASTPFFIL(SOUPFM_REC.PAS_EMIS,SOUPFM_REC.PAS_GIND))
                   CALL PASIO_OPENRO(PASFDB)
                   IF(PASFDB.ERR .NE. IOE_NOERR) THEN
                       TYPE*, IAM(), '       '
                       TYPE*, IAM(), 'SOUPFM - Erro: ', PASFDB.ERR, ' a abrir o ficheiro: ', PASFDB.FILNAM
                       TYPE*, IAM(), '       '
                       TYPE*, IAM(), 'DUMP:'
                       CALL PASIO_DUMP(PASFDB)
                       CALL GSTOP(GEXIT_FATAL)
                   ENDIF
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------

C----+------------------------------------------------------------------
C V01| Cycling through PRG Passive file
C----+------------------------------------------------------------------
                   EXIT_CONDITION = .FALSE.
                   DO WHILE (EXIT_CONDITION .EQ. .FALSE.)
                       ST = 0
                       CALL READ_PRGREC_PAS(SOUPFM_REC, VALREC, ST)
                       IF(ST .EQ. ERREND .OR. ST .NE. 0) THEN
                           EXIT_CONDITION = .TRUE.
                       ELSE
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
                           SOUPFM_REC.GAME_DATA_REC.OUT_DRAW_DATE_CDC = PASESD(
     *                           SOUPFM_REC.PAS_EMIS
     *                         , SOUPFM_REC.PAS_GIND)
                           SOUPFM_REC.GAME_DATA_REC.OUT_PURGE_DATE_CDC = PASPRGCDC(
     *                           SOUPFM_REC.PAS_EMIS
     *                         , SOUPFM_REC.PAS_GIND)
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
                           ST = 0
                           CALL HANDLE_VALIDATION_PAS(VALREC, SOUPFM_REC, PASFDB, ST)
                           IF(ST .NE. 0) THEN
                               CALL GSTOP(GEXIT_FATAL)
                           ENDIF
                       ENDIF
                   ENDDO
C----+------------------------------------------------------------------
C V01| Closing PRG Passive file
C----+------------------------------------------------------------------
                   ST = 0
                   CALL CLOSE_FILE_SOUPFM_PRG_PAS(SOUPFM_REC, ST)
                   IF(ST .NE. 0) THEN
                      CALL GSTOP(GEXIT_FATAL)
                   ENDIF
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
                   CALL PASIO_CLOSE(PASFDB)
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
                 ENDIF
               ENDDO
             ENDIF  
           ENDDO
           
C----+------------------------------------------------------------------
C V01| Stop TMF File Read cycle
C----+------------------------------------------------------------------
           CALL PRINT_REPORT_HEADER(SOUPFM_REC)

C----+------------------------------------------------------------------
C V01| Closing TMF File
C----+------------------------------------------------------------------
           ST = 0
           CALL CLOSE_FILE_SOUPFM_TMF(SOUPFM_REC, ST)
           IF(ST .NE. 0) THEN
              CALL GSTOP(GEXIT_FATAL)
           ENDIF

C----+------------------------------------------------------------------
C V01| Closing PRG File
C----+------------------------------------------------------------------
C           ST = 0
C           CALL CLOSE_FILE_SOUPFM_PRG(SOUPFM_REC, ST)
C           IF(ST .NE. 0) THEN
C              CALL GSTOP(GEXIT_FATAL)
C           ENDIF
           ST = 0
           CALL CLOSE_FILE_SOUPFM_PRG_BLK(SOUPFM_REC, ST)
           IF(ST .NE. 0) THEN
              CALL GSTOP(GEXIT_FATAL)
           ENDIF

C----+------------------------------------------------------------------
C V01| Closing output file
C----+------------------------------------------------------------------
           CALL CLOSE_SOUPFM_FILES(SOUPFM_REC, ST)
           IF(ST .NE. 0) THEN
              CALL GSTOP(GEXIT_FATAL)
           ENDIF
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
           CALL CLOSE_GAME_DATA_FILES(SOUPFM_REC, ST)
           IF(ST .NE. 0) THEN
              CALL GSTOP(GEXIT_FATAL)
           ENDIF
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
C----+------------------------------------------------------------------
C V01| End Main loop
C----+------------------------------------------------------------------
        ENDDO
        
C----+------------------------------------------------------------------
C V01| Exit
C----+------------------------------------------------------------------
        IF( .TRUE. ) THEN
            CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
	END





        SUBROUTINE OPEN_SOUPFM_FILES(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4 ST
        ST = 0
        CALL OPEN_FILE_SOUPFM_LN_PAY(SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
        ST = 0
        CALL OPEN_FILE_SOUPFM_LN_EXPIRED(SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
        ST = 0
        CALL OPEN_FILE_SOUPFM_LN_RETURNED(SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
        ST = 0
        CALL OPEN_FILE_SOUPFM_AM_PAY(SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
        ST = 0
        CALL OPEN_FILE_SOUPFM_AM_EXPIRED(SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        ST = 0
        CALL OPEN_FILE_SOUPFM_REL_AM_EXPIRED(SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
        ST = 0
        CALL OPEN_FILE_SOUPFM_REL_LN_EXPIRED(SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        RETURN
        END


        SUBROUTINE HANDLE_TRANSACTION(LOGREC, SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4 LOGREC(LREC*3), ST

        CALL LOGTRA(TRABUF,LOGREC)

        ST = 0
        CALL HANDLE_TRANSACTION_SOUPFM_LN_PAY(TRABUF, SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
        ST = 0
        CALL HANDLE_TRANSACTION_SOUPFM_LN_RETURNED(TRABUF, SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
        ST = 0
        CALL HANDLE_TRANSACTION_SOUPFM_AM_PAY(TRABUF, SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
        RETURN
        END

        SUBROUTINE HANDLE_VALIDATION(VALREC, SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'

        INTEGER*4  ST
        
        ST = 0
        CALL HANDLE_VALIDATION_SOUPFM_AM_EXPIRED(VALREC, SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        ST = 0
        CALL HANDLE_VALIDATION_SOUPFM_REL_AM_EXPIRED(VALREC, SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------

        RETURN
        END

        SUBROUTINE HANDLE_VALIDATION_PAS(VALREC, SOUPFM_REC, PASFDB, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:PASIOSUBS.DEF'

        RECORD /STPASFDB/  PASFDB
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        INTEGER*4  ST
        
        ST = 0
        CALL HANDLE_VALIDATION_SOUPFM_LN_EXPIRED(VALREC, SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        ST = 0
        CALL HANDLE_VALIDATION_SOUPFM_REL_LN_EXPIRED(VALREC, SOUPFM_REC, PASFDB, ST)
        IF(ST .NE. 0) RETURN
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------

        RETURN
        END

        SUBROUTINE CLOSE_SOUPFM_FILES(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        
        INTEGER*4 ST
        
        ST = 0
        CALL CLOSE_FILE_SOUPFM_LN_PAY(SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
        ST = 0
        CALL CLOSE_FILE_SOUPFM_LN_EXPIRED(SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
        ST = 0
        CALL CLOSE_FILE_SOUPFM_LN_RETURNED(SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
        ST = 0
        CALL CLOSE_FILE_SOUPFM_AM_PAY(SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
        ST = 0
        CALL CLOSE_FILE_SOUPFM_AM_EXPIRED(SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        ST = 0
        CALL CLOSE_FILE_SOUPFM_REL_AM_EXPIRED(SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
        ST = 0
        CALL CLOSE_FILE_SOUPFM_REL_LN_EXPIRED(SOUPFM_REC, ST)
        IF(ST .NE. 0) RETURN
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
        RETURN
        END
        
        
        
        
        
        SUBROUTINE PICK_FILES_TO_GENERATE(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4 ST, I, J, K, INPUTLEN
        INTEGER*4 YNFLG                   !Yes No Flag.
        CHARACTER*2 OPTION
        LOGICAL EXIT_CONDITION
        LOGICAL INVALID_OPTION
        
        EXIT_CONDITION = .FALSE.
        
        ST = 0
        
        DO WHILE(EXIT_CONDITION .EQ. .FALSE.)
            INVALID_OPTION = .TRUE.
            TYPE *
            TYPE *, '           Soup FM - Menu de opcoes de geracao de ficheiros'
            TYPE *
            DO I = 1, MAX_NR_FILES
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
                IF(GEN_FILES_MAP(1,I) .NE. 0) THEN
                    TYPE *, '      ', I, '- ', GENERATED_FILES_LIST(GEN_FILES_MAP(1,I))
                ENDIF 
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
            ENDDO
            TYPE *, '                 A - Todos os ficheiros para AM'
            TYPE *, '                 L - Todos os ficheiros para LN'
            TYPE *, '                 T - Todos os ficheiros'
C           TYPE *, '                 C - Nova Data CDC'
C           TYPE *, '                 F - Novo ficheiro TMF'
C           TYPE *, '                 P - Novo ficheiro PRG'
            TYPE *, '                 E - Sair'
            TYPE *
            WRITE(6, 1001) IAM(),'Indique qual a opcao escolhida                '
1001        FORMAT(' ',A,A,' >',$)
            READ (5, 1002) INPUTLEN, OPTION
1002        FORMAT(Q,A)
            TYPE *
            !-----------------------------------------------------------
            ! Handling exit option:
            IF(OPTION .EQ. 'E ' .OR. OPTION .EQ. 'e ') THEN
                INVALID_OPTION = .FALSE.
                CALL GSTOP(GEXIT_OPABORT)
            !-----------------------------------------------------------
            ! Handling all option:
            ELSE IF(OPTION .EQ. 'T ' .OR. OPTION .EQ. 't ') THEN
                INVALID_OPTION = .FALSE.
                DO I = 1, MAX_NR_FILES
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
                    DO J = 1, MAX_MAP_FILES
                        IF(GEN_FILES_MAP(J,I) .NE. 0) THEN
                            TYPE *,IAM(), 'Ira ser gerado o ficheiro ',GENERATED_FILES_LIST(GEN_FILES_MAP(J,I))
                            SOUPFM_REC.HANDLE_FILE(GEN_FILES_MAP(J,I)) = .TRUE.
                            IF(EXIT_CONDITION .EQ. .FALSE.) THEN
                                EXIT_CONDITION = .TRUE.
                            ENDIF
                        ENDIF
                    ENDDO 
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
                ENDDO
            !-----------------------------------------------------------
            ! Handling all option LN:
            ELSE IF(OPTION .EQ. 'L ' .OR. OPTION .EQ. 'l ') THEN
                INVALID_OPTION = .FALSE.
                DO I = 1, MAX_NR_FILES
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
                    DO J = 1, MAX_MAP_FILES
                        IF(GEN_FILES_MAP(J,I) .NE. 0) THEN
                            IF(  I .EQ. IDX_LN_PAY_FILE
     *                      .OR. I .EQ. IDX_LN_EXPIRED_FILE
     *                      .OR. I .EQ. IDX_LN_RETURNED_FILE
     *                      ) THEN
                                TYPE *,IAM(), 'Ira ser gerado o ficheiro ',GENERATED_FILES_LIST(GEN_FILES_MAP(J,I))
                                SOUPFM_REC.HANDLE_FILE(GEN_FILES_MAP(J,I)) = .TRUE.
                                IF(EXIT_CONDITION .EQ. .FALSE.) THEN
                                    EXIT_CONDITION = .TRUE.
                                ENDIF
                            ENDIF
                        ENDIF 
                    ENDDO
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
                ENDDO
            !-----------------------------------------------------------
            ! Handling all option AM:
            ELSE IF(OPTION .EQ. 'A ' .OR. OPTION .EQ. 'a ') THEN
                INVALID_OPTION = .FALSE.
                DO I = 1, MAX_NR_FILES
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
                    DO J = 1, MAX_MAP_FILES
                        IF(GEN_FILES_MAP(J,I) .NE. 0) THEN
                            IF(  I .EQ. IDX_AM_PAY_FILE
     *                      .OR. I .EQ. IDX_AM_EXPIRED_FILE
     *                      ) THEN
                                TYPE *,IAM(), 'Ira ser gerado o ficheiro ',GENERATED_FILES_LIST(GEN_FILES_MAP(J,I))
                                SOUPFM_REC.HANDLE_FILE(GEN_FILES_MAP(J,I)) = .TRUE.
                                IF(EXIT_CONDITION .EQ. .FALSE.) THEN
                                    EXIT_CONDITION = .TRUE.
                                ENDIF
                            ENDIF
                        ENDIF
                    ENDDO 
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
                ENDDO
C            !-----------------------------------------------------------
C            ! Handling new CDC date:
C            ELSE IF(OPTION .EQ. 'C ' .OR. OPTION .EQ. 'c ') THEN
C                CALL GET_CDC_DATE(SOUPFM_REC, ST)
C            !-----------------------------------------------------------
C            ! Handling new TMF file:
C            ELSE IF(OPTION .EQ. 'F ' .OR. OPTION .EQ. 'f ') THEN
C                CALL GET_TMF_FILENAME(SOUPFM_REC, ST)
C            !-----------------------------------------------------------
C            ! Handling new PRG file:
C            ELSE IF(OPTION .EQ. 'P ' .OR. OPTION .EQ. 'p ') THEN
C                CALL GET_PRG_FILENAME(SOUPFM_REC, ST)
            !-----------------------------------------------------------
            ! Handling single file option:
            ELSE
                K = CTOI(OPTION, INPUTLEN)
                DO I = 1, MAX_NR_FILES
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
                    DO J = 1, MAX_MAP_FILES
                        IF(GEN_FILES_MAP(J,I) .NE. 0) THEN
                            IF(K .EQ. I) THEN 
                                INVALID_OPTION = .FALSE.
                                SOUPFM_REC.HANDLE_FILE(GEN_FILES_MAP(J,I)) = .TRUE.
                                TYPE *,IAM(), 'Ira ser gerado o ficheiro ',GENERATED_FILES_LIST(GEN_FILES_MAP(J,I))
                                IF(EXIT_CONDITION .EQ. .FALSE.) THEN
                                    EXIT_CONDITION = .TRUE.
                                ENDIF
                            ENDIF
                        ENDIF
                    ENDDO 
C----+------------------------------------------------------------------
C V02| Adding support for new expired prizes reports
C----+------------------------------------------------------------------
                ENDDO
            ENDIF
            IF(INVALID_OPTION .EQ. .TRUE.) THEN
                TYPE *,IAM(), 'Opcao invalida :',OPTION
            ENDIF
            
        ENDDO

        RETURN
900     FORMAT(A2)
        END
        
        
        
        
        SUBROUTINE GET_TMF_FILENAME(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4 ST

        ST = 0
        CALL COPY_TO_FNAME(SOUPFM_REC
     *          , IDX_TMF_FILE, INT_SRC_FILE_NAME)

        TYPE *, IAM(), 'Ficheiro TMF por omissao : '
     *          , DEFAULT_FILES_NAME(IDX_TMF_FILE)
        IF(INT_SRC_FILE_NAME(1) .NE. 0 .AND. CHR_SRC_FILE_NAME(1:1) .NE. ' ') THEN
           TYPE *, IAM(), ' Ficheiro TMF escolhido : ', CHR_SRC_FILE_NAME
        ENDIF
        CALL FASTSET(0, INT_SRC_FILE_NAME, 7)
        CALL WIMG(5,'-Insira o nome do ficheiro TMF (VOLN:FILNAME)  ')
        READ(5,901) INT_SRC_FILE_NAME

        IF(INT_SRC_FILE_NAME(1) .EQ. 0 .OR. CHR_SRC_FILE_NAME(1:1) .EQ. ' ') THEN
            CALL STRING_COPY(DEFAULT_FILES_NAME(IDX_TMF_FILE)
     *                  , CHR_SRC_FILE_NAME, 28)
        ENDIF
        CALL COPY_FROM_FNAME(SOUPFM_REC
     *       , IDX_TMF_FILE, INT_SRC_FILE_NAME)
        
        RETURN
901     FORMAT(7A4)
        END
        
        
        SUBROUTINE GET_PRG_FILENAME(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'

        INTEGER*4 ST
        
        ST = 0
        CALL COPY_TO_FNAME(SOUPFM_REC
     *          , IDX_PRG_FILE, INT_SRC_FILE_NAME)

        TYPE *, IAM(), 'Ficheiro PRG por omissao : '
     *          , DEFAULT_FILES_NAME(IDX_PRG_FILE)
        IF(INT_SRC_FILE_NAME(1) .NE. 0 .AND. CHR_SRC_FILE_NAME(1:1) .NE. ' ') THEN
           TYPE *, IAM(), ' Ficheiro PRG escolhido : ', CHR_SRC_FILE_NAME
        ENDIF
        CALL FASTSET(0, INT_SRC_FILE_NAME, 7)
        CALL WIMG(5,'-Insira o nome do ficheiro PRG (VOLN:FILNAME)  ')
        READ(5,901) INT_SRC_FILE_NAME

        IF(INT_SRC_FILE_NAME(1) .EQ. 0 .OR. CHR_SRC_FILE_NAME(1:1) .EQ. ' ') THEN
            CALL STRING_COPY(DEFAULT_FILES_NAME(IDX_PRG_FILE)
     *                  , CHR_SRC_FILE_NAME, 28)
        ENDIF
        CALL COPY_FROM_FNAME(SOUPFM_REC
     *       , IDX_PRG_FILE, INT_SRC_FILE_NAME)
        
        RETURN
901     FORMAT(7A4)
        END
        
        
        
        SUBROUTINE GET_CDC_DATE(SOUPFM_REC, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SOUPFM.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        
        INTEGER*4 ST, MAXCDC, EXT
        INTEGER*4 DATE_ARR(3)
        INTEGER*4 YEAR,MONTH,DAY
        EQUIVALENCE(DATE_ARR(DATE_ARR_YEAR),YEAR)
        EQUIVALENCE(DATE_ARR(DATE_ARR_MONTH),MONTH)
        EQUIVALENCE(DATE_ARR(DATE_ARR_DAY),DAY)
        
        CHARACTER*10 STR_DATE

        ST = 0
        EXT = 0
        
                
        CALL CDC_TO_DATE_ARR(DAYCDC
     *            , DATE_ARR)
            WRITE(STR_DATE, 901)
     *          , YEAR
     *          , MONTH
     *          , DAY
            TYPE *
     *          , IAM()
     *          , 'Data CDC presente      : '
     *          , DAYCDC
     *          , ' ('
     *          , STR_DATE
     *          , ')'

        IF(SOUPFM_REC.SRC_CDC_DATE .NE. 0) THEN
            CALL CDC_TO_DATE_ARR(SOUPFM_REC.SRC_CDC_DATE
     *            , DATE_ARR)
            WRITE(STR_DATE, 901)
     *          , YEAR
     *          , MONTH
     *          , DAY
            TYPE *
     *          , IAM()
     *          , ' Data CDC escolhida     : '
     *          , SOUPFM_REC.SRC_CDC_DATE
     *          , ' ('
     *          , STR_DATE
     *          , ')'
        ENDIF
        
        MAXCDC = DAYCDC
        IF(MAXCDC .EQ. 0) THEN
            MAXCDC = 9999 
        ENDIF
        CALL INPNUM('-Insira a data CDC para geracao dos ficheiros  ',
     *              SOUPFM_REC.SRC_CDC_DATE,1,MAXCDC,EXT)
        IF(EXT .LT. 0) CALL GSTOP(GEXIT_OPABORT)

        CALL CDC_TO_DATE_ARR(SOUPFM_REC.SRC_CDC_DATE
     *            , DATE_ARR)
            WRITE(STR_DATE, 901)
     *          , YEAR
     *          , MONTH
     *          , DAY
            TYPE *
     *          , IAM()
     *          , ' Nova data CDC escolhida: '
     *          , SOUPFM_REC.SRC_CDC_DATE
     *          , ' ('
     *          , STR_DATE
     *          , ')'
        
        IF(SOUPFM_REC.SRC_CDC_DATE .LT. DAYCDC) THEN
            TYPE *
     *         , IAM()
     *         , 'ATENCAO! A data CDC escolhida e inferior a data CDC '
     *         , ' do sistema. O programa sera executado em modo de CONTINGENCIA!'
        ENDIF
        
        
        RETURN
901     FORMAT(I4.4,'-',I2.2,'-',I2.2)
        END
