      PROGRAM OLMAUDIT
      IMPLICIT NONE
C      INCLUDE 'INCLIB:PRMLOG.DEF'
      INCLUDE 'INCLIB:OLMAUDIT.DEF'

         RECORD /OLM_AUDIT_CONF/ CONF
         !211229-06-3139466976-822
         CHARACTER*24 SERIAL_NUMBER /''/
         CHARACTER*20 MESSID_NUMBER
         CHARACTER*1  OPTION
         INTEGER*1    PARAM_ID /0/

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         
C        Initializing structures especialy the field CONF_PARAMS          C
C        logic from abaudit.for see if its better to have in a logical    C
C        table and insert the key/value pair as its being done in job     C
C        table to get info from messageq Millennium attachs (comolm)      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         CALL INIT_CONF (CONF)
C        Loading data from configuration file
         CALL LOAD_CONFIG_FILE(CONF)      
         
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C        TMF FILE HAVES THE INTERNAL SERIAL THAT REPRESENTS THE RECORDS    C
C        POSITION THAT'S ITS PK OF SORTS SEE RLOG.FOR (OFFSET AND INDEX OF C
C        THE BLOCK) SO SHOULD BE BETTER TO USE ISN (INTERNAL SERIAL NUMBER)C 
C        FETCH DIRECTLY A RECORD INSTEAD OF OBTAINING ALL RECORDS UNTIL IT C
C        FINDS THE RIGHT ONE.                                              C   
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         
         DO WHILE(.TRUE.)
            PRINT '(A)'
            PRINT '(A)'
            PRINT *,'SEARCH FOR SERIAL NUMBER (EXTERNAL)? [Y/N] :'
            READ(5,100) OPTION
            IF(OPTION .EQ. 'E' .OR. OPTION .EQ. 'e') EXIT
            IF(OPTION .EQ. 'y' .OR. OPTION .EQ. 'Y' ) THEN 
               PRINT *,'WHAT IS THE SERIAL NUMBER: '
               READ(5,100) SERIAL_NUMBER
c               PRINT *,'SERIAL NUMBER SELECTED: ',SERIAL_NUMBER

C              CALL SYSTEM_STATUS()
               CALL PROCESS_TMF(CONF,SERIAL_NUMBER,PARAM_ID)
            
            ELSE
               PRINT *,'SEARCH FOR MESSAGE ID? [Y/N] :'
               READ(5,100) OPTION
               IF(OPTION .EQ. 'E' .OR. OPTION .EQ. 'e') EXIT
               IF(OPTION .EQ. 'y' .OR. OPTION .EQ. 'Y' ) THEN
                  PRINT *,'WHAT IS THE MESSAGE ID: '
                  READ(5,100) MESSID_NUMBER
                  PARAM_ID = 1
                  CALL PROCESS_TMF(CONF,MESSID_NUMBER,PARAM_ID)
               ELSE   
                  CALL PROCESS_TMF(CONF,SERIAL_NUMBER,PARAM_ID)
               ENDIF
            ENDIF
            PARAM_ID = 0
         ENDDO   

100      FORMAT(A)

      END

      SUBROUTINE SYSTEM_STATUS()
      IMPLICIT NONE   
      
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
   
C           PRINT *,'STOPSYS: ',STOPSYS
           PRINT *,'DSOPEN: ',DSOPEN
           PRINT *,'DSSUSP: ',DSSUSP
           PRINT *,'DSCLOS: ',DSCLOS
           PRINT *,'DAYCDC: ',DAYCDC
           PRINT *,'DSKILL: ',DSKILL

      RETURN
      END

      SUBROUTINE PROCESS_TMF(CONF,SERIAL_NUMBER,PARAM_ID)
      IMPLICIT NONE

      INCLUDE 'INCLIB:OLMAUDIT.DEF'

      INTEGER*4 I,CNT
      INTEGER*1 PARAM_ID
      RECORD /OLM_AUDIT_CONF/ CONF
      CHARACTER*(*) SERIAL_NUMBER  

CC         CONF.TRX_SER = CONF.P_LAST_TRX_NR

         CALL OPEN_TMF(CONF)   
         CNT = 0
C         PRINT *,'SERIAL_NUMBER:',SERIAL_NUMBER,'  PARAM_ID:',PARAM_ID
         PRINT *,'[OLMAUDIT]:[CTG]: Begin TMF read cycle'          

         DO WHILE(.NOT. CONF.EOT)
             CALL READ_TMF_TRX(CONF,SERIAL_NUMBER,PARAM_ID)
             CNT = CNT + 1
             IF(.NOT. CONF.EOT) THEN
C                PRINT *,'TRANSACTION SERIAL: ',CONF.TRX_SER
CCCCCCCCCCCC                
C                  CALL WRITE_REPORT_TRX(CONF)
C                 for audit recover the savepoint other wise ignore it
C                  CONF.P_LAST_TRX_NR = CONF.TRX_SER
CCCCCCCCCCCC
             ELSE 
                PRINT *,'REACHED THE EOF - END OF FILE'
             ENDIF
         ENDDO
         PRINT *,'[OLMAUDIT]:[CTG]: End TMF read cycle' 
         CALL CLOSE_TMF(CONF)     

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C               reset for the next iteration                             C                
                CONF.EOT = .FALSE.
                CONF.TRX_SER = 0

      RETURN
      END

      SUBROUTINE OPEN_TMF(CONF)
      IMPLICIT NONE
      
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:OLMAUDIT.DEF'
           
           RECORD /OLM_AUDIT_CONF/ CONF
           INTEGER*4 I,k
           
           IF(CONF.LUN_TMF .EQ. 0) THEN
                   CALL FIND_AVAILABLE_LUN(CONF.LUN_TMF,CONF.ST)
           ENDIF
C          No LUN available kills the process 
C          (something to improve like await to see if some becames available)
           IF(CONF.ST .NE. 0) CALL GSTOP(GEXIT_FATAL)

           PRINT *,TRIM(AA_CONF_NAME(AA_CI_SRC_TMF_FILE)),' : ',AA_CONF_DEF_VALUE(AA_CI_SRC_TMF_FILE)


           CALL CONVERT_FILENAME(CONF, AA_CI_SRC_TMF_FILE)

C          28*4 its the max number any value higher gives overflow (30,31,32,etc...)
           WRITE(6,3000) (CONF.ICONV(k),k=1,28)


           CALL OPENWY(CONF.LUN_TMF
     *                 ,CONF.ICONV
     *                 ,0,4,0,CONF.ST)
           CALL TOPEN(CONF.LUN_TMF)
   
           IF(CONF.ST .NE. 0) THEN
                   CALL FILERR(CONF.ICONV,1,CONF.ST,0)
                   CALL GSTOP(GEXIT_FATAL)
           ENDIF
           CONF.EOT = .FALSE.

3000    FORMAT(2X,'TMF File Name:',28A4)           
      END

      SUBROUTINE CLOSE_TMF(CONF)
      IMPLICIT NONE
      
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:OLMAUDIT.DEF'

           RECORD /OLM_AUDIT_CONF/ CONF
   
           CLOSE(CONF.LUN_TMF, IOSTAT = CONF.ST)
           IF(CONF.ST .NE. 0) THEN
                PRINT *,'OCURREU ERRO AO TENTAR FECHAR TMIR: ',AA_CI_SRC_TMF_FILE
                PRINT *,'STATUS DO ERRO FOI: ',CONF.ST
C               CALL L_ERROR(CONF,TRIM(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_SRC_TMF_FILE)) //
C     *                      ' close error')
               RETURN
           ENDIF
        
      END

      SUBROUTINE READ_TMF_TRX(CONF,SERIAL_NUMBER,PARAM_ID)
      IMPLICIT NONE
         
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:OLMAUDIT.DEF'
 
         RECORD /OLM_AUDIT_CONF/ CONF
         INTEGER*4 I
         CHARACTER*(*) SERIAL_NUMBER
         INTEGER*1 PARAM_ID

         INTEGER*8  I8TMP
         INTEGER*4  I4TMP(2)
         EQUIVALENCE (I8TMP,I4TMP)

         REAL*16     SERIALNUM_OLM
         CHARACTER*24  SERIALNUM_OLMSTR,  SERIAL_AUX
         CHARACTER*32 MESSID_STR

         INTEGER*8  MESSID,PARAM_INT /0/
         REAL*16     OVER8BYTES
         PARAMETER  (OVER8BYTES = 18446744073709551616.0) 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C        logical channel flag that indicates the origin is from either OLM_COM or
C        from X2X_COM,MXS_COM that are refered the both as old channel where the OLM_COM
C        is now refered as the new channel all defined in procom.def where the value is
C        field in PRCSRC thats refered as COMMUNICATIONS PROCESSOR ID
C        CHANNEL = 1 will be the new channel
C        CHANNEL = 0 will be for old channel's
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         LOGICAL CHANNEL /0/
         CHARACTER*30 MSG 
 
         CONF.TMF_READ_SUCCESS      = .FALSE.
C         PRINT *,'!!!READ_TMF_TRX!!!'
C         PRINT *,'SERIAL_NUMBER:',SERIAL_NUMBER,'  PARAM_ID:',PARAM_ID
 
         CALL FASTSET(0,CONF.TRABUF,TRALEN)
         CALL FASTSET(0,CONF.LOGREC,LREC*3)
 
C         CALL LOG_I4(CONF,AA_LOG_LEVEL_TRACE,'ReadTmfTrx:CONF.TRX_SER',CONF.TRX_SER)
C         PRINT *,'Transaction Serial: ',CONF.TRX_SER
         CALL READTMF(CONF.LOGREC,CONF.TRX_SER,CONF.EOT)
         CONF.ST = 0
C         PRINT *,'IS END OF FILE EOF?: ',CONF.EOT
C         CALL LOG_L(CONF,AA_LOG_LEVEL_TRACE,'ReadTmfTrx:CONF.EOT',CONF.EOT)

CCCCCCCCCCC Its a loop so dont         
         IF( CONF.EOT ) THEN
             CONF.ST = 1 
             RETURN
         ENDIF

         CALL LOGTRA(CONF.TRABUF,CONF.LOGREC)
         CONF.TMF_READ_SUCCESS      = .TRUE.
C         CALL LOG_I4(CONF,AA_LOG_LEVEL_TRACE,'ReadTmfTrx:CONF.TRABUF(TSER)',CONF.TRABUF(TSER))
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C        SHOW NEW FIELDS INFORMATION FROM RECORD READED FROM TMF 
C  
C        OLIMPO SERIAL NUMBER
C        MESSAGE ID OF MESSAGEQ
C        FLAG INDICATING THE CHANNEL FROM WHERE THE TRANSACTION ORIGINATED 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

         I4TMP(1) = ZEXT(CONF.TRABUF(TWCOLMMIDL_TLTO)) 
         I4TMP(2) = ZEXT(CONF.TRABUF(TWCOLMMIDH_TLTO)) 
         MESSID = I8TMP 
C         WRITE(MESSID_STR,980) MESSID

         IF(PARAM_ID .EQ. 1 .OR. PARAM_ID .EQ. '1') THEN
C            WRITE(PARAM_INT,980) SERIAL_NUMBER
            READ(SERIAL_NUMBER,'(I8)') PARAM_INT  
C            PRINT *,'PARAM_INT: ',PARAM_INT
         ENDIF
         


         I4TMP(1) = ZEXT(CONF.TRABUF(TWCOLMSERL_TLTO)) 
         I4TMP(2) = ZEXT(CONF.TRABUF(TWCOLMSERM_TLTO))
         
         SERIALNUM_OLM = DFLOAT(ZEXT(CONF.TRABUF(TWCOLMSERH_TLTO)))*OVER8BYTES+DFLOAT(I8TMP)
         WRITE(SERIAL_AUX,990) SERIALNUM_OLM          
         SERIALNUM_OLMSTR = SERIAL_AUX(1:6)//'-'//SERIAL_AUX(7:8)//'-'//SERIAL_AUX(9:18)//'-'//SERIAL_AUX(19:21) 

         IF ((CONF.TRABUF(TGOLMCOMF_IL) .EQ. 1 
     *   .OR. CONF.TRABUF(TVOLMCOMF_IL) .EQ. 1 
     *   .OR. CONF.TRABUF(TWCOLMCOMF_TLTO) .EQ. 1 
     *   .OR. CONF.TRABUF(TCOLMCOMF_TLTO) .EQ. 1 
     *   .OR. CONF.TRABUF(TVOLMCOMF_TLTO) .EQ. 1)
     *   .AND. MESSID .NE. 0) CHANNEL = 1 
   
         CALL CHANNEL_STR(CHANNEL,MSG)
C         PRINT *,'PARAM_ID:',PARAM_ID

C         IF(MESSID_STR .EQ. SERIAL_NUMBER) PRINT *,'SERIAL_NUMBER:',SERIAL_NUMBER



         IF(CHANNEL) THEN
C            PRINT *,'MESSID: ',MESSID
C            PRINT *,'SERIAL_NUMBER aaaa:',SERIAL_NUMBER
C            PRINT *,'PARAM_INT aaaa:',PARAM_INT
C            IF(MESSID .EQ. PARAM_INT) PRINT *,'SERIAL_NUMBER !!!:',SERIAL_NUMBER
            IF( 
     *         (PARAM_ID .EQ. 1 .AND. MESSID .EQ. PARAM_INT) .OR. 
     *         (PARAM_ID .EQ. 0 .AND. SERIAL_NUMBER .EQ. '') .OR. 
     *         (PARAM_ID .EQ. 0 .AND. SERIAL_NUMBER .NE. '' .AND. SERIAL_NUMBER .EQ. SERIALNUM_OLMSTR) 
     *      ) THEN
               PRINT *,'Transaction Serial: ',CONF.TRX_SER
C               PRINT *,'IS END OF FILE EOF?: ',CONF.EOT

               PRINT *,'Transaction Internal Serial readed: ',CONF.TRABUF(TSER)
               PRINT *,"Transaction Channel Flag(",CHANNEL,") readed: ",MSG

               PRINT *,'Transaction Olimpo Serial   readed: ',SERIALNUM_OLMSTR
               PRINT *,'Transaction Message Id      readed: ',MESSID
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC               
               PRINT *,'MESSID_STR:',MESSID_STR
            ENDIF
         ENDIF

        CHANNEL = 0 

        RETURN

C980     FORMAT(I8)
980     FORMAT(A24)
990     FORMAT(F22.0) 
  

      END

      SUBROUTINE CHANNEL_STR(CHANNEL,MSG)
      IMPLICIT NONE

         LOGICAL CHANNEL
         CHARACTER*(*) MSG 
         
         IF (CHANNEL .EQ. 1) THEN 
            MSG = 'NEW(OLM) CHANNEL' 
         ELSE 
            MSG = 'OLD(MXS,X2X) CHANNEL' 
         ENDIF   

         RETURN
      END  
      
      SUBROUTINE CONVERT_FILENAME(CONF, INDEX)
      IMPLICIT NONE

      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:OLMAUDIT.DEF'
 
         RECORD /OLM_AUDIT_CONF/ CONF
         INTEGER*4 I, INDEX
         
         CHARACTER*128 LINE
         INTEGER*4 ILINE(32)
         EQUIVALENCE(LINE,ILINE)
         BYTE AUX_BYTE

C         PRINT *,'File Type: ',AA_CONF_NAME(INDEX)
C         PRINT *,'File Name: ',AA_CONF_DEF_VALUE(INDEX)
 

C         PRINT *,'CONF_PARAMS: ',CONF.CONF_PARAMS(AA_I_VALUE, INDEX)
         LINE = TRIM(ADJUSTL(CONF.CONF_PARAMS(AA_I_VALUE, INDEX)))
C         LINE = TRIM(AA_CONF_DEF_VALUE(INDEX))  

C         PRINT *,'AA_I_VALUE: ',AA_I_VALUE !2
C         PRINT *,'INDEX: ',INDEX !1
C         PRINT *,'LINE: ',LINE
 
         DO I = 1, 32
D           CALL SHOW_CONVERSION(ILINE(I))
            CONF.ICONV(I) = ILINE(I)
         ENDDO
            
      END

      SUBROUTINE SHOW_CONVERSION(INT_CONV)
      IMPLICIT NONE

      INTEGER*4 INT_CONV

         IF (INT_CONV .EQ. 538976288) GOTO 500 !only spaces no content...
         PRINT *,'ILINE integer value: ',INT_CONV
         WRITE (6, 1000) INT_CONV
         WRITE (6, 1010) IAND(INT_CONV,'FF000000'X)
         WRITE (6, 1020) IAND(INT_CONV,'00FF0000'X)
         WRITE (6, 1030) IAND(INT_CONV,'0000FF00'X)
         WRITE (6, 1040) IAND(INT_CONV,'000000FF'X)

         PRINT *,'Characters: ',
     *   CHAR(IAND(INT_CONV,'000000FF'X)),
     *   CHAR(ISHFT(IAND(INT_CONV,'0000FF00'X),-8)),
     *   CHAR(ISHFT(IAND(INT_CONV,'00FF0000'X),-16)),
     *   CHAR(ISHFT(IAND(INT_CONV,'FF000000'X),-24))

         PRINT '(A)'
         PRINT '(A)'

500      CONTINUE         
         RETURN 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         
C        Z format for Hexa follow by 8 number of digits in this case 4bytes needs 8 digits Hexa        C
C        the .8 forcess to show at least 8 digits that means it keeps left 0 explicitly                C
C        for spaces that is its not filled is 20 --> Space hex code: 20                                C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC             
1000     FORMAT(1X,'Hexa Value:',Z8.8) 
1010     FORMAT(1X,'Hexa Value first  byte:',Z8.8) 
1020     FORMAT(1X,'Hexa Value second byte:',Z8.8) 
1030     FORMAT(1X,'Hexa Value third  byte:',Z8.8) 
1040     FORMAT(1X,'Hexa Value fourth byte:',Z8.8)              
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
         
C         CALL L_DEBUG(CONF,'[ABPAUDIT]:Initializing CONF structure')
         PRINT *,'[OLMAUDIT]:Initializing CONF structure'
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
 
C         CALL UPDATE_CONF(CONF)
C 
C         CONF.REMAINING_CYCLE_TIME  = CONF.P_CYCLE_WAIT_TIME
C         
C         CALL FORMATTED_TO_UNIX(CONF.P_CHECKPOINT_TIME,CONF.CHECKPOINT_TIME_UX)
         RETURN
         
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
                 PRINT *,TRIM(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CONF_FILE)) //
     *                       ' open error'
                 RETURN
             ENDIF
             
C            continue from here the analyse  -------         
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
             PRINT *,'Configuration load failed!'
             PRINT *,'-  File ' // 
     *       TRIM(CONF.CONF_PARAMS(AA_I_VALUE,AA_CI_CONF_FILE)) //
     *       ' does not exist!'
         ENDIF
 
         RETURN
      END
   