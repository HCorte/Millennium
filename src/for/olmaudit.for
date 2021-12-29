      PROGRAM OLMAUDIT
      IMPLICIT NONE
C      INCLUDE 'INCLIB:PRMLOG.DEF'
      INCLUDE 'INCLIB:OLMAUDIT.DEF'

         RECORD /OLM_AUDIT_CONF/ CONF
           
         CALL SYSTEM_STATUS()
         CALL PROCESS_TMF(CONF)

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

      SUBROUTINE PROCESS_TMF(CONF)
      IMPLICIT NONE

      INCLUDE 'INCLIB:OLMAUDIT.DEF'

      INTEGER*4 I,CNT
      RECORD /OLM_AUDIT_CONF/ CONF
        

CC         CONF.TRX_SER = CONF.P_LAST_TRX_NR

         CALL OPEN_TMF(CONF)   
         CNT = 0
         PRINT *,'[OLMAUDIT]:[CTG]: Begin TMF read cycle'          

         DO WHILE(.NOT. CONF.EOT)
             CALL READ_TMF_TRX(CONF)
             CNT = CNT + 1
             IF(.NOT. CONF.EOT) THEN
                PRINT *,'TRANSACTION SERIAL: ',CONF.TRX_SER
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

      SUBROUTINE READ_TMF_TRX(CONF)
      IMPLICIT NONE
         
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:OLMAUDIT.DEF'
 
         RECORD /OLM_AUDIT_CONF/ CONF
         INTEGER*4 I

         INTEGER*8  I8TMP
         INTEGER*4  I4TMP(2)
         EQUIVALENCE (I8TMP,I4TMP)

         REAL*16     SERIALNUM_OLM
         CHARACTER*24  SERIALNUM_OLMSTR,  SERIAL_AUX

         INTEGER*8  MESSID
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
 
         CALL FASTSET(0,CONF.TRABUF,TRALEN)
         CALL FASTSET(0,CONF.LOGREC,LREC*3)
 
C         CALL LOG_I4(CONF,AA_LOG_LEVEL_TRACE,'ReadTmfTrx:CONF.TRX_SER',CONF.TRX_SER)
         PRINT *,'Transaction Serial: ',CONF.TRX_SER
         CALL READTMF(CONF.LOGREC,CONF.TRX_SER,CONF.EOT)
         CONF.ST = 0
         PRINT *,'IS END OF FILE EOF?: ',CONF.EOT
C         CALL LOG_L(CONF,AA_LOG_LEVEL_TRACE,'ReadTmfTrx:CONF.EOT',CONF.EOT)
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
         
         I4TMP(1) = ZEXT(CONF.TRABUF(TWCOLMSERL_TLTO)) 
         I4TMP(2) = ZEXT(CONF.TRABUF(TWCOLMSERM_TLTO))
         
         SERIALNUM_OLM = DFLOAT(ZEXT(CONF.TRABUF(TWCOLMSERH_TLTO)))*OVER8BYTES+DFLOAT(I8TMP)
         WRITE(SERIAL_AUX,990) SERIALNUM_OLM          
         SERIALNUM_OLMSTR = SERIAL_AUX(1:6)//'-'//SERIAL_AUX(7:8)//'-'//SERIAL_AUX(9:18)//'-'//SERIAL_AUX(19:21) 

         IF ( CONF.TRABUF(TGOLMCOMF_IL) .EQ. 1 
     *   .OR. CONF.TRABUF(TVOLMCOMF_IL) .EQ. 1 
     *   .OR. CONF.TRABUF(TWCOLMCOMF_TLTO) .EQ. 1 
     *   .OR. CONF.TRABUF(TCOLMCOMF_TLTO) .EQ. 1 
     *   .OR. CONF.TRABUF(TVOLMCOMF_TLTO)) CHANNEL = 1 

         PRINT *,'Transaction Internal Serial readed: ',CONF.TRABUF(TSER)
         IF(CHANNEL .EQ. 1) THEN
           PRINT *,'Transaction Olimpo Serial   readed: ',SERIALNUM_OLMSTR
           PRINT *,'Transaction Message Id      readed: ',MESSID
         ENDIF
         CALL CHANNEL_STR(CHANNEL,MSG)
         PRINT *,"Transaction Channel Flag(''CHANNEL') readed: ",MSG
C         PRINT *,'Transaction Serial          readed: ',CONF.TRABUF(TSER)
C         PRINT *,'Transaction Serial          readed: ',CONF.TRABUF(TSER)


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

         PRINT *,'File Type: ',AA_CONF_NAME(INDEX)
         PRINT *,'File Name: ',AA_CONF_DEF_VALUE(INDEX)
 

         PRINT *,'CONF_PARAMS: ',CONF.CONF_PARAMS(AA_I_VALUE, INDEX)
         LINE = TRIM(AA_CONF_DEF_VALUE(INDEX))  !TRIM(ADJUSTL(CONF.CONF_PARAMS(AA_I_VALUE, INDEX)))

         PRINT *,'AA_I_VALUE: ',AA_I_VALUE !2
         PRINT *,'INDEX: ',INDEX !1
         PRINT *,'LINE: ',LINE
 
         DO I = 1, 32

            CALL SHOW_CONVERSION(ILINE(I))

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

   