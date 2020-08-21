C
C  GXSRC:IGSSIM.FOR
C
C V01 2014-MAR-06 SCML PLACARD PROJECT - IGS - Creation
C
C IGS transaction simulator.
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
        PROGRAM IGSSIM
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:APUCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'

        BYTE OUTBUF(1024)

        INTEGER*4 ST, OPT, BUF,MESLEN
        INTEGER*4 SER/1/,I,IFILENAME(6)
        CHARACTER*24 CFILENAME
        EQUIVALENCE(IFILENAME,CFILENAME)
        LOGICAL   EOF

        INTEGER*4 MIN_OPT, MAX_OPT, LUN, CFG_LUN
        CHARACTER*255 AUX_BUF

        INTEGER*4 MAX_PAR
        PARAMETER(MAX_PAR = 100)
        
        INTEGER*4 PARAMS(MAX_PAR)
        CHARACTER*1024 LINE
        
        LOGICAL END_OF_CYCLE
        LOGICAL IS_COMMENT
        LOGICAL IS_END
        
        INTEGER*4 MAX_REPS
        INTEGER*4 NR_REPS
        BYTE BT
        
        LOGICAL CFG_FILE_EXISTS


        INTEGER*8 I8START_TIME
        INTEGER*4 I4START_TIME(2)
        EQUIVALENCE(I8START_TIME,I4START_TIME)
        
        CALL RESET_DEFAULT_VALUES(PARAMS, CFILENAME)

        CALL LOAD_CONFIG_FILE('IGSSIM.CFG', PARAMS, CFILENAME, ST)
        IF(ST .NE. 0) CALL GSTOP(GEXIT_FATAL)

        CALL SNIF_AND_WRKSET

        P(SIMLAT) = 1

        IS_END = .FALSE.
        
        DO WHILE(IS_END .EQ. .FALSE.)
            ST = 0
            CALL GET_OPTIONS(PARAMS,CFILENAME,ST)
            IF(ST .NE. 0) CALL GSTOP(GEXIT_OPABORT)
            
            IF(PARAMS(1) .EQ. 14) THEN
                IS_END = .TRUE.
            ENDIF
            
            ST = 0
            CALL FIND_AVAILABLE_LUN(LUN,ST)
            IF(ST .NE. 0) CALL GSTOP(GEXIT_FATAL)
            
            END_OF_CYCLE = .FALSE.
            NR_REPS = 0
            MAX_REPS = PARAMS(12)
            PARAMS(10) = 0
            PARAMS(17) = 0
            
            
            ! Setting DESFLG value
            IF(PARAMS(35) .EQ. 1 .AND. PARAMS(37) .EQ. 0 .AND. PARAMS(14) .EQ. 2) THEN
                CALL SET_DESFLG(PARAMS,PARAMS(36))
            ENDIF
            
            
            DO WHILE(END_OF_CYCLE .EQ. .FALSE.)
                NR_REPS = NR_REPS + 1
                PARAMS(13) = 0
                TYPE*,IAM(),'----------------------------------------------'
                TYPE*,IAM(),'            CYCLE #',NR_REPS
                TYPE*,IAM(),'----------------------------------------------'
                OPEN(UNIT = LUN, FILE = CFILENAME, IOSTAT = ST, STATUS = 'OLD')
                IF(ST.NE.0) THEN
                    TYPE*,IAM(),CFILENAME,' open error, status>',ST
                    CALL GSTOP(GEXIT_FATAL)
                ENDIF
                
                EOF = .FALSE.
                DO WHILE(.NOT. EOF)
                    CALL GET_IGS_LINE_FROM_FILE(LUN,LINE,EOF,PARAMS)
                    IF(.NOT. EOF) THEN
                        IS_COMMENT = .FALSE.
                        CALL PARSE_IGS_LINE(PARAMS,LINE,OUTBUF,MESLEN,IS_COMMENT)
                        IF(.NOT. IS_COMMENT) THEN
                            TYPE*,IAM(),'----------------------------------------------'
                            TYPE*,IAM(),'            MSG   #',PARAMS(10)
                            TYPE*,IAM(),'----------------------------------------------'
                            ! Inject control/sequence byte
                            IF(PARAMS(15) .NE. 2) THEN
                                CALL INJECT_CONTROL_SEQUENCE(PARAMS,OUTBUF,MESLEN)
                            ENDIF
                            ! Inject time stamp as message id
                            IF(PARAMS(18) .NE. 2) THEN
                                CALL INJECT_TIME_STAMP_AS_MESSAGE_ID(PARAMS,OUTBUF,MESLEN)
                            ENDIF
                            IF(PARAMS(7) .NE. 2) THEN
                                CALL CALCULATE_MSG_CHECKSUM(PARAMS,OUTBUF,MESLEN)
                            ENDIF
                            
                            CALL DUMP_MESSAGE(PARAMS(10),PARAMS(13), OUTBUF,MESLEN)
                            
                            !Only send messages if 'do not send messages flag' is inactive
                            IF(PARAMS(14) .EQ. 2) THEN
            
                                ! Setting DESFLG only for the n-th message
                                IF(PARAMS(35) .EQ. 1 .AND. PARAMS(37) .EQ. PARAMS(10)) THEN
                                    CALL SET_DESFLG(PARAMS,PARAMS(36))
                                ENDIF
                            
                                BUF = -1
                                DO WHILE(BUF .LE. 0)
                                    ! Queue message to dispat
                                    CALL GETBUF(BUF)
                                    IF(BUF.LE.0) THEN
                                        IF(PARAMS(6) .GE. 2) THEN
                                            TYPE *,IAM(), 'GETBUF: WAITING 1 SEC.'
                                        ENDIF
                                        CALL XWAIT(1,2,ST)
                                    ELSE
                                        TYPE *,IAM(), 'GETBUF: GOT BUFFER : ',BUF
                                    ENDIF
                                ENDDO
                                HPRO(SIMLTR,BUF)=PARAMS(8) ! Term. Nr.
                                HPRO(SIMHTR,BUF)=PARAMS(8) ! Term. Nr.
                                HPRO(SIMMOD,BUF)=-999
                                HPRO(TERNUM,BUF)=PARAMS(8) ! Term. Nr.
                                HPRO(INPLEN,BUF)=MESLEN
                                HPRO(TRCODE,BUF)=TYPREG
                                
                                CALL MOVBYT(OUTBUF,1,BPRO(1,BUF),BINPTAB,MESLEN)
                                
                                ST = -1
                                DO WHILE(ST .NE. 0)
                                    ST = 0
                                    CALL QUEINP(BUF,ST)
                                    IF(ST .NE. 0) THEN
                                        IF(PARAMS(6) .GE. 3) THEN
                                            TYPE *,IAM(), 'QUEINP: WAITING 1 SEC.'
                                        ENDIF
                                        CALL XWAIT(1,2,ST)
                                    ELSE
                                        TYPE *,IAM(), 'GETBUF: MESSAGE QUEUED'
                                    ENDIF
                                ENDDO
            
                                ! Resetting DESFLG only after the n-th message has been sent
                                IF(PARAMS(35) .EQ. 1 .AND. PARAMS(37) .EQ. PARAMS(10)) THEN
                                    CALL SET_DESFLG(PARAMS,PARAMS(38))
                                ENDIF
                            
                            ENDIF
                            
                            IF(PARAMS(3) .EQ. 1) THEN ! write msecs
                                WRITE(AUX_BUF, 901) PARAMS(2), 'msecs' 
                            ELSEIF(PARAMS(3) .EQ. 2) THEN ! write secs
                                WRITE(AUX_BUF, 901) PARAMS(2), 'secs' 
                            ELSEIF(PARAMS(3) .EQ. 3) THEN ! write mins
                                WRITE(AUX_BUF, 901) PARAMS(2), 'mins' 
                            ELSE
                                WRITE(AUX_BUF, 901) PARAMS(2), 'undef' 
                            ENDIF
901                         FORMAT('Waiting ', I4, ' ', A)
                            TYPE *,IAM(), '', TRIM(AUX_BUF)
                            CALL XWAIT(PARAMS(2),PARAMS(3),ST) ! wait for x msecs/secs/mins...
                        ENDIF
                    ENDIF
                ENDDO
                IF(EOF) THEN
                    CLOSE(LUN)
                ENDIF
                IF(PARAMS(11) .EQ. 2) THEN
                    END_OF_CYCLE = .TRUE.
                ELSE
                   IF(NR_REPS .GE. MAX_REPS .AND. MAX_REPS .NE. 0) THEN
                      END_OF_CYCLE = .TRUE.
                   ENDIF 
                ENDIF
            ENDDO
            
            ! resetting DESFLG value
            IF(PARAMS(35) .EQ. 1 .AND. PARAMS(37) .EQ. 0 .AND. PARAMS(14) .EQ. 2) THEN
                CALL SET_DESFLG(PARAMS,PARAMS(38))
            ENDIF
        ENDDO

        CALL GSTOP(GEXIT_SUCCESS)
        END


        
        SUBROUTINE GET_OPTIONS(PARAMS,CFILENAME,ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'

        INTEGER*4 MAX_PAR
        PARAMETER(MAX_PAR = 100)
        
        INTEGER*4 PARAMS(MAX_PAR)
        CHARACTER*24  CFILENAME
        CHARACTER*32  CNUMBER
        CHARACTER*1024 BUF
        
        INTEGER*4 HTOI
        INTEGER*8 HTOI8
        INTEGER*8 CTOI8
        INTEGER*4 I, OPT_MIN, OPT_MAX, ST, J, K, AUX, CFG_LUN
        
        INTEGER*8 I8TEMP
        INTEGER*4 I4TEMP(2)
        INTEGER*1 I1TEMP(8)
        EQUIVALENCE(I8TEMP,I4TEMP)
        EQUIVALENCE(I8TEMP,I1TEMP)
        
        LOGICAL EXIT_CONDITION, SUB_CONDITION, CFG_FILE_EXISTS, EOF
        
        DO I = 1, 1024
            BUF(I:I)  = CHAR(0)
        ENDDO
        
        EXIT_CONDITION = .FALSE.
        
        DO WHILE(EXIT_CONDITION .EQ. .FALSE.)
            WRITE(BUF,901) PARAMS(2), PARAMS(3)
901         FORMAT('(current values: units = ',I4,', unit type = ',I1,' )') 
            TYPE*,IAM(),'*********************************************************'
            TYPE*,IAM(),'*** IGS Message Simulator Menu *** (V03 - 2014.05.29) ***'
            TYPE*,IAM(),'*********************************************************'
            TYPE*,IAM()
            TYPE*,IAM(),'    This program reads messages from a text file and'
            TYPE*,IAM(),'    injects them into Millennium. These messages are'
            TYPE*,IAM(),'    numbers separated by semicoluns(;), and each line'
            TYPE*,IAM(),'    can have up to 1024 characters in length.'
            TYPE*,IAM()
            TYPE*,IAM(),' 1 - Change wait time between messages'
            TYPE*,IAM(),' 2 - Change source file name for IGS input text messages'
            TYPE*,IAM(),' 3 - Change input mode'
            TYPE*,IAM(),' 4 - Change Treat last consecutive separators as null flag'
            TYPE*,IAM(),' 5 - Change debug mode flag'
            TYPE*,IAM(),' 6 - Change calculate message checksum mode'
            TYPE*,IAM(),' 7 - Change message repeat mode'
            TYPE*,IAM(),' 8 - Change Do not send messages flag'
            TYPE*,IAM(),' 9 - Change Inject control/sequence flag'
            TYPE*,IAM(),'10 - Change Inject timestamp as message id flag'
            TYPE*,IAM(),'11 - Change DESFLG flag'
            TYPE*,IAM(),'12 - Change System Parameter P(...)'
            TYPE*,IAM(),'13 - Manage configurations'
            TYPE*,IAM(),'14 - Inject messages from file and exit'
            TYPE*,IAM(),'15 - Inject messages from file'
            TYPE*,IAM(),'16 - Exit'

            OPT_MIN = 1
            OPT_MAX = 16
        
            TYPE*,IAM()
            TYPE*,IAM()
            CALL INPNUM('Enter option',PARAMS(1),OPT_MIN,OPT_MAX,ST)
            TYPE*,IAM()
            
            IF(PARAMS(1) .EQ. 1) THEN
                TYPE*,IAM(),' 1 - Set wait time between messages'
                WRITE(BUF,911) PARAMS(3)
911             FORMAT(' 1.1 - Set wait time unit type (current value : ',I1,')') 
                TYPE*,IAM(),'', TRIM(BUF)
                TYPE*,IAM(),'     (unit type : 1 = msec, 2 = sec, 3 = min)'
                OPT_MIN = 1
                OPT_MAX = 3
                CALL INPNUM('Enter unit type',PARAMS(3),OPT_MIN,OPT_MAX,ST)
                TYPE*,IAM()
                WRITE(BUF,912) PARAMS(2)
912             FORMAT(' 1.2 - Set wait time units (current value : ',I4,')') 
                TYPE*,IAM(),'', TRIM(BUF)
                OPT_MIN = 1
                OPT_MAX = 1000
                CALL INPNUM('Enter units',PARAMS(2),OPT_MIN,OPT_MAX,ST)
                TYPE*,IAM()
            ELSEIF(PARAMS(1) .EQ. 2) THEN
                TYPE*,IAM(),' 2 - Change source file name for IGS input text messages'
                TYPE*,IAM(),'    (current value : ', TRIM(CFILENAME), ')'
                CALL PRMTEXT('Enter input file name ',CFILENAME,I)
                TYPE*,IAM()
            ELSEIF(PARAMS(1) .EQ. 3) THEN
                WRITE(BUF,931) PARAMS(4)
                TYPE*,IAM(),' 3 - Change input mode'
931             FORMAT(' 3.1 - Set input mode (current value : ',I2,')') 
                TYPE*,IAM(),'', TRIM(BUF)
                TYPE*,IAM(),'    (mode : 1 = byte array , 2 = hex byte array)'
                OPT_MIN = 1
                OPT_MAX = 2
                CALL INPNUM('Enter mode',PARAMS(4),OPT_MIN,OPT_MAX,ST)
                TYPE*,IAM()
            ELSEIF(PARAMS(1) .EQ. 4) THEN
                WRITE(BUF,941) PARAMS(5)
                TYPE*,IAM(),' 4 - Change Treat last consecutive separators as null flag'
941             FORMAT(' 4.1 - Set treat mode flag (current value : ',I2,')') 
                TYPE*,IAM(),'', TRIM(BUF)
                TYPE*,IAM(),'    (flag : 1 = yes, 2 = no)'
                OPT_MIN = 1
                OPT_MAX = 2
                CALL INPNUM('Enter treat mode flag',PARAMS(5),OPT_MIN,OPT_MAX,ST)
                TYPE*,IAM()
            ELSEIF(PARAMS(1) .EQ. 5) THEN
                WRITE(BUF,951) PARAMS(6)
                TYPE*,IAM(),' 5 - Change debug mode flag'
951             FORMAT(' 5.1 - Set debug mode flag (current value : ',I2,')') 
                TYPE*,IAM(),'', TRIM(BUF)
                TYPE*,IAM(),'    (flag : 1 = NONE, 2 = INFO, 3 = DEBUG)'
                OPT_MIN = 1
                OPT_MAX = 3
                CALL INPNUM('Enter debug mode flag',PARAMS(6),OPT_MIN,OPT_MAX,ST)
                TYPE*,IAM()
            ELSEIF(PARAMS(1) .EQ. 6) THEN
                WRITE(BUF,961) PARAMS(7)
                TYPE*,IAM(),' 6 - Change calculate message checksum mode'
961             FORMAT(' 6.1 - Calc.msg.checksum flag (current value : ',I2,')') 
                TYPE*,IAM(),'', TRIM(BUF)
                TYPE*,IAM(),'    (flag : 1 = yes, 2 = no, 3 = use seed from msg)'
                OPT_MIN = 1
                OPT_MAX = 3
                CALL INPNUM('Enter calculate message checksum flag',PARAMS(7),OPT_MIN,OPT_MAX,ST)
                TYPE*,IAM()
                IF(PARAMS(7) .EQ. 1) THEN
                   WRITE(BUF,962) PARAMS(8)
962                FORMAT(' 6.2 - Calc.msg.checksum term.nr. (current value : ',I5,')') 
                   TYPE*,IAM(),'', TRIM(BUF)
                   OPT_MIN = 0
                   OPT_MAX = 99999
                   CALL INPNUM('Enter term.nr.',PARAMS(8),OPT_MIN,OPT_MAX,ST)
                   TYPE*,IAM()
                   WRITE(BUF,963) PARAMS(9)
963                FORMAT(' 6.3 - Calc.msg.checksum msg.offset (current value : ',I4,')') 
                   TYPE*,IAM(),'', TRIM(BUF)
                   OPT_MIN = 1
                   OPT_MAX = 9999
                   CALL INPNUM('Enter msg.offset',PARAMS(9),OPT_MIN,OPT_MAX,ST)
                   TYPE*,IAM()
                ENDIF
            ELSEIF(PARAMS(1) .EQ. 7) THEN
                WRITE(BUF,971) PARAMS(11)
                TYPE*,IAM(),' 7 - Change message repeat mode'
971             FORMAT(' 7.1 - Set repeat mode flag (current value : ',I2,')') 
                TYPE*,IAM(),'', TRIM(BUF)
                TYPE*,IAM(),'    (flag : 1 = yes, 2 = no)'
                OPT_MIN = 1
                OPT_MAX = 2
                CALL INPNUM('Enter repeat mode flag',PARAMS(11),OPT_MIN,OPT_MAX,ST)
                TYPE*,IAM()
                IF(PARAMS(11) .EQ. 1) THEN
                   WRITE(BUF,972) PARAMS(12)
972                FORMAT(' 7.2 - Nr. repetitions (current value : ',I8,')') 
                   TYPE*,IAM(),'', TRIM(BUF)
                   OPT_MIN = 0
                   OPT_MAX = 1000000
                   CALL INPNUM('Enter nr.repetitions',PARAMS(12),OPT_MIN,OPT_MAX,ST)
                   TYPE*,IAM()
                ENDIF
            ELSEIF(PARAMS(1) .EQ. 8) THEN
                WRITE(BUF,981) PARAMS(14)
                TYPE*,IAM(),' 8 - Change Do not send messages flag'
981             FORMAT(' 8.1 - Do not send messages flag (current value : ',I2,')') 
                TYPE*,IAM(),'', TRIM(BUF)
                TYPE*,IAM(),'    (flag : 1 = yes, 2 = no)'
                OPT_MIN = 1
                OPT_MAX = 2
                CALL INPNUM('Enter do not send messages flag ',PARAMS(14),OPT_MIN,OPT_MAX,ST)
                TYPE*,IAM()
            ELSEIF(PARAMS(1) .EQ. 9) THEN
                WRITE(BUF,991) PARAMS(15)
                TYPE*,IAM(),' 9 - Change Inject control/sequence flag'
991             FORMAT(' 9.1 - Set Inject control/sequence flag (current value : ',I2,')') 
                TYPE*,IAM(),'', TRIM(BUF)
                TYPE*,IAM(),'    (flag : 1 = yes, 2 = no)'
                OPT_MIN = 1
                OPT_MAX = 2
                CALL INPNUM('Enter Inject control/sequence flag',PARAMS(15),OPT_MIN,OPT_MAX,ST)
                TYPE*,IAM()
                IF(PARAMS(15) .EQ. 1) THEN
                   WRITE(BUF,992) PARAMS(16)
992                FORMAT(' 9.2 - Inject ctrl/seq msg.offset (current value : ',I4,')') 
                   TYPE*,IAM(),'', TRIM(BUF)
                   OPT_MIN = 1
                   OPT_MAX = 1024
                   CALL INPNUM('Enter Inject ctrl/seq msg.offset',PARAMS(16),OPT_MIN,OPT_MAX,ST)
                   TYPE*,IAM()
                ENDIF
            ELSEIF(PARAMS(1) .EQ. 10) THEN
                WRITE(BUF,9101) PARAMS(18)
                TYPE*,IAM(),'10 - Change Inject timestamp as message id flag'
9101            FORMAT('10.1 - Set Inject tstamp as msg id flag (current value : ',I2,')') 
                TYPE*,IAM(),'', TRIM(BUF)
                TYPE*,IAM(),'    (flag : 1 = yes, 2 = no, 3 = from seed, '
                TYPE*,IAM(),'            4 = from seed with time delta increment)'
                OPT_MIN = 1
                OPT_MAX = 4
                CALL INPNUM('Enter Inject tstamp as msg id flag',PARAMS(18),OPT_MIN,OPT_MAX,ST)
                TYPE*,IAM()
                IF(PARAMS(18) .NE. 2) THEN
                   WRITE(BUF,9102) PARAMS(19)
9102               FORMAT('10.2 - Inject timestamp size in bytes (current value : ',I4,')') 
                   TYPE*,IAM(),'', TRIM(BUF)
                   OPT_MIN = 1
                   OPT_MAX = 1024
                   CALL INPNUM('Enter Inject timestamp size in bytes',PARAMS(19),OPT_MIN,OPT_MAX,ST)
                   TYPE*,IAM()
                   WRITE(BUF,9103) PARAMS(20)
9103               FORMAT('10.3 - Selector byte offset (current value : ',I4,')') 
                   TYPE*,IAM(),'', TRIM(BUF)
                   OPT_MIN = 1
                   OPT_MAX = 1024
                   CALL INPNUM('Enter Selector byte offset',PARAMS(20),OPT_MIN,OPT_MAX,ST)
                   TYPE*,IAM()
                   SUB_CONDITION = .FALSE.
                   DO WHILE(SUB_CONDITION .EQ. .FALSE.)
                       TYPE*,IAM(),'10.4 - Markers and offsets for injection:'
                       DO J = 0, 9
                           WRITE(BUF, 9104) J, PARAMS(41 + (J*2)), PARAMS(41 + (J*2)), PARAMS(42 + (J*2))
9104                       FORMAT('    ',I2,' - Change Marker: 0x',Z2.2,' (',I3,'), Offset: ',I4)
                           TYPE*,IAM(),TRIM(BUF)
                       ENDDO
                       WRITE(BUF, 91050) (J)
91050                  FORMAT('    ',I2,' - Exit')
                       TYPE*,IAM(),TRIM(BUF)
                       OPT_MIN = 0
                       OPT_MAX = (J)
                       CALL INPNUM('Enter Marker to change',J,OPT_MIN,OPT_MAX,ST)
                       TYPE*,IAM()
                       IF(J .EQ. OPT_MAX) THEN
                           SUB_CONDITION = .TRUE.
                       ELSE
                           WRITE(BUF, 91041) PARAMS(41 + (J*2)), PARAMS(41+ (J*2))
91041                      FORMAT('10.4.1 - Change Marker: 0x',Z2.2,' (',I3,')')
                           TYPE*,IAM(),TRIM(BUF)
                           OPT_MIN = 0
                           OPT_MAX = 255
                           CALL PRMTEXT('Enter Marker (hexadecimal format)',CNUMBER, K)
                           PARAMS(41 + (J*2)) = MOD(HTOI(CNUMBER,K),256)
C                           CALL INPNUM('Enter Marker (decimal format)',PARAMS(41 + (J*2)),OPT_MIN,OPT_MAX,ST)
                           TYPE*,IAM()
                           WRITE(BUF, 91042) PARAMS(42 + (J*2))
91042                      FORMAT('10.4.2 - Change Injection Offset: ',I4)
                           TYPE*,IAM(),TRIM(BUF)
                           OPT_MIN = 1
                           OPT_MAX = 1024
                           CALL INPNUM('Enter Injection offset',PARAMS(42 + (J*2)),OPT_MIN,OPT_MAX,ST)
                           TYPE*,IAM()
                       ENDIF
                   ENDDO
                   IF(PARAMS(18) .EQ. 3 .OR. PARAMS(18) .EQ. 4) THEN
                       I4TEMP(1) = PARAMS(31)
                       I4TEMP(2) = PARAMS(32)
                       WRITE(BUF,9105) I8TEMP, (I1TEMP(K), K = 8,1,-1)
9105                   FORMAT('    ',I20,' [ ',8(Z2.2,1X),'])')
                       TYPE*,IAM(), '10.5 - Change initial seed (current value :' 
                       TYPE*,IAM(),'', TRIM(BUF)
                       OPT_MIN = 1
                       OPT_MAX = 2
                       CALL INPNUM('Enter Input type (1 = decimal, 2 = hexadecimal)',J,OPT_MIN,OPT_MAX,ST)
                       TYPE*,IAM()
                       IF(J .EQ. 1) THEN
                           CALL PRMTEXT('Enter Marker (decimal format)',CNUMBER, K)
                           I8TEMP = CTOI8(CNUMBER, K)
                       ELSE
                           CALL PRMTEXT('Enter Marker (hexadecimal format)',CNUMBER, K)
                           I8TEMP = HTOI8(CNUMBER, K)
                       ENDIF
                       PARAMS(31) = I4TEMP(1)
                       PARAMS(32) = I4TEMP(2)
                       WRITE(BUF,9105) I8TEMP, (I1TEMP(K), K = 8,1,-1)
                       TYPE*,IAM(), 'Initial seed changed! (current value :' 
                       TYPE*,IAM(),'', TRIM(BUF)
                       TYPE*,IAM()
                    ENDIF
                ENDIF
            ELSEIF(PARAMS(1) .EQ. 11) THEN
                WRITE(BUF,9111) PARAMS(35)
                TYPE*,IAM(),'11 - Change DESFLG flag'
9111             FORMAT('11.1 - Set DESFLG flag (current value : ',I2,')') 
                TYPE*,IAM(),'', TRIM(BUF)
                TYPE*,IAM(),'    (flag : 1 = yes, 2 = no)'
                OPT_MIN = 1
                OPT_MAX = 2
                CALL INPNUM('Enter DESFLG flag',PARAMS(35),OPT_MIN,OPT_MAX,ST)
                TYPE*,IAM()
                IF(PARAMS(35) .EQ. 1) THEN
                   WRITE(BUF,9112) PARAMS(36)
9112               FORMAT('11.2 - DESFLG value (current value : ',I8,')') 
                   TYPE*,IAM(),'', TRIM(BUF)
                   OPT_MIN = 0
                   OPT_MAX = 1
                   CALL INPNUM('Enter DESFLG value',PARAMS(36),OPT_MIN,OPT_MAX,ST)
                   TYPE*,IAM()
                   WRITE(BUF,9113) PARAMS(37)
9113               FORMAT('11.3 - Which msg to set DESFLG value (current value : ',I8,')') 
                   TYPE*,IAM(),'', TRIM(BUF)
                   TYPE*,IAM(),'    (mode : 0 = all, n-th message, max = 1000000)'
                   OPT_MIN = 0
                   OPT_MAX = 1000000
                   CALL INPNUM('Enter Which msg to set DESFLG value',PARAMS(37),OPT_MIN,OPT_MAX,ST)
                   TYPE*,IAM()
                ENDIF
            ELSEIF(PARAMS(1) .EQ. 12) THEN
                TYPE*,IAM(),'12 - Change System Parameter P(...)'
                TYPE*,IAM(),'    (1 = yes, 2 = no)'
                OPT_MIN = 1
                OPT_MAX = 2
                CALL INPNUM('Change System Parameter P(...) ?',AUX,OPT_MIN,OPT_MAX,ST)
                TYPE*,IAM()
                IF(AUX .EQ. 1) THEN
                   SUB_CONDITION = .FALSE.
                   DO WHILE(SUB_CONDITION .EQ. .FALSE.)
                      WRITE(BUF,9120)
9120                  FORMAT('12.1 - Param index to change :') 
                      TYPE*,IAM(),'', TRIM(BUF)
                      WRITE(BUF,9121) 1, NUMPAR
9121                  FORMAT('      (range: ',I0,' - ', I0,', 0 - return, -1 - IGSDEBUGF)') 
                      TYPE*,IAM(),'', TRIM(BUF)
                      OPT_MIN = -1
                      OPT_MAX = NUMPAR
                      CALL INPNUM('Enter param index to change',AUX,OPT_MIN,OPT_MAX,ST)
                      IF(AUX .EQ. 0) THEN
                         SUB_CONDITION = .TRUE.
                      ELSE
                         IF(AUX .NE. -1) THEN
                             WRITE(BUF,9122) AUX, P(AUX), P(AUX)
9122                         FORMAT('12.2 - Current value for P(',I0,') = ',I,' [0x',Z8.8,']') 
                         ELSE
                             AUX = IGSDEBUGF
                             WRITE(BUF,9123) 'IGSDEBUGF', AUX
9123                         FORMAT('12.2 - Setting value for parameter ',A,' [ = ',I0,' ] :') 
                             TYPE*,IAM(),'', TRIM(BUF)
                             WRITE(BUF,9124) AUX, P(AUX), P(AUX)
9124                         FORMAT('       Current value for P(',I0,') = ',I,' [0x',Z8.8,']') 
                         ENDIF
                         TYPE*,IAM(),'', TRIM(BUF)
                         OPT_MIN = 0
                         OPT_MAX = 2
                         CALL INPNUM('Enter Input type (1 = decimal, 2 = hexadecimal, 0 = return)',J,OPT_MIN,OPT_MAX,ST)
                         TYPE*,IAM()
                         IF(J .EQ. 1) THEN
                             CALL PRMTEXT('Enter value (decimal format)',CNUMBER, K)
                             I4TEMP(1) = CTOI(CNUMBER, K)
                         ELSEIF(J .EQ. 2) THEN
                             CALL PRMTEXT('Enter value (hexadecimal format)',CNUMBER, K)
                             I4TEMP(1) = HTOI(CNUMBER, K)
                         ENDIF
                         IF(J .NE. 0) THEN
                             TYPE*,IAM(),'12.3 - Set parameter directly (without QUECOM)'
                             TYPE*,IAM(),'    (1 = yes, 2 = no)'
                             OPT_MIN = 1
                             OPT_MAX = 2
                             CALL INPNUM('Enter option',J,OPT_MIN,OPT_MAX,ST)
                             TYPE*,IAM()
                             IF(J .EQ. 1) THEN
                                 P(AUX) = I4TEMP(1)
                             ELSE
                                 CALL SET_CMD(PARAMS,AUX,I4TEMP(1))
                             ENDIF
                         ENDIF
                      ENDIF
                   ENDDO
                ENDIF
            ELSEIF(PARAMS(1) .EQ. 13) THEN
                TYPE*,IAM(),'13 - Manage configurations'
                TYPE*,IAM(),'    (flag : 1 = load file    , 2 = save file '
                TYPE*,IAM(),'          , 3 = reset values , 0 = return      )'
                OPT_MIN = 0
                OPT_MAX = 3
                CALL INPNUM('Enter option',J,OPT_MIN,OPT_MAX,ST)
                TYPE*,IAM()
                IF(J .EQ. 1) THEN
                    CALL LOAD_CONFIG_FILE('IGSSIM.CFG', PARAMS, CFILENAME, ST)
                    IF(ST .NE. 0) CALL GSTOP(GEXIT_FATAL)
                ELSEIF(J .EQ. 2) THEN
                    CALL SAVE_CONFIG_FILE('IGSSIM.CFG', PARAMS, CFILENAME, ST)
                    IF(ST .NE. 0) CALL GSTOP(GEXIT_FATAL)
                ELSEIF(J .EQ. 3) THEN
                    CALL RESET_DEFAULT_VALUES(PARAMS, CFILENAME)
                ENDIF
            ELSE
                EXIT_CONDITION = .TRUE.
            ENDIF

        ENDDO
        
        IF(PARAMS(1) .EQ. OPT_MAX) THEN
            CALL GSTOP(GEXIT_OPABORT)
        ENDIF
        
        RETURN
        END



        SUBROUTINE PARSE_IGS_LINE(PARAMS,LINE,OUTBUF,MESLEN,IS_COMMENT)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        
        ! Constants
        INTEGER*4 MAX_PAR
        PARAMETER(MAX_PAR = 100)
        
        INTEGER*4 HTOI

        ! Input parameters
        INTEGER*4 PARAMS(MAX_PAR)
        CHARACTER*1024 LINE
        ! Output parameters
        BYTE OUTBUF(*)
        INTEGER*4 MESLEN
        LOGICAL IS_COMMENT

        INTEGER*4 I4BUF(256)
        INTEGER*2 I2BUF(512)
        INTEGER*1 I1BUF(1024)
        BYTE      BBUF(1024)
        EQUIVALENCE(I4BUF,I2BUF,I1BUF,BBUF)
        
        INTEGER*4 I4TMP
        INTEGER*2 I2TMP(2)
        INTEGER*1 I1TMP(4)
        BYTE      BTMP(4)
        EQUIVALENCE(I4TMP,I2TMP,I1TMP,BTMP)
        
        ! Auxiliary variables
        INTEGER*4 I, J, AUX, LINE_LEN, SIZE
        INTEGER*4 FROM,TO, XLEN
        CHARACTER*255 BUF

        DO I = 1,256
            I4BUF(I) = 0
        ENDDO
        I4TMP = 0
        
        IS_COMMENT = .FALSE.
        
        IF(LINE(1:1) .EQ. '!') THEN
            IS_COMMENT = .TRUE.
            IF(PARAMS(6) .GE. 3) THEN
                TYPE*,IAM(),'LINE IS COMMENT!'
            ENDIF
        ENDIF
        
        I = 1
        DO WHILE(I .LE. 1024 
     *     .AND. LINE(I:I) .NE. ' '
     *     .AND. LINE(I:I) .NE. CHAR(0))
            I = I + 1
        ENDDO
        LINE_LEN = I - 1
        
        IF(PARAMS(6) .GE. 3) THEN
            TYPE *,IAM(),'LINE_LEN = ', LINE_LEN
            TYPE *,IAM(),'LINE     = ', TRIM(LINE)
        ENDIF
        
        IF(IS_COMMENT) THEN
            RETURN
        ENDIF
        PARAMS(10) = PARAMS(10) + 1 ! Line count
        !---------------------------------------------------------------
        !
        ! Mode 1: BYTE ARRAY
        !
        !---------------------------------------------------------------
        IF(PARAMS(4) .EQ. 1) THEN
            I = 1
            FROM = 1
            TO = 1
            J = 1
            DO WHILE(I .LE. LINE_LEN)
                IF(  LINE(I:I) .EQ. ';'
     *          .OR. LINE(I:I) .EQ. ':'
     *          .OR. LINE(I:I) .EQ. ' '
     *          .OR. LINE(I:I) .EQ. ',') THEN
                    TO = I - 1
                    IF(FROM .LE. TO) THEN
                        BBUF(J) = INT1(MOD(CTOI(LINE(FROM:TO),XLEN),256))
                    ELSE
                        BBUF(J) = 0
                    ENDIF
                    J = J + 1
                    IF( (I + 1) .LE. LINE_LEN) THEN
                        FROM = I + 1
                    ENDIF
                ELSE
                    TO = I
                ENDIF
                I = I + 1
            ENDDO
            IF(FROM .LE. TO) THEN
                BBUF(J) = INT1(MOD(CTOI(LINE(FROM:TO),XLEN),256))
            ELSE
                BBUF(J) = 0
            ENDIF
            MESLEN = J
            IF(PARAMS(5) .EQ. 1) THEN
                I = MESLEN
                DO WHILE(BBUF(I) .EQ. 0 .AND. I .GT. 0)
                    I = I - 1
                ENDDO
                MESLEN = I
            ENDIF
            ! Copying from buffer to OUTBUF
            IF(PARAMS(6) .GE. 3) THEN
                TYPE *,IAM(),'(Mode:2)MESLEN (bytes) = ', MESLEN
            ENDIF
            DO I = 1, MESLEN
                OUTBUF(I) = BBUF(I)
                IF(PARAMS(6) .GE. 3) THEN
                    TYPE *,IAM(),'(Mode:1)BBUF(',I,') = ',BBUF(I)
                ENDIF
            ENDDO
        !---------------------------------------------------------------
        !
        ! Mode 2: HEX BYTE ARRAY
        !
        !---------------------------------------------------------------
        ELSEIF(PARAMS(4) .EQ. 2) THEN
            I = 1
            FROM = 1
            TO = 1
            J = 1
            DO WHILE(I .LE. LINE_LEN)
                IF(  LINE(I:I) .EQ. ';'
     *          .OR. LINE(I:I) .EQ. ':'
     *          .OR. LINE(I:I) .EQ. ' '
     *          .OR. LINE(I:I) .EQ. ',') THEN
                    TO = I - 1
                    IF(FROM .LE. TO) THEN
                        BBUF(J) = INT1(MOD(HTOI(LINE(FROM:TO),XLEN),256))
                    ELSE
                        BBUF(J) = 0
                    ENDIF
                    J = J + 1
                    IF( (I + 1) .LE. LINE_LEN) THEN
                        FROM = I + 1
                    ENDIF
                ELSE
                    TO = I
                ENDIF
                I = I + 1
            ENDDO
            IF(FROM .LE. TO) THEN
                BBUF(J) = INT1(MOD(HTOI(LINE(FROM:TO),XLEN),256))
            ELSE
                BBUF(J) = 0
            ENDIF
            MESLEN = J
            IF(PARAMS(5) .EQ. 1) THEN
                I = MESLEN
                DO WHILE(BBUF(I) .EQ. 0 .AND. I .GT. 0)
                    I = I - 1
                ENDDO
                MESLEN = I
            ENDIF
            ! Copying from buffer to OUTBUF
            IF(PARAMS(6) .GE. 3) THEN
                TYPE *,IAM(),'(Mode:2)MESLEN (bytes) = ', MESLEN
            ENDIF
            DO I = 1, MESLEN
                OUTBUF(I) = BBUF(I)
                IF(PARAMS(6) .GE. 3) THEN
                    WRITE(BUF,902) I,BBUF(I),ZEXT(BBUF(I))
902                 FORMAT('(Mode:2)BBUF(',I,') = ',Z2.2, '(', I3,')') 
                    TYPE *,IAM(),'',TRIM(BUF)
                ENDIF
            ENDDO
        ENDIF
        
        RETURN
        END
        
        
        SUBROUTINE DUMP_MESSAGE(MESSAGE_ID, LINE_ID, OUTBUF, MESLEN)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        BYTE OUTBUF(*)
        INTEGER*4 MESLEN
        INTEGER*4 MESSAGE_ID, LINE_ID

        CHARACTER*255 BUF
        CHARACTER*3 ARR(16)
        INTEGER*4 I, J, K, DIV, REMAIN, OFFSET
        
        DO I = 1, 255
            BUF(I:I) = CHAR(0)
        ENDDO
        
        DIV = MESLEN / 16
        REMAIN = MOD(MESLEN,16)
        
        WRITE(BUF, 900) MESSAGE_ID, LINE_ID, MESLEN
        TYPE *, IAM(), '', TRIM(BUF)
        
        DO K = 1, DIV
           DO I = 1, 16
               DO J = 1, 2
                   ARR(I)(J:J) = ' '
               ENDDO
               ARR(I)(3:3) = CHAR(0)
           ENDDO
           DO I = 1, 16
               OFFSET = ((K - 1) * 16) + I
               WRITE(ARR(I), 901) OUTBUF(OFFSET)
           ENDDO
           OFFSET = ((K - 1) * 16)
           WRITE(BUF, 902) OFFSET + 1, OFFSET + 16,( ARR(I), I = 1, 16)
           TYPE *, '', TRIM(BUF)
        ENDDO
        IF(REMAIN .NE. 0) THEN
           DO I = 1, 16
               DO J = 1, 2
                   ARR(I)(J:J) = ' '
               ENDDO
               ARR(I)(3:3) = CHAR(0)
           ENDDO
           DO I = 1, REMAIN
               OFFSET = ((K - 1) * 16) + I
               WRITE(ARR(I), 901) OUTBUF(OFFSET)
           ENDDO
           OFFSET = ((K - 1) * 16)
           WRITE(BUF, 902) OFFSET + 1, OFFSET + REMAIN, (ARR(I), I = 1, 16)
           TYPE *, '', TRIM(BUF)
        ENDIF
        TYPE *, ''

900     FORMAT('PARSED MESSAGE #',I8,' (@ LINE #',I8,') : LEN = ', I8)
901     FORMAT(Z2.2)
902     FORMAT('[',I4,':',I4,'] = ',16(A2,1X))

        RETURN
        END




        SUBROUTINE CALCULATE_MSG_CHECKSUM(PARAMS,OUTBUF,OUTLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
        BYTE      OUTBUF(*)
        INTEGER*4 OUTLEN
        
        INTEGER*4 MAX_PAR
        PARAMETER(MAX_PAR = 100)
        INTEGER*4 PARAMS(MAX_PAR)

        INTEGER*4 MYCHKSUM, CHKLEN
        INTEGER*4 TER, MSG_OFFSET
        
        CHARACTER*255 LINE
        
        TER = PARAMS(8)
        MSG_OFFSET = PARAMS(9)

        IF(PARAMS(7) .EQ. 1) THEN
            BASECHKSUM = IAND(DAYCDC,'FFFF'X)
            I4CCITT   = IAND(BASECHKSUM+TER,'FFFF'X)
            OUTBUF(MSG_OFFSET + 0) = I1CCITT(2)
            OUTBUF(MSG_OFFSET + 1) = I1CCITT(1)
        ENDIF
        I4CCITT = 0
        I1CCITT(2) = OUTBUF(MSG_OFFSET + 0)
        I1CCITT(1) = OUTBUF(MSG_OFFSET + 1)
        WRITE(LINE, 901) OUTBUF(MSG_OFFSET + 0), OUTBUF(MSG_OFFSET + 1), I4CCITT
901     FORMAT('BASECHKSUM = [',Z2.2, ' ', Z2.2, '] : ', I8)
        TYPE *,IAM(), '', TRIM(LINE)
        
        CHKLEN=OUTLEN-1
        CALL GETCCITT(OUTBUF,1,CHKLEN,MYCHKSUM)
        I4CCITT = MYCHKSUM
        OUTBUF(MSG_OFFSET + 0) = I1CCITT(2)
        OUTBUF(MSG_OFFSET + 1) = I1CCITT(1)

        I4CCITT = 0
        I1CCITT(2) = OUTBUF(MSG_OFFSET + 0)
        I1CCITT(1) = OUTBUF(MSG_OFFSET + 1)
        WRITE(LINE, 902) OUTBUF(MSG_OFFSET + 0), OUTBUF(MSG_OFFSET + 1), I4CCITT
902     FORMAT('CALCCHKSUM = [',Z2.2, ' ', Z2.2, '] : ', I8)
        TYPE *,IAM(), '', TRIM(LINE)
        
        RETURN
        END



        SUBROUTINE INJECT_CONTROL_SEQUENCE(PARAMS,OUTBUF,OUTLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        BYTE      OUTBUF(*)
        INTEGER*4 OUTLEN
        
        INTEGER*4 MAX_PAR
        PARAMETER(MAX_PAR = 100)
        INTEGER*4 PARAMS(MAX_PAR)
        BYTE BT, OLD

        CHARACTER*255 LINE
        
        OLD = OUTBUF(PARAMS(16))
        IF(PARAMS(17) .EQ. 0) THEN 
            PARAMS(17) = ZEXT(OUTBUF(PARAMS(16)))
            PARAMS(17) = IAND(PARAMS(17),7) ! pick only last 3 bits
        ELSE
            PARAMS(17) = IAND(PARAMS(17) + 1,7) ! pick only last 3 bits
            IF(PARAMS(17) .EQ. 0) THEN
                PARAMS(17) = PARAMS(17) + 1
            ENDIF
            BT = OUTBUF(PARAMS(16))
            BT = IAND(BT,'F8'X)
            BT = IOR(BT,PARAMS(17)) 
            OUTBUF(PARAMS(16)) = BT
        ENDIF
        
        WRITE(LINE, 902) PARAMS(16), OLD, OUTBUF(PARAMS(16))
902     FORMAT('INJECTING CTRL SEQ FOR BYTE #',I4,' (OLD = 0x',Z2.2,',NEW = 0x', Z2.2,')')
        IF(PARAMS(6) .GE. 2) THEN
            TYPE *,IAM(),TRIM(LINE)
        ENDIF
        RETURN
        END


        SUBROUTINE GET_DELTA_TIME_IN_MS(IO_START_TIME_MS,O_CURR_DELTA_MS)
        INCLUDE '($SYSSRVNAM)'
        
        INTEGER*8 IO_START_TIME_MS,O_CURR_DELTA_MS,TEMP
        INTEGER*4 ST

        ST = SYS$GETTIM(TEMP)
        
        IF(IO_START_TIME_MS .LE. 0) THEN
            IO_START_TIME_MS = TEMP / 10000
            O_CURR_DELTA_MS = 0
        ELSE
            O_CURR_DELTA_MS = (TEMP / 10000) - IO_START_TIME_MS
        ENDIF
        
        
        RETURN
        END



        SUBROUTINE INJECT_TIME_STAMP_AS_MESSAGE_ID(PARAMS,OUTBUF,OUTLEN)
        IMPLICIT NONE
C
        INCLUDE '($SYSSRVNAM)'
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        BYTE      OUTBUF(*)
        INTEGER*4 OUTLEN
        
        INTEGER*4 MAX_PAR
        PARAMETER(MAX_PAR = 100)
        INTEGER*4 PARAMS(MAX_PAR)
        
        INTEGER*8 UX_TS, MARKER, MARKER_OFFSET
        
        BYTE BARR(1024)
        CHARACTER*255 LINE
        INTEGER*4 I,ST, SIZE
        
        INTEGER*8 DELTA, START_TIME
        
        INTEGER*8 I8TSTAMP
        INTEGER*1 I1TSTAMP(8)
        INTEGER*4 I4TSTAMP(2)
        EQUIVALENCE(I8TSTAMP,I1TSTAMP)
        EQUIVALENCE(I8TSTAMP,I4TSTAMP)
        
        INTEGER*8 I8TMP
        INTEGER*1 I1TMP(8)
        INTEGER*4 I4TMP(2)
        EQUIVALENCE(I8TMP,I1TMP)
        EQUIVALENCE(I8TMP,I4TMP)
        
        DO I = 1, 255
            LINE(I:I) = CHAR(0)
        ENDDO
        
        CALL GET_UNIX_TIME_MS(UX_TS)
        I8TSTAMP = UX_TS
        
        ! Using seed as base
        IF(PARAMS(18) .EQ. 3) THEN
            I4TSTAMP(1) = PARAMS(31)
            I4TSTAMP(2) = PARAMS(32)
            I8TSTAMP = I8TSTAMP + 1
            PARAMS(31)  = I4TSTAMP(1)
            PARAMS(32)  = I4TSTAMP(2)
            UX_TS = I8TSTAMP
        ! Using seed as base with delta time increases
        ELSEIF(PARAMS(18) .EQ. 4) THEN
            I4TSTAMP(1) = PARAMS(33)
            I4TSTAMP(2) = PARAMS(34)
            START_TIME = I8TSTAMP
            DELTA = UX_TS - START_TIME
            I4TSTAMP(1) = PARAMS(31)
            I4TSTAMP(2) = PARAMS(32)
            I8TSTAMP = I8TSTAMP + DELTA
            PARAMS(31)  = I4TSTAMP(1)
            PARAMS(32)  = I4TSTAMP(2)
            UX_TS = I8TSTAMP
        ENDIF

        SIZE = PARAMS(19)
        
        I8TMP = I8TSTAMP
        DO I = 1,8
            I1TMP(I) = I1TSTAMP(9 - I)
        ENDDO
        DO I = 1,1024
            BARR(I) = 0
        ENDDO

        ST = SIZE - 8
        DO I = 1,8
            BARR(ST + I) = I1TMP(I)
        ENDDO
        
        MARKER = ZEXT(OUTBUF(PARAMS(20)))
        MARKER_OFFSET = 0
        I = 0

        DO WHILE (MARKER_OFFSET .EQ. 0 .AND. I .LE. 9)
            IF(PARAMS(41 + (I*2)) .EQ. MARKER) THEN
                MARKER_OFFSET = PARAMS(42 + (I*2))
            ENDIF
            I = I + 1
        ENDDO
        
        IF(MARKER_OFFSET .NE. 0) THEN
            DO I = MARKER_OFFSET, MARKER_OFFSET + SIZE - 1
                OUTBUF(I) = BARR(I - MARKER_OFFSET + 1)
            ENDDO
            
            IF(PARAMS(6) .GE. 2) THEN
                TYPE *,IAM(),'INJECTING TSTAMP AS MSG ID: (MODE = ',PARAMS(18),')'
            ENDIF

            WRITE(LINE, 902) MARKER, MARKER_OFFSET, SIZE
902         FORMAT('    MARKER = 0x',Z2.2,', OFFSET = ',I4,', SIZE = ',I4)
            IF(PARAMS(6) .GE. 2) THEN
                TYPE *,IAM(),TRIM(LINE)
            ENDIF
            
            WRITE(LINE, 903) UX_TS
903         FORMAT('    TSTAMP = ', I20)
            IF(PARAMS(6) .GE. 2) THEN
                TYPE *,IAM(),TRIM(LINE)
            ENDIF

            WRITE(LINE, 904) (I1TSTAMP(I), I = 8, 1, -1)
904         FORMAT('           ( ', 8(Z2.2,1X), ')')
            IF(PARAMS(6) .GE. 2) THEN
                TYPE *,IAM(),TRIM(LINE)
            ENDIF

        ELSE
            IF(PARAMS(6) .GE. 2) THEN
                TYPE *,IAM(),'SKIPPING INJECTING TSTAMP AS MSG ID:'
            ENDIF
            
            WRITE(LINE, 902) MARKER, MARKER_OFFSET, SIZE
905         FORMAT('    MARKER = 0x',Z2.2,' NOT FOUND IN CFG PARAMS')
            IF(PARAMS(6) .GE. 2) THEN
                TYPE *,IAM(),TRIM(LINE)
            ENDIF
            
        ENDIF
        
        RETURN
        END


        SUBROUTINE PARSE_TXT_LINE(LINE, PARAMS, CFILENAME)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSDEFINE.DEF'

        CHARACTER*1024 LINE
        
        INTEGER*4 MAX_PAR
        PARAMETER(MAX_PAR = 100)
        INTEGER*4 PARAMS(MAX_PAR)

        CHARACTER*24 CFILENAME

        CHARACTER*128 NAME
        CHARACTER*1024 VAL
        
        INTEGER*4 INDEX, I, I4VAL
        
        INTEGER*4 CTOI
        
        DO I = 1,128
            NAME(I:I) = CHAR(0)
        ENDDO
        
        DO I = 1,1024
            VAL(I:I) = CHAR(0)
        ENDDO
        
        I = 1
        INDEX = 0
        
        IF(LINE(1:1) .EQ. '!') THEN
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

        IF(NAME .EQ. 'FILENAME') THEN
            CFILENAME(1:24) = VAL(1:24)
        ELSE
            INDEX = CTOI(NAME,I)
            I4VAL = CTOI(VAL,I)
            PARAMS(INDEX) = I4VAL
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

        SUBROUTINE WRITE_CFG_FILE(LUN,PARAMS,CFILENAME)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSDEFINE.DEF'

        INTEGER*4 LUN, I

        INTEGER*4 MAX_PAR
        PARAMETER(MAX_PAR = 100)
        INTEGER*4 PARAMS(MAX_PAR)

        CHARACTER*24 CFILENAME
        
        WRITE(LUN, 902) CFILENAME
902     FORMAT('FILENAME     = ',A)

        DO I = 1, MAX_PAR
            WRITE(LUN, 903) I, PARAMS(I)
903         FORMAT(I,' = ',I)
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



        SUBROUTINE LOAD_CONFIG_FILE(CFG_FILENAME, PARAMS, CFILENAME, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        CHARACTER CFG_FILENAME*(*)
        INTEGER*4 CFG_LUN, I, ST

        INTEGER*4 MAX_PAR
        PARAMETER(MAX_PAR = 100)
        INTEGER*4 PARAMS(MAX_PAR)

        CHARACTER*24 CFILENAME
        
        CHARACTER*1024 LINE
        
        LOGICAL CFG_FILE_EXISTS, EOF

        INQUIRE(FILE = CFG_FILENAME, EXIST = CFG_FILE_EXISTS)
        IF(CFG_FILE_EXISTS) THEN
            ST = 0
            CALL FIND_AVAILABLE_LUN(CFG_LUN,ST)
            IF(ST .NE. 0) CALL GSTOP(GEXIT_FATAL)

            OPEN(UNIT = CFG_LUN, FILE = CFG_FILENAME, IOSTAT = ST, STATUS = 'OLD')
            IF(ST.NE.0) THEN
                TYPE*,IAM(),TRIM(CFG_FILENAME),' open error, status>',ST
                TYPE*,IAM()
                RETURN
            ENDIF
            
            EOF = .FALSE.
            DO WHILE(.NOT. EOF)
                CALL GET_TXT_LINE_FROM_FILE(CFG_LUN,LINE,EOF)
                IF(.NOT. EOF) THEN
                    CALL PARSE_TXT_LINE(LINE, PARAMS, CFILENAME)
                ENDIF
            ENDDO
            
            CLOSE(CFG_LUN, IOSTAT = ST)
            IF(ST.NE.0) THEN
                TYPE*,IAM(),TRIM(CFG_FILENAME),' close error, status>',ST
                RETURN
            ENDIF
            TYPE*,IAM(),'Configuration load succeeded!'
            TYPE*,IAM(),'-  Loaded file '// TRIM(CFG_FILENAME) // ' !'
            TYPE*,IAM()
        ELSE
            TYPE*,IAM(),'Configuration load failed!'
            TYPE*,IAM(),'-  File ' // TRIM(CFG_FILENAME) // ' does not exist!'
            TYPE*,IAM()
        ENDIF

        RETURN
        END


        SUBROUTINE SAVE_CONFIG_FILE(CFG_FILENAME, PARAMS, CFILENAME, ST)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        CHARACTER CFG_FILENAME*(*)
        INTEGER*4 CFG_LUN, I, ST

        INTEGER*4 MAX_PAR
        PARAMETER(MAX_PAR = 100)
        INTEGER*4 PARAMS(MAX_PAR)

        CHARACTER*24 CFILENAME
        
        LOGICAL CFG_FILE_EXISTS

        INQUIRE(FILE = CFG_FILENAME, EXIST = CFG_FILE_EXISTS)
        IF(CFG_FILE_EXISTS) THEN
            CALL DFILX(CFG_FILENAME,0,0,ST)
            IF (ST .NE. 0) THEN
                TYPE*,IAM(),'Configuration save failed!'
                TYPE*,IAM(),'-  Could not delete file '// TRIM(CFG_FILENAME) // '!'
                TYPE*,IAM()
            ENDIF
        ENDIF
        OPEN(UNIT = CFG_LUN, 
     *       FILE = CFG_FILENAME, 
     *       ACCESS = 'SEQUENTIAL',
     *       STATUS = 'NEW',
     *       RECL = 1024, 
     *       IOSTAT = ST)
        IF(ST .NE. 0) THEN
            TYPE*,IAM(),TRIM(CFG_FILENAME),' open error, status>',ST
            TYPE*,IAM()
            RETURN
        ENDIF

        CALL WRITE_CFG_FILE(CFG_LUN, PARAMS, CFILENAME)

        CLOSE(CFG_LUN, IOSTAT = ST)
        IF(ST .NE. 0) THEN
            TYPE*,IAM(),TRIM(CFG_FILENAME),' close error, status>',ST
            TYPE*,IAM()
            RETURN
        ENDIF
        TYPE*,IAM(),'Configuration save succeeded!'
        TYPE*,IAM(),'-  Saved file '// TRIM(CFG_FILENAME) // '!'
        TYPE*,IAM()

        RETURN
        END


        SUBROUTINE RESET_DEFAULT_VALUES(PARAMS, CFILENAME)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        
        INTEGER*4 MAX_PAR
        PARAMETER(MAX_PAR = 100)
        INTEGER*4 PARAMS(MAX_PAR)
        
        INTEGER*4 I

        CHARACTER*24 CFILENAME



        INTEGER*8 I8START_TIME
        INTEGER*4 I4START_TIME(2)
        EQUIVALENCE(I8START_TIME,I4START_TIME)

        TYPE*,IAM(),'Resetting configuration default values...'
        
        DO I = 1, MAX_PAR
            PARAMS(I) = 0
        ENDDO

        !--------------------------------------------------------------!
        ! Initializing default values for IGSSIM parameters            !
        !--------------------------------------------------------------!
        !PARAMS(1)    ! chosen option
        ! Setting default wait time: 1 second
        PARAMS(2) = 1 ! nr. of units of time to wait
        PARAMS(3) = 2 ! type of units of time to wait: 1 = msec, 2=seconds, 3= minutes
        PARAMS(4) = 2 ! input mode: 1 = byte array, 2 = hex byte array
        PARAMS(5) = 2 ! treat last consecutive separators as null: 1 = yes, 2 = no
        PARAMS(6) = 2 ! set debug mode: 1 = NONE, 2 = INFO, 3 = DEBUG
        PARAMS(7) = 1 ! calculate checksum for message: 1 = yes, 2 = no, 3 = use seed from msg bytes
        PARAMS(8) = 6 ! calculate checksum for message: terminal number
        PARAMS(9) = 3 ! calculate checksum for message: checksum offset
        !PARAMS(10)   ! message count
        PARAMS(11)= 2 ! set repeat mode: 1 = yes, 2 = no
        PARAMS(12)= 0 ! nr. repetitions: 0 = infinite, n, max = 1000000
        !PARAMS(13)   ! line count
        PARAMS(14)= 2 ! do not send messages flag: 1 = yes, 2 = no
        PARAMS(15)= 1 ! inject control sequence: flag: 1 = yes, 2 = no
        PARAMS(16)= 1 ! inject control sequence: control sequence offset
        !PARAMS(17)   ! control sequence register
        PARAMS(18)= 1 ! inject date/time as message id: flag: 
                      !    1 = yes       , 2 = no
                      !  , 3 = from seed , 4 = from seed with delta increment
        PARAMS(19)= 8 ! inject date/time as message id: message id size in bytes
        PARAMS(20)= 2 ! inject date/time as message id: message type selector offset
        PARAMS(41)= (14) * 16 + (0) ! 1) wagers       : message type selector: E0
        PARAMS(42)= 12              ! 1) wagers       : offset
        PARAMS(43)= (14) * 16 + (1) ! 2) cancellations: message type selector: E1
        PARAMS(44)= 12              ! 2) cancellations: offset
        PARAMS(45)= (14) * 16 + (2) ! 3) validations  : message type selector: E2
        PARAMS(46)= 12              ! 3) validations  : offset
        PARAMS(47)= (14) * 16 + (3) ! 4) payments     : message type selector: E3
        PARAMS(48)= 12              ! 4) payments     : offset
        PARAMS(49)= (14) * 16 + (4) ! 5) game prog rep: message type selector: E4
        PARAMS(50)= 11              ! 5) game prog rep: offset
        PARAMS(51)= ( 8) * 16 + (1) ! 6) reprint trx  : message type selector: 81
        PARAMS(52)= 5               ! 6) reprint trx  : offset
        PARAMS(53)= ( 8) * 16 + (2) ! 7) reprint wgr  : message type selector: 82
        PARAMS(54)= 5               ! 7) reprint wgr  : offset
        PARAMS(55)= ( 8) * 16 + (4) ! 8) reprint val  : message type selector: 84
        PARAMS(56)= 5               ! 8) reprint val  : offset
        PARAMS(57)= (15) * 16 + (15)! 9) TBD          : message type selector: FF
        PARAMS(58)= 0               ! 9) TBD          : offset
        PARAMS(59)= ( 0) * 16 + (0) !10) TBD          : message type selector: 00
        PARAMS(60)= 0               !10) TBD          : offset
        PARAMS(31)= 0 ! inject date/time as message id: initial seed
        PARAMS(32)= 0 ! inject date/time as message id: initial seed
        !PARAMS(33)   ! inject date/time as message id: start time for delta increase
        !PARAMS(34)   ! inject date/time as message id: start time for delta increase
        PARAMS(35)= 2 ! set DESFLG: 1 = yes, 2 = no
        PARAMS(36)= 1 ! set DESFLG: value
        PARAMS(37)= 0 ! set DESFLG for which message: 0 = all, n-th message, max = 1000000
        PARAMS(38)= P(DESFLG) ! DESFLG initial value

        CALL GET_UNIX_TIME_MS(I8START_TIME)
        PARAMS(33) = I4START_TIME(1)
        PARAMS(34) = I4START_TIME(2)
        
        CFILENAME = 'IGSSIM.DAT' ! default filename
        
        TYPE*,IAM(),'Resetting configuration default values - Done!'
        TYPE*,IAM()

        RETURN
        END



        SUBROUTINE GET_IGS_LINE_FROM_FILE(LUN,LINE,EOF,PARAMS)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSDEFINE.DEF'

        LOGICAL   EOF
        INTEGER*4 LUN
        INTEGER*4 MAX_PAR
        PARAMETER(MAX_PAR = 100)
        INTEGER*4 PARAMS(MAX_PAR)
        CHARACTER*1024 LINE

        READ(UNIT = LUN,FMT='(A)',END = 100) LINE
        PARAMS(13) = PARAMS(13) + 1 ! Line count
        RETURN

100     CONTINUE
        EOF = .TRUE.
        RETURN
        END




        INTEGER*4 FUNCTION HTOI(STRING, XLEN)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'

        CHARACTER   STRING*(*)
        CHARACTER*1 CH
        INTEGER*1  ICH
        EQUIVALENCE(CH,ICH)
        
        CHARACTER*1 BCH
        INTEGER*1 IBCH
        EQUIVALENCE(BCH,IBCH)
        
        INTEGER*4   XLEN

        INTEGER*4   LEN
        INTEGER*4   AUX, I

        INTEGER*4   STRLEN
        INTEGER*4   K

        LOGICAL IS_DIGIT
        STRLEN = LEN(STRING)
        AUX = 0
        XLEN = 0
        
        DO K = 1, STRLEN
            IS_DIGIT = .FALSE.
            CH = STRING(K:K)
            IF( CH .GE. '0' .AND. CH .LE. '9') THEN
                BCH = '0'
                I = ICH - IBCH
                IS_DIGIT = .TRUE.
            ENDIF
            IF( CH .GE. 'A' .AND. CH .LE. 'F') THEN
                BCH = 'A'
                I = ICH - IBCH + 10
                IS_DIGIT = .TRUE.
            ENDIF
            IF( CH .GE. 'a' .AND. CH .LE. 'f') THEN
                BCH = 'a'
                I = ICH - IBCH + 10
                IS_DIGIT = .TRUE.
            ENDIF
            IF(IS_DIGIT .EQ. .TRUE.) THEN
                AUX = AUX * 16 + I
                XLEN = XLEN + 1
            ENDIF
        ENDDO

        HTOI = AUX
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



        SUBROUTINE GET_UNIX_TIME_MS(UX_TIME_MS)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C ARGUMENTS
        INTEGER*8 UX_TIME_MS

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

        UX_TIME_MS = TIM(8)
     *             + TIM(7) * 1000
     *             + TIM(6) * 1000 * 60
     *             + TIM(5) * 1000 * 60 * 60
     
        DO I = 1970, TIM(1) - 1
            CALL GET_JULIAN_DAY_FOR_YEAR(I,12,31,JDAY)
            UX_TIME_MS = UX_TIME_MS + JDAY * 1000 * 60 * 60 * 24
        ENDDO
        CALL GET_JULIAN_DAY_FOR_YEAR(TIM(1),TIM(2),TIM(3),JDAY)
        JDAY = JDAY - 1 ! Must remove one day, because of that day's milliseconds
        UX_TIME_MS = UX_TIME_MS + (JDAY * 1000 * 60 * 60 * 24)

        RETURN
        END





        INTEGER*8 FUNCTION HTOI8(STRING, XLEN)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'

        CHARACTER   STRING*(*)
        CHARACTER*1 CH
        INTEGER*1  ICH
        EQUIVALENCE(CH,ICH)
        
        CHARACTER*1 BCH
        INTEGER*1 IBCH
        EQUIVALENCE(BCH,IBCH)
        
        INTEGER*4   XLEN

        INTEGER*4   LEN
        INTEGER*8   AUX, I

        INTEGER*4   STRLEN
        INTEGER*4   K

        LOGICAL IS_DIGIT
        STRLEN = LEN(STRING)
        AUX = 0
        XLEN = 0
        
        DO K = 1, STRLEN
            IS_DIGIT = .FALSE.
            CH = STRING(K:K)
            IF( CH .GE. '0' .AND. CH .LE. '9') THEN
                BCH = '0'
                I = ICH - IBCH
                IS_DIGIT = .TRUE.
            ENDIF
            IF( CH .GE. 'A' .AND. CH .LE. 'F') THEN
                BCH = 'A'
                I = ICH - IBCH + 10
                IS_DIGIT = .TRUE.
            ENDIF
            IF( CH .GE. 'a' .AND. CH .LE. 'f') THEN
                BCH = 'a'
                I = ICH - IBCH + 10
                IS_DIGIT = .TRUE.
            ENDIF
            IF(IS_DIGIT .EQ. .TRUE.) THEN
                AUX = AUX * 16 + I
                XLEN = XLEN + 1
            ENDIF
        ENDDO

        HTOI8 = AUX
        RETURN
        END



        INTEGER*8 FUNCTION CTOI8(STRING, XLEN)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'

        CHARACTER   STRING*(*)
        CHARACTER*1 CH
        INTEGER*1  ICH
        EQUIVALENCE(CH,ICH)
        
        CHARACTER*1 BCH
        INTEGER*1 IBCH
        EQUIVALENCE(BCH,IBCH)
        
        INTEGER*4   XLEN

        INTEGER*4   LEN
        INTEGER*8   AUX, I

        INTEGER*4   STRLEN
        INTEGER*4   K

        LOGICAL IS_DIGIT
        STRLEN = LEN(STRING)
        AUX = 0
        XLEN = 0
        
        DO K = 1, STRLEN
            IS_DIGIT = .FALSE.
            CH = STRING(K:K)
            IF( CH .GE. '0' .AND. CH .LE. '9') THEN
                BCH = '0'
                I = ICH - IBCH
                IS_DIGIT = .TRUE.
            ENDIF
            IF(IS_DIGIT .EQ. .TRUE.) THEN
                AUX = AUX * 10 + I
                XLEN = XLEN + 1
            ENDIF
        ENDDO

        CTOI8 = AUX
        RETURN
        END



        SUBROUTINE SET_DESFLG(PARAMS,VAL)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INTEGER*4 VAL
        INTEGER*4 MAX_PAR
        PARAMETER(MAX_PAR = 100)
        INTEGER*4 PARAMS(MAX_PAR)
        
        INTEGER*4 NAME(2), CBUF(CDLEN), ST
        
        IF(PARAMS(6) .GE. 2) THEN
            TYPE *,IAM(),'SETTING NEW VALUE FOR P(DESFLG) : ', VAL
        ENDIF
        
        CALL GETNAM(NAME)                              !FOR QUECMD
        CALL FASTSET(0,CBUF,CDLEN)
        CBUF(1)=DESFLG
        CBUF(2)=VAL
        CBUF(3)=TCPAR
        CBUF(6)=NAME(1)

        ST = 0
        CALL QUECMD(CBUF,ST)
        IF(PARAMS(6) .GE. 3) THEN
            TYPE *,IAM(),'SET_DESFLG: STATUS AFTER QUECMD = ',ST
        ENDIF
        CALL XWAIT(2,1,ST)
        IF(PARAMS(6) .GE. 3) THEN
            TYPE *,IAM(),'SET_DESFLG: STATUS AFTER XWAIT = ',ST
        ENDIF
        RETURN
        END



        SUBROUTINE SET_CMD(PARAMS,INDEX,VAL)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INTEGER*4 INDEX, VAL
        INTEGER*4 MAX_PAR
        PARAMETER(MAX_PAR = 100)
        INTEGER*4 PARAMS(MAX_PAR)
        
        INTEGER*4 NAME(2), CBUF(CDLEN), ST
        CHARACTER*255 LINE
        
        IF(PARAMS(6) .GE. 2) THEN
            WRITE(LINE, 902) INDEX,VAL,VAL
902         FORMAT('SETTING NEW VALUE FOR P(',I0,') = ', I,' [0x',Z8.8,']')
            TYPE *,IAM(),'', TRIM(LINE)
        ENDIF
        
        CALL GETNAM(NAME)                              !FOR QUECMD
        CALL FASTSET(0,CBUF,CDLEN)
        CBUF(1)=INDEX
        CBUF(2)=VAL
        CBUF(3)=TCPAR
        CBUF(6)=NAME(1)

        ST = 0
        CALL QUECMD(CBUF,ST)
        IF(PARAMS(6) .GE. 3) THEN
            TYPE *,IAM(),'SET_CMD: STATUS AFTER QUECMD = ',ST
        ENDIF
        CALL XWAIT(2,1,ST)
        IF(PARAMS(6) .GE. 3) THEN
            TYPE *,IAM(),'SET_CMD: STATUS AFTER XWAIT = ',ST
        ENDIF
        RETURN
        END
