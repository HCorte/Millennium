C
C ORCSUBS.FOR
C
C V04 17-NOV-2010 MAC CHECKING SIGN ADDED
C V03 12-NOV-2010 MAC TOO LONG INT CHECKING ADDED
C V02 03-SEP-2010 MAC RFSS0145 - ASFIV FILE ADDED
C V01 23-MAR-2010 FRP Initial Release for Portugal ePassive
C
C ROUTINES TO LOAD PASSIVE ORACLE INTERFACES
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 2010 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C*************************
C SUBROUTINE OPEN_LOG_FILE
C*************************
C OPEN ORACLE LOG FILE
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE OPEN_LOG_FILE
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:ORCCOM.DEF'
C
      INTEGER*4 ST
C
      CALL FIND_AVAILABLE_LUN(LOGLUN,ST)
      IF(ST .NE. 0) THEN
        TYPE*,IAM(),'ERROR GETTING LOGICAL UNIT FOR ',LOGNAM
        CALL GSTOP(GEXIT_FATAL) 
      ENDIF
C
      OPEN(UNIT   = LOGLUN,
     *     FILE   = LOGNAM,
     *     STATUS = 'UNKNOWN', 
     *     IOSTAT = ST)
C
      IF(ST .NE. 0) THEN
        TYPE*,IAM(),'ERROR OPENING ',LOGNAM,' STATUS ',ST
        CALL GSTOP(GEXIT_FATAL) 
      ENDIF
C
      WRITE(LOGLUN,100,IOSTAT=ST) DAYCDC
      IF(ST .NE. 0) THEN
        TYPE*,IAM(),'ERROR WRITING ',LOGNAM,' STATUS ',ST
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
100   FORMAT(17X,'************** ORACLE LOG FILE **************',/,5X,'CDC: ',I4.4,//,
     *       80('-'),/,20X,'ERROR DESCRIPTION',30X,'RECORD NUMBER')
C
      RETURN
      END
C
C**********************                                                !V02...
C SUBROUTINE OPEN_ASFIV
C**********************
C OPEN ASFIV FILE
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE OPEN_ASFIV
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:ORCCOM.DEF'
      INCLUDE 'INCLIB:RECAGTIV.DEF'
C
      INTEGER*4 ST
C
      CALL FIND_AVAILABLE_LUN(ASFIVLUN,ST)
      IF(ST .NE. 0) THEN
        TYPE*,IAM(),'ERROR GETTING LOGICAL UNIT FOR ',SFNAMES(1,ASFIV)
        CALL GSTOP(GEXIT_FATAL) 
      ENDIF
C
      CALL OPENW(ASFIVLUN,SFNAMES(1,ASFIV),4,0,0,ST)
      CALL IOINIT(ASFIVFDB,ASFIVLUN,ASFIVSEC*256)
      IF(ST.NE.0) THEN
        TYPE*,IAM(),'OPEN ERROR - ',SFNAMES(1,ASFIV)
        CALL GSTOP(GEXIT_FATAL) 
      ENDIF
C
      RETURN
      END                                                              !...V02
C
C**************************
C SUBROUTINE WRITE_LOG_FILE
C**************************
C WRITE ORACLE LOG FILE
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE WRITE_LOG_FILE
      IMPLICIT NONE                
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:ORCCOM.DEF'
C
      INTEGER*4 ST
C
      WRITE(LOGLUN,100,IOSTAT=ST) ORC_ERR_STR,ORC_LIN_CNT
      IF(ST .NE. 0) THEN
        TYPE*,IAM(),'ERROR WRITING ',LOGNAM,' STATUS ',ST
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      ORC_ERR_CNT = ORC_ERR_CNT+1
C
100   FORMAT(1X,A70,2X,I6.6)
C
      RETURN
      END
C
C*************************
C SUBROUTINE OPEN_ORC_FILE
C*************************
C OPEN ORACLE INTERFACE FILE
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE OPEN_ORC_FILE
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:ORCCOM.DEF'
C
      INTEGER*4 ST
C
      CALL FIND_AVAILABLE_LUN(ORCLUN,ST)
      IF(ST .NE. 0) THEN
        TYPE*,IAM(),'ERROR GETTING LOGICAL UNIT FOR ',ORCNAM
        CALL GSTOP(GEXIT_FATAL) 
      ENDIF
C
      OPEN(UNIT         = ORCLUN,
     *     FILE         = ORCNAM,
     *     FORM         = 'FORMATTED',
     *     ORGANIZATION = 'SEQUENTIAL',
     *     ACCESS       = 'SEQUENTIAL',
     *     STATUS       = 'OLD', 
     *     IOSTAT       = ST)
C
      IF(ST .NE. 0) THEN
        TYPE*,IAM(),'ERROR OPENING ',ORCNAM,' STATUS ',ST
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      RETURN
      END
C
C*************************
C SUBROUTINE READ_ORC_FILE
C*************************
C READ ORACLE INTERFACE RECORD FILE
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE READ_ORC_FILE(ST)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:STANDARD.DEF'
      INCLUDE 'INCLIB:ORCCOM.DEF'
C
      INTEGER*4 ST
C
      WRITE(ORC_REC,100)
C
      READ(UNIT=ORCLUN,IOSTAT=ST,END=1000,FMT='(<REC_LEN>A)') ORC_REC
      IF(ST.NE.0) THEN
        TYPE*,IAM(),'ERROR READING ',ORCNAM,' STATUS ',ST
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      ST = 0
      ORC_LIN_CNT = ORC_LIN_CNT+1
      RETURN
C
1000  CONTINUE
      ST = EOF
C
100   FORMAT(<REC_LEN>(' '))
C
      RETURN
      END
C
C************************
C SUBROUTINE GET_REC_TYPE
C************************
C GET ORACLE INTERFACE FILE RECORD TYPE
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE GET_REC_TYPE(POS,REC_TYPE)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:ORCCOM.DEF'
C
      INTEGER*4 POS
      INTEGER*4 FIELD_LEN/2/
      CHARACTER REC_TYPE*2
C
      REC_TYPE = ORC_REC(POS:(POS+FIELD_LEN-1))
      POS = POS+FIELD_LEN
C
      IF(REC_TYPE .EQ. 'HP' .OR.
     *   REC_TYPE .EQ. '01' .OR.
     *   REC_TYPE .EQ. '02' .OR.
     *   REC_TYPE .EQ. 'TP') RETURN
C
      ORC_ERR_STR = IAM()//' RECORD INVALID. UNKNOWN RECORD TYPE'
      CALL WRITE_LOG_FILE
C
      RETURN
      END
C
C************************
C SUBROUTINE GET_GEN_DATE
C************************
C GET ORACLE INTERFACE FILE GENERATION DATE
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE GET_GEN_DATE(POS)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:ORCCOM.DEF'
C
      INTEGER*4 POS,GEN_DAT_POS
      INTEGER*4 FIELD_LEN/8/
      INTEGER*4 YEAR,MONTH,DAY
      INTEGER*4 LIM_DAYS(12)
      LOGICAL   IS_LEAP
C
      LIM_DAYS(01) = 31
      LIM_DAYS(02) = 28
      LIM_DAYS(03) = 31
      LIM_DAYS(04) = 30
      LIM_DAYS(05) = 31
      LIM_DAYS(06) = 30
      LIM_DAYS(07) = 31
      LIM_DAYS(08) = 31
      LIM_DAYS(09) = 30
      LIM_DAYS(10) = 31
      LIM_DAYS(11) = 30
      LIM_DAYS(12) = 31
C
      ORC_GEN_DAT = ORC_REC(POS:(POS+FIELD_LEN-1))
      POS = POS+FIELD_LEN
C
      GEN_DAT_POS = 1
      CALL GET_VALUE(GEN_DAT_POS,4,YEAR,ORC_GEN_DAT)
      IF(YEAR .LT. 2010) THEN
        ORC_ERR_STR = IAM()//' HEADER RECORD INVALID. YEAR LOWER THAN 2010'
        CALL WRITE_LOG_FILE
      ENDIF
C
      CALL IS_LEAP_YEAR(YEAR,IS_LEAP)
      IF(IS_LEAP) LIM_DAYS(2) = 29
C
      GEN_DAT_POS = 5
      CALL GET_VALUE(GEN_DAT_POS,2,MONTH,ORC_GEN_DAT)
      IF(MONTH .LT. 1 .OR. MONTH .GT. 12) THEN
        ORC_ERR_STR = IAM()//' HEADER RECORD INVALID. INVALID MONTH'
        CALL WRITE_LOG_FILE
      ENDIF
C
      GEN_DAT_POS = 7
      CALL GET_VALUE(GEN_DAT_POS,2,DAY,ORC_GEN_DAT)
      IF(DAY .LT. 1 .OR. DAY .GT. LIM_DAYS(MONTH)) THEN
        ORC_ERR_STR = IAM()//' HEADER RECORD INVALID. INVALID DAY'
        CALL WRITE_LOG_FILE
      ENDIF
C
      RETURN
      END
C
C*********************
C SUBROUTINE GET_VALUE
C*********************
C GET ORACLE INTERFACE FILE FIELD VALUE
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE GET_VALUE(POS,NUMDIG,INT_VAL,RECORD)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:ORCCOM.DEF'
C
      INTEGER*4 POS,NUMDIG,INT_VAL
      CHARACTER RECORD*(*)                                        !V02
C
      INTEGER*4 NUMDIGCNV,IDX
C
      INTEGER*8 INT_VAL8                                          !V03
      INTEGER*8 CTOI8                                             !V03
C
      INT_VAL=0                                                   !V03
C
      DO IDX = POS,POS+NUMDIG-1
        IF(RECORD(IDX:IDX) .LT. '0' .OR. RECORD(IDX:IDX) .GT. '9') THEN
          ORC_ERR_STR = IAM()//' NUMERIC FIELD CONTAINS NON-DIGITS'
          CALL WRITE_LOG_FILE
        ENDIF
      ENDDO
C
      INT_VAL8 = CTOI8(RECORD(POS:(POS+NUMDIG-1)),NUMDIGCNV)      !V03
C
      IF(NUMDIGCNV .NE. NUMDIG) THEN
        ORC_ERR_STR = IAM()//' SOME DIGITS OF NUMERIC FIELD NOT CONVERTED TO ASC'
        CALL WRITE_LOG_FILE
      ENDIF
C
      IF(INT_VAL8 .LT. 0 .OR. INT_VAL8 .GT. 2145000000) THEN      !V03
        ORC_ERR_STR = IAM()//' INTEGER CONVERSION OVERFLOW'
        CALL WRITE_LOG_FILE
      ELSE                                                        !V03
        INT_VAL=INT_VAL8                                          !V03
      ENDIF
C
      POS = POS+NUMDIG
C
      RETURN
      END
C
C**********************
C SUBROUTINE GET_VALUE8
C**********************
C GET ORACLE INTERFACE FILE FIELD VALUE
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE GET_VALUE8(POS,NUMDIG,INT8_VAL,RECORD)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:ORCCOM.DEF'
C
      INTEGER*4 POS,NUMDIG
      INTEGER*8 INT8_VAL
      CHARACTER RECORD*200
C
      INTEGER*8 CTOI8
      INTEGER*4 NUMDIGCNV,IDX
C
      DO IDX = POS,POS+NUMDIG-1
        IF(RECORD(IDX:IDX) .LT. '0' .OR. RECORD(IDX:IDX) .GT. '9') THEN
          ORC_ERR_STR = IAM()//' NUMERIC FIELD CONTAINS NON-DIGITS'
          CALL WRITE_LOG_FILE
        ENDIF
      ENDDO
C
      INT8_VAL = CTOI8(RECORD(POS:(POS+NUMDIG-1)),NUMDIGCNV)
C
      IF(NUMDIGCNV .NE. NUMDIG) THEN
        ORC_ERR_STR = IAM()//' SOME DIGITS OF NUMERIC FIELD NOT CONVERTED TO ASC'
        CALL WRITE_LOG_FILE
      ENDIF
C
      POS = POS+NUMDIG
C
      RETURN
      END
C
C************************
C SUBROUTINE IS_LEAP_YEAR
C************************
C TO KNOW IF IS LEAP YEAR
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE IS_LEAP_YEAR(YEAR,IS_LEAP)
      IMPLICIT NONE
C
      INTEGER*4 YEAR
      LOGICAL IS_LEAP
C
      IS_LEAP = .FALSE.
C
C IF YEAR IS DIVISIBLE BY 400 IS LEAP YEAR
      IF(MOD(YEAR,400) .EQ. 0) IS_LEAP = .TRUE.
C
C IF YEAR IS DIVISIBLE BY 4 AND NOT BY 100 IT'S LEAP YEAR
      IF(MOD(YEAR,4) .EQ. 0 .AND. MOD(YEAR,100) .NE. 0) IS_LEAP = .TRUE.
C
      RETURN
      END
C
C*************************                                        !V02...
C SUBROUTINE GET_VALUE_SGN
C*************************
C GET ORACLE INTERFACE FILE FIELD VALUE
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE GET_VALUE_SGN(POS,NUMDIG,INT_VAL,RECORD)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:ORCCOM.DEF'
C
      INTEGER*4 POS,NUMDIG,INT_VAL
      CHARACTER RECORD*(*)
      INTEGER*4 SGN
C
      INTEGER*4 NUMDIGCNV,IDX
C
      INTEGER*8 INT_VAL8                                         !V03
      INTEGER*8 CTOI8                                            !V03
C
      INT_VAL=0                                                  !V03
C
      IF (RECORD(POS:POS) .EQ. '-') THEN                         !V04...
        SGN=-1
      ELSEIF (RECORD(POS:POS) .EQ. '+' .OR. RECORD(POS:POS) .EQ. ' ' .OR. RECORD(POS:POS) .EQ. '0') THEN
        SGN=1
      ELSE
        SGN=0
        ORC_ERR_STR = IAM()//' SIGN FIELD IS WRONG'
        CALL WRITE_LOG_FILE
      ENDIF                                                      !...V04
C
      POS=POS+1
C
      DO IDX = POS,POS+NUMDIG-1
        IF(RECORD(IDX:IDX) .LT. '0' .OR. RECORD(IDX:IDX) .GT. '9') THEN
          ORC_ERR_STR = IAM()//' NUMERIC FIELD CONTAINS NON-DIGITS'
          CALL WRITE_LOG_FILE
        ENDIF
      ENDDO
C
      INT_VAL8 = CTOI8(RECORD(POS:(POS+NUMDIG-1)),NUMDIGCNV)     !V03
C
      IF(NUMDIGCNV .NE. NUMDIG) THEN
        ORC_ERR_STR = IAM()//' SOME DIGITS OF NUMERIC FIELD NOT CONVERTED TO ASC'
        CALL WRITE_LOG_FILE
      ENDIF
C
      IF(INT_VAL8 .LT. 0 .OR. INT_VAL8 .GT. 2145000000) THEN     !V03
        ORC_ERR_STR = IAM()//' INTEGER CONVERSION OVERFLOW'
        CALL WRITE_LOG_FILE
      ELSE                                                       !V03
        INT_VAL=INT_VAL8                                         !V03
      ENDIF
C
      INT_VAL = SGN * INT_VAL
C
      POS = POS+NUMDIG
C
      RETURN
      END                                                               !...V02
C
