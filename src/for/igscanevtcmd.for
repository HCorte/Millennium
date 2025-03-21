C IGSCANEVTCMD.FOR
C
C SUBROUTINES CHECK IF THE INFORMATION THAT HAS BEEN SEND BY THE "IGS" PLATFORM
C IS GOOD TO BE PROCESSED BY THE EVOLUTION SYSTEM ACCORDING WITH THE MESSAGE DATA.
C IF ONE OF THE FIELS IN THE MESSAGE THAT HAS BEEN SEND IS NOT GOOD THE SPORTS
C CANCEL EVENTS COMMAND WILL BE NOT EXECUTED.
C
C     **************************************************************************
C
C        THIS ITEM IS THE PROPERTY OF GTECH CORPORATION, POVIDENCE, RHODE
C     ISLAND, AND CONTAINS CONFIDENTIAL AND TRADE SECRET INFORMATION. IT MAY
C     NOT BE TRANSFERRED FROM THE CUSTODY OR CONTROL OF GTECH EXCEPT AS AUTO -
C     RIZED IN WRITING BY AN OFFICER OF GTECH. NEITHER THIS ITEM NOR THE
C     INFORMATION IT CONTAINS MAY BE USED, TRANSFERRED, REPRODUCED, PUBLISHED
C     OR DISCLOSED, IN WHOLE OR IN PART, AND DIRECTLY OR INDIRECTLY, EXCEPT AS
C     EXPRESSLY AUTHORIZED BY AN OFFICER OR GTECH, PURSUANT TO WRITTEN AGREEMENT
C
C     Copyright 2000 GTECH Corporation. All Rights Reserved
C
C     **************************************************************************
C
C ******************************************************************************
C
C     SUBROUTINE: SHOW_SPORTS_CMD_ERROR
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 09 / 06 / 2017
C
C ******************************************************************************
C
C SUBROUTINE TO SHOW AN ERROR MESSAGE IN THE CONSOLE IF ONE OF THE FIELDS SEND IN
C THE "IGS" SPORTS COMMAND MESSAGE IS NOT GOOD
C
C==== OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE SHOW_SPORTS_CMD_ERROR(ERROR_MESSAGE, ERROR_VAL1, ERROR_VAL2)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
      INTEGER * 4 ERROR_VAL1         ! ERROR VALUE TO BE DISPLAYED
      INTEGER * 4 ERROR_VAL2         ! ERROR VALUE TO BE DISPLAYED
C
      CHARACTER * (*) ERROR_MESSAGE   ! ERROR MESSAGE TO BE DISPLAYED
C
      INTEGER * 4 SYS_LIVE            ! SYSTEM IS LIVE

      ! CHECK IF THE SYSTEM IS UP TO SHOW THE ERROR MESSAGE IN THE CONSOLE
      CALL CHCKDIS(SYS_LIVE)
C
      IF(SYS_LIVE .EQ. 0) THEN
        CALL OPS('SPORTS ERROR CANCEL EVENT COMMAND DATA FROM IGS PLATFORM !!!', 0, 0)
        CALL OPS(ERROR_MESSAGE, ERROR_VAL1, ERROR_VAL2)
      ELSE
        TYPE *, IAM(), 'Sports Error Cancel Event Command Data From IGS Platform'
        TYPE *, IAM()
        TYPE *, IAM(), 'Error Message: ', ERROR_MESSAGE
        TYPE *, IAM(), 'Error Value 1: ', ERROR_VAL1
        TYPE *, IAM(), 'Error Value 2: ', ERROR_VAL2
        TYPE *, IAM()
      ENDIF
C
      END


C ******************************************************************************
C
C     SUBROUTINE: IGS_CANCEL_EVENTS_CMD
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 09 / 06 / 2017
C
C ******************************************************************************
C
C ROUTINE TO PROCESS THE "IGS" SPORTS CANCEL EVENTS COMMAND. THE ROUTINE WILL CHECK
C THE INPUT DATA AND IF THE INPUT DATA DON'T HAVE ERRORS, THE COMMAND TO CANCEL THE
C SPORTS EVENT WILL BE SEND TO THE "CMDPRO" TASK TO BE EXECUTED. IF THE INPUT DATA
C HAS GOT ERROR AN ERROR MESSAGE WILL BE DISPLAYED IN THE CONSOLE AND THE COMMAND
C WILL NOT BE EXECUTED IN THE EVOLUTION SYSTEM
C
C==== OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE IGS_CANCEL_EVENTS_CMD(AGT_NUM,
     *                                 IGS_CRN,    ! CROSS REFERENCE NUMBER
     *                                 IGS_HBN,    ! HOST BUFFER NUMBER
     *                                 IGS_CDC,    ! HOST "CDC" DATE
     *                                 TER_NUM,    ! TERMINAL NUMBER
     *                                 IGS_CAS,    ! CONTROL AND SEQUENCE
     *                                 IGS_MTS,    ! MESSAGE TYPE AND MESSAGE SUBTYPE
     *                                 IGS_CHK,    ! CHECKSUM MESSAGE
     *                                 GTYP,       ! GAME TYPE
     *                                 GIND,       ! GAME INDEX
     *                                 IGS_AGT,    ! AGENT NUMBER
     *                                 IGS_MID,    ! MESSAGE ID
     *                                 CMD_VAL,    ! COMMAND VALUE
     *                                 CMD_SRC,    ! COMMAND SOURCE
     *                                 CMD_DATA,   ! COMMAND DATA
     *                                 CMD_DTLN)   ! COMMAND DATA LENGTH
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:DSPREC.DEF'

      ! ROUTINE PARAMETERS
      INTEGER * 8 IGS_MID      ! MESSAGE ID
C
      INTEGER * 4 AGT_NUM      ! AGENT NUMBER
      INTEGER * 4 IGS_CRN      ! CROSS REFERENCE NUMBER
      INTEGER * 4 IGS_HBN      ! HOST BUFFER NUMBER
      INTEGER * 4 IGS_CDC      ! HOST "CDC" DATE
      INTEGER * 4 TER_NUM      ! TERMINAL NUMBER
      INTEGER * 4 IGS_CAS      ! CONTROL AND SEQUENCE
      INTEGER * 4 IGS_MTS      ! MESSAGE TYPE AND MESSAGE SUBTYPE
      INTEGER * 4 IGS_CHK      ! CHECKSUM MESSAGE
      INTEGER * 4 GTYP         ! GAME TYPE
      INTEGER * 4 GIND         ! GAME INDEX
      INTEGER * 4 IGS_AGT      ! AGENT NUMBER
      INTEGER * 4 CMD_VAL      ! COMMAND VALUE
      INTEGER * 4 CMD_SRC      ! COMMAND SOURCE
      INTEGER * 4 CMD_DATA(*)  ! COMMAND DATA
      INTEGER * 4 CMD_DTLN     ! COMMAND DATA LENGTH

      ! ROUTINE VARIABLES
      INTEGER * 4 IDX          ! INDEX COUNTER
      INTEGER * 4 FSTS         ! FUNCTION STATUS
      INTEGER * 4 GNUM         ! GAME NUMBER
      INTEGER * 4 DRAW         ! DRAW NUMBER
      INTEGER * 4 ROW_NUM      ! ROW NUMBER COUNTER
      INTEGER * 4 DRAW_WEEK    ! DRAW WEEK
      INTEGER * 4 DRAW_YEAR    ! DRAW YEAR
      INTEGER * 4 EVNT_DCBM    ! EVENT DRAW CANCEL BITMAP

      ! EXTERNAL ROUTINES
      INTEGER * 4  GETDRW      ! FUNCTION TO GET THE DRAW NUMBER FROM DRAW WEEK AND DRAW YEAR

      ! CHECK THE SPORT CANCEL EVENT COMMAND DATA SEND BY "IGS" PLATFORM -- AGENT NUMBER --
      IF(AGT_NUM .NE. 0) THEN
        CALL SHOW_SPORTS_CMD_ERROR('Agent Number Is not Zero', AGT_NUM, AGT_NUM)
        RETURN
      ENDIF

      ! CHECK THE SPORT CANCEL EVENT COMMAND DATA SEND BY "IGS" PLATFORM -- CROSS REFERENCE NUMBER --
      IF(IGS_CRN .NE. 0) THEN
        CALL SHOW_SPORTS_CMD_ERROR('Cross Reference Number Is Not Zero', IGS_CRN, IGS_CRN)
        RETURN
      ENDIF

      ! CHECK THE SPORT CANCEL EVENT COMMAND DATA SEND BY "IGS" PLATFORM -- HOST BUFFER NUMBER --
      IF(IGS_HBN .NE. 0) THEN
        CALL SHOW_SPORTS_CMD_ERROR('Host Buffer Number Is Not Zero', IGS_HBN, IGS_HBN)
        RETURN
      ENDIF

      ! CHECK THE SPORT CANCEL EVENT COMMAND DATA SEND BY "IGS" PLATFORM -- HOST "CDC" DATE --
      IF(IGS_CDC .NE. 0) THEN
        CALL SHOW_SPORTS_CMD_ERROR('Host CDC Date Is Not zero', IGS_CDC, IGS_CDC)
        RETURN
      ENDIF

      ! CHECK THE SPORT CANCEL EVENT COMMAND DATA SEND BY "IGS" PLATFORM -- TERMINAL NUMBER --
      IF(TER_NUM .NE. 0) THEN
        CALL SHOW_SPORTS_CMD_ERROR('Terminal Number Is Not Zero', TER_NUM, TER_NUM)
        RETURN
      ENDIF

      ! CHECK THE SPORT CANCEL EVENT COMMAND DATA SEND BY "IGS" PLATFORM -- CONTROL AND SEQUENCE --
      IF(IGS_CAS .NE. 0) THEN
        CALL SHOW_SPORTS_CMD_ERROR('IGS Control And Sequence Is Not Zero', IGS_CAS, IGS_CAS)
        RETURN
      ENDIF

      ! CHECK THE SPORT CANCEL EVENT COMMAND DATA SEND BY "IGS" PLATFORM -- MESSAGE TYPE AND MESSAGE SUBTYPE --
      IF(IGS_MTS .NE. 238) THEN
        CALL SHOW_SPORTS_CMD_ERROR('IGS Message Type != 14 Or SubType != 14', IGS_MTS, IGS_MTS)
        RETURN
      ENDIF

      ! CHECK THE SPORT CANCEL EVENT COMMAND DATA SEND BY "IGS" PLATFORM -- CHECKSUM MESSAGE --
      IF(IGS_CHK .NE. 0) THEN
        CALL SHOW_SPORTS_CMD_ERROR('IGS Message Checksum Is Not Zero', IGS_CHK, IGS_CHK)
        RETURN
      ENDIF

      ! CHECK THE SPORT CANCEL EVENT COMMAND DATA SEND BY "IGS" PLATFORM -- GAME TYPE --
      IF(GTYP .NE. TSPT) THEN
        CALL SHOW_SPORTS_CMD_ERROR('The Game Type Is Not An Sports Game Type', GTYP, GTYP)
        RETURN
      ENDIF

      ! CHECK THE SPORT CANCEL EVENT COMMAND DATA SEND BY "IGS" PLATFORM -- GAME INDEX --
      IF(GIND .LE. 0 .OR. GIND .GT. MAXIND) THEN
        CALL SHOW_SPORTS_CMD_ERROR('The Game Index Is Out The Valid Limits', GIND, GIND)
        RETURN
      ENDIF

      ! CHECK THE GAME INDEX REQUESTED, ONLY THE TOTOBOLA AND THE TOTOBOLA EXTRA INDEX ARE VALID ONES
      IF(GIND .NE. 1 .AND. GIND .NE. 3) THEN
        CALL SHOW_SPORTS_CMD_ERROR('The Game Index Is Not A Valid Game Index', GIND, GIND)
        RETURN
      ENDIF

      ! CHECK IF THE GAME NUMBER IS ACTIVE IN THE SYSTEM
      GNUM =  GTNTAB(GTYP, GIND)

      IF(GNUM .LE. 0 .OR. GNUM .GT. MAXGAM) THEN
        CALL SHOW_SPORTS_CMD_ERROR('The Sport Game Is Not Active In The System', GNUM, GIND)  ! SHOW GAME INDEX IN ERROR
        RETURN
      ENDIF
 
      ! CHECK THE SPORT CANCEL EVENT COMMAND DATA SEND BY "IGS" PLATFORM -- "IGS" AGENT NUMBER --
      IF(IGS_AGT .NE. 0) THEN
        CALL SHOW_SPORTS_CMD_ERROR('Second Agent Number Is not Zero', IGS_AGT, IGS_AGT)
        RETURN
      ENDIF

      ! GET COMMAND DATA
      EVNT_DCBM = CMD_VAL       ! EVENT DRAW CANCEL BITMAP
      DRAW_WEEK = CMD_DATA(1)   ! DRAW WEEK
      DRAW_YEAR = CMD_DATA(2)   ! DRAW YEAR

      ! CHECK THE SPORT CANCEL EVENT COMMAND DATA SEND BY "IGS" PLATFORM -- DRAW WEEK --
      IF(DRAW_WEEK .LE. 0  .OR. DRAW_WEEK .GT. 105) THEN
        CALL SHOW_SPORTS_CMD_ERROR('The Cancel Draw Week Is Wrong', DRAW_WEEK, DRAW_WEEK)
        RETURN
      ENDIF

      ! CHECK THE SPORT CANCEL EVENT COMMAND DATA SEND BY "IGS" PLATFORM -- DRAW WEEK --
      IF(DRAW_YEAR .LT. 2005 .OR. DRAW_YEAR .GT. 2100) THEN
        CALL SHOW_SPORTS_CMD_ERROR('The Cancel Draw Year Is Wrong', DRAW_YEAR, DRAW_YEAR)
        RETURN
      ENDIF

      ! GET THE DRAW NUMBER TO CANCEL THE EVENTS
      DRAW = GETDRW(DRAW_YEAR, DRAW_WEEK, GNUM)

      ! CHECK THE DRAW NUMBER -- DRAW NUMBER MUST BE SET --
      IF(DRAW .LE. 0) THEN
        CALL SHOW_SPORTS_CMD_ERROR('Draw Week-Year For Draw Number Errors', DRAW_WEEK, DRAW_YEAR)
        CALL SHOW_SPORTS_CMD_ERROR('Draw Not Found For The Draw Week-Year', DRAW, DRAW)
        RETURN
      ENDIF

      ! CHECK THE DRAW DATA -- DRAW ALREAY CANCELLED OR WINNING NUMBERS ALREADY ENTERED --
      CALL READ_SPORTS_DRAW_FROM_MEM_OR_DISK(DRAW, GIND, DSPREC, .TRUE., FSTS)
C
      IF(FSTS .NE. 0) THEN
        CALL SHOW_SPORTS_CMD_ERROR('Unable To Read The Draw Requested', DRAW, FSTS)
        RETURN
      ENDIF

      ! CHECK IF THE FULL DRAW THAT HAS BEEN REQUESTED HAS BEEN ALREADY CANCELLED
      IF(DSPDCD .NE. 0) THEN
        CALL SHOW_SPORTS_CMD_ERROR('The Full Draw Has Been Already Cancelled', DRAW, DSPDCD)
        RETURN
      ENDIF

      ! CHECK IF THE WINNING NUMBERS FOR THE DRAW REQUESTED HAVE BEEN ALREADY ENTERED
      IF(DSPSTS .GE. GAMEN1) THEN
        CALL SHOW_SPORTS_CMD_ERROR('The Winning Number Have Been Already Entered', DRAW, DSPDCD)
        RETURN
      ENDIF

      ! CHECK THE SPORT CANCEL EVENT COMMAND DATA SEND BY "IGS" PLATFORM -- COMMAND DATA FROM INDEX 3 TO 5 --
      DO IDX = 3, CMD_DTLN
         IF(CMD_DATA(IDX) .NE. 0) THEN
           CALL SHOW_SPORTS_CMD_ERROR('Cancel Event Command Data Is Not Zero', CMD_DATA(IDX), IDX)
           RETURN
         ENDIF
      ENDDO
C
      ! LOOP TO SEND THE COMMAND FOR THE EVENTS THAT SHOULD BE CANCELLED -- CHECK COMMAND BITMAP --
      !
      ! NOTE: - SOME DATA FROM THE INPUT MESSAGE WILL BE CHECKED WHEN THE COMMAND WILL BE PROCESSED
      !         SO IS NOT SENSE CHECK THE SAME DATA HERE. DUE THE COMMAND PROCESS ROW BY ROW IF THERE
      !         ARE ERROR THE COMMAND WILL BE REJECTED AND THE DATA THAT IS WRONG CAN BE CHECKED IN THE
      !         SAME COMMAND STRUCTURE
      DO ROW_NUM = 1, 32
        IF(BTEST(EVNT_DCBM, 32 - ROW_NUM) .EQ. .TRUE.) THEN
          CALL SEND_SPORTS_CANCEL_EVENTS_CMD(GIND, DRAW, ROW_NUM, CMD_SRC, IGS_MID)
        ENDIF
      ENDDO
C
      END


C ******************************************************************************
C
C     SUBROUTINE: SEND_SPORTS_CANCEL_EVENTS_CMD
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 12 / 03 / 2003
C
C ******************************************************************************
C
C FUNCTION REQUEST TO SEMD THE CANCELLATION COMMAND TO THE "CMDPRO" TASK
C
C==== OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE SEND_SPORTS_CANCEL_EVENTS_CMD(GIND, DRAW, CMD_VAL, CMD_SRC, IGS_MID)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
 
      ! ROUTINE PARAMETERS
      INTEGER * 8 IGS_MID      ! MESSAGE ID
C
      INTEGER * 4 GIND     ! GAME NUMBER
      INTEGER * 4 DRAW     ! DRAW NUMBER
      INTEGER * 4 CMD_VAL  ! COMMAND VALUE
      INTEGER * 4 CMD_SRC  ! COMMAND SOURCE

      ! ROUTINE VARIABLES
      INTEGER * 4 FSTS           ! FUNCTION STATUS
      INTEGER * 4 CMD_BUF(CDLEN) ! COMMAMD BUFFER
C
      CALL FASTSET(0, CMD_BUF, CDLEN)
C
      CMD_BUF(1)  = 5              ! COMMAND NUMBER
      CMD_BUF(2)  = CMD_VAL        ! COMMAND VALUE
      CMD_BUF(3)  = TCSPT          ! SPORTS COMMAND
      CMD_BUF(6)  = CMD_SRC        ! COMMAND SOURCE SEND BY "IGS" PLATFORM
      CMD_BUF(8)  = DRAW           ! DRAW NUMBER
      CMD_BUF(9)  = GIND           ! GAME INDEX
      CMD_BUF(10) = CMD_VAL        ! COMMAND VALUE, ROW_NUM OR TOTAL CANCEL EVENTS TO CANCEL DRAW
      CMD_BUF(11) = IAND(ISHFT(IGS_MID, -32), 'FFFFFFFF'X)  ! MESSAGE ID -- HIGH PART --
      CMD_BUF(12) = IAND(ISHFT(IGS_MID,   0), 'FFFFFFFF'X)  ! MESSAGE ID -- LOW  PART --
C
      CALL QUECMD(CMD_BUF, FSTS)
C
      IF(FSTS .NE. 0) THEN
        CALL SHOW_SPORTS_CMD_ERROR('Unable To Send The Cancel Event Command', FSTS, FSTS)
        RETURN
      ENDIF
C
      END
