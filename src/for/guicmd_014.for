C GUICMD_014.FOR
C
C V05 24-APR-2017 JHR ADDED CANCELLED EVENTS
C V04 30-MAR-2015 MTK MODIFIED SUPER 14 GAME
C V03 02-DEC-2003 GXR SUPER14 ADDED
C V02 13-FEB-2001 HXK ALLOW WINNING ROWS TO BE ENTERED FOR
C                 PREVIOUS DRAWS (E.G. POSTPONED DRAW)
C V01 05-FEB-2001 HXK INITIAL RELEASE
C
C 1X2 WINNING NUMBERS ENTRY COMMAND
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 2001 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C This subroutine returns GUI FUNCTION.
C
C Input parameters: NONE               
C
C Output parameters:
C
C  BYTE     OUTBUF(*)    OUTPUT MESSAGE
C  INTEGER*4   MES_LEN       MESSAGE LENGTH
C  INTEGER*4   RET_CODE: 
C                  0 -  no error, message accepted;
C                  value >= 11 -  error number to be sent to Client.
C
C              ERROR NUMBERS > 1000 ARE TO INDICATE THE ROW NUMBER WHERE THE ERROR IS
C
C
C=====OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE GUICMD_014(OUTBUF,MES_LEN,RET_CODE)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'

      INCLUDE 'INCLIB:GUIMPRM.DEF'
      INCLUDE 'INCLIB:GUIARGS.DEF'
      INCLUDE 'INCLIB:RESCOM.DEF'
      INCLUDE 'INCLIB:SPTCOM.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:GUIFIL.DEF'
C
      BYTE        OUTBUF(*)
      INTEGER * 4 MES_LEN,RET_CODE
C
      INTEGER * 4 ST
      INTEGER * 4 NUM_COLS, NUM_ROWS
      INTEGER * 4 PAR,GIND,DRW
      INTEGER * 4 N_WINNING_NUMS
      INTEGER * 4 BUF(CDLEN)
      INTEGER * 4 STATUS
      INTEGER * 4 IND
      INTEGER * 4 I, SCORE(2), J
      INTEGER * 4 OFF
      INTEGER * 4 GNUM
      INTEGER * 4 BCNT
C
      CHARACTER * 40 STATUS_STR
C
      INTEGER * 4 PARAM_CNT 
      PARAMETER(PARAM_CNT = 5 + SPGNBR)
C
C
      RET_CODE = 0
      STATUS   = 0
      STATUS_STR = ' '
C
C
      CALL GUI_GETPARAMS(OUTBUF,ST)
      IF(ST .NE.0 ) THEN
         RET_CODE = 11
         RETURN
      ENDIF

      PAR = GUI_ARGVAL(1)
      IF(PAR .LT. 1 .OR. PAR .GT. 2) THEN
         RET_CODE = 11
         RETURN
      ENDIF

      GIND = GUI_ARGVAL(2)
      IF(GIND .LT. 1 .OR. GIND .GT. MAXIND) THEN
         STATUS = 2
         WRITE(STATUS_STR, 902) 1, MAXIND
         GOTO 100
      ENDIF

      DRW = GUI_ARGVAL(3)
      IF(DRW .LT. 0 .OR. DRW .GT. SPTDRW(GIND)) THEN
         STATUS = 3
         WRITE(STATUS_STR, 903) DRW
         GOTO 100
      ENDIF
      
      GNUM = GTNTAB(TSPT,GIND)

      IF(PAR.EQ.1) THEN
         IF(DRW .EQ. SPTDRW(GIND)) THEN
            CALL GAMLOG(TSPT, GIND, DSPREC, SPTBLK)
         ELSE 
            CALL READW(GAMFDB(1,GNUM), DRW, DSPREC, ST)
            IF(ST .NE. 0) THEN
              CALL OPS('Failed to read '//CGFNAMES(GNUM), ST, DRW)
              WRITE(STATUS_STR, 914) DRW
              RET_CODE = 14
              GOTO 100
            ENDIF
         ENDIF
      ENDIF

      BCNT = 0
      IF(DSPFRG .EQ. 1) BCNT = 1  ! SUPER 14 IS CHECKD OUT OF THE LOOP
      IF(DSPFRG .EQ. 2) BCNT = 0  ! IN "01M" FORMAT WEBVISION SENDS 15 NUMBERS

      N_WINNING_NUMS = GUI_ARGVAL(4) - BCNT
      
      IF(N_WINNING_NUMS + BCNT .NE. DSPMAX) THEN
         STATUS = 4
         WRITE(STATUS_STR, 904) N_WINNING_NUMS
        GOTO 100
      ENDIF

      IND = 4

      BCNT = 0
      IF(DSPFRG .NE. 0) BCNT = 1
C
      IF(PAR .EQ. 1 .AND. DSPSTS .EQ. GAMBFD) THEN
         DO I = 1, N_WINNING_NUMS - BCNT
            IND = IND + 1
            IF(GUI_ARGVAL(IND) .EQ. 1 .OR.            ! "1"
     *         GUI_ARGVAL(IND) .EQ. 2 .OR.            ! "X"
     *         GUI_ARGVAL(IND) .EQ. 4 .OR.            ! "2"
     *         GUI_ARGVAL(IND) .EQ. WINNUM_CANEVENT)  ! "C", Event Cancelled
     *      THEN
               IF(DSPECD(I) .EQ. 0 .AND. GUI_ARGVAL(IND) .EQ. WINNUM_CANEVENT) THEN 
                  STATUS = 17 * 1000 + I     ! Set Row Number In The Error
                  WRITE(STATUS_STR, 917) I   ! Event Not Cancelled, Only "1", "X" Or "2" Entry Is Allowd
                  GOTO 100
               ENDIF
               IF(DSPECD(I) .NE. 0 .AND. GUI_ARGVAL(IND) .NE. WINNUM_CANEVENT) THEN
                  STATUS = 16 * 1000 + I     ! Set Row Number In The Error                     
                  WRITE(STATUS_STR, 916) I   ! Event Cancelled, Only "C" Entry Is Allowed
                  GOTO 100
               ENDIF
               DSPWIN(I) = GUI_ARGVAL(IND)
            ELSE
               STATUS = 6
               WRITE(STATUS_STR, 906) GUI_ARGVAL(IND)
               GOTO 100
            ENDIF
         ENDDO
            
         IF(DSPFRG .EQ. 1) THEN
            DO J = 1, 2
               IND = IND + 1
               IF(GUI_ARGVAL(IND).EQ. 1 .OR.             ! "0"
     *            GUI_ARGVAL(IND).EQ. 2 .OR.             ! "1"
     *            GUI_ARGVAL(IND).EQ. 4 .OR.             ! "M"
     *            GUI_ARGVAL(IND) .EQ. WINNUM_CANEVENT)  ! "C", Event Cancelled
     *         THEN
                  IF(DSPECD(DSPMAX) .EQ. 0 .AND. GUI_ARGVAL(IND) .EQ. WINNUM_CANEVENT) THEN 
                     STATUS = 18 * 1000 + DSPMAX     ! Set Row Number In The Error
                     WRITE(STATUS_STR, 918) DSPMAX   ! Event Not Cancelled, Only "0", "1" Or "M" Entry Is Allowd
                     GOTO 100
                  ENDIF
                  IF(DSPECD(DSPMAX) .NE. 0 .AND. GUI_ARGVAL(IND) .NE. WINNUM_CANEVENT) THEN
                     STATUS = 16 * 1000 + DSPMAX     ! Set Row Number In The Error
                     WRITE(STATUS_STR, 916) DSPMAX   ! Event Cancelled, Only "C" Entry Is Allowed
                     GOTO 100
                  ENDIF
                  SCORE(J) = GUI_ARGVAL(IND)     
               ELSE
                  STATUS = 6
                  WRITE(STATUS_STR, 906) GUI_ARGVAL(IND)
                  GOTO 100
               ENDIF 
            ENDDO
            DSPWIN(DSPMAX) = ISHFT(SCORE(1), 4) + IAND(SCORE(2), '0F'X)
         ENDIF

         IF(DSPFRG .EQ. 2) THEN
            IND = IND + 1
            IF(GUI_ARGVAL(IND) .EQ. 1 .OR.            ! "1"
     *         GUI_ARGVAL(IND) .EQ. 2 .OR.            ! "X"
     *         GUI_ARGVAL(IND) .EQ. 4 .OR.            ! "2"
     *         GUI_ARGVAL(IND) .EQ. WINNUM_CANEVENT)  ! "C", Event Cancelled
     *      THEN
               IF(DSPECD(DSPMAX) .EQ. 0 .AND. GUI_ARGVAL(IND) .EQ. WINNUM_CANEVENT) THEN 
                  STATUS = 17 * 1000 + DSPMAX     ! Set Row Number In The Error
                  WRITE(STATUS_STR, 917) DSPMAX   ! Event Not Cancelled, Only "1", "X" Or "2" Entry Is Allowd
                  GOTO 100
               ENDIF
               IF(DSPECD(DSPMAX) .NE. 0 .AND. GUI_ARGVAL(IND) .NE. WINNUM_CANEVENT) THEN
                  STATUS = 16 * 1000 + DSPMAX     ! Set Row Number In The Error
                  WRITE(STATUS_STR, 916) DSPMAX   ! Event Cancelled, Only "C" Entry Is Allowed
                  GOTO 100
               ENDIF
               DSPWIN(DSPMAX) = GUI_ARGVAL(IND)
            ELSE
               STATUS = 6
               WRITE(STATUS_STR, 906) GUI_ARGVAL(IND)
               GOTO 100
            ENDIF
         ENDIF

         DSPSTS = GAMEN1
         CUR_GIND = GIND
         CUR_GTYP = TSPT 
         CUR_DRAW = DRW
C
      ELSEIF(PAR .EQ. 2 .AND. DSPSTS .EQ. GAMEN1) THEN
         DO I=1,N_WINNING_NUMS-BCNT
            IND = IND + 1
            IF(GUI_ARGVAL(IND) .EQ. 1 .OR.            ! "1"
     *         GUI_ARGVAL(IND) .EQ. 2 .OR.            ! "X"
     *         GUI_ARGVAL(IND) .EQ. 4 .OR.            ! "2"
     *         GUI_ARGVAL(IND) .EQ. WINNUM_CANEVENT)  ! "C", Event Cancelled
     *      THEN
               IF(DSPECD(I) .EQ. 0 .AND. GUI_ARGVAL(IND) .EQ. WINNUM_CANEVENT) THEN 
                  STATUS = 17 * 1000 + I     ! Set Row Number In The Error
                  WRITE(STATUS_STR, 917) I   ! Event Not Cancelled, Only "1", "X" Or "2" Entry Is Allowd
                  GOTO 100
               ENDIF
               IF(DSPECD(I) .NE. 0 .AND. GUI_ARGVAL(IND) .NE. WINNUM_CANEVENT) THEN
                  STATUS = 16 * 1000 + I     ! Set Row Number In The Error
                  WRITE(STATUS_STR, 916) I   ! Event Cancelled, Only "C" Entry Is Allowed
                  GOTO 100
               ENDIF
               DSPHLD(I) = GUI_ARGVAL(IND)
            ELSE
               STATUS = 8
               WRITE(STATUS_STR,908) GUI_ARGVAL(IND)
               GOTO 100
            ENDIF
         ENDDO

         IF(DSPFRG .EQ. 1) THEN
            DO J = 1, 2
               IND = IND+1
               IF(GUI_ARGVAL(IND) .EQ. 1 .OR.            ! "0"
     *            GUI_ARGVAL(IND) .EQ. 2 .OR.            ! "1"
     *            GUI_ARGVAL(IND) .EQ. 4 .OR.            ! "M"
     *            GUI_ARGVAL(IND) .EQ. WINNUM_CANEVENT)  ! "C", Event Cancelled
     *         THEN
                  IF(DSPECD(DSPMAX) .EQ. 0 .AND. GUI_ARGVAL(IND) .EQ. WINNUM_CANEVENT) THEN 
                     STATUS = 18 * 1000 + DSPMAX     ! Set Row Number In The Error
                     WRITE(STATUS_STR, 918) DSPMAX   ! Event Not Cancelled, Only "0", "1" Or "M" Entry Is Allowd
                     GOTO 100
                  ENDIF
                  IF(DSPECD(DSPMAX) .NE. 0 .AND. GUI_ARGVAL(IND) .NE. WINNUM_CANEVENT) THEN
                     STATUS = 16 * 1000 + DSPMAX     ! Set Row Number In The Error
                     WRITE(STATUS_STR, 916) DSPMAX   ! Event Cancelled, Only "C" Entry Is Allowed
                     GOTO 100
                  ENDIF
                  SCORE(J) = GUI_ARGVAL(IND)
               ELSE
                  STATUS = 8
                  WRITE(STATUS_STR, 906) GUI_ARGVAL(IND)
                  GOTO 100
               ENDIF 
            ENDDO
            DSPHLD(DSPMAX) = ISHFT(SCORE(1), 4) +  IAND(SCORE(2), '0F'X)
         ENDIF

         IF(DSPFRG .EQ. 2) THEN
            IND = IND + 1
            IF(GUI_ARGVAL(IND) .EQ. 1 .OR.            ! "1"
     *         GUI_ARGVAL(IND) .EQ. 2 .OR.            ! "X"
     *         GUI_ARGVAL(IND) .EQ. 4 .OR.            ! "2"
     *         GUI_ARGVAL(IND) .EQ. WINNUM_CANEVENT)  ! "C", Event Cancelled
     *      THEN
               IF(DSPECD(DSPMAX) .EQ. 0 .AND. GUI_ARGVAL(IND) .EQ. WINNUM_CANEVENT) THEN 
                  STATUS = 17 * 1000 + DSPMAX     ! Set Row Number In The Error
                  WRITE(STATUS_STR, 917) DSPMAX   ! Event Not Cancelled, Only "1", "X" Or "2" Entry Is Allowd
                  GOTO 100
               ENDIF
               IF(DSPECD(DSPMAX) .NE. 0 .AND. GUI_ARGVAL(IND) .NE. WINNUM_CANEVENT) THEN
                  STATUS = 16 * 1000 + DSPMAX     ! Set Row Number In The Error
                  WRITE(STATUS_STR, 916) DSPMAX   ! Event Cancelled, Only "C" Entry Is Allowed
                  GOTO 100
               ENDIF
               DSPHLD(DSPMAX) = GUI_ARGVAL(IND)
            ELSE
               STATUS = 6
               WRITE(STATUS_STR, 906) GUI_ARGVAL(IND)
               GOTO 100
            ENDIF
         ENDIF

         DSPSTS = GAMENV
         DO I = 1, N_WINNING_NUMS
            IF(DSPWIN(I) .NE. DSPHLD(I)) THEN
               DSPSTS = GAMBFD
               STATUS = 10
               WRITE(STATUS_STR,910)
               GOTO 100
            ENDIF
         ENDDO

         CALL MLTWIN(GNUM, DRW, ST)
         IF(ST .NE. 0) THEN
            DSPSTS = GAMBFD
            RET_CODE = 11
            RETURN
         ENDIF
            

         IF(DRW .EQ. SPTDRW(GIND)) THEN
            CALL FASTSET(0,BUF,CDLEN)
            BUF(1) = 2
            BUF(2) = DRW
            IF(DRW .NE. SPTDRW(GIND) .AND. DRW .NE. 0) THEN
              DSPSTS = GAMBFD  ! because current draw is not being updated
            ENDIF
            BUF(3) = TCSPT
            BUF(6) = 'GUI '
            BUF(8) = GIND
            OFF = 0
            DO I = 1,DSPMAX
               CALL ISBYTE(DSPWIN(I),BUF(9),OFF)
               OFF = OFF + 1
            ENDDO
            CALL QUECMD(BUF, ST)
            IF(ST .NE. 0) THEN
               DSPSTS=GAMBFD
               RET_CODE = 11
               RETURN
            ENDIF
            IF(DRW .EQ. SPTDRW(GIND) .OR. DRW .EQ. 0) THEN
               CALL FASTSET(0, BUF, CDLEN)
               BUF(1) = 1
               BUF(2) = DSPSTS
               BUF(3) = TCSPT
               BUF(6) = 'GUI '
               BUF(8) = GIND
               CALL QUECMD(BUF, ST)
               IF(ST.NE.0) THEN
                  DSPSTS=GAMBFD
                  RET_CODE = 11
                  RETURN
               ENDIF
            ENDIF
         ELSE
            CALL WRITEW(GAMFDB(1, GNUM), DRW, DSPREC, ST)
            IF(ST .NE. 0) THEN
               STATUS = 15
               CALL OPS('Failed to write '//CGFNAMES(GNUM),ST,DRW)
               WRITE(STATUS_STR, 915) DRW
               GOTO 100
            ENDIF 
            CALL VARESULT(GIND, DRW)
         ENDIF  
      ELSEIF(DSPSTS .EQ. GAMENV) THEN
         STATUS = 12
         WRITE(STATUS_STR,912) 
         GOTO 100
      ELSE
         STATUS_STR = 'Bad request combination'
         STATUS = 13
C***     WRITE(STATUS_STR, 913) PAR, DSPSTS
         GOTO 100
      ENDIF
C
C SEND DATA TO GUI
C
100   CONTINUE
      CALL GUIARG_INIT()
C
      NUM_COLS = 2
      NUM_ROWS = 1
      CALL GUIARG_NEXT_SET(OUTBUF, NUM_COLS)
C
C STATUS BACK
C
      CALL GUIARG_INT4(OUTBUF, STATUS)
      CALL GUIARG_CHAR(OUTBUF, %REF(STATUS_STR), 40)
C
      CALL GUIARG_SET_MESLEN(MES_LEN)
C
      RETURN
C
902   FORMAT('Invalid game index: <',X, I2, X, 'or >', X, I2)
903   FORMAT('Invalid Draw#:', X, I8)
904   FORMAT('Invalid Number of win numbers:', X, I8)
906   FORMAT('Invalid number', X, I2, X, 'entered')
908   FORMAT('Invalid verification number', X, I2, X, 'entered')
910   FORMAT('Winning numbers verification error.')
912   FORMAT('Winning results entered and verified.')
913   FORMAT('Invalid parameter', X, I2, X, 'and game status', X, I2)
914   FORMAT('Error reading draw file for draw:',X, I5)
915   FORMAT('Error writting draw file for draw:', X, I5)
916   FORMAT(I2.2, X, 'Event Is Cancelled, Enter C')         ! SET THE EVENT NUMBRE AT THE 
917   FORMAT(I2.2, X, 'Event Is Not Cancelled, Enter 1X2')   ! BEGINING, SO THE WEBVISION
918   FORMAT(I2.2, X, 'Event Is Not Cancelled, Enter 01M')   ! CAN DISPLAY THE DATA IN PORTUGUESE
      END
