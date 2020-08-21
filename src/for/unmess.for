C
C PROGRAM UNMESS
C
C V14 05-JAN-2016 FRP CR67 New UNSO subtype=3 for non-blocking message
C V13 22-DEC-2014 SCML Only send UNSO and BRO to MXTerminals
C V12 16-OCT-2013 FRP Send UNSO to all MXT in just 1 buffer
C V11 19-JUN-2013 FJG UNSO to all not working for MXT
C V10 21-JUN-2011 FJG Include MXT Terminals
C
C $Log:   GXAFXT:[GOLS]UNMESS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:42:34   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.3   28 Jan 1996 19:00:04   RXK
C  5 line message. Scandinavian letters prepared but commented out 
C  
C     Rev 1.1   17 Nov 1993 16:38:16   SXH
C  Added display of column numbers
C  
C     Rev 1.0   21 Jan 1993 17:57:32   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - unmess.for;1 **
C
C UNMESS.FOR
C
C V02 16-APR-91 MTK INITIAL RELEASE FOR MARYLAND
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C SENDS AN UNSOLICITED NEWS MESSAGE TO A SPECIFIED TERMINAL OR LINE
C MAX 4 LINES OF MESSAGE CAN BE TRANSFERED.
C
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM UNMESS
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'      ! MXSRV
C       INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:X2XPRM.DEF'
C
C
        INTEGER*4 LINMAX                 !
        PARAMETER(LINMAX=5)              ! MAX # OF LINES 
C
        INTEGER*4  J                     !
        INTEGER*4  BUF                   !
        INTEGER*4  FLAG                  !
        INTEGER*4  I                     !
        INTEGER*4  EXT                   !
        INTEGER*4  LEN                   !
        INTEGER*4  TMPLEN                !
        INTEGER*4  LINCNT                !
        INTEGER*4  ST                    !
        INTEGER*4  LIST(NUMAGT)          !
        INTEGER*4  COUNT                 !
        INTEGER*4  MSGLENGHT             ! LENGHT OF READED MESSAGE
        LOGICAL    BROADCAST             ! 
C
        CHARACTER*25  PROMPT             !
        CHARACTER*26  MESS(LINMAX)       !
        CHARACTER*26  TMESS              !
        CHARACTER*26  COLNUM             !
        CHARACTER*1   MODMESS(LINMAX*26) !

        INTEGER*2     DMESS(LINMAX*26)   !
        INTEGER*2     CONTRL             !
        INTEGER*2     CONTRL_BLOCKING    ! V14
        INTEGER*2     CONTRL_NON_BLOCKING! V14

        EQUIVALENCE  (MODMESS(1),DMESS(1))
        EQUIVALENCE(DMESS(1),MESS(1))

        DATA PROMPT/'Enter message for line X:'/
        DATA COLNUM/'12345678901234567890123456'/

        CHARACTER*60 MSG1,MSG2,MSG3
        CHARACTER*61 MSG4            ! V13
        INTEGER*4    TOT_NOTSENT     ! TOTAL MESSAGES NOT SENT !V13

C       DATA CONTRL/ZB120/               !V14
        DATA CONTRL_BLOCKING/ZB120/      !V14
        DATA CONTRL_NON_BLOCKING/ZB320/  !V14

        INTEGER*4   AE1/Z000000C4/,OE1/Z000000D6/,AO1/Z000000C5/
        INTEGER*4   AE2/Z0000008E/,OE2/Z00000099/,AO2/Z0000008F/
        INTEGER*4   SYMBV
        CHARACTER*1 SYMBOL
        EQUIVALENCE (SYMBV,SYMBOL)
C
        LOGICAL     SEND_TO_MXT_ONLY ! V13
        SEND_TO_MXT_ONLY = .TRUE.    ! ONLY SEND UNSO AND BRO TO MXTERMINALS !V13
        TOT_NOTSENT = 0              ! V13
C
C
        CALL COPYRITE
C
C
C
10      CONTINUE
C        TYPE*,IAM(),' Unsolicited message program'                     !V13
        IF(SEND_TO_MXT_ONLY) THEN                                       !V13
          TYPE*,IAM(),' Unsolicited message program (MXTerminals only)' !V13
        ELSE                                                            !V13
          TYPE*,IAM(),' Unsolicited message program'                    !V13
        ENDIF                                                           !V13
        BROADCAST=.FALSE.
        CALL ASELECT(3,LIST,COUNT,BROADCAST,EXT)
        IF(EXT.LT.0) CALL GSTOP(GEXIT_SUCCESS)
        IF(BROADCAST) THEN
          TYPE*,IAM(),COUNT,' broadcast servers selected'
        ELSE  
          TYPE*,IAM(),COUNT,' terminals selected'
        ENDIF
        LINCNT=0
        LEN=0
        TMPLEN=0
        CALL FASTSET(0,DMESS,65)                  ! LINMAX*26/2
        TMESS=' '
C
C
        IF(COUNT.EQ.NUMAGT) THEN
          COUNT=1                ! Send first buffer for Unso message to all X2X terminals
          LIST(1)=-1
          DO 60 I=1,NUMAGT       ! V11
            IF(BTEST(AGTTAB(AGTTYP,I),AGTMXT)) THEN
              COUNT = 2          ! V12 Send a second buffer for Unso message to all MXS terminals
              LIST(2)=-1
              GOTO 61
            ENDIF
60        CONTINUE               ! V11
        ENDIF
61      CONTINUE
C
C GET MESSAGE
C
C
C       CALL CLRSCR(5) ! V11
        TYPE*,IAM(),' Enter Message, Maximum 5 Lines Of 26 Characters:'
        WRITE(5,904)COLNUM
20      CONTINUE
        LINCNT=LINCNT+1
1000    CONTINUE
C
C CHECK IF WE HAVE ENTER ALL LINES
C
        IF(LINCNT .GT. LINMAX) GOTO 40
C
C WRITE PROMPT TO ENTER USER INFORMATION
C
        WRITE(PROMPT(24:24),903) LINCNT
        MSGLENGHT = 0
        CALL INPTEXT(PROMPT, TMESS, MSGLENGHT)
C
C CHECK IF ENTERED IS LESS THAN 26 CHARACTERS
C
        IF(MSGLENGHT .GT. 26) THEN
          TYPE *, IAM()
          TYPE *, IAM(), 'Only 26 Characters Are Allowed By Line'
          TYPE *, IAM()
          GOTO 1000
        ENDIF
C
C SET MESSAGE TO MESSAGE BUFFER ( AND GO TO READ NEX LINE )
C
        IF(LINCNT .LE. LINMAX) MESS(LINCNT) = TMESS
        GOTO 20
C
C (V14) ASK TO THE USER IF THE UNSOLICITED MESSAGE IS:
C BLOCKING (CURRENT SUBTYPE=1: THE REATILER MUST WAIT BEFORE CLOSING THE MESSAGE)
C OR
C NON-BLOCKING (NEW SUBTYPE=3: ALLOW THE RETAILER TO CLOSE THE MESSAGE WITHOUT WAITING)
C
40      CONTINUE
        TYPE *, IAM()
        CALL WIMG(5,'Allow Closing Message [Y/N]')
        CALL YESNO(FLAG)
        CONTRL = CONTRL_BLOCKING                !BLOCKING AS DEFAULT (CURRENT B120)
        IF(FLAG.EQ.1) CONTRL = CONTRL_NON_BLOCKING  !NON-BLOCKING (NEW B320)
C
C SEND MESSAGE TO THE TERMINALS
C
        TYPE *, IAM()
        CALL WIMG(5,'Are You Sure You Want To Send This Message [Y/N]')
        CALL YESNO(FLAG)
        IF(FLAG.NE.1) GOTO 10
        LEN = LINMAX * 26
C
C MODIFY SPECIAL CHARACTERS
C
        DO 45 J=1,LEN
          SYMBOL = MODMESS(J)
          IF(SYMBV.EQ.AE1 .OR. SYMBV.EQ.AE2) MODMESS(J) = 'A'
          IF(SYMBV.EQ.OE1 .OR. SYMBV.EQ.OE2) MODMESS(J) = 'O'
          IF(SYMBV.EQ.AO1 .OR. SYMBV.EQ.AO2) MODMESS(J) = 'A'
          IF(SYMBV.EQ.36) MODMESS(J) = CHAR(164)
45      CONTINUE
C
C SEND MESSAGE
C
        TYPE *, IAM()
        IF(LIST(1).EQ.-1 .OR. LIST(2).EQ.-1) THEN                    !V13
          IF(SEND_TO_MXT_ONLY) THEN                                  !V13
            WRITE(MSG1,905) 1                                        !V13
          ELSE                                                       !V13
            WRITE(MSG1,905) COUNT                                    !V13
          ENDIF                                                      !V13
        ELSE                                                         !V13
          IF(LIST(1).GT.0 .AND. COUNT.EQ.1) THEN ! ONE TERMINAL ONLY !V13
            IF(.NOT. BTEST(AGTTAB(AGTTYP,LIST(1)),AGTMXT)) THEN      !V13
              IF(SEND_TO_MXT_ONLY) THEN                              !V13
                COUNT=0                                              !V13
                WRITE(MSG4,907) LIST(1)                              !V13
                TYPE*,IAM(),MSG4                                     !V13
                WRITE(MSG1,905) COUNT                                !V13
              ENDIF                                                  !V13
            ENDIF                                                    !V13
          ELSE                                                       !V13
          ENDIF                                                      !V13
        ENDIF                                                        !V13
        TYPE*,IAM(),MSG1
        TYPE *, IAM() ! blank line
        DO 100 I=1,COUNT
C
          IF(I.EQ.1 .AND. LIST(I).EQ.-1) THEN  ! ALL TERMINALS THAT ARE NOT MXT !V13
            IF(SEND_TO_MXT_ONLY) THEN          ! V13
              TOT_NOTSENT=TOT_NOTSENT+1        ! V13
              GOTO 100                         ! SKIP ALL TERMINALS THAT ARE NOT MXT !V13
            ENDIF                              ! V13
          ENDIF                                ! V13
C
          IF(LIST(I).GT.0.AND.LIST(I).LE.NUMAGT) THEN           ! V13
            IF(.NOT. BTEST(AGTTAB(AGTTYP,LIST(I)),AGTMXT)) THEN ! TERMINAL IS NOT MXT !V13
              IF(SEND_TO_MXT_ONLY) THEN                         ! SKIP TERMINAL THAT IS NOT MXT !V13
                TOT_NOTSENT=TOT_NOTSENT+1                       ! V13
                WRITE(MSG4,907) LIST(I)                         ! V13
                TYPE*,IAM(),MSG4                                ! V13
                GOTO 100                                        ! V13
              ENDIF                                             ! V13
            ENDIF                                               ! V13
          ENDIF                                                 ! V13
C
          CALL GETBUF(BUF)
          IF(BUF.LE.0) THEN
            TYPE*,IAM(),' Buffer allocation error'
            CALL XWAIT(10,2,ST)
          ENDIF
C
C TRANSFER MESSAGE TO BUFFER
C
          HPRO(OUTLEN,BUF)=LEN+2
          HPRO(MSGNUM,BUF)=0
          IF(BROADCAST) THEN
            HPRO(TRCODE,BUF)=TYPBRO
          ELSE
            HPRO(TRCODE,BUF)=TYPUNS
          ENDIF
          HPRO(TERNUM,BUF)=LIST(I)
C
          IF(I.EQ.2 .AND. LIST(I).EQ.-1) THEN  ! V12 Second buffer for Unso message to all MXS terminals
            HPRO(PRCSRC,BUF)=MXS_COM           !MXSRV
          ENDIF
C
          IF(LIST(I).GT.0.AND.LIST(I).LE.NUMAGT) THEN       !MXSRV
            IF(BTEST(AGTTAB(AGTTYP,LIST(I)),AGTMXT)) THEN   !MXSRV
              HPRO(PRCSRC,BUF)=MXS_COM                      !MXSRV
            ENDIF                                           !MXSRV
          ENDIF                                             !MXSRV
C
          HPRO(INPTAB*2-1,BUF)=CONTRL
          TMPLEN=LEN/2+1
          DO 50 J=1,TMPLEN
            HPRO(INPTAB*2-1+J,BUF)=DMESS(J)
50        CONTINUE
C
C QUEUE BUFFER TO THE OUTPUT QUEUE
C
          CALL QUETRA(LOG,BUF)

          IF(MOD(I,50).EQ.0) THEN
            WRITE(MSG2,906) I,COUNT-I
            TYPE*,IAM(),MSG2
            CALL XWAIT(10,2,ST) ! WAITS FOR 10 SECONDS EACH 50 MESSAGES
          ENDIF
100     CONTINUE
C
        WRITE(MSG3,906) COUNT-TOT_NOTSENT,0
        TYPE*,IAM(),MSG3
        TYPE *, IAM()
        TYPE*,IAM(),'All messages have just been sent.'
        TYPE *, IAM()
        CALL GSTOP(GEXIT_SUCCESS)
C
C900     FORMAT(A26)
900     FORMAT(A)
901     FORMAT(1X,A,' Message terminator not found, try again     ')
903     FORMAT(I1)
904     FORMAT(46X,A26)
C905     FORMAT('# messages to send: ',I4.1)
C906     FORMAT('Sending ',I2.1,' messages  >>  ',I4.1,' messages to finish ...')
905     FORMAT('Starting to send ',I4.1, ' messages...')
906     FORMAT('Sent ',I4.1,' messages  >>  ',I4.1,' messages to finish ...')
907     FORMAT('Message not sent to terminal ',I5.1,'. It is not a MXS terminal.')  ! V13
        END
