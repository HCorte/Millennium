C
C EURTIMR.FOR
C
C V02 20-MAY-2014 SCML Added support to PLACARD Project - IGS
C

C
C     ADDTIMER - ADD BUFFER TO TIMER QUEUE
C
C     CALL ADDTIMER(LIST_INDEX,XRFNUM,BUF)
C
C INPUT:
C
C LIST_INDEX: LIST INDEX
C     XRFNUM: REFERENCE # TO STORE IN TABLE (CROSS REFERENCE #)
C        BUF: PROCOM BUFFER #
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE ADDTIMER(LIST_INDEX,XRFNUM,BUF)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:EURCON.DEF'
C
        INTEGER*4 XRFNUM
        INTEGER*4 BUF
        INTEGER*4 OLDBOT
        
        ! Main difference is the introduction and extension of LIST_INDEX parameter
        INTEGER*4 LIST_INDEX
C
        IF (EURTIMERTOP(LIST_INDEX) .EQ. -1) THEN
           EURTIMERTOP(LIST_INDEX) = BUF
           EURTIMERBOT(LIST_INDEX) = BUF
           EURTIMERLIST(LIST_INDEX,EURTIMERNXT,BUF) = -1
           EURTIMERLIST(LIST_INDEX,EURTIMERPRV,BUF) = -1
        ELSE
           OLDBOT=EURTIMERBOT(LIST_INDEX)
           EURTIMERLIST(LIST_INDEX,EURTIMERNXT,OLDBOT) = BUF
           EURTIMERLIST(LIST_INDEX,EURTIMERPRV,BUF) = OLDBOT
           EURTIMERLIST(LIST_INDEX,EURTIMERNXT,BUF) = -1
           EURTIMERBOT(LIST_INDEX) = BUF
        ENDIF
C
        EURTIMERLIST(LIST_INDEX,EURTIMERBEG,BUF) = P(ACTTIM)  !CURRENT TIME
        EURTIMERLIST(LIST_INDEX,EURTIMERSER,BUF) = XRFNUM     !SERIAL #
C
        RETURN
        END
C
C
C**********************************************
C
C REMTIMER
C
C CALL REMTIMER(LIST_INDEX,XRFNUM,BUF,STATUS)
C
C INPUT:
C   LIST_INDEX: LIST INDEX
C       XRFNUM: SERIAL # WHICH SHOULD BE PRESENT
C          BUF: PROCOM BUFFER TO REMOVE
C
C OUTPUT:
C   STATUS:  REMOVE STATUS (-1=NOT ON QUEUE, -2=SERIAL NOT MATCH)
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE REMTIMER(LIST_INDEX,XRFNUM,BUF,STATUS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:EURCON.DEF'
C
        INTEGER*4 XRFNUM
        INTEGER*4 BUF
        INTEGER*4 NXT
        INTEGER*4 PRV
        INTEGER*4 STATUS
        INTEGER*4 LIST_INDEX
C
C CHECK FOR AN INVALID BUFFER NUMBER.
C
        STATUS=0
        IF(BUF .LT. 1 .OR. BUF .GT. NUMPRO) THEN
          STATUS = -3
          CALL OPS('CHECK FOR AN INVALID BUFFER NUMBER',STATUS,0)
          GOTO 9000
        ENDIF
C
C If the buffer # is no longer linked in the list, then return
C with error code -1.
C
        IF(EURTIMERLIST(LIST_INDEX,EURTIMERNXT,BUF) .EQ. 0 .OR. 
     *     EURTIMERLIST(LIST_INDEX,EURTIMERPRV,BUF) .EQ. 0) THEN
          STATUS = -1
          GOTO 9000
        ENDIF
C
C If serial # doesn't match, return with error code -2
C
        IF(EURTIMERLIST(LIST_INDEX,EURTIMERSER,BUF) .NE. XRFNUM) THEN
          STATUS = -2
          GOTO 9000
        ENDIF
C
C Otherwise, remove from linked list.
C
        NXT = EURTIMERLIST(LIST_INDEX,EURTIMERNXT,BUF)
        PRV = EURTIMERLIST(LIST_INDEX,EURTIMERPRV,BUF)
C
        IF(PRV .NE. -1)THEN
          EURTIMERLIST(LIST_INDEX,EURTIMERNXT,PRV) = NXT
        ELSE
          EURTIMERTOP(LIST_INDEX) = NXT
        ENDIF
C
        IF(NXT .NE. -1)THEN
          EURTIMERLIST(LIST_INDEX,EURTIMERPRV,NXT) = PRV
        ELSE
          EURTIMERBOT(LIST_INDEX) = PRV
        ENDIF
C
        EURTIMERLIST(LIST_INDEX,EURTIMERNXT,BUF) = 0
        EURTIMERLIST(LIST_INDEX,EURTIMERPRV,BUF) = 0
C
        GOTO 9000
C
9000    CONTINUE
        RETURN
        END
C
C**********************************************
C
C CHKTIMER
C
C CALL CHKTIMER(LIST_INDEX,XRFNUM,BUF)
C
C INPUT:
C   LIST_INDEX: LIST INDEX
C
C OUTPUT:
C       XRFNUM: INTERNAL SRL # OF TIMED OUT XACTION
C          BUF: PROCOM BUFFER NUMBER TO CHECK FOR TIMEOUT
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CHKTIMER(LIST_INDEX,XRFNUM,BUF)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:EURCON.DEF'
C
        INTEGER*4 XRFNUM
        INTEGER*4 BUF
        INTEGER*4 PASTIM
        INTEGER*4 LIST_INDEX
        INTEGER*4 WHICH_PARAM
C
        IF(EURTIMERTOP(LIST_INDEX) .EQ. -1) THEN
          BUF = 0                             !NOTHING IN LIST
          GOTO 9000
        ENDIF

        IF(LIST_INDEX .EQ. EUR_TL_MAIN) THEN
          WHICH_PARAM = EUTIMOUT
        ELSEIF (LIST_INDEX .EQ. EUR_TL_CRS) THEN
          WHICH_PARAM = EUFINTO
        ELSE
          WHICH_PARAM = EUTIMOUT
        ENDIF

C
        IF (P(WHICH_PARAM) .LT. 1) THEN 
           PASTIM = P(ACTTIM)-1                  !TIME TO CHECK FOR
        ELSE IF (P(WHICH_PARAM) .GT. 50) THEN 
           PASTIM = P(ACTTIM)-50                  !TIME TO CHECK FOR
        ELSE
           PASTIM = P(ACTTIM)-P(WHICH_PARAM)      !TIME TO CHECK FOR
        ENDIF
C
C Note that this is a FIFO queue.  If the first one on the list
C has not timed out, it is safe to assume that none of the others
C has timed out either.
C
        BUF = EURTIMERTOP(LIST_INDEX)
        IF(EURTIMERLIST(LIST_INDEX,EURTIMERBEG,BUF) .GE. PASTIM) THEN
          BUF = 0
        ELSE
          XRFNUM = EURTIMERLIST(LIST_INDEX,EURTIMERSER,BUF)
C
          EURTIMERTOP(LIST_INDEX) = EURTIMERLIST(LIST_INDEX,EURTIMERNXT,BUF)
          IF(EURTIMERTOP(LIST_INDEX) .EQ. -1)THEN
            EURTIMERBOT(LIST_INDEX) = -1
          ELSE
            EURTIMERLIST(LIST_INDEX,EURTIMERPRV,EURTIMERTOP) = -1
          ENDIF
          EURTIMERLIST(LIST_INDEX,EURTIMERNXT,BUF) = 0
          EURTIMERLIST(LIST_INDEX,EURTIMERPRV,BUF) = 0
        ENDIF
C
9000    CONTINUE
        RETURN
        END
C
C**********************************************
C
C VERIFY TIMER
C
C THIS ROUTINE WILL CHECK TO DETERMINE IF A BUFFER
C STILL EXISTS ON THE TIMER QUEUES.
C
C CALL VERTIMER(LIST_INDEX,BUF,STATUS)
C
C INPUT:
C   LIST_INDEX: LIST INDEX
C          BUF: (Int*4) Procom buffer number
C
C OUTPUT:
C       STATUS: (Int*4) 0 - Buffer exists
C                      -1 - Nothing on queue
C                      -2 - Buffer not on timer list.
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE VERTIMER(LIST_INDEX,BUF,STATUS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:EURCON.DEF'
C
        INTEGER*4 STATUS
        INTEGER*4 BUF
        INTEGER*4 LIST_INDEX
C
C IF NOTHING OF QUEUE, SET STATUS AND RETURN.
C
        STATUS=0
        IF(EURTIMERTOP(LIST_INDEX) .EQ. -1) THEN
          STATUS = -1
          GOTO 9000
        ENDIF
C
C IF THE BUFFER IS NO LONGER IN THE LINKED LIST, RETURN WITH
C AN ERROR CODE OF -2
C
        IF(EURTIMERLIST(LIST_INDEX,EURTIMERNXT,BUF) .EQ. 0 .OR.
     *     EURTIMERLIST(LIST_INDEX,EURTIMERPRV,BUF) .EQ. 0) THEN
          STATUS = -2
        ENDIF
C
9000    CONTINUE
        RETURN
        END
        
C----+-----------------------------------------------------------------
C    | SUBROUTINE RESET_TIMERS
C    |    This subroutine resets all EM timer structures. It is 
C    |    called at the start of the COMMGR program to initialize the 
C    |    structures
C----+-----------------------------------------------------------------
        SUBROUTINE RESET_TIMERS
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:EURCON.DEF'
C
C ZERO OUT THE TIMER LIST
C
        INTEGER*4 I,J,K
        DO I = 1, EUR_MAX_TIMER_LISTS
           EURTIMERTOP(I) = -1
           EURTIMERBOT(I) = -1
           DO J = 1, 4
              DO K = 0, NUMPRO + 2
                  EURTIMERLIST(I,J,K) = 0
              ENDDO
           ENDDO
        ENDDO
        RETURN
        END

