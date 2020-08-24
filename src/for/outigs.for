C-----------------------------------------------------------------------
C PROGRAM OUTIGS
C-----------------------------------------------------------------------
C OUTIGS.FOR
C
C V01 2014-FEB-10 SCML PLACARD PROJECT - IGS - Creation
C
C The process OUTIGS is responsible for creating the response message to
C the corresponding ALTURA terminal and to send to the logger to record 
C the message in the TMF.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2014 SCML/Accenture. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM OUTIGS
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:APUCOM.DEF'
        INCLUDE 'INCLIB:IGSDEBUG.DEF'
        
        INTEGER*4  MESS(EDLEN),WRKBUF(TRALEN)       !
        INTEGER*4  TASK              !
        INTEGER*4  BUF               !
        INTEGER*4  STATUS,RETRYIND
        INTEGER*4  MES_LEN,I,IND,TER,RETRY,OPTIONS,LEN
C        INTEGER*4 SIZEOFRBUF
        INTEGER*4  I4TEMP,TEMP1,TEMP2
        INTEGER*2  I2TEMP(2)
        BYTE       I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
        BYTE ERRORNUM 
        INTEGER*4 MYCHKSUM
        INTEGER*4 MTYPE,SUBTYP

        INTEGER*4 MES_IND

        INTEGER*8 I8AUX_AMOUNT
        INTEGER*4 I4AUX_AMOUNT(2)
        EQUIVALENCE(I8AUX_AMOUNT,I4AUX_AMOUNT)
        
        INTEGER*4 I4AUX(19)

        CALL OPSTXT(' Copyright 2014 SCML. All rights reserved. ')
        CALL SNIF_AND_WRKSET

        TASK    = IGO

        MESS(1) = TASK

        CALL OPSTXT(' ******************* OUTIGS ******************* ')
C----+------------------------------------------------------------------
C    | Entry Point: wait for something to do
C----+------------------------------------------------------------------
10      CONTINUE
C----+------------------------------------------------------------------
C    | If day close then send stop... 
C----+------------------------------------------------------------------
        IF(DAYSTS .EQ. DSCLOS) THEN
            CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
C----+------------------------------------------------------------------
C    | If day is suspended then stay on hold 
C----+------------------------------------------------------------------
        IF(DAYSTS .EQ. DSSUSP) THEN
            CALL HOLD(0,STATUS)
            IF(DAYSTS .EQ. DSOPEN) GOTO 10
            GOTO 10
        ENDIF
        
        CALL HOLD(0,STATUS)
C----+------------------------------------------------------------------
C    | Get buffer number from queue top. If there are no wagers queued,
C    | then go back to WAIT state 
C----+------------------------------------------------------------------
20      CONTINUE
        CALL TOPQUE(TASK,BUF)
        IF(BUF .EQ. 0) THEN
            GOTO 10
        ELSE
            IF(IGSDEBUG(IA_OUTIGS)) THEN
                CALL OPS('TASK',TASK,TASK)
                CALL OPS('BUF',BUF,BUF)
                CALL DUMP_MESSAGE(0,0,BPRO(BINPTAB,BUF),HPRO(INPLEN,BUF))
                CALL DUMP_MESSAGE(0,0,BPRO(WRKTAB*4-3+1,BUF),HPRO(OUTLEN,BUF))
            ENDIF
        ENDIF
C----+------------------------------------------------------------------
C    | Check if COMIGS and INIGS are running. If not, start them.
C----+------------------------------------------------------------------
        CALL CHECKPROCESS()
        
C----+------------------------------------------------------------------
C    | Get TRABUF out of PROCOM buffer
C----+------------------------------------------------------------------
        CALL FASTSET(0,TRABUF,TRALEN)
        CALL LOGTRA(TRABUF,APUBUF(2,BUF))
C----+------------------------------------------------------------------
C    | Set up TRABUF
C----+------------------------------------------------------------------
        TER           = HPRO(TERNUM,BUF)
        TRABUF(TSER)  = PRO(SERIAL,BUF)
        TRABUF(TTIM)  = PRO(TSTAMP,BUF)
        TRABUF(TSIZE) = HPRO(NUMLRC,BUF)
        TRABUF(TCDC)  = DAYCDC     
C----+------------------------------------------------------------------
C    | 1) Process suppressions
C----+------------------------------------------------------------------
        IF (TRABUF(TERR) .EQ. SUPR) THEN

          CALL SAVE_TRABUF_FIELDS(I4AUX,TRABUF)

          CALL OIGSSUPR(WRKBUF,TRABUF,MESS,MES_LEN,BUF,TER,STATUS)

          CALL LOAD_TRABUF_FIELDS(I4AUX,TRABUF)

          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPSTXT('AFTER OIGSSUPR')
              CALL OPS('BUF',BUF,BUF)
              CALL OPS('STATUS',STATUS,STATUS)
          ENDIF
          GOTO 2000
        ENDIF

C----+------------------------------------------------------------------
C    | 2) Process invalidities
C----+------------------------------------------------------------------
        IF (TRABUF(TERR) .EQ. INVL) THEN

          CALL SAVE_TRABUF_FIELDS(I4AUX,TRABUF)

          CALL OIGSINVL(WRKBUF,TRABUF,MESS,MES_LEN,BUF,TER,STATUS)

          CALL LOAD_TRABUF_FIELDS(I4AUX,TRABUF)

          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPSTXT('AFTER OIGSINVL')
              CALL OPS('BUF',BUF,BUF)
              CALL OPS('STATUS',STATUS,STATUS)
              CALL OPS('TRABUF(TSDT1)',TRABUF(TSDT1),TRABUF(TSDT1))
              CALL OPS('TRABUF(TSDT2)',TRABUF(TSDT2),TRABUF(TSDT2))
          ENDIF
          GOTO 2000
        ENDIF

C----+------------------------------------------------------------------
C    | 3) Process syntax errors
C----+------------------------------------------------------------------
        IF (TRABUF(TERR) .EQ. SYNT) THEN

          CALL SAVE_TRABUF_FIELDS(I4AUX,TRABUF)

          CALL OIGSSYNT(WRKBUF,TRABUF,MESS,MES_LEN,BUF,TER,STATUS)

          CALL LOAD_TRABUF_FIELDS(I4AUX,TRABUF)

          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPSTXT('AFTER OIGSSYNT')
              CALL OPS('BUF',BUF,BUF)
              CALL OPS('STATUS',STATUS,STATUS)
          ENDIF
          GOTO 2000
        ENDIF

C----+------------------------------------------------------------------
C    | 4) Process not sign on errors
C----+------------------------------------------------------------------
        IF (TRABUF(TERR) .EQ. NOTON) THEN

          CALL SAVE_TRABUF_FIELDS(I4AUX,TRABUF)

          CALL OIGSNOTON(WRKBUF,TRABUF,MESS,MES_LEN,BUF,TER,STATUS)

          CALL LOAD_TRABUF_FIELDS(I4AUX,TRABUF)

          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPSTXT('AFTER OIGSNOTON')
              CALL OPS('BUF',BUF,BUF)
              CALL OPS('STATUS',STATUS,STATUS)
          ENDIF
          GOTO 2000
        ENDIF

C----+------------------------------------------------------------------
C    | 5) Process timed-out messages 
C----+------------------------------------------------------------------
        IF ((HPRO(REMSTS,BUF) .EQ. RMTMOT) .OR. (TRABUF(TERR) .EQ. BCRS)) THEN

          CALL SAVE_TRABUF_FIELDS(I4AUX,TRABUF)

          CALL OIGSTOUT(WRKBUF,TRABUF,MESS,MES_LEN,BUF,TER,STATUS)

          CALL LOAD_TRABUF_FIELDS(I4AUX,TRABUF)

          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPSTXT('AFTER OIGSTOUT')
              CALL OPS('BUF',BUF,BUF)
              CALL OPS('STATUS',STATUS,STATUS)
          ENDIF
          IF(STATUS .EQ. -1) THEN
            GOTO 4321
          ENDIF
          GOTO 2000
        ENDIF 

C----+------------------------------------------------------------------
C    | 6) Process errors while trying to put message into MessageQ, and
C    |    create output message to Altura (Error message) 
C----+------------------------------------------------------------------
        IF ((HPRO(REMSTS,BUF) .EQ. RMDOWN) .OR. (TRABUF(TERR) .EQ. SDOR)) THEN

          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPSTXT('BEFORE OIGSDOWN')
              CALL OPS('BUF',BUF,BUF)
              CALL OPS('HPRO(REMSTS,BUF)',ZEXT(HPRO(REMSTS,BUF)),ZEXT(HPRO(REMSTS,BUF)))
              CALL OPS('TRABUF(TERR)',ZEXT(TRABUF(TERR)),ZEXT(TRABUF(TERR)))
          ENDIF

          CALL SAVE_TRABUF_FIELDS(I4AUX,TRABUF)

          CALL OIGSDOWN(WRKBUF,TRABUF,MESS,MES_LEN,BUF,TER,STATUS)

          CALL LOAD_TRABUF_FIELDS(I4AUX,TRABUF)

          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPSTXT('AFTER OIGSDOWN')
              CALL OPS('BUF',BUF,BUF)
              CALL OPS('STATUS',STATUS,STATUS)
          ENDIF
          IF(STATUS .EQ. -1) THEN
            GOTO 4321
          ENDIF
          GOTO 2000
        ENDIF
        
4321    CONTINUE
C----+------------------------------------------------------------------
C    | 7) Process wagers 
C----+------------------------------------------------------------------
        IF (TRABUF(TTYP) .EQ. TIGS .AND. TRABUF(TIGS_TTYP) .EQ. IGSWAG) THEN
          CALL OIGSTWAG(WRKBUF,TRABUF,MESS,MES_LEN,BUF,TER,STATUS)
          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPSTXT('AFTER OIGSTWAG')
              CALL OPS('TER  ',TER,TER)
              CALL OPS('AGTTAB(ALSTRA,TER)',AGTTAB(ALSTRA,TER),AGTTAB(ALSTRA,TER))
              CALL OPS('AGTTAB(ALSWAG,TER)',AGTTAB(ALSWAG,TER),AGTTAB(ALSWAG,TER))
              CALL OPS('AGTTAB(ALSCAN,TER)',AGTTAB(ALSCAN,TER),AGTTAB(ALSCAN,TER))
              CALL OPS('AGTTAB(ALSVAL,TER)',AGTTAB(ALSVAL,TER),AGTTAB(ALSVAL,TER))
          ENDIF
C----+------------------------------------------------------------------
C    | 8) Process cancellations
C----+------------------------------------------------------------------
        ELSE IF (TRABUF(TTYP) .EQ. TIGS .AND. TRABUF(TIGS_TTYP) .EQ. IGSCAN) THEN
          CALL OIGSTCAN(WRKBUF,TRABUF,MESS,MES_LEN,BUF,TER,STATUS)
          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPSTXT('AFTER OIGSTCAN')
              CALL OPS('TER  ',TER,TER)
              CALL OPS('AGTTAB(ALSTRA,TER)',AGTTAB(ALSTRA,TER),AGTTAB(ALSTRA,TER))
              CALL OPS('AGTTAB(ALSWAG,TER)',AGTTAB(ALSWAG,TER),AGTTAB(ALSWAG,TER))
              CALL OPS('AGTTAB(ALSCAN,TER)',AGTTAB(ALSCAN,TER),AGTTAB(ALSCAN,TER))
              CALL OPS('AGTTAB(ALSVAL,TER)',AGTTAB(ALSVAL,TER),AGTTAB(ALSVAL,TER))
          ENDIF
C----+------------------------------------------------------------------
C    | 9) Process validations
C----+------------------------------------------------------------------
        ELSE IF (TRABUF(TTYP) .EQ. TIGS .AND. TRABUF(TIGS_TTYP) .EQ. IGSVAL) THEN
          CALL OIGSTVAL(WRKBUF,TRABUF,MESS,MES_LEN,BUF,TER,STATUS)
          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPSTXT('AFTER OIGSTVAL')
              CALL OPS('TER  ',TER,TER)
              CALL OPS('AGTTAB(ALSTRA,TER)',AGTTAB(ALSTRA,TER),AGTTAB(ALSTRA,TER))
              CALL OPS('AGTTAB(ALSWAG,TER)',AGTTAB(ALSWAG,TER),AGTTAB(ALSWAG,TER))
              CALL OPS('AGTTAB(ALSCAN,TER)',AGTTAB(ALSCAN,TER),AGTTAB(ALSCAN,TER))
              CALL OPS('AGTTAB(ALSVAL,TER)',AGTTAB(ALSVAL,TER),AGTTAB(ALSVAL,TER))
          ENDIF
C----+------------------------------------------------------------------
C    |10) Process payments
C----+------------------------------------------------------------------
        ELSE IF (TRABUF(TTYP) .EQ. TIGS .AND. TRABUF(TIGS_TTYP) .EQ. IGSPAY) THEN
          CALL OIGSTPAY(WRKBUF,TRABUF,MESS,MES_LEN,BUF,TER,STATUS)
          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPSTXT('AFTER OIGSTPAY')
              CALL OPS('TER  ',TER,TER)
              CALL OPS('AGTTAB(ALSTRA,TER)',AGTTAB(ALSTRA,TER),AGTTAB(ALSTRA,TER))
              CALL OPS('AGTTAB(ALSWAG,TER)',AGTTAB(ALSWAG,TER),AGTTAB(ALSWAG,TER))
              CALL OPS('AGTTAB(ALSCAN,TER)',AGTTAB(ALSCAN,TER),AGTTAB(ALSCAN,TER))
              CALL OPS('AGTTAB(ALSVAL,TER)',AGTTAB(ALSVAL,TER),AGTTAB(ALSVAL,TER))
          ENDIF
C----+------------------------------------------------------------------
C    |11) Process game programme reports
C----+------------------------------------------------------------------
        ELSE IF (TRABUF(TTYP) .EQ. TIGS .AND. TRABUF(TIGS_TTYP) .EQ. IGSREP) THEN
          CALL OIGSTREP(WRKBUF,TRABUF,MESS,MES_LEN,BUF,TER,STATUS)
          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPSTXT('AFTER OIGSTREP')
              CALL OPS('TER  ',TER,TER)
              CALL OPS('AGTTAB(ALSTRA,TER)',AGTTAB(ALSTRA,TER),AGTTAB(ALSTRA,TER))
              CALL OPS('AGTTAB(ALSWAG,TER)',AGTTAB(ALSWAG,TER),AGTTAB(ALSWAG,TER))
              CALL OPS('AGTTAB(ALSCAN,TER)',AGTTAB(ALSCAN,TER),AGTTAB(ALSCAN,TER))
              CALL OPS('AGTTAB(ALSVAL,TER)',AGTTAB(ALSVAL,TER),AGTTAB(ALSVAL,TER))
          ENDIF
C----+------------------------------------------------------------------
C    |12) Process special functions
C    |    NOTE: there is also some special functions processing in 
C    |    the following steps:
C    |        3) Process timed-out messages
C    |        4) Process errors while trying to put message into Message
C----+------------------------------------------------------------------
        ELSE IF (TRABUF(TTYP) .EQ. TSPE) THEN
          CALL OIGSTSPE(WRKBUF,TRABUF,MESS,MES_LEN,BUF,TER,STATUS)
          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPSTXT('AFTER OIGSTSPE')
              CALL OPS('TER  ',TER,TER)
              CALL OPS('AGTTAB(ALSTRA,TER)',AGTTAB(ALSTRA,TER),AGTTAB(ALSTRA,TER))
              CALL OPS('AGTTAB(ALSWAG,TER)',AGTTAB(ALSWAG,TER),AGTTAB(ALSWAG,TER))
              CALL OPS('AGTTAB(ALSCAN,TER)',AGTTAB(ALSCAN,TER),AGTTAB(ALSCAN,TER))
              CALL OPS('AGTTAB(ALSVAL,TER)',AGTTAB(ALSVAL,TER),AGTTAB(ALSVAL,TER))
          ENDIF
C----+------------------------------------------------------------------
C    |13) Process other transactions (error)
C----+------------------------------------------------------------------
        ELSE
          MTYPE  =     ZEXT(BPRO(INPTAB + 1,BUF))/ 16
          SUBTYP = MOD(ZEXT(BPRO(INPTAB + 1,BUF)), 16)
          
          CALL OIGSTERR(WRKBUF, TRABUF, PRO(OUTTAB,BUF), MES_LEN
     *                  , BUF, TER, STATUS
     *                  , TRABUF(TGAMTYP)
     *                  , TRABUF(TGAMIND)
     *                  , MTYPE
     *                  , SUBTYP
     *                  , IGS_MILSYS
     *                  , 'MIL-00002' ! Syntax error
     *                  , 'Erro de sintaxe'
     *                  , 0 ) ! Don't use XREF from WRKTAB
          IF(IGSDEBUG(IA_OUTIGS)) THEN
              CALL OPSTXT('AFTER OIGSTERR')
              CALL OPS('TER  ',TER,TER)
              CALL OPS('AGTTAB(ALSTRA,TER)',AGTTAB(ALSTRA,TER),AGTTAB(ALSTRA,TER))
              CALL OPS('AGTTAB(ALSWAG,TER)',AGTTAB(ALSWAG,TER),AGTTAB(ALSWAG,TER))
              CALL OPS('AGTTAB(ALSCAN,TER)',AGTTAB(ALSCAN,TER),AGTTAB(ALSCAN,TER))
              CALL OPS('AGTTAB(ALSVAL,TER)',AGTTAB(ALSVAL,TER),AGTTAB(ALSVAL,TER))
          ENDIF
        ENDIF
2000    CONTINUE
C----+------------------------------------------------------------------
C    | Put good transaction into TRABUF
C----+------------------------------------------------------------------
        AGTHTB(ATRNUM,TER) = TRABUF(TTRN)

        IF(IGSDEBUG(IA_OUTIGS)) THEN
            CALL OPSTXT('BEFORE CALC CHKSUM')
            CALL OPS('TRABUF(TCHK)',TRABUF(TCHK),TRABUF(TCHK))
            CALL OPS('BUF',BUF,BUF)
            CALL OPS('HPRO(OUTLEN,BUF)',HPRO(OUTLEN,BUF),HPRO(OUTLEN,BUF))
            CALL DUMP_MESSAGE(292,BUF,BPRO(BOUTTAB,BUF),HPRO(OUTLEN,BUF))
        ENDIF
        CALL CALCULATE_MSG_CHECKSUM_W_SEED(TRABUF(TCHK),3,BPRO(BOUTTAB,BUF),HPRO(OUTLEN,BUF))
        IF(IGSDEBUG(IA_OUTIGS)) THEN
            CALL OPSTXT('AFTER CALC CHKSUM')
            CALL DUMP_MESSAGE(297,BUF,BPRO(BOUTTAB,BUF),HPRO(OUTLEN,BUF))
        ENDIF

        CALL TRALOG(TRABUF,PRO(WRKTAB,BUF))
        HPRO(TRCODE,BUF) = TYPREG

        CALL QUETRA(APU, BUF)
        CALL DQUTRA(TASK,BUF)
        GOTO 20
9998    FORMAT(' OUTIGS SPEC: ',I3.1,' - ', Z3.2)         
        END
        
C----+-----------------------------------------------------------------
C    | SUBROUTINE CHECKPROCESS
C    |    This subroutine checks the status of the INIGS and COMIGS 
C    |    processes. If they are down, restarts them.
C----+-----------------------------------------------------------------
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CHECKPROCESS()
        IMPLICIT NONE
C**************************************************
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        
        INTEGER*4 TSKSTS,STATUS
C----+------------------------------------------------------------------
C    | If COMIGS is not running, then restart COMIGS
C----+------------------------------------------------------------------
        STATUS = 0 
        CALL STTSK(8HCOMIGS  ,TSKSTS,STATUS) !VERIFY IF COMIGS IS OK
        IF (STATUS .EQ. 4) THEN
           CALL OPSTXT('ERROR!!!!! COMIGS IS NOT RUNNING')
           CALL OPSTXT('STARTING COMIGS AND TRY TO PROCESS ALL TRANSACTIONS ')
           CALL START(8HCOMIGS  )
        ENDIF
C----+------------------------------------------------------------------
C    | If INIGS is not running, then restart INIGS
C----+------------------------------------------------------------------
        STATUS = 0 
        CALL STTSK(8HINIGS   ,TSKSTS,STATUS) ! VERIFY IF INIGS IS OK
        IF (STATUS .EQ. 4) THEN
           CALL OPSTXT('ERROR!!!!! INIGS IS NOT RUNNING')
           CALL OPSTXT('STARTING INIGS AND TRY TO PROCESS ALL TRANSACTIONS ')
           CALL START(8HINIGS   )
        ENDIF
        END

C----+-----------------------------------------------------------------
C    | SUBROUTINE PUTIME
C    |    This subroutine puts time into messages.
C----+-----------------------------------------------------------------
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PUTIME(TIME,OUTTAB,IND)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        BYTE      OUTTAB(*)                     !Output Message table.

        INTEGER*4 IND                           !Index into Output Table
        INTEGER*4 TIME                          !Time in ms since midnight
        INTEGER*4 HOURS
        INTEGER*4 MINS
        INTEGER*4 SECS

        BYTE      I1TEMP(4)
        INTEGER*4 TEMP
        EQUIVALENCE(TEMP,I1TEMP)

        LOGICAL   HH_MM_SS_FLAG/.TRUE./         !Indicate what format.

        IF(HH_MM_SS_FLAG) THEN
           HOURS = TIME/3600
           MINS = (TIME-HOURS*3600) / 60
           SECS = TIME - HOURS*3600 - MINS*60

           OUTTAB(IND+0) = HOURS
           OUTTAB(IND+1) = MINS
           OUTTAB(IND+2) = SECS
        ELSE
           TEMP = TIME
           OUTTAB(IND+0) = I1TEMP(3)
           OUTTAB(IND+1) = I1TEMP(2)
           OUTTAB(IND+2) = I1TEMP(1)
        ENDIF
        IND = IND + 3

        RETURN
        END

C----+-----------------------------------------------------------------
C    | SUBROUTINE CLEARBUF
C    |    This subroutine clears a buffer.
C----+-----------------------------------------------------------------
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CLEARBUF(VALUE,OUARY,LEN)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'

        INTEGER*4   VALUE
        INTEGER*4   OUARY(*)
        INTEGER*4   LEN

        INTEGER*4   XLEN
        INTEGER*4   K

        IF(VALUE.NE.0) THEN
          DO 1100 K = 1, LEN
            OUARY(K) = VALUE
1100      CONTINUE
          GOTO 9000
        ENDIF
C----+------------------------------------------------------------------
C    | Special case for 0 value (most common)
C----+------------------------------------------------------------------
        XLEN = LEN
        K = 1
2000    CONTINUE
        IF(XLEN.GT.64000)THEN
          CALL LIB$MOVC5(1,0,0,64000,OUARY(K))
          XLEN = XLEN-64000
          K    = K + 16000
          GOTO 2000
        ENDIF

        IF(XLEN.GT.0)THEN
          CALL LIB$MOVC5(1,0,0,XLEN,OUARY(K))
        ENDIF

9000    CONTINUE
        RETURN
        END

        
        
        
        
        
        
        





        SUBROUTINE DUMP_BUF(BUF,COMMENT)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'

        INTEGER*4 BUF
        CHARACTER*255 LINE
        CHARACTER*(*) COMMENT
1001    FORMAT('(',I5,'):',A,' # ')
1002    FORMAT('(',I5,'):',A,I10,' ',Z8)
        WRITE(LINE, 1001) BUF, '--------------------------------------------------'
        CALL OPSTXT(TRIM(LINE))
        WRITE(LINE, 1001) BUF, TRIM(COMMENT)
        CALL OPSTXT(TRIM(LINE))
        WRITE(LINE, 1001) BUF, '..................................................'
        CALL OPSTXT(TRIM(LINE))
        WRITE(LINE, 1002) BUF, 'DUMP_BUF:',BUF, BUF
        CALL OPSTXT(TRIM(LINE))
        WRITE(LINE, 1002) BUF, ' HPRO (REMSTS,BUF):',ZEXT(HPRO(REMSTS,BUF)), ZEXT(HPRO(REMSTS,BUF))
        CALL OPSTXT(TRIM(LINE))
        WRITE(LINE, 1002) BUF, '  HPRO(TRCODE,BUF):',ZEXT(HPRO(TRCODE,BUF)), ZEXT(HPRO(TRCODE,BUF))
        CALL OPSTXT(TRIM(LINE))
        WRITE(LINE, 1001) BUF, '== DUMP INPTAB =='
        CALL OPSTXT(TRIM(LINE))
        CALL DUMP_MESSAGE(-1,BUF,BPRO(BINPTAB,BUF),HPRO(INPLEN,BUF))
        WRITE(LINE, 1001) BUF, '== DUMP WRKTAB =='
        CALL OPSTXT(TRIM(LINE))
        CALL DUMP_MESSAGE(-1,BUF,BPRO(WRKTAB*4-3+1,BUF),HPRO(INPLEN,BUF)+14)
        WRITE(LINE, 1001) BUF, '--------------------------------------------------'
        CALL OPSTXT(TRIM(LINE))
        
        
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
        INTEGER*4 I, J, K, DIV, REMAIN, OFFSET, MAXSIZE
        
        PARAMETER(MAXSIZE = 640)
        
        DO I = 1, 255
            BUF(I:I) = CHAR(0)
        ENDDO
        
        WRITE(BUF, 900) LINE_ID, MESSAGE_ID, LINE_ID, MESLEN
        TYPE *, IAM(), '', TRIM(BUF)
        CALL OPSTXT(TRIM(BUF))
        
        IF(MESLEN .GT. MAXSIZE) THEN
903        FORMAT('(',I5,'):MESSAGE SIZE IS TOO BIG: ', I
     *           , ' - ONLY THE FIRST ', I, ' BYTES WILL BE DISPLAYED')
           WRITE(BUF,903) LINE_ID, MESLEN, MAXSIZE
           CALL OPSTXT(TRIM(BUF))
           MESLEN = MAXSIZE
        ENDIF
        
        DIV = MESLEN / 16
        REMAIN = MOD(MESLEN,16)
        

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
           WRITE(BUF, 902) LINE_ID, OFFSET + 1, OFFSET + 16,( ARR(I), I = 1, 16)
           TYPE *, '', TRIM(BUF)
           CALL OPSTXT(TRIM(BUF))
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
           WRITE(BUF, 902) LINE_ID, OFFSET + 1, OFFSET + REMAIN, (ARR(I), I = 1, 16)
           TYPE *, '', TRIM(BUF)
           CALL OPSTXT(TRIM(BUF))
        ENDIF
        TYPE *, ''

900     FORMAT('(',I5,'):PARSED MESSAGE #',I8,' (@ LINE #',I8,') : LEN = ', I8)
901     FORMAT(Z2.2)
902     FORMAT('(',I5,'):[',I4,':',I4,'] = ',16(A2,1X))

        RETURN
        END

        
        SUBROUTINE CALCULATE_MSG_CHECKSUM_W_SEED(SEED,MSG_OFFSET,OUTBUF,OUTLEN)
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
        
        INTEGER*4 MYCHKSUM, CHKLEN
        INTEGER*4 TER, MSG_OFFSET,SEED
        
        CHARACTER*255 LINE
        
        TER = 0
        BASECHKSUM = SEED
        I4CCITT   = IAND(BASECHKSUM+TER,'FFFF'X)
        OUTBUF(MSG_OFFSET + 0) = I1CCITT(2)
        OUTBUF(MSG_OFFSET + 1) = I1CCITT(1)

        I4CCITT = 0
        I1CCITT(2) = OUTBUF(MSG_OFFSET + 0)
        I1CCITT(1) = OUTBUF(MSG_OFFSET + 1)
        
        CHKLEN=OUTLEN-1
        CALL GETCCITT(OUTBUF,1,CHKLEN,MYCHKSUM)
        I4CCITT = MYCHKSUM
        OUTBUF(MSG_OFFSET + 0) = I1CCITT(2)
        OUTBUF(MSG_OFFSET + 1) = I1CCITT(1)

        I4CCITT = 0
        I1CCITT(2) = OUTBUF(MSG_OFFSET + 0)
        I1CCITT(1) = OUTBUF(MSG_OFFSET + 1)
        
        RETURN
        END
        
        
        SUBROUTINE SAVE_TRABUF_FIELDS(I4AUX, TRABUF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'

        INCLUDE 'INCLIB:IGSDEBUG.DEF'
        
        INTEGER*4 I4AUX(19)
        
        IF(   TRABUF(TTYP)  .EQ. TSPE
     *  .AND. TRABUF(TSFUN) .EQ. TREPR) THEN
            I4AUX(01) = TRABUF(TSFUN )
            I4AUX(02) = TRABUF(TSOLD )
            I4AUX(03) = TRABUF(TSNEW )
            I4AUX(04) = TRABUF(TSSGN )
            I4AUX(05) = TRABUF(TSDT1 )
            I4AUX(06) = TRABUF(TSDT2 )
            I4AUX(07) = TRABUF(TSDT3 )
            I4AUX(08) = TRABUF(TSDT4 )
            I4AUX(09) = TRABUF(TSDT5 )
            I4AUX(10) = TRABUF(TSDT6 )
            I4AUX(11) = TRABUF(TSDT7 )
            I4AUX(12) = TRABUF(TSDT8 )
            I4AUX(13) = TRABUF(TSDT9 )
            I4AUX(14) = TRABUF(TSDT10)
            I4AUX(15) = TRABUF(TSDT11)
            I4AUX(16) = TRABUF(TSDT12)
            I4AUX(17) = TRABUF(TSDT13)
            I4AUX(18) = TRABUF(TSDT14)
            I4AUX(19) = TRABUF(TSDT15)
        ENDIF
        
        RETURN
        END
        
        
        
        SUBROUTINE LOAD_TRABUF_FIELDS(I4AUX, TRABUF)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'

        INCLUDE 'INCLIB:IGSDEBUG.DEF'
        
        INTEGER*4 I4AUX(19)
        
        IF(   TRABUF(TTYP)  .EQ. TSPE
     *  .AND. TRABUF(TSFUN) .EQ. TREPR) THEN
            TRABUF(TSDT4 ) = TRABUF(TIGS_SERR) !I4AUX(08)
            TRABUF(TSDT5 ) = TRABUF(TIGS_XERR) !I4AUX(09)
            TRABUF(TSDT6 ) = TRABUF(TIGS_XERR + 1)!I4AUX(10)
            TRABUF(TSDT7 ) = TRABUF(TIGS_XERR + 2)!I4AUX(11)
            TRABUF(TSFUN ) = I4AUX(01)
            TRABUF(TSOLD ) = I4AUX(02)
            TRABUF(TSNEW ) = I4AUX(03)
            TRABUF(TSSGN ) = I4AUX(04)
            TRABUF(TSDT1 ) = I4AUX(05)
            TRABUF(TSDT2 ) = I4AUX(06)
            TRABUF(TSDT3 ) = I4AUX(07)
            TRABUF(TSDT8 ) = I4AUX(12)
            TRABUF(TSDT9 ) = I4AUX(13)
            TRABUF(TSDT10) = I4AUX(14)
            TRABUF(TSDT11) = I4AUX(15)
            TRABUF(TSDT12) = I4AUX(16)
            TRABUF(TSDT13) = I4AUX(17)
            TRABUF(TSDT14) = I4AUX(18)
            TRABUF(TSDT15) = I4AUX(19)
        ENDIF
        
        RETURN
        END
