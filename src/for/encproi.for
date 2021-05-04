C  GXSRC:ENCPROI.FOR
C  
C  $Log:   GXAGBR:[GOLS]ENCPROI.FOV  $
C  
C     Rev 1.9   11 Feb 1998 19:00:22   NXA
C  Restart DES board on takeover as primary system & double wait period [RFC 2066]
C  DXA:  Check range of AGTBTB(AGTPASOFF,TER) [SP-009]
C  
C     Rev 1.8   26 Feb 1996 15:41:18   GXS
C  MODIFICATIONS FOR VOUCHERS AND LUCKY DIP.
C  
C     Rev 1.7   25 Jan 1995 17:23:42   JJOLY
C  UPDATED SUBROTINUE THAT HANDLES PASSNUMBER UPDATES FOR INSTANTS
C  
C     Rev 1.6   25 Jan 1995 17:00:52   JJOLY
C  CHECK FOR INSTANT SIGN ONS
C  
C     Rev 1.5   24 Jan 1995  9:21:48   JJOLY
C  UPDATED LOGIC THAT HANDLES RUNNING DESCHK ROUTINES
C  
C     Rev 1.4   17 Jan 1995 11:21:14   JJOLY
C  PERFORM SOFT ENC ON NON ON-LINE TERMINALS
C  
C     Rev 1.3   09 Nov 1994 17:04:00   MCM
C  CORRECTED TO HANDLE SIGNON ENCRYPTION
C  
C     Rev 1.2   29 Sep 1994 13:05:56   MCM
C  X2X Baseline release for the UK
C  
C     Rev 1.1   08 Jun 1994 14:28:22   MCM
C  CHANGED PASSNUMBER OFFSET FROM A HALFWORD TO A BYTE
C  
C     Rev 1.0   15 Feb 1994 11:05:18   JPJ
C  Initial revision.
C  
C     Rev 1.2   25 Jan 1994 15:42:56   JPJ
C  Updated procom TERNUM location from 2 bytes to 4 bytes
C  
C     Rev 1.1   03 Jan 1994 20:21:12   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 17:41:26   SYSTEM
C  Initial revision.
C
C
C 
C V13 01-FEB-21 SCML New Terminal Project (MILL-OLM)
C V12 16-MAR-03 GPW ENCSGNON (PASS NUM. CHANGED,MAXPAS=8)
C V11 15-MAR-03 GPW PASS NUMBER FROM 10 BYTE
C V10 14-MAR-03 GPW DESENCR AGTBTB->AGTHTB (PAS NUMB.),PRO(TERNUM ->HPRO(TERNUM
C V09 13-MAR-03 GPW DESENCR TAKEN FROM UK SIM_DELAYS CHANGED TO SIM_DELAY
C V07 08-MAY-01 GLS CALL SFTCHKBF REPLACED BY CHKENCPT
C V06 05-FEB-98 DXA Check range of Agtbtb(agtpasoff,ter)
C V05 14-NOV-91 TKO Change encstart to set up for DES encryption
C V04 09-MAY-91 MP  ADDED CALL TO SNIF_AND_WRKSET
C V03 03-MAY-91 TKO If clerk accounting not active (P(CLRKACT).NE.0),
C                   do not do any clerk stuff
C V02 18-MAR-90 WS INCORPORATED SIMULATION AND NEW ENCRYPTION METHOD
C                  WATCH OUT FOR COMMENTS WITH !!!, WILL CHANGE
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 20-SEP-89 LOU R.    RELEASED FOR FINLAND.
C
C
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C       the function of this task is to provide encryption/simulation
C       interface between communication (x2x) and game (dispat/logout)
C
C       ADDITIONAL ROUTINES ARE "BUILT IN" IN THIS MODULE
C       SIMCHKINP(BUFFER)                 - CHECK FOR INPUT SIMULATION BUFFER
C       SIMCHKOUT(BUFFER,ORIGINAL_BUFFER) - CHECK FOR SIMULATION RANS FROM GAME
C       ENCSGNON(BUFFER,STATUS)           - PROCESS ENCRYPTED SIGNON
C       SENDOUT(BUFFER)                   - SEND BUFFER OT X2X
C       ENCSTART                          - INITIALIZE ENCRYPTION
C
C
C       (1) initialize - call encstart
C       (2) main loop
C           check daysts
C           if daysts is suspended, wait, do nothing
C           if daysts closed exit smoothly
C           wait a while
C       (3) loop while could dequeue transaction from decryption queue:
C           - if transaction is simulated, process simulation output
C             call simchkout
C           - if transaction should be encrypted, send to encryption
C             call sftencbf
C           - if should not be encrypted send to x2x - call sendout
C       (4) loop while could dequeue buffer form input queue:
C           - if transaction should not be decrypted:
C               if simulation active check for simulation generation
C                   call simchkinp
C               add transaction to dispatcher
C           - if transaction should be decrypted, try to decrypt it
C                   call sftdecbf or desdecbf
C           - if anything dequeued and no more to dequeue, goto (3)
C       (5) loop to prcess decrypted or encrypted transaction:
C           - if encryption and not 2-nd phase of sign-on, send transaction
C             to x2x
C           - if signon
C               process signon - call encsgnon
C           - if simulation active, try to process simulation input
C                   call simchkinp
C           - send transaction to dispatcher
C           - if anything dequeued and no more to dequeue, goto (3)
C       (6) loop if internal simulation is active and transactions
C           in simulation queue:
C           - process simulation  input transactions (call simchkinp)
C           - send it to dispatcher
C           - if anything dequeued and no more to dequeue, goto (3)
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXTEND_SOURCE
        PROGRAM ENCPROI
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:ENCCOM.DEF'
        INCLUDE 'INCLIB:SIMCOM.DEF'
C
C
        INTEGER*4 STATUS, BUF_NO, ST
        INTEGER*4 MAXDELAY, DELAY
        INTEGER*4 AGAIN                         !NON 0 IF SHOULD REDO LOOP
        INTEGER*4 OUTPUT_BUF                    !OUTPUT BUFFER
        INTEGER*4 ENC_TYPE                      !CRYPTION_TYPE
        INTEGER*4 XREF_BUF                      !BUFFER INDEX
        INTEGER*4 TYPE_MASK /Z0000FF00/         !TYPE/SUBTYPE MASK
        INTEGER*4 SIGN_ON   /Z00003000/         !SIGN_ON TYPE
        INTEGER*4 SIGN_ON_I /Z0000D900/         !SIGN_ON TYPE INSTANTS
        INTEGER*4 OLDSYS                        ! OLD SYSTEM TYPE
C       INTEGER*4 VSTATUS                       ! VMS return status
C
        INTEGER*4   NOFTLSIG
        EXTERNAL    NOFTLSIG
        INTEGER*4   INIT_HARDWARE /0/
C
C
C       (1) initialize - call encstart
C
        CALL COPYRITE
C
        CALL LIB$ESTABLISH ( NOFTLSIG )         !DON'T STOP IF ERROR
C
        CALL SNIF_AND_WRKSET
C



CCCCCCC          TYPE*,'****** ENCSTART*******'



        CALL ENCSTART
C
C
        DELAY=0
        MAXDELAY=100           !EXECUTE LOOP 100 TIMES AFTER DAY CLOSED
        SIM_DELAY_RESOLUTION=100
        INIT_HARDWARE=0
        OLDSYS = P(SYSTYP)
C
C       (2) main loop
C           check daysts
C           if daysts is suspended, wait, do nothing
C           if daysts closed exit smoothly
C           wait a while
C

10      CONTINUE

C
        IF(DAYSTS.EQ.DSSUSP) THEN
          CALL XWAIT(100,1,ST)
          GOTO 10
        ENDIF
C
C
        IF (DAYSTS.EQ.DSCLOS) THEN
          DELAY=DELAY+1
          IF (DELAY.EQ.MAXDELAY) GOTO 9000
        ELSE
          DELAY=0
        ENDIF
C
C Check for a takeover
C
        IF ( OLDSYS.NE.LIVSYS .AND. P(SYSTYP).EQ.LIVSYS ) THEN
          OLDSYS = P(SYSTYP)
          IF (INIT_HARDWARE.EQ.0 .AND. P(DESFLG_TYPE).EQ.DESFLG_HARD) THEN
C
C System is now primary so break old connection and reinitialize hardware
C
            CALL DESSTOP                                ! Stop DES module
            TYPE *,IAM(),'ENCPROI  -  DES Module halted'
            CALL DESSTART(1)                            ! Re-start DES module
            INIT_HARDWARE = -1
            CALL XWAIT(P(ENCPRO_WAIT),1,ST)             ! Wait (for hardware?)
            TYPE *,IAM(),'ENCPROI  -  DES Module reinitialised'
          ENDIF
        ENDIF
C
C Make sure we keep OLDSYS up to date
C
        IF ( OLDSYS.NE.P(SYSTYP) ) OLDSYS = P(SYSTYP)
C
        CALL XWAIT(P(ENCPRO_WAIT),1,ST)                 ! Wait (for hardware?)
C
C       (3) loop while could dequeue transaction from decryption queue:
C           - if transaction is simulated, process simulation output
C             call simchkout
C           - if transaction should be encrypted, send to encryption
C             call sftencbf or call sftencbf
C           - if should not be encrypted send to x2x - call sendout
C
100     CONTINUE
C
C SKIP OUTPUT QUEUE IF 2 ENCPROS ARE RUNNING
C
        IF(P(NUMENC).EQ.2) THEN
          AGAIN=0
          GOTO 200
        ENDIF
C
C
C
        CALL RTL(OUTPUT_BUF,GAME_OUTQUE,ST)
        IF (ST.NE.GLIST_STAT_EMPTY) THEN
C
C       if this is simulated transaction queue it to simulation
C       
           XREF_BUF=SIM_XREF(OUTPUT_BUF)   !BUFFER SIMULATED IF .NON. 0
           SIM_XREF(OUTPUT_BUF)=0
           IF (XREF_BUF.NE.0) THEN
             CALL SIMCHKOUT(OUTPUT_BUF,XREF_BUF)
             IF (XREF_BUF.LT.0) GOTO 100        !IF INTERNALLY SIMULATED
           ENDIF
C
C       check if should encrypt
C       queue to encryption if should be encrypted, otherwise
C       queue to x2x. DES HARD IS ONLY DONE TO ON-LINE TERMINALS
C
          IF (IAND(PRO(OUTTAB,OUTPUT_BUF),ENCRYPTION_ON).NE.0) THEN
            IF (P(DESFLG_TYPE).EQ.DESFLG_HARD.AND.
     *         TSBIT(AGTTAB(AGTTYP,HPRO(TERNUM,OUTPUT_BUF)),AGTTOI)) THEN
D              TYPE *,IAM(),'CALLING DESENCBF'
D              CALL PRTOUT(OUTPUT_BUF)
               CALL DESENCBF(OUTPUT_BUF,ST)
            ELSE
D              TYPE *,IAM(),'CALLING SFTENCBF'
D              CALL PRTOUT(OUTPUT_BUF)
               CALL SFTENCBF(OUTPUT_BUF,ST)             !DO ENCRYPTION
            ENDIF
            IF (ST.NE.0) CALL SENDOUT(OUTPUT_BUF)       !IF ERROR
          ELSE
            CALL SENDOUT(OUTPUT_BUF) !GAME OUTPUT
          ENDIF
C
          GOTO 100
        ENDIF
C
        AGAIN=0
C
C     get decrypted buffer and queue it to dispatcher queue
C
C       (4) loop while could dequeue buffer form input queue:
C           - if transaction should not be decrypted:
C               if simulation active check for simulation generation
C                   call simchkinp
C               add transaction to dispatcher
C           - if transaction should be decrypted, try to decrypt it
C                   call sftdecbf or desdecbf
C           - if anything dequeued and no more to dequeue, goto (3)
C
C
200     CONTINUE
C
        CALL DQUINP(BUF_NO)
        IF (BUF_NO.GT.0) THEN
          AGAIN=-1
          IF (BUF_NO.GT.NUMPRO) THEN
            TYPE 900,IAM(),BUF_NO
900         FORMAT(1H ,A,' invalid buf in input ',I8)
            GOTO 200
          ENDIF
          SIM_XREF(BUF_NO)=0
          IF (IAND(PRO(INPTAB,BUF_NO),ENCRYPTION_ON).EQ.0.OR.
     *        P(SYSTYP).NE.LIVSYS.OR.
     *        HPRO(TRCODE,BUF_NO).NE.TYPREG) THEN
C
C       this buffer came unencrypted
C       check if simulation active and this is simulated transaction
C
             IF (P(SIMULATION_STATUS).EQ.SIMULATION_ON) THEN
                IF (P(SYSTYP).EQ.LIVSYS          .AND.
     *              SIM_INTERNAL_SIMULATION.EQ.0 .AND.
     *              HPRO(TRCODE,BUF_NO).EQ.TYPREG)
     *                  CALL SIMCHKINP(BUF_NO)
             ENDIF
C
C            add buffer to dispatcher queue
C
             CALL ABL(BUF_NO,QUETAB(1,DIS),STATUS)
             GOTO 200
          ELSE
C
C       try to decrypt DES HARD IS ONLY DONE TO ON-LINE TERMINALS
C
             IF (P(DESFLG_TYPE).EQ.DESFLG_HARD.AND.
     *          TSBIT(AGTTAB(AGTTYP,HPRO(TERNUM,BUF_NO)),AGTTOI)) THEN
D               TYPE *,IAM(),'CALLING DESDECBF'
D               CALL PRTOUT(BUF_NO)
                CALL DESDECBF(BUF_NO,STATUS)
             ELSE
D              TYPE *,IAM(),'CALLING SFTDECBF'     
D               CALL PRTOUT(BUF_NO)
                CALL SFTDECBF(BUF_NO,STATUS)

CCCCCC                 TYPE*,'CALLING SFTDECBF' ,STATUS

             ENDIF
D              TYPE *,IAM(),'STATUS ',ST        
             IF (STATUS.NE.0) CALL ABL(BUF_NO,QUETAB(1,DIS),STATUS) !IF ERROR
             GOTO 200
          ENDIF
        ENDIF
        IF (AGAIN.NE.0) GOTO 100
C
C       (5) loop to prcess decrypted or encrypted transaction:
C           - if encryption and not 2-nd phase of sign-on, send transaction
C             to x2x
C           - if signon
C               process signon - call encsgnon
C           - if simulation active, try to process simulation input
C                   call simchkinp
C           - send transaction to dispatcher
C           - if anything dequeued and no more to dequeue, goto (3)
C
C
210     CONTINUE
C
C WE NOW HAVE TO CHECK BOTH HARD AND SOFT
C
        IF (P(DESFLG_TYPE).EQ.DESFLG_HARD) THEN
           CALL DESCHKBF(BUF_NO,ENC_TYPE,STATUS)
        ELSE
           GOTO 2210
        ENDIF
C
        IF (STATUS.EQ.0) THEN
          IF (ENC_TYPE.EQ.ENC_TYPE_ENCRYPT .AND.
     *      HPRO(TRCODE,BUF_NO).NE.TYPPAS) THEN
C
C       encrypt message (not second phase of multipassword signon)
C
            AGAIN=-1
D           TYPE *,IAM(),'AFTER ENCRYPTION'
D           CALL PRTOUT(BUF_NO)
            CALL SENDOUT(BUF_NO)
            GOTO 210
          ELSE          !STATUS.EQ.0, DECRYPT OR TYPPAS
C
C       decryption, or second phase of sign on (encrypted back
C       message)
C
            AGAIN=-1
C
C     check if it is not sign on, check for legal password
C     and set encryption key if needed.
C
            IF(IAND(PRO(INPTAB,BUF_NO),TYPE_MASK).EQ.SIGN_ON.OR.
     *        IAND(PRO(INPTAB,BUF_NO),TYPE_MASK).EQ.SIGN_ON_I) THEN
              CALL ENCSGNON(BUF_NO,STATUS)
              IF (STATUS.NE.0) GOTO 210  !IF WENT TO DECRYPTION
C                                         OR SECOND PHASE ENCRYPTION
C                                         OR ENCRYPTION/DECRYPTION ERROR
            ENDIF
C
C       check if simulation active and this is simulated transaction
C
            IF (P(SIMULATION_STATUS).EQ.SIMULATION_ON) THEN
              IF (SIM_INTERNAL_SIMULATION.EQ.0) CALL SIMCHKINP(BUF_NO)
            ENDIF
C
C
D           TYPE *,IAM(),'AFTER DECRYPTION'
D           CALL PRTOUT(BUF_NO)
            CALL ABL(BUF_NO,QUETAB(1,DIS),STATUS)
            GOTO 210
          ENDIF
        ENDIF
C
        IF (AGAIN.NE.0) GOTO 100
C
C       (5) loop to prcess decrypted or encrypted transaction:
C           - if encryption and not 2-nd phase of sign-on, send transaction
C             to x2x
C           - if signon
C               process signon - call encsgnon
C           - if simulation active, try to process simulation input
C                   call simchkinp
C           - send transaction to dispatcher
C           - if anything dequeued and no more to dequeue, goto (3)
C
C
2210    CONTINUE
C
C WE NOW HAVE TO CHECK BOTH HARD AND SOFT
C
        CALL CHKENCPT(BUF_NO,ENC_TYPE,STATUS)         !V07
C*V07*          CALL SFTCHKBF(BUF_NO,ENC_TYPE,STATUS)
C
        IF (STATUS.EQ.0) THEN
          IF (ENC_TYPE.EQ.ENC_TYPE_ENCRYPT .AND.
     *      HPRO(TRCODE,BUF_NO).NE.TYPPAS) THEN
C
C       encrypt message (not second phase of multipassword signon)
C
            AGAIN=-1
D           TYPE *,IAM(),'AFTER ENCRYPTION'
D           CALL PRTOUT(BUF_NO)
            CALL SENDOUT(BUF_NO)
            GOTO 2210
          ELSE          !STATUS.EQ.0, DECRYPT OR TYPPAS
C
C       decryption, or second phase of sign on (encrypted back
C       message)
C
            AGAIN=-1
C
C     check if it is not sign on, check for legal password
C     and set encryption key if needed.
C
            IF(IAND(PRO(INPTAB,BUF_NO),TYPE_MASK).EQ.SIGN_ON.OR.
     *        IAND(PRO(INPTAB,BUF_NO),TYPE_MASK).EQ.SIGN_ON_I) THEN
              CALL ENCSGNON(BUF_NO,STATUS)
              IF (STATUS.NE.0) GOTO 2210  !IF WENT TO DECRYPTION
C                                         OR SECOND PHASE ENCRYPTION
C                                         OR ENCRYPTION/DECRYPTION ERROR
            ENDIF
C
C       check if simulation active and this is simulated transaction
C
            IF (P(SIMULATION_STATUS).EQ.SIMULATION_ON) THEN
              IF (SIM_INTERNAL_SIMULATION.EQ.0) CALL SIMCHKINP(BUF_NO)
            ENDIF
C
C
D           TYPE *,IAM(),'AFTER DECRYPTION'
D           CALL PRTOUT(BUF_NO)
            CALL ABL(BUF_NO,QUETAB(1,DIS),STATUS)
            GOTO 2210
          ENDIF
        ENDIF
C
        IF (AGAIN.NE.0) GOTO 100
C
C       (6) loop if internal simulation is active and transactions
C           in simulation queue:
C           - process simulation  input transactions (call simchkinp)
C           - send it to dispatcher
C           - if anything dequeued and no more to dequeue, goto (3)
C
C
230     CONTINUE
        IF (P(SIMULATION_STATUS).EQ.SIMULATION_ON) THEN
          IF (SIM_INTERNAL_SIMULATION.NE.0) THEN
              BUF_NO=-1                 !ASSUME NO INPUT BUF FOR INTERNAL
C                                       !SIMULATION
              CALL SIMCHKINP(BUF_NO)
              IF (BUF_NO.GT.0)  THEN
                CALL ABL(BUF_NO,QUETAB(1,DIS),STATUS)
                AGAIN=-1
                GOTO 230   !CHANGE IT TO GOTO 100 IF U WANT TO PLAY
C                           WITH TERMINAL RESPONSE TIME !!!!!!!
              ENDIF
          ENDIF
        ENDIF
 
        IF (AGAIN.NE.0) GOTO 100
C
        GOTO 10
C
9000    CONTINUE
        CALL GSTOP(GEXIT_SUCCESS)
        END
C
C
C
C
C *** SIMCHKINP
C
C       CHECK FOR SIMULATED INPUT TRANSACTION
C
C       CALL SIMCHKINP(BUF_NO)
C       IN:
C       BUF_NO  -   BUFFER # TO CHECK
C
C       routine will check if incoming transaction should be
C       simulated, if it should, incoming buffer will be "waited"
C       and simulation buffer will be passed through the system
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE SIMCHKINP(BUF_NO)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:SIMCOM.DEF'
C
        
        INTEGER*4 BUF_NO,SIMBUF                 !BUFFER PROCESSED
        INTEGER*4 STATUS
        INTEGER*4 TYPE_MASK /Z0000FF00/         !TYPE/SUBTYPE MASK
        INTEGER*4 LOOPBACK_TYPE /Z0000C000/     !LOOPBACK TYPE/SUBTYPE
        LOGICAL*4 LOOPBACK                      !.TRUE. IF LOOPBACK TRANS
C
C       skip if internal simulation
C
        IF (BUF_NO.LT.0) GOTO 100
C
        LOOPBACK=IAND(PRO(INPTAB,BUF_NO),TYPE_MASK)
     *                   .EQ.LOOPBACK_TYPE
C
C       replace transaction with simulated transaction if loopback
C
        IF (LOOPBACK) THEN
C
C       get transaction from the simulation queue, replace the buffer
C
                CALL RTL(SIMBUF,SIM_SIMQUE_IN,STATUS)
                IF (STATUS.NE.GLIST_STAT_EMPTY) THEN
                   SIM_XREF(SIMBUF)=BUF_NO
C
C       remember time transaction simulation started (for stats)
C       and transaction length (so terminal will send next transaction
C       of the length the same as currently processed transaction
C
                    SIM_MSG_INPUT_LEN(SIMBUF)=HPRO(INPLEN,SIMBUF)
                    SIM_DELAY(SIMBUF)=SYSTIM      !V08
                    BUF_NO=SIMBUF
                ELSE
                     SIM_PASS_LOOPBACK=SIM_PASS_LOOPBACK+1 !WANTED BUT COULD
C                                                           SIMULATE
                ENDIF
C
C       put some protection for not simulated transacations here !!!
C
        ENDIF
        RETURN
C
C       internal simulation buffer processing
C
100     CONTINUE
        CALL RTL(SIMBUF,SIM_SIMQUE_IN,STATUS)
        IF (STATUS.NE.GLIST_STAT_EMPTY) THEN
             SIM_XREF(SIMBUF)=BUF_NO
C
C       remember time transaction simulation started (for stats)
C       and transaction length (so terminal will send next transaction
C       of the length the same as currently processed transaction
C
             SIM_MSG_INPUT_LEN(SIMBUF)=HPRO(INPLEN,SIMBUF)
             SIM_DELAY(SIMBUF)=SYSTIM        !V08
             BUF_NO=SIMBUF
        ENDIF
        RETURN  
        END
C
C
C
C *** SIMCHKOUT
C
C
C       check if this is simulated transaction, if it is some stats
C       will be done, transaction will sent to simulation oputput
C       and loopback response will be sent to terminal
C
C       CALL SIMCHKOUT(OUTPUT_BUF,XREF_BUF)
C       IN:
C       OUTPUT_BUF  -   BUFFER # RECEIVED FROM GAME
C       XREF_BUF    -   ORIGINAL BUFFER AS IT CAME FROM COMM
C                       (SHOULD BE NON 0)
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE SIMCHKOUT(OUTPUT_BUF,XREF_BUF)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:SIMCOM.DEF'
C
        INTEGER*4 OUTPUT_BUF,XREF_BUF
        INTEGER*4 DELAY_OFFSET                  !OFFSET IN STATS TAB
        INTEGER*4 LOOPBACK_TYPE /Z0000C000/     !LOOPBACK TYPE/SUBTYPE
        INTEGER*4 INPUT_LEN,OUTPUT_LEN          !MESSAGE LENGTH
        INTEGER*4 TRANSACTION_TYPE
        INTEGER*4 TYPE_MASK /Z0000FF00/         !TYPE/SUBTYPE MASK
        INTEGER*4 ST
C
        INTEGER*4 LOOP_INPLEN_OFF,LOOP_OUTLEN_OFF,LOOP_DELAY_OFF !OFFSETS
C                                                           IN LOOBACK MESSAGE
        INTEGER*4 LOOP_RESPONSE_OFF             !RESPONSE OFFSET
        PARAMETER (LOOP_RESPONSE_OFF=4)
        PARAMETER (LOOP_INPLEN_OFF=9)           !LENGTH TO RETURN
        PARAMETER (LOOP_OUTLEN_OFF=10)          !LENGTH OF THIS MESSAGE
        PARAMETER (LOOP_DELAY_OFF=2)            !DELAY TO SEND NEXT MESSAGE
        INTEGER*4 LOOP_MIN_LEN                  !MINIMUM LENGTH OF LOOPBACK MSG
        PARAMETER (LOOP_MIN_LEN=10)
C
        INTEGER*4 PRO_SIM_TYPE                  !!!!SIMULATION TYPE OFFSET
C                                               !IN PROCOM BUF
        PARAMETER (PRO_SIM_TYPE=24)
C
C
C       update delay stats
C
        IF (XREF_BUF.LT.0) THEN
            SIM_DELAY(OUTPUT_BUF)=SYSTIM-SIM_DELAY(OUTPUT_BUF)   !V08
            DELAY_OFFSET=SIM_DELAY(OUTPUT_BUF)/SIM_DELAY_RESOLUTION  !V08
            IF (DELAY_OFFSET.GT.SIM_DELAY_STAT_MAX)
     *                  DELAY_OFFSET=SIM_DELAY_STAT_MAX
            IF (DELAY_OFFSET.LE.0) DELAY_OFFSET=1
            SIM_DELAY_STATS(DELAY_OFFSET)=SIM_DELAY_STATS(DELAY_OFFSET)+1
            TRANSACTION_TYPE=SIM_SIMULATED_TYPE(OUTPUT_BUF) !TRANS TYPE
            SIM_DELAY_TYPE(DELAY_OFFSET,TRANSACTION_TYPE)=
     *                SIM_DELAY_TYPE(DELAY_OFFSET,TRANSACTION_TYPE)+1
            CALL ABL(OUTPUT_BUF,SIM_SIMQUE_OUT,ST) !SIMULATION OUTPUT
C
         ELSEIF (XREF_BUF.GT.0 .AND. XREF_BUF.LE.NUMPRO) THEN
           IF (IAND(PRO(INPTAB,XREF_BUF),TYPE_MASK).EQ.LOOPBACK_TYPE) THEN
 
C
C           should do some further checks here
C           if it should process the message
C           queue transaction to output
C
            OUTPUT_LEN=HPRO(OUTLEN,OUTPUT_BUF)   !LENGTH TO SEND BACK
            IF (OUTPUT_LEN.LE.LOOP_MIN_LEN) OUTPUT_LEN=LOOP_MIN_LEN
            INPUT_LEN=SIM_MSG_INPUT_LEN(OUTPUT_BUF) !SIMULATED MSG LEN
            CALL ABL(OUTPUT_BUF,SIM_SIMQUE_OUT,ST) !SIMULATION OUTPUT
C
C       update loopback output message
C
            OUTPUT_BUF=XREF_BUF
            HPRO(OUTLEN,OUTPUT_BUF)=OUTPUT_LEN
            CALL ISBYTE(INPUT_LEN,PRO(INPTAB,OUTPUT_BUF),
     *                                          LOOP_INPLEN_OFF-1)
            CALL ISBYTE(OUTPUT_LEN,PRO(INPTAB,OUTPUT_BUF),
     *                                          LOOP_OUTLEN_OFF-1)
            CALL ISBYTE(P(LOOPDLAY),PRO(INPTAB,OUTPUT_BUF),
     *                                          LOOP_DELAY_OFF-1)
           ENDIF
        ENDIF
        RETURN
        END
C
C
C
C *** ENCSGNON
C
C       CALL ENCSGNON(BUF_NO,STATUS)
C       IN:
C       BUF_NO  -   INCOMING BUFFER
C       OUT:
C       STATUS  -   NON 0 IF ENCRYPTION OPERATION WAS DONE AND TRANSACTION
C                   WAS ALREADY QUEUED TO ENCRYPT
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE ENCSGNON(BUF_NO,STATUS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:ENCCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
C
        INTEGER*4 BUF_NO            !DECRYPTION BUFFER
        INTEGER*4 STATUS
        INTEGER*4 FIRST(NUMAGT) /NUMAGT*-1/
        INTEGER*4 CURRENT_PASS(NUMAGT) /NUMAGT*-1/
        INTEGER*4 PASS/0/, TRM
        BYTE BPASS(4)
        EQUIVALENCE (BPASS,PASS)
        INTEGER*4 ST
        INTEGER*4 AGT_MAXPASS
        PARAMETER (AGT_MAXPASS=8)   !V12 MAX NO OF PASSWORDS IN AGTCOM
        INTEGER*4   I4TEMP
        BYTE        I1TEMP(4)
        EQUIVALENCE (I4TEMP,I1TEMP)
        INTEGER*4 TYPE_MASK /Z0000FF00/         !TYPE/SUBTYPE MASK
        INTEGER*4 SIGN_ON   /Z00003000/         !SIGN_ON TYPE

        STATUS=0
        TRM=HPRO(TERNUM,BUF_NO)
C
C       get password from buffer
C
        IF(IAND(PRO(INPTAB,BUF_NO),TYPE_MASK).EQ.SIGN_ON) THEN
          CALL MOVBYTN(PRO(INPTAB,BUF_NO),12,PASS,3,2)             !11

CCCCC                  TYPE*,'PASS',PASS     !%%%GPW%%%


CCCC          I4TEMP          = 0
CCCC          I1TEMP(1) = BPASS(2)
CCCC          I1TEMP(2) = BPASS(1)
CCCC          PASS      = I4TEMP                         ! V12

             

        ELSE
          CALL MOVBYTN(PRO(INPTAB,BUF_NO),23,PASS,3,2)
          I4TEMP          = 0
          I1TEMP(1) = BPASS(2)
          I1TEMP(2) = BPASS(1)
          PASS      = I4TEMP                          ! V12        
        ENDIF
C
C****   TYPE 9000,PASS,BPASS,TRM,AGTPASOFF,APSNUM
C****9000       FORMAT(' PASS ',Z8.8,4(1X,Z2.2),3(1X,I5))
C****   TYPE 9001,AGTTAB(APSNUM+AGTHTB(AGTPASOFF,TRM)-1,TRM)
C****9001       FORMAT(' IN AGTTAB ',Z8.8)
C****   TYPE *,'PRO ',HPRO(TRCODE,BUF_NO),TYPREG, ' OFF ',
C****     *       AGTHTB(AGTPASOFF,TRM)
C
C       if password in buffer matches current password in agtcom
C
        IF ((AGTHTB(AGTPASOFF,TRM).LT.1).OR.
     +      (AGTHTB(AGTPASOFF,TRM).GT.AGT_MAXPASS)) THEN !V06
C
           AGTHTB(AGTPASOFF,TRM) = 1
C
        ENDIF
C
        IF (HPRO(TRCODE,BUF_NO).EQ.TYPREG.AND.
     *       (PASS.EQ.AGTTAB(APSNUM+AGTHTB(AGTPASOFF,TRM)-1,TRM))) THEN
C
               FIRST(TRM)=-1
               CURRENT_PASS(TRM)=-1
        ELSE
C
C             if not encrypt message back
C             we may be entering this code in 1 of 2 cases:
C               message afetr encryption comes back
C               original message (after last encryption/decryption
C                   with wrong key comes back (typpas)
C
C           in case of message coming back after decryption, we have to
C           encrypted back using the same key
C           in case of message coming back after encryption, we should
C           point to the next key, ant try to decrypt is with new key
C
C
C       try to encrypt message back if from decryption
C
              IF (HPRO(TRCODE,BUF_NO).EQ.TYPREG) THEN !IF DECRYPTED
                  HPRO(TRCODE,BUF_NO)=TYPPAS
C****             TYPE *,'Encrypting back '
                  IF (P(DESFLG_TYPE).EQ.DESFLG_HARD.AND.
     *              TSBIT(AGTTAB(AGTTYP,HPRO(TERNUM,BUF_NO)),
     *              AGTTOI)) THEN
                    CALL DESENCBF(BUF_NO,ST)
                  ELSE
                    CALL SFTENCBF(BUF_NO,ST)            !DO ENCRYPTION
                  ENDIF
C****             TYPE *,'Encrypt back status ',ST
                  IF (ST.NE.0) CALL ABL(BUF_NO,QUETAB(1,DIS),STATUS)
                  STATUS=-1
                  GOTO 100
              ENDIF
C
C       try to point to next password and decrypt it
C
C             on the first pass take password offset from agtcom
C
              IF(FIRST(TRM).EQ.-1) THEN
                IF (AGTHTB(AGTPASOFF,TRM).NE.1 .AND.
     *              AGTTAB(APSNUM+AGTHTB(AGTPASOFF,TRM)-1,TRM).EQ.0)
     *                        AGTHTB(AGTPASOFF,TRM)=1
                CURRENT_PASS(TRM)=AGTHTB(AGTPASOFF,TRM)
                FIRST(TRM)=AGTHTB(AGTPASOFF,TRM)
C****         TYPE *,'First pass current, first ',CURRENT_PASS(TRM),
C****     *                   FIRST(TRM)
              ENDIF
C
C             generate next sign on key
C
              HPRO(TRCODE,BUF_NO)=TYPREG
C
50            CONTINUE
              CURRENT_PASS(TRM)=CURRENT_PASS(TRM)+1
C****         TYPE *,'generating new password, CURRENT, PASS, ',
C****     *               CURRENT_PASS(TRM),
C****     *               AGTTAB(APSNUM+CURRENT_PASS(TRM)-1,TRM),
C****     *               ' FIRST ',FIRST(TRM),
C****     *               ' MAX ',AGT_MAXPASS
C
C       if current password is 0 (no password is set) check next
C
              IF(AGTTAB(APSNUM+CURRENT_PASS(TRM)-1,TRM).EQ.0 .AND.
     *           CURRENT_PASS(TRM).LE.AGT_MAXPASS) GOTO 50
C
C       if reached highest password offset
C
              IF (CURRENT_PASS(TRM).GT.AGT_MAXPASS)  CURRENT_PASS(TRM)=1
C
C      if checked all passwords already and non found
C
              IF (CURRENT_PASS(TRM).EQ.FIRST(TRM)) THEN
C****               TYPE *,'not found status ',STATUS
                    FIRST(TRM)=-1
                    CURRENT_PASS(TRM)=-1
                    GOTO 100
              ENDIF
C
C             and set new key in enccom
C
              CALL ENCINI1(AGTTAB(APSNUM+CURRENT_PASS(TRM)-1,TRM),TRM)
C
C       remember current password
C
              AGTHTB(AGTPASOFF,TRM)=CURRENT_PASS(TRM)
C
C       decrypt
C
              IF(P(DESFLG_TYPE).EQ.DESFLG_HARD.AND.
     *          TSBIT(AGTTAB(AGTTYP,HPRO(TERNUM,BUF_NO)),AGTTOI)) THEN
                CALL DESDECBF(BUF_NO,STATUS)
              ELSE
                CALL SFTDECBF(BUF_NO,STATUS)
              ENDIF
C
              IF (STATUS.NE.0) CALL ABL(BUF_NO,QUETAB(1,DIS),ST) !IF ERROR
              STATUS=-2
C
C     encryption key is also reinitialised in spesrv (son) and
C     in reprotra and in reset for reprocessing
C
        ENDIF
100     CONTINUE
        RETURN
        END
C
C
C
C *** SENDOUT
C
C
C       SENDOUT(BUF)    QUEUE BUFFER TO X2X, IF X2X IS NOT
C                       UP RELEASE THE BUFFER
C
C       IN:
C       BUF -   BUFFER NO
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE SENDOUT(BUF)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'     ! MXSRV
        INCLUDE 'INCLIB:TASKID.DEF'     ! MXSRV
                
        INTEGER*4 BUF
 
C        IF (X2X_GAME_STATE.EQ.X2X_GAMES_UP) THEN
CC         CALL X2ADDPRO(BUF)                        ! MXSRV
C          IF (HPRO(PRCSRC,BUF).EQ.MXS_COM) THEN     ! MXSRV
C            CALL QUETRA(MXS,BUF)                    ! MXSRV
C          ELSE                                      ! MXSRV
C            CALL X2ADDPRO(BUF)                      ! MXSRV
C          ENDIF                    
C        ELSE
C            CALL X2RELBUF(BUF)
C        ENDIF

        IF (X2X_GAME_STATE.EQ.X2X_GAMES_UP) THEN      
C         CALL X2ADDPRO(BUF)                        ! MXSRV
          IF (HPRO(PRCSRC,BUF).EQ.OLM_COM) THEN ! V13 - OLM            
C            CALL QUETRA(OLM,BUF)                    ! V13 - OLM (rever se deve usar o ABL ou é QUETRA)   
             CALL OLM_QUETRA(BUF) 
C V13          IF (HPRO(PRCSRC,BUF).EQ.MXS_COM) THEN     ! MXSRV            
          ELSEIF (HPRO(PRCSRC,BUF).EQ.MXS_COM) THEN     ! MXSRV
            CALL QUETRA(MXS,BUF)                    ! MXSRV
          ELSE                                      ! MXSRV
            CALL X2ADDPRO(BUF)                      ! MXSRV
          ENDIF                                     ! MXSRV
        ELSE
            CALL X2RELBUF(BUF)
        ENDIF
 
        RETURN
        END
C
C
C
C *** ENCSTART
C
C       initialize encryption data structures
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE ENCSTART
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:ENCCOM.DEF'
        INCLUDE 'INCLIB:SIMCOM.DEF'
C
C
C       initialize all encryption queues
C
        CALL ENCINI(1,NUMAGT)               !INITIALIZE QUEUES
        CALL DEFLST(SOFT_ENCQUE,NUMPRO)     !SOFT ENCRYPTION QUEUE
        CALL DEFLST(SIM_SIMQUE_IN,NUMPRO)   !INPUT SIMULATION QUEUE
        CALL FASTSET(0,SIM_XREF,NUMPRO)     !SIM XREF TABLE
        IF(P(NUMENC).NE.2) THEN
          CALL DEFLST(SIM_SIMQUE_OUT,NUMPRO)  !OUTPUT SIMULATION QUEUE
          CALL DEFLST(GAME_OUTQUE,NUMPRO)
        ENDIF
C
        IF (P(DESFLG_TYPE).EQ.DESFLG_HARD) THEN !If DES on this system
          CALL DESSTART(1)                      !start up DES module
        ENDIF
C
        IF (P(ENCPRO_WAIT).EQ.0) P(ENCPRO_WAIT)=ENCPRO_WAIT_DEFAULT
C
        RETURN
        END
