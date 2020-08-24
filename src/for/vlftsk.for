C PROGRAM VLFTSK
C
C V10 20-OCT-2000 UXN Alpha baseline release. 
C V09 11-APR-2000 UXN UPDTSK_RUN_AGAIN added.
C V08 07-MAR-2000 OXK POOLBLD added.
C V07 16-JAN-2000 UXN DISPWIN added.
C V06 31-AUG-1999 RXK Fix for INVOICE_DAY checking.
C V05 27-AUG-1999 RXK FTRANS is back, run it on invoice day after INVCLC is done
C                     (FBNKWN and PAYUPD which are here have to be done as well)
C V04 10-AUG-1999 UXN Don't run FTRANS on invoice day, because on invoice
C                     day it has to run after INVCLC (started from STSYSTEM).
C V03 25-MAY-1999 RXK Do not allow run if manual stopmod. set statuses "started"
C                     and "ready for balans" for STSYSTEM.
C V02 30-APR-1999 RXK Call of ISSUBPROC, BIGWAF and WINXFR added,
C                     check that MULTIVLF has been done. 
C V01 12-JAN-1999 GLA INITIAL RELEASE FOR FINLAND
C
C MAIN PROGRAM FOR VLF PURGE, UPDATE AND REPORTING
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM VLFTSK
        IMPLICIT NONE
C       
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
C
        INTEGER*4 FLAG,ST,DUM(2),COUNT
        INTEGER*4 ST1  
C       INTEGER*4 ST2,ST3,ST4,ST5
        INTEGER*4 ST8,ST9
        LOGICAL   RUNNING/.FALSE./
        LOGICAL   INVOICE_RUNNING
C
        LOGICAL*4 ISSUB
C
        LOGICAL DSPSTOPMSG            ! DISPLAY STOP MESSAGE
C
	INTEGER*4 I,J,GTYP
        LOGICAL*4 RUN_POOLBLD
C
        CALL COPYRITE
        CALL SNIF_AND_WRKSET
        DSPSTOPMSG = .TRUE.
C
C DO NOT ALLOW RUN THIS TASK IN THE MANUAL MODE OF WINNER SELECTION
C
C ( NOT USET FOR PORTUGAL )
C
C        IF(STOPMOD.EQ.WINMANUAL) THEN
C           TYPE *,IAM(),'VLFTSK is not supposed to be used in the manual',
C     *            ' mode of winner selection'
C           CALL GSTOP(GEXIT_SUCCESS)
C        ENDIF
C
C START VLFTSK PROCEDURE
C
        ISSUB = ISSUBPROC()
        IF(.NOT.ISSUB) THEN
          CALL PRMYESNO('Are you sure you want run (not SUBRUN) VLFTSK [Y/N]?',
     *                  FLAG)
          IF(FLAG.NE.1) CALL GSTOP(GEXIT_OPABORT)
        ENDIF
C
C Check WINSEL statuses
C
	CALL DISPWIN(2)
C
C ASK TO USER IF HE/SHE WANS RUN MULTIWIN
C
        CALL PRMYESNO('Do You Want To Run Winner Selection Today [Y/N] ', FLAG)
        IF(FLAG .EQ. 1) THEN
          WRITE(6, 901) IAM(), 'MULTIWIN'
          CALL NRUNTSK(8HMULTIWIN )
        ENDIF
C
C MAKE SURE THAT MULTIWIN HAS FINISHED IF NOT DO NOT CONTINUE WITH VLFTSK
C
1000    CONTINUE
        RUNNING = .FALSE.
        CALL XWAIT(2, 2, ST1)
        CALL STTSK(8HMULTIWIN , DUM, ST1)
        IF (ST1 .NE. 4) RUNNING = .TRUE.
        IF(RUNNING) GOTO 1000 
C
C RUN POOLBLD
C
	RUN_POOLBLD=.TRUE.
	DO 95, I=1,MAX_WINSEL
	    DO 94, J=1,MAXGAM
		GTYP=GNTTAB(GAMTYP,J)
		IF (GTYP.NE.TLTO .AND. GTYP.NE.TSPT) GOTO 94
		IF (DRWSTS(I,J).EQ.WINSOK .OR. DRWSTS(I,J).EQ.SHAROK) GOTO 96
94	    CONTINUE
95	CONTINUE
	RUN_POOLBLD=.FALSE.

96	CONTINUE 
	IF (RUN_POOLBLD) THEN
          WRITE(6,901) IAM(),'POOLBLD '
          CALL NRUNTSK(8HPOOLBLD )
	ENDIF
C
C TELL TO STSYSTEM THAT VLFTSK HAS BEEN STARTED
C
        VLFTSKSTS = VLFSTR 
C
C BE SURE VLF MERGE TASK IS DONE   
C
        IF(VLCSTS.NE.WMRG) THEN
           COUNT=1
10         CONTINUE
           CALL XWAIT(2,2,ST)
           CALL STTSK(8HMULTIVLF,DUM,ST1)
           IF (ST1.NE.4) THEN 
              COUNT=COUNT+1
              IF(MOD(COUNT,100).EQ.0) 
     *           TYPE *,IAM(),'Waiting for MULTIVLF completion'
              GOTO 10
           ENDIF
        ENDIF
C
C RUN SHARE CALCULATION PROCEDURES
C
        CALL PRMYESNO('Do you want to run ShareClc Task [Y/N]?', FLAG)
        IF(FLAG .EQ. 1) THEN
          WRITE(6, 901) IAM(), 'SHARECLC'
          CALL RUNTSK(8HSHARECLC)                 ! SHARCAL PROCEDURE
          WRITE(6, 901) IAM(), 'SHARERPT'
          CALL RUNTSK(8HSHARERPT)                 ! GENERATE SHARE REPORTS
        ENDIF
C
C RUN UPDTSK
C
12      CONTINUE
        CALL PRMYESNO('Do You Want To Run UpdTsk [Y/N]',FLAG)
        IF(FLAG. EQ. 1) THEN
          WRITE(6, 901) IAM(), 'UPDTSK  '
          CALL RUNTSK(8HUPDTSK  )
        ENDIF
	IF(UPDTSK_RUN_AGAIN .EQ. 1) THEN 
          TYPE *, IAM()
          TYPE *, IAM(), 'Warning ...'
          TYPE *, IAM()
          TYPE *, IAM(), 'UpdTsk Did Not Run OK !'
          TYPE *, IAM()
          TYPE *, IAM(), 'VlfTsk Detected Proplems During Execution Of Updtsk'
          TYPE *, IAM(), 'Recommended Run UpdTsk Again'
          TYPE *, IAM()
          CALL PRMYESNO('Do You Want To Run UpdTsk Again [Y/N] ', FLAG)
          IF(FLAG .EQ. 1) GOTO 12
        ENDIF
C
C RUN BANK WINNERS PROGRAM
C
C        WRITE(6,901) IAM(), 'FBNKWN  '
C        CALL RUNTSK(8HFBNKWN  )
C        CALL PRMYESNO('Did FBNKWN run O.K. ? (Y/N) ',FLAG)
C        IF(FLAG.NE.1) THEN
C           WRITE(6,902) IAM()
C           CALL GPAUSE
C        ENDIF
C
C RUN PAYUPD TASK ( REMOVED THIS TASK IS STARTED IN STSYSTEM PROCEDURE )
C
C        WRITE(6,901) IAM(), 'PAYUPD  '
C        CALL RUNTSK(8HPAYUPD  )
C
C NOW PARALLEL PROCESING OF REPORTING TASKS
C
C IF IT IS AN INVOICE DAY THEN WAIT FOR INVOICE CALCULATION COMPLETION AND 
C THEN RUN FTRANS TAPE PROGRAM
C
85      CONTINUE
        INVOICE_RUNNING = .FALSE.

        IF(INVOICE_DAY) THEN
90         CONTINUE
           CALL XWAIT(2,2,ST)
           CALL STTSK(8HINVCLC  ,DUM,ST8)
           IF (ST8.NE.4) THEN
              INVOICE_RUNNING = .TRUE.
              GOTO 90
           ENDIF
           IF(.NOT.INVOICE_RUNNING) THEN ! it could be not started or
                                         ! already done
               CALL OPENASF(1)          
               CALL READASF(NUMAGT,ASFREC,ST)    !read last record
               IF(ASFINV(ASFEND,1).NE.DAYCDC) THEN
                  TYPE *, IAM(), 'Waiting for completion of INVCLC '
                  IF(DSPSTOPMSG) CALL DPL_CONT_WITH_STSYSTEM(DSPSTOPMSG)
                  CALL CLOSASF 
                  CALL XWAIT(30,2,ST)
                  GOTO 85
               ENDIF
           ENDIF 
        ENDIF

C
C RUN LIABLE REPORT ( REMOVED THIS TASK IS STARTED IN STSYSTEM PROCEDURE )
C
C        WRITE(6,901) IAM(), 'LIABLE  '
C        CALL NRUNTSK(8HLIABLE  )
C
C RUN CSHREP REPORT ( REMOVED THIS TASK IS STARTED IN STSYSTEM PROCEDURE )
C
C        WRITE(6,901) IAM(), 'CSHREP  '
C        CALL NRUNTSK(8HCSHREP  )
C
C RUN BIGWIN & FCANWIN REPORTS 
C
C ( REMOVED THIS TASK IS STARTED IN STSYSTEM PROCEDURE )
C
C        WRITE(6,901) IAM(),'FBIGWIN '
C        CALL NRUNTSK(8HFBIGWIN )
C
C        WRITE(6,901) IAM(),'FCANWIN '
C        CALL NRUNTSK(8HFCANWIN )
C
        COUNT=1
100     CONTINUE
        COUNT=COUNT+1
        IF(MOD(COUNT,100).EQ.0) THEN
C         IF(ST2.NE.4) TYPE *,IAM(),'Waiting for LIABLE completion'
C         IF(ST3.NE.4) TYPE *,IAM(),'Waiting for CSHREP completion'
C         IF(ST4.NE.4) TYPE *,IAM(),'Waiting for FBIGWIN completion'
C         IF(ST5.NE.4) TYPE *,IAM(),'Waiting for FCANWIN completion'
          IF(ST9.NE.4) TYPE *,IAM(),'Waiting for POOLBLD copmletion'
        ENDIF
        RUNNING = .FALSE.
        CALL XWAIT(2,2,ST)
C
C       CALL STTSK(8HLIABLE  ,DUM,ST2)
C       IF (ST2.NE.4) RUNNING = .TRUE.
C
C       CALL STTSK(8HCSHREP  ,DUM,ST3)
C       IF (ST3.NE.4) RUNNING = .TRUE.
C
C       CALL STTSK(8HFBIGWIN ,DUM,ST4)
C       IF (ST4.NE.4) RUNNING = .TRUE.
C
C       CALL STTSK(8HFCANWIN ,DUM,ST5)
C       IF (ST5.NE.4) RUNNING = .TRUE.
C
        CALL STTSK(8HPOOLBLD ,DUM,ST9)
        IF (ST9.NE.4) RUNNING = .TRUE.
C
C CHECK IF STSYSTEM CAN CONTINUE WITH BALANS PROGRAM
C
C       IF(ST2.EQ.4.AND.ST3.EQ.4) THEN
C          VLFTSKSTS = REDBAL 
C          TYPE*,IAM(),'Ready For BALANS'
C          IF(DSPSTOPMSG) CALL DPL_CONT_WITH_STSYSTEM(DSPSTOPMSG)
C       ENDIF
        IF(RUNNING) GOTO 100
C
C RUN PRGTSK
C
        CALL PRMYESNO('Do you want to run PRGTSK today [Y/N]?',FLAG)
        IF(FLAG.EQ.1) THEN
          WRITE(6,901) IAM(), 'PRGTSK  '
          CALL RUNTSK(8HPRGTSK  )
        ENDIF
C
        VLFTSKSTS = REDBAL
        CALL STTSK(8HSTSYSTEM,DUM,ST)
        IF (ST.EQ.4) THEN
            TYPE *, IAM(),'VLFTSK Finished - Perform file backups'
            IF(DSPSTOPMSG) CALL DPL_CONT_WITH_STSYSTEM(DSPSTOPMSG)
            CALL GSTOP(GEXIT_SUCCESS)
        ENDIF

        STOPMOD = WINMANUAL
        TYPE*,IAM(),' VLFTSK complete - Continue With Stopsys'
        IF(DSPSTOPMSG) CALL DPL_CONT_WITH_STSYSTEM(DSPSTOPMSG)
        CALL GSTOP(GEXIT_SUCCESS)
C
901     FORMAT(1X,A,' Begining execution of ',A8)
902     FORMAT(1X,A,' Continue VLFTSK if FBNKWN has been completed')
C
        END



C ******************************************************************************
C
C     SUBROUTINE: DPL_CONT_WITH_STSYSTEM
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 22 / 05 / 2001
C
C ******************************************************************************
C
C FUNCTION TO DISPLAY MESSAGE PLEASE CONTINUE WITH STSYSTEM
C
C=======OPTIONS /CHECK = NOOVERFLOW /EXT
      SUBROUTINE  DPL_CONT_WITH_STSYSTEM(MSGDISPLAYED)
      IMPLICIT NONE
C
C INCLUDES DEFINITION TO DISPLAY MESSAGE PLEASE CONTINUE WITH STSYSTEM
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
C PARAMETERS DEFINITION TO DISPLAY MESSAGE PLEASE CONTINUE WITH STSYSTEM
C
      LOGICAL MSGDISPLAYED         ! MESSAGE DISPLAYED OR NOT
C
C VARIABLES DEFINITION TO DISPLAY MESSAGE PLEASE CONTINUE WITH STSYSTEM
C
      CHARACTER * 1 ESC / Z1B /    ! ESCAPE CHARACTER
C
C SET DISPLAYED MESSAGE TO FLASE ( MESSAGE ALRREADY DISPLAYED )
C
      MSGDISPLAYED = .FALSE.
C
C DISPLAY MESSAGET TO USER ( PLEASE ENTER STSYSTEM CONTINUE )
C
      TYPE *, IAM()
      TYPE *, ' '
      TYPE *, ' '
      TYPE 100, ESC
      TYPE 200, ESC
      TYPE *, ' '
      TYPE *, IAM()
C
C FORMATS DEFINITION TO DISPLAY MESSAGE PLEASE CONTINUE WITH STSYSTEM
C
100   FORMAT('+', A1, '#5', 4X, 'Please Enter StSystem Cont')
200   FORMAT('+', A1, '#6', 4X, 'Please Enter StSystem Cont')
C
C THIS IS THE END TO DISPLAY MESSAGE PLEASE CONTINUE WITH STSYSTEM
C
      END

