C
C     FILE   : SPTCANEVENTS.FOR
C     AUTHOR : J.H.R
C     VERSION: 01            DATE:  04 / 04 / 2017
C
C V01 12-MAR-2004 JHR INITIAL RELEASE FOR PORTUGAL PROJECT
C
C PROGRAM TO CANCEL AN EVENT FOR THE SPORTS GAME. THE TASK WILL REQUEST TO THE USER
C THE SPORTS GAME INDEX, THE DRAW NUMBER AND THE EVENT TO BE CANCELLED. THE PROCESS
C WILL REQUEST TO THE USER BY AN AUTHORIZATION CODE TO SEND THE COMMAND TO THE TASK.
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
C===  OPTIONS /CHECK=NOOVERFLOW
      PROGRAM SPTCANEVENTS
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
      INTEGER * 4 USROPT_CANEVT  ! CANCEL EVENT
      INTEGER * 4 USROPT_UPDFDW  ! UPDATE FUTURE DRAWS
      INTEGER * 4 USROPT_UPDCDW  ! UPDATE CURRENT DRAW
      INTEGER * 4 USROPT_FINISH  ! EXIT FROM THIS TOOL
C
      PARAMETER(USROPT_CANEVT = 1)
      PARAMETER(USROPT_UPDFDW = 2)
      PARAMETER(USROPT_UPDCDW = 3)
      PARAMETER(USROPT_FINISH = 4)
C
      INTEGER * 4 GIND     ! GAME NUMBER
      INTEGER * 4 DRAW     ! DRAW NUMBER
      INTEGER * 4 ROW_NUM  ! ROW NUMBER TO BE CANCELLED
      INTEGER * 4 USER_OPT ! USER OPTION
      INTEGER * 4 FSTS     ! FUNCTION STATUS
C
      CALL COPYRITE
C
      TYPE   *, IAM(), '* * * * * * * * * * * * * * * * * * * * * * * * * * * * *'
      TYPE   *, IAM(), '*   S P O R T S   C A N C E L   E V E N T S   T O O L   *'
      TYPE   *, IAM(), '* * * * * * * * * * * * * * * * * * * * * * * * * * * * *'
      TYPE   *, IAM()
      TYPE 100, IAM(), USROPT_CANEVT, 'Cancel An Sport Event'
      TYPE 100, IAM(), USROPT_UPDFDW, 'Update The Events Needed To Cancel The Full Draw (Future)'
      TYPE 100, IAM(), USROPT_UPDCDW, 'Update The Events Needed To Cancel The Full Draw (Current)'
      TYPE 100, IAM(), USROPT_FINISH, 'EXIT From This Tool'
      TYPE   *, IAM()
C
      CALL INPNUM('Enter The Option To Be Executed   ', USER_OPT, USROPT_CANEVT, USROPT_FINISH, FSTS)
C
      IF(FSTS .LT. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Operation Aborted By The User ...'
        TYPE *, IAM()
        CALL GSTOP(GEXIT_OPABORT)
      ENDIF
C
      CALL CHCKDIS(FSTS)
C
      IF(FSTS .EQ. 0 .AND. USER_OPT .EQ. USROPT_UPDFDW) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'The System Is Active, Please Kill The System'
        TYPE *, IAM(), 'To Be Able To Update The Sports Game File Or Execute'
        TYPE *, IAM(), 'This Option When The Stopsys Process Has Already Finish'
        TYPE *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      IF(FSTS .NE. 0 .AND. USER_OPT .NE. USROPT_UPDFDW) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'The System Is Down, Bring-Up The System'
        TYPE *, IAM(), 'To Be Able To To Send The Sports Commands'
        TYPE *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      IF(USER_OPT .NE. USROPT_FINISH) CALL GET_USER_CANCEL_OPTIONS(GIND, DRAW, USER_OPT, USROPT_UPDFDW)
C
      IF(USER_OPT .EQ. USROPT_CANEVT) CALL CANCEL_SPORTS_EVENT(GIND, DRAW)  ! CANCEL AN EVENT
      IF(USER_OPT .EQ. USROPT_UPDFDW) CALL UPDATE_SPORTS_MFECD(GIND, DRAW)  ! MODIFY FUTURE DRAWS MAXIMUM EVENTS TO CANCEL THE DRAW
      IF(USER_OPT .EQ. USROPT_UPDCDW) CALL UPDATE_SPORTS_MCECD(GIND, DRAW)  ! MODIFY CURRENT DRAW MAXIMUM EVENTS TO CANCEL THE DRAW
C
      TYPE *, IAM()
C
      CALL GSTOP(GEXIT_SUCCESS)
C
100   FORMAT(X, A, I1.1, '.-', X, A)
C
      END


C ******************************************************************************
C
C     SUBROUTINE: GET_USER_CANCEL_OPTIONS
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 12 / 03 / 2003
C
C ******************************************************************************
C
C FUNCTION REQUEST TO THE USER BY ALL THE DATA NEEDED TO CANCEL THE SPORT EVENT
C OR MODIFY THE TOTAL NUMBER OF EVEENTS CANCELLED NEEDED TO CANCEL THE FULL DRAW
C
C==== OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE GET_USER_CANCEL_OPTIONS(GIND, DRAW, USER_OPT, UPD_FDW)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:DSPREC.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:SPTCOM.DEF'
C
      INTEGER * 4 GIND     ! GAME NUMBER
      INTEGER * 4 DRAW     ! DRAW NUMBER
      INTEGER * 4 USER_OPT ! USER OPTION
      INTEGER * 4 UPD_FDW  ! OPTION THAT SHOULD BE SELECTED TO UPDATE FUTURE DRAWS
C
      INTEGER * 4 FSTS     ! FUNCTION STATUS
C
      LOGICAL DRAW_IN_MEM  ! DRAW IN MEMORY
C
      CHARACTER * 32 EVENT_CANCEL_MESSAGE  ! CANCEL MESSATE: Cancelled dd/mm/yyyy At hh:mm:ss
C
      CALL INPNUM('Enter The Cancel Event Game Index ', GIND, 1, NUMSPT, FSTS)
C
      IF(FSTS .LT. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Operation Aborted By The User ...'
        TYPE *, IAM()
        CALL GSTOP(GEXIT_OPABORT)
      ENDIF
C
      IF(GTNTAB(TSPT, GIND) .LE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'The Sports Game Is Not Defined'
        TYPE *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      CALL INPNUM('Enter The Cancel Event Draw Number', DRAW, 1, 99999999, FSTS)
C
      IF(FSTS .LT. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Operation Aborted By The User ...'
        TYPE *, IAM()
        CALL GSTOP(GEXIT_OPABORT)
      ENDIF
C
      IF(DRAW .GT. SPTDRW(GIND) .AND. USER_OPT .NE. UPD_FDW) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'The Draw Number Selected Can''t Be Greater Than The'
        TYPE *, IAM(), 'Active Draw In The Common Memory Because The The Update'
        TYPE *, IAM(), 'Command For Future Draws Has To Be Executed In Disk'
        TYPE *, IAM(), 'And Doesn''t Check If The Full Draw Should Be Cancelled'
        TYPE *, IAM()
        TYPE *, IAM(), 'Draw Entered: ', DRAW
        TYPE *, IAM(), 'Draw Active : ', SPTDRW(GIND)
        TYPE *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      IF(DRAW .LE. SPTDRW(GIND) .AND. USER_OPT .EQ. UPD_FDW) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'The Draw Number Selected To Update Future Draws Should'
        TYPE *, IAM(), 'Greater Than The One Active In The Common Memory Because'
        TYPE *, IAM(), 'The Update Is For Future Draws In The File Disk'
        TYPE *, IAM()
        TYPE *, IAM(), 'Draw Entered: ', DRAW
        TYPE *, IAM(), 'Draw Active : ', SPTDRW(GIND)
        TYPE *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      ! THE USER SELECTED UPDATE FOR FUTURE DRAWS (ON DISK), TO NEXT CHECKS WILL BE DONE IN THE UPDATE ROUTINE
      IF(USER_OPT .EQ. UPD_FDW) RETURN
C
      CALL READ_SPORTS_DRAW_FROM_MEM_OR_DISK(DRAW, GIND, DSPREC, .FALSE., FSTS)
C
      IF(FSTS .NE. 0) CALL GSTOP(GEXIT_FATAL)
C
      CALL FRMT_CANCEL_EVENT_DATE_TIME(DSPDCD, DSPDAT(CURDRW), EVENT_CANCEL_MESSAGE)
C
      IF(DSPRWD .NE. 0 .OR. DSPDCD .NE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'The Draw Selected Is Already Cancelled.' 
        TYPE *, IAM()
        TYPE *, IAM(), 'Refund Div: ', DSPRWD
        TYPE *, IAM()
        TYPE *, IAM(), EVENT_CANCEL_MESSAGE   !  Cancelled dd/mm/yyyy At hh:mm:ss
        TYPE *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      IF(DSPSTS .GE. GAMEN1) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'The Winning Number Already Entered For The Draw Selected'
        TYPE *, IAM()
        TYPE *, IAM(), 'Draw Status: ', DSPSTS
        TYPE *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      END


C ******************************************************************************
C
C     SUBROUTINE: UPDATE_SPORTS_MFECD
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 12 / 03 / 2003
C
C ******************************************************************************
C
C FUNCTION TO REQUEST TO THE USER BY ALL THE DATA NEEDED TO MODIFY THE TOTAL NUMBER
C OF EVEENTS CANCELLED NEEDED TO CANCEL THE FULL DRAW FOR ALL FUTURE DRAWS STARTING
C FROM NEXT TO THE ONE IN THE CURRENT MEMORY
C
C==== OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE UPDATE_SPORTS_MFECD(GIND, DRAW)
      IMPLICIT NONE
C
      INCLUDE '(LIB$ROUTINES)'
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:DSPREC.DEF'
      INCLUDE 'INCLIB:DISKQIO.DEF'
C
      INTEGER * 4 GIND     ! GAME NUMBER
      INTEGER * 4 DRAW     ! DRAW NUMBER
C
      INTEGER * 4 IDX           ! INDEX COUNTER
      INTEGER * 4 FSTS          ! FUNCTION STATUS
      INTEGER * 4 IDFIL         ! FILE ID
      INTEGER * 4 GNUM          ! GAME NUMBER
      INTEGER * 4 UPD_DRAW      ! UPDATED DRAW
      INTEGER * 4 NEW_CANEVENTS ! NEW CANCEL EVENTS
      INTEGER * 4 FILE_FDB(7)   ! FILE DESCRIPTOR BLOCK
      INTEGER * 4 BEG_DRAW      ! BEGIND DRAW NUMBER TO BE UPDATE
      INTEGER * 4 END_DRAW      ! END DRAW NUMBET TO BE UPDATE
      INTEGER * 4 SEC_SIZE      ! FILE SECTOR SIZE
C
      CALL READ_SPORTS_DRAW_FROM_MEM_OR_DISK(DRAW, GIND, DSPREC, .FALSE., FSTS)
C
      IF(FSTS .NE. 0) CALL GSTOP(GEXIT_FATAL)      
C
      ! ASSUME THAT THE MAXIMUM NUMBER OF CANCELED WILL BE NOT GREATER THAN THE MAXIMUN SET FOR THE CURRENT DRAW IN MEMORY -- SPTMAX -
      !
      ! NOTE: - IF THE USER ENTER 0 IT MEAND THAT THIS CONFIGURATION DOESN'T APPLY AND THE DRAW NEVER WILL BE CANCELLED
      !
      CALL INPNUM('Enter The Events Cancelled Needed To Cancel The Draw ', NEW_CANEVENTS, 0, DSPMAX, FSTS)
C
      IF(FSTS .LT. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Operation Aborted By The User ...'
        TYPE *, IAM()
        CALL GSTOP(GEXIT_OPABORT)
      ENDIF
C
      CALL REQUEST_USER_AUTHORIZATION_CODE(GIND, DRAW, NEW_CANEVENTS, 428596)
C
      IDFIL = 3
C
      GNUM = GTNTAB(TSPT, GIND)
C
      CALL OPENW(IDFIL, GFNAMES(1, GNUM), 4, 0, 0, FSTS)
C
      IF(FSTS .NE. 0) THEN
         TYPE   *, IAM()
         TYPE 100, IAM(), (GFNAMES(IDX, GNUM), IDX = 1, 5), FSTS
         TYPE   *, IAM()
         CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      CALL IOINIT(FILE_FDB, IDFIL, DSPSEC * 256)
C
      CALL VAXGETFSIZ(FILE_FDB(FDB_LUN), SEC_SIZE)
C
      BEG_DRAW = DRAW
      END_DRAW =  SEC_SIZE / (DSPSEC / 2 )
C
      DO UPD_DRAW = BEG_DRAW, END_DRAW
        CALL UPDATE_IN_DISK_SPORTS_MFECD(FILE_FDB, UPD_DRAW, GNUM, NEW_CANEVENTS)
      ENDDO
C
      CALL CLOSEFIL(FILE_FDB)
C
100   FORMAT(X, A, 'Error Opening File:', X, 5A4, 'Status:', X, I)
C
      END


C ******************************************************************************
C
C     SUBROUTINE: UPDATE_IN_DISK_SPORTS_MFECD
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 12 / 03 / 2003
C
C ******************************************************************************
C
C FUNCTION TO REQUEST TO THE USER BY ALL THE DATA NEEDED TO MODIFY THE TOTAL NUMBER
C OF EVEENTS CANCELLED NEEDED TO CANCEL THE FULL DRAW FOR ALL FUTURE DRAWS STARTING
C FROM NEXT TO THE ONE IN THE CURRENT MEMORY -- UDPATE DATA IN DISK --
C
C==== OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE UPDATE_IN_DISK_SPORTS_MFECD(FILE_FDB, DRAW, GNUM, NEW_CANEVENTS)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:DSPREC.DEF'
C
      INTEGER * 4 FILE_FDB(*)   ! FILE DESCRIPTOR BLOCK
      INTEGER * 4 DRAW          ! DRAW NUMBER
      INTEGER * 4 GNUM          ! GAME NUMBER
      INTEGER * 4 NEW_CANEVENTS ! NEW CANCEL EVENTS
C
      INTEGER * 4 FSTS          ! FUNCTION STATUS
      INTEGER * 4 IDX           ! INDEX COUNTER
      INTEGER * 4 ROW_IDX       ! ROW INDEX COUNTER
      INTEGER * 4 TOTCAN_EVENTS ! TOTAL NUMBER OF EVENTS CANCELLED
C
      CHARACTER * 32 EVENT_CANCEL_MESSAGE  ! CANCEL MESSATE: Cancelled dd/mm/yyyy At hh:mm:ss
C
      CALL READW(FILE_FDB, DRAW, DSPREC, FSTS)
C
      IF(FSTS .NE. 0) THEN
        TYPE   *, IAM()
        TYPE 100, IAM(), (GFNAMES(IDX, GNUM), IDX = 1, 5), FSTS
        TYPE   *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      IF(DSPRWD .NE. 0 .OR. DSPDCD .NE. 0) THEN
        CALL FRMT_CANCEL_EVENT_DATE_TIME(DSPDCD, DSPDAT(CURDRW), EVENT_CANCEL_MESSAGE)
        TYPE *, IAM()
        TYPE *, IAM(), 'The Draw Selected Is Already Cancelled.' 
        TYPE *, IAM()
        TYPE *, IAM(), 'Draw Number: ', DRAW
        TYPE *, IAM(), 'Refund Div : ', DSPRWD
        TYPE *, IAM()
        TYPE *, IAM(), EVENT_CANCEL_MESSAGE   !  Cancelled dd/mm/yyyy At hh:mm:ss
        TYPE *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      IF(DSPSTS .GE. GAMEN1) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'The Winning Number Already Entered For The Draw Selected'
        TYPE *, IAM()
        TYPE *, IAM(), 'Draw Number: ', DRAW
        TYPE *, IAM(), 'Draw Status: ', DSPSTS
        TYPE *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      TOTCAN_EVENTS = 0
C
      DO ROW_IDX = 1, MIN(DSPMAX, SPGNBR)
        IF(DSPECD(ROW_IDX) .NE. 0) TOTCAN_EVENTS = TOTCAN_EVENTS + 1
      ENDDO
C
      IF(NEW_CANEVENTS .LE. TOTCAN_EVENTS .AND. NEW_CANEVENTS .NE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'The New Total Number Of Cancel Events To Cancel The Draw'
        TYPE *, IAM(), 'Must Be Greater Than The Total Number Of Events Already'
        TYPE *, IAM(), 'Cancelled Because The Proces To Start The Cancel Draw'
        TYPE *, IAM(), 'Must Be Through A Event Cancelation'
        TYPE *, IAM()
        TYPE *, IAM(), 'Draw Number               : ', DRAW
        TYPE *, IAM(), 'Events Already Cancelled  : ', TOTCAN_EVENTS
        TYPE *, IAM(), 'Events New Value Requested: ', NEW_CANEVENTS
        TYPE *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      IF(DSPMCE .EQ. NEW_CANEVENTS) RETURN  ! DRAW ALREADY UPDATED WITH THE REQUESTED NEW VALUE
C
      IF(DSPSTS .LT. GAMCLD) RETURN         ! DRAW NOT SETUP
C
      DSPMCE = NEW_CANEVENTS
C
      CALL WRITEW(FILE_FDB, DRAW, DSPREC, FSTS)
C
      IF(FSTS .NE. 0) THEN
        TYPE   *, IAM()
        TYPE 101, IAM(), (GFNAMES(IDX, GNUM), IDX = 1, 5), FSTS
        TYPE   *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
100   FORMAT(X, A, 'Error Reading File:', X, 5A4, 'Status:', X, I)
101   FORMAT(X, A, 'Error Writing File:', X, 5A4, 'Status:', X, I)
C
      END


C ******************************************************************************
C
C     SUBROUTINE: UPDATE_SPORTS_MCECD
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 12 / 03 / 2003
C
C ******************************************************************************
C
C FUNCTION TO REQUEST TO THE USER BY ALL THE DATA NEEDED TO MODIFY THE TOTAL NUMBER
C OF EVEENTS CANCELLED NEEDED TO CANCEL THE FULL DRAW FOR THE CURRENT DRAW IN MEMORY
C
C==== OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE UPDATE_SPORTS_MCECD(GIND, DRAW)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:DSPREC.DEF'
C
      INTEGER * 4 GIND     ! GAME NUMBER
      INTEGER * 4 DRAW     ! DRAW NUMBER
C
      INTEGER * 4 FSTS            ! FUNCTION STATUS
      INTEGER * 4 ROW_NUM         ! ROW NUMBER TO BE CANCELLED
      INTEGER * 4 TOTCAN_EVENTS   ! TOTAL NUMBER OF EVENTS CANCELLED
      INTEGER * 4 NEW_CANEVENTS   ! NEW CANCEL EVENTS
C
      CALL READ_SPORTS_DRAW_FROM_MEM_OR_DISK(DRAW, GIND, DSPREC, .FALSE., FSTS)
C
      IF(FSTS .NE. 0) CALL GSTOP(GEXIT_FATAL)      
C
      !
      ! NOTE: - IF THE USER ENTER 0 IT MEAND THAT THIS CONFIGURATION DOESN'T APPLY AND THE DRAW NEVER WILL BE CANCELLED
      !
      CALL INPNUM('Enter The Events Cancelled Needed To Cancel The Draw ', NEW_CANEVENTS, 0, DSPMAX, FSTS)
C
      IF(FSTS .LT. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Operation Aborted By The User ...'
        TYPE *, IAM()
        CALL GSTOP(GEXIT_OPABORT)
      ENDIF
C
      IF(DSPMCE .EQ. NEW_CANEVENTS) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'The Draw Is Already Setup With This Number Of Cancelled'
        TYPE *, IAM(), 'Events To Cancel The Draw'
        TYPE *, IAM()
        TYPE *, IAM(), 'Draw Number Of Cancel Events To Cancel A Draw: ', DSPMCE
        TYPE *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      CALL GET_SPORTS_TOT_CANCEL_EVENTS(DRAW, GIND, .FALSE., TOTCAN_EVENTS, FSTS)
C
      IF(FSTS .NE. 0) CALL GSTOP(GEXIT_FATAL)
C
      IF(NEW_CANEVENTS .LE. TOTCAN_EVENTS .AND. NEW_CANEVENTS .NE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'The New Total Number Of Cancel Events To Cancel The Draw'
        TYPE *, IAM(), 'Must Be Greater Than The Total Number Of Events Already'
        TYPE *, IAM(), 'Cancelled Because The Proces To Start The Cancel Draw'
        TYPE *, IAM(), 'Must Be Through A Event Cancelation'
        TYPE *, IAM()
        TYPE *, IAM(), 'Events Already Cancelled  : ', TOTCAN_EVENTS
        TYPE *, IAM(), 'Events New Value Requested: ', NEW_CANEVENTS
        TYPE *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      CALL REQUEST_USER_AUTHORIZATION_CODE(GIND, DRAW, NEW_CANEVENTS, 584268)
C
      CALL SEND_CANCEL_EVENTS_CMD(GIND, DRAW, NEW_CANEVENTS, 6)
C
      END


C ******************************************************************************
C
C     SUBROUTINE: CANCEL_SPORTS_EVENT
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 12 / 03 / 2003
C
C ******************************************************************************
C
C FUNCTION REQUEST TO THE USER BY ALL THE DATA NEEDED TO CANCEL THE SPORT EVENT
C OR MODIFY THE TOTAL NUMBER OF EVEENTS CANCELLED NEEDED TO CANCEL THE FULL DRAW
C
C==== OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE CANCEL_SPORTS_EVENT(GIND, DRAW)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:DSPREC.DEF'
C
      INTEGER * 4 GIND     ! GAME NUMBER
      INTEGER * 4 DRAW     ! DRAW NUMBER
C
      INTEGER * 4 TOTCAN_EVENTS ! TOTAL NUMBER OF EVENTS CANCELLED
      INTEGER * 4 ROW_NUM       ! ROW NUMBER TO BE CANCELLED
      INTEGER * 4 ROW_IDX       ! ROW INDEX COUNTER
      INTEGER * 4 FSTS          ! FUNCTION STATUS
      INTEGER * 4 USER_ANSWER   ! USER ANSWER
C
      CHARACTER * 32 EVENT_CANCEL_MESSAGE  ! CANCEL MESSATE: Cancelled dd/mm/yyyy At hh:mm:ss
C
      CALL READ_SPORTS_DRAW_FROM_MEM_OR_DISK(DRAW, GIND, DSPREC, .FALSE., FSTS)
C
      IF(FSTS .NE. 0) CALL GSTOP(GEXIT_FATAL)
C
      IF(DSPMCE .LE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'The Maximun Number Of Events Cancelled To Cancel The Draw'
        TYPE *, IAM(), 'Is Not Configured, Please Configure First The Total Number'
        TYPE *, IAM(), 'Of Events To Cancel The Full Draw'
        TYPE *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      CALL INPNUM('Enter The Cancel Event Row Number ', ROW_NUM, 1, DSPMAX, FSTS)
C
      IF(FSTS .LT. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Operation Aborted By The User ...'
        TYPE *, IAM()
        CALL GSTOP(GEXIT_OPABORT)
      ENDIF
C
      CALL FRMT_CANCEL_EVENT_DATE_TIME(DSPECD(ROW_NUM), DSPDAT(CURDRW), EVENT_CANCEL_MESSAGE)
C
      IF(DSPECD(ROW_NUM) .NE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'The Event Number Selected Is Already Cancelled'
        TYPE *, IAM()
        TYPE *, IAM(), EVENT_CANCEL_MESSAGE   !  Cancelled dd/mm/yyyy At hh:mm:ss
        TYPE *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      CALL GET_SPORTS_TOT_CANCEL_EVENTS(DRAW, GIND, .FALSE., TOTCAN_EVENTS, FSTS)
C
      IF(FSTS .NE. 0) CALL GSTOP(GEXIT_FATAL)
C
      TOTCAN_EVENTS = TOTCAN_EVENTS + 1  ! ALWAYS ADD ONE
C
      IF(TOTCAN_EVENTS .GT. DSPMCE .AND. DSPDIV .GE. SPGDIV) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'The Draw Is Must Be Cancelled But The Draw Has Been Setup With'
        TYPE *, IAM(), 'The Maximun Number Of Division For The Sports Games, So The'
        TYPE *, IAM(), 'Refund Winning Division Can''t Be Added'
        TYPE *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      CALL REQUEST_USER_AUTHORIZATION_CODE(GIND, DRAW, ROW_NUM, 359642)
C
      IF(TOTCAN_EVENTS .LE. DSPMCE) THEN   ! THE DRAW WILL BE NO CANCELLED, SO ALL DONE
         CALL SEND_CANCEL_EVENTS_CMD(GIND, DRAW, ROW_NUM, 5)
         RETURN
      ENDIF
C
      TYPE *, IAM()
      TYPE *, IAM(), 'Attention !!!'
      TYPE *, IAM(), '-------------'
      TYPE *, IAM()
      TYPE *, IAM(), 'If The Sports Event Is Cancelled, According With The Current'
      TYPE *, IAM(), 'Draw Setup The Full Draw Will Be Cancelled With This Command'
      TYPE *, IAM()
      TYPE *, IAM(), 'Events Already Cancelled       : ', TOTCAN_EVENTS - 1
      TYPE *, IAM(), 'Events Setup To Cancel The Draw: ', DSPMCE
      TYPE *, IAM()
C
      CALL PRMYESNO('Are You Sure You Want To Continue [Y/N]?', USER_ANSWER)
C
      IF(USER_ANSWER .NE. 1) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Operation Aborted By The User ...'
        TYPE *, IAM()
        CALL GSTOP(GEXIT_OPABORT)
      ENDIF
C
      CALL SEND_CANCEL_EVENTS_CMD(GIND, DRAW, ROW_NUM, 5)  ! ALL DONE
C
      END


C ******************************************************************************
C
C     SUBROUTINE: REQUEST_USER_AUTHORIZATION_CODE
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 12 / 03 / 2003
C
C ******************************************************************************
C
C FUNCTION REQUEST TO THE USER THE AUTHORIZATION CODE TO RUN THIS TASK AND
C CHECK IF THE AUTHORIZATION CODE ENTERED IS THE CORRECT ONE ACCORDING WITH
C THE PARAMETERS ENTERED BY THE USED AND THE RUN DAY CDC DATE
C
C==== OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE REQUEST_USER_AUTHORIZATION_CODE(GIND, DRAW, CMD_VAL, OFF_NUM)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:DATBUF.DEF'
C
      INTEGER * 4 GIND     ! GAME NUMBER
      INTEGER * 4 DRAW     ! DRAW NUMBER
      INTEGER * 4 CMD_VAL  ! COMMAND VALUE -- ROW NUMBER OR NEW MAX CANCEL EVENTS TO CANCEAL A DRAW --
      INTEGER * 4 OFF_NUM  ! OFFSET NUMBER -- TO HAVE DIFFERENT CODES IF INPUT DATA IS EQUAL --
C
      INTEGER * 4 FSTS            ! FUNCTION STATUS
      INTEGER * 4 USER_AUTH_CODE  ! AUTHORIZATION CODE ENTERED BY THE USER
      INTEGER * 4 CALC_AUTH_CODE  ! AUTHORIZATION CODE CALCULATED BY THE SYSTEM
C
      INTEGER * 2 DATE(LDATE_LEN) ! DATE BUFFER
C
C CALCULATE THE AUTHORIZATION CONDE
C
C   AUTH_CODE = (DAY * MONTH * YEAR + DAYCDC + DRAW) * ROW_NUM * GIND + OFF_NUM
C
      DATE(VCDC) = DAYCDC
C
      CALL CDATE(DATE)
C
      CALC_AUTH_CODE = DAYCDC + DRAW
      CALC_AUTH_CODE = CALC_AUTH_CODE + DATE(VDAY) * DATE(VMON) * (DATE(VYEAR) + 2000)
      CALC_AUTH_CODE = CALC_AUTH_CODE * CMD_VAL * GIND
      CALC_AUTH_CODE = CALC_AUTH_CODE + OFF_NUM
C
D     TYPE *, IAM()
D     TYPE *, IAM(), 'Autorization Code: ', CALC_AUTH_CODE
D     TYPE *, IAM()
C
      CALL INPNUM('Enter The User Code Needed To Send The System Command', USER_AUTH_CODE, 0, 999999999, FSTS)
C
      IF(FSTS .LT. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Operation Aborted By The User ...'
        TYPE *, IAM()
        CALL GSTOP(GEXIT_OPABORT)
      ENDIF
C
      IF(USER_AUTH_CODE .NE. CALC_AUTH_CODE) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Wrong Authorization Code Entered ...'
        TYPE *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      END


C ******************************************************************************
C
C     SUBROUTINE: SEND_CANCEL_EVENTS_CMD
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 12 / 03 / 2003
C
C ******************************************************************************
C
C FUNCTION REQUEST TO SEMD THE CANCELLATION COMMAND TO THE "CMDPRO" TASK
C
C==== OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE SEND_CANCEL_EVENTS_CMD(GIND, DRAW, CMD_VAL, CMD_NUM)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
C
      INTEGER * 8 UNIX_TIME ! TIME IN UNIX FORMAT -- EPOCH TIME --
C
      INTEGER * 4 GIND      ! GAME NUMBER
      INTEGER * 4 DRAW      ! DRAW NUMBER
      INTEGER * 4 CMD_VAL   ! COMMAND VALUE
      INTEGER * 4 CMD_NUM   ! COMMAND NUMBER
C
      INTEGER * 4 FSTS           ! FUNCTION STATUS
      INTEGER * 4 CMD_BUF(CDLEN) ! COMMAMD BUFFER
C
      CALL FASTSET(0, CMD_BUF, CDLEN)
C
      UNIX_TIME = 0
      UNIX_TIME = UNIX_TIME + INT8(988675200000)                   ! CDC 1, 1-May-2001, EPOCH TIME IN MILISECONDS
      UNIX_TIME = UNIX_TIME + INT8(DAYCDC - 1)  * INT8(86400000)   ! ADD NUMBER OF DAYS PER MILISECONDS BY DAY
      UNIX_TIME = UNIX_TIME + INT8(SECNDS(0.0)) * INT8(1000)       ! ADD TODAY NUMBER OF MILI-SECONDS
C
      CMD_BUF(1)  = CMD_NUM        ! COMMAND NUMBER
      CMD_BUF(2)  = CMD_VAL        ! COMMAND VALUE
      CMD_BUF(3)  = TCSPT          ! SPORTS COMMAND
      CMD_BUF(6) = 'SCET'          ! SOURCE COMMAND: "SPORT CANCEL EVENTS TOOL"
      CMD_BUF(8)  = DRAW           ! DRAW NUMBER
      CMD_BUF(9)  = GIND           ! GAME INDEX
      CMD_BUF(10) = CMD_VAL        ! COMMAND VALUE, ROW_NUM OR TOTAL CANCEL EVENTS TO CANCEL DRAW
      CMD_BUF(11) = IAND(ISHFT(UNIX_TIME, -32), 'FFFFFFFF'X)  ! MESSAGE ID -- HIGH PART --
      CMD_BUF(12) = IAND(ISHFT(UNIX_TIME,   0), 'FFFFFFFF'X)  ! MESSAGE ID -- LOW  PART --
C
      CALL QUECMD(CMD_BUF, FSTS)
C
      IF(FSTS .NE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Unable To Send The Command To The CMDPRO Task'
        TYPE *, IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      END
