C
C SUBROUTINE NET_TIMEOUT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFIP:[GOLS]NET_TIMEOUT.FOV                              $
C  $Date::   07 Jan 1997 13:03:58                                         $
C  $Revision::   1.1                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - net_timeout.for ***
C
C V01 21-NOV-96 PJS CHANGED CALL TO DN_FREEBUF() -> NET_FREEBUF().
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C       CHECK AND GENERATE THE TIMEOUT FOR THE NODE.
C
C Calling Sequence:
C       CALL NET_TIMEOUT(NODE, QUEUE, TIMEOUT, QUEUE_ID, STATUS)
C
C Input:
C       NODE            - NODE # TO CHECK
C       QUEUE           - QUEUE TO CHECK FOR
C       QUEUE_ID        - .NE. 0 ONLY 1 TIMEOUT, SO NOT TOO MANY TIMEOUTS
C       TIMEOUT         - TIMEOUT VALUE
C
C Output:
C       STATUS
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
        SUBROUTINE NET_TIMEOUT(NODE, QUEUE, TIMEOUT,
     *                         QUEUE_ID, NODE_STATUS)
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
        INCLUDE 'INCLIB:GLIST.DEF'
        INCLUDE 'INCLIB:DCNEVN.DEF'
        INCLUDE 'INCLIB:DESNET.DEF'
        INCLUDE 'INCLIB:DN_LINK.DEF'                    ! DECNET STRUCTURES
        INCLUDE 'INCLIB:DN_BLOCK.DEF'                   ! DECNET DATA BLOCKS
C
        INCLUDE '($SYSSRVNAM)'
C
C LOCAL DECLARATIONS
C
        INTEGER*4       BUFPTR,
     *                  BUFPTR1,
     *                  BUF_NO,
     *                  CHECK_BUF,
     *                  DCNBUF_NO,
     *                  DIF_TIME,
     *                  LAST_TIMEOUT_OK(NETSYS) /NETSYS * 0/,
     *                  NODE,
     *                  NODE_STATUS,
     *                  QUEUE(*),
     *                  QUEUE_ID,                       ! IF !0, SEND 1 TIMEOUT
     *                  STAT,
     *                  TIMEOUT                         ! IN SECONDS
C
C COMMON DECLARATIONS
C
        COMMON /TEST_CHECK/ CHECK_BUF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CHECK FIRST WRITE STARTED QUEUE.
C
        CALL LISTTOP (BUFPTR, QUEUE, STAT)
        IF (STAT .EQ. GLIST_STAT_EMPTY) THEN
          NODE_STATUS=0
          GOTO 100
        ENDIF
C
C GET TIME SINCE STARTED.
C
        CALL NET_DIFTIM(DN_BUFFER(BUFPTR), DIF_TIME)
C
C IF TIMEOUT EXCEEDED, FILL 'NEW' BUFFER WITH TIMEOUT DATA.
C
        IF (DIF_TIME .LE. TIMEOUT) THEN
          NODE_STATUS = 0
          GOTO 100
        ENDIF
C
        IF (LAST_TIMEOUT_OK(NODE) .NE. 0) THEN
          NODE_STATUS = -2
          GOTO 100
        ENDIF
C
        CALL NET_FILLNET(DN_BUFFER(BUFPTR), NODE,
     *                   BUF_NO, DCNBUF_NO, STAT)
C
        IF (STAT .NE. 0) THEN
          CALL OPS('*** NET_TIMEOUT - NODE TIMEOUT ***', NODE, DIF_TIME)
          CALL OPS('*** NET_TIMEOUT - ' //
     *             'COULD NOT GET A BUFFER TO TERMINATE NODE ***',
     *             STAT, 0)
          NODE_STATUS = -1
          GOTO 100
        ENDIF
C
C VERIFY THAT I/O DID NOT TERMINATE IN THE MEANTIME.
C
        CALL LISTTOP (BUFPTR1, QUEUE, STAT)
C
        IF (BUFPTR .NE. BUFPTR1 .OR. STAT .EQ. GLIST_STAT_EMPTY) THEN
          CHECK_BUF = 701
          CALL FREEBUF(BUF_NO)
          CALL NET_FREEBUF(DCNBUF_NO, STAT)
          IF (STAT .NE. 0) CALL OPS('*** NET_TIMEOUT ***', 0, 0)
          NODE_STATUS = 0
          GOTO 100
        ENDIF
C
        CALL ABL(DCNBUF_NO, DCN_NETQUE, STAT)
C
        STAT = SYS$SETEF(%VAL(NETIOTRAP))
C
        NODE_STATUS = DIF_TIME
C
C IF WAITING ON I/O OUTSTANDING ...
C
        IF (QUEUE_ID .NE. 0) LAST_TIMEOUT_OK(NODE) = -1
C
C JUMP TO HERE FOR PROBLEMS.
C
100     CONTINUE
        IF (NODE_STATUS .EQ. 0) LAST_TIMEOUT_OK(NODE) = 0
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
        RETURN
        END
