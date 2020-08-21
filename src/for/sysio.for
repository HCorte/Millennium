C
C SUBROUTINE SYSIO 
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFIP:[GOLS]SYSIO.FOV                                    $
C  $Date::   07 Jan 1997 13:04:04                                         $
C  $Revision::   1.1                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C *** Pre-Baseline Source - net_sysio.for ***
C
C V07 21-NOV-96 PJS CHANGED CALL TO DN_FREEBUF() -> NET_FREEBUF().
C V06 ??-???-??  WS ADDED QUEING BUFFER ADDRESS TO NET_IOSTARTED QUEUE
C V05 04-???-93 DAS RENAMED EVENT FLAG             
C V04 29-APR-92 JWE ADD QUEUE INTERLOCK RETRY
C V03 14-MAR-91 JWE MODIFY FOR NEW DCNPRO
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
C       THIS ROUTINE WILL EMUALTE THE SYSIO DRIVER INTERFACE ON THE CONCURRENT
C       SYSTEM AND REPLACE ITS FUNCTION USING DECNET LOGICAL LINKS.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
        SUBROUTINE SYSIO(PBLOCK, FUNCTION_CODE, NODE,
     *                   BUFFER, NBYTES, RANADD, BUFNUM)
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
        INCLUDE 'INCLIB:DCNEVN.DEF'
        INCLUDE 'INCLIB:DESNET.DEF'
        INCLUDE 'INCLIB:DN_LINK.DEF'                    ! DECNET STRUCTURES.
        INCLUDE 'INCLIB:DN_BLOCK.DEF'                   ! DECNET DATA BLOCKS.
        INCLUDE 'INCLIB:TASKID.DEF'
C
        INCLUDE '($SYSSRVNAM)'
C
C LOCAL DECLARATIONS.
C
        INTEGER*4       BUFNUM,                         ! BUF #,SYS FOR OPN/CLS.
     *                  BUF_NO,
     *                  FUNCTION_CODE,                  ! SVC1 FUNCTION CODE.
     *                  NBYTES,                         ! # OF BYTES TO XFER.
     *                  NODE,                           ! NODE.
     *                  PBLOCK(*),                      ! SVC1 PARAMETER BLOCK.
     *                  RANADD,                         ! LOGICAL REC #, UNUSED.
     *                  ST,
     *                  STATUS                          ! DECNET BUFFER/STATUS.
C
        BYTE            BUFFER                          ! BUFFER TO SEND.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
        PBLOCK(2) = %LOC(BUFFER)
        PBLOCK(3) = BUFNUM      
        PBLOCK(4) = NODE
        PBLOCK(5) = 0                                   ! NOT A READ.
        PBLOCK(7) = 1                                   ! ONLY ONE "WAY".
C
C GET DCN BUFFER FIRST
C
        CALL RTL(BUF_NO, DCN_FREE, STATUS)
C
        IF (STATUS .EQ. 2) THEN
          CALL OPS('*** SYSIO - DCN QUEUE EMPTY ***',
     *             FUNCTION_CODE, BUFNUM)
          CALL OPS('*** SYSIO - SOFTWARE ERROR ***', STATUS, NODE)
          GOTO 9999
        ENDIF
C
C OPEN PRIMARY/SECONDARY LINK REQUEST FUNCTION.
C
        IF (FUNCTION_CODE .EQ. 'A0'X .OR.
     *      FUNCTION_CODE .EQ. 'C0'X) THEN
          CALL DN_BUILD_BUFFER(DN_BUFFER(BUF_NO), FUNCTION_CODE,
     *                         NTL, 0, 0, PBLOCK, NODE)
          CALL ABL(BUF_NO, DCN_QUE, STATUS)             ! ADD BUF TO DCNPRO QUE.
C
C READ DATA REQUEST.
C
        ELSE IF (IAND(FUNCTION_CODE, '40'X) .NE. 0) THEN
          PBLOCK(5) = 1                                 ! IS A READ.
          CALL DN_BUILD_BUFFER(DN_BUFFER(BUF_NO), FUNCTION_CODE,
     *                         NTL, BUFFER, NBYTES, PBLOCK, NODE)
          CALL ABL(BUF_NO, DCN_QUE, STATUS)             ! ADD BUF TO DCNPRO QUE.
          DN_BUFFER(BUF_NO).DBUFFER_NO = BUFNUM
C
C WRITE DATA REQUEST.
C
        ELSE IF (IAND(FUNCTION_CODE, '20'X) .NE. 0) THEN
          CALL DN_BUILD_BUFFER(DN_BUFFER(BUF_NO), FUNCTION_CODE,
     *                         NTL, BUFFER, NBYTES, PBLOCK, NODE)
          CALL ABL(BUF_NO, NET_IOSTARTED(1,1,NODE), ST) ! ADD TO NETIOSTARTED Q.
          CALL ABL(BUF_NO, DCN_QUE, STATUS)             ! ADD BUF TO DCNPRO QUE.
          DN_BUFFER(BUF_NO).DBUFFER_NO = BUFNUM
C
C HALT REQUEST - ABORT AN EXISTING LINK.
C
        ELSE IF (FUNCTION_CODE .EQ. '80'X) THEN
          CALL DN_BUILD_BUFFER(DN_BUFFER(BUF_NO), FUNCTION_CODE,
     *                         NTL, BUFFER, NBYTES, PBLOCK, NODE)
          CALL ABL(BUF_NO, DCN_QUE, STATUS)             ! ADD BUF TO DCNPRO QUE.
C
C UNKNOWN REQUEST.
C
        ELSE
          CALL OPS('*** SYSIO - ILLEGAL FUNCTION CODE***',
     *             0, FUNCTION_CODE)
          CALL NET_FREEBUF(BUF_NO, STATUS)
          IF (STATUS .NE. 0)
     *      CALL OPS('*** SYSIO - UNABLE TO FREE BUF_NO ***',
     *               BUF_NO, STATUS)
        ENDIF
C
C WAKE UP DCNPRO.
C
        STATUS = SYS$SETEF(%VAL(DCN_EVENT))
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999    CONTINUE
C
        RETURN
        END
