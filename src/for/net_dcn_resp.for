C
C SUBROUTINE NET_DCN_RESP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFIP:[GOLS]NET_DCN_RESP.FOV                             $
C  $Date::   07 Jan 1997 13:03:52                                         $
C  $Revision::   1.1                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - net_dcn_resp.for ***
C
C V03 21-NOV-96 PJS CHANGED CALL TO DN_FREEBUF() -> NET_FREEBUF().
C V02 18-APR-91 JWE MODIFIED FOR NEW DCNPRO.
C V01 26-DEC-90 MRM INITIAL RELEASE.
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
C       THIS ROUTINE WILL HANDLE RESPONSES TO DATA RECEIVED FROM DCNPRO.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
        SUBROUTINE NET_DCN_RESP(BUFFER)
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:DCNEVN.DEF'
        INCLUDE 'INCLIB:DESNET.DEF'
        INCLUDE 'INCLIB:DN_LINK.DEF'
        INCLUDE 'INCLIB:DN_BLOCK.DEF'
        INCLUDE 'INCLIB:SYSTEMID.DEF'
C
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
C LOCAL DECLARATIONS
C
        INTEGER*4       PBLOCK(7, NETNUM + 1, NETLUN),  ! I/O PROCEED PARAM BLK
     *                  READ_BUFFER,
     *                  STATUS
C
C
C        CHARACTER*21    DN_ERRORS(0:DNE_READFAILED)
C     *                  /'INVALID Internal Error',
C     *                   'Zero Link Block',
C     *                   'Link In Wrong State',
C     *                   'No Data Buffer',
C     *                   'Queue Locked To Long',
C     *                   'Zero Length Buffer',
C     *                   'System Returned Error',
C     *                   'Buffer Pool Exusted',
C     *                   '$SETEF Failed',
C     *                   'Zero Channel #',
C     *                   'No Data Buffer',
C     *                   'Zero Length Buffer',
C     *                   'Unable To Get Buffer',
C     *                   'Read failed'/
C
        RECORD /DN_BUFFER_STRUCT/ BUFFER                ! BUFFER ARGUMENT
C
C COMMON DECLARATIONS
C
        COMMON /PBLOCKS/ PBLOCK
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C COMMAND TYPE BUFFER.
C
        IF (BUFFER.COMMAND .EQ. 'A0'X .OR.
     *      BUFFER.COMMAND .EQ. 'C0'X) THEN
          IF (BUFFER.IOSB.STAT .EQ. SS$_NORMAL .AND.
     *        BUFFER.AP_STATUS .EQ. DNE_SUCCESS) THEN
            NETROUT(BUFFER.LINK, 1) = ROUACT
            NETSTAT(BUFFER.LINK, 1) = ABS(NETSTAT(BUFFER.LINK, 1))
            NETOPN(BUFFER.LINK,  1) = BUFFER.LINK
            CALL OPS(CHAR(7) // '    *** CONNECTING TO ' //
     *               SYSID(BUFFER.LINK) // ' ***' // CHAR(7),
     *               BUFFER.LINK, 0)
          ENDIF
C
C DATA MESSAGE RECEIVED.
C
        ELSEIF (BUFFER.COMMAND .EQ. '40'X) THEN
          CALL DN_FILL_PBLOCK(%VAL(BUFFER.PBLOCK), BUFFER.IOSB.STAT,
     *                        BUFFER.AP_STATUS)
          CALL IOTRAP(%VAL(BUFFER.PBLOCK))
          READIOCHK(BUFFER.LINK) = READIOCHK(BUFFER.LINK) - 1
C
C DATA MESSAGE SENT.
C
        ELSEIF (BUFFER.COMMAND .EQ. '20'X) THEN
          CALL DN_FILL_PBLOCK(%VAL(BUFFER.PBLOCK), BUFFER.IOSB.STAT,
     *                        BUFFER.AP_STATUS)
          CALL IOTRAP(%VAL(BUFFER.PBLOCK))
          SNDIOCHK(BUFFER.LINK) = SNDIOCHK(BUFFER.LINK) - 1
C
C WE ABORTED ... MAKE SURE EVERONE KNOWS.
C
        ELSEIF (BUFFER.COMMAND .EQ. '80'X) THEN
          CALL OPS(CHAR(7) // '    *** ABORTING SYSTEM ' //
     *             SYSID(BUFFER.LINK) // ' ***' // CHAR(7),
     *             BUFFER.LINK, 0)
C
C WAIT.
C
        ELSEIF (BUFFER.COMMAND .EQ. '08'X) THEN
C
C TIMEOUT MESSAGE RECEIVED.
C
        ELSEIF (BUFFER.COMMAND .EQ. '13'X) THEN
          CALL IOTRAP(BUFFER.LOCAL_PBLOCK)
C
C INVALID BUFFER.
C
        ELSE
          CALL OPS('*** NET_DCN_RESP - UNKNOWN DCNPRO TYPE ***',
     *             0, BUFFER.COMMAND)
        ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C QUEUE SOME READS FOR NODE IF NEEDED AND NODE NOT IN ERROR STATE
C
        IF (BUFFER.IOSB.STAT .EQ. SS$_NORMAL .AND.
     *      BUFFER.AP_STATUS .EQ. DNE_SUCCESS .AND.
     *      READIOCHK(BUFFER.LINK) .LT. MAX(NETNUM/NETSYS/2, 2)) THEN
C
100       CONTINUE
          CALL GRABBUF(READ_BUFFER, 1, STATUS)
          IF (STATUS .EQ. GLIST_STAT_GOOD .OR.
     *        STATUS .EQ. GLIST_STAT_LASTONE) THEN
            CALL SYSIO(PBLOCK(1, READ_BUFFER, BUFFER.LINK),
     *                 '40'X,
     *                 BUFFER.LINK,
     *                 NETBUF(NCNLEN + 1, READ_BUFFER),
     *                 (NETLEN - NCNLEN) * 4 - 2,
     *                 0,
     *                 READ_BUFFER)
            READIOCHK(BUFFER.LINK)=READIOCHK(BUFFER.LINK)+1
C
          ELSE
            GOTO 200                                    ! NO BUFFERS, TRY LATER
          ENDIF
C
          IF (READIOCHK(BUFFER.LINK) .LT. MAX(NETNUM/NETSYS/2, 2))
     *      GOTO 100
        ENDIF
C
C FREE THE BUFFER HEADER
C
200     CONTINUE
        IF (BUFFER.IOSB.STAT .NE. SS$_NORMAL) THEN
          IF (NETROUT(BUFFER.LINK, 1) .EQ. ROUACT) THEN
D           CALL SYS$GETMSG(%VAL(INT(MIN(BUFFER.IOSB.STAT, 999999))),
D    *                      ERROR_LENGTH,
D    *                      ERROR_TEXT,,)
D           CALL OPS(CHAR(7) // ERROR_TEXT // CHAR(7),
D    *               INT(BUFFER.IOSB.STAT), 2)
C
C ON PRIMARY LINK CLOSE LINKS DO NOT CLOSE ON READS ... WRITE WILL DO IT
C
            IF (BUFFER.COMMAND .NE. '80'X .AND.
     *          BUFFER.COMMAND .NE. '20'X .AND.
     *          BUFFER.COMMAND .NE. '13'X) THEN
              IF (NETROUT(BUFFER.LINK, 1) .EQ. ROUACT) THEN
                IF (BUFFER.COMMAND .NE. '40'X .OR.
     *              (BUFFER.COMMAND .EQ. '40'X .AND. 
     *               NETSTAT(BUFFER .LINK, 1) .EQ. NSTASEC)) THEN
D                 CALL OPS('*** NET_DCN_RESP - ' //
D    *                     'ATTEMPT TO REMOVE SYSTEM ***',
D    *                     BUFFER.LINK, BUFFER.COMMAND)
                  CALL DN_CLOSE_LINK(BUFFER.LINK)
                ENDIF 
              ENDIF
            ENDIF
          ENDIF
C
        ELSEIF (BUFFER.AP_STATUS .NE. DNE_SUCCESS) THEN
          IF (NETROUT(BUFFER.LINK, 1) .EQ. ROUACT) THEN
            IF (BUFFER.AP_STATUS .LT. 0 .OR.
     *          BUFFER.AP_STATUS .GT. DNE_READFAILED) THEN
D             CALL OPS('*** NET_DCN_RESP - ' //
D    *                 'DCNPRO AP_STATUS ***',
D    *                 BUFFER.AP_STATUS, 0)
            ELSE
D             CALL OPS('*** NET_DCN_RESP *** ' //
D    *                 DN_ERRORS(BUFFER.AP_STATUS),
D    *                 BUFFER.AP_STATUS, 0)
            ENDIF
          ENDIF 
C
C ON PRIMARY LINK CLOSE LINKS DO NOT CLOSE ON READS ... WRITE WILL DO IT
C
          IF (BUFFER.COMMAND .NE. '80'X .AND. 
     *        BUFFER.COMMAND .NE. '20'X .AND.
     *        BUFFER.COMMAND .NE. '13'X) THEN
            IF (NETROUT(BUFFER.LINK, 1) .EQ. ROUACT) THEN
              IF (BUFFER.COMMAND .NE. '40'X .OR.
     *            (BUFFER.COMMAND .EQ. '40'X .AND. 
     *             NETSTAT(BUFFER.LINK, 1) .EQ. NSTASEC)) THEN
D               CALL OPS('*** NET_DCN_RESP - REMOVING SYSTEM ***',
D    *                   BUFFER.LINK, BUFFER.COMMAND)
                CALL DN_CLOSE_LINK(BUFFER.LINK)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
C
C FREE THE BUFFER.
C
        CALL NET_FREEBUF(BUFFER.BUF_NO, STATUS)
C
D       IF (STATUS .NE. 0)
D    *    CALL OPS('*** NET_DCN_RESP ***', BUFFER.COMMAND, BUFFER.LINK)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
        RETURN
        END
