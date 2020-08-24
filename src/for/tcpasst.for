C TCPASST.FOR
C
C V20 10-OCT-2001 OXK Verify validity of IPS IPs before trying to use them
C V19 13-JUN-2000 OXK TC_EVNMASK as a local variable (used only in one subr)
C V18 08-JUN-2000 OXK Removed NETBYT here as it is anyway a independent module
C                       + removed a bunch of non-used variables
C V17 15 Jan 1997 HXK Removed temporary assignment of TCP IP address
C V16 06 Jan 1997 HXK Temporarily changed tcp prefix and suffix for Veikkaus 
C                       test purposes
C V15 05 Dec 1996 HXK Updated for Finland IPS pre-release
C V14 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C                       Instant Pass Thru Phase 1
C V13 18 Jan 1995 JJOLY UPDATED LOGIC THAT CHANGES PORTS TO SET PRIMARY PORT #
C V12 10 Jan 1995 ITSD ADDED LOGIC TO ALLOW RECONNECT AFTER BAD LENGTH FROM 
C                       INSTANT SYSTEM
C V11 27 Dec 1994 DJO Updates as a result of shakedown testing for install 
C                       system.
C V10 15 Dec 1994 DJO Changed the usage of the new TCP_ parameters from BLDSYS.
C V09 06 Dec 1994 DJO Changed to support the TCPASST parameters in BLDSYS.
C V08 24 Sep 1994 MCM DEFINE THE PORTS USED TO BE 5300-GBLMSA AND 5400-GBLMSB
C V07 12 Sep 1994 MCM MODIFIED THE ADDRESS AND PORT FOR THE ONLINE SYSTEMS
C V06 21 Jun 1994 MCM CHANGED STRATUS TO INSTANT SYSTEM
C V05 03 Jan 1994 SYSTEM Applying PVCS header for automatic revision history
C V04 21 Dec 1993 SYSTEM Initial revision.
C V03 07-Oct-1992 ceb Removed opening message.
C V02 08-APR-1992 JPJ UPDATED TO SWITCH TO BACKUP IP
C V01 20-NOV-1991 KWP INITIAL RELEASE FOR VAX
C
C This program is used to transfer buffers via a TCP/IP Link.
C
C This program contains the following subroutines:
C
C TCP_TSKTRAP:
C               This routine is called via the DCLAST( declare AST) call in
C               the main line of TCPASST. There are several event flags that
C               other programs( CRSPRO, TCPCNTRL) can set via the TCPQUEUE
C               routine. Once one of these event flags are set, the TCPASST
C               will DCLAST to this routine. This routine will call one of
C               several routines based on the parameter passed into the
C               routine.
C
C 
C               This routine will start a timer trap for a specified amount
C               of time. There are 5 different types of timer traps. Only
C               one timer trap can be outstanding for each different type.
C               If a timer trap is currently outstanding for a certain type
C               and somebody wants to start another one for the same type then
C               the 2nd one will be ignored. Once the timer trap completes the
C               program will trap to the TCP_TIMTRAP routine.
C
C TCP_TIMTRAP:
C               This routine will be called once a timer trap completes. The
C               type of timer trap will be passed into this routine. Based on
C               the type of timer trap, the routine will call one of several
C               routines.
C
C TCP_CHEKREAD:
C               This routine will get a TCP buffer and attempt to start
C               a read( using the DOREAD routine) for each read that can be
C               outstanding. If there is no connection then the routine will
C               start a timer trap to connect.
C
C TCP_DOREAD:
C               This routine will start a read( using QIO) with the passed
C               in TCP buffer. Once the read is complete the program will trap
C               to the TCP_RDIOCOMP routine.
C
C TCP_RDIOCOMP:
C               This routine will be called once a read completes. If a read
C               completes with a good status then another read will be started(
C               using the DOREAD routine). The buffer status in the TCP header
C               will be set to TCGODRED for good reads. The TCP buffer will be
C               added to the RCVQUE. If a read completes with an error then an
C               error message will be printed. The buffer status will be set
C               to TCBADRED. If the connection status is currently connected 
C               then the routine will force a disconnect by calling TCP_DODISC. 
C               In this case the buffer status will be set to TCDISRED. The
C               TCP buffer will be added to the RCVQUE.
C
C TCP_DOWRIT:
C               This routine will start a write( using QIO) if there is a
C               connection, a buffer on the SNDQUE and the number of outstanding
C               writes has not exceeded the maximum. Once the write is complete
C               the program will trap to the TCP_WRIOCOMP routine. If there is
C               no connection then the routine will remove all buffers on the
C               SNDQUE and add them to the RCVQUE. The buffer status in the
C               TCP header will be set to TCNOCWRT.
C
C TCP_WRIOCOMP:
C               This routine will be called once a write completes. If a write
C               completes with a good status then another write will be started
C               using the DOWRIT routine. The buffer status in the TCP header
C               will be set to TCGODWRT for good writes. The TCP buffer will be
C               added to the RCVQUE. If a write completes with an error then an
C               error message will be printed. The buffer status will be set
C               to TCBADWRT. If the connection status is currently connected 
C               then the routine will force a disconnect by calling TCP_DODISC. 
C               In this case the buffer status will be set to TCDISWRT. The
C               TCP buffer will be added to the RCVQUE.
C
C TCP_DOACONN:
C               This routine will attempt to do an active connection to the
C               other system. There are several steps to an active connection.
C               The 1st step is to assign a channel into UCX using a SYS$ASSIGN.
C               The 2nd step is to create and bind the socket to the local host.
C               This is done by issuing a QIOW with a function code as SETMODE.
C               The 3rd step is to get the local socket name. This is done by 
C               issuing a QIOW with a function code as SENSEMODE. The 4th step
C               is to get the peer socket name. This is done by issuing a QIOW
C               with a function code as SENSEMODE. The final step is to define
C               the peer socket name and address and connect. This is done by
C               issuing a QIO with a function code as ACCESS. Once the access
C               is complete the program will trap to the TCP_COIOCOMP routine.
C               If there are any errors with the QIOW then an error message will
C               be printed. The channel to UCX will be deassigned using the
C               TCP_DODASGN routine. A timer trap will be started to attempt to
C               connect again in 10 seconds( TCP_CONWAIT parameter).
C
C TCP_COIOCOMP:
C               This routine will be called once the ACCESS completes. If the
C               connection completes with a good status then the connection
C               status will be set to TCCONN( connected). The routine will
C               attempt to hang reads on the line using the TCP_CHEKREAD
C               routine. If the connection completes with an error then the
C               connection status will be set to TCDISCON( disconnected). The
C               channel to UCX will be deassigned using the TCP_DODASGN routine.
C               A timer trap will be started to attempt to connect again in
C               10 seconds( TCP_CONWAIT parameter).
C
C TCP_DOPCONN:
C               This routine will attempt to do an passive connection to the
C               other system. This routine is included in TCPASST for testing
C               purposes only. The routine should be changed to use a QIO
C               without wait if the routine will ever be used for live passive
C               connections
C
C TCP_DODISC:
C               This routine will attempt to disconnect. There are several
C               steps to a disconnection. The 1st step is to shutdown the local
C               socket. This is done by issuing a QIOW with a function code
C               of DEACCESS and SHUTDOWN. The 2nd step is to close the local
C               socket. This is done by issuing a QIOW with a function code
C               of DEACCESS. The 3rd step is to deassign the channel to UCX.
C               This is done by calling the TCP_DODASGN routine. Once the
C               disconnection has been completed a timer trap will be started
C               to attempt to connect again. The timer will be 250 milliseconds
C               ( TCP_DISWAIT parameter).
C               
C TCP_DODASGN:
C               This routine will deassign any channels into UCX. To do this
C               the SYS$DASSGN service is called.
C
C TCP_STOP:
C               This routine will attempt to disconnect using TCP_DODISC if
C               there is an active connection. The program will then stop.
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
C Copyright 1991- GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM TCPASST
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:CRSCOM.DEF'
        INCLUDE 'INCLIB:TCPEVN.DEF'
C
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
        INTEGER*4       TC_EVNMASK              !BITMAP OF ALL EVENTS SET

        INTEGER*4   I                   !Work variables
        INTEGER*4   STATUS              !Event status
        INTEGER*4   FLGSTS              !Status
C
        INTEGER*4   NOFTLSIG
        EXTERNAL    NOFTLSIG
C
        EXTERNAL    TCP_TSKTRAP
C
        CALL COPYRITE
C
        CALL LIB$ESTABLISH(NOFTLSIG)        !No fatal errors
C
        CALL SNIF_AND_WRKSET
C
C CREATE THE COMMON EVENT FLAG CLUSTER.
C
        STATUS=SYS$ASCEFC(%VAL(TC_EVNTIMER),TC_EVNNAME,0,0)
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
C CREATE THE EVENT FLAG MASK OF EVENTS FOR WHICH
C TO TRAP ON.
C
        DO 100 I=1,TC_MAXQUE
          TC_EVNMASK = IBSET(TC_EVNMASK, MOD(TC_EVNQUE(I),32))
100     CONTINUE
C
C CLEAR ALL EVENT FLAGS.
C
        DO 200 I=1,TC_MAXQUE
          STATUS=SYS$CLREF(%VAL(TC_EVNQUE(I)))
200      CONTINUE
C
C CLEAR OUT ALL VARIABLES USED
C
        TCP_CONNSTS=TCDISCON
        TCP_CONTYPE=TCACTV
        TCP_CONNOVR=TCOKAY
        TCP_CONWAIT=10000           !WAIT 10 SECS TO CONNECT
        TCP_DISWAIT=2000            !WAIT 250 MS TO CONNECT AFTER DISCONNECT
C
        TCP_DEBUG = 0       !ASSUME NO DEBUGGING NEEDED
        TCP_REDIGNORE = 0
C
        TCP_READS = 0
        TCP_READERRS = 0
        TCP_READLERR = 0
        TCP_WRITES = 0
        TCP_WRITEERRS = 0
        TCP_WRITENOCS = 0
        TCP_WRITELERR = 0
        TCP_CONNECTS = 0
        TCP_CONNERRS = 0
        TCP_CONNLERR = 0
        TCP_DISCONNS = 0
        TCP_DISCERRS = 0
        TCP_DISCLERR = 0
        TCP_IP_CNT   = 0
C
        DO 300 I=1,TCMAXREADS
          TCP_READSOUT(I)=TCREADY
          TCP_READTBUF(I)=0
300     CONTINUE
C
        DO 400 I=1,TCMAXWRITES
          TCP_WRITESOUT(I)=TCREADY
          TCP_WRITETBUF(I)=0
400     CONTINUE
C
        DO 500 I=1,TCMAXTIMTRAPS
          TCP_TIMEINMS(I)   = 0
          TCP_TIMEDELY(1,I) = 0
          TCP_TIMEDELY(2,I) = 0
          TCP_TIMEINPR(I)   = TCREADY
500     CONTINUE
C
        TCP_CHANNEL1 = -1
        TCP_CHANNEL2 = -1
C
        TCP_REMPRT = TCP_PORTS(1,1)
        TCP_LOCPRT = TCP_PORTS(1,2)
        
        TCP_B_REMADR(1) = TCP_PREFIX(1)
        TCP_B_REMADR(2) = TCP_PREFIX(2)
C
C
C PLACE TASK IN TRAP WAIT STATE. NOTE: TASK WILL STILL
C SERVICE AST TRAPS WHILE WAITING FOR EVENT FLAGS TO BE SET.
C
C ====================== MAIN PROCESSING =====================
C
1000    CONTINUE
        STATUS=SYS$WFLOR(%VAL(TC_EVNTIMER),%VAL(TC_EVNMASK))
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
C CHECK FOR CRSPRO TASK TRAP.
C
        DO 1100 I=1,TC_MAXQUE
          STATUS=SYS$READEF(%VAL(TC_EVNQUE(I)),FLGSTS)
          IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
          IF(STATUS.EQ.SS$_WASSET) THEN
            STATUS=SYS$CLREF(%VAL(TC_EVNQUE(I)))
            IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
            STATUS=SYS$DCLAST(TCP_TSKTRAP,I,)    !TRAP TO TTSKTRAP
            IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
            GOTO 1000
          ENDIF
1100    CONTINUE
C
        GOTO 1000
        END
C
C*****************************
C 
C       SUBROUTINE TCP_TSKTRAP
C
C*****************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TCP_TSKTRAP(PARAM)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:CRSCOM.DEF'
        INCLUDE 'INCLIB:TCPEVN.DEF'
C
        INTEGER*4   PARAM               !Parameter passed into routine
C
        CHARACTER*132   C_MESBUF
        INTEGER*4       MESBUF(33)
        EQUIVALENCE    (C_MESBUF,MESBUF)
C
        IF(PARAM.EQ.BEGASST) THEN
          CALL TCP_CHEKREAD
C
        ELSE IF(PARAM.EQ.SNDASST) THEN
          CALL TCP_DOWRIT
C
        ELSE IF(PARAM.EQ.ENDASST) THEN
          CALL TCP_STOP
C
        ELSE IF(PARAM.EQ.ACONASST) THEN
          CALL TCP_DOACONN
C
        ELSE IF(PARAM.EQ.PCONASST) THEN
          CALL TCP_DOPCONN
C
        ELSE IF(PARAM.EQ.DISASST) THEN
          CALL TCP_DODISC
C
        ENDIF
C
        RETURN
        END
C
C*****************************
C 
C       SUBROUTINE TCP_STARTTIME
C
C*****************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TCP_STARTTIME(TIMETYPE, TIMEINMS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:CRSCOM.DEF'
        INCLUDE 'INCLIB:TCPEVN.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SYSSRVNAM)'
C
        INTEGER*4 TIMETYPE          !TYPE OF TIMER TRAP
        INTEGER*4 TIMEINMS          !TIMER TRAP LENGTH
        INTEGER*4 STATUS            !STATUS FROM SYSTEM CALL
        CHARACTER CURTIM*30         !ASCII TIME
C
        CHARACTER*132   C_MESBUF
        INTEGER*4       MESBUF(33)
        EQUIVALENCE    (C_MESBUF,MESBUF)
C
        INTEGER*4       BLANK
        DATA            BLANK/'    '/
C
C
        EXTERNAL TCP_TIMTRAP
C
        IF(TCP_DEBUG.NE.0) THEN
          TYPE *,IAM(),'TCP_STARTTIME:  ',TIMETYPE, TIMEINMS
        ENDIF
C
C VERIFY THAT THIS IS AN OKAY TIMER TRAP TYPE
C
        IF(TIMETYPE.LT.1 .OR. TIMETYPE.GT.TCMAXTIMTRAPS) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9000) IAM(),TIMETYPE
          CALL WRITEBRK(C_MESBUF)
          RETURN
        ENDIF
C
C IF THERE IS ALREADY A TIMER TRAP OF THIS TYPE IN PROGRESS, IGNORE
C
        IF(TCP_TIMEINPR(TIMETYPE).EQ.TCINPROG) RETURN
C
C
C IF THE DELAY TIME IS DIFFERENT THEN THE LAST TIMER TRAP OF THIS
C TYPE, THEN RECONVERT THE TIME
C
        IF(TCP_TIMEINMS(TIMETYPE).NE.TIMEINMS)THEN
          WRITE(CURTIM,9100) TIMEINMS/1000,MOD(TIMEINMS,1000)
          STATUS=SYS$BINTIM(CURTIM,TCP_TIMEDELY(1,TIMETYPE))
          IF(.NOT.STATUS) THEN
            CALL FASTSET(BLANK,MESBUF,33)
            WRITE(C_MESBUF,9200) IAM(),TIMEINMS,STATUS
            CALL WRITEBRK(C_MESBUF)
            RETURN
          ENDIF
          TCP_TIMEINMS(TIMETYPE)=TIMEINMS
        ENDIF
C
C
C ENABLE TIMER TRAP
C
        STATUS=SYS$SETIMR(,TCP_TIMEDELY(1,TIMETYPE),
     *                     TCP_TIMTRAP,TIMETYPE,0)
        IF(.NOT.STATUS) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9300) IAM(),TIMETYPE,STATUS
          CALL WRITEBRK(C_MESBUF)
          RETURN
        ENDIF
C
C SET THE TYPE TO HAVING A TIMER TRAP IN PROGRESS
C
        TCP_TIMEINPR(TIMETYPE) = TCINPROG
C
        RETURN
C
9000    FORMAT(A,'TCP_STARTTIME: Invalid TC Buffer  # ',I8)
9100    FORMAT('0000 00:00:',I2.2,'.',I3.3)
9200    FORMAT(A,'TCP_STARTTIME: Error converting Time ',I8,
     *              ' Status ',I8)
9300    FORMAT(A,'TCP_STARTTIME: Error starting Timer Trap ',I8,
     *              ' Status ',I8)
        END
C
C*****************************
C 
C       SUBROUTINE TCP_TIMTRAP
C
C*****************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TCP_TIMTRAP(TIMETYPE)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:CRSCOM.DEF'
        INCLUDE 'INCLIB:TCPEVN.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
        INTEGER*4 TIMETYPE              !TIMER TRAP TYPE
C
        CHARACTER*132   C_MESBUF
        INTEGER*4       MESBUF(33)
        EQUIVALENCE    (C_MESBUF,MESBUF)
C
        IF(TCP_DEBUG.NE.0) THEN
          TYPE *,IAM(),'TCP_TIMTRAP:  ',TIMETYPE
        ENDIF
C
        IF(TIMETYPE.GT.1 .AND. TIMETYPE.LE.TCMAXTIMTRAPS) THEN
          TCP_TIMEINPR(TIMETYPE) = TCREADY
        ENDIF
C
        IF(TIMETYPE.EQ.1) THEN
          CALL TCP_CHEKREAD
C
        ELSE IF(TIMETYPE.EQ.2) THEN
          CALL TCP_DOWRIT
C
        ELSE IF(TIMETYPE.EQ.3) THEN
          CALL TCP_STOP
C
        ELSE IF(TIMETYPE.EQ.4) THEN
          CALL TCP_DOACONN
C
        ELSE IF(TIMETYPE.EQ.5) THEN
          CALL TCP_DOPCONN
C
        ENDIF
C
        RETURN
        END
C
C*****************************
C 
C       SUBROUTINE TCP_CHEKREAD
C
**************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TCP_CHEKREAD
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:CRSCOM.DEF'
        INCLUDE 'INCLIB:TCPEVN.DEF'
C
        INTEGER*4 RIO           !READ IOSB COUNTER
        INTEGER*4 TBUF          !TCBUF #
        INTEGER*4 IERR          !RTL ERROR STATUS
        INTEGER*4 CONWAIT       !WAIT TIME
C
        CHARACTER*132   C_MESBUF
        INTEGER*4       MESBUF(33)
        EQUIVALENCE    (C_MESBUF,MESBUF)
C
        IF(TCP_DEBUG.NE.0) THEN
          TYPE *,IAM(),'TCP_CHEKREAD:  '
        ENDIF
C
        IF(TCP_CONNSTS.NE.TCCONN) THEN
          IF(TCP_CONNSTS.EQ.TCDISCON) THEN
            CONWAIT=TCP_CONWAIT
            IF(TCP_CONTYPE.EQ.TCACTV) THEN
              CALL TCP_STARTTIME(4,CONWAIT)
            ELSE
              CALL TCP_STARTTIME(5,CONWAIT)
            ENDIF
          ENDIF
          GOTO 10000
        ENDIF
C
        DO 100 RIO=1,TCMAXREADS
          IF(TCP_READSOUT(RIO).EQ.TCINPROG) GOTO 100
C
          CALL RTL(TBUF,FREQUE,IERR)
          IF(IERR.EQ.2) THEN        !NO FREE BUFFERS
            CALL TCP_STARTTIME(1,100)
            RETURN
          ELSE
            CALL TCP_DOREAD(RIO,TBUF)
          ENDIF
100     CONTINUE
C
C
10000   CONTINUE
        RETURN
        END
C
C*****************************
C 
C       SUBROUTINE TCP_DOREAD
C
C*****************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TCP_DOREAD(RIO, TBUF)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:CRSCOM.DEF'
        INCLUDE 'INCLIB:TCPEVN.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
        INTEGER*4 RIO           !READ IOSB COUNTER
        INTEGER*4 TBUF          !TCBUF #
        INTEGER*4 IERR          !RTL ERROR STATUS
        INTEGER*4 STATUS        !STATUS RETURNED FROM QIO
        INTEGER*4 IFUNC         !FUNCTION CODE
C
        CHARACTER*132   C_MESBUF
        INTEGER*4       MESBUF(33)
        EQUIVALENCE    (C_MESBUF,MESBUF)
C
        INTEGER*4       BLANK
        DATA            BLANK/'    '/
C
        EXTERNAL  TCP_RDIOCOMP
C
        IF(TCP_DEBUG.NE.0) THEN
          TYPE *,IAM(),'TCP_DOREAD:  ',RIO,TBUF
        ENDIF
C
        IF(TBUF.LT.1 .OR. TBUF.GT.TCBUFMAX) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9000) IAM(),TBUF
          CALL WRITEBRK(C_MESBUF)
          RETURN
        ENDIF
C
        IF(RIO.LT.1 .OR. RIO.GT.TCMAXREADS) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9100) IAM(),RIO
          CALL WRITEBRK(C_MESBUF)
          CALL ABL(TBUF,FREQUE,IERR)
          RETURN
        ENDIF
C
        IF(TCP_READSOUT(RIO).EQ.TCINPROG) THEN
          CALL ABL(TBUF,FREQUE,IERR)
          RETURN
        ENDIF
C
C       HANG A READ
C
C       VERIFY LENGTH TO SEND IS VALID
C
        TCP_READSOUT(RIO) = TCINPROG
        TCP_READTBUF(RIO) = TBUF
        TCBUF(TCSTSCNT,TBUF) = RIO
C
        IFUNC=IO$_READVBLK
        STATUS=SYS$QIO(,%VAL(TCP_CHANNEL2),
     *                  %VAL(IFUNC),
     *                  %REF(TCP_READ_IOSB(RIO)),
     *                  TCP_RDIOCOMP,
     *                  TCBUF(TCSTSCNT,TBUF),
     *                  %REF(TCBUF(TCBUFBEG,TBUF)),
     *                  %VAL(MSGLEN),
     *                  ,,,)
C
        IF(.NOT.STATUS) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9200) IAM(),STATUS
          CALL WRITEBRK(C_MESBUF)
          TCP_READSOUT(RIO) = TCREADY 
          CALL ABL(TBUF,FREQUE,IERR)
          RETURN
        ENDIF
C
C
        RETURN
9000    FORMAT(A,'TCP_DOREAD: Invalid TC Buffer  # ',I8)
9100    FORMAT(A,'TCP_DOREAD: Invalid RIO Buffer  # ',I8)
9200    FORMAT(A,'TCP_DOREAD: Error starting Read ',I4,'  Status ',I8)
C
        END
C
C*****************************
C 
C       SUBROUTINE TCP_RDIOCOMP
C
C*****************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TCP_RDIOCOMP(RIO)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:CRSCOM.DEF'
        INCLUDE 'INCLIB:TCPEVN.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
        INTEGER*4 RIO           !READ IOSB COUNTER
        INTEGER*4 TBUF          !TCBUF #
        INTEGER*4 IERR          !RTL ERROR STATUS
C
        CHARACTER*132   C_MESBUF
        INTEGER*4       MESBUF(33)
        EQUIVALENCE    (C_MESBUF,MESBUF)
C
        INTEGER*4       BLANK
        DATA            BLANK/'    '/
C
C
C
        IF(TCP_DEBUG.NE.0) THEN
          TYPE *,IAM(),'TCP_RDIOCOMP:  RIO ',RIO
        ENDIF
C
        IF(RIO.LT.1 .OR. RIO.GT.TCMAXREADS) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9000) IAM(),RIO
          CALL WRITEBRK(C_MESBUF)
          RETURN
        ENDIF
C
        TBUF=TCP_READTBUF(RIO)
        IF(TCP_DEBUG.NE.0) THEN
          TYPE *,IAM(),'TCP_RDIOCOMP:  TBUF ',TBUF
        ENDIF
C
        IF(TBUF.LT.1 .OR. TBUF.GT.TCBUFMAX) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9100) IAM(),TBUF
          CALL WRITEBRK(C_MESBUF)
          RETURN
        ENDIF
C
        TCP_READSOUT(RIO)=TCREADY
C
        IF(TCP_READ_IOSB(RIO).STAT .EQ. SS$_NORMAL) THEN
          IF(TCP_DEBUG.NE.0) THEN
            TYPE *,IAM(),'TCP_RDIOCOMP : GOOD READ'
          ENDIF
          TCP_READS = TCP_READS + 1
          TCBUF(TCBUFSTS,TBUF) = TCGODRED
          TCBUF(TCBUFLEN,TBUF) = TCP_READ_IOSB(RIO).XSIZE
          TCBUF(TCBUFERR,TBUF) = 0
          CALL ABL (TBUF,RCVQUE,IERR)
C
          CALL RTL (TBUF,FREQUE,IERR)
          IF(IERR.EQ.2) THEN        !NO BUFFERS AVAILABLE
            CALL TCP_STARTTIME(1,500)
          ELSE
            CALL TCP_DOREAD(RIO,TBUF)
          ENDIF
        ELSE
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9200) IAM(),TCP_READ_IOSB(RIO).STAT
          CALL WRITEBRK(C_MESBUF)
          TCP_READERRS = TCP_READERRS + 1
          TCP_READLERR = TCP_READ_IOSB(RIO).STAT
          TCBUF(TCBUFSTS,TBUF) = TCBADRED
          TCBUF(TCBUFLEN,TBUF) = TCP_READ_IOSB(RIO).XSIZE
          TCBUF(TCBUFERR,TBUF) = TCP_READ_IOSB(RIO).STAT
          IF(TCP_CONNSTS.EQ.TCCONN) THEN
            IF(TCP_DEBUG.NE.0) THEN
              TYPE *,IAM(),
     *               'TCP_RDIOCOMP :SETTING CONNECT FLAG TO DISCONNECTED'
            ENDIF
            TCBUF(TCBUFSTS,TBUF) = TCDISRED
            CALL ABL (TBUF,RCVQUE,IERR)
            CALL TCP_DODISC
          ELSE
            CALL ABL (TBUF,RCVQUE,IERR)
          ENDIF
        ENDIF
C
C
        RETURN
9000    FORMAT(A,'TCP_RDIOCOMP: Invalid RIO Buffer  # ',I8)
9100    FORMAT(A,'TCP_RDIOCOMP: Invalid TC Buffer  # ',I8)
9200    FORMAT(A,'TCP_RDIOCOMP: Error completing read ',I8)
C
        END
C
C*****************************
C 
C       SUBROUTINE TCP_DOWRIT
C
C*****************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TCP_DOWRIT
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:CRSCOM.DEF'
        INCLUDE 'INCLIB:TCPEVN.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
        INTEGER*4 WIO           !WRITE IOSB COUNTER
        INTEGER*4 TBUF          !TCBUF #
        INTEGER*4 IERR          !RTL ERROR STATUS
        INTEGER*4 STATUS        !STATUS RETURNED FROM QIO
        INTEGER*4 IFUNC         !FUNCTION CODE
C
        CHARACTER*132   C_MESBUF
        INTEGER*4       MESBUF(33)
        EQUIVALENCE    (C_MESBUF,MESBUF)
C
        INTEGER*4       BLANK
        DATA            BLANK/'    '/
C
        EXTERNAL  TCP_WRIOCOMP
C
        IF(TCP_CONNSTS.NE.TCCONN) THEN
100       CONTINUE
          CALL RTL(TBUF,SNDQUE,IERR)
          IF(IERR.EQ.2) GOTO 10000          !NO BUFFERS TO SEND
C
          IF(TCP_DEBUG.NE.0) THEN
            TYPE *,IAM(),'TCP_DOWRIT: BUFSTS SET TO TCNOCWRT   TBUF=',TBUF
          ENDIF
C
          TCP_WRITENOCS = TCP_WRITENOCS + 1
          TCBUF(TCBUFSTS,TBUF) = TCNOCWRT
          TCBUF(TCBUFERR,TBUF) = 0
          CALL ABL (TBUF,RCVQUE,IERR)
          GOTO 10000
        ENDIF
C
        DO 200 WIO=1,TCMAXWRITES
          IF(TCP_WRITESOUT(WIO).EQ.TCINPROG) GOTO 200
C
C           TAKE A BUFFER OFF OF THE SEND QUEUE
C
          CALL RTL(TBUF,SNDQUE,IERR)
          IF(IERR.EQ.2) GOTO 10000          !NO BUFFERS TO SEND
C
C         OKAY SEND A BUFFER
C
C         VERIFY LENGTH TO SEND IS VALID
C
          IF(TCBUF(TCBUFLEN,TBUF).GT.MSGLEN .OR.
     *       TCBUF(TCBUFLEN,TBUF).LT.1) THEN
            CALL FASTSET(BLANK,MESBUF,33)
            WRITE(C_MESBUF,9000) IAM(), TBUF, TCBUF(TCBUFLEN,TBUF)
            CALL WRITEBRK(C_MESBUF)
            TCP_WRITENOCS = TCP_WRITENOCS + 1
            TCBUF(TCBUFSTS,TBUF) = TCNOCWRT
            TCBUF(TCBUFERR,TBUF) = 0
            CALL ABL(TBUF,RCVQUE,IERR)
            GOTO 10000
          ENDIF
C
          TCP_WRITESOUT(WIO) = TCINPROG
          TCP_WRITETBUF(WIO) = TBUF
          TCBUF(TCSTSCNT,TBUF) = WIO
C
          IFUNC=IO$_WRITEVBLK
          STATUS=SYS$QIO(,%VAL(TCP_CHANNEL2),
     *                    %VAL(IFUNC),
     *                    %REF(TCP_WRITE_IOSB(WIO)),
     *                    TCP_WRIOCOMP,
     *                    TCBUF(TCSTSCNT,TBUF),
     *                    %REF(TCBUF(TCBUFBEG,TBUF)),
     *                    %VAL(TCBUF(TCBUFLEN,TBUF)),
     *                    ,,,)
C
          IF(.NOT.STATUS) THEN
            CALL FASTSET(BLANK,MESBUF,33)
            WRITE(C_MESBUF,9100) IAM(),TBUF,STATUS
            CALL WRITEBRK(C_MESBUF)
            TCP_WRITEERRS = TCP_WRITEERRS + 1
            TCP_WRITELERR = STATUS
            TCBUF(TCBUFSTS,TBUF) = TCBADWRT
            TCBUF(TCBUFERR,TBUF) = STATUS
            TCP_WRITESOUT(WIO) = TCREADY 
            CALL ABL(TBUF,RCVQUE,IERR)
            RETURN
          ENDIF
200     CONTINUE
C
C
C
10000   CONTINUE
        RETURN
C
9000    FORMAT(A,'TCP_DOWRIT: Invalid Length in TC Buffer ',I4,
     *              '  Length ',I8)
9100    FORMAT(A,'TCP_DOWRIT: Error starting Write ',I4,
     *              '  Status ',I8)
        END
C
C*****************************
C 
C       SUBROUTINE TCP_WRIOCOMP
C
C*****************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TCP_WRIOCOMP(WIO)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:CRSCOM.DEF'
        INCLUDE 'INCLIB:TCPEVN.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
        INTEGER*4 WIO           !WRITE IOSB COUNTER
        INTEGER*4 TBUF          !TCBUF #
        INTEGER*4 IERR          !RTL ERROR STATUS
C
        CHARACTER*132   C_MESBUF
        INTEGER*4       MESBUF(33)
        EQUIVALENCE    (C_MESBUF,MESBUF)
C
        INTEGER*4       BLANK
        DATA            BLANK/'    '/
C
C
C
        IF(TCP_DEBUG.NE.0) THEN
          TYPE *,IAM(),'TCP_WRIOCOMP:  WIO ',WIO
        ENDIF
C
        IF(WIO.LT.1 .OR. WIO.GT.TCMAXWRITES) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9000) IAM(),WIO
          CALL WRITEBRK(C_MESBUF)
          RETURN
        ENDIF
C
        TCP_WRITESOUT(WIO)=TCREADY
C
        TBUF=TCP_WRITETBUF(WIO)
        IF(TCP_DEBUG.NE.0) THEN
          TYPE *,IAM(),'TCP_WRIOCOMP:  TBUF',TBUF
        ENDIF
C
        IF(TBUF.LT.1 .OR. TBUF.GT.TCBUFMAX) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9100) IAM(),TBUF
          CALL WRITEBRK(C_MESBUF)
          RETURN
        ENDIF
C
C
        IF(TCP_WRITE_IOSB(WIO).STAT .EQ. SS$_NORMAL) THEN
          IF(TCP_DEBUG.NE.0) THEN
            TYPE *,IAM(),'TCP_WRIOCOMP : GOOD WRITE'
          ENDIF
          TCP_WRITES = TCP_WRITES + 1
          TCBUF(TCBUFSTS,TBUF) = TCGODWRT
          TCBUF(TCBUFERR,TBUF) = 0
          CALL ABL (TBUF,RCVQUE,IERR)
          CALL TCP_DOWRIT
C
        ELSE
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9200) IAM(),TCP_WRITE_IOSB(WIO).STAT
          CALL WRITEBRK(C_MESBUF)
          TCP_WRITEERRS = TCP_WRITEERRS + 1
          TCP_WRITELERR = TCP_WRITE_IOSB(WIO).STAT
          TCBUF(TCBUFSTS,TBUF) = TCBADWRT
          TCBUF(TCBUFERR,TBUF) = TCP_WRITE_IOSB(WIO).STAT
          IF(TCP_CONNSTS.EQ.TCCONN) THEN
            IF(TCP_DEBUG.NE.0) THEN
              TYPE *,IAM(),
     *               'TCP_WRIOCOMP :SETTING CONNECT FLAG TO DISCONNECTED'
            ENDIF
            TCBUF(TCBUFSTS,TBUF) = TCDISWRT
            CALL ABL (TBUF,RCVQUE,IERR)
            CALL TCP_DODISC
          ELSE
            CALL ABL (TBUF,RCVQUE,IERR)
          ENDIF
        ENDIF
C
C
        RETURN
9000    FORMAT(A,'TCP_WRIOCOMP: Invalid WIO Buffer  # ',I8)
9100    FORMAT(A,'TCP_WRIOCOMP: Invalid TC Buffer  # ',I8)
9200    FORMAT(A,'TCP_WRIOCOMP: Error completing write ',I8)
C
        END
C
C*****************************
C
C       SUBROUTINE TCP_DOACONN
C
C*****************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TCP_DOACONN
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:CRSCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:TCPEVN.DEF'
        INCLUDE 'INCLIB:INETDEF.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
        INTEGER*4 STATUS        !STATUS RETURNED FROM QIO
        INTEGER*4 IFUNC         !FUNCTION CODE
C
        CHARACTER*132   C_MESBUF
        INTEGER*4       MESBUF(33)
        EQUIVALENCE    (C_MESBUF,MESBUF)
C
        INTEGER*4       BLANK
        DATA            BLANK/'    '/
C
C
C SOCKET PARAMETERS.
C
        INTEGER*2   SCK_PARM(2)                 !SOCKET PARAMETERS
        BYTE        BSCK_PARM(4)                !BYTE PARAMETERS
        EQUIVALENCE (SCK_PARM,BSCK_PARM)
C
C
        INTEGER*4   DESCRIP_P3(2)               !QIO INFORMATION
        INTEGER*4   REM_DESCRIP_P3(2)
        CHARACTER*1 LOCAL_HOSTADDR(16)          !LOCAL HOST ADDRESS
        INTEGER*4   RETLEN                      !RETURNED BUFFER LENGTH
        INTEGER*4   LOCPRT                      !LOCAL PORT NUMBER
        INTEGER*4   REMPRT                      !REMOTE PORT NUMBER
C
        INTEGER*2   NETBYT
        EXTERNAL    NETBYT
C
C STRUCTURE USED FOR SOCK ADDRESS.
C
        STRUCTURE /SOCKADR/
          INTEGER*2   INET_FAMILY
          INTEGER*2   INET_PORT
          INTEGER*4   ADRS
          CHARACTER*8 BLKB
        END STRUCTURE
        RECORD /SOCKADR/    REMOTE_HOST
        RECORD /SOCKADR/    LOCAL_HOST
C
C STRUCTURE USED TO PASS PARAMETERS TO QIO CALLS (ITEMLIST 1).
C
        STRUCTURE /ITLST_1/
          INTEGER*4 LGTH
          INTEGER*4 RMT_ADRS
          INTEGER*4 RETLTH
        END STRUCTURE
        RECORD /ITLST_1/    LSCK_ADRS
C
C
C STRUCTURE USED TO PASS PARAMETERS 5 TO SETMODE
C
        STRUCTURE /P5STR/
          INTEGER*2 LEN
          INTEGER*2 OPT
          INTEGER*4 ADR
        END STRUCTURE
        RECORD /P5STR/    P5PARM
C
C
C
        STRUCTURE /ITLST_2/
          INTEGER*2 LEN1
          INTEGER*2 OPT1
          INTEGER*4 ADR1
          INTEGER*2 LEN2
          INTEGER*2 OPT2
          INTEGER*4 ADR2
        END STRUCTURE
        RECORD /ITLST_2/ P5LST2
C
C
        INTEGER*4 LINGOPT(2)
        INTEGER*4 DELYOPT
C
C
        RECORD /TC_IOSSTRUCT/ LOCAL_IOSB
C
        INTEGER*4   REMADR          !REMOTE INTERNET ADDRESS
        INTEGER*4   CONWAIT         !WAIT TIME
C
        EXTERNAL  TCP_COIOCOMP
C
        LOGICAL*4   P1_IP_OK, P2_IP_OK  ! PRIMARY IPS HAS GOOD IP ADDRESS 1&2 ??
        LOGICAL*4   B1_IP_OK, B2_IP_OK  ! BACKUP  IPS HAS GOOD IP ADDRESS 1&2 ??
        LOGICAL*4   LAN1_USED           ! ADDRESS TO BE SELECTED FOR IPS-CONN
C
        IF(TCP_DEBUG.NE.0) THEN
          TYPE *,IAM(),'TCP_DOACONN'
        ENDIF
C
        TCP_CONTYPE  = TCACTV
C
C DJO - CHECK TO SEE IF 'NO CONNECTION' HAS BEEN SELECTED (PRMSTR=0). IF
C       SO, THEN RETURN WITHOUT DOING ANYTHING.
C
        IF(P(PRMSTR).EQ.0) RETURN
C
C CHECK IF SYSTEM IS READY FOR CONNECTIONS
C
        IF(TCP_CONNOVR.NE.TCOKAY) THEN
          CONWAIT=TCP_CONWAIT
          CALL TCP_STARTTIME(4,CONWAIT)
          GOTO 10000
        ENDIF
C
        IF(TCP_CONNSTS.NE.TCDISCON) GOTO 10000
C
C FIND OUT WHAT INSTANT SYSTEM WE ARE TALKING TO
C
        P1_IP_OK = (TCP_SUFFIX(1,1).NE.0 .AND. TCP_SUFFIX(2,1).NE.0)
        B1_IP_OK = (TCP_SUFFIX(1,2).NE.0 .AND. TCP_SUFFIX(2,2).NE.0)
        P2_IP_OK = (TCP_SUFFIX(1,3).NE.0 .AND. TCP_SUFFIX(2,3).NE.0)
        B2_IP_OK = (TCP_SUFFIX(1,4).NE.0 .AND. TCP_SUFFIX(2,4).NE.0)

        LAN1_USED = (MOD(TCP_IP_CNT,2).EQ.0)

        IF (P(PRMSTR).EQ.1) THEN                        ! PRIMARY INSTANT SYSTEM
           IF (LAN1_USED .AND. P1_IP_OK) THEN
              TCP_B_REMADR(3) = TCP_SUFFIX(1,1)
              TCP_B_REMADR(4) = TCP_SUFFIX(2,1)
              TCP_REMPRT = TCP_PORTS(1,1)
              TCP_LOCPRT = TCP_PORTS(1,2)
           ELSEIF (P2_IP_OK) THEN
              TCP_B_REMADR(3) = TCP_SUFFIX(1,3)
              TCP_B_REMADR(4) = TCP_SUFFIX(2,3)
              TCP_REMPRT = TCP_PORTS(1,1)
              TCP_LOCPRT = TCP_PORTS(1,2)
           ELSEIF (P1_IP_OK) THEN
              TCP_B_REMADR(3) = TCP_SUFFIX(1,1)
              TCP_B_REMADR(4) = TCP_SUFFIX(2,1)
              TCP_REMPRT = TCP_PORTS(1,1)
              TCP_LOCPRT = TCP_PORTS(1,2)
           ELSE
              CALL FASTSET(BLANK,MESBUF,33)
              WRITE(C_MESBUF,9800) IAM()
              CALL WRITEBRK(C_MESBUF)
           ENDIF            
        ELSE                                            ! BACKUP INSTANT SYSTEM
            IF (LAN1_USED .AND. B1_IP_OK) THEN
              TCP_B_REMADR(3) = TCP_SUFFIX(1,2)
              TCP_B_REMADR(4) = TCP_SUFFIX(2,2)
              TCP_REMPRT=TCP_PORTS(2,1)
              TCP_LOCPRT=TCP_PORTS(2,2)
            ELSEIF (B2_IP_OK) THEN
              TCP_B_REMADR(3) = TCP_SUFFIX(1,4)
              TCP_B_REMADR(4) = TCP_SUFFIX(2,4)
              TCP_REMPRT=TCP_PORTS(2,1)
              TCP_LOCPRT=TCP_PORTS(2,2)
            ELSEIF (B1_IP_OK) THEN
              TCP_B_REMADR(3) = TCP_SUFFIX(1,2)
              TCP_B_REMADR(4) = TCP_SUFFIX(2,2)
              TCP_REMPRT=TCP_PORTS(2,1)
              TCP_LOCPRT=TCP_PORTS(2,2)
            ELSE
              CALL FASTSET(BLANK,MESBUF,33)
              WRITE(C_MESBUF,9800) IAM()
              CALL WRITEBRK(C_MESBUF)
           ENDIF            
        ENDIF

        TCP_IP_CNT=TCP_IP_CNT+1
        TCP_CONNSTS=TCCONINP
C
C GET A CHANNEL NUMBER ASSIGNED TO THE DEVICE.
C
        IF(TCP_CHANNEL2.EQ.-1) THEN
          STATUS=SYS$ASSIGN('UCX$DEVICE',TCP_CHANNEL2,,)
          IF(.NOT.STATUS) THEN
            TCP_CHANNEL2=-1
            CALL FASTSET(BLANK,MESBUF,33)
            WRITE(C_MESBUF,9000) IAM(),STATUS
            CALL WRITEBRK(C_MESBUF)
            GOTO 8000
          ENDIF
          IF(TCP_DEBUG.NE.0) THEN
            TYPE *,IAM(),'TCP_DOACONN: ASSIGN COMPLETED'
          ENDIF
        ENDIF
C
C SETUP PARAMETER 1.
C
        SCK_PARM(1)= INET$C_TCP
        SCK_PARM(2)= INET_PROTYP$C_STREAM
C
C ITEM LIST FOR LOCAL IP ADDRESS.
C
        LOCPRT=TCP_LOCPRT
        LOCAL_HOST.INET_FAMILY= INET$C_AF_INET
        LOCAL_HOST.INET_PORT=NETBYT(LOCPRT)
        LOCAL_HOST.ADRS = INET$C_INADDR_ANY
        LOCAL_HOST.BLKB=' '
        DESCRIP_P3(1)=16
        DESCRIP_P3(2)=%LOC(LOCAL_HOST)
C
C ITEM LIST FOR REMOTE IP ADDRESS.
C
        REMADR=TCP_REMADR
        REMPRT=TCP_REMPRT
C
        REMOTE_HOST.INET_FAMILY= INET$C_AF_INET
        REMOTE_HOST.INET_PORT=NETBYT(REMPRT)
        REMOTE_HOST.ADRS = REMADR
        REMOTE_HOST.BLKB=' '
        REM_DESCRIP_P3(1)=16
        REM_DESCRIP_P3(2)=%LOC(REMOTE_HOST)
C
        P5PARM.LEN = 16
        P5PARM.OPT = UCX$C_SOCKOPT
        P5PARM.ADR = %LOC( P5LST2)
C
        P5LST2.LEN2 = 8
        P5LST2.OPT2 = UCX$M_LINGER
        P5LST2.ADR2 = %LOC(LINGOPT)
        P5LST2.LEN1 = 4
        P5LST2.OPT1 = UCX$C_TCP_NODELAY
        P5LST2.ADR1 = %LOC(DELYOPT)
C
        LINGOPT(1) = 0
        LINGOPT(2) = 0
        DELYOPT = 1
C
C CREATE AND BIND THE SOCKET TO THE LOCAL HOST.
C
        IFUNC=IO$_SETMODE
        STATUS=SYS$QIOW(,%VAL(TCP_CHANNEL2),
     *                   %VAL(IFUNC),
     *                   %REF(LOCAL_IOSB),,,
     *                   SCK_PARM,                 !P1
     *                   ,                         !P2
     *                   DESCRIP_P3,               !P3
     *                   ,                         !P4
     *                   ,)                        !P5
C***     *                       P5PARM,)                  !P5
        IF(.NOT.STATUS) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9100) IAM(),STATUS,ZEXT(TCP_B_REMADR(3))
          CALL WRITEBRK(C_MESBUF)
          GOTO 7000
        ELSE IF(LOCAL_IOSB.STAT .NE. SS$_NORMAL) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9200) IAM(),LOCAL_IOSB.STAT,
     *                         ZEXT(TCP_B_REMADR(3))
          CALL WRITEBRK(C_MESBUF)
          GOTO 7000
        ENDIF
        IF(TCP_DEBUG.NE.0) THEN
          TYPE *,IAM(),'TCP_DOACONN: SETMODE COMPLETED'
        ENDIF
C
C SETUP FOR LOCAL SOCKET ADDRESS.
C
        LSCK_ADRS.LGTH =16
        LSCK_ADRS.RMT_ADRS = %LOC(LOCAL_HOSTADDR)
        LSCK_ADRS.RETLTH = %LOC(RETLEN)
        RETLEN=0
C
C GET LOCAL SOCKET NAME.
C
        IFUNC=IO$_SENSEMODE
        STATUS=SYS$QIOW(,%VAL(TCP_CHANNEL2),
     *                   %VAL(IFUNC),
     *                   %REF(LOCAL_IOSB),,,
     *                   ,,                        !P1 AND P2
     *                   LSCK_ADRS,                !P3
     *                   ,,)                       !P4
        IF(.NOT.STATUS) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9300) IAM(),STATUS
          CALL WRITEBRK(C_MESBUF)
          GOTO 7000
        ELSE IF(LOCAL_IOSB.STAT .NE. SS$_NORMAL) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9400) IAM(),LOCAL_IOSB.STAT
          CALL WRITEBRK(C_MESBUF)
          GOTO 7000
        ENDIF
        IF(TCP_DEBUG.NE.0) THEN
          TYPE *,IAM(),'TCP_DOACONN: 1ST SENSEMODE COMPLETED'
        ENDIF
C
C DEFINE THE PEER SOCKET NAME AND ADDRESS - CONNECT.
C
        IFUNC=IO$_ACCESS
        STATUS=SYS$QIO(,%VAL(TCP_CHANNEL2),
     *                  %VAL(IFUNC),
     *                  %REF(TCP_CONN_IOSB),
     *                  TCP_COIOCOMP,,
     *                  ,                         !P1
     *                  ,                         !P2
     *                  REM_DESCRIP_P3,           !P3
     *                  ,,)                       !P4 P5 P6
        IF(.NOT.STATUS) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9700) IAM(),STATUS
          CALL WRITEBRK(C_MESBUF)
          GOTO 7000
        ENDIF 
        GOTO 10000
C
C
7000    CONTINUE
        CALL TCP_DODASGN            !DEASSIGN THE CHANNELS IF ANY ERRORS
C
8000    CONTINUE
        TCP_CONNSTS = TCDISCON
        TCP_CONNERRS= TCP_CONNERRS + 1
        IF(.NOT.STATUS) THEN
          TCP_CONNLERR = STATUS
        ELSE
          TCP_CONNLERR = LOCAL_IOSB.STAT
        ENDIF
C
C       TRY AND CONNECT IN 10 SECONDS
C
        CONWAIT=TCP_CONWAIT
        CALL TCP_STARTTIME(4,CONWAIT)
C
10000   CONTINUE
        RETURN
C
9000    FORMAT(A,'TCP_DOACONN: Error ASSIGNING to UCX  ',I8)
9100    FORMAT(A,'TCP_DOACONN: Error starting SETMODE  ',I8,' IP ',I3.3)
9200    FORMAT(A,'TCP_DOACONN: Error completing SETMODE  ',I8,' IP ',I3.3)
9300    FORMAT(A,'TCP_DOACONN: Error starting 1st SENSEMODE  ',I8)
9400    FORMAT(A,'TCP_DOACONN: Error completing 1st SENSEMODE  ',I8)
9500    FORMAT(A,'TCP_DOACONN: Error starting 2nd SENSEMODE  ',I8)
9600    FORMAT(A,'TCP_DOACONN: Error completing 2nd SENSEMODE  ',I8)
9700    FORMAT(A,'TCP_DOACONN: Error starting ACCESS  ',I8)
9800    FORMAT(A,'TCP_DOACONN: No good IP addresses for IPS defined')
        END
C
C*****************************
C 
C       SUBROUTINE TCP_COIOCOMP
C
C*****************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TCP_COIOCOMP
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:CRSCOM.DEF'
        INCLUDE 'INCLIB:TCPEVN.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
        INTEGER*4   CONWAIT             !CONNECTION WAIT TIME
C
        CHARACTER*132   C_MESBUF
        INTEGER*4       MESBUF(33)
        EQUIVALENCE    (C_MESBUF,MESBUF)
C
        INTEGER*4       BLANK
        DATA            BLANK/'    '/
C
C
        IF(TCP_DEBUG.NE.0) THEN
          TYPE *,IAM(),'TCP_COIOCOMP:  '
        ENDIF
C
        IF(TCP_CONN_IOSB.STAT .EQ. SS$_NORMAL) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9000) IAM(),ZEXT(TCP_B_REMADR(3))
          CALL WRITEBRK(C_MESBUF)
          TCP_CONNSTS   = TCCONN            !CONNECTED
          TCP_CONNECTS  = TCP_CONNECTS + 1  
C
C ADDED CODE BELOW TO HANDLE BAD LENGTHS SO WHEN WE RECONECT WE CAN GET
C DATA FROM INSTANT SYSTEM AGAIN
C
          TCP_BUFFOFF   = 0
          TCP_BUFFLEN   = 0
          TCP_REDIGNORE = 0
          CALL TCP_CHEKREAD
C
        ELSE
          CALL FASTSET(BLANK,MESBUF,33)
          TCP_CONNSTS = TCDISCON
          TCP_CONNERRS= TCP_CONNERRS + 1
          TCP_CONNLERR = TCP_CONN_IOSB.STAT
          IF(P(PRMSTR).EQ.0) RETURN     !DJO - IF NO CONNECT, SKIP ERROR MSG
          WRITE(C_MESBUF,9100) IAM(),TCP_CONN_IOSB.STAT,
     *                         ZEXT(TCP_B_REMADR(3))
          CALL WRITEBRK(C_MESBUF)
          CALL TCP_DODASGN
C
C         TRY AND CONNECT IN 10 SECONDS
C
          CONWAIT=TCP_CONWAIT
          CALL TCP_STARTTIME(4,CONWAIT)
        ENDIF
C
C
        RETURN
9000    FORMAT(A,
     *  'TCP_COIOCOMP: Good TCP/IP connection with Instant System',
     *         ' IP ',I3.3)
9100    FORMAT(A,'TCP_COIOCOMP: Error completing connection  ',I8,
     *         ' IP ',I3.3)
C
        END
C
C*****************************
C 
C       SUBROUTINE TCP_DOPCONN
C
C*****************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TCP_DOPCONN
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:CRSCOM.DEF'
        INCLUDE 'INCLIB:TCPEVN.DEF'
        INCLUDE 'INCLIB:INETDEF.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
        INTEGER*4 STATUS        !STATUS RETURNED FROM QIO
        INTEGER*4 IFUNC         !FUNCTION CODE
C
        CHARACTER*132   C_MESBUF
        INTEGER*4       MESBUF(33)
        EQUIVALENCE    (C_MESBUF,MESBUF)
C
        INTEGER*4       BLANK
        DATA            BLANK/'    '/
C
C
C SOCKET PARAMETERS.
C
        INTEGER*2   SCK_PARM(2)                 !SOCKET PARAMETERS
        BYTE        BSCK_PARM(4)                !BYTE PARAMETERS
        EQUIVALENCE (SCK_PARM,BSCK_PARM)
C
        INTEGER*4   NUMCONC
        INTEGER*4   DESCRIP_P3(2)
        CHARACTER*1 REMOTE_HOSTADDR(16)         !REMOTE HOST ADDRESS
        INTEGER*4   RETLEN                      !RETURNED BUFFER LENGTH
        INTEGER*4   LOCPRT                      !LOCAL PORT NUMBER

        INTEGER*2   NETBYT
        EXTERNAL    NETBYT
C
C STRUCTURE USED FOR SOCK ADDRESS.
C
        STRUCTURE /SOCKADR/
          INTEGER*2   INET_FAMILY
          INTEGER*2   INET_PORT
          INTEGER*4   ADRS
          CHARACTER*8 BLKB
        END STRUCTURE
        RECORD /SOCKADR/    LOCAL_HOST
C
C STRUCTURE USED TO PASS PARAMETERS TO QIO CALLS (ITEMLIST 2).
C
        STRUCTURE /ITLST_1/
          INTEGER*4 LGTH
          INTEGER*4 RMT_ADRS
          INTEGER*4 RETLTH
        END STRUCTURE
        RECORD /ITLST_1/    RHST_ADRS
C
C STRUCTURE USED TO PASS PARAMETERS 5 TO SETMODE
C
        STRUCTURE /P5STR/
          INTEGER*2 LEN
          INTEGER*2 OPT
          INTEGER*4 ADR
        END STRUCTURE
        RECORD /P5STR/    P5PARM
C
C
C
        STRUCTURE /ITLST_2/
          INTEGER*2 LEN1
          INTEGER*2 OPT1
          INTEGER*4 ADR1
          INTEGER*2 LEN2
          INTEGER*2 OPT2
          INTEGER*4 ADR2
        END STRUCTURE
        RECORD /ITLST_2/ P5LST2
C
C
        INTEGER*4 LINGOPT(2)
        INTEGER*4 DELYOPT
C
        RECORD /TC_IOSSTRUCT/ LOCAL_IOSB
        INTEGER*4 CONWAIT       !WAIT TIME
C
C
C CHECK IF SYSTEM IS READY FOR CONNECTIONS
C
        IF(TCP_CONNOVR.NE.TCOKAY) THEN
          CONWAIT=TCP_CONWAIT
          CALL TCP_STARTTIME(5,CONWAIT)
          GOTO 10000
        ENDIF
C
        IF(TCP_CONNSTS.NE.TCDISCON) GOTO 10000
        TCP_CONNSTS=TCCONINP
C
C
C GET A CHANNEL NUMBER ASSIGNED TO THE DEVICE.
C
        IF(TCP_CHANNEL1.EQ.-1) THEN
          STATUS=SYS$ASSIGN('UCX$DEVICE',TCP_CHANNEL1,,)
          IF(.NOT.STATUS) THEN
            TCP_CHANNEL1=-1
            CALL FASTSET(BLANK,MESBUF,33)
            WRITE(C_MESBUF,9000) IAM(),STATUS
            CALL WRITEBRK(C_MESBUF)
            GOTO 8000
          ENDIF
        ENDIF
C
        IF(TCP_CHANNEL2.EQ.-1) THEN
          STATUS=SYS$ASSIGN('UCX$DEVICE',TCP_CHANNEL2,,)
          IF(.NOT.STATUS) THEN
            TCP_CHANNEL2=-1
            CALL FASTSET(BLANK,MESBUF,33)
            WRITE(C_MESBUF,9100) IAM(),STATUS
            CALL WRITEBRK(C_MESBUF)
            GOTO 8000
          ENDIF
        ENDIF
C
C SETUP PARAMETER 1.
C
        SCK_PARM(1)= INET$C_TCP
        SCK_PARM(2)= INET_PROTYP$C_STREAM
C
C ITEM LIST FOR LOCAL IP ADDRESS.
C
        LOCPRT=TCP_REMPRT
        LOCAL_HOST.INET_FAMILY= INET$C_AF_INET
        LOCAL_HOST.INET_PORT=NETBYT(LOCPRT)
        LOCAL_HOST.ADRS = INET$C_INADDR_ANY
        LOCAL_HOST.BLKB=' '
        DESCRIP_P3(1)=16
        DESCRIP_P3(2)=%LOC(LOCAL_HOST)
C
        P5PARM.LEN = 16
        P5PARM.OPT = UCX$C_SOCKOPT
        P5PARM.ADR = %LOC( P5LST2)
C
        P5LST2.LEN2 = 8
        P5LST2.OPT2 = UCX$M_LINGER
        P5LST2.ADR2 = %LOC(LINGOPT)
        P5LST2.LEN1 = 4
        P5LST2.OPT1 = UCX$C_TCP_NODELAY
        P5LST2.ADR1 = %LOC(DELYOPT)
C
        LINGOPT(1) = 0
        LINGOPT(2) = 0
        DELYOPT = 1
C
C CREATE AND BIND THE SOCKET.
C
        IFUNC=IO$_SETMODE
        NUMCONC=2
        STATUS=SYS$QIOW(,%VAL(TCP_CHANNEL1),
     *                   %VAL(IFUNC),
     *                   %REF(LOCAL_IOSB),,,
     *                   SCK_PARM,                 !P1
     *                   ,                         !P2
     *                   DESCRIP_P3,               !P3
     *                   %DESCR(NUMCONC),          !P4
     *                   ,)
        IF(.NOT.STATUS) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9200) IAM(),STATUS
          CALL WRITEBRK(C_MESBUF)
          GOTO 7000
        ELSE IF(LOCAL_IOSB.STAT .NE. SS$_NORMAL) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9300) IAM(),LOCAL_IOSB.STAT
          CALL WRITEBRK(C_MESBUF)
          GOTO 7000
        ENDIF
        IF(TCP_DEBUG.NE.0) THEN
          TYPE *,IAM(),'PASSIVE OPEN COMPLETE - WAITING FOR CONNECTION'
        ENDIF
C
C SETUP FOR REMOTE HOST ADDRESS.
C
        RHST_ADRS.LGTH =16
        RHST_ADRS.RMT_ADRS = %LOC(REMOTE_HOSTADDR)
        RHST_ADRS.RETLTH = %LOC(RETLEN)
        RETLEN=0
C
C ACCEPT A CONNECTION FROM A CLIENT.
C
        IFUNC=IO$_ACCESS .OR. IO$M_ACCEPT
        STATUS=SYS$QIOW(,%VAL(TCP_CHANNEL1),
     *                   %VAL(IFUNC),
     *                   %REF(LOCAL_IOSB),,,
     *                   ,                         !P1
     *                   ,                         !P2
     *                   RHST_ADRS,                !P3
     *                   TCP_CHANNEL2,             !P4
     *                   ,)                        !P5 AND P6
        IF(.NOT.STATUS) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9400) IAM(),STATUS
          CALL WRITEBRK(C_MESBUF)
          GOTO 7000
        ELSE IF(LOCAL_IOSB.STAT .NE. SS$_NORMAL) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9500) IAM(),LOCAL_IOSB.STAT
          CALL WRITEBRK(C_MESBUF)
          GOTO 7000
        ENDIF
        IF(TCP_DEBUG.NE.0) THEN
          TYPE *,IAM(),'PASSIVE CONNECTION ESTABLISHED....'
        ENDIF
C
        TCP_CONNSTS=TCCONN
        TCP_CONNECTS = TCP_CONNECTS + 1
        CALL TCP_CHEKREAD
        GOTO 10000
C
C
7000    CONTINUE
        CALL TCP_DODASGN
C
8000    CONTINUE
        TCP_CONNSTS = TCDISCON
        TCP_CONNERRS= TCP_CONNERRS + 1
        IF(.NOT.STATUS) THEN
          TCP_CONNLERR = STATUS
        ELSE
          TCP_CONNLERR = LOCAL_IOSB.STAT
        ENDIF
C
C       TRY AND CONNECT IN 10 SECONDS
C
        CONWAIT=TCP_CONWAIT
        CALL TCP_STARTTIME(5,CONWAIT)
C
10000   CONTINUE
        TCP_CONTYPE=TCPASS
        RETURN
C
9000    FORMAT(A,'TCP_DOPCONN: Error ASSIGNING 1st chan to UCX  ',I8)
9100    FORMAT(A,'TCP_DOPCONN: Error ASSIGNING 2nd chan to UCX  ',I8)
9200    FORMAT(A,'TCP_DOPCONN: Error starting SETMODE  ',I8)
9300    FORMAT(A,'TCP_DOPCONN: Error completing SETMODE  ',I8)
9400    FORMAT(A,'TCP_DOPCONN: Error starting ACCEPT  ',I8)
9500    FORMAT(A,'TCP_DOPCONN: Error completing ACCEPT  ',I8)
        END
C
C*****************************
C
C       SUBROUTINE TCP_DODISC
C
C*****************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TCP_DODISC
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:CRSCOM.DEF'
        INCLUDE 'INCLIB:TCPEVN.DEF'
        INCLUDE 'INCLIB:INETDEF.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
        INTEGER*4 STATUS        !STATUS RETURNED FROM QIO
        INTEGER*4 IFUNC         !FUNCTION CODE
C
        CHARACTER*132   C_MESBUF
        INTEGER*4       MESBUF(33)
        EQUIVALENCE    (C_MESBUF,MESBUF)
C
        INTEGER*4       BLANK
        DATA            BLANK/'    '/
C
C
C SOCKET PARAMETERS.
C
        INTEGER*2   SCK_PARM(2)                 !SOCKET PARAMETERS
        BYTE        BSCK_PARM(4)                !BYTE PARAMETERS
        EQUIVALENCE (SCK_PARM,BSCK_PARM)
C
C
        INTEGER*2   NETBYT
        EXTERNAL    NETBYT
C
C STRUCTURE USED FOR SOCK ADDRESS.
C
        STRUCTURE /SOCKADR/
          INTEGER*2   INET_FAMILY
          INTEGER*2   INET_PORT
          INTEGER*4   ADRS
          CHARACTER*8 BLKB
        END STRUCTURE
C
C STRUCTURE USED TO PASS PARAMETERS TO QIO CALLS (ITEMLIST 2).
C
        STRUCTURE /ITLST_1/
          INTEGER*4 LGTH
          INTEGER*4 RMT_ADRS
          INTEGER*4 RETLTH
        END STRUCTURE
C
        RECORD /TC_IOSSTRUCT/ LOCAL_IOSB
C
        INTEGER*4   REMADR                      !REMOTE INTERNET ADDRESS
        BYTE        B_REMADR(4)
        EQUIVALENCE (REMADR,B_REMADR)
C
        INTEGER*4   P4PARM
        INTEGER*4 DISWAIT       !WAIT TIME
C
        IF(TCP_DEBUG.NE.0) THEN
          TYPE *,IAM(),'TCP_DODISC:  '
        ENDIF
C
        TCP_CONNSTS=TCDISINP
C
C SHUTDOWN THE LOCAL SOCKET
C
        IFUNC=IO$_DEACCESS .OR. IO$M_SHUTDOWN
        P4PARM = UCX$C_DSC_ALL
        STATUS=SYS$QIOW(,%VAL(TCP_CHANNEL2),
     *                   %VAL(IFUNC),
     *                   %REF(LOCAL_IOSB),,,
     *                   ,                         !P1
     *                   ,                         !P2
     *                   ,                         !P3
     *                   %VAL(P4PARM),             !P4
     *                   ,)                        !P5,P6
        IF(.NOT.STATUS) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9000) IAM(),STATUS
          CALL WRITEBRK(C_MESBUF)
          GOTO 7000
        ELSE IF(LOCAL_IOSB.STAT .NE. SS$_NORMAL) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9100) IAM(),LOCAL_IOSB.STAT
          CALL WRITEBRK(C_MESBUF)
          GOTO 7000
        ENDIF
        IF(TCP_DEBUG.NE.0) THEN
          TYPE *,IAM(),'TCP_DODISC: 1ST DEACCESS COMPLETED'
        ENDIF
C
C CLOSE THE LOCAL SOCKET
C
        IFUNC=IO$_DEACCESS
        STATUS=SYS$QIOW(,%VAL(TCP_CHANNEL2),
     *                   %VAL(IFUNC),
     *                   %REF(LOCAL_IOSB),,,
     *                   ,                         !P1
     *                   ,                         !P2
     *                   ,                         !P3
     *                   ,                         !P4
     *                   ,)                        !P5,P6
        IF(.NOT.STATUS) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9200) IAM(),STATUS
          CALL WRITEBRK(C_MESBUF)
          GOTO 7000
        ELSE IF(LOCAL_IOSB.STAT .NE. SS$_NORMAL) THEN
          CALL FASTSET(BLANK,MESBUF,33)
          WRITE(C_MESBUF,9300) IAM(),LOCAL_IOSB.STAT
          CALL WRITEBRK(C_MESBUF)
          GOTO 7000
        ENDIF
        IF(TCP_DEBUG.NE.0) THEN
          TYPE *,IAM(),'TCP_DODISC: 2ND DEACCESS COMPLETED'
        ENDIF
C
C CLOSE THE LISTNER SOCKET
C
        IF(TCP_CONTYPE.EQ.TCPASS) THEN
          IFUNC=IO$_DEACCESS
          STATUS=SYS$QIOW(,%VAL(TCP_CHANNEL1),
     *                     %VAL(IFUNC),
     *                     %REF(LOCAL_IOSB),,,
     *                     ,                       !P1
     *                     ,                       !P2
     *                     ,                       !P3
     *                     ,                       !P4
     *                     ,)                      !P5,P6
          IF(.NOT.STATUS) THEN
            CALL FASTSET(BLANK,MESBUF,33)
            WRITE(C_MESBUF,9400) IAM(),STATUS
            CALL WRITEBRK(C_MESBUF)
            GOTO 7000
          ELSE IF(LOCAL_IOSB.STAT .NE. SS$_NORMAL) THEN
            CALL FASTSET(BLANK,MESBUF,33)
            WRITE(C_MESBUF,9500) IAM(),LOCAL_IOSB.STAT
            CALL WRITEBRK(C_MESBUF)
            GOTO 7000
          ENDIF
          IF(TCP_DEBUG.NE.0) THEN
            TYPE *,IAM(),'TCP_DODISC: 3RD DEACCESS COMPLETED'
          ENDIF
        ENDIF
C
C
        CALL TCP_DODASGN
        TCP_CONNSTS  = TCDISCON
        TCP_DISCONNS = TCP_DISCONNS + 1
        GOTO 10000
C
C
7000    CONTINUE
        CALL TCP_DODASGN
        TCP_CONNSTS  = TCDISCON
        TCP_DISCERRS = TCP_DISCERRS + 1
        IF(.NOT.STATUS) THEN
          TCP_DISCLERR = STATUS
        ELSE
          TCP_DISCLERR = LOCAL_IOSB.STAT
        ENDIF
C
C TRY AND CONNECT IN 100 MILLESECONDS
C
10000   CONTINUE
        DISWAIT=TCP_DISWAIT
        IF(TCP_CONTYPE.EQ.TCACTV) THEN
          DISWAIT = DISWAIT
          CALL TCP_STARTTIME(4,DISWAIT)
        ELSE
          CALL TCP_STARTTIME(5,DISWAIT)
        ENDIF
C
        RETURN
9000    FORMAT(A,'TCP_DODISC: Error starting 1st DEACCESS  ',I8)
9100    FORMAT(A,'TCP_DODISC: Error completing 1st DEACCESS  ',I8)
9200    FORMAT(A,'TCP_DODISC: Error starting 2nd DEACCESS  ',I8)
9300    FORMAT(A,'TCP_DODISC: Error completing 2nd DEACCESS  ',I8)
9400    FORMAT(A,'TCP_DODISC: Error starting 3rd DEACCESS  ',I8)
9500    FORMAT(A,'TCP_DODISC: Error completing 3rd DEACCESS  ',I8)
C
        END
C
C*****************************
C
C       SUBROUTINE TCP_DODASGN
C
C*****************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TCP_DODASGN
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:CRSCOM.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
        INTEGER*4 STATUS        !STATUS RETURNED FROM QIO
C
        CHARACTER*132   C_MESBUF
        INTEGER*4       MESBUF(33)
        EQUIVALENCE    (C_MESBUF,MESBUF)
C
        INTEGER*4       BLANK
        DATA            BLANK/'    '/
C
C
        IF(TCP_DEBUG.NE.0) THEN
          TYPE *,IAM(),'TCP_DODASGN:  '
        ENDIF
C
C DEASSIGN THE 1ST CHANNEL ASSIGNED TO THE DEVICE.
C
        IF(TCP_CHANNEL2.NE.-1) THEN
          STATUS=SYS$DASSGN(%VAL(TCP_CHANNEL2))
          IF(.NOT.STATUS) THEN
            CALL FASTSET(BLANK,MESBUF,33)
            WRITE(C_MESBUF,9000) IAM(),STATUS
            CALL WRITEBRK(C_MESBUF)
            GOTO 10000
          ENDIF
          TCP_CHANNEL2=-1
        ENDIF
C
C DEASSIGN THE 2ND CHANNEL ASSIGNED TO THE DEVICE.
C
        IF(TCP_CHANNEL1.NE.-1) THEN
          IF(TCP_CONTYPE.EQ.TCPASS) THEN
            STATUS=SYS$DASSGN(%VAL(TCP_CHANNEL1))
            IF(.NOT.STATUS) THEN
              CALL FASTSET(BLANK,MESBUF,33)
              WRITE(C_MESBUF,9100) IAM(),STATUS
              CALL WRITEBRK(C_MESBUF)
              GOTO 10000
            ENDIF
            TCP_CHANNEL1=-1
          ELSE
            TCP_CHANNEL1=-1
          ENDIF
        ENDIF
C
10000   CONTINUE
C
        RETURN
9000    FORMAT(A,'TCP_DODASGN: Error DEASSIGNING 1st channel  ',I8)
9100    FORMAT(A,'TCP_DODASGN: Error DEASSIGNING 2nd channel  ',I8)
C
        END
C
C*****************************
C 
C       SUBROUTINE TCP_STOP
C
C*****************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TCP_STOP
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:CRSCOM.DEF'
C
C       DISCONNECT IF CURRENTLY CONNECTED
C
        IF(TCP_CONNSTS.EQ.TCCONN) THEN
          CALL TCP_DODISC
        ENDIF
C
        CALL GSTOP(GEXIT_SUCCESS)
C
        RETURN
        END
