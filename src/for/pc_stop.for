C
C PROGRAM PC_STOP
C $Log:   GXAFXT:[GOLS]PC_STOP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:23:06   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:16:50   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pc_stop.for **
C
C
C PC_STOP.FOR
C
C V01 29-JAN-91 KWP INITIAL RELEASE
C
C This program will allow commands to be send to PCLOG
C This program is required as VAX do not handle and
C "SEND" messages from the console.
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM PC_STOP
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:PCEVN.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4    ST			!STATUS VARIABLE
	INTEGER*4    WRITE_CODE		!QIO CODE TO WRITE TO MAIL BOX
C
C QIO STATUS BLOCK.
C
        INTEGER*2   IOSTAT
        INTEGER*2   MSGLEN
        INTEGER*4   READ_PID
        COMMON /IOBLOCK/ IOSTAT, MSGLEN, READ_PID
C
C
	INTEGER*4   STATUS
	CHARACTER   MESS*64	!INPUT MESSAGE
C
	CALL COPYRITE
C
	TYPE *
	TYPE *,'<<<<< PCLOG COMMAND INTERFACE PROGRAM  >>>>>'
	TYPE *
	TYPE *,'USED TO STOP PCLOG FROM TRANSMITTING TO GVIZ'
	TYPE *
C
C ASSIGN TO PCLOG MAILBOX.
C
	TYPE *,'ATTACHING TO PCLOG MAILBOX'
        STATUS=SYS$ASSIGN(PC_MESNAME,PC_MESCHANNEL,,)
        IF(.NOT.STATUS) THEN
D	  CALL LIB$SIGNAL(%VAL(STATUS))
	  TYPE *,'ERROR ATTACHING TO PCLOG'
	  GOTO 7000
	ENDIF
C
C
	CALL XWAIT(1,2,ST)
	MESS='STOP'
C
C SEND MESSAGE TO MAILBOX
C
  	WRITE_CODE = IO$_WRITEVBLK .OR. IO$M_NOW
        STATUS=SYS$QIOW(,
     *                  %VAL(PC_MESCHANNEL),	  !CHANNEL
     *                  %VAL(WRITE_CODE),         !FUNCTION CODE
     *                  IOSTAT,                   !STATUS BLOCK
     *                  ,,
     *                  %REF(MESS),               !P1
     *                  %VAL(64),,,,)             !P2
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
7000	CONTINUE
C
	CALL GSTOP(GEXIT_SUCCESS)
	END
