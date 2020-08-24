C
C SUBROUTINE LAN_START
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]LAN_START.FOV                                $
C  $Date::   17 Apr 1996 13:47:58                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - lan_start.for;1 **
C
C LAN_START.FOR
C
C V03 09-OCT-97 UXN CHECKING THE STATUS OF SYS$ASSIGN. IF LANPRO
C	            MAILBOX HAS NOT BEEN CREATED, WAIT 2 SECONDS
C		    AND TRY AGAIN.
C V02 23-APR-91 RRB CONVERT TO SUBROUTINE FOR CALL FROM 
C               RESET AND ACTIVATE LANS 1 & 2
C V01 29-JAN-91 KWP INITIAL RELEASE
C
C This program will allow commands to be send to LANPRO.
C This program is required as VAX do not handle and
C "SEND" messages from the console.
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE LAN_START
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:LANEVN.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
        CHARACTER   GXEVNNAM*4  !NAME FUNCTION
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
	INTEGER*2   CHANNEL
	INTEGER*4   STATUS
	CHARACTER   MESS*64	!INPUT MESSAGE
C
C ASSIGN TO LANPRO MAILBOX.
C
10	CONTINUE
        STATUS=SYS$ASSIGN(GXEVNNAM()//LN_MESNAME,LN_MESCHANNEL,,)
        IF(STATUS.EQ.SS$_NOSUCHDEV) THEN
D	  CALL LIB$SIGNAL(%VAL(STATUS))
	  CALL OPS('**** WAITING FOR LANPRO MAILBOX TO BE CREATED ****',0,0)
	  CALL XWAIT(2,2,STATUS)
	  GOTO 10
	ELSEIF(STATUS.NE.SS$_NORMAL) THEN 
D	  CALL LIB$SIGNAL(%VAL(STATUS))
	  CALL OPS('**** ERROR ATTACHING TO LANPRO ****',STATUS,STATUS)
	  GOTO 7000
	ENDIF
C
C
	MESS='ACT LAN 1'
C
C SEND MESSAGE TO MAILBOX
C
  	WRITE_CODE = IO$_WRITEVBLK .OR. IO$M_NOW
        STATUS=SYS$QIOW(,
     *                  %VAL(LN_MESCHANNEL),	  !CHANNEL
     *                  %VAL(WRITE_CODE),         !FUNCTION CODE
     *                  IOSTAT,                   !STATUS BLOCK
     *                  ,,
     *                  %REF(MESS),               !P1
     *                  %VAL(64),,,,)             !P2
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
C
C
	MESS='ACT LAN 2'
C
C SEND MESSAGE TO MAILBOX
C
  	WRITE_CODE = IO$_WRITEVBLK .OR. IO$M_NOW
        STATUS=SYS$QIOW(,
     *                  %VAL(LN_MESCHANNEL),	  !CHANNEL
     *                  %VAL(WRITE_CODE),         !FUNCTION CODE
     *                  IOSTAT,                   !STATUS BLOCK
     *                  ,,
     *                  %REF(MESS),               !P1
     *                  %VAL(64),,,,)             !P2
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
C
C
	MESS='ACT STATION'
C
C SEND MESSAGE TO MAILBOX
C
  	WRITE_CODE = IO$_WRITEVBLK .OR. IO$M_NOW
        STATUS=SYS$QIOW(,
     *                  %VAL(LN_MESCHANNEL),	  !CHANNEL
     *                  %VAL(WRITE_CODE),         !FUNCTION CODE
     *                  IOSTAT,                   !STATUS BLOCK
     *                  ,,
     *                  %REF(MESS),               !P1
     *                  %VAL(64),,,,)             !P2
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
7000	CONTINUE
C
	RETURN
	END
