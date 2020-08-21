C
C SUBROUTINE X2KILL
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2KILL.FOV                                   $
C  $Date::   17 Apr 1996 16:20:44                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2xmgr.for;1 **
C
C
C
C************************************************************
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
	SUBROUTINE X2KILL
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:LANEVN.DEF'
	INCLUDE 'INCLIB:CTLEVN.DEF'
	INCLUDE 'INCLIB:NOTEVN.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
        CHARACTER     GXEVNNAM*4        !Event name function
C
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
C ASSIGN TO LANPRO MAILBOX.
C
        STATUS=SYS$ASSIGN(GXEVNNAM()//LN_MESNAME,LN_MESCHANNEL,,)
        IF(.NOT.STATUS) THEN
D	  CALL LIB$SIGNAL(%VAL(STATUS))
	  TYPE *,IAM(),'ERROR ATTACHING TO LANPRO'
	  GOTO 2000
	ENDIF
C
C SEND 'QUIESCE' MESSAGE TO LANPRO
C
	MESS='QUIESCE          '
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
C SEND 'KILL' MESSAGE TO LANPRO
C
	MESS='KILL              '
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
2000	CONTINUE
C
C ASSIGN TO CTLPRO MAILBOX.
C
        STATUS=SYS$ASSIGN(GXEVNNAM()//CT_MESNAME,CT_MESCHANNEL,,)
        IF(.NOT.STATUS) THEN
D	  CALL LIB$SIGNAL(%VAL(STATUS))
	  TYPE *,IAM(),'ERROR ATTACHING TO CTLPRO'
	  GOTO 10000
	ENDIF
C
C SEND 'KILL' MESSAGE TO CTLPRO
C
	MESS='KILL          '
        STATUS=SYS$QIOW(,
     *                  %VAL(CT_MESCHANNEL),	  !CHANNEL
     *                  %VAL(WRITE_CODE),         !FUNCTION CODE
     *                  IOSTAT,                   !STATUS BLOCK
     *                  ,,
     *                  %REF(MESS),               !P1
     *                  %VAL(64),,,,)             !P2
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
10000	CONTINUE
C
	RETURN
	END
