C
C PROGRAM VAX_SEND.FOR
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]VAX_SEND.FOV                                 $
C  $Date::   17 Apr 1996 15:52:30                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C V03 01-FEB-90 RRB ADDED ABILITY TO SEND MESSAGES TO MSCMGR
C V02 07-JAN-90 KWP ADDED ABILITY TO SEND MESSAGES TO CTLPRO
C V01 10-SEP-90 MRM INITIAL RELEASE
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
	PROGRAM VAX_SEND
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:LANEVN.DEF'
	INCLUDE 'INCLIB:CTLEVN.DEF'
	INCLUDE 'INCLIB:PCEVN.DEF'
	INCLUDE 'INCLIB:MSCEVN.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
        CHARACTER       GXEVNNAM*4      !Name function
C
	INTEGER*4    NUMBOX
	PARAMETER   (NUMBOX=4)		!NUMBER OF DIFFERENT MAIL BOXES
C
	INTEGER*4    BOX_STAT(NUMBOX)	!STATUS OF MAIL BOX
	DATA	     BOX_STAT/NUMBOX*0/
C
	INTEGER*4    PRGID	        !ID OF PROGRAM SENDING TO
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
	CALL COPYRITE
C
	TYPE *
	TYPE *,'<<<<< VAX_SEND COMMAND INTERFACE V01 >>>>>'
	TYPE *
C
C ASSIGN TO LANPRO MAILBOX.
C
	TYPE *,'ATTACHING TO LANPRO MAILBOX'
        STATUS=SYS$ASSIGN(GXEVNNAM()//LN_MESNAME,LN_MESCHANNEL,,)
        IF(.NOT.STATUS) THEN
D	  CALL LIB$SIGNAL(%VAL(STATUS))
	  TYPE *,'ERROR ATTACHING TO LANPRO'
	  BOX_STAT(1)=-1
	ENDIF
C
C ASSIGN TO CTLPRO MAILBOX.
C
	TYPE *,'ATTACHING TO CTLPRO MAILBOX'
        STATUS=SYS$ASSIGN(GXEVNNAM()//CT_MESNAME,CT_MESCHANNEL,,)
        IF(.NOT.STATUS) THEN
D	  CALL LIB$SIGNAL(%VAL(STATUS))
	  TYPE *,'ERROR ATTACHING TO CTLPRO'
	  BOX_STAT(2)=-1
	ENDIF
C
C ASSIGN TO PCLOG MAILBOX.
C
	TYPE *,'ATTACHING TO PCLOG MAILBOX'
        STATUS=SYS$ASSIGN(PC_MESNAME,PC_MESCHANNEL,,)
        IF(.NOT.STATUS) THEN
D	  CALL LIB$SIGNAL(%VAL(STATUS))
	  TYPE *,'ERROR ATTACHING TO PCLOG'
	  BOX_STAT(3)=-1
	ENDIF
C
C ASSIGN TO MSCMGR MAILBOX.
C
	TYPE *,'ATTACHING TO MSCMGR MAILBOX'
        STATUS=SYS$ASSIGN(GXEVNNAM()//MSC_MESNAME,MSC_MESCHANNEL,,)
        IF(.NOT.STATUS) THEN
D	  CALL LIB$SIGNAL(%VAL(STATUS))
	  TYPE *,'ERROR ATTACHING TO MSCMGR'
	  BOX_STAT(4)=-1
	ENDIF
C
C
C
1000	CONTINUE
	TYPE *,'This program can be used to communicate'
	TYPE *,'with the following programs:'
	TYPE *
	IF(BOX_STAT(1).EQ.0) TYPE *,'1) LANPRO'
	IF(BOX_STAT(2).EQ.0) TYPE *,'2) CTLPRO'
	IF(BOX_STAT(3).EQ.0) TYPE *,'3) PCLOG'
	IF(BOX_STAT(4).EQ.0) TYPE *,'4) MSCMGR'
	TYPE *
	TYPE *,'E) EXIT'
C
	CALL INPNUM('Enter Program #  :',PRGID,1,NUMBOX,ST)
	IF(ST.NE.0) GOTO 7000
C
	IF(BOX_STAT(PRGID).EQ.-1) THEN
	  TYPE *,'CAN NOT SEND TO THIS TASK CURRENTLY'
	  GOTO 1000
	ENDIF
C
C READ THE INPUT MESSAGE.
C
100	CONTINUE
	REWIND(5)
	MESS=' '
	CALL WIMG(5,'Input message (64 chars EX=EXIT)  ')
	READ(5,9000) MESS
9000	FORMAT(A64)
	IF(MESS(1:1).EQ.'E' .OR. MESS(1:1).EQ.'e') GOTO 7000
C
	CHANNEL=-1
	IF(PRGID.EQ.1) CHANNEL=LN_MESCHANNEL
	IF(PRGID.EQ.2) CHANNEL=CT_MESCHANNEL
	IF(PRGID.EQ.3) CHANNEL=PC_MESCHANNEL
	IF(PRGID.EQ.4) CHANNEL=MSC_MESCHANNEL
C
C SEND MESSAGE TO MAILBOX
C
  	WRITE_CODE = IO$_WRITEVBLK .OR. IO$M_NOW
        STATUS=SYS$QIOW(,
     *                  %VAL(CHANNEL),		  !CHANNEL
     *                  %VAL(WRITE_CODE),         !FUNCTION CODE
     *                  IOSTAT,                   !STATUS BLOCK
     *                  ,,
     *                  %REF(MESS),               !P1
     *                  %VAL(64),,,,)             !P2
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
	GOTO 100
C
C
7000	CONTINUE
C
	CALL GSTOP(GEXIT_SUCCESS)
	END
