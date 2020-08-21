C GUICMSG_DATA.FOR
C
C V03 13-NOV-2000 UXN GUI_WORKER queues added for multi-threaded GUIMGR
C V02 08-NOV-2000 UXN Unused parameters removed from GUIRECV()
C V01 24-JUN-1993 MP  INITIAL RELEASE FOR VAX
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C INPUT:      CONN_INX,
C	      Client's message is in GUI_ASMB_BBUF(1,CONN_INX)
C OUTPUT:     COMPLETION_CODE:
C		0  - no error
C		#0 - error code to be sent to Client
C RESULTS:    message passed to one of the GOLS system processes.
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUICMSG_DATA(COMPLETION_CODE, BUF, OUT_BUF, OUT_MSG_LEN) 
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
C
	INTEGER*4   CONN_INX		!connection number
	INTEGER*4   COMPLETION_CODE	!return code
	INTEGER*4   BUF                 ! GUI_WORKER_BUF index
	BYTE        OUT_BUF(*)          ! OUTPUT MESSAGE
	INTEGER*4   OUT_MSG_LEN         ! OUTPUT MESSAGE LENGTH
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
	INTEGER*4   BUF_BOFF
C
	INTEGER*4   DATA_CLASS
	INTEGER*4   DEVICE_TYPE	! device type (1 for phase one, July 1993)
	INTEGER*4   SOURCE_ID	! source ID (0 for phase one, July 1993)
	INTEGER*4   DEST_ID	! destination ID (0 for phase one, July 1993)
	INTEGER*4   ADDR_FORMAT	! address format (0 for phase one, July 1993)
	INTEGER*4   USER_DATA   ! user data      (0 for phase 1)
	INTEGER*4   ADDR_LEN	! address format (0 for phase one, July 1993)
	BYTE	    ADDRESS(GUI_MAX_MSG_LEN)	! address 
	INTEGER*4   MSG_LEN     ! Byte length of message  ....
	BYTE	    MESSAGE(GUI_MAX_MSG_LEN)  ! message
	INTEGER*4   RET_CODE	! return code from GUIRECV
C
	INTEGER*4   BLANK
	DATA	    BLANK/'    '/
C
	COMPLETION_CODE = 0
	CONN_INX = GUI_WORKER_SOURCE_ID(BUF)
C
C If debug mode enabled dump transaction to output file.
C
	IF(GUI_DBG_UNIT.GT.0) THEN
	  WRITE(GUI_DBG_UNIT,*)IAM(),
     *	      'GUICMSG_DATA:  conn=', CONN_INX
	  CALL GUICMSG_DUMP_ASMB_BUF(GUI_DBG_UNIT,
     *		'GUICMSG_DATA: ',GUI_WORKER_BUF(1,BUF),CONN_INX)
	ENDIF
C
C If not Primary, reject with code 2
C
	IF(P(SYSTYP).NE.LIVSYS) THEN
	    IF(GUI_DBG_UNIT.GT.0) THEN
		WRITE(GUI_DBG_UNIT,*)IAM(),'GUICMSG_DATA: Not Primary'
	    ENDIF
	    COMPLETION_CODE=2			!not Primary
	    GOTO 10000
	ENDIF
C
C If suppressed, reject with code 3
C
	IF(CON_PATH_THRU_SUPPRESS.EQ.1) THEN
	    IF(GUI_DBG_UNIT.GT.0) THEN
		WRITE(GUI_DBG_UNIT,*)IAM(),'GUICMSG_DATA: Suppressed'
	    ENDIF
	    COMPLETION_CODE=3			!suppresssed
	    GOTO 10000
	ENDIF
C
C If not signed-on, reject
C
	IF(.NOT.GUI_CONN_SIGNED_ON(CONN_INX)) THEN
	    IF(GUI_DBG_UNIT.GT.0) THEN
		WRITE(GUI_DBG_UNIT,*)IAM(),'GUICMSG_DATA: Not Signed-on'
	    ENDIF
	    COMPLETION_CODE = 7
	    GOTO 10000
	ENDIF
C
C Determine data class
C
	BUF_BOFF=GUI_LH_SZ+				! skip link header
     *		 GUI_PH_DATA_CLASS_OFF
	I4TEMP=0
	I1TEMP(1)=GUI_WORKER_BBUF(BUF_BOFF+1,BUF)
	DATA_CLASS=I4TEMP
C
C Determine device type
C
	BUF_BOFF=GUI_LH_SZ+GUI_PH_DEVICE_TYPE_OFF
	I4TEMP=0
	I1TEMP(1)=GUI_WORKER_BBUF(BUF_BOFF+1,BUF)
	DEVICE_TYPE=I4TEMP
C
C Determine source ID
C
	BUF_BOFF=GUI_LH_SZ+GUI_PH_SOURCE_ID_OFF
	I4TEMP=0
	I1TEMP(1)=GUI_WORKER_BBUF(BUF_BOFF+1,BUF)
	SOURCE_ID=I4TEMP
C
	IF(SOURCE_ID.NE.CONN_INX) THEN
	    IF(GUI_DBG_UNIT.GT.0) THEN
		WRITE(GUI_DBG_UNIT,*)IAM(),'GUICMSG_DATA: Bad Source ID'
	    ENDIF
	    COMPLETION_CODE = 8
	    GOTO 10000
	ENDIF
C
C Determine destination ID
C
	BUF_BOFF=GUI_LH_SZ+GUI_PH_DEST_ID_OFF
	I4TEMP=0
	I1TEMP(1)=GUI_WORKER_BBUF(BUF_BOFF+1,BUF)
	DEST_ID=I4TEMP
	IF(DEST_ID.GT.100) THEN
C
	  DEST_ID=I4TEMP - 100				! DEST is task ID + 100
C
	  IF(DEST_ID.LT.1.OR.DEST_ID.GT.NUMTSK) THEN
C
C For now only GOLS task numbers are allowed
C
	    CALL FASTSET(BLANK,GUI_MES_BUF,33)
	    WRITE(GUI_MES_CBUF,9020) IAM(), DEST_ID+100
9020	    FORMAT(A,'GUICMSG_DATA: Bad DEST ID:',I4)
	    CALL WRITEBRK(GUI_MES_CBUF)
	    COMPLETION_CODE = 9
	    GOTO 10000
	  ENDIF
	ENDIF
C
C Get User Data
C
	BUF_BOFF=GUI_LH_SZ+GUI_PH_USER_DATA_OFF
	I4TEMP=0
	I1TEMP(1)=GUI_WORKER_BBUF(BUF_BOFF+1,BUF)
	I1TEMP(2)=GUI_WORKER_BBUF(BUF_BOFF+2,BUF)
	USER_DATA=I4TEMP
C
C Get AFI
C
	BUF_BOFF=GUI_LH_SZ+GUI_PH_AFI_OFF
	I4TEMP=0
	I1TEMP(1)=GUI_WORKER_BBUF(BUF_BOFF+1,BUF)
	ADDR_FORMAT=I4TEMP
C
C Get address length
C
	BUF_BOFF=GUI_LH_SZ+GUI_PH_ADDR_LEN_OFF
	I4TEMP=0
	I1TEMP(1)=GUI_WORKER_BBUF(BUF_BOFF+1,BUF)
	ADDR_LEN=I4TEMP
C
C Get address
C
	IF(ADDR_LEN.GT.0) THEN
	    BUF_BOFF=GUI_LH_SZ+GUI_PH_ADDR_OFF
  	    CALL MOVBYT(GUI_WORKER_BBUF(BUF_BOFF+1,BUF),1,
     *			ADDRESS,1,ADDR_LEN)
	ENDIF
C
C Get message length (message includes PH header)
C
	I4TEMP=0
	I1TEMP(1) = GUI_WORKER_BBUF(2,BUF)
	I1TEMP(2) = GUI_WORKER_BBUF(1,BUF)
	BUF_BOFF=GUI_LH_SZ
	MSG_LEN = I4TEMP - BUF_BOFF
C
C Get message
C
	IF(MSG_LEN.GT.0) THEN
  	    CALL MOVBYT(GUI_WORKER_BBUF(BUF_BOFF+1,BUF),1,
     *			MESSAGE,1,MSG_LEN)
	ENDIF
C
C Call application GUIice receive routine
C
	CALL GUIRECV(DATA_CLASS, ADDR_FORMAT, MSG_LEN, MESSAGE, RET_CODE, 
     *               BUF, OUT_MSG_LEN, OUT_BUF)
C
	IF(RET_CODE.NE.0) THEN				! bad address
	    COMPLETION_CODE = 9
	    GOTO 10000
	ENDIF
C
10000	CONTINUE
	RETURN
	END
