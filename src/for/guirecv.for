C GUIRECV.FOR
C
C V02 08-NOV-2000 UXN Unsolicited messages removed for now.
C V01 04-Aug-1993 MP  INITIAL RELEASE FOR VAX
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
C This subroutine accepts Path Thru message(s) from GUICMSG_DATA routine, 
C places them into PROCOM buffer(s) in format depending on the DATA_CLASS or
C DEST_ID and sends PROCOM buffer(s) to appropriate GOLS queue(s).
C
C Input parameters:
C
C	INTEGER*4 DATA_CLASS	- data class (2 for phase one, July 1993)
C	INTEGER*4 DEVICE_TYPE	- device type (1 for phase one, July 1993)
C	INTEGER*4 SOURCE_ID	- source ID (0 for phase one, July 1993)
C	INTEGER*4 DEST_ID	- destination ID (0 for phase one, July 1993)
C	INTEGER*4 USER_DATA     - 2-byte user data (0 for phase 1)  
C	INTEGER*4 ADDR_FORMAT	- address format
C	INTEGER*4 ADDR_LEN	- address length
C	BYTE	  ADDRESS(*)	- address 
C	INTEGER*4 MSG_LEN	- message length (GUI_DAT_SIZ*4 -
C						  GUI_LH_SZ -
C						  GUI_PH_SZ_MAX =
C						  450 currently)
C	BYTE	  MESSAGE(*)	- message
C
C Output parameters:
C
C	INTEGER*4 RET_CODE:
C		   0		-  no error, message accepted;
C		 value >= 11	-  error number to be sent to Client.
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUIRECV(DATA_CLASS,ADDR_FORMAT, 
     *                     MSG_LEN, MESSAGE, RET_CODE, BUF, OUT_MSG_LEN,
     *                     OUT_BUF)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
	INTEGER*4   DATA_CLASS
	INTEGER*4   ADDR_FORMAT	! address format
	INTEGER*4   MSG_LEN     ! Byte length of message  ....
	BYTE	    MESSAGE(*)  ! message
	INTEGER*4   RET_CODE
	INTEGER*4   BUF
	BYTE        OUT_BUF(*)
	INTEGER*4   OUT_MSG_LEN	
C
	INTEGER*4   I4TEMP
	INTEGER*2	I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE(I4TEMP, I2TEMP, I1TEMP)
C
	INTEGER*4	MES_BUF(33)
	CHARACTER*132	MES_CBUF
	EQUIVALENCE	(MES_BUF, MES_CBUF)
	INTEGER*4   SOURCE_ID	! source ID (0 for phase one, July 1993)
C
	INTEGER*4   BLANK
	DATA	    BLANK/'    '/
C
	INTEGER*4	LH_TYPE,CLIENT_ID,HOST_ID
C
	RET_CODE  = 0
	SOURCE_ID = GUI_WORKER_SOURCE_ID(BUF)
C
	IF(ADDR_FORMAT.EQ.GUI_ADDR_FORMAT_MISSING) THEN
C
C DO FO DATA CLASS
C
	  IF (DATA_CLASS.EQ.6) THEN        ! CHECK FOR NCC RPC TYPE 06
C                                          ! =========================
	    CALL GUIPASS_006(MESSAGE,MSG_LEN,RET_CODE)
	    IF (RET_CODE.EQ.0) THEN
	      LH_TYPE=GUI_LH_TYPE_DATA_MSG
	      OUT_MSG_LEN=GUI_LH_SZ+MSG_LEN
C
	    ELSEIF (RET_CODE.EQ.11) THEN
	      LH_TYPE=GUI_LH_TYPE_DATA_REJECT
C
C  ORIGINAL MESSAGE LEN FROM CLIENT IS REV BYTED
C
	      I1TEMP(2)=GUI_WORKER_BBUF(1,BUF)
	      I1TEMP(1)=GUI_WORKER_BBUF(2,BUF)
C
	      OUT_MSG_LEN=GUI_LH_DATA_REJECT_HED_SIZ+I2TEMP(1)+GUI_LH_SZ
	    ELSE
	      RETURN      !  SHOULD NEVER GET HERE, BUT IN CASE WE DO 
C                         !  JUST RETURN AND HOPE FOR THE BEST.
	    ENDIF
	    CLIENT_ID=SOURCE_ID
	    HOST_ID=ICHAR('A')+P(SYSNAM)-1
C
	    CALL GUICMSG_OUT_TO_CLIENT_DATA(LH_TYPE, RET_CODE, HOST_ID, 
     *                     MESSAGE, MSG_LEN, BUF, OUT_BUF, OUT_MSG_LEN)
	    RETURN
	  ELSEIF (DATA_CLASS.EQ.20) THEN   ! CHECK FOR GUI RPC TYPE 20
C                                          ! =========================
	    CALL GUIPASS_020(MESSAGE,MSG_LEN,RET_CODE)
	    IF (RET_CODE.EQ.0) THEN
	      LH_TYPE=GUI_LH_TYPE_DATA_MSG
	      OUT_MSG_LEN=GUI_LH_SZ+MSG_LEN
C
	    ELSEIF (RET_CODE.EQ.11) THEN
	      LH_TYPE=GUI_LH_TYPE_DATA_REJECT
C
C  ORIGINAL MESSAGE LEN FROM CLIENT IS REV BYTED
C
	      I1TEMP(2)=GUI_WORKER_BBUF(1,BUF)
	      I1TEMP(1)=GUI_WORKER_BBUF(2,BUF)
C
	      OUT_MSG_LEN=GUI_LH_DATA_REJECT_HED_SIZ+I2TEMP(1)+GUI_LH_SZ
	    ELSE
C
C  SHOULD NEVER GET HERE, BUT IN CASE WE DO 
C  JUST RETURN AND HOPE FOR THE BEST.
C
	      RETURN
	    ENDIF
	    CLIENT_ID=SOURCE_ID
	    HOST_ID=ICHAR('A')+P(SYSNAM)-1
C
	    CALL GUICMSG_OUT_TO_CLIENT_DATA(LH_TYPE, RET_CODE, HOST_ID, 
     *                     MESSAGE, MSG_LEN, BUF, OUT_BUF, OUT_MSG_LEN)
	    RETURN
	  ELSEIF (DATA_CLASS.EQ.21) THEN   ! CHECK FOR GUI RPC TYPE 21
C                                          ! =========================
	    CALL GUIPASS_021(MESSAGE,MSG_LEN,RET_CODE)
	    IF (RET_CODE.EQ.0) THEN
	      LH_TYPE=GUI_LH_TYPE_DATA_MSG
	      OUT_MSG_LEN=GUI_LH_SZ+MSG_LEN
C
	    ELSEIF (RET_CODE.EQ.11) THEN
	      LH_TYPE=GUI_LH_TYPE_DATA_REJECT
C
C  ORIGINAL MESSAGE LEN FROM CLIENT IS REV BYTED
C
	      I1TEMP(2)=GUI_WORKER_BBUF(1,BUF)
	      I1TEMP(1)=GUI_WORKER_BBUF(2,BUF)
C
	      OUT_MSG_LEN=GUI_LH_DATA_REJECT_HED_SIZ+I2TEMP(1)+GUI_LH_SZ
	    ELSE
C
C  SHOULD NEVER GET HERE, BUT IN CASE WE DO 
C  JUST RETURN AND HOPE FOR THE BEST.
C
	      RETURN
	    ENDIF
	    CLIENT_ID=SOURCE_ID
	    HOST_ID=ICHAR('A')+P(SYSNAM)-1
C
	    CALL GUICMSG_OUT_TO_CLIENT_DATA(LH_TYPE, RET_CODE, HOST_ID, 
     *                     MESSAGE, MSG_LEN, BUF, OUT_BUF, OUT_MSG_LEN)
	    RETURN
	  ENDIF
C
C to be defined ..........................................
C
	ELSE IF(ADDR_FORMAT.EQ.GUI_ADDR_FORMAT_TER_NR) THEN
C
C to be defined
C
	ELSE IF(ADDR_FORMAT.EQ.GUI_ADDR_FORMAT_LINE_DROP) THEN
C
C to be defined ..........................................
C
	ELSE IF(ADDR_FORMAT.EQ.GUI_ADDR_FORMAT_STA_PRT_DRP) THEN
C
C to be defined ..........................................
C
	ELSE IF(ADDR_FORMAT.EQ.GUI_ADDR_FORMAT_PHYSICAL) THEN
C
C to be defined ..........................................
C
	ELSE IF(ADDR_FORMAT.EQ.GUI_ADDR_FORMAT_X121) THEN
C
C to be defined ..........................................
C
	ELSE IF(ADDR_FORMAT.EQ.GUI_ADDR_FORMAT_AGT_NR) THEN
C
C to be defined
C
	ENDIF
C
C bad address format
C
	CALL FASTSET(BLANK,MES_BUF,33)
	WRITE(MES_CBUF,9030) IAM(), ADDR_FORMAT
9030	FORMAT(A,'GUIRECV: Bad Address Format:',I)
	CALL WRITEBRK(MES_CBUF)
	RET_CODE = -1
	GOTO 10000
C
10000	CONTINUE
	RETURN
	END
