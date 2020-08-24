C GUICMSG_OUT_TO_CLIENT.FOR
C
C V02 13-NOV-2000 UXN Multi-threaded GUIMGR changes.
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
C Builds Link header and some other data depending on LH_TYPE
C INPUT:      
C	OUT_BUF    	- output message
C	LH_TYPE		- link header type
C	COMPLETION_CODE	- completeion code to be sent to client
C	OUT_MSG_LEN	- output message length
C	CLIENT_ID	- client id
C	HOST_ID		- host id (A, B, C, D)
C OUTPUT:     none
C RESULTS:    response messages built in GUI_OUT_BUFs
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUICMSG_OUT_TO_CLIENT(LH_TYPE, COMPLETION_CODE, HOST_ID, 
     *                                   BUF, OUT_BUF, OUT_MSG_LEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
C
	BYTE	OUT_BUF(4*GUI_BUF_SIZ)
	INTEGER*4   LH_TYPE
	INTEGER*4   COMPLETION_CODE
	INTEGER*4   OUT_MSG_LEN
	INTEGER*4   CLIENT_ID
	INTEGER*4   HOST_ID
	INTEGER*4   OUT_BOFF
	INTEGER*4   MSG_LEN
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
	BYTE	MESSAGE(*)
	INTEGER*4   BUF
C

	ENTRY GUICMSG_OUT_TO_CLIENT_DATA(LH_TYPE, COMPLETION_CODE,
     *	                                 HOST_ID, MESSAGE, MSG_LEN, 
     *                                   BUF, OUT_BUF, OUT_MSG_LEN)
C
	OUT_BOFF=GUI_LH_LEN_OFF
C
	CLIENT_ID = GUI_WORKER_SOURCE_ID(BUF)
C
C put length
C
	I4TEMP=OUT_MSG_LEN
	OUT_BUF(OUT_BOFF+1)=I1TEMP(2)
	OUT_BUF(OUT_BOFF+2)=I1TEMP(1)
	OUT_BOFF=OUT_BOFF+2
C
C skip message nr - has to be done right before buffer is placed on the queue
C
	OUT_BUF(OUT_BOFF+1)=0
	OUT_BUF(OUT_BOFF+2)=0
	OUT_BOFF=OUT_BOFF+2
C
C put time
C
	CALL ICLOCK(2,I4TEMP)
	OUT_BUF(OUT_BOFF+1)=I1TEMP(4)
	OUT_BUF(OUT_BOFF+2)=I1TEMP(3)
	OUT_BUF(OUT_BOFF+3)=I1TEMP(2)
	OUT_BUF(OUT_BOFF+4)=I1TEMP(1)
	OUT_BOFF=OUT_BOFF+4
C
C spare is 0
C
	I4TEMP=0
	OUT_BUF(OUT_BOFF+1)=I1TEMP(1)
	OUT_BOFF=OUT_BOFF+1
C
C put type
C
	I4TEMP=LH_TYPE
	OUT_BUF(OUT_BOFF+1)=I1TEMP(1)
	OUT_BOFF=OUT_BOFF+1
C
C spool sequence (TM serial) nr =0
C 
	I4TEMP=0
	OUT_BUF(OUT_BOFF+1)=I1TEMP(4)
	OUT_BUF(OUT_BOFF+2)=I1TEMP(3)
	OUT_BUF(OUT_BOFF+3)=I1TEMP(2)
	OUT_BUF(OUT_BOFF+4)=I1TEMP(1)
	OUT_BOFF=OUT_BOFF+4
C
C check the LH_TYPE
C
	IF(LH_TYPE.EQ.GUI_LH_TYPE_SIGN_ON_RESP) THEN
C
C put completion code, client ID, Host ID
C
	    I4TEMP=COMPLETION_CODE
	    OUT_BUF(OUT_BOFF+1)=I1TEMP(1)
	    OUT_BOFF=OUT_BOFF+1
C
	    I4TEMP=CLIENT_ID
	    OUT_BUF(OUT_BOFF+1)=I1TEMP(1)
	    OUT_BOFF=OUT_BOFF+1
C
	    I4TEMP=HOST_ID
	    OUT_BUF(OUT_BOFF+1)=I1TEMP(1)
	    OUT_BOFF=OUT_BOFF+1
C
	ELSE IF(LH_TYPE.EQ.GUI_LH_TYPE_SIGN_OFF_RESP) THEN
C
C put completion code, client ID
C
	    I4TEMP=COMPLETION_CODE
	    OUT_BUF(OUT_BOFF+1)=I1TEMP(1)
	    OUT_BOFF=OUT_BOFF+1
C
	    I4TEMP=CLIENT_ID
	    OUT_BUF(OUT_BOFF+1)=I1TEMP(1)
	    OUT_BOFF=OUT_BOFF+1
C
C
	ELSE IF(LH_TYPE.EQ.GUI_LH_TYPE_DATA_REJECT) THEN
C
C put 'DREJ', completion code
C
	    OUT_BUF(OUT_BOFF+1)='D'
	    OUT_BUF(OUT_BOFF+2)='R'
	    OUT_BUF(OUT_BOFF+3)='E'
	    OUT_BUF(OUT_BOFF+4)='J'
	    OUT_BOFF=OUT_BOFF+4
C
	    I4TEMP=COMPLETION_CODE
	    OUT_BUF(OUT_BOFF+1)=I1TEMP(2)
	    OUT_BUF(OUT_BOFF+2)=I1TEMP(1)
	    OUT_BOFF=OUT_BOFF+2
C
C  ORIGINAL MESSAGE LEN FROM CLIENT IS REV BYTED
C
	    I4TEMP = 0
	    I1TEMP(2)=GUI_WORKER_BBUF(1,BUF)
	    I1TEMP(1)=GUI_WORKER_BBUF(2,BUF)
C
C  PUT ENTIRE MESSAGE FROM ASMB_BBUF TO OUT_BBUF.
C
	    CALL MOVBYT(GUI_WORKER_BBUF(1,BUF),1,
     *                  OUT_BUF(OUT_BOFF),2,
     *                  I4TEMP)
C
	ELSE IF(LH_TYPE.EQ.GUI_LH_TYPE_LOOP_BACK_RESP) THEN
C
C nothing to do
C
	ELSEIF(LH_TYPE.EQ.GUI_LH_TYPE_DATA_MSG) THEN
C
C  MOVE RPC MESSAGE INTO OUTPUT BYTE BUFFER
C
	  CALL MOVBYT(MESSAGE,1,OUT_BUF(OUT_BOFF),2,MSG_LEN)
C
	ENDIF
C
	RETURN
	END
