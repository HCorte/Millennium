C GUICMSG_LOOP_BACK.FOR
C
C V02 13-NOV-2000 UXN GUI prefix added.
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
C INPUT:      CONN_INX
C OUTPUT:     none
C RESULTS:    response message to Loop-back request
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUICMSG_LOOP_BACK(BUF,OUT_BUF,OUT_MSG_LEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
C
	INTEGER*4   BUF
	BYTE        OUT_BUF(*)
	INTEGER*4   LH_TYPE
	INTEGER*4   COMPLETION_CODE
	INTEGER*4   CLIENT_ID
	INTEGER*4   HOST_ID
	INTEGER*4   MSG_OFF
	INTEGER*4   OUT_MSG_LEN
	INTEGER*4   I4TEMP
	BYTE        I1TEMP(4)
	EQUIVALENCE(I4TEMP,I1TEMP)
C
	MSG_OFF=GUI_LH_SZ+1
	I4TEMP = 0
	I1TEMP(2) = GUI_WORKER_BBUF(1,BUF)
	I1TEMP(1) = GUI_WORKER_BBUF(2,BUF)
	OUT_MSG_LEN = I4TEMP
	CALL MOVBYT(GUI_WORKER_BBUF(1,BUF),MSG_OFF,
     *		    OUT_BUF,MSG_OFF, 
     *		    OUT_MSG_LEN-GUI_LH_SZ)
C
	LH_TYPE=GUI_LH_TYPE_LOOP_BACK_RESP		!loop-back response
	CLIENT_ID=0		!initialize
	HOST_ID=0		!initialize
	COMPLETION_CODE=0
C
	CALL GUICMSG_OUT_TO_CLIENT(LH_TYPE, COMPLETION_CODE, HOST_ID, 
     *                             BUF, OUT_BUF, OUT_MSG_LEN)
C
	RETURN
	END
