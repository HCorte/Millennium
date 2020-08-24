C GUICMSG_SIGN_OFF.FOR
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
C INPUT:      CONN_INX
C OUTPUT:     none
C RESULTS:    response message to Sign-off request
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUICMSG_SIGN_OFF(BUF, OUT_BUF, OUT_MSG_LEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
C
	INTEGER*4   CONN_INX		!connection number
	INTEGER*4   BUF
	BYTE	    OUT_BUF(*)
	INTEGER*4   LH_TYPE
	INTEGER*4   COMPLETION_CODE
	INTEGER*4   CLIENT_ID
	INTEGER*4   HOST_ID
	INTEGER*4   MSG_OFF
	INTEGER*4   AUTH_INX
	INTEGER*4   OUT_MSG_LEN
	BYTE	     TEXTB(32)
	CHARACTER*32 TEXTC
	EQUIVALENCE (TEXTC,TEXTB)
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
	INTEGER*4   BLANK
	DATA	    BLANK/'    '/
C
	HOST_ID=0		!initialize
	CONN_INX = GUI_WORKER_SOURCE_ID(BUF)
C
C Get client ID
C
	MSG_OFF=GUI_LH_SZ+1
	I4TEMP=0
	I1TEMP(1)=GUI_WORKER_BBUF(MSG_OFF, BUF)
	CLIENT_ID=I4TEMP			!save for output
	MSG_OFF=MSG_OFF+1
C
	MSG_OFF=MSG_OFF+1			!skip spare
C
	CALL MOVBYT(GUI_WORKER_BBUF(1,BUF),MSG_OFF,TEXTB,1,32)  ! system
	CALL FASTSET(BLANK, GUI_MES_BUF,33)
	WRITE(GUI_MES_CBUF,9010) IAM(), CLIENT_ID, TEXTC
9010	FORMAT(A,'GUICMSG: Sign-off request by client:',I4,' system ',A)
	CALL WRITEBRK(GUI_MES_CBUF)
C
C If not Primary, reject with code 2
C
	IF(P(SYSTYP).NE.LIVSYS) THEN
	    CALL FASTSET(BLANK, GUI_MES_BUF,33)
	    WRITE(GUI_MES_CBUF,9020) IAM()
9020	    FORMAT(A,'GUICMSG: Sign-off reject: not Primary')
	    CALL WRITEBRK(GUI_MES_CBUF)
C
	    COMPLETION_CODE=2			!not Primary
	    GOTO 8000
	ENDIF
C
C If suppressed, reject with code 3
C
	IF(CON_PATH_THRU_SUPPRESS.EQ.1) THEN
	    WRITE(GUI_MES_CBUF,9030) IAM()
9030	    FORMAT(A,'GUICMSG: Sign-off reject: function suppressed')
	    CALL WRITEBRK(GUI_MES_CBUF)
C
	    COMPLETION_CODE=3			!suppresssed
	    GOTO 8000
	ENDIF
C
C Check client ID
C
	IF(CONN_INX.NE.CLIENT_ID) THEN
	    WRITE(GUI_MES_CBUF,9040) IAM()
9040	    FORMAT(A,'GUICMSG: Sign-off reject: wrong client id')
	    CALL WRITEBRK(GUI_MES_CBUF)
C
	    COMPLETION_CODE=8			!wrong ID
	    GOTO 8000
	ENDIF
C
	IF(.NOT.GUI_CONN_SIGNED_ON(CONN_INX)) THEN
	    WRITE(GUI_MES_CBUF,9050) IAM()
9050	    FORMAT(A,'GUICMSG: Sign-off reject: not signed-on')
	    CALL WRITEBRK(GUI_MES_CBUF)
C
	    COMPLETION_CODE=7			!not signed-on
	    GOTO 8000
	ENDIF
C
	AUTH_INX = GUI_CONN_AUTH_INX(CONN_INX)
	CALL FASTSET(BLANK, GUI_MES_BUF,33)
	WRITE(GUI_MES_CBUF,9060) IAM(),GUI_AUTH_NAME(AUTH_INX)
9060	FORMAT(A,'GUICMSG: Signed-off Name: ',A)
	CALL WRITEBRK(GUI_MES_CBUF)
C
	CALL FASTSET(BLANK, GUI_MES_BUF,33)
	WRITE(GUI_MES_CBUF,9070) IAM(),GUI_CONN_SYSTEM_NAME(CONN_INX)
9070	FORMAT(A,'GUICMSG: Signed-off System: ',A)
	CALL WRITEBRK(GUI_MES_CBUF)
C
	GUI_CONN_SIGNED_ON(CONN_INX) = .FALSE.
	COMPLETION_CODE=0			!OK
	HOST_ID=0
C
8000	CONTINUE
	LH_TYPE=GUI_LH_TYPE_SIGN_OFF_RESP		!sign-off response
	OUT_MSG_LEN=GUI_LH_SZ+2
	CALL GUICMSG_OUT_TO_CLIENT(LH_TYPE, COMPLETION_CODE, HOST_ID, 
     *                             BUF, OUT_BUF, OUT_MSG_LEN)
C
	RETURN
	END
