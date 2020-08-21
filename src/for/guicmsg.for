C GUICMSG.FOR
C
C V04 07-FEB-2001 UXN Sequence number fixed.
C V03 19-NOV-2000 HXK Output message length fixed for errors.
C V02 14-NOV-2000 UXN GUIWORK changed to multi-threaded.
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
C This subroutine processes input messages assembled by GUICBUF routine;
C messages reside in GUI_ASMB_BUFs.
C
C Input parameters:
C
C     None
C
C Output parameters:
C
C Results:
C
C     Output messages formatted in GUI_OUT_BUF destined to 
C     a.GUI_TO_LINK_QUE with responses to Clients.
C	 or
C     b.to GOLS queue (not needed for CA Aug 1993 install)
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUICMSG(BUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
C
	INTEGER*4   BUF
	INTEGER*4   CONN_INX		!connection number
	INTEGER*4   BUF_CUR		!buffer number
	INTEGER*4   LH_TYPE		!Link header type
	INTEGER*4   COMPLETION_CODE
	INTEGER*4   CLIENT_ID
	INTEGER*4   HOST_ID
	INTEGER*4   OUT_MSG_LEN
	INTEGER*4   MOVE_SIZE
	INTEGER*4   ST                  !Work variable
	INTEGER*4   BUF_BOFF		!buffer byte offset
	INTEGER*4   MAX_DATA_TO_MOVE   
	PARAMETER(MAX_DATA_TO_MOVE = 
     *            GUI_MAX_MSG_LEN - (GUI_LH_SZ + GUI_LH_DATA_REJECT_HED_SIZ) )
C
	INTEGER*4   MESNR
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
	INTEGER*4   BLANK
	DATA	    BLANK/'    '/
	LOGICAL*4   REJECT
C
	BYTE	    OUT_BUF(4*GUI_BUF_SIZ)
C
	CONN_INX = GUI_WORKER_SOURCE_ID(BUF)
C
C THE Length of the message is already checked by GUILINK.
C Check message number (LINK HEADER OFFSETS ARE FROM 0!)
C
	I4TEMP=0
	I1TEMP(2)=GUI_WORKER_BBUF(GUI_LH_MSG_NR_OFF+1,BUF)
	I1TEMP(1)=GUI_WORKER_BBUF(GUI_LH_MSG_NR_OFF+2,BUF)
C
	GUI_CONN_LAST_MSN_IN(CONN_INX) = I4TEMP
	MESNR = I4TEMP
C
C Check TIME 
C
	  I4TEMP=0
	  I1TEMP(4)=GUI_WORKER_BBUF(GUI_LH_TIME_OFF+1,BUF)
	  I1TEMP(3)=GUI_WORKER_BBUF(GUI_LH_TIME_OFF+2,BUF)
	  I1TEMP(2)=GUI_WORKER_BBUF(GUI_LH_TIME_OFF+3,BUF)
	  I1TEMP(1)=GUI_WORKER_BBUF(GUI_LH_TIME_OFF+4,BUF)
	  IF(I4TEMP.LT.5*3600) I4TEMP=I4TEMP+24*3600
C
	  GUI_CONN_LAST_TIME(CONN_INX) = I4TEMP
C
C Check message type 
C
	  I4TEMP=0
	  I1TEMP(1)=GUI_WORKER_BBUF(GUI_LH_TYPE_OFF+1,BUF)
	  LH_TYPE = I4TEMP
	  REJECT = .FALSE.
290       CONTINUE
	  IF ((LH_TYPE.NE.GUI_LH_TYPE_SIGN_ON_REQ.AND.
     *	       LH_TYPE.NE.GUI_LH_TYPE_DATA_MSG.AND.
     *	       LH_TYPE.NE.GUI_LH_TYPE_SIGN_OFF_REQ.AND.
     *	       LH_TYPE.NE.GUI_LH_TYPE_LOOP_BACK_REQ).OR.REJECT) THEN
            IF(REJECT) GOTO 300
	    CALL FASTSET(BLANK, GUI_MES_BUF,33)
	    WRITE(GUI_MES_CBUF,9030) IAM(),I4TEMP,CONN_INX
9030	    FORMAT(A,'GUICMSG: bad TYPE received:',I4,' connection:',I4)
	    CALL WRITEBRK(GUI_MES_CBUF)
C
	    COMPLETION_CODE=1				! Invalid type
C
300	    CONTINUE				! DATA REJECT back to Client
C
C move input message to output buffer
C
            I4TEMP=0
            I1TEMP(2)=GUI_WORKER_BBUF(GUI_LH_LEN_OFF+1,BUF)
            I1TEMP(1)=GUI_WORKER_BBUF(GUI_LH_LEN_OFF+2,BUF)

	    MOVE_SIZE = I4TEMP

	    IF(MOVE_SIZE.LT.0) MOVE_SIZE = 0
	    IF(MOVE_SIZE.GT.MAX_DATA_TO_MOVE) MOVE_SIZE = MAX_DATA_TO_MOVE

            CALL MOVBYT(GUI_WORKER_BBUF(1,BUF),1,
     *          OUT_BUF(GUI_BUF_DAT_OFF),
     *          GUI_LH_SZ+GUI_LH_DATA_REJECT_HED_SIZ+1,MOVE_SIZE)
C
            OUT_MSG_LEN = MOVE_SIZE + GUI_LH_SZ + GUI_LH_DATA_REJECT_HED_SIZ
C
	    LH_TYPE=GUI_LH_TYPE_DATA_REJECT		! Data reject
	    CLIENT_ID=0
	    HOST_ID=0
C
	    CALL GUICMSG_OUT_TO_CLIENT(LH_TYPE, COMPLETION_CODE, HOST_ID, 
     *                                 BUF, OUT_BUF, OUT_MSG_LEN)
	    GOTO 900
	  ENDIF
C
C Switch on message type
C
	  IF(LH_TYPE.EQ.GUI_LH_TYPE_SIGN_ON_REQ) THEN		! sign-on
	    CALL GUICMSG_SIGN_ON(BUF,OUT_BUF,OUT_MSG_LEN)
	  ELSE IF(LH_TYPE.EQ.GUI_LH_TYPE_DATA_MSG) THEN	! data
	    CALL GUICMSG_DATA(COMPLETION_CODE,BUF,OUT_BUF,OUT_MSG_LEN)
C
C  11 IS CURRENTLY USED FOR ALL RPC ERROR MESSAGES
C
	    IF (COMPLETION_CODE.NE.0.AND.
     *        COMPLETION_CODE.NE.11) THEN			! bad data
	      REJECT = .TRUE.
	      GOTO 290
            ENDIF
	  ELSE IF(LH_TYPE.EQ.GUI_LH_TYPE_SIGN_OFF_REQ) THEN	! sign-off
	    CALL GUICMSG_SIGN_OFF(BUF,OUT_BUF,OUT_MSG_LEN)
	  ELSE IF(LH_TYPE.EQ.GUI_LH_TYPE_LOOP_BACK_REQ) THEN	! loopback
	    CALL GUICMSG_LOOP_BACK(BUF,OUT_BUF,OUT_MSG_LEN)
	  ENDIF
C
900	  CONTINUE
C
C Process OUT_BUF messages
C
C
C Try to send the message
C
1000	    CONTINUE
	    CALL RTL(BUF_CUR, GUI_LINK_FRE_QUE, ST)
	    IF(ST.EQ.2) THEN
		CALL OPSTXT('GUICMSG: no free GUI_LINK_FRE_QUE buffers')
	        CALL XWAIT(50,1,ST)
		GOTO 1000		    ! no buffers
	    ENDIF
C
C Check buffer condition
C
	    CALL GUIMGR_CHECK_BUF(BUF_CUR,'GUIHBUF')
C
C set up the buffer
C
	    GUI_LINK_BUF(GUI_BUF_NUM_OFF, BUF_CUR) = BUF_CUR
	    GUI_LINK_BUF(GUI_BUF_LEN_OFF, BUF_CUR) = OUT_MSG_LEN
	    GUI_LINK_BUF(GUI_BUF_IO_INX_OFF, BUF_CUR) = 0
	    GUI_LINK_BUF(GUI_BUF_IO_STS_OFF, BUF_CUR) = 0
	    GUI_LINK_BUF(GUI_BUF_ERR_OFF, BUF_CUR) = 0
	    GUI_LINK_BUF(GUI_BUF_CONN_OFF, BUF_CUR) = CONN_INX
C
	    CALL MOVBYT(OUT_BUF,1,	    ! copy data
     *		  GUI_LINK_BBUF(GUI_BUF_DAT_BOFF, BUF_CUR),1,
     *		  OUT_MSG_LEN) 
C
C put message nr 
C	
	    I4TEMP = MESNR

	    BUF_BOFF=GUI_BUF_DAT_BOFF-1+GUI_LH_MSG_NR_OFF
	    GUI_LINK_BBUF(BUF_BOFF+1,BUF_CUR)=I1TEMP(2)
	    GUI_LINK_BBUF(BUF_BOFF+2,BUF_CUR)=I1TEMP(1)
C
	    IF(GUI_DBG_UNIT.GT.0) THEN
	      CALL GUICBUF_DUMP_BUF(GUI_DBG_UNIT,
     *		'GUICMSG:write buffer ',
     *		GUI_LINK_BUF(1,BUF_CUR),BUF_CUR)
	    ENDIF
C
C
C put buffer on the queue
C
	    CALL ABL(BUF_CUR, GUI_TO_LINK_QUES(1,CONN_INX), ST)
C
C release GUI_WORKER_BUF
C
	    CALL ABL(BUF, GUI_WORKER_FREE_QUEUE, ST)

	RETURN
	END
