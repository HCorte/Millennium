C GUIWRIOCOMP.FOR
C
C V02 31-OCT-2000 UXN GUI prefix added.
C V01 16-JUN-1993 MP  INITIAL RELEASE FOR VAX (Produced From TCPASST).
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
C Copyright 1991-1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C		This routine is called once a write completes. If a write
C		completes with a good status then another write is started
C		using the DOWRIT routine. The GUI buffer is added to the 
C		GUI_FROM_LINK_QUES. If a write completes with an error then an
C		error message is printed. The buffer status is set
C		to GUI_BADWRT. If the connection status is currently connected 
C		then the routine forces a disconnect by calling GUITCPPDODISC. 
C		In this case the buffer status is set to GUI_DISWRT. The
C		GUI buffer is added to the GUI_FROM_LINK_QUES.
C
C INPUT:
C	CONN_WIO - following product (CONN-1)*MAX_CONN+WIO
C OUTPUT:
C	none
C RESULTS:
C	buffer with proper status is placed on one of GUI_FROM_LINK_QUES
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUITCPPWRIOCOMP(CONN_WIO)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
	INCLUDE 'INCLIB:GUILCOM.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4 CONN_WIO	!CONN and IOSB COUNTER
	INTEGER*4 CONN_CUR	!Current connection
	INTEGER*4 WIO	        !WRITE IOSB COUNTER
	INTEGER*4 BUF_CUR	!GUI_BUF #
	INTEGER*4 IERR		!ABL ERROR STATUS
	LOGICAL   RED_IGN_ONLY  ! If .FALSE. no matter what RED_IGNORE is
C
	INTEGER*4   BLANK
	DATA	    BLANK/'    '/
C
	CONN_CUR = CONN_WIO/GUI_MAX_WRITES+1
	WIO      = MOD(CONN_WIO,GUI_MAX_WRITES)
	IF(GUI_DBG_UNIT.NE.0) THEN
	  TYPE *,IAM(),'GUILWRIOCOMP: CONN, WIO ', CONN_CUR, WIO
	ENDIF
C
	IF(CONN_CUR.LT.1 .OR. CONN_CUR.GT.GUI_MAX_CONN) THEN
	  CALL FASTSET(BLANK,GUI_MES_BUF,33)
	  WRITE(GUI_MES_CBUF,9000) IAM(), CONN_CUR,WIO
9000	  FORMAT(A,'GUILWRIOCOMP: Invalid CONN,WIO  # ',2(I))
	  CALL WRITEBRK(GUI_MES_CBUF)
	  RETURN
	ENDIF
C
        GUI_WRITE_OUT(WIO,CONN_CUR)=GUI_READY
C
	BUF_CUR=GUI_WRITE_BUF(WIO,CONN_CUR)
	IF(GUI_DBG_UNIT.NE.0) THEN
	  TYPE *,IAM(),'GUILWRIOCOMP:  BUF_CUR',BUF_CUR
	ENDIF
C
	IF(BUF_CUR.LT.1 .OR. BUF_CUR.GT.GUI_LINK_BUFS) THEN
	  CALL FASTSET(BLANK,GUI_MES_BUF,33)
	  WRITE(GUI_MES_CBUF,9100) IAM(),BUF_CUR
9100	  FORMAT(A,'GUILWRIOCOMP: Invalid GUI Buffer  # ',I)
	  CALL WRITEBRK(GUI_MES_CBUF)
	  RETURN
	ENDIF
C
	IF(GUI_WRITE_IOSB(WIO,CONN_CUR).STAT .EQ. SS$_NORMAL) THEN
	  IF(GUI_DBG_UNIT.NE.0) THEN
	    TYPE *,IAM(),'GUILWRIOCOMP : GOOD WRITE'
	  ENDIF
	  GUI_WRITES(CONN_CUR) = GUI_WRITES(CONN_CUR) + 1
	  GUI_LINK_BUF(GUI_BUF_IO_STS_OFF,BUF_CUR) = GUI_GODWRT
	  GUI_LINK_BUF(GUI_BUF_ERR_OFF,BUF_CUR) = 0
	  CALL ABL(BUF_CUR,GUI_FROM_LINK_QUES(1,CONN_CUR),IERR)
	  CALL GUITCPPDOWRIT(CONN_CUR)
C
	ELSE
	  IF(GUI_READ_IOSB(WIO,CONN_CUR).STAT.NE.SS$_DISCONNECT
     *  .AND.GUI_READ_IOSB(WIO,CONN_CUR).STAT.NE.SS$_LINKDISCON) THEN
	    CALL FASTSET(BLANK,GUI_MES_BUF,33)
 	    WRITE(GUI_MES_CBUF,9200) IAM(),GUI_WRITE_IOSB(WIO,CONN_CUR).STAT
9200	    FORMAT(A,'GUILWRIOCOMP: Error completing write ',I)
	    CALL WRITEBRK(GUI_MES_CBUF)
	  ENDIF
C
	  GUI_WRITEERRS(CONN_CUR) = GUI_WRITEERRS(CONN_CUR) + 1
	  GUI_WRITELERR(CONN_CUR) = 
     *			  GUI_WRITE_IOSB(WIO,CONN_CUR).STAT
	  GUI_LINK_BUF(GUI_BUF_IO_STS_OFF,BUF_CUR) = GUI_BADWRT
	  GUI_LINK_BUF(GUI_BUF_ERR_OFF,BUF_CUR) = 
     *			  GUI_WRITE_IOSB(WIO,CONN_CUR).STAT
	  IF(GUI_CONN_STS(CONN_CUR).EQ.GUI_CONN_STS_CONNECTED) THEN
	    IF(GUI_DBG_UNIT.NE.0) THEN
	      TYPE *,IAM(),
     *	             'GUILWRIOCOMP :SETTING CONNECT FLAG TO DISCONNECTED'
	    ENDIF
	    GUI_LINK_BUF(GUI_BUF_IO_STS_OFF,BUF_CUR) = GUI_DISWRT
	    CALL ABL(BUF_CUR,GUI_FROM_LINK_QUES(1,CONN_CUR),IERR)
	    RED_IGN_ONLY=.FALSE.
	    CALL GUITCPPDODISC(CONN_CUR, RED_IGN_ONLY)
	  ELSE
	    CALL ABL(BUF_CUR,GUI_FROM_LINK_QUES(1,CONN_CUR),IERR)
	  ENDIF
	ENDIF
C
C
	RETURN
C
	END
