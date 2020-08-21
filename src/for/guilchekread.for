C GUICHEKREAD.FOR
C
C V02 31-OCT-2000 UXN GUITCP prefix added.
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
C		This routine gets a GUI buffer and attempts to start
C		a read (using the DOREAD routine) for each read that can be
C		outstanding. If there is no connection then the routine
C		starts a timer trap to connect.
C INOUT:
C	none
C OUTPUT:
C	none
C RESULTS:
C	read I/O is posted on all established connections
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUITCPPCHEKREAD
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
	INCLUDE 'INCLIB:GUILCOM.DEF'
C
	INTEGER*4 RIO	        !READ IOSB COUNTER
	INTEGER*4 BUF_CUR	!GUI_BUF #
	INTEGER*4 IERR		!RTL ERROR STATUS
	INTEGER*4 CONN_WAIT	!WAIT TIME
	INTEGER*4 CONN_INX	!CONNECTION INDEX
	INTEGER*4 QUE_SIZE	!to keep LINK_FRE queue size
C
	IF(GUI_DBG_UNIT.NE.0) THEN
D	  TYPE *,IAM(),'GUILCHEKREAD: '
	ENDIF
C
	DO 1000 CONN_INX=1,GUI_MAX_CONN
	 IF(GUI_CONN_STS(CONN_INX).NE.GUI_CONN_STS_CONNECTED) THEN
	  IF(GUI_CONN_STS(CONN_INX).EQ.GUI_CONN_STS_DISCON) THEN
	    CONN_WAIT=GUI_CONN_WAIT
	    CALL GUITCPPSTARTTIME(GUITCP_TIME_PCON,CONN_WAIT)
	  ENDIF
	  GOTO 1000
	 ENDIF
C
	 DO 100 RIO=0,GUI_MAX_READS-1
	  IF(GUI_READ_OUT(RIO,CONN_INX).EQ.GUI_INPROG) GOTO 100
C
	  CALL LISTSIZE(GUI_LINK_FRE_QUE, QUE_SIZE)
	  IF(QUE_SIZE.LT.GUI_LINK_FRE_BUFS_GOAL) THEN
	    CALL GUITCPPSTARTTIME(GUITCP_TIME_READ,300)
	    RETURN
	  ENDIF
C
	  CALL RTL(BUF_CUR,GUI_LINK_FRE_QUE,IERR)
	  IF(IERR.EQ.2) THEN	    !NO FREE BUFFERS
	    CALL GUITCPPSTARTTIME(GUITCP_TIME_READ,300)
	    RETURN
	  ELSE
	    CALL GUIMGR_CHECK_BUF(BUF_CUR,'GUITCPPCHEKRED')
	    CALL GUITCPPDOREAD(RIO,BUF_CUR,CONN_INX)
	  ENDIF
100	 CONTINUE
C
C
1000	CONTINUE
	RETURN
	END
