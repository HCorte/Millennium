C GUIDOREAD.FOR
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C		This routine starts a read( using QIO) with the passed-in
C		GUI buffer. Once the read is complete the program traps
C		to the GUITCPPRDIOCOMP routine.
C
C INPUT:
C	    RIO		- read IOSB index
C	    BUF_CUR	- current buffer
C	    CONN_CUR	- current connection
C OUTPUT:
C	    none
C
C RESULTS:
C	    QIO read request 
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUITCPPDOREAD(RIO, BUF_CUR, CONN_CUR)
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
	INTEGER*4 RIO	        !READ IOSB COUNTER
	INTEGER*4 BUF_CUR	!GUI_BUF #
	INTEGER*4 CONN_CUR	!Connection #
	INTEGER*4 IERR		!RTL/ABL ERROR STATUS
	INTEGER*4 STATUS	!STATUS RETURNED FROM QIO
	INTEGER*4 IFUNC	        !FUNCTION CODE
C
	EXTERNAL  GUITCPPRDIOCOMP
C
	INTEGER*4   BLANK
	DATA	    BLANK/'    '/
C
	IF(GUI_DBG_UNIT.NE.0) THEN
D	  TYPE *,IAM(),'GUILDOREAD: RIO,BUF,CONN ',RIO,BUF_CUR, CONN_CUR
	ENDIF
C
	IF(BUF_CUR.LT.1 .OR. BUF_CUR.GT.GUI_LINK_BUFS) THEN
	  CALL FASTSET(BLANK,GUI_MES_BUF,33)
	  WRITE(GUI_MES_CBUF,9000) IAM(),BUF_CUR
9000	  FORMAT(A,'GUILDOREAD: Invalid GUI_LINK Buffer  # ',I)
	  CALL WRITEBRK(GUI_MES_CBUF)
	  RETURN
	ENDIF
C
	IF(RIO.LT.0 .OR. RIO.GE.GUI_MAX_READS) THEN
	  CALL FASTSET(BLANK,GUI_MES_BUF,33)
	  WRITE(GUI_MES_CBUF,9100) IAM(),RIO
9100	  FORMAT(A,'GUILDOREAD: Invalid RIO Buffer  # ',I8)
	  CALL WRITEBRK(GUI_MES_CBUF)
	  CALL ABL(BUF_CUR,GUI_LINK_FRE_QUE,IERR)
	  RETURN
	ENDIF
C
	IF(GUI_READ_OUT(RIO,CONN_CUR).EQ.GUI_INPROG) THEN ! CANNOT HAPPEN...
	  CALL ABL(BUF_CUR,GUI_LINK_FRE_QUE,IERR)
	  RETURN
	ENDIF
C
C	HANG A READ
C
C	VERIFY LENGTH TO SEND IS VALID
C
	GUI_READ_OUT(RIO,CONN_CUR) = GUI_INPROG
	GUI_READ_BUF(RIO,CONN_CUR) = BUF_CUR
	GUI_LINK_BUF(GUI_BUF_IO_INX_OFF, BUF_CUR) =
     *	  (CONN_CUR-1)*GUI_MAX_READS+RIO
C
	IFUNC=IO$_READVBLK
	STATUS=SYS$QIO(,
     *		%VAL(GUI_CONN_CHAN(CONN_CUR)),
     *          %VAL(IFUNC),
     *          %REF(GUI_READ_IOSB(RIO,CONN_CUR)),
     *		GUITCPPRDIOCOMP,
     *		GUI_LINK_BUF(GUI_BUF_IO_INX_OFF, BUF_CUR),
     *          %REF(GUI_LINK_BUF(GUI_BUF_DAT_OFF,BUF_CUR)),
     *          %VAL(GUI_MAX_MSG_LEN),
     *          ,,,)
C
	IF(.NOT.STATUS) THEN
	  CALL FASTSET(BLANK,GUI_MES_BUF,33)
	  WRITE(GUI_MES_CBUF,9200) IAM(),STATUS
9200	  FORMAT(A,'GUILDOREAD: Error starting Read Status ',I)
	  CALL WRITEBRK(GUI_MES_CBUF)
	  GUI_READ_OUT(RIO,CONN_CUR) = GUI_READY ! ??? SHOULD WE SET STATUS

	  CALL ABL(BUF_CUR,GUI_LINK_FRE_QUE,IERR)
	  RETURN
	ENDIF
C
	RETURN
C
	END
