C GUIDODASGN.FOR
C
C V02 31-OCT-2000 UXN GUI prefix added
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
C		This routine will deassign any channels into UCX. To do this
C		the SYS$DASSGN service is called.
C
C INPUT:
C	CHAN	- channel nr
C	CONN	- connection nr
C OUTPUT:
C	none
C RESULTS:
C	channel is deassigned
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUITCPPDODASGN(CHAN, CONN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4 CHAN, CONN
	INTEGER*4 STATUS	!STATUS RETURNED FROM QIO
C
	INTEGER*4   BLANK
	DATA	    BLANK/'    '/
C
	IF(GUI_DBG_UNIT.NE.0) THEN
	  TYPE *,IAM(),'GUILDODASGN:  chan,conn= ', CHAN, CONN
	ENDIF
C
C DEASSIGN THE 1ST CHANNEL ASSIGNED TO THE DEVICE.
C
	IF(CHAN.NE.-1) THEN
	  STATUS=SYS$DASSGN(%VAL(CHAN))
	  IF(.NOT.STATUS) THEN
	    CALL FASTSET(BLANK,GUI_MES_BUF,33)
	    WRITE(GUI_MES_CBUF,9000) IAM(),STATUS, CHAN, CONN
9000	    FORMAT(A,'GUILDODASGN: Error,chann,conn= ',3(I))
	    CALL WRITEBRK(GUI_MES_CBUF)
	    GOTO 10000
	  ENDIF
	ENDIF
C
	CHAN=-1
C
10000	CONTINUE
C
	RETURN
C
	END
