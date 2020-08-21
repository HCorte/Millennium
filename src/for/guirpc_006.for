C
C SUBROUTINE GUIRPC_006
C
C V04 08-NOV-2000 UXN  Cleaned up.
C V03 24-JAN-2000 ANDY Start of Changes for Rolldown
C V02 07-APR-2000 XXX  MOVED RPC PARAMETERS TO SERVCOM
C V01 06-JAN-2000 AMY  Initial revision.
C  
C GUIRPC_006.FOR
C
C This subroutine returns the current CDC.
C
C Input parameters:
C	NONE               
C
C Output parameters:
C
C	BYTE		OUTBUF(*)	OUTPUT MESSAGE
C	INTEGER*4	MES_LEN	MESSAGE LENGTH
C	INTEGER*4	RET_CODE:
C		0		-  no error, message accepted;
C		value >= 11	-  error number to be sent to Client.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUIRPC_006(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4	NUM_COLS,NUM_ROWS
C
	RET_CODE = 0
C
C Build RPC message
C
	CALL RPCARG_INIT() ! use RPC format
C
	NUM_COLS = 2
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
	CALL GUIARG_INT4(OUTBUF,DAYCDC) ! CDC
	CALL GUIARG_DATE(OUTBUF,DAYCDC) ! DD.MM.YYYY
C
	CALL GUIARG_SET_MESLEN(MES_LEN)
C
	RETURN
	END
