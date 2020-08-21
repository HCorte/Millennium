C
C SUBROUTINE GUIRPC_018
C
C V04 08-NOV-2000 UXN  Cleaned up.
C V03 24-JUN-2000 ANDY Start of Changes for Rolldown
C V02 04-APR-2000 AMY  Start of Changes for Spring 2000
C V01 06-JAN-2000 AMY  Initial revision.
C  
C GUIRPC_018.FOR
C
C This subroutine broadcasts to a specified terminal.
C
C Input parameters:
C	TERMINAL NUMBER
C	BROADCAST MESSAGE NUMBER
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
	SUBROUTINE GUIRPC_018(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:PRMDLL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:GUIMPRM.DEF'
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4	BUF(CDLEN),ST,STN,TER,VALUE
C
	BYTE		I1TEMP(4)
	INTEGER*4	I4TEMP
	EQUIVALENCE	(I1TEMP,I4TEMP)
C
C  GET TERMINAL NUMBER 
C
	CALL MOVBYT(OUTBUF,10,I1TEMP,1,4)
	TER = I4TEMP
C
C  IF INVALID TERMINAL NUMBER, RETURN WITH ERROR
C
	IF (TER.LT.1.OR.TER.GT.X2X_TERMS) THEN
	  RET_CODE = 11
	  RETURN
	ENDIF
	IF (AGTTAB(AGTNUM,TER).EQ.0) THEN
	  RET_CODE = 11
	  RETURN
	ENDIF

	STN=X2XT_STATION_NO(TER)
	IF(STN.LT.1.OR.STN.GT.X2X_STATIONS) STN=0   !FOR NOW
C
C  GET BRO NUMBER
C
	CALL MOVBYT(OUTBUF,14,I1TEMP,1,4)
	VALUE=I4TEMP
C
C  Build RPC message
C
	CALL GUIARG_EMPTY_RESPONSE(OUTBUF,MES_LEN)
C
C  BRONUM CHANGE
C
40	CONTINUE
	IF(VALUE.LT.1.OR.VALUE.GT.256) GOTO 20
	IF(VALUE.EQ.MNEWS) GOTO 20
C
	BUF(1)=5
	BUF(2)=VALUE
	BUF(3)=TCSPE
	BUF(4)=STN
	BUF(5)=TER
C
C QUEUE COMMAND BUFFER TO SYSTEM INPUT QUEUE
C
	CALL QUECMD(BUF,ST)
C
	RET_CODE=0
	RETURN
C
20	RET_CODE=11
	RETURN
	END
