C GUI_049.FOR
C
C AGENT PASSIVE INVOICE DETAILS
C
C V01 21-OCT-2010 FRP Initial release.
c
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
C Copyright 2001 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C This subroutine returns GUI FUNCTION.
C
C Input parameters:
C	NONE               
C
C Output parameters:
C
C	BYTE		OUTBUF(*)    OUTPUT MESSAGE
C	INTEGER*4	MES_LEN	     MESSAGE LENGTH
C	INTEGER*4	RET_CODE:
C		0		-  no error, message accepted;
C		value >= 11	-  error number to be sent to Client.
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUI_049(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
        INCLUDE 'INCLIB:GUIFIL.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 ST,NUM_COLS,I
	INTEGER*4 AGT,TER,LASTTER
C
	RET_CODE = 0
C
	CALL GUI_GETPARAMS(OUTBUF,ST)
	IF(ST.NE.0) THEN
	   RET_CODE = 11
	   RETURN
	ENDIF
C
        TER = GUI_ARGVAL(1)
	IF(TER.EQ.0) THEN
	   AGT = GUI_ARGVAL(2)
	   CALL FIND_AGENT(AGT, TER, ST)
	   IF(ST.NE.0) THEN
	      RET_CODE = 11
	      RETURN
	   ENDIF
        ENDIF
C
	IF(TER.LT.1.OR.TER.GT.NUMAGT) THEN
	   RET_CODE = 11
	   RETURN
	ENDIF
C
        CALL READW(ASFFDB,TER,ASFREC,ST)
        IF(ST.NE.0) THEN
	  CALL OPS('Failed to read ASF file',ST,TER)
	  RET_CODE = 11
          RETURN
        ENDIF
C
	CALL GUIARG_INIT()
C
	NUM_COLS = 23
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
        CALL IVPSNP(TER,29,LASTTER)
C
        CALL GUIARG_INT4(OUTBUF,TER)
        DO I=1,22
           CALL GUIARG_CHAR(OUTBUF,NEW(1,I),80)
        ENDDO
C
	CALL GUIARG_SET_MESLEN(MES_LEN)
C
        LASTTER=TER
C
	RETURN
	END
