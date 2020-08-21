C GUI_AGTINF.FOR
C
C V01 16-FEB-2001 UXN INITIAL RELEASE.
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
	SUBROUTINE GUI_AGTINF(OUTBUF,MES_LEN,RET_CODE,OPT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:HASF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
	INCLUDE 'INCLIB:GUIFIL.DEF'
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE,OPT
C
	INTEGER*4 I, J, LEN, ST
	INTEGER*4 NUM_COLS

        INTEGER*4 MENU_CNT
        PARAMETER(MENU_CNT = 7)
        INTEGER*4 COLS(MENU_CNT)
	SAVE      COLS

	INTEGER*4 AGT,TER
	INTEGER*4 POS, OFF
	LOGICAL*4 FIRST/.TRUE./
	CHARACTER*1 CZERO/Z0/
C
	OFF = OPT*29 
	IF(FIRST) THEN
           FIRST = .FALSE.
	   DO I=1,MENU_CNT
	      COLS(I) = 0
	      DO J=1,29
		 IF(FLDBEG((I-1)*29+J).NE.0) COLS(I) = COLS(I) + 1
	      ENDDO
	   ENDDO
	ENDIF
C
	CALL GUI_GETPARAMS(OUTBUF,ST)
	IF(ST.NE.0) THEN
	   RET_CODE = 11
	   RETURN
	ENDIF

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
C BUILD GUI MESSAGE
C
	CALL GUIARG_INIT()
C
	NUM_COLS = COLS(OPT+1)
	IF(OPT.EQ.0) NUM_COLS = NUM_COLS + 1
C
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
C Terminal number, if AGENT ID was requested.
C
	IF(OPT.EQ.0) CALL GUIARG_INT4(OUTBUF,TER)
C
	DO 100 I=1,COLS(OPT+1)
	  POS = FLDBEG(OFF+I)
	  LEN = FLDEND(OFF+I)-FLDBEG(OFF+I)+1
	  IF(ASFBYT(POS).EQ.CZERO) ASFBYT(POS) = ' '
	  CALL GUIARG_CHAR(OUTBUF,%REF(ASFBYT(POS)),LEN)
100	CONTINUE
C
	CALL GUIARG_SET_MESLEN(MES_LEN)
C
	RETURN
C
	END
