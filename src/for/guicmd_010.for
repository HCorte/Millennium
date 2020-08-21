C GUICMD_010.FOR
C
C V01 05-FEB-2001 UXN Initial release.
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
	SUBROUTINE GUICMD_010(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
	INCLUDE 'INCLIB:GUIFIL.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:HASF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
C
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 ST
	INTEGER*4 NUM_COLS, NUM_ROWS
	INTEGER*4 STATUS,LEN,I
	CHARACTER*40 STATUS_STR

	INTEGER*4 CNT, TER, IDX
	BYTE      AMSG(512)
	INTEGER*4 BEG
	PARAMETER(BEG=3*29)
C
	RET_CODE = 0
	STATUS   = 0
	STATUS_STR = ' '
C
	CALL GUI_GETPARAMS(OUTBUF,ST)
	IF(ST.NE.0) THEN
	   RET_CODE = 11
	   RETURN
	ENDIF

	TER = GUI_ARGVAL(1)
	IF(TER.LT.1.OR.TER.GT.NUMAGT) THEN
	   STATUS_STR = 'Invalid terminal number'
	   STATUS = 1
	   GOTO 100
	ENDIF
C
C Read agent record.
C	   
        CALL READW(ASFFDB,TER,ASFREC,ST)
        IF(ST.NE.0) THEN
          CALL OPS('Failed to read ASF file',ST,TER)
          RET_CODE = 11
          RETURN
        ENDIF
C
C GET OTHER FIELDS ACCORDING TO HASF.DEF
C
	CNT = 0
	IDX = 2
	DO 10 I=1,26
	   IF(GUI_ARGLEN(I+1).LE.0) GOTO 10
	   CNT = CNT + 1
	   LEN = FLDEND(BEG+I) - FLDBEG(BEG+I) + 1
	   LEN = MIN(LEN,GUI_ARGLEN(I+1))
	   AMSG( IDX + 0 ) = BEG+I
	   AMSG( IDX + 1 ) = LEN
	   IDX = IDX + 2
	   CALL MOVBYT(B_GUI_ARGCHAR(1,I+1),1,AMSG,IDX,LEN)
	   IDX = IDX + LEN
10	CONTINUE
C
	IF(CNT.EQ.0) THEN
	   STATUS_STR = 'Nothing to change!'
	   STATUS = 2
	   GOTO 100
	ENDIF	   

	AMSG(1) = CNT
C
C Build the command
C
	CALL QUEAGTINF(TER,AMSG,ST)
	IF(ST.NE.0) THEN
	   CALL OPS('Agent update failed for terminal',TER,ST)
	   STATUS = 3
	   STATUS_STR = 'Agent update failed!'
	   GOTO 100
	ENDIF
C
C SEND DATA TO GUI
C
100	CONTINUE
	CALL GUIARG_INIT()
C
	NUM_COLS = 2
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
C STATUS BACK
C
	CALL GUIARG_INT4(OUTBUF,STATUS)	
	CALL GUIARG_CHAR(OUTBUF,%REF(STATUS_STR),40)	
C
	CALL GUIARG_SET_MESLEN(MES_LEN)
C
	RETURN
	END
