C GUI_019.FOR
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
	SUBROUTINE GUI_019(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'

	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
C
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 NUM_COLS,NUM_ROWS
	INTEGER*4 MAX_ROWS_TO_SEND
	PARAMETER(MAX_ROWS_TO_SEND=100)
C
        INTEGER*4  OPT
	INTEGER*8  AMOUNT
        INTEGER*4  ST,I,J
	INTEGER*4  TOTSAL
	INTEGER*4  AMT(2,NUMAGT)
C
	CALL GUI_GETPARAMS(OUTBUF,ST)
	IF(ST.NE.0) THEN
	    RET_CODE = 11
	    RETURN
	ENDIF
	OPT = GUI_ARGVAL(1)
	IF(OPT.NE.1.AND.OPT.NE.2) THEN
	   RET_CODE = 11
	   RETURN
	ENDIF
	AMOUNT = GUI_ARGVAL(2) * DOLL_BASE / DYN_BETUNIT
C
C GET AGENT DATA
C
	NUM_ROWS = 0
	DO 30 I=1,NUMAGT
	   IF(AGTTAB(AGTNUM,I).EQ.0) GOTO 30
	   TOTSAL=0
	    DO J=1,MAXGAM
	       TOTSAL=TOTSAL+AGTGAM(GSAMT,J,I)-AGTGAM(GCAMT,J,I)
	    ENDDO
	   IF(OPT.EQ.1.AND.TOTSAL.LE.AMOUNT) GOTO 30
	   IF(OPT.EQ.2.AND.TOTSAL.GE.AMOUNT) GOTO 30
	   NUM_ROWS = NUM_ROWS + 1
	   IF(NUM_ROWS.GT.MAX_ROWS_TO_SEND) GOTO 30	    		   
	   AMT(1,NUM_ROWS) = TOTSAL
	   AMT(2,NUM_ROWS) = AGTTAB(AGTNUM,I)
30	CONTINUE
C
40	CONTINUE
C
C BUILD GUI MESSAGE
C
	CALL GUIARG_INIT()
C
	NUM_COLS = 1
	CALL GUIARG_NEXT_SET(OUTBUF, NUM_COLS)	
C
        CALL GUIARG_INT4(OUTBUF,NUM_ROWS)
C
C RESULT SET 2
C
        NUM_COLS = 2
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
	IF(NUM_ROWS.EQ.0) THEN
	   CALL GUIARG_NO_DATA(OUTBUF,NUM_COLS)	    
	   GOTO 9000
	ENDIF
C	       
	DO I=1, MIN(NUM_ROWS,MAX_ROWS_TO_SEND)
	   CALL GUIARG_MONY(OUTBUF,AMT(1,I))
	   CALL GUIARG_INT4(OUTBUF,AMT(2,I))
	ENDDO
C
9000	CONTINUE
	CALL GUIARG_SET_MESLEN(MES_LEN)
C
	RETURN
C
	END
