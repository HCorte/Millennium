C GUI_047.FOR
C
C V01 06-FEB-2001 UXN Initial release.
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
	SUBROUTINE GUI_047(OUTBUF,MES_LEN,RET_CODE)
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
	INTEGER*4 STATUS,I
	CHARACTER*40 STATUS_STR

	INTEGER*4 TER, TRATYP, ADJTYP, GNUM, WEEK, YEAR
	INTEGER*4 AGT, VNUM, AMT(2)
	INTEGER*4 I4TEMP
	BYTE	  I1TEMP(4)
	EQUIVALENCE(I1TEMP,I4TEMP)
	LOGICAL*4 FOUND
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

	AGT = GUI_ARGVAL(1)
	CALL FIND_AGENT(AGT,TER,ST)
	IF(ST.NE.0.OR.TER.LT.1.OR.TER.GT.NUMAGT) THEN
	   RET_CODE = 11
	   RETURN
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
C INITIALIZE GUI MESSAGE
C
	CALL GUIARG_INIT()
C
	FOUND = .FALSE.
	DO 5 I=1,15
	   IF(ASFLGR(LGRCDC,I).LE.ASFINV(ASFEND,1)) GOTO 5
	   FOUND = .TRUE.
5	CONTINUE

	NUM_COLS = 9
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
C IF THERE ARE NO ADJUSTMENTS, THEN SEND ONLY HEADER.
C
	IF(.NOT.FOUND) THEN
	    CALL GUIARG_NO_DATA(OUTBUF,NUM_COLS)
	    GOTO 100
	ENDIF
	    
	DO 10 I=1,15
	   IF(ASFLGR(LGRCDC,I).LE.ASFINV(ASFEND,1)) GOTO 10
	   TRATYP = ASFLGR(LGRCOD,I)/10000 + 1
	   VNUM   = MOD(ASFLGR(LGRCOD,I),10000)	   
	   AMT(1) = ASFLGR(LGRAMTU,I)
	   AMT(2) = ASFLGR(LGRAMTP,I)	
	   I4TEMP = ASFLGR(LGRINF,I)
	   WEEK   = I1TEMP(4)
           YEAR   = I1TEMP(3)+2000
           ADJTYP = I1TEMP(2)+1
           GNUM   = I1TEMP(1)
C
	   CALL GUIARG_INT4(OUTBUF,AGT)   
	   CALL GUIARG_BYTE(OUTBUF,TRATYP)
	   CALL GUIARG_BYTE(OUTBUF,ADJTYP)
	   CALL GUIARG_BYTE(OUTBUF,GNUM)

	   CALL GUIARG_INT2(OUTBUF,VNUM)
	   CALL GUIARG_BYTE(OUTBUF,WEEK)
	   CALL GUIARG_INT2(OUTBUF,YEAR)

	   IF(TRATYP.EQ.1.OR.TRATYP.EQ.2) THEN
	      AMT(1) = -AMT(1)
	      AMT(2) = -AMT(2)
	   ENDIF

	   CALL GUIARG_MONYI8(OUTBUF,AMT)
	   CALL GUIARG_DATE(OUTBUF,ASFLGR(LGRCDC,I))
10	CONTINUE
C
100	CONTINUE
	CALL GUIARG_SET_MESLEN(MES_LEN)
C
	RETURN
	END
