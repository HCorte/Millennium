C
C GUICMD_017.FOR
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
	SUBROUTINE GUICMD_017(OUTBUF,MES_LEN,RET_CODE)
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

	INTEGER*4 TER, HSFBUF(6), FUN, TRATYP, ADJTYP, GNUM, WEEK, YEAR
	INTEGER*4 AGT, VNUM, AMT(2), INFOTAB, TNUM
	INTEGER*8 I8MONY
	INTEGER*4 I4MONY(2)
	EQUIVALENCE(I8MONY,I4MONY)

	INTEGER*4 I4TEMP
	BYTE	  I1TEMP(4)
	EQUIVALENCE(I1TEMP,I4TEMP)
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
	   STATUS_STR = 'Invalid agent number'
	   STATUS     = 1
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
C Function - 1- add an adjustment, 2- remove adjustment
C
	FUN = GUI_ARGVAL(2)
	IF(FUN.LT.1.OR.FUN.GT.2) THEN
	   STATUS_STR = 'Invalid function code'	
	   STATUS = 1	
	   GOTO 100
	ENDIF
C
C Transaction type 
C	
	TRATYP = GUI_ARGVAL(3)
	IF(TRATYP.LT.1.OR.TRATYP.GT.4) THEN
	   STATUS_STR = 'guicmd_017: Invalid transaction type'
	   STATUS = 1
	   GOTO 100
	ENDIF
C
C Adjustment type
C
	ADJTYP = GUI_ARGVAL(4)
	IF(ADJTYP.LT.1.OR.ADJTYP.GT.4) THEN
	   STATUS_STR = 'Invalid adjustment type'
	   STATUS = 1
	   GOTO 100
	ENDIF
C
C Game number
C
	GNUM = GUI_ARGVAL(5)
	IF(GNUM.LT.0.OR.GNUM.GT.MAXGAM) THEN
	   STATUS_STR = 'Invalid game number'
	   STATUS = 1
	   GOTO 100
	ENDIF
C
C Voucher number
C
	VNUM = GUI_ARGVAL(6)
	IF(VNUM.LT.1.OR.VNUM.GT.9999) THEN
	   STATUS_STR = 'Invalid voucher number'
	   STATUS = 1
	   GOTO 100
	ENDIF
C
C Week number
C
	WEEK = GUI_ARGVAL(7)
	IF(WEEK.LT.1.OR.WEEK.GT.53) THEN
	   STATUS_STR = 'Invalid week number'
	   STATUS = 1
	   GOTO 100
	ENDIF
C
C Year number
C
	YEAR = GUI_ARGVAL(8)
C
C Amount
C
	AMT(1) = 0
	AMT(2) = 0
	I8MONY = GUI_ARGVAL(9)
	IF(I8MONY.EQ.0) THEN
	   STATUS_STR = 'Nothing to change'
	   STATUS = 1
	   GOTO 100
	ENDIF

C	I8MONY = I8MONY/100

	CALL ADDI8I4(AMT,I4MONY(1),1)
	IF(TRATYP.EQ.1.OR.TRATYP.EQ.2) THEN
	   AMT(1) = -AMT(1)
	   AMT(2) = -AMT(2)
	ENDIF
C
C CHECK FOR DUPLICATE ENTRY IN LEDGER TABLE
C
	TNUM = (TRATYP-1)*10000 + VNUM
        DO 50,I=1,15
           IF(ASFLGR(LGRCDC,I).LE.ASFINV(ASFEND,1)) GOTO 60
           IF(TNUM.EQ.ASFLGR(LGRCOD,I)) THEN
	      IF(FUN.EQ.2) GOTO 80              ! remove adjustment
	      STATUS_STR = 'Duplicate entry'
	      STATUS = 1
	      GOTO 100
	   ENDIF
50     CONTINUE
C
C FIND SLOT IN LEDGER TABLE
C
60     CONTINUE
       IF(ASFLGR(LGRCDC,15).GT.ASFINV(ASFEND,1)) THEN
	  STATUS_STR = 'Ledger table full'
	  STATUS = 1
	ENDIF
C
80	CONTINUE
C
C QUEUE TRANSACTION TO SYSTEM FOR PROCESSING
C
	I1TEMP(1) = GNUM
	I1TEMP(2) = ADJTYP-1
	I1TEMP(3) = MOD(YEAR,100)
	I1TEMP(4) = WEEK
	INFOTAB   = I4TEMP

        HSFBUF(1) = TER
        HSFBUF(2) = TNUM
        HSFBUF(3) = AMT(1)
        HSFBUF(4) = AMT(2)
        HSFBUF(5) = FUN-1
        HSFBUF(6) = INFOTAB
C
        CALL QUEHSF(HSFBUF,ST)
        IF(ST.NE.0) THEN
	  STATUS_STR = 'Adjustment failed'
	  STATUS = 1
	  GOTO 100
	ENDIF
	GOTO 100
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
