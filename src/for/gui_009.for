C GUI_009.FOR
C
C V01 10-FEB-2001 UXN Initial release.
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
	SUBROUTINE GUI_009(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUISTR.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:DLTREC.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
	INCLUDE 'INCLIB:GUIFIL.DEF'
C
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 NUM_COLS, NUM_ROWS
	INTEGER*4 GIND, DRAW, GNUM, ST
C
C  GET CDC
C
	CALL GUI_GETPARAMS(OUTBUF,ST)
	IF(ST.NE.0) THEN
	   RET_CODE = 11
	   RETURN
	ENDIF

	GIND = GUI_ARGVAL(1)
	DRAW = GUI_ARGVAL(2)

	IF(GIND.LT.1.OR.GIND.GT.NUMLTO) THEN
	  RET_CODE = 11
	  RETURN
	ENDIF

	GNUM = GTNTAB(TLTO,GIND)
	IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
	  RET_CODE = 11
	  RETURN
	ENDIF

	IF(DRAW.LT.1) DRAW = DAYDRW(GNUM)	
	IF(DRAW.EQ.0) DRAW = DAYHDR(GNUM)
C
C GET DATA FROM COMMON OR DISK
C
5       CONTINUE
	IF(DRAW.EQ.DAYDRW(GNUM)) THEN
	    CALL GAMLOG(TLTO,GIND,DLTREC,LTOSTS)
	    GOTO 100
	ENDIF
C
	CALL READW(GAMFDB(1,GNUM),DRAW,DLTREC,ST)
	IF(ST.NE.0) THEN
	    CALL OPS('Failed to read '//CGFNAMES(GNUM),ST,DRAW)
	    RET_CODE = 11
	    RETURN
	ENDIF
C
100	CONTINUE
C
C
C SEND DATA TO GUI
C
	CALL GUIARG_INIT()
C
	NUM_COLS = 5
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
	CALL GUIARG_BYTE(OUTBUF,DLTMAX)
	CALL GUIARG_BYTE(OUTBUF,DLTNUM)
	CALL GUIARG_BYTE(OUTBUF,DLTBFL)
	CALL GUIARG_INT2(OUTBUF,DRAW)
	CALL GUIARG_BYTE(OUTBUF,DLTLFL)	
C                                          
	CALL GUIARG_SET_MESLEN(MES_LEN)
	RETURN
C
	END
