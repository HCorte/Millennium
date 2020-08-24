C GUI_030.FOR
C
C V01 05-FEB-2001 HXK Initial release for AlphaGOLS
C
C TERMINAL TYPE DATA REQUEST
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
	SUBROUTINE GUI_030(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
	INCLUDE 'INCLIB:GUIFIL.DEF'
C
C
	BYTE	    OUTBUF(*)
	INTEGER*4   MES_LEN,RET_CODE
C
	INTEGER*4   NUM_COLS,NUM_ROWS
        INTEGER*4   GNUM,GIND,GTYP,ST
        INTEGER*4   AGTNO,TERNO
C
C
	RET_CODE = 0	
	CALL GUI_GETPARAMS(OUTBUF,ST)
	IF(ST.NE.0) THEN
	    RET_CODE = 11
	    RETURN
	ENDIF
        TERNO = GUI_ARGVAL(1)
	AGTNO = GUI_ARGVAL(2)
        GNUM  = GUI_ARGVAL(3)
	
	IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) GNUM = 1
	GTYP = GNTTAB(GAMTYP,GNUM)
	GIND = GNTTAB(GAMIDX,GNUM)
	IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP) THEN
	    RET_CODE = 11
	    RETURN
	ENDIF
	IF(GIND.LT.1.OR.GIND.GT.MAXIND) THEN
	    RET_CODE = 11
	    RETURN
	ENDIF
C
	IF(TERNO.EQ.0) THEN
	   CALL FIND_AGENT(AGTNO, TERNO, ST)
           IF(ST.NE.0) THEN
              RET_CODE = 11
              RETURN
           ENDIF
	ENDIF
C
	IF(TERNO.LT.1 .OR. TERNO.GT.NUMAGT) THEN 
           RET_CODE = 11
	   RETURN
	ENDIF
C
C ENCODE TERMINAL TYPE DATA
C
C INITIALIZE OUTPUT 
C
	CALL GUIARG_INIT()
C
	NUM_COLS = 2
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
	CALL GUIARG_INT4(OUTBUF,AGTTAB(AGTTYP,TERNO)) 
	CALL GUIARG_INT4(OUTBUF,AGTGAM(GFLAGS,GNUM,TERNO)) 
C
C FINALLY SET OUTPUT MESSAGE LENGTH 
C
	CALL GUIARG_SET_MESLEN(MES_LEN)
C
	RETURN
C
	END
