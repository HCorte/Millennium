C GUI_017.FOR
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
	SUBROUTINE GUI_017(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:POOLLTO.DEF'
C
	BYTE	  OUTBUF(*)
	INTEGER*4 MES_LEN,RET_CODE
	INTEGER*4 BOARD(20)
C
	INTEGER*4 NUM_COLS,NUM_ROWS
C
        INTEGER*4 GAME,GIND,GTYP
        INTEGER*4 ST,I
	INTEGER*4 LEN,CNT
	LOGICAL*4 DISP_COMBS
        CHARACTER*8 STATUS(0:1)
        DATA STATUS/' enabled','disabled'/
C
C
	CALL GUI_GETPARAMS(OUTBUF,ST)
	IF(ST.NE.0) THEN
	    RET_CODE = 11
	    RETURN
	ENDIF
	GIND = GUI_ARGVAL(1)
	IF(GIND.LT.1.OR.GIND.GT.NUMLTO) THEN
	   RET_CODE = 11
	   RETURN
	ENDIF
	GTYP = TLTO
	GAME = LTPOOL_GAMENR(GTYP,GIND)
	IF(GAME.LT.1.OR.GAME.GT.LTPOOL_MAXTYP) THEN
	   RET_CODE = 11
	   RETURN
	ENDIF
C
	DISP_COMBS = .FALSE.
	LEN = GUI_ARGLEN(2)	
	IF(LEN.GT.0) THEN
	   DO I=1,LTONUM(GIND)
              CALL ASCBIN(B_GUI_ARGCHAR(1,2),I*2-1,2,BOARD(I),ST)
	   ENDDO
	   CALL GETBETS(BOARD,CNT,ST,GAME)
	   DISP_COMBS = .TRUE.
	ENDIF
C
C INITIALIZE OUTPUT 
C
	CALL GUIARG_INIT()
C
	NUM_COLS = 7
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
	CALL GUIARG_CHAR(OUTBUF,SFNAMES(1,LPR),20)
	CALL GUIARG_BYTE(OUTBUF,GTYP)
	CALL GUIARG_BYTE(OUTBUF,GIND)
	CALL GUIARG_INT2(OUTBUF,LTPOOLDRAW(GAME))

	CALL GUIARG_BYTE(OUTBUF,GAME)
	CALL GUIARG_INT4(OUTBUF,LTPOOL_TOT(GAME))
	CALL GUIARG_CHAR(OUTBUF,%REF(STATUS(P(SUPPUD))),8)
C
	IF(.NOT.DISP_COMBS) GOTO 9000	
C
	NUM_COLS = 1
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
	CALL GUIARG_INT4(OUTBUF,CNT)
C
C FINALLY SET OUTPUT MESSAGE LENGTH 
C
9000	CONTINUE
	CALL GUIARG_SET_MESLEN(MES_LEN)
C
	RETURN
C
	END
