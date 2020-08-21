C GUI_022.FOR
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
C Copyright 1999 GTECH Corporation. All rights reserved.
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
	SUBROUTINE GUI_022(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
	INCLUDE 'INCLIB:GUIFIL.DEF'
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 NUM_COLS
	INTEGER*4 GTYP, GIND
C
        INTEGER*4  DAT
C
	INTEGER*4  SALEN
 	PARAMETER (SALEN=NUMTOT*NUMFIN)
C
        INTEGER*4  GNUM
        INTEGER*4  I
        INTEGER*4  ST
	INTEGER*4  TEMP
	INTEGER*4  SALES(NUMTOT,NUMFIN)
C
C  GET CDC
C
        CALL GUI_GETPARAMS(OUTBUF,ST)
        IF(ST.NE.0) THEN
           RET_CODE = 11
           RETURN
        ENDIF

        DAT = GUI_ARGVAL(1)
C
	IF(DAT.LE.0) DAT=DAYCDC
C
	IF(DAT.EQ.DAYCDC) THEN
	  CALL FASTMOV(DAYSTS,DAFSTS,DAFLEN)
	  GOTO 10
	ENDIF
C
	CALL READW(DAFFDB,DAT,DAFREC,ST)
	IF(ST.NE.0) THEN
	  CALL OPS('Failed to read DAF file',ST,DAT)
	  RET_CODE = 11
	  RETURN
	ENDIF
C
	IF(DAFSTS.EQ.DUNUSD) THEN
	  RET_CODE = 11
	  RETURN
	ENDIF
C
	IF(DAFSTS.EQ.DNOSAL) THEN
	  RET_CODE = 11
	  RETURN
	ENDIF
C
10	CONTINUE
	CALL FASTSET(0,SALES,SALEN)
C
	DO GTYP=1,MAXTYP
	DO 20 GIND=1,MAXIND
	  GNUM=GTNTAB(GTYP,GIND)
	  IF(GNUM.EQ.0) GOTO 20
	  DO I=1,TREF
	       SALES(TRACNT,I) = SALES(TRACNT,I)+
     *	                              DAFTYP(TRACNT,I,GNUM)
	       SALES(DOLAMT,I) = SALES(DOLAMT,I)+
     *	                              DAFTYP(DOLAMT,I,GNUM)
	       IF(I.EQ.TVAL) THEN
	         SALES(TRACNT,I) = SALES(TRACNT,I)+
     *	                                DAFDIS(TRACNT,GNUM)
	         SALES(DOLAMT,I) = SALES(DOLAMT,I)+
     *                                  DAFDIS(DOLAMT,GNUM)
	       ENDIF
	  ENDDO
20	CONTINUE
	ENDDO
C
C Build GUI message
C
	CALL GUIARG_INIT()
C
	NUM_COLS = 3
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
	DO I=1,NUMFIN
	  CALL GUIARG_BYTE(OUTBUF,I)
	  TEMP =SALES(TRACNT,I)
	  CALL GUIARG_INT4(OUTBUF,TEMP)
	  TEMP =SALES(DOLAMT,I)
	  CALL GUIARG_MONY(OUTBUF,TEMP)
	ENDDO
C
	CALL GUIARG_SET_MESLEN(MES_LEN)
C
	RETURN
C
	END
