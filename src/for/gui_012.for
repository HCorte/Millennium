C GUI_012.FOR
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
	SUBROUTINE GUI_012(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
C
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 NUM_COLS,NUM_ROWS
C
        INTEGER*4  GNUM
        INTEGER*4  ST,I
	INTEGER*4  GTOTAL,NTOTAL
	INTEGER*4  NET(24),GROSS(24)
	INTEGER*4  GAM
	INTEGER*4  FIRST_GAM,LAST_GAM
C
C
	CALL GUI_GETPARAMS(OUTBUF,ST)
	IF(ST.NE.0) THEN
	    RET_CODE = 11
	    RETURN
	ENDIF
	GNUM = GUI_ARGVAL(1)
	IF(GNUM.LT.0.OR.GNUM.GT.MAXGAM) THEN
	   RET_CODE = 11
	   RETURN
	ENDIF

	DO I=1,24
	   NET(I) = 0
	   GROSS(I) = 0
	ENDDO

	IF(GNUM.EQ.0) THEN
	   FIRST_GAM = 1
	   LAST_GAM  = MAXGAM
	ELSE
	   FIRST_GAM = GNUM
	   LAST_GAM  = GNUM
	ENDIF
        GTOTAL=0
        NTOTAL=0
        DO I=1,24
C	   NET(I) = NET(I) - NTOTAL
C	   GROSS(I) = GROSS(I) - GTOTAL
	   DO GAM=FIRST_GAM,LAST_GAM
              NET(I)=NET(I)+HOURSAL(GAM,2,I)
              GROSS(I)=GROSS(I)+HOURSAL(GAM,1,I)
      	      NTOTAL=NTOTAL+HOURSAL(GAM,2,I)
      	      GTOTAL=GTOTAL+HOURSAL(GAM,1,I)
	   ENDDO
           IF(NET(I).LT.0) NET(I)=0
           IF(GROSS(I).LT.0) GROSS(I)=0
        ENDDO
C
C INITIALIZE OUTPUT 
C
	CALL GUIARG_INIT()
C
	NUM_COLS = 2
	NUM_ROWS = 24
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)

        DO I=1,24
	   CALL GUIARG_MONY(OUTBUF,NET(I))
	   CALL GUIARG_MONY(OUTBUF,GROSS(I))
	ENDDO
C
C FINALLY SET OUTPUT MESSAGE LENGTH 
C
	CALL GUIARG_SET_MESLEN(MES_LEN)
C
	RETURN
C
	END
