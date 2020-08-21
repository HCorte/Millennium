C GUI_026.FOR
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
	SUBROUTINE GUI_026(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:CRSCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
C
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
	INTEGER*4	OFFSET
C
	INTEGER*4 NUM_COLS,NUM_ROWS
        CHARACTER*25 CONSTS(0:4)
        DATA CONSTS/'CONNECTED                ',
     *              'CONNECTION IN PROGRESS   ',
     *              'DISCONNECTION IN PROGRESS',
     *              'DISCONNECTED             ',
     *              'UNKNOWN                  '/
C
	CHARACTER*15 IPADDR
C
	RET_CODE = 0	
C
        OFFSET=TCP_CONNSTS
        IF(TCP_CONNSTS.LT.0.OR.TCP_CONNSTS.GT.4) OFFSET=4
	WRITE(IPADDR,900) ZEXT(TCP_B_REMADR(1)),
     *                    ZEXT(TCP_B_REMADR(2)),
     *                    ZEXT(TCP_B_REMADR(3)),
     *                    ZEXT(TCP_B_REMADR(4))

C
C INITIALIZE OUTPUT 
C
	CALL GUIARG_INIT()
C
	NUM_COLS = 4
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)

	CALL GUIARG_BYTE(OUTBUF,P(PRMSTR))
	CALL GUIARG_CHAR(OUTBUF,%REF(CONSTS(OFFSET)),25)
	CALL GUIARG_CHAR(OUTBUF,%REF(IPADDR),15)
	CALL GUIARG_INT2(OUTBUF,TCP_REMPRT)
C
C RESULT SET 2
C
	NUM_COLS = 4
	NUM_ROWS = 4
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
C TCP READS
C
	CALL GUIARG_INT4(OUTBUF,TCP_READS)
	CALL GUIARG_INT4(OUTBUF,TCP_READERRS)
	CALL GUIARG_INT4(OUTBUF,0)
	CALL GUIARG_INT4(OUTBUF,TCP_READLERR)
C
C TCP WRITES
C
	CALL GUIARG_INT4(OUTBUF,TCP_WRITES)
	CALL GUIARG_INT4(OUTBUF,TCP_WRITEERRS)
	CALL GUIARG_INT4(OUTBUF,TCP_WRITENOCS)
	CALL GUIARG_INT4(OUTBUF,TCP_WRITELERR)
C
C TCP CONNECTS
C
	CALL GUIARG_INT4(OUTBUF,TCP_CONNECTS)
	CALL GUIARG_INT4(OUTBUF,TCP_CONNERRS)
	CALL GUIARG_INT4(OUTBUF,0)
	CALL GUIARG_INT4(OUTBUF,TCP_CONNLERR)
C
C TCP DISCONNECTS
C
	CALL GUIARG_INT4(OUTBUF,TCP_DISCONNS)
	CALL GUIARG_INT4(OUTBUF,TCP_DISCERRS)
	CALL GUIARG_INT4(OUTBUF,0)
	CALL GUIARG_INT4(OUTBUF,TCP_DISCLERR)
C
C FINALLY SET OUTPUT MESSAGE LENGTH 
C
	CALL GUIARG_SET_MESLEN(MES_LEN)
C
	RETURN
C
900	FORMAT(I3,'.',I3,'.',I3,'.',I3)
	END
