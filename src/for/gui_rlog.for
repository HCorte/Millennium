C GUI_RLOG.FOR
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1993 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUI_RLOG(SERIAL, LBUF, STS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'

	INCLUDE 'INCLIB:DISKIO.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GUIFIL.DEF'
C
	INTEGER*4	BLOCK,
     *			CNT,
     *			INDEX,
     *			IOBUF(DBLOCK),
     *			LBUF(LMUREC),
     *			LENGTH,
     *			OFFSET,
     *			RTYPE,
     *			SERIAL,
     *			STATUS,
     *			STS,
     *			RETRY_COUNT
C
	RETRY_COUNT=1
	CALL GETBI(SERIAL, BLOCK, INDEX, OFFSET)
40	CONTINUE
	STS = 0
	CALL READW(TMFFDB, BLOCK, IOBUF, STATUS)
	IF (STATUS .NE. 0) THEN
	  STS    = -1
	  GOTO 900
	ENDIF
C
C CHECK RECORD TYPE
C
	CALL ILBYTE(RTYPE, IOBUF(OFFSET), LREC1 - 1)
	IF (RTYPE .GT. LONE) THEN
	  STS = 1
	  RETURN
	ENDIF
C
	CNT = 1
	IF (RTYPE .EQ. LONE) THEN
	  CALL ILBYTE(RTYPE, IOBUF(OFFSET), LREC2 - 1)
	  IF (RTYPE .EQ. LTWO) CNT = 3
	  IF (RTYPE .EQ. LEND) CNT = 2
	ENDIF
	LENGTH = CNT * LREC
C
	CALL FASTMOV(IOBUF(OFFSET), LBUF, LENGTH)
	IF (LENGTH .LT. LREC * 3)
     *	  CALL FASTSET(0, LBUF(LENGTH + 1), LREC * 3 - LENGTH)

900	CONTINUE
	  IF(STS.NE.0) THEN
C	    LOOP AROUND 3 TIME LOOKING FOR IT IN THE TM
	  IF (RETRY_COUNT .LE. 3) THEN
	    RETRY_COUNT = RETRY_COUNT + 1
	    CALL XWAIT(100, 1, STATUS)
	    GOTO 40 
	  ELSE
	    STS=-1
	  ENDIF	  
	ENDIF
C
9999	CONTINUE
	RETURN
	END
