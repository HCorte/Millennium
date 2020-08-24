C
C SUBROUTINE FLUSH
C
C V02 01-AUG-2000 UXN TYPE* REPLACED WITH OPS(),OPSTXT()
C V01 XX-XXX-XXXX XXX INITIAL RELEASE.
C
C SUBROUTINE TO WRITE ALL MEMORY RESIDENT LOG BUFFERS TO
C AVAILABLE UNITS IN THE EVENT OF A FATAL LOGGER ERROR.
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE FLUSH(MFDB,BFDB)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:LOGCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INTEGER*4 MFDB(7),BFDB(7), ST, BLOCK, I
C
C SCAN ALL MEMORY RESIDENT LOG BUFFERS
C
	CALL OPSTXT('FLUSHING BUFFERS TO BACKUP LOGGING UNITS')
	CALL OPS('HIGHEST SERIAL NUMBER RELEASED',HSER,0)
	DO 10 I=1,NUMLOG
	IF(LOGBUF(BLONUM,I).NE.0) THEN
	  BLOCK=LOGBUF(BLONUM,I)
	  LOGBUF(LRCNUM,I)=BLOCK
	  CALL WRITEW(MFDB,BLOCK,LOGBUF(DSKREC,I),ST)
	  IF(P(DISKSW).NE.0)
     *	    CALL WRITEW(BFDB,BLOCK,LOGBUF(DSKREC,I),ST)
	ENDIF
10	CONTINUE
	RETURN
	END
