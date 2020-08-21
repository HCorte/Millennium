C
C SUBROUTINE OPSDBG
C
C CALL OPSDBG("STRING",INTVAL,HEXVAL)
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
	SUBROUTINE OPSDBG(STRING,INTVAL,HEXVAL)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER*(*) STRING
	INTEGER*4     INTVAL
	INTEGER*4     HEXVAL
	INTEGER*4     ST
C
	CHARACTER*132 BUFFER
        CHARACTER*50  TEXT
C
	TEXT=STRING
C
	WRITE(UNIT=BUFFER,FMT=900) TEXT, INTVAL,HEXVAL
900	FORMAT('_DBG:',A,':I[',I11,']H[',Z8,']')
C
	CALL MLOG(BUFFER,ST)
C
	RETURN
	END
