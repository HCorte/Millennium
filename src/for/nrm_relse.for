C
C SUBROUTINE RELSE
C
C V03 17-JUN-2000 UXN TNAME_x variables removed.
C V02 11-Nov-1999 RXK Changed for ALPHA (UXN)
C V01 21-Jan-1993 DAB Initial Release Based on Netherlands Bible, 12/92, and
C                     Comm 1/93 update DEC Baseline
C
C
C RELSE.FOR
C
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
	SUBROUTINE RELSE (NAME, STATUS)
	IMPLICIT    NONE
	REAL*8	    NAME
	INTEGER*4   STATUS
C
	REAL*8	    TNAME
	BYTE	    TNAME_BYTES(8)
	CHARACTER*1  TNAME_CHARS(8)
	EQUIVALENCE (TNAME,TNAME_BYTES,TNAME_CHARS)
C
	LOGICAL		FIRST_TIME/.TRUE./
	INTEGER*4	I, PROJ_PREFIX, PROJ_PREFIX_LEN
	CHARACTER*15	PRCNAM
	BYTE		PRCNAM_BYTES(15)
	EQUIVALENCE	(PROJ_PREFIX, PRCNAM,PRCNAM_BYTES)
C
	INTEGER*4	SYS$WAKE
	EXTERNAL	SYS$WAKE    
	INTEGER*4	PRCNAM_LEN
C
	IF(FIRST_TIME) THEN
	    FIRST_TIME = .FALSE.
	    CALL GETPRFX(PROJ_PREFIX, PROJ_PREFIX_LEN)
	ENDIF
C
C	FIND THE LENGTH OF THE TASK NAME
C
	TNAME = NAME
	PRCNAM_LEN = PROJ_PREFIX_LEN
	DO 10 I=1,8
	   IF (TNAME_BYTES(I) .EQ. 0 .OR. TNAME_CHARS(I) .EQ. ' ') GOTO 20
	   PRCNAM_LEN = PRCNAM_LEN + 1
	   PRCNAM_BYTES(PRCNAM_LEN) = TNAME_BYTES(I)
10	CONTINUE
20	CONTINUE
C
D	TYPE *,IAM(),'NRM_RELSE: ACTIVATING ',PRCNAM
	STATUS = SYS$WAKE (,PRCNAM(1:PRCNAM_LEN))
C**	IF (.NOT. STATUS) THEN
C**		TYPE *,'BAD STATUS FROM ''SYS$WAKE'' FOR ', PRCNAM
C***		CALL LIB$SIGNAL(%VAL(STATUS))
C**	ENDIF
C
	RETURN
	END
