C
C SUBROUTINE GETNAM
C
C V04 26-JUL-2000 UXN PROC_NAME removed.
C V03 26-MAR-1991 TKO ADD GETFULNAM TO GET FULL NAME (INCLUDING PREFIX)
C V02 26-JAN-1991 TKO FIX UP SOME BUGS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C GETNAM:     Returns  8 byte name after the prefix
C GETFULNAM:  Returns 16 byte full name
C
C CALL GETNAM (I4ARRAY)	      !(INTEGER*4 I4ARRAY(2))
C CALL GETFULNAM (I4ARRAY)    !(INTEGER*4 I4ARRAY(4))
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
	SUBROUTINE GETNAM (I4NAME)
	IMPLICIT NONE
C
	INCLUDE	    '($SYSSRVNAM)'
	INCLUDE	    '($JPIDEF)'
C
	INTEGER*4	I4NAME(2)
C
	INTEGER*4	I4FULNAM(4)
	CHARACTER*15	CXFULNAM
	EQUIVALENCE	(I4FULNAM,CXFULNAM)
C
	INTEGER*4	PREFIX_LEN
C
	INTEGER*4	I4PRFNAM
	CHARACTER*4	CXPRFNAM
	EQUIVALENCE	(CXPRFNAM, I4PRFNAM)
C
	LOGICAL*4       FIRST/.TRUE./
C
	IF(.NOT.FIRST) GOTO 9000
C
	FIRST = .FALSE.
	CALL GETFULNAM(I4FULNAM)    !GET FULL NAME
C
C GET THE CURRENT PROJECT PREFIX (I.E., VALUE OF GXPROJ)
C
	CALL GETPRFX(I4PRFNAM, PREFIX_LEN)
C
C IF PROJECT PREFIX DOES NOT EXIST IN NAME, JUST RETURN FIRST 8 CHARACTERS
C
	IF( CXFULNAM(1:PREFIX_LEN).NE.CXPRFNAM(1:PREFIX_LEN) ) GOTO 9000
C
C OTHERWISE REMOVE PREFIX BEFORE RETURNING
C
	CXFULNAM = CXFULNAM(PREFIX_LEN+1: )
C
C
9000	CONTINUE
C
	I4NAME(1) = I4FULNAM(1)
	I4NAME(2) = I4FULNAM(2)
C
	RETURN
	END
