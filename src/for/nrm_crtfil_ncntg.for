C
C SUBROUTINE CRTFIL_NCNTG 
C
C This subroutine allocates non-contiguos (contiguos-best-try) file and 
C clears it.
C
C V01 07-JUN-1999 UXN Initial revision (from CRTFIL)
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CRTFIL_NCNTG(NAME, LEN, STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4	DSIZE,
     *			FSIZE,
     *			LEN,
     *			NAME(5),
     *			NREC,
     *			SECTOR,
     *			ST,
     *			STATUS
C
	LOGICAL*4	FILE_EXISTS,
     *			KEEP
C
	INTEGER*4	I4NAME(5)
	CHARACTER*20	CXNAME
	EQUIVALENCE	(I4NAME(1), CXNAME)
C
C GET THE FILE NAME
C
	I4NAME(1) = NAME(1)
	I4NAME(2) = NAME(2)
	I4NAME(3) = NAME(3)
	I4NAME(4) = NAME(4)
	I4NAME(5) = NAME(5)
C
C ALLOCATE AND CLEAR FILE TO NEAREST 100 BLOCK INCREMENT
C
	STATUS   = 0
	SECTOR   = LEN
	NREC     = SECTOR / 100
	IF (MOD(SECTOR, 100) .NE. 0 .OR. NREC .LT. 1) THEN
	  NREC   = MAX(1, NREC + 1)
	  SECTOR = NREC * 100
	ENDIF
C
	KEEP = .FALSE.
	INQUIRE(FILE = CXNAME, EXIST = FILE_EXISTS)
C
	IF (FILE_EXISTS) THEN
	  CALL OPENX(1, CXNAME, 4, 0, 0, ST)
	  IF (ST .EQ. 0) THEN
	    CALL VAXGETFSIZ(1, FSIZE)
	    DSIZE = FSIZE - SECTOR
	    IF (DSIZE .GE. 0 .AND. DSIZE .LT. 100) KEEP = .TRUE.
	    CLOSE(1)
	  ENDIF
	ENDIF
C
	IF (.NOT. KEEP) CALL DFILW(NAME, 0, 0, ST)
C
	WRITE(6, 1000) IAM(), NAME, SECTOR
1000	FORMAT(X, A, 'Allocating & clearing ', 5A4, X, I8, ' sectors')
	CALL NEWFIL_NCNTG(1, CXNAME, SECTOR, KEEP, ST)
C
	IF (ST .NE. 0) THEN
	  WRITE(6, 2000) IAM(), NAME, ST
2000	  FORMAT(X, A, X, 5A4, ' allocation error ', I4)
	  STATUS = -1
	ENDIF
C
	RETURN
	END
