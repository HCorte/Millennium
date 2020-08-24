C
C PROGRAM MAKFIL2
C
C V01 18-MAR-98 UXN Initial release. (Taken from MAKFIL)
C
C This will create a non-contiguous file and initialize it to 0's
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM MAKFIL2
	IMPLICIT NONE
C
	INCLUDE	'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4	SECTORS
	INTEGER*4	BUKSIZ
	INTEGER*4	BKTNUM
	INTEGER*4	FDB(7)
	INTEGER*4	ST
	LOGICAL		ONDISK
        LOGICAL         KEEP
C
	INTEGER*4	BIGBUF(128,100)/12800*0/
C
	CHARACTER*60	FILENAME
C
        KEEP = .FALSE.
C
	CALL COPYRITE
C
	TYPE*,IAM()
	TYPE*,IAM(),'This program will allocate non-contiguous file'
	TYPE*,IAM()
 	CALL WIMG(5,'ENTER FILE NAME')
	ACCEPT 101,FILENAME
101	FORMAT(A)
100	CONTINUE
	CALL WIMG(5,'# OF VAX SECTORS   ')
	ACCEPT *,SECTORS
	IF(SECTORS.LT.1)GOTO 100
C
C
C If the file already exists, simply stop
C
	INQUIRE(FILE=FILENAME, EXIST=ONDISK)
	IF(ONDISK)THEN
	  TYPE *,IAM(),'FILE ALREADY EXISTS'
	  CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
	CALL NEWFIL_NCNTG(1, FILENAME, SECTORS, KEEP, ST)
C
	CALL GSTOP(GEXIT_SUCCESS)
	END
