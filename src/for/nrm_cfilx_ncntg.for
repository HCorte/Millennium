C SUBROUTINE CFILX_NCNTG
C
C This subroutine will create 'contiguous best try' file. 
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
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CFILX_NCNTG(FILENAME, FILTYP, RECLEN, SIZE, ISIZE,
     *                   DUM1, DUM2, ST)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER   FILENAME*(*)
	INTEGER*4   FILTYP
	INTEGER*4   RECLEN
	INTEGER*4   SIZE
	INTEGER*4   ISIZE
	INTEGER*4   DUM1
	INTEGER*4   DUM2
	INTEGER*4   ST
	
	LOGICAL*1   ISTHERE
C
	INTEGER*4   LOLUN/1/	    !SMALLEST LUN TO LOOK AT
	INTEGER*4   HILUN/14/	    !HIGHEST  LUN TO LOOK AT
C
	INTEGER*4   LUN
	INTEGER*4   SECTORS
C
	INTEGER*4   CFILNCNTG
	EXTERNAL    CFILNCNTG	    !USEROPEN FOR NON-CONTIGUOUS FILE
C
C
C
C If the file doesn't exist, just return with status = -1
C
	INQUIRE(FILE=FILENAME, EXIST=ISTHERE)
	IF(ISTHERE)THEN
	  TYPE *,IAM(), 'CFILX: File ', FILENAME, ' already exists.'
	  ST = -1
	  GOTO 9000
	ENDIF
C
C Find a free lun to use for the open
C
	DO 1100 LUN = LOLUN, HILUN
C
C ISTHERE is set to true if LU is already used.
C
	  INQUIRE(UNIT=LUN, OPENED=ISTHERE, IOSTAT=ST)
	  IF(.NOT.ISTHERE) GOTO 1200
C
1100	CONTINUE
	TYPE *,IAM(),'CFILX - NO LUN AVAIL FOR CREATE'
	ST = -1		    ! NO AVAILABLE LUN
	GOTO 9000
C
C
C
1200	CONTINUE
C
	IF(FILTYP.EQ.0)THEN
	  SECTORS = (SIZE+1)/2
	  OPEN(LUN, FILE=FILENAME, ACCESS='DIRECT',
	1     INITIALSIZE=SECTORS+1, STATUS='NEW', RECL=512/4,
	1     ORGANIZATION='RELATIVE', IOSTAT=ST, USEROPEN=CFILNCNTG)
	ELSE
	  OPEN(LUN, FILE=FILENAME, ACCESS='SEQUENTIAL',
	1      STATUS='NEW', RECL=RECLEN, IOSTAT=ST)
	ENDIF
C
	CALL USRCLOS1(LUN)
C
9000	CONTINUE
	RETURN
	END
