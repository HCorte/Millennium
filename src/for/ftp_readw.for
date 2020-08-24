C
C SUBROUTINE FTP_READW
C
C V01 22-NOV-1999 OXK Initial release
C
C *** FTP_READW       <<<READ and wait for completion>>>
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
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE FTP_READW(FDB, BLOCK, BUFFER, STATUS)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
	INCLUDE	    'INCLIB:DISKIO.DEF'
C
	INTEGER*4   FDB(FDB_LENGTH)
	INTEGER*4   BLOCK
	INTEGER*4   BUFFER(*)
	INTEGER*4   STATUS
C
	INTEGER*4   FOR$RAB
	INTEGER*4   LUN
	INTEGER*4   BEGBLK
C
C
C
	LUN   = FDB(FDB_LUN)
        IF(BLOCK.EQ.1) THEN
	   BEGBLK= (BLOCK-1)*FDB(FDB_BLKSZ)
        ELSE
           BEGBLK= (BLOCK-1)*FDB(FDB_BLKSZ) + 1
        ENDIF
C
	CALL READXX(%VAL(FOR$RAB(LUN)), FDB, BEGBLK, BUFFER, STATUS)
C
	RETURN
	END
