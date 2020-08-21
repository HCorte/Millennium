C NRM_READNW.FOR
C
C V02 04-JUL-2000 OXK -> NORMLIB
C V01             XXX Initial revision.
C
C *** READNW       <<<READ and do not wait for completion>>>
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
        SUBROUTINE READNW(FDB, BLOCK, BUFFER, STATUS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:DISKIO.DEF'
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
        LUN    = FDB(FDB_LUN)
        BEGBLK = (BLOCK-1)*FDB(FDB_BLKSZ) + 2
C
        CALL READNWXX(%VAL(FOR$RAB(LUN)), FDB, BEGBLK, BUFFER, STATUS)
C
        RETURN
        END
