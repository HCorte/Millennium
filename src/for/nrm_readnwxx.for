C
C $Log:   GXAFXT:[GOLS]READNWXX.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:38:46   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   30 Jan 1994 22:07:18   HXK
C  Initial revision.
C
C *** READNWXX    <<< general purpose READ with no wait >>>
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
C Copyright 1992 GTECH Corporation. All rights reserved.
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE READNWXX(RAB, FDB, BEGBLK, BUFFER, STATUS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:DISKIO.DEF'
        INCLUDE '($SYSSRVNAM)'
        INCLUDE '($RABDEF)'
        INCLUDE '($RMSDEF)'
C
        RECORD      /RABDEF/ RAB
        INTEGER*4   FDB(FDB_LENGTH)
        INTEGER*4   BEGBLK
        INTEGER*4   BUFFER(*)
        INTEGER*4   STATUS
C
C
C
C       set # of bytes to transfer, beginning block #, and
C       user buffer address
C
        RAB.RAB$W_USZ = FDB(FDB_BYTSZ)
        RAB.RAB$L_BKT = BEGBLK
        RAB.RAB$L_UBF = %LOC(BUFFER)
C
C       be sure asynchronous bit is OFF
C
        RAB.RAB$L_ROP = RAB.RAB$L_ROP .OR. RAB$M_ASY
C
C Now do the READ
C
        STATUS = SYS$READ(RAB)
        FDB(FDB_IOLEN) = ZEXT(RAB.RAB$W_RSZ) !(ZEXT is zero extend)
        IF(STATUS)THEN
          STATUS = 0                ! FOR COMPATIBILITY
        ENDIF
C
        FDB(FDB_STAT) = STATUS
C
        RETURN
        END
