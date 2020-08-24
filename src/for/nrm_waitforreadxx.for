C
C $Log:   GXAFXT:[GOLS]WAITFORREADXX.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:58:32   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   30 Jan 1994 22:31:12   HXK
C  Initial revision.
C
C *** WAITFORREADXX - WRITE WITHOUT A WAIT
C
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
C Copyright 1992 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE WAITFORREADXX(RAB, FDB, STATUS)
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
        INTEGER*4   STATUS
C
C
C
C       WAIT FOR THE READ TO COMPLETE
C
        STATUS = SYS$WAIT(RAB)
        FDB(FDB_IOLEN) = ZEXT(RAB.RAB$W_RSZ)
        IF(STATUS)THEN
C
C If we didn't get everything, return 144 for warning
C
          IF(FDB(FDB_IOLEN).NE.FDB(FDB_BYTSZ))THEN
            STATUS = 144
          ELSE
            STATUS = 0              ! FOR COMPATIBILITY
          ENDIF
        ENDIF
C
        IF(STATUS .EQ. RMS$_EOF)THEN
          STATUS = 144              ! FOR COMPATIBILITY
        ENDIF
C
        FDB(FDB_STAT) = STATUS
C
C       turn asynchronous bit OFF
C
        RAB.RAB$L_ROP = RAB.RAB$L_ROP .AND. .NOT.RAB$M_ASY
C
        RETURN
        END
