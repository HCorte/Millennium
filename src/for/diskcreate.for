        INTEGER*4 FUNCTION DISKCREATE(FAB, RAB, LUN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:DISKIO.DEF'
C
        INCLUDE '($SYSSRVNAM)'
        INCLUDE '($FABDEF)'
        INCLUDE '($RABDEF)'
        INCLUDE '($RMSDEF)'
C
        INTEGER*4 LUN
C
        RECORD /FABDEF/ FAB
        RECORD /RABDEF/ RAB
C
C Set the block I/O bit in the FAC
C
        FAB.FAB$B_FAC = FAB.FAB$B_FAC .OR. FAB$M_BIO
C
C Set complete access privileges
C
        FAB.FAB$B_SHR = FAB.FAB$B_SHR .OR. FAB$M_UPI
C
C       contig best try and supersede existing file
C
        FAB.FAB$L_FOP = FAB.FAB$L_FOP .OR. FAB$M_CBT .OR. FAB$M_SUP
C
C Now open the file and connect to the record stream
C
C       DISKCREATE = SYS$OPEN(FAB)
        DISKCREATE = SYS$CREATE(FAB)
        IF(.NOT.DISKCREATE) THEN
          CALL LIB$SIGNAL(%VAL(DISKCREATE))
          GOTO 9000
        ENDIF
C
        DISKCREATE = SYS$CONNECT(RAB)
        IF(.NOT.DISKCREATE) THEN
          CALL LIB$SIGNAL(%VAL(DISKCREATE))
          GOTO 9000
        ENDIF
C
9000    CONTINUE
        RETURN
        END
