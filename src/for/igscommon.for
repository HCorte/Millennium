C----+-----------------------------------------------------------------
C    | SUBROUTINE IGS_QUETRA
C    |    This subroutine queues a transaction to the COMIGS task
C    +-----------------------------------------------------------------
C    | INPUT PARAMETERS:
C    |    BUFNUM    Number of the buffer to be queued
C----+-----------------------------------------------------------------
        SUBROUTINE IGS_QUETRA_ST (BUFNUM, STAT)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:GLIST.DEF'

        INTEGER*4       TASK
        INTEGER*4       BUFNUM

        INTEGER*4       STAT, RESULT
        
        CALL ABL (BUFNUM, COMIGSQUE(1), STAT)
        
        RETURN
        END

        SUBROUTINE IGS_QUETRA (BUFNUM)
        IMPLICIT NONE

        INTEGER*4       BUFNUM

        INTEGER*4       STAT
        
        CALL IGS_QUETRA_ST (BUFNUM, STAT)
        
        RETURN
        END



C----+-----------------------------------------------------------------
C    | SUBROUTINE IGS_DQUTRA
C    |    This subroutine dequeues a transaction for the COMIGS task
C    +-----------------------------------------------------------------
C    | INPUT PARAMETERS:
C    |    BUFNUM    Number of the buffer to be dequeued
C----+-----------------------------------------------------------------
        SUBROUTINE IGS_DQUTRA_ST(BUFNUM, STAT)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:GLIST.DEF'

        INTEGER*4  STAT
        INTEGER*4  BUFNUM

        CALL RTL (BUFNUM, COMIGSQUE(1), STAT)
        IF (STAT .EQ. GLIST_STAT_EMPTY) THEN
          BUFNUM = 0
        ENDIF
        
        RETURN
        END
        
        SUBROUTINE IGS_DQUTRA(BUFNUM)
        IMPLICIT NONE
        

        INTEGER*4  STAT
        INTEGER*4  BUFNUM

        CALL IGS_DQUTRA_ST(BUFNUM, STAT)
        
        RETURN
        END
        
        
        
C----+-----------------------------------------------------------------
C    | SUBROUTINE IGS_TOPQUE
C    |    This subroutine returns the top element of the queue, or zero,
C    |    if none
C    +-----------------------------------------------------------------
C    | OUTPUT PARAMETERS:
C    |    BUFNUM    Number of the buffer to be dequeued
C----+-----------------------------------------------------------------
        SUBROUTINE IGS_TOPQUE_ST (BUFNUM)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:GLIST.DEF'
C
        INTEGER*4       TASK
        INTEGER*4       BUFNUM
C
        INTEGER*4       STAT
C
        CALL LISTTOP (BUFNUM, COMIGSQUE(1), STAT)
C
        IF (STAT .EQ. GLIST_STAT_EMPTY) THEN
          BUFNUM = 0
        ENDIF
C
        RETURN
        END

        SUBROUTINE IGS_TOPQUE (BUFNUM)
        IMPLICIT NONE

        INTEGER*4       BUFNUM
        INTEGER*4       STAT
C
        CALL IGS_TOPQUE_ST(BUFNUM, STAT)

        RETURN
        END
        
