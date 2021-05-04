C----+-----------------------------------------------------------------
C    | SUBROUTINE OLM_QUETRA
C    |    This subroutine queues a transaction to the COMOLM task
C    +-----------------------------------------------------------------
C    | INPUT PARAMETERS:
C    |    BUFNUM    Number of the buffer to be queued
C    |    STAT      Status of the operation (output)
C----+-----------------------------------------------------------------
        SUBROUTINE OLM_QUETRA (BUFNUM)
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
        
        CALL OPSTXT('BUFFER WILL BE INSERTED IN COMOLMQUE(1)')
        CALL ABL (BUFNUM, COMOLMQUE(1), STAT)
        CALL OPS('BUFFER is INSERTED IN COMOLMQUE(1) status:',STAT,STAT)        
C        IF (STAT .EQ. GLIST_STAT_GOOD) THEN
C            CALL LIB$ADAWI(1, ACTTSK(TASK), RESULT)
C        ENDIF        
        
        RETURN
        END


C----+-----------------------------------------------------------------
C    | SUBROUTINE OLM_DQUTRA
C    |    This subroutine dequeues a transaction for the COMOLM task
C    +-----------------------------------------------------------------
C    | INPUT PARAMETERS:
C    |    BUFNUM    Number of the buffer to be dequeued
C    |    STAT      Status of the operation (output)
C----+-----------------------------------------------------------------
        SUBROUTINE OLM_DQUTRA(BUFNUM)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:GLIST.DEF'

        INTEGER*4  STAT
        INTEGER*4  BUFNUM

        CALL RTL (BUFNUM, COMOLMQUE(1), STAT)
        IF (STAT .EQ. GLIST_STAT_EMPTY) THEN
          BUFNUM = 0
C        ELSE
C           CALL LIB$ADAWI(-1, ACTTSK(TASK), RESULT)
        ENDIF
        
        RETURN
        END