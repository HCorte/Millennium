C       *************************************
        SUBROUTINE GET_TERM (AGENT, TERM, ST)
C       *************************************
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'

        INTEGER*4  TERM      !TERMINAL NUMBER (OUTPUT)
        INTEGER*4  AGENT     !AGENT NUMBER    (INPUT)
        INTEGER*4  ST        !<> 0 => AGENT NOT FOUND

	IF (AGENT.EQ.0) THEN
	   ST = -1
        ELSE
	   ST = 0
           TERM = 1
           DO WHILE (TERM.LE.NUMAGT)
              IF (AGTTAB(AGTNUM,TERM).EQ.AGENT) EXIT  !OUT OF LOOP
              TERM = TERM + 1                 
           ENDDO
           IF (TERM.GT.NUMAGT) THEN
              ST = -1
              TERM = 0
           ENDIF
        ENDIF

        RETURN
        END
