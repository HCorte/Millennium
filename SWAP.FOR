      PROGRAM SWAPNUMBS
      IMPLICIT NONE
        integer m, n
        character*17 log
        character*34 logfinal
        character*17 logaux
        character*80 path
        !character*24 date
        character*8 date
        character*10 time
        character*20 logdate

c
        m = 1
        n = 2
        path = 'GXPROJ:[WRK.HMC.EXAMPLES.LOGFILES]logs.dat'         
        CALL CONCATENATE_INT(m, n, logfinal) 

        print *,'first daterow is: '//logfinal
        CALL LOG_ROW(logfinal, path)

        call iswap(m, n)        
        write(*,*) m, n

        CALL CONCATENATE_INT(m, n, logfinal)
        CALL LOG_ROW(logfinal, path)

        stop    
      END