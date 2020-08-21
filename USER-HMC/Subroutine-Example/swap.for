      PROGRAM SWAPNUMBS
      IMPLICIT NONE
        integer m, n
c
        m = 1
        n = 2 

        call iswap(m, n)
        write(*,*) m, n

        stop    
      END