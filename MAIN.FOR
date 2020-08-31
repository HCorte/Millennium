c File: main.f90  ! This program calculates the average of five numbers
      PROGRAM MAIN
        USE ARRAY_CALCULATOR                         
        REAL, DIMENSION(5) :: A = 0        
        REAL :: AVERAGE

        PRINT *, 'Type five numbers: '        
        READ  (*,'(BN,F10.3)') A        
        AVERAGE = CALC_AVERAGE(A)                    
        PRINT *, 'Average of the five numbers is: ', AVERAGE     
      END PROGRAM MAIN