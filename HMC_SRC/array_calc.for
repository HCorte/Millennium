c File: array_calc.f90.  
c Module containing various calculations on arrays.
      MODULE ARRAY_CALCULATOR      
        INTERFACE        
          FUNCTION CALC_AVERAGE(D)          
            REAL :: CALC_AVERAGE          
            REAL, INTENT(IN) :: D(:)        
          END FUNCTION CALC_AVERAGE      
        END INTERFACE     
       ! Other subprogram interfaces...     
      END MODULE ARRAY_CALCULATOR
