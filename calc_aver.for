c File: calc_aver.f90.
c External function returning average of array
      FUNCTION CALC_AVERAGE(D)
        REAL :: CALC_AVERAGE
        REAL, INTENT(IN) :: D(:)
        CALC_AVERAGE = SUM(D) / UBOUND(D, DIM = 1)
      END FUNCTION CALC_AVERAGE 