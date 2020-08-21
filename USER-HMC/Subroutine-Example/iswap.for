      subroutine iswap (a, b)
      integer a, b
c Local variables
      integer tmp

      tmp = a
      a = b
      b = tmp

      return
      end