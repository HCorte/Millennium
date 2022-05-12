#include <stdio.h>
/*
The sleep function is declared in unistd.h.
*/
#include <unistd.h>
/* 
cc SLEEP.C <-- compile files extension .c

http://www.yolinux.com/TUTORIALS/LinuxTutorialMixingFortranAndC.html
Fortran subroutines are the equivalent of "C" functions returning "(void)".

The entry point names for some FORTRAN compilers have an underscore appended to the name. This is also true for common block/structure names as shown above.
FORTRAN	            C
CALL SUBRA( ... )	subra_( ... )

*/
//unsigned int sleep(unsigned int seconds);

void sleep_aux(int seconds)
{
    printf("Starts to sleep for %d seconds\n",seconds);
    sleep(seconds);
    printf("\nfinished sleeping...\n");
}