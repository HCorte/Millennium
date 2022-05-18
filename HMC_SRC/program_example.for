PROGRAM PROGRAM_EXAMPLE
    IMPLICIT NONE

      CHARACTER*60  FILENAME,FILENAME2

      WRITE(6,102) 'ENTER FILE NAME:'
      READ(5,101) FILENAME
      write(6,102) 'File Name is: ',FILENAME
C-------second Read-------------------------
C        PAUSE
C        WRITE(6,102) 'ENTER FILE NAME2:'
      CALL WIMG_TEST(5,'ENTER FILE NAME2:')
      READ(5,101) FILENAME2
      write(6,102) 'File Name is: ',FILENAME2
101     FORMAT(' ',A)
102     FORMAT(' ',A)
    END