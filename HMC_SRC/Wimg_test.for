
C        SUBROUTINE WIMG_TEST(LUN, STRING)
C        IMPLICIT NONE
C        INCLUDE 'INCLIB:SYSEXTRN.DEF'
CC
C        INTEGER*4   LUN
C        CHARACTER   STRING*(*)
CC
CC
C        WRITE(LUN, 1001) ,STRING
C1001    FORMAT(' ',A,' >',$) !$ is creating a carriage return
CC
C        RETURN
C        END
C
        SUBROUTINE WIMG_TEST(LUN, STRING)
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE '(LIB$ROUTINES)'
C        include '($ssdef)'
C
        INTEGER*4   LUN
        CHARACTER   STRING*(*)
C Validate if symbol flag exists if it does then
C subroutine is being called from script scope wrapper that invokes dynamicaly
C a group of executables  
C        integer*4   LIB$GET_SYMBOL
C        INTEGER*4   SYMBOL_STATUS
        CHARACTER*10 SYMBOL_VALUE

C        SYMBOL_STATUS = LIB$GET_SYMBOL('script_origin',SYMBOL_VALUE)
C        WRITE (*,*) SYMBOL_STATUS

C        IF(SYMBOL_STATUS.EQ.SS$_NORMAL) THEN
C        IF(SYMBOL_STATUS) THEN
        IF(LIB$GET_SYMBOL('script_origin',SYMBOL_VALUE)) THEN
          WRITE(6, 1011) ,STRING
C          WRITE(*,*) "Script runned"
        ELSE
          WRITE(LUN, 1001) ,STRING
C          WRITE(*,*) "Executable runned"
        ENDIF

1000    FORMAT('0',A,' >')
1001    FORMAT('0',A,' >',$)
C
        RETURN
        END