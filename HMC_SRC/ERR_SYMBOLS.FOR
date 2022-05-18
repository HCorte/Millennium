      PROGRAM ERR_SYMBOLS
      IMPLICIT NONE 
C      INCLUDE '(LIB$ROUTINES)'
      INCLUDE '($SSDEF)'
      INCLUDE '($CHFDEF)'
      INCLUDE '($FORDEF)'
      
C      INTEGER*4 FOR$IOS_RECNUMOUT

            write (*,*) "Error code:",FOR$_FILNOTFOU
            write (*,*) "Error code:",FOR$_NO_SUCDEV
            write (*,*) "Error code:",FOR$_FILNAMSPE
            write (*,*) "Error code:",FOR$_OPEFAI
            write (*,*) "Error code:",FOR$_NOTFORSPE
C            FOR$IOS_RECNUMOUT

      END            