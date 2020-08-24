C
C FUNCTION IGSDEBUG
C
C V01 04-APR-2014 SCML Creation
C
C This test function is used to see if the debug flag for a given IGS application
C is active or not
C
C Calling sequence:
C
C     LOGICAL = IGSDEBUG(IGS_APP)
C
C Input parameters:
C
C     IGS_APP      Int*4       ! The IGS Application flag (see IGSDEBUG.DEF)
C
C Output parameters:
C
C     IGSDEBUG     Logical     ! Is the IGS Application flag active ?
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2014 SCML. All rights reserved.
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW

        LOGICAL FUNCTION IGSDEBUG(IGSAPP)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        
        INTEGER*4 IGSAPP
        
        IGSDEBUG = .FALSE.
        
        IF(IAND(P(IGSDEBUGF),IGSAPP) .NE. 0) THEN
            IGSDEBUG = .TRUE.
        ENDIF

        RETURN
        END
