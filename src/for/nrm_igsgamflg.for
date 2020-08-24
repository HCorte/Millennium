C
C FUNCTION IGSGAMFLG
C
C V01 21-MAY-2014 SCML Creation
C
C This test function is used to see if the game flag for a given IGS 
C parameter is active or not
C
C Calling sequence:
C
C     LOGICAL = IGSGAMFLG(P(IGS...),IGS_GAMNUM(GAMNUM,GAMTYP))
C
C Input parameters:
C
C       I2ARY:  (Int*2(*)) ! The P(...) parameter to test
C   GAMBITNUM:  (Int*4)    ! The game number in IGS system; for further 
C                          ! information, please see PRMIGS.DEF
C
C Output parameters:
C
C   IGSGAMFLG:  (Logical)  ! Is the IGS Game flag for a P(...) parameter active ?
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2014 SCML. All rights reserved.
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW

        LOGICAL*1 FUNCTION IGSGAMFLG(I2ARY, GAMBITNUM)
        IMPLICIT NONE
C
        INTEGER*2   I2ARY(0:*)
        INTEGER*4   GAMBITNUM
C
        INTEGER*4   WRD
        INTEGER*4   NDX
C
        LOGICAL BTEST
C
        IF((GAMBITNUM - 1) .LT. 0) THEN
            IGSGAMFLG = .FALSE.
            RETURN
        ENDIF
        WRD = ISHFT((GAMBITNUM - 1), -4)
        NDX = IAND ((GAMBITNUM - 1), '0000000F'X)
        IGSGAMFLG = BTEST(I2ARY(WRD),NDX)
        RETURN
        END
