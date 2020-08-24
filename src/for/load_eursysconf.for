C
C SUBROUTINE LOAD_EURSYSCONF
C
C V01 11-MAR-2016 SCML CREATION (M16 PROJECT)
C
C SUBROUTINE TO LOAD GAME CONFIGURATION FILES INTO MEMORY OF 
C EUROMILLIONS GAMING SYSTEM
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Copyright 2016 SCML Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE LOAD_EURSYSCONF
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:EURCOM.DEF'
C
        INTEGER*4 SIZE
C
C       CLEAR EURCOM
C
        SIZE = %LOC(LAST_EURCOM) - %LOC(FRST_EURCOM(1)) + 4
        CALL FASTSET(0, FRST_EURCOM, SIZE/4)
C
C       SETTING EUROMILLIONS CONFIGURATION PARAMETERS (THIS SHOUDE BE MIGRATED TO
C       A CONFIGURATION FILE NAMED ECF.FIL IN THE FUTURE)
C
        EGTNTAB(TEUM,EUM1GI) = EUM1GN                                           !EM GAME NUMBER IN EUROMILLIONS SYSTEM
        EGTNTAB(TRAF,RAF1GI) = RAF1GN                                           !SoM GAME NUMBER IN EUROMILLIONS SYSTEM
        EGTNTAB(TRAF,RAF2GI) = RAF2GN                                           !SM GAME NUMBER IN EUROMILLIONS SYSTEM
C       EUM GAME
        EGNTTAB(GAMTYP,EUM1GN) = TEUM                                           !EUM GAME TYPE
        EGNTTAB(GAMIDX,EUM1GN) = EUM1GI                                         !EUM GAME INDEX
C       SoM GAME
        EGNTTAB(GAMTYP,RAF1GN) = TRAF                                           !SoM GAME TYPE
        EGNTTAB(GAMIDX,RAF1GN) = RAF1GI                                         !SoM GAME INDEX
C       SM GAME
        EGNTTAB(GAMTYP,RAF2GN) = TRAF                                           !SM GAME TYPE
        EGNTTAB(GAMIDX,RAF2GN) = RAF2GI                                         !SM GAME INDEX
C       RAFFLE GAME NUMBER
        ERGNTAB(EUM1GN) = RAF2GN                                                !SM GAME NUMBER ASSOCIATED TO EM GAME

        TYPE*,IAM(),'Loading config files of games of EuroMillions gaming system'
C
C       EUROMILLIONS GAME
C
        CALL LOAD_EUM1CF
C
C       SM GAME
C
        CALL LOAD_RAF2CF
C
        RETURN
        END
C
C END LOAD_EURSYSCONF
C
