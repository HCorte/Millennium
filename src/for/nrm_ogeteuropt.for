C
C SUBROUTINE OGETEUROPT
C
C V01 08-MAR-2016 SCML Initial
C
C SUBROUTINE TO GET THE OUTPUT OPTION FLAG VALUE FOR EUROMILLIONS SYSTEM
C MESSAGES.
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE OGETEUROPT(TRABUF,OPTION)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
C
        INTEGER*4 OPTION                                                        !OUTPUT
C
        INTEGER*4 EURTYP                                                        !EUROMILLIONS TRANSACTION TYPE
        INTEGER*4 GTYP                                                          !GAME TYPE 
        INTEGER*4 GIND                                                          !GAME INDEX
C
        EURTYP = TRABUF(TEUTYP)
        GTYP = TRABUF(TGAMTYP)
        GIND = TRABUF(TGAMIND)
C
        OPTION = 0
C
        IF(EURTYP.EQ.TWAG) GOTO 100                                             !EUR WAGER
        IF(EURTYP.EQ.TVAL) GOTO 200                                             !EUR VALIDATION
        IF(EURTYP.EQ.TCAN) GOTO 300                                             !EUR CANCELLATION
C
        RETURN
C
C EUR WAG
C
100     CONTINUE
        IF(GTYP.EQ.TEUM .AND. GIND.EQ.EUM1GI) THEN                              !EUROMILLIONS GAME
          IF(TRABUF(TEUW_SMWFL).NE.0) OPTION = OPTION + '08'X                   !SM DATA PRESENT
          IF(TRABUF(TEUW_SHWFL).NE.0) OPTION = OPTION + '04'X                   !SoM DATA PRESENT
        ENDIF
        RETURN
C
C EUR VAL
C
200     CONTINUE
        IF(GTYP.EQ.TEUM .AND. GIND.EQ.EUM1GI) THEN                              !EUROMILLIONS GAME
          IF(TRABUF(TEUV_NIFFL).NE.0) OPTION = OPTION + '80'X                   !PLAYER NIF PRESENT FLAG
          IF(TRABUF(TEUV_NIFCF).NE.0) OPTION = OPTION + '40'X                   !PLAYER NIF CONFIRMATION REQUIRED FLAG
          IF(TRABUF(TEUV_SHVFL).NE.0) OPTION = OPTION + '20'X                   !SoM VALIDATION FLAG
        ELSEIF(GTYP.EQ.TRAF .AND. GIND.EQ.RAF2GI) THEN                          !SM GAME
          IF(TRABUF(TEUV_NIFFL).NE.0) OPTION = OPTION + '80'X                   !PLAYER NIF PRESENT FLAG
          IF(TRABUF(TEUV_NIFCF).NE.0) OPTION = OPTION + '40'X                   !PLAYER NIF CONFIRMATION REQUIRED FLAG
        ENDIF
        RETURN
C
C EUR CAN
C
300     CONTINUE
        IF(GTYP.EQ.TEUM .AND. GIND.EQ.EUM1GI) THEN                              !EUROMILLIONS GAME
          IF(TRABUF(TEUC_SMCFL).NE.0) OPTION = OPTION + '80'X                   !SM CANCELLATION DATA PRESENT FLAG
          IF(TRABUF(TEUC_SHCFL).NE.0) OPTION = OPTION + '40'X                   !SoM CANCELLATION FLAG
        ENDIF
        RETURN
C
        END
