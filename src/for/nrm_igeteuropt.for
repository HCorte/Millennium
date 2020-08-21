C
C SUBROUTINE IGETEUROPT
C
C V01 09-MAR-2016 SCML Initial
C
C SUBROUTINE TO GET THE INPUT OPTION FLAG VALUE FOR EUROMILLIONS SYSTEM
C MESSAGES.
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE IGETEUROPT(TRABUF,OPTION)
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
C
        RETURN
C
C EUR WAG
C
100     CONTINUE
        IF(GTYP.EQ.TEUM .AND. GIND.EQ.EUM1GI) THEN                              !EUROMILLIONS GAME
          IF(TRABUF(TEUWQP).NE.0) THEN
            OPTION = OPTION + '0200'X                                           !QUICK PICK FLAGS BY BOARD
          ENDIF
          IF(TRABUF(TEUWNMK).NE.0 .AND. TRABUF(TEUWNST).NE.0) THEN
            OPTION = OPTION + '0100'X                                           !SYSTEM NUMBER PRESENT
          ENDIF
          IF(TRABUF(TEUW_KIWFL).NE.0) THEN
            OPTION = OPTION + '0080'X                                           !EUROMILLIONS WAS PLAYED WITH JOKER (PARTICIPATING IN JOKER 1)
          ENDIF
        ENDIF
        RETURN
C
        END
