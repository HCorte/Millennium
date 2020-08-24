CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C File      : DIGSODSWAG.FOR
C Change Log:
C
C Ver Date       Author  Comment
C --- ---------- ------- ----------------------------------------------
C V01 2014.02.24 SCML    PLACARD PROJECT - Created - Decodes terminal
C                        wager input messages for IGS;
C
C THIS ROUTINE CAN GENERATE THE FOLLOWING 'SYNTERRCOD'
C         5     GAME INDEX NOT PLACARD
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C                 THIS ITEM IS THE PROPERTY OF SCML.
C
C             COPYRIGHT 2014 SCML. ALL RIGHTS RESERVED.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE DIGSODSWAG(TERMES,TRABUF)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'

C DEBUG - START /*
        INCLUDE 'INCLIB:IGSDEBUG.DEF'
C DEBUG - END */

        BYTE        TERMES(*)


C
C IGS WAGER TRANSACTION BODY
C
C       GAME INDEX (1 - Placard)                                    (12)
        TRABUF(TGAMIND) = ZEXT(TERMES(7))
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TGAMIND:',TRABUF(TGAMIND),TRABUF(TGAMIND))
           TYPE*,'TGAMIND:',TRABUF(TGAMIND)
        ENDIF
C DEBUG - END */

C       VALIDATE GAME INDEX
        IF(TRABUF(TGAMIND) .EQ. 1) THEN !PLACARD

C          ABP GAME ID                                              (34)
           TRABUF(TIGSW_XGID) = ZEXT(TERMES(20))
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSW_XGID:',TRABUF(TIGSW_XGID),TRABUF(TIGSW_XGID))
           TYPE*,'TIGSW_XGID:',TRABUF(TIGSW_XGID)
        ENDIF
C DEBUG - END */

C          SYBTYPE ID                                               (35)
           CALL TERM_TO_HOST(TERMES(21), TRABUF(TIGSW_STID), 2)
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSW_STID:',TRABUF(TIGSW_STID),TRABUF(TIGSW_STID))
           TYPE*,'TIGSW_STID:',TRABUF(TIGSW_STID)
        ENDIF
C DEBUG - END */

C          PLAYER NIF NUMBER                                        (33)
           CALL TERM_TO_HOST(TERMES(23), TRABUF(TIGSW_PNIF), 4)
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSW_PNIF:',TRABUF(TIGSW_PNIF),TRABUF(TIGSW_PNIF))
           TYPE*,'TIGSW_PNIF:',TRABUF(TIGSW_PNIF)
        ENDIF
C DEBUG - END */

C          UNIT STAKE OF THE BET                                    (36)
           CALL TERM_TO_HOST(TERMES(27), TRABUF(TIGSW_USTK), 4)
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSW_USTK:',TRABUF(TIGSW_USTK),TRABUF(TIGSW_USTK))
           TYPE*,'TIGSW_USTK:',TRABUF(TIGSW_USTK)
        ENDIF
C DEBUG - END */

C          NUMBER OF SELECTIONS                                     (37)
           TRABUF(TIGSW_TBET) = ZEXT(TERMES(31))
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSW_TBET:',TRABUF(TIGSW_TBET),TRABUF(TIGSW_TBET))
           TYPE*,'TIGSW_TBET:',TRABUF(TIGSW_TBET)
        ENDIF
C DEBUG - END */

C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('P(SUPWAG): ',P(SUPWAG),P(SUPWAG))
           TYPE*,'P(SUPWAG): ',P(SUPWAG)
        ENDIF
C DEBUG - END */

C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('P(IGSPPLA): ',P(IGSPPLA),P(IGSPPLA))
           TYPE*,'P(IGSPPLA): ',P(IGSPPLA)
        ENDIF
C DEBUG - END */

C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('P(IGSPWAG): ',P(IGSPWAG),P(IGSPWAG))
           TYPE*,'P(IGSPWAG): ',P(IGSPWAG)
        ENDIF
C DEBUG - END */

C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('P(IGSPGWAG): ',P(IGSPGWAG),P(IGSPGWAG))
           TYPE*,'P(IGSPGWAG): ',P(IGSPGWAG)
        ENDIF
C DEBUG - END */

C
C IF FUNCTION SUPRESSED THEN SET ERROR MESSAGE
C
           IF((P(SUPWAG) .NE. 0) .OR. P(IGSPPLA) .NE. 0
     *                           .OR. P(IGSPWAG) .NE. 0
     *                           .OR. IGSGAMFLG(P(IGSPGWAG),
     *                                          IGS_GAMNUM(TRABUF(TGAMTYP),
     *                                                     TRABUF(TGAMIND)))) 
     *     THEN
              TRABUF(TERR)  = SUPR
              TRABUF(TSTAT) = REJT

C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('FUNCTION SUPRESSED!!!',0,0)
           TYPE*,'FUNCTION SUPRESSED!!!'
        ENDIF
C DEBUG - END */

           ENDIF
        ELSE
           TRABUF(TSTAT) = REJT
           TRABUF(TERR)  = SYNT
           SYNTERRCOD=5
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('INVALID GAME INDEX: ',TRABUF(TGAMIND),TRABUF(TGAMIND))
           TYPE*,'INVALID GAME INDEX: ',TRABUF(TGAMIND)
        ENDIF
C DEBUG - END */
        ENDIF


       END
