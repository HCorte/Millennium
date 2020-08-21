CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C File      : DIGSODSREP.FOR
C Change Log:
C
C Ver Date       Author  Comment
C --- ---------- ------- ----------------------------------------------
C V01 2014.02.24 SCML    PLACARD PROJECT - Created - Decodes terminal 
C                        report input messages for IGS;
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
        SUBROUTINE DIGSODSREP(TERMES,TRABUF)
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
C IGS VALIDATION TRANSACTION BODY
C
C       GAME INDEX (1 - Placard)                                    (12)
        TRABUF(TGAMIND) = ZEXT(TERMES(6))
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TGAMIND',TRABUF(TGAMIND),TRABUF(TGAMIND))
        ENDIF
C DEBUG - END */

C       VALIDATE GAME INDEX
        IF(TRABUF(TGAMIND) .EQ. 1) THEN !PLACARD

C          SEGMENT NUMBER REQUESTED                                 (33)
           TRABUF(TIGSR_SEGN) = ZEXT(TERMES(19))
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSR_SEGN',TRABUF(TIGSR_SEGN),TRABUF(TIGSR_SEGN))
        ENDIF
C DEBUG - END */

C          MEDIA ID                                                 (34)
           CALL TERM_TO_HOST(TERMES(20), TRABUF(TIGSR_MEID), 4)
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSR_MEID',TRABUF(TIGSR_MEID),TRABUF(TIGSR_MEID))
        ENDIF
C DEBUG - END */

C          PROGRAMME TEMPLATE ID                                    (35)
           CALL TERM_TO_HOST(TERMES(24), TRABUF(TIGSR_PTID), 4)
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSR_PTID',TRABUF(TIGSR_PTID),TRABUF(TIGSR_PTID))
        ENDIF
C DEBUG - END */

C          MEDIA VERSION LOW PART                                   (36)
           CALL TERM_TO_HOST(TERMES(28), TRABUF(TIGSR_MVRL), 4)
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSR_MVRL',TRABUF(TIGSR_MVRL),TRABUF(TIGSR_MVRL))
        ENDIF
C DEBUG - END */

C          MEDIA VERSION HIGH PART                                  (37)
           CALL TERM_TO_HOST(TERMES(32), TRABUF(TIGSR_MVRH), 4)
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSR_MVRH',TRABUF(TIGSR_MVRH),TRABUF(TIGSR_MVRH))
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
           CALL OPS('P(IGSPREP): ',P(IGSPREP),P(IGSPREP))
           TYPE*,'P(IGSPREP): ',P(IGSPREP)
        ENDIF
C DEBUG - END */

C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('P(IGSPGREP): ',P(IGSPGREP),P(IGSPGREP))
           TYPE*,'P(IGSPGREP): ',P(IGSPGREP)
        ENDIF
C DEBUG - END */

C
C IF FUNCTION SUPRESSED THEN SET ERROR MESSAGE
C
           IF(P(IGSPPLA) .NE. 0 .OR. P(IGSPREP) .NE. 0
     *                          .OR. IGSGAMFLG(P(IGSPGREP),
     *                                         IGS_GAMNUM(TRABUF(TGAMTYP),
     *                                                    TRABUF(TGAMIND))))
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


        RETURN
       END
