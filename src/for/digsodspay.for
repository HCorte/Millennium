CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C File      : DIGSODSPAY.FOR
C Change Log:
C
C Ver Date       Author  Comment
C --- ---------- ------- ----------------------------------------------
C V02 2014.10.02 SCML    PLACARD PROJECT - Correction of NIB registration
C                        in TRABUF
C V01 2014.02.24 SCML    PLACARD PROJECT - Created - Decodes terminal 
C                        payment input messages for IGS;
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
        SUBROUTINE DIGSODSPAY(TERMES,TRABUF)
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
C----+------------------------------------------------------------------
C V02| PLACARD PROJECT - Correction of NIB registration in TRABUF
C----+------------------------------------------------------------------
        INTEGER*8 I8TEMP
        INTEGER*4 I4TEMP(2)
        BYTE      I1TEMP(8)
        EQUIVALENCE (I8TEMP,I1TEMP)
        EQUIVALENCE (I8TEMP,I4TEMP)
C----+------------------------------------------------------------------
C V02| PLACARD PROJECT - Correction of NIB registration in TRABUF
C----+------------------------------------------------------------------


C       INITIALIZE SOME FIELDS TO -1 (UNDEFINED)
        TRABUF(TIGSP_PMOD) = IGS_PMUND
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSP_PMOD',TRABUF(TIGSP_PMOD),TRABUF(TIGSP_PMOD))
        ENDIF
C DEBUG - END */


C
C IGS VALIDATION TRANSACTION BODY
C
C       GAME INDEX (1 - Placard)                                    (12)
        TRABUF(TGAMIND) = ZEXT(TERMES(7))
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TGAMIND',TRABUF(TGAMIND),TRABUF(TGAMIND))
        ENDIF
C DEBUG - END */

C       VALIDATE GAME INDEX
        IF(TRABUF(TGAMIND) .EQ. 1) THEN !PLACARD

C          BET REFERENCE DATE YEAR                                  (32)
           TRABUF(TIGSP_WRDY) = ZEXT(TERMES(20))
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSP_WRDY',TRABUF(TIGSP_WRDY),TRABUF(TIGSP_WRDY))
        ENDIF
C DEBUG - END */

C          BET REFERENCE DATE MONTH                                 (33)
           TRABUF(TIGSP_WRDM) = ZEXT(TERMES(21))
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSP_WRDM',TRABUF(TIGSP_WRDM),TRABUF(TIGSP_WRDM))
        ENDIF
C DEBUG - END */

C          BET REFERENCE DATE DAY                                   (34)
           TRABUF(TIGSP_WRDD) = ZEXT(TERMES(22))
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSP_WRDD',TRABUF(TIGSP_WRDD),TRABUF(TIGSP_WRDD))
        ENDIF
C DEBUG - END */

C          BET REFERENCE GAME                                       (35)
           TRABUF(TIGSP_WRGM) = ZEXT(TERMES(23))
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSP_WRGM',TRABUF(TIGSP_WRGM),TRABUF(TIGSP_WRGM))
        ENDIF
C DEBUG - END */

C          BET REFERENCE SERIAL NUMBER (HIGH ONE BYTE)              (37)
           TRABUF(TIGSP_WRSH) = ZEXT(TERMES(24))
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSP_WRSH',TRABUF(TIGSP_WRSH),TRABUF(TIGSP_WRSH))
        ENDIF
C DEBUG - END */

C          BET REFERENCE SERIAL NUMBER (LOW FOUR BYTES)             (36)
           CALL TERM_TO_HOST(TERMES(25), TRABUF(TIGSP_WRSL), 4)
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSP_WRSL',TRABUF(TIGSP_WRSL),TRABUF(TIGSP_WRSL))
        ENDIF
C DEBUG - END */

C          BET REFERENCE CHECK DIGITS                               (38)
           CALL TERM_TO_HOST(TERMES(29), TRABUF(TIGSP_WRCD), 2)
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSP_WRCD',TRABUF(TIGSP_WRCD),TRABUF(TIGSP_WRCD))
        ENDIF
C DEBUG - END */

C          PAYMENT MODE                                             (39)
           TRABUF(TIGSP_PMOD) = ZEXT(TERMES(31))
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSP_PMOD',TRABUF(TIGSP_PMOD),TRABUF(TIGSP_PMOD))
        ENDIF
C DEBUG - END */

C IF BANK TRANSFER
           IF(TRABUF(TIGSP_PMOD) .EQ. IGS_PMBNK) THEN
C             PLAYER ID TYPE(*)                                     (40)
              TRABUF(TIGSP_IDTY) = ZEXT(TERMES(32))
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSP_IDTY',TRABUF(TIGSP_IDTY),TRABUF(TIGSP_IDTY))
        ENDIF
C DEBUG - END */

C             PLAYER ID(*)                                          (41)
              CALL TERM_TO_HOST(TERMES(33), TRABUF(TIGSP_PYID), 4)
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSP_PYID',TRABUF(TIGSP_PYID),TRABUF(TIGSP_PYID))
        ENDIF
C DEBUG - END */

C             NIB BRANCH(*)                                         (42)
              CALL TERM_TO_HOST(TERMES(37), TRABUF(TIGSP_NIBB), 2)
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSP_NIBB',TRABUF(TIGSP_NIBB),TRABUF(TIGSP_NIBB))
        ENDIF
C DEBUG - END */

C             NIB OFFICE(*)                                         (43)
              CALL TERM_TO_HOST(TERMES(39), TRABUF(TIGSP_NIBO), 2)
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSP_NIBO',TRABUF(TIGSP_NIBO),TRABUF(TIGSP_NIBO))
        ENDIF
C DEBUG - END */

C----+------------------------------------------------------------------
C V02| PLACARD PROJECT - Correction of NIB registration in TRABUF
C----+------------------------------------------------------------------
        I8TEMP = KZEXT(0)
        I1TEMP(5) = ZEXT(TERMES(41))
        I1TEMP(4) = ZEXT(TERMES(42))
        I1TEMP(3) = ZEXT(TERMES(43))
        I1TEMP(2) = ZEXT(TERMES(44))
        I1TEMP(1) = ZEXT(TERMES(45))
        TRABUF(TIGSP_NIA1) = I8TEMP/100
        TRABUF(TIGSP_NIA2) = MOD(I8TEMP,100)

C             NIB ACCOUNT NUMBER PART 1(*)                          (44)
C             CALL TERM_TO_HOST(TERMES(41), TRABUF(TIGSP_NIA1), 4)
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSP_NIA1',TRABUF(TIGSP_NIA1),TRABUF(TIGSP_NIA1))
        ENDIF
C DEBUG - END */

C             NIB ACCOUNT NUMBER PART 2(*)                          (45)
C             TRABUF(TIGSP_NIA2) = ZEXT(TERMES(45))
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSP_NIA2',TRABUF(TIGSP_NIA2),TRABUF(TIGSP_NIA2))
        ENDIF
C DEBUG - END */

C----+------------------------------------------------------------------
C V02| PLACARD PROJECT - Correction of NIB registration in TRABUF
C----+------------------------------------------------------------------


C             NIB CHECK DIGITS(*)                                   (46)
              TRABUF(TIGSP_NICD) = ZEXT(TERMES(46))
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('TIGSP_NICD',TRABUF(TIGSP_NICD),TRABUF(TIGSP_NICD))
        ENDIF
C DEBUG - END */

C IF NOT CASH PAYMENT
           ELSEIF(TRABUF(TIGSP_PMOD) .NE. IGS_PMCSH) THEN
              TRABUF(TSTAT) = REJT
              TRABUF(TERR)  = SYNT
              SYNTERRCOD=6
C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('INVALID PAYMENT MODE TYPE(TIGSP_PMOD):',TRABUF(TIGSP_PMOD),TRABUF(TIGSP_PMOD))
        ENDIF
C DEBUG - END */
           ENDIF

C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('P(SUPVAL): ',P(SUPVAL),P(SUPVAL))
           TYPE*,'P(SUPVAL): ',P(SUPVAL)
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
           CALL OPS('P(IGSPVAL): ',P(IGSPVAL),P(IGSPVAL))
           TYPE*,'P(IGSPVAL): ',P(IGSPVAL)
        ENDIF
C DEBUG - END */

C DEBUG - START /*
        IF(IGSDEBUG(IA_INIGS)) THEN
           CALL OPS('P(IGSPGVAL): ',P(IGSPGVAL),P(IGSPGVAL))
           TYPE*,'P(IGSPGVAL): ',P(IGSPGVAL)
        ENDIF
C DEBUG - END */

C
C IF FUNCTION SUPRESSED THEN SET ERROR MESSAGE
C
           IF((P(SUPVAL) .NE. 0) .OR. P(IGSPPLA) .NE. 0
     *                           .OR. P(IGSPVAL) .NE. 0
     *                           .OR. IGSGAMFLG(P(IGSPGVAL),
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


        RETURN
       END
