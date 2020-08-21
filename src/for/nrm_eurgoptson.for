C
C SUBROUTINE EURGOPTSON
C
C V01 15-MAR-2016 SCML CREATION (M16 PROJECT)
C
C SUBROUTINE TO ADD GAME OPTION DATA TO SON MESSAGES OF GAMES OF
C EUROMILLIONS SYSTEM.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2016 SCML Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE EURGOPTSON(MESTAB, MESIND, EGNUM)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:EURCOM.DEF'
C
        BYTE      MESTAB(*)                                                     !TERMINAL MESSAGE TABLE
        INTEGER*4 MESIND                                                        !INDEX INTO MESSAGE TABLE
        INTEGER*4 EGNUM                                                         !GAME NUMBER IN EUROMILLIONS SYSTEM
C
        INTEGER*4 GTYP                                                          !GAME TYPE TO PROCESS
        INTEGER*4 GIND                                                          !GAME INDEX TO PROCESS
C
        INTEGER*4 I
C
        INTEGER*2 BETLIM_OPT /Z0040/                                            !MAX BET LIMIT PRESENT OPTION FLAG
        INTEGER*2 MAXFRA_OPT /Z0020/                                            !MAX FRACTION PRESENT OPTION FLAG
        INTEGER*2 MDRBIT_OPT /Z0010/                                            !MULTI-DRAW BITMAP PRESENT OPTION FLAG
        INTEGER*2 TKTCHA_OPT /Z0008/                                            !TICKET CHARGE PRESENT OPTION FLAG
        INTEGER*2 CTRREV_OPT /Z0004/                                            !CONTROL REVISION PRESENT OPTION FLAG
        INTEGER*2 TXTREV_OPT /Z0002/                                            !TEXT REVISION PRESENT OPTION FLAG
        INTEGER*2 TTXREV_OPT /Z0001/                                            !TICKET TEXT REVISION PRESENT OPTION FLAG
C
C TEMP. WORK VARIABLES
C
        BYTE      I1TEMP(4)
        INTEGER*2 I2TEMP(2)
        INTEGER*4 I4TEMP
        EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        IF(EGNUM.LT.1 .OR. EGNUM.GT.EMAXGAM) RETURN
        GTYP = EGNTTAB(GAMTYP, EGNUM)
        GIND = EGNTTAB(GAMIDX, EGNUM)
        IF(GTYP.LT.1 .OR. GTYP.GT.MAXTYP) RETURN
C
        IF(GTYP.EQ.TEUM) THEN                                                   !EUM GAME TYPE
          IF(GIND.GE.1 .AND. GIND.LE.ENUMEUM) THEN
            I4TEMP = EUMGOPTSON(GIND)                                           !SON GAME OPTION FLAGS
            MESTAB(MESIND+0) = I1TEMP(2)
            MESTAB(MESIND+1) = I1TEMP(1)
            MESIND = MESIND + 2
C
            IF(IAND(EUMGOPTSON(GIND), BETLIM_OPT).NE.0) THEN                    !MAX BET LIMIT PRESENT
              I4TEMP = EUMMAXBETS(GIND)
              MESTAB(MESIND+0) = I1TEMP(2)
              MESTAB(MESIND+1) = I1TEMP(1)
              MESIND = MESIND + 2
            ENDIF
C
            IF(IAND(EUMGOPTSON(GIND), MAXFRA_OPT).NE.0) THEN                    !MAX NUMBER OF FRACTIONS PRESENT
              I4TEMP = EUMMAXFRAC(GIND)
              MESTAB(MESIND+0) = I1TEMP(1)
              MESIND = MESIND + 1
            ENDIF
C
            IF(IAND(EUMGOPTSON(GIND), MDRBIT_OPT).NE.0) THEN                    !MULTI-DRAW BITMAP PRESENT
               DO I = 1,EMAXDBM
                 I4TEMP = EUMDBITMAP(I,GIND)
                 MESTAB(MESIND+0) = I1TEMP(1)
                 MESIND = MESIND+1
               ENDDO
            ENDIF
C
            IF(IAND(EUMGOPTSON(GIND), TKTCHA_OPT).NE.0) THEN                    !TICKET CHARGE PRESENT
              I4TEMP = EUMTKTCHAR(GIND)
              MESTAB(MESIND+0) = I1TEMP(2)
              MESTAB(MESIND+1) = I1TEMP(1)
              MESIND = MESIND + 2
            ENDIF
C
            IF(IAND(EUMGOPTSON(GIND), CTRREV_OPT).NE.0) THEN                    !CONTROL REVISION PRESENT
              I4TEMP = EUMCTRLREV(GIND)
              MESTAB(MESIND+0) = I1TEMP(2)
              MESTAB(MESIND+1) = I1TEMP(1)
              MESIND = MESIND + 2
            ENDIF
C
            IF(IAND(EUMGOPTSON(GIND), TXTREV_OPT).NE.0) THEN                    !TEXT REVISION PRESENT
              I4TEMP = EUMTEXTREV(GIND)
              MESTAB(MESIND+0) = I1TEMP(2)
              MESTAB(MESIND+1) = I1TEMP(1)
              MESIND = MESIND + 2
            ENDIF
C
            IF(IAND(EUMGOPTSON(GIND), TTXREV_OPT).NE.0) THEN                    !TICKET TEXT REVISION PRESENT
              I4TEMP = EUMTTXTREV(GIND)
              MESTAB(MESIND+0) = I1TEMP(2)
              MESTAB(MESIND+1) = I1TEMP(1)
              MESIND = MESIND + 2
            ENDIF
          ENDIF
        ELSEIF(GTYP.EQ.TRAF) THEN                                               !RAF GAME TYPE
          IF(GIND.GE.1 .AND. GIND.LE.ENUMRAF) THEN
            I4TEMP = RAFGOPTSON(GIND)                                           !SON GAME OPTION FLAGS
            MESTAB(MESIND+0) = I1TEMP(2)
            MESTAB(MESIND+1) = I1TEMP(1)
            MESIND = MESIND + 2
C
            IF(IAND(RAFGOPTSON(GIND), BETLIM_OPT).NE.0) THEN                    !MAX BET LIMIT PRESENT
              I4TEMP = RAFMAXBETS(GIND)
              MESTAB(MESIND+0) = I1TEMP(2)
              MESTAB(MESIND+1) = I1TEMP(1)
              MESIND = MESIND + 2
            ENDIF
C
            IF(IAND(RAFGOPTSON(GIND), MAXFRA_OPT).NE.0) THEN                    !MAX NUMBER OF FRACTIONS PRESENT
              I4TEMP = RAFMAXFRAC(GIND)
              MESTAB(MESIND+0) = I1TEMP(1)
              MESIND = MESIND + 1
            ENDIF
C
            IF(IAND(RAFGOPTSON(GIND), MDRBIT_OPT).NE.0) THEN                    !MULTI-DRAW BITMAP PRESENT
               DO I = 1,EMAXDBM
                 I4TEMP = RAFDBITMAP(I,GIND)
                 MESTAB(MESIND+0) = I1TEMP(1)
                 MESIND = MESIND+1
               ENDDO
            ENDIF
C
            IF(IAND(RAFGOPTSON(GIND), TKTCHA_OPT).NE.0) THEN                    !TICKET CHARGE PRESENT
              I4TEMP = RAFTKTCHAR(GIND)
              MESTAB(MESIND+0) = I1TEMP(2)
              MESTAB(MESIND+1) = I1TEMP(1)
              MESIND = MESIND + 2
            ENDIF
C
            IF(IAND(RAFGOPTSON(GIND), CTRREV_OPT).NE.0) THEN                    !CONTROL REVISION PRESENT
              I4TEMP = RAFCTRLREV(GIND)
              MESTAB(MESIND+0) = I1TEMP(2)
              MESTAB(MESIND+1) = I1TEMP(1)
              MESIND = MESIND + 2
            ENDIF
C
            IF(IAND(RAFGOPTSON(GIND), TXTREV_OPT).NE.0) THEN                    !TEXT REVISION PRESENT
              I4TEMP = RAFTEXTREV(GIND)
              MESTAB(MESIND+0) = I1TEMP(2)
              MESTAB(MESIND+1) = I1TEMP(1)
              MESIND = MESIND + 2
            ENDIF
C
            IF(IAND(RAFGOPTSON(GIND), TTXREV_OPT).NE.0) THEN                    !TICKET TEXT REVISION PRESENT
              I4TEMP = RAFTTXTREV(GIND)
              MESTAB(MESIND+0) = I1TEMP(2)
              MESTAB(MESIND+1) = I1TEMP(1)
              MESIND = MESIND + 2
            ENDIF
          ENDIF
        ENDIF
        RETURN
C
        END
C
C END NRM_EURGOPTSON.FOR
C
