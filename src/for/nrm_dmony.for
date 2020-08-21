C
C FUNCTION DMONY
C
C V01 04-APR-2014 SCML Creation
C
C This function formats monetary values according to portuguese standards
C
C Calling sequence:
C
C     STRING = DMONY(IN_VALUE)
C
C Input parametrs:
C
C     IN_VALUE     Int*4       ! AMOUNT IN CENTS - i.e. 1000000000 EUR cents
C
C Output parameters:
C
C     DMONY        Char*32     ! AMOUNT IN CURRENCY FORMAT - i.e. 10.000.000,00
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2014 SCML. All rights reserved.
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        CHARACTER FUNCTION DMONY*32(IN_VALUE)
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'


        INTEGER*4 IN_VALUE
        CHARACTER*32 OUT_BUF
        CHARACTER*32 AUX_OUT

        INTEGER*4 I,J,DIG_CNT
        INTEGER*4 AUX
        INTEGER*4 REMAIN
        INTEGER*4 DIGIT
        CHARACTER*1 CH

        DO I = 1, 32
            AUX_OUT(I:I) = ' '        ! reset to SPACES
            OUT_BUF(I:I) = ' '        ! reset to SPACES
        ENDDO

        AUX = ABS(IN_VALUE)
        I = 1                        !!iterator counter
        J = 1                        !!struct position
        DIG_CNT = 1
        DO WHILE(AUX .NE. 0)
            REMAIN = AUX / 10
            DIGIT = MOD(AUX,10)
            CH = CHAR(48 + DIGIT)
C           SET ',' CHAR AT 3RD POSITION
            IF(I .EQ. 2) THEN
               DIG_CNT = 1
               AUX_OUT(J:J) = CH
               J = J + 1
               CH = ','
               AUX_OUT(J:J) = CH
               J = J + 1
C           SET '.' AT 7TH AND 11TH POSITION
            ELSEIF(MOD(DIG_CNT-1,3) .EQ. 0 .AND. DIG_CNT .NE. 1) THEN
               AUX_OUT(J:J) = '.'
               J = J + 1
               AUX_OUT(J:J) = CH
               DIG_CNT = DIG_CNT + 1
               J = J + 1
            ELSE
               AUX_OUT(J:J) = CH
               J = J + 1
               DIG_CNT = DIG_CNT + 1
            ENDIF
            I = I + 1
            AUX = REMAIN
        ENDDO

C       IF ONLY ONE DIGIT FORCE LEFT ZERO(S)
        IF(I .EQ. 1) THEN
           AUX_OUT(1:4) = '00,0'
        ENDIF

C       IF ONLY ONE DIGIT FORCE LEFT ZERO(S)
        IF(I .EQ. 2) THEN
           AUX_OUT(2:4) = '0,0'
        ENDIF
C       IF ONLY TWO DIGITS FORCE LEFT ZERO(S)
        IF(I .EQ. 3) THEN
           AUX_OUT(4:4) = '0'
        ENDIF

C       INVERT STRING
        J=13
        DO I = 1, J
            OUT_BUF(I:I) = AUX_OUT(J+1-I:J+1-I)
        ENDDO

C       RETURN OUT_BUF
        DMONY=OUT_BUF

        RETURN

        END

