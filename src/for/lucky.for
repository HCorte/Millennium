C LUCKY.FOR
C
C V02 08-jun-2000 L_UNI_RND defined as int*4
C v01 20-jun-1994 HHe Initial version
C
C       This module generates LUCKY NUMBER from BINGO index
C
C

C=======OPTIONS /CHECK=NOOVERFLOW/EXT

C       -------------------------------------
C       Initializes the module
C       CYCLE retuns the LYCKY NUMBER range
C
        SUBROUTINE LUCKY_INIT ( CSIZE )

        IMPLICIT    NONE
        INCLUDE     'INCLIB:SYSPARAM.DEF'
        INCLUDE     'INCLIB:SYSEXTRN.DEF'
        INCLUDE     'INCLIB:LUCKY.DEF'

        INTEGER*4   SEED, CYCLE, EVEN_PART, ODD_PART
        INTEGER*4   L_UNI_RND
	EXTERNAL    L_UNI_RND
	INTEGER*4   L_UNI_INV
	EXTERNAL    L_UNI_INV
        INTEGER*4   CSIZE, I, N, P, R

        P = 1
        DO I = 1, LUCKY_EVEN_DIGITS
            P = P * 10
        END DO
        R = P / 2
        P = 1
        DO I = 1, LUCKY_ODD_DIGITS
            P = P * 10
        END DO
        P = P / 2
        L_MAX_ODD = P
        CSIZE = R * P
        L_CYCLE_SIZE = CSIZE

        IF ( CSIZE .LT. 8 ) THEN
            L_ODIGITS = 1
        ELSE IF ( CSIZE .LT. 64 ) THEN
            L_ODIGITS = 2
        ELSE IF ( CSIZE .LT. 512 ) THEN
            L_ODIGITS = 3
        ELSE IF ( CSIZE .LT. 4096 ) THEN
            L_ODIGITS = 4
        ELSE IF ( CSIZE .LT. 32768 ) THEN
            L_ODIGITS = 5
        ELSE IF ( CSIZE .LT. 262144 ) THEN
            L_ODIGITS = 6
        ELSE IF ( CSIZE .LT. 2097152 ) THEN
            L_ODIGITS = 7
        ELSE
            L_ODIGITS = 8
        END IF

        RETURN


C       ---------------------------------
C       Counts even and odd part of lucky number form bingo index
C       Lucky number cycle numer is also returned
C
        ENTRY LUCKY_NUMBER ( SEED, CYCLE, EVEN_PART, ODD_PART )

        CYCLE = SEED / L_CYCLE_SIZE
        N     = MOD( SEED, L_CYCLE_SIZE )
        IF ( LUCKY_RANDOM ) N = L_UNI_RND( N )
        EVEN_PART = N / L_MAX_ODD
        EVEN_PART = 2 * EVEN_PART
        ODD_PART  = 2 * MOD( N, L_MAX_ODD ) + 1

        RETURN


C       -----------------------------------------
C       Counts bingo index from even and odd part of lucky number
C       and lucky cycle
C
        ENTRY LUCKY_SEED ( EVEN_PART, ODD_PART, CYCLE, SEED )

        N = L_MAX_ODD * EVEN_PART / 2 + ( ODD_PART - 1 ) / 2
        IF ( LUCKY_RANDOM ) N = L_UNI_INV( N )
        SEED = CYCLE * L_CYCLE_SIZE + N

        END


C       **************
C
C       Lycky number randomizer functions
C

        INTEGER*4 FUNCTION L_UNI_RND ( NUM )

        IMPLICIT  NONE        
	INCLUDE	  'INCLIB:SYSPARAM.DEF'
        INCLUDE	  'INCLIB:SYSEXTRN.DEF'
        INCLUDE	  'INCLIB:LUCKY.DEF'

        INTEGER*4 L_UNI_INV, NUM, N

        N = NUM
        CALL RND64( N, 1, 1, L_CYCLE_SIZE - 1, L_ODIGITS )
        L_UNI_RND = N

        RETURN

C       --------------------
C
        ENTRY   L_UNI_INV ( NUM )

        N = NUM
        CALL INV64( N, 1, 1, L_CYCLE_SIZE - 1, L_ODIGITS )
        L_UNI_INV = N

        END
