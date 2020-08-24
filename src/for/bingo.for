C BINGO.FOR

C       Rev
C       1.0     1994.06.16  HHe     Initial version for GTECH use
C       1.1     1994.05.08  HHe     Algorithm made cover more combinations
C       1.2     1994.09.23  HHe     Algorithm made cover combinations
C                                   more evently. Reversability lost.
C       2.0     1995.06.16  HHe     Three panes that together have all
C                                   numbers (1..75) can be made from a seed
C       2.1     1997.05.13  HHe     Three panels, finalised

C	3.0	2000.06.08  OXK	    SOME TYPE CHANGES I*2<->I*4 TO MATCH
C				    CALLING PARAMS

C       *********************************

C=======OPTIONS /CHECK

C       --------------------------
C       Initialize the unit

        SUBROUTINE BINGO_INIT

        IMPLICIT NONE
        INCLUDE 'INCLIB:PRMBNG.DEF'
        INCLUDE 'INCLIB:BINGO.DEF'

        INTEGER*2   ADDR( BGOCOL )
        INTEGER*2   COL( BGOCOL )
        INTEGER*2   ROW( BGOROW, BGOCOL )

        INTEGER*4   SEED
        INTEGER*4   SWSEED, B_UNI_RND

	INTEGER*2   A			! I*2 FOR BINGO_COL_OF
        INTEGER*4   D, I, K, N, P1, P2, S, T



        DO N = 1, BGONCL
            DO D = 1, BGOCOL
                B_TAB( N, D ) = 0
            END DO
        END DO

        DO N = BGONCL - 1, 1, -1
            B_TAB( N, 5 ) = B_TAB( N+1, 5 ) + 1
            DO D = BGOCOL - 1, 1, -1
                B_TAB( N, D ) = B_TAB( N+1, D+1 ) + B_TAB( N+1, D )
            END DO
        END DO


        DO A = 0, B_COL_ADDRS
            DO D = 1, BGONCL
                B_NTAB( A, D ) = 0
            END DO
        END DO
        DO A = 0, B_COL_ADDRS - 1
            CALL BINGO_COL_OF( A, COL )
            DO N = 1, BGOCOL
                DO D = 1, BGONCL
                    IF ( COL( N ) .EQ. D ) B_NTAB( A, D ) = N
                END DO
            END DO
        END DO

        RETURN  



C       ---------------------------------------
C       Produce panel from column addresses


        ENTRY   BINGO_ADDR_ROW ( ADDR, ROW )

        DO I = 1, BGOROW
            CALL BINGO_COL_OF( ADDR( I ), COL )
            DO N = 1, BGOCOL
                ROW( I, N ) = COL( N ) + ( I - 1 ) * BGONCL
            END DO
        END DO

        RETURN


C       -------------------------------------
C       Convert seed to column addresses

        ENTRY BINGO_SEED_ADDR ( SEED, ADDR )

        S = SWSEED( SEED )
        T = 0
        P1 = 1
        P2 = 1
        DO I = 1, BGOCOL
            K = MOD( S, B_COL_ADDRS )
            N = B_UNI_RND( K, MOD( P1, B_PAR( I )), P2 )
            ADDR( I ) = N
            P1 = N
            IF ( I .EQ. 1 ) P2 = P1
            S = S / B_COL_ADDRS
            S = S * N
        END DO

        END

C       **********************************************


C=======OPTIONS /CHECK

C       --------------------------
C       Calculate column address

        INTEGER*2 FUNCTION BINGO_ADDR_OF ( COL )

        IMPLICIT NONE
        INCLUDE 'INCLIB:PRMBNG.DEF'
        INCLUDE 'INCLIB:BINGO.DEF'

        INTEGER*2   COL( BGOCOL )
        INTEGER*4   A, I

        A = 0
        DO I = 1, BGOCOL
            A = A + B_TAB( COL( I ), I )
        END DO
        BINGO_ADDR_OF = A

        END


C       **********************************************

C=======OPTIONS /CHECK

C       --------------------
C       Swap nibbles 2 and 7

        INTEGER*4 FUNCTION SWSEED ( SEED )

        IMPLICIT NONE
        INTEGER*4       SEED, S1, S2

        S2 = IAND( SEED, 'F0FFFF0F'X )
    
        S1 = IAND( SEED, '0F000000'X )
        S1 = ISHFTC( S1, 12, 32 )
        S2 = IOR( S2, S1 )               ! Nibble 7 => Nibble 2
    
        S1 = IAND( SEED, '000000F0'X )
        S1 = ISHFT( S1, 20 )
        S2 = IOR( S2, S1 )               ! Nibble 2 => Nibble 7

        SWSEED = S2
        END

C       **********************************************

C=======OPTIONS /CHECK

C       ----------------------------------
C       Produce column form column address

        SUBROUTINE B_COL_OF

        IMPLICIT    NONE
        INCLUDE     'INCLIB:PRMBNG.DEF'
        INCLUDE     'INCLIB:BINGO.DEF'

        INTEGER*2   ADDR
        INTEGER*2   COL( BGOCOL )
        INTEGER*4   A, I, N


C       ------------------------------------

        ENTRY BINGO_COL_OF ( ADDR, COL )

        A = ADDR
        N = 1
        DO I = 1, BGOCOL
            DO WHILE ( B_TAB( N, I ) .GT. A )
                N = N + 1
            END DO
            A = A - B_TAB( N, I )
            COL( I ) = N
            N = N + 1
        END DO

        RETURN

        END



C       *********************************************

C       Bingo coupon procedure
C       Produces three panels with column addesses from a seed

C=======OPTIONS /CHECK

        SUBROUTINE BINGO_COUPON ( SEED, ADDR, ROW )

        IMPLICIT    NONE
        INCLUDE     'INCLIB:PRMBNG.DEF'
        INCLUDE     'INCLIB:BINGO.DEF'

        INTEGER*4       SEED
        INTEGER*2       ADDR( BGONBB, BGOCOL )
        INTEGER*2       ROW( BGONBB, BGOROW, BGOCOL )

        INTEGER*2       COL_1( BGOCOL ), COL_2( BGOCOL )
        INTEGER*2       COL_3( BGOCOL )
        INTEGER*2       ADR( BGOCOL ), TAB( BGOCOL )

        INTEGER*4       A, C, N, N1, N2, P, U
	INTEGER*2	U2		! I*2 FOR BINGO_COL_OF

        INTEGER*2       BINGO_ADDR_OF
        INTEGER*4       B_UNI_RND


        CALL BINGO_SEED_ADDR( SEED, ADR )
        DO C = 1, BGOCOL
            ADDR( 1, C ) = ADR( C )
        END DO

        DO C = 1, BGOCOL

C           set parameters for randomiser 
            IF ( C .EQ. 1 ) THEN
                N1 = MOD( ADDR( 1, BGOCOL ), B_PAR( c ))
            ELSE
                N1 = MOD( ADDR( 1, C - 1 ), B_PAR( C ))
            ENDIF

            IF ( C .EQ. BGOCOL ) THEN
                N2 = ADDR( 1, 1 )
            ELSE
                N2 = ADDR( 1, C + 1 )
            ENDIF

C           randomise
            A = ADDR( 1, C )
            U = B_UNI_RND( A, N1, N2 )

C           create 5 "tabs" between 1 .. 15
	    U2 = U
            CALL BINGO_COL_OF( U2, TAB )
	    U = U2

C           set start offset to numbers table
            IF ( C .EQ. 1 ) THEN
                P = MOD(ADDR(1,BGOCOL),BGONCL)+1
            ELSE
                P = MOD( ADDR( 1, C-1 ), BGONCL ) + 1
            ENDIF

C           produce second and third row columns
            CALL OTHER_COLS( ADDR( 1, C ), P, TAB, COL_2, COL_3 )

C           make 1st column
            CALL BINGO_COL_OF( ADDR( 1, C ), COL_1 )

C           set column addresses for new panels
            ADDR( 2, C ) = BINGO_ADDR_OF( COL_2 )
            ADDR( 3, C ) = BINGO_ADDR_OF( COL_3 )

C           modify numbers to correct range
            DO N = 1, BGOCOL
                ROW(1,C,N) = COL_1(N) + (C-1) * BGONCL
                ROW(2,C,N) = COL_2(N) + (C-1) * BGONCL
                ROW(3,C,N) = COL_3(N) + (C-1) * BGONCL
            END DO
        END DO

        END

C       ----------------------------------------

C=======OPTIONS /CHECK

        SUBROUTINE OTHER_COLS ( ADDR, POS, TAB, COL_2, COL_3 )   


        IMPLICIT    NONE
        INCLUDE     'INCLIB:PRMBNG.DEF'
        INCLUDE     'INCLIB:BINGO.DEF'

        INTEGER*2       ADDR
	INTEGER*4	POS
        INTEGER*2       TAB( BGOCOL )
        INTEGER*2       COL_2( BGOCOL )
        INTEGER*2       COL_3( BGOCOL )

        INTEGER*4       STAB( BGONCL )
        INTEGER*4       I, K, P, NUM


        DO I = 1, BGONCL
            STAB( I ) = 0
        END DO

        DO NUM = 1, BGOCOL

C           Tabulate 'number' times (1..15)
            IF (( POS + TAB( NUM )) .LE. BGONCL ) THEN
                POS = POS + TAB( NUM )
            ELSE
                POS = POS + TAB( NUM ) - BGONCL
            ENDIF

C           Find an empty place
            DO WHILE (B_NTAB(ADDR,POS).GT.0 .OR. STAB(POS).GT.0 )
                IF ( POS .LT. BGONCL ) THEN
                    POS = POS + 1
                ELSE
                    POS = 1
                ENDIF
            END DO

C           mark second row column number
            STAB( POS ) = NUM
        END DO

C       second row column, pick number marked
        P = 1
        DO K = 1, BGONCL
            IF ( STAB( K ) .GT. 0 ) THEN
                COL_2( P ) = K
                P = P + 1
            ENDIF
        END DO

C       third row column, find free numbers
        P = 1
        DO K = 1, BGONCL
            IF ( B_NTAB( ADDR, K ) .EQ. 0 .AND. STAB( K ) .EQ. 0 ) THEN
                COL_3( P ) = K
                P = P + 1
            ENDIF
        END DO
    
        END



C       *********************************************

C       Randomizer functions


        FUNCTION    B_UNIRAN

        IMPLICIT NONE
        INCLUDE 'INCLIB:PRMBNG.DEF'
        INCLUDE 'INCLIB:BINGO.DEF'

C       -----------------
C       Forward random

        INTEGER*4   B_UNIRAN, B_UNI_RND, B_UNI_INV
        INTEGER*4   NUM, N, P1, P2

        ENTRY   B_UNI_RND ( NUM, P1, P2 )

        N = NUM
        CALL RND64( N, P1, P2, B_COL_ADDRS - 1, 4 )
        B_UNI_RND = N

        RETURN

C       ---------------------------------
C       Reverse random

        ENTRY   B_UNI_INV ( NUM, P1, P2 )

        N = NUM
        CALL INV64( N, P1, P2, B_COL_ADDRS - 1, 4 )
        B_UNI_INV = N

        RETURN

        END

