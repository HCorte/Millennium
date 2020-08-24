C
C $Log:   GXAFXT:[GOLS]WITSORT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 16:02:26   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   13 Dec 1993 16:16:38   SXH
C  Initial revision.
C SUBROUTINE WITSORT
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE  WITSORT(INDEX,ODDS)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'

        ! arguments
        INTEGER*4  INDEX(MAXWRW)           !
        INTEGER*4  ODDS(MAXWRW)            !

        ! variables
        INTEGER*4  LEAST                   !
        INTEGER*4  TEMP                    !
        INTEGER*4  I                       !
        INTEGER*4  J                       !
        INTEGER*4  I1                      !


        DO I = 1, MAXWRW -1
            I1 = I
            LEAST = ODDS(INDEX(I))
            DO J = I+1,MAXWRW
                IF (ODDS(INDEX(J)).LT. LEAST) THEN
                    I1 = J
                    LEAST = ODDS(INDEX(J))
                END IF
            END DO

            IF (I1.NE.I) THEN
                TEMP      = INDEX(I)
                INDEX(I)  = INDEX(I1)
                INDEX(I1) = TEMP
            END IF
        END DO

        RETURN

        END

