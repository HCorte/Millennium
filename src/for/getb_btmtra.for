C
C SUBROUTINE GET_BBOARD
C
C SUBROUTINE TO GET BINGO BITMAPS FROM SEED
C
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE GETB_BTMTRA(TRABUF)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:LUCKY.DEF'

        INTEGER*4 EVEN, ODD, BASE, I, K
        INTEGER*2 BBITMAPS(BGONBB,BGOCOL)

        CALL LUCKY_NUMBER(TRABUF(TWBSED),BASE,EVEN,ODD)
        CALL GETB_SEDBTM(TRABUF(TWBSED),BBITMAPS)

        TRABUF(TWBBAS) = BASE
        TRABUF(TWBLUK) = (EVEN*1000)+ODD

        DO I=1,BGONBB
           DO K=1,BGOCOL
              TRABUF(TWBBFH1+(I-1)*5+(K-1)) = BBITMAPS(I,K)
           ENDDO
        ENDDO
            
        RETURN

        END
