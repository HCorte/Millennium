C
C SUBROUTINE GETB_BTMBRD
C
C SUBROUTINE TO GET BINGO BOARDS FROM BITMAPS
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE GETB_BTMBRD(TRABUF,BBOARDS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'

        INTEGER*4 BBOARDS( BGONBB,BGOROW,BGOCOL)
        INTEGER*4 BBITMAPS( BGONBB,BGOCOL)
        INTEGER*4 I,J,K, ROW
        INTEGER*4 I4BITS(BGONCL)
        DATA      I4BITS/Z00000001,Z00000002,Z00000004,Z00000008,
     *                   Z00000010,Z00000020,Z00000040,Z00000080,
     *                   Z00000100,Z00000200,Z00000400,Z00000800,
     *                   Z00001000,Z00002000,Z00004000/

C
C create all bingo boards
C -------------------------
        DO I=1,BGONBB
            DO J=1,BGOCOL
               BBITMAPS(I,J)=TRABUF(TWBBFH1+(I-1)*5+(J-1))
               ROW=1     
               DO K=1,BGONCL
                  IF(IAND(BBITMAPS(I,J),I4BITS(K)).NE.0) THEN
                     BBOARDS(I,ROW,J)=(J-1)*BGONCL+K
                     ROW = ROW + 1
                  ENDIF
               ENDDO
            ENDDO
        ENDDO 
C
        RETURN
        END
