C
C SUBROUTINE GETB_SEDBTM
C SUBROUTINE TO GET BINGO BITMAPS FROM SEED
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
        SUBROUTINE GETB_SEDBTM(SEED,BBITMAPS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:BINGO.DEF'

        INTEGER*2 ADDR(BGOCOL)
        INTEGER*2 BBITMAPS(BGONBB,BGOCOL)
        INTEGER*2 TABS(BGOCOL), TESTMAP(BGOCOL)

        INTEGER*4 I,J,K
        INTEGER*4 SEED,EXT, POSITION
        INTEGER*4 A,U,N1,N2,C,N,P,B_UNI_RND 

        INTEGER*2 I2BITS(BGONCL)
        DATA      I2BITS/Z0001,Z0002,Z0004,Z0008,
     *                   Z0010,Z0020,Z0040,Z0080,
     *                   Z0100,Z0200,Z0400,Z0800,
     *                   Z1000,Z2000,Z4000/

C
C column addresses for 1st board
c --------------------------------
        call BINGO_SEED_ADDR(SEED,ADDR)
C
C bitmaps for 1st board
c ----------------------
        DO J=1,BGOCOL
           BBITMAPS(1,J)=0 
           DO K=1,BGONCL
               IF(B_NTAB(ADDR(J),K).NE.0)
     *            BBITMAPS(1,J)=IEOR(BBITMAPS(1,J),I2BITS(K))
           ENDDO
        ENDDO

C bitmaps for 2nd board
c ---------------------
        DO C = 1, BGOCOL
C set parameters for randomiser  and start offset to numbers table (P)
C ---------------------------------------------------------------------
           IF(C.EQ.1) THEN
              N1=MOD(ADDR(BGOCOL),B_PAR(C))
              N2=ADDR(C+1)
              P=MOD(ADDR(BGOCOL),BGONCL)+1
           ELSEIF (C.EQ.BGOCOL) THEN
              N1=MOD(ADDR(C-1),B_PAR(C))
              N2=ADDR(1)
              P=MOD(ADDR(C-1),BGONCL)+1
           ELSE
              N1=MOD(ADDR(C-1),B_PAR(C))
              N2=ADDR(C+1) 
              P=MOD(ADDR(C-1),BGONCL)+1
           ENDIF
C randomise
C ----------
           A=ADDR(C)
           U=B_UNI_RND(A,N1,N2 )
C create 5 "tabs" between 1 .. bgoncl
C -----------------------------------------------
           N=1
           DO I=1,BGOCOL
              DO WHILE(B_TAB(N,I).GT.U)
                 N=N+1
              ENDDO
              U=U-B_TAB(N,I)
              TABS(I)=N
              N=N+1
           END DO
C
           BBITMAPS(2,C)=0 
           TESTMAP(C)=BBITMAPS(1,C)
           POSITION=P
           DO K=1,BGOCOL
              POSITION=POSITION+TABS(K)
50            CONTINUE
              IF(POSITION.GT.BGONCL) 
     *           POSITION=POSITION-BGONCL
              IF(IAND(TESTMAP(C),I2BITS(POSITION)).NE.0) THEN 
                 POSITION=POSITION+1
                 GOTO 50   
              ELSE
                 BBITMAPS(2,C)=IEOR(BBITMAPS(2,C),I2BITS(POSITION))
                 TESTMAP(C)=IEOR(TESTMAP(C),I2BITS(POSITION))
              ENDIF 
           ENDDO      ! end of column
        ENDDO         ! end of second board
C
C bitmaps for 3rd board
c -----------------------
        DO C = 1, BGOCOL
           BBITMAPS(3,C)=0
           DO K=1,BGONCL
              IF(IAND(TESTMAP(C),I2BITS(K)).EQ.0) 
     *           BBITMAPS(3,C)=IEOR(BBITMAPS(3,C),I2BITS(K))
           ENDDO
        ENDDO
C
        RETURN
C
        END
