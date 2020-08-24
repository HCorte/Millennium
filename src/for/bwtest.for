C BWTEST.FOR
C
C V02 15-JUN-2000 OXK CLEANUP W/ WARNINGS=ALL
C V01  4-NOV-1997 XXX INITIAL RELEASE
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM BWTEST
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:BINGO.DEF'
        INCLUDE 'INCLIB:LUCKY.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'

        INTEGER*4 I,K,J,L,C,D

        INTEGER*4 BBOARDS( BGONBB,BGOCOL,BGOCOL)
        INTEGER*4 WBOARDS( BGONBB,BGOCOL,BGOCOL)

        INTEGER*4 SEED,WIN


        CALL BINGO_INIT


ccc        DO  L=0,2147483647
        DO  L=65,65
            SEED = L
            TRABUF(TWBSED)=L
            TRABUF(TGAMTYP)=TBNG
            TRABUF(TTYP)=TWAG
            CALL GETB_BTMTRA(TRABUF)


            CALL GETB_BTMBRD(TRABUF,BBOARDS)

C            call BWIN_WIN
           
            CALL BPHWIN(TRABUF,V4BUF,WIN)


        if(win.gt.0) then
            CALL FASTSET(0,WBOARDS,
     *                BGONBB*BGOCOL*BGOCOL)
            type*,'seed=',seed
            TYPE*
        endif
        if(win.gt.0) then
            DO I=1,BGONBB                                  ! =3
                DO K=1,BGOCOL                                ! =5
                   TYPE 1111,(BBOARDS(I,k,j),J=1,BGOCOL)  ! =5
                ENDDO
                TYPE*
            ENDDO
            
            TYPE*,'I phase hit numbers'
            DO I=1,BGONBB                                  ! =3
                DO K=1,BGOCOL
                   DO C =1,BGOCOL
                      DO D=1,31
                         IF(BBOARDS(I,k,C).EQ.LBNWIN(D,1)) THEN
                            WBOARDS(I,K,C)=BBOARDS(I,k,C)
                            GOTO 100
                         ENDIF
                      ENDDO
100                   CONTINUE
                   ENDDO
                   TYPE 1111,(WBOARDS(I,k,j),J=1,BGOCOL)
                ENDDO
                TYPE*
            ENDDO
            TYPE*,'II phase hit numbers'
            DO I=1,BGONBB                                  ! =3
                DO K=1,BGOCOL
                   DO C =1,BGOCOL
                      DO D=1,52
                         IF(BBOARDS(I,k,C).EQ.LBNWIN(D,1)) THEN
                            WBOARDS(I,K,C)=BBOARDS(I,k,C)
                            GOTO 200
                         ENDIF
                      ENDDO
200                   CONTINUE
                   ENDDO
                   TYPE 1111,(WBOARDS(I,k,j),J=1,BGOCOL)
                ENDDO
                TYPE*
            ENDDO

        endif
             
        ENDDO

40      STOP

1111    FORMAT (1X, 5I4)
1112    FORMAT (1X, 'bitm ', 10 (Z2.2))
1113    FORMAT (1X, 'Word ', 3(1X,(Z8.8)))
1114    FORMAT (1X, 'i4 ', Z8.8)
1116    FORMAT (1X, i2,' trabuf ', Z8.8)
1115    FORMAT (1X, 'I2 ', 2(Z4.4))
        END
