C
C SUBROUTINE TO DISPALY RESULTS OF BINGO SCAN
C
C V03 02-MAR-2000 RXK Bounds for scan increased.
C V02 01-FEB-2000 RXK Changed for new Bingo rules
C V01 11-JAN-1999 GPW STOPSYS OPTIMIZATION
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
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
         SUBROUTINE BSCANRES(BMSCAN,BWSCAN,BW2SCAN,WRKPHS,WRKWST,
     *                       WRKWS2,WRKNDF,GIND)
         IMPLICIT NONE
C
         INCLUDE 'INCLIB:SYSPARAM.DEF'
         INCLUDE 'INCLIB:SYSEXTRN.DEF'
         INCLUDE 'INCLIB:GLOBAL.DEF'
         INCLUDE 'INCLIB:WINCOM.DEF'


         INTEGER*4   BMSCAN(BGOPHS,BGODIV,BGONBR)!# of wins won on ball N
         INTEGER*4   BWSCAN(BGOPHS,BGONBR)       !# worst match on ball N
         INTEGER*4   BW2SCAN(BGOPHS,BGONBR)      !# second worst match on ball N
         INTEGER*4   WRKPHS(BGOSPH)              !#s for subphases
         INTEGER*4   WRKWST                      !worst match
         INTEGER*4   WRKWS2                      !second worst match
         INTEGER*4   WRKNDF(BGODIV)              !#s drawn for first hit 
         INTEGER*4   GIND

         INTEGER*4   NUMNDF(BGODIV,BGONBR)
         INTEGER*4   MINK
         INTEGER*4   I,K,J,EXT
         INTEGER*4   PHASE1,PHASE2,PHS,DISPEND
         CHARACTER*7 DISPLAY(10)
         CHARACTER*7 SPACE/'       '/
         CHARACTER*32 STRING
         LOGICAL     WRITTEN
         LOGICAL     DISPNDF(BGODIV)


         PHASE1=50
         PHASE2=55
C
C PHASE1. GET NUMBERS FOR FIRST HITS 
C
         PHS = 1
         CALL FASTSET(BGONBR,NUMNDF,BGODIV*BGONBR)
         DO I=1,BGODIV,2
            DO K=5,PHASE1
               IF(BMSCAN(PHS,I,K).NE.0) THEN
                  DO J=K,PHASE1
                     NUMNDF(I,J) = K
                  ENDDO
                 GOTO 10
              ENDIF
           ENDDO
10         CONTINUE
         ENDDO
C
C DISPLAY DISTRIBUTION OF WINS AND ASK TO ENTER SUBPHASE END BALLS
C
         TYPE 970, IAM(), (I,I=1,10)
         WRITTEN=.FALSE. 
         DO K=5,PHASE1
            DO I=1,10,2
               IF(K.EQ.NUMNDF(I,K)) THEN
                  WRITE(DISPLAY(I),980)
     *               BMSCAN(PHS,I,NUMNDF(I,K))
                  WRITTEN=.TRUE.
               ELSEIF(K.GT.NUMNDF(I,K)) THEN
                  WRITE(DISPLAY(I+1),980)
     *                BMSCAN(PHS,I,K) - BMSCAN(PHS,I,NUMNDF(I,K))
                  WRITTEN=.TRUE.
               ELSE
                  WRITE(DISPLAY(I),981) SPACE
                  WRITE(DISPLAY(I+1),981) SPACE
               ENDIF
            ENDDO
            IF(WRITTEN) TYPE 990,K,(DISPLAY(I),I=1,10)
         ENDDO

         TYPE*
         DO I=1,LBNNSP(GIND)-1
            WRITE(STRING,910) I
            CALL PRMNUM(STRING,WRKPHS(I),1,BGONBR,EXT)
            IF(EXT.NE.0) CALL GSTOP(GEXIT_FATAL)
         ENDDO
         PHASE1 = WRKPHS(LBNNSP(GIND)-1)
C
C SAVE FINAL RESULTS OF PHASE1
C
         DO K=5,PHASE1
            DO I=1,10,2
               WRKNDF(I) = NUMNDF(I,PHASE1)
               IF(LBNFST(I,BGOFHS,GIND).GT.0) THEN
                  IF(K.EQ.WRKNDF(I)) THEN
                     BMSCAN(PHS,I,K)   = BMSCAN(PHS,I,WRKNDF(I))    !first
                     BMSCAN(PHS,I+1,K) = 0                          !other
                  ELSEIF(K.GT.WRKNDF(I)) THEN
                     BMSCAN(PHS,I+1,K) = BMSCAN(PHS,I,K) -          !other
     *                                   BMSCAN(PHS,I,WRKNDF(I))
                     BMSCAN(PHS,I,K)   = 0                          !first
                  ENDIF
               ELSE
                  BMSCAN(PHS,I+1,K) = BMSCAN(PHS,I,K)               !other
                  BMSCAN(PHS,I,K)   = 0                             !first
               ENDIF
            ENDDO
         ENDDO
C
C PHASE2. IF NUMBER FOR PHASE2 END HAS NOT BEEN SET THEN DISPLAY NUMBERS OF
C WINS, CORRESPOND WORST AND SECOND WORST MATCH  AND ASK TO ENTER END FOR 
C PHASE2 
C
         PHS = 2

         MINK=PHASE2
         DO K=40,PHASE2
            DO I=11,15
               IF(BMSCAN(PHS,I,K).NE.0.AND.K.LT.MINK) MINK=K
            ENDDO
         ENDDO

         TYPE 971, IAM(), (I,I=11,15)
         DO K=MINK,PHASE2
            DO I=1,10
               WRITE(DISPLAY(I),981) SPACE
            ENDDO

            DO I=11,15
               IF(BMSCAN(PHS,I,K).NE.0) 
     *            WRITE(DISPLAY(I-10),980) BMSCAN(PHS,I,K) 
               WRITE(DISPLAY(6),980) BWSCAN(PHS,K)
               IF(BWSCAN(PHS,K).NE.99) 
     *            WRITE(DISPLAY(6),980) BWSCAN(PHS,K)
               IF(BW2SCAN(PHS,K).NE.99) 
     *            WRITE(DISPLAY(7),980) BW2SCAN(PHS,K)
            ENDDO

            TYPE 990,K,(DISPLAY(I),I=1,9)
         ENDDO

         TYPE*
         CALL PRMNUM('Enter number to end phase 2',   
     *        WRKPHS(LBNNSP(GIND)),1,BGONBR,EXT)
         IF(EXT.NE.0) CALL GSTOP(GEXIT_FATAL)
   

         WRKWST = BWSCAN(PHS,WRKPHS(LBNNSP(GIND)))
         WRKWS2 = BW2SCAN(PHS,WRKPHS(LBNNSP(GIND)))

         RETURN
 
910      FORMAT('Enter number to end subphase',I3)
970      FORMAT(/1X,A,10X,'Phase1. Results of scan'
     *          /1X,75('-')/
     *          3X,10('  Div',I2.2)/3X,5('  first  other')/1X,75('-'))
980      FORMAT(I7)
981      FORMAT(A7)
990      FORMAT(1X,I2.2,10A7)
971      FORMAT(/1X,A,10X,'Phase2. Results of scan'/1X,75('-')/
     *          3X,5('  Div',I2.2),'  Worst Worst2'/1X,75('-'))
        END
