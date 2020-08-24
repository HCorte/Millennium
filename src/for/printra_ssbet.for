C
C BUILD BET IMAGE FOR SUPERSCORE TRANSACTIONS
C
C V01 10-JAN-98 RXK Initial release.
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE PRINTRA_SSBET(TRABUF,BETS,LINES)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'

	INTEGER*4 SCORES(20,2,3)
	INTEGER*4 I,K,OFF,LINES,MAXNO
	INTEGER*4 NOTHING/'----'/
        CHARACTER*80 BETS(14)

C
C
           CALL FASTSET(NOTHING,SCORES,120)
           MAXNO=JMAX0(1,TRABUF(TWSSHM1),TRABUF(TWSSHM2),TRABUF(TWSSHM3),
     *                 TRABUF(TWSSAW1),TRABUF(TWSSAW2),TRABUF(TWSSAW3))

           ! first set
           OFF=-1
           DO I=1,TRABUF(TWSSHM1)
	      CALL BINASC(SCORES(I,1,1),1,2,TRABUF(TWSSBET+OFF+I))
           ENDDO
           OFF=OFF+TRABUF(TWSSHM1)
           DO I=1,TRABUF(TWSSAW1)
	      CALL BINASC(SCORES(I,2,1),1,2,TRABUF(TWSSBET+OFF+I))
           ENDDO

           ! second set
           OFF=OFF+TRABUF(TWSSAW1)
           DO I=1,TRABUF(TWSSHM2)
	      CALL BINASC(SCORES(I,1,2),1,2,TRABUF(TWSSBET+OFF+I))
           ENDDO
           OFF=OFF+TRABUF(TWSSHM2)
           DO I=1,TRABUF(TWSSAW2)
	      CALL BINASC(SCORES(I,2,2),1,2,TRABUF(TWSSBET+OFF+I))
           ENDDO

           ! third set
           OFF=OFF+TRABUF(TWSSAW2)
           DO I=1,TRABUF(TWSSHM3)
	      CALL BINASC(SCORES(I,1,3),1,2,TRABUF(TWSSBET+OFF+I))
           ENDDO
           OFF=OFF+TRABUF(TWSSHM3)
           DO I=1,TRABUF(TWSSAW3)
	      CALL BINASC(SCORES(I,2,3),1,2,TRABUF(TWSSBET+OFF+I))
           ENDDO

           DO I=1,MAXNO
              WRITE(BETS(I),900) (SCORES(I,K,1),K=1,2),
     *          (SCORES(I,K,2),K=1,2),(SCORES(I,K,3),K=1,2)
           ENDDO

 	   LINES=MAXNO+1
	   RETURN

C
900     FORMAT(3('Home ',A2,' Away 'A2,4X))

	END

