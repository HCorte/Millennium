C
C SUBROUTINE SSBET
C
C V01 23-DEC-97 RXK INITIAL RELEASE.  
C
C SUBROUTINE TO BUILD BET IMAGE FOR SUPERSCORE TRANSACTION
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
	SUBROUTINE SSBET(TRABUF,BIMAGE)
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:SSCCOM.DEF'
C
        CHARACTER*56 BIMAGE(12)
	CHARACTER*2  SCORE(10,2,3)	    !Score mark table(..,home/away,set)
	INTEGER*4   OFF			    !Offset into trabuf
	INTEGER*4   I,K   		    !Loop variable
        INTEGER*4   MAXNO                   !Max no scores in system bet
        INTEGER*4   GIND
C
C
	   GIND=TRABUF(TGAMIND)  
	   IF(SSCEST(2,GIND).EQ.GAMOPN.AND.SSCEST(3,GIND).EQ.GAMOPN) THEN
              MAXNO=JMAX0(TRABUF(TWSSHM1),TRABUF(TWSSHM2),TRABUF(TWSSHM3),
     *		       TRABUF(TWSSAW1),TRABUF(TWSSAW2),TRABUF(TWSSAW3))
	   ELSEIF(SSCEST(2,GIND).EQ.GAMOPN.AND.SSCEST(3,GIND).NE.GAMOPN) THEN
              MAXNO=JMAX0(TRABUF(TWSSHM1),TRABUF(TWSSHM2),
     *		       TRABUF(TWSSAW1),TRABUF(TWSSAW2))
	   ELSEIF(SSCEST(2,GIND).NE.GAMOPN.AND.SSCEST(3,GIND).NE.GAMOPN) THEN
              MAXNO=JMAX0(TRABUF(TWSSHM1),TRABUF(TWSSAW1))
           ENDIF

           ! first set
           OFF=-1
	   DO I=1,TRABUF(TWSSHM1)
              WRITE(SCORE(I,1,1),905) TRABUF(TWSSBET+OFF+I)
              IF(TRABUF(TWSSBET+OFF+I).EQ.15) WRITE(SCORE(I,1,1),903) 'Mu'
           ENDDO
           DO I=TRABUF(TWSSHM1)+1,MAXNO
              WRITE(SCORE(I,1,1),903) '--'
           ENDDO 
           OFF=OFF+TRABUF(TWSSHM1)
	   DO I=1,TRABUF(TWSSAW1)
              WRITE(SCORE(I,2,1),905) TRABUF(TWSSBET+OFF+I)
              IF(TRABUF(TWSSBET+OFF+I).EQ.15) WRITE(SCORE(I,2,1),903) 'Mu'
           ENDDO
           DO I=TRABUF(TWSSAW1)+1,MAXNO
              WRITE(SCORE(I,2,1),903) '--'
           ENDDO 

           ! second set
           OFF=OFF+TRABUF(TWSSAW1)
	   DO I=1,TRABUF(TWSSHM2)
              WRITE(SCORE(I,1,2),905) TRABUF(TWSSBET+OFF+I)
              IF(TRABUF(TWSSBET+OFF+I).EQ.15) WRITE(SCORE(I,1,2),903) 'Mu'
           ENDDO
           DO I=TRABUF(TWSSHM2)+1,MAXNO
              WRITE(SCORE(I,1,2),903) '--'
           ENDDO 
           OFF=OFF+TRABUF(TWSSHM2)
	   DO I=1,TRABUF(TWSSAW2)
              WRITE(SCORE(I,2,2),905) TRABUF(TWSSBET+OFF+I)
              IF(TRABUF(TWSSBET+OFF+I).EQ.15) WRITE(SCORE(I,2,2),903) 'Mu'
           ENDDO
           DO I=TRABUF(TWSSAW2)+1,MAXNO
              WRITE(SCORE(I,2,2),903) '--'
           ENDDO 

           ! third set
           OFF=OFF+TRABUF(TWSSAW2)
	   DO I=1,TRABUF(TWSSHM3)
              WRITE(SCORE(I,1,3),905) TRABUF(TWSSBET+OFF+I)
              IF(TRABUF(TWSSBET+OFF+I).EQ.15) WRITE(SCORE(I,1,3),903) 'Mu'
           ENDDO
           DO I=TRABUF(TWSSHM3)+1,MAXNO
              WRITE(SCORE(I,1,3),903) '--'
           ENDDO 
           OFF=OFF+TRABUF(TWSSHM3)
	   DO I=1,TRABUF(TWSSAW3)
              WRITE(SCORE(I,2,3),905) TRABUF(TWSSBET+OFF+I)
              IF(TRABUF(TWSSBET+OFF+I).EQ.15) WRITE(SCORE(I,2,3),903) 'Mu'
           ENDDO
           DO I=TRABUF(TWSSAW3)+1,MAXNO
              WRITE(SCORE(I,2,3),903) '--'
           ENDDO 

           WRITE(BIMAGE(1),900)
           DO I=1,MAXNO 
              WRITE(BIMAGE(I+1),904) (SCORE(I,K,1),K=1,2),
     *          (SCORE(I,K,2),K=1,2),(SCORE(I,K,3),K=1,2)
           ENDDO

  	   RETURN
C
900     FORMAT(3X,' Set1 ',2X,' Set2 ',2X,' Set3')
901     FORMAT(3(3X,I2,1X,I2))
903     FORMAT(A2)
905     FORMAT(I2)
904     FORMAT(3(3X,A2,1X,A2))

	END
