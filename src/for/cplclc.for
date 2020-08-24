C SUBROUTINE CPLCLC
C
C V10 17-MAY-2000 OXK ACTUAL_WINS initialized to 0
C V09 09-AUG-1999 UXN Fix for changing odds for tied winners.
C V08 15-JUL-1999 UXN Debug statements removed.
C V07 18-MAY-1999 UXN DCPWIN changed.
C V06 04-FEB-1999 UXN Fix for big odds.
C V05 01-FEB-1996 HXK Fix for calculating sales when there are many cancelled 
C                     rows
C V04 21-JAN-1996 HXK Various fixes for Double / Couple for system bets and ties
C V03 12-JAN-1996 HXK Major fix for odds
C V02 28-NOV-1995 HXK Batch of changes for Double/Couple release
C V01 23-NOV-1995 PXB Initial revision.
C  
C SUBROUTINE TO CALCULATE SUPER DOUBLE ODDS
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, W.Greenwich, Rhode
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
	SUBROUTINE CPLCLC(GNUM)
	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'

	INTEGER*4 GNUM
C---- Local variables used.

	INTEGER*4 INDEX
	INTEGER*4 TOTSAL
	INTEGER*4 I
	INTEGER*4 J
	INTEGER*4 K
	INTEGER*4 IND
	INTEGER*4 IND1
	INTEGER*4 IND2
	INTEGER*4 ODDS
	INTEGER*4 FLAG
	INTEGER*4 EXT
	INTEGER*4 ACTUAL_WINS
	INTEGER*4 CDC,TIME

	REAL*8 TOTAL(MAXCPLTI)
	REAL*8 RODDS_ALL(MAXCPLTI)
	REAL*8 RODDS
	REAL*8 TOTPOL
	REAL*8 WINS

C--------------------------- Start of code -------------------------------
	
	CALL RC_PROMPT(CDC,TIME,EXT)
	IF(EXT.EQ.1) CALL RESPOL_TCPL(GNUM,CDC,TIME)

1       CONTINUE

	TOTSAL = 0
	ACTUAL_WINS=0

        DO INDEX = 1,(MAXCPLRW/2)*(MAXCPLRW/2)
           IF(.NOT.(CPROWSTS((INDEX-1)/(MAXCPLRW/2)+1,1).EQ.GAMCAN .OR. 
     *              CPROWSTS((INDEX-1)/(MAXCPLRW/2)+1,1).EQ.GAMREF .OR.
     *              CPROWSTS(MOD(INDEX-1,(MAXCPLRW/2))+MAXCPLRW/2+1,1).EQ.
     *                                                       GAMCAN .OR.
     *              CPROWSTS(MOD(INDEX-1,(MAXCPLRW/2))+MAXCPLRW/2+1,1).EQ.
     *                                                       GAMREF)) THEN
              TOTSAL = TOTSAL + DCPODT(INDEX)
           ENDIF
	ENDDO

 	TOTPOL = DFLOAT(TOTSAL) * CALPER(DCPSPR)
	TOTPOL = TOTPOL + DFLOAT(DCPPOL(1)) + DFLOAT(DCPBRK(1))
	DCPTPL = IDINT(TOTPOL)

	DO 10 I = 1,MAXCPLTI
	   TOTAL(I) = 0.0D0
	   RODDS_ALL(I) = 0.0D0
10	CONTINUE

	DO 20 I = 1,DCPCMB
	   TOTAL(I) = TOTPOL * DFLOAT(DYN_BETUNIT)
20	CONTINUE

        DO I=1,DCPCMB
           RODDS = 0.0D0
	   IND1 = DCPWIN(1,I)
	   IND2 = DCPWIN(2,I)
	   IND = (IND1-1)*(MAXCPLRW/2)+(IND2-MAXCPLRW/2)
           WINS = DFLOAT(DCPODT(IND)*DYN_BETUNIT)

           IF(WINS.NE.0.0D0) THEN
	      RODDS = TOTAL(I) / WINS
	      ACTUAL_WINS = ACTUAL_WINS + 1
	   ELSE
	      RODDS = 1.0D0
	   ENDIF
	   RODDS_ALL(I) = RODDS
           ODDS = IDNINT(RODDS*100.0D0)
           DCPODS(I) = ODDS
           IF(DCPODS(I).LT.100) DCPODS(I) = 100
           WRITE(6,901) IAM(),I, IND1, (DCPNMS(K,IND1),K=1,3),
     *                           IND2-MAXCPLRW/2, (DCPNMS(K,IND2),K=1,3),
     *                           DCPODS(I)/100,MOD(DCPODS(I),100)
           CALL INPYESNO('Do you want to change these odds (Y/N)', FLAG)
           IF(FLAG.EQ.1) THEN
50           CONTINUE
             CALL INPNUM('Enter new odds [100-999999] :',DCPODS(I),
     *                       100,999999,EXT)
             WRITE(6,902) IAM(),DCPODS(I)/100,MOD(DCPODS(I),100)
             CALL INPYESNO('Are the new odds entered correct (Y/N)',FLAG)
             IF(FLAG.NE.1) GOTO 50
             RODDS_ALL(I) = DFLOAT(DCPODS(I))/100.0D0
           ENDIF
        ENDDO

	IF(DCPCMB.GT.1 .AND. ACTUAL_WINS.GT.0) THEN
	   TYPE*,IAM(),'Final Adjusted odds because of ties '
	   TYPE*,IAM(),'=================================== '
	   TYPE*,IAM(),'Number of possible winning ties:',DCPCMB
	   TYPE*,IAM(),'Number of actual winning ties:  ',ACTUAL_WINS
	   DO J = 1,DCPCMB
              DCPODS(J) = IDNINT(RODDS_ALL(J)*100.0D0/DFLOAT(ACTUAL_WINS))
	      IF(DCPODS(J).LT.100) DCPODS(J)=100
	      WRITE(6,903) IAM(),J,DCPODS(J)/100,MOD(DCPODS(J),100)
           ENDDO
           CALL INPYESNO('Are the adjusted odds correct (Y/N)',FLAG)
           IF(FLAG.NE.1) GOTO 1
	ENDIF

	RETURN
C------------------------ Format statements ---------------------------
901     FORMAT(1X,A,'Winners: ',I2,/,
     *         T20,'Winner in first  event: ',I2,2X,3A4,/,
     *         T20,'Winner in second event: ',I2,2X,3A4,/,
     *         T20,'Payout is ',I8,'.',I2.2,' to 1')
902	FORMAT(1X,A,1X,' Payout entered is ',I8,'.',I2.2,' to 1')
903	FORMAT(1X,A,1X,'Tie:',I2,' odds adjusted to ',I8,'.',I2.2,' to 1')

	END
