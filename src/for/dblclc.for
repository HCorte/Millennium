C SUBROUTINE DBL_ODDCLC
C 
C V12 30-JUN-2000 UXN Renamed to DBLCLC
C V11 17-MAY-2000 OXK ACTUAL_WINS initialized to 0
C V10 09-AUG-1999 UXN Fix for changing odds for tied winners.
C V09 15-JUL-1999 UXN DEBUG statements taken out.
C V08 18-MAY-1999 UXN DDBCMB added.
C V07 04-FEB-1999 UXN Fix for big odds.
C V06 23-FEB-1996 HXK Fix for calculating total sales amount; number of ties
C V05 09-FEB-1996 HXK Bug fix
C V04 21-JAN-1996 HXK Various fixes for Double / Couple for system bets and 
C                     ties
C V03 12-JAN-1996 HXK Major fix
C V02 09-JAN-1996 HXK Fix for TIES2 equal to zero
C V01 23-NOV-1995 PXB Initial revision.
C  
C SUBROUTINE TO CALCULATE SUPER DOUBLE ODDS
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

	SUBROUTINE DBLCLC(GNUM)

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
	INTEGER*4 IND
	INTEGER*4 IND1
	INTEGER*4 IND2
	INTEGER*4 ODDS
	INTEGER*4 K
	INTEGER*4 FLAG
	INTEGER*4 EXT
        INTEGER*4 ACTUAL_WINS
	INTEGER*4 CDC,TIME

	REAL*8 TOTAL(MAXDBLTI)
	REAL*8 RODDS_ALL(MAXDBLTI)
	REAL*8 RODDS
	REAL*8 TOTPOL
	REAL*8 WINS

C--------------------------- Start of code -------------------------------
	
	CALL RC_PROMPT(CDC,TIME,EXT)
	IF(EXT.EQ.1) CALL RESPOL_TDBL(GNUM,CDC,TIME)

1	CONTINUE

	TOTSAL = 0
	ACTUAL_WINS=0
 
        DO INDEX = 1,MAXDBLRW*MAXDBLRW
           IF(.NOT.(DBROWSTS((INDEX-1)/MAXDBLRW+1,1).EQ.GAMCAN .OR.
     *              DBROWSTS((INDEX-1)/MAXDBLRW+1,1).EQ.GAMREF .OR.
     *              DBROWSTS(MOD(INDEX-1,MAXDBLRW)+1,1).EQ.
     *                                                       GAMCAN .OR.
     *              DBROWSTS(MOD(INDEX-1,MAXDBLRW)+1,1).EQ.
     *                                                       GAMREF)) THEN
              TOTSAL = TOTSAL + DDBODT(INDEX)
           ENDIF
        ENDDO
 
	TOTPOL = DFLOAT(TOTSAL) * CALPER(DDBSPR)
	TOTPOL = TOTPOL + DFLOAT(DDBPOL(1)) + DFLOAT(DDBBRK(1))
	DDBTPL = IDINT(TOTPOL)

	DO 20 I = 1,DDBCMB
	   RODDS_ALL(I) = 0.0D0
	   TOTAL(I) = TOTPOL * DFLOAT(DYN_BETUNIT)
20	CONTINUE

	DO I=1,DDBCMB
	   RODDS = 0.0D0
           IND1 = DDBWIN(1,I)
	   IND2 = DDBWIN(2,I)
           IND  = MAXDBLRW*(IND1-1)+IND2
	   WINS = DFLOAT(DDBODT(IND)*DYN_BETUNIT)

	   IF(WINS.NE.0.0D0) THEN
              RODDS = TOTAL(I) / WINS
              ACTUAL_WINS = ACTUAL_WINS + 1
           ELSE
              RODDS = 1.0D0
           ENDIF
	   RODDS_ALL(I) = RODDS
	   ODDS = IDNINT(RODDS*100.0D0)
	   DDBODS(I) = ODDS
	   IF(DDBODS(I).LT.100) DDBODS(I) = 100
	   WRITE(6,901) IAM(),I, IND1, (DDBNMS(K,IND1),K=1,3),
     *                           IND2, (DDBNMS(K,IND2),K=1,3),
     *                        DDBODS(I)/100,MOD(DDBODS(I),100)
	   CALL INPYESNO('Do you want to change these odds (Y/N)',FLAG)
	   IF(FLAG.EQ.1) THEN
50	      CONTINUE
	      CALL INPNUM('Enter new odds [100-999999] :',DDBODS(I),
     *	                        100,999999,EXT)
	      WRITE(6,902) IAM(),DDBODS(I)/100,MOD(DDBODS(I),100)
              CALL INPYESNO('Are the new odds entered correct (Y/N)',FLAG)
	      IF(FLAG.NE.1) GOTO 50
	      RODDS_ALL(I) = DFLOAT(DDBODS(I))/100.0D0
	   ENDIF
	ENDDO

        IF(DDBCMB.GT.1 .AND. ACTUAL_WINS.GT.0) THEN
           TYPE*,IAM(),'Final Adjusted odds because of ties '
           TYPE*,IAM(),'=================================== '
           TYPE*,IAM(),'Number of possible winning ties:',DDBCMB
           TYPE*,IAM(),'Number of actual winning ties:  ',ACTUAL_WINS
           DO J = 1,ACTUAL_WINS
              DDBODS(J) = IDNINT(RODDS_ALL(J)*100.0D0/DFLOAT(ACTUAL_WINS))
              IF(DDBODS(J).LT.100) DDBODS(J)=100
              WRITE(6,903) IAM(),J,DDBODS(J)/100,MOD(DDBODS(J),100)
           ENDDO
           CALL INPYESNO('Are the adjusted odds correct (Y/N)',FLAG)
           IF(FLAG.NE.1) GOTO 1
        ENDIF

	RETURN

C------------------------ Format statements ---------------------------

900	FORMAT(1X,A,'Tie -> ',I2)
901     FORMAT(1X,A,'Winners: ',I2,/,
     *         T20,'First:   ',I2,2X,3A4,/,
     *         T20,'Second:  ',I2,2X,3A4,/,
     *         T20,'Payout is ',I8,'.',I2.2,' to 1')
902	FORMAT(1X,A,'Payout entered is ',I8,'.',I2.2,' to 1')
903     FORMAT(1X,A,'Tie:',I2,' odds adjusted to ',I8,'.',I2.2,' to 1')

	END
