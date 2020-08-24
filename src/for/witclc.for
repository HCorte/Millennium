C
C SUBROUTINE WITCLC
C
C V03 30-JUN-2000 UXN Refund too late played tickets.
C V02 09-AUG-1999 UXN DEBUG statements taken out.
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C
C SUBROUTINE TO CALCULATE WINNERS TIP ODDS
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE WITCLC(GNUM,TIES)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
C
	INTEGER*4 GNUM, TIES
C
	INTEGER*4 TOTSAL, I, IND, ODDS, K, FLAG, EXT
C
	INTEGER*4 CDC,TIME
C
	REAL*8 TOTAL(4),RODDS,TOTPOL,WINS
C
	CALL RC_PROMPT(CDC,TIME,EXT)
	IF(EXT.EQ.1) CALL RESPOL_TWIT(GNUM,CDC,TIME)
C
	TOTSAL = 0
	DO 5 I = 1,MAXWRW
	IF(WROWSTS(I,1).EQ.GAMCAN.OR.WROWSTS(I,1).EQ.GAMREF) GOTO 5
	TOTSAL = TOTSAL + DWISBR(I)
5	CONTINUE
C
C
	TOTPOL = DFLOAT(TOTSAL) * CALPER(DWISPR)
	TOTPOL = TOTPOL + DFLOAT(DWIPOL(1)) + DFLOAT(DWIBRK(1))
	DWITPL = IDINT(TOTPOL)
	DO 10 I = 1,4
	   TOTAL(I) = 0.0D0
10	CONTINUE
C
C
	DO 20 I = 1,TIES
	   TOTAL(I) = TOTPOL * DFLOAT(DYN_BETUNIT) / DFLOAT(TIES)
20	CONTINUE
C
C
	DO 30 I = 1,TIES
	   RODDS = 0.0D0
	   IND = DWIWIN(I)
	   WINS = DFLOAT(DWISBR(IND)*DYN_BETUNIT)
	   IF(WINS.NE.0.0D0) RODDS = TOTAL(I) / WINS
	   ODDS = INT(RODDS*1000.0D0)+5
	   ODDS = ODDS/10
	   DWIODS(I) = ODDS
	   IF(DWIODS(I).LT.100) DWIODS(I) = 100
30	CONTINUE
C
C
	DO 100 I = 1,TIES
	   IND = DWIWIN(I)
	   WRITE(6,901) IAM(),IND,(DWINMS(K,IND),K=1,3),DWIODS(I)/100,
     *	                MOD(DWIODS(I),100)
	   CALL INPYESNO('Do you want to change these odds (Y/N)',FLAG)
	   IF(FLAG.EQ.1) THEN
50	      CONTINUE
	      CALL INPNUM('Enter new odds [100-999999] :',DWIODS(I),
     *	                  100,999999,EXT)
	      WRITE(6,902) IAM(),DWIODS(I)/100,MOD(DWIODS(I),100)
	      CALL INPYESNO('Are the new odds entered correct (Y/N)',FLAG)
	      IF(FLAG.NE.1) GOTO 50
	   ENDIF
100	CONTINUE
	RETURN
C
C
901	FORMAT(1X,A,'Row ', I2,'.', 1X,3A4,' payout is ',I8,'.',I2.2,' to 1')
902	FORMAT(1X,A,1X,' Payout entered is ',I8,'.',I2.2,' to 1')
	END
