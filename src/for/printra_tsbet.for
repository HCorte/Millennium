C
C SUBROUTINE PRINTRA_TSBET
C
C V06 01-FEB-2000 UXN TNFRAC added.
C V05 05-AUG-1993 GXA Adjusted 1X2 again.
C V04 05-AUG-1993 GXA Changed 1X2 to 12X.
C V03 26-JUL-1993 SXH Changed 123 to 1X2
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 07-JAN-1993 TD  CHANGED SUBROUTINE CALL TO PRINTRA_TSBET
C
C
C BUILD BET IMAGE FOR TOTO SELECT TRANSACTIONS
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE PRINTRA_TSBET(TRABUF,BETS,LINES)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INTEGER*4 STAT(0:3)
	CHARACTER*56 BETS(14)
	CHARACTER*3 POOL(0:3)
	INTEGER*4 LINES,I,LIN
	DATA POOL/'   ','1--','--2','-X-'/
	DATA STAT/'    ','won ','can ','lost'/
	INTEGER*4 FCNT, AMT
C
	FCNT = TRABUF(TNFRAC)
C
	LIN=1
	AMT = TRABUF(TWTAMT1)
	IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / FCNT
	WRITE (BETS(LIN),900) CMONY(AMT,8,BETUNIT)
	DO 10 I=0,TRABUF(TWTSEL1)-1,2
	WRITE (BETS(LIN),901) TRABUF(TWTROW1+I*TWTBLEN),
     *	                          POOL(TRABUF(TWTPOL1+I*TWTBLEN)),
     *	                          STAT(TRABUF(TWTSTS1+I*TWTBLEN)),
     *	                          TRABUF(TWTROW1+(I+1)*TWTBLEN),
     *	                          POOL(TRABUF(TWTPOL1+(I+1)*TWTBLEN)),
     *	                          STAT(TRABUF(TWTSTS1+(I+1)*TWTBLEN))
	LIN=LIN+1
10	CONTINUE
	IF(TRABUF(TWNBET).EQ.1) GOTO 100
C
C
	LIN=5
	AMT = TRABUF(TWTAMT2)
	IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / FCNT
	WRITE (BETS(LIN),900) CMONY(AMT,8,BETUNIT)
	DO 20 I=0,TRABUF(TWTSEL2)-1,2
	WRITE (BETS(LIN),901) TRABUF(TWTROW2+I*TWTBLEN),
     *	                          POOL(TRABUF(TWTPOL2+I*TWTBLEN)),
     *	                          STAT(TRABUF(TWTSTS2+I*TWTBLEN)),
     *	                          TRABUF(TWTROW2+(I+1)*TWTBLEN),
     *	                          POOL(TRABUF(TWTPOL2+(I+1)*TWTBLEN)),
     *	                          STAT(TRABUF(TWTSTS2+(I+1)*TWTBLEN))
	LIN=LIN+1
20	CONTINUE
	IF(TRABUF(TWNBET).EQ.2) GOTO 100
C
C
	LIN=9
	AMT = TRABUF(TWTAMT3)
	IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / FCNT
	WRITE (BETS(LIN),900) CMONY(AMT,8,BETUNIT)
	DO 30 I=0,TRABUF(TWTSEL3)-1,2
	WRITE (BETS(LIN),901) TRABUF(TWTROW3+I*TWTBLEN),
     *	                        POOL(TRABUF(TWTPOL3+I*TWTBLEN)),
     *	                        STAT(TRABUF(TWTSTS3+I*TWTBLEN)),
     *	                        TRABUF(TWTROW3+(I+1)*TWTBLEN),
     *	                        POOL(TRABUF(TWTPOL3+(I+1)*TWTBLEN)),
     *	                        STAT(TRABUF(TWTSTS3+(I+1)*TWTBLEN))
	LIN=LIN+1
30	CONTINUE
C
C
100	CONTINUE
	LINES=LIN+1
	RETURN
C
C
900	FORMAT(44X,A6)
901	FORMAT(3(I2.0,1X,A3,1X,A4,3X))
	END
