C
C SUBROUTINE PRINTRA_WIBET
C
C V03 01-FEB-2000 UXN TNFRAC added. 
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 07-JAN-1993  TD CHANGED SUBROUTINE CALL TO PRINTRA_WIBET FROM WIBET
C
C BUILD BET IMAGE FOR WINNERS TIP TRANSACTIONS
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE PRINTRA_WIBET(TRABUF,BETS,LINES)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
        CHARACTER*56 BETS(14)
	INTEGER*4 LINES,I,OFF,AMT
C
C
	DO 100 I=1,TRABUF(TWNBET)
	OFF=I-1
	AMT = TRABUF(TWWAMT+OFF*TWWBLEN)
	IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / TRABUF(TNFRAC)
	WRITE (BETS(I),900) TRABUF(TWWROW+OFF*TWWBLEN),
     *	      CMONY(AMT,8,BETUNIT)
100	CONTINUE
	LINES=TRABUF(TWNBET)+1
	RETURN
C
C
900	FORMAT('Row ',I2,2X,' Amount ',A8)
	END
