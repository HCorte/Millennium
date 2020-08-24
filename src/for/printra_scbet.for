C
C SUBROUTINE PRINTRA_SCBET
C 
C V04 01-FEB-2000 UXN TNFRAC added.
C V03 11-DEC-1994 JXP REALLIGNMENT FOR SCORE WAGERS
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 07-JAN-1993 TD  CHANGED SUBROUTINE CALL TO PRINTRA_SCBET FROM SCBET
C
C BUILD BET IMAGE FOR SCORE TRANSACTIONS
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
	SUBROUTINE PRINTRA_SCBET(TRABUF,BETS,LINES)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INTEGER*4 SCORE(2,TWSBMAX)
	INTEGER*4 I,OFF,HOME,AWAY,LINES
        CHARACTER*80 BETS(14)
	INTEGER*4 AMT
C
C
	DO 10 I=1,TRABUF(TWNBET)
	OFF=I-1
	HOME=TRABUF(TWSSCR1+OFF*TWSBLEN)
	AWAY=TRABUF(TWSSCR2+OFF*TWSBLEN)
	CALL BINASC(SCORE(1,I),1,2,HOME)
	CALL BINASC(SCORE(2,I),1,2,AWAY)
	IF(HOME.EQ.'FF'X) SCORE(1,I)='--'
	IF(AWAY.EQ.'FF'X) SCORE(2,I)='--'
10	CONTINUE
C
C
	DO 100 I=1,TRABUF(TWNBET)
	OFF=I-1
	IF(I.NE.1.AND.TRABUF(TWSYST).NE.NOSYS) THEN
	  WRITE (BETS(I),900) SCORE(1,I),SCORE(2,I)
	ELSE
	  AMT = TRABUF(TWSAMT+OFF*TWSBLEN)
	  IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / TRABUF(TNFRAC)
	  WRITE (BETS(I),901) SCORE(1,I),SCORE(2,I),
     *	         CMONY(AMT,8,BETUNIT)
	ENDIF
100	CONTINUE
	LINES=TRABUF(TWNBET)+1
	RETURN
C
C
900	FORMAT('Home ',A2,2X,'Away ',A2)
901	FORMAT('Home ',A2,2X,'Away ',A2,' Amount ',A8)
	END
