C
C V02 03-OCT-97 WXM KICKER NUMBER "DE-RANDOMIZED" TO GET KICKER SEED (FOR ALL
C		    WAGERS OF ALL GAMES WITH KICKER ACTIVE)
C V01 25-JUN-1993 GXA Initial revision.
C
C     KIKREP(TRABUF)      - KICKER REPROCESSING
C     IN - TRABUF         - TRANSACTION BODY
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
        SUBROUTINE KIKREP(TRABUF)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
C
	INTEGER*4 KGIND				!Kicker Game Index.
	INTEGER*4 KGNUM				!Kicker Game Number.
	INTEGER*4 PAR2				!V02 PARAMETER FOR INV64
	INTEGER*4 OCTDIG			!V02 PARAMETER FOR INV64
	INTEGER*4 LIMIT				!V02 PARAMETER FOR INV64
C
C GET AND CHECK KICKER GAME NUMBER
C
	KGNUM = KGNTAB(TRABUF(TGAM))
	IF(KGNUM.LT.1.OR.KGNUM.GT.MAXGAM) RETURN
	KGIND = GNTTAB(GAMIDX,KGNUM)
	OCTDIG=KIKOCT(KGIND)			!V02
	LIMIT =KIKMAX(KGIND)			!V02
C
C UPDATE KICKER SEED
C NOTE!!!!!
C		FINLAND WILL ALLWAYS HAVE TWO KICKER NUMBERS ASSIGNED.
C		NO NEED TO UPDATE KICKER 1, SINCE KICKER 2 WILL OVERWRITE.
C		IF THERE WOULD BE AN OPTION TO GENERATE ONLY ONE KICKER#
C		WE WOULD NOW KNOW WHEN TO UPDATE THE SEED DUE TO THE FACT
C		THAT KICKER # ZERO (O) IS A VALID NUMBER. 
C
	KIKSED(1,KGIND) = TRABUF(TWKICK2)
	PAR2=MOD(TRABUF(TWKBEG),64)				!V02
	CALL INV64(KIKSED(1,KGIND),PAR2,1,LIMIT,OCTDIG)		!V02
C
	RETURN
	END
