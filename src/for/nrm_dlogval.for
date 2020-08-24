C
C SUBROUTINE DLOGVAL
C
C V09 05-FEB-2001 EPH VOP ADDED
C V08 29-NOV-2000 UXN TTGL ADDED.
C V07 17-OCT-2000 UXN Alpha baseline release.
C V06 29-JUL-1998 RXK VSUB for KICKER added.
C V05 24-NOV-1997 UXN VSUB for LOTTO added.
C
C SUBROUTINE TO CONVERT DETAIL PRIZE DATA
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
	SUBROUTINE DLOGVAL(VALREC,VDETAIL)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VDETAIL.DEF'
C
	INTEGER*4 DRWOFF, I, PRZCNT, TEMP, IND, TMPDIV
	BYTE	  I1TEMP(4)
	EQUIVALENCE (TEMP,I1TEMP)
C
C
	PRZCNT = VALREC(VPZOFF)
	IF(PRZCNT.GT.VMAX) PRZCNT = VMAX
	CALL FASTSET(0,VDETAIL,VPLEN*VMAX)
C
	IND = 1
	DO 100 I = 1,PRZCNT
	   CALL MOVBYT(VALREC(VPDATA),IND,VDETAIL(VSHR,I),1,4)
	   CALL MOVBYT(VALREC(VPDATA),IND+4,TEMP,1,3)

	   DRWOFF = ZEXT(I1TEMP(1))

	   TMPDIV = ZEXT(I1TEMP(3))
	   VDETAIL(VDIV,I) = IAND(TMPDIV,15)
           IF (IAND(TMPDIV,16).NE.0) VDETAIL(VOP,I) = 1     !V09

	   TEMP = ZEXT(I1TEMP(2))
           IF(VALREC(VGTYP).EQ.TBNG.OR.VALREC(VGTYP).EQ.TLTO.OR.
     *        VALREC(VGTYP).EQ.TSPT.OR.VALREC(VGTYP).EQ.TKIK.OR.
     *        VALREC(VGTYP).EQ.TTGL) 
     *        VDETAIL(VSUB,I) = IAND(TEMP,'0003'X)  !2 bits for Bingo subgame
	   IF(IAND(TEMP,  4).NE.0) VDETAIL(VPRG,I) = 1
	   IF(IAND(TEMP,  8).NE.0) VDETAIL(VKI2,I) = 1
	   IF(IAND(TEMP, 16).NE.0) VDETAIL(VREF,I) = 1
	   IF(IAND(TEMP, 32).NE.0) VDETAIL(VBDR,I) = 1
	   IF(IAND(TEMP, 64).NE.0) VDETAIL(VUPD,I) = 1
	   IF(IAND(TEMP,128).NE.0) VDETAIL(VKIK,I) = 1
	   IF(VDETAIL(VKIK,I).EQ.0.AND.VDETAIL(VKI2,I).EQ.0) THEN
	     VDETAIL(VDRW,I) = VALREC(VEXP)-DRWOFF+1
	   ELSE
	     VDETAIL(VDRW,I) = VALREC(VKEXP)-DRWOFF+1
	   ENDIF
	   IND = IND+7
100	CONTINUE
C
	RETURN
	END
