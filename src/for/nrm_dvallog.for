C
C SUBROUTINE DVALLOG
C
C V10 05-FEB-2001 EPH VOP added
C V09 29-NOV-2000 UXN TTGL ADDED.
C V08 17-OCT-2000 UXN Alpha baseline release. 
C V07 29-JUL-1998 RXK VSUB added for KICKER 
C V06 24-NOV-1997 UXN VSUB added for LOTTO
C
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
	SUBROUTINE DVALLOG(VALREC,VDETAIL)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMVLF.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VDETAIL.DEF'
C
	INTEGER*4 DRWOFF, I, PRZCNT, IND
C
	INTEGER*4 TEMP
	BYTE	  I1TEMP(4)
	EQUIVALENCE (TEMP,I1TEMP)
C
C
	PRZCNT = VALREC(VPZOFF)
	IF(PRZCNT.GT.VMAX) PRZCNT = VMAX
	CALL FASTSET(0,VALREC(VPDATA),VFPLEN)
C
	IND=1
	DO 100 I = 1,PRZCNT
	   CALL MOVBYT(VDETAIL(VSHR,I),1,VALREC(VPDATA),IND,4)
           IF(VDETAIL(VKIK,I).EQ.0.AND.VDETAIL(VKI2,I).EQ.0) THEN
              DRWOFF = VALREC(VEXP) - VDETAIL(VDRW,I)+1
           ELSE
              DRWOFF = VALREC(VKEXP) - VDETAIL(VDRW,I)+1
           ENDIF
	   TEMP = 0

	   I1TEMP(1) = DRWOFF

           IF(VALREC(VGTYP).EQ.TBNG.OR.VALREC(VGTYP).EQ.TLTO.OR.
     *        VALREC(VGTYP).EQ.TSPT.OR.VALREC(VGTYP).EQ.TKIK.OR.
     *        VALREC(VGTYP).EQ.TTGL) 
     *        I1TEMP(2) = I1TEMP(2) + IAND(VDETAIL(VSUB,I),'00000003'X)
	   IF(VDETAIL(VPRG,I).NE.0) I1TEMP(2) = I1TEMP(2)+4
	   IF(VDETAIL(VKI2,I).NE.0) I1TEMP(2) = I1TEMP(2)+8
	   IF(VDETAIL(VREF,I).NE.0) I1TEMP(2) = I1TEMP(2)+16
	   IF(VDETAIL(VBDR,I).NE.0) I1TEMP(2) = I1TEMP(2)+32
	   IF(VDETAIL(VUPD,I).NE.0) I1TEMP(2) = I1TEMP(2)+64
	   IF(VDETAIL(VKIK,I).NE.0) I1TEMP(2) = I1TEMP(2)+128

	   I1TEMP(3) = IAND(VDETAIL(VDIV,I),15)
           IF (VDETAIL(VOP,I).NE.0) I1TEMP(3) = I1TEMP(3) + 16   !V10

	   CALL MOVBYT(TEMP,1,VALREC(VPDATA),IND+4,3)
 	   IND = IND+7
100	CONTINUE
C
	RETURN
	END
