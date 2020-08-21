C
C V01 03-MAR-2000 UXN Separated from MULSUBS. Code cleaned up.
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
C=======OPTIONS /CHECK=NOOVERFLOW /EXTEND
        SUBROUTINE CHKSTS(STS, GNUM, DRAW, CDC)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'

        INTEGER*4 STS, GNUM, DRAW, CDC
        INTEGER*4 I,K
 
        IF(STS.NE.GAMBFD.AND.STS.NE.GAMENV.AND.STS.NE.GAMCAN.AND.
     *     CDC.NE.DAYCDC) RETURN

        DO I=1,MAX_WINSEL
           IF ((DRWGAM(I,GNUM).EQ.DRAW).OR.(DRWGAM(I,GNUM).EQ.0)) THEN
              IF ((STS.LE.GAMBFD).AND.(CDC.LE.DAYCDC)) DRWSTS(I,GNUM)=RESNOT
              IF ((STS.EQ.GAMENV).AND.(CDC.EQ.DAYCDC)) DRWSTS(I,GNUM)=WINYES
              IF ((STS.EQ.GAMENV).AND.(CDC.LT.DAYCDC)) DRWSTS(I,GNUM)=WINYES!PRV
              IF ((STS.EQ.GAMCAN).AND.(CDC.EQ.DAYCDC)) DRWSTS(I,GNUM)=WINYES
              IF ((STS.EQ.GAMCAN).AND.(CDC.LT.DAYCDC)) DRWSTS(I,GNUM)=WINYES!PRV
              IF (DRWSTS(I,GNUM).NE.WINNOT) DRWGAM(I,GNUM)=DRAW
              RETURN
           ENDIF
        ENDDO
C
        WRITE(6,9000) IAM(),(GLNAMES(K,GNUM), K=1,4), DRAW
C
9000    FORMAT(1X,A,'WARNING: ',4A4,' draw ',I4,' not added to MULTIWIN!')
        END
