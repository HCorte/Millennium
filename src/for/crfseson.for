C  GXSRC:CRFSESON.FOR
C
C V02 06-OCT-2000 UXN AlphaIPS release.
C V01 07-NOV-1997 DXA INITIAL RELEASE FOR UK NATIONAL LOTTERY
C
C SUBROUTINE TO BUILD CROSS SYSTEM FSE SIGN-ON MESSAGE
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXTEND_SOURCE
C
      SUBROUTINE CRFSESON(OUTTAB,TRABUF,BUF,SALPAS)
C
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:DESTRA.DEF'
C
      BYTE OUTTAB(*),
     +     I1TEMP(4)
C
      INTEGER*2 I2TEMP(2)
C
      INTEGER*4 I4TEMP,
     +          SALPAS,
     +          CROSS,
     +          IND,
     +          BUF
C
      EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
      IF (TRABUF(TIFSETYP).NE.0) THEN
C
         ! FSE SIGN-ON/OFF WITH NO PASSWORD CHECKING : DO NOTHING
C
      ELSE
C
C SET LENGTH
C
         IND=2
         I4TEMP = 22
         OUTTAB(IND+0) = I1TEMP(1)
         OUTTAB(IND+1) = I1TEMP(2)
         IND=IND+2
C
C SET GTECH BUFFER NUMBER
C
         I4TEMP = BUF
         OUTTAB(IND+0) = I1TEMP(1)
         OUTTAB(IND+1) = I1TEMP(2)
         IND=IND+2
C
C SET GTECH CROSS REFERENCE NUMBER
C
         CALL GETXRF(CROSS)
         TRABUF(TIXRF)=CROSS
         I4TEMP=CROSS
         OUTTAB(IND+0) = I1TEMP(1)
         OUTTAB(IND+1) = I1TEMP(2)
         OUTTAB(IND+2) = I1TEMP(3)
         OUTTAB(IND+3) = I1TEMP(4)
         IND=IND+4
C
C SET TRANSACTION TYPE CODE
C
         I4TEMP = 29
         OUTTAB(IND+0) = I1TEMP(1)
         OUTTAB(IND+1) = I1TEMP(2)
         IND=IND+2
C
C SET RETAILER NUMBER
C
         I4TEMP = TRABUF(TAGT)
         OUTTAB(IND+0) = I1TEMP(1)
         OUTTAB(IND+1) = I1TEMP(2)
         OUTTAB(IND+2) = I1TEMP(3)
         OUTTAB(IND+3) = I1TEMP(4)
         IND=IND+4
C
C SET FSE NUMBER
C
         I4TEMP = TRABUF(TIFSEREP)
         OUTTAB(IND+0) = I1TEMP(1)
         OUTTAB(IND+1) = I1TEMP(2)
         OUTTAB(IND+2) = I1TEMP(3)
         OUTTAB(IND+3) = I1TEMP(4)
         IND=IND+4
C
C SET FSE PASS NUMBER
C
         I4TEMP = SALPAS
         OUTTAB(IND+0) = I1TEMP(1)
         OUTTAB(IND+1) = I1TEMP(2)
         IND=IND+2
C
C SET REQUEST STATUS (NOT USED AT PRESENT)
C
         I4TEMP = 0
         OUTTAB(IND+0) = I1TEMP(1)
         OUTTAB(IND+1) = I1TEMP(2)
         IND=IND+2
C
      ENDIF
C
      RETURN
      END
