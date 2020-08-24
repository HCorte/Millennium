C  GXSRC:DCFSESON.FOR
C
C V02 06-OCT-2000 UXN AlphaIPS release.
C V01 11-NOV-1997 DXA INITIAL RELEASE FOR UK NATIONAL LOTTERY
C
C SUBROUTINE TO DECODE CROSS SYSTEM FSE SIGN-ON.
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
      SUBROUTINE DCFSESON(TRABUF,OUTTAB)
C
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:DESTRA.DEF'
C
      BYTE OUTTAB(*),
     +     I1TEMP(4)
C
      INTEGER*2 I2TEMP(2)
C
      INTEGER*4 CHKLEN,
     +          SPACES
C
      PARAMETER (CHKLEN = 39,
     +           SPACES = '20202020'X)
C
      INTEGER*4 LENGTH,
     +          I4TEMP
C
      EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
      IF (TRABUF(TIFSETYP).NE.0) THEN  ! FSE SIGN-ON/OFF WITHOUT PASSWORD CHECKING
C
         TRABUF(TIERR)     = INOER
         TRABUF(TIFSERSLT) = INOER
C
         TRABUF(TIFSEOFF)  = 0
C
         CALL FASTSET(SPACES,TRABUF(TIFSENAMS),TIFSELNAM)
C
      ELSE
C
C GET LENGTH
C
         I4TEMP=0
         I1TEMP(1)=OUTTAB(2)
         I1TEMP(2)=OUTTAB(3)
         LENGTH=I4TEMP
C
C CHECK ERROR CODE
C
         I4TEMP=0
         I1TEMP(1)=OUTTAB(39)
         I1TEMP(2)=OUTTAB(40)
         TRABUF(TIFSERSLT) = I4TEMP
C
         IF (TRABUF(TIFSERSLT).EQ.92) THEN
            TRABUF(TIERR)=INOER
         ELSE
            TRABUF(TIERR)=TRABUF(TIFSERSLT)
         ENDIF
C
         IF (TRABUF(TIERR).NE.INOER) THEN
            ! DO NOTHING
         ELSEIF (LENGTH.NE.CHKLEN) THEN
           TRABUF(TIERR)=INLTH
         ELSE
C
C GET OFFICE NUMBER
C
            I4TEMP=0
            I1TEMP(1)=OUTTAB(12)
            I1TEMP(2)=OUTTAB(13)
            TRABUF(TIFSEOFF)=I4TEMP
C
C GET FSE NAME
C
            TRABUF(TIFSENAME) = '20202020'X
            CALL MOVBYT(OUTTAB(14),1,TRABUF(TIFSENAMS),1,TIFSEBNAM)
C
         ENDIF
C
      ENDIF
C
      RETURN
      END
