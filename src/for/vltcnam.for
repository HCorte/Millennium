C
C V01 03-MAR-2000 UXN Separated from MULSUBS
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
      SUBROUTINE VLTCNAM
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:STOPCOM.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
C
      INTEGER * 4 I,VOLVLW,VOLTCW
      CHARACTER * 20 CTEMP
      INTEGER * 4 TEMP(5)
      EQUIVALENCE(CTEMP,TEMP)
C
C------------- VLW VLC NAMES -----------------
C
      VOLVLW=SFNAMES(1,VLW)
      VOLTCW=SFNAMES(1,TCW)
C
      DO I=1,MAXMVLF
         CALL VLTCSET(VOLVLW,I,'VLW',VLWNAM(1,I))
      ENDDO
C
      DO I=1,MAXMTCF
         CALL VLTCSET(VOLTCW,I,'TCW',TCWNAM(1,I))
      ENDDO
C
      RETURN
      END
