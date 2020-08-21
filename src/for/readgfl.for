C
C V01 01-MAR-2000 UXN SEPARATED FROM MULSUBS.
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
      SUBROUTINE READGFL(LUN,FILNAM,LGTSEC,DRAW,REC)
      IMPLICIT NONE
C
      INTEGER * 4 LUN,FILNAM(*),LGTSEC,DRAW,REC(*)
      INTEGER * 4 ST,FDB(7)
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
C
      CALL OPENW(LUN,FILNAM,4,0,0,ST)
      CALL IOINIT(FDB,LUN,LGTSEC*256)
      IF(ST.NE.0) CALL FILERR(FILNAM,1,ST,0)
      CALL READW(FDB,DRAW,REC,ST)
      IF(ST.NE.0) CALL FILERR(FILNAM,2,ST,DRAW)
C
      CALL CLOSEFIL(FDB)

      END
