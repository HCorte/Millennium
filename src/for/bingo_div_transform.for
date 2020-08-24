C
C SUBROUTINE BINGO_DIV_TRANSFORM
C $Log:   GXAFXT:[GOLS]BINGO_DIV_TRANSFORM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:16:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   23 Nov 1994 16:13:24   HXK
C  Allow for subgames
C  
C     Rev 1.0   10 Oct 1994 11:53:16   HXK
C  Initial revision.
C  
C  
C
C SUBROUTINE TO TRANSFORM WINNING BINGO DIVISIONS
C FROM HOST ORDERING TO TERMINAL OREDERING
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE BINGO_DIV_TRANSFORM(DRAW,DOFF,GIND,DIV,SUB,MDIV)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:PRZCOM.DEF'
   
        INTEGER*4 DIV, MDIV, SUB, DRAW, DOFF, GIND

        IF(DRAW.LE.BNGLOB(GIND).AND.SUB.EQ.BGOBAB) THEN      ! old AB win
           MDIV = DIV
        ELSEIF(DRAW.LE.BNGLOB(GIND).AND.SUB.EQ.BGOFHS) THEN  ! old Fullhouse win
           IF(DIV.EQ.1) MDIV = 4
           IF(DIV.EQ.2) MDIV = 5
           IF(DIV.EQ.3) MDIV = 7
           IF(DIV.EQ.4) MDIV = 9
           IF(DIV.EQ.5) MDIV = 11
           IF(DIV.EQ.6) MDIV = 6
        ELSE
           MDIV=BPZLOT(DIV,DOFF,GIND)                     ! new Fullhouse win   
        ENDIF
        RETURN

        END
