C BNGSET.FOR
C
C $Log:   GXAFXT:[GOLS]BNGSET.FOV  $  
C  
C     Rev 1.0   17 Apr 1996 12:21:08   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   16 Jan 1996 16:23:34   RXK
C  ST = 0 added
C  
C     Rev 1.1   13 Jan 1995 19:44:58   HXK
C  Fix for duplicate seeds
C  
C     Rev 1.0   01 Nov 1994 18:38:30   HXK
C  Initial revision.
C  
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE BNGSET(PRO_BUF,GIND,ST)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
C
C
        INTEGER*2   PRO_BUF(*)		!procom half word buffer
	INTEGER*4   GIND		!Kicker Game Index.
        INTEGER*4   LENGTH		!input buffer length
	INTEGER*4   I			!Loop Variable.
	INTEGER*4   ST                  !Subroutine Return Status.

        INTEGER*4   BNG_NUM, BNG_SEED
C
C CHECK IF BINGO GAME INDEX IS VALID
C
        ST = 0
	IF(GIND.LT.1.OR.GIND.GT.NUMBGO) THEN
	   ST = -1
	   RETURN
	ENDIF

C
C CHECK SYSTEM STATUS
C
10	CONTINUE
        IF(P(SYSTYP).EQ.LIVSYS) THEN
C
C GET BUFFER LENGTH AND INCREASE IT BY THREE BYTES
C
           LENGTH = PRO_BUF(INPLEN)
           PRO_BUF(INPLEN) = PRO_BUF(INPLEN) + 4
C
C GET BINGO SEED
C
           BNGSED(GIND) = BNGSED(GIND) + 1
           IF(BNGSED(GIND).GT.2147483647) BNGSED(GIND) = 0

           BNG_SEED = BNGSED(GIND)
C
C STORE BINGO SEED IN INPUT BUFFER
C INPTAB*2 BECAUSE INPTAB IS IN FULLWORDS, PRO_BUF IN HALFWORDS
C
	   CALL MOVBYT(BNG_SEED,1,PRO_BUF(INPTAB*2-1),LENGTH+1,4)

        ELSE
C
C DECREMENT INPUT LENGTH
C
           LENGTH = PRO_BUF(INPLEN)-4

           CALL MOVBYT(PRO_BUF(INPTAB*2-1),LENGTH+1,BNG_SEED,1,4)
           BNGSED(GIND) = BNG_SEED

        ENDIF
C
        RETURN
        END
