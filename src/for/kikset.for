C
C $Log:   GXAFXT:[GOLS]KIKSET.FOV  
C  
C     Rev 1.0   17 Apr 1996 13:43:10   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.4   16 Jan 1996 16:25:46   RXK
C  Line ST=0 added
C  
C     Rev 1.3   12 Feb 1994 13:06:30   HXK
C  CHANGED JOKER ALGORITHM TO UNIRAN64.
C  
C     Rev 1.2   19 Jul 1993 22:34:46   GXA
C  Adjusted index into which the prime# is put into for 7 digit Kicker#.
C  
C     Rev 1.1   15 Jul 1993 21:54:42   GXA
C  Added Status parameter to call and changed check if Kicker#
C  to return negative status for invalid kicker#.
C  
C     Rev 1.0   24 Jun 1993 13:32:16   GXA
C  Initial revision.
C  
C     Rev 1.0   24 Jun 1993 13:29:10   GXA
C  Initial revision.
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE KIKSET(PRO_BUF,KGIND,ST)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
C
C
        INTEGER*2   PRO_BUF(*)		!procom half word buffer
	INTEGER*4   KGIND		!Kicker Game Index.
        INTEGER*4   LENGTH		!input buffer length
	INTEGER*4   I			!Loop Variable.
	INTEGER*4   ST                  !Subroutine Return Status.

        INTEGER*4   JOK_DRW,JOK_NUM,LIMIT,OCTDIG
C
C CHECK IF KICKER GAME INDEX IS VALID
C
        ST = 0 
	IF(KGIND.LT.1.OR.KGIND.GT.NUMKIK) THEN
	   ST = -1
	   RETURN
	ENDIF

        LIMIT = KIKMAX(KGIND)
        OCTDIG = KIKOCT(KGIND)
C
C Increase the seed number.
C
        KIKSED(1,KGIND) = KIKSED(1,KGIND) + 1
        IF(KIKSED(1,KGIND).GT.KIKMAX(KGIND)) KIKSED(1,KGIND) = 0

C
C CHECK SYSTEM STATUS
C
        IF(P(SYSTYP).EQ.LIVSYS) THEN
C
C GET BUFFER LENGTH AND INCREASE IT BY THREE BYTES
C
           LENGTH = PRO_BUF(INPLEN)
           PRO_BUF(INPLEN) = PRO_BUF(INPLEN) + 3
C
C GENERATE A NEW JOKER NUMBER FROM SEED, THIS ALGORITHM WILL
C GENERATE ALL POSSIBLE JOKER NUMBERS BEFORE WRAPPING
C
           JOK_DRW = MOD(KIKDRW(KGIND),64)
           JOK_NUM = KIKSED(1,KGIND)

           CALL RND64(JOK_NUM,JOK_DRW,1,LIMIT,OCTDIG)
C
C STORE JOKER NUMBER IN INPUT BUFFER
C INPTAB*2 BECAUSE INPTAB IS IN FULLWORDS, PRO_BUF IN HALFWORDS
C
	   CALL MOVBYT(JOK_NUM,1,PRO_BUF(INPTAB*2-1),LENGTH+1,3)
        ELSE
C
C DECREMENT INPUT LENGTH
C
           LENGTH = PRO_BUF(INPLEN)
           LENGTH = LENGTH - 3
        ENDIF
C
        RETURN
        END
