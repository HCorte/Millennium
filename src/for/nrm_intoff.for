C
C SUBROUTINE INTOFF
C $Log:   GXAFXT:[GOLS]INTOFF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:39:08   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   30 Jul 1993 19:01:48   HXN
C  Increase MAX_TAB_MARKS from 10  to 25.
C  
C     Rev 1.0   21 Jan 1993 16:40:50   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_intoff.for **
C
C INTOFF.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C        REVISION HISTORY
C        CONVERT INTERVAL CODING TO I*4
C
C        FUNCTION RETURNS INTEGER*4 VALUE - 0 - INTERVAL CORRECT
C                                  AND BOARDS SET
C
C CALL INTOFF(NBOARD,NUMMRK,MAXMRK,INTARY,ST,MRKARY,OFFSETS)
C INPUT:      NBOARD - # OF INTARY
C             NUMMRK - # OF MARKS ON BOARD
C             MAXMRK - HIGHEST MARK
C             INTARY - BOARDS (IN INTERVAL FORM)
C
C OUTPUT:     ST - STATUS:
C			-0 OK
C                       -1 TO BIG MARK ON BOARD
C                       -2 TO MANY 0s IN INTERVALS
C                       -3 NO LOGICAL END 3 0s	      (????)
C              MRKARY   - I*4 BOARDS' MARKS ARRAY
C              OFFSETS  - I*4 TABLE OF OFFSETS
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE INTOFF(NBOARD,NUMMRK,MAXMRK,INTARY,ST,MRKARY,OFFSETS)
	IMPLICIT    NONE
C
	INTEGER*4   NBOARD
	INTEGER*4   NUMMRK
	INTEGER*4   MAXMRK
	BYTE	    INTARY(*)
	INTEGER*4   ST
	INTEGER*4   MRKARY(*)
	INTEGER*4   OFFSETS(*)
C
C
	INTEGER*4   BRD,PNT,VAL,CNT,XBYT,NIB
	LOGICAL	    LEFT
	LOGICAL	    INITIALIZED/.FALSE./
	INTEGER*4   MAX_TAB_MRK
	PARAMETER   (MAX_TAB_MRK = 60)	    ! MAXIMUM MARK FOR ANY GAME
	INTEGER*4   MAX_TAB_MRKS
	PARAMETER   (MAX_TAB_MRKS = 25)	    ! MAX. # MARKS FOR ANY GAME
	INTEGER*4   PERTAB(0:MAX_TAB_MRK, MAX_TAB_MRKS)
	INTEGER*4   MRKARY_INX
	INTEGER*4   OFFSETS_INX
C
C
	IF (.NOT. INITIALIZED) THEN
	    CALL PERM(PERTAB,MAX_TAB_MRK,MAX_TAB_MRKS,-1)
	    INITIALIZED = .TRUE.
	ENDIF
	MRKARY_INX = 1
	OFFSETS_INX = 1
	PNT = 0
	LEFT = .TRUE.
	DO 2900 BRD = 1, NBOARD
	  VAL = 0
	  CNT = 0
	  OFFSETS(OFFSETS_INX) = 0
2100	  CONTINUE
	  IF(LEFT)THEN
	    PNT  = PNT+1
	    XBYT = INTARY(PNT)
	    XBYT = IAND(XBYT,'000000FF'X)
	    NIB  = ISHFT(XBYT,-4)
	    LEFT = .FALSE.
	  ELSE
	    NIB  = XBYT
	    LEFT = .TRUE.
	  ENDIF
	  NIB = IAND (NIB, '0F'X)
	  IF(NIB.EQ.0)THEN
	    VAL = VAL+15
	    IF(VAL.GT.MAXMRK)THEN
	      ST = -2
	      GOTO 9000
	    ENDIF
	    GOTO 2100
	  ENDIF
C
	  VAL = VAL + NIB
	  IF(VAL.GT.MAXMRK)THEN
	    ST = -1
	    GOTO 9000
	  ENDIF
C
C	  SAVE THE VALUE OF THE MAKR
C
	  MRKARY(MRKARY_INX) = VAL
	  MRKARY_INX = MRKARY_INX + 1
	  CNT = CNT+1
C
C	  KEEP CALCULATING OFFSET
C
	  OFFSETS(OFFSETS_INX) = OFFSETS(OFFSETS_INX)+PERTAB(VAL-1,CNT)
	  IF(CNT.LT.NUMMRK)GOTO 2100
C
C	  PREPARE FOR NEXT OFFSET
C
	  OFFSETS_INX = OFFSETS_INX + 1
C
2900	CONTINUE
	ST = 0
C
9000	CONTINUE
	RETURN
	END
