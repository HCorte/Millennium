C
C SUBROUTINE SINTMAP
C $Log:   GXAFXT:[GOLS]SINTMAP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:07:30   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:37:54   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_sintmap.for **
C
C NRM_SINTMAP.FOR
C
C V02 07-OCT-91 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SINTMAP.FOR
C
C V01 01-AUG-90 TKO  RELEASED FOR VAX
C
C ...... SINTMAP    INTERVAL CODING TO BIT MAPPING
C
C CALLING SEQUENCE:
C
C        CALL SINTMAP(TERMES,INDEX,NBOARD,SYSTEM,MAXNUM,BITMAP,ST)
C
C INPUT:
C
C        TERMES - TERMINAL MESSAGE (I4)
C        INDEX  - BYTE INDEX FOR START OF BOARDS
C        NBOARD - NUMBER OF BOARDS
C        SYSTEM - SYSTEM NUMBER ARRAY
C        MAXNUM - MAXIMUM # BET
C
C OUTPUT:
C
C        BITMAP - BOARDS IN BIT MAP (I2)
C        STATUS - 0 = OK, -1 = ERROR
C
C The Concurrent version always zeroed out 80 bytes in BITMAP.  This
C version only zeroes out what it needs to, based on MAXBDS.
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SINTMAP(TERMES,INDEX,NBOARD,SYSTEM,MAXNUM,BITMAP,ST)
	IMPLICIT NONE
C
	BYTE	    TERMES(*)
	INTEGER*4   INDEX
	INTEGER*4   NBOARD
	INTEGER*4   SYSTEM(*)
	INTEGER*4   MAXNUM
	INTEGER*2   BITMAP(4,*)
	INTEGER*4   ST
C
C The following parameters are used because the old code did so
C
	INTEGER*4    MAPLEN
	PARAMETER   (MAPLEN = 8)	    !MAX # OF BYTES PER BOARD
C
C
	INTEGER*4   BRD,PNT,VAL,CNT,XBYT,NIB
	LOGICAL	    LEFT
C
C
	IF(MAXNUM.GT.MAPLEN*8)THEN
	  ST = -1
	  GOTO 9000
	ENDIF
C
	PNT = 0
	LEFT = .TRUE.
	DO 2900 BRD = 1, NBOARD
	  BITMAP(1,BRD) = 0
	  BITMAP(2,BRD) = 0
	  BITMAP(3,BRD) = 0
	  BITMAP(4,BRD) = 0
C
	  VAL = 0
	  CNT = 0
2100	  CONTINUE
	  IF(LEFT)THEN
	    PNT  = PNT+1
	    XBYT = TERMES(PNT)
	    XBYT = IAND(XBYT, '000000FF'X)
	    NIB  = ISHFT(XBYT,-4)
	    LEFT = .FALSE.
	  ELSE
	    NIB  = XBYT
	    LEFT = .TRUE.
	  ENDIF
	  NIB = IAND (NIB, '0F'X)
	  IF(NIB.EQ.0)THEN
	    VAL = VAL+15
	    IF(VAL.GT.MAXNUM)THEN
	      ST = -2
	      GOTO 9000
	    ENDIF
	    GOTO 2100
	  ENDIF
C
	  VAL = VAL + NIB
	  IF(VAL.GT.MAXNUM)THEN
	    ST = -3
	    GOTO 9000
	  ENDIF
C
	  CALL BSET(BITMAP(1,BRD),VAL-1)
	  CNT = CNT+1
	  IF(CNT.LT.SYSTEM(BRD))GOTO 2100
C
2900	CONTINUE
	ST = 0
C
9000	CONTINUE
	RETURN
	END
