C
C SUBROUTINE CHKINT
C $Log:   GXAFXT:[GOLS]CHKINT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:32:22   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:49:42   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_chkint.for **
C
C CHKINT.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C ...... CHKINT    INTERVAL CODING TO BIT MAPPING
C
C CALLING SEQUENCE:
C
C        CALL CHKINT(NBOARD,SYSTEM,MAXNUM,INTARY,ST)
C
C INPUT:
C
C        NBOARD - NUMBER OF BOARDS
C        SYSTEM - SYSTEM NUMBER
C        MAXNUM - MAXIMUM # BET
C	 INTARY - INTERVAL CODED ARRAY
C
C OUTPUT:
C
C        STATUS - 0 = OK, -1 = ERROR
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CHKINT(NBOARD,SYSTEM,MAXNUM,INTARY,ST)
	IMPLICIT NONE
C
	INTEGER*4   NBOARD
	INTEGER*4   SYSTEM
	INTEGER*4   MAXNUM
	BYTE	    INTARY(*)
	INTEGER*4   ST
C
C
	INTEGER*4   BRD,PNT,VAL,CNT,XBYT,NIB
	LOGICAL	    LEFT
C
C
	PNT = 0
	LEFT = .TRUE.
	DO 2900 BRD = 1, NBOARD
	  VAL = 0
	  CNT = 0
2100	  CONTINUE
	  IF(LEFT)THEN
	    PNT  = PNT+1
	    XBYT = ZEXT(INTARY(PNT))
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
	      ST = -1
	      GOTO 9000
	    ENDIF
	    GOTO 2100
	  ENDIF
C
	  VAL = VAL + NIB
	  IF(VAL.GT.MAXNUM)THEN
	    ST = -1
	    GOTO 9000
	  ENDIF
C
	  CNT = CNT+1
	  IF(CNT.LT.SYSTEM)GOTO 2100
C
2900	CONTINUE
	ST = 0
C
9000	CONTINUE
	RETURN
	END
