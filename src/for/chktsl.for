C SUBROUTINE CHKTSL
C
C V05 27-NOV-97 UXN Changes to allow some rows to be played as single
C	            and doubles. Error code 250 and 260 added.
C     Rev 1.0   17 Apr 1996 12:34:10   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C     Rev 1.3   14 Oct 1993 17:04:28   HXK
C  CHANGE FOR SUB ERROR.
C     Rev 1.2   14 Oct 1993 12:53:36   HXK
C  RETURN INVALID ERROR FOR ROW CLOSED.
C     Rev 1.1   28 Jun 1993 11:03:08   GXA
C  Released for Finland Dec Conversion / Oddset.
C     Rev 1.0   21 Jan 1993 15:51:44   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C SUBROUTINE TO CHECK TOTO SELECT DETAIL BET
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
	SUBROUTINE CHKTSL(BETBUF,GIND,ERROR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:TSLCOM.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
	INTEGER*4 ROWS(MAXSRW),BETBUF(20)
	INTEGER*4 GIND, ERROR, I, R, POL
C
C
	IF(BETBUF(2).LT.1.OR.BETBUF(2).GT.6) THEN
	  ERROR=SYNT
	  SYNTERRCOD=200
	  RETURN
	ENDIF
C
	IF(BETBUF(1).LT.1) THEN
	  ERROR=SYNT
	  SYNTERRCOD=210
	  RETURN
	ENDIF
C
C CHECK ROWS BET
C
	CALL FASTSET(0,ROWS,MAXSRW)
	DO 10 I=0,BETBUF(2)-1
	   R=BETBUF(3+I*3)
	   POL=BETBUF(4+I*3)
C
	   IF(R.LT.1.OR.R.GT.TSLRWS(GIND)) THEN
	      ERROR=SYNT
	      SYNTERRCOD=220
	      RETURN
	   ENDIF
C
	   IF(BETBUF(2).EQ.1.AND.TSLROWTYP(R,GIND).NE.1) THEN
	      ERROR=SYNT
	      SYNTERRCOD=250
	      RETURN
	   ENDIF
C
	   IF(BETBUF(2).EQ.2.AND.
     *        TSLROWTYP(R,GIND).NE.1.AND.
     *        TSLROWTYP(R,GIND).NE.2) THEN
	      ERROR=SYNT
	      SYNTERRCOD=260
	      RETURN
	   ENDIF	
C
	   ROWS(R)=ROWS(R)+1
	   IF(ROWS(R).GT.1) THEN
	      ERROR=SYNT
	      SYNTERRCOD=230
	      RETURN
	   ENDIF
C
	   IF(POL.LT.ROWWIN.OR.POL.GT.ROWTIE) THEN
	      ERROR=SYNT
	      SYNTERRCOD=240
	      RETURN
	   ENDIF
C
	   IF(TSLSTA(R,GIND).NE.GAMOPN) THEN
	      ERROR = GREV_GAMCLT
	      RETURN
	   ENDIF
C
	   IF(TSPLOK(R,POL,GIND).GT.0)THEN
	      ERROR=LIAB
	      RETURN
	   ENDIF
10	CONTINUE
	RETURN
	END
