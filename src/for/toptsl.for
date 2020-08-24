C
C SUBROUTINE TOPTSL
C $Log:   GXAFXT:[GOLS]TOPTSL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:36:22   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   28 Feb 1994 22:35:46   JXP
C  Corrected initial value stored in tslwtop table
C  
C     Rev 1.1   25 Feb 1994 19:14:36   HXK
C  PITKA LIABILITY LIMITATIONS CHANGE.
C  
C     Rev 1.0   21 Jan 1993 17:52:38   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - toptsl.for **
C
C TOPTSL.FOR
C
C V01 30-JUN-92 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C SUBROUTINE TO CALCULATE THE TOP 30 MOST PLAYED
C TOTO SELECT COMBINATIONS
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
	SUBROUTINE TOPTSL(OFFSET,VALUE,GIND,POLCAN,ROWCNT,
     *		ROWNRS,TAB1X2,WINVAL)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TSLCOM.DEF'
C
	LOGICAL     POLCAN, UPDATED
	INTEGER*4   MIN/0/
	INTEGER*4   TMPMIN
	INTEGER*4   OFFSET, VALUE, GIND, ROWCNT, I, INDEX


	INTEGER*4   MINWIN/0/
        INTEGER*4   WINVAL
        INTEGER*4   ROWNRS(6),TAB1X2(6),ODDLIABI4
        REAL*8      ODDLIAB
C

C
	UPDATED = .FALSE.
C
C IF THE VALUE TO BE ENTERED IS LESS THAN THE LOWEST IN THE TOP
C 30 TABLE RETURN
C
	IF(VALUE.LT.MIN.AND..NOT.POLCAN) GOTO 12
C
C CALCULATE NEW MINIMUM VALUE IN THE TOP 30 TABLE
C
	MIN = '7FFFFFFF'X
	DO 10 I = 1,30
	   IF (OFFSET.EQ.TSLTOP(1,I,GIND).AND.
     *	       ROWCNT.EQ.IAND(TSLTOP(2,I,GIND),15)) THEN
	       TSLTOP(2,I,GIND) = ISHFT(VALUE,4) + ROWCNT
	       UPDATED = .TRUE.
	   ENDIF
	   TMPMIN = ISHFT(TSLTOP(2,I,GIND),-4)
	   IF (TMPMIN.LT.MIN) THEN
	      MIN = TMPMIN
	      INDEX = I
	   ENDIF
10	CONTINUE
C
	IF(.NOT.UPDATED) THEN
	    TSLTOP(1,INDEX,GIND) = OFFSET
	    TSLTOP(2,INDEX,GIND) = ISHFT(VALUE,4) + ROWCNT
	ENDIF
C
C Calculate the the new minimum winning liability amount in top 100 table
C
12      CONTINUE
        ODDLIAB=1
        DO 15 I=1,ROWCNT
            IF(ROWNRS(I).NE.0) THEN
                IF (TAB1X2(I).EQ.4) TAB1X2(I)=3
                ODDLIAB=ODDLIAB * TSLODS(TAB1X2(I),ROWNRS(I),GIND)
            ENDIF
15      CONTINUE
        ODDLIABI4 = ODDLIAB
        IF(ROWCNT.GT.2) ODDLIAB = DNINT(ODDLIAB/DFLOAT(100**(ROWCNT-2)))
        IF(ROWCNT.GT.1) ODDLIAB = DNINT(ODDLIAB/100.0D0)
        ODDLIAB = ODDLIAB/100.0D0
        WINVAL = IDNINT(ODDLIAB*DFLOAT(VALUE)+.00000001)
C
C Only concerned with higher win values and possible new minimum due to
C pool cancellations
C
          UPDATED = .FALSE.
C
C CalcuLate new minimum value in the top 100 table
C If polcan and not in table already then skip out

        IF(WINVAL.GT.MINWIN.OR.POLCAN) THEN
	    MINWIN = '7FFFFFFF'X
	    DO 20 I = 1,100
	    IF (OFFSET.EQ.TSLWTOP(1,I,GIND).AND.
     *	        ROWCNT.EQ.IAND(TSLWTOP(2,I,GIND),15)) THEN
	        TSLWTOP(2,I,GIND) = ISHFT(WINVAL,4) + ROWCNT
	        UPDATED = .TRUE.
	    ENDIF

	    IF((.NOT.UPDATED).AND.I.EQ.100.AND.POLCAN)  RETURN

	    TMPMIN = ISHFT(TSLWTOP(2,I,GIND),-4)
	    IF (TMPMIN.LT.MINWIN) THEN
	        MINWIN = TMPMIN
	        INDEX = I
	    ENDIF
20	    CONTINUE
C
	    IF(.NOT.UPDATED) THEN
		TSLWTOP(1,INDEX,GIND) = OFFSET
		TSLWTOP(2,INDEX,GIND) = ISHFT(WINVAL,4) + ROWCNT
	    ENDIF
	ENDIF

	RETURN
	END
