C
C SUBROUTINE TOPATS
C $Log:   GXAFXT:[GOLS]TOPATS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:36:08   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.3   22 Jan 1994 17:04:30   JXP
C  Do not allow cancelled pools to affect tables if not already in table
C  for their particular combination.
C  
C     Rev 1.2   12 Jan 1994 12:22:42   JXP
C  Utilize daily minimum values and allow for cancellations
C  
C     Rev 1.1   11 Jan 1994 11:00:02   JXP
C  Update table TSPWTL (Win liability)
C  
C     Rev 1.0   21 Jan 1993 17:52:22   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - topats.for **
C
C TOPATS.FOR
C
C V01 30-JUN-92 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C
C SUBROUTINE TO CALCULATE THE TOP 20 MOST PLAYED
C TOTO SELECT COMBINATIONS PER AGENT
C
C THE INFORMATION IN AGTTLG IS STORED AS FOLLOWS:
C ----------------------------------------------
C
C WORD 1             : OFFSET FOR THE COMBINATION
C BYTE 4 TO BYTE 6.5 : AMOUNT BET ON THIS COMBINATION
C BYTE 6.5 TO BYTE 7 : ROW COUNT ON THIS COMBINATION
C WORD 3             : TIME STAMP
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
	SUBROUTINE TOPATS(OFFSET,VALUE,GIND,POLCAN,ROWCNT,TER,TIME,
     *			 ROWNRS,TAB1X2)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:TSLCOM.DEF'
C
	LOGICAL     POLCAN, UPDATED
	INTEGER*4   MIN(ATSAGT)/ATSAGT*0/
	INTEGER*4   MINWIN(ATSAGT)/ATSAGT*0/
	INTEGER*4   OFFSET, VALUE, GIND, ROWCNT, TER, TIME, I, INDEX
	INTEGER*4   TMPMIN
	INTEGER*4   WINVAL
	INTEGER*4   ROWNRS(6),TAB1X2(6),ODDLIABI4
        REAL*8      ODDLIAB
C
C IF THE VALUE TO BE ENTERED IS LESS THAN THE LOWEST IN THE TOP
C 5 TABLE RETURN
C
	IF(VALUE.LT.MIN(TER).AND..NOT.POLCAN) GO TO 12
C
C CALCULATE NEW MINIMUM VALUE IN THE TOP 5 TABLE
C
	UPDATED = .FALSE.
	MIN(TER)='7FFFFFFF'X
	DO 10 I = 1,ATSTOP
	   IF (OFFSET.EQ.TSPATL(ATSOFF,I,TER,GIND).AND.
     *	       ROWCNT.EQ.IAND(TSPATL(ATSAMT,I,TER,GIND),15)) THEN
	       TSPATL(ATSAMT,I,TER,GIND) = ISHFT(VALUE,4) + ROWCNT
	       UPDATED = .TRUE.
	   ENDIF
C
C Skip out at this point, if polcan and not already in table
C
	   IF((.NOT.UPDATED).AND.I.EQ.ATSTOP.AND.
     *		       POLCAN)	GO TO 12 

	   TMPMIN = ISHFT(TSPATL(ATSAMT,I,TER,GIND),-4)
	   IF (TMPMIN.LT.MIN(TER)) THEN
	      MIN(TER) = TMPMIN 
	      INDEX = I
	   ENDIF
10	CONTINUE
C
	IF(.NOT.UPDATED) THEN
	    TSPATL(ATSOFF,INDEX,TER,GIND) = OFFSET
	    TSPATL(ATSAMT,INDEX,TER,GIND) = ISHFT(VALUE,4) + ROWCNT
	    TSPATL(ATSTIM,INDEX,TER,GIND) = TIME
	ENDIF
C
C Calculate the the new minimum winning liability amount in top 5 table
C
12	CONTINUE
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
	IF(WINVAL.GT.MINWIN(TER).OR.POLCAN) THEN
	  UPDATED = .FALSE.
	  MINWIN(TER)='7FFFFFFF'X
	  INDEX = 0
  	  DO 20 I = 1,ATSTOP
	   IF (OFFSET.EQ.TSPWTL(ATSOFF,I,TER,GIND).AND.
     *	       ROWCNT.EQ.IAND(TSPWTL(ATSAMT,I,TER,GIND),15)) THEN
	       TSPWTL(ATSAMT,I,TER,GIND) = ISHFT(WINVAL,4) + ROWCNT
	       UPDATED = .TRUE.
	   ENDIF

	  IF((.NOT.UPDATED).AND.I.EQ.ATSTOP.AND.
     *		       POLCAN)	RETURN

	   TMPMIN = ISHFT(TSPWTL(ATSAMT,I,TER,GIND),-4)
	   IF(TMPMIN.LT.MINWIN(TER)) THEN
 	      MINWIN(TER) = TMPMIN 
	      INDEX = I
	   ENDIF
20	  CONTINUE
	  IF(.NOT.UPDATED) THEN
 	    TSPWTL(ATSOFF,INDEX,TER,GIND) = OFFSET
 	    TSPWTL(ATSAMT,INDEX,TER,GIND) = ISHFT(WINVAL,4) + ROWCNT
	    TSPWTL(ATSTIM,INDEX,TER,GIND) = TIME
	  ENDIF
	ENDIF
C
	RETURN
	END
