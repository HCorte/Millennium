C
C FUNCTION IMONY
C $Log:   GXAFXT:[GOLS]IMONY.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:35:46   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:38:28   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_imony.for **
C
C IMONY.FOR
C
C V01 05-AUG-91 WLM INITIAL RELEASE FOR VAX
C
C
C THIS INTEGER*4 FUNCTION CONVERTS AMOUNT OF MONEY, PASSED AS
C A CHARACTER STRING ENCODED IN INTEGER*4 ARRAY, TO THE COUNT
C OF MONEY UNITS WE USE (E.G. BET OR VALIDATION UNITS)
C
C Calling sequence:
C
C     NUMBER=IMONY(SLINE,POS,LEN,FACTOR)
C
C Input parameters:
C
C     SLINE	  Variable containing character string to be converted
C		  (INTEGER*4 array)
C     POS	  Starting position of the string in SLINE (byte number)
C     LEN	  Actual length of SLINE (INTEGER*4 count; max 20)
C     FACTOR      Money storing unit descriptor:
C		   less than -2   invalid
C		             -2   DYN_BETUNIT
C		             -1   DYN_VALUNIT
C		              0   invalid
C		             >0   actual unit itself
C
C
C Output parameters:
C
C     IMONY       Amount of money converted to INTEGER*4 count of units
C		  defined by FACTOR
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
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	INTEGER*4 FUNCTION IMONY(SLINE,POS,LEN,FACTOR)
	IMPLICIT NONE
    	INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
C
	INTEGER*4	SLINE(20)	!INPUT I*4 ARRAY
	INTEGER*4	POS		!STARTING POSITION
	INTEGER*4	LEN		!LENGTH OF OUTPUT
        INTEGER*4       FACTOR          !SHOWS WHICH UNIT OF MONEY TO USE
	INTEGER*4	I		!LOOP COUNTER
        INTEGER*4       MON_UNIT,DLINE(20)
	INTEGER*4	ANUM, ONUM, DCNT, DIG, FLGA, ERR
	LOGICAL		DPNT
	CHARACTER*1	CLINE(80)
	EQUIVALENCE	(DLINE,CLINE)
C
C DEFAULT: INVALID INPUT
C
	IMONY=-1
C
	IF(FACTOR.EQ.BETUNIT.OR.
     *	   FACTOR.EQ.VALUNIT.OR.
     *	   FACTOR.GT.0) THEN
C
C GET INPUT LINE
C
	  DO 20,I=1,20
	    DLINE(I)=SLINE(I)
20	  CONTINUE
C
C ASSIGN VALUE TO THE UNIT OF MONEY
C
          IF (FACTOR .EQ. VALUNIT) THEN
            MON_UNIT=DYN_VALUNIT
          ELSEIF (FACTOR .EQ. BETUNIT) THEN
            MON_UNIT=DYN_BETUNIT
          ELSE
            MON_UNIT=FACTOR
          ENDIF
C
C
	  FLGA=0
	  ANUM=0
	  DCNT=0
	  DPNT=.FALSE.
	  DO 120 I=POS,LEN*4
	    IF(CLINE(I).EQ.' ') GO TO 125
            IF(CLINE(I).EQ.'.') THEN
              DPNT=.TRUE.
              GOTO 120
            ENDIF
	    IF(DPNT) DCNT=DCNT+1
	    IF(DCNT.GT.CENT_LEN) GOTO 40
	    CALL ASCBIN(DLINE,I,1,DIG,ERR)
	    IF(ERR.LT.0) GOTO 40
	    FLGA=1
	    ANUM=ANUM*10+DIG
120	  CONTINUE
125	  CONTINUE
	  IF(FLGA.EQ.0) GOTO 40
	  I=CENT_LEN-DCNT
	  ANUM=ANUM*(10**I)
C
	  ONUM=ANUM
	  ANUM=ANUM/MON_UNIT
	  IF(ANUM*MON_UNIT.NE.ONUM) GOTO 40 
C
	  IMONY=ANUM
C
40	  CONTINUE
	  RETURN
	ELSE
	  WRITE(5,900) FACTOR
	ENDIF
900	FORMAT(1X,'*** Invalid money unit passed to IMONY function >',I10)
	END
