C
C SUBROUTINE ENCDAT
C $Log:   GXAFXT:[GOLS]ENCDAT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:04:20   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:12:42   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_datsnp.for **
C
C
C
C SUBROUTINE TO ENCODE DATE LINE
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE ENCDAT(LINE,DATE,I,DRAW,ADV)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INTEGER*2 D(LDATE_LEN)
	CHARACTER*26 HEAD(17)
	CHARACTER*3 ADVNAM(0:7)
	INTEGER*4 LINE(20), K, DRAW, I, J, DATE, ADV(7), AIND
C
	INTEGER*4 XLIN(20)
	CHARACTER*80 CXLIN
	EQUIVALENCE (XLIN,CXLIN)
C
	DATA ADVNAM/'   ','MON','TUE','WED','THU','FRI','SAT','SUN'/
	DATA HEAD/'System  sales  date ',
     *	          'Current draw             ',
     *	          'Next draw 1             ',
     *	          'Next draw 2             ',
     *	          'Next draw 3             ',
     *	          'Next draw 4             ',
     *	          'Next draw 5             ',
     *	          'Next draw 6             ',
     *	          'Next draw 7             ',
     *	          'Next draw 8             ',
     *	          'Next draw 9             ',
     *	          'Next draw 10            ',
     *	          'Next draw 11            ',
     *	          'Next draw 12            ',
     *	          'Next draw 13            ',
     *	          'Next draw 14            ',
     *	          'Next draw 15            '/
C
	IF(DATE.GT.0) THEN
	  AIND=0
	  DO 10 J=1,7
	  IF(ADV(J).EQ.DRAW) AIND=J
10	  CONTINUE
	  D(VCDC)=DATE
	  CALL LCDATE(D)
	  WRITE(CXLIN,901) HEAD(I),DRAW,D(5),D(4),(D(K),K=1,2),D(14),
     *	                   (D(K),K=7,13),ADVNAM(AIND)
	ELSE
	  WRITE(CXLIN,902) HEAD(I),DRAW
	ENDIF
	CALL FASTMOV(XLIN,LINE,20)
C
	RETURN
901	FORMAT(A25,I4,'  ',2(I4,'    '),I2,'/',I2,'/',I4,
     *	       ' ',7A2,' ',A3)
902	FORMAT(A25,I4,20(' '),'Date  not  initialized')
	END
