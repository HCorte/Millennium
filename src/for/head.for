C
C SUBROUTINE HEAD
C $Log:   GXAFXT:[GOLS]HEAD.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:32:08   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:35:02   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - taprsub.for **
C
C
C=====================================================================
C
C SUBROUTINE TO PRINT TAVR REPORT HEADER AND INCREMENT LINE COUNT
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE HEAD(DBUF,OPT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INTEGER*2 DBUF(12)
	CHARACTER*40 NAME
	INTEGER OPT,PAGE/0/
	SAVE PAGE
C
	DBUF(VCDC) = DAYCDC
	CALL CDATE(DBUF)
	WRITE (NAME,900)
	CALL TITLE(NAME,'    TAPR',1,7,PAGE,DAYCDC)
C
	IF (OPT .EQ. 0) THEN    !detail page
	   WRITE(7,901)
	   WRITE(7,903)
	   WRITE(7,902)
	ELSE                    !total page
	   WRITE(7,902)
	ENDIF
C
	RETURN
900	FORMAT('TERMINAL AVAILIBILITY PERFORMANCE REPORT')
901	FORMAT(3X,'LINE',2X,'DROP',1X,'TERM #',1X,'RETAILER',3X,
     ! 'TOTAL TIME',2X,'TOTAL TIME',4X,'RESPONSE',5X,'TOTAL TIMES',3X,
     !	 'LONGEST',5X,'NET SALES',2X,'LINE PERFORMANCE')
903	FORMAT(32X,'RESPONDING',2X,'NONRESPOND.',3X,'PERFORMANCE',
     !	 3X,'NONRESPOND.',2X,'INTERVAL')
902	FORMAT(1X,131('='),/)
C
	END
