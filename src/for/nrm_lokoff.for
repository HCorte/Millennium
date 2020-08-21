C
C FUNCTION LOKOFF
C $Log:   GXAFXT:[GOLS]LOKOFF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:55:30   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:55:22   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_glock.for **
C
C
C
C *** LOKOFF
C
C The concurrent system had a LOKOFF call which would clear a lock whether
C or not it was previously locked.  The VAX system has the instruction
C BBCCI which will return an indicator as to whether the flag was already
C clear.
C
C On concurrent, you call LOKOFF as a subroutine: CALL LOKOFF(LOCKVAR).
C This routine is declared as a function so that you may either call it
C as a subroutine or treat it as a function and find out if the bit was
C already clear.
C
C If you use it as a function, you must declare LOKOFF as a logical*4
C
C
C CALLING SEQUENCE:
C
C	IF( .NOT. LOKOFF(LOCKBYTE) )THEN
C	  was already clear
C	ENDIF
C
C	 - OR -
C
C	CALL LOKOFF(LOCKBYTE)
C
C	LOCKBYTE MAY BE A BYTE, WORD, OR LONGWORD
C
C	LOKOFF RETURNS THE VALUE .FALSE. IF THE BIT WAS PREVIOUSLY CLEAR
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	LOGICAL*4 FUNCTION LOKOFF( LOCKBYTE )
	IMPLICIT NONE
C
C
	BYTE	    LOCKBYTE
	VOLATILE    LOCKBYTE
C
	INTEGER*4   LIB$BBCCI
	EXTERNAL    LIB$BBCCI
C
	LOKOFF = .NOT. LIB$BBCCI( 0, LOCKBYTE )
C
	RETURN
	END
