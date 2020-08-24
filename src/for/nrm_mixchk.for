C
C SUBROUTINE MIXCHK
C $Log:   GXAFXT:[GOLS]MIXCHK.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:02:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:00:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_scram.for **
C
C
C
C *** MIXCHK
C
C This is an internally called subroutine to modify the check digits based
C on the external serial #
C
C CALLING SEQUENCE:
C	 CALL MIXCHK( EXTSER, CHKDIG)
C
C INPUT:
C	 EXTSER	    External serial #
C	 CHKDIG	    'Normal' value of check digits
C
C OUTPUT:
C	 CHKDIG	    Scrambled with serial #
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MIXCHK( EXTSER, CHKDIG )
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INTEGER*4      EXTSER    ! INPUT: EXTERNAL SRL #
	INTEGER*4      CHKDIG    ! INPUT/OUTPUT: CHECKSUM
C
	INTEGER*4      EXT
	INTEGER*4      CHK
C
	INTEGER*4      SITE
	PARAMETER(SITE='13'X)
C
	EXT = EXTSER
	CHK = CHKDIG
C
	CHK = IEOR ( EXT, CHK)
	EXT = ISHFT( EXT, -7)
	CHK = IEOR ( EXT, CHK)
	EXT = ISHFT( EXT, -7)
	CHK = IEOR ( EXT, CHK)
C
	CHK = IEOR ( CHK, SITE )
C
	CHKDIG = IAND ( CHK, '000000FF'X)
C
	RETURN
	END
