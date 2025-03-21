C FUNCTION FREESEG
C
C
C V01 04-MAR-96 wsm Extracted from ALLDLL for Finland.
C
C ** Source - alldll.for **
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C REMOVE FREE SEGMENT FROM FREE LIST AND RETURN ITS NUMBER AS
C FUNCTION VALUE, 0 IF NO FREE SEGMENTS LEFT:
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	INTEGER*4 FUNCTION FREESEG(DUMMY)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:MSGCOM.DEF'
C
        INTEGER*4 DUMMY
C
	FREESEG = I4MSGTAB(MFRLNK, MSGCTL)		! OBTAIN FREE SEGMENT.
C
	IF (FREESEG .GT. 0) THEN
	  I4MSGTAB(MFRLNK, MSGCTL) = I4MSGTAB(MFRLNK, FREESEG)	! ADJUST LIST.
	  I4MSGTAB(MFRCNT, MSGCTL) = I4MSGTAB(MFRCNT, MSGCTL) - 1	! INC. COUNT.
	ENDIF
C
	RETURN
	END
C
