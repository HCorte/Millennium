C
C FUNCTION NOCONSIG
C $Log:   GXAFXT:[GOLS]NOCONSIG.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:12:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:07:46   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_noconsig.for **
C
C NOCONSIG.FOR
C
C V01 09-MAY-91 TKO  INITIAL RELEASE
C
C This is a condition handler that will inhibit traps when fortran tries
C to convert a field and cannot do so because of a conversion error.  For
C example, when trying to output 9999 into an I3 field, Fortran will normally
C produce a traceback error.  You may establish this signal handler to prevent
C the traceback from occurring (the field will be set with asterisks).
C
C To use this, you must declare this function as an external.  I.e.,:
C
C	INTEGER*4   NOCONSIG
C	EXTERNAL    NOCONSIG
C
C Then you must establish this as the condition handler as follows:
C
C	CALL LIB$ESTABLISH( NOCONSIG )
C
C Note that you cannot establish both this and any other condition handler
C from the same routine.
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
	INTEGER*4 FUNCTION NOCONSIG(SIGARGS, MECHARGS)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    '(LIB$ROUTINES)'
	INCLUDE	    '($SSDEF)'
	INCLUDE	    '($FORDEF)'
C
	INTEGER*4   SIGARGS(*)
	INTEGER*4   MECHARGS(*)
C
	INTEGER*4   INDEX
C
C
C See if condition passed to me is a Fortran output conversion error.
C If so, simply continue.  If not, resignal the error to a higher level.
C
	INDEX = LIB$MATCH_COND (SIGARGS(2), FOR$_OUTCONERR)
C
	IF(INDEX.EQ.0)THEN
	  NOCONSIG = SS$_RESIGNAL
	ELSE
	  NOCONSIG = SS$_CONTINUE
	ENDIF
C
	END
