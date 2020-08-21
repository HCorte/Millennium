C
C SUBROUTINE GETCCITT
C $Log:   GXAFXT:[GOLS]GETCCITT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:19:02   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:24:26   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_getccitt.for **
C
C GETCCITT.FOR
C
C V01 09-JUL-91 TKO  Released for VAX
C
C
C This routine will determine a CCITT of the passed data
C
C Calling sequence:
C
C	CALL GETCCITT( BARY, BEGOFF, NUMBYT, RESULT)
C
C	(note that this intentionally has the same calling sequence as
C	 CHECKSUM)
C
C Input:
C	BARY        An array containing string
C	BEGOFF	    Beginning offset within array (relative to 0)
C	NUMBYT	    # of bytes to calculate
C
C Output:
C	RESULT	    I*4 CCITT of the string (only low order 16 bits used)
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETCCITT( BARY, BEGOFF, NUMBYT, RESULT)
	IMPLICIT NONE
C
	BYTE		BARY(0:*)
	INTEGER*4	BEGOFF
	INTEGER*4	NUMBYT
	INTEGER*4	RESULT
C
	INTEGER*4	LIB$CRC
	EXTERNAL	LIB$CRC
C
	INTEGER*4	CRCTAB(16)	    !FOR CRC POLYNOMIAL
	INTEGER*4	DSCBLK(2)	    !DESCRIPTOR BLOCK
C
	LOGICAL		FIRSTCALL/.TRUE./
C
C
C On the first call, create the polynomial table
C
	IF( FIRSTCALL )THEN
	  FIRSTCALL = .FALSE.
	  CALL LIB$CRC_TABLE( '102010'O, CRCTAB )
	ENDIF
C
C The following is to trick LIB$CRC so it thinks it is getting a
C character string descriptor rather than a byte array
C
	IF( NUMBYT.GT.1 .AND. NUMBYT.LE.'0000FFFF'X ) THEN
	  DSCBLK(1) = '010E0000'X + NUMBYT
	  DSCBLK(2) = %LOC(BARY(BEGOFF))
C
	  RESULT = LIB$CRC( CRCTAB, '00000000'X, %REF(DSCBLK) )
	  RESULT = IAND( RESULT, '0000FFFF'X)
	ELSE
	  RESULT = 0
	ENDIF
C
	RETURN
9300	FORMAT('  INITIAL VALUE OF TWOBYTE CHECKSUM = ',Z8)
	END
