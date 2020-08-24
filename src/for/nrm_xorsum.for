C
C SUBROUTINE XORSUM
C $Log:   GXAFXT:[GOLS]XORSUM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 16:47:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 18:38:32   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_xorsum.for **
C
C XORSUM.FOR
C
C V01 1-JAN-91 KWP    INITIAL VERSION
C
C THIS SUBROUTINE WILL DO AN 'EXCLUSIVE OR' CHECKSUM
C
C	CALL XORSUM(ARRAY, LEN, RESULT)
C
C	INPUT:
C	    INTEGER*4	ARRAY	   ARRAY OF BYTES TO BE CHECKSUMMED
C	    INTEGER*4	LEN	   # OF BYTES TO BE CHECKSUMMED
C
C	OUTPUT:
C	    INTEGER*4	RESULT	   CHECKSUM
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
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE XORSUM(BINARRAY,LEN,RESULT)
	IMPLICIT NONE
C
	BYTE	    BINARRAY(*)	    !INPUT BYTE ARRAY
	INTEGER*4   LEN		    !# OF BYTES TO CHECKSUM
	INTEGER*4   I		    !LOOP VARIABLE
	INTEGER*4   RESULT	    !CHECKSUM RESULT
	INTEGER*4   DUMMY	    !DUMMY I*4 VARIABLE USED IN IEOR
C
	RESULT = 0		    !INITIALIZE RESULT
C
	DO 1000 I=1,LEN
	  DUMMY=ZEXT( BINARRAY(I) ) !FORCE 0'S IN HIGH ORDER
	  RESULT=IEOR(DUMMY,RESULT)
1000	CONTINUE
C
	RETURN
	END
