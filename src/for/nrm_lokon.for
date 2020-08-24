C
C FUNCTION LOKON
C $Log:   GXAFXT:[GOLS]LOKON.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:55:32   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:55:28   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_glock.for **
C
C GLOCK.FOR
C
C V03 15-APR-91 TKO REMOVED GLOKON, GLOKOFF
C V02 28-JAN-91 TKO MODIFIED CALLING SEQUENCES, ETC.
C V01 05-JUL-90 MP  INITIAL RELEASE
C			TARGETED FOR PORT OF CONCURRENT FORTRAN
C			TO VAX/VMS ENVIRONMENT
C
C	OPERATIONAL SPEC:
C
C	HISTORY:
C	ON THE 'CONCURRENT' COMPUTERS LOCKING FUNCTIONS WERE IMPLEMENTED
C	WITH 'LOKON' AND 'LOKOFF'. THESE ROUTINES EXPECTED LOCATION
C	ADDRESS
C	. DURING TRANSALATION THIS INTERFACE HAVE BEEN PPRESERVED
C	IN SOME CASES. FOR LOCKING OF LISTS (TOP OR BOTTOM)
C	A DIFFERENT INTERFACE HAVE BEEN APPLIED TO PROVIDE MORE
C	PORTABLE SOLUTION FOR THE FUTURE.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C *** LOKON
C
C This routine performs the equivalent of a concurrent LOKON call.
C It performs a test & set to the low order bit of the byte (or word)
C specified in the call and returns a status indicating whether or not
C the bit was previously set.
C
C
C CALLING SEQUENCE:
C
C	IF( LOKON(LOCKBYTE) )THEN
C	  WAIT OR SOMETHING
C	  AND TRY AGAIN
C	ENDIF
C
C 	LOCKBYTE MAY BE A BYTE, WORD, OR LONGWORD
C
C	LOKON RETURNS THE VALUE .TRUE. IF THE BIT WAS PREVIOUSLY SET
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	LOGICAL*4 FUNCTION LOKON( LOCKBYTE )
	IMPLICIT NONE
C
C
	BYTE	    LOCKBYTE
	VOLATILE    LOCKBYTE
C
	INTEGER*4   LIB$BBSSI
	EXTERNAL    LIB$BBSSI
C
	LOKON = LIB$BBSSI( 0, LOCKBYTE )
C
	RETURN
	END
