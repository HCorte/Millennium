C
C SUBROUTINE NMOVBYT
C $Log:   GXAFXT:[GOLS]NMOVBYT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:12:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:07:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_nmovbyt.for **
C
C VAX_NMOVBYT.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C NMOVBYT.FOR
C
C V01 16-JUL-90 TKO  RELEASED FOR VAX
C
C
C CALL NMOVBYT(INARY,INOFF,OUARY,OUOFF,LEN)
C
C	INARY:	BYTE ARRAY
C	INOFF:	STARTING OFFSET
C	OUARY:	BYTE ARRAY FOR OUTPUT
C	OUOFF:	STARTING OFFSET
C	LEN:	# OF BYTES TO MOVE
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE NMOVBYT(INARY,INOFF,OUARY,OUOFF,LEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE	'INCLIB:SYSEXTRN.DEF'
C
	BYTE	    INARY(*)
	INTEGER*4   INOFF
	BYTE	    OUARY(*)
	INTEGER*4   OUOFF
	INTEGER*4   LEN
C
	INTEGER*4   K
C
C
	IF(INOFF.LT.1 .OR. INOFF.GT.4)THEN
	  TYPE *,IAM(),'NMOVBYT - BAD INPUT OFFSET = ',INOFF
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
	IF(INOFF+LEN.LT.2 .OR. INOFF+LEN.GT.5)THEN
	  TYPE *,IAM(),'NMOVBYT - BAD INPUT,LENGTH = ',INOFF,LEN
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
C On concurrent, in an I4 value the most significant byte is 0, the least
C significant byte is 3.  On dec, the most significant byte is 3, the least
C is byte 0.
C
	DO 1100 K = 0, LEN-1
	  OUARY(OUOFF+K) = INARY(5-(INOFF+K))
1100	CONTINUE
C
	RETURN
	END
