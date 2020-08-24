C SUBROUTINE FASTSET
C
C V05 30-MAY-2011 FJG New NFASTSET with LEN bytes
C
C $Log:   GXAFXT:[GOLS]FASTSET.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:07:40   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:15:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_fastset.for **
C
C VAX_FASTSET.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C FASTSET.FOR
C
C V01 16-JUL-90 TKO  RELEASED FOR VAX
C
C This emulates the FASTSET.MAC routine on Concurrent
C
C CALL FASTSET(VALUE,OUARY,LEN)
C
C	VALUE:	VALUE TO STORE
C	OUARY:	I*4 ARRAY FOR OUTPUT
C	LEN:	# OF I*4 WORDS TO MOVE
C
C CALL NFASTSET(OUARY,LEN)
C
C	OUARY:	I*4 ARRAY FOR OUTPUT
C	LEN:	# OF I*1 WORDS TO INIT (Bytes)
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
	SUBROUTINE FASTSET(VALUE,OUARY,LEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INTEGER*4   VALUE
	INTEGER*4   OUARY(*)
	INTEGER*4   LEN
C
	INTEGER*4   XLEN
	INTEGER*4   K
C
	IF(VALUE.NE.0)THEN
	  DO 1100 K = 1, LEN
	    OUARY(K) = VALUE
1100	  CONTINUE
	  GOTO 9000
	ENDIF
C
C Special case for 0 value (most common)
C
	XLEN = LEN*4
        GOTO 1900
C===============================================================================        
        ENTRY NFASTSET(OUARY,LEN)        
	XLEN = LEN
C        
1900    CONTINUE        	
	K = 1
2000	CONTINUE
	IF(XLEN.GT.64000)THEN
	  CALL LIB$MOVC5(1,0,0,64000,OUARY(K))
	  XLEN = XLEN-64000
	  K    = K + 16000
	  GOTO 2000
	ENDIF
C
	IF(XLEN.GT.0)THEN
	  CALL LIB$MOVC5(1,0,0,XLEN,OUARY(K))
	ENDIF
C
9000	CONTINUE
	RETURN
	END
