C
C SUBROUTINE MAPSTF
C $Log:   GXAFXT:[GOLS]MAPSTF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:00:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:58:38   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_bitmap.for **
C
C BITMAP.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
C  INPUT: ADDRESS (STARTING ADDRESS) MUST BE I*2 OR I*4
C         NBYTE (NUMBER BYTES)
C         NUMS (ARRAY OF NUMBERS)
C         N (NUMBER OF NUMBERS IN ARRAY NUMS)
C
C  CALLING SEQUENCE: CALL MAP(ADDRES,NUMS,N)
C  ADDRESS MUST BE I*2 OR I*4 ARRAY OF AT LEAST N/8 BYTES
C  NUMS MUST BE I*4 ARRAY DIMENSION NUMS(N)
C
C  CALLING SEQUENCE: CALL UNMAP(ADDRES,NBYTE,NUMS)
C  ADDRESS MUST BE AS IN MAP.
C  NBYTE IS NUMBER OF BYTES IN ADDRESS TO SCAN (TEST).
C  NUMS MUST BE I*4 ARRAY CAPABLE OF HOLDING ALL BITS ON.
C  I.E. IF YOU EXPECT NO MORE THAN 7 BITS ON, NUMS COULD BE
C  NUMS(7)
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MAPSTF(ADDRES,NBYTE,NUMS,N)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
C
	INTEGER*2   ADDRES(4)
	INTEGER*4   NBYTE
	INTEGER*4   N
	INTEGER*4   NUMS(N)
C
	INTEGER*4   MX,X,J,COUNT
C
	LOGICAL SET1
C
C*****
C I DON'T THINK ANYBODY COMES FROM HERE...
C******
	IF (NBYTE.GT.0) GOTO 2
	IF (N.EQ.0) RETURN
C******
C ENTRY MAP(...)
C******
	ENTRY MAP(ADDRES,NBYTE,NUMS,N)
	SET1=.FALSE.
C
C INITIALIZE BYTES
C
	DO 50 J=1,NBYTE/2
	  ADDRES(J)=0
50	CONTINUE
C
	MX=NBYTE*8-1
	DO 100 J=1,N
	   X=NUMS(J)-1
	   IF (X.GT.MX) GOTO100 !INVALID ELEMENT
	   IF (X.LT.0) GOTO100 ! INVALID ELEMENT
	   IF (X.EQ.0) SET1=.TRUE.
	   IF (X.GE.0) CALL BSET(ADDRES,X)
100	CONTINUE
	IF (SET1) CALL BSET(ADDRES,0)
	RETURN
C******
C ENTRY UNMAP(...)
C******
	ENTRY UNMAP(ADDRES,NBYTE,NUMS,N)
2	X=1
	COUNT=(NBYTE*8)-1
	DO 200 J=0,COUNT
	   IF (TSBIT(ADDRES,J)) THEN
	      NUMS(X)=J+1
	      X=X+1
	      IF (X.GT.N) RETURN !FILLED IT UP
	   END IF
200	CONTINUE
	RETURN
	END
