C
C FUNCTION RAN2
C
C V02 08-JUN-2000 UXN IMPLICIT NONE added.
C V01 21-JAN-1993 DAB Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C
C   RETURNS A UNIFORM RANDOM DEVIATE BETWEEN 0.0 AND 1.0.
C   SET  IDUM  TO ANY NEGATIVE VALUE TO INITIALIZE OR
C   REINITIALIZE THE SEQUENCE
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	INTEGER*4 FUNCTION RAN2(IDUM)
	IMPLICIT NONE
C
	INTEGER*4 M,IA,IC,RM
	PARAMETER (M=714025, IA=1366, IC=150889, RM=1.0/M)
	INTEGER*4 IR(97)
	INTEGER*4 IFF,IY,IDUM,J

	DATA IFF /0/,IY/0/
	IF(IDUM .LT. 0 .OR. IFF .EQ. 0) THEN
	  IFF = 1
	  IDUM = MOD(IC - IDUM, M)
	  DO 2110 J=1,97
	    IDUM = MOD(IA*IDUM + IC, M)
	    IR(J) = IDUM
2110	    CONTINUE
	  IDUM = MOD(IA*IDUM + IC, M)
	  IY = IDUM
	ENDIF
	J = 1 + (97*IY)/M
	IF (J .GT. 97 .OR. J .LT. 1)  CALL GPAUSE
	IY = IR(J)
	IDUM = MOD(IA*IDUM + IC, M)
	IR(J) = IDUM
	RAN2 = IY*RM
	RETURN
	END
