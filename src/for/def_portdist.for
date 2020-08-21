C
C SUBROUTINE DEF_PORTDIST
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DEF_PORTDIST.FOV                             $
C  $Date::   17 Apr 1996 12:50:30                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2loddef.for;1 **
C
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DEF_PORTDIST( PREV_NUM_OF_PORTS, ACT_NUM_OF_PORTS )
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2NETDEF.DEF'
C
	INTEGER*4 PREV_NUM_OF_PORTS, ACT_NUM_OF_PORTS
	INTEGER*4 I, J, K, M, N
C
	IF (PREV_NUM_OF_PORTS .GE. ACT_NUM_OF_PORTS)  RETURN
	IF (PREV_NUM_OF_PORTS .EQ. 0) THEN
	  DO 2220 I = 1, X2X_MAXNODES
	    DO 2110 J = 0, 200
	      POASTONO(I,J) = 0
2110	    CONTINUE
2220	  CONTINUE
	ENDIF
	K = ACT_NUM_OF_PORTS / NUM_OF_NODES
	IF (K .LT. 2 ) THEN
	  TYPE *, '   TOO FEW PORTS  OR  TOO MANY NODES ',
     *	          ACT_NUM_OF_PORTS,NUM_OF_NODES
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
	M = PREV_NUM_OF_PORTS + 1
2330	CONTINUE
	I = 0
	N = 0
2440	CONTINUE
	I = I + 1
	IF (POASTONO(I,0) .LT. K) THEN
	  POASTONO(I,0) = POASTONO(I,0) + 1
	  POASTONO( I,POASTONO(I,0) ) = M
	  M = M + 1
	  N = N + 1
	ENDIF
	IF (M .GT. ACT_NUM_OF_PORTS) GO TO 2550
	IF (I .LT. NUM_OF_NODES) GO TO 2440
	IF (N .EQ. 0) K = K + 1
	GO TO 2330
2550	CONTINUE
	NUM_OF_PORTS = ACT_NUM_OF_PORTS
	RETURN
	END
