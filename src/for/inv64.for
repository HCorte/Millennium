C
C SUBROUTINE INV64
C
C INV64.FOR
C
C V03 18-JUN-93 HJK SPERATED FROM UNIRAN64 FOR PVCS
C V02 01-AUG-90 XXX RELEASED FOR VAX
C V01 ??-???-88 WS  INITIAL RELEASE
C
CC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C     SUBROUTINE INV64(N,G,P,L,D)         INVERSE RANDOMIZER
C
C     N - NUMBER       (0-L)              INPUT/OUTPUT PARAMETER
C                                         NUMBER SHOULD NEVER BE HIGHER THAN
C                                         LIMIT (L)
C     G - GAME         (0-63)             INPUT PARAMETER
C     P - PARAMETER    (0-8**8/L)         INPUT PARAMETER
C     L - LIMIT        (0-8**8)           INPUT PARAMETER
C     D - NUMBER OF OCTAL DIGITS (1-8)    INPUT PARAMETER
C
C

C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE INV64(N,G,P,L,D)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 X, N1, TEMP, K, M3, M2, M1, M, J, D1
	INTEGER*4 DD, D, L, P, G, N
C
	INTEGER*4 FF(64),F(0:63)          !FORWARD TABLE
	EQUIVALENCE (FF(1),F(0))
	INTEGER*4 RR(64),R(0:63)          !REVERSE TABLE
	EQUIVALENCE (RR(1),R(0))
	DATA FF/49,29,46,21,19,11,27,20,
     *	        56,18,13,57,06,16,41,08,
     *	        40,48,02,23,53,60,35,03,
     *	        51,15,00,01,25,17,47,30,
     *	        09,45,55,31,43,59,37,24,
     *	        05,58,04,14,44,50,42,28,
     *	        12,38,10,33,54,39,63,36,
     *	        52,32,26,61,07,62,22,34/
C
	DATA RR/26,27,18,23,42,40,12,60,
     *	        15,32,50,05,48,10,43,25,
     *	        13,29,09,04,07,03,62,19,
     *	        39,28,58,06,47,01,31,35,
     *	        57,51,63,22,55,38,49,53,
     *	        16,14,46,36,44,33,02,30,
     *	        17,00,45,24,56,20,52,34,
     *	        08,11,41,37,21,59,61,54/
C
C
C  INVERSE RANDOMIZER
C
C  N GIVES BACK SER #
C
	X=8**8
	DD=8**D
	D1=DD/8
300	DO 400 J=1,D
	   N1=N/D1
	   TEMP=ISHFT(N,3)
	   N=IAND(TEMP+N1,DD-1)
	   M=DD*P+N
	   M1=ISHFT(M,-6)
	   M2=ISHFT(M1,-6)
	   M3=ISHFT(M2,-6)
	   K=IAND(M,63)
	   K=R(K)
	   K=IAND((X+K-G),63)
	   K=R(K)
	   K=IAND((X+K-M3),63)
	   K=R(K)
	   K=IAND((X+K-M2),63)
	   K=R(K)
	   K=IAND((X+K-M1),63)
	   K=R(K)
	   TEMP=ISHFT(M1,6)
	   N=IAND(TEMP+K,DD-1)
400	CONTINUE
	IF(N.GT.L) GOTO 300
	RETURN
	END
