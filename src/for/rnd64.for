C
C SUBROUTINE RND64
C
C V03 18-JUN-93 HJK SEPERATED FROM UNIRAN64 FOR PVCS
C V02 01-AUG-90 XXX RELEASED FOR VAX
C V01 ??-???-88 WS  INITIAL RELEASE
C
C C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
C ************************************************ 
C *                                              *
C * WARNING: ANY CHANGES IN THIS FILE MUST BE    *
C *          CO-ORDINATED WITH INV64.FOR         *
C *                                              *
C ************************************************
C
C
C SUBROUTINE RND64(N,G,P,L,D)    ;RANDOMIZER
C
C     N - NUMBER       (0-L)     INPUT/OUTPUT PARAMETER
C                                NUMBER SHOULD NEVER BE HIGHER THAN
C                                LIMIT (L)
C     G - GAME         (0-63)    INPUT PARAMETER
C     P - PARAMETER    (0-8**8/L) INPUT PARAMETER
C     L - LIMIT        (0-8**8)   INPUT PARAMETER
C     D - NUMBER OF OCTAL DIGITS (1-8)  INPUT PARAMETER
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE RND64(N,G,P,L,D)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 N1, TEMP, K, M3, M2, M1, M, J, D1
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
C     FORWARD RANDOMIZER
C
C     N - NUMBER       (0-L)
C     G - GAME         (0-63)
C     P - PARAMETER    (0-8**8/L)
C     L - LIMIT        (0-8**8)
C     D - NUMBER OF OCTAL DIGITS (1-8)
C
	DD=8**D
	D1=DD/8
100	DO 200 J=1,D
	   M=DD*P+N                  !PRECALCULATE DD*P
	   M1=ISHFT(M,-6)                         !M1=M/64
	   M2=ISHFT(M1,-6)                        !M2=M/4096
	   M3=ISHFT(M2,-6)                        !M3=M/262144
	   K=IAND(M,63)               !REMAINDER OF M1 DIV
	   K=F(K)
	   K=IAND((K+M1),63)
	   K=F(K)
	   K=IAND((K+M2),63)
	   K=F(K)
	   K=IAND((K+M3),63)              !COULD BE AN IF
	   K=F(K)
	   K=IAND((K+G),63)              !COULD BE AN IF
	   K=F(K)
	   TEMP=ISHFT(M1,6)
	   N1=IAND(TEMP+K,DD-1)
	   N1=ISHFT(N1,-3)
C
	   N=D1*IAND(K,7)+N1
200	CONTINUE
	IF(N.GT.L) GOTO 100
	RETURN
        END
