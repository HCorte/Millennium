C
C SUBROUTINE I4MSHELL
C
C I4MSHELL.FOR
C
C V03 10-NOV-97 UXN Produced from I4SHELL
C V02 12-APR-91 TKO ADDED I1SHELL TO SORT ASCII STRINGS (SAME CALLING SEQUENCE)
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C ***** I4SHELL
C
C THIS WILL PERFORM A SHELL SORT OF A INTEGER*4'S IN AN I*4 ARRAY
C
C CALLING SEQUENCE:
C     CALL I4MSHELL(CNT,I4ARAY,INTEGER*4)
C INPUT
C     CNT       - NUMBER OF ITEMS TO SORT
C     I4ARAY    - ARRAY OF INTEGER*4 VALUES TO SORT
C     INTEGER*4 - FIRST DIMENSION OF I*4 ARRAY
C OUTPUT
C     INTEGER*4 - SORTED IN ASCENDING ORDER
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE I4MSORT(CNT,I4ARAY,LEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 XX, ZZ, L, I, K, J, M, LEN, CNT
	INTEGER*4 I4ARAY(LEN,*)
	INTEGER*4 I4TEMP
C
C
	M=CNT
C
20	CONTINUE
	M=M/2
	IF(M.EQ.0) GO TO 8000
	J=1
	K=CNT-M
C
140	CONTINUE
	I=J
C
150	CONTINUE
	L=I+M
	DO 160 ZZ=1,LEN
	  IF(I4ARAY(ZZ,I) .LT. I4ARAY(ZZ,L))GO TO 210
	  IF(I4ARAY(ZZ,I) .GT. I4ARAY(ZZ,L))GO TO 170
160	CONTINUE
	GO TO 210
C
170	CONTINUE
	DO 175 XX=ZZ,LEN
	  I4TEMP=I4ARAY(XX,I)
	  I4ARAY(XX,I)=I4ARAY(XX,L)
	  I4ARAY(XX,L)=I4TEMP
175	CONTINUE
	I=I-M
	IF(I.GE.1)GO TO 150
C
210	CONTINUE
	J=J+1
	IF(J.GT.K)GO TO 20
	GO TO 140
C
C
8000	CONTINUE
	RETURN
	END
