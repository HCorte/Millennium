C
C SUBROUTINE I4SHELL
C $Log:   GXAFXT:[GOLS]I4SHELL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:33:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:36:18   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_i4shell.for **
C
C I4SHELL.FOR
C
C V02 12-APR-91 TKO ADDED I1SHELL TO SORT ASCII STRINGS (SAME CALLING SEQUENCE)
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C ***** I4SHELL
C
C THIS WILL PERFORM A SHELL SORT OF A INTEGER*4'S IN AN I*4 ARRAY
C
C CALLING SEQUENCE:
C     CALL I4SHELL(SORT,CNT,I4ARAY,INTEGER*4)
C INPUT
C     SORT      - SORT LIST OF POINTERS.
C     CNT       - NUMBER OF ITEMS TO SORT
C     I4ARAY    - ARRAY OF INTEGER*4 VALUES TO SORT
C     INTEGER*4 - FIRST DIMENSION OF I*4 ARRAY
C OUTPUT
C     SORT      - SORTED LIST OF POINTERS.
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE I4SHELL(SORT,CNT,I4ARAY,LEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 TEMP, XX, ZZ, L, I, K, J, M, LEN, CNT
	INTEGER*4 SORT(*)
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
	TEMP=SORT(I)
	SORT(I)=SORT(L)
	SORT(L)=TEMP
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
