C
C SUBROUTINE ISORT5
C $Log:   GXAFXT:[GOLS]ISORT5.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:41:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:42:58   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - isort5.for **
C
C ISORT5.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C THIS WILL PERFORM A SHELL SORT OF A 5-INTEGER*4 I*4 ARRAY
C
C
C CALLING SEQUENCE:
C     CALL ISORT5(I4ARAY,CNT,IND)
C INPUT
C     CNT   - NUMBER OF ITEMS TO SORT
C     I4ARAY - ARRAY OF ALPHAS IN I*4 FORMAT
C     INDEX-INDEX TO TO SORT ON
C OUTPUT
C     NONE
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
	SUBROUTINE ISORT5(I4ARAY,CNT,IND)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 I4TEMP, XX, L, I, K, J, M, IND, CNT
	INTEGER*4 I4ARAY(5,*)
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
	IF(I4ARAY(IND,I) .LT. I4ARAY(IND,L))GO TO 210
	IF(I4ARAY(IND,I) .GT. I4ARAY(IND,L))GO TO 170
	GO TO 210
C
170	CONTINUE
	DO 175 XX=1,5
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
