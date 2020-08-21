C
C SUBROUTINE PERM3
C $Log:   GXAFXT:[GOLS]PERM3.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:23:26   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:17:06   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_perm3.for **
C
C PERM3.FOR
C
C V01 26-NOV-90 ??? RELEASE FOR VAX BASELINE.
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
	SUBROUTINE PERM3(NUM,CMBTAB,CNT)
	IMPLICIT NONE
C
	INTEGER*4 CMBTAB(*),P6(3,6),P3(3,3),DIG(3)
	INTEGER*4 N, NUM, I, SAME, J, CNT, TEMP
C
	DATA P6 /1,2,3, 1,3,2, 2,1,3, 2,3,1, 3,1,2, 3,2,1/
	DATA P3 /1,2,3, 2,1,3, 2,3,1/
C
C
	N=NUM
	DO 10 I=1,3
	DIG(I)=MOD(N,10)
	N=N/10
10	CONTINUE
C
C
	SAME=0
        DO 20 I=1,3
        DO 21 J=1,I-1
        IF(DIG(I).EQ.DIG(J)) SAME=SAME+1
21	CONTINUE
20      CONTINUE
C
	IF(SAME.EQ.0) THEN
	  CNT=6
	  DO 30 I=1,6
	  CMBTAB(I)=DIG(P6(1,I))*100+DIG(P6(2,I))*10+DIG(P6(3,I))
30        CONTINUE
          RETURN
        ENDIF
C
C
	IF(SAME.EQ.1) THEN
	  IF(DIG(1).EQ.DIG(2)) THEN
	    TEMP=DIG(3)
	    DIG(3)=DIG(1)
	    DIG(1)=TEMP
	  ELSEIF(DIG(1).EQ.DIG(3)) THEN
	    TEMP=DIG(2)
	    DIG(2)=DIG(1)
	    DIG(1)=TEMP
	  ENDIF
	  CNT=3
	  DO 40 I=1,3
          CMBTAB(I)=DIG(P3(1,I))*100+DIG(P3(2,I))*10+DIG(P3(3,I))
40        CONTINUE
	  RETURN
	ENDIF
C
C
	CNT=1
	CMBTAB(1)=NUM
	RETURN
	END
