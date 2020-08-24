C
C SUBROUTINE PERM4
C $Log:   GXAFXT:[GOLS]PERM4.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:23:30   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:17:12   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_perm4.for **
C
C PERM4.FOR
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
	SUBROUTINE PERM4(NUM,CMBTAB,CNT)
	IMPLICIT NONE
C
	INTEGER*4 CMBTAB(*),P24(4,24),P12(4,12),P6(4,6),P4(4,4),DIG(4)
	INTEGER*4 N, NUM, I, SAME, J, CNT, TEMP
C
	DATA P24/1,2,3,4, 1,2,4,3, 1,3,2,4, 1,3,4,2, 1,4,2,3, 1,4,3,2,
     *           2,1,3,4, 2,1,4,3, 2,3,1,4, 2,3,4,1, 2,4,1,3, 2,4,3,1,
     *           3,1,2,4, 3,1,4,2, 3,2,1,4, 3,2,4,1, 3,4,1,2, 3,4,2,1,
     *           4,1,2,3, 4,1,3,2, 4,2,1,3, 4,2,3,1, 4,3,1,2, 4,3,2,1/
	DATA P12/1,2,3,4, 1,3,2,4, 1,3,4,2, 2,1,3,4, 2,3,1,4, 2,3,4,1,
     *           3,1,2,4, 3,1,4,2, 3,2,1,4, 3,2,4,1, 3,4,1,2, 3,4,2,1/
	DATA P6 /1,2,3,4, 1,3,2,4, 3,1,4,1, 3,4,1,2, 1,3,4,2, 3,1,2,4/
	DATA P4 /1,2,3,4, 2,1,3,4, 2,3,1,4, 2,3,4,1/
C
C
	N=NUM
	DO 10 I=1,4
	DIG(I)=MOD(N,10)
	N=N/10
10	CONTINUE
C
C
	SAME=0
        DO 20 I=1,4
        DO 20 J=1,I-1
        IF(DIG(I).EQ.DIG(J)) SAME=SAME+1
20      CONTINUE
C
	IF(SAME.EQ.0) THEN
	  CNT=24
	  DO 30 I=1,24
	  CMBTAB(I)=DIG(P24(1,I))*1000+DIG(P24(2,I))*100+
     *              DIG(P24(3,I))*10+DIG(P24(4,I))
30        CONTINUE
          RETURN
        ENDIF
C
C
	IF(SAME.EQ.1) THEN
	  IF(DIG(1).EQ.DIG(2)) THEN
	    TEMP=DIG(4)
	    DIG(4)=DIG(1)
	    DIG(1)=TEMP
	    TEMP=DIG(3)
	    DIG(3)=DIG(2)
	    DIG(2)=TEMP
	  ELSEIF(DIG(2).EQ.DIG(3)) THEN
            TEMP=DIG(2)
	    DIG(2)=DIG(4)
	    DIG(4)=TEMP
	  ELSEIF(DIG(1).EQ.DIG(4)) THEN
	    TEMP=DIG(1)
	    DIG(1)=DIG(3)
	    DIG(3)=TEMP
	  ENDIF
	  CNT=12
          DO 40 I=1,12
          CMBTAB(I)=DIG(P12(1,I))*1000+DIG(P12(2,I))*100+
     *              DIG(P12(3,I))*10+DIG(P12(4,I))
40        CONTINUE
	  RETURN
	ENDIF
C
C
	IF(SAME.EQ.2) THEN
	  IF(DIG(1).EQ.DIG(4)) THEN
	    TEMP=DIG(1)
	    DIG(1)=DIG(3)
	    DIG(3)=TEMP
	  ELSEIF(DIG(1).EQ.DIG(3)) THEN
	    TEMP=DIG(2)
	    DIG(2)=DIG(3)
	    DIG(3)=TEMP
	  ENDIF
          CNT=6
          DO 50 I=1,6
          CMBTAB(I)=DIG(P6(1,I))*1000+DIG(P6(2,I))*100+
     *              DIG(P6(3,I))*10+DIG(P6(4,I))
50        CONTINUE
          RETURN
        ENDIF
C
C
	IF(SAME.EQ.3) THEN
	  IF(DIG(1).EQ.DIG(4)) THEN
	    IF(DIG(2).EQ.DIG(4)) THEN
	      TEMP=DIG(3)
	      DIG(3)=DIG(1)
	      DIG(1)=TEMP
	    ELSE
              TEMP=DIG(2)
	      DIG(2)=DIG(1)
	      DIG(1)=TEMP
	    ENDIF
	  ELSEIF(DIG(1).EQ.DIG(2)) THEN
	    TEMP=DIG(1)
	    DIG(1)=DIG(4)
	    DIG(4)=TEMP
	  ENDIF
	  CNT=4
      	  DO 60 I=1,4
          CMBTAB(I)=DIG(P4(1,I))*1000+DIG(P4(2,I))*100+
     *              DIG(P4(3,I))*10+DIG(P4(4,I))
60        CONTINUE
          RETURN
        ENDIF
C
C
	CNT=1
	CMBTAB(1)=NUM
	RETURN
	END
