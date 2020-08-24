C
C SUBROUTINE PERM
C $Log:   GXAFXT:[GOLS]PERM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:23:22   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:17:02   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_perm.for **
C
C PERM.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-JUN-88 XXX RELEASED FOR MICHIGAN
C
C     PERM.FTN
C     ----------
C
C     INITIALISE PERMUTATION TABLE
C
C     THIS TABLE WILL ALLOW YOU TO CREATE A TABLE WHERE ALL DIFFRENT
C     PERMUTATION OF ALL 'NR' COMBINATION HAVE THE SAME ADDRESS
C     THIS IS TO SERVE TO MINIMISE MEMORY FOR ALL 'BOX' BETS
C
C
C     CALL PERM(PERTAB,MAXDIG,N,REPEAT)
C     IN - MAXDIG   - NR OF DIFFRENT 'DIGITS'+1
C          N       - NR OF DIGS IN PERMUTATION
C     REPEAT - 0 IF WITHOUT REPETITION
C     OUT- PERTAB  - FILLED
C
C     E.G. FOR 3 DIGIT NR GAME TO GET PER TABLE CALL PERM(PER,9,3,0)
C     FOR RI 4/40 GAME YOU CALL PERM(PER,39,3,1) - 39 BECAUSE THESE
C     'DIGITS' START FROM 1
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
	SUBROUTINE PERM(PERTAB,MAXNR,N,REPEAT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 J, I
	INTEGER*4 MAXNR   !# OF DIGITS
	INTEGER*4 N	  !# OF DIGS IN PERMUTATION
C
	INTEGER*4 PERTAB(0:MAXNR,N)
     *	 ,REPEAT  !0 IF REPETIONS ALLOWED IN PERMUTATION
C
	DO 5, I=0,MAXNR           !CLEAR TABLE FIRST
	DO 5, J=1,N
5	PERTAB(I,J)=0
C
C     INITIALISE TABLE
C
	DO 10, I=0,MAXNR
10	PERTAB(I,1)=I
C
	DO 20, J=2,N
	DO 20, I=1,MAXNR
	IF (REPEAT.NE.0) PERTAB(I,J)=PERTAB(I-1,J)+PERTAB(I-1,J-1)
	IF (REPEAT.EQ.0) PERTAB(I,J)=PERTAB(I-1,J)+PERTAB(I,J-1)
20	CONTINUE
	DO 30, I=0,MAXNR
30	PERTAB(I,1)=PERTAB(I,1)+1
C****    TYPE *,'1 - ',(PERTAB(II,1),II=0,60)
C****    TYPE *,'2 - ',(PERTAB(II,2),II=0,60)
C****    TYPE *,'3 - ',(PERTAB(II,3),II=0,60)
C****    TYPE *,'4 - ',(PERTAB(II,4),II=0,60)
C****    TYPE *,'5 - ',(PERTAB(II,5),II=0,60)
C****    TYPE *,'6 - ',(PERTAB(II,6),II=0,60)
	RETURN
	END
