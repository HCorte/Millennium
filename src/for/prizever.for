C
C PROGRAM PRIZEVER
C $Log:   GXAFXT:[GOLS]PRIZEVER.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:29:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:21:36   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - prizever.for **
C
C PRIZEVER.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C LOTTERY RESULTS ENTRY PROGRAM
C
C
C V01 01-FEB-89 MTK INITIAL RELEASE FOR SWEDEN
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
	PROGRAM PRIZEVER
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C
	INTEGER*4 K, DRAW, GNUM, GIND, GTYP, ST
C***	CHARACTER*17 OPTIONS
C***	CHARACTER*8 LOTDEV
C***	INTEGER*4 I4OPT(5),
C***	EQUIVALENCE (OPTIONS,LOTDEV,I4OPT)
C
C
	CALL COPYRITE
C
C
100	CONTINUE
	IF(PRZREADY.EQ.0) THEN
	  CALL XWAIT(5,2,ST)
	  GOTO 100
	ENDIF
C
  	GTYP=CUR_GTYP
	GIND=CUR_GIND
	GNUM=CUR_GNUM
	DRAW=CUR_DRAW
C
C***	CALL GETOPTS(OPTIONS,LENGTH)
C***	CALL OPENW(5,LOTDEV,4,0,0,ST)
C***	CALL ASCBIN(I4OPT,9,1,GTYP,ST)
C***	CALL ASCBIN(I4OPT,10,1,GIND,ST)
C***	CALL ASCBIN(I4OPT,11,2,GNUM,ST)
C***	CALL ASCBIN(I4OPT,13,5,DRAW,ST)
	WRITE(5,910) IAM(),GTNAMES(GTYP),GIND,(SCFLGN(K,GNUM),K=1,4),DRAW
C
C WAIT FOR OPERATOR ENTRY TO FINISH
C
10	CONTINUE
	IF(OPDONE.EQ.0) THEN
	  TYPE*,IAM(),'Waiting for operator entry'
	  CALL XWAIT(5,2,ST)
	  GOTO 10
	ENDIF
C
C
	IF(GTYP.EQ.TLTO) CALL PRZVER(GNUM,GIND,DRAW,ST)
C	IF(GTYP.EQ.TSPT) CALL SPTVER(GNUM,GIND,DRAW,ST)
C       IF(GTYP.EQ.TNBR) CALL NBRVER(GNUM,GIND,DRAW,ST)
	IF(ST.NE.0) GOTO 10
	WRITE(5,920) IAM(),GTNAMES(GTYP),GIND
	CALL GSTOP(GEXIT_SUCCESS)
910	FORMAT(1X,A18,1X,A8,I1,2X,4A4,'Draw/Event ',I5,/)
920	FORMAT(1X,A18,1X,A8,I1,' prize entry complete')
	END
