C
C PROGRAM CHKPNT
C $Log:   GXAFXT:[GOLS]CHKPNT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:32:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   09 Mar 1993 15:42:16   EBD
C  Added G. Arpander Fix for checkpoint.  Otherwise previous fix
C  can cause performance problems when chkpnt is initiated.
C  
C     Rev 1.1   05 Mar 1993 15:52:52   EBD
C  Removed comment from chkpnt write, otherwise restart from checkpoint
C  does not work.
C  
C     Rev 1.0   21 Jan 1993 15:50:22   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - chkpnt.for **
C
C CHKPNT.FOR
C
C V01 07-OCT-91 MTK INITAL RELEASE FOR NETHERLANDS
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
C Copyright 1991,1992,1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM CHKPOINT
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CHKPNT.DEF'
	INCLUDE 'INCLIB:CHKCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
C
	INTEGER*4 FDB(7),FDB1(7),CFILES(0:4)
	DATA CFILES/CP0,CP1,CP2,CP3,CP4/

	LOGICAL FIRSTTIME
	DATA FIRSTTIME /.TRUE./
C
	INTEGER*4 LENGTH, SFIL, FILE, ST, START, I
C
C
	CALL COPYRITE
	CALL SNIF_AND_WRKSET
C
C CHECK IF THERE IS A CHECKPOINT PENDING
C

10	CONTINUE
	P(CHKFLG)=0
	CALL CLOSEQFIL(FDB)
	CALL CLOSEQFIL(FDB1)
20	CONTINUE
	IF(P(CHKFLG).NE.0) GOTO 30
	IF(LCHKPNT.EQ.LLODCHK) GOTO 30
	IF(DAYSTS.NE.DSOPEN) CALL GSTOP(GEXIT_SUCCESS)
	CALL HOLD(0,ST)
	GOTO 20
30	CONTINUE
C
C     IF LOTTO CHECKPOINT OUTSTANDING DO IT
C
	IF (LCHKPNT.EQ.LLODCHK) CALL OVRCHKPNT
	IF(P(CHKFLG).EQ.0) GOTO 20
C
C OPEN CHECKPOINT FILE
C
	FILE=IABS(P(CHKFLG))
        SFIL=CFILES(FILE)
        IF(MOD(FILE,2).EQ.0) GOTO 100   
C
C
	CALL OPENQW(1,SFNAMES(1,SFIL),4,0,0,ST)	!IE. CHK1, CHK2, CHK3, CHK4
	CALL IOQINIT(FDB,1,CHKSEC*256)
	IF(ST.NE.0) THEN
	  CALL CHKMES(3,SFNAMES(1,SFIL),ST,0)
	  GOTO 10
	ENDIF
C
C CLEAR CHECKPOINT HEADER IN FILE
C
	CHKHEADER(CHKPNTDAY)=0
	CALL WRITEQW(FDB,1,CHKHEADER,ST)
	IF(ST.NE.0)THEN
	  CALL CHKMES(5,SFNAMES(1,SFIL),ST,1)
	  GOTO 10
	ENDIF
C
C GET CHECKPOINT HEADER FROM CHKCOM TO DETERMINE LENGTH AND
C CALCULATE STARTING "VAX" SECTOR TO WRITE CHECKPOINT IMAGE
C
	START=CHKSEC*256/SECSIZE+1
        CALL FASTMOV(CHKTAB(1,1),CHKHEADER,2048)
	LENGTH=(CHKHEADER(CHKGAMLEN)-1)*CHKSEC*256

	IF (FIRSTTIME) THEN
	    DO 40 I=2,CHKHEADER(CHKGAMLEN)
              CALL WRITEQW(FDB,I,CHKTAB(1,I),ST)
	      IF(ST.NE.0)THEN
	         CALL CHKMES(5,SFNAMES(1,SFIL),ST,I)
	         GOTO 10
	      ENDIF
40	    CONTINUE
	    FIRSTTIME = .FALSE.
	ELSE
	    CALL WRITEQIO(FDB,START,CHKTAB(1,2),LENGTH,ST)
	ENDIF

200	CONTINUE
C
C WRITE CHECKPOINT HEADER
C
	CALL WRITEQW(FDB,1,CHKHEADER,ST)
	IF(ST.NE.0) THEN
	  CALL CHKMES(5,SFNAMES(1,SFIL),ST,1)
	  GOTO 10
	ENDIF
C
C
100	CONTINUE
	CALL CHKMES(6,SFNAMES(1,SFIL),0,0)
	GOTO 10
	END
