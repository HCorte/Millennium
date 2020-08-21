C
C PROGRAM MRGTSK
C
C V03 13-NOV-97 Starting MRGVLF and MRGTCF simultaneously.
C
C $Log:   GXAFXT:[GOLS]MRGTSK.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:04:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   30 Jan 1994 21:56:58   HXK
C  FILE CLEARING AND ALLOCATION IS RUN FROM OTHER TASKS.
C  
C     Rev 1.0   21 Jan 1993 17:02:22   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - mrgtsk.for **
C
C MRGTSK.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-OCT-89 LOU R. INITIAL RELEASE FOR FINLAND
C
C INDIRECT FILE MERGE PROCEDURE FOR WINNER SELECTION
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
	PROGRAM MRGTSK
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INTEGER*4 K, FLAG, ST
	INTEGER*4 FDB(7)
	REAL*8 MRGLST(2)
	DATA MRGLST/'MRGVLF  ','MRGTCF  '/
	LOGICAL MRGVLF_FINISHED,MRGTCF_FINISHED
	INTEGER*4   TSKSTS
C
C
	CALL COPYRITE
C
	MRGVLF_FINISHED = .FALSE.
	MRGTCF_FINISHED = .FALSE.
C
	CALL OPENX(1,'SCF.FIL',4,0,0,ST)
	CALL IOINIT(FDB,1,SCFSEC*256)
	IF(ST.NE.0) THEN
	  TYPE*,'SCF.FIL open error > ',ST
	  CALL GPAUSE
	ENDIF
	CALL READW(FDB,1,SCFREC,ST)
	IF(ST.NE.0) THEN
	  TYPE*,'SCF.FIL read error > ',ST
	  CALL GPAUSE
	ENDIF
	CALL CLOSEFIL(FDB)
C
C
	CALL PRMYESNO(
     *	 'Are you sure you want winner selection file merge [Y/N]? ',FLAG)
	IF(FLAG.NE.1) CALL GSTOP(GEXIT_SUCCESS)
C
C CREATE NEW FILES
C
	IF(SCFSFN(1,VLF).NE.SCFSFN(1,VLC)) THEN
	  TYPE*,'Validation copy file must have same volume as vlf'
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
	IF(SCFSFN(1,TCF).NE.SCFSFN(1,TCC)) THEN
	  TYPE*,'Carryover copy file must have same volume as tcf'
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
	IF(SCFFSZ(VLC).NE.SCFFSZ(VLF)) THEN
	  TYPE*,'Validation copy file must be same size as vlf'
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
C
C***	CALL CRTFIL(SCFSFN(1,VLC),SCFFSZ(VLC),ST)
C***	IF(ST.NE.0) THEN
C***	  WRITE(5,904) IAM(),(SCFSFN(K,VLC),K=1,5)
C***	  CALL GPAUSE
C***	ENDIF
C
C***	CALL CRTFIL(SCFSFN(1,TCC),SCFFSZ(TCC),ST)
C***	IF(ST.NE.0) THEN
C***	  WRITE(5,904) IAM(),(SCFSFN(K,TCC),K=1,5)
C***	  CALL GPAUSE
C***	ENDIF
C
C LOAD AND START VLF AND TCF MERGE PROGRAM
C
	WRITE(5,901) IAM(),MRGLST(1)
	CALL NRUNTSK(MRGLST(1))
	CALL NRUNTSK(MRGLST(2))
10	CONTINUE
	CALL STTSK(MRGLST(1),TSKSTS,ST)
	IF(ST.EQ.4.AND..NOT.MRGVLF_FINISHED) THEN
	    MRGVLF_FINISHED = .TRUE.
	    CALL PRMYESNO('Did MRGVLF run ok [Y/N]? ',FLAG)
	    IF(FLAG.NE.1) THEN
		WRITE(5,900) IAM(),(SCFSFN(K,VLC),K=1,5)
		CALL GPAUSE
	    ENDIF
	    CALL FMAINT(VLF,VLC,ST)
	    WRITE(5,902) IAM()
	ENDIF
	CALL STTSK(MRGLST(2),TSKSTS,ST)
	IF(ST.EQ.4.AND..NOT.MRGTCF_FINISHED) THEN
	    MRGTCF_FINISHED = .TRUE.
	    CALL PRMYESNO('Did MRGTCF run ok [Y/N]? ',FLAG)
	    IF(FLAG.NE.1) THEN
		WRITE(5,903) IAM(),(SCFSFN(K,TCC),K=1,5)
		CALL GPAUSE
	    ENDIF
	    CALL FMAINT(TCF,TCC,ST)
	    WRITE(5,902) IAM()
	ENDIF
	IF(.NOT.MRGVLF_FINISHED.OR..NOT.MRGTCF_FINISHED) THEN
	    CALL XWAIT(2,2,ST)
	    GOTO 10
	ENDIF
	CALL GSTOP(GEXIT_SUCCESS)
C
C
900	FORMAT(1X,A,'Initialize ',5A4,' and rerun MRGVLF ')
901	FORMAT(1X,A,'Begining execution of ',A8)
902	FORMAT(1X,A,'Validation file file merge complete')
903	FORMAT(1X,A,'Initialize ',5A4,' and rerun MRGTCF')
904	FORMAT(1X,A,'Error while allocating ',5A4)
	END
