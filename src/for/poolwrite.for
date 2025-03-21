C
C SUBROUTINE POOLWRITE
C $Log:   GXAFXT:[GOLS]POOLWRITE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:26:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:20:10   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - poolmgr.for **
C
C
C     POOLWRITE
C
C     WRITE POOL INFO FROM MEMORY
C
C+++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE POOLWRITE
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INTEGER*4 PFDB(7),FDB(7), ST
C
C
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
C
	CALL CLOSEFIL(FDB)
C
10	CONTINUE
	IF(SCFSFN(1,LP1).EQ.'    ') CALL SYSVOL(SCFSFN(1,LP1))
	CALL OPENQW(3,SCFSFN(1,LP1),4,0,0,ST)
	IF(ST.NE.0) THEN
	  CALL FILERR(SCFSFN(1,LP1),1,ST,0)
	  CALL GPAUSE
	  GOTO 10
	ENDIF
C
	CALL IOQINIT(PFDB,3,LTOSEC*256)
C
C
	CALL WRITEQW(PFDB,LTNUMPAG+1,LTOVR,ST)
	IF (ST.NE.0) THEN
	   TYPE *,IAM(),'Could not write to pool1 file ',ST
	ENDIF
C
	CALL CLOSEQFIL(PFDB)
	RETURN
	END
