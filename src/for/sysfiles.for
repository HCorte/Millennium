C
C PROGRAM SYSFILES
C $Log:   GXAFXT:[GOLS]SYSFILES.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:24:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:46:58   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - sysfiles.for **
C
C SYSFILES.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C PROGRAM TO ALLOCATE AND CLEAR FILES FOR INITIAL SYSTEM STARTUP
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
	PROGRAM SYSFILES
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INTEGER*4 FDB(7),SCFNAM(5), I, ST, NOCHECK0
	DATA SCFNAM/'SCF.','FIL ',3*'    '/
	COMMON /NOCHECK0/ NOCHECK0
C
	CALL COPYRITE
C
	NOCHECK0=-1
C
C
	CALL OPENW(1,SCFNAM,4,0,0,ST)
	CALL IOINIT(FDB,1,SCFSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFNAM,1,ST,0)
	CALL READW(FDB,1,SCFREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFNAM,2,ST,1)
	CALL USRCLOS1 (     1)
C
C IF NO FILE VOLUME NAMES THEN SET TO SYSTEM VOLUME
C
	DO 10 I=1,MAXFIL
	IF(SCFSFN(1,I).EQ.'    ') CALL SYSVOL(SCFSFN(1,I))
	IF(SCFFSZ(I).NE.0) THEN
	  CALL CRTFIL(SCFSFN(1,I),SCFFSZ(I),ST)
	  IF(ST.NE.0) CALL GPAUSE
	ENDIF
10	CONTINUE
	DO 20 I=1,MAXGAM
	IF(SCFGFN(1,I).EQ.'    ') CALL SYSVOL(SCFGFN(1,I))
	IF(SCFGSZ(I).NE.0) THEN
	  CALL CRTFIL(SCFGFN(1,I),SCFGSZ(I),ST)
	  IF(ST.NE.0) CALL GPAUSE
	ENDIF
20	CONTINUE
	CALL GSTOP(GEXIT_SUCCESS)
	END
