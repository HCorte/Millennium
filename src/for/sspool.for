C
C SUBROUTINE SSPOOL
C $Log:   GXAFXT:[GOLS]SSPOOL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:18:18   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:43:42   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - sspool.for **
C
C SSPOOL.FOR
C
C V03 29-JUL-92 GCAN RE-RELEASED FOR THE NETHERLANDS.
C V02 12-NOV-91 MTK  INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C SUBROUTINE TO DUMP SCORE POOL TO DISK
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
	SUBROUTINE SSPOOL(GIND)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:SCRCOM.DEF'
	INCLUDE 'INCLIB:RECSPF.DEF'
C
C
	INTEGER*4 FDB(7)
	INTEGER*4 GIND, I, J, ST
C
C OPEN POOL FILE AND READ FIRST RECORD
C
10	CONTINUE
	CALL OPENW(4,SCRPFN(1,GIND),4,0,0,ST)
	CALL IOINIT(FDB,4,SPFSEC*256)
	IF(ST.NE.0) THEN
	   CALL FILERR(SCRPFN(1,GIND),1,ST,0)
	   GOTO 10
	ENDIF
C
	CALL READW(FDB,1,SPFREC,ST)
	IF(ST.NE.0) THEN
	   CALL FILERR(SCRPFN(1,GIND),2,ST,1)
	   CALL CLOSEFIL(FDB)
	   CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
C SAVE POOL ARRAY TO FILE
C
	DO 110 I=1,SLEN
	   DO 100 J=1,TYPS
	      SPFPOL(I,J)=SCPOOL(I,J,SPDYNM,GIND)
100	   CONTINUE
110	CONTINUE
C
C SAVE DISPLAY ARRAY TO FILE
C
	DO 120 I=1,MAXD
	   IF(DPOOL(I,SPSCOR,GIND).NE.0) THEN
	      SPFDPL(I)=DPOOL(I,SPSCOR,GIND)
	      SPFPOL(SPFDPL(I),SPODDS)=DPOOL(I,SPDODS,GIND)
	      SPFPOL(SPFDPL(I),SPAMNT)=DPOOL(I,SPDAMT,GIND)
	   ENDIF
120	CONTINUE
C
	DO 130 I=1,MAXO
	   IF(OPOOL(I,SPSCOR,GIND).NE.0) THEN
	      SPFOPL(I)=OPOOL(I,SPSCOR,GIND)
	      SPFPOL(SPFOPL(I),SPODDS)=OPOOL(I,SPDODS,GIND)
	      SPFPOL(SPFOPL(I),SPAMNT)=OPOOL(I,SPDAMT,GIND)
	   ENDIF
130	CONTINUE
C
C WRITE POOLS TO FILE
C
	CALL WRITEW(FDB,1,SPFREC,ST)
	IF(ST.NE.0) THEN
	   CALL FILERR(SCRPFN(1,GIND),3,ST,1)
	   CALL CLOSEFIL(FDB)
	   CALL GSTOP(GEXIT_FATAL)
	ENDIF
	CALL CLOSEFIL(FDB)
	RETURN
	END
