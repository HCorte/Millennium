C
C PROGRAM GBDAILY
C $Log:   GXAFXT:[GOLS]GBDAILY.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:17:24   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:23:02   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - gbdaily.for **
C
C GBDAILY.FOR
C
C V02 15-FEB-91 WOL USES NEW CHARACTER CMONY ROUTINES
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 28-NOV-89 MGM INTIAL RELEASE FOR FINLAND
C
C THIS PROGRAM TAKES A RECORD FROM THE ASF.FIL AND WRITES
C SALES DATA TO THE GBSALES.FIL
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
	PROGRAM  GBDAILY
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*2  DATE(LDATE_LEN)
	INTEGER*4  FDB(7), K, GAM, I, ST
	INTEGER*4  SCFFDB(7), SCFNAM(5)
	DATA       SCFNAM/'SCF.','FIL ',3*'    '/
C
	CHARACTER*80 OUTLINE
C
C
	CALL COPYRITE
C
C
	TYPE *
	TYPE *,'<<<<< GBDAILY Daily Sales/Game Extract File  V01 >>>>>'
	TYPE *
C
C READ SCF RECORD
C
	CALL OPENW(1,SCFNAM,4,0,0,ST)
	CALL IOINIT(SCFFDB,1,SCFSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFNAM,1,ST,0)
	CALL READW(SCFFDB,1,SCFREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFNAM,2,ST,1)
	CALL CLOSEFIL(SCFFDB)
	DO 10 I=1,MAXFIL
	   IF(SCFSFN(1,I).EQ.'    ') CALL SYSVOL(SCFSFN(1,I))
10	CONTINUE
C
C OPEN DAILY ACTIVITY FILE
C
	CALL OPENW(1,SCFSFN(1,DAF),4,0,0,ST)
	CALL IOINIT(FDB,1,DAFSEC*256)
	IF(ST.NE.0) THEN
	  CALL CLOSEFIL(FDB)
	  CALL FILERR(SCFSFN(1,DAF),1,ST,0)
	ENDIF
C
	CALL READW(FDB,DAYCDC,DAFREC,ST)
	IF(ST.NE.0) THEN
	   TYPE*,'DAF.FIL read error >> ',ST,' record ',DAYCDC
	   CALL GPAUSE
	ENDIF
C
	DATE(VCDC) = DAYCDC
	CALL LCDATE(DATE)
	OPEN(UNIT=1,FILE='GBDAILY.FIL',IOSTAT=ST,
     *	     STATUS='NEW',RECL=80/4,ACCESS='SEQUENTIAL',
     *       CARRIAGECONTROL='NONE',RECORDTYPE='FIXED',
     *       FORM='UNFORMATTED')
	IF(ST.NE.0) THEN
	  TYPE *, 'Error opening GBDAILY.FIL > ',ST
	  CALL GPAUSE
	ENDIF
C
C  FORMAT:
C
C     DAY #  CDC  GAME #  SALES ETC...
C
	DO 2100 GAM=1,MAXGAM
	   IF(GNTTAB(GAMTYP,GAM).LE.0) GOTO 2100 !SKIP IF NOT ACTIVE
	   WRITE(OUTLINE,2001) DATE(VDOW),DAYCDC,GAM,
     *	        (DAFTYP(1,K,GAM),
     *           CMONY(DAFTYP(2,K,GAM),11,BETUNIT),K=1,4),
     *	         CHAR('0D'X),CHAR('0A'X)
	   WRITE(1) OUTLINE
2100	CONTINUE
C
C
2001	FORMAT(I1,I4,I2.2,4(I6,A11),3(' '),2A)
C
	END
