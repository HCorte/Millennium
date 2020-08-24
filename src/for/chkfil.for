C
C PROGRAM CHKFIL
C $Log:   GXAFXT:[GOLS]CHKFIL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:32:02   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:49:24   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - chkfil.for **
C
C CHKFIL.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C
C
C     GET FILE CHKSUM
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
	PROGRAM CHKFIL
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INTEGER*4 FILNAM(6)
	INTEGER*4 FCB(7)
	INTEGER*4 BUF(64)
C
	INTEGER*4 CHECK, RECORD, START, CHKSUM, SIZE, ST
C
	INTEGER*4 I4
	INTEGER*2 I2(2)
	EQUIVALENCE (I4,I2)
C
	CALL COPYRITE
C
C
10	CONTINUE
	CALL USRCLOS1(     1)
	TYPE *,'enter file name to get check, enter EXIT to exit'
	ACCEPT 900,FILNAM
900	FORMAT(6A4)
	IF (FILNAM(1).EQ.'EXIT') CALL GSTOP(GEXIT_OPABORT)
	CALL OPENW(1,FILNAM,4,0,0,ST)
	IF (ST.NE.0) THEN
	  TYPE *,'cannot open file, status ',ST
	  GOTO 10
	ENDIF
	CALL GETSIZ_USED(1,SIZE)
	CALL IOINIT(FCB,1,1*256)
	CHKSUM=0
	TYPE *,'enter starting block # to check '
	TYPE *,'1 - for data file, 2 for task'
	ACCEPT *,START
	DO 100, RECORD=START,SIZE
	CALL READW(FCB,RECORD,BUF,ST)
	IF (ST.NE.0)  THEN
	  TYPE *,'read error, status ',ST,' record ',RECORD
	  GOTO 10
	ENDIF
C***  CALL GETCHKSUM(BUF(1),BUF(64),CHKSUM)
	CALL CHKTAB(CHECK,BUF,64)
	CHKSUM=CHKSUM+CHECK
100	CONTINUE
C
	TYPE 910,SIZE,CHKSUM
910	FORMAT(1H ,I7,' records read, checksum = ',Z8)
	GOTO 10
	END
