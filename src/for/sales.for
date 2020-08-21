C
C SUBROUTINE SALES
C
C V02 23-NOV-2000 UXN Logical unit for SYS$OUTPUT: is 6
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SALES
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	COMMON SCFREC
	INTEGER*4 FDB(7)
	INTEGER*4 OPT, EXT, CDC, K, REC, ST
	INTEGER*2 DATE(LDATE_LEN)
C
C
	TYPE*,'Initializing sales days'
	CALL OPENW(1,SCFSFN(1,DAF),4,0,0,ST)
	CALL IOINIT(FDB,1,DAFSEC*256)
	IF(ST.NE.0) THEN
	  CALL FILERR(SCFSFN(1,DAF),1,ST,0)
	  RETURN
	ENDIF
	REC=0
C
C
10	CONTINUE
	REC=REC+1
	CALL READW(FDB,REC,DAFREC,ST)
	IF(ST.EQ.144) GOTO 20
	IF(ST.NE.0) THEN
	  CALL FILERR(SCFSFN(1,DAF),2,ST,REC)
	  CALL CLOSEFIL(FDB)
	  RETURN
	ENDIF
C
C
	IF(DAFSTS.NE.0) GOTO 10
	DAFSTS=DSOPEN
	CALL WRITEW(FDB,REC,DAFREC,ST)
	IF(ST.NE.0) THEN
	  CALL FILERR(SCFSFN(1,DAF),3,ST,REC)
	  CALL CLOSEFIL(FDB)
	  RETURN
	ENDIF
	GOTO 10
C
C
20	CONTINUE
	DATE(5)=REC-1
	CALL LCDATE(DATE)
	WRITE(6,901) (DATE(K),K=7,13)
C
C
30	CONTINUE
	TYPE*,'Enter date to change (E-exit) '
	CALL INPDAT(CDC,EXT)
	IF(EXT.LT.0) THEN
	  CALL CLOSEFIL(FDB)
	  RETURN
	ENDIF
C
C
	IF(CDC.GE.REC) THEN
	  TYPE*,'Invalid date entered'
	  GOTO 30
	ENDIF
C
C
	CALL INPNUM('Enter option (1=sales, 2=nosales): ',
     *	            OPT,1,2,EXT)
	IF(EXT.LT.0) GOTO 30
C
C
	CALL READW(FDB,CDC,DAFREC,ST)
	IF(ST.NE.0) THEN
	  CALL FILERR(SCFSFN(1,DAF),2,ST,CDC)
	  CALL CLOSEFIL(FDB)
	  RETURN
	ENDIF
C
C
	IF(DAFSTS.GT.2) THEN
	  TYPE*,'Date entered is already closed and cannot be changed'
	  GOTO 20
	ENDIF
C
C
	IF(OPT.EQ.1) DAFSTS=DSOPEN
	IF(OPT.EQ.2) DAFSTS=DNOSAL
C
C
	CALL WRITEW(FDB,CDC,DAFREC,ST)
	IF(ST.NE.0) THEN
	  CALL FILERR(SCFSFN(1,DAF),3,ST,CDC)
	  CALL CLOSEFIL(FDB)
	  RETURN
	ENDIF
	GOTO 20
C
C
901	FORMAT(' Last date initialized ',7A2)
	END
