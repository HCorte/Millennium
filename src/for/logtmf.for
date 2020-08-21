C
C PROGRAM LOGTMF
C $Log:   GXAFXT:[GOLS]LOGTMF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:55:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:55:00   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - logtmf.for **
C
C LOGTMF.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 20-NOV-89   LOU R.   INITIAL RELEASE FOR FINLAND.
C
C PROGRAM TO RECONSTRUCT TMF FROM LOG TAPE(S).
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
	PROGRAM LOGTMF
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*4 TFDB(7),DFDB(7),BUF(2048),HBUF(16,128)
	INTEGER*4 SCFFDB(7), BLOCK, TT, ANS, ST1, EXT, NUM, I
	INTEGER*4 SCFNAM(4), ST, NOCHECK0, TEMP
C
	INTEGER*2 DATE(LDATE_LEN),I2TEMP(2)
C
	EQUIVALENCE (TEMP,I2TEMP)
	REAL*8 TAPTYP(3)
	CHARACTER*20 TAPNAM
	DATA SCFNAM/'SCF.','FIL ','    ','    '/
	DATA TAPTYP/'  Logger','  Tmfdmp','  ??????'/
	EQUIVALENCE(HBUF,BUF)
	COMMON/NOCHECK0/ NOCHECK0
C
	CALL COPYRITE
C
C
C GET TMF.FIL NAME FROM CONFIGURATION FILE
C
	NOCHECK0=-1
	CALL OPENX(1,'SCF.FIL',4,0,0,ST)
	CALL IOINIT(SCFFDB,1,SCFSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFNAM,1,ST,0)
	CALL READW(SCFFDB,1,SCFREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFNAM,2,ST,1)
	CALL CLOSEFIL(SCFFDB)
	DO 10 I=1,MAXFIL
	   IF(SCFSFN(1,I).EQ.'    ') CALL SYSVOL(SCFSFN(1,I))
10	CONTINUE
C
C OPEN TMF FILE FROM APPROPRIATE VOLUME
C
	CALL OPENW(1,SCFSFN(1,PTMF),4,0,0,ST)
	CALL IOINIT(DFDB,1,32*256)
	IF(ST.NE.0) CALL FILERR(SCFSFN(1,PTMF),1,ST,0)
C
C GET TAPE DRIVE NUMBER
C AND INITIALIZE TAPE FDB
C
	CALL WIMG(5,'Enter tape drive name: ')
	ACCEPT '(A)',TAPNAM
	IF(TAPNAM.EQ.'E ')CALL GSTOP(GEXIT_SUCCESS)
	CALL TAPOPEN(TFDB,TAPNAM,ST)
	CALL TAPINT(TFDB,7,8192)
	CALL XREWIND(TFDB,ST)
C
C START READING TAPE
C
30	CONTINUE
	CALL RTAPEW(TFDB,BUF,ST)
	IF(ST.NE.0) THEN
	  CALL XREWIND(TFDB,ST1)
	  WRITE(5,900) ST
	  READ(5,901) ANS
	  IF(ANS.EQ.'E') THEN
	    CALL TAPCLOS(TFDB,ST)
	    CALL USRCLOS1(     1)
	    CALL GSTOP(GEXIT_SUCCESS)
	  ENDIF
	  CALL TAPINT(TFDB,7,8192)
	  GOTO 30
	ENDIF
C
C CHECK FOR HEADER RECORD
C
	IF(HBUF(2,1).EQ.-1) THEN
	  TT=HBUF(3,1)+1
	  IF(TT.LT.1.OR.TT.GT.2) TT=3
	  TEMP=HBUF(1,2)
	  DATE(VCDC)=TEMP
	  CALL LCDATE(DATE)
	  WRITE(5,903) TAPTYP(TT),DATE(VMON),DATE(VDAY),DATE(VYEAR2),
     *	               DATE(VCDC)
	  GOTO 30
	ENDIF
C
C GET DISK BLOCK NUMBER
C
	BLOCK=BUF(2)
C
C
	IF(BLOCK.LE.0) THEN
	  WRITE(5,902) BLOCK
	  CALL BELLS(2)
	  GOTO 30
	ENDIF
C
C WRITE RECORD TO DISK
C
	CALL WRITEW(DFDB,BLOCK,BUF,ST)
	IF(ST.NE.0) THEN
	  CALL FILERR(SCFSFN(1,PTMF),3,ST,BLOCK)
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
C READ NEXT TAPE RECORD
C
	GOTO 30
C
C
900	FORMAT(' Tape read error ',Z8,/,
     *	       ' Mount next tape and hit return ',/,
     *	       ' Hit E if no more tapes')
901	FORMAT(A1)
902	FORMAT('   Bad block number from tape ',I8)
903   FORMAT(1X,A8,' tape created ',I2.2,'/',I2.2,'/',I4.4,' cdc ',I4)
	END
