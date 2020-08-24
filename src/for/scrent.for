C
C SUBROUTINE SCRENT
C
C
C V08 30-JUN-2000 UXN  Refund too late played tickets.
C V07 17-DEC-1999 PXO  Added a call to report subroutine  
C V06 23-MAY-1997 WPW  Fix for cmony.
C V05 17-APR-1996 HXK  Release of Finland for X.25, Telephone Betting,
C                      Instant Pass Thru Phase 1
C V04 21-JAN-1993 DAB  Initial Release
C                      Based on Netherlands Bible, 12/92, and Comm 1/93 update
C V03 14-AUG-1992 GCAN CORRECTED WINNING ODDS CALCULATIONS.
C V02 14-JUL-1992 WLM  CHECKED AND RELEASED FOR THE NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C SUBROUTINE TO PROCESS OPERATOR ENTRY OF SCORE RESULTS.
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
	SUBROUTINE SCRENT(GNUM,GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:RECSPF.DEF'
C
	COMMON/RESULTS_SPFREC/ SPFREC
C
	CHARACTER*45 BUF
	REAL*8 TOTAL,RODDS
	INTEGER*4 FDB(7),PFDB(7)
	INTEGER*4 GNUM, ST, DRAW, GIND, X, K, NUM, EXT, FLAG
	INTEGER*4 INDEX, WIN, I
	INTEGER*4 CDC,TIME
C
C
	CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,3,DSCSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
	CALL READW(FDB,DRAW,DSCREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
C
	IF(DSCSTS.NE.GAMBFD) THEN
	  WRITE(6,900) IAM(),GTNAMES(TSCR),GIND,DRAW,DSCSTS
	  CALL GPAUSE
	ENDIF
C
	DSCLAT(LATCDC) = 0
	DSCLAT(LATTIM) = 0
C
	WRITE(6,901) IAM(),GTNAMES(TSCR),GIND,DRAW,
     *	             (DSCNM1(X),X=1,3),(DSCNM2(X),X=1,3)
C
C
100	CONTINUE
	WRITE (BUF,902) (DSCNM1(K),K=1,3)
	CALL INPNUM(BUF,NUM,0,99,EXT)
	IF(EXT.NE.0.AND.EXT.NE.-5) GOTO 100
	IF(EXT.EQ.-5) NUM=-1
	DSCWIN(1)=NUM
C
C
110	CONTINUE
	WRITE (BUF,902) (DSCNM2(K),K=1,3)
	CALL INPNUM(BUF,NUM,0,99,EXT)
	IF(EXT.NE.0.AND.EXT.NE.-5) GOTO 110
	IF(EXT.EQ.-5) NUM=-1
	DSCWIN(2)=NUM
C
	WRITE(6,903) IAM(),(DSCNM1(K),K=1,3),DSCWIN(1),
     *	             IAM(),(DSCNM2(K),K=1,3),DSCWIN(2)
	CALL INPYESNO('Are the scores entered correct [Y/N] ',FLAG)
	IF(FLAG.NE.1) GOTO 100
	DSCSTS=GAMEN1
	OPDONE=1
C
C WAIT FOR VERIFICATION FROM REMOTE TERMINAL.
C
120	CONTINUE
	TYPE*,IAM(),' Waiting for verification from remote terminal'
	IF(DSCSTS.EQ.GAMENV) GOTO 130
	CALL XWAIT(5,2,ST)
	IF(DSCSTS.EQ.GAMBFD) THEN
	  TYPE*,IAM(),' Verification error, please re-enter '
	  GOTO 100
	ENDIF
	GOTO 120
C
C
130	CONTINUE
	TYPE*,IAM(),' Results verified...calculating odds'
	IF(DSCWIN(1).EQ.-1.AND.DSCWIN(2).EQ.-1)THEN
	  WRITE(6,904) IAM(),GTNAMES(TSCR),GIND,DRAW
	  DSCSTS=GAMCAN
	  DSCODS=100
	  GOTO 1000
	ENDIF
C
	CALL RC_PROMPT(CDC,TIME,ST)
	IF(ST.EQ.1) THEN
	     CALL RESPOL_TSCR(GNUM,CDC,TIME)
	     GOTO 135
	ENDIF
C
C READ POOL FILE
C
	WRITE(6,905) IAM(),GTNAMES(TSCR),GIND,DRAW
	IF(DSCPFN(1).EQ.'    ') CALL SYSVOL(DSCPFN(1))
	CALL OPENW(4,DSCPFN,4,0,0,ST)
	CALL IOINIT(PFDB,4,SPFSEC*256)
	IF(ST.NE.0) CALL FILERR(DSCPFN,1,ST,0)
	CALL READW(PFDB,1,SPFREC,ST)
	IF(ST.NE.0) CALL FILERR(DSCPFN,2,ST,1)
	CALL CLOSEFIL(PFDB)
C
135	CONTINUE
C
C Calculate totals
C 
	TOTAL = 0.0D0
	DO I=1,SLEN
	   TOTAL = TOTAL + SPFPOL(I,SPAMNT)
	ENDDO
C
C CALCULATE ODDS
C
	RODDS = 0.0D0
	TOTAL = TOTAL * CALPER(DSCSPR)
	TOTAL = TOTAL + DFLOAT(DSCPOL(1)) + DFLOAT(DWIBRK(1))
	DSCTPL= IDINT(TOTAL)
C
	TOTAL = TOTAL * DFLOAT(DYN_BETUNIT)

	CALL POLIND(DSCWIN(1),DSCWIN(2),INDEX)
	WIN = DFLOAT(SPFPOL(INDEX,2)*DYN_BETUNIT)
	IF(WIN.NE.0.0D0) RODDS = TOTAL/WIN
	DSCABW = SPFPOL(INDEX,2)
	DSCODS = INT(RODDS*1000.0D0)+5
	DSCODS = DSCODS/10
	IF(DSCODS.LT.100) DSCODS = 100
C
C
	WRITE(6,906) IAM(),DSCODS/100,MOD(DSCODS,100),
     *               CMONY(DSCABW,10,BETUNIT)
	CALL INPYESNO('Do you want to change these odds [Y/N] ',FLAG)
	IF(FLAG.EQ.1) THEN
140	  CONTINUE
	  CALL INPNUM('Enter new odds [100-999999]:',DSCODS,
     *	              100,999999,EXT)
	  WRITE(6,907) IAM(),DSCODS/100,MOD(DSCODS,100)
	  CALL INPYESNO('Are the new odds entered correct [Y/N] ',FLAG)
	  IF(FLAG.NE.1) GOTO 140
	ENDIF
C
C
1000	CONTINUE
	CALL WRITEW(FDB,DRAW,DSCREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
	CALL CLOSEFIL(FDB)
	CALL SCRESULT(GIND,DRAW)
	RETURN
C
C
C
900	FORMAT(1X,A,1X,A8,I1,' event ',I4,' invalid game status> ',I4)
901	FORMAT(1X,A,1X,A8,I1,' event ',I4,1X,3A4,' vs ',3A4)
902	FORMAT('Enter ',3A4,' score [C to cancel event]:')
903	FORMAT(1X,A,' Score entered for ',3A4,' is ',I2,/,
     *	       1X,A,' Score entered for ',3A4,' is ',I2)
904	FORMAT(1X,A,1X,A8,I1,' event ',I4,' has been cancelled')
905	FORMAT(1X,A,' Reading ',A8,I1,' pools for event ',I4)
906	FORMAT(1X,A,' Current payout odds are ',I7,'.',I2.2,' to 1',
     *	       '    Winning Investment  -  ',A10)
907	FORMAT(1X,A,' New payout odds entered are ',I5,'.',I2.2,' to 1')
	END
