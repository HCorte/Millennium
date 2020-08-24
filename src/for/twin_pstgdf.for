C
C SUBROUTINE TWIN_PSTGDF
C 
C V02 15-DEC-1999 UXN MULTIWIN CHANGES.
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C
C SUBROUTINE TO UPDATE GAME FILES WITH DRAW INFORMATION
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TWIN_PSTGDF
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DTSREC.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'
C
	INTEGER*4 FDB(7)
	INTEGER*4 GIND, DRAW, GNUM, I, K, ST, MORE
C
C
	DO 1000 GIND=1,NUMTSL
	DRAW=LTSDRW(GIND)
	IF(DRAW.LT.1) GOTO 1000
	GNUM=GTNTAB(TTSL,GIND)
	WRITE(5,900) IAM(),(GFNAMES(K,GNUM),K=1,5)
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,3,DTSSEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
	CALL READW(FDB,DRAW,DTSREC,ST)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
C
C COMPARE WINSEL SALES FIGURES WITH DAILY SALES FIGURES
C
	IF(DTSSAL.NE.LTSSAL(GIND)) THEN
	  CALL BELLS(2)
	  WRITE(5,901) IAM(),GTNAMES(TTSL),GIND,
     *	               CMONY(DTSSAL,12,BETUNIT),
     *                 CMONY(LTSSAL(GIND),12,BETUNIT)
	  CALL GPAUSE
	ENDIF
	IF(LTSSTS(GIND).EQ.GAMCAN) LTSSTS(GIND)=GAMREF
C
	MORE=0
	DO 100 I=1,DTSRWS
	IF(LTSSTA(I,GIND).EQ.GAMOPN) MORE=MORE+1
	IF(LTSSTA(I,GIND).EQ.GAMCAN) LTSSTA(I,GIND)=GAMREF
	IF(LTSSTA(I,GIND).EQ.GAMENV) LTSSTA(I,GIND)=GFINAL
100	CONTINUE
C
	IF(LTSSTS(GIND).NE.GAMREF.AND.MORE.GT.0.AND.
     *	   LTSSTS(GIND).NE.GAMCAN) LTSSTS(GIND)=GAMOPN
C
	IF(LTSSTS(GIND).NE.GAMREF.AND.MORE.EQ.0.AND.
     *	   LTSSTS(GIND).GE.GAMENV) LTSSTS(GIND)=GFINAL
	WRITE(5,902) IAM(),DTSSTS
	CALL GAMLOG(TTSL,GIND,DTSREC,LTSSTS)
	CALL WRITEW(FDB,DRAW,DTSREC,ST)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),3,ST,DRAW)
	CALL CLOSEFIL(FDB)
C
        IF(STOPMOD.EQ.WINMULTI) DRWSTS(MLWININD,GNUM)=WINSOK
C
1000	CONTINUE
	RETURN
C
C
900	FORMAT(1X,A,' Posting winsel data to ',5A4)
901	FORMAT(1X,A,1X,A8,I1,'   sales discrepancy ',/,
     *	       1X,'Daily posting ',A12,' Winsel posting ',A12)
902	FORMAT(1X,A,1X,'Posting Status ',I2,' to Toto Select Game File')
	END
