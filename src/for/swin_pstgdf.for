C
C SUBROUTINE SWIN_PSTGDF
C 
C V02 15-DEC-1999 UXN MULTIWIN CHANGES.
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C
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
	SUBROUTINE SWIN_PSTGDF
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DSCREC.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'

	INTEGER*4 FDB(7)
	INTEGER*4 GIND, DRAW, GNUM, K, ST
C
C SCORE GAMES
C
	DO 1000 GIND=1,NUMSCR
	DRAW=LSCDRW(GIND)
	IF(DRAW.LT.1) GOTO 1000
	GNUM=GTNTAB(TSCR,GIND)
	WRITE(5,900) IAM(),(GFNAMES(K,GNUM),K=1,5)
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,3,DSCSEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
	CALL READW(FDB,DRAW,DSCREC,ST)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
C
C COMPARE WINSEL SALES FIGURES WITH DAILY SALES FIGURES
C
	IF(DSCSAL.NE.LSCSAL(GIND)) THEN
	  CALL BELLS(2)
	  WRITE(5,901) IAM(),GTNAMES(TSCR),GIND,
     *	               CMONY(DSCSAL,12,BETUNIT),
     *		       CMONY(LSCSAL(GIND),12,BETUNIT)
	  CALL GPAUSE
	ENDIF
C
C
	IF(LSCSTS(GIND).EQ.GAMCAN) LSCSTS(GIND)=GAMREF
	IF(LSCSTS(GIND).NE.GAMREF.AND.
     *	   LSCSTS(GIND).GE.GAMENV) LSCSTS(GIND)=GFINAL
	LSCDAT(GIND)=DAYCDC
C
C CALCULATE ROLL POOL AND BREAKAGE
C
	IF( (LSCWON(GIND)-LSCREF(GIND)) .NE.0 ) THEN
	   LSCBRK(2,GIND) = LSCTPL(GIND) -		!TOTAL POOL (LESS REF)
     *			   (LSCWON(GIND) -		!WHAT IS PAYED OUT
     *			    LSCREF(GIND))	        !(LESS REF).
	ELSE
	   LSCBRK(2,GIND) = 0
	   LSCPOL(2,GIND) = LSCTPL(GIND)
	   CALL UPDRDF(GNUM,LSCPOL(2,GIND),DRAW)
	ENDIF
C
	CALL GAMLOG(TSCR,GIND,DSCREC,LSCSTS)
	CALL WRITEW(FDB,DRAW,DSCREC,ST)
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
901	FORMAT(1X,A,1X,A8,I1,' sales discrepancy ',/,
     *	       1X,'Daily posting ',A12,' Winsel posting ',A12)
	END
