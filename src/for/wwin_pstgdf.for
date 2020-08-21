C
C SUBROUTINE WWIN_PSTGDF
C 
C V02 15-DEC-1999 UXN MULTIWIN changes.
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
	SUBROUTINE WWIN_PSTGDF
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DWIREC.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'
C
	INTEGER*4 FDB(7)
	INTEGER*4 I, GIND, DRAW, GNUM, K, ST
C
C
	DO 1000 GIND=1,NUMWIT
	DRAW=LWIDRW(GIND)
	IF(DRAW.LT.1) GOTO 1000
	GNUM=GTNTAB(TWIT,GIND)
	WRITE(5,900) IAM(),(GFNAMES(K,GNUM),K=1,5)
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,3,DWISEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
	CALL READW(FDB,DRAW,DWIREC,ST)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
C
C COMPARE WINSEL SALES FIGURES WITH DAILY SALES FIGURES
C
	IF(DWISAL.NE.LWISAL(GIND)) THEN
	  CALL BELLS(2)
	  WRITE(5,901) IAM(),GTNAMES(TWIT),GIND,
     *	               CMONY(DWISAL,12,BETUNIT),
     *		       CMONY(LWISAL(GIND),12,BETUNIT)
	  CALL GPAUSE
	ENDIF
	DO 100 I=1,MAXWRW
	IF(DWISBR(I).NE.LWISBR(I,GIND)) THEN
	  CALL BELLS(2)
	  WRITE(5,902) IAM(),GTNAMES(TWIT),GIND,I,
     *	  CMONY(DWISBR(I),12,BETUNIT),CMONY(LWISBR(I,GIND),12,BETUNIT)
	  CALL GPAUSE
	ENDIF
100	CONTINUE
C
C
        IF(LWISTS(GIND).EQ.GAMCAN) LWISTS(GIND)=GAMREF
        IF(LWISTS(GIND).NE.GAMREF.AND.
     *     LWISTS(GIND).GE.GAMENV) LWISTS(GIND)=GFINAL
        DO 110 I=1,MAXWRW
        IF(LWISTA(I,GIND).EQ.GAMCAN) LWISTA(I,GIND)=GAMREF
110     CONTINUE
	IF(LWISTS(GIND).EQ.GFINAL.OR.LWISTS(GIND).EQ.GAMREF) THEN
	   LWIDAT(GIND)=DAYCDC
	ENDIF
C
C CALCULATE ROLL POOL AND BREAKAGE
C
	IF( (LWIWON(GIND) - LWIREF(GIND)) .NE.0 ) THEN
	  LWIBRK(2,GIND) = LWITPL(GIND) -		 !TOTAL POOL (LESS REF) 
     *                    (LWIWON(GIND)	-		 !WHAT IS PAYED OUT
     *                     LWIREF(GIND))                 !(LESS REF).
	ELSE
	  LWIBRK(2,GIND) = 0
	  LWIPOL(2,GIND) = LWITPL(GIND) 
	  CALL UPDRDF(GNUM,LWIPOL(2,GIND),DRAW)
	ENDIF
C
	CALL GAMLOG(TWIT,GIND,DWIREC,LWISTS)
	CALL WRITEW(FDB,DRAW,DWIREC,ST)
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
902	FORMAT(1X,A,1X,A8,I1,' sales discrepancy ',/,
     *	       'row ',I2,' Daily posting ',A12,' Winsel posting ',A12)
	END
