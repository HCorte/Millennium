C
C SUBROUTINE DWN_PSTGDF
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
	SUBROUTINE DWN_PSTGDF
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DWNCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DNBREC.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'
	INTEGER*4 FDB(7)
	INTEGER*4 GIND, DRAW, GNUM, I, K, ST
C
C NUMBERS GAMES
C
	DO 1000 GIND=1,NUMNBR
	DRAW=LNBDRW(GIND)
	IF(DRAW.LT.1) GOTO 1000
	GNUM=GTNTAB(TNBR,GIND)
	WRITE(5,900) IAM(),(GFNAMES(K,GNUM),K=1,5)
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,3,DNBSEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
	CALL READW(FDB,DRAW,DNBREC,ST)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
C
C COMPARE WINSEL SALES FIGURES WITH DAILY SALES FIGURES
C
	DO 100 I=1,LNBPOL(GIND)
	IF(DNBSAL(DOLAMT,I).NE.LNBSAL(DOLAMT,I,GIND)) THEN
	  CALL BELLS(2)
	  WRITE(5,901) IAM(),GTNAMES(TNBR),GIND,I,
     *	               DNBSAL(DOLAMT,I),LNBSAL(DOLAMT,I,GIND)
	  CALL GPAUSE
	ENDIF
100	CONTINUE
	CALL GAMLOG(TNBR,GIND,DNBREC,LNBSTS)
	CALL WRITEW(FDB,DRAW,DNBREC,ST)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),3,ST,DRAW)
C
C
	DRAW=DRAW+1
	CALL READW(FDB,DRAW,DNBREC,ST)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
C
C POST RESERVE STUFF HERE
C
	CALL WRITEW(FDB,DRAW,DNBREC,ST)
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
901	FORMAT(1X,A,1X,A8,I1,' sales discrepancy for pool ',I4,/,
     *	       20X,'Daily posting ',I8,' Winsel posting ',I8)
	END
