C
C SUBROUTINE DWIN_PSTGDF
C
C V02 15-DEC-1999 UXN MULTIWIN changes.
C V01 23-NOV-1995 PXB Initial revision.
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
C  
C SUBROUTINE TO UPDATE GAME FILES WITH DRAW INFORMATION
C
C=======OPTIONS /CHECK=NOOVERFLOW

	SUBROUTINE DWIN_PSTGDF

	IMPLICIT NONE

C---- Inckude files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DDBREC.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'

C---- Local Variables.

	INTEGER*4 FDB(7)
	INTEGER*4 GIND, DRAW, GNUM, K, ST


C-------------------------- Start of Code ----------------------------

C---- Super double games.

	DO 1000 GIND = 1,NUMDBL
	  DRAW = LDBDRW(GIND)

	  IF (DRAW .LT. 1) GOTO 1000

	  GNUM = GTNTAB(TDBL,GIND)

	  WRITE(5,900) IAM(),(GFNAMES(K,GNUM),K=1,5)

	  CALL OPENW(3,
     *		     GFNAMES(1,GNUM),
     *		     4,
     *		     0,
     *               0,
     *               ST)

	CALL IOINIT(FDB,3,DDBSEC*256)

	IF (ST .NE. 0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	CALL READW(FDB,DRAW,DDBREC,ST)

	IF (ST .NE. 0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

C---- Compare winsel sales figures with daily sales figures.

	IF (DDBSAL(DOLAMT) .NE. LDBSAL(DOLAMT,GIND)) THEN
	  CALL BELLS(2)
	  WRITE(5,901) IAM(),GTNAMES(TDBL),GIND,
     *	               CMONY(DDBSAL(DOLAMT),12,BETUNIT),
     *		       CMONY(LDBSAL(DOLAMT,GIND),12,BETUNIT)
	  CALL GPAUSE
	ENDIF


	IF (LDBSTS(GIND) .EQ. GAMCAN) LDBSTS(GIND) = GAMREF
	IF (LDBSTS(GIND) .NE. GAMREF .AND.
     *	    LDBSTS(GIND) .GE. GAMENV) LDBSTS(GIND) = GFINAL

	LDBDAT(GIND) = DAYCDC

C---- Calculate roll pool and breakage.

	IF ((LDBWON(GIND)-LDBREF(GIND)) .NE. 0) THEN
	  LDBBRK(2,GIND) = LDBTPL(GIND) -		!TOTAL POOL (LESS REF)
     *			  (LDBWON(GIND) -		!WHAT IS PAYED OUT
     *			   LDBREF(GIND))	        !(LESS REF).
	ELSE
	   LDBBRK(2,GIND) = 0
	   LDBPOL(2,GIND) = LDBTPL(GIND)
	   CALL UPDRDF(GNUM,LDBPOL(2,GIND),DRAW)
	END IF

	CALL GAMLOG(TDBL,GIND,DDBREC,LDBSTS)

	CALL WRITEW(FDB,DRAW,DDBREC,ST)

	IF (ST .NE. 0) CALL FILERR(GFNAMES(1,GNUM),3,ST,DRAW)

	CALL CLOSEFIL(FDB)
C
        IF(STOPMOD.EQ.WINMULTI) DRWSTS(MLWININD,GNUM)=WINSOK
C

1000	CONTINUE

	RETURN

C----------------------- Format Statements ----------------------------

900	FORMAT(1X,A,' Posting winsel data to ',5A4)

901	FORMAT(1X,A,1X,A8,I1,' sales discrepancy ',/,
     *	       1X,'Daily posting ',A12,' Winsel posting ',A12)


	END
