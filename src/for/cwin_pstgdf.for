C
C SUBROUTINE CWIN_PSTGDF
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

	SUBROUTINE CWIN_PSTGDF

	IMPLICIT NONE

C---- Inckude files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DCPREC.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'

C---- Local Variables.

	INTEGER*4 FDB(7)
	INTEGER*4 GIND, DRAW, GNUM, K, ST

C-------------------------- Start of Code ----------------------------

C---- Super double games.

	DO 1000 GIND = 1,NUMCPL
	  DRAW = LCPDRW(GIND)

	  IF (DRAW .LT. 1) GOTO 1000

	  GNUM = GTNTAB(TCPL,GIND)

	  WRITE(5,900) IAM(),(GFNAMES(K,GNUM),K=1,5)

	  CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)

	CALL IOINIT(FDB,3,DCPSEC*256)

	IF (ST .NE. 0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	CALL READW(FDB,DRAW,DCPREC,ST)

	IF (ST .NE. 0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

C---- Compare winsel sales figures with daily sales figures.

	IF (DCPSAL(DOLAMT) .NE. LCPSAL(DOLAMT,GIND)) THEN
	  CALL BELLS(2)
	  WRITE(5,901) IAM(),GTNAMES(TCPL),GIND,
     *	               CMONY(DCPSAL(DOLAMT),12,BETUNIT),
     *		       CMONY(LCPSAL(DOLAMT,GIND),12,BETUNIT)
	  CALL GPAUSE
	ENDIF


	IF (LCPSTS(GIND) .EQ. GAMCAN) LCPSTS(GIND) = GAMREF
	IF (LCPSTS(GIND) .NE. GAMREF .AND.
     *	    LCPSTS(GIND) .GE. GAMENV) LCPSTS(GIND) = GFINAL

	LCPDAT(GIND) = DAYCDC

C---- Calculate roll pool and breakage.

	IF ((LCPWON(GIND)-LCPREF(GIND)) .NE. 0) THEN
	  LCPBRK(2,GIND) = LCPTPL(GIND) -		!TOTAL POOL (LESS REF)
     *			  (LCPWON(GIND) -		!WHAT IS PAYED OUT
     *			   LCPREF(GIND))	        !(LESS REF).
	ELSE
	   LCPBRK(2,GIND) = 0
	   LCPPOL(2,GIND) = LCPTPL(GIND)
	   CALL UPDRDF(GNUM,LCPPOL(2,GIND),DRAW)
	END IF

	CALL GAMLOG(TCPL,GIND,DCPREC,LCPSTS)

	CALL WRITEW(FDB,DRAW,DCPREC,ST)

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
