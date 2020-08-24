C
C SUBROUTINE TRWIN_PSTGDF
C  
C V02 15-DEC-1999 UXN MULTIWIN CHANGES.
C V01 XX-XXX-XXXX RXK INITIAL RELEASE.
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
	SUBROUTINE TRWIN_PSTGDF
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DTRREC.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'

	INTEGER*4 FDB(7)
	INTEGER*4 GIND, DRAW, GNUM, K, ST

C-------------------------- Start of Code ----------------------------

C---- Today's triple games.

	DO 1000 GIND = 1,NUMTRP
	  DRAW = LTRDRW(GIND)
	  IF (DRAW .LT. 1) GOTO 1000
	  GNUM = GTNTAB(TTRP,GIND)
	  WRITE(5,900) IAM(),(GFNAMES(K,GNUM),K=1,5)

	  CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	  CALL IOINIT(FDB,3,DTRSEC*256)
	  IF (ST .NE. 0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
	  CALL READW(FDB,DRAW,DTRREC,ST)
	  IF (ST .NE. 0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

C---- Compare winsel sales figures with daily sales figures.

	  IF (DTRSAL(DOLAMT) .NE. LTRSAL(DOLAMT,GIND)) THEN
	     CALL BELLS(2)
	     WRITE(5,901) IAM(),GTNAMES(TTRP),GIND,
     *	               CMONY(DTRSAL(DOLAMT),12,BETUNIT),
     *		       CMONY(LTRSAL(DOLAMT,GIND),12,BETUNIT)
	     CALL GPAUSE
	  ENDIF


	  IF (LTRSTS(GIND) .EQ. GAMCAN) LTRSTS(GIND) = GAMREF
	  IF (LTRSTS(GIND) .NE. GAMREF .AND.
     *	      LTRSTS(GIND) .GE. GAMENV) LTRSTS(GIND) = GFINAL

	  LTRDAT(GIND) = DAYCDC

C---- Calculate roll pool and breakage.

	  IF ((LTRWON(GIND)-LTRREF(GIND)) .NE. 0) THEN
	     LTRBRK(1,GIND) = LTRTPL(GIND) -		!TOTAL POOL (LESS REF)
     *	 		  (LTRWON(GIND) -		!WHAT IS PAYED OUT
     *			   LTRREF(GIND))	        !(LESS REF).
	  ELSEIF(LTRSTS(GIND).NE.GAMREF) THEN                                                 
	     LTRBRK(1,GIND) = 0
	     LTRPOL(2,GIND) = LTRTPL(GIND)
	     CALL UPDRDF(GNUM,LTRPOL(2,GIND),DRAW)
	  END IF

	CALL GAMLOG(TTRP,GIND,DTRREC,LTRSTS)
	CALL WRITEW(FDB,DRAW,DTRREC,ST)
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
