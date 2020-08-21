C
C SUBROUTINE STWIN_PSTGDF
C
C V02 15-DEC-1999 UXN MULTIWIN CHANGES.
C V01 17-MAY-1999 UXN INITIAL RELEASE.
C  
C SUBROUTINE TO UPDATE GAME FILES WITH DRAW INFORMATION
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE STWIN_PSTGDF
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DSTREC.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'

	INTEGER*4 FDB(7)
	INTEGER*4 GIND, DRAW, GNUM, K, ST

C-------------------------- Start of Code ----------------------------

C---- SUPER TRIPLE games.

	DO 1000 GIND = 1,NUMSTR
	  DRAW = LSTDRW(GIND)
	  IF (DRAW .LT. 1) GOTO 1000
	  GNUM = GTNTAB(TSTR,GIND)
	  WRITE(5,900) IAM(),(GFNAMES(K,GNUM),K=1,5)

	  CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	  CALL IOINIT(FDB,3,DSTSEC*256)
	  IF (ST .NE. 0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
	  CALL READW(FDB,DRAW,DSTREC,ST)
	  IF (ST .NE. 0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

C---- Compare winsel sales figures with daily sales figures.

	  IF (DSTSAL(DOLAMT) .NE. LSTSAL(DOLAMT,GIND)) THEN
	     CALL BELLS(2)
	     WRITE(5,901) IAM(),GTNAMES(TSTR),GIND,
     *	               CMONY(DSTSAL(DOLAMT),12,BETUNIT),
     *		       CMONY(LSTSAL(DOLAMT,GIND),12,BETUNIT)
	     CALL GPAUSE
	  ENDIF


	  IF (LSTSTS(GIND) .EQ. GAMCAN) LSTSTS(GIND) = GAMREF
	  IF (LSTSTS(GIND) .NE. GAMREF .AND.
     *	      LSTSTS(GIND) .GE. GAMENV) LSTSTS(GIND) = GFINAL

	  LSTDAT(GIND) = DAYCDC

C---- Calculate roll pool and breakage.

	  IF ((LSTWON(GIND)-LSTREF(GIND)) .NE. 0) THEN
	     LSTBRK(1,GIND) = LSTTPL(GIND) -		!TOTAL POOL (LESS REF)
     *	 		  (LSTWON(GIND) -		!WHAT IS PAYED OUT
     *			   LSTREF(GIND))	        !(LESS REF).
	  ELSEIF(LSTSTS(GIND).NE.GAMREF) THEN                                                 
	     LSTBRK(1,GIND) = 0
	     LSTPOL(2,GIND) = LSTTPL(GIND)
	     CALL UPDRDF(GNUM,LSTPOL(2,GIND),DRAW)
	  END IF

	CALL GAMLOG(TSTR,GIND,DSTREC,LSTSTS)
	CALL WRITEW(FDB,DRAW,DSTREC,ST)
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
