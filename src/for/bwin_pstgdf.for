C SUBROUTINE BWIN_PSTGDF.FOR
C
C V05 15-DEC-1999 UXN MULTIWIN changes.
C V04 07-JAN-1995 HXK Removed game status being set to done
C                     (this is now done by SHARECLC)
C V03 08-DEC-1994 HXK Use LBNSTS instead of DBNSTS
C V02 07-DEC-1994 HXK Set game staus to GAMDON as shareclc/shareupd is not
C                      run as prizes are fixed
C V01 27-OCT-1994 HXK Initial revision.
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
	SUBROUTINE BWIN_PSTGDF
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DBNREC.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'
	INTEGER*4 FDB(7)
	INTEGER*4 GIND, DRAW, GNUM, K, ST, DAY
C
C
	DO 1000 GIND = 1,NUMBGO
	    DRAW = LBNDRW(GIND)
	    IF(DRAW.LT.1) GOTO 1000
	    GNUM = GTNTAB(TBNG,GIND)
	    WRITE(6,900) IAM(),(GFNAMES(K,GNUM),K=1,5)
	    CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	    CALL IOINIT(FDB,3,DBNSEC*256)
	    IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
	    CALL READW(FDB,DRAW,DBNREC,ST)
	    IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
C
C COMPARE WINSEL SALES FIGURES WITH DAILY SALES FIGURES
C
	    DO DAY = 1,BGOENT
		IF(DBNSAL(DAY).NE.LBNSAL(DAY,GIND).AND.DAY.NE.2) THEN
		    CALL BELLS(2)
		    WRITE(6,901) IAM(),GTNAMES(TBNG),GIND,DAY,
     *			         CMONY(DBNSAL(DAY),12,BETUNIT),
     *			         CMONY(LBNSAL(DAY,GIND),12,BETUNIT)
		    CALL GPAUSE
		ENDIF
	    ENDDO 
C
	    CALL GAMLOG(TBNG,GIND,DBNREC,LBNSTS)
	    CALL WRITEW(FDB,DRAW,DBNREC,ST)
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
