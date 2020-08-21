C
C SUBROUTINE WIN_WININT_TSPT
C
C SUBROUTINE TO INITIALIZE WINNER SELECTION COMMON FOR SPORTS GAMES
C
C VO3 27-APR-2017 MTK Modified for cancelled draws
C V02 05-JUN-2000 UXN FIX FOR POSTPONING POSTPONED DRAWS.
C V01 01-MAR-2000 UXN INITIAL RELEASE.
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
	SUBROUTINE WIN_WININT_TSPT(FILES,FTYPE,FILCNT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:DSPREC.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'
	INTEGER*4 FDB(7), FTYPE(200)
	CHARACTER*20 FILES(200)
	INTEGER*4 CDC, ST, GNUM, DRAW
	INTEGER*4 GIND, FILCNT, FLAG, EXT, K
	CHARACTER STRING*50
C
	INTEGER*4   INPLEN
C
	INTEGER*4   SVOL
	CHARACTER*4 CXSVOL
	EQUIVALENCE (SVOL,CXSVOL)
C
C
	CALL FASTSET(-1000,LSPDRW,NUMSPT)
	CALL FASTSET(0,SADVSAL,NUMSPT)
	SVOL=P(REG_DRWPCK)
C
	DO 100 GIND=1, NUMSPT
	   GNUM=GTNTAB(TSPT,GIND)
	   IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) GOTO 100
C
           IF(STOPMOD.EQ.WINMANUAL) THEN
              WRITE (STRING,800) (GLNAMES(K,GNUM),K=1,4),GIND
              CALL PRMNUM(STRING,DRAW,1,999999,EXT)
              IF (EXT .LT. 0) GOTO 100
           ELSE
              DRAW = DRWGAM(MLWININD,GNUM)
              IF ((DRWSTS(MLWININD,GNUM).NE.WINYES).AND.
     *            (DRWSTS(MLWININD,GNUM).NE.WINPRV)) DRAW=0
              IF(DRAW.EQ.0) GOTO 100
           ENDIF
C
	   WRITE(6,802) IAM(),(GLNAMES(K,GNUM),K=1,4),DRAW
	   CALL PRMYESNO('at a later date [Y/N]',FLAG)
	   IF(FLAG.EQ.1) THEN
	        SPDELAY(GIND) = 2
		SPDELDR(GIND) = DRAW
	   ELSE
	      IF(DRAW.NE.DAYDRW(GNUM).OR.    ! It is postponed draw ???
     *           (GIND.EQ.1.AND.KIKDAT(CURDRW,1).GT.SPTDAT(CURDRW,1))) THEN	
	         WRITE(6,801) IAM(),(GLNAMES(K,GNUM),K=1,4),DRAW
	         SPDELAY(GIND) = 1
	         SPDELDR(GIND) = DRAW
	      ELSE
	         SPDELAY(GIND) = 0
		 SPDELDR(GIND) = 0
	      ENDIF
	   ENDIF
   	   WRITE(6,930) IAM(),GTNAMES(TSPT),GIND,DRAW

	   CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
	   CALL IOINIT(FDB,1,DSPSEC*256)
	   IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
	   CALL READW(FDB,DRAW,DSPREC,ST)
	   IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
	   CALL CLOSEFIL(FDB)

	   CALL LOGGAM(TSPT,GIND,DSPREC,LSPSTS)
           IF(LSPDCD(GIND).NE.0.AND.LSPSTS(GIND).LT.GAMENV)       !V03
     *       LSPSTS(GIND) = GAMENV                                !V03
	   IF(LSPSTS(GIND).LT.GAMENV.AND.SPDELAY(GIND).NE.2) THEN
	      WRITE(6,920) IAM(),GTNAMES(TSPT),GIND,LSPSTS(GIND),GAMENV
	      CALL GPAUSE
	   ENDIF
	   CALL FASTSET(0,LSPSAL(1,GIND),SPGENT)
	   CALL FASTSET(0,LSPSHR(1,GIND),SPGDIV)
C
	   IF(SVOL.EQ.0) THEN
	      CALL PRMTEXT('Enter Sports draw pack volume name: ',CXSVOL,INPLEN)
	   ENDIF

	   CALL OPENW(1,SFNAMES(1,DAF),4,0,0,ST)
	   CALL IOINIT(FDB,1,DAFSEC*256)
	   IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),1,ST,0)
	   DO 10 CDC=LSPBSD(GIND),LSPESD(GIND)
	      CALL READW(FDB,CDC,DAFREC,ST)
	      IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),2,ST,CDC)
	      IF(DAFSTS.EQ.DNOSAL) GOTO 10
	      FILCNT=FILCNT+1
	      FTYPE(FILCNT)=0
	      WRITE (FILES(FILCNT),900) SVOL,GSNAMES(GNUM),CDC
10	   CONTINUE
	   CALL CLOSEFIL(FDB)
C
C SET WINREP_AUTO FOR WINRPT.
C
	   IF(SPDELAY(GIND).NE.2) THEN
	      WINREP_AUTO(GNUM) = DRAW
	   ENDIF
C
100	CONTINUE
C
800     FORMAT('Enter ',4A4,I1,' draw number [E-none]    ')
801	FORMAT(1X,A,'Postponed winner selection for ',4A4,' draw ',I4)
802     FORMAT(1X,A,'Do you want to run ', 4A4, ' draw ', I4,
     *         ' winner selection')
900	FORMAT(A4,':',A4,I4.4,'.FIL')
920	FORMAT(1X,A,A8,I1,' invalid game status> ',I4,' should be> ',I4)
930	FORMAT(1X,A,1X,'Loading game data for ',A8,I1,' draw> ',I4,
     *	       ' winner selection')
950	FORMAT(1X,A,A8,I1,' winner selection will be run for ',
     *	       A8,I1)
960	FORMAT(1X,A,'Winner selection will only run this postponed ',
     *	          'event.')
970	FORMAT(1X,A,'You must Re-run winner selection if needed for '
     *	 'any other games closed today   ')
	END
