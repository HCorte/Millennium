C WIN_WININT_TKIK.FOR
C
C V06 19-MAY-2009 FRP Modify for EM Joker: allow Jok drw a day other than today
C V05 12-DEC-2000 UXN IS_ADDED() function added.
C V04 03-DEc-2000 UXN Totogolo added.
C V03 13-APR-2000 UXN Fix for VAKIO.
C V02 13-MAR-2000 UXN LOGGAM,GAMLOG ADDED FOR VAKIO.
C V01 01-MAR-2000 UXN SEPARATED FROM WIN_WININT
C
C SUBROUTINE TO INITIALIZE WINNER SELECTION COMMON FOR JOKER
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE WIN_WININT_TKIK(FILES,FTYPE,FILCNT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:DKKREC.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'
C
C
	INTEGER*4 FDB(7), FTYPE(200)
	CHARACTER*20 FILES(200)
	CHARACTER*20 TMPSTR
	INTEGER*4 WNUM, I, MNUM, DIV, CDC, ST, GNUM, DRAW
	INTEGER*4 GIND, FILCNT, J, K, EXT
        INTEGER*4 GAM

	INTEGER*4   INPLEN
C
        INTEGER*4   KVOL
        CHARACTER*4 CXKVOL
        EQUIVALENCE (KVOL,CXKVOL)
C
	LOGICAL*4   IS_ADDED
	EXTERNAL    IS_ADDED
C
        CHARACTER STRING*50
C
C
	CALL FASTSET(-1000,LKKDRW,NUMKIK)
	CALL FASTSET(0,KADVSAL,MAXGAM*NUMKIK)
	KVOL=P(REG_DRWPCK)
C
	DO 300 GIND=1,NUMKIK
	   GNUM=GTNTAB(TKIK,GIND)
	   IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) GOTO 300
C	   IF(KIKDAT(CURDRW,GIND).NE.DAYCDC) GOTO 300

C	   DRAW=KIKDRW(GIND)

           IF(STOPMOD.EQ.WINMANUAL) THEN
              WRITE (STRING,800) (GLNAMES(K,GNUM),K=1,4),GIND
              CALL PRMNUM(STRING,DRAW,1,999999,EXT)
              IF (EXT .LT. 0) GOTO 300
           ELSE
              DRAW = DRWGAM(MLWININD,GNUM)
              IF ((DRWSTS(MLWININD,GNUM).NE.WINYES).AND.
     *            (DRWSTS(MLWININD,GNUM).NE.WINPRV)) DRAW=0
              IF(DRAW.EQ.0) GOTO 300
           ENDIF

	   WRITE(6,930) IAM(), GTNAMES(TKIK),GIND,DRAW

	   CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
	   CALL IOINIT(FDB,1,DKKSEC*256)
	   IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
	   CALL READW(FDB,DRAW,DKKREC,ST)
	   IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
	   CALL CLOSEFIL(FDB)

  	   CALL LOGGAM(TKIK,GIND,DKKREC,LKKSTS)
	   IF(LKKSTS(GIND).LT.GAMENV) THEN
	      WRITE(6,920) IAM(),GTNAMES(TKIK),GIND,LKKSTS(GIND),GAMENV
	      CALL GPAUSE
	   ENDIF

	   IF(LKKDAT(CURDRW,GIND).NE.DAYCDC) GOTO 300

      	   DO 310 GAM=1,MAXGAM
              CALL FASTSET(0,LKKSAL(1,GAM,GIND),KIGENT)
310        CONTINUE
C
C OLD KICKER SALES
C
 	   DO I=16, MAXDRW
	      LKKBAL(I,GIND) = 0
	      KIKBAL(I,GIND) = 0
	   ENDDO

	   CALL FASTSET(0,LKKSHR(1,GIND),KIGDIV) 

	   DO 280 DIV=1,LKKDIV(GIND)
              DO 280 J=1,3
	         MNUM=LKKMAT(J,DIV,GIND)
	         DO 280 I=1,7
  	            KMATCH(I,J,DIV,GIND)=MOD(MNUM,10)
	            MNUM=MNUM/10
280	   CONTINUE

	   WNUM=LKKWIN(GIND)

	   DO 290 I=1,7
	      KWINNUM(I,GIND)=MOD(WNUM,10)
	      WNUM=WNUM/10
290	   CONTINUE

C
           IF(KVOL.EQ.0) THEN
             CALL PRMTEXT('Enter Joker draw pack volume name: ',CXKVOL, INPLEN)
           ENDIF

           CALL OPENW(1,SFNAMES(1,DAF),4,0,0,ST)
           CALL IOINIT(FDB,1,DAFSEC*256)
           IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),1,ST,0)
           DO 295 CDC=LKKBSD(GIND),LKKESD(GIND)
              CALL READW(FDB,CDC,DAFREC,ST)
      	      IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),2,ST,CDC)
      	      IF(DAFSTS.EQ.DNOSAL) GOTO 295
	      DO 296 I=1,MAXGAM
C	         IF(KGNTAB(I).NE.GNUM)  GOTO 296
	         IF(KGNTAB(I).NE.GNUM .OR. DAFDRW(I).LE.0)  GOTO 296   ! DAFDRW TEST AVOIDS WINSEL TRYING TO OPEN DRAW
								       ! FILES WHEN GAME DOES NOT SELL ON THE PERIOD (EXTRAS) 
      	         WRITE(TMPSTR,900) KVOL,GSNAMES(I),CDC
		 IF(IS_ADDED(TMPSTR,FILES,FILCNT)) GOTO 296
      	         FILCNT = FILCNT + 1
      	         FTYPE(FILCNT) = 0
		 FILES(FILCNT) = TMPSTR
296           CONTINUE
295   	  CONTINUE
          CALL CLOSEFIL(FDB)
	  WINREP_AUTO(GNUM) = DRAW
300     CONTINUE
C
C
800     FORMAT('Enter ',4A4,I1,' draw number [E-none]    ')
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

	LOGICAL*4 FUNCTION IS_ADDED(STR,FILES,FILCNT)
	IMPLICIT NONE
	CHARACTER*20 STR
	INTEGER*4    FILCNT
	CHARACTER*20 FILES(200)
	    
	INTEGER*4    I

	IS_ADDED = .FALSE.

	DO I=1,FILCNT
	   IF(FILES(I).EQ.STR) THEN
	      IS_ADDED = .TRUE.
	      RETURN
	   ENDIF
	ENDDO
	END
