C SUBROUTINE UPDINT
C
C SUBROUTINE TO INITIALIZE GAME RECORDS FOR WINUPD
C
C V20 02-JUN-2017 JHR SET THE JOKER HIGH DRAW DUE THE JOKER HAS BEEN REMOVED
C V19 28-JAN-2011 HXK LOTO2 CHANGES: TIR 2321 fix week/ccc
C V18 15-DEC-2003 FRP Modify for Batch2 Totobola Changes.
C V17 01-DEC-2000 UXN Totogolo added.
C V16 10-JAN-2001 EPH Keep always kicker LKK filled for OPS value calculation
C V15 22-MAY-2000 OXK Check D%%STS also when posting big prizes (PPP,WRL)
C V14 11-APR-2000 UXN WINUPD.DEF added.
C V13 15-FEB-2000 UXN BIGWRL flag added.
C V12 13-DEC-1999 OXK MULTIWIN changes.
C V11 13-OCT-1999 RXK World Tour game added.
C V10 21-MAY-1999 UXN MAXGAM changes.
C V09 27-APR-1999 RXK STOPSYS optimization. Call of INPNUM and WIMG/YESNO 
C                     replaced with call of PRMNUM and PRMYESNO.
C V08 01-APR-1999 UXN UPDTSK_AUTO contains now draw numbers for the
C                     games, that had UPDTSK run. This information
C                     is used only by FBNKWN and only for TEBE big winners. 
C V07 25-FEB-1998 UXN AUTOPROMT added.
C V06 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting, 
C                     Instant Pass Thru Phase 1
C V05 23-NOV-1994 HXK Added BINGO
C V04 10-JAN-1994 HXK FIXED FORMAT ERROR.
C V03 07-SEP-1993 HXK Check for BIGPPP flag
C V02 01-SEP-1993 HXK Added BIGPPP logical, etc.
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
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
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE UPDINT(CLOSE_CDC)          !V16
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'

	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DLTREC.DEF'
	INCLUDE 'INCLIB:DSPREC.DEF'
	INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:DBNREC.DEF'
        INCLUDE 'INCLIB:DTGREC.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'
	INCLUDE 'INCLIB:WINUPD.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 FDB(7)
	INTEGER*4 FLAG, DRAW, GNUM, GIND, EXT, GTYP, K, GCNT, ST
C	CHARACTER*32 STRING
	INTEGER*4 CC(MAXGAM)

	INTEGER*4 SELECTED(MAXGAM)              !V16
        INTEGER*4 WEEK, YEAR                    !V16

	INTEGER*4 GETDRW    !FUNCTION           !V16

	INTEGER*4 CLOSE_CDC(*)                  !V16

	DATA CC/'1st ','2nd ','3rd ','4th ','5th ','6th ','7th ','8th ',
     *          '9th ','10th',40*'    '/
C

	CALL GETSCONF(SCFREC,ST)
	IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)

C
C GET GAMES AND DRAWS TO BE UPDATED
C ---------------------------------
	GCNT=1
	CALL FASTSET(-1,LLTDRW,NUMLTO)
	CALL FASTSET(-1,LSPDRW,NUMSPT)
	CALL FASTSET(-1,LKKDRW,NUMKIK)
        CALL FASTSET(-1,LBNDRW,NUMBGO)
        CALL FASTSET(-1,LTGDRW,NUMTGL)
	CALL FASTSET(0,SELECTED,MAXGAM)
	CALL FASTSET(1,UPDIND,MAXGAM)


100	CONTINUE
	IF(GCNT.GT.MAXGAM) RETURN
	WRITE(6,900) IAM(),CC(GCNT),(IAM(),K,GTNAMES(K),K=1,MAXTYP)
	CALL PRMNUM('Enter Game Type [E: Exit]', GTYP, 1, MAXTYP, EXT)
	IF(EXT.LT.0) RETURN
	CALL PRMNUM('Enter Game Index [E: Exit]', GIND, 1, MAXIND, EXT)
	IF(EXT.LT.0) RETURN

	GNUM=SCFGTN(GTYP,GIND)

        IF(GNUM.LE.0.OR.GNUM.GT.MAXGAM) THEN
C           TYPE*,IAM(),'Jogo nao habilitado'
           TYPE*,IAM(),'Game NOT active'
           GOTO 100
        ENDIF

	IF(GTYP.NE.TLTO.AND.GTYP.NE.TSPT.AND.GTYP.NE.TKIK.AND.
     *     GTYP.NE.TBNG.AND.GTYP.NE.TTGL) THEN
	  WRITE(6,901) IAM()
	  GOTO 100
	ENDIF

        WRITE(6,777)         							  !V16
777     FORMAT(//,1X,'Enter YEAR and WEEK/CCC for this game:')
	CALL PRMNUM('Enter the year: ',YEAR,2000,3000,EXT)			  !V16
        IF (EXT.LT.0) RETURN							  !V16
	CALL PRMNUM('Enter the week OR ccc: ',WEEK,1,105,EXT)			  !V16
        IF (EXT.LT.0) RETURN							  !V16

	IF(GTYP.EQ.TLTO.AND.GIND.LT.3.AND.WEEK.GT.54) THEN
	  TYPE*,IAM(),'WEEK value entered must not exceed 54'
	  GOTO 100
	ENDIF

	IF(SELECTED(GNUM).NE.0) THEN
	    TYPE*,IAM()
	    TYPE*,IAM(),'WARNING!'
	    TYPE*,IAM(),'You can select one draw per index only '
	    TYPE*,IAM()
	    TYPE*,IAM(),'To update other draws for this index you must'
	    TYPE*,IAM(),'later re-run UPDTSK !'
	    TYPE*,IAM()
	    CALL XWAIT(5,2,EXT)	
	    UPDTSK_RUN_AGAIN = 1
	    GOTO 100
	ENDIF

	DRAW = GETDRW(YEAR,WEEK,GNUM)					!V16

        IF (DRAW.LE.0) THEN						!V16
C	   TYPE*,IAM(),' Nao foi encontrado numero interno de'		!V16
C	   TYPE*,IAM(),' concurso para Game number = ',GNUM    	        !V16
C	   TYPE*,IAM(),' Semana =', WEEK		    		!V16
C	   TYPE*,IAM(),' Ano    =', YEAR		    		!V16

	   TYPE*,IAM(),' Internal draw number not found for'
	   TYPE*,IAM(),' Game number = ',GNUM 
	   TYPE*,IAM(),' Week/CCC    = ',WEEK 
	   TYPE*,IAM(),' Year        = ',YEAR 
           TYPE*,IAM(),' <<< TYPE  CONT  TO TRY AGAIN >>>'
	   CALL GPAUSE							!V16
           GOTO 100
        ENDIF
 
C	WRITE (STRING,800) GTNAMES(GTYP),GIND				   !V16
C	CALL PRMNUM(STRING,DRAW,1,9999,EXT)				   !V16
C       IF (EXT.LT.0) RETURN						   !V16
C	WRITE(6,910) IAM(),GTNAMES(GTYP),GIND,(SCFLGN(K,GNUM),K=1,4),DRAW  !V16

	IF(GTYP.EQ.TLTO.AND.GIND.GT.2) THEN
	  WRITE(6,711) IAM(),GTNAMES(GTYP),GIND,(SCFLGN(K,GNUM),K=1,4),
     *                 YEAR,WEEK,DRAW  !V16
	ELSE
	  WRITE(6,710) IAM(),GTNAMES(GTYP),GIND,(SCFLGN(K,GNUM),K=1,4),
     *                 YEAR,WEEK,DRAW  !V16
	ENDIF
	CALL PRMYESNO('Is this correct (Y/N) ',FLAG)
	IF(FLAG.NE.1) GOTO 100	

	SELECTED(GNUM) = 1
	IF(UPDTSK_AUTO(GNUM,1).NE.0.AND.UPDTSK_AUTO(GNUM,1).NE.DRAW) THEN
	    UPDIND(GNUM) = 2
	ENDIF
C
C READ GAME RECORD
C ----------------
	CALL OPENW(1,SCFGFN(1,GNUM),4,0,0,ST)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)

C LOTTO GAME TYPE
C ---------------
	IF(GTYP.EQ.TLTO) THEN
	  CALL IOINIT(FDB,1,DLTSEC*256)
	  CALL READW(FDB,DRAW,DLTREC,ST)
	  CALL CLOSEFIL(FDB)
	  IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
	  IF(DLTSTS.NE.GAMDON) THEN
	    IF(DLTSTS.LT.GAMDON) THEN
	      TYPE*,IAM(),' Sorry, prize values not set for this draw'
	    ELSE
	      TYPE*,IAM(),
     *             ' Sorry, prize values already posted for this draw'
	    ENDIF
	    CALL GPAUSE
	  ENDIF
C         CLOSE_CDC(GNUM) = DLTESD                 !V16
          CLOSE_CDC(GNUM) = DLTDAT(CURDRW)         !V18
	  CALL LOGGAM(TLTO,GIND,DLTREC,LLTSTS)
          UPDTSK_AUTO(GNUM,UPDIND(GNUM)) = DRAW
          GSALES_AUTO(GNUM) = DRAW
	  LLTTAX(GIND)=0
	  GCNT=GCNT+1
	ENDIF

C SPORTS GAME TYPE
C ----------------
	IF(GTYP.EQ.TSPT) THEN
	  CALL IOINIT(FDB,1,DSPSEC*256)
	  CALL READW(FDB,DRAW,DSPREC,ST)
	  CALL CLOSEFIL(FDB)
	  IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
	  IF(DSPSTS.NE.GAMDON) THEN
	    IF(DSPSTS.LT.GAMDON) THEN
	      TYPE*,IAM(),' Sorry, prize values not set for this draw'
	    ELSE
	      TYPE*,IAM(),
     *             ' Sorry, prize values already posted for this draw'
	    ENDIF
	    CALL GPAUSE
	  ENDIF
C         CLOSE_CDC(GNUM) = DSPESD                !V16
          CLOSE_CDC(GNUM) = DSPDAT(CURDRW)        !V18
	  CALL LOGGAM(TSPT,GIND,DSPREC,LSPSTS)
 	  UPDTSK_AUTO(GNUM,UPDIND(GNUM)) = DRAW
          GSALES_AUTO(GNUM) = DRAW
	  LSPTAX(GIND)=0
	  GCNT=GCNT+1
	ENDIF

C
C TOTOGOLO GAME TYPE
C ----------------
        IF(GTYP.EQ.TTGL) THEN
          CALL IOINIT(FDB,1,DTGSEC*256)
          CALL READW(FDB,DRAW,DTGREC,ST)
          CALL CLOSEFIL(FDB)
          IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
          IF(DTGSTS.NE.GAMDON) THEN
            IF(DTGSTS.LT.GAMDON) THEN
              TYPE*,IAM(),' Sorry, prize values not set for this draw'
            ELSE
              TYPE*,IAM(),
     *             ' Sorry, prize values already posted for this draw'
            ENDIF
            CALL GPAUSE
          ENDIF
C         CLOSE_CDC(GNUM) = DTGESD                !V16
          CLOSE_CDC(GNUM) = DTGDAT(CURDRW)        !V18
          CALL LOGGAM(TTGL,GIND,DTGREC,LTGSTS)
          UPDTSK_AUTO(GNUM,UPDIND(GNUM)) = DRAW
          GSALES_AUTO(GNUM) = DRAW
          LTGTAX(GIND)=0
          GCNT=GCNT+1
        ENDIF

C
C BINGO GAME TYPE
C ----------------
        IF(GTYP.EQ.TBNG) THEN
          CALL IOINIT(FDB,1,DBNSEC*256)
          CALL READW(FDB,DRAW,DBNREC,ST)
          CALL CLOSEFIL(FDB)
          IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
          IF(DBNSTS.NE.GAMDON) THEN
            IF(DBNSTS.LT.GAMDON) THEN
              TYPE*,IAM(),' Sorry, prize values not set for this draw'
            ELSE
              TYPE*,IAM(),
     *             ' Sorry, prize values already posted for this draw'
            ENDIF
            CALL GPAUSE
          ENDIF
C         CLOSE_CDC(GNUM) = DBNESD                !V16
          CLOSE_CDC(GNUM) = DBNDAT(CURDRW)        !V18
          CALL LOGGAM(TBNG,GIND,DBNREC,LBNSTS)
     	  UPDTSK_AUTO(GNUM,UPDIND(GNUM)) = DRAW
          GSALES_AUTO(GNUM) = DRAW
          LBNTAX(GIND)=0
          GCNT=GCNT+1
        ENDIF
C
C KICKER GAME TYPE
C ----------------
	IF(GTYP.EQ.TKIK) THEN
	  CALL IOINIT(FDB,1,DKKSEC*256)
	  CALL READW(FDB,DRAW,DKKREC,ST)
	  CALL CLOSEFIL(FDB)
	  IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
	  IF(DKKSTS.NE.GAMDON) THEN
	    IF(DKKSTS.LT.GAMDON) THEN
	      TYPE*,IAM(),' Sorry, prize values not set for this draw'
	    ELSE
	      TYPE*,IAM(),
     *             ' Sorry, prize values already posted for this draw'
	    ENDIF
	    CALL GPAUSE
	  ENDIF
C         CLOSE_CDC(GNUM) = DKKESD              !V16
          CLOSE_CDC(GNUM) = DKKDAT(CURDRW)      !V18
	  CALL LOGGAM(TKIK,GIND,DKKREC,LKKSTS)
          UPDTSK_AUTO(GNUM,UPDIND(GNUM)) = DRAW
          GSALES_AUTO(GNUM) = DRAW
	  LKKTAX(GIND)=0
	  GCNT=GCNT+1
	ELSE									!V16                  
          IF (LKKDRW(1).LE.0 .AND. SCFKGN(GNUM).GT.0) THEN						!V16
             GNUM = SCFGTN (TKIK, 1)						!V16
             DRAW = GETDRW(YEAR,WEEK,GNUM)                                      !v16
             IF (DRAW.LE.0 .AND. DAYDRW(GNUM) .LE. 0) DRAW = DAYHDR(GNUM)       !V20 SET HIGH DRAW DUE JOKER HAS BEEN REMOVED
             IF (DRAW.LE.0) THEN
		TYPE*,IAM(),' Internal draw number not found for JOKER'
		TYPE*,IAM(),' Game number = ',GNUM 
		TYPE*,IAM(),' Week        = ',WEEK 
		TYPE*,IAM(),' Year        = ',YEAR 
	        CALL GPAUSE							!V16
             ENDIF

	     CALL OPENW(1,SCFGFN(1,GNUM),4,0,0,ST)				!V16
	     IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)			!V16
	     CALL IOINIT(FDB,1,DKKSEC*256)				        !V16
	     CALL READW(FDB,DRAW,DKKREC,ST)					!V16
	     CALL CLOSEFIL(FDB)							!V16
	     IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)			!V16

	     IF (DKKSTS.LT.GAMDON) THEN						!V16
		TYPE*,IAM(),' JOKER prize values not set for this draw.'
                TYPE*,IAM(),' Week = ',WEEK
		TYPE*,IAM(),' Year = ',YEAR 
	        CALL GPAUSE							!V16
	     ENDIF								!V16

	     IF(DKKSTS .LT. GFINAL) THEN                                        !V20
	        CALL LOGGAM(TKIK,GIND,DKKREC,LKKSTS)	                        !V16
	     ENDIF                                                              !V20
	     LKKTAX(1)=0							!V16
	  ENDIF									!V16
        ENDIF

	GOTO 100

800	FORMAT('Enter ',A8,I1,' draw number ')
900	FORMAT(//,1X,A,' Prize update ',A4,' game selection',//,
     *	        <MAXTYP>(1X,A,5X,I2,' - ',A8,/))
901	FORMAT(1X,A,' Sorry, function not available for that game type')
C910	FORMAT(1X,A,1X,A8,I1,2X,4A4,'Draw ',I5)								   !v16
710	FORMAT(1X,A,1X,A8,I1,2X,4A4,' Year ',I4,'/ Week ',I2,/,10X,'(Internal draw number = ',I5,')') !v16
711	FORMAT(1X,A,1X,A8,I1,2X,4A4,' Year ',I4,'/ CCC  ',I3,/,10X,'(Internal draw number = ',I5,')') !v16
911     FORMAT(20X,I2,' - ',4A4)
	END
