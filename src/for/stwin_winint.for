C SUBROUTINE STWIN_WININT
C
C V06 03-JUL-2000 UXN LSTLAT added.
C V05 15-FEB-2000 UXN P(ODD_DRWPCK) added.
C V04 29-DEC-1999 OXK DRWSTS checking added.
C V03 13-DEC-1999 OXK MULTIWIN changes.
C V02 12-JUL-1999 UXN More variables initialized making possible to re-run
C                     WINSEL
C V01 17-MAY-1999 UXN Initial release.  
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

C=======OPTIONS /CHECK=NOOVERFLOW

	SUBROUTINE STWIN_WININT(FILES,FILCNT)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:STRCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:DSTREC.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEf'

C---- Local Variables.

	INTEGER*4 FDB(7)
	INTEGER*4 FILCNT, WVOL, GIND, DRAW, EXT, GNUM, ST, I, K
	INTEGER*4 CDC, INPLEN
	CHARACTER FILES(200)*20   
	CHARACTER STRING*50
	CHARACTER CXWVOL*4

	EQUIVALENCE (WVOL,CXWVOL)

	INTEGER*4 AWNTAB(2,NUMAGT)
	COMMON/BIGWIN/ AWNTAB

C-------------------- Start of code -----------------------------------

	CALL FASTSET(0,AWNTAB,2*NUMAGT)

	CALL FASTSET(-1000,LSTDRW,NUMSTR)

	FILCNT = 0
	WVOL = P(ODD_DRWPCK)

	DO 100 GIND = 1,NUMSTR
	   GNUM = GTNTAB(TSTR,GIND)
	   IF (GNUM .LT. 1 .OR. GNUM .GT. MAXGAM) GOTO 100
	   STREFUND(GIND) = .FALSE.

           IF(STOPMOD.EQ.WINMANUAL) THEN
	      WRITE (STRING,800) (GLNAMES(K,GNUM),K=1,4)
	      CALL PRMNUM(STRING,DRAW,1,999999,EXT)
	      IF (EXT .LT. 0) GOTO 100
           ELSE
              DRAW = DRWGAM(MLWININD,GNUM)
              IF ((DRWSTS(MLWININD,GNUM).NE.WINYES).AND.
     *            (DRWSTS(MLWININD,GNUM).NE.WINPRV)) DRAW=0
              IF(DRAW.EQ.0) GOTO 100
           ENDIF

	   WRITE(6,930) IAM(),GTNAMES(TSTR),GIND,DRAW
	   CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
	   CALL IOINIT(FDB,1,DSTSEC*256)
	   IF (ST .NE. 0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
	   CALL READW(FDB,DRAW,DSTREC,ST)
	   IF (ST .NE. 0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
	   CALL LOGGAM(TSTR,GIND,DSTREC,LSTSTS)
	   IF (LSTSTS(GIND) .EQ. GAMCAN .OR.
     *         LSTSTS(GIND) .EQ. GAMREF) THEN
	      STREFUND(GIND) = .TRUE.
	   ELSE
	      DO I=1,MAXSTRRW
		   IF (LSTSTA(I,GIND) .EQ. GAMCAN) THEN
		      STREFUND(GIND) = .TRUE.
		      WRITE(6,910) IAM(),GTNAMES(TSTR),GIND,I
		   ENDIF
	      ENDDO	    
	   END IF

	   IF ( LSTSTS(GIND) .LT. GAMENV .OR.
     *	        LSTSTS(GIND) .EQ. GFINAL .OR.
     *	        LSTSTS(GIND) .EQ. GAMREF) THEN
	     WRITE(6,920) IAM(),GTNAMES(TSTR),GIND,LSTSTS(GIND),GAMENV
	     CALL GPAUSE
	   ENDIF
	   
	   IF(LSTSTS(GIND).EQ.GAMREF) LSTSTS(GIND) = GAMCAN

	   IF(LSTLAT(LATCDC,GIND).GT.0) STREFUND(GIND) = .TRUE.
	   LSTLAT(LATCNT,GIND) = 0
	   LSTLAT(LATAMT,GIND) = 0

	   LSTSAL(1,GIND) = 0
	   LSTSAL(2,GIND) = 0
	   LSTTAX(GIND) = 0

	   LSTREF(GIND)   = 0
	   LSTWON(GIND)   = 0
	   CALL FASTSET(0,LSTWCP(1,GIND),MAXSTRTI)
	   CALL FASTSET(0,LSTWBT(1,1,GIND),NUMTOT*MAXSTRTI)
           CALL FASTSET(0,LSTWRA(1,1,GIND),NUMTOT*2)
           CALL FASTSET(0,LSTWPR(1,1,GIND),NUMTOT*2)
           CALL FASTSET(0,LSTWRO(1,1,GIND),NUMTOT*2)
           CALL FASTSET(0,LSTWPA(1,1,GIND),NUMTOT*2)

           CALL FASTSET(0,LSTWPO(1,1,GIND),NUMTOT*2)
           CALL FASTSET(0,LSTHST(1,GIND),MAXSTRTI)
	   IF(WVOL.EQ.0) THEN
	      WRITE(STRING,810) (GLNAMES(K,GNUM),K=1,4)
	      CALL PRMTEXT(STRING,CXWVOL,INPLEN)
	   ENDIF
	   CALL CLOSEFIL(FDB)

	   CALL OPENW(1,SFNAMES(1,DAF),4,0,0,ST)
	   CALL IOINIT(FDB,1,DAFSEC*256)
	   IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),1,ST,0)
	   DO 90 CDC = LSTBSD(GIND),LSTESD(GIND)
	      CALL READW(FDB,CDC,DAFREC,ST)
	      IF (ST .NE. 0) CALL FILERR(SFNAMES(1,DAF),2,ST,CDC)
	      IF (DAFSTS .EQ. DNOSAL) GOTO 90
	      IF (DAFSTS .EQ. DSOPEN) GOTO 90
	      FILCNT = FILCNT+1
	      WRITE (FILES(FILCNT),900) WVOL,GSNAMES(GNUM),CDC
90	   CONTINUE
	   CALL CLOSEFIL(FDB)
	   WINREP_AUTO(GNUM) = DRAW
	   BKKREP_AUTO(GNUM) = DRAW
100	CONTINUE

	RETURN

C------------------------- Format Statements ----------------------------

800	FORMAT('Enter ',4A4,' event code (E-none)    ')

810	FORMAT('Enter ',4A4,' draw pack volume name: ')

900	FORMAT(A4,':',A4,I4.4,'.FIL')

910	FORMAT(1X,A,1X,A8,I1,' refund scan for row ',I2)

920	FORMAT(1X,A,1X,A8,I1,' invalid game status> ',I4,' should be> ',I4)

930	FORMAT(1X,A,1X,'Loading game date for ',A8,I1,' event> ',I4,
     *	       ' winner selection')

940	FORMAT(A4)


	END
