C SUBROUTINE SSWIN_WININT
C 
C V08 03-JUL-2000 UXN LSSLAT added.
C V07 15-FEB-2000 UXN P(ODD_DRWPCK) added.
C V06 29-DEC-1999 OXK DRWSTS checking added.
C V05 13-DEC-1999 OXK MULTIWIN changes.
C V04 12-JUL-1999 UXN More variables initialized making possible to re-run
C                     WINSEL
C V03 27-APR-1999 RXK Call of INPNUM replaced with call of PRMNUM.
C                     Event # asked only if manual winner selection.
C V02 23-JAN-1998 UXN Automated WINREP and BKKREP added.
C V01 23-JAN-1998 RXK Initial release.
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
	SUBROUTINE SSWIN_WININT(FILES,FILCNT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:SSCCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:DSSREC.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'
C
	INTEGER*4 FDB(7)
	INTEGER*4 FILCNT, GIND, GNUM, DRAW, EXT, ST, K, CDC
	INTEGER*4 SVOL, INPLEN
        INTEGER*4 AWNTAB(2,NUMAGT)
C
	CHARACTER CXSVOL*4
	CHARACTER FILES(200)*20
	CHARACTER STRING*50
C
	EQUIVALENCE (SVOL,CXSVOL)
C
        COMMON/BIGWIN/ AWNTAB
C
C
        CALL FASTSET(0,AWNTAB,2*NUMAGT)
	CALL FASTSET(-1000,LSSDRW,NUMSSC)
	FILCNT=0
	SVOL=P(ODD_DRWPCK)
C
C
	DO 100 GIND = 1,NUMSSC
	   GNUM = GTNTAB(TSSC,GIND)
	   IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) GOTO 100

           IF(STOPMOD.EQ.WINMANUAL) THEN
	      WRITE (STRING,800) (GLNAMES(K,GNUM),K=1,4),GIND
	      CALL PRMNUM(STRING,DRAW,1,999999,EXT)
	      IF(EXT.LT.0) GOTO 100
           ELSE
              DRAW = DRWGAM(MLWININD,GNUM)
              IF ((DRWSTS(MLWININD,GNUM).NE.WINYES).AND.
     *            (DRWSTS(MLWININD,GNUM).NE.WINPRV)) DRAW=0
              IF(DRAW.EQ.0) GOTO 100
           ENDIF

	   WRITE(6,930) IAM(),GTNAMES(TSSC),GIND,DRAW
	   CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
	   CALL IOINIT(FDB,1,DSSSEC*256)
	   IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
	   CALL READW(FDB,DRAW,DSSREC,ST)
	   IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
	   CALL LOGGAM(TSSC,GIND,DSSREC,LSSSTS)
	   IF(LSSSTS(GIND).LT.GAMENV.OR.
     *	      LSSSTS(GIND).EQ.GFINAL.OR.
     *	      LSSSTS(GIND).EQ.GAMREF) THEN
	      WRITE(6,920) IAM(),GTNAMES(TSSC),GIND,LSSSTS(GIND),GAMENV
	      CALL GPAUSE
	   ENDIF

           IF(LSSSTS(GIND).EQ.GAMREF) LSSSTS(GIND) = GAMCAN

	   LSSLAT(LATCNT,GIND) = 0
	   LSSLAT(LATAMT,GIND) = 0

	   LSSSAL(GIND)=0
	   LSSWON(GIND)=0
	   LSSTAX(GIND)=0

           LSSREF(GIND)   = 0
           CALL FASTSET(0,LSSWRA(1,1,GIND),NUMTOT*2)
           CALL FASTSET(0,LSSWPR(1,1,GIND),NUMTOT*2)
           CALL FASTSET(0,LSSWRO(1,1,GIND),NUMTOT*2)
           CALL FASTSET(0,LSSWPA(1,1,GIND),NUMTOT*2)

           CALL FASTSET(0,LSSWPO(1,1,GIND),NUMTOT*2)
	   IF(SVOL.EQ.0) THEN
	      WRITE(STRING,810) (GLNAMES(K,GNUM),K=1,4),GIND
	      CALL PRMTEXT(STRING,CXSVOL,INPLEN)
	   ENDIF
	   CALL CLOSEFIL(FDB)
C
C
	   CALL OPENW(1,SFNAMES(1,DAF),4,0,0,ST)
	   CALL IOINIT(FDB,1,DAFSEC*256)
	   IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),1,ST,0)
	   DO 90 CDC=LSSBSD(GIND),LSSESD(GIND)
	      CALL READW(FDB,CDC,DAFREC,ST)
	      IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),2,ST,CDC)
	      IF(DAFSTS.EQ.DNOSAL) GOTO 90
	      IF(DAFSTS.EQ.DSOPEN) GOTO 90
	      FILCNT=FILCNT+1
	      WRITE (FILES(FILCNT),900) SVOL,GSNAMES(GNUM),CDC
90	   CONTINUE
	   CALL CLOSEFIL(FDB)
	   WINREP_AUTO(GNUM) = DRAW
	   BKKREP_AUTO(GNUM) = DRAW
100	CONTINUE
C
C
800	FORMAT('Enter ',4A4,I1,' event code (E-none) ')
810	FORMAT('Enter ',4A4,I1,' draw pack volume name: ')
900	FORMAT(A4,':',A4,I4.4,'.FIL')
920	FORMAT(1X,A,1X,A8,I1,' invalid game status> ',I4,' should be> ',I4)
930	FORMAT(1X,A,1X,'Loading game date for ',A8,I1,' event> ',I4,
     *	       ' winner selection')
940	FORMAT(A4)
	END
