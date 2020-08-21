C SUBROUTINE CWIN_WININT
C
C V09 03-JUL-2000 UXN LCPLAT added.
C V08 15-FEB-2000 UXN P(ODD_DRWPCK) added.
C V07 29-DEC-1999 OXK DRWSTS checking added.
C V06 13-DEC-1999 OXK MULTIWIN changes.
C V05 12-JUL-1999 UXN More variables initialized making possible to re-run
C                     WINSEL
C V04 27-APR-1999 RXK Call of INPNUM replaced with call of PRMNUM
C                     Event # asked only if manual winner selection.
C V03 10-NOV-1997 UXN WINREP_AUTO,BKKREP_AUTO ADDED FOR AUTOMATED WINRPT AND
C	  	      BKKREP.
C V02 19-FEB-1996 HXK Fix for HST count
C V01 23-nov-1995 PXB Initial revision.
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

	SUBROUTINE CWIN_WININT(FILES,FILCNT)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:CPLCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:DCPREC.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'

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

	CALL FASTSET(-1000,LCPDRW,NUMCPL)

	FILCNT = 0
	WVOL = P(ODD_DRWPCK)

	DO 100 GIND = 1,NUMCPL
	   GNUM = GTNTAB(TCPL,GIND)

	   IF (GNUM .LT. 1 .OR. GNUM .GT. MAXGAM) GOTO 100

	   CPREFUND(GIND) = .FALSE.

           IF(STOPMOD.EQ.WINMANUAL) THEN
	      WRITE (STRING,800) (GLNAMES(K,GNUM),K=1,4),GIND
	      CALL PRMNUM(STRING,DRAW,1,999999,EXT)
	      IF (EXT .LT. 0) GOTO 100
           ELSE
              DRAW = DRWGAM(MLWININD,GNUM) 
	      IF ((DRWSTS(MLWININD,GNUM).NE.WINYES).AND.
     *	          (DRWSTS(MLWININD,GNUM).NE.WINPRV)) DRAW=0
              IF(DRAW.EQ.0) GOTO 100
           ENDIF

	   WRITE(6,930) IAM(),GTNAMES(TCPL),GIND,DRAW

	   CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)

	   CALL IOINIT(FDB,1,DCPSEC*256)

	   IF (ST .NE. 0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	   CALL READW(FDB,DRAW,DCPREC,ST)

	   IF (ST .NE. 0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

	   CALL LOGGAM(TCPL,GIND,DCPREC,LCPSTS)

	   IF (LCPSTS(GIND) .EQ. GAMCAN .OR.
     *         LCPSTS(GIND) .EQ. GAMREF) THEN
	      CPREFUND(GIND) = .TRUE.
	   ELSE
	      DO 105 I=1,MAXCPLRW
		 IF (LCPSTA(I,GIND) .EQ. GAMCAN .OR.
     *		     LCPSTA(I,GIND) .EQ. GAMREF) THEN
		    CPREFUND(GIND) = .TRUE.
		    WRITE(6,910) IAM(),GTNAMES(TCPL),GIND,I
		 END IF
105	      CONTINUE
	   END IF

	   IF ( LCPSTS(GIND) .LT. GAMENV .OR.
     *	        LCPSTS(GIND) .EQ. GFINAL .OR.
     *	        LCPSTS(GIND) .EQ. GAMREF) THEN
	     WRITE(6,920) IAM(),GTNAMES(TCPL),GIND,LCPSTS(GIND),GAMENV
	     CALL GPAUSE
	   END IF

           IF(LCPSTS(GIND).EQ.GAMREF) LCPSTS(GIND) = GAMCAN

	   IF(LCPLAT(LATCDC,GIND).GT.0) CPREFUND(GIND) = .TRUE.
	   LCPLAT(LATCNT,GIND) = 0
	   LCPLAT(LATAMT,GIND) = 0

	   LCPSAL(1,GIND) = 0
	   LCPSAL(2,GIND) = 0
	   LCPTAX(GIND) = 0

           LCPREF(GIND)   = 0
           LCPWON(GIND)   = 0
           CALL FASTSET(0,LCPWCP(1,GIND),MAXCPLTI)
           CALL FASTSET(0,LCPWBT(1,1,GIND),NUMTOT*MAXCPLTI)
           CALL FASTSET(0,LCPWRA(1,1,GIND),NUMTOT*2)
           CALL FASTSET(0,LCPWPR(1,1,GIND),NUMTOT*2)
           CALL FASTSET(0,LCPWRO(1,1,GIND),NUMTOT*2)
           CALL FASTSET(0,LCPWPA(1,1,GIND),NUMTOT*2)

           CALL FASTSET(0,LCPWPO(1,1,GIND),NUMTOT*2)
	   CALL FASTSET(0,LCPSBR(1,GIND),MAXCPLRW)
           CALL FASTSET(0,LCPHST(1,GIND),MAXCPLTI)
	   IF(WVOL.EQ.0) THEN
	      WRITE(STRING,810) (GLNAMES(K,GNUM),K=1,4),GIND
	      CALL PRMTEXT(STRING,CXWVOL,INPLEN)
	   ENDIF
	   CALL CLOSEFIL(FDB)

	   CALL OPENW(1,SFNAMES(1,DAF),4,0,0,ST)

	   CALL IOINIT(FDB,1,DAFSEC*256)

	   IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),1,ST,0)

	   DO 90 CDC = LCPBSD(GIND),LCPESD(GIND)
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

800	FORMAT('Enter ',4A4,I1,' event code (E-none)    ')

810	FORMAT('Enter ',4A4,I1,' draw pack volume name: ')

900	FORMAT(A4,':',A4,I4.4,'.FIL')

910	FORMAT(1X,A,1X,A8,I1,' refund scan for row ',I2)

920	FORMAT(1X,A,1X,A8,I1,' invalid game status> ',I4,' should be> ',I4)

930	FORMAT(1X,A,1X,'Loading game date for ',A8,I1,' event> ',I4,
     *	       ' winner selection')

940	FORMAT(A4)


	END
