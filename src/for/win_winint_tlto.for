C
C SUBROUTINE WIN_WININT_TLTO
C
C SUBROUTINE TO INITIALIZE WINNER SELECTION COMMON FOR LOTTO
C
C V02 30-NOV-2010 MAC LUCKY NUMBER
C V01 JHR 14-FEB-2001 ADDED GSALES AUTO TO RUN GSALES LOADERS
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE WIN_WININT_TLTO(FILES,FTYPE,FILCNT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:DLTREC.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'
	INTEGER*4 FDB(7), FTYPE(200)
	CHARACTER*20 FILES(200)
	INTEGER*4 I, CDC, ST, GNUM, DRAW
	INTEGER*4 GIND, FILCNT, FLAG, EXT, K

	CHARACTER STRING*50
C
	INTEGER*4   INPLEN
	INTEGER*4   LVOL
	CHARACTER*4 CXLVOL
	EQUIVALENCE (LVOL,CXLVOL)
C
	CALL FASTSET(-1000,LLTDRW,NUMLTO)
	CALL FASTSET(0,LADVSAL,NUMLTO)
	LVOL=P(REG_DRWPCK)
C
C POSTPONED LOTTO WINSEL
C
	DO 100 GIND=1, NUMLTO
	   GNUM=GTNTAB(TLTO,GIND)
	   IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) GOTO 100

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
           IF(DRAW.NE.DAYDRW(GNUM)) THEN        ! It is postponed draw
              WRITE(6,801) IAM(),(GLNAMES(K,GNUM),K=1,4),DRAW
              LTDELAY(GIND) = 1
              LTDELDR(GIND) = DRAW
           ELSE
              WRITE(6,802) IAM(),(GLNAMES(K,GNUM),K=1,4),DRAW
              CALL PRMYESNO('at a later date [Y/N]',FLAG)
              IF(FLAG.EQ.1) THEN
                LTDELAY(GIND) = 2
                LTDELDR(GIND) = DRAW
              ELSE
                LTDELAY(GIND) = 0
                LTDELDR(GIND) = 0
              ENDIF
           ENDIF
           WRITE(6,930) IAM(),GTNAMES(TLTO),GIND,DRAW
	
           CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
	   CALL IOINIT(FDB,1,DLTSEC*256)
	   IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
	   CALL READW(FDB,DRAW,DLTREC,ST)
	   IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
  	   CALL CLOSEFIL(FDB)
	   CALL LOGGAM(TLTO,GIND,DLTREC,LLTSTS)
	   IF(LLTSTS(GIND).LT.GAMENV.AND.LTDELAY(GIND).NE.2) THEN
	      WRITE(6,920) IAM(),GTNAMES(TLTO),GIND,LLTSTS(GIND),GAMENV
	      CALL GPAUSE
	   ENDIF
	   CALL FASTSET(0,LLTSAL(1,GIND),LTGENT)
	   CALL FASTSET(0,LLTSHR(1,1,GIND),LTGDIV)
	   CALL FASTSET(0,LLTMSR(1,1,GIND),LTGDIV*2)
C
C SET WINNING BITMAPS (REGULAR AND BONUS)
C
	   DO 23 I=0,LLTBDR(GIND)
	      CALL MAP(LWINMAP(1,I+1,GIND),8,LLTWIN(1,I+1,GIND),LLTNUM(GIND))
	      IF(LLTBFL(GIND).NE.0)
     *	         CALL MAP(LBONMAP(1,I+1,GIND),8,LLTBNM(1,I+1,GIND),LLTBFL(GIND))
              IF (LLTLFL(GIND).GT.0) LLNMMAP(1,I+1,GIND) = LLTLNM(I+1,GIND)   !V02
23	   CONTINUE
C
	   IF(LVOL.EQ.0) THEN
	     CALL PRMTEXT('Enter Lotto draw pack volume name: ',CXLVOL,INPLEN)
	   ENDIF
C
	   CALL OPENW(1,SFNAMES(1,DAF),4,0,0,ST)
	   CALL IOINIT(FDB,1,DAFSEC*256)
	   IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),1,ST,0)

	   DO 25 CDC=LLTBSD(GIND),LLTESD(GIND)
	      CALL READW(FDB,CDC,DAFREC,ST)
	      IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),2,ST,CDC)
	      IF(DAFSTS.EQ.DNOSAL) GOTO 25
	      FILCNT=FILCNT+1
	      FTYPE(FILCNT)=0
	      WRITE (FILES(FILCNT),900) LVOL,GSNAMES(GNUM),CDC
25	   CONTINUE
	   CALL CLOSEFIL(FDB)
C
C SET WINREP_AUTO FOR WINRPT.
C
	   IF(LTDELAY(GIND).NE.2) THEN
	      WINREP_AUTO(GNUM) = DRAW
	   ENDIF
100	CONTINUE
C
C
800     FORMAT('Enter ',4A4,I1,' draw number [E-none]    ')
801     FORMAT(1X,A,'Postponed winner selection for ',4A4,' draw ',I4)
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
