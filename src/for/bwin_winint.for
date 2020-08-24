C SUBROUTINE BWIN_WININT.FOR
C
C
C V07 15-FEB-2000 UXN P(REG_DRWPCK) added.
C V06 13-DEC-1999 OXK MULTIWIN changes.
C V05 27-APR-1999 RXK Call of INPNUM replaced with call of PRMNUM
C                   Event # asked only if manual winner selection.
C V04 10-NOV-1997 UXN WINREP_AUTO ADDED FOR AUTOMATED WINRPT
C V03 06-JAN-1995 HXK Clear LBNSHR array
C V02 23-NOV-1994 HXK Initialise B%WINTABs
C V01 27-OCT-1994 HXK Initial revision.
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
	SUBROUTINE BWIN_WININT(FILES,FILCNT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:BNGCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:DBNREC.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'

	INTEGER*4 FDB(7)
	INTEGER*4 FILCNT, BVOL, GIND, DRAW, EXT, GNUM, ST, I, J, K
	INTEGER*4 CDC, INPLEN
	CHARACTER FILES(200)*20   
	CHARACTER STRING*50
	CHARACTER CXBVOL*4
	EQUIVALENCE (BVOL,CXBVOL)
C
	CALL FASTSET(-1000,LBNDRW,NUMBGO)
	FILCNT=0
	BVOL = P(REG_DRWPCK)
C
C
	DO 100 GIND=1,NUMBGO
	   GNUM=GTNTAB(TBNG,GIND)
	   IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) GOTO 100

           IF(STOPMOD.EQ.WINMANUAL) THEN
 	      WRITE (STRING,800) (GLNAMES(K,GNUM),K=1,4),GIND
	      CALL PRMNUM(STRING,DRAW,1,999999,EXT)
	      IF(EXT.LT.0) GOTO 100
           ELSE
              DRAW = DRWGAM(MLWININD,GNUM)
              IF(DRAW.EQ.0) GOTO 100 
           ENDIF

	   WRITE(6,930) IAM(),GTNAMES(TBNG),GIND,DRAW
	   CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
	   CALL IOINIT(FDB,1,DBNSEC*256)
	   IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
	   CALL READW(FDB,DRAW,DBNREC,ST)
	   IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
	   CALL LOGGAM(TBNG,GIND,DBNREC,LBNSTS)
	   IF(LBNSTS(GIND).LT.GAMENV) THEN
	       WRITE(6,920) IAM(),GTNAMES(TBNG),GIND,LBNSTS(GIND),GAMENV
	       CALL GPAUSE
	   ENDIF
	   CALL FASTSET(0,LBNSAL(1,GIND),BGOENT)
	   LBNTAX(GIND) = 0
	   IF(BVOL.EQ.0) THEN
	      WRITE(STRING,810) (GLNAMES(K,GNUM),K=1,4),GIND
	      CALL PRMTEXT(STRING,CXBVOL,INPLEN)
	   ENDIF
	   CALL CLOSEFIL(FDB)
C
C
	   CALL OPENW(1,SFNAMES(1,DAF),4,0,0,ST)
	   CALL IOINIT(FDB,1,DAFSEC*256)
	   IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),1,ST,0)
	   DO 90 CDC=LBNBSD(GIND),LBNESD(GIND)
	      CALL READW(FDB,CDC,DAFREC,ST)
	      IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),2,ST,CDC)
	      IF(DAFSTS.EQ.DNOSAL) GOTO 90
	      IF(DAFSTS.EQ.DSOPEN) GOTO 90
	      FILCNT=FILCNT+1
	      WRITE (FILES(FILCNT),900) BVOL,GSNAMES(GNUM),CDC
90	   CONTINUE

           DO I=1,BGOFHS
              DO J=1,BGODIV
                 LBNSHR(J,I,GIND)=0
              ENDDO
           ENDDO

	   CALL CLOSEFIL(FDB)
           CALL BINGBMAP(GIND)
           DO I =1,BGONBR
              BWWINTAB(I,GIND) = 75
              DO J=1,BGOPHS
                 BMWINTAB(J,I,GIND) = 0
              ENDDO
           ENDDO
           DO I=1,BGOPHS
              BNWINTAB(I,GIND) = 75
           ENDDO
	   WINREP_AUTO(GNUM) = DRAW
100	CONTINUE
	RETURN
C
C
800	FORMAT('Enter ',4A4,I1,' event code (E-none)    ')
810	FORMAT('Enter ',4A4,I1,' draw pack volume name: ')
900	FORMAT(A4,':',A4,I4.4,'.FIL')
910	FORMAT(1X,A,1X,A8,I1,' refund scan for row ',I2)
920	FORMAT(1X,A,1X,A8,I1,' invalid game status> ',I4,' should be> ',I4)
930	FORMAT(1X,A,1X,'Loading game date for ',A8,I1,' event> ',I4,
     *	       ' winner selection')
940	FORMAT(A4)
	END
