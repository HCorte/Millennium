C
C PROGRAM SHAREUPD
C
C V08 01-DEC-2000 UXN TOTOGOLO ADDED.
C V07 13-OCT-1999 RXK World Tour added.
C V06 28-SEP-1999 UXN Game number checking added.
C V05 27-APR-1999 RXK Call of WIMG/YESNO replaced with call of PRMYESNO.
C V04 06-JAN-1995 HXK Allow V65, Bingo games to be updated
C V03 19-DEC-1994 HXK Added Bingo
C V02 06-FEB-1994 HXK added Ravi game.
C V01 21-NOV-1991 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C
C
C SHAREUPD.FOR
C
C
C MANUAL PROGRAM TO SET SHARE VALUES
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
	PROGRAM SHAREUPD
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'
C
	INTEGER*4   CONLU
	PARAMETER   (CONLU=5)			 !Default Lu to the Console
C
	BYTE	    BELL			 !Bell character (BEEP)
	INTEGER*4   GTYP			 !Game Type Selected
	INTEGER*4   GIND			 !Game Index Selected
	INTEGER*4   GNUM			 !Game Number Selected
	INTEGER*4   DRAW			 !Draw number to update
	INTEGER*4   ST				 !Subroutine Return Status
	INTEGER*4   EXT				 !Exit Code (INPNUM)
	INTEGER*4   FLAG			 !YESNO Answer Flag
	INTEGER*4   K				 !Loop Variable
	CHARACTER   STRING*41			 !String to hold prompt message
C
	COMMON	    SCFREC
C
	DATA	    DRAW/0/
	DATA	    BELL/07/
C
C
	CALL COPYRITE
C
C GET SYSTEM CONTROL CONFIGURATION INFO.
C
	CALL GETSCONF(SCFREC,ST)
	IF(ST.NE.0) THEN
	  WRITE(CONLU,810) IAM(),ST,BELL
	  CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
100	CONTINUE
C
C DISPLAY ALL GAMES 
C
        WRITE(CONLU,900) IAM()
C
	WRITE(CONLU,960) (IAM(),K,GTNAMES(K),K=1,MAXTYP)
	CALL PRMNUM('Enter game type  ',GTYP,1,MAXTYP,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
	CALL PRMNUM('Enter game index ',GIND,1,MAXIND,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)	
C
	GNUM = GTNTAB(GTYP,GIND)
	IF(GNUM.LE.0.OR.GNUM.GT.MAXGAM) THEN
	   WRITE(CONLU,941) IAM(),GTNAMES(GTYP),GIND
	   GOTO 100
	ENDIF
C
	IF(GTYP.NE.TLTO.AND.GTYP.NE.TSPT.AND.GTYP.NE.TKIK.AND.
     *     GTYP.NE.TBNG.AND.GTYP.NE.TTGL) THEN
	  WRITE(CONLU,930) IAM(),BELL
	  GOTO 100
	ENDIF
C
	WRITE (STRING,800) (SCFLGN(K,GNUM),K=1,4)
	CALL PRMNUM(STRING,DRAW,1,999,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
       	WRITE(CONLU,910) IAM(),(SCFLGN(K,GNUM),K=1,4),DRAW
	CALL PRMYESNO('Is this correct (Y/N) ',FLAG)
	IF(FLAG.NE.1) GOTO 100
C
	WINREP_AUTO(GNUM) = DRAW
	IF(GTYP.EQ.TDBL.OR.GTYP.EQ.TSCR.OR.GTYP.EQ.TWIT.OR.
     *     GTYP.EQ.TTSL.OR.GTYP.EQ.TCPL)
     *     BKKREP_AUTO(GNUM) = DRAW
C
	IF(GTYP.EQ.TLTO) CALL LSHARE(GNUM,GIND,DRAW)
	IF(GTYP.EQ.TSPT) CALL SSHARE(GNUM,GIND,DRAW)
	IF(GTYP.EQ.TTGL) CALL TGSHARE(GNUM,GIND,DRAW)
	IF(GTYP.EQ.TKIK) CALL KSHARE(GNUM,GIND,DRAW)
        IF(GTYP.EQ.TBNG) CALL BSHARE(GNUM,DRAW)
	WRITE(CONLU,920) IAM(),(SCFLGN(K,GNUM),K=1,4)
C
	CALL PRMYESNO('Do you want to enter Shares for any other games? ',
     *       FLAG)
	IF(FLAG.NE.2) GOTO 100
	CALL GSTOP(GEXIT_SUCCESS)
C
C
800	FORMAT('Enter ',4A4,' event/draw number ')
810	FORMAT(1X,A,'Unable to get System Control Information,',
     *	       '  Status: ',I4,A1)
820	FORMAT(1X,A)
900	FORMAT(//,1X,A,'*** Game share value entry ***',/)
910	FORMAT(/,1X,A,'Selected Game: ',4A4,'Draw: ',I5)
920	FORMAT(/,1X,A,4A4,' share entry complete')
930	FORMAT(1X,A,'Sorry, Share Update not available for ',
     *	       'this game ',A1)
940	FORMAT(1X,A,'Sorry, game selected is not active ',A1)
941     FORMAT(1X,A,'Invalid game - ', A8, ' index ',I2)
950	FORMAT(1X,A,I2,' - ',4A4)
960	FORMAT(<MAXTYP>(1X,A,I2,' - ',A8,/))
	END
