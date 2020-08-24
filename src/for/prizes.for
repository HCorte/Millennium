C
C PROGRAM PRIZES
C $Log:   GXAFXT:[GOLS]PRIZES.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:29:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:21:30   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - prizes.for **
C
C PRIZES.FOR
C
C V01 13-MAR-91 JPJ INITIAL RELEASE FOR MARYLAND
C
C PRIZES ENTRY CONTROL PROGRAM
C
C
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM PRIZES
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 FLAG, DRAW, GNUM, GIND, EXT, GTYP, K, ST
C
	CHARACTER*34 STRING
C
C
C
	CALL COPYRITE
C
C
	PRZREADY=0
	OPDONE=0
	ONLINE=.FALSE.
	IF(DAYSTS.EQ.DSOPEN) ONLINE=.TRUE.

	CALL GETSCONF(SCFREC,ST)
	IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
100	CONTINUE
	WRITE(5,900) IAM(),(K,GTNAMES(K),K=1,MAXTYP)
	CALL INPNUM('Enter game type ',GTYP,1,MAXTYP,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
	IF(GTYP.NE.TLTO) THEN
	  TYPE*,IAM(),' Sorry, game selected is not lotto'
          GOTO 100
	ENDIF
	CALL INPNUM('Enter game index ',GIND,1,MAXIND,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
C
	GNUM=SCFGTN(GTYP,GIND)
	IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
	  TYPE*,IAM(),' Sorry, game selected is not active'
	  GOTO 100
	ENDIF
C
C
	WRITE (STRING,800) GTNAMES(GTYP),GIND
	CALL INPNUM(STRING,DRAW,1,999,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
C
	WRITE(5,910) IAM(),GTNAMES(GTYP),GIND,(SCFLGN(K,GNUM),K=1,4),DRAW
	CALL WIMG(5,'Is this correct (Y/N) ')
	CALL YESNO(FLAG)
	IF(FLAG.NE.1) GOTO 100
C
C
	CUR_GTYP=GTYP
	CUR_GIND=GIND
	CUR_GNUM=GNUM
	CUR_DRAW=DRAW
	PRZREADY=1
C
C
	IF(GTYP.EQ.TLTO) CALL PRZENT(GNUM,GIND,DRAW)
	WRITE(5,920) IAM(),GTNAMES(GTYP),GIND
	PRZREADY=0
	CALL GSTOP(GEXIT_SUCCESS)
C
C
800	FORMAT('Enter ',A8,I1,' draw number ')
810	FORMAT(1X,A18,1X,A8)
820	FORMAT(1X,A18,1X,A8,I1,I1,I2.2,I5.5)
900	FORMAT(//,1X,A18,1X,' Game prize entry ',//,
     *	        6(1X,I1,' - ',A8,/))
910	FORMAT(1X,A18,1X,A8,I1,2X,4A4,'Draw ',I5,/)
920	FORMAT(1X,A18,1X,A8,I1,' prize entry complete')
	END
