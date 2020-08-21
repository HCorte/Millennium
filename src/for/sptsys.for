C
C SUBROUTINE SPTSYS
C $Log:   GXAFXT:[GOLS]SPTSYS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:17:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:42:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - sptsys.for **
C
C SPTSYS.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SPTSYS.FIL    SUBROUTINE TO ENTER SPORTS SYSTEM BET TABLES
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
	SUBROUTINE SPTSYS(FILE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSSF.DEF'
	INTEGER*4 FLAG, GIDX, K, J, I, ROWS, SGL, DBL, TRI, BROWS
	INTEGER*4 STYP, SYS, EXT, FUN, ST
	INTEGER*4 SFDB(7),ROWTAB(SPGNBR,1296),FILE(5)
	COMMON /SCOMON/SSFREC
C
C OPEN SPORTS SYSTEM FILE
C
	CALL OPENQW(2,FILE,4,0,0,ST)
	CALL IOQINIT(SFDB,2,SSFSEC*256)
	IF(ST.NE.0) CALL FILERR(FILE,1,ST,0)
C
C
	CALL READQW(SFDB,1,SSFREC,ST)
	IF(ST.NE.0) CALL FILERR(FILE,2,ST,1)
10	CONTINUE
	CALL CLRSCR(5)
	WRITE(5,900)
	CALL INPNUM('Enter function ',FUN,1,4,EXT)
	IF(EXT.LT.0) THEN
	  CALL CLOSEQFIL(SFDB)
	  RETURN
	ENDIF
C
C
	IF(FUN.EQ.1) GOTO 1000
	IF(FUN.EQ.2) GOTO 2000
	IF(FUN.EQ.3) GOTO 3000
	IF(FUN.EQ.4) GOTO 4000
	GOTO 10
C
C
C ENTER NEW SYSTEM
C
1000	CONTINUE
	CALL CLRSCR(5)
	TYPE*,'New system definition '
	CALL INPNUM('Enter system number ',SYS,1,SPGSYS,EXT)
	IF(EXT.LT.0) GOTO 10
C
C
	IF(SSFATR(SYS).NE.NOSYS) THEN
	  TYPE*,'System ',SYS,' already defined'
	  CALL XWAIT(2,2,ST)
	  GOTO 1000
	ENDIF
C
C
	CALL INPNUM('Enter system type (1=full, 2=Reduced, 3=Usys) ',
     *	            STYP,1,3,EXT)
	IF(EXT.LT.0) GOTO 10
	CALL INPNUM('Enter number of rows/bet ',BROWS,1,SPGNBR,EXT)
	IF(EXT.LT.0) GOTO 10
	IF(STYP.EQ.1) GOTO 1070
C
C
	CALL INPNUM('Enter number of triples ',TRI,0,9999,EXT)
	IF(EXT.LT.0) GOTO 10
	CALL INPNUM('Enter number of doubles ',DBL,0,9999,EXT)
	IF(EXT.LT.0) GOTO 10
	CALL INPNUM('Enter number of singles ',SGL,0,9999,EXT)
	IF(EXT.LT.0) GOTO 10
C
C
	CALL INPNUM('Enter number of bets to enter ',ROWS,1,1296,EXT)
	IF(EXT.LT.0) GOTO 10
C
C
	DO 1020 I=1,SPGNBR
	DO 1020 J=1,1296
	ROWTAB(I,J)=1
1020	CONTINUE
C
C
	DO 1060 J=1,ROWS
1030	CONTINUE
	CALL CLRSCR(5)
	TYPE*,'Entry of bet ',J
	TYPE*,'Type R to restart '
	DO 1050 I=1,BROWS
	CALL GETENT(ROWTAB(I,J),I,ST)
	IF(ST.EQ.-2) THEN
	  DO 1040 K=1,SPGNBR
	  ROWTAB(K,J)=1
1040	  CONTINUE
	  GOTO 1030
	ENDIF
	IF(ST.EQ.-1) THEN
	  TYPE*,'Bet ',J,' Completed'
	  GOTO 1060
	ENDIF
1050	CONTINUE
1060	CONTINUE
C
C
1070	CONTINUE
	CALL SAVSYS(SYS,STYP,TRI,DBL,SGL,ROWS,BROWS,ROWTAB,ST)
	IF(ST.EQ.0) THEN
	  CALL WRITEQW(SFDB,1,SSFREC,ST)
	  IF(ST.NE.0) THEN
	    TYPE*,'SYSBET.FIL write error - ',ST,' record - 1'
	    CALL GPAUSE
	  ENDIF
	ENDIF
	GOTO 10
C
C DELETE SYSTEM DEFINITION
C
2000	CONTINUE
	CALL CLRSCR(5)
	TYPE*,'System deletion '
	CALL INPNUM('Enter system number ',SYS,1,SPGSYS,EXT)
	IF(SSFATR(SYS).EQ.NOSYS) THEN
	  TYPE*,'System not defined  '
	  CALL XWAIT(2,2,ST)
	  GOTO 10
	ENDIF
C
C
	CALL SYSDEL(SYS,ST)
	IF(ST.EQ.0) THEN
	  CALL WRITEQW(SFDB,1,SSFREC,ST)
	  IF(ST.NE.0) CALL FILERR(FILE,3,ST,1)
	ENDIF
	GOTO 10
C
C EDIT SYSTEM DEFINITION
C
3000	CONTINUE
	CALL CLRSCR(5)
	TYPE*,'System edit '
	CALL INPNUM('Enter system number ',SYS,1,SPGSYS,EXT)
	IF(SSFATR(SYS).EQ.NOSYS) THEN
	  TYPE*,'System ',SYS,' not defined  '
	  CALL XWAIT(2,2,ST)
	  GOTO 10
	ENDIF
C
C
	CALL EDIT(SYS)
	CALL WRITEQW(SFDB,1,SSFREC,ST)
	IF(ST.NE.0) CALL FILERR(FILE,3,ST,1)
	GOTO 10
C
C ENTER VALID FULL SYSTEM
C
4000	CONTINUE
	CALL CLRSCR(5)
4001	CONTINUE
	TYPE*,'Full system entry '
	CALL INPNUM('Enter game index ',GIDX,1,NUMSPT,EXT)
	IF(EXT.LT.0) GOTO 10
	CALL INPNUM('Enter number of triples ',TRI,0,15,EXT)
	IF(EXT.LT.0) GOTO 10
	CALL INPNUM('Enter number of doubles ',DBL,0,15,EXT)
	IF(EXT.LT.0) GOTO 10
C
C
	TYPE*,'Full system bet has ',(3**TRI)*(2**DBL),' Combinations.'
	CALL WIMG(5,'Is this correct? (Y/N) ')
	CALL YESNO(FLAG)
	IF(FLAG.NE.1) THEN
	   TYPE*,'Reenter system attributes '
	   CALL XWAIT(2,2,ST)
	   GOTO 4000
	ENDIF
C
C
	IF(SSFFSF(TRI,DBL,GIDX).EQ.1) THEN     !FLAG IS ALREADY SET
	   TYPE*,'This is already set up as a full system'
	   CALL WIMG(5,'Do you want to delete it? (Y/N)')
	   CALL YESNO(FLAG)
	   IF(FLAG.EQ.1) THEN
	      SSFFSF(TRI,DBL,GIDX)=0
	   ENDIF
	ELSE
	   SSFFSF(TRI,DBL,GIDX)=1
	ENDIF
C
	IF(SSFFSF(TRI,DBL,GIDX).NE.1) THEN
	   TYPE*,'Full system has been deleted '
	ELSE
	   TYPE*,'Full system has been added '
	ENDIF
	TYPE*,' '
C
C
	CALL WRITEQW(SFDB,1,SSFREC,ST)
	IF(ST.NE.0) THEN
	   CALL FILERR(FILE,3,ST,1)
	   CALL XWAIT(2,2,ST)
	ENDIF
	GOTO 4001
C
C
900	FORMAT(//,1X,' Sysbet program functions: ',/,
     *	          1X,' 1 - Enter new system definition',/,
     *	          1X,' 2 - Delete existing system definition',/,
     *	          1X,' 3 - Edit exisiting system definition',/,
     *	          1X,' 4 - Enter valid FULL system definition',//)
	END
