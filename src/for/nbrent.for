C
C SUBROUTINE NBRENT
C $Log:   GXAFXT:[GOLS]NBRENT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:09:08   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:05:02   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nbrent.for **
C
C NBRENT.FOR
C
C V03 22-JAN-92 GCAN ALSO SET BOX AND STRAIGHT BET WINNING NUMBERS.
C V02 12-NOV-91 MTK  INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C SUBROUTINE TO PROCESS OPERATOR ENTRY OF NUMBERS RESULTS.
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
	SUBROUTINE NBRENT(GNUM,GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:NBRCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INTEGER*4 FDB(7), FLAG, EXT, NUM, ST, DRAW, GIND, GNUM, MAX
	INTEGER*4 CBUF(CDLEN), BDROFF
	CHARACTER*20 WINBUF(MAXBDR)
	DATA WINBUF/'Enter winning number',
     *	            'Enter bonus 1 number',
     *	            'Enter bonus 2 number',
     *	            'Enter bonus 3 number',
     *              'Enter bonus 4 number'/
C
C
	IF(ONLINE) THEN
	  CALL GAMLOG(TNBR,GIND,DNBREC,NBRBLK)
	ELSE
          CALL OPENW(2,SCFGFN(1,GNUM),4,0,0,ST)
          CALL IOINIT(FDB,2,DNBSEC*256)
          IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
          CALL READW(FDB,DRAW,DNBREC,ST)
          IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
	ENDIF
C
C
	IF(DNBSTS.NE.GAMBFD) THEN
	  WRITE(5,900) IAM(),GTNAMES(TNBR),GIND,DRAW,DNBSTS
	  CALL GPAUSE
	ENDIF
	WRITE(5,901) IAM(),GTNAMES(TNBR),GIND,DRAW
C
	MAX=999
	IF(DNBTYP.EQ.NB4TYP) MAX=9999
C
90	CONTINUE
	DO 125 BDROFF=1,DNBBDR+1
100	   CONTINUE
	   CALL INPNUM(WINBUF(BDROFF),NUM,0,MAX,EXT)
	   IF(EXT.LT.0) GOTO 100
	   DNBWIN(TNB3ST,BDROFF)=NUM
	   DNBWIN(TNB3B6,BDROFF)=NUM
C
C
	   IF (DNBTYP.EQ.NB4TYP) THEN
	       WRITE(5,904) IAM(),DNBWIN(TNB3ST,BDROFF)
	   ELSE
	       WRITE (5,903) IAM(),DNBWIN(TNB3ST,BDROFF)
	   ENDIF
	   CALL WIMG(5,'Is the number entered correct [Y/N] ')
	   CALL YESNO(FLAG)
	   IF(FLAG.NE.1) GOTO 100
125	CONTINUE
	DNBSTS=GAMEN1
	OPDONE=1
C
C WAIT FOR VERIFICATION FROM REMOTE TERMINAL.
C
130	CONTINUE
	TYPE*,IAM(),' Waiting for verification from remote terminal'
	IF(DNBSTS.EQ.GAMENV) THEN
	  IF(ONLINE) THEN
	    DO 155 BDROFF=1,DNBBDR+1
	    CALL FASTSET(0,CBUF,CDLEN)
	    CBUF(1)=2
	    CBUF(2)=DNBWIN(1,BDROFF)
	    CBUF(3)=TCNBR
	    CBUF(6)='SYS '
	    CBUF(8)=GIND
	    CBUF(9)=BDROFF
	    CALL RESCMD(CBUF)
155	    CONTINUE
	    CALL FASTSET(0,CBUF,CDLEN)
	    CBUF(1)=1
	    CBUF(2)=GAMDON
	    CBUF(3)=TCNBR
	    CBUF(6)='SYS '
	    CBUF(8)=GIND
	    CALL RESCMD(CBUF)
	  ELSE
            CALL WRITEW(FDB,DRAW,DNBREC,ST)
            IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
	  ENDIF
	  RETURN
	ENDIF
	CALL XWAIT(5,2,ST)
	IF(DNBSTS.EQ.GAMBFD) THEN
	  TYPE*,IAM(),' Verification error, please re-enter '
	  GOTO 90
	ENDIF
	GOTO 130
C
C
C
900	FORMAT(1X,A,1X,A8,I1,' draw ',I4,' invalid game status> ',I4)
901	FORMAT(1X,A,1X,A8,I1,' draw ',I4)
903	FORMAT(1X,A,' Number entered:  ',I3.3)
904	FORMAT(1X,A,' Number entered: ',I4.4)
	END
