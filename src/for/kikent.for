C
C SUBROUTINE KIKENT
C $Log:   GXAFXT:[GOLS]KIKENT.FOV  $
C
C  V06 17-DEC-1999 PXO Added a call to report subroutine  
C  V05 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting,
C                      Instant Pass Thru Phase 1
C  V04 23-JUL-1993 SXH Released for Finland
C  V03 21-JAN-1993 DAB Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - kikent.for **
C
C KIKENT.FOR
C
C V02 12-NOV-91 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO PROCESS OPERATOR ENTRY OF KICKER RESULTS.
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
	SUBROUTINE KIKENT(GNUM,GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'


        ! Arguments
        INTEGER*4  GNUM             !
        INTEGER*4  GIND             !
        INTEGER*4  DRAW             !

        ! variables
	INTEGER*4  FDB(7)           !
	INTEGER*4  CBUF(CDLEN)      !
	INTEGER*4  FLAG             !
	INTEGER*4  K                !
	INTEGER*4  EXT              !
	INTEGER*4  NUM              !
	INTEGER*4  I                !
	INTEGER*4  ST               !
	INTEGER*4  MAX              !
C
C
	IF(ONLINE) THEN
	    CALL GAMLOG(TKIK,GIND,DKKREC,KIKBLK)
	ELSE
            CALL OPENW(2,SCFGFN(1,GNUM),4,0,0,ST)
            CALL IOINIT(FDB,2,DKKSEC*256)
            IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)

            CALL READW(FDB,DRAW,DKKREC,ST)
            IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
        ENDIF
C
C
	IF(DKKSTS.NE.GAMBFD) THEN
	    WRITE(5,900) IAM(),GTNAMES(TKIK),GIND,DRAW,DKKSTS
	    CALL GPAUSE
	ENDIF
	WRITE(5,901) IAM(),GTNAMES(TKIK),GIND,DRAW
C
C
100	CONTINUE
	CALL INPNUM('Enter winning number: ',NUM,0,DKKMAX,EXT)
	IF(EXT.LT.0) GOTO 100
	DKKWIN=NUM
C
C
	WRITE(5,903) IAM(),DKKWIN
	CALL WIMG(5,'Is the number entered correct [Y/N] ')
	CALL YESNO(FLAG)
	IF(FLAG.NE.1) GOTO 100
	DKKSTS=GAMEN1
	OPDONE=1
C
C WAIT FOR VERIFICATION FROM REMOTE TERMINAL.
C
130	CONTINUE
	TYPE*,IAM(),' Waiting for verification from remote terminal'
	IF(DKKSTS.GE.GAMENV) THEN
	    IF(ONLINE) THEN
	        CALL FASTSET(0,CBUF,CDLEN)
	        CBUF(1)=2
	        CBUF(2)=DKKWIN
	        CBUF(3)=TCKIK
	        CBUF(6)='SYS '
	        CBUF(8)=GIND
	        CALL RESCMD(CBUF)

	        CALL FASTSET(0,CBUF,CDLEN)
	        CBUF(1)=1
	        CBUF(2)=DKKSTS
	        CBUF(3)=TCKIK
	        CBUF(6)='SYS '
	        CBUF(8)=GIND
	        CALL RESCMD(CBUF)
	    ELSE
                CALL WRITEW(FDB,DRAW,DKKREC,ST)
                IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
	        CALL CLOSEFIL(FDB)
		CALL JORESULT(GIND,DRAW)
            ENDIF

	    RETURN
	ENDIF

	CALL XWAIT(5,2,ST)
	IF(DKKSTS.EQ.GAMBFD) THEN
	    TYPE*,IAM(),' Verification error, please re-enter '
	    GOTO 100
	ENDIF
	GOTO 130
C
C
C
900	FORMAT(1X,A,1X,A8,I1,' draw ',I4,' invalid game status> ',I4)
901	FORMAT(1X,A,1X,A8,I1,' draw ',I4)
903	FORMAT(1X,A,' Number entered:  ',I7.7)
	END
