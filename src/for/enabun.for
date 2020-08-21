C
C PROGRAM ENABUN
C $Log:   GXAFXT:[GOLS]ENABUN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:03:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:12:14   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - enabun.for **
C
C ENABUN.FOR
C
C V01 20-MAR-91 JPJ INITIAL RELEASE FOR MARYLAND
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
	PROGRAM ENABUN
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DLTREC.DEF'
	INCLUDE 'INCLIB:DNBREC.DEF'
C
	INTEGER*4 SCFNAM(5), PRZTAB(NBGPOL)
	INTEGER*4 SCFFDB(7), BLANK, ST, I, OPT, EXT, GIND, DRAW
	INTEGER*4 GNUM, K, FSTDRW, LSTDRW, FLAG, BUNCNT, FDB(7)
	INTEGER*4 NUMPOL
	DATA         SCFNAM/'SCF.','FIL ',3*'    '/
C
	CALL COPYRITE
C
C READ SCF RECORD
C
	CALL OPENW(1,SCFNAM,4,0,0,ST)
	CALL IOINIT(SCFFDB,1,SCFSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFNAM,1,ST,0)
	CALL READW(SCFFDB,1,SCFREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFNAM,2,ST,1)
	CALL CLOSEFIL(SCFFDB)
	DO 10 I=1,MAXFIL
	IF(SCFSFN(1,I).EQ.'    ') CALL SYSVOL(SCFSFN(1,I))
10	CONTINUE
C
C GET OPTION
C
100	CONTINUE
	WRITE(5,900)
	CALL INPNUM('Enter option [E-Exit] ',OPT,1,2,EXT)
	IF(EXT.NE.0) GOTO 8000
	GOTO (1000,2000) OPT
C
C ENABLE/DISABLE BONUS DRAWS LOTTO
C
1000	CONTINUE
C
	DO 1100 GIND=1,MAXIND
	   GNUM=SCFGTN(TLTO,GIND)
	   IF(GNUM.LT.1) GOTO 1100
	   WRITE(5,901) GIND,(SCFLGN(K,GNUM),K=1,4)
1100	CONTINUE
C
	CALL INPNUM('Enter game index [E-Exit] ',
     *               GIND,1,MAXIND,EXT)
	IF(EXT.NE.0) GOTO 100
	GNUM=SCFGTN(TLTO,GIND)
	IF(GNUM.LT.1) GOTO 1000
C
C GET START AND END DRAWS
C
	CALL INPNUM('Enter start bonus draw ',FSTDRW,1,9999,EXT)
	CALL INPNUM('Enter last  bonus draw ',LSTDRW,FSTDRW,9999,EXT)
C
C GET NUMBER OF BONUS DRAWINGS
C
	CALL INPNUM('Enter number of bonus drawings ',
     *               BUNCNT,0,MAXBDR,EXT)
C
C ASK IF INFO ENTERED IS CORRECT
C
	CALL WIMG(5,'Do you want to update file with info entered Y/N ')
	CALL YESNO(FLAG)
	IF(FLAG.NE.1) GOTO 100
C
C OPEN LOTTO FILE
C
        CALL OPENW(1,SCFGFN(1,GNUM),4,0,0,ST)
        CALL IOINIT(FDB,1,DLTSEC*256)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
C
C UPDATE BONUS DRAW FIELD
C
	TYPE *,'Updating lotto file with bonus draw info '
	DO 1200 DRAW = FSTDRW,LSTDRW
C
           CALL READW(FDB,DRAW,DLTREC,ST)
	   IF(ST.EQ.144) THEN
	     TYPE*,'Last draw initialized - ',DRAW-1
	     CALL CLOSEFIL(FDB)
	     CALL XWAIT(2,2,ST)
	     GOTO 100
	   ENDIF
           IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
	   IF(DLTSTS.GT.GAMOPN) THEN
	     TYPE*,'Game already closed for draw ',DRAW
	     CALL GPAUSE
	     CALL CLOSEFIL(FDB)
	     GOTO 100
	   ENDIF
C
	   DLTBDR = BUNCNT
C
           CALL WRITEW(FDB,DRAW,DLTREC,ST)
           IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
C
1200	CONTINUE
        CALL CLOSEFIL(FDB)
	GOTO 100
C
C
C
2000	CONTINUE
C
	DO 2100 GIND=1,MAXIND
	   GNUM=SCFGTN(TNBR,GIND)
	   IF(GNUM.LT.1) GOTO 2100
	   WRITE(5,901) GIND,(SCFLGN(K,GNUM),K=1,4)
2100	CONTINUE
C
	CALL INPNUM('Enter game index [E-Exit] ',
     *               GIND,1,MAXIND,EXT)
	IF(EXT.NE.0) GOTO 100
	GNUM=SCFGTN(TNBR,GIND)
	IF(GNUM.LT.1) GOTO 2000
	NUMPOL = 10
	IF(GIND.EQ.NB4TYP) NUMPOL = 16
C
C GET START AND END DRAWS
C
	CALL INPNUM('Enter start bonus draw ',FSTDRW,1,9999,EXT)
	CALL INPNUM('Enter last  bonus draw ',LSTDRW,FSTDRW,9999,EXT)
C
C GET NUMBER OF BONUS DRAWINGS
C
	CALL INPNUM('Enter number of bonus drawings ',
     *               BUNCNT,0,MAXBDR,EXT)
C
C OPEN NUMBERS FILE
C
        CALL OPENW(1,SCFGFN(1,GNUM),4,0,0,ST)
        CALL IOINIT(FDB,1,DNBSEC*256)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
        CALL READW(FDB,FSTDRW,DNBREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
C GET BONUS PRIZE VALUES
C
        CALL GETPRZ(GIND,DNBOPT,PRZTAB)
C
C ASK IF INFO ENTERED IS CORRECT
C
	CALL WIMG(5,'Do you want to update file with info entered Y/N ')
	CALL YESNO(FLAG)
	IF(FLAG.NE.1) GOTO 100
C
C UPDATE BONUS DRAW FIELD
C
	TYPE *,'Updating numbers file with bonus draw info '
	DO 2200 DRAW = FSTDRW,LSTDRW
C
           CALL READW(FDB,DRAW,DNBREC,ST)
	   IF(ST.EQ.144) THEN
	     TYPE*,'Last draw initialized - ',DRAW-1
	     CALL CLOSEFIL(FDB)
	     CALL XWAIT(2,2,ST)
	     GOTO 100
	   ENDIF
           IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
	   IF(DNBSTS.GT.GAMOPN) THEN
	     TYPE*,'Game already closed for draw ',DRAW
	     CALL GPAUSE
	     CALL CLOSEFIL(FDB)
	     GOTO 100
	   ENDIF
C
	   DNBBDR = BUNCNT
           CALL FASTMOV(PRZTAB,DNBPRZ(1,2),NUMPOL)
C
           CALL WRITEW(FDB,DRAW,DNBREC,ST)
           IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
C
2200	CONTINUE
        CALL CLOSEFIL(FDB)
	GOTO 100
C
C
C
8000	CONTINUE
	CALL GSTOP(GEXIT_SUCCESS)
C
C FORMAT SECTION
C
900	FORMAT(1X,'1. ENABLE/DISABLE BONUS DRAWS LOTTO    ',/,
     *         1X,'2. ENABLE/DISABLE BONUS DRAWS NUMBERS  ',/,
     *         1X,'   AND SET NUMBERS BONUS DRAW PRIZE VALUES ',/)
901	FORMAT(1X,I2,3X,4A4)
	END
