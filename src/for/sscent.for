C
C SUBROUTINE SSCENT
C
C V03 03-JUL-2000 UXN Refund too late played tickets.
C V02 17-DEC-1999 PXO Added a call to report subroutine  
C V01 ??-???-???? RXK Initial release.
C
C SUBROUTINE TO PROCESS OPERATOR ENTRY OF SUPERSCORE RESULTS.
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SSCENT(GNUM,GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:RECSPF.DEF'
	INCLUDE 'INCLIB:SSPCOM.DEF'
	INCLUDE 'INCLIB:SSFREC.DEF'
	INCLUDE 'INCLIB:SSOREC.DEF'

	CHARACTER*70 BUF
	REAL*8 TOTAL,RODDS
	INTEGER*4 FDB(7),PFDB(7),OFDB(7)
	INTEGER*4 GNUM, ST, DRAW, GIND, K, I, NUM, EXT, FLAG
	INTEGER*4 WINAMT
	INTEGER*4 UCID, BUCKET, ENTR
        INTEGER*4 SSODMAIN(SSGISZ,0:SSGNOB)
        INTEGER*4 SSODAMT(SSGISZ,0:SSGNOB)
        INTEGER*4 GAMST

	LOGICAL*4 FOUND
	LOGICAL*4 LATEFLG
	INTEGER*4 CDC,TIME 

        INTEGER*4 I4TEMP
        BYTE      I1TEMP(4)
        EQUIVALENCE(I4TEMP,I1TEMP)

C
C
	CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,3,DSSSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
	CALL READW(FDB,DRAW,DSSREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
C
	IF(DSSSTS.NE.GAMBFD) THEN
	  WRITE(6,900) IAM(),GTNAMES(TSSC),GIND,DRAW,DSSSTS
	  CALL GPAUSE
	ENDIF
C
	DSSLAT(LATCDC) = 0
	DSSLAT(LATTIM) = 0
C
        DO I=1,3
           DSSWIN(1,I)=0
           DSSWIN(2,I)=0
        ENDDO
	GAMST=0

100	CONTINUE
        DO 112 I=1,3
	   IF(DSSEST(I).NE.GAMOPN) GOTO 112
	   WRITE (6,903) IAM(),I,(DSSSNM(K,I),K=1,SSNMS_LEN/4)
	   WRITE (BUF,902)
	   CALL INPNUM(BUF,NUM,0,15,EXT)
	   IF(EXT.NE.0.AND.EXT.NE.-5) GOTO 100
	   IF(EXT.EQ.-5) THEN
	      GAMST=GAMCAN
	      GOTO 115	
	   ENDIF
	   DSSWIN(1,I)=NUM
C
110	   CONTINUE
	   WRITE (BUF,9021)
	   CALL INPNUM(BUF,NUM,0,15,EXT)
	   IF(EXT.NE.0) GOTO 110
	   DSSWIN(2,I)=NUM
C
112 	CONTINUE

        DO I=1,3
	   IF(DSSEST(I).NE.GAMOPN) GOTO 114
	   WRITE(6,903) IAM(),I,(DSSSNM(K,I),K=1,SSNMS_LEN/4),' entered'
	   WRITE(6,9031) IAM(),DSSWIN(1,I),DSSWIN(2,I)
        ENDDO
114     CONTINUE
	CALL INPYESNO('Are the scores entered correct [Y/N] ',FLAG)
	IF(FLAG.NE.1) GOTO 100

115     CONTINUE
	DSSSTS=GAMEN1
	OPDONE=1
C
C WAIT FOR VERIFICATION FROM REMOTE TERMINAL.
C
120	CONTINUE
	TYPE*,IAM(),' Waiting for verification from remote terminal'
	IF(DSSSTS.EQ.GAMENV) GOTO 130
	CALL XWAIT(5,2,ST)
	IF(DSSSTS.EQ.GAMBFD) THEN
	  TYPE*,IAM(),' Verification error, please re-enter '
	  GOTO 100
	ENDIF
	GOTO 120
C
130	CONTINUE
	
	IF(GAMST.EQ.GAMCAN) THEN 
	   WRITE(6,904) IAM(),(DSSMNM(K),K=1,SSNMS_LEN/4)    
    	   DSSODS=100
	   DSSSTS=GAMCAN
	   GOTO 1000
	ENDIF

C
C READ POOL FILE
C
	TYPE*,IAM(),' Results verified...calculating odds'
C
        LATEFLG = .FALSE.
        CALL RC_PROMPT(CDC,TIME,ST)
        IF(ST.EQ.1) THEN
           LATEFLG = .TRUE.
	   DSSLAT(LATCDC) = CDC
	   DSSLAT(LATTIM) = TIME
	   CALL SSODSCAN(DRAW,GIND,UCID,DSSODS,DSSABW,LATEFLG,CDC,TIME) 
	   WRITE(6,906) IAM(),DSSODS/100,MOD(DSSODS,100),
     *                  CMONY(DSSABW,10,BETUNIT)
           GOTO 450
        ENDIF
C
	WRITE(6,905) IAM(),GTNAMES(TSSC),GIND,DRAW
	IF(DSSPFN(1).EQ.'    ') CALL SYSVOL(DSSPFN(1))
10      CONTINUE
        CALL OPENQW(4,DSSPFN,4,0,0,ST)
        IF(ST.NE.0) THEN
           WRITE(6,900) DSSPFN,ST
           CALL GPAUSE
           GOTO 10
        ENDIF
        CALL IOQINIT(PFDB,4,SSFSEC*256)
C
        CALL READQW(PFDB,1,SSFREC,ST)
        IF(ST.NE.0) THEN
           WRITE(6,902) DSSPFN,ST
           CALL GPAUSE
           GOTO 10
        ENDIF
	CALL CLOSEFIL(PFDB)
20	CONTINUE
        CALL OPENW(2,DSSPOF,4,0,0,ST)
        IF(ST.NE.0) THEN
              WRITE(6,900) DSSPOF,ST
              CALL GPAUSE
              GOTO 20
        ENDIF
        CALL IOINIT(OFDB,2,SSOSEC*256)
        CALL READW(OFDB,1,SSOREC,ST)
	CALL CLOSEFIL(OFDB)
C
	CALL FASTMOV(SSFMAIN,SSODMAIN(1,0),SSGPOL)
        CALL FASTMOV(SSFCAMT,SSODAMT(1,0),SSGPOL)
C
C SEARCH FOR WINNING COMBINATION IN POOLS FILE
C
        I4TEMP=0
        I1TEMP(3) = ISHFT(DSSWIN(1,1),4) + IAND(DSSWIN(2,1),'0F'X)
        I1TEMP(2) = ISHFT(DSSWIN(1,2),4) + IAND(DSSWIN(2,2),'0F'X)
        I1TEMP(1) = ISHFT(DSSWIN(1,3),4) + IAND(DSSWIN(2,3),'0F'X)
	UCID=I4TEMP
        CALL GETCCITT(UCID,1,3,BUCKET)
        CALL RND64(BUCKET,GIND,1,65535,6)

	FOUND=.FALSE.
        DO I=1,SSFNBA
           DO ENTR=1,SSGISZ
              IF(IAND(SSODMAIN(ENTR,BUCKET),'00FFFFFF'X).EQ.
     *           UCID) THEN
                 FOUND=.TRUE.
                 WINAMT=SSODAMT(ENTR,BUCKET)
                 GOTO 300
              ENDIF
           ENDDO
           BUCKET=MOD(BUCKET+1,65536)
        ENDDO
C
C READ POOL OVERFLOW FILE
C
        DO I=1,SSFONUM
           IF(SSOCMB(I).EQ.UCID) THEN
              WINAMT=SSOAMT(I)
  	      FOUND=.TRUE.  
              GOTO 300
           ENDIF
        ENDDO
C
C IF WINNING COMBINATION FOUND THEN CALCULATE ODDS
 
300     CONTINUE

	TOTAL = 0.0D0
	RODDS = 0.0D0
        DSSODS = 0
	TOTAL = DFLOAT(DSSSAL) * CALPER(DSSSPR)
	TOTAL = TOTAL + DFLOAT(DSSPOL(1))
	DSSTPL= IDINT(TOTAL)

	IF(FOUND) THEN
	   IF(WINAMT.NE.0) THEN
              RODDS = TOTAL/WINAMT
	      DSSODS = IDNINT(RODDS*100.0D0)
	      IF(DSSODS.LT.100) DSSODS = 100
           ENDIF
	   DSSABW = WINAMT

  	   IF(SSFDCMB.EQ.0) THEN
	      WRITE(6,906) IAM(),DSSODS/100,MOD(DSSODS,100),
     *                     CMONY(DSSABW,10,BETUNIT)
           ELSE
	      WRITE(6,909) IAM(),DSSODS/100,MOD(DSSODS,100),
     *                     CMONY(DSSABW,10,BETUNIT)
 	      WRITE(6,908) IAM()
	      CALL SSODSCAN(DRAW,GIND,UCID,DSSODS,DSSABW,LATEFLG,CDC,TIME) 
	      WRITE(6,906) IAM(),DSSODS/100,MOD(DSSODS,100),
     *                     CMONY(DSSABW,10,BETUNIT)
	   ENDIF    
	ELSEIF(SSFDCMB.EQ.0) THEN
 	   WRITE(6,910) IAM()
	   DSSODS = 0
	   DSSABW = 0
	ELSE
 	   WRITE(6,908) IAM() 
	   CALL SSODSCAN(DRAW,GIND,UCID,DSSODS,DSSABW) 
	   WRITE(6,906) IAM(),DSSODS/100,MOD(DSSODS,100),
     *                  CMONY(DSSABW,10,BETUNIT)
 	ENDIF
C
450     CONTINUE
C
	CALL INPYESNO('Do you want to change these odds [Y/N] ',FLAG)
	IF(FLAG.EQ.1) THEN
500	  CONTINUE
	  CALL INPNUM('Enter new odds [100-99999999]:',DSSODS,
     *	              100,99999999,EXT)
	  WRITE(6,907) IAM(),DSSODS/100,MOD(DSSODS,100)
	  CALL INPYESNO('Are the new odds entered correct [Y/N] ',FLAG)
	  IF(FLAG.NE.1) GOTO 500
	ENDIF
C
C
1000	CONTINUE
	CALL WRITEW(FDB,DRAW,DSSREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
	CALL CLOSEFIL(FDB)
	CALL SSRESULT(GIND,DRAW)	
	RETURN
C
C
C
900	FORMAT(1X,A,1X,A8,I1,' event ',I4,' invalid game status> ',I4)
902	FORMAT('Enter home score [C to cancel event]:')
9021	FORMAT('Enter away score ')
903	FORMAT(1X,A,' Set ',I1,1X,<SSNMS_LEN/4>A4,A)
9031	FORMAT(1X,A,' Home Score - Away score  ',I2,' - ',I2)
904	FORMAT(1X,A,' Master Event ',<SSNMS_LEN/4>A4,' cancelled')
905	FORMAT(1X,A,' Reading ',A8,I1,' pools for event ',I4)
906	FORMAT(1X,A,' Payout odds are ',I8,'.',I2.2,' to 1',
     *	       '    Winning Investment  -  ',A10)
907	FORMAT(1X,A,' New payout odds entered are ',I8,'.',I2.2,' to 1')
908	FORMAT(1X,A,' Dropped combinations in pools. Scan of drawfiles')
909	FORMAT(1X,A,' Preliminary payout odds are ',I8,'.',I2.2,' to 1',
     *	       '    Winning Investment  -  ',A10)
910     FORMAT(1X,A,'Winning combination not played')
	END

