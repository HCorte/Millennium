C SUBROUTINE TSLSET
C
C V19 27-NOV-97 UXN ROWTYP field added for PITKA.
C     Rev 1.1   05 Jun 1997 10:43:00   UXN
C  Calculation of REV3 changed.
C     Rev 1.0   17 Apr 1996 15:38:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C     Rev 1.11   14 Apr 1996 18:33:04   HXK
C  Putting RXK's (Rita's) changes into archive
C     Rev 1.12   03 Apr 1996 11:19:42   RXK
C  Calculation of control revision byte changed
C     Rev 1.11   22 Mar 1996 15:32:44   RXK
C  Rfss 266 : lines with non-blank team name and with odds=0 modified
C     Rev 1.10   19 Feb 1996 21:10:16   HXK
C  Fix for updating game control counter
C     Rev 1.9   05 Nov 1993 20:22:58   GXA
C  Restored Previous Version. (Revisions work this way after all).
C     Rev 1.7   17 Oct 1993 22:28:56   HXK
C  removed debug type*,
C     Rev 1.6   13 Sep 1993 20:22:16   GXA
C  Set default system volume for poolfile volume if none set.
C     Rev 1.5   07 Sep 1993 16:06:58   HXK
C  Changed draw number output such that leading zeroes are not shown
C     Rev 1.4   07 Sep 1993 15:55:40   HXK
C  Made TV channels available for all rows
C     Rev 1.3   13 Aug 1993 11:46:46   HXN
C  Displayed DTSODS in 1,x,2 order instead of 1,2,x.
C     Rev 1.2   07 Jul 1993 17:37:58   GXA
C  Added TV-Chanel option and cleaned up input.
C     Rev 1.1   29 Jun 1993 18:12:04   GXA
C  Released for Finland Dec Conversion / Oddset.
C     Rev 1.0   21 Jan 1993 17:53:54   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C V03 02-SEP-92 GCAN ADDED ENTRY AND CHECKING OF EVENT DRAW DATE FOR
C		     POSTPONED DRAW FEATURE.
C V02 13-JUL-92 WLM  CHECKED AND RELEASED FOR THE NETHERLANDS
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C THIS IS A TEST CHANGE
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE TSLSET(GIND)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DTSREC.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	COMMON SCFREC
	INTEGER*4 DFDB(7),FDB(7),VFDB(7),INPFDB(7),BYTTAB(500)
	INTEGER*4 NAME(TNMS_LEN/4)
	INTEGER*4 GIND,GNUM,ST,I,DRW,EXT,LSTAT,J,WEK,ROW,TEMP,YEAR
	INTEGER*4 K,BROW,EROW,CDC,ODDS,BUFIND,REV1,REV2,REV3,REV4
	INTEGER*4 VERR,MXROW,STDRW,PREV3
	INTEGER*4 PRICE
	INTEGER*4 CLEARED				!Row cleared counter.
	INTEGER*2 BDATE(LDATE_LEN),EDATE(LDATE_LEN),EDDATE(LDATE_LEN)
        INTEGER*2 DATE(LDATE_LEN)
	INTEGER*2 DDATE(LDATE_LEN,MAXSRW)
	CHARACTER*6 PASS,OKPASS,GPASS
	CHARACTER*20 PWORD,CDTSPFN             
	CHARACTER*8 TOTO_NAME
	EQUIVALENCE (PASS,PWORD)
	CHARACTER*2 FUN
	CHARACTER*8 TIME				!Time Hold Variable
	CHARACTER CNAME(TNMS_LEN),INPFIL,ANS
C
	LOGICAL   RUNNING
C
	EQUIVALENCE (DTSPFN,CDTSPFN)
	EQUIVALENCE (NAME,CNAME)
	CHARACTER*12 GSTAT(11)
	DATA GSTAT/'Not set     ','Game closed ',
     *	           'Info entered','Game open   ',
     *	           3*'End of game ',
     *	           'Drawing done','Game final  ',
     *	           'Cancelled   ','Refund      '/
	CHARACTER*14 CDAY
	INTEGER*2 DAY(7)
	DATA CDAY/'---not set--- '/
	DATA OKPASS/'PERKIN'/
	DATA GPASS/'PERKIN'/
	EQUIVALENCE(CDAY,DAY)
C
        INTEGER*4 M251
        PARAMETER(M251=251)
C
	WRITE(TOTO_NAME,908) GTNAMES(TTSL)
C
C CHECK IF TOTO SELECT GAME IS ACTIVE
C
	GNUM=SCFGTN(TTSL,GIND)
	IF(GNUM.LT.1) THEN
	   WRITE(5,991) IAM(),GTNAMES(TTSL),GIND
	   CALL XWAIT(2,2,ST)
	   RETURN
	ENDIF
C
C
	CALL OPENW(1,SCFSFN(1,DAF),4,0,0,ST)
	CALL IOINIT(DFDB,1,DAFSEC*256)
	IF(ST.NE.0) THEN
	  CALL FILERR(SCFSFN(1,DAF),1,ST,0)
	  RETURN
	ENDIF
C
C
	CALL OPENW(2,SCFGFN(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,2,DTSSEC*256)
	IF(ST.NE.0)THEN
	  CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
	  CALL CLOSEFIL(DFDB)
	  RETURN
	ENDIF
C
C
	CALL OPENW(3,SCFGVN(1,GNUM),4,0,0,ST)
	CALL IOINIT(VFDB,3,DTSSEC*256)
	IF (ST.NE.0)THEN
	   CALL FILERR(SCFGVN(1,GNUM),1,ST,0)
	   CALL CLOSEFIL(DFDB)
	   CALL CLOSEFIL(FDB)
	   RETURN
	ENDIF
C
C
10	CONTINUE
	CALL CLRSCR(5)
	WRITE(5,900)
	CALL WIMG(5,'Enter function: ')
	READ(5,901) FUN
C
C
	IF(FUN.EQ.'EX') THEN
	  CALL CLOSEFIL(DFDB)
	  CALL CLOSEFIL(FDB)
	  CALL CLOSEFIL(VFDB)
	  RETURN
	ENDIF
C
C
	IF(FUN.NE.'LI'.AND.FUN.NE.'MO'.AND.FUN.NE.'VE'.AND.
     *	   FUN.NE.'UN'.AND.FUN.NE.'CH'.AND.FUN.NE.'GT') THEN
	  CALL CLRSCR(5)
	  TYPE*,IAM(),'Invalid input '
	  CALL XWAIT(1,2,ST)
	  GOTO 10
	ENDIF
	IF(FUN.EQ.'GT') GOTO 4000
C
	CALL FASTMOV(FDB,INPFDB,7)
	IF(FUN.EQ.'LI'.OR.FUN.EQ.'MO') THEN
15	  CONTINUE
	  CALL WIMG(5,'Use (P)rimary or (V)erification file as input?')
	  READ(5,902) INPFIL
	  IF(INPFIL.EQ.'E') GOTO 10
	  IF(INPFIL.EQ.'V') THEN
	    CALL FASTMOV(VFDB,INPFDB,7)
	  ELSE
	    IF (INPFIL.NE.'P')THEN
	       TYPE*,IAM(),'Options are P, V, or E only...Retry.'
	       GOTO 15
	    ENDIF
	  ENDIF
	ENDIF
C
20	CONTINUE
	CALL INPNUM('Enter event number (E-exit) ',DRW,1,9999,EXT)
	IF(EXT.LT.0)GOTO 10
C
C       Read text checksum of previous draw
C
	PREV3 = 0
	IF(DRW.GT.1) THEN
	  CALL READW(INPFDB,DRW-1,DTSREC,ST)
	  IF(ST.NE.0) THEN
	    CALL CLRSCR(5)
	    IF(INPFIL.EQ.'V')THEN
	      CALL FILERR(SCFGVN(1,GNUM),2,ST,DRW-1)
	    ELSE
	      CALL FILERR(SCFGFN(1,GNUM),2,ST,DRW-1)
	    ENDIF
	    CALL XWAIT(2,2,ST)
	    GOTO 20
	  ENDIF
	  CALL ILBYTE(PREV3,DTSREV,2)          !TEXT REV # BYTE   (SEQUENCE #)
        ENDIF
C
C	Read the event record.
C
	
	CALL CLRSCR(5)
	CALL READW(INPFDB,DRW,DTSREC,ST)
	IF(ST.NE.0) THEN
	  CALL CLRSCR(5)
	  IF(INPFIL.EQ.'V')THEN
	    CALL FILERR(SCFGVN(1,GNUM),2,ST,DRW)
	  ELSE
	    CALL FILERR(SCFGFN(1,GNUM),2,ST,DRW)
	  ENDIF
	  CALL XWAIT(2,2,ST)
	  GOTO 20
	ENDIF
C
C
	LSTAT=DTSSTS+1
	DO 30 I=1,7
	BDATE(I+6)=DAY(I)
	EDATE(I+6)=DAY(I)
30	CONTINUE
	DO 50 I=1,DTSRWS
	DO 40 J=1,7
	DDATE(J+6,I)=DAY(J)
40	CONTINUE
50	CONTINUE
	BDATE(5)=DTSBSD
	IF(BDATE(5).NE.0)CALL LCDATE(BDATE)
	EDATE(5)=DTSESD
	IF(EDATE(5).NE.0)CALL LCDATE(EDATE)
	EDDATE(5)=DTSDTE
	IF(EDDATE(5).NE.0)CALL LCDATE(EDDATE)
	DO 60 I=1,DTSRWS
	DDATE(5,I)=DTSDAT(I)
	IF(DDATE(5,I).NE.0)CALL LCDATE(DDATE(1,I))
60	CONTINUE
	WEK=DTSWEK
	PRICE=DTSPRC
C
C
	IF(FUN.EQ.'MO'.AND.DTSSTS.GE.GAMOPN) THEN
	  CALL CLRSCR(5)
	   WRITE(5,992) IAM(),GTNAMES(TTSL),GIND,DRW,IAM()
	   CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	ENDIF
C
C
	DO 70 I=1,TNMS_LEN/4
	DO 70 J=1,2
	DO 70 K=1,DTSRWS
	   IF(DTSNMS(I,J,K).EQ.0)THEN
	     DTSNMS(I,J,K)='    '
	   ENDIF
70	CONTINUE
C
C DETERMINE IF EVENT PICKED IS ACTIVE
C
	RUNNING=.FALSE.
	IF(DTSBSD.GT.0) THEN
	  DO 80 I=DTSBSD,DTSESD
	     CALL READW(DFDB,I,DAFREC,ST)
	     IF(ST.NE.0) THEN
	        CALL CLRSCR(5)
	        CALL FILERR(SCFSFN(1,DAF),2,ST,I)
	        GOTO 10
	     ENDIF
	     IF(DAFSTS.GT.DSOPEN) RUNNING=.TRUE.
	     IF(DTSBSD.LE.0) RUNNING=.FALSE.
80	  CONTINUE
	ENDIF
C
	IF(FUN.EQ.'MO') GOTO 120
	IF(FUN.EQ.'VE') GOTO 1000
	IF(FUN.EQ.'UN') GOTO 2000
	IF(FUN.EQ.'CH') GOTO 3000
C
C DISPLAY GAME DATA
C
	BROW=1
	EROW=18
105	CONTINUE
	CALL CLRSCR(5)
	WRITE(5,903) (SCFGFN(I,GNUM),I=1,5),GIND,DRW,WEK,GSTAT(LSTAT)
      WRITE(5,904)(BDATE(I),I=7,13),BDATE(5),(EDATE(I),I=7,13),EDATE(5)
	WRITE(5,905) (EDDATE(I),I=7,13),EDDATE(5)
	DO 110 I=BROW,EROW
	WRITE(TIME,908) DISTIM(DTSTIM(I))
	WRITE(5,906) I,(DTSNMS(J,1,I),J=1,4),(DTSNMS(J,2,I),J=1,4),
     *                 DTSROWTYP(I),
     *	               DTSODS(1,I)/100,MOD(DTSODS(1,I),100),
     *	               DTSODS(3,I)/100,MOD(DTSODS(3,I),100),
     *	               DTSODS(2,I)/100,MOD(DTSODS(2,I),100),
     *	               (DDATE(J,I),J=7,13),DDATE(5,I),TIME(1:5)
110	 CONTINUE
	 IF(EROW.EQ.DTSRWS) GOTO 115
	 IF(BROW.EQ.1.OR.BROW.EQ.19.OR.BROW.EQ.37) THEN
	   CALL WIMG(5,'Hit return to see remaining rows')
	   READ(5,901) ANS
	   BROW=BROW+18
	   EROW=EROW+18
	   IF(EROW.GT.DTSRWS) EROW=DTSRWS
	   IF(ANS.EQ.'T') THEN
	      TYPE*,IAM(),'***** TEST DATA *****'
	      TYPE*,IAM()
	      TYPE*,IAM(),'DTSREV: ',DTSREV
	      TYPE*,IAM()
	      CALL WIMG(5,'Hit return to continue')
	      READ(5,901) ANS
	   ENDIF
	   GOTO 105
	ENDIF
115	WRITE(5,910) (DTSPFN(K),K=1,5)
C
	GOTO 20
C
C
C MODIFY GAME DATE
C
C
120	CONTINUE
	IF(.NOT.RUNNING) THEN
	   WRITE(5,917) IAM()
	   WRITE(5,918) IAM(),(BDATE(I),I=7,13)
	   CALL INPDAT(CDC,EXT)
	   IF(EXT.NE.0) CDC = DTSBSD
	ELSE
	   TYPE*,IAM(),'Begining sales date will be CDC..........:',DTSBSD
	   CDC=DTSBSD
	ENDIF
	BDATE(5)=CDC
	CALL LCDATE(BDATE)
C
C
        WRITE(5,919) IAM()
        WRITE(5,918) IAM(),(EDATE(I),I=7,13)
	CALL INPDAT(CDC,EXT)
	IF(EXT.NE.0) CDC = DTSESD
	EDATE(5)=CDC
	CALL LCDATE(EDATE)
	CALL FIGWEK(CDC-WEEK_OFFSET,WEK,YEAR)
C
C
        WRITE(5,923) IAM()
        WRITE(5,924) IAM(),(EDDATE(I),I=7,13)
	CALL INPDAT(CDC,EXT)
	IF(EXT.NE.0) CDC = DTSDTE
	EDDATE(5) = CDC
	CALL LCDATE(EDDATE)
C
C***	CALL INPMONY('Enter base price (E-no change)..........',
C***     *  PRICE,BETUNIT,EXT)
C***	IF(EXT.NE.0) PRICE = DTSPRC
C
	IF(.NOT.RUNNING) THEN
	   DO 130 I=1,DTSRWS
	      IF(DTSSTA(I).NE.GAMBFD) DTSSTA(I)=0
130	   CONTINUE
	ENDIF
C

C
300	CONTINUE
	TYPE*,IAM(),'Number of Rows in this Event............:',DTSRWS
	CALL INPNUM('Enter row number to be modified (E-Exit)',
     *	      ROW,1,DTSRWS,EXT)
	IF(EXT.LT.0) GOTO 500
C
C CHECK IF ROW IS MODIFIABLE
C
	IF(DTSSTA(ROW).NE.GAMINF.AND.DTSSTA(ROW).NE.0) THEN
	   TYPE*,IAM(),'Row ',ROW,' is not open, so you do not want ',
     *	         'to modify it. '
	   GOTO 300
	ENDIF
C
        WRITE(5,940) IAM(),'1'
        WRITE(5,941) IAM(),(DTSNMS(K,1,ROW),K=1,TNMS_LEN/4)
 	CALL WIMG(5,'Enter Team 1 Name......................')
	READ(5,907)NAME
	CLEARED = 0
	DO I = 1,TNMS_LEN/4
	   IF(NAME(I).EQ.'    ') CLEARED = CLEARED + 1
	END DO
	IF(CLEARED.EQ.TNMS_LEN/4) GOTO 210
	IF(CNAME(1).EQ.'+') THEN
	   DO I = 1,TNMS_LEN/4
	      NAME(I) = '    '
	   END DO
	ENDIF
	CALL FASTMOV(NAME,DTSNMS(1,1,ROW),TNMS_LEN/4)
210	CONTINUE
C
C
        WRITE(5,940) IAM(),'2'
        WRITE(5,941) IAM(),(DTSNMS(K,2,ROW),K=1,TNMS_LEN/4)
	CALL WIMG(5,'Enter Team 2 Name......................')
	READ(5,907)NAME
	WRITE(5,907) (NAME(I),I=1,4)
	CLEARED = 0 
	DO I = 1,TNMS_LEN/4
	   IF(NAME(I).EQ.'    ')  CLEARED = CLEARED + 1
	END DO
	IF(CLEARED.EQ.TNMS_LEN/4) GOTO 220
	IF(CNAME(1).EQ.'+') THEN
	   DO I = 1,TNMS_LEN/4
	      NAME(I) = '    '
	   END DO
	ENDIF
	CALL FASTMOV(NAME,DTSNMS(1,2,ROW),TNMS_LEN/4)
220	CONTINUE
C
C
	TYPE*,IAM()
	TYPE*,IAM(),'Enter Odds X.YY in the form of XYY........'
	TYPE*,IAM()
	TYPE*,IAM(),'Current home odds is......................',
     *  DTSODS(1,ROW)
	CALL INPNUM('Enter home odds (E-no change)...........',
     *  ODDS,0,9999,EXT)
	IF(EXT.GE.0) DTSODS(1,ROW)=ODDS
C
C
	TYPE*,IAM(),'Current tie odds is.......................',
     *  DTSODS(3,ROW)
	CALL INPNUM('Enter tie odds (E-no change)............',
     *  ODDS,0,9999,EXT)
	IF(EXT.GE.0) DTSODS(3,ROW)=ODDS
C
C
	TYPE*,IAM(),'Current away odds is......................',
     *  DTSODS(2,ROW)
	CALL INPNUM('Enter away odds (E-no change)...........',
     *  ODDS,0,9999,EXT)
	IF(EXT.GE.0) DTSODS(2,ROW)=ODDS



        WRITE(5,937) IAM()
        WRITE(5,938) IAM(),(DTSTVC(K,ROW),K=1,TTVC_LEN/4)
        CALL WIMG(5,'Enter TV-Chanel Name...................')
        READ(5,939) (NAME(K),K=1,TTVC_LEN/4)
C
C DID HE HOT RETURN?
C
        CLEARED = 0
        DO I = 1,TTVC_LEN/4
           IF(NAME(I).EQ.'    ') CLEARED = CLEARED + 1
        END DO
        IF(CLEARED.EQ.TTVC_LEN/4) GOTO 400
C
C DOES HE WANT US TO CLEAR IT?
C
        IF(CNAME(1).EQ.'+') THEN
           DO I = 1,TTVC_LEN/4
              NAME(I) = '    '
           END DO
        ENDIF
C
C MOVE THE NAME OVER.
C
        DO I = 1,TTVC_LEN/4
           DTSTVC(I,ROW) = NAME(I)
        END DO
C




400     CONTINUE


	TYPE*,' '
	WRITE(5,993) IAM(),GTNAMES(TTSL),GIND,ROW	!Event date for row.
	WRITE(5,994) IAM(),DTSDAT(ROW)
	CALL INPDAT(CDC,EXT)
	IF(EXT.NE.0) CDC = DTSDAT(ROW)
	DDATE(5,ROW)=CDC
	CALL LCDATE(DDATE(1,ROW))
	WRITE(5,921) IAM(),ROW				!CLOSE TIME FOR ROW
	WRITE(5,922) IAM(),DISTIM(DTSTIM(ROW))
	CALL INPTIM('Enter time HH:MM:SS.....................',TEMP,EXT)
	IF(EXT.EQ.0) DTSTIM(ROW) = TEMP
	CALL INPNUM('Enter row type 1-single,2-double,3-regular',TEMP,1,3,EXT)
	IF(EXT.EQ.0) DTSROWTYP(ROW) = TEMP
	GOTO 300
C
C
500	CONTINUE
	DTSDRW=DRW
	DTSBSD=BDATE(5)
	DTSESD=EDATE(5)
	DTSDTE=EDDATE(5)
	DTSWEK=WEK
	DTSSTS=GAMINF
	DTSPRC=PRICE
	DO I=1,DTSRWS
	   DTSDAT(I)=DDATE(5,I)
	   IF(DTSNMS(1,1,I).NE.'    '.AND.DTSSTA(I).EQ.0)DTSSTA(I)=GAMINF
	END DO
        WRITE(5,929) IAM()
        WRITE(5,930) IAM(),DTSPFN(1)
 	WRITE (CDTSPFN,911) GIND,DRW
	CALL WIMG(5,
     *	'Enter pool file volume name (RETURN for system volume) ')
	READ(5,913) NAME(1)
	IF(NAME(1).NE.'    ') THEN
	   DTSPFN(1) = NAME(1)
	ELSE
	   CALL SYSVOL(DTSPFN(1))
	ENDIF
C
C CREATE TABLE OF TEXT MESSAGE TO CHECKSUM FOR REV
C
	BUFIND=1
C
C LOAD UP ODDS AND PLAYER NAMES
C
	DO I=1,MAXSRW		                      !FOR ALL ROWS
	   DO J=1,3		                      !FOR ALL ODDS
	      CALL MOVBYT(DTSODS(J,I),1,BYTTAB,BUFIND,2)
	      BUFIND=BUFIND+2
	   END DO
C
	   DO J=1,2		                      !FOR ALL TEAMS
	      CALL MOVBYT(DTSNMS(1,J,I),1,BYTTAB,BUFIND,TNMS_LEN-2)
	      BUFIND = BUFIND + (TNMS_LEN-2)
	   END DO
	END DO
	BUFIND = BUFIND - 1
	CALL CHECKSUM(BYTTAB,1,BUFIND,REV4)
	CALL ILBYTE(REV1,DTSREV,0)
        IF(DTSDRW.EQ.M251-1) THEN
           REV1 = MOD(REV1+DTSDRW,(M251-10)) + 1
        ELSE
           REV1 = MOD(REV1+DTSDRW,M251) + 1
        ENDIF
	REV2 = MOD(DTSDRW,255)
	CALL ILBYTE(REV3,DTSREV,2)          !GET PREVIOUS TEXT REV #
	REV3 = MOD(PREV3 + REV3,255) + 1
	CALL ISBYTE(REV1,DTSREV,0)          !CONTROL REV BYTE  (SEQUENCE #)
	CALL ISBYTE(REV2,DTSREV,1)          !DRAW REV BYTE
	CALL ISBYTE(REV3,DTSREV,2)          !TEXT REV # BYTE   (SEQUENCE #)
	CALL ISBYTE(REV4,DTSREV,3)          !TEXT CHECKSUM BYTE
	CALL WRITEW(INPFDB,DRW,DTSREC,ST)
	IF(ST.NE.0) THEN
	  CALL CLRSCR(5)
	  IF(INPFIL.EQ.'P')THEN
	    CALL FILERR(SCFGFN(1,GNUM),3,ST,DRW)
	  ELSEIF(INPFIL.EQ.'V')THEN
	    CALL FILERR(SCFGVN(1,GNUM),3,ST,DRW)
	  ENDIF
	  GOTO 10
	ENDIF
	GOTO 20
C
C
C VERIFICATION
C
1000	CONTINUE
	CALL CLRSCR(5)
	CALL PASSWORD(5,PWORD)
	IF(PASS.NE.OKPASS) THEN
	  TYPE*,IAM(),'Invalid password entered'
	  CALL XWAIT(2,2,ST)
	  GOTO 10
	ENDIF
	VERR=0
C
C
	IF(DTSSTS.EQ.GAMOPN) THEN
	  TYPE*,IAM(),TOTO_NAME,GIND,' event ',DRW,' already verified'
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	ENDIF
C
C
	IF(DTSSTS.NE.GAMINF) THEN
	  TYPE*,IAM(),TOTO_NAME,GIND,' event ',DRW,' invalid game status ',
     *	        DTSSTS
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	ENDIF
	IF(DTSBSD.GT.DTSESD) THEN
	  TYPE*,IAM(),'Begining sales date greater then ending sales date'
	  VERR=VERR+1
	ENDIF
C
C
	IF(DTSBSD.EQ.0) THEN
	  TYPE*,IAM(),'Begining sales date not set '
	  VERR=VERR+1
	ENDIF
C
C
	IF(DTSESD.EQ.0) THEN
	  TYPE*,IAM(),'Ending sales date not set'
	  VERR=VERR+1
	ENDIF
C
C
	IF(DTSDTE.EQ.0) THEN
	   TYPE*,IAM(),'Event draw date not set'
	   VERR = VERR + 1
	ENDIF
C
C
	IF(DTSDTE.LT.DTSESD) THEN
	   TYPE*,IAM(),'Event draw date is less then ending sales date'
	   VERR = VERR + 1
	ENDIF
C
C
	IF(DTSPRC.LE.0) THEN
	  TYPE*,IAM(),'Base price not set'
	  VERR=VERR+1
	ENDIF
C
C
	DO 1010 I=1,DTSRWS
	IF(DTSSTA(I).EQ.0 .OR. DTSSTA(I).EQ.GAMBFD) GOTO 1010
	IF(DTSDAT(I).LT.DTSBSD) THEN
	  TYPE*,IAM(),'Date for row ',I,' is less than begining sales date'
	  VERR=VERR+1
	ENDIF
	IF(DTSDAT(I).GT.DTSDTE) THEN
	  TYPE*,IAM(),'Date for row ',I,' is greater than event draw date'
	  VERR=VERR+1
	ENDIF
	IF(DTSODS(1,I).EQ.0) THEN
	  TYPE*,IAM(),'Home odds for row ',I,' not set'
CCC	  VERR=VERR+1
	ENDIF
	IF(DTSODS(2,I).EQ.0) THEN
	  TYPE*,IAM(),'Away odds for row ',I,' not set'
CCC	  VERR=VERR+1
	ENDIF
	IF(DTSODS(3,I).EQ.0) THEN
	  TYPE*,IAM(),'Tie odds for row ',I,' not set'
CCC	  VERR=VERR+1
	ENDIF
1010	CONTINUE
C
C
	IF(VERR.NE.0) THEN
	  TYPE*,IAM(),VERR,' data entry errors found for Toto Select event ',
     *    DRW
	  TYPE*,IAM(),'Event has not been verified'
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	ENDIF
C
C
	DO 1020 I=DTSBSD,DTSESD
	DATE(5)=I
	CALL LCDATE(DATE)
	CALL READW(DFDB,I,DAFREC,ST)
	IF(ST.NE.0) THEN
	  CALL CLRSCR(5)
	  CALL FILERR(SCFSFN(1,DAF),2,ST,I)
	  GOTO 10
	ENDIF
C
C
	IF(DAFDRW(GNUM).NE.0.AND.DAFDRW(GNUM).NE.DRW) THEN
	  WRITE(5,909) (DATE(K),K=7,13),GTNAMES(TTSL),DAFDRW(GNUM)
	  VERR=VERR+1
	ENDIF
1020	CONTINUE
C
C
	IF(VERR.NE.0) THEN
	  TYPE*,IAM(),VERR,' game date errors found for ',TOTO_NAME,
     *    ' event ',DRW
	  TYPE*,IAM(),'Event has not been verified'
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	ENDIF
C
C
	DTSSTS=GAMOPN
	DO I=1,DTSRWS
           IF(DTSSTA(I).EQ.GAMINF. AND.
     *       .NOT.(DTSODS(1,I).EQ.0 .OR. 
     *             DTSODS(2,I).EQ.0 .OR.
     *             DTSODS(3,I).EQ.0)) DTSSTA(I) = GAMOPN
	END DO
C
C
	DO I=DTSBSD,DTSESD
	   CALL READW(DFDB,I,DAFREC,ST)
	   IF(ST.NE.0) THEN
	      CALL FILERR(SCFSFN(1,DAF),2,ST,I)
	      GOTO 10
	   ENDIF
	   DAFDRW(GNUM)=DRW
	   CALL WRITEW(DFDB,I,DAFREC,ST)
	   IF(ST.NE.0) THEN
	      CALL FILERR(SCFSFN(1,DAF),3,ST,I)
	      GOTO 10
	   ENDIF
	END DO
C
C
	 CALL WRITEW(FDB,DRW,DTSREC,ST)
	 IF(ST.NE.0) THEN
	    CALL FILERR(SCFGFN(1,GNUM),3,ST,DRW)
	    GOTO 10
	 ENDIF
	 TYPE*,IAM(),TOTO_NAME,GIND,' event ',DRW,' verify complete'
	 CALL WIMG(5,'Hit return to continue')
	 READ(5,901) ANS
	 GOTO 10
C
C
2000	CONTINUE
	CALL CLRSCR(5)
	CALL PASSWORD(5,PWORD)
	IF(PASS.NE.OKPASS) THEN
	   TYPE*,IAM(),'Invalid password entered'
	   CALL XWAIT(2,2,ST)
	   GOTO 10
	ENDIF
C
C
	IF(DTSSTS.LE.GAMINF) THEN
	  TYPE*,IAM(),TOTO_NAME,'event ',DRW,' has not been verified '
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	ENDIF
C
C
	IF(DTSSTS.GT.GAMOPN) THEN
	  TYPE*,IAM(),TOTO_NAME,'event ',DRW,' has already ended'
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	ENDIF
C
C
	DTSSTS=GAMINF
	DO  I=1,MAXSRW
	   IF(DTSSTA(I).EQ.GAMOPN) DTSSTA(I)=GAMINF
	END DO
C
C
	DO I=DTSBSD,DTSESD
	   CALL READW(DFDB,I,DAFREC,ST)
	   IF(ST.NE.0) THEN
	      CALL CLRSCR(5)
	      CALL FILERR(SCFSFN(1,DAF),2,ST,I)
	      GOTO 10
	   ENDIF
	   DAFDRW(GNUM)=0
	   CALL WRITEW(DFDB,I,DAFREC,ST)
	   IF(ST.NE.0) THEN
	      CALL CLRSCR(5)
	      CALL FILERR(SCFSFN(1,DAF),3,ST,I)
	      GOTO 10
	   ENDIF
	END DO
C
	CALL WRITEW(FDB,DRW,DTSREC,ST)
	IF(ST.NE.0) THEN
	   CALL CLRSCR(5)
	   CALL FILERR(SCFGFN(1,GNUM),3,ST,DRW)
	   GOTO 10
	ENDIF
	TYPE*,IAM(),TOTO_NAME,GIND,' event ',DRW,
     *	      ' is un-verified and can be modified'
	CALL WIMG(5,'Hit return to continue')
	READ(5,901) ANS
	GOTO 10
C
C  CHECK AND VALIDATE DATA FILE.
C
3000	CONTINUE
	IF((FUN.EQ.'CH').AND.(DTSSTS.GE.GAMOPN)) THEN
	  CALL CLRSCR(5)
        TYPE*,IAM(),TOTO_NAME,GIND,' event ',DRW,
     *        ' data has been verified.'
	  TYPE*,IAM(),' Changes cannot be made unless event is UNverified.'
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901)ANS
	  GOTO 10
	ENDIF
C
	CALL LNCOMP(FDB,VFDB,GIND,DRW,GNUM,ST)
C
	IF(ST.NE.0)TYPE*,'Error in LNCOMP, ST :',ST
	GOTO 20
C
C  Set maximum number of rows.
C
4000	CONTINUE
	CALL CLRSCR(5)
	TYPE*,IAM(),'This function is for GTECH programmers ONLY!'
	CALL PASSWORD(5,PWORD)
	IF(PASS.NE.GPASS)  THEN
	  TYPE*
	  TYPE*,IAM(),'HEY!,Don''t mess with this if you don''t'
	  TYPE*,IAM(),'know what you are doing!!!!'
	  CALL XWAIT(2,2,ST)
	  GOTO 10
	ENDIF
	CALL CLRSCR(5)
	CALL INPNUM('Enter highest number of rows:',MXROW,1,40,EXT)
	CALL INPNUM('Enter starting draw number:',STDRW,1,9999,EXT)
        I=STDRW
4005	CONTINUE
	CALL READW(FDB,I,DTSREC,ST)
	IF(ST.EQ.144) GOTO 4020
	IF(ST.NE.0)  THEN
	   CALL FILERR(SCFGFN(1,GNUM),2,ST,I)
	   CALL XWAIT(2,2,ST)
	   GOTO 10
	ENDIF
	IF(DTSSTS.GT.GAMOPN.AND.MXROW.LT.DTSRWS)  THEN
	   TYPE*,IAM(),'Can''t change it below existing value ',DTSRWS
	   TYPE*,IAM(),'when game is closed'
	   CALL XWAIT(2,2,ST)
	   GOTO 10
	ENDIF
	DTSRWS=MXROW
	CALL WRITEW(FDB,I,DTSREC,ST)
	IF(ST.NE.0)  THEN
	   CALL FILERR(SCFGFN(1,GNUM),2,ST,I)
	   CALL XWAIT(2,2,ST)
	   GOTO 10
	ENDIF
4010	CONTINUE
	CALL READW(VFDB,I,DTSREC,ST)
	IF(ST.EQ.144) GOTO 4020
	IF(ST.NE.0)  THEN
	   CALL FILERR(SCFGVN(1,GNUM),2,ST,I)
	   CALL XWAIT(2,2,ST)
	   GOTO 10
	ENDIF
	IF(DTSSTS.GT.GAMOPN.AND.MXROW.LT.DTSRWS)  THEN
	   TYPE*,IAM(),'Can''t change it below existing value ',DTSRWS
	   TYPE*,IAM(),'when game is closed'
	   CALL XWAIT(2,2,ST)
	   GOTO 10
	ENDIF
	DTSRWS=MXROW
	CALL WRITEW(VFDB,I,DTSREC,ST)
	IF(ST.NE.0)  THEN
	   CALL FILERR(SCFGVN(1,GNUM),2,ST,I)
	   CALL XWAIT(2,2,ST)
	   GOTO 10
	ENDIF
4015	CONTINUE
	I=I+1
	GOTO 4005
4020	CONTINUE
	TYPE*,IAM(),'File has been initialized from draw number ',STDRW
	TYPE*,IAM(),'to draw number ',I
	CALL XWAIT(2,2,ST)
	GOTO 10
C
C  Format statements.
C
900	FORMAT(' LI - List event data',/,
     *	       ' MO - Modify event data',/,
     *	       ' VE - Verify event data',/,
     *	       ' UN - Unverify event data',/,
     *	       ' CH - Check event data',/,
     *	       ' GT - Max rownumbers (GTECH only)',/,
     *	       ' EX - Return to main menu',/)
901	FORMAT(A2)
902	FORMAT(A1)
903	FORMAT(1X,5A4,I1,2X,'DRAW ',I4,2X,'WEEK ',I2.2,
     *	       2X,'STATUS - ',A14)
904	FORMAT(/,' SALES DATES  ',7A2,'  CDC - ',I4.4,
     *	               '  < TO >  ',7A2,' CDC - ',I4.4)
905	FORMAT(' ROW      NAME',18X,'TYP',1X,'EVENT DATE  ',7A2,' CDC - ',I4.4)

906	FORMAT(' ',I2.2,1X,4A4,'/',4A4,1X,I1,1X,
     *	       I2,'.',I2.2,1X,I2,'.',I2.2,1X,I2,'.',I2.2,1X,
     *	       7A2,1X,I4.4,1X,A5)
907	FORMAT(<TNMS_LEN/4>A4)
908	FORMAT(A8)
909	FORMAT(1X,7A2,' is already active for ',A8,' event # ',I4)
910	FORMAT(1X,'Pool file name ',5A4)
911	FORMAT(4X,':TS',I1,'P',I4.4,'.FIL   ')
912	FORMAT(3A4)
913     FORMAT(A4)
917     FORMAT(1X,A,'Begining sales date')
918	FORMAT(1X,A,'Current is, (E-no change)...............',7A2)
C
919     FORMAT(1X,A,'Ending sales date')
C
921     FORMAT(1X,A,'Close time for Row# ',I2)
922     FORMAT(1X,A,'Current is, (E-no change)...............',A8)

923     FORMAT(1X,A,'Event date')
924	FORMAT(1X,A,'Current is, (E-no change)...............',7A2)
929     FORMAT(1X,A,'Pool file volume name')
930     FORMAT(1X,A,'Current is, (RETURN for System Volume)..',3A4)
937     FORMAT(1X,A,'TV-Chanel Name')
938     FORMAT(1X,A,'Current is, (RETURN no change /+ clear)',
     *         <TTVC_LEN/4>A4)
939     FORMAT(<TTVC_LEN/4>A4)
940     FORMAT(1X,A,'Team name'1X,A1)
941     FORMAT(1X,A,'Current is, (RETURN no change /+ clear)',
     *         <TNMS_LEN/4>A4)
991	FORMAT(1X,A,'Sorry, ',A8,1X,I1,' game not active')
992	FORMAT(1X,A,A8,1X,I1,' draw ',I4,' has been verified',/,
     *         1X,A,A8,'Changes can not be made unless event ',
     *                 'is UNverified')
993	FORMAT(1X,A,'Enter date for ',A8,1X,I1,' row# ',I2,
     *              ' (E-no change):')
994	FORMAT(1X,A,'Current is, (E-no change)...............',I4)
C
	END
