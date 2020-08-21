C
C SUBROUTINE TSLENT
C $Log:   GXAFXT:[GOLS]TSLENT.FOV  $
C
C V09 17-DEC-1999 PXO Added a call to report subroutine
C V08 09-SEP-1996 RXK Rfss 259. Possibilty to postpone Winsel
C                     over initialized drawdate
C V07 05-FEB-1994 HXK FIX CHECK FOR DRAW RESULT; DISPLAY RESULTS IN SCANDO WAY.
C V06 26-JAN-1994 JXP Corrected display to screen 
C V05 21-JAN-1993 DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - tslent.for **
C
C TSLENT.FOR
C
C V04 02-SEP-92 GCAN ADDED CHECK FOR EVENT DRAW DATE INSTEAD OF ENDING
C                    SALES DATE (FOR POSTPONED WINNER SELECTION).
C V03 13-JUL-92 WLM  LOOK FOR TOTO SELECT NAME IN SCF RECORD 
C V02 01-NOV-91 MTK  INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C RESULT ENTRY SUBROUTINE FOR TOTO SELECT GAME.
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TSLENT(GNUM,GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

	INTEGER*4 FDB(7)
	INTEGER*4 GNUM, GIND, DRAW, ST, ROW, FLAG, EFLG, I, K

	INTEGER*4 POOL(3)
	INTEGER*4 EXT, RES, CDC
	INTEGER*2 DATE(LDATE_LEN)
	INTEGER*4 ROW_CHANGE(MAXSRW)
	INTEGER*4 TYPE_CHANGE(MAXSRW)
	INTEGER*4 CDC_CHANGE(MAXSRW)
	DATA POOL/ROWWIN,ROWLOS,ROWTIE/
C
C
	CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,3,DTSSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
	CALL READW(FDB,DRAW,DTSREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
C SCAN THROUGH THE DATE TABLE FOR TOTO SELECT AND SEE WHAT
C ROWS ARE TO BE DRAWN ON.
C
10	CONTINUE
	CALL FASTMOV(DTSDAT,TROWDAT(1,1),MAXSRW)
	CALL FASTMOV(DTSWIN,TROWWIN(1,1),MAXSRW)
	EFLG=0
C
C
	DO 15 ROW=1,DTSRWS
	   ROW_CHANGE(ROW)=0
	   TYPE_CHANGE(ROW)=0
	   CDC_CHANGE(ROW)=0
15	CONTINUE
C
	DO 1000 ROW=1,DTSRWS
	IF(DTSDAT(ROW).NE.DAYCDC) GOTO 1000
	IF(DTSSTA(ROW).NE.GAMBFD) GOTO 1000
	 EFLG=1
	WRITE(5,900)IAM(),ROW,(DTSNMS(K,1,ROW),K=1,3),
     *	                (DTSNMS(K,2,ROW),K=1,3)
	CALL INPNUM
     *		  ('Enter C=Cancel, P=Postpone, 1=Home, X=draw, 2=away, E=Exit',
     *		  RES,1,2,EXT)
	IF(EXT.EQ.-1) THEN		! Exit
	    EFLG=0
	    GOTO 1200
	ENDIF

	IF(EXT.EQ.-2) THEN		! Postpone
	   DATE(5)=DTSDTE
	   CALL LCDATE(DATE)
30	   CONTINUE
	   WRITE(5,904) IAM(),(DATE(K),K=7,13)
	   TYPE*,IAM(),' Enter new date for this row '
	   CALL INPDAT(CDC,EXT)
	   IF(EXT.LT.0) RETURN
	   CALL WIMG(5,' Is this correct [Y/N] ')
	   CALL YESNO(FLAG)
	   IF(FLAG.NE.1) GOTO 30
	   IF(CDC.LE.DAYCDC) THEN
	      TYPE*,IAM(),' Invalid date entered'
	      GOTO 30
	   ENDIF
	   IF(CDC.GT.DTSDTE) THEN
	      TYPE*,IAM(),' Date entered',CDC
	      TYPE*,IAM(),' Event draw date is',DTSDTE
	      CALL WIMG(5,' Do you want postpone over event draw date [Y/N]')
	      CALL YESNO(FLAG)
	      IF(FLAG.NE.1) GOTO 30
	   ENDIF
	   ROW_CHANGE(ROW)=1
	   TYPE_CHANGE(ROW)=-2
	   CDC_CHANGE(ROW)=CDC
	   GOTO 1000
	ENDIF
C
C
	IF(EXT.EQ.-5) THEN		! Cancel
	   ROW_CHANGE(ROW)=1
	   TYPE_CHANGE(ROW)=-5
	   GOTO 1000
	ENDIF
C
C
	IF((EXT.EQ.-6).OR.(RES.EQ.1).OR.(RES.EQ.2)) THEN
	   IF(EXT.EQ.-6) RES = 3				!Draw	
	   ROW_CHANGE(ROW)=1
	   TYPE_CHANGE(ROW)=RES
	ENDIF
1000	CONTINUE

1200	CONTINUE
	IF(EFLG.EQ.0) THEN
	  TYPE 902,IAM(),(SCFLGN(K,GNUM),K=1,4)
	  CALL CLOSEFIL(FDB)
	  RETURN
	ENDIF

	DO 1500 ROW=1,DTSRWS
	    IF(ROW_CHANGE(ROW).EQ.0) GOTO 1500
	    IF(TYPE_CHANGE(ROW).EQ.-2) THEN
		WRITE(5,903)IAM(),ROW,
     *			  (DTSNMS(K,1,ROW),K=1,3),
     *			  (DTSNMS(K,2,ROW),K=1,3)
  	    ENDIF
	    IF(TYPE_CHANGE(ROW).EQ.-5) THEN
		WRITE(5,905)IAM(),ROW,
     *			  (DTSNMS(K,1,ROW),K=1,3),
     *			  (DTSNMS(K,2,ROW),K=1,3)
  	    ENDIF
	    IF(TYPE_CHANGE(ROW).EQ.3) THEN
		WRITE(5,906)IAM(),ROW,
     *			  (DTSNMS(K,1,ROW),K=1,3),
     *			  (DTSNMS(K,2,ROW),K=1,3)
  	    ENDIF
	    IF(TYPE_CHANGE(ROW).EQ.1) THEN
		WRITE(5,907)IAM(),ROW,
     *			  (DTSNMS(K,1,ROW),K=1,3),
     *			  (DTSNMS(K,2,ROW),K=1,3)
  	    ENDIF
	    IF(TYPE_CHANGE(ROW).EQ.2) THEN
		WRITE(5,908)IAM(),ROW,
     *			  (DTSNMS(K,1,ROW),K=1,3),
     *			  (DTSNMS(K,2,ROW),K=1,3)
  	    ENDIF
1500	CONTINUE

	CALL WIMG(5,'Is this correct [Y/N] ')
	CALL YESNO(FLAG)
	IF(FLAG.NE.1) GOTO 10

	DO 1700 ROW=1,DTSRWS
	    IF(ROW_CHANGE(ROW).EQ.0) GOTO 1700

	    IF(TYPE_CHANGE(ROW).EQ.-2) 
     *		TROWDAT(ROW,1)=CDC_CHANGE(ROW)

	    IF(TYPE_CHANGE(ROW).EQ.-5) THEN
	    	DTSSTA(ROW)=GAMCAN
		TROWWIN(ROW,1)=ROWCAN
  	    ENDIF
	    IF(TYPE_CHANGE(ROW).EQ.1) THEN
		TROWWIN(ROW,1)=POOL(1)
		DTSSTA(ROW)=GAMENV
  	    ENDIF
	    IF(TYPE_CHANGE(ROW).EQ.2) THEN
		TROWWIN(ROW,1)=POOL(2)
		DTSSTA(ROW)=GAMENV
  	    ENDIF
	    IF(TYPE_CHANGE(ROW).EQ.3) THEN
		TROWWIN(ROW,1)=POOL(3)
		DTSSTA(ROW)=GAMENV
  	    ENDIF
1700	CONTINUE

	DTSSTS=GAMEN1
	OPDONE=1
C
C
C WAIT FOR VERIFICATION FROM REMOTE TERMINAL
C
2000	CONTINUE
	TYPE*,IAM(),' Waiting for remote terminal verification '
	IF(DTSSTS.EQ.GAMBFD) THEN
	  TYPE*,IAM(),' Remote entry does not match, please re-enter'
	  GOTO 10
	ENDIF
	CALL XWAIT(5,2,ST)
	IF(DTSSTS.NE.GAMENV) GOTO 2000
C
C
	CALL FASTMOV(TROWDAT(1,1),DTSDAT,MAXSRW)
	CALL FASTMOV(TROWWIN(1,1),DTSWIN,MAXSRW)
	DO 2100 I=1,DTSRWS
	IF(DTSSTA(I).NE.GAMCAN) GOTO 2100
	DTSODS(1,I)=100
	DTSODS(2,I)=100
	DTSODS(3,I)=100
2100	CONTINUE
	WRITE(5,901) IAM(),DTSSTS
	CALL WRITEW(FDB,DRAW,DTSREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
	CALL CLOSEFIL(FDB)
	CALL TSRESULT(GIND,DRAW)
	RETURN
C
C
900	FORMAT(1X,A,1X,'Result entry for row ',I4,1X,3A4,' vs ',3A4)
901	FORMAT(1X,A,1X,'Posting Status ',I2,' to Select Game File')
902	FORMAT(1X,A,' No result entry today for ',4A4)
903	FORMAT(1X,A,' Postponing row ',I4,1X,3A4,' vs ',3A4)
904	FORMAT(1X,A,' Last valid date for this match is ',7A2)
905     FORMAT(1X,A,' Cancellation of row ',I4,1X,3A4,' vs ',3A4)
906     FORMAT(1X,A,' You have entered X for row ',I4,1X,3A4,
     *		  ' vs ',3A4)
907     FORMAT(1X,A,' You have entered 1 for row ',I4,1X,3A4,
     *		  ' vs ',3A4)
908     FORMAT(1X,A,' You have entered 2 for row ',I4,1X,3A4,
     *		  ' vs ',3A4)
	END
