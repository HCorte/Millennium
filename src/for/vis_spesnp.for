C
C SUBROUTINE SPESNP
C
C V08 19-JAN-2000 UXN Fix for SUPRPT.
C V07 23-NOV-1994 HXK Rearranged for print
C V06 25-OCT-1994 PXB Move screen up 1 line to enable bingo game to be 
C                     displayed.
C V05 13-JUN-1993 HXK added AGTINF.DEF, PRMAGT.DEF
C V04 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V03 07-AUG-1992 WLM  FIXED A BUG IN DISPLAY LAYOUT.
C V02 29-APR-1992 GCAN ADDED ODSNEW PARAMETER.
C V01 10-JAN-1991 GCAN INITIAL RELEASE FOR THE NETHERLANDS.
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SPESNP(CLINE,GAM,NUM,PAGE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
C
	INTEGER*4 MAXREP
	PARAMETER(MAXREP=7)
C
	REAL*8 K(MAXREP)
C
	INTEGER*4 T, S, BITVAL, I, IND, ST, BITMAP(3), GTYP
	INTEGER*4 BITIND, VALUE, KEYNUM, POS, NUM, GAM
	INTEGER*4 CLINE(20),BUF(CDLEN),BITSTS(MAXREP)
	INTEGER*4 PAGE
C
	INTEGER*4    G
	CHARACTER*34 GAMSTAT(MAXGAM+MAXREP)
	CHARACTER*46 REPTYPE(MAXGAM+MAXREP)
C
	CHARACTER*8 STATUS(2)
	CHARACTER*28 TEXT(MAXREP)
	DATA STATUS/'Enabled ','Disabled'/
	DATA K/'GAMRep  ','SALRep  ','INVRep  ','DSLRep  ','INFRep  ',
     *	       'NEWRep  ','ODSNew  '/
	DATA TEXT/' - All game result reports..',
     *	          ' - All sales reports........',
     *	          ' - All invoice reports......',
     *	          ' - All daily sales reports..',
     *	          ' - All information reports..',
     *	          ' - All news message reports.',
     *            ' - All Odds are sent........'/
	DATA BITSTS/MAXREP*0/
	INTEGER*4   FROM,UPTO,REPIND
C
	IF(PAGE.LE.0) THEN
	    PAGE = 1
	ELSEIF((PAGE-1)*20.GE.MAXGAM) THEN
	    PAGE = MAXGAM/20
	    IF(MOD(MAXGAM,20).NE.0) PAGE=PAGE+1
	ENDIF 
C
	FROM = (PAGE-1)*20 + 1
	UPTO = MIN(FROM+19,MAXGAM)
C
C GET GAME NUMBER AND SPESNP INPUT
C
	IF(GAM.LT.1.OR.GAM.GT.MAXGAM) THEN
	   POS=1
	   CALL KEY(CLINE,K,MAXREP,POS,KEYNUM)
	   IF(POS.GT.40) GOTO 100
	   IF(KEYNUM.EQ.0) THEN
	      WRITE(CLIN23,2023)
	      GOTO 100
	   ENDIF
	   CALL NUMB(CLINE,POS,VALUE)
	   IF(VALUE.LT.0) THEN
	      WRITE(CLIN23,2023)
	      GOTO 100
	   ENDIF
	   BITIND=MAXGAM+KEYNUM
	ELSE
	   GTYP=GNTTAB(GAMTYP,GAM)
	   IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP) THEN
	      WRITE(CLIN23,1023) GAM
	      RETURN
	   ENDIF
	   BITIND=GAM
	   VALUE=NUM
	ENDIF
C
C SET OR CLEAR SUPRESSION BITS
C
	BITMAP(1)=P(SUPRPT)
	BITMAP(2)=P(SUPRPT1)
	BITMAP(3)=P(SUPRPT2)
	IF(VALUE.EQ.0) THEN
	   CALL BCLR(BITMAP,BITIND)
	ELSE
	   CALL BSET(BITMAP,BITIND)
	ENDIF
C
C FILL IN MESSAGE BUFFER AND
C QUEUE COMMAND TO SYSTEM INPUT QUEUE
C
	BUF(1) = SUPRPT
	BUF(2) = BITMAP(1)   ! TCMOLD
	BUF(3) = TCPAR
	BUF(6) = IDNUM
	BUF(9) = BITMAP(2)   ! TCMDT2
	BUF(11)= BITMAP(3)   ! TCMDT4
C
	CALL VISCMD(BUF,ST)
	CALL XWAIT(2,1,ST)
C
C ENCODE SNAPSHOT
C
100	CONTINUE
	WRITE(CLIN1,1001)
	WRITE(CLIN2,1002)
	IND=1
	DO 200 I=1,MAXGAM
	   IF(GNTTAB(GAMIDX,I).EQ.0) GOTO 200
	   BITVAL=0
	   IF(TSBIT(P(SUPRPT),I)) BITVAL=1
	   WRITE(GAMSTAT(  IND),1005) I,(GLNAMES(S,I),S=1,4),
     *	                           STATUS(BITVAL+1)
	   G=IND
	   IND=IND+1
200	CONTINUE
C
C TEST AND ENCODE REPORT PART OF P(SUPRPT)
C
	IND=1
	DO 300 T=1,MAXREP
	   BITSTS(T)=0
	   IF(TSBIT(P(SUPRPT),MAXGAM+T)) BITSTS(T)=1
	   WRITE(REPTYPE(  IND),2003) K(T),TEXT(T),STATUS(BITSTS(T)+1)
	   IND=IND+1
300	CONTINUE
C
	IND=3
	REPIND = 0
	DO 400 I=FROM,UPTO
	   IF(I.LE.MIN(MAXGAM,MAXREP)) THEN
	     REPIND = REPIND + 1 
	     WRITE(XNEW(  IND),3001) GAMSTAT(I),REPTYPE(REPIND)
	   ELSE IF(I.GT.MAXGAM) THEN
	     REPIND = REPIND + 1
	     WRITE(XNEW(  IND),3002) REPTYPE(REPIND)
	   ELSE
	     WRITE(XNEW(  IND),3003) GAMSTAT(I)
           ENDIF
	   IF (IND .GE. 22) GOTO 401
	   IND=IND+1
400	CONTINUE

401	CONTINUE

	GAM=0
	RETURN
C
C FORMAT STATEMENTS
C
1001	FORMAT('Terminal Report Supression ')
1002	FORMAT(' Num  Game Name         Status',5X,'Comm.',6X,'Type',
     *	         21X,'Status')
1005	FORMAT(' *',I2.2,'  ',4(A4),'  ',A8)
1023	FORMAT(' Sorry, game ',I2,' is not active')
2003	FORMAT('*',A8,A28,A8)
2023	FORMAT(' Input Error ')
3001	FORMAT(A34,A46)
3002	FORMAT(T34,A46)
3003	FORMAT(A34,46(' '))
	END
