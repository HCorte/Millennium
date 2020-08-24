C
C SUBROUTINE PRZENT
C $Log:   GXAFXT:[GOLS]PRZENT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:33:34   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:23:24   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - przent.for **
C
C PRZENT.FOR
C
C V01 13-MAR-91 JPJ INITIAL RELEASE FOR MARYLAND
C
C SUBROUTINE TO PROCESS OPERATOR ENTRY OF LOTTO RESULTS.
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
	SUBROUTINE PRZENT(GNUM,GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	CHARACTER*28 WINBUF(LTGDIV)
	CHARACTER*30 ANNBUF(1)
	CHARACTER*34 BONBUF(LTGDIV)
	CHARACTER*22 CNT_WINBUF(LTGDIV)
	CHARACTER*28 CNT_BONBUF(LTGDIV)
	INTEGER*4 FDB(7), FLAG, K, EXT, NUM, I, ST, DRAW, GIND, GNUM
	INTEGER*4 BDROFF, CBUF(CDLEN), CNT, ANNNUM
	DATA ANNBUF/'Enter tier 1 ANNUITY value'/
	DATA WINBUF/'Enter tier 1 share value',
     *	            'Enter tier 2 share value',
     *	            'Enter tier 3 share value',
     *	            'Enter tier 4 share value',
     *	            'Enter tier 5 share value',
     *	            'Enter tier 6 share value',
     *	            'Enter tier 7 share value',
     *	            'Enter tier 8 share value'/
	DATA BONBUF/'Enter tier 1 bonus share value',
     *	            'Enter tier 2 bonus share value',
     *	            'Enter tier 3 bonus share value',
     *	            'Enter tier 4 bonus share value',
     *	            'Enter tier 5 bonus share value',
     *	            'Enter tier 6 bonus share value',
     *	            'Enter tier 7 bonus share value',
     *	            'Enter tier 8 bonus share value'/
	DATA CNT_WINBUF/'Enter tier 1 count',
     *	                'Enter tier 2 count',
     *	                'Enter tier 3 count',
     *	                'Enter tier 4 count',
     *	                'Enter tier 5 count',
     *	                'Enter tier 6 count',
     *	                'Enter tier 7 count',
     *	                'Enter tier 8 count'/
	DATA CNT_BONBUF/'Enter tier 1 bonus count',
     *	                'Enter tier 2 bonus count',
     *	                'Enter tier 3 bonus count',
     *	                'Enter tier 4 bonus count',
     *	                'Enter tier 5 bonus count',
     *	                'Enter tier 6 bonus count',
     *	                'Enter tier 7 bonus count',
     *	                'Enter tier 8 bonus count'/
C
C CHECK DRAW NUMBER ENTERED IF NOT LAST DRAW TELL OPERATIONS
C
	IF(DRAW.NE.LTODRW(GIND)-1) THEN
	  WRITE(5,904) IAM(),GTNAMES(TLTO),GIND,DRAW,LTODRW(GIND)-1
	  RETURN
	ENDIF
	IF(DRAW.EQ.0) DRAW=1
C
C READ DRAW ENTERED FILE
C
        CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
        CALL IOINIT(FDB,3,DLTSEC*256)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
        CALL READW(FDB,DRAW,DLTREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
	LSTDIV=DLTDIV
	LSTBDR=1
	IF(DLTBDR.GT.0) THEN
   	  LSTBDR=LSTBDR+1
	ENDIF
	IF(DLTSTS.NE.GAMENV) THEN
	  WRITE(5,900) IAM(),GTNAMES(TLTO),GIND,DRAW,DLTSTS
	  CALL GPAUSE
	ENDIF
C
	CALL GAMLOG(TLTO,GIND,DLTREC,LTOBLK)
	WRITE(5,901) IAM(),GTNAMES(TLTO),GIND,DRAW
C
C
90      CONTINUE
	DO 125 BDROFF=1,LSTBDR
100        CONTINUE
	   DO 110 I=1,LSTDIV
	      IF(BDROFF.EQ.1) THEN
		CALL INPNUM(CNT_WINBUF(I),CNT,0,999999,EXT)
	        CALL INPMONY(WINBUF(I),NUM,VALUNIT,EXT)
	        IF(I.EQ.1) CALL INPMONY(ANNBUF(I),ANNNUM,VALUNIT,EXT)
	      ELSE
		CALL INPNUM(CNT_BONBUF(I),CNT,0,999999,EXT)
	        CALL INPMONY(BONBUF(I),NUM,VALUNIT,EXT)
	      ENDIF
	      IF(EXT.LT.0) GOTO 100
	      DLTTSR(I,BDROFF)=CNT
	      DLTLSV(I,BDROFF)=NUM
	      IF(I.EQ.1) DLTLAN(I,BDROFF)=ANNNUM
110	   CONTINUE
C
C
	   DO 120 K=1,LSTDIV
	      IF(BDROFF.EQ.1) THEN
	        WRITE(5,905) IAM(),K,DLTTSR(K,BDROFF)
	        WRITE(5,902) IAM(),K,CMONY(DLTLSV(K,BDROFF),12,VALUNIT)
	        IF(K.EQ.1) 
     *          WRITE(5,907) IAM(),K,CMONY(DLTLAN(K,BDROFF),12,VALUNIT)
	      ELSE
	        WRITE(5,906) IAM(),K,DLTTSR(K,BDROFF)
	        WRITE(5,903) IAM(),K,CMONY(DLTLSV(K,BDROFF),12,VALUNIT)
	      ENDIF
120	   CONTINUE
	   CALL WIMG(5,'Are the values entered correct [Y/N] ')
	   CALL YESNO(FLAG)
	   IF(FLAG.NE.1) GOTO 100
125     CONTINUE
	DLTLST=GAMDON
	OPDONE=1
C
C WAIT FOR VERIFICATION FROM REMOTE TERMINAL.
C
130	CONTINUE
	TYPE*,IAM(),' Waiting for verification from remote terminal'
	IF(DLTLST.EQ.GFINAL) THEN
          DO 155 BDROFF=1,LSTBDR
	     DO 140 I=1,LSTDIV
	        CALL FASTSET(0,CBUF,CDLEN)
	        CBUF(1)=5
	        CBUF(2)=DLTLSV(I,BDROFF)
	        CBUF(3)=TCLTO
	        CBUF(6)='SYS '
	        CBUF(8)=GIND
	        CBUF(9)=I
	        CBUF(10)=BDROFF
	        CALL RESCMD(CBUF)
140	     CONTINUE
155	  CONTINUE
C
C SET SHARE COUNE
C
          DO 165 BDROFF=1,LSTBDR
	     DO 170 I=1,LSTDIV
	        CALL FASTSET(0,CBUF,CDLEN)
	        CBUF(1)=6
	        CBUF(2)=DLTTSR(I,BDROFF)
	        CBUF(3)=TCLTO
	        CBUF(6)='SYS '
	        CBUF(8)=GIND
	        CBUF(9)=I
	        CBUF(10)=BDROFF
	        CALL RESCMD(CBUF)
170	     CONTINUE
165	  CONTINUE
C
C SET ANNUITY PRIZE VALUE
C
          DO 185 BDROFF=1,1
	     DO 180 I=1,1
	        CALL FASTSET(0,CBUF,CDLEN)
	        CBUF(1)=9
	        CBUF(2)=DLTLAN(I,BDROFF)
	        CBUF(3)=TCLTO
	        CBUF(6)='SYS '
	        CBUF(8)=GIND
	        CBUF(9)=I
	        CBUF(10)=BDROFF
	        CALL RESCMD(CBUF)
180	     CONTINUE
185	  CONTINUE
C
C
C
	  CALL FASTSET(0,CBUF,CDLEN)
	  CBUF(1)=8
	  CBUF(2)=GAMDON
	  CBUF(3)=TCLTO
	  CBUF(6)='SYS '
	  CBUF(8)=GIND
	  CALL RESCMD(CBUF)
	  RETURN
	ENDIF
	CALL XWAIT(5,2,ST)
	IF(DLTLST.EQ.GAMENV) THEN
	  TYPE*,IAM(),'Verification error, please re-enter '
	  GOTO 90
	ENDIF
	GOTO 130
C
C
C
900	FORMAT(1X,A18,1X,A8,I1,' event ',I4,' invalid game status> ',I4)
901	FORMAT(1X,A18,1X,A8,I1,' draw ',I4)
902	FORMAT(1X,A18,1X,'Tier ',I2,' Shares entered:       ',A12)
903	FORMAT(1X,A18,1X,'Tier ',I2,' Bonus shares entered: ',A12)
904     FORMAT(1X,A18,1X,A8,I1,' event ',I4,
     *         ' draw entered not last draw> ',I4)
905	FORMAT(1X,A18,1X,'Tier ',I2,' Counts entered:       ',I12)
906	FORMAT(1X,A18,1X,'Tier ',I2,' Bonus counts entered: ',I12)
907	FORMAT(1X,A18,1X,'Tier ',I2,' Annuity share entered:',A12)
	END
