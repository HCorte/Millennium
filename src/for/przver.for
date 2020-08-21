C
C SUBROUTINE PRZVER
C $Log:   GXAFXT:[GOLS]PRZVER.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:33:40   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:23:30   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - przver.for **
C
C PRZVER.FOR
C
c V02 08-MAR-91 JPJ INITIAL RELEASE FOR MARYLAND
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO PROCESS LOTTERY ENTRY OF LOTTO RESULTS.
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
	SUBROUTINE PRZVER(GNUM,GIND,DRAW,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INTEGER*4  FLAG, K, EXT, NUM, I, ST, DRAW, GIND, GNUM
	CHARACTER*28 WINBUF(LTGDIV)
	CHARACTER*28 ANNBUF(1)
	CHARACTER*34 BONBUF(LTGDIV)
	CHARACTER*22 CNT_WINBUF(LTGDIV)
	CHARACTER*28 CNT_BONBUF(LTGDIV)
	INTEGER*4 BDROFF, CNT, ANNNUM
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
C
C
	WRITE(5,901) IAM(),GTNAMES(TLTO),GIND,DRAW
	DO 125 BDROFF=1,LSTBDR
100	   CONTINUE
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
	      TSRHLD(I,BDROFF)=CNT
	      LSVHLD(I,BDROFF)=NUM
	      IF(I.EQ.1) LANHLD(I,BDROFF)=ANNNUM
110	   CONTINUE
C
C
C
	   DO 120 K =1,LSTDIV
	      IF(BDROFF.EQ.1) THEN
	        WRITE(5,905) IAM(),K,TSRHLD(K,BDROFF)
	        WRITE(5,902) IAM(),K,CMONY(LSVHLD(K,BDROFF),12,VALUNIT)
	        IF(K.EQ.1)
     *          WRITE(5,907) IAM(),K,CMONY(LANHLD(K,BDROFF),12,VALUNIT)
	      ELSE
	        WRITE(5,906) IAM(),K,TSRHLD(K,BDROFF)
		WRITE(5,903) IAM(),K,CMONY(LSVHLD(K,BDROFF),12,VALUNIT)
	      ENDIF
120	   CONTINUE
	   CALL WIMG(5,'Are the values entered correct (Y/N) ')
	   CALL YESNO(FLAG)
	   IF(FLAG.NE.1) GOTO 100
C
C CHECK AGAINST OPERATOR ENTRY
C
	   DO 130 I=1,LSTDIV
	      IF(DLTLSV(I,BDROFF).NE.LSVHLD(I,BDROFF).OR.
     *		 DLTTSR(I,BDROFF).NE.TSRHLD(I,BDROFF).OR.
     *           (DLTLAN(I,BDROFF).NE.LANHLD(I,BDROFF).AND.
     *            I.EQ.1.AND.BDROFF.EQ.1)) THEN
	         TYPE*,IAM(),'Verification error, please re-enter'
	         OPDONE=0
	         DLTLST=GAMENV
	         ST=-1
	         RETURN
	      ENDIF
130	   CONTINUE
125	CONTINUE
C
C
	ST=0
	DLTLST=GFINAL
	RETURN
C
C
901	FORMAT(1X,A18,1X,A8,I1,' draw ',I4)
902     FORMAT(1X,A18,1X,'Tier ',I2,' Shares entered:       ',A12)
903     FORMAT(1X,A18,1X,'Tier ',I2,' Bonus shares entered: ',A12)
905	FORMAT(1X,A18,1X,'Tier ',I2,' Counts entered:       ',I12)
906	FORMAT(1X,A18,1X,'Tier ',I2,' Bonus counts entered: ',I12)
907     FORMAT(1X,A18,1X,'Tier ',I2,' Annuity share entered:',A12)
	END
