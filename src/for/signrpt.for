C
C PROGRAM SIGNRPT
C $Log:   GXAFXT:[GOLS]SIGNRPT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:05:28   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:37:34   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - signrpt.for **
C
C SIGNRPT.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 20-JUN-88 DSL Released for Michigan
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
	PROGRAM SIGNRPT
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
C
C Define variables
C
	INTEGER*4 BUF(DBLOCK),TMNAME(4)
	INTEGER*4 DEVICE,CNT,EOF
	INTEGER*4 RTYP, REC, K, EXT, COPY, ST, J, I
	INTEGER*4 CDC
	INTEGER*4 DFDB(7),TRNINF(6,5000)
	DATA TMNAME/'XXXX',':MTM','F01.','FIL '/
	DATA CNT/1/,EOF/0/
C
C Functions for quick pointers into tmf buffer
C
	INTEGER*4 INDX,WRD12
	INDX(I) = I*12-3
	WRD12(I) = INDX(I) + 11
C
C Clear out signon transaction arrays
C
	DO 10 I=1,6
	  DO 15 J=1,5000
	    TRNINF(I,J)=0
15	  CONTINUE
10	CONTINUE
C
C Get date for report
C
	DEVICE='CON:'
	CALL OPENW(5,DEVICE,4,0,0,ST)
C
C Get number of report copies
C
	CALL INPNUM('Enter number of SIGNRPT report copies: ',
     *	            COPY,0,20,EXT)
	IF (EXT.LT.0) CALL GSTOP(GEXIT_SUCCESS)
C
C Get CDC date
C
C     CALL INPNUM('Enter CDC date of SIGNRPT report : ',
C    *            CDC,1,9999,EXT)
C     IF (EXT.LT.0) CALL GSTOP(GEXIT_SUCCESS)
C
C Open disk file
C
	TMNAME(1)=SFNAMES(1,PTMF)
	CALL OPENW(9,TMNAME,4,0,0,ST)
	CALL IOINIT(DFDB,9,32*256)
	IF (ST.NE.0) THEN
	   WRITE(5,923) (TMNAME(K),K=1,4),ST
	   CALL GPAUSE
	ENDIF
C
C Set first record to read  1
C
	REC=1
C
C Read next block
C
100	CONTINUE
	CALL READW(DFDB,REC,BUF,ST)
	IF (ST.NE.0) THEN
	   WRITE(5,906) ST,REC
	   CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
C Loop through block
C
	DO 1000 I=1,LBLK
	   RTYP=IAND(BUF(WRD12(I)),3)
	   IF (RTYP.GT.1) GOTO 1000
	   CALL LOGTRA(TRABUF,BUF(INDX(I)))
C
C Check for end of file
C
	   IF(TRABUF(TSTAT).EQ.NUSD) THEN
	      EOF=EOF+1
	      IF (EOF.GT.1500) GOTO 2000
	      GOTO 1000
	   ENDIF
	   EOF=0
C
C Check if transaction should be printed on report
C
	   IF(TRABUF(TTYP).NE.TCMD)  GOTO 1000
	   IF(TRABUF(TCMNUM).NE.204) GOTO 1000
	   IF(TRABUF(TCMDT1).LT.0.OR.TRABUF(TCMDT1).GT.4) GOTO 1000
C
C Fill in transaction data array for report
C
	   TRNINF(1,CNT)=TRABUF(TAGT)
	   TRNINF(2,CNT)=TRABUF(TSER)
	   TRNINF(3,CNT)=TRABUF(TTIM)
	   TRNINF(4,CNT)=TRABUF(TCMSRC)
	   TRNINF(5,CNT)=TRABUF(TCMDT1)
	   TRNINF(6,CNT)=TRABUF(TCMDT2)
	   CDC=TRABUF(TCDC)
C
C Increment table counter  and   make sure table does not overflow
C
	   CNT=CNT+1
	   IF(CNT.GT.5000) THEN
	     WRITE(5,9066)
 9066	      FORMAT(' Internal table overflow ')
	     CALL GSTOP(GEXIT_SUCCESS)
	   ENDIF
C
C Next record
C
1000	CONTINUE
C
C Next block
C
	REC=REC+1
	GOTO 100
C
C End of report
C
2000	CONTINUE
	CALL USRCLOS1(     9)
C
C Call print routine for transactions
C
	CALL PRTSON(TRNINF,CDC)
C
C Spool report file
C
	CALL SPOOL('SIGNRPT.REP',COPY,ST)
C
906	FORMAT(1X,'TMF   Read error - ',Z8,' record - ',I4)
923	FORMAT(1X,4A4,' open error - ',Z8)
	END
