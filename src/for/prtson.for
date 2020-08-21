C
C SUBROUTINE PRTSON
C $Log:   GXAFXT:[GOLS]PRTSON.FOV  $
C  
C     Rev 1.1   19 May 1996 17:46:00   HXK
C  Wojtek's security stuff added
C  
C     Rev 1.0   21 Jan 1993 17:23:12   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - signrpt.for **
C
C
C Subroutine to print signon transactions to signrpt.rep
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE PRTSON(TRNINF,CDC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECUSE.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

	INTEGER*4 TRNINF(6,5000),SIGNTIME
	INTEGER*4 YY, DD, MM, SEC, MIN, HOUR, CCHK, CSER, CJUL, REC
	INTEGER*4 LINCNT, I, ST
	INTEGER*2 DTES(LDATE_LEN)
	INTEGER*4 CDC
	CHARACTER *8 TME
	REAL *4 STAT(5)
	DATA LINCNT/55/
	DATA STAT/'DEFT','REJT','GOOD','CHGN','SOFF'/
C
C Open report file
C
	 CALL ROPEN('SIGNRPT.REP',6,ST)
C
C  open  USER file
C
	 CALL OPENX(2,'GXTSK:USER.FIL',4,0,0,ST)
	 CALL IOINIT(SONFDB,2,USESEC*256)
	 IF (ST.NE.0) THEN
	    WRITE(5,900) ST
 900	    FORMAT('  user open error   st -  ',I4)
	    CALL GSTOP(GEXIT_SUCCESS)
	 ENDIF
C
C Use ID numbers in trninf table to read user.fil with
C
	DO 5 I=1,5000
	  REC=TRNINF(1,I)
	  IF(REC.EQ.0) GOTO 5001
	  CALL READW(SONFDB,REC,USEREC,ST)
	  IF(ST.NE.0) THEN
	     WRITE(5,901) REC,ST
 901	     FORMAT(' user file read error  record= ',I4,' st= ',I4)
	     CALL GSTOP(GEXIT_SUCCESS)
	  ENDIF
C
C Convert serial number to external (scrambled)
C
	  DTES(5)=CDC
	  CALL LCDATE(DTES)
	  CJUL=DTES(VJUL)
	  CALL OUTGEN(CDC,TRNINF(2,I),CSER,CCHK)
C
C Convert seconds to hours minutes seconds
C
	  SIGNTIME=TRNINF(3,I)
	  HOUR=(SIGNTIME)/3600
	  MIN=(SIGNTIME-HOUR*3600)/60
	  SEC=SIGNTIME-(HOUR*3600)-(MIN*60)
	  WRITE (TME,969) HOUR,MIN,SEC
969	  FORMAT(I2.2,':',I2.2,':',I2.2)
C
C Convert cdc to month day year
C
	  MM=DTES(VMON)
	  DD=DTES(VDAY)
	  YY=DTES(VYEAR2)
C
C Print header to report
C
	LINCNT=LINCNT+1
	IF(LINCNT.GE.44) THEN
	  CALL PRTHDR(DTES)
	  LINCNT=0
	ENDIF
C
C Write detail line of signon transaction
C
	WRITE(6,9001) STAT(TRNINF(5,I)+1),CJUL,CSER,CCHK,TME,MM,DD,YY,
     *	             FUNCNAME(TRNINF(6,I)),TRNINF(4,I),USERID,
     *	             USERNAM,DATEADD(1),DATEADD(2),
     *	             DATEADD(3),DATECHA(1),DATECHA(2),DATECHA(3),
     *	             USERADD,USERCHA
C
 5	CONTINUE
C
 9001	 FORMAT(1X,A4,1X,I3.3,'-',I8.8,'-',I3.3,1X,A8,1X,I2,'/',I2,'/',
     *	       I4,2X,A8,2X,
     *	       A6,3X,I4,3X,A20,1X,I2,'/',I2,'/',I2,3X,I2,'/',I2,'/',I2,
     *	       7X,I4,7X,I4)
C
C Print transaction record to report file
C
 5001	CONTINUE
	CALL USRCLOS1(     2)
	CALL USRCLOS1(     6)
	RETURN
	END
