C
C SUBROUTINE TITLE
C
C V08 24-NOV-2010 FJG Lotto2 Batch: Page number overflow
C V07 13-JUN-2000 UXN IDATE replaced with GDATE
C V06 11-Nov-1999 RXK Changed for ALPHA (UXN)
C V05 03-Jan-1994 HXK CHANGED DATE FORMAT FROM USA TO FINLAND.
C V04 12-Jul-1993 SXH Released for VEIKKAUS
C V03 21-Jan-1993 DAB Initial Release Based on Netherlands Bible, 12/92, and
C                     Comm 1/93 update DEC Baseline
C V02 12-NOV-91 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO PRINT A REPORT TITLE.
C CALLING SEQUENCE:
C     CALL TITLE('REPORT NAME','PGMNAM',REV,UNIT,PAGE,CDC)
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
	SUBROUTINE TITLE(REPNAM,PGMNAM,REV,UNIT,PAGE,CDC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 K, IND, SYS, I, OFF, LENGTH, TEMP, CDC
	INTEGER*4 PAGE, UNIT, REV, WEEK
C
	CHARACTER REPNAM*(*)
	CHARACTER*100 BTITLE
	CHARACTER LOWA,LOWZ,CTEMP(4),SNAME(6)
	CHARACTER PGMNAM*(*)
	INTEGER*2 VDAT(LDATE_LEN)
	CHARACTER FF
	CHARACTER STITLE(100),HEAD(112)
	INTEGER*4 CTIM(2),CDAT(8),YEAR
	EQUIVALENCE (BTITLE,STITLE)
	EQUIVALENCE (TEMP,CTEMP)
	DATA FF/Z31/   !0C
	DATA LOWA/'a'/
	DATA LOWZ/'z'/
	DATA SNAME/'?','A','B','C','D','E'/
C
	VDAT(VCDC)=CDC
	CALL LCDATE(VDAT)	
	CALL FIGWEK(CDC,WEEK,YEAR)
	PAGE=PAGE+1
	LENGTH=LEN(REPNAM)
	LENGTH=LENGTH*2
	OFF=132-LENGTH
	OFF=OFF/2+1
	BTITLE=REPNAM
C
	DO 10 I=1,112
	 HEAD(I)=' '
10	CONTINUE
C
	SYS=P(SYSNAM)+1
	IF(SYS.LT.1.OR.SYS.GT.6) SYS=1
	IND=1
	DO 20 I=OFF,OFF+LENGTH,2
	 IF(I.GT.112) GOTO 20
	 HEAD(I)=STITLE(IND)
	 IF(HEAD(I).GE.LOWA.AND.HEAD(I).LE.LOWZ) THEN
	   CTEMP(1)=HEAD(I)
	   TEMP=TEMP-32
	   HEAD(I)=CTEMP(1)
	 ENDIF
	 IND=IND+1
20	CONTINUE
C
	CALL ICLOCK(1,CTIM)
	CALL GDATE(CDAT(2),CDAT(3),CDAT(1))
        IF(CDAT(1).LT.77) THEN
	  CDAT(1) = CDAT(1) + 2000
	ELSE
	  CDAT(1) = CDAT(1) + 1900
	ENDIF
C
	WRITE(UNIT,900) FF,PGMNAM,REV,CTIM,PAGE
C
C REMOVED WEEK
C
C	WRITE(UNIT,901) CDAT(3),CDAT(2),CDAT(1),
C     *	                (HEAD(K),K=13,112),(VDAT(K),K=9,13),
C     *	                VDAT(VCDC),
C     *	                WEEK,SNAME(SYS)
	WRITE(UNIT,901) CDAT(3),CDAT(2),CDAT(1),
     *	                (HEAD(K),K=13,112),(VDAT(K),K=9,13),
     *	                VDAT(VCDC),
     *	                SNAME(SYS)
	RETURN
C
C FORMAT AREA
C
900	FORMAT(A1,1X,A,T11,'.',I2.2,18X,
     *  'S A N T A  C A S A  D A  M I S E R I C O R D I A  D E  L I S B O A',
     *	       16X,2A4,1X,'PAGE',I6)
C901	FORMAT(2X,I2.2,'.',I2.2,'.',I4.4,100A1,1X,5A2,1X,'CDC ',I4,/,
C     *	       2X,'WEEK ',I2,115X,'SYS',A5)     
901	FORMAT(2X,I2.2,'.',I2.2,'.',I4.4,100A1,1X,5A2,1X,'CDC ',I4,/,
     *	       2X,'     ',117X,'SYS',A5)
	END
