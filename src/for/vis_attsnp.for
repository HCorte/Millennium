C SUBROUTINE ATTSNP
C  
C V08 17-MAY-1996 HXK Update from Wojtek, Siew Mun
C V07 07-DEC-1994 HXK Rearranged Agent number again
C V06 05-DEC-1994 HXK Fixed display of agent number
C V05 11-JUN-1993 HXK rearranged AGTINF.DEF, AGTCOM.DEF
C V04 18-FEB-1993 RXD Put boundcheck on TSPATL to prevent subscript overflow 
C                     traps 
C V03 21-JAN-1993 DAB Initial Release Based on Netherlands Bible, 12/92,
C                     and Comm 1/93 update DEC Baseline
C V02 17-JUL-1992 XXX RELEASED FOR VAX
C V01 04-AUG-1991 JAN  INITIAL RELEASE FOR SWEDEN
C
C VIS_ATTSNP.FOR    AGENT TOP 5 ON TOTO-SELECT SNAPSHOT
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
	SUBROUTINE ATTSNP(TER,LTER,LKEY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:TSLCOM.DEF'
C
	INTEGER*4    ATLTOP	    !MAX NO. OF ROWS
	PARAMETER    (ATLTOP=30)
	INTEGER*4    ATT	    !TOP COMBINATIONS
	PARAMETER    (ATT=5)
	INTEGER*4    TER,ANUM,GNUM,LTER,LKEY,ST,FDB
	INTEGER*4    S,LIN,CMB,OFFSET
	INTEGER*4    NUMROW,AMOUNT,TIME,I,J
	CHARACTER*1  CZERO
	DATA CZERO/Z0/
	CHARACTER*29 BETLIN
	EXTERNAL     BETLIN
	CHARACTER*12  WARNING
	INTEGER*4    START_TIME
	INTEGER*4    VALUES(ATLTOP) , SORT(ATLTOP)
	DIMENSION    FDB(7)
C
C  CHECK TERMINAL NUMBER
C
	SMODE=.FALSE.
	CALL ICLOCK(2,START_TIME)
	IF(TER.LT.1.OR.TER.GT.NUMAGT) TER=1
	ANUM=AGTTAB(AGTNUM,TER)
	LSTAGT=AGTTAB(AGTNUM,TER)
	GNUM=GTNTAB(TTSL,1)
C
C CHECK IF GAME IS ACTIVE IN SYSTEM 
C
        IF(GNUM .LE. 0 .OR. GNUM .GT. MAXGAM) THEN
           WRITE(CLIN23, 3010)
           RETURN
        ENDIF
	IF (LTER.EQ.TER.AND.LKEY.EQ.14) GOTO 100
C
C READ AGENTS RECORD
C
	CALL OPENW(1,SFNAMES(1,ASF),0,0,0,ST)
	CALL IOINIT(FDB,1,ASFSEC*256)
	IF(ST.NE.0) THEN
	  CALL USRCLOS1(     1)
	  WRITE(CLIN23,1500) (SFNAMES(J,ASF),J=1,5),ST
	  RETURN
	ENDIF
	CALL READW(FDB,TER,ASFREC,ST)
	IF(ST.NE.0) THEN
	  CALL USRCLOS1(     1)
	  WRITE(CLIN23,1501) (SFNAMES(J,ASF),J=1,5),ST,TER
	  RETURN
	ENDIF
	CALL USRCLOS1(     1)
	DO 110 I=1,512
	IF(ASFBYT(I).EQ.CZERO) ASFBYT(I)=' '
110	CONTINUE
C
C    ENCODE THE HEADER OF THE ATL SNAPSHOT
C
100	CONTINUE
	WRITE(CLIN1,901)
	WRITE(CLIN2,902) (ASFBYT(S),S=SNAME,ENAME),A
     *                    NUM,
     *	                  TER
	WRITE(CLIN3,903) (ASFBYT(S),S=SSTRT,ESTRT)
	WRITE(CLIN4,904) (ASFBYT(S),S=SZIPC,EZIPC),
     *	                 (ASFBYT(S),S=SCITY,ECITY)
	WRITE(CLIN5,905) (ASFBYT(S),S=STELE,ETELE),
     *	                  AGTTAB(ALSWAG,TER)
	WRITE(CLIN6,900)
      	WRITE(CLIN7,907) CMONY((AGTGAM(GSAMT,GNUM,TER)
     *			  - AGTGAM(GCAMT,GNUM,TER)),8,BETUNIT)
	WRITE(CLIN8,900)
	WRITE(CLIN9,909) 'Most played combinations in TotoSelect today '
C
C ENCODE THE TOP 5 COMBINATIONS
C
	WRITE(CLIN11,911) '     Combination                   ',
     *	                  'Amount         Time'
	WRITE(CLIN12,911) '-----------------------------      ',
     *	                  '------         ----'

C GET VALUES AND SORT THEM
C
	DO 10 I=1,ATLTOP
 	   IF ( I .LE. ATSTOP ) THEN
	      VALUES(I)=TSPATL(ATSAMT,I,TER,1)/16
	   ENDIF
10	CONTINUE
	CALL LNGSRT2(VALUES,SORT)
	LIN = 12
	DO 5 I=1,ATT
	   CMB=SORT(I)
	   LIN=LIN+1
	   OFFSET=TSPATL(ATSOFF,CMB,TER,1)
	   NUMROW=IAND(TSPATL(ATSAMT,CMB,TER,1),15)
	   IF (OFFSET.EQ.0.OR.NUMROW.LT.1.OR.NUMROW.GT.6) THEN
	      WRITE(XNEW(  LIN),1000)
	      GOTO 5
	   ENDIF
	   AMOUNT=(TSPATL(ATSAMT,CMB,TER,1)/16)*TSLPRC(1)
	   TIME  = START_TIME-TSPATL(ATSTIM,CMB,TER,1)
	   IF(TIME.LT.0) TIME = 24*60*60
	   WARNING = '            '
	   IF (AMOUNT.GE.P(TSLMAX).AND.TIME.LE.P(TSTLIM).OR.
     *	       AMOUNT.GE.P(TSLWRN)) WARNING = 'HIGH BETTING'
	   WRITE(XNEW(  LIN),910)BETLIN(OFFSET,NUMROW),
     *                           CMONY(AMOUNT,10,BETUNIT),
     *	                         DISTIM(TIME),WARNING
5	CONTINUE
CCCCC
C
C     THE FOLLOWING ARE ALL THE FORMAT STATEMENTS
C
900	FORMAT(1X,79(' '))
901	FORMAT('ATTSNP  -  AGENT TOP SELECT snapshot')
902	FORMAT('Name ',<LNAME>A1,T57,' Agt/Trm ',I7.7,'/',I4)
903	FORMAT('Addr',1X,<LSTRT>A1)
904	FORMAT(5X,<LZIPC>A1,1X,<LCITY>A1)
905	FORMAT('Telephone ',<LTELE>A1,15X,'Last Tra',4X,I9)
CCC907	FORMAT('Dagens f|rs{ljning p} L}ngen ',I8)
907	FORMAT('Daily sales for Toto Select  ',A8)
909	FORMAT(A)
910	FORMAT(2X,A29,A10,5X,A8,3X,A12)
911	FORMAT(A,A)
1000	FORMAT('   ---  Not Initialized  ---            ',40(' '))
C
C
1500	FORMAT(5A4,' open error - ',I4)
1501	FORMAT(5A4,' read error ',I4,' record ',I20)
9923	FORMAT('Input error ')
9924	FORMAT('Value error  ')
3010    FORMAT('Toto game not active')
	END
