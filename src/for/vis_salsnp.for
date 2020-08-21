C
C SUBROUTINE SALSNP
C 
C V12 08-JUN-2000 UXN FTNAMES.DEF added.
C V11 13-OCT-1999 RXK World Tour added, display split into 2 pages.
C V10 26-MAY-1999 UXN Super Triple added.
C V09 04-FEB-1999 UXN Fix for big sales.
C V08 12-DEC-1995 PXB Added double and couple games
C V07 27-AUG-1995 PXB Reprocessing bug
C V06 30-JUN-1995 PXB Ravi V5 game changes.
C V05 26-OCT-1994 PXB Reformatted screen.
C V04 02-JUL-1993 SXH Don't display NUMBERS and KENO
C V03 14-JUN-1993 HXK added AGTINF.DEF, PRMAGT.DEF
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 01-AUG-1990 XXX RELEASED FOR VAX
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW

	SUBROUTINE SALSNP (DAT,PAGE)

	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:FTNAMES.DEF'

C---- Argument.

        INTEGER*4  DAT, PAGE

C---- Variables.

	INTEGER*4  SALEN
 	PARAMETER (SALEN=NUMTOT*(NUMFIN+1)*MAXTYP)

	INTEGER*4  RSALEN
 	PARAMETER (RSALEN=NUMTOT*(NUMFIN+1)*2)

        INTEGER*4  TLEN
	PARAMETER (TLEN=NUMTOT*(NUMFIN+1))

        INTEGER*4  UNIT
        INTEGER*4  LIN
        INTEGER*4  I
        INTEGER*4  GNUM
        INTEGER*4  GIND
        INTEGER*4  GTYP
        INTEGER*4  REC
        INTEGER*4  K
        INTEGER*4  ST
        INTEGER*4  OFF
	INTEGER*4  SALES(NUMTOT,NUMFIN+1,MAXTYP)
	INTEGER*4  TOTALS(NUMTOT,NUMFIN+1)
        INTEGER*4  FDB(7)

	INTEGER*2  D(LDATE_LEN)


C---------------- Start of program code ---------------------------------

	IF(DAT.LE.0) DAT=DAYCDC
	IF(PAGE.LT.1) PAGE=1

	D(VCDC)=DAT

	CALL LCDATE(D)

	IF(DAT.EQ.DAYCDC) THEN
	    SMODE=.FALSE.
	    CALL FASTMOV(DAYSTS,DAFSTS,DAFLEN)
	    GOTO 10
	ENDIF

C---- Read data from file.

	SMODE=.TRUE.

	CALL OPENW(1,SFNAMES(1,DAF),0,0,0,ST)

	IF(ST.NE.0) THEN
	    CALL USRCLOS1(1)
	    WRITE(CLIN23,8000) (SFNAMES(K,DAF),K=1,5),ST
	    RETURN
	ENDIF

	CALL IOINIT(FDB,1,DAFSEC*256)

	REC=DAT

	CALL READW(FDB,REC,DAFREC,ST)

	IF(ST.NE.0) THEN
	    CALL USRCLOS1(1)
	    WRITE(CLIN23,8001) (SFNAMES(K,DAF),K=1,5),ST,REC
	    RETURN
	ENDIF

	CALL USRCLOS1(1)

	IF(DAFSTS.EQ.DUNUSD) THEN
	    WRITE(CLIN23,8002) (D(K),K=7,13)
	    RETURN
	ENDIF

	IF(DAFSTS.EQ.DNOSAL) THEN
	    WRITE(CLIN23,8003) (D(K),K=7,13)
	    RETURN
	ENDIF

C---- Format sales snapshot.

10	CONTINUE

	CALL FASTSET(0,SALES,SALEN)

	CALL FASTSET(0,TOTALS,TLEN)

C---- Get total sales by game type.

	DO 30 GTYP=1,MAXTYP
	DO 30 GIND=1,MAXIND
	   GNUM=GTNTAB(GTYP,GIND)
	   IF(GNUM.LT.1) GOTO 30
	   DO 20 I=1,NUMFIN+1
	     IF(I.LE.NUMFIN) THEN
	       OFF=I
	       IF(OFF.EQ.TREF) OFF=TVAL
	       SALES(TRACNT,OFF,GTYP) = SALES(TRACNT,OFF,GTYP)+
     *	                                  DAFTYP(TRACNT,I,GNUM)
	       SALES(DOLAMT,OFF,GTYP) = SALES(DOLAMT,OFF,GTYP)+
     *	                                  DAFTYP(DOLAMT,I,GNUM)
	       TOTALS(TRACNT,I)       = TOTALS(TRACNT,I)+
     *	                                DAFTYP(TRACNT,I,GNUM)
	       TOTALS(DOLAMT,I)       = TOTALS(DOLAMT,I)+
     *	                                DAFTYP(DOLAMT,I,GNUM)
	     ELSE
	       SALES(TRACNT,I,GTYP) = SALES(TRACNT,I,GTYP)+
     *                                DAFDIS(TRACNT,GNUM)
	       SALES(DOLAMT,I,GTYP) = SALES(DOLAMT,I,GTYP)+
     *                                DAFDIS(DOLAMT,GNUM)
	       TOTALS(TRACNT,I)     = TOTALS(TRACNT,I)+
     *                                DAFDIS(TRACNT,GNUM)
	       TOTALS(DOLAMT,I)     = TOTALS(DOLAMT,I)+
     *                                DAFDIS(DOLAMT,GNUM)
	     ENDIF

20	   CONTINUE
30	CONTINUE

C---- Display Screen.

	   WRITE(CLIN1,9001) (D(K),K=7,13)
	   LIN = 3
	   WRITE(XNEW(  LIN),9002) 
	   LIN=LIN + 1
	   FTNAMES(TVAL)='csh/ref '

	   DO 100 GTYP=1,MAXTYP
	      IF(GTNTAB(GTYP,1).EQ.0) GOTO 100
	      WRITE(XNEW(  LIN),9003) GTNAMES(GTYP),
     *			        SALES(TRACNT,1,GTYP),
     *				CMONY(SALES(DOLAMT,1,GTYP),12,BETUNIT),
     *			        SALES(TRACNT,2,GTYP),
     *				CMONY(SALES(DOLAMT,2,GTYP),12,BETUNIT),
     *			        SALES(TRACNT,4,GTYP),
     *				CMONY(SALES(DOLAMT,4,GTYP),12,VALUNIT)

	       LIN=LIN+1
100	   CONTINUE

	   LIN=LIN+1

C---- TOTALS


	LIN = 18

	FTNAMES(TVAL)='cashes  '
	DO 120 I=1,NUMFIN,2
	   UNIT=BETUNIT
	   IF(I.EQ.TVAL) UNIT=VALUNIT
	   WRITE(XNEW(  LIN),9005) 'Total   ',FTNAMES(I),
     *	                           TOTALS(TRACNT,I),
     *	                           CMONY(TOTALS(DOLAMT,I),12,UNIT),
     *	                          'Total   ',FTNAMES(I+1),
     *	                           TOTALS(TRACNT,I+1),
     *	                           CMONY(TOTALS(DOLAMT,I+1),12,UNIT)
	   LIN=LIN+1
120	CONTINUE

	RETURN


C     ================== FORMAT STATEMENTS ================

C---- Message format statements.

8000	FORMAT (5A4,' file open error ',I4)

8001	FORMAT (5A4,' file read error ',I4,' record - ',I4)

8002	FORMAT ('Record not initialized for ',7A2)

8003	FORMAT (7A2,' is not a sales date')

C---- Screen format statements.

9001	FORMAT ('System Sales for ',7A2)

9002	FORMAT ('--GAME--',1X,8('-'),'SALES',8('-'),1X,7('-'),'CANCELS',
     *	        7('-'),1X,7('-'),'CSH/REF',7('-'))

9003	FORMAT (A8,1X,I8,1X,A12,1X,I8,1X,A12,1X,I8,1X,A12)

9004	FORMAT (80(' '))

9005	FORMAT (2(2A8,1X,I8,1X,A12,1X))

C9003	FORMAT (80(' '))

	END
