C SUBROUTINE RWTSNP
C  
C V17 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting,
C                     Instant Pass Thru Phase 1
C V16 05-FEB-1994 HXK MOVED DESCRIPTION STUFF AGAIN.
C V15 05-FEB-1994 HXK COPY EVENT DESCRIPTION AT LATER POINT IN ROUTINE.
C V14 05-FEB-1994 HXK RECALCULATE WHEN GAME INDEX CHANGES.
C V13 03-FEB-1994 HXK FIX FOR SCREEN APPEARANCE.
C V12 03-FEB-1994 HXK Fix for minor cosmetic problems.
C V11 02-FEB-1994 HXK FURTHER CHANGES IN FORMATS.
C V10 02-FEB-1994 HXK REORGANISE EVENT DESCRIPTION.
C V09 02-FEB-1994 HXK FIX FORMAT STATEMENT.
C V08 02-FEB-1994 HXK MINOR BUG FIXES FOR ODDSET INSTALLATION
C V07 01-FEB-1994 HXK FIXED BUGS.
C V06 11-JAN-1994 JXP Simple text changes
C V05 13-JUN-1993 HXK added AGTINF.DEF, PRMAGT.DEF
C V04 21-JAN-1993 DAB Initial Release Based on Netherlands Bible, 12/92,
C                     and Comm 1/93 update DEC Baseline
C VO3 01-JUL-1992 HDB  UPDATING FIXED, ADDED COMMENTS, FIXED TOTAL CALCULATION
C V02 21-OCT-1991 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C VIS_RWTSNP.FOR
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT

	SUBROUTINE RWTSNP(NUM,GIND)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:WITCOM.DEF'
	INCLUDE 'INCLIB:DWIREC.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'

	INTEGER*4 NUM		    !EVENT NUMBER
	INTEGER*4 GIND		    !GAME INDEX

	INTEGER*4 FDB(7)	    !FILE DISCRIPTOR BLOCK
	INTEGER*2 CBUF(LDATE_LEN)   !DATE BUFFER
	INTEGER*2 DBUF2(LDATE_LEN)  !DATE BUFFER
	INTEGER*2 TIME		    !TIME
	INTEGER*4 NETPOL	    !70% OF SALES + ROLL POT + ROUND POT
	INTEGER*4 GNUM		    !GAME NUMBER
	INTEGER*4 LNS		    !LINE COUNTER FOR FORMATTING
	INTEGER*4 ST		    !RESULT CODE AFTER FILE CALLS
	INTEGER*4 DRAW		    !REQUESTED DRAW
	INTEGER*4 TOTPOL	    !TOTAL POOL
	INTEGER*4 RNDPOL	    !ROUNDED POOL
	INTEGER*4 TAX		    !TAX AMOUNT
	INTEGER*4 IODDS		    !(INTEGER) ROW ODDS
	INTEGER*4 GSTATUS	    !GAME STATUS (SEE POLSTS)
	INTEGER*4 I,J,K,R,W	    !COUNTERS
	INTEGER*4 SALTAX/0/	    !SALES TAX
	INTEGER*4 DIS_DRW	    !LAST REQUESTED DRAW (EVENT)
        INTEGER*4 DIS_GIND          !LAST REQUESTED INDEX (GAME)
	INTEGER*4 TOPKRN(36)	    !AMOUNT FOR ROW
	INTEGER*4 TOPODD(36)	    !ODDS FOR ROW

        CHARACTER C_DWIDES(WDES_LEN)
        INTEGER*4 I4DWIDES(WDES_LEN/4)

	REAL*8    TOTAL		    !TOTAL AMOUNT IN POOL (70 %)
	REAL*8	  RODDS		    !ROW ODDS
	CHARACTER POLSTS(3)*13	    !POOL STATUS DISCRIPTION

        EQUIVALENCE(I4DWIDES,C_DWIDES)

	DATA      POLSTS/'CHANGING ODDS',' FINAL ODDS  ','   RESULTS   '/

	INTEGER*4   WEEK,YEAR
C
C	CLEAR ARRAYS
C
	CALL FASTSET(0,TOPKRN,36)
	CALL FASTSET(0,TOPODD,36)

	DRAW   = NUM
	IF(GIND.LT.1.OR.GIND.GT.MAXIND) THEN
	  WRITE (CLIN23,3000)
	  RETURN
	ENDIF

	GNUM=GTNTAB(TWIT,GIND)
	IF(GNUM.LT.1) THEN
	  WRITE (CLIN23,3010) GIND
	  RETURN
	ENDIF

	IF(DRAW.LT.1) DRAW=DAYDRW(GNUM)
	IF(DRAW.EQ.0) DRAW=DAYHDR(GNUM)
C
C GET DATA FROM COMMON OR DISK
C
	IF(DRAW.EQ.DAYDRW(GNUM)) THEN
	  CALL GAMLOG(TWIT,GIND,DWIREC,WITSTS)
	  GOTO 100
	ENDIF

	SMODE=.TRUE.
	CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,1,DWISEC*256)
	IF(ST.NE.0) THEN
	    WRITE (CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
	    CALL USRCLOS1(1)
	    RETURN
	ENDIF

	CALL READW(FDB,DRAW,DWIREC,ST)
	IF(ST.NE.0) THEN
	    WRITE (CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
	    CALL USRCLOS1(1)
	    RETURN
	ENDIF
	CALL USRCLOS1(1)

	IF(DRAW.EQ.DIS_DRW.AND.GIND.EQ.DIS_GIND.AND.
     *     DWISTS.EQ.GFINAL) GOTO 350

	IF(DWISTS.EQ.GAMNUL) THEN
	  WRITE (CLIN23,3040) GIND,DRAW
	  RETURN
	ENDIF

	DIS_DRW  = DRAW
        DIS_GIND = GIND
	TOTAL   = 0

	DO I=1,MAXWRW
	   TOPKRN(I)=DWISBR(I)
	   IF(DWISTA(I).NE.GAMCAN) THEN
	      TOTAL=TOTAL+TOPKRN(I)
	   ENDIF
   	ENDDO

	TOTAL=TOTAL*.7
	IF(DWISTS.GE.GFINAL) GOTO 310
	DO 275 J=1,MAXWRW
	   IF(TOPKRN(J).EQ.0) THEN
	      RODDS=99999
	   ELSE
	      RODDS=TOTAL/REAL(TOPKRN(J))*100
	   ENDIF
	   IODDS=NINT(RODDS)
	   IF(IODDS.LT.100)IODDS=100
C***	   IF(IODDS.GT.100.AND.IODDS.LT.150)IODDS=(IODDS/5)*5
C***	   IF(IODDS.GT.150.AND.IODDS.LT.1000)IODDS=(IODDS/10)*10
C***	   IF(IODDS.GE.1000.AND.IODDS.LE.99999)IODDS=(IODDS/100)*100
	   IF(IODDS.GT.99999)IODDS=99999
	   IF(DWISTA(J).EQ.GAMCAN) THEN
	      IODDS=99999
	   ENDIF
	   TOPODD(J)=IODDS
275	CONTINUE
	GOTO 310
C
C     GET DATA FROM MEMORY AND LOAD INTO FILE VARIABLES
C
100	CONTINUE
	IF(DWISTS.EQ.GAMNUL) THEN
	   WRITE (CLIN23,3040) GIND,DRAW
	   RETURN
	ENDIF
	TOTAL = 0
	DO 280 I=1,MAXWRW
	   TOPODD(I)=WTPOOL(I,1,2,GIND)
	   TOPKRN(I)=WTPOOL(I,2,2,GIND)
	   IF(DWISTA(I).NE.GAMCAN) THEN
	      TOTAL=TOTAL+TOPKRN(I)
	   ELSE
	      TOPODD(I)=99999
	   ENDIF
280	CONTINUE
310	CONTINUE
C
C CALCULATE TAXES AND NET POOL
C
	TOTPOL=IDNINT(TOTAL)
	TAX=IDNINT(DFLOAT(TOTPOL)*CALPER(SALTAX))
	RNDPOL=DWIBRK(1)
	NETPOL=(TOTPOL-TAX)+RNDPOL
C
C ENCODE DATA FOR FORMAT LINE
C
350	CONTINUE
	IF(DWISTS.EQ.GAMNUL) THEN
	   WRITE (CLIN23,3040) GIND,DRAW
	   RETURN
	ENDIF
	CBUF(VCDC) = DWIESD
	CALL LCDATE(CBUF)
        DBUF2(5) = DWIDAT
        CALL LCDATE(DBUF2)
C
	GSTATUS=1
C
C MOVE EVENT DESCRIPTION
C
        DO I=1,WDES_LEN/4
           I4DWIDES(I)=DWIDES(I)
        ENDDO
C
C CALCULATE CLOSING TIME IN HOURS AND MINUTES
C
	TIME   = DWICTM
	IF(DWISTS.GE.GAMBFD) GSTATUS=3
	IF(DWISTS.GE.GFINAL) GSTATUS=2
	CALL FIGWEK(DWIESD - WEEK_OFFSET, WEEK, YEAR) 
	WRITE (CLIN1,901)  GIND
	WRITE (CLIN3,902)  WEEK
	WRITE (CLIN4,907)  DRAW,(DBUF2(I),I=7,13)
	WRITE (CLIN5,903)  (CBUF(I),I=7,13),DISTIM(TIME),
     *	                  POLSTS(GSTATUS),GTNAMES(TWIT),GIND
        WRITE (CLIN6,1000)
	WRITE (CLIN6,904) (DWIENM(I),I=1,4),
     *	                  (C_DWIDES(I),I= 1,30)
        WRITE (CLIN7,1000)
        WRITE (CLIN7,9041)(C_DWIDES(I),I=31,60)
        WRITE (CLIN8,1000)
        WRITE (CLIN8,9041)(C_DWIDES(I),I=61,90)
	LNS=10
	IF(DWISTS.GE.GFINAL)GOTO 600
	WRITE (CLIN9,905)
	DO 400 R=1,12
	   WRITE (XNEW(LNS),906) R,(DWINMS(I,R),I=1,3),TOPODD(R)/100,
     *	                          MOD(TOPODD(R),100),R+12,
     *	                         (DWINMS(I,(R+12)),I=1,3),
     *	                          TOPODD(R+12)/100,
     *	                          MOD(TOPODD(R+12),100),R+24,
     *	                         (DWINMS(I,(R+24)),I=1,3),
     *	                          TOPODD(R+24)/100,
     *	                          MOD(TOPODD(R+24),100)
	   LNS=LNS+1
400	CONTINUE
	GOTO 700
600	CONTINUE
	WRITE (CLIN9,1000)
	DO 610 W=1,4
	   IF(DWIWIN(W).NE.0) THEN
	      WRITE (XNEW(LNS),1004)   DWIWIN(W),
     *                                (DWINMS(I,DWIWIN(W)),I=1,4),
     *	                               DWIODS(W)/100,MOD(DWIODS(W),100)
	   ENDIF
	   IF(DWIWIN(W).EQ.0) WRITE (XNEW(LNS),1000)
	   LNS=LNS+1
610	CONTINUE
	WRITE (CLIN14,1000)
 	WRITE (CLIN15,1005) CMONY(TOTPOL,12,BETUNIT)
	WRITE (CLIN16,10051) CMONY(DWISAL,12,BETUNIT)
 	WRITE (CLIN17,1006) DWIWPR(1,PRWON)        
	WRITE (CLIN18,1000)
	WRITE (CLIN19,1000)
	WRITE (CLIN20,1000)
	WRITE (CLIN21,1000)
	WRITE (CLIN22,1000)
700	CONTINUE

C
C      FORMAT STATEMENTS
C
901	FORMAT('Win Tip',1X,I1,' Oy Veikkaus Ab')
902	FORMAT(' RESULT SERVICE    WEEK: ',I2.2,
     *	 '     SIGNATURE:_______________________')
903	FORMAT(' CLOSING TIME:  ',7A2,'  HR: ',A8,
     *	        '  ',A13,' - ',A8,1X,I1)
904	FORMAT(' EVENT:     ',4A4,4(' '),'( ',30A1,' )',9X)
9041    FORMAT('            ',20X,'( ',30A1,' )',14X)  
905	FORMAT(' NR',1(' '),'NAME',8(' '),'ODDS',9(' '),
     *	       ' NR',1(' '),'NAME',8(' '),'ODDS',9(' '),
     *	       ' NR',1(' '),'NAME',8(' '),'ODDS',2(' '))
906	FORMAT(' ',I2.2,' ',2A4,A1,' ',I3.1,'.',I2.2,9(' '),
     *	       ' ',I2.2,' ',2A4,A1,' ',I3.1,'.',I2.2,9(' '),
     *	       ' ',I2.2,' ',2A4,A1,' ',I3.1,'.',I2.2,2(' '))
907	FORMAT(' EVENT: ',I4,4X,'Draw ',7A2)
1000	FORMAT(80(' '))
1004	FORMAT(' WINNER:    ',I2,'. ',4A4,6(' '),'ODDS: ',I4.1,'.',I2.2,
     *	 29(' '))
1005	FORMAT(' TOTAL WINNINGS:   ',A12,' EUROS  ',41(' '))
10051   FORMAT(' TOTAL SALES:      ',A12,' EUROS  ',41(' '))
1006	FORMAT(' NUMBER OF WINNERS:',I8,50(' '))
3000	FORMAT('Enter !Win Tip game index ')
3010	FORMAT('Win Tip ',I1,' game not active')
3020	FORMAT(5A4,' open error ',I4)
3030	FORMAT(5A4,' read error ',I4,' record > ',I4)
3040	FORMAT('Win Tip ',I1,' game not initialized event > ',I4)
C
	END
