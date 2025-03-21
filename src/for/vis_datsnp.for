C
C SUBROUTINE DATSNP
C
C DATSNP.FOR
C
C V12 31-JAN-2011 FRP Initialize next draws to display
C                     if current draw is initialized
C V11 01-DEC-2000 UXN TOTOGOLO ADDED.
C v10 13-OCT-1999 RXK World Tour added.
C V09 13-MAY-1999 UXN Super Triple added.
C V08 06-FEB-1996 RXK Super Double and Today's Couple added
C V07 01-NOV-1994 HXK Fixed bug with Bingo, such that it looks to memory 
C                     for current draw
C V06 31-OCT-1994 HXK Added Bingo
C V05 23-JUL-1993 SXH Added RAVi and SPEDEN, message to say not supported 
C                     for ODDSET games
C V04 11-JUN-1993 HXK ADDED AGTINF.DEF, PRMAGT.DEF
C V03 16-FEB-1993 EBD Incorporated logic to check the GAM index bounds for 
C                     GNTTAB
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 01-AUG-1990 XXX RELEASED FOR VAX
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE DATSNP(DRAW,GAM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:TGLCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:NBRCOM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
	INCLUDE 'INCLIB:DLTREC.DEF'
	INCLUDE 'INCLIB:DSPREC.DEF'
	INCLUDE 'INCLIB:DTGREC.DEF'
        INCLUDE 'INCLIB:DPAREC.DEF'
	INCLUDE 'INCLIB:DKKREC.DEF'
	INCLUDE 'INCLIB:DNBREC.DEF'
        INCLUDE 'INCLIB:DBNREC.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'

        ! arguments
        INTEGER*4  DRAW             !
        INTEGER*4  GAM              !

        ! variables
	INTEGER*4  DATES(17)        !
	INTEGER*4  FDB(7)           !
	INTEGER*4  GBUF(1100)       !
	INTEGER*4  GAMDAT(DATLEN)   !
	INTEGER*4  GAMADV(7)        !
	INTEGER*4  DNUM             !
	INTEGER*4  I                !
	INTEGER*4  K                !
	INTEGER*4  ST               !
	INTEGER*4  GNUM             !
        INTEGER*4  GIND             !
	INTEGER*4  GTYP             !
                                    
	INTEGER*2  D(LDATE_LEN)     !

	CHARACTER*9  GAMNAM         !

	EQUIVALENCE (DLTREC,DSPREC,DTGREC,DKKREC,DNBREC,DBNREC,GBUF)
	EQUIVALENCE (GBUF(9),GAMDAT)
	EQUIVALENCE (GBUF(29),GAMADV)
C
C
	SMODE=.TRUE.
	IF (GAM .GE. 1 .AND. GAM .LE. MAXGAM) THEN
	    GTYP=GNTTAB(GAMTYP,GAM)
	    GIND=GNTTAB(GAMIDX,GAM)
	ELSE
	    WRITE(CLIN23,3000) GAM
	    RETURN
	ENDIF
	  
	IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP) THEN
	    WRITE(CLIN23,3000) GAM
	    RETURN
	ENDIF

        IF (GTYP .EQ. TSCR .OR. GTYP .EQ. TWIT .OR. GTYP .EQ. TTSL .OR.
     *      GTYP .EQ. TCPL .OR. GTYP. EQ. TDBL .OR.
     *      GTYP .EQ. TSSC .OR. GTYP. EQ. TTRP .OR. GTYP.EQ.TSTR) THEN
            WRITE(CLIN23,3050)(GLNAMES(K,GAM),K=1,4)
	    RETURN
	ENDIF
C
C
	GNUM=GTNTAB(GTYP,GIND)
	IF(GNUM.LT.1) THEN
	    WRITE(CLIN23,3010) GNUM
	    RETURN
	ENDIF

	WRITE (GAMNAM,3040) GTNAMES(GTYP),GIND
	IF(DRAW.LT.1) DRAW=DAYDRW(GNUM)
	IF(DRAW.EQ.0) DRAW=DAYHDR(GNUM)
        CALL FASTSET(0, GAMDAT, DATLEN)
C
C GET DATA FROM COMMON OR DISK
C
	IF(DRAW.EQ.DAYDRW(GNUM)) THEN
	  IF(GTYP.EQ.TLTO) CALL GAMLOG(TLTO,GIND,DLTREC,LTOSTS)
	  IF(GTYP.EQ.TSPT) CALL GAMLOG(TSPT,GIND,DSPREC,SPTSTS)
	  IF(GTYP.EQ.TTGL) CALL GAMLOG(TTGL,GIND,DTGREC,TGLSTS)
          IF(GTYP.EQ.TPAS) CALL GAMLOGPAS(DRAW,GIND,DPAREC,PASSTS)
	  IF(GTYP.EQ.TKIK) CALL GAMLOG(TKIK,GIND,DKKREC,KIKSTS)
	  IF(GTYP.EQ.TNBR) CALL GAMLOG(TNBR,GIND,DNBREC,NBRSTS)
          IF(GTYP.EQ.TBNG) CALL GAMLOG(TBNG,GIND,DBNREC,BNGSTS)
	  GOTO 100
	ENDIF
C
C
	CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
	IF(GTYP.EQ.TLTO) CALL IOINIT(FDB,1,DLTSEC*256)
	IF(GTYP.EQ.TSPT) CALL IOINIT(FDB,1,DSPSEC*256)
	IF(GTYP.EQ.TTGL) CALL IOINIT(FDB,1,DTGSEC*256)
        IF(GTYP.EQ.TPAS) CALL IOINIT(FDB,1,DPASEC*256)
	IF(GTYP.EQ.TKIK) CALL IOINIT(FDB,1,DKKSEC*256)
	IF(GTYP.EQ.TNBR) CALL IOINIT(FDB,1,DNBSEC*256)
        IF(GTYP.EQ.TBNG) CALL IOINIT(FDB,1,DBNSEC*256)
	IF(ST.NE.0) THEN
	    WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
	    CALL USRCLOS1(     1)
	    RETURN
	ENDIF
	IF(GTYP.EQ.TLTO) CALL READW(FDB,DRAW,DLTREC,ST)
	IF(GTYP.EQ.TSPT) CALL READW(FDB,DRAW,DSPREC,ST)
	IF(GTYP.EQ.TTGL) CALL READW(FDB,DRAW,DTGREC,ST)
        IF(GTYP.EQ.TPAS) CALL READW(FDB,DRAW,DPAREC,ST)
	IF(GTYP.EQ.TKIK) CALL READW(FDB,DRAW,DKKREC,ST)
	IF(GTYP.EQ.TNBR) CALL READW(FDB,DRAW,DNBREC,ST)
        IF(GTYP.EQ.TBNG) CALL READW(FDB,DRAW,DBNREC,ST)
	IF(ST.NE.0) THEN
	    WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
	    CALL USRCLOS1(     1)
	    RETURN
	ENDIF
	CALL USRCLOS1(     1)
C
C IF GAME TYPE PASSIVE, WE DON'T HAVE TABLE FOR FUTURE DATES
C
100	CONTINUE
        IF(GTYP .EQ. TPAS) GAMDAT(CURDRW) = DPAESD
C
C ENCODE DATE SNAPSHOT
C
        IF(GTYP.NE.TNBR) CALL FASTSET(0,GAMADV,7)
C
        DO I=1,16
            DATES(I+1)=GAMDAT(I)
            IF(DAYDRW(GNUM) .EQ. 0 .AND. I.GT.1)  !Current draw is initialized, then
     *        DATES(I+1)=0  !initialize next draws to display
        END DO
        DATES(1) = DAYCDC
C
	WRITE(CLIN1,901) GAMNAM
	WRITE(CLIN3,903)
	D(VCDC)=DATES(1)
	CALL LCDATE(D)
	WRITE(CLIN4,904) D(VCDC),D(VJUL),D(VMON),D(VDAY),D(VYEAR2),(D(K),K=7,13)
	CALL ENCDAT(NEW(1,5),DATES(2),2,DRAW,GAMADV)
	DO I = 3,17
	    DNUM=DRAW+I-2
	    CALL ENCDAT(NEW(1,I+4),DATES(I),I,DNUM,GAMADV)
        END DO

	RETURN
C
C
C
C FORMAT STATEMENTS
C
901	FORMAT(' ',A9,' GAME DATE SNAPSHOT')
903	FORMAT(25X,'Draw ',2X,'Cdc',3X,'Julian')
904	FORMAT('System  sales  date ',11X,2(I4,4X),I2,'/',I2.2,'/',I4,
     *	       1X,7A2)
3000	FORMAT('Sorry, game ',I2,' not active')
3010	FORMAT('Invalid game number ',I2)
3020	FORMAT(5A4,' open error ',I4)
3030	FORMAT(5A4,' read error ',I4,' record > ',I4)
3040	FORMAT(A8,I1)
3050    FORMAT('Sorry, VISION screen not available for ',4A4)

	END
