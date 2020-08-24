C
C SUBROUTINE PERSNP
C  
C V13 08-JUN-2000 UXN FTNAMES.DEF added.
C V12 13-OCT-1999 RXK Hack for V5 removed.
C V11 13-JAN-1997 RXK Loopback counter added, to get place for it cashes and
C                     refunds summed up.
C V10 26-FEB-1996 RXK Layout fixed 
C V09 25-OCT-1994 PXB Modified to include bingo game.
C V08 28-APR-1994 JXP Updated format specification
C V07 28-APR-1994 JXP Show lotto viking
C V06 06_JUL-1993 SXH Don't display NUMBERS and KENO (hence can use 1 screen)
C V05 13-JUN-1993 HXK added AGTINF.DEF, PRMAGT.DEF
C V04 21-JAN-1993 DAB Initial Release Based on Netherlands Bible,
C                     DEC Baseline
C V03 15-FEB-1991 WOL USES NEW CHARACTER CMONY ROUTINES
C V02 01-AUG-1990 XXX RELEASED FOR VAX
C V01 01-FEB-1989 XXX INITIAL RELEASE FOR SWEDEN
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
	SUBROUTINE PERSNP
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:FTNAMES.DEF'
C
	INTEGER*4  TLEN                              !
	PARAMETER (TLEN=NUMTOT*NUMFIN)
C
	INTEGER*4  LIN                               !
	INTEGER*4  K                                 !
	INTEGER*4  TOTVAL                            !
	INTEGER*4  TOTCAN                            !
	INTEGER*4  TOTWAG                            !
	INTEGER*4  I                                 !
	INTEGER*4  GNUM                              !
	INTEGER*4  GIND                              !
	INTEGER*4  GTYP                              !
	INTEGER*4  PTAB(MAXTYP)                      !
	INTEGER*4  TOTALS(NUMTOT,NUMFIN)             !
	INTEGER*4  SECSLEFT                          !
	INTEGER*4 S,E
	CHARACTER*80	TEMPLINE
	INTEGER*2  D(LDATE_LEN)                             !
	INTEGER*4  ACT_GTYP(MAXTYP)/MAXTYP*0/
C
C
	D(VCDC)=DAYCDC
	CALL LCDATE(D)
C
C FORMAT SALES SNAPSHOT
C
	CALL FASTSET(0,PTAB,MAXTYP)
	CALL FASTSET(0,TOTALS,TLEN)
C
C GET PERFORMANCE TOTALS BY GAME TYPE
C
	DO 30 GTYP=1,MAXTYP
	    DO 30 GIND=1,MAXIND
	        GNUM=GTNTAB(GTYP,GIND)
	        IF(GNUM.LT.1) GOTO 30
		ACT_GTYP(GTYP) = 1
	        PTAB(GTYP)=PTAB(GTYP)+PERFRM(3,GNUM)
	        DO I=1,NUMFIN
	            TOTALS(TRACNT,I) = TOTALS(TRACNT,I)+
     *	                               DAYTYP(TRACNT,I,GNUM)
	            TOTALS(DOLAMT,I) = TOTALS(DOLAMT,I)+
     *	                               DAYTYP(DOLAMT,I,GNUM)
                END DO
30	CONTINUE
C
C
	TOTWAG=0
	DO GTYP=1,MAXTYP
	    TOTWAG=TOTWAG+PTAB(GTYP)
        END DO
	TOTCAN = PERFRM(3,PERCAN)
	TOTVAL = PERFRM(3,PERVAL)
C
C
	WRITE(CLIN1,901) (D(K),K=7,13)
	LIN=4
	S=1
	E=39
	DO 105 GTYP = 1, MAXTYP
	    IF(ACT_GTYP(GTYP).EQ.0) GOTO 105
	    TEMPLINE = XNEW(LIN)
	    WRITE(TEMPLINE(S:E),903) GTNAMES(GTYP),PTAB(GTYP)
            XNEW(LIN) = TEMPLINE
	    LIN=LIN+1
105     CONTINUE
	LIN = 6
	S = 40
	E = 79
	TEMPLINE = XNEW(LIN)
	WRITE(TEMPLINE(S:E),904) 'wagers  ',TOTWAG
	XNEW(LIN) = TEMPLINE
	LIN=LIN+1
	TEMPLINE = XNEW(LIN)
	WRITE(TEMPLINE(S:E),904) 'cancels ',TOTCAN
	XNEW(LIN) = TEMPLINE
	LIN=LIN+1
	TEMPLINE = XNEW(LIN)
	WRITE(TEMPLINE(S:E),904) 'cashes  ',TOTVAL
	XNEW(LIN) = TEMPLINE
	LIN=LIN+1
	TEMPLINE = XNEW(LIN)
        WRITE(TEMPLINE(S:E),904) 'lpbacks ',PERFRM(3,PERLBK)
	XNEW(LIN) = TEMPLINE
	LIN=LIN+1
C
C FINANCIAL TOTALS
C
	DO I=1,NUMFIN
	    IF (I .eq. TRET) goto 111
	    IF (I .eq. TREF) goto 111
	    IF (I .eq. TVAL) THEN
	       TEMPLINE = XNEW(LIN)
 	       WRITE(TEMPLINE(S:E),905) 'cash/ref',
     *            TOTALS(TRACNT,I) + TOTALS(TRACNT,TREF),
     *	          CMONY((TOTALS(DOLAMT,I)+TOTALS(DOLAMT,TREF)),13,BETUNIT) 
	       XNEW(LIN) = TEMPLINE
            ELSE
	       TEMPLINE = XNEW(LIN)
 	       WRITE(TEMPLINE(S:E),905) FTNAMES(I),TOTALS(TRACNT,I),
     *	                           CMONY(TOTALS(DOLAMT,I),13,BETUNIT)
	       XNEW(LIN) = TEMPLINE
            ENDIF
	    IF (LIN .GE. 22) GOTO 112
	    LIN=LIN+1
111	    continue
        END DO
	TEMPLINE = XNEW(LIN)
        WRITE(TEMPLINE(S:E),905) 'lpbacks ',PERFRM(1,PERLBK)
        XNEW(LIN) = TEMPLINE
        LIN=LIN+1
112	CONTINUE

	LIN=LIN+1
	SECSLEFT = (487-P(TIMER_THRLOP))/8
	TEMPLINE = XNEW(LIN)
	WRITE(TEMPLINE(S:E),906) SECSLEFT
        XNEW(LIN) = TEMPLINE

	RETURN
C
C
901	FORMAT('System performance for ',7A2)
903	FORMAT(1X,A8,4X,'wagers/minute ',I9)
907	FORMAT(1X,3A4,'wagers/minute ',I9)
904	FORMAT(1X,'Total    ',A8,' / minute ',I9)
905	FORMAT(1X,'Total ',A8,I8,1X,A13)
906	FORMAT(1X,I4,' Seconds until next update')
	END
