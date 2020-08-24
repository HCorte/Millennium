C 
C PROGRAM GAMTOT
C
C GAMTOT.FOR
C
C V27 14-JAN-2011 RXK Redundant parenthesis removed.
C V26 14-MAY-2010 RXK Passive added.
C V25 29-NOV-2000 UXN TTGL added.
C V24 08-JUN-2000 UXN RGTNAMES replaced with GTNAMES
C V23 21-DEC-1999 OXK Fixed output to avoid overlapping columns.
C V22 13-OCT-1999 RXK World Tour added.
C V21 14-MAY-1999 UXN Super Triple added.
C V20 15-DEC-1995 PXB Changes for double and couple games
C V19 30-JUN-1995 PXB Ravi V5 game changes.
C V18 15-DEC-1994 PXB Fixed bug. Pitka total now shows correct amount.
C V17 05-DEC-1994 HXK Merging from 25-Nov -> 5 Dec
C V16 29-NOV-1994 PXB Bug Fixes
C V15 24-OCT-1994 PXB Changed to have two sales tables and performance page 
C                     has been removed.
C V14 05-OCT-1994 HXK Test of PVCS itself, again.
C V13 04-OCT-1994 HXK No change, just testing PVCS tools
C V12 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V11 27-APR-1994 JXP COPY=0
C V10 26-OCT-1993 HXK seperate lotto games.
C V09 22-OCT-1993 HXN LOTTO AND VIKING BEING SEPARATED BY GAME.
C V08 16-SEP-1993 SXH Corrected CALL to BALWRI so that GAME info is sent, 
C                     not TYPE
C V07 16-SEP-1993 SXH Use divide by 1000 trick to avoid PTAB being treated as 
C                     negative when greater than 2 to the power of 31 
C                     (in VALUNITS)
C V06 14-SEP-1993 SXH Made output length in CMONY CALL 12 chars
C V05 10-SEP-1993 SXH Removed NUMBERS and KENO from display
C V04 04-JUN-1993 SXH Released for Finland
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 12-NOV-1991 MTK INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
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
	PROGRAM GAMTOT
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C

	INTEGER*4 FDB(7)              !
	INTEGER*4 STAB(MAXTYP+1,8)    !
        INTEGER*4 GAME_STAB(MAXGAM,8) ! 
	INTEGER*4 FIX_STAB(MAXTYP+1,8)! holds info divided by 100 to avoid
                                      ! overflow
	INTEGER*4 TABTOT1(8)	      !Table 1 totals
	INTEGER*4 TABTOT2(8)	      !Table 2 totals
	INTEGER*4 PAGE                !
	INTEGER*4 TOTSAL              !
	INTEGER*4 GAM                 !
	INTEGER*4 IND                 
	INTEGER*4 CDC                 !
	INTEGER*4 YEAR2		      ! Year in 4 digits
	INTEGER*4 WEEK                !
	INTEGER*4 GAMCNT              !
	INTEGER*4 SCDC                !
	INTEGER*4 I                   !
	INTEGER*4 COPY                !
	INTEGER*4 ST                  !
	INTEGER*4 J                   !
	INTEGER*4 GTYP                !

        INTEGER*4 RAPCODE             !
        INTEGER*4 GAMESUMS(MAXGAM,NUMFIN,NUMTOT)
        INTEGER*4 TOTSUMS(NO_BALSUMS)

	INTEGER*2 DATE(LDATE_LEN)/LDATE_LEN*0/      !

	REAL*8    PTAB(MAXTYP+1,8)    !
	REAL*8    GAME_PTAB(2,8) /16*0.0D0/ ! PERCENTAGE TABLE FOR LOTTO & VIKING
        REAL*8    TOTREAL             !

	CHARACTER HEAD*40             !
        CHARACTER * 7 GAMENAMES(5)    ! NAME OF GAMES

        INTEGER*4 MUL1,MUL2
        PARAMETER(MUL1=8*(MAXTYP+1))
        PARAMETER(MUL2=8*MAXGAM)
	DATA      STAB/MUL1*0/
	DATA      GAME_STAB/MUL2*0/
	DATA      FIX_STAB/MUL1*0/
        DATA      PTAB/MUL1*0.0D0/
C
        DATA GAMENAMES / '  Lotto',      ! NAME FOR LOTTO
     *                   '  Joker',      ! NAME FOR JOKER
     *                   ' Sports',      ! NAME FOR SPORTS
     *                   'Results',      ! NAME FOR RESULTS
     *                   'Passive'  /    ! NAME FOR RESULTS

C
C================== Start of program code. ================================

	CALL COPYRITE

	PAGE   = 0
	GAMCNT = 0
	COPY   = 0

C OPEN DAILY ACTIVITY FILE
	
	CALL OPENW(1,SFNAMES(1,DAF),4,0,0,ST)
	CALL IOINIT(FDB,1,DAFSEC*256)
	IF(ST.NE.0) THEN
	    CALL CLOSEFIL(FDB)
	    CALL FILERR(SFNAMES(1,DAF),1,ST,0)
	ENDIF

	SCDC=DAYCDC
15	CONTINUE
	DATE(VCDC)=SCDC
	CALL LCDATE(DATE)
	CALL FIGWEK(DAYCDC,WEEK,YEAR2)
	IF(DATE(VDOW).NE.SUNDAY) THEN
	    SCDC = SCDC - 1
	    GOTO 15
	ENDIF

	DO 30 CDC = SCDC, SCDC + 6
	    IF(CDC.LE.0) GOTO 30
	    CALL READW(FDB,CDC,DAFREC,ST)
	    IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),1,ST,0)
	    IND = CDC - SCDC + 1
 
            DO 20 GAM = 1, MAXGAM
	        GTYP = GNTTAB(GAMTYP,GAM)
	        IF(GTYP.EQ.0 .OR. GTYP.EQ.TNBR) GOTO 20  
		STAB(GTYP,IND) = STAB(GTYP,IND) + DAFTYP(DOLAMT,TWAG,GAM)
                ! store also for BALWRI CALL by game
                GAME_STAB(GAM,IND) = DAFTYP(DOLAMT,TWAG,GAM)
	        STAB(MAXTYP+1,IND) = STAB(MAXTYP+1,IND) + 
     *                               DAFTYP(DOLAMT,TWAG,GAM)
20	    CONTINUE

            ! write today's sales to BALANS file
            IF (CDC .EQ. DAYCDC) THEN
                RAPCODE = 5
                DO 27 GAM = 1, MAXGAM
                    GTYP = GNTTAB(GAMTYP,GAM)
	            IF(GTYP.EQ.0 .OR. GTYP.EQ.TNBR) GOTO 27  
                    GAMESUMS(GAM,1,DOLAMT) = GAME_STAB(GAM,IND)
27              CONTINUE

                TOTSUMS(2) = STAB(MAXTYP+1,IND)
                CALL BALWRI(RAPCODE,GAMESUMS,TOTSUMS,TOTREAL)
            END IF
30	CONTINUE




C CALCULATE PERCENT SALES FOR EACH GAME
C -------------------------------------

	DO I = 1, 7
	   IF(STAB(MAXTYP+1,I).GT.0) THEN
	      DO J=1,2
	         GAME_PTAB(J,I) = DFLOAT(GAME_STAB(J,I))/
     *                           DFLOAT(STAB(MAXTYP+1,I))*100.0D0
              END DO
	   ENDIF
        END DO


	DO 50 I = 1, 7
	    IF(STAB(MAXTYP+1,I).LE.0) GOTO 50
	    DO 40 J=1,MAXTYP
	        PTAB(J,I) = DFLOAT(STAB(J,I))/
     *                      DFLOAT(STAB(MAXTYP+1,I))*100.0D0
40	    CONTINUE
	    PTAB(MAXTYP+1,I) = 100.0D0
50	CONTINUE




C TOTAL FOR EACH GAME TYPE FOR ALL THE DAYS OF THE WEEK
C  ----------------------------------------------------

	DO 60 I = 1, 7
            DO 60 J=1,MAXTYP+1
	      STAB(J,8) = STAB(J,8) + STAB(J,I)
	      FIX_STAB(J,8) = FIX_STAB(J,8) + STAB(J,I)/1000
60	CONTINUE


	DO 70 J = 1, MAXTYP
	    TOTSAL = FIX_STAB(MAXTYP+1,8)
	    IF(TOTSAL.EQ.0) TOTSAL = 1
	    PTAB(J,8) = (DFLOAT(STAB(J,8))/ DFLOAT(TOTSAL)) *100.0D0
            PTAB(J,8) = PTAB(J,8)/1000.0D0
	    PTAB(MAXTYP+1,8) = PTAB(MAXTYP+1,8) + PTAB(J,8)
70	CONTINUE



C       FOR LOTTO
C       --------------------

	DO I = 1, 7
           DO J=1,2
	    GAME_STAB(J,8)   = GAME_STAB(J,8) + GAME_STAB(J,I) !LOTTO
	   END DO
	END DO

	DO 701 J = 1, 2
	    TOTSAL = FIX_STAB(MAXTYP+1,8)
	    IF(TOTSAL.EQ.0) TOTSAL = 1
	    GAME_PTAB(J,8) = (DFLOAT(GAME_STAB(J,8))/ DFLOAT(TOTSAL)) *100.0D0
            GAME_PTAB(J,8) = GAME_PTAB(J,8)/1000.0D0
701	CONTINUE

C---- Work out Table Line Totals.

	DO I = 1,8
	  TABTOT1(I) =  STAB(TLTO,I) + STAB(TKIK,I) +
     *		        STAB(TBNG,I) + STAB(TSPT,I) + STAB(TTGL,I) + 
     *                  STAB(TPAS,I)
	  TABTOT2(I) =  STAB(TSCR,I) + STAB(TWIT,I) + STAB(TTSL,I) + 
     *                  STAB(TDBL,I) + STAB(TCPL,I) + STAB(TSSC,I) + 
     *                  STAB(TTRP,I) + STAB(TSTR,I)
	END DO

C---- Start of report.

	CALL ROPEN('GAMTOT.REP',6,ST)

	DATE(VCDC)=SCDC

	CALL LCDATE(DATE)

C---- Write out report header.

	WRITE (HEAD,900) WEEK, YEAR2

	CALL TITLE(HEAD,'GAMTOT  ',1,6,PAGE,DAYCDC)

	WRITE (6,901)
C
C---- Write out column headings for table 1.
C
       WRITE(6, 9011) (GAMENAMES(I), I = 1, 5) 
C
C---- Write out sales info.
C
	DO I = 1, 7
	    DATE(VCDC) = SCDC + I - 1
	    CALL LCDATE(DATE)
	    WRITE(6,902) (DATE(J),J=7,13),
     *                    CMONY(STAB(TLTO,I),11,BETUNIT),    ! LOTTO
     *                    CMONY(STAB(TKIK,I),11,BETUNIT),    ! JOKERI
     *                    CMONY(STAB(TSPT,I),11,BETUNIT),    ! 1X2
     *                    CMONY(STAB(TTGL,I),11,BETUNIT),    ! TOTOGOLA
     *                    CMONY(STAB(TPAS,I),11,BETUNIT),    ! PASSIVE
     *	                  CMONY(TABTOT1(I),13,BETUNIT)	     ! Line Total
	END DO

C---- Total up columns.

	WRITE(6,903)  CMONY(STAB(TLTO,8),11,BETUNIT),    ! LOTTO
     *                CMONY(STAB(TKIK,8),11,BETUNIT),    ! JOKERI
     *                CMONY(STAB(TSPT,8),11,BETUNIT),    ! 1X2
     *                CMONY(STAB(TTGL,8),11,BETUNIT),    ! TOTOGOLA
     *                CMONY(STAB(TPAS,8),11,BETUNIT),    ! PASSIVE
     *                CMONY(TABTOT1(8),13,BETUNIT)	 ! Line Total
C
C---- Close file and print.
C
	CALL CLOSEFIL(FDB)

	CALL SPOOL('GAMTOT.REP',COPY,ST)
C===================== Format Statements =================

C---- Header and titles format statements.

900	format ('SALES SUMMARY BY GAME FOR WEEK ',I2.2,'/',I4.4)

901	format (1X, 131('='), /, 1X)

9011	format (16X,5(3X, A8),8X,'TOTAL',/)

C
C---- Display data format statements.
C
902	format (1X,7A2,1X,5(A11),A13,/)

903	format (1X,130('-'),/,1X,'TOTAL',10X,5(A11),A13,/)

C
C END OF PROGRAM
C
	END

C********************** End of Program. ************************************
