C SUBROUTINE CHKROW
C
C V11 11=APR-2017 MTK Fixed system bet check for Super 14 game
C V10 30-MAR-2017 MTK Modified Super 14 game
C V09 27-OCT-2003 FRP Modify for Batch2 Totobola Changes.
C V08 08-FEB-2001 JHR CHECK FOR VALID FULL SYSTEM BET
C V07 24-NOV-2000 UXN TOTOGOLO ADDED FOR PORTUGAL.
C V06 15-feb-2000 OXK Resized ROWS-table
C V05 03-FEB-2000 OXK Bitmasks 1xxx no longer accepted (Vakio changes)  
C V04 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C			Instant Pass Thru Phase 1
C V03 16 Jul 1993 SXH Removed Math systems, and check for FULL system 
C			bets as all system bets are valid in FINLAND
C V02 14 Jun 1993 SXH Released for Finland
C V01 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C
C SUBROUTINE TO CHECK SPORTS BOARDS
C	The following SYNTERRCODs are currently generated:
C	500	Row(s) with no marks bet for simple bets or full systems
C	501	Incorrect number of matches in the row
C       502     Incorrect System bet
C	505	System bet played but not marked
C	510	Selected system doesn't fit into the rows
C	520	Row(s) with no marks bet for reduced systems
C	530	Incorrect # of partly varied marks for reduced system
C	540	Incorrect # of fully varied marks for reduced system
C	560	Row(s) with no marks bet for U-systems
C	570	Incorrect # of partly varied marks for U-system
C	580	Incorrect # of fully varied marks for U-system
C	590	\
C	591	 \ Mismatches with marks in U-systems
C	600	 /
C	610	/
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
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE CHKROW(TRABUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'


        ! variables
	INTEGER*4  POWER2(0:16)        !
	INTEGER*4  POWER3(0:16)        !
	INTEGER*4  POWER4(0:16)        !
	INTEGER*4  MARKS(0:15)         !
	INTEGER*4  TAB(0:4,16)         !
	INTEGER*4  POS(SPGNBR,2)       !
	INTEGER*4  ROWS(SPGNBR,12)     !
	INTEGER*4  RROWS(2,TGGNBR,12)
	INTEGER*4  DBL                 !
	INTEGER*4  TPL                 !
	INTEGER*4  FULL                !
	INTEGER*4  SIMPLE              !
	INTEGER*4  ROWCNT              !
	INTEGER*4  CNT                 !
	INTEGER*4  I                   !
	INTEGER*4  J                   !
	INTEGER*4  SYS                 !
	INTEGER*4  GIND
	INTEGER*4  BCNT

C                  - 1 2 12 X 1X 12 1X2 - - - - - - - -
	DATA MARKS/0,1,1, 2,1, 2, 2,  3,1,2,2,3,2,3,3,4/
CV05	DATA MARKS/0,1,1, 2,1, 2, 2,  3,0,1,1,1,1,1,1,1/
	DATA POWER2/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,
     *	            16384,32768,65536/
	DATA POWER3/1,3,9,27,81,243,729,2187,6561,19683,59049,
     *	            177147,531441,1594323,4782969,14348907,43046721/
	DATA POWER4/1,4,16,64,256,1024,4096,16384,65536,262144,1048576,
     *              4194304,16777216,67108864,268435456,1073741824,0/
C
C First check the validity in general
C
	GIND = TRABUF(TGAMIND)
	IF (TRABUF(TWSRW).NE.SPTMAX(GIND)) THEN
                TRABUF(TERR)=SYNT
                SYNTERRCOD=501
                RETURN
	ENDIF
C
	BCNT = 0
	IF(TRABUF(TWSPFRG).NE.0) BCNT = 1

	SYS = TRABUF(TWSYSN)
	CALL GETROW(TRABUF,ROWS,RROWS)
	CALL FASTSET(0,TAB,5*16)
	IF(TRABUF(TWSYST).EQ.REDSYS) GOTO 1000
	IF(TRABUF(TWSYST).EQ.USYS  ) GOTO 2000
C
C FULL SYSTEM AND SIMPLE WAGERS
C
	DO J = 1, TRABUF(TWNBET)
	    DO I = 1, TRABUF(TWSRW)-BCNT
	        CNT        = MARKS(ROWS(I,J))
	        TAB(CNT,J) = TAB(CNT,J)+1
                IF(CNT .NE. 1 .AND. SPTECD(I, GIND) .NE. 0)  THEN
  	          TRABUF(TERR)  = WMEC   ! EVENT HAS BEEN CANCELLED SO ONLY ONE MARK
                  TRABUF(TSTAT) = REJT   ! IS ALLOWED IN THIS EVENT
                  RETURN
                ENDIF
            END DO
        END DO
C
C CALCULATE TOTAL NUMBER OF ROWS
C
	ROWCNT = 0
	SIMPLE = 0
	FULL   = 0
	DO I = 1, TRABUF(TWNBET)
	    IF(TAB(0,I).NE.0) THEN
	        TRABUF(TERR)=SYNT
	        SYNTERRCOD=500
	        RETURN
	    ENDIF


	    IF(TAB(1,I) .EQ. TRABUF(TWSRW)-BCNT) THEN
	        ROWCNT = ROWCNT+1
	        SIMPLE = SIMPLE+1
	    ELSE
	        ROWCNT = ROWCNT + POWER2(TAB(2,I)) * 
     *                            POWER3(TAB(3,I)) * POWER4(TAB(4,I))
	        FULL = FULL+1
	    ENDIF
C
C CHECK FOR VALID FULL SYSTEM BET
C
	    IF(SPSFSF(TAB(3,I),TAB(2,I),GIND).NE.1) THEN
	        TRABUF(TERR)=SYNT
	        SYNTERRCOD=502
	        RETURN
	    ENDIF
C
        END DO
C
	TRABUF(TWSIMP)=ROWCNT
	IF(TRABUF(TWSYST).EQ.NOSYS.AND.FULL.NE.0) THEN
	    TRABUF(TERR)=SYNT
	    SYNTERRCOD=505
	    RETURN
	ENDIF
	IF(TRABUF(TWSYST).EQ.FULSYS.AND.FULL.EQ.0) TRABUF(TWSYST)=NOSYS

C
C CHECK SUPER14 ROW
C
	IF(TRABUF(TWSPFRG).NE.SPTFRG(GIND)) THEN
          TRABUF(TERR)=SYNT
          SYNTERRCOD=501
          RETURN
	ENDIF

	CALL FASTSET(0,TAB,5*16)

	IF(TRABUF(TWSPFRG).EQ.1) THEN
          CNT        = MARKS(RROWS(1,1,1))
          TAB(CNT,1) = TAB(CNT,1)+1
          CNT        = MARKS(RROWS(2,1,1))
          TAB(CNT,1) = TAB(CNT,1)+1
        ENDIF

        IF(TRABUF(TWSPFRG).EQ.2) THEN
          CNT        = MARKS(RROWS(1,1,1))
          TAB(CNT,1) = TAB(CNT,1)+1
        ENDIF

        FULL   = 0
        IF(TAB(0,1).NE.0) THEN
           TRABUF(TERR)=SYNT
           SYNTERRCOD=500
           RETURN
        ENDIF

	IF(TRABUF(TWSPFRG).EQ.1) THEN
          IF(TAB(1,1) .NE. 2) FULL = 1
          IF(TAB(1,1) .NE. 2 .AND. SPTECD(I, GIND) .NE. 0)  THEN
  	     TRABUF(TERR)  = WMEC   ! EVENT HAS BEEN CANCELLED SO ONLY TWO MARKS
             TRABUF(TSTAT) = REJT   ! IS ALLOWED IN THIS EVENT
             RETURN
          ENDIF
	ENDIF

        IF(TRABUF(TWSPFRG).EQ.2) THEN
          IF(TAB(1,1) .NE. 1) FULL = 1
          IF(TAB(1,1) .NE. 1 .AND. SPTECD(I, GIND) .NE. 0)  THEN
  	     TRABUF(TERR)  = WMEC   ! EVENT HAS BEEN CANCELLED SO ONLY ONE MARK
             TRABUF(TSTAT) = REJT   ! IS ALLOWED IN THIS EVENT
             RETURN
          ENDIF
        ENDIF

        IF(FULL.NE.0) THEN
          TRABUF(TERR)=SYNT
          SYNTERRCOD=510
          RETURN
        ENDIF
	RETURN
C
C PROCESS REDUCED SYSTEM BETS
C
1000	CONTINUE
C
C
	TPL    = SPSNUM(2,SYS)-SPSNUM(1,SYS)
	DBL    = SPSNUM(3,SYS)-SPSNUM(2,SYS)
C
C Does the system fit into the board in the first place...
C
	IF ((TPL+DBL).GT.SPTMAX(GIND)) THEN
                TRABUF(TERR)=SYNT
                SYNTERRCOD=510
                RETURN
	ENDIF
C
C
	DO I = 1, TRABUF(TWSRW)-BCNT
	    CNT        = MARKS(ROWS(I,1))
	    TAB(CNT,1) = TAB(CNT,1)+1
        END DO

	TRABUF(TWSIMP) = SPSNUM(5,SYS) 
	IF(TAB(0,1).NE.0) THEN
	    TRABUF(TERR)=SYNT
	    SYNTERRCOD=520
	    RETURN
	ENDIF

	IF(TAB(2,1).NE.DBL) THEN
	    TRABUF(TERR)=SYNT
	    SYNTERRCOD=530
	    RETURN
	ENDIF

	IF(TAB(3,1).NE.TPL) THEN
	    TRABUF(TERR)=SYNT
	    SYNTERRCOD=540
	    RETURN
	ENDIF
	RETURN
C
C PROCESS U-SYSTEM BETS
C
2000	CONTINUE
	TPL=SPSNUM(2,SYS)-SPSNUM(1,SYS)
	DBL=SPSNUM(3,SYS)-SPSNUM(2,SYS)

	DO I = 1, TRABUF(TWSRW)-BCNT
	    CNT        = MARKS(ROWS(I,1))
	    TAB(CNT,1) = TAB(CNT,1)+1
	    POS(I,1)=CNT
        END DO

	TRABUF(TWSIMP) = SPSNUM(5,SYS)
	IF(TAB(0,1).NE.0) THEN
	    TRABUF(TERR)=SYNT
	    SYNTERRCOD=560
	    RETURN
	ENDIF

	IF(TAB(2,1).NE.DBL) THEN
	    TRABUF(TERR)=SYNT
	    SYNTERRCOD=570
	    RETURN
	ENDIF

	IF(TAB(3,1).NE.TPL) THEN
	    TRABUF(TERR)=SYNT
	    SYNTERRCOD=580
	    RETURN
	ENDIF
C
C CHECK SECOND U-SYS BOARD
C
	DO I = 1, TRABUF(TWSRW)-BCNT
	    CNT      = MARKS(ROWS(I,2))
	    POS(I,2) = CNT
        END DO
C
C
	DO I = 1, TRABUF(TWSRW)-BCNT
	    IF(POS(I,1).EQ.3.AND.POS(I,2).NE.1) THEN
	        TRABUF(TERR)=SYNT
	        SYNTERRCOD=590
	        RETURN
	    ENDIF

	    IF(POS(I,1).EQ.2.AND.POS(I,2).NE.1) THEN
	        TRABUF(TERR)=SYNT
	        SYNTERRCOD=591
	        RETURN
	    ENDIF

	    IF(POS(I,1).EQ.1.AND.POS(I,2).NE.0) THEN
	        TRABUF(TERR)=SYNT
	        SYNTERRCOD=600
	        RETURN
	    ENDIF

	    IF(MARKS(ROWS(I,1)).GT.1) THEN   ! MUST HAVE A U-MARK
	        IF(IAND(ROWS(I,1),ROWS(I,2)).EQ.0) THEN
	            TRABUF(TERR)=SYNT
	            SYNTERRCOD=610
	            RETURN
	        ENDIF
	    ENDIF

        END DO

	RETURN

	END
