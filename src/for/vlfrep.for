C
C PROGRAM VLFREP
C
C VLFREP.FOR
C
C V12 02-DEC-2000 UXN  TOTOGOLO ADDED
C V11 13-OCT-1999 RXK  World Tour added.
C V10 17-MAY-1999 UXN  SUPER TRIPLE ADDED.
C V09 15-DEC-1994 PXB  Added bingo game.
C V08 02-SEP-1993 SXH  ERROR IN PREVIOUS COMMENT
C V07 26-AUG-1993 SXH  AddeD  IAM() etc
C V06 13-JUL-1993 SXH  Released for Finland
C V05 21-JAN-1993 DAB  Initial Release
C                      Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                      DEC Baseline
C V04 07-APR-1992 GCAN PROMPT FOR MONY, BUT ONLY IF ODDSET GAMES ARE ACTIVE.
C V03 08-FEB-1992 MGM  REMOVED PROMPT FOR MONEY AS BIWIN DOES NOT
C                      CHECK
C V02 12-NOV-1991 MTK  INITIAL RELEASE FOR NETHERLANDS
C V01 10-APR-1991 MTK  INITIAL RELEASE FOR MARYLAND
C
C GENERATE CANCELLED/BIG WINNERS REPORTS
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
	PROGRAM VLFREP
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:TGLCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:NBRCOM.DEF'
        INCLUDE 'INCLIB:SCRCOM.DEF'
        INCLUDE 'INCLIB:WITCOM.DEF'
	INCLUDE 'INCLIB:TSLCOM.DEF'
	INCLUDE 'INCLIB:BNGCOM.DEF'
	INCLUDE 'INCLIB:TRPCOM.DEF'
	INCLUDE 'INCLIB:STRCOM.DEF'
	INCLUDE 'INCLIB:SSCCOM.DEF'

	INCLUDE 'INCLIB:PRMHSH.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VALFIL.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C
        ! parameter
        INTEGER*4    TUBSIZ
        PARAMETER   (TUBSIZ=I4BUCSIZ*7)

        ! variables
	INTEGER*4  DRAWS(MAXGAM)         !
	INTEGER*4  VLFBUF(TUBSIZ)        !
	INTEGER*4  GIND                  !
	INTEGER*4  GTYP                  !
	INTEGER*4  GAMCNT                !
	INTEGER*4  GAM                   !
	INTEGER*4  I                     !
	INTEGER*4  K                     !
	INTEGER*4  ST                    !
	INTEGER*4  COPY                  !
	INTEGER*4  EXT                   !
	INTEGER*4  BIGAMT                !
	INTEGER*4  FLAG                  !


	LOGICAL	    MONYGAM/.FALSE./
C
C
	CALL COPYRITE
	TYPE*,IAM(),' Generating cancelled/big winners reports'
	COPY=1
C
C
	GAMCNT=0
	DO 100 GAM=1,MAXGAM
	    DRAWS(GAM)=0

	    GTYP = GNTTAB(GAMTYP,GAM)
	    GIND = GNTTAB(GAMIDX,GAM)
	    IF(GTYP.LE.0) GOTO 100

	    IF(GTYP.EQ.TLTO) THEN
	        IF(LTODAT(CURDRW,GIND).NE.DAYCDC) GOTO 100
	        GAMCNT=GAMCNT+1
	        DRAWS(GAM)=LTODRW(GIND)
	        WRITE(5,901) IAM(),GTNAMES(TLTO),GIND,LTODRW(GIND)
	    ENDIF

            IF(GTYP.EQ.TSPT) THEN
                IF(SPTDAT(CURDRW,GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=SPTDRW(GIND)
                WRITE(5,901) IAM(),GTNAMES(TSPT),GIND,SPTDRW(GIND)
            ENDIF

            IF(GTYP.EQ.TTGL) THEN
                IF(TGLDAT(CURDRW,GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=TGLDRW(GIND)
                WRITE(5,901) IAM(),GTNAMES(TTGL),GIND,TGLDRW(GIND)
            ENDIF

            IF(GTYP.EQ.TNBR) THEN
                IF(NBRDAT(CURDRW,GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=NBRDRW(GIND)
                WRITE(5,901) IAM(),GTNAMES(TNBR),GIND,NBRDRW(GIND)
            ENDIF

            IF(GTYP.EQ.TWIT) THEN
                IF(WITDAT(GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=WITDRW(GIND)
                WRITE(5,901) IAM(),GTNAMES(TWIT),GIND,WITDRW(GIND)
            ENDIF

            IF(GTYP.EQ.TKIK) THEN
                IF(KIKDAT(CURDRW,GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=KIKDRW(GIND)
                WRITE(5,901) IAM(),GTNAMES(TKIK),GIND,KIKDRW(GIND)
            ENDIF

            IF(GTYP.EQ.TSCR) THEN
                IF(SCRDAT(GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=SCRDRW(GIND)
                WRITE(5,901) IAM(),GTNAMES(TSCR),GIND,SCRDRW(GIND)
            ENDIF

            IF(GTYP.EQ.TSSC) THEN
                IF(SSCDAT(GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=SSCDRW(GIND)
                WRITE(5,901) IAM(),GTNAMES(TSSC),GIND,SSCDRW(GIND)
            ENDIF

            IF(GTYP.EQ.TTRP) THEN
                IF(TRPDAT(GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=TRPDRW(GIND)
                WRITE(5,901) IAM(),GTNAMES(TTRP),GIND,TRPDRW(GIND)
            ENDIF

            IF(GTYP.EQ.TSTR) THEN
                IF(STRDAT(GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=STRDRW(GIND)
                WRITE(5,901) IAM(),GTNAMES(TSTR),GIND,STRDRW(GIND)
            ENDIF

            IF(GTYP.EQ.TTSL) THEN
	        DO 10 K=1,MAXSRW
                    IF(TSLDAT(K,GIND).NE.DAYCDC) GOTO 10
                    GAMCNT=GAMCNT+1
                    DRAWS(GAM)=TSLDRW(GIND)
                    WRITE(5,901) IAM(),GTNAMES(TTSL),GIND,TSLDRW(GIND)
	            GOTO 100
10	        CONTINUE
            ENDIF

            IF (GTYP .EQ. TBNG) THEN
                IF (BNGDAT(CURDRW,GIND) .NE. DAYCDC) GOTO 100
                GAMCNT = GAMCNT+1
                DRAWS(GAM) = BNGDRW(GIND)
                WRITE(5,901) IAM(),GTNAMES(TBNG),GIND,BNGDRW(GIND)
            END IF


100	CONTINUE
C
C
       	CALL PRMYESNO('Do you want to override games on reports? ',FLAG)
	IF(FLAG.EQ.1) CALL VLF_GETGAM(DRAWS,GAMCNT)
C
C CHECK IF ANY ODDSET GAMES ARE PICKED (MONYGAMES).
C
	DO GAM = 1,MAXGAM
	    GTYP = GNTTAB(GAMTYP,GAM)
	    IF(GTYP.EQ.TTSL.OR.GTYP.EQ.TWIT.OR.GTYP.EQ.TSCR.OR.
     *         GTYP.EQ.TCPL.OR.GTYP.EQ.TDBL.OR.GTYP.EQ.TSSC.OR.
     *         GTYP.EQ.TTRP.OR.GTYP.EQ.TSTR)
     *	    MONYGAM = .TRUE.
        END DO
C
C
	IF(GAMCNT.EQ.0) THEN
	    TYPE*,IAM(),' No drawings today'
	    CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
	IF(MONYGAM) THEN
	    CALL PRMMONY('Enter amount for big winners report ',
     *	        	  BIGAMT,VALUNIT,EXT)
	    IF(EXT.LT.0) CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
	CALL PRMYESNO('Do you want to override winning divisions on report',
     *	FLAG)
C
C
	DO 110 GAM = 1, MAXGAM
	    IF(DRAWS(GAM).EQ.0) GOTO 110

	    GTYP=GNTTAB(GAMTYP,GAM)
	    GIND=GNTTAB(GAMIDX,GAM)

            CALL CANWIN(VALREC,GAM,GTYP,GIND,DRAWS(GAM),COPY,1)
	    IF(FLAG.EQ.1) THEN
      	        CALL BIGWIN(VALREC,-(GAM),GTYP,GIND,DRAWS(GAM),COPY,1,BIGAMT)
	    ELSE
	        CALL BIGWIN(VALREC,GAM,GTYP,GIND,DRAWS(GAM),COPY,1,BIGAMT)
	    ENDIF
110	CONTINUE
C
C SCAN VLF FOR CANCELLED/BIG WINNERS
C
        CALL IOPEN(SFNAMES(1,VLF),1,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLF),1,ST,0)
        CALL ITUBSIZE(1,TUBSIZ)
C
C
1000	CONTINUE
        CALL ISREAD(V4BUF,1,VLFBUF,ST)
        IF(ST.EQ.ERREND) THEN
            CALL ICLOSE(1,VLFBUF,ST)
            DO 200 I=1,MAXGAM
                IF(DRAWS(I).NE.0) THEN

 	            GTYP=GNTTAB(GAMTYP,I)
	            GIND=GNTTAB(GAMIDX,I)
                    CALL CANWIN(VALREC,I,GTYP,GIND,DRAWS(I),COPY,3)
	            IF(FLAG.EQ.1) THEN
      	                CALL BIGWIN(VALREC,-(I),GTYP,GIND,DRAWS(I),
     *                              COPY,3,BIGAMT)
	            ELSE
                        CALL BIGWIN(VALREC,I,GTYP,GIND,DRAWS(I),COPY,3,BIGAMT)
	            ENDIF
                ENDIF
200	    CONTINUE
	    CALL GSTOP(GEXIT_SUCCESS)
	ENDIF

	IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLF),2,ST,0)
	CALL LOGVAL(VALREC,V4BUF)
	GAM   = VALREC(VGAM)
	GTYP  = VALREC(VGTYP)
	GIND  = VALREC(VGIND)

 	IF(DRAWS(GAM).EQ.0) GOTO 1000

        CALL CANWIN(VALREC,GAM,GTYP,GIND,DRAWS(GAM),COPY,2)
	IF(FLAG.EQ.1) THEN
	    CALL BIGWIN(VALREC,-(GAM),GTYP,GIND,DRAWS(GAM),COPY,2,BIGAMT)
	ELSE
            CALL BIGWIN(VALREC,GAM,GTYP,GIND,DRAWS(GAM),COPY,2,BIGAMT)
	ENDIF
	GOTO 1000
C
C
901	FORMAT(1X,A,' Generating reports for ',A8,I1,' draw ',I5)

	END
