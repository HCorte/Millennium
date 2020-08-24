C PROGRAM WINRPT
C
C V18 02-DEC-2000 UXN TOTOGOLO ADDED.
C V17 13-OCT-1999 RXK World Tour added.
C V16 17-MAY-1999 UXN SUPER TRIPLE ADDED.
C V15 10-JAN-1999 GPW STOPSYS OPTIMISATION
C V14 24-NOV-1994 HXK Added Bingo
C V13 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V12 29-APR-1994 JXP COPY=0
C V11 03-JAN-1994 HXK ADDED SCORE, WINNERSTIP GAMES.
C V10 17-OCT-1993 GXA Added Toto Select.
C V09 26-AUG-1993 SXH Added IAM() etc
C V08 14-JUL-1993 SXH Adde RAVI and SPEDEN subroutine CALLs
C V07 19-MAY-1993 SXH Released for Finland VAX
C V06 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V05 10-FEB-1992 GCAN FIX KICKER DRAW NUMBER.
C V04 12-NOV-1991 MTK  INITIAL RELEASE FOR NETHERLANDS
C V03 27-JUL-1991 KWP  INITIAL RELEASE FOR POLAND
C V02 03-APR-1991 MTK  INITIAL RELEASE FOR MARYLAND
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
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
	PROGRAM WINRPT
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:NBRCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:TGLCOM.DEF'
	INCLUDE 'INCLIB:TSLCOM.DEF'
        INCLUDE 'INCLIB:SCRCOM.DEF'
        INCLUDE 'INCLIB:WITCOM.DEF'
        INCLUDE 'INCLIB:DBLCOM.DEF'
        INCLUDE 'INCLIB:CPLCOM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:SSCCOM.DEF'
        INCLUDE 'INCLIB:TRPCOM.DEF'
        INCLUDE 'INCLIB:STRCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'

	INTEGER*4  FLAG            !
	INTEGER*4  GNUM            ! game number
	INTEGER*4  GIND            ! game index
	INTEGER*4  EXT             ! return status
	INTEGER*4  GTYP            ! game type
	INTEGER*4  K               ! counter
	INTEGER*4  I               ! counter
	INTEGER*4  ST              ! status
	INTEGER*4  DRAW            ! draw number
	INTEGER*4  COPY            ! number of copies
	INTEGER*4  CNT             ! counter
	INTEGER*4  ROW		   ! Row Number for Toto Select.

	CHARACTER*32 STRING2
C
	LOGICAL	   REP_FLG          !Generate Report Flag
	LOGICAL*4  AUTOPRMPT,SETFLG
	
C
	CALL COPYRITE
	CNT=0
	REP_FLG = .FALSE.
	COPY=0
	SETFLG = .FALSE.
	DO I=1,MAXGAM
	   IF(WINREP_AUTO(I).NE.0) SETFLG = .TRUE.
	ENDDO
C
C USE AUTOPROMPT IF WINREP IS SET IN STOPCOM AND WINRPT IS SUBPROCESS AND
C STSYSTEM IS STILL RUNNING.
C
        AUTOPRMPT=.FALSE.                                   !V06...
        IF(STOPMOD.EQ.WINMULTI) THEN                         
            IF(SETFLG) AUTOPRMPT=.TRUE.
            GO TO 99
        ENDIF                                                !...V06
	CALL STTSK(8HSTSYSTEM,I,ST)
	AUTOPRMPT = SETFLG .AND. ISSUBPROC() .AND. ST.NE.4
   99   CONTINUE
	IF(AUTOPRMPT) GOTO 1000
C
C	CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)
C	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
	CALL PRMYESNO('Do you to override games on reports? ', FLAG)
	IF(FLAG.NE.1) GOTO 2000
C
1000	CONTINUE	       
	IF(AUTOPRMPT) THEN
	    DO I = 1,MAXGAM
	       IF(WINREP_AUTO(I).NE.0) THEN
	         DRAW = WINREP_AUTO(I)
		 GTYP = GNTTAB(GAMTYP,I)
		 GIND = GNTTAB(GAMIDX,I)
		 WINREP_AUTO(I) = 0
		 GOTO 1100
	       ENDIF
	    ENDDO
	    CALL GSTOP(GEXIT_SUCCESS)
	ENDIF

	WRITE(5,900) IAM(),(K,GTNAMES(K),K=1,MAXTYP)

	CALL PRMNUM('Enter game type (E-no more) ',GTYP,1,MAXTYP,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)

	CALL PRMNUM('Enter game index ',GIND,1,MAXIND,EXT)
	IF(EXT.LT.0) GOTO 1000
C
1100	CONTINUE
C
 	IF(GTYP.NE.TLTO .AND. GTYP.NE.TSPT .AND. GTYP.NE.TTGL .AND.
     *     GTYP.NE.TNBR .AND. GTYP.NE.TKIK .AND.
     *     GTYP.NE.TTSL .AND. GTYP.NE.TSCR .AND.
     *     GTYP.NE.TDBL .AND. GTYP.NE.TCPL .AND.
     *     GTYP.NE.TSSC .AND. GTYP.NE.TTRP .AND.
     *     GTYP.NE.TWIT .AND. GTYP.NE.TBNG .AND.
     *     GTYP.NE.TSTR) THEN
	    WRITE(5,901) IAM()
	    GOTO 1000
	ENDIF
C
	GNUM = GTNTAB(GTYP,GIND)
	IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
	    TYPE*,IAM(),' Sorry, game selected is not active'
	    GOTO 1000
	ENDIF
C
	IF(AUTOPRMPT) GOTO 1200
C
	WRITE (STRING2,800) GTNAMES(GTYP),GIND
	CALL PRMNUM(STRING2,DRAW,1,9999,EXT)
	IF(EXT.LT.0) GOTO 1000
C
	WRITE(5,910) IAM(),GTNAMES(GTYP),GIND,(GLNAMES(K,GNUM),K=1,4),DRAW
	CALL PRMYESNO('Is this correct (Y/N) ',FLAG)
C
1200	CONTINUE
C
        IF(GTYP.EQ.TLTO) CALL LSHRRPT(GNUM,GIND,DRAW,COPY)
        IF(GTYP.EQ.TSPT) CALL SSHRRPT(GNUM,GIND,DRAW,COPY)
        IF(GTYP.EQ.TTGL) CALL TGSHRRPT(GNUM,GIND,DRAW,COPY)
        IF(GTYP.EQ.TNBR) CALL NSHRRPT(GNUM,GIND,DRAW,COPY)
        IF(GTYP.EQ.TKIK) CALL KSHRRPT(GNUM,GIND,DRAW,COPY)
	IF(GTYP.EQ.TTSL) CALL TSHRRPT(GNUM,GIND,DRAW,COPY)
        IF(GTYP.EQ.TSCR) CALL SCSHRRPT(GNUM,GIND,DRAW,COPY)
        IF(GTYP.EQ.TWIT) CALL WISHRRPT(GNUM,GIND,DRAW,COPY)
        IF(GTYP.EQ.TBNG) CALL BNSHRRPT(GNUM,GIND,DRAW,COPY)
        IF(GTYP.EQ.TDBL) CALL DBSHRRPT(GNUM,GIND,DRAW,COPY)
        IF(GTYP.EQ.TCPL) CALL CPSHRRPT(GNUM,GIND,DRAW,COPY)
        IF(GTYP.EQ.TSSC) CALL SSSHRRPT(GNUM,GIND,DRAW,COPY)
        IF(GTYP.EQ.TTRP) CALL TRSHRRPT(GNUM,GIND,DRAW,COPY)
        IF(GTYP.EQ.TSTR) CALL STSHRRPT(GNUM,GIND,DRAW,COPY)

	GOTO 1000
C
C
2000	CONTINUE
	DO 2100 GNUM=1,MAXGAM
	    GTYP=GNTTAB(GAMTYP,GNUM)
	    GIND=GNTTAB(GAMIDX,GNUM)
	    IF(GTYP.EQ.0.OR.GIND.EQ.0) GOTO 2100

	    IF(GTYP.EQ.TLTO) THEN
	        IF(LTODAT(CURDRW,GIND).EQ.DAYCDC) THEN
	            DRAW = LTODRW(GIND)
                    CALL LSHRRPT(GNUM,GIND,DRAW,COPY)
	            CNT = CNT + 1
	        ENDIF
	    ENDIF

 	    IF(GTYP.EQ.TSPT) THEN
	        IF(SPTDAT(CURDRW,GIND).EQ.DAYCDC) THEN
	            DRAW=SPTDRW(GIND)
                    CALL SSHRRPT(GNUM,GIND,DRAW,COPY)
	            CNT = CNT + 1
	        ENDIF
	    ENDIF

 	    IF(GTYP.EQ.TTGL) THEN
	        IF(TGLDAT(CURDRW,GIND).EQ.DAYCDC) THEN
	            DRAW=TGLDRW(GIND)
                    CALL TGSHRRPT(GNUM,GIND,DRAW,COPY)
	            CNT = CNT + 1
	        ENDIF
	    ENDIF

            IF(GTYP.EQ.TNBR) THEN
                IF(NBRDAT(CURDRW,GIND).EQ.DAYCDC) THEN
                    DRAW=NBRDRW(GIND)
                    CALL NSHRRPT(GNUM,GIND,DRAW,COPY)
	            CNT = CNT + 1
                ENDIF
            ENDIF

            IF(GTYP.EQ.TKIK) THEN
                IF(KIKDAT(CURDRW,GIND).EQ.DAYCDC) THEN
                    DRAW = KIKDRW(GIND)
                    CALL KSHRRPT(GNUM,GIND,DRAW,COPY)
                    CNT = CNT + 1
                ENDIF
            ENDIF

	   IF(GTYP.EQ.TTSL) THEN
	      DRAW = TSLDRW(GIND)
	      DO ROW = 1,MAXSRW
	         IF(TSLDAT(ROW,GIND).EQ.DAYCDC) REP_FLG = .TRUE.
	      END DO
	      IF(REP_FLG) THEN
	         CALL TSHRRPT(GNUM,GIND,DRAW,COPY)
		 CNT = CNT + 1
	      ENDIF
	   ENDIF

            IF(GTYP.EQ.TSCR) THEN
                IF(SCRDAT(GIND).EQ.DAYCDC) THEN
                    DRAW = SCRDRW(GIND)
                    CALL SCSHRRPT(GNUM,GIND,DRAW,COPY)
                    CNT = CNT + 1
                ENDIF
            ENDIF

            IF(GTYP.EQ.TWIT) THEN
                IF(WITDAT(GIND).EQ.DAYCDC) THEN
                    DRAW = WITDRW(GIND)
                    CALL WISHRRPT(GNUM,GIND,DRAW,COPY)
                    CNT = CNT + 1
                ENDIF
            ENDIF

            IF(GTYP.EQ.TBNG) THEN
                IF(BNGDAT(CURDRW,GIND).EQ.DAYCDC) THEN
                    DRAW = BNGDRW(GIND)
                    CALL BNSHRRPT(GNUM,GIND,DRAW,COPY)
                    CNT = CNT + 1
                ENDIF
            ENDIF

            IF(GTYP.EQ.TDBL) THEN
                IF(DBLDAT(GIND).EQ.DAYCDC) THEN
                    DRAW = DBLDRW(GIND)
                    CALL DBSHRRPT(GNUM,GIND,DRAW,COPY)
                    CNT = CNT + 1
                ENDIF
            ENDIF

            IF(GTYP.EQ.TCPL) THEN
                IF(CPLDAT(GIND).EQ.DAYCDC) THEN
                    DRAW = CPLDRW(GIND)
                    CALL CPSHRRPT(GNUM,GIND,DRAW,COPY)
                    CNT = CNT + 1
                ENDIF
            ENDIF

            IF(GTYP.EQ.TSSC) THEN
                IF(SSCDAT(GIND).EQ.DAYCDC) THEN
                    DRAW = SSCDRW(GIND)
                    CALL SSSHRRPT(GNUM,GIND,DRAW,COPY)
                    CNT = CNT + 1
                ENDIF
            ENDIF

            IF(GTYP.EQ.TTRP) THEN
                IF(TRPDAT(GIND).EQ.DAYCDC) THEN
                    DRAW = TRPDRW(GIND)
                    CALL TRSHRRPT(GNUM,GIND,DRAW,COPY)
                    CNT = CNT + 1
                ENDIF
            ENDIF

            IF(GTYP.EQ.TSTR) THEN
                IF(STRDAT(GIND).EQ.DAYCDC) THEN
                    DRAW = STRDRW(GIND)
                    CALL STSHRRPT(GNUM,GIND,DRAW,COPY)
                    CNT = CNT + 1
                ENDIF
            ENDIF
 
2100	CONTINUE

	IF(CNT.EQ.0) TYPE*,IAM(),' no drawings today'

	CALL GSTOP(GEXIT_SUCCESS)

800	FORMAT('Enter ',A8,I1,' draw number ')
900	FORMAT(//,1X,A,' WINRPT  game selection',//,
     *	        <MAXTYP>(19X,I2,' - ',A8,/))
901	FORMAT(1X,A,' Sorry, function not available for that game type')
910	FORMAT(1X,A,A8,I1,2X,4A4,'Draw ',I5,/)
911	FORMAT(1X,A,A,A8,A,I2,1X,A,I6)
	END
