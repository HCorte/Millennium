C
C SUBROUTINE SALVAL
C
C SALVAL.FOR
C
C V13 27-MAR-2017 HXK Omit Joker details if Joker removed > 90 days
C V12 01-JUL-2015 SCML Correction of SALVAL for Portugal
C V11 02-JAN-2011 HXK ADD LOTTO3 AND LOTTO4 TO ONE GAME (TOTOLOTO)
C V10 17-MAY-1999 UXN Super Triple added.
C V09 23-NOV-1995 PXB Couple and Double games added
C V08 04-JAN-1994 HXK ACCUMULATE SCORE, WINNERS TIP.
C V07 05-OCT-1993 GXA Added Refunds to validations.
C V06 16-AUG-1993 SXH Debugged
C V05 21-JUN-1993 HXK CHANGED FOR FINLAND VAX CONVERSION!
C V04 10-JUN-1993 HXK Changed AGTINF.DEF, AGTCOM.DEF includes.
C V03 10-FEB-1993 EBD Changed dimension of sales to *, so calling routine 
C                     dimensions the array
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 18-NOV-1991 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C
C CALLING SEQUENCE:
C     CALL SALVAL(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
C INPUT
C     SUBCLASS - REPORT SUBCLASS
C     MANAGER  - MANAGER FLAG
C
C OUTPUT
C     SALES    - SALES INFO FOR REPORT
C     SALOFF   - TOTAL WORDS USED
C     CDC      - CDC OF REPORT
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C----+------------------------------------------------------------------
C V12| Correction of SALVAL for Portugal
C----+------------------------------------------------------------------
C       SUBROUTINE SALVAL(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
        SUBROUTINE SALVAL_OLD(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
C----+------------------------------------------------------------------
C V12| Correction of SALVAL for Portugal
C----+------------------------------------------------------------------
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:SPECOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'

        ! arguments
	INTEGER*4   SALES(*)          !
        INTEGER*4   SUBCLASS          !
        INTEGER*4   SALOFF            !
        INTEGER*4   CDC               !
        INTEGER*4   RTER              !

	LOGICAL	    MANAGER           !

        ! variables                   !
	INTEGER*4   SALIND            !
	INTEGER*4   GNUM              !
	INTEGER*4   GTYP              !
        INTEGER*4   CLERK             !
	INTEGER*4   I                 !
	INTEGER*4   TMPCNT/0/	      !TEMP. TOTAL COUNT, UNTIL INDEX COMPUTED
	INTEGER*4   TMPAMT(2)/0,0/    !TEMP. TOTAL AMOUNT UNTIL INDEX COMPUTED
        INTEGER*4   GIND              !
	INTEGER*4   TMP_GNUM          ! used to sum Lotto4 cnt/amt to Lotto3
C
        INTEGER*4  IDX(MAXGAM)
C
C SET / CLEAR VARIABLES
C
	TMPCNT = 0
	TMPAMT(1) = 0
	TMPAMT(2) = 0
  
        SALIND   = 1


C
C PROCESS ONLY ONLINE VALIDATIONS REPORT
C
	IF(SUBCLASS.EQ.0.OR.SUBCLASS.EQ.8) THEN  !TODAY OR W. T. D.
            SALIND = 1
            CALL FASTSET(0,IDX,MAXGAM)
	    DO 400 GNUM=1,MAXGAM
	        GTYP=GNTTAB(GAMTYP,GNUM)
	        GIND=GNTTAB(GAMIDX,GNUM)
	        IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 400
                IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 400
                IF(GTYP.EQ.TINS) GOTO 400                 

	        IF(GNUM.NE.7) THEN          ! treat Lotto4 as Lotto3
                  IF(IDX(GNUM).EQ.0) THEN
                    IDX(GNUM) = SALIND
                    SALIND = SALIND + 2
                  ENDIF
	          TMP_GNUM = GNUM
	        ELSE
	          TMP_GNUM = 6
	        ENDIF

	        SALES(IDX(TMP_GNUM)) = SALES(IDX(TMP_GNUM))+ 
     *                          AGTGAM(GVCNT,GNUM,RTER) + 
     *                          AGTGAM(GRCNT,GNUM,RTER)
                
	        SALES(IDX(TMP_GNUM)+1) = SALES(IDX(TMP_GNUM)+1)+ 
     *                          AGTGAM(GVAMT,GNUM,RTER) +
     *                          AGTGAM(GRAMT,GNUM,RTER)
                
	        TMPCNT = TMPCNT + AGTGAM(GVCNT,GNUM,RTER) +
     *                            AGTGAM(GRCNT,GNUM,RTER)
	        CALL ADDI8I4(TMPAMT,AGTGAM(GVAMT,GNUM,RTER),VALUNIT)
		CALL ADDI8I4(TMPAMT,AGTGAM(GRAMT,GNUM,RTER),VALUNIT)
400	    CONTINUE
	    CDC=DAYCDC
	ENDIF


C
C IF SHOP OWNER GET EVERYONE OF HIS CLERKS ACCOUNTS
C
	IF(MANAGER)  THEN   !GET CLERKS ACCOUNTS ALSO
	    DO 420 CLERK=2,8
	        IF(AGTTAB((APSNUM+CLERK)-1,RTER).EQ.0) GOTO 420
	        SALIND = 1
                CALL FASTSET(0,IDX,MAXGAM)
	        DO 410 GNUM=1,MAXGAM
	            GTYP=GNTTAB(GAMTYP,GNUM)
	            GIND=GNTTAB(GAMIDX,GNUM)
                    IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 410
                    IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 410                
                    IF(GTYP.EQ.TINS) GOTO 410

	            IF(GNUM.NE.7) THEN
                      IF(IDX(GNUM).EQ.0) THEN
                        IDX(GNUM) = SALIND
                        SALIND = SALIND + 2
                      ENDIF
	              TMP_GNUM = GNUM
	            ELSE
	              TMP_GNUM = 6
	            ENDIF

	            SALES(IDX(TMP_GNUM)) = SALES(IDX(TMP_GNUM))+
     *                              CLRKDAY(GVCNT,GNUM,CLERK) +
     *                              CLRKDAY(GRCNT,GNUM,CLERK)
                    
                    SALES(IDX(TMP_GNUM)+1) = SALES(IDX(TMP_GNUM)+1)+
     *                              CLRKDAY(GVAMT,GNUM,CLERK) +
     *                              CLRKDAY(GRAMT,GNUM,CLERK)
                    
	            TMPCNT = TMPCNT + CLRKDAY(GVCNT,GNUM,CLERK) +
     *                                CLRKDAY(GRCNT,GNUM,CLERK)
	            CALL ADDI8I4(TMPAMT,CLRKDAY(GVAMT,GNUM,CLERK),VALUNIT)
		    CALL ADDI8I4(TMPAMT,CLRKDAY(GRAMT,GNUM,CLERK),VALUNIT)
410	        CONTINUE
420	    CONTINUE
	ENDIF
C
C IF WEEK TO DATE ALSO ACCUMULATE WEEK TO DATE
C
	IF(SUBCLASS.EQ.8) THEN
	    DO 440 I= 1,9
	        IF(ASFDAT(ASFCDC,I).LE.ASFINV(ASFEND,1)) GOTO 440
                SALIND = 1
                CALL FASTSET(0,IDX,MAXGAM)
	        DO 430 GNUM=1,MAXGAM
		    GTYP=GNTTAB(GAMTYP,GNUM)
	            GIND=GNTTAB(GAMIDX,GNUM)
		    IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 430
      		    IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 430
                    IF(GTYP.EQ.TINS) GOTO 430

	            IF(GNUM.NE.7) THEN
                      IF(IDX(GNUM).EQ.0) THEN
                        IDX(GNUM) = SALIND
                        SALIND = SALIND + 2
                      ENDIF
	              TMP_GNUM = GNUM
	            ELSE
	              TMP_GNUM = 6
	            ENDIF

		    SALES(IDX(TMP_GNUM)) = SALES(IDX(TMP_GNUM))+ 
     *                              ASFDAY(GVCNT,GNUM,I) +
     *                              ASFDAY(GRCNT,GNUM,I)
                   
		    SALES(IDX(TMP_GNUM)+1) = SALES(IDX(TMP_GNUM)+1)+ 
     *                              ASFDAY(GVAMT,GNUM,I) +
     *                              ASFDAY(GRAMT,GNUM,I)
                    TMPCNT = TMPCNT + ASFDAY(GVCNT,GNUM,I) +
     *                                ASFDAY(GRCNT,GNUM,I)
		    CALL ADDI8I4(TMPAMT,ASFDAY(GVAMT,GNUM,I),VALUNIT)
		    CALL ADDI8I4(TMPAMT,ASFDAY(GRAMT,GNUM,I),VALUNIT)
430	        CONTINUE
440	    CONTINUE
	    CDC=DAYCDC
	ENDIF
C
C PROCESS FOR DAY REQUESTED
C
	IF(SUBCLASS.GE.1.AND.SUBCLASS.LE.7) THEN
	    DO I= 1,9
	        IF(ASFDAT(ASFDOW,I).EQ.SUBCLASS) GOTO 460
            END DO
	    TRABUF(TERR)=INVL
	    GOTO 8000
460	    CONTINUE

            SALIND = 1
            CALL FASTSET(0,IDX,MAXGAM)
	    DO 470 GNUM=1,MAXGAM
	        GTYP=GNTTAB(GAMTYP,GNUM)
	        GIND=GNTTAB(GAMIDX,GNUM)
	        IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 470
                IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 470
                IF(GTYP.EQ.TINS) GOTO 470

	        IF(GNUM.NE.7) THEN
                  IF(IDX(GNUM).EQ.0) THEN
                    IDX(GNUM) = SALIND
                    SALIND = SALIND + 2
                  ENDIF
	          TMP_GNUM = GNUM
	        ELSE
	          TMP_GNUM = 6
	        ENDIF

	        SALES(IDX(TMP_GNUM)) = SALES(IDX(TMP_GNUM))+
     *                          ASFDAY(GVCNT,GNUM,I) +
     *                          ASFDAY(GRCNT,GNUM,I)
                
	        SALES(IDX(TMP_GNUM)+1) = SALES(IDX(TMP_GNUM)+1)+
     *                          ASFDAY(GVAMT,GNUM,I) +
     *                          ASFDAY(GRAMT,GNUM,I)
	        TMPCNT=TMPCNT+ASFDAY(GVCNT,GNUM,I) +
     *                        ASFDAY(GRCNT,GNUM,I)
	        CALL ADDI8I4(TMPAMT,ASFDAY(GVAMT,GNUM,I),VALUNIT)
		CALL ADDI8I4(TMPAMT,ASFDAY(GRAMT,GNUM,I),VALUNIT)
470	    CONTINUE
	    CDC=ASFDAT(ASFCDC,I)
	ENDIF
C
C SET TOTALS FOR INSTANT VALIDATIONS FOR REQUEST TERMINAL
C
       CALL SALVALINS(SALES, SALIND, RTER, SUBCLASS, MANAGER)
C
C SET TOTALS FOR INSTANTS VALIDATIONS
C
       TMPCNT = TMPCNT + SALES(SALIND + 0)
       CALL ADDI8I4(TMPAMT, SALES(SALIND + 1), VALUNIT)
       SALIND = SALIND + 2
C
C BUILD REPORT BACK TO TERMINAL
C
       SALES(SALIND) = TMPCNT
       SALES(SALIND + 1) = TMPAMT(2)
       SALES(SALIND + 2) = TMPAMT(1)
       SALIND = SALIND + 2
       SALOFF = SALIND
8000   CONTINUE
C
       RETURN

       END


C----+------------------------------------------------------------------
C V12| Correction of SALVAL for Portugal
C----+------------------------------------------------------------------
        SUBROUTINE SALVAL(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SPECOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'

        ! arguments
        INTEGER*4   SALES(*)          !
        INTEGER*4   SUBCLASS          !
        INTEGER*4   SALOFF            !
        INTEGER*4   CDC               !
        INTEGER*4   RTER              !

        LOGICAL     MANAGER           !

        ! variables                   !
        INTEGER*4   SALIND            !
        INTEGER*4   GNUM              !
        INTEGER*4   GTYP              !
        INTEGER*4   CLERK             !
        INTEGER*4   I                 !
        INTEGER*4   TMPCNT/0/         !TEMP. TOTAL COUNT, UNTIL INDEX COMPUTED
        INTEGER*4   TMPAMT(2)/0,0/    !TEMP. TOTAL AMOUNT UNTIL INDEX COMPUTED
        INTEGER*4   GIND              !
        INTEGER*4   TMP_GNUM          ! used to sum Lotto4 cnt/amt to Lotto3
C
        INTEGER*4  IDX(MAXGAM)

        ! Auxiliary variables for 8-byte integer
        INTEGER*8 I8TMPAMT
        INTEGER*4 I4TMPCNT

        !functions
        LOGICAL   NO_JOK_DET
        EXTERNAL  NO_JOK_DET

        I8TMPAMT = KZEXT(0)
        I4TMPCNT = 0

C
C SET / CLEAR VARIABLES
C
        TMPCNT = 0
        TMPAMT(1) = 0
        TMPAMT(2) = 0
  
        SALIND   = 1

C
C PROCESS ONLY ONLINE VALIDATIONS REPORT
C
        IF(SUBCLASS.EQ.0.OR.SUBCLASS.EQ.8) THEN  !TODAY OR W. T. D.
            SALIND = 1
            CALL FASTSET(0,IDX,MAXGAM)
            DO 400 GNUM=1,MAXGAM
                GTYP=GNTTAB(GAMTYP,GNUM)
                GIND=GNTTAB(GAMIDX,GNUM)
                IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 400
                IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 400
                IF(GTYP.EQ.TINS) GOTO 400                 
                IF(GTYP.EQ.TKIK .AND. NO_JOK_DET()) GOTO 400

                IF(GNUM.NE.7) THEN          ! treat Lotto4 as Lotto3
                  IF(IDX(GNUM).EQ.0) THEN
                    IDX(GNUM) = SALIND
                    SALIND = SALIND + 2
                  ENDIF
                  TMP_GNUM = GNUM
                ELSE
                  TMP_GNUM = 6
                ENDIF

                SALES(IDX(TMP_GNUM)) = SALES(IDX(TMP_GNUM))+ 
     *                          AGTGAM(GVCNT,GNUM,RTER) + 
     *                          AGTGAM(GRCNT,GNUM,RTER)
                
                SALES(IDX(TMP_GNUM)+1) = SALES(IDX(TMP_GNUM)+1)+ 
     *                          AGTGAM(GVAMT,GNUM,RTER) +
     *                          AGTGAM(GRAMT,GNUM,RTER)
                
                TMPCNT = TMPCNT + AGTGAM(GVCNT,GNUM,RTER) +
     *                            AGTGAM(GRCNT,GNUM,RTER)
C               CALL ADDI8I4(TMPAMT,AGTGAM(GVAMT,GNUM,RTER),VALUNIT)
C               CALL ADDI8I4(TMPAMT,AGTGAM(GRAMT,GNUM,RTER),VALUNIT)
                CALL I8ADDI8I4(I8TMPAMT,
     *                         I8TMPAMT,AGTGAM(GVAMT,GNUM,RTER))
                CALL I8ADDI8I4(I8TMPAMT,
     *                         I8TMPAMT,AGTGAM(GRAMT,GNUM,RTER))

400         CONTINUE
            CDC=DAYCDC
        ENDIF


C
C IF SHOP OWNER GET EVERYONE OF HIS CLERKS ACCOUNTS
C
        IF(MANAGER)  THEN   !GET CLERKS ACCOUNTS ALSO
            DO 420 CLERK=2,8
                IF(AGTTAB((APSNUM+CLERK)-1,RTER).EQ.0) GOTO 420
                SALIND = 1
                CALL FASTSET(0,IDX,MAXGAM)
                DO 410 GNUM=1,MAXGAM
                    GTYP=GNTTAB(GAMTYP,GNUM)
                    GIND=GNTTAB(GAMIDX,GNUM)
                    IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 410
                    IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 410                
                    IF(GTYP.EQ.TINS) GOTO 410
                    IF(GTYP.EQ.TKIK .AND. NO_JOK_DET()) GOTO 410

                    IF(GNUM.NE.7) THEN
                      IF(IDX(GNUM).EQ.0) THEN
                        IDX(GNUM) = SALIND
                        SALIND = SALIND + 2
                      ENDIF
                      TMP_GNUM = GNUM
                    ELSE
                      TMP_GNUM = 6
                    ENDIF

                    SALES(IDX(TMP_GNUM)) = SALES(IDX(TMP_GNUM))+
     *                              CLRKDAY(GVCNT,GNUM,CLERK) +
     *                              CLRKDAY(GRCNT,GNUM,CLERK)
                    
                    SALES(IDX(TMP_GNUM)+1) = SALES(IDX(TMP_GNUM)+1)+
     *                              CLRKDAY(GVAMT,GNUM,CLERK) +
     *                              CLRKDAY(GRAMT,GNUM,CLERK)
                    
                    TMPCNT = TMPCNT + CLRKDAY(GVCNT,GNUM,CLERK) +
     *                                CLRKDAY(GRCNT,GNUM,CLERK)
C                   CALL ADDI8I4(TMPAMT,CLRKDAY(GVAMT,GNUM,CLERK),VALUNIT)
C                   CALL ADDI8I4(TMPAMT,CLRKDAY(GRAMT,GNUM,CLERK),VALUNIT)
                    CALL I8ADDI8I4(I8TMPAMT,
     *                         I8TMPAMT,CLRKDAY(GVAMT,GNUM,CLERK))
                    CALL I8ADDI8I4(I8TMPAMT,
     *                         I8TMPAMT,CLRKDAY(GRAMT,GNUM,CLERK))
410             CONTINUE
420         CONTINUE
        ENDIF
C
C IF WEEK TO DATE ALSO ACCUMULATE WEEK TO DATE
C
        IF(SUBCLASS.EQ.8) THEN
            DO 440 I= 1,9
                IF(ASFDAT(ASFCDC,I).LE.ASFINV(ASFEND,1)) GOTO 440
                SALIND = 1
                CALL FASTSET(0,IDX,MAXGAM)
                DO 430 GNUM=1,MAXGAM
                    GTYP=GNTTAB(GAMTYP,GNUM)
                    GIND=GNTTAB(GAMIDX,GNUM)
                    IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 430
                    IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 430
                    IF(GTYP.EQ.TINS) GOTO 430
                    IF(GTYP.EQ.TKIK .AND. NO_JOK_DET()) GOTO 430

                    IF(GNUM.NE.7) THEN
                      IF(IDX(GNUM).EQ.0) THEN
                        IDX(GNUM) = SALIND
                        SALIND = SALIND + 2
                      ENDIF
                      TMP_GNUM = GNUM
                    ELSE
                      TMP_GNUM = 6
                    ENDIF

                    SALES(IDX(TMP_GNUM)) = SALES(IDX(TMP_GNUM))+ 
     *                              ASFDAY(GVCNT,GNUM,I) +
     *                              ASFDAY(GRCNT,GNUM,I)
                   
                    SALES(IDX(TMP_GNUM)+1) = SALES(IDX(TMP_GNUM)+1)+ 
     *                              ASFDAY(GVAMT,GNUM,I) +
     *                              ASFDAY(GRAMT,GNUM,I)
                    TMPCNT = TMPCNT + ASFDAY(GVCNT,GNUM,I) +
     *                                ASFDAY(GRCNT,GNUM,I)
C                   CALL ADDI8I4(TMPAMT,ASFDAY(GVAMT,GNUM,I),VALUNIT)
C                   CALL ADDI8I4(TMPAMT,ASFDAY(GRAMT,GNUM,I),VALUNIT)
                    CALL I8ADDI8I4(I8TMPAMT,
     *                         I8TMPAMT,ASFDAY(GVAMT,GNUM,I))
                    CALL I8ADDI8I4(I8TMPAMT,
     *                         I8TMPAMT,ASFDAY(GRAMT,GNUM,I))
430             CONTINUE
440         CONTINUE
            CDC=DAYCDC
        ENDIF
C
C PROCESS FOR DAY REQUESTED
C
        IF(SUBCLASS.GE.1.AND.SUBCLASS.LE.7) THEN
            DO I= 1,9
                IF(ASFDAT(ASFDOW,I).EQ.SUBCLASS) GOTO 460
            END DO
            TRABUF(TERR)=INVL
            GOTO 8000
460         CONTINUE

            SALIND = 1
            CALL FASTSET(0,IDX,MAXGAM)
            DO 470 GNUM=1,MAXGAM
                GTYP=GNTTAB(GAMTYP,GNUM)
                GIND=GNTTAB(GAMIDX,GNUM)
                IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 470
                IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 470
                IF(GTYP.EQ.TINS) GOTO 470
                IF(GTYP.EQ.TKIK .AND. NO_JOK_DET()) GOTO 470

                IF(GNUM.NE.7) THEN
                  IF(IDX(GNUM).EQ.0) THEN
                    IDX(GNUM) = SALIND
                    SALIND = SALIND + 2
                  ENDIF
                  TMP_GNUM = GNUM
                ELSE
                  TMP_GNUM = 6
                ENDIF

                SALES(IDX(TMP_GNUM)) = SALES(IDX(TMP_GNUM))+
     *                          ASFDAY(GVCNT,GNUM,I) +
     *                          ASFDAY(GRCNT,GNUM,I)
                
                SALES(IDX(TMP_GNUM)+1) = SALES(IDX(TMP_GNUM)+1)+
     *                          ASFDAY(GVAMT,GNUM,I) +
     *                          ASFDAY(GRAMT,GNUM,I)
                TMPCNT=TMPCNT+ASFDAY(GVCNT,GNUM,I) +
     *                        ASFDAY(GRCNT,GNUM,I)
C               CALL ADDI8I4(TMPAMT,ASFDAY(GVAMT,GNUM,I),VALUNIT)
C               CALL ADDI8I4(TMPAMT,ASFDAY(GRAMT,GNUM,I),VALUNIT)
                CALL I8ADDI8I4(I8TMPAMT,
     *                         I8TMPAMT,ASFDAY(GVAMT,GNUM,I))
                CALL I8ADDI8I4(I8TMPAMT,
     *                         I8TMPAMT,ASFDAY(GRAMT,GNUM,I))
470         CONTINUE
            CDC=ASFDAT(ASFCDC,I)
        ENDIF
C
C SET TOTALS FOR INSTANT VALIDATIONS FOR REQUEST TERMINAL
C
       CALL SALVALINS(SALES, SALIND, RTER, SUBCLASS, MANAGER)
C
C SET TOTALS FOR INSTANTS VALIDATIONS
C
       TMPCNT = TMPCNT + SALES(SALIND + 0)
C      CALL ADDI8I4(TMPAMT, SALES(SALIND + 1), VALUNIT)
       CALL I8ADDI8I4(I8TMPAMT,
     *                         I8TMPAMT,SALES(SALIND + 1))
       SALIND = SALIND + 2
C
C BUILD REPORT BACK TO TERMINAL
C
       SALES(SALIND) = TMPCNT
C       SALES(SALIND + 1) = TMPAMT(2)
C      SALES(SALIND + 2) = TMPAMT(1)

       CALL I8ADDI8I8(SALES(SALIND+1),
     *                 SALES(SALIND+1),I8TMPAMT)
       I = SALES(SALIND + 1)
       SALES(SALIND + 1) = SALES(SALIND + 2)
       SALES(SALIND + 2) = I

       SALIND = SALIND + 2
       SALOFF = SALIND
8000   CONTINUE
C
       RETURN

       END
C----+------------------------------------------------------------------
C V12| Correction of SALVAL for Portugal
C----+------------------------------------------------------------------
