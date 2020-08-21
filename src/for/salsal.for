C
C SUBROUTINE SALSAL
C
C SALSAL.FOR
C
C V14 27-MAR-2017 HXK Omit Joker details if Joker removed > 90 days
C V13 01-JUL-2015 SCML Correction of SALSAL for Portugal
C V12 02-JAN-2011 HXK combine LOTTO3 AND LOTTO 4 AS ONE GAME (TOTOLOTO)
C V11 21-MAY-2010 RXK Changes for ePassive
C V10 05-APR-2001 ANG There is no passive sales, only devolutions.
C V09 17-MAY-1999 UXN Super Triple added.
C V08 23-NOV-1995 PXB Couple and Double games added
C V07 04-JAN-1994 HXK ACCUMULATE SCORE, WINNERS TIP.
C V06 16-AUG-1993 SXH Debugged
C V05 21-JUN-1993 HXK CHANGED FOR FINLAND VAX CONVERSION
C V04 10-JUN-1993 HXK Changed AGTINF.DEF, AGTCOM.DEF includes.
C V03 10-FEB-1993 EBD Changed sales dimension to *, so calling routine 
C                     dimensions the array
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 15-NOV-1991 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C
C CALLING SEQUENCE:
C     CALL SALSAL(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
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

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
C----+------------------------------------------------------------------
C V13| Correction of SALSAL for Portugal
C----+------------------------------------------------------------------
C       SUBROUTINE SALSAL(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
        SUBROUTINE SALSAL_OLD(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
C----+------------------------------------------------------------------
C V13| Correction of SALSAL for Portugal
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
	INTEGER*4   SALES(*)            !
	INTEGER*4   SUBCLASS            !
	INTEGER*4   SALOFF              !
	INTEGER*4   CDC                 !
	INTEGER*4   RTER                !

	LOGICAL	    MANAGER             !

        ! variables                     
	INTEGER*4   SALIND              !
	INTEGER*4   GNUM                !
	INTEGER*4   TMP_GNUM            ! used to accumulated Lotto4 with Lotto3
	INTEGER*4   GTYP                !
	INTEGER*4   CLERK               !
	INTEGER*4   I                   !
	INTEGER*4   GIND                !
	INTEGER*4   TMPCNT		!TEMP. TOTAL COUNT, UNTIL INDEX COMPUTED
	INTEGER*4   TMPAMT(2)		!TEMP. TOTAL AMOUNT UNTIL INDEX COMPUTED
        INTEGER*4   RETCNT(NUMPAS)
        INTEGER*4   RETAMT(NUMPAS)
C
        INTEGER*4   IDX(MAXGAM)
C
C SET / CLEAR VARIABLES
C
	TMPCNT = 0
	TMPAMT(1) = 0
	TMPAMT(2) = 0
        CALL FASTSET(0,RETCNT,NUMPAS)
        CALL FASTSET(0,RETAMT,NUMPAS)

        SALIND        = 1
C
C DRAW BREAKDOWN OPTION BYTE AND GTYP,GIND BYTES
C 

C
C PROCESS ONLY ONLINE SALES REPORT
C
	IF(SUBCLASS.EQ.0.OR.SUBCLASS.EQ.8) THEN  !TODAY OR W. T. D.
            SALIND = 1
            CALL FASTSET(0,IDX,MAXGAM)
	    DO 200 GNUM = 1,MAXGAM
	        GTYP = GNTTAB(GAMTYP,GNUM)
		GIND = GNTTAB(GAMIDX,GNUM)
	        IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 200
                IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 200

	        IF(GNUM.NE.7) THEN           ! use Lotto3 gnum (6) for Lotto4
                  IF(IDX(GNUM).EQ.0) THEN
                    IDX(GNUM) = SALIND
                    SALIND = SALIND + 2
                  ENDIF
	          TMP_GNUM = GNUM
	        ELSE
                  TMP_GNUM = 6
	        ENDIF 

	        SALES(IDX(TMP_GNUM)) = SALES(IDX(TMP_GNUM))+
     *                             AGTGAM(GSCNT,GNUM,RTER)-
     *	                           AGTGAM(GCCNT,GNUM,RTER)
	        TMPCNT = TMPCNT +  AGTGAM(GSCNT,GNUM,RTER)-
     *	                           AGTGAM(GCCNT,GNUM,RTER)
	        SALES(IDX(TMP_GNUM)+1) = SALES(IDX(TMP_GNUM)+1)+
     *                             AGTGAM(GSAMT,GNUM,RTER)-
     *	                           AGTGAM(GCAMT,GNUM,RTER)
                CALL ADDI8I4(TMPAMT,AGTGAM(GSAMT,GNUM,RTER),BETUNIT)
                CALL SUBI8I4(TMPAMT,AGTGAM(GCAMT,GNUM,RTER),BETUNIT)

		IF (GTYP.EQ.TPAS) THEN
	            RETCNT(GIND) = RETCNT(GIND) + AGTGAM(GCLCNT,GNUM,RTER)
                    RETAMT(GIND) = RETAMT(GIND) + AGTGAM(GCLAMT,GNUM,RTER) 
                ENDIF
200	    CONTINUE
	    CDC=DAYCDC
	ENDIF
C
C IF SHOP OWNER GET EVERYONE OF HIS CLERKS ACCOUNTS
C
	IF(MANAGER)  THEN   !GET CLERKS ACCOUNTS ALSO
	    DO 220 CLERK=2,8
	        IF(AGTTAB((APSNUM+CLERK)-1,RTER).EQ.0) GOTO 220
                SALIND = 1
                CALL FASTSET(0,IDX,MAXGAM)
	        DO 210 GNUM=1,MAXGAM
	            GTYP = GNTTAB(GAMTYP,GNUM)
	       	    GIND = GNTTAB(GAMIDX,GNUM)
	            IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 210
		    IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 210

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
     *                                 CLRKDAY(GSCNT,GNUM,CLERK)-
     *	                               CLRKDAY(GCCNT,GNUM,CLERK)
	            TMPCNT=TMPCNT+CLRKDAY(GSCNT,GNUM,CLERK)-
     *	                               CLRKDAY(GCCNT,GNUM,CLERK)
                    SALES(IDX(TMP_GNUM)+1) = SALES(IDX(TMP_GNUM)+1)+
     *                                 CLRKDAY(GSAMT,GNUM,CLERK)-
     *	                               CLRKDAY(GCAMT,GNUM,CLERK)

                    CALL ADDI8I4(TMPAMT,CLRKDAY(GSAMT,GNUM,CLERK),BETUNIT)
                    CALL SUBI8I4(TMPAMT,CLRKDAY(GCAMT,GNUM,CLERK),BETUNIT)

                    IF(GTYP.EQ.TPAS) THEN
                       RETCNT(GIND) = RETCNT(GIND) + CLRKDAY(GCLCNT,GNUM,CLERK)
                       RETAMT(GIND) = RETAMT(GIND) + CLRKDAY(GCLAMT,GNUM,CLERK)
                    ENDIF
210	        CONTINUE
220	    CONTINUE
	ENDIF


C
C IF WEEK TO DATE ALSO ACCUMULATE WEEK TO DATE
C
	IF(SUBCLASS.EQ.8) THEN
	    DO 240 I= 1,9
	        IF(ASFDAT(ASFCDC,I).LE.ASFINV(ASFEND,1)) GOTO 240
                SALIND = 1
                CALL FASTSET(0,IDX,MAXGAM)
	        DO 230 GNUM = 1,MAXGAM
	            GTYP = GNTTAB(GAMTYP,GNUM)
		    GIND = GNTTAB(GAMIDX,GNUM)
	            IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 230
                    IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 230

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
     *                                 ASFDAY(GSCNT,GNUM,I)-
     *	                               ASFDAY(GCCNT,GNUM,I)
             
	            TMPCNT = TMPCNT +  ASFDAY(GSCNT,GNUM,I)-
     *	                               ASFDAY(GCCNT,GNUM,I)
	            SALES(IDX(TMP_GNUM)+1) = SALES(IDX(TMP_GNUM)+1)+
     *                                 ASFDAY(GSAMT,GNUM,I)-
     *	                               ASFDAY(GCAMT,GNUM,I)
                    CALL ADDI8I4(TMPAMT,ASFDAY(GSAMT,GNUM,I),BETUNIT)
                    CALL SUBI8I4(TMPAMT,ASFDAY(GCAMT,GNUM,I),BETUNIT)

                    IF(GTYP.EQ.TPAS) THEN
                       RETCNT(GIND) = RETCNT(GIND) + ASFDAY(GCLCNT,GNUM,I)  
                       RETAMT(GIND) = RETAMT(GIND) + ASFDAY(GCLAMT,GNUM,I) 
                    ENDIF
230	        CONTINUE
240	    CONTINUE
	    CDC=DAYCDC
	ENDIF
C
C PROCESS FOR DAY REQUESTED
C
	IF(SUBCLASS.GE.1.AND.SUBCLASS.LE.7) THEN
	    DO I= 1,9
	        IF(ASFDAT(ASFDOW,I).EQ.SUBCLASS) GOTO 260
            END DO
	    TRABUF(TERR)=INVL
	    GOTO 8000
C
260	    CONTINUE

            SALIND=1
            CALL FASTSET(0,IDX,MAXGAM)
	    DO 270 GNUM=1,MAXGAM
	        GTYP = GNTTAB(GAMTYP,GNUM)
		GIND = GNTTAB(GAMIDX,GNUM)
	        IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 270
                IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 270

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
     *                             ASFDAY(GSCNT,GNUM,I)-
     *	                           ASFDAY(GCCNT,GNUM,I)
                SALES(IDX(TMP_GNUM)+1) = SALES(IDX(TMP_GNUM)+1)+
     *                             ASFDAY(GSAMT,GNUM,I)-
     *	                           ASFDAY(GCAMT,GNUM,I)
	        TMPCNT = TMPCNT +  ASFDAY(GSCNT,GNUM,I)-
     *	                           ASFDAY(GCCNT,GNUM,I)
                CALL ADDI8I4(TMPAMT,ASFDAY(GSAMT,GNUM,I),BETUNIT)
                CALL SUBI8I4(TMPAMT,ASFDAY(GCAMT,GNUM,I),BETUNIT)

	        IF(GTYP.EQ.TPAS) THEN
                   RETCNT(GIND) = RETCNT(GIND) + ASFDAY(GCLCNT,GNUM,I) 
                   RETAMT(GIND) = RETAMT(GIND) + ASFDAY(GCLAMT,GNUM,I)
		ENDIF
270	    CONTINUE
	    CDC=ASFDAT(ASFCDC,I)
	ENDIF
C
C BUILD SALES SECTION  BACK TO TERMINAL
C 
	SALES(SALIND)   = TMPCNT
	SALES(SALIND+1) = TMPAMT(2)
	SALES(SALIND+2) = TMPAMT(1)
	SALIND = SALIND + 3
C
C BUILD RETURNS SECTION BACK TO TERMINAL
C
        TMPCNT = 0
        TMPAMT(1) = 0
        DO GIND = 1,NUMPAS
           SALES(SALIND)   = RETCNT(GIND)
           SALES(SALIND+1) = RETAMT(GIND)
           SALIND = SALIND + 2
           TMPCNT = TMPCNT + RETCNT(GIND)
           TMPAMT(1) = TMPAMT(1) + RETAMT(GIND)
        ENDDO
        SALES(SALIND)   = TMPCNT
        SALES(SALIND+1) = TMPAMT(1)
        SALIND = SALIND + 2
   
	SALOFF = SALIND -1

8000	CONTINUE

	RETURN

	END
C----+------------------------------------------------------------------
C V13| Correction of SALSAL for Portugal
C----+------------------------------------------------------------------
        SUBROUTINE SALSAL(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)

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
        INTEGER*4   SALES(*)            !
        INTEGER*4   SUBCLASS            !
        INTEGER*4   SALOFF              !
        INTEGER*4   CDC                 !
        INTEGER*4   RTER                !

        LOGICAL     MANAGER             !

        ! variables                     
        INTEGER*4   SALIND              !
        INTEGER*4   GNUM                !
        INTEGER*4   TMP_GNUM            ! used to accumulated Lotto4 with Lotto3
        INTEGER*4   GTYP                !
        INTEGER*4   CLERK               !
        INTEGER*4   I                   !
        INTEGER*4   GIND                !
        INTEGER*4   TMPCNT              !TEMP. TOTAL COUNT, UNTIL INDEX COMPUTED
        INTEGER*4   TMPAMT(2)           !TEMP. TOTAL AMOUNT UNTIL INDEX COMPUTED
        INTEGER*4   RETCNT(NUMPAS)
        INTEGER*4   RETAMT(NUMPAS)
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
        
        CALL FASTSET(0,RETCNT,NUMPAS)
        CALL FASTSET(0,RETAMT,NUMPAS)

        SALIND        = 1
C
C DRAW BREAKDOWN OPTION BYTE AND GTYP,GIND BYTES
C 

C
C PROCESS ONLY ONLINE SALES REPORT
C
        IF(SUBCLASS.EQ.0.OR.SUBCLASS.EQ.8) THEN  !TODAY OR W. T. D.
            SALIND = 1
            CALL FASTSET(0,IDX,MAXGAM)
            DO 200 GNUM = 1,MAXGAM
                GTYP = GNTTAB(GAMTYP,GNUM)
                GIND = GNTTAB(GAMIDX,GNUM)
                IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 200
                IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 200

                IF(GTYP.EQ.TKIK.AND.NO_JOK_DET()) GOTO 200

                IF(GNUM.NE.7) THEN           ! use Lotto3 gnum (6) for Lotto4
                  IF(IDX(GNUM).EQ.0) THEN
                    IDX(GNUM) = SALIND
                    SALIND = SALIND + 2
                  ENDIF
                  TMP_GNUM = GNUM
                ELSE
                  TMP_GNUM = 6
                ENDIF 

                SALES(IDX(TMP_GNUM)) = SALES(IDX(TMP_GNUM))+
     *                             AGTGAM(GSCNT,GNUM,RTER)-
     *                             AGTGAM(GCCNT,GNUM,RTER)
                TMPCNT = TMPCNT +  AGTGAM(GSCNT,GNUM,RTER)-
     *                             AGTGAM(GCCNT,GNUM,RTER)
                SALES(IDX(TMP_GNUM)+1) = SALES(IDX(TMP_GNUM)+1)+
     *                             AGTGAM(GSAMT,GNUM,RTER)-
     *                             AGTGAM(GCAMT,GNUM,RTER)
C               CALL ADDI8I4(TMPAMT,AGTGAM(GSAMT,GNUM,RTER),BETUNIT)
C               CALL SUBI8I4(TMPAMT,AGTGAM(GCAMT,GNUM,RTER),BETUNIT)
                CALL I8ADDI8I4(I8TMPAMT,
     *                         I8TMPAMT,AGTGAM(GSAMT,GNUM,RTER))
                CALL I8SUBI8I4(I8TMPAMT,
     *                         I8TMPAMT,AGTGAM(GCAMT,GNUM,RTER))

                IF (GTYP.EQ.TPAS) THEN
                    RETCNT(GIND) = RETCNT(GIND) + AGTGAM(GCLCNT,GNUM,RTER)
                    RETAMT(GIND) = RETAMT(GIND) + AGTGAM(GCLAMT,GNUM,RTER) 
                ENDIF
200         CONTINUE
            CDC=DAYCDC
        ENDIF
C
C IF SHOP OWNER GET EVERYONE OF HIS CLERKS ACCOUNTS
C
        IF(MANAGER)  THEN   !GET CLERKS ACCOUNTS ALSO
            DO 220 CLERK=2,8
                IF(AGTTAB((APSNUM+CLERK)-1,RTER).EQ.0) GOTO 220
                SALIND = 1
                CALL FASTSET(0,IDX,MAXGAM)
                DO 210 GNUM=1,MAXGAM
                    GTYP = GNTTAB(GAMTYP,GNUM)
                    GIND = GNTTAB(GAMIDX,GNUM)
                    IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 210
                    IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 210

                    IF(GTYP.EQ.TKIK.AND.NO_JOK_DET()) GOTO 210

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
     *                                 CLRKDAY(GSCNT,GNUM,CLERK)-
     *                                 CLRKDAY(GCCNT,GNUM,CLERK)
                    TMPCNT=TMPCNT+CLRKDAY(GSCNT,GNUM,CLERK)-
     *                                 CLRKDAY(GCCNT,GNUM,CLERK)
                    SALES(IDX(TMP_GNUM)+1) = SALES(IDX(TMP_GNUM)+1)+
     *                                 CLRKDAY(GSAMT,GNUM,CLERK)-
     *                                 CLRKDAY(GCAMT,GNUM,CLERK)

C                   CALL ADDI8I4(TMPAMT,CLRKDAY(GSAMT,GNUM,CLERK),BETUNIT)
C                   CALL SUBI8I4(TMPAMT,CLRKDAY(GCAMT,GNUM,CLERK),BETUNIT)
                    CALL I8ADDI8I4(I8TMPAMT,
     *                         I8TMPAMT,CLRKDAY(GSAMT,GNUM,CLERK))
                    CALL I8SUBI8I4(I8TMPAMT,
     *                         I8TMPAMT,CLRKDAY(GCAMT,GNUM,CLERK))

                    IF(GTYP.EQ.TPAS) THEN
                       RETCNT(GIND) = RETCNT(GIND) + CLRKDAY(GCLCNT,GNUM,CLERK)
                       RETAMT(GIND) = RETAMT(GIND) + CLRKDAY(GCLAMT,GNUM,CLERK)
                    ENDIF
210             CONTINUE
220         CONTINUE
        ENDIF


C
C IF WEEK TO DATE ALSO ACCUMULATE WEEK TO DATE
C
        IF(SUBCLASS.EQ.8) THEN
            DO 240 I= 1,9
                IF(ASFDAT(ASFCDC,I).LE.ASFINV(ASFEND,1)) GOTO 240
                SALIND = 1
                CALL FASTSET(0,IDX,MAXGAM)
                DO 230 GNUM = 1,MAXGAM
                    GTYP = GNTTAB(GAMTYP,GNUM)
                    GIND = GNTTAB(GAMIDX,GNUM)
                    IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 230
                    IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 230

                    IF(GTYP.EQ.TKIK.AND.NO_JOK_DET()) GOTO 230

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
     *                                 ASFDAY(GSCNT,GNUM,I)-
     *                                 ASFDAY(GCCNT,GNUM,I)
             
                    TMPCNT = TMPCNT +  ASFDAY(GSCNT,GNUM,I)-
     *                                 ASFDAY(GCCNT,GNUM,I)
                    SALES(IDX(TMP_GNUM)+1) = SALES(IDX(TMP_GNUM)+1)+
     *                                 ASFDAY(GSAMT,GNUM,I)-
     *                                 ASFDAY(GCAMT,GNUM,I)
C                   CALL ADDI8I4(TMPAMT,ASFDAY(GSAMT,GNUM,I),BETUNIT)
C                   CALL SUBI8I4(TMPAMT,ASFDAY(GCAMT,GNUM,I),BETUNIT)

                    CALL I8ADDI8I4(I8TMPAMT,
     *                         I8TMPAMT,ASFDAY(GSAMT,GNUM,I))
                    CALL I8SUBI8I4(I8TMPAMT,
     *                         I8TMPAMT,ASFDAY(GCAMT,GNUM,I))

                    IF(GTYP.EQ.TPAS) THEN
                       RETCNT(GIND) = RETCNT(GIND) + ASFDAY(GCLCNT,GNUM,I)  
                       RETAMT(GIND) = RETAMT(GIND) + ASFDAY(GCLAMT,GNUM,I) 
                    ENDIF
230             CONTINUE
240         CONTINUE
            CDC=DAYCDC
        ENDIF
C
C PROCESS FOR DAY REQUESTED
C
        IF(SUBCLASS.GE.1.AND.SUBCLASS.LE.7) THEN
            DO I= 1,9
                IF(ASFDAT(ASFDOW,I).EQ.SUBCLASS) GOTO 260
            END DO
            TRABUF(TERR)=INVL
            GOTO 8000
C
260         CONTINUE

            SALIND=1
            CALL FASTSET(0,IDX,MAXGAM)
            DO 270 GNUM=1,MAXGAM
                GTYP = GNTTAB(GAMTYP,GNUM)
                GIND = GNTTAB(GAMIDX,GNUM)
                IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 270
                IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 270

                IF(GTYP.EQ.TKIK.AND.NO_JOK_DET()) GOTO 270

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
     *                             ASFDAY(GSCNT,GNUM,I)-
     *                             ASFDAY(GCCNT,GNUM,I)
                SALES(IDX(TMP_GNUM)+1) = SALES(IDX(TMP_GNUM)+1)+
     *                             ASFDAY(GSAMT,GNUM,I)-
     *                             ASFDAY(GCAMT,GNUM,I)
                TMPCNT = TMPCNT +  ASFDAY(GSCNT,GNUM,I)-
     *                             ASFDAY(GCCNT,GNUM,I)
C               CALL ADDI8I4(TMPAMT,ASFDAY(GSAMT,GNUM,I),BETUNIT)
C               CALL SUBI8I4(TMPAMT,ASFDAY(GCAMT,GNUM,I),BETUNIT)

                CALL I8ADDI8I4(I8TMPAMT,
     *                         I8TMPAMT,ASFDAY(GSAMT,GNUM,I))
                CALL I8SUBI8I4(I8TMPAMT,
     *                         I8TMPAMT,ASFDAY(GCAMT,GNUM,I))

                IF(GTYP.EQ.TPAS) THEN
                   RETCNT(GIND) = RETCNT(GIND) + ASFDAY(GCLCNT,GNUM,I) 
                   RETAMT(GIND) = RETAMT(GIND) + ASFDAY(GCLAMT,GNUM,I)
                ENDIF
270         CONTINUE
            CDC=ASFDAT(ASFCDC,I)
        ENDIF
C
C BUILD SALES SECTION  BACK TO TERMINAL
C 
        SALES(SALIND)   = TMPCNT
C       SALES(SALIND+1) = TMPAMT(2)
C       SALES(SALIND+2) = TMPAMT(1)

        CALL I8ADDI8I8(SALES(SALIND+1),
     *                 SALES(SALIND+1),I8TMPAMT)
        I = SALES(SALIND + 1)
        SALES(SALIND + 1) = SALES(SALIND + 2)
        SALES(SALIND + 2) = I

        SALIND = SALIND + 3
C
C BUILD RETURNS SECTION BACK TO TERMINAL
C
        TMPCNT = 0
        TMPAMT(1) = 0
        DO GIND = 1,NUMPAS
           SALES(SALIND)   = RETCNT(GIND)
           SALES(SALIND+1) = RETAMT(GIND)
           SALIND = SALIND + 2
           TMPCNT = TMPCNT + RETCNT(GIND)
           TMPAMT(1) = TMPAMT(1) + RETAMT(GIND)
        ENDDO
        SALES(SALIND)   = TMPCNT
        SALES(SALIND+1) = TMPAMT(1)
        SALIND = SALIND + 2
   
        SALOFF = SALIND -1

8000    CONTINUE

        RETURN

        END
C----+------------------------------------------------------------------
C V13| Correction of SALSAL for Portugal
C----+------------------------------------------------------------------
