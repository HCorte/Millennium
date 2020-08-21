C
C SUBROUTINE KLIABLE
C
C KLIABLE.FOR
C
C V17 24-JAN-2011 RXK Redundant parenthesis removed.
C V16 06-APR-1999 UXN Fix for old/new JOKERI.
C V15 05-AUG-1998 RXK Changed to dispaly 2 kickers
C V14 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting, 
C                     Instant Pass Thru Phase 1
C V13 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V12 27-APR-1994 JXP COPY=0
C V11 21-NOV-1993 SXH CHANGE CSMONYI8 TO CMONYI8
C V10 21-NOV-1993 SXH No change.
C V09 21-NOV-1993 SXH FIX OVERFLOW 
C V08 18-OCT-1993 HXK 54 rdraw
C V07 17-OCT-1993 HXK replaced cmony with csmony
C V06 24-AUG-1993 SXH Fix for BALWRI CALL
C V05 23-JUN-1993 SXH Added fractions, debugging FORMAT statements
C V04 07-JUN-1993 SXH released for Finland
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 12-NOV-1991 MTK INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C
C SUBROUTINE TO GENERATE LIABILITY REPORTS FOR ALL
C AVAILABLE KICKER GAMES
C
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE KLIABLE(KLIB, KPAY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*4  MDRAWS                       !
        INTEGER*4  RDRAWS                       !
	PARAMETER (MDRAWS=400)
	PARAMETER (RDRAWS=54)
C
        ! arguments
	INTEGER*4  KLIB(KIGDIV,MDRAWS,NUMKIK)   !
	INTEGER*4  KPAY(KIGDIV,MDRAWS,NUMKIK)   !

        ! variables
        INTEGER*4 TOTWON(KIGDIV),           !
     *            STOTWON(2)                !amount in I8
        INTEGER*4 GRNWON(2,KIGDIV+1)        !

        INTEGER*4 TOTPAD(KIGDIV),          !
     *            STOTPAD(2)               !
        INTEGER*4 GRNPAD(2,KIGDIV+1)       !

        INTEGER*4 TOTLIB(KIGDIV),          !
     *            STOTLIB(2)               !
        INTEGER*4 GRNLIB(2,KIGDIV+1)       !

        INTEGER*4 TOTPRG(KIGDIV),          !
     *            STOTPRG(2)               !
        INTEGER*4 GRNPRG(2,KIGDIV+1)       !

        INTEGER*4 TOTDAY(KIGDIV),          !
     *            STOTDAY(2)               !
        INTEGER*4 GRNDAY(2,KIGDIV+1)       !
   


	INTEGER*4  I                            !
	INTEGER*4  DRAW                         !
	INTEGER*4  DRWIND                       !
	INTEGER*4  LINCNT                       !
	INTEGER*4  PAGE                         !
	INTEGER*4  ST                           !
	INTEGER*4  K                            !
	INTEGER*4  GNUM                         !
	INTEGER*4  GIND                         !
	INTEGER*4  COPY                         !
C
	INTEGER*4  DKKFDB(7)                    !
	INTEGER*4  REPLU/7/                     !
                                                
        INTEGER*4 RAPCODE                       !
        INTEGER*4 GAMESUMS(MAXGAM,NUMFIN,NUMTOT)!
        INTEGER*4 TOTSUMS(NO_BALSUMS)           !

        REAL*8    TOTREAL,
     *            R_TOTDAY,     !TOTAL OF PAID TODAY AMOUNTS OF ALL DIVISIONS,
C                               !AND PER DRAW (LAST COLUMN OF REPORT LAYOUT)
     *            R_GRNDAY      !GRAND TOTAL OF PAID TODAY AMOUNTS.

        INTEGER*2  DATE(LDATE_LEN) /LDATE_LEN*0/     !
C
	CHARACTER  STATUS*17                    !
	CHARACTER  HEAD(9)*5                    !
	CHARACTER  REPHDR*45                    !
	CHARACTER  REPNAM*13                    !


        ! function
        INTEGER*4  FRAMT

C
	DATA      HEAD/'DIV 1','DIV 2','DIV 3','DIV 4','DIV 5',
     *	               'DIV 6','DIV 7','DIV 8','TOTAL'/


C BEGIN CODE ---------------------------------------------------
C
C
	DATE(5) = DAYCDC
	CALL LCDATE(DATE)
C
C
	DO 1000  GIND =1, NUMKIK
	    GNUM=GTNTAB(TKIK,GIND)
	    IF(GNUM.LT.1) GOTO 1000

	    IF(DAYHDR(GNUM).LT.1) GOTO 1000

            WRITE(5,8000) IAM(),(GLNAMES(K,GNUM),K=1,4)

C	    CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)
	    COPY=0
C
C
	    CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	    CALL IOINIT(DKKFDB,3,DKKSEC*256)
	    IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	    WRITE (REPHDR,8001) GIND,(DATE(K),K=7,13)
	    WRITE (REPNAM,8002) GIND
	    CALL ROPEN(REPNAM,REPLU,ST)
	    IF(ST.NE.0) THEN
	        TYPE*,IAM(),REPNAM,' report file open error > ',ST
	        CALL GPAUSE
	    ENDIF
C
C CLEAR TOTALS
C
	    PAGE=0
	    LINCNT=70


            CALL FASTSET(0,GRNWON,2*(KIGDIV+1))
            CALL FASTSET(0,GRNPAD,2*(KIGDIV+1))
            CALL FASTSET(0,GRNLIB,2*(KIGDIV+1))
            CALL FASTSET(0,GRNPRG,2*(KIGDIV+1))
            CALL FASTSET(0,GRNDAY,2*(KIGDIV+1))

            R_GRNDAY = 0.0D0
  

	    DO 300 DRWIND = 1, RDRAWS
	        DRAW=DAYHDR(GNUM)-DRWIND+1
	        IF(DRAW.LT.1) GOTO 310

	        CALL READW(DKKFDB,DRAW,DKKREC,ST)
	        IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
	        IF(DKKUPD.GT.DAYCDC) GOTO 300


20              CONTINUE

                CALL FASTSET(0,TOTWON,KIGDIV)
                CALL FASTSET(0,TOTPAD,KIGDIV)
                CALL FASTSET(0,TOTLIB,KIGDIV)
                CALL FASTSET(0,TOTPRG,KIGDIV)
                CALL FASTSET(0,TOTDAY,KIGDIV)
                R_TOTDAY = 0.0D0

                CALL FASTSET(0,STOTWON,2)
                CALL FASTSET(0,STOTPAD,2)
                CALL FASTSET(0,STOTLIB,2)
                CALL FASTSET(0,STOTPRG,2)
                CALL FASTSET(0,STOTDAY,2)

  
	           DO  K = 1, DKKDIV
	              TOTWON(K)=TOTWON(K)+DKKSHR(K)
	              TOTLIB(K) = KLIB(K,DRWIND,GIND)
	              TOTPAD(K) = DKKPAD(K)
	              IF(DAYCDC.EQ.DKKUPD)
     *                   TOTPAD(K) = TOTPAD(K)-KPAY(K,DRWIND,GIND)
	              TOTWON(K) = TOTWON(K)*DKKSHV(K)
	              TOTDAY(K) = KPAY(K,DRWIND,GIND)
	              TOTLIB(K) = FRAMT(MAXFRC(GNUM),TOTLIB(K),DKKSHV(K))
	              TOTPAD(K) = FRAMT(MAXFRC(GNUM),TOTPAD(K),DKKSHV(K))
	              TOTDAY(K) = FRAMT(MAXFRC(GNUM),TOTDAY(K),DKKSHV(K))
	              TOTPRG(K) = TOTWON(K)-TOTLIB(K)-
     *                          TOTPAD(K)-TOTDAY(K)
                   ENDDO 

	        DO  K = 1, DKKDIV
                    CALL ADDI8I4 (STOTWON ,TOTWON(K),VALUNIT)
                    CALL ADDI8I4 (STOTPAD ,TOTPAD(K),VALUNIT)
                    CALL ADDI8I4 (STOTLIB ,TOTLIB(K),VALUNIT)
                    CALL ADDI8I4 (STOTPRG ,TOTPRG(K),VALUNIT)
                    CALL ADDI8I4 (STOTDAY ,TOTDAY(K),VALUNIT)
                    R_TOTDAY = R_TOTDAY + DFLOAT (TOTDAY(K))
                ENDDO

	        IF(LINCNT.GT.LINSPP) THEN
	            CALL TITLE(REPHDR,REPNAM,1,REPLU,PAGE,DAYCDC)
	            WRITE(REPLU,9000)
	            LINCNT=7
	        ENDIF

	        LINCNT=LINCNT+7
	        STATUS = '                 '
 	        WRITE(REPLU,9001)  DRAW,(HEAD(I),I=1,DKKDIV),HEAD(9)
	        WRITE(REPLU,8003)  STATUS


                    WRITE(REPLU,9002)  (CSMONY(TOTWON(I),13,VALUNIT),
     *                                  I = 1, DKKDIV),
     *                                  CSMONYI8 (STOTWON(1),13,VALUNIT)

                    WRITE(REPLU,9003)  (CSMONY(TOTPAD(I),13,VALUNIT),
     *                                  I = 1, DKKDIV),
     *                                  CSMONYI8 (STOTPAD(1),13,VALUNIT)

                    WRITE(REPLU,90041)  (CSMONY(TOTDAY(I),13,VALUNIT),
     *                                  I = 1, DKKDIV),
     *                                  CSMONYI8 (STOTDAY(1),13,VALUNIT)

                    WRITE(REPLU,9005)  (CSMONY(TOTPRG(I),13,VALUNIT),
     *                                  I = 1, DKKDIV),
     *                                  CSMONYI8 (STOTPRG(1),13,VALUNIT)

                    WRITE(REPLU,9006)  (CSMONY(TOTLIB(I),13,VALUNIT),
     *                                  I = 1, DKKDIV),
     *                                  CSMONYI8 (STOTLIB(1),13,VALUNIT)



                    DO I = 1, DKKDIV
                        CALL ADDI8I4 (GRNWON(1,I), TOTWON(I),VALUNIT)
                        CALL ADDI8I4 (GRNPAD(1,I), TOTPAD(I),VALUNIT)
                        CALL ADDI8I4 (GRNLIB(1,I), TOTLIB(I),VALUNIT)
                        CALL ADDI8I4 (GRNPRG(1,I), TOTPRG(I),VALUNIT)
                        CALL ADDI8I4 (GRNDAY(1,I), TOTDAY(I),VALUNIT)
                    END DO

                    CALL ADDI8I8 (GRNWON(1,DKKDIV+1), STOTWON(1),VALUNIT)
                    CALL ADDI8I8 (GRNPAD(1,DKKDIV+1), STOTPAD(1),VALUNIT)
                    CALL ADDI8I8 (GRNLIB(1,DKKDIV+1), STOTLIB(1),VALUNIT)
                    CALL ADDI8I8 (GRNPRG(1,DKKDIV+1), STOTPRG(1),VALUNIT)
                    CALL ADDI8I8 (GRNDAY(1,DKKDIV+1), STOTDAY(1),VALUNIT)
                    R_GRNDAY = R_GRNDAY + R_TOTDAY
 

300         CONTINUE
310	    CONTINUE
C
C
	    LINCNT = LINCNT + 7
	    IF(LINCNT.GT.LINSPP) THEN
	        CALL TITLE(REPHDR,REPNAM,1,REPLU,PAGE,DAYCDC)
	        WRITE(REPLU,9000)
	        LINCNT=7
	    ENDIF

            WRITE(REPLU,9007) (HEAD(K),K=1,DKKDIV),HEAD(9)
	    WRITE(REPLU,9002) (CMONYI8(GRNWON(1,K),13,VALUNIT),K=1,DKKDIV+1)
	    WRITE(REPLU,9003) (CMONYI8(GRNPAD(1,K),13,VALUNIT),K=1,DKKDIV+1)
	    WRITE(REPLU,90041)(CMONYI8(GRNDAY(1,K),13,VALUNIT),K=1,DKKDIV+1)
	    WRITE(REPLU,9005) (CMONYI8(GRNPRG(1,K),13,VALUNIT),K=1,DKKDIV+1)
	    WRITE(REPLU,9006) (CMONYI8(GRNLIB(1,K),13,VALUNIT),K=1,DKKDIV+1)
C
	    CALL USRCLOS1(     3)
	    CALL USRCLOS1(REPLU)

	    STATUS = '                 '
	    CALL SPOOL(REPNAM,COPY,STATUS)

            ! send GRAND TOTAL TO BALANSFILE
C***        TOTREAL = GRNDAY(KIGDIV+1)                   
            TOTREAL = R_GRNDAY

            RAPCODE = 80 + GNUM                   
            CALL BALWRI(RAPCODE,GAMESUMS,TOTSUMS,TOTREAL)

1000	CONTINUE
C
C     ===================== Format Statements =================
C
8000	FORMAT(1X,A,' Generating ',4A4,' liability report')
8001	FORMAT('JOKER  ',I2,' LIABILITY REPORT FOR ',7A2)
8002	FORMAT('JO', I1, 'LIABLE.REP')
8003	FORMAT(1X,A)
C
9000	FORMAT(1X,131('='))
9001	FORMAT(/,1X,'DRAW   ',I5,2X,<DKKDIV+1>(9X,A5))
9002	FORMAT(1X,'TOTAL WON      ',<DKKDIV+1>(A13,1X))
9003	FORMAT(1X,'PREVIOUSLY PAID',<DKKDIV+1>(A13,1X))
C9004	FORMAT(1X,'PAID TODAY     ',<DKKDIV+1>(A13,1X),3X,A13,1X,A13)
90041	FORMAT(1X,'PAID TODAY     ',<DKKDIV+1>(A13,1X))
9005	FORMAT(1X,'PURGED/EXPIRED ',<DKKDIV+1>(A13,1X))
9006	FORMAT(1X,'OUTSTANDING    ',<DKKDIV+1>(A13,1X))
C
9007	FORMAT(/,1X,'GRAND TOTALS',2X,<DKKDIV+1>(9X,A5))
C9010    FORMAT(/,1X,'NET PAID : ',A13,' TAXES : ',A13)
C
	RETURN

	END
