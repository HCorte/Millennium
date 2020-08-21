C SUBROUTINE SLIABLE
C
C V15 24-JAN-2011 RXK Redundant parenthesis removed.
C V14 24-FEB-2000 OXK SLIB & SPAY dimension fixed to NUMSPT
C V13 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C			Instant Pass Thru Phase 1
C V12 02 Sep 1994 HXK Merge of May,June RFSS batch 
C V11 27 Apr 1994 JXP COPY=0
C V10 21 Nov 1993 SXH FIX OVERFLOW
C V09 03 Nov 1993 HXK CHANGES FOR PITKA
C V08 18 Oct 1993 HXK 54 rdraw
C V07 17 Oct 1993 HXK replaced cmony with csmony
C V06 24 Aug 1993 SXH Fix for BALWRI CALL
C V05 23 Jun 1993 SXH Added fractions, ddebugging FORMAT statements
C V04 10 Jun 1993 SXH Released for Finland
C V03 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C V02 12-NOV-91 MTK INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO GENERATE LIABILITY REPORTS FOR ALL AVAILABLE SPORTS GAMES
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE SLIABLE(SLIB, SPAY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DSPREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
        
        ! parameters
	INTEGER*4  MDRAWS
        INTEGER*4  RDRAWS
	PARAMETER (MDRAWS=400)
	PARAMETER (RDRAWS=54)

C
        ! arguments
	INTEGER*4  SLIB(SPGDIV,MDRAWS,NUMSPT)
	INTEGER*4  SPAY(SPGDIV,MDRAWS,NUMSPT)

C       ! variables

        INTEGER*4 TOTWON(SPGDIV),           !
     *            STOTWON(2)                !1st index=amount in I8,
        INTEGER*4 GRNWON(2,SPGDIV+1)        !

        INTEGER*4 TOTPAD(SPGDIV),          !
     *            STOTPAD(2)               !
        INTEGER*4 GRNPAD(2,SPGDIV+1)       !

        INTEGER*4 TOTLIB(SPGDIV),          !
     *            STOTLIB(2)               !
        INTEGER*4 GRNLIB(2,SPGDIV+1)       !

        INTEGER*4 TOTPRG(SPGDIV),          !
     *            STOTPRG(2)               !
        INTEGER*4 GRNPRG(2,SPGDIV+1)       !

        INTEGER*4 TOTDAY(SPGDIV),          !
     *            STOTDAY(2)               !
        INTEGER*4 GRNDAY(2,SPGDIV+1)       !
 

	INTEGER*4  I 
	INTEGER*4  DRAW 
	INTEGER*4  DRWIND 
	INTEGER*4  LINCNT 
	INTEGER*4  PAGE 
	INTEGER*4  ST 
	INTEGER*4  K
	INTEGER*4  GNUM 
	INTEGER*4  GIND 
	INTEGER*4  COPY
	INTEGER*4  DSPFDB(7) 
	INTEGER*4  REPLU/7/
        INTEGER*4  RAPCODE
        INTEGER*4  GAMESUMS(MAXGAM,NUMFIN,NUMTOT)
        INTEGER*4  TOTSUMS(NO_BALSUMS)

        REAL*8    TOTREAL,
     *            R_TOTDAY,     !TOTAL OF PAID TODAY AMOUNTS OF ALL DIVISIONS,
C                               !AND PER DRAW (LAST COLUMN OF REPORT LAYOUT)
     *            R_GRNDAY      !GRAND TOTAL OF PAID TODAY AMOUNTS.
 

	INTEGER*2  DATE(LDATE_LEN) /LDATE_LEN*0/
C
	CHARACTER  STATUS*17
	CHARACTER  HEAD(9)*5
	CHARACTER  REPHDR*45
	CHARACTER  REPNAM*13
C
	DATA      HEAD/'DIV 1','DIV 2','DIV 3','DIV 4','DIV 5',
     *	               'DIV 6','DIV 7','DIV 8','TOTAL'/


        ! function
        INTEGER*4  FRAMT


C BEGIN CODE -----------------------------------------
C
C
C
	DATE(5) = DAYCDC
	CALL LCDATE(DATE)
C
C
	DO 1000  GIND = 1, NUMSPT
	    GNUM=GTNTAB(TSPT,GIND)
	    IF(GNUM.LT.1) GOTO 1000
	    IF(DAYHDR(GNUM).LT.1) GOTO 1000

            WRITE(6,8000) IAM(),(GLNAMES(K,GNUM),K=1,4)
C	    CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)
	    COPY=0
C
C
	    CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	    CALL IOINIT(DSPFDB,3,DSPSEC*256)
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

            CALL FASTSET(0,GRNWON,2*(SPGDIV+1))
            CALL FASTSET(0,GRNPAD,2*(SPGDIV+1))
            CALL FASTSET(0,GRNLIB,2*(SPGDIV+1))
            CALL FASTSET(0,GRNPRG,2*(SPGDIV+1))
            CALL FASTSET(0,GRNDAY,2*(SPGDIV+1))

            R_GRNDAY = 0.0D0
 




	   DO 300 DRWIND = 1, RDRAWS
	       DRAW = DAYHDR(GNUM) - DRWIND + 1
	       IF(DRAW.LT.1) GOTO 310
	       CALL READW(DSPFDB,DRAW,DSPREC,ST)
	       IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
	       IF(DSPUPD.GT.DAYCDC) GOTO 300


                CALL FASTSET(0,TOTWON,SPGDIV)
                CALL FASTSET(0,TOTPAD,SPGDIV)
                CALL FASTSET(0,TOTLIB,SPGDIV)
                CALL FASTSET(0,TOTPRG,SPGDIV)
                CALL FASTSET(0,TOTDAY,SPGDIV)
                R_TOTDAY = 0.0D0
   
                CALL FASTSET(0,STOTWON,2)
                CALL FASTSET(0,STOTPAD,2)
                CALL FASTSET(0,STOTLIB,2)
                CALL FASTSET(0,STOTPRG,2)
                CALL FASTSET(0,STOTDAY,2)



	       DO K = 1, DSPDIV
	           TOTWON(K) = DSPSHR(K)
	           TOTLIB(K) = SLIB(K,DRWIND,GIND)
	           TOTPAD(K) = DSPPAD(K)
	           IF(DAYCDC.EQ.DSPUPD)
     *               TOTPAD(K)=TOTPAD(K)-SPAY(K,DRWIND,GIND)
	           TOTWON(K) = TOTWON(K)*DSPSHV(K)
	           TOTDAY(K) = SPAY(K,DRWIND,GIND)
	           TOTLIB(K) = FRAMT(MAXFRC(GNUM),TOTLIB(K),DSPSHV(K))
	           TOTPAD(K) = FRAMT(MAXFRC(GNUM),TOTPAD(K),DSPSHV(K))
	           TOTDAY(K) = FRAMT(MAXFRC(GNUM),TOTDAY(K),DSPSHV(K))
	           TOTPRG(K) = TOTWON(K)-TOTLIB(K)-
     *                         TOTPAD(K)-TOTDAY(K)


		   CALL ADDI8I4 (STOTWON ,TOTWON(K),VALUNIT)
		   CALL ADDI8I4 (STOTPAD ,TOTPAD(K),VALUNIT)
		   CALL ADDI8I4 (STOTLIB ,TOTLIB(K),VALUNIT)
		   CALL ADDI8I4 (STOTPRG ,TOTPRG(K),VALUNIT)
		   CALL ADDI8I4 (STOTDAY ,TOTDAY(K),VALUNIT)
		   R_TOTDAY = R_TOTDAY + DFLOAT (TOTDAY(K))
               END DO

	       IF(LINCNT.GT.LINSPP) THEN
	           CALL TITLE(REPHDR,REPNAM,1,REPLU,PAGE,DAYCDC)
	           WRITE(REPLU,9000)
	           LINCNT=7
	       ENDIF

	       LINCNT=LINCNT+7
	       STATUS = '                 '

	       WRITE(REPLU,9001)  DRAW,(HEAD(I),I=1,DSPDIV),HEAD(9)
	       WRITE(REPLU,8003)  STATUS

                    WRITE(REPLU,9002)  (CSMONY(TOTWON(I),13,VALUNIT),
     *                                  I = 1, DSPDIV),
     *                                  CSMONYI8 (STOTWON(1),13,VALUNIT)

                    WRITE(REPLU,9003)  (CSMONY(TOTPAD(I),13,VALUNIT),
     *                                  I = 1, DSPDIV),
     *                                  CSMONYI8 (STOTPAD(1),13,VALUNIT)

                    WRITE(REPLU,9004)  (CSMONY(TOTDAY(I),13,VALUNIT),
     *                                  I = 1, DSPDIV),
     *                                  CSMONYI8 (STOTDAY(1),13,VALUNIT)

                    WRITE(REPLU,9005)  (CSMONY(TOTPRG(I),13,VALUNIT),
     *                                  I = 1, DSPDIV),
     *                                  CSMONYI8 (STOTPRG(1),13,VALUNIT)

                    WRITE(REPLU,9006)  (CSMONY(TOTLIB(I),13,VALUNIT),
     *                                  I = 1, DSPDIV),
     *                                  CSMONYI8 (STOTLIB(1),13,VALUNIT)






C
                    DO I = 1, DSPDIV
                        CALL ADDI8I4 (GRNWON(1,I), TOTWON(I),VALUNIT)
                        CALL ADDI8I4 (GRNPAD(1,I), TOTPAD(I),VALUNIT)
                        CALL ADDI8I4 (GRNLIB(1,I), TOTLIB(I),VALUNIT)
                        CALL ADDI8I4 (GRNPRG(1,I), TOTPRG(I),VALUNIT)
                        CALL ADDI8I4 (GRNDAY(1,I), TOTDAY(I),VALUNIT)
                    END DO

                    CALL ADDI8I8 (GRNWON(1,DSPDIV+1), STOTWON(1),VALUNIT)
                    CALL ADDI8I8 (GRNPAD(1,DSPDIV+1), STOTPAD(1),VALUNIT)
                    CALL ADDI8I8 (GRNLIB(1,DSPDIV+1), STOTLIB(1),VALUNIT)
                    CALL ADDI8I8 (GRNPRG(1,DSPDIV+1), STOTPRG(1),VALUNIT)
                    CALL ADDI8I8 (GRNDAY(1,DSPDIV+1), STOTDAY(1),VALUNIT)
                    R_GRNDAY = R_GRNDAY + R_TOTDAY



300        CONTINUE
310        CONTINUE
C
C
	   LINCNT = LINCNT + 7
	   IF(LINCNT.GT.LINSPP) THEN
	       CALL TITLE(REPHDR,REPNAM,1,REPLU,PAGE,DAYCDC)
	       WRITE(REPLU,9000)
	       LINCNT=7
	   ENDIF

	   WRITE(REPLU,9007) (HEAD(K),K=1,DSPDIV),HEAD(9)

           WRITE(REPLU,9002) ( CMONYI8 (GRNWON(1,K),13,VALUNIT),K=1,DSPDIV+1 )
           WRITE(REPLU,9003) ( CMONYI8 (GRNPAD(1,K),13,VALUNIT),K=1,DSPDIV+1 )
           WRITE(REPLU,9004) ( CMONYI8 (GRNDAY(1,K),13,VALUNIT),K=1,DSPDIV+1 )
           WRITE(REPLU,9005) ( CMONYI8 (GRNPRG(1,K),13,VALUNIT),K=1,DSPDIV+1 )
           WRITE(REPLU,9006) ( CMONYI8 (GRNLIB(1,K),13,VALUNIT),K=1,DSPDIV+1 )




C
C
	   CALL USRCLOS1(     3)
	   CALL USRCLOS1(REPLU)

	   CALL SPOOL(REPNAM,COPY,STATUS)

C                                                                               
C GRAND TOTAL TO BALANSFILE  
C                                                                               
C***       TOTREAL = GRNDAY(DSPDIV+1)                                             
           TOTREAL = R_GRNDAY

           RAPCODE = 80 + GNUM             
           CALL BALWRI(RAPCODE,GAMESUMS,TOTSUMS,TOTREAL)                          

1000    CONTINUE
C
C     ===================== Format Statements =================
C
8000	FORMAT(1X,A,' Generating ',4A4,' liability report')
8001	FORMAT('SPORTS ',I2,' LIABILITY REPORT FOR ',7A2)
8002	FORMAT('TB', I1, 'LIABLE.REP')
8003	FORMAT(1X,A)
C
9000	FORMAT(1X,131('='))
9001	FORMAT(/,1X,'DRAW   ',I5,2X,<DSPDIV+1>(9X,A5))
9002	FORMAT(1X,'TOTAL WON      ',<DSPDIV+1>(A13,1X))
9003	FORMAT(1X,'PREVIOUSLY PAID',<DSPDIV+1>(A13,1X))
9004	FORMAT(1X,'PAID TODAY     ',<DSPDIV+1>(A13,1X),3X,A13,1X,A13)
9005	FORMAT(1X,'PURGED/EXPIRED ',<DSPDIV+1>(A13,1X))
9006	FORMAT(1X,'OUTSTANDING    ',<DSPDIV+1>(A13,1X))
C
C9007	FORMAT(/,1X,'GRAND TOTALS',2X,<DSPDIV+1>(9X,A5),
C     *    'NET PAID        TAXES')
9007	FORMAT(/,1X,'GRAND TOTALS',2X,<DSPDIV+1>(9X,A5))
C
	RETURN
	END
