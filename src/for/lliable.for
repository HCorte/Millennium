C
C SUBROUTINE LLIABLE
C
C V11 24-JAN-2011 RXK Redundant parenthesis removed.
C V10 27 Apr 1994 JXP COPY=0
C V09 21 Nov 1993 SXH FIX OVERFLOW (USE REAL*8)
C V08 16 Nov 1993 HXN STORED TOTAL AMOUNTS IN 2*I4 INSTEAD OF I4.
C V07 03 Nov 1993 HXK NEW STUFF FOR PITKA
C V07 18 Oct 1993 HXK 54 rdraw
C V06 17 Oct 1993 HXK replaced cmony with csmony
C V05 24 Aug 1993 SXH Fix for BALWRI CALL
C V04 04 Jun 1993 SXH  Initial release for Finland
C V03 21-JAN-1993 DAB Initial Release Based on Netherlands Bible, 12/92,
C                     and Comm 1/93 update. DEC Baseline
C V02 12-NOV-1991 MTK INITIAL RELEASE FOR NETHERLANDS
C V02 12-NOV-1991 MTK INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C
C SUBROUTINE TO GENERATE LIABILITY REPORTS FOR NORMAL
C LOTTO 1 GAME
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE LLIABLE(LLIB, LPAY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
         INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DLTREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*4 MDRAWS                       !
	PARAMETER(MDRAWS=400)
        INTEGER*4 RDRAWS                       !
	PARAMETER(RDRAWS=54)

        ! arguments
	INTEGER*4 LLIB(LTGDIV,2,MDRAWS,NUMLTO) !
	INTEGER*4 LPAY(LTGDIV,2,MDRAWS,NUMLTO) !

        ! variables
C
	INTEGER*4 TOTWON(LTGDIV,2),           !
     *            STOTWON(2,2)               !1rst index=amount in I8,
C                                            !2nd  index=# bonus draws
	INTEGER*4 GRNWON(2,LTGDIV+1)         !

	INTEGER*4 TOTPAD(LTGDIV,2),          !
     *            STOTPAD(2,2)               !
	INTEGER*4 GRNPAD(2,LTGDIV+1)         !

	INTEGER*4 TOTLIB(LTGDIV,2),          !
     *            STOTLIB(2,2)               !
	INTEGER*4 GRNLIB(2,LTGDIV+1)         !

	INTEGER*4 TOTPRG(LTGDIV,2),          !
     *            STOTPRG(2,2)               !
	INTEGER*4 GRNPRG(2,LTGDIV+1)         !

	INTEGER*4 TOTDAY(LTGDIV,2),          !
     *            STOTDAY(2,2)               !
	INTEGER*4 GRNDAY(2,LTGDIV+1)         !

	INTEGER*4 I             !
	INTEGER*4 B             !
	INTEGER*4 DRAW          !
	INTEGER*4 DRWIND        !
	INTEGER*4 LINCNT        !
	INTEGER*4 PAGE          !
	INTEGER*4 ST            !
	INTEGER*4 K             !
	INTEGER*4 GNUM          !
	INTEGER*4 GIND          !
	INTEGER*4 COPY          !
	INTEGER*4 NUMB          !
C
	INTEGER*4 DLTFDB(7)     !
	INTEGER*4 REPLU/7/      !

        INTEGER*4 RAPCODE
        INTEGER*4 GAMESUMS(MAXGAM,NUMFIN,NUMTOT)
        INTEGER*4 TOTSUMS(NO_BALSUMS)

        REAL*8    TOTREAL,  
     *            R_TOTDAY,     !TOTAL OF PAID TODAY AMOUNTS OF ALL DIVISIONS,
C                               !AND PER DRAW (LAST COLUMN OF REPORT LAYOUT)
     *            R_GRNDAY(LTGDIV+1)      !GRAND TOTAL OF PAID TODAY AMOUNTS.

        REAL*8    R_TOTWON,R_GRNWON(LTGDIV+1)
        REAL*8    R_TOTPAD,R_GRNPAD(LTGDIV+1)
        REAL*8    R_TOTLIB,R_GRNLIB(LTGDIV+1)
        REAL*8    R_TOTPRG,R_GRNPRG(LTGDIV+1)


	INTEGER*2 DATE(LDATE_LEN) /LDATE_LEN*0/   !
C
	CHARACTER STATUS*17         !
	CHARACTER HEAD(9)*6         !
	CHARACTER REPHDR*40         !
	CHARACTER REPNAM * 13       ! REPORT NAME
C
	REAL*8  CNVAMT
C
        ! Functions
        INTEGER*4 FRAMT             !

	DATA      HEAD/'DIV  1','DIV  2','DIV  3','DIV  4','DIV  5',
     *	               'DIV  6','DIV  7','DIV  8',' TOTAL'/

C BEGIN CODE -----------------------------------------
C
C
	CNVAMT = DFLOAT(DOLL_BASE/DYN_BETUNIT)

	DATE(5) = DAYCDC
	CALL LCDATE(DATE)
C
C
	DO 1000  GIND=1,NUMLTO
	    GNUM=GTNTAB(TLTO,GIND)
	    IF(GNUM.LT.1) GOTO 1000
	    IF(DAYHDR(GNUM).LT.1) GOTO 1000

            WRITE(5,8000) IAM(),(GLNAMES(K,GNUM),K=1,4)
C	    CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)
	    COPY=0
C
C
	    CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	    CALL IOINIT(DLTFDB,3,DLTSEC*256)
	    IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	    WRITE (REPHDR,8001) GSNAMES(GNUM),(DATE(K),K=7,13)
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
	    CALL FASTSET(0,GRNWON,2*(LTGDIV+1))
	    CALL FASTSET(0,GRNPAD,2*(LTGDIV+1))
	    CALL FASTSET(0,GRNLIB,2*(LTGDIV+1))
	    CALL FASTSET(0,GRNPRG,2*(LTGDIV+1))
	    CALL FASTSET(0,GRNDAY,2*(LTGDIV+1))

            DO I = 1, LTGDIV+1
	        R_GRNDAY(I) = 0.0D0
	        R_GRNPAD(I) = 0.0D0
	        R_GRNLIB(I) = 0.0D0
	        R_GRNWON(I) = 0.0D0
	        R_GRNPRG(I) = 0.0D0
            END DO
C
	    DO 300 DRWIND=1,RDRAWS
	        DRAW = DAYHDR(GNUM) - DRWIND + 1
	        IF(DRAW.LT.1) GOTO 310

	        CALL READW(DLTFDB,DRAW,DLTREC,ST)
	        IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
	        IF(DLTUPD.GT.DAYCDC) GOTO 300
C
	        CALL FASTSET(0,TOTWON,LTGDIV*2)
	        CALL FASTSET(0,TOTPAD,LTGDIV*2)
	        CALL FASTSET(0,TOTLIB,LTGDIV*2)
	        CALL FASTSET(0,TOTPRG,LTGDIV*2)
	        CALL FASTSET(0,TOTDAY,LTGDIV*2)

	        CALL FASTSET(0,STOTWON,2*2)
	        CALL FASTSET(0,STOTPAD,2*2)
	        CALL FASTSET(0,STOTLIB,2*2)
	        CALL FASTSET(0,STOTPRG,2*2)
	        CALL FASTSET(0,STOTDAY,2*2)


C
	        NUMB = 1
	        IF(DLTBDR.NE.0) NUMB=2
	        DO 40 B = 1, NUMB

		    R_TOTDAY = 0.0D0
		    R_TOTPAD = 0.0D0
		    R_TOTLIB = 0.0D0
		    R_TOTWON = 0.0D0
		    R_TOTPRG = 0.0D0

	            DO 30 K = 1, DLTDIV
	                TOTWON(K,B) = DLTSHR(K,B)
	                TOTLIB(K,B) = LLIB(K,B,DRWIND,GIND)
	                TOTPAD(K,B) = DLTPAD(K,B)
	                IF (DAYCDC.EQ.DLTUPD) THEN
                            TOTPAD(K,B)=TOTPAD(K,B)-LPAY(K,B,DRWIND,GIND)
                        END IF

	                TOTWON(K,B) = TOTWON(K,B)*DLTSHV(K,B)
	                TOTDAY(K,B) = LPAY(K,B,DRWIND,GIND)
	                TOTLIB(K,B) = FRAMT(MAXFRC(GNUM), TOTLIB(K,B),
     *                                      DLTSHV(K,B))
	                TOTPAD(K,B) = FRAMT(MAXFRC(GNUM), TOTPAD(K,B),
     *                                      DLTSHV(K,B))
	                TOTDAY(K,B) = FRAMT(MAXFRC(GNUM), TOTDAY(K,B),
     *                                      DLTSHV(K,B))
	                TOTPRG(K,B) = TOTWON(K,B)-TOTLIB(K,B)-
     *                                TOTPAD(K,B)-TOTDAY(K,B)

	                CALL ADDI8I4 (STOTWON(1,B) ,TOTWON(K,B),VALUNIT)
	                CALL ADDI8I4 (STOTPAD(1,B) ,TOTPAD(K,B),VALUNIT)
	                CALL ADDI8I4 (STOTLIB(1,B) ,TOTLIB(K,B),VALUNIT)
	                CALL ADDI8I4 (STOTPRG(1,B) ,TOTPRG(K,B),VALUNIT)
	                CALL ADDI8I4 (STOTDAY(1,B) ,TOTDAY(K,B),VALUNIT)
			R_TOTDAY = R_TOTDAY + DFLOAT (TOTDAY(K,B))
			R_TOTWON = R_TOTWON + DFLOAT (TOTWON(K,B))
			R_TOTPAD = R_TOTPAD + DFLOAT (TOTPAD(K,B))
			R_TOTPRG = R_TOTPRG + DFLOAT (TOTPRG(K,B))
			R_TOTLIB = R_TOTLIB + DFLOAT (TOTLIB(K,B))

30	            CONTINUE


	            IF(LINCNT.GT.LINSPP) THEN
	                CALL TITLE(REPHDR,REPNAM,1,REPLU,PAGE,DAYCDC)
	                WRITE(REPLU,9000)
	                LINCNT = 7
	            ENDIF

	            LINCNT = LINCNT + 7
	            STATUS = '                 '
	            IF(B.NE.1) STATUS = '<<<BONUS DRAW>>> '

	            WRITE(REPLU,9001)  DRAW,(HEAD(I),I=1,DLTDIV),HEAD(9)
	            WRITE(REPLU,8003)  STATUS
	            WRITE(REPLU,9002)  (CSMONY(TOTWON(I,B),13,VALUNIT),
     *                                  I = 1, DLTDIV),
     *                                  CSMONYI8 (STOTWON(1,B),13,VALUNIT)

	            WRITE(REPLU,9003)  (CSMONY(TOTPAD(I,B),13,VALUNIT),
     *                                  I = 1, DLTDIV),
     *                                  CSMONYI8 (STOTPAD(1,B),13,VALUNIT)

	            WRITE(REPLU,9004)  (CSMONY(TOTDAY(I,B),13,VALUNIT),
     *                                  I = 1, DLTDIV),
     *                                  CSMONYI8 (STOTDAY(1,B),13,VALUNIT)

	            WRITE(REPLU,9005)  (CSMONY(TOTPRG(I,B),13,VALUNIT),
     *                                  I = 1, DLTDIV),
     *                                  CSMONYI8 (STOTPRG(1,B),13,VALUNIT)

	            WRITE(REPLU,9006)  (CSMONY(TOTLIB(I,B),13,VALUNIT),
     *                                  I = 1, DLTDIV),
     *                                  CSMONYI8 (STOTLIB(1,B),13,VALUNIT)
C
	            DO I = 1, DLTDIV
	                CALL ADDI8I4 (GRNWON(1,I), TOTWON(I,B),VALUNIT)
	                CALL ADDI8I4 (GRNPAD(1,I), TOTPAD(I,B),VALUNIT)
	                CALL ADDI8I4 (GRNLIB(1,I), TOTLIB(I,B),VALUNIT)
	                CALL ADDI8I4 (GRNPRG(1,I), TOTPRG(I,B),VALUNIT)
	                CALL ADDI8I4 (GRNDAY(1,I), TOTDAY(I,B),VALUNIT)
		        R_GRNWON(I) = R_GRNWON(I) + DFLOAT(TOTWON(I,B))
		        R_GRNPAD(I) = R_GRNPAD(I) + DFLOAT(TOTPAD(I,B))
		        R_GRNLIB(I) = R_GRNLIB(I) + DFLOAT(TOTLIB(I,B))
		        R_GRNPRG(I) = R_GRNPRG(I) + DFLOAT(TOTPRG(I,B))
		        R_GRNDAY(I) = R_GRNDAY(I) + DFLOAT(TOTDAY(I,B))

		    END DO

	            CALL ADDI8I8 (GRNWON(1,DLTDIV+1), STOTWON(1,B),VALUNIT)
	            CALL ADDI8I8 (GRNPAD(1,DLTDIV+1), STOTPAD(1,B),VALUNIT)
	            CALL ADDI8I8 (GRNLIB(1,DLTDIV+1), STOTLIB(1,B),VALUNIT)
	            CALL ADDI8I8 (GRNPRG(1,DLTDIV+1), STOTPRG(1,B),VALUNIT)
	            CALL ADDI8I8 (GRNDAY(1,DLTDIV+1), STOTDAY(1,B),VALUNIT)
                    R_GRNWON(DLTDIV+1) = R_GRNWON(DLTDIV+1) + R_TOTWON
                    R_GRNPAD(DLTDIV+1) = R_GRNPAD(DLTDIV+1) + R_TOTPAD
                    R_GRNLIB(DLTDIV+1) = R_GRNLIB(DLTDIV+1) + R_TOTLIB
                    R_GRNPRG(DLTDIV+1) = R_GRNPRG(DLTDIV+1) + R_TOTPRG
                    R_GRNDAY(DLTDIV+1) = R_GRNDAY(DLTDIV+1) + R_TOTDAY

40 	        CONTINUE
300          CONTINUE
310          CONTINUE
C
C
	     LINCNT = LINCNT + 7
	     IF(LINCNT.GT.LINSPP) THEN
	         CALL TITLE(REPHDR,REPNAM,GIND,REPLU,PAGE,DAYCDC)
	         WRITE(REPLU,9000)
	         LINCNT=7
	     ENDIF

	     WRITE(REPLU,9007) (HEAD(K),K=1,DLTDIV),HEAD(9)

C           WRITE(REPLU,9002) ( CMONYI8 (GRNWON(1,K),13,VALUNIT),K=1,DLTDIV+1 )
C           WRITE(REPLU,9003) ( CMONYI8 (GRNPAD(1,K),13,VALUNIT),K=1,DLTDIV+1 )
C           WRITE(REPLU,9004) ( CMONYI8 (GRNDAY(1,K),13,VALUNIT),K=1,DLTDIV+1 )
C           WRITE(REPLU,9005) ( CMONYI8 (GRNPRG(1,K),13,VALUNIT),K=1,DLTDIV+1 )
C           WRITE(REPLU,9006) ( CMONYI8 (GRNLIB(1,K),13,VALUNIT),K=1,DLTDIV+1 )

           WRITE(REPLU,9102) ( R_GRNWON(K)/CNVAMT,K=1,DLTDIV+1 )
           WRITE(REPLU,9103) ( R_GRNPAD(K)/CNVAMT,K=1,DLTDIV+1 )
           WRITE(REPLU,9104) ( R_GRNDAY(K)/CNVAMT,K=1,DLTDIV+1 )
           WRITE(REPLU,9105) ( R_GRNPRG(K)/CNVAMT,K=1,DLTDIV+1 )
           WRITE(REPLU,9106) ( R_GRNLIB(K)/CNVAMT,K=1,DLTDIV+1 )

C
C
	     CALL USRCLOS1(3)
	     CALL USRCLOS1(REPLU)

	     CALL SPOOL(REPNAM,COPY,STATUS)

             ! send GRAND TOTAL TO BALANSFILE
C                                                                               
C***         TOTREAL = GRNDAY(DLTDIV+1)                                     
             TOTREAL = R_GRNDAY(DLTDIV+1)

             RAPCODE = 80 + GNUM                   
	     CALL BALWRI(RAPCODE,GAMESUMS,TOTSUMS,TOTREAL)                  

1000	 CONTINUE
C
C     ===================== Format Statements =================
C
8000	FORMAT(1X,A,' Generating ',4A4,' liability report')
8001	FORMAT(A4,' LIABILITY REPORT FOR ',7A2)
8002	FORMAT('LO', I1, 'LIABLE.REP')
8003	FORMAT(1X,A)
C
9000	FORMAT(1X,131('='))
9001	FORMAT(/,1X,'DRAW   ',I5,2X,<DLTDIV+1>(8X,A6))
9002	FORMAT(1X,'TOTAL WON      ',<DLTDIV+1>(A13,1X))
9003	FORMAT(1X,'PREVIOUSLY PAID',<DLTDIV+1>(A13,1X))
9004	FORMAT(1X,'PAID TODAY     ',<DLTDIV+1>(A13,1X),2X,A13,1X,A13)
9005	FORMAT(1X,'PURGED/EXPIRED ',<DLTDIV+1>(A13,1X))
9006	FORMAT(1X,'OUTSTANDING    ',<DLTDIV+1>(A13,1X))
C
9102	FORMAT(1X,'TOTAL WON      ',<DLTDIV+1>(F13.2,1X))
9103	FORMAT(1X,'PREVIOUSLY PAID',<DLTDIV+1>(F13.2,1X))
9104	FORMAT(1X,'PAID TODAY     ',<DLTDIV+1>(F13.2,1X))
9105	FORMAT(1X,'PURGED/EXPIRED ',<DLTDIV+1>(F13.2,1X))
9106	FORMAT(1X,'OUTSTANDING    ',<DLTDIV+1>(F13.2,1X))
C
C9007	FORMAT(/,1X,'GRAND TOTALS',2X,<DLTDIV+1>(8X,A6),
C     *    'NET PAID        TAXES')
9007	FORMAT(/,1X,'GRAND TOTALS',2X,<DLTDIV+1>(8X,A6))



	RETURN
	END
