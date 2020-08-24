C SUBROUTINE LOTSNP
C
C V20 10-OCT-2012 FRP GETWEEK: AAAACCC FORMAT INSTEAD OF YYYYWW FORMAT
C V19 20-DEC-2010 HXK Added Lucky number, generalised num of winning numbers.
C                 FJG New DIVISION considerations
C                 FJG Show draw status
C V18 01-AUG-2009 FJG Portugal Fiscal Legislation changes
C V17 04-NOV-2003 FRP Show draw date and time.
C V16 24-MAY-2001 ANG Removed Hard codes for Lotto. 
C V15 20-FEB-2001 EPH Show offline sales (LTOSAL(2,*)) again
C V14 05-FEB-2001 ANG Enabled year-week input 
C V13 31-MAY-2000 PXO Subroutine name from LTOSNP -> LOTSNP
C V12 18-OCT-1999 UXN OFFLINE sales field removed. RFSS #92
C V11 23-JUL-1999 PXO Changed sales numbers from 11 to 12 characters
C V10 19-MAY-1996 HXK Wojtek added round to snapshot
C V09 14-SEP-1995 HXK Added Rollover to lower part of Snapshot,
C                     do not show Rollover in grand total
C V08 20-AUG-1995 RXK Field for bonus draw numbers is cleared
C V07 16-AUG-1995 HXK Added Rollover to screen
C V06 13-JUL-1995 HXK Changes for Viking Bonus
C V05 06-JUL-1993 SXH Released for Finland
C V04 13-JUN-1993 HXK added AGTINF.DEF, PRMAGT.DEF
C V03 21-JAN-1993 DAB Initial Release Based on Netherlands Bible, 12/92,
C                     and Comm 1/93 update DEC Baseline
C V02 08-FEB-1992 MGM FIX BUG IN DISPLAY OF SALES
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C LOTSNP.FOR
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
	SUBROUTINE LOTSNP(NUM,GIND,CLINE)
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
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:DLTREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
        ! arguments
        INTEGER*4  NUM                      !
        INTEGER*4  GIND                     !
        INTEGER*4  CLINE(20)                !

        ! variables
	INTEGER*4  LPAD(LTGDIV)             !
	INTEGER*4  LVAL(LTGDIV)             !
	INTEGER*4  LWIN(LTGDIV)             !
	INTEGER*4  BPAD(LTGDIV)             !
	INTEGER*4  BVAL(LTGDIV)             !
	INTEGER*4  BWIN(LTGDIV)             !
	INTEGER*4  FDB(7)                   !
	INTEGER*4  TOTWON(2)                !
	INTEGER*4  TOTPAD(2)                !
	INTEGER*4  LSAL(12)                 !
	INTEGER*4  LIN                      !
	INTEGER*4  DIV                      !
	INTEGER*4  J                        !
	INTEGER*4  I                        !
	INTEGER*4  LTOT1                    !
	INTEGER*4  K
	INTEGER*4  ST                       !
	INTEGER*4  GNUM                     !
	INTEGER*4  DRAW                     !
!       INTEGER*4  VALUE                    !
!       INTEGER*4  POS                      !
!       INTEGER*4  KEYNUM                   !
!       INTEGER*4  CBUF(CDLEN)              !
C
        INTEGER*4  WEEK, DAT
        INTEGER*4  GETDRW                   !FUNCTION
        INTEGER*2  WEK
	INTEGER*4  CCC
        INTEGER*4  YEAR
C                                       'Not initialized     '
        CHARACTER*20 DRAWSTAT(0:GAMREF)/'--Not  initialized--',        
     *                                  '-Not a  drawing day-',
     *                                  '-Game info  entered-',
     *                                  '-----GAME  OPEN-----',
     *                                  'End of game bef draw',
     *                                  '--Winning  numbers--',
     *                                  'Winning numbers verd',
     *                                  '--Prize values set--',
     *                                  '*Results are  FINAL*',
     *                                  '---Draw Cancelled---',
     *                                  'Draw enabled refunds'/
C
	INTEGER*2 SDAT(LDATE_LEN,12)        !
	INTEGER*2 DRDAT(LDATE_LEN)         ! DRAWING DATE

	CHARACTER*28 CSDAT(12)              !

	EQUIVALENCE  (CSDAT(1),SDAT(1,1))
C
	REAL*8  LTOT                        !
	REAL*8  POOL                        !
C       REAL*8  KEYS(1)                     !

        ! functions
        INTEGER*4  FRAMT                    !

	CHARACTER*5 DNAME(LTGDIV)           !
	INTEGER*4   MATCH(2,LTGDIV)
	CHARACTER*1 PLUS(2)                 !
	DATA PLUS/' ','+'/
	CHARACTER*1 LUCK(2)                 !
	DATA LUCK/'L',' '/
C       DATA KEYS/'ESTjack '/

	CHARACTER*8 STATUS                  !
C
C INITIALIZE SOME VARIABLES
C
	CALL FASTSET(0,BVAL,LTGDIV)
	CALL FASTSET(0,BPAD,LTGDIV)
	CALL FASTSET(0,BWIN,LTGDIV)             
	CALL FASTSET(0,MATCH,LTGDIV) 	
C
	DRAW=NUM
	IF(GIND.LT.1.OR.GIND.GT.MAXIND) GIND = 1
C
C
	GNUM=GTNTAB(TLTO,GIND)
	IF(GNUM.LT.1) THEN
	    WRITE(CLIN23,3010) GIND
	    RETURN
	ENDIF

	CALL GETWEEK(CLINE,DAT)
        IF (DAT.GT.0) THEN
            WEEK = MOD(DAT,1000)
            YEAR = INT(DAT/1000)
            DRAW = GETDRW(YEAR,WEEK,GNUM)
            IF (DRAW.LE.0) THEN
                WRITE(CLIN23,3040)
                RETURN
             ENDIF
             GOTO 5
        ENDIF

	IF(DRAW.LT.1) DRAW=DAYDRW(GNUM)
	IF(DRAW.EQ.0) DRAW=DAYHDR(GNUM)
C                                                                               
C CHANGE ESTIMATED JACKPOT                                                      
C                                                                               
!        VALUE=0                                                                   
!        POS=1                                                                     
!C                                                                               
!        CALL KEY(CLINE,KEYS,1,POS,KEYNUM)                                         
!C                                                                               
!        IF(POS.GT.40) GOTO 5                                                      
!        IF(KEYNUM.EQ.0) THEN                                                      
!            WRITE(CLIN23,3040)                                                      
!            RETURN                                                                  
!        ENDIF                                                                     
!C                                                                               
!        CALL NUMB(CLINE,POS,VALUE)                                                
!        IF(VALUE.LT.0) THEN                                                       
!            WRITE(CLIN23,3050)                                                      
!            RETURN                                                                  
!        ENDIF                                                                     
!C                                                                               
!        IF(KEYNUM.EQ.1) THEN                                                      
!            CBUF(1) = 10                                                               
!            CBUF(2) = VALUE                                                           
!            CBUF(3) = TCLTO                                                           
!            CBUF(6) = IDNUM                                                           
!            CBUF(8) = GIND                                                            
!            CALL VISCMD(CBUF,ST)                                                    
!        ENDIF                                                                     
!
C
C GET DATA FROM COMMON OR DISK
C
5       CONTINUE
	IF(DRAW.EQ.DAYDRW(GNUM)) THEN
	    CALL GAMLOG(TLTO,GIND,DLTREC,LTOSTS)
	    GOTO 100
	ENDIF
C
C
	SMODE=.TRUE.
	CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,1,DLTSEC*256)
	IF(ST.NE.0) THEN
	    WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
	    CALL USRCLOS1(     1)
	    RETURN
	ENDIF
	CALL READW(FDB,DRAW,DLTREC,ST)
	IF(ST.NE.0) THEN
	    WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
	    CALL USRCLOS1(     1)
	    RETURN
	ENDIF
	CALL USRCLOS1(     1)
C
C
100	CONTINUE

	LTOT=0.0D0
	LTOT1=0
	DO I = 1,2
	    TOTWON(I)=0
	    TOTPAD(I)=0
        END DO

C**v16	DO I = 1, LTGENT
C**	    IF(I.EQ.1) THEN
C**               IF(GIND .EQ. 1) WRITE (CSDAT(1)(17:26),801)
C**                IF(GIND .EQ. 2) WRITE (CSDAT(1)(17:26),804)
C**            END IF
C**
C**	    IF(I.EQ.2) THEN
C**                IF(GIND .EQ. 1) WRITE (CSDAT(2)(17:26),802)
C**                IF(GIND .EQ. 2) WRITE (CSDAT(2)(17:26),805)
C**            END IF  
C**
C**            IF(I.EQ.3 .AND. GIND.EQ.2) WRITE(CSDAT(3)(17:26),801)
C**
C**	    IF(I.EQ.12) WRITE (CSDAT(12)(17:26),803)
C**
C**	    IF(I.GT.2.AND.I.LT.12 .AND. GIND.EQ.1) THEN
C**	        SDAT(VCDC,I)=DLTDAT(CURDRW)-(I-3)
C**	        CALL LCDATE(SDAT(1,I))
C**	    ENDIF
C**
C**           IF(I.GT.3.AND.I.LT.12.AND.GIND.EQ.2) THEN                                 
C**                SDAT(VCDC,I)=DLTDAT(CURDRW)-(I-4)                                       
C**                CALL LCDATE(SDAT(1,I))                                                   
C**           ENDIF                                                                     
C**
C**	    IF(I.LT.12) THEN
C**               IF(GIND.EQ.1) THEN
C**	            LSAL(I)=DLTSAL(I)
C**                ELSE
C**                    IF(I.EQ.1) LSAL(1) = DLTEST
C**                    IF(I.EQ.2) LSAL(2) = DLTSHR(1,1)*DLTSHV(1,1)
C**                    IF(I.EQ.3) LSAL(3) = DLTSAL(1)
C**                    IF(I.NE.1.AND.I.NE.2.AND.I.NE.3)                                      
C**     *                LSAL(I) = DLTSAL(I-1)                                           
C**                    IF(I.EQ.11) LSAL(12) = DLTSAL(11)                                 
C**                END IF  
C**	    ELSE
C**	        LSAL(12)=LSAL(12)+DLTSAL(I)
C**	    ENDIF
C**       END DO

	DO I=1,12
	   LSAL(I) = 0
	ENDDO

	DO I=1,LTGENT
	    IF (I.EQ.1)  WRITE (CSDAT(1)(17:26),801)   
	    IF (I.EQ.2)  WRITE (CSDAT(2)(17:26),802)
	    IF (I.EQ.12) WRITE (CSDAT(12)(17:26),803)

            IF(I.GT.2.AND.I.LT.12) THEN
C               SDAT(VCDC,I)=DLTDAT(CURDRW)-(I-3)
               SDAT(VCDC,I)=DLTESD-(I-3)
               CALL LCDATE(SDAT(1,I))
	    ENDIF
	       
            K = I + (DLTDAT(CURDRW)-DLTESD) ! SINCE DTGSAL INDEXED W/ DTGDAT -
                                            ! BETTER WOULD BE W/ DTGESD, BUT...
           IF (I.LE.2) THEN
                LSAL(I) = DLTSAL(I)
            ELSEIF (I.LE.11 .AND. K.LE.TGGENT) THEN
                LSAL(I) = DLTSAL(K)
            ELSEIF (K.LT.TGGENT) THEN
                LSAL(12) = LSAL(12)+DLTSAL(K)
            ELSEIF (K.EQ.TGGENT) THEN
                LSAL(2) = LSAL(2) + DLTSAL(K)
            ELSE
                ! FALLTHROUGH
            ENDIF



C	    IF (I.LT.12) THEN
C		LSAL(I) = DLTSAL(I)
C	    ELSE
C	        LSAL(12) = LSAL(12) + DLTSAL(I)
C	    ENDIF
	ENDDO
Cv16

C
C WEEK No. / CCC No.
C
	IF(GIND.GT.2) THEN
          CALL FIGWEK(DLTESD,WEK,YEAR)
	ELSE
          CALL FIGWEK(DLTBSD,WEK,YEAR)
	ENDIF
	IF(DLTLFL.NE.0) CALL FIGCCC(DLTDAT(CURDRW),CCC,YEAR)
C
C POOL DATA
C
	DO I = 1, LTGENT
	    LTOT = LTOT + DFLOAT(DLTSAL(I))
	    LTOT1=LTOT1+DLTSAL(I)
        END DO
C
	POOL=LTOT*CALPER(DLTSPR)
	DO I=1,DLTDIV
	    LVAL(I)=IDNINT(POOL*CALPER(DLTPER(I)))
	    LVAL(I)=LVAL(I)+DLTPOL(I)        !ADD POOL CARRIED OVER
	    LWIN(I)=0
	    LPAD(I)=0
        END DO
C
C
	IF(DLTSTS.GE.GAMDON.OR.DLTSPR.EQ.0) THEN
	    DO I = 1, DLTDIV
	        LVAL(I) = DLTSHV(I,1)
	        LWIN(I) = DLTSHR(I,1)*LVAL(I)
	        LPAD(I) = FRAMT(MAXFRC(GNUM),DLTPAD(I,1),LVAL(I))
                IF(DLTBDR.GT.0) THEN
	           BVAL(I) = DLTSHV(I,2)
	           BWIN(I) = DLTSHR(I,2)*BVAL(I)
	           BPAD(I) = FRAMT(MAXFRC(GNUM),DLTPAD(I,2),BVAL(I))
                ENDIF
            END DO
	ENDIF
C
C GET DIVISION NAMES
C
	DO I=1,LTGDIV
	    DNAME(I)=' - '
        END DO
C
        DO 180 K=1,LTGBET
	  IF(DLTBET(K).EQ.0) GOTO 180
	  DO 170 I=1,DLTBET(K)
	      DO 170 J=1,2
	          IF(DLTWTB(I,J,K).NE.0) THEN
	              DIV=DLTWTB(I,J,K)
	              IF(DLTLFL.EQ.0) THEN
                        IF(DNAME(DIV).EQ.' - ') WRITE (DNAME(DIV),800) I,PLUS(J), DLTBET(K)
                      ELSE
                        MATCH(1,DIV) = I
                        MATCH(2,DIV) = DLTBET(K)                 
                      ENDIF
	          ENDIF
170        CONTINUE
180     CONTINUE
!=======V19 FJG=================================================================
        IF(DLTLFL.NE.0) THEN	           
          DO I=1,LTGDIV          
            DO K=1,2
              IF(DLTLNC(K,I).NE.0) THEN
                DIV = DLTLNC(K,I)                
                WRITE (DNAME(DIV),800) MATCH(1,I),LUCK(K), MATCH(2,I)
              ENDIF
            ENDDO
          ENDDO
          DIV = DLTLDV
          WRITE(DNAME(DIV),8001)          
        ENDIF
!=======V19 FJG=================================================================
C
	DO I=1,DLTDIV
	    TOTWON(1) = TOTWON(1) + DLTSHR(I,1)
	    TOTWON(2) = TOTWON(2) + LWIN(I)
	    TOTPAD(1) = TOTPAD(1) + DLTPAD(I,1)
	    TOTPAD(2) = TOTPAD(2) + LPAD(I)
        END DO
C
C ENCODE LOTTO GAME SNAPSHOT
C
	STATUS='*closed*'
	IF(DLTSTS.EQ.GAMOPN) STATUS='**open**'
	IF(DLTSTS.EQ.GAMDON) STATUS='**done**'
	IF(DLTSTS.EQ.GFINAL) STATUS='*final* '

	DRDAT(VCDC)=DLTDAT(CURDRW)
	CALL LCDATE(DRDAT(1))

	WRITE(CLIN1,901) GIND,DLTNUM,DLTMAX,DRAW
	WRITE(CLIN2,902) (GLNAMES(K,GNUM),K=1,4),(DRDAT(K),K=7,13),DISTIM(DLTTIM)
	IF(DLTLFL.EQ.0) THEN
	  WRITE(CLIN3,9010) WEK,YEAR,STATUS,DRAWSTAT(DLTSTS),DLTSTS
	ELSE
	  WRITE(CLIN3,90101) DLTCCC,YEAR,WEK,STATUS,DRAWSTAT(DLTSTS),DLTSTS
	ENDIF
	IF(DLTSTS.LT.GAMDON.AND.DLTSPR.GT.0) THEN
	    WRITE(CLIN4,903)
	ELSE
	    WRITE(CLIN4,9031)
	ENDIF

	LIN=5

	DO I=1,DLTDIV
	    LIN=LIN+1
	    IF(DLTSTS.GT.GAMBFD.OR.DLTSPR.EQ.0.OR.DLTPER(I).NE.0) THEN
C	      IF(GIND.EQ.1.AND.I.EQ.2) THEN
C	        WRITE(XNEW(  LIN),90411) I,DNAME(I),CMONY(LVAL(I),11,VALUNIT),
C     *	                               DLTSHR(I,1),CMONY(LWIN(I),11,VALUNIT),
C     *	                               DLTPAD(I,1),CMONY(LPAD(I),11,VALUNIT)
C	      ELSE
	        WRITE(XNEW(  LIN),904) I,DNAME(I),CMONY(LVAL(I),11,VALUNIT),
     *	                               DLTSHR(I,1),CMONY(LWIN(I),11,VALUNIT),
     *	                               DLTPAD(I,1),CMONY(LPAD(I),11,VALUNIT),
     *	                              (SDAT(K,I),K=9,13),
     *                                 CMONY(LSAL(I),12,BETUNIT)
C	      ENDIF
	    ELSE
                WRITE(XNEW(  LIN),9041)I,DNAME(I),
     *                                 DLTSHR(I,1),CMONY(LWIN(I),11,VALUNIT),
     *                                 DLTPAD(I,1),CMONY(LPAD(I),11,VALUNIT),
     *                                (SDAT(K,I),K=9,13),
     *                                 CMONY(LSAL(I),12,BETUNIT)
	    ENDIF
        END DO

	LIN=LIN+1
	WRITE(XNEW(  LIN),905) TOTWON(1),CMONY(TOTWON(2),11,VALUNIT),
     *	                       TOTPAD(1),CMONY(TOTPAD(2),11,VALUNIT),
     *	                       (SDAT(K,I),K=9,13),
     *                         CMONY(LSAL(I),12,BETUNIT)

        IF(DLTBDR.EQ.0) THEN
	   DO I=DLTDIV+2,12
	      LIN=LIN+1
	      WRITE(XNEW(  LIN),906) (SDAT(K,I),K=9,13),
     *                             CMONY(LSAL(I),12,BETUNIT)
           END DO 
        ELSE
           I=DLTDIV+2
	   LIN=LIN+1
	   WRITE(XNEW(  LIN),906) (SDAT(K,I),K=9,13),
     *                             CMONY(LSAL(I),12,BETUNIT)
           I=DLTDIV+3
	   LIN=LIN+1
	   WRITE(XNEW(  LIN),9061) CMONY(BVAL(1),11,VALUNIT),
     *                             DLTSHR(1,2),CMONY(BWIN(1),11,VALUNIT),
     *                             DLTPAD(1,2),CMONY(BPAD(1),11,VALUNIT),
     *                             (SDAT(K,I),K=9,13),
     *                             CMONY(LSAL(I),12,BETUNIT)
           I=DLTDIV+4
	   LIN=LIN+1
	   WRITE(XNEW(  LIN),9062) TOTWON(1)+DLTSHR(1,2),
     *                             CMONY(TOTWON(2)+BWIN(1),11,VALUNIT),
     *                             TOTPAD(1)+DLTPAD(1,2), 
     *                             CMONY(TOTPAD(2)+BPAD(1),11,VALUNIT),
     *                             (SDAT(K,I),K=9,13),
     *                             CMONY(LSAL(I),12,BETUNIT)
	   DO I=DLTDIV+5,12
	      LIN=LIN+1
	      WRITE(XNEW(  LIN),906) (SDAT(K,I),K=9,13),
     *                             CMONY(LSAL(I),12,BETUNIT)
           END DO 
        ENDIF

	LIN=LIN+1
C==============================================================================
C INI V18 FJG Fiscal Changes 
C==============================================================================
	WRITE(XNEW(  LIN),907) CMONY(DLTMIN,12,VALUNIT),CMONY(DLTAPL,12,VALUNIT),
     *                         CMONY(LTOT1,12,BETUNIT)
C==============================================================================
C FIN V18 FJG Fiscal Changes
C==============================================================================
	LIN=LIN+2

	WRITE(XNEW(  LIN),908)
	WRITE(XNEW(  LIN+1),908)

C**v16	IF(DLTSTS.LT.GAMENV) THEN
C**         IF(GIND.EQ.1) THEN
C**            WRITE(XNEW(  LIN),9042) CMONY(DLTPOL(1),11,VALUNIT)
C**         ELSE
C**            WRITE(XNEW(LIN),908)
C**         ENDIF
C**      ELSE
C**         IF(GIND.EQ.1) THEN
C**            WRITE(XNEW(LIN),9082) (DLTWIN(I,1),I=1,DLTNUM),
C**   *                              CMONY(DLTPOL(1),11,VALUNIT)
C**         ELSE
C**            WRITE(XNEW(LIN),9081) (DLTWIN(I,1),I=1,DLTNUM)
C**         ENDIF
C**      ENDIF

	IF(DLTSTS.LT.GAMENV) THEN
              WRITE(XNEW(  LIN),9042) CMONY(DLTPOL(1),11,VALUNIT)
        ELSE
	      IF(DLTLFL.EQ.0) THEN
                WRITE(XNEW(LIN),9082) (DLTWIN(I,1),I=1,DLTNUM),
     *                              CMONY(DLTPOL(1),11,VALUNIT)
	      ELSE
                WRITE(XNEW(LIN),90821) (DLTWIN(I,1),I=1,DLTNUM),DLTLNM(1),
     *                              CMONY(DLTPOL(1),11,VALUNIT)
	      ENDIF 
        ENDIF
C**v16
	LIN=LIN+1

	IF(DLTSTS.LT.GAMENV) THEN
           WRITE(XNEW(LIN),908)
        ELSE
           IF(DLTBFL.NE.0) THEN 
              WRITE(XNEW(LIN),909) (DLTBNM(K,1),K=1,DLTBFL)
           ELSE
              WRITE(XNEW(LIN),908)
           ENDIF
        ENDIF
	LIN=LIN+1

        IF(DLTBDR.EQ.1) THEN
	   IF(DLTSTS.LT.GAMENV) THEN
              WRITE(XNEW(LIN),9092) 
           ELSE
              WRITE(XNEW(LIN),9091) (DLTWIN(I,2),I=1,DLTNUM)
           ENDIF
        ELSE
           WRITE(XNEW(LIN),908)
        ENDIF
C
	RETURN

800	FORMAT(I1,A1,'/',I1,' ')
8001	FORMAT('LUCK ')
801	FORMAT('Advance   ')
802	FORMAT('Offline   ')
803	FORMAT('Previous  ')
804     FORMAT('*ESTjack  ')   
805     FORMAT('Jackpot   ')    
901     FORMAT('Lotto ',I1,3X,I2,'/',I2,' game data for draw ',I4)
C***9010 FORMAT(25X,' pools are ',A8)
9010	FORMAT('Round ',I2,'/',I4,13X,' pools are ',A8,' [',A20,'] (',I2,')')
90101	FORMAT('DrawId ',I3,'/',I4,3X,'(Round ',I2,')  pools are ',A8,' [',A20,'] (',I2,')')
902	FORMAT(4A4,T37,'Draw',7A2,2X,A8)
903	FORMAT('--DIV-- ----POOL--- ',4('-'),'SHARES WON',4('-'),
     *	    1X,2('-'),'FRACTIONS PAID--',1X,9('-'),'SALES',8('-'))
9031	FORMAT('--DIV-- ----PRIZE-- ',4('-'),'SHARES WON',4('-'),
     *	    1X,2('-'),'FRACTIONS PAID--',1X,9('-'),'SALES',8('-'))
904	FORMAT(I1,'(',A4,')',A11,1X,I6,1X,A11,1X,I7,1X,A11,1X,
     *	       5A2,A12)
90411	FORMAT(I1,'(',A4,')',A11,1X,I6,1X,A11,1X,I7,1X,A11)
9041    FORMAT(I1,'(',A4,')','---FIXED---',1X,I6,1X,A11,1X,I7,1X,A11,1X,
     *         5A2,A12)
9042    FORMAT(58X,'Rollover   ',A11)
905	FORMAT('Total',14X,I6,1X,A11,1X,I7,1X,A11,1X,5A2,A12)
906	FORMAT(58X,5A2,A12)
9061    FORMAT('Bns draw',A11,I6,1X,A11,1X,I7,1X,A11,1X,5A2,A12)
9062    FORMAT('Total2',13X,I6,1X,A11,1X,I7,1X,A11,1X,5A2,A12)
907	FORMAT('Guaranteed: ',A12,6X,'FromFund: ',A12,6X,'Total',5X,A12)
908	FORMAT(80(' '))
9081	FORMAT('Winning numbers : ',10(I2.2,1X))
9082	FORMAT('Winning numbers : ',<DLTNUM>(I2.2,1X),22X,'Rollover   ',A11)
90821   FORMAT('Winning numbers : ',<DLTNUM>(I2.2,1X),2X,'Lucky:',I2.2,12X,
     *         'Rollover   ',A11)
909	FORMAT('Bonus number(s) : ',I2.2,1X,I2.2,1X,I2.2)
9091	FORMAT('Bonus draw nums : ',10(I2.2,1X))
9092    FORMAT('BONUS DRAW ENABLED')
3000	FORMAT('Enter !lotto game index ')
3010	FORMAT('Lotto ',I1,' game not active')
3020	FORMAT(5A4,' open error ',I4)
3030	FORMAT(5A4,' read error ',I4,' record > ',I4)
3040    FORMAT('Input error ')
3050    FORMAT('Value error ')

	END
