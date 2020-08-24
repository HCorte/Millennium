C LT1SREP.FOR
C
C V27 21-DEC-2016 SCML Save TOTOLOTOQ/S share data into a data structure for
C                      further interface file generation
C V26 21-MAR-2011 FJG Lotto2 Batch label changes
C V25 17-MAR-2011 HXK DISPLAY NEW DETAIL ON HANDLING FIXED PRIZE TOTAL AMOUNT  
C V24 15-MAR-2011 HXK INCLUDE TOTLOTO DIV 6 (fixed price refund) IN TOTOLOTOQ/S
C V23 07-MAR-2011 FJG Sorteio CCC/YY
C                 FJG Variable Overflow
C V22 27-JUL-2010 RXK Jackpot is rounded, not truncated.
C V21 01-AUG-2009 FJG Portugal Fiscal Legislation changes
C V20 11-DEC-2000 EPH Work with variable number of decimals to base price
C V19 11-MAY-2000 OXK Modifications for *SPL (promotion)
C V18 01-DEC-1999 OXK COPY=0 removed.
C V17 25-OCT-1999 UXN Additional field added.
C V16 15-JUL-1999 PXO Changed variables from integer*4 to real*8 (penny money)
C V15 27-NOV-1997 UXN Changes for LOTTO extra draw.
C V14 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting,
C                     Instant Pass Thru Phase 1
C V13 23-AUG-1995 RXK Change of formats
C V12 23-AUG-1995 HXK
C V11 10-APR-1995 HXK Correction of toal penny money (TOTBREAK should be REAL*8)
C V10 30-MAR-1995 HXK FIX FOR PENNY MONEY
C V09 24-MAR-1995 HXK New rule - when jackpot amount promised:
C  i) if jackpot won at least the promised amount must be paid unless
C     the 95% rule applies.
C  2) if jackpot not won the difference between the jackpot as it would
C     have been if there had been a winner and the promised jackpot must
C     be rolled to the next round. 95% rule doe NOT apply in this case.
C V08 21-MAR-1995 HXK Handle sales as real*8
C V07 01-FEB-1995 HXK Initial revision.
C V06 16-DEC-1994 JXP
C V05 29-NOV-1994 JXP INCLUDED LOTTO 7/39 REPORT
C V04 02-NOV-1993 HXK 5 PENNY OUT.
C V03 18-OCT-1993 HXK Use CSMONY instead of CMONY in Win Reserve Fund.
C V02 20-MAR-1993 HJK MODIFICATIONS FOR HJK
C V01 20-JAN-1993 HJK INITIAL RELEASE FOR FINLAND #*#
C
C SUBROUTINE TO PRINT LOTTO SHARE REPORT.
C
C SINCE V27 VERSION SAVES LOTTO SHARE DATA FOR FURTHER LOTTO SHARE
C INTERFACE FILE CREATION
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, W.Greenwich, Rhode            
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
C====== OPTIONS/CHECK=NOOVERFLOW/EXT                                       
CV27        SUBROUTINE LTSREP(GNUM,GIND,DRAW)                             
        SUBROUTINE LTSREP(GNUM,GIND,DRAW,LTSHDATA)                              !V27
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'                                        
        INCLUDE 'INCLIB:RECSCF.DEF'                                        
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:LTSHFIL.DEF'                                            !V27

        !arguments
        INTEGER*4  GNUM             ! game number
        INTEGER*4  GIND             ! game index
        INTEGER*4  DRAW             ! draw number
C        INTEGER*4  COPY             ! number of report copies

        ! variables
        INTEGER*4  FDB(7)           ! file dscription block
        REAL*8     DPOOL(LTGDIV)    !
        REAL*8     DREAL(LTGDIV)    !        
        REAL*8     DRESI(LTGDIV)    !                
        REAL*8     GROSS(LTGDIV)    ! (in pennies)
        INTEGER*4  I                ! counter
        INTEGER*4  J                ! counter
        INTEGER*4  K                ! counter
        REAL*8     TOTSAL           ! total sales
        REAL*8     TOTASH           !
        REAL*8     TOTWON           !
        REAL*8     TOTCAR           !
        INTEGER*4  TOTSHR           !
        INTEGER*4  TOTBRK           ! (in pennies)
        REAL*8     TOTBREAK         !
        REAL*8     TOTWPL           !
        REAL*8     TOTPOL           !
        REAL*8     TOTGRS           ! (in pennies)
        INTEGER*4  WINBAL           ! (in pennies)
        INTEGER*4  BNS              ! 1 = No bonus, 2 = Bonus
        REAL*8     TPOOL            !
        REAL*8     TREAL        
        REAL*8     TRESI              
        REAL*8     TDIST
        REAL*8     FINALTOT         !
        INTEGER*4  REV              !
        INTEGER*4  PAGE             ! page number of report
        INTEGER*4  WEEK             ! week no for report
        INTEGER*4  ST               ! error status
        INTEGER*4  DIV              ! counter variable for divisions
        REAL*8     DRWSAL(3)
	INTEGER*4  FROM_WFUND       ! Amount taken from the win reserve funds

        INTEGER*4  DRWSHR(LTGDIV,2,3)  ! total shares for previous draws
        INTEGER*4  DRWSHV(LTGDIV,2,3)  ! total shares for previous draws

	INTEGER*4  TOTPER           ! total of percentages for parimutual divs

	INTEGER*4  YEAR             ! Year in 4 digits
	INTEGER*4  NEXTJACKPOT      ! NEXT JACKPOT
     
	INTEGER*4  MATCH_DIV(LTGDIV)

	INTEGER*4     LOTS/3/,LOTQ/4/
	CHARACTER*3   LUCKY_NUM(2)  !
        CHARACTER*12  REPNAM        !                                      
        CHARACTER     HEAD1*49      !
        CHARACTER     PLUS(2)       !                                         
        CHARACTER*4   DNAME(LTGDIV) !                                      
        CHARACTER*22  TITGAMREP     ! TITLE GAME REPORT
        CHARACTER*32  NSDIFS/'FUNDO RESERVA                   '/
C
        RECORD /STCLTSHFIL/ LTSHDATA                                            !V27
C
        ! functions
	REAL*8     TOTPENNY
	REAL*8	   TOTBRUTTO

        COMMON SCFREC

	DATA LUCKY_NUM/'+NS','   '/
        DATA PLUS/' ','+'/                                                 
        DATA DNAME/LTGDIV*'----'/                                            
        DATA REPNAM/'            '/                                        
C                                                                               
        CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)                              
        CALL IOINIT(FDB,3,DLTSEC*256)                                      
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)                     
C
        IF(GIND.EQ.LOTQ.OR.GIND.EQ.LOTS) THEN                                   !V27
          LTSHDATA.LTGTYP = TLTO                                                !V27 SAVE LOTTO GAME TYPE
          LTSHDATA.LTGNUM = GNUM                                                !V27 SAVE LOTTO GAME NUMBER
          LTSHDATA.LTGIND = GIND                                                !V27 SAVE LOTTO GAME INDEX
        ENDIF                                                                   !V27
C
        BNS=1
	CALL FASTSET(0,MATCH_DIV,LTGDIV)

        DO J=1,3
            DO DIV = 1, DLTDIV
              DO I = 1, BNS
                DRWSHR(DIV,I,J) = 0
                DRWSHV(DIV,I,J) = 0
              ENDDO
              DRWSAL(J)=0.0D0
            ENDDO
        ENDDO

        DO J=1,3
          IF(DRAW.GT.J) THEN
            CALL READW(FDB,DRAW-J,DLTREC,ST)
            IF(ST.NE.0) THEN
              CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
            ELSE
              DO DIV = 1, DLTDIV
                DO I = 1, BNS
                  DRWSHR(DIV,I,J) = DLTSHR(DIV,I)
                  DRWSHV(DIV,I,J) = DLTSHV(DIV,I)
                ENDDO
              ENDDO
              DO I = 1, LTGENT
                DRWSAL(J) = DRWSAL(J) + DFLOAT(DLTSAL(I))
              ENDDO
            ENDIF
          ENDIF
        ENDDO

        ! read record for DRAW
        CALL READW(FDB,DRAW,DLTREC,ST)                                     
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)                  
C
        IF(GIND.EQ.LOTQ.OR.GIND.EQ.LOTS) THEN                                   !V27
          LTSHDATA.LTDIV = DLTDIV                                               !V27 TOTAL LOTTO DIVISIONS
        ENDIF                                                                   !V27
C                                                                               
C	DLTPAW = 39100
C                                                                               
        ! open report file
        WRITE(REPNAM,800) GIND                                             
800     FORMAT('LO',I1,'SHARE.REP')                                        
        CALL ROPEN(REPNAM,8,ST)                                            
C
C GET TITLE REPORT MESSAGE ( GAME OF THE REPORT )
C
        WRITE(TITGAMREP, 802) (GLNAMES(I,GNUM),I=1,4)
C       IF(GIND .EQ. 2) WRITE(TITGAMREP, 802) '  JOGO DE LOTO 2, '
C
C GET WEEL / YEAR INFORMATION
C
        CALL GETWEK(DRAW, GNUM, WEEK, YEAR, ST)
        IF(ST .NE. 0) THEN
          TYPE *, IAM()
          TYPE *, IAM(), 'Error Getting Week / Year Draw Number'
          TYPE *, IAM()
          CALL GPAUSE
        ENDIF
C
        IF(GIND.EQ.LOTQ.OR.GIND.EQ.LOTS) THEN                                   !V27
          LTSHDATA.LTCCC = WEEK                                                 !V27 SAVE DRAW NUMBER
          LTSHDATA.LTYEAR = YEAR                                                !V27 SAVE DRAW YEAR
        ENDIF                                                                   !V27
C
C WRITE GAME TITLE INFORMATION
C
        PAGE = 0
        WRITE(HEAD1,9001) WEEK,YEAR                                 
C
C WRITE TITLE INFORMATION
C
        CALL TITLE(HEAD1,REPNAM(1:8),REV,8,PAGE,DAYCDC)                     
        WRITE(8,900) TITGAMREP, WEEK,YEAR
        WRITE(8,9013)
C
C GET DIVISION NAMES                                                            
C                                                                               
        DO I = 1, LTGDIV                                                    
            DNAME(I) = '----'                                                
        ENDDO

        DO I = 1, DLTNUM
            DO J = 1, 2    
                DO K = 1, LTGBET                                   
	            IF(DLTBET(K).EQ.0) CYCLE
                    IF(DLTWTB(I,J,K) .NE. 0) THEN                            
                        DIV = DLTWTB(I,J,K)                              
	                IF(DLTLFL.EQ.0) THEN
                          IF(DNAME(DIV).EQ.'----') THEN
                             WRITE(DNAME(DIV),801) I,PLUS(J)
	                  ENDIF
	                ELSE
	                  MATCH_DIV(DIV) = I
                        ENDIF
                    ENDIF
                ENDDO
            ENDDO                                       
        ENDDO
	IF(DLTLFL.NE.0) THEN
          DO I=1,LTGDIV
            DO J=1,2
              IF(DLTLNC(J,I).NE.0) THEN
                DIV = DLTLNC(J,I)
                WRITE (DNAME(DIV),804) MATCH_DIV(I),LUCKY_NUM(J)
              ENDIF
            ENDDO
          ENDDO
          DIV = DLTLDV
          WRITE(DNAME(DIV),803)
	ENDIF
C
        ! set bonus flag as no bonus for Lotto
        BNS = 1
C                                                                               
C                                                                               
C GET TOTAL SALES                                                               
C       
        TOTSAL=0.0D0                                                   
        DO I = 1, LTGENT                                           
            TOTSAL = TOTSAL + DFLOAT(DLTSAL(I))                            
        ENDDO
C                                                                               
C                                                                               
        TOTASH = 0.0D0                                                 
        TOTWON = 0.0D0                                             
        TOTCAR = 0.0D0                                             
        TOTSHR = 0                                                 
        TOTBRK = 0                                                 
C                                                                               
C                                                                               
        DO I = 1, LTGDIV
           TOTWON = TOTWON + DFLOAT(DLTSHR(I,BNS) * DLTSHV(I,BNS))        
           TOTSHR = TOTSHR + DLTSHR(I,BNS)                        
           TOTBRK = TOTBRK + DLTBRK(I) * DLTSHR(I,BNS)
        ENDDO

C                                                                               
C GET POOL AMOUNTS                                                              
C                                                                               
        TPOOL  = 0.0D0                                                 
        TREAL  = 0.0D0                                                         
        TRESI  = 0.0D0                                                                 
        TOTPOL = TOTSAL*DFLOAT(DYN_BETUNIT)*CALPER(DLTSPR) + DFLOAT(DLTSPL)
        TPOOL  = TOTSAL*DFLOAT(DYN_BETUNIT)*CALPER(DLTPAW) + DFLOAT(DLTSPL)

C       DPOOL(1) = DFLOAT(DLTSHV(1,BNS)*DYN_BETUNIT*DLTSHR(1,BNS))   
        DO I = 1, DLTDIV                                           
           DPOOL(I) = TPOOL*CALPER(DLTPER(I))     
        END DO
        WINBAL = DLTRES(1) + DLTRES(2) - TOTBRK
C                                                                
C CALCULATE ACTUAL ADVANCE SHARES                                               
C                                                                               
	NEXTJACKPOT = 0                
        DO I = 1, DLTDIV                                           
            IF(DLTSHR(I,BNS).EQ.0) THEN                            
                IF(I.EQ.1) THEN
		    NEXTJACKPOT = IDNINT(DPOOL(I)) + DLTPOL(1) ! POOL + JACKPOT
                    TPOOL = TPOOL - DPOOL(I)                                 
                    DPOOL(I)  = 0.0D0 
                    DLTASH(I) = 0  
                ENDIF                                                        

                IF(I.NE.DLTDIV) THEN                                         
                    DPOOL(I+1)  = DPOOL(I+1) + DPOOL(I)                      
                    DLTASH(I+1) = DLTASH(I+1) + DLTASH(I)                    
                    DPOOL(I)=0.0D0                                        
                    DLTASH(I)=0                             
                ENDIF                                                        
            ENDIF                                                            
        ENDDO

        DO I = 1, DLTDIV
           GROSS(I) = DFLOAT(DLTSHV(I,BNS) + DLTSTX(I,BNS))
           GROSS(I) = GROSS(I)*DFLOAT(DYN_BETUNIT) + DFLOAT(DLTBRK(I))
        ENDDO

        FINALTOT = TOTSAL*DFLOAT(DYN_BETUNIT)*CALPER(DLTPAW) + DFLOAT(DLTSPL)

        IF(DLTSHR(1,BNS).NE.0) THEN
            J=1
            DO WHILE (J.LT.3)
                IF(DRWSHR(1,BNS,J).EQ.0) THEN
                    TOTCAR=TOTCAR+DRWSAL(J)*DFLOAT(DYN_BETUNIT)*
     *                                             CALPER(DLTPER(1))
                    J=J+1
                ELSE
                    J=3             ! SKIP OUT
                ENDIF
            ENDDO
            DLTASH(1) = DLTASH(1) + IDNINT(TOTCAR/DFLOAT(DYN_BETUNIT))
C    *                            + DLTAPL !V21
        ELSEIF(DLTSHR(2,BNS).NE.0) THEN
            IF(DRWSHR(1,BNS,1).EQ.0.AND.
     *         DRWSHR(1,BNS,2).EQ.0)
     *            TOTCAR = DRWSAL(2) * DFLOAT(DYN_BETUNIT)
     *                              * CALPER(DLTPER(1))
            DLTASH(2) = DLTASH(2) + IDNINT(TOTCAR/DFLOAT(DYN_BETUNIT))
        ENDIF

        DO I=1,LTGDIV
           TOTASH = TOTASH + DFLOAT(DLTASH(I))
        ENDDO

        IF(DLTSHR(1,BNS).NE.0) THEN 
C           FROM_WFUND = DLTAPL - DLTRES(2)/DYN_BETUNIT !V21
            FROM_WFUND = DLTAPL
        ELSE
            FROM_WFUND = 0
	ENDIF

        TOTWPL = TPOOL + TOTCAR + (FROM_WFUND*DFLOAT(DYN_BETUNIT))

        TOTCAR = TOTCAR + DFLOAT(DLTAPL*DYN_BETUNIT)
        TOTASH = 0.0D0                           
        TOTGRS = 0.0D0   
	TPOOL = 0.0D0
        TOTPER = 0
        DO I = 1, DLTDIV           
	   DREAL(I) = DFLOAT(DLTSHR(I,BNS) * DLTSHV(I,BNS))	   
	   TREAL = TREAL + DREAL(I)
           TOTPER = TOTPER+DLTPER(I)                                                    
	   IF((GIND.EQ.LOTQ.OR.GIND.EQ.LOTS).AND.I.EQ.DLTLDV) THEN
	     DLTPER(I) = 100000-TOTPER
	     DPOOL(I)  = FINALTOT*CALPER(DLTPER(I)) 
           ELSE
             IF(I.NE.1) THEN 
               DRESI(I) = DPOOL(I) - DREAL(I) + DLTPOL(I)
               TRESI = TRESI + DRESI(I)
             ENDIF
	   ENDIF
	   TPOOL = TPOOL + DPOOL(I)
	   ! do not show notional percentage for LN div (div=6)
	   ! IF((GIND.EQ.LOTQ.OR.GIND.EQ.LOTS).AND.I.EQ.DLTLDV) THEN 
	   !   DLTPER(I) = IDNINT((DPOOL(I)/FINALTOT)*100000)
	   ! ENDIF
           TOTASH = TOTASH + DFLOAT(DLTASH(I))
           TOTGRS = TOTGRS + GROSS(I) * DFLOAT(DLTSHR(I,BNS)) 
        ENDDO
        IF(DLTSHR(1,BNS).NE.0) THEN 
          TDIST = TREAL-DLTPOL(1)
        ELSE
          TDIST = TREAL
        ENDIF
        
        WRITE(8,901) (TOTSAL*DFLOAT(DYN_BETUNIT)) / 100.0D0,
     *               FINALTOT / 100.0D0,                             
     *               DISPER(DLTPAW),                                         
     *               DLTPOL(1) / 100.0D0, 
     *               TDIST / 100.0D0
C    *               CMONY(DLTRES(1),7,BETUNIT)        
        DO I = 1, DLTDIV 
           WRITE(8,902) I,DNAME(I),
     *                  DPOOL(I)/100.0D0,
     *                  DISPER(DLTPER(I)),
     *                  DREAL(I)/100.0D0,
     *                  DLTSHR(I,BNS),
     *                  CMONY(DLTSHV(I,BNS),14,VALUNIT),
     *                  DRESI(I)/100.0D0        
C
           IF(GIND.EQ.LOTQ.OR.GIND.EQ.LOTS) THEN                                !V27
             LTSHDATA.LTDIVNUMB(I) = I                                          !V27 SAVE NUMBER OF PRIZE DIVISION
             LTSHDATA.LTDIVNAME(I) = DNAME(I)                                   !V27 SAVE NAME OF PRIZE DIVISION
             LTSHDATA.LTDIVPOOL(I) = DPOOL(I)/100.0D0                           !V27 SAVE TOTAL POOL VALUE OF PRIZE DIVISION
             LTSHDATA.LTDIVPERC(I) = DISPER(DLTPER(I))                          !V27 SAVE ASSIGNED PERCENTAGE OF PRIZE DIVISION
             LTSHDATA.LTDIVREAL(I) = DREAL(I)/100.0D0                           !V27 SAVE TOTAL EFECTIVE VALUE OF PRIZE DIVISION
             LTSHDATA.LTDIVSHRQ(I) = DLTSHR(I,BNS)                              !V27 SAVE TOTAL PRIZED BETS OF PRIZE DIVISION
             LTSHDATA.LTDIVSHRV(I) = DLTSHV(I,BNS)                              !V27 SAVE SHARE AMOUNT VALUE OF PRIZE DIVISION
             LTSHDATA.LTDIVRESI(I) = DRESI(I)/100.0D0                           !V27 SAVE TOTAL RESIDUAL VALUE OF PRIZE DIVISION
           ENDIF                                                                !V27
        ENDDO
        
	TOTBREAK = TOTWPL - (TOTWON*DFLOAT(DYN_VALUNIT))
	TOTPENNY = 0
	IF(TOTWON.GT.0) TOTPENNY = TOTBREAK        

	TOTBRUTTO = TOTPENNY+TOTWON*DFLOAT(DYN_BETUNIT)

        WRITE(8,9057) TPOOL/100.0D0,
     *                TREAL/100.0D0,                
     *                TOTSHR,
     *                TRESI/100.0D0
C    *                TOTBRUTTO/100.0D0
C
C WRITE TITLE TO WRITE TOTALS
C
C       CALL TITLE(HEAD1, REPNAM(1:8), REV,8 , PAGE, DAYCDC)                     
        WRITE(8, 100)
	WRITE(8, 201)
C
        IF(GIND.EQ.LOTQ.OR.GIND.EQ.LOTS) THEN                                   !V27
          LTSHDATA.LTTOTSALE = TOTSAL                                           !V27 SAVE TOTAL SALES
          LTSHDATA.LTTOTPOOL = TPOOL                                            !V27 SAVE TOTAL SALES FOR PRIZE DISTRIBUTION
          LTSHDATA.LTPREVJKT = DLTPOL(1)                                        !V27 SAVE PREVIOUS JACKPOT AMOUNT
          LTSHDATA.LTTOTRESI = TRESI/100.0D0                                    !V27 SAVE TOTAL ROUNDING AMOUNT
          LTSHDATA.LTNEXTJKT = NEXTJACKPOT                                      !V27 SAVE NEXT JACKPOT AMOUNT
        ENDIF                                                                   !V27
C
C WRITE TOTALS
C
        WRITE(8, 123) CMONY(INT(TOTSAL), 14, BETUNIT), DISPER(DLTPAW)
	WRITE(8, 112) CMONY(INT(TPOOL), 14, BETUNIT)
	WRITE(8, 114) CMONY(DLTPOL(1), 14, BETUNIT)
C       WRITE(8, 126) CMONY(DLTRES(1), 14, BETUNIT)
        WRITE(8, 127) TRESI/100.0D0
        WRITE(8, 118) CMONY(NEXTJACKPOT, 14, BETUNIT)
C
C CALCULATE NOTIOINAL DATA RESULTING FROM FIXED PRIZE FOR 
C TOTOLOTO QUARTA/SABADO LUCKY NUMBER DIVISION 6
C
	IF((GIND.EQ.LOTQ.OR.GIND.EQ.LOTS).AND.DLTLDV.NE.0) THEN
C	  TOTPER = 0
C	  DO I=1,DLTLDV-1
C	    TOTPER = TOTPER+DLTPER(I)
C	  ENDDO
C	  TOTPER = 100000-TOTPER
          WRITE(8, 149)
	  WRITE(8, 150) DPOOL(DLTLDV)/100.0D0
	  WRITE(8, 155) DREAL(DLTLDV)/100.0D0
	  IF(DPOOL(DLTLDV).GT.DREAL(DLTLDV)) NSDIFS = 'REMANESCENTE - FUNDO RESERVA    '
	  IF(DPOOL(DLTLDV).LT.DREAL(DLTLDV)) NSDIFS = 'DIFERENCAS - FUNDO RESERVA      '	  
	  WRITE(8, 160) NSDIFS,(DPOOL(DLTLDV)-DREAL(DLTLDV))/100.0D0
        LTSHDATA.LTLKYPOOL = DPOOL(DLTLDV)/100.0D0                              !V27 SAVE TOTAL PRIZE AMOUNT POOL OF LUCKY NUMBER DIVISION
        LTSHDATA.LTLKYRESP = DREAL(DLTLDV)/100.0D0                              !V27 SAVE TOTAL PRIZE AMOUNT RESPONSABILITY OF LUCKY NUMBER DIVISION
        LTSHDATA.LTLKYDIFF = (DPOOL(DLTLDV)-DREAL(DLTLDV))/100.0D0              !V27 SAVE DIFFERENCE BETWEEN TOTAL PRIZE AMOUNT POOL AND TOTAL PRIZE AMOUNT RESPONSABILITY OF LUCKY NUMBER DIVISION
        LTSHDATA.LTCTRFLG = 1                                                   !V27 LTSHDATA STRUCTURE LOADED SUCCESSFULLY
	ENDIF
C
C  CLOSE FILES AND EXIT
C
        CALL CLOSEFIL(FDB)                                                   
C
        RETURN                                                               
C                                                                               
C FORMATS DEFINITION                   
C
100   FORMAT(1X, 131('-'))
112   FORMAT(20X,  'TOTAL DESTINADO A PREMIOS       : ', A14)
114   FORMAT(20X,  'JACKPOT DA SEMANA ANTERIOR      : ', A14)
123   FORMAT(20X,  'TOTAL RECEITA                   : ', A14,
     *     05X   F6.2 ' % DA RECEITA DESTINADO A PREMIOS', /)
118   FORMAT(20X,  'JACKPOT P/ PROXIMO CONCURSO     : ', A14,/)
126   FORMAT(20X,  'VALOR DE ARREDONDAMENTO         : ', A14)
127   FORMAT(20X,  'VALOR DE ARREDONDAMENTO         : ', F14.2)
149   FORMAT(20X,  '================================================',/)
150   FORMAT(20X,  'NUMERO SORTE - VALOR DA RECEITA : ', F14.2)
155   FORMAT(20X,  'NUMERO SORTE RESPONSABILIDADE   : ', F14.2)
160   FORMAT(20X, A32,': ', F14.2)
C
201   FORMAT(X, /
     *       X, '=============', /
     *       X  'T O T A I S :', /
     *       X, '=============', /, /)
C
801     FORMAT(I1,1A)                                                        
802     FORMAT('JOGO: ',4A4)
803     FORMAT('NS  ')
804     FORMAT(I1,A3)
900	FORMAT(1X,131('-'),//,
     *         20X,A,1X,'SORTEIO ',I3.3,'/',I4,//,
     *         1X,91('-'),/)
9013    FORMAT(04X,'TOTAL RECEITA',
     *         05X,'TOTAL PARA PREMIOS RECEITA   %',
     *         08X,'JACKPOT',
     *         06X,'TOTAL DISTRIBUIDO')
C    *         11X,'RESIDUO'/)
901     FORMAT(3X,F12.2,13X,F12.2,7X,F5.2,2X,F13.2,     
C    *         10X,F13.2,11X,A7,//,     
     *         10X,F13.2,11X,//,     
     *  01X, 91('-'),///,                                             
     *  01X,'CLASSE             VALOR PREVISTO           VALOR EFECTIVO     QUANTIDADE APOSTAS           VALOR UNITARIO',
     *  '              ',/,                                     
     *  01X,'PREMIO  ACERTOS         PREMIO          %       PREMIO              PREMIADAS                   PREMIO    ',
     *  '       RESIDUO',/,
     *  1X,120('-'), /)                                                  
902     FORMAT(3X,I2,6X,A4,4X,F14.2,2X,F7.2,3X,F14.2,9X,I11,14X,A14,2X,F12.2)
9054    FORMAT(/ ,45('-'))
9057    FORMAT(/,1X,120('='),//,                                            
     *         1X,'TOTAIS:',11X,F14.2,12X,F14.2,9X,I11,32X,F10.2,//)
9001    FORMAT('RELATORIO DE PREMIOS DO TOTOLOTO PARA ',I3.3,              
     *         '/',I4.4)                                                     
C9002    FORMAT(1X)                                                           
9003    FORMAT('LOTTO  WIN RESERVE FUND FOR WEEK ',I2.2,              
     *         '/',I4.4,'  ')                                                
        END                                                                  
