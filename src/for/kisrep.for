C  GXSRC:KISREP.FOR
C  
C V02 22-DEC-2016 SCML Save JOKER share data into a data structure for
C                      further interface file generation
C V01 19-MAR-2011 FJG Lotto2 Batch label changes
C
C  $Log:   GXAFXT:[GOLS]KISREP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:43:34   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.3   01 Feb 1995 21:14:58   HXK
C  MINOR CHANGES
C  
C     Rev 1.2   16 Dec 1994 14:32:06   JXP
C  
C     Rev 1.1   29 Nov 1994 17:26:50   JXP
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    29 Nov 1994 17:25:56   JXP
C  Initial revision.
C  
C KISREP.FOR
C
C SUBROUTINE TO PRINT JOKER / KICKER SHARE REPORT.
C
C SINCE V02 VERSION SAVES JOKER SHARE DATA FOR FURTHER JOKER SHARE
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
C Copyright 1995 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
CV02        SUBROUTINE KISREP(GNUM,GIND,DRAW,COPY)                             
        SUBROUTINE KISREP(GNUM,GIND,DRAW,COPY,KISHDATA)                         !V02
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'                                        
        INCLUDE 'INCLIB:RECSCF.DEF'                                        
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:KISHFIL.DEF'                                            !V02

        !arguments
        INTEGER*4  GNUM             ! game number
        INTEGER*4  GIND             ! game index
        INTEGER*4  DRAW             ! draw number
        INTEGER*4  COPY             ! number of report copies

        ! variables
        INTEGER*4  FDB(7)           ! file dscription block
        INTEGER*4  DPOOL(KIGDIV)    !
        INTEGER*4  GROSS(KIGDIV)    ! (in pennies)                           
        INTEGER*4  ACTPER(KIGDIV)   !                                      
        INTEGER*4  I                ! counter
        INTEGER*4  J                ! counter
        INTEGER*4  TOTSAL           ! total sales
        INTEGER*4  TOTASH           !
        INTEGER*4  TOTWON           !
        REAL*8     TOTCAR           !
        INTEGER*4  TOTPER           !
        INTEGER*4  TOTSHR           !
        INTEGER*4  TOTBRK           ! (in pennies)
        INTEGER*4  TOTWPL           !
        INTEGER*4  TOTGRS           ! (in pennies)
        INTEGER*4  TPOOL            !
        INTEGER*4  FINALTOT         !
        INTEGER*4  REV              !
        INTEGER*4  PAGE             ! page number of report
        INTEGER*4  WEEK             ! week no for report
        INTEGER*4  ST               ! error status

	INTEGER*4  YEAR             ! Year in 4 digits
	INTEGER*4  TOTRES,PRZAMT
     
        CHARACTER*13  REPNAM        !                                      
        CHARACTER     HEAD1*49      !
        CHARACTER*7   DNAME(KIGDIV) !                                      
C
        RECORD /STCKISHFIL/ KISHDATA                                            !V27
C
        COMMON SCFREC

        DATA REPNAM/'            '/                                        
C                                                                               
	COPY=0
        ! open game file                                    
        CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)                              
        CALL IOINIT(FDB,3,DKKSEC*256)                                      
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)                     
C
        KISHDATA.KIGTYP = TKIK                                                  !V02 SAVE KICKER GAME TYPE
        KISHDATA.KIGNUM = GNUM                                                  !V02 SAVE KICKER GAME NUMBER
        KISHDATA.KIGIND = GIND                                                  !V02 SAVE KICKER GAME INDEX
C
        ! read record for DRAW
        CALL READW(FDB,DRAW,DKKREC,ST)                                     
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)                  
C                                                                               
        KISHDATA.KIDIV = DKKDIV                                                 !V02 TOTAL KICKER DIVISIONS
C                                                                               
        ! open report file
        WRITE(REPNAM,800) GIND                                             
800     FORMAT('JO',I1,'SHARE.REP')                                        
        CALL ROPEN(REPNAM,8,ST)                                            

        CALL FIGWEK(DKKESD,WEEK,YEAR)                                   
        PAGE=1                                                             
        WRITE(HEAD1,9001) WEEK,YEAR                                 
C
        KISHDATA.KICCC = WEEK                                                   !V02 SAVE DRAW NUMBER
        KISHDATA.KIYEAR = YEAR                                                  !V02 SAVE DRAW YEAR
C
        CALL TITLE(HEAD1,REPNAM(1:8),REV,8,PAGE,DAYCDC)                     
C       WRITE(8,9002)                                                      
        WRITE(8,900) WEEK,YEAR
C
C GET DIVISION NAMES
C
        DO I = 1, KIGDIV
           WRITE(DNAME(I),801) DKKMAT(1,I)
        ENDDO
C                                                                               
C                                                                               
C GET TOTAL SALES                                                               
C       
        TOTSAL=0                                                   
        DO I = 1, KIGENT                                           
          DO J = 1, MAXGAM
            TOTSAL = TOTSAL + DKKSAL(I,J)                            
          END DO
        END DO
C                                                                               
C                                                                               
        TOTASH = 0                                                 
        TOTWON = 0                                                 
        TOTCAR = 0                                                 
        TOTPER = 0                                                 
        TOTSHR = 0                                                 
        TOTBRK = 0                                                 
C                                                                               
C
        DO I = 1, KIGDIV                                           
            TOTWON = TOTWON + DKKSHR(I) * DKKSHV(I)        
            TOTSHR = TOTSHR + DKKSHR(I)                        
            TOTASH = TOTASH + DKKASH(I)                            
            TOTCAR = TOTCAR + DFLOAT(DKKPOL(1,I))*DFLOAT(DYN_BETUNIT)+
     *                        DFLOAT(DKKPOL(2,I))
            TOTBRK = TOTBRK + DKKBRK(I) * DKKSHR(I)            
        END DO
C                                                                               
C GET POOL AMOUNTS                                                              
C       TOTGRS = 0                                                                        
        TPOOL  = 0                                                 
        TPOOL  = IDNINT(DFLOAT(TOTSAL*DYN_BETUNIT) * CALPER(DKKSPR))

        DO I = 1, DKKDIV
          DPOOL(I) = DKKSHV(I)*DYN_BETUNIT * DKKSHR(I)
          TOTWPL = TOTWPL + DPOOL(I)
          GROSS(I) = DKKSHV(I)*DYN_VALUNIT + DKKBRK(I)
	  TOTGRS = TOTGRS + GROSS(I)
        END DO
C                                                                               
        FINALTOT = IDNINT(DFLOAT(TOTSAL*DYN_BETUNIT)*CALPER(DKKSPR))                     

	TOTRES = 0
	DO I=1,DKKDIV 
	    IF (DKKSHR(I).GT.0.AND.DKKPER(I).GT.0) THEN
	       IF (I.EQ.1.AND.DKKSHV(I).EQ.DKKMIN) THEN
		  PRZAMT = DKKMIN*DKKSHR(I)
	       ELSE
	          PRZAMT = FINALTOT * CALPER(DKKPER(I))
	       ENDIF
	       IF(DKKPER(I) .NE. 0) TOTRES = TOTRES + DKKRES(I)
	    ENDIF
	ENDDO

        WRITE(8,901) CMONY(TOTSAL,12,VALUNIT),
     *               DFLOAT(FINALTOT)/100.0D0,                             
     *               DISPER(DKKSPR),                                         
     *               CMONY(IDINT(TOTCAR),13,VALUNIT), 
     *               DFLOAT(TOTWPL)/100.0D0,
     *               CMONY(TOTRES,7,BETUNIT)

C                                                                               
        TOTASH = 0                                                           
        TOTGRS = 0                                                           
        DO I = 1, DKKDIV                                                     
            TOTASH = TOTASH + DKKASH(I)                                      
            TOTGRS = TOTGRS + GROSS(I) * DKKSHR(I)                       
            ACTPER(I) = 0	      
            WRITE(8,902) I,DNAME(I),
     *        DFLOAT(DPOOL(I))/100.0D0,                     
     *        DISPER(ACTPER(I)),
     *        DKKSHR(I),
     *        DFLOAT(GROSS(I))/100.0D0                    
	    TOTPER = TOTPER + ACTPER(I)   
C
          KISHDATA.KIDIVNUMB(I) = I                                             !V02 SAVE NUMBER OF PRIZE DIVISION
          KISHDATA.KIDIVNAME(I) = DNAME(I)                                      !V02 SAVE NAME OF PRIZE DIVISION
          KISHDATA.KIDIVPOOL(I) = DFLOAT(DPOOL(I))/100.0D0                      !V02 SAVE TOTAL POOL VALUE OF EACH PRIZE DIVISION
          KISHDATA.KIDIVPERC(I) = DISPER(ACTPER(I))                             !V02 SAVE ASSIGNED PERCENTAGE OF EACH PRIZE DIVISION
          KISHDATA.KIDIVSHRQ(I) = DKKSHR(I)                                     !V02 SAVE TOTAL PRIZED BETS OF EACH PRIZE DIVISION
          KISHDATA.KIDIVSHRV(I) = DFLOAT(GROSS(I))/100.0D0                      !V02 SAVE SHARE AMOUNT VALUE OF EACH PRIZE DIVISION
C
        END DO
C                                                                               
        WRITE(8,904) DFLOAT(TOTGRS)/100.0D0,
     *               DISPER(TOTPER),
     *               TOTSHR              
C
C WRITE TITLE TO WRITE TOTALS
C

        CALL TITLE(HEAD1, REPNAM(1:8), REV, 8, PAGE, DAYCDC)
        WRITE(8, 100)
        WRITE(8, 201)
C
C WRITE TOTALS
C
        WRITE(8, 123) CMONY(INT(TOTSAL), 14, BETUNIT), DISPER(DKKSPR)
        WRITE(8, 112) CMONY(INT(TPOOL), 14, BETUNIT)
        WRITE(8, 114) CMONY(IDINT(TOTCAR), 14, BETUNIT)
        WRITE(8, 126) CMONY(TOTRES, 14, BETUNIT)
C
        KISHDATA.KITOTSALE = TOTSAL                                             !V02 SAVE TOTAL SALES AMOUNT
        KISHDATA.KITOTPOOL = TPOOL                                              !V02 SAVE TOTAL SALES AMOUNT FOR PRIZE DISTRIBUTION
        KISHDATA.KIPREVJKT = TOTCAR                                             !V02 SAVE PREVIOUS JACKPOT AMOUNT
        KISHDATA.KITOTRESI = TOTRES                                             !V02 SAVE TOTAL ROUNDING AMOUNT
        KISHDATA.KICTRFLG = 1                                                   !V02 KISHDATA STRUCTURE LOADED SUCCESSFULLY
C
        CLOSE(UNIT=8)                                                        
        CALL SPOOL(REPNAM,COPY,ST)                                           
	RETURN

C                                                                               
C FORMATS DEFINITION                                                                               
C
100   FORMAT(1X, 131('-'))
112   FORMAT(20X, 'TOTAL DESTINADO A PREMIOS   : ', A14)
114   FORMAT(20X, 'JACKPOT DA SEMANA ANTERIOR  : ', A14)
123   FORMAT(20X, 'TOTAL RECEITA               : ', A14,
     *     05X   F6.2 ' % DA RECEITA DESTINADO A PREMIOS', /)
118   FORMAT(20X, 'JACKPOT P/ PROXIMO CONCURSO : ', A14)
126   FORMAT(20X, 'VALOR DE ARREDONDAMENTO     : ', A14)
C
201   FORMAT(X, /
     *       X, '=============', /
     *       X  'T O T A I S :', /
     *       X, '=============', /, /)
C
801     FORMAT(I7.7)                                                        
900	FORMAT(01X, 131('-'), //,                                               
     *         30X, 'JOGO DE JOKER, SEMANA ', I2, '/', I4, //,
     *         01X, 95('-'), //,                                               
     *         04X, 'TOTAL RECEITA', 3X, 'TOTAL PARA PREMIOS RECEITA', 1X, '%', 
     *         10X, 'JACKPOT', 4X, 'TOTAL DISTRIBUIDO',3X,'RESIDUO', /)
C
901     FORMAT(
     *   03X, A12, 12X, F12.2, 4X, F5.2, 4X, A13, 8X, F13.2, 3X, A7, /,
     *   01X, /, X, 95('-'),///,                                             
     *   03X, 'CLASSE', 7X,'       ',11X,'VALOR',  17X, 'QUANTIDA DE APOSTAS', 3X, 'VALOR UNITARIO',/,
     *   03X, 'PREMIO', 7X,'ACERTOS',11X,'PREMIO', 11X, '%',
     *   10X, 'PREMIADAS', 15X, 'PREMIO', /,
     *   01X, 95('-'))
C
902     FORMAT(7X,I2,7X, A7, 3X, F14.2, 5X, F7.2, 8X, I11, 6X, F15.2)
C
904     FORMAT(1X, 95('='), //,                                            
     *         1X,'TOTAIS:  ', 4X, F11.2, 6X, F6.2, 8X, I11, //)
C
9001    FORMAT('RELATORIO DE PREMIOS DO JOKER PARA ',I2.2,              
     *         '/',I4.4)                                                     
9002    FORMAT(1X)                                                           
9003    FORMAT('JOKER WIN RESERVE FUND FOR WEEK ',I2.2,              
     *         '/',I4.4,'  ')                                                

        END                                                                  
