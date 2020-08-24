C RELCONC.FOR
C
C V17 06-JAN-2017 SCML New interface files creation
C V16 03-JUN-2011 FRP OOB issue fixed
C V15 11-APR-2011 RXK Skip the game-loop if game is not defined
C V14 19-MAR-2011 FJG Lotto2 DIV6 percentage
C     21-MAR-2011 FJG COMISSAO title fix
C V13 13-MAR-2011 HXK Fix TOTOLOTO ccc assignments
C V12 07-JAN-2011 FJG Lotto2 Changes CCCWEEK
C V11 09-OCT-2009 CMB Create invjoker
C V10 01-AUG-2009 FJG Portugal Fiscal Legislation changes
C V09 12-JUL-2009 FRP Check DRWGME before calling INVLLOTOS
C     24-JUL-2009 FJG Problems with JOKER counters
c v08 11-SEP-2006 CMB INCLUDE 0% COMMISSION FOR DISTRICT 00
C V07 09-MAR-2005 CMB DONT ASK N. COPYS
C V06 10-FEB-2004 CMB CALL INVLLOTOS
c v05 10-FEB-2004 CMB DONT CREATE INVJOKER.REP
c v04 10-FEB-2004 CMB INCLUDE KICK AMOUNT  
C V03 23-JAN-2004 CMB ADD DISTRICT 00 TO REPORT
C V02 05-NOV-2003 FRP Modify for Batch2 Totobola Changes.
C V01 08-APR-01 CS  INITIAL RELEASE FOR PORTUGAL
C

C PROGRAM TO GENERATE A REPORT CONTAINING
C TOTAL SALES, COMISSION AND CARRYOVERS SOLD
C DURING A DRAW PERIOD ( JUST AFTER DRAW BREAK )
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
C Copyright 1992,1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C

C=======OPTIONS  /CHECK=NOOVERFLOW
        PROGRAM  RELCONC
	IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:TGLCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
        INCLUDE 'INCLIB:DTGREC.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:RECDAF.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'
        INCLUDE 'INCLIB:RELCONC.DEF'
        INCLUDE 'INCLIB:GMDRFIL.DEF'                                            !V17
C
C FUNCTION DEFINITIONS
C
	CHARACTER*32 MYCMONYI8
C
C LOCAL VARIABLES
C
	INTEGER*2 FSTDATE(12,MAXGAM),LSTDATE(12,MAXGAM)

	INTEGER*4 CLANG,DATECDC,UNIT,LOAD,INDNEXT,INDBEFR
	INTEGER*4 ST,COPY,LINCNT,POOL,IDX_FDB(7)
	INTEGER*4 GTYP,GIND,GNUM
	INTEGER*4 TKTBYST,TOTLIN
	INTEGER*4 CLOSTIM(MAXGAM)
	INTEGER*4 VERSION,PAGE,K,DRWGME(MAXGAM)
	INTEGER*4 NUMCARRY(0:NUMUF,MAXGAM)
	INTEGER*4 TOTTKTS(0:NUMUF,MAXGAM),NUMACTREVS(0:NUMUF,MAXGAM)
	INTEGER*4 REVS,BEGCDC(MAXGAM),ENDCDC(MAXGAM)
	INTEGER*4 TER,DAYS,REVSEQ,AGTNUMB,UF,UFS
        INTEGER*4 DAYSAUX,DAYGMES(MAXGAM),MULTI(MAXGAM)
	INTEGER*4 CNTTKTS(4,3,MAXGAM),FILES,STATUS
	INTEGER*4 FLAG,I,J,MAXDIV,JKRNBR
	INTEGER*4 TOTGAMS,SELGAMS
C	INTEGER*4 EXT

	INTEGER*8 ROLLOVER(KIGDIV)
	INTEGER*8 TMPPOOL
	INTEGER*8 TOTAREC
C
	INTEGER*4 CANTAB(6,MAXCAN,MAXGAM)                   !V04
	INTEGER*4 CANIND(MAXGAM)
	INTEGER*4 WEEK,YEAR,WEEKBEF,YEARBEF,WOFF
C
	INTEGER*4 COMAMT(2),TOTCOMAMT(2),TOTCOMMSAP(MAXGAM)
	INTEGER*8 I8_TOTCOMAMT
C
C 1-FLAG FOR REV CNT, 2-SALES AMOUNT, 3-UF
C
	INTEGER*8 AMTUFS(0:NUMUF,MAXGAM)
	INTEGER*8 TABREVS(3,NUMAGT,MAXGAM) 
	INTEGER*8 TOTSAL(0:NUMUF,MAXGAM)
	INTEGER*8 TOTCARRY(0:NUMUF,MAXGAM)
	INTEGER*8 TOTCOMM(0:NUMUF,MAXGAM),TOTAGT(MAXGAM)
	INTEGER*8 TOTARRECAD(MAXGAM),LIQCONC,PRMDIV(20,MAXGAM)
	REAL*8    LIQPREM
	INTEGER*8 POLMIN(MAXGAM)	
	INTEGER*8 NEXTDRWTOT(2,6,MAXGAM)				!NET AMOUNT AND COMMISSION PER DRAW

	CHARACTER*11 DESCFILE(3)/'VALIDOS    ','ANULADOS   ','INVALIDOS  '/
	CHARACTER*12 CONCRPT(MAXGAM),CONCLOAD(MAXGAM)

	LOGICAL GEN_REPORTS, ERRFLG, TOWRITE
                                          
C
C ARRAY TO KEEP ALL MULTIDRAW AMOUNTS (PER AGENT/GAME)
C
	INTEGER*4 NEXTDRWAMT(6,MAXGAM,NUMAGT)		!BETS SOLD DURING DRAW PERIOD
C
        INTEGER*4 TOTPER   ! V14	
C
C STRUCT FOR SAP FILE
C
	CHARACTER*20 FILNAM
	INTEGER*4    IFILNAM(5)
	EQUIVALENCE  (FILNAM,IFILNAM)
C
	STRUCTURE   /STCAMTDRW/
	    INTEGER*8	AMTDRWONL(6,MAXGAM)
	    INTEGER*8	COMDRWONL(6,MAXGAM)
            INTEGER*8	AMTDRWOFF(6,MAXGAM)
	    INTEGER*8	COMDRWOFF(6,MAXGAM)
	END STRUCTURE
C
	RECORD /STCAMTDRW/ REGAMTDRW, REGAMTAUX
C
        INTEGER*4 L                                                             !V17
        RECORD /STCGMDRFIL/ GMDRDATA                                            !V17
C
        WRITE(6,9000) IAM(),IAM(),'V11',IAM()
C
C INITIALIZE SOME VARIABLES
C
	DATECDC     = DAYCDC
	UNIT        = 33
	LOAD	    = 34
	POOL        = 0
	VERSION     = 1
	ERRFLG      = .FALSE.
	GEN_REPORTS = .FALSE.
	CLANG       = PORTUG
	JKRNBR      = 0
	TOTGAMS     = 0
	SELGAMS     = 0

	CALL FASTSET(0, MULTI,     SIZEOF(MULTI)/4)
	CALL FASTSET(0, TOTCARRY,  SIZEOF(TOTCARRY)/4)
	CALL FASTSET(0, TOTSAL,    SIZEOF(TOTSAL)/4)
	CALL FASTSET(0, TOTCOMM,   SIZEOF(TOTCOMM)/4)
	CALL FASTSET(0, TOTCOMMSAP,SIZEOF(TOTCOMMSAP)/4)
	CALL FASTSET(0, TOTARRECAD,SIZEOF(TOTARRECAD)/4)
	CALL FASTSET(0, PRMDIV,    SIZEOF(PRMDIV)/4)
	CALL FASTSET(0, TOTTKTS,   SIZEOF(TOTTKTS)/4)
	CALL FASTSET(0, NUMACTREVS,SIZEOF(NUMACTREVS)/4)
	CALL FASTSET(0, NUMCARRY,  SIZEOF(NUMCARRY)/4)
	CALL FASTSET(0, DRWGME,    SIZEOF(DRWGME)/4)
	CALL FASTSET(0, BEGCDC,    SIZEOF(BEGCDC)/4)
	CALL FASTSET(0, ENDCDC,    SIZEOF(ENDCDC)/4)
	CALL FASTSET(0, DAYGMES,   SIZEOF(DAYGMES)/4)
	CALL FASTSET(0, TABREVS,   SIZEOF(TABREVS)/4)
	CALL FASTSET(0, CNTTKTS,   SIZEOF(CNTTKTS)/4)
	CALL FASTSET(0, AMTUFS,    SIZEOF(AMTUFS)/4)
	CALL FASTSET(0, CANIND,    SIZEOF(CANIND)/4)
	CALL FASTSET(0, CANTAB,    SIZEOF(CANTAB)/4)
	CALL FASTSET(0, NEXTDRWAMT,SIZEOF(NEXTDRWAMT)/4)
	CALL FASTSET(0, NEXTDRWTOT,SIZEOF(NEXTDRWTOT)/4)
	CALL FASTSET(0, POLMIN,    SIZEOF(POLMIN)/4)	
C
C GET NUMBER OF REPORT COPIES
C
C      	CALL PRMNUM('Relatorio de ENCERRAMENTO DE CONCURSO - Copias:', !v07
C     *		    COPY,0,20,EXT)
C	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
C VERIFY GAME(S) TO BE PROCESSED
C 
	DO 200 GNUM=1,MAXGAM              !1,MAXGAM
	    GTYP = GNTTAB(GAMTYP,GNUM)
	    GIND = GNTTAB(GAMIDX,GNUM)
            IF(GTYP.EQ.0.OR.GIND.EQ.0) GOTO 200
	    TMPPOOL = 0
	    TOTAREC = 0
C	    
            IF(GTYP.EQ.TLTO) THEN
                CLOSTIM(GNUM) = LTOTIM(GIND)
            ELSEIF(GTYP.EQ.TSPT) THEN
                CLOSTIM(GNUM) = SPTTIM(GIND)
            ELSEIF(GTYP.EQ.TTGL) THEN
                CLOSTIM(GNUM) = TGLTIM(GIND)
            ENDIF	
C
C LOAD MEMORY ON RECORD AND
C CHECK IF TODAY IS A CLOSING DAY
C
	    IF((GTYP.EQ.TLTO.AND.LTOESD(GIND).EQ.DATECDC))THEN
                TOTGAMS = TOTGAMS + 1
                IF(LTOSTS(GIND).GE.GAMBFD) THEN
		  WRITE (5,2000) IAM(),(GLNAMES(K,GNUM),K=1,4)
		  CALL PRMYESNO('  SIM ou NAO [S/N] ?',FLAG)
		ELSE
		  WRITE (5,2010) IAM(),(GLNAMES(K,GNUM),K=1,4)		
		  FLAG=0
		ENDIF

		IF(FLAG.EQ.1) THEN
		    SELGAMS = SELGAMS + 1
		    CALL GAMLOG(GTYP,GIND,DLTREC,LTOSTS)
		    MULTI(GNUM) = LTOMLT(GIND) + LTOADW(GIND)
		    BEGCDC(GNUM) = DLTBSD
		    ENDCDC(GNUM) = DLTESD
		    DRWGME(GNUM) = DLTDRW
		    POLMIN(GNUM) = DLTMIN                  !V10
		    FSTDATE(VCDC,GNUM) = DLTBSD
		    CALL CDATE(FSTDATE(1,GNUM))
		    LSTDATE(VCDC,GNUM) = DLTESD
		    CALL CDATE(LSTDATE(1,GNUM))
		    WRITE(CONCRPT(GNUM),1000) GIND
		    WRITE(CONCLOAD(GNUM),1001) GIND
		    GEN_REPORTS = .TRUE.

		    !CALCULATE PRIZES INFO. (ONLY FOR ONLINE - WITHOUT OFFLINE 5 WEEKS)
		    DO I = 1, LTGENT-1
                       TOTARRECAD(GNUM) = TOTARRECAD(GNUM) + DLTSAL(I)
		       TOTAREC = TOTAREC + DLTSAL(I)
                    ENDDO
		    TMPPOOL = TOTAREC*CALPER(DLTSPR)

                    TOTPER = 0
                    DO I=1,DLTDIV
                       IF (DLTPER(I).NE.0) THEN
                         PRMDIV(I,GNUM) = TMPPOOL * CALPER(DLTPER(I))
                         PRMDIV(I,GNUM) = PRMDIV(I,GNUM) + DLTPOL(I)        !ADD POOL CARRIED OVER
                         TOTPER = TOTPER + DLTPER(I)
                       ELSE
                         IF(I.EQ.6) THEN
                           TOTPER = 100000 - TOTPER
                           PRMDIV(I,GNUM) = TMPPOOL * CALPER(TOTPER)            
                         ELSE
                           PRMDIV(I,GNUM) = -1
                         ENDIF
                       ENDIF
                    ENDDO
		ENDIF
	    ENDIF

	    IF((GTYP.EQ.TSPT.AND.SPTESD(GIND).EQ.DATECDC))THEN
                TOTGAMS = TOTGAMS + 1
                IF(SPTSTS(GIND).GE.GAMBFD) THEN
		  WRITE (5,2000) IAM(),(GLNAMES(K,GNUM),K=1,4)
		  CALL PRMYESNO('  SIM ou NAO [S/N] ?',FLAG)
		ELSE
		  WRITE (5,2010) IAM(),(GLNAMES(K,GNUM),K=1,4)		
		  FLAG=0
		ENDIF

		IF(FLAG.EQ.1) THEN
		    SELGAMS = SELGAMS + 1		        
		    CALL GAMLOG(GTYP,GIND,DSPREC,SPTSTS)
		    BEGCDC(GNUM) = DSPBSD
		    ENDCDC(GNUM) = DSPESD
		    DRWGME(GNUM) = DSPDRW
		    FSTDATE(VCDC,GNUM) = DSPBSD
		    CALL CDATE(FSTDATE(1,GNUM))
		    LSTDATE(VCDC,GNUM) = DSPESD
		    CALL CDATE(LSTDATE(1,GNUM))
		    WRITE(CONCRPT(GNUM),1010) GIND
		    WRITE(CONCLOAD(GNUM),1011) GIND
		    GEN_REPORTS = .TRUE.

		    !CALCULATE PRIZES INFO. (ONLY FOR ONLINE - WITHOUT OFFLINE 5 WEEKS)
		    DO I = 1, SPGENT-1
                       TOTARRECAD(GNUM) = TOTARRECAD(GNUM) + DSPSAL(I)
                       TOTAREC = TOTAREC + DSPSAL(I)
                    ENDDO
		    TMPPOOL = TOTAREC * CALPER(DSPSPR)

      		    DO I=1,DSPDIV
                      IF(DSPDCD .EQ. 0) THEN
                         PRMDIV(I, GNUM) = TMPPOOL * CALPER(DSPPER(I)) + DSPPOL(I) + DSPASH(I)
                      ELSE
                         PRMDIV(I, GNUM) = TMPPOOL * CALPER(DSPPER(I)) ! CANCELLED DRAW, DON'T ADD JACKPOTS
                      ENDIF
                      IF(I .EQ. 1 .AND. DSPPOL(I) .NE. 0 .AND. GIND .EQ. 1 .AND. DSPDCD .EQ. 0) THEN
                        PRMDIV(I,GNUM) = PRMDIV(I,GNUM) + DSPAPL
                      ENDIF
	            ENDDO

		ENDIF
	    ENDIF

	    IF((GTYP.EQ.TTGL.AND.TGLESD(GIND).EQ.DATECDC))THEN
                TOTGAMS = TOTGAMS + 1
                IF(TGLSTS(GIND).GE.GAMBFD) THEN
		  WRITE (5,2000) IAM(),(GLNAMES(K,GNUM),K=1,4)
		  CALL PRMYESNO(' SIM ou NAO [S/N] ?',FLAG)
		ELSE
		  WRITE (5,2010) IAM(),(GLNAMES(K,GNUM),K=1,4)		
		  FLAG=0
		ENDIF

		IF(FLAG.EQ.1) THEN
		    SELGAMS = SELGAMS + 1		        
		    CALL GAMLOG(GTYP,GIND,DTGREC,TGLSTS)
		    BEGCDC(GNUM) = DTGBSD
		    ENDCDC(GNUM) = DTGESD
		    DRWGME(GNUM) = DTGDRW
		    FSTDATE(VCDC,GNUM) = DTGBSD
		    CALL CDATE(FSTDATE(1,GNUM))
		    LSTDATE(VCDC,GNUM) = DTGESD
		    CALL CDATE(LSTDATE(1,GNUM))
		    WRITE(CONCRPT(GNUM),1020) GIND
		    WRITE(CONCLOAD(GNUM),1021) GIND
		    GEN_REPORTS = .TRUE.

		    !CALCULATE PRIZES INFO. (ONLY FOR ONLINE - WITHOUT OFFLINE 5 WEEKS)
		    DO I = 1, TGGENT-1
                       TOTARRECAD(GNUM) = TOTARRECAD(GNUM) + DTGSAL(I)
                       TOTAREC = TOTAREC + DTGSAL(I)
                    ENDDO
		    TMPPOOL = TOTAREC * CALPER(DTGSPR)

      		    DO I=1,DTGDIV
                      PRMDIV(I,GNUM) = TMPPOOL*CALPER(DTGPER(I)) + DTGPOL(I) + DTGASH(I)
                      IF(I.EQ.1 .AND. DTGPOL(I).NE.0 .AND. GIND.EQ.1)
     *                     PRMDIV(I,GNUM) = PRMDIV(I,GNUM) + DTGAPL
	            ENDDO
		ENDIF
	    ENDIF

	    IF(GTYP.EQ.TKIK) THEN
              IF(KIKESD(GIND).EQ.DATECDC)THEN
                TOTGAMS = TOTGAMS + 1
	        IF(KIKSTS(GIND).GE.GAMBFD) THEN
		  WRITE (5,2000) IAM(),(GLNAMES(K,GNUM),K=1,4)
		  CALL PRMYESNO('  SIM ou NAO [S/N] ?',FLAG)
		ELSE
		  WRITE (5,2010) IAM(),(GLNAMES(K,GNUM),K=1,4)		
		  FLAG=0
		ENDIF		

		IF(FLAG.EQ.1) THEN
		    SELGAMS = SELGAMS + 1		        
		    JKRNBR = GNUM
		    CALL GAMLOG(GTYP,GIND,DKKREC,KIKSTS)
		    BEGCDC(GNUM) = DKKBSD
		    ENDCDC(GNUM) = DKKESD
		    DRWGME(GNUM) = DKKDRW
		    FSTDATE(VCDC,GNUM) = DKKBSD
		    CALL CDATE(FSTDATE(1,GNUM))
		    LSTDATE(VCDC,GNUM) = DKKESD
		    CALL CDATE(LSTDATE(1,GNUM))
		    WRITE(CONCRPT(GNUM),1030) GIND
		    WRITE(CONCLOAD(GNUM),1031) GIND
		    GEN_REPORTS = .TRUE.

		    !CALCULATE PRIZES INFO. (ONLY FOR ONLINE - WITHOUT OFFLINE 5 WEEKS)
                    DO I = 1, KIGENT-1
                      DO J = 1, MAXGAM
                        TOTARRECAD(GNUM) = TOTARRECAD(GNUM)+DKKSAL(I,J)
                        TOTAREC = TOTAREC + DKKSAL(I,J)
                      ENDDO
                    ENDDO
		    TMPPOOL = TOTAREC * CALPER(DKKSPR)

                    DO I = 1, DKKDIV
                       IF (DKKPER(I).NE.0) THEN
                          ROLLOVER(I) = DKKPOL(1,I)*DYN_BETUNIT + DKKPOL(2,I)
                          PRMDIV(I,GNUM) = TMPPOOL*CALPER(DKKPER(I)) + (ROLLOVER(I)/DYN_BETUNIT)
                       ELSE
                          PRMDIV(I,GNUM) = DKKSHV(I)
                       ENDIF 
                    ENDDO
                    PRMDIV(1,GNUM)=-1 !In case negative, don't show anything

		ENDIF
	      ENDIF
	    ENDIF
200     CONTINUE

	IF(.NOT.GEN_REPORTS) THEN
	    TYPE *,IAM(),' Nao sera gerado nenhum relatorio'
	    CALL GSTOP(GEXIT_SUCCESS)
        ELSE
       	  IF(TOTGAMS.EQ.SELGAMS) THEN
	    TYPE *,IAM(),' SIM sera escrito no ficheiro de SAP'       	        
            TOWRITE = .TRUE.
          ELSE
	    TYPE *,IAM(),' NAO sera escrito no ficheiro de SAP'       	                        
            TOWRITE = .FALSE.
          ENDIF	                	    
	ENDIF
C
C GET CARRYOVERS FOR NEXT DRAWS BY GAME
C
	TYPE *,IAM(),' Buscando 5 Semanas nos arquivos de DRAW'
	CALL DRAWCARRY(DRWGME,BEGCDC,ENDCDC,TOTCARRY,
     *                 NUMCARRY,CNTTKTS, AMTUFS,CANTAB,CANIND,ERRFLG,JKRNBR,NEXTDRWAMT)

	TYPE *,IAM(),' Buscando 5 Semanas no TCF'
	CALL MOUSECARRY(DRWGME,ENDCDC,TOTCARRY,NUMCARRY,MULTI,
     *                  CNTTKTS,AMTUFS,CANTAB,CANIND,ERRFLG,JKRNBR)

	TYPE *,IAM(),' Buscando 5 Semanas no TMF'
	CALL TMFCARRY(DRWGME,ENDCDC,TOTCARRY,
     *		      NUMCARRY,CNTTKTS,AMTUFS,CANTAB,CANIND,ERRFLG,JKRNBR,NEXTDRWAMT)
C
C OPEN ASF FILE AND GET SALES ON DRAW PERIOD
C FOR ALL CLOSING GAMES...
C
	CALL OPENASF(ASF)
	
	

	DO  TER=1,NUMAGT
	    CALL FASTSET(0,TOTAGT,SIZEOF(TOTAGT)/4)

	    IF(MOD(TER,1000).EQ.0) TYPE *,IAM(),TER,' terminais processados'
	    CALL READASF(TER,ASFREC,ST)
	    IF(ST.NE.0) CALL FILERR(GFNAMES(1,ASF),2,ST,TER)

	    AGTNUMB = AGTTAB(AGTNUM,TER)
	    IF (AGTNUMB.GT.0) THEN
	      UF = AGTNUMB/100000
	      IF (UF.LT.0.OR.UF.GT.NUMUF) UF = NUMUF
	      

	      DO  GNUM=1,MAXGAM
		IF(BEGCDC(GNUM).GT.0 .AND. ENDCDC(GNUM).EQ.DATECDC) THEN
C
C GET THE MAXIMUM RANGE TO BE READ FROM ASF
C
CCCC                  DAYSAUX = ASFDAT(ASFCDC,1)-BEGCDC(GNUM)+2
                  DAYSAUX = DATECDC-BEGCDC(GNUM)
                  IF (DAYSAUX.GT.ANUMDAY) DAYSAUX = ANUMDAY
C
                  DO DAYS = 1,DAYSAUX
C
C READ ALL ASFDAY TOTALS
C
		     TOTAGT(GNUM) = TOTAGT(GNUM) + 
     *			    ASFDAY(GSAMT,GNUM,DAYS) - ASFDAY(GCAMT,GNUM,DAYS) 
		     TOTTKTS(UF,GNUM) = TOTTKTS(UF,GNUM) +
     *		            ASFDAY(GSCNT,GNUM,DAYS) - ASFDAY(GCCNT,GNUM,DAYS) 
		     TOTSAL(UF,GNUM) = TOTSAL(UF,GNUM) +
     *		            ASFDAY(GSAMT,GNUM,DAYS) - ASFDAY(GCAMT,GNUM,DAYS)

		     IF (ASFDAY(GSAMT,GNUM,DAYS).NE.0.AND.UF.EQ.NUMUF)THEN 
		        TYPE*,IAM(),' (2)UF INVALIDA NO TERMINAL',TER
		     ENDIF
C
		  ENDDO
C
C READ TODAY FROM MEMORY
C
		  TOTAGT(GNUM) = TOTAGT(GNUM) +	 AGTGAM(GSAMT,GNUM,TER) - 
     *						 AGTGAM(GCAMT,GNUM,TER)
		  TOTTKTS(UF,GNUM) = TOTTKTS(UF,GNUM) + 
     *                        AGTGAM(GSCNT,GNUM,TER) - AGTGAM(GCCNT,GNUM,TER)
		  TOTSAL(UF,GNUM)  = TOTSAL(UF,GNUM) +
     *                        AGTGAM(GSAMT,GNUM,TER) - AGTGAM(GCAMT,GNUM,TER)

		  IF(AGTGAM(GSAMT,GNUM,TER).NE.0.AND.UF.EQ.NUMUF)THEN 
		    TYPE*,IAM(),' (3)UF INVALIDA NO TERMINAL',TER
		  ENDIF

		  REVSEQ = TER
C
CC		  REVSEQ = AGTNUMB/100000
CC		  IF  (REVSEQ.EQ.0.OR.REVSEQ.EQ.99) THEN
CC		      IF(TOTAGT(GNUM).GT.0) THEN
CC     		        TYPE*,IAM(),'Erro no revendedor ',AGTNUMB,
CC     *		           ' terminal = ',TER,' e central de recepcao'
CC			CALL GPAUSE()
CC		      ENDIF
CC		  ELSE
C
		      TABREVS(2,REVSEQ,GNUM) = TABREVS(2,REVSEQ,GNUM) + 
     *					       TOTAGT(GNUM)
		      TABREVS(3,REVSEQ,GNUM) = UF
		      IF(TOTAGT(GNUM).GT.0.AND.TABREVS(1,REVSEQ,GNUM).EQ.0) THEN
			TABREVS(1,REVSEQ,GNUM) = 1
			NUMACTREVS(UF,GNUM) = NUMACTREVS(UF,GNUM) + 1
		      ENDIF
CC		  ENDIF
		ENDIF
	      ENDDO   !GNUM...
	    ENDIF
	ENDDO	    !TER....

	CALL CLOSASF
C
C CALCULATE COMMISION BY AGENT
C
	DO REVS=1,NUMAGT
	  DO GNUM=1,MAXGAM
	    IF(BEGCDC(GNUM).GT.0 .AND. ENDCDC(GNUM).EQ.DATECDC) THEN

		IF  ( TABREVS(3,REVS,GNUM).LT.0 .OR.
     *		      TABREVS(3,REVS,GNUM).GT.NUMUF ) THEN
		    TYPE*,IAM(),' Erro de DISTRITO no revendedor',REVS,
     *		                ' DIST= ',TABREVS(3,REVS,GNUM),' jogo ',GNUM
                    CALL GPAUSE()
		ENDIF

		IF(TABREVS(2,REVS,GNUM).GT.0) THEN
	          POOL = TABREVS(2,REVS,GNUM)
		  CALL FASTSET(0, TOTCOMAMT, SIZEOF(TOTCOMAMT)/4)
      		  CALL GETCOM( POOL,
     *			       TWAG,
     *			       GNUM,
     *			       COMAMT,
     *			       TOTCOMAMT,
     *                         0, 0, 0   )
     
C IF NO COMMISSION FLAG DON'T SEND COMMISSION     !V08
C
      IF(TSBIT(AGTTAB(AGTTYP,REVS),AGTNCM))THEN
                    TOTCOMAMT(1)=0
                    TOTCOMAMT(2)=0
                ENDIF                               !V08
		  I8_TOTCOMAMT = TOTCOMAMT(1)*DYN_BETUNIT+TOTCOMAMT(2)
	          TOTCOMM(TABREVS(3,REVS,GNUM),GNUM) = TOTCOMM(TABREVS(3,REVS,GNUM),GNUM) + I8_TOTCOMAMT
		  TOTCOMMSAP(GNUM) = TOTCOMMSAP(GNUM) + I8_TOTCOMAMT
		ENDIF
C
C CALC COMMISSION AND NET AMOUNT FOR CURRENT AND NEXT DRAWS
C
		DO  INDNEXT = 1, 6
		    NEXTDRWTOT(1,INDNEXT,GNUM) = NEXTDRWTOT(1,INDNEXT,GNUM) + NEXTDRWAMT(INDNEXT,GNUM,REVS)
		    POOL = NEXTDRWAMT(INDNEXT,GNUM,REVS)

		    CALL FASTSET(0, TOTCOMAMT, SIZEOF(TOTCOMAMT)/4)
      		    CALL GETCOM( POOL,
     *			         TWAG,
     *			         GNUM,
     *			         COMAMT,
     *			         TOTCOMAMT,
     *                           0, 0, 0   )
C IF NO COMMISSION FLAG DON'T SEND COMMISSION     !V08
C
      IF(TSBIT(AGTTAB(AGTTYP,REVS),AGTNCM))THEN
                    TOTCOMAMT(1)=0
                    TOTCOMAMT(2)=0
                ENDIF                               !V08
                
		    I8_TOTCOMAMT = TOTCOMAMT(1)*DYN_BETUNIT+TOTCOMAMT(2)
		    NEXTDRWTOT(2,INDNEXT,GNUM) = NEXTDRWTOT(2,INDNEXT,GNUM) + I8_TOTCOMAMT	    !COMMISSION
		ENDDO
	    ENDIF
	  ENDDO
	ENDDO
C
C PRINT REPORTS FOR ALL GAMES
C CLOSING TODAY
C
       DO GNUM=1,MAXGAM
         LINCNT = 100
         PAGE   = 0
C
         CALL FASTSET(0,GMDRDATA,SIZEOF(GMDRDATA)/4)                            !V17 CLEAR DATA STRUCTURE
C
         IF(BEGCDC(GNUM).GT.0 .AND. ENDCDC(GNUM).EQ.DATECDC) THEN
!+++++++++++++++++++++++++++V12+++++++++++++++++++++++++++++++++++++++++++++++++
           CALL GETWEK(DRWGME(GNUM),GNUM,WEEK,YEAR,ST)
           IF(ST.NE.0) THEN
             TYPE*,IAM(),' Error getting WEEK',GNUM,DRWGME(GNUM),ST
             CALL GPAUSE
             IF(GNUM.EQ.6.OR.GNUM.EQ.7) THEN
               CALL FIGCCC(ENDCDC(GNUM),WEEK,YEAR)  !week will contain ccc
             ELSE                                   !for totolotos/q
               CALL FIGWEK(ENDCDC(GNUM),WEEK,YEAR)      
             ENDIF
           ENDIF
!+++++++++++++++++++++++++++V12+++++++++++++++++++++++++++++++++++++++++++++++++	    
C
           GMDRDATA.DRCCC = WEEK                                                !V17 SAVE DRAW NUMBER
           GMDRDATA.DRYEAR = YEAR                                               !V17 SAVE DRAW YEAR
C
C GENERATE CANCELLED TICKETS REPORT
C					       !v11 create invjoker
C	    IF (GNUM .NE. 5) THEN              !V05 DONT DO FOR JOKER    
	    	     
           WRITE (5,2100) IAM(), (GLNAMES(K,GNUM),K=1,4)

           CALL CANSORT (GNUM, DRWGME(GNUM),
     *                   CANTAB(1,1,GNUM), CANIND(GNUM))
c            ENDIF

C OPEN REPORT FILE
C
           CALL ROPEN(CONCRPT(GNUM),UNIT,ST)
           IF (ST .NE. OK) THEN
             IF (CLANG.EQ.SPANISH) THEN
               TYPE*,' ERROR en apertura del reporte'
             ELSEIF (CLANG.EQ.PORTUG) THEN
               TYPE*,' ERRO na abertura do arquivo de relatorio, jogo ',
     *                GNUM
             ELSE
               TYPE*,' Open error on report file'
             ENDIF
             CALL BELLS(2)
             CALL  USRCLOS1(UNIT)
             CALL GSTOP(GEXIT_FATAL)
           ELSE
             CALL ROPEN(CONCLOAD(GNUM),LOAD,ST)
             IF (ST .NE. OK) THEN
               IF (CLANG.EQ.SPANISH) THEN
                 TYPE*,' ERROR en apertura del carga'
               ELSEIF (CLANG.EQ.PORTUG) THEN
                 TYPE*,' ERRO na abertura do arquivo de carga, jogo ',
     *                  GNUM
               ELSE
                 TYPE*,' Open error on load file'
               ENDIF
               CALL BELLS(2)
               CALL  USRCLOS1(UNIT)
               CALL GSTOP(GEXIT_FATAL)
             ELSE
               DO UFS = 0, NUMUF-1                     !V03 INCLUDE DIST 00
                 IF(LINCNT.GT.MAXPRTLN) THEN
                   IF(CLANG.EQ.PORTUG) THEN
                     CALL TITLE('RELATORIO DE ENCERRAMENTO DE CONCURSO',
     *                           CONCRPT(GNUM),VERSION,
     *                           UNIT,PAGE,DATECDC)
                   ENDIF
                   IF (PAGE.EQ.1) THEN
                     IF(GNUM.EQ.6.OR.GNUM.EQ.7) THEN
                       WRITE(UNIT,10401) (GLNAMES(K,GNUM),K=1,4),
     *                                   WEEK,YEAR,
     *                                   DISTIM(CLOSTIM(GNUM)),
     *                                   (FSTDATE(K,GNUM),K=7,12),
     *                                   (LSTDATE(K,GNUM),K=7,12)
                     ELSE
                       WRITE(UNIT,1040) (GLNAMES(K,GNUM),K=1,4),
     *                                  WEEK,YEAR,
     *                                  DISTIM(CLOSTIM(GNUM)),
     *                                  (FSTDATE(K,GNUM),K=7,12),
     *                                  (LSTDATE(K,GNUM),K=7,12)
                     ENDIF
                     WRITE(LOAD,1041) (GLNAMES(K,GNUM),K=1,4),
     *                                 WEEK,YEAR
                     LINCNT = 17
                   ELSE
                     LINCNT = 10
                   ENDIF
                   WRITE(UNIT,1050)
                 ENDIF
C
C GET GRAND TOTALS
C
                 TOTSAL(NUMUF,GNUM)  =  TOTSAL(NUMUF,GNUM) + 
     *                                  TOTSAL(UFS,GNUM)
                 TOTTKTS(NUMUF,GNUM)  = TOTTKTS(NUMUF,GNUM) + 
     *                                  TOTTKTS(UFS,GNUM)
                 TOTCOMM(NUMUF,GNUM)  = TOTCOMM(NUMUF,GNUM) + 
     *                                  TOTCOMM(UFS,GNUM)
                 TOTCARRY(NUMUF,GNUM) = TOTCARRY(NUMUF,GNUM) + 
     *                                  TOTCARRY(UFS,GNUM)
                 NUMCARRY(NUMUF,GNUM) = NUMCARRY(NUMUF,GNUM) + 
     *                                  NUMCARRY(UFS,GNUM)
                 NUMACTREVS(NUMUF,GNUM) = NUMACTREVS(NUMUF,GNUM) + 
     *                                    NUMACTREVS(UFS,GNUM)

                 IF (TOTSAL(UFS,GNUM).GT.0 .OR. TOTCARRY(UFS,GNUM).GT.0) THEN
                   WRITE(UNIT,1100) UFS,
     *                              MYCMONYI8(TOTSAL(UFS,GNUM),13,BETUNIT),
     *                              TOTTKTS(UFS,GNUM),
     *                              NUMACTREVS(UFS,GNUM),
     *                              MYCMONYI8(TOTCOMM(UFS,GNUM),13,BETUNIT),
     *                              MYCMONYI8(TOTCARRY(UFS,GNUM),13,BETUNIT),
     *                              NUMCARRY(UFS,GNUM)
                   WRITE(LOAD,1101) UFS,
     *                              TOTSAL(UFS,GNUM),
     *                              TOTTKTS(UFS,GNUM),
     *                              NUMACTREVS(UFS,GNUM),
     *                              TOTCOMM(UFS,GNUM),
     *                              TOTCARRY(UFS,GNUM),
     *                              NUMCARRY(UFS,GNUM)
                   LINCNT = LINCNT + 1
                 ENDIF
               ENDDO
C
C WRITE TOTAL LINE
C
               WRITE(UNIT,1110) 
     *                          MYCMONYI8(TOTSAL(NUMUF,GNUM),14,BETUNIT),
     *                          TOTTKTS(NUMUF,GNUM),
     *                          NUMACTREVS(NUMUF,GNUM),
     *                          MYCMONYI8(TOTCOMM(NUMUF,GNUM),13,BETUNIT),
     *                          MYCMONYI8(TOTCARRY(NUMUF,GNUM),13,BETUNIT),
     *                          NUMCARRY(NUMUF,GNUM)
               WRITE(LOAD,1111) 
     *                          TOTSAL(NUMUF,GNUM),
     *                          TOTTKTS(NUMUF,GNUM),
     *                          NUMACTREVS(NUMUF,GNUM),
     *                          TOTCOMM(NUMUF,GNUM),
     *                          TOTCARRY(NUMUF,GNUM),
     *                          NUMCARRY(NUMUF,GNUM)
C
C WRITE SECOND PAGE WITH TICKETS VERIFIED
C
               CALL TITLE('RELATORIO DE ENCERRAMENTO DE CONCURSO',
     *              CONCRPT(GNUM),VERSION,
     *              UNIT,PAGE,DATECDC)
               LIQCONC = TOTARRECAD(GNUM)
               GIND = GNTTAB(GAMIDX,GNUM)
               GMDRDATA.TOTSALE = TOTARRECAD(GNUM)                              !V17 TOTAL DRAW SALES AMOUNT                      (TOTARRECAD)
               IF  (GNTTAB(GAMTYP,GNUM) .EQ. TLTO) THEN
                 LIQPREM = DFLOAT(LIQCONC)*CALPER(LTOSPR(GIND))/100.D0
                 MAXDIV = LTODIV(GIND)
C
                 GMDRDATA.PREVJKT = DLTPOL(1)                                   !V17 SAVE LOTTO PREVIOUS JACKPOT AMOUNT
                 GMDRDATA.GNUM = GNUM                                           !V17 SAVE LOTTO GAME NUMBER
                 GMDRDATA.GTYP = GNTTAB(GAMTYP,GNUM)                            !V17 SAVE LOTTO GAME TYPE
                 GMDRDATA.GIND = GNTTAB(GAMIDX,GNUM)                            !V17 SAVE LOTTO GAME INDEX
                 GMDRDATA.TOTPOOL = LIQPREM                                     !V17 TOTAL DRAW NET PRIZE AMOUNT FOR DISTRIBUTION (LIQPREM)
                 GMDRDATA.GDIV = LTODIV(GIND)                                   !V17 TOTAL NUMBER OF GAME PRIZE DIVISIONS
                 GMDRDATA.PYTPERC = LTOSPR(GIND)                                !V17 PAYOUT PERCENTAGE
                 DO L = 1, GMDRDATA.GDIV                                        !V17 ASSIGNED PERCENTAGE OF PRIZE DIVISION
                   GMDRDATA.DIVPERC(L) = DLTPER(L)
                 ENDDO
C
               ELSEIF (GNTTAB(GAMTYP,GNUM) .EQ. TSPT) THEN
                 LIQPREM = DFLOAT(LIQCONC)*CALPER(SPTSPR(GIND))/100.D0
                 MAXDIV = SPTDIV(GIND)
C
                 GMDRDATA.PREVJKT = DSPPOL(1)                                   !V17 SAVE SPORTS PREVIOUS JACKPOT AMOUNT
                 GMDRDATA.GNUM = GNUM                                           !V17 SAVE SPORTS GAME NUMBER
                 GMDRDATA.GTYP = GNTTAB(GAMTYP,GNUM)                            !V17 SAVE SPORTS GAME TYPE
                 GMDRDATA.GIND = GNTTAB(GAMIDX,GNUM)                            !V17 SAVE SPORTS GAME INDEX
                 GMDRDATA.TOTPOOL = LIQPREM                                     !V17 TOTAL DRAW NET PRIZE AMOUNT FOR DISTRIBUTION (LIQPREM)
                 GMDRDATA.GDIV = SPTDIV(GIND)                                   !V17 TOTAL NUMBER OF GAME PRIZE DIVISIONS
                 GMDRDATA.PYTPERC = SPTSPR(GIND)                                !V17 PAYOUT PERCENTAGE
                 DO L = 1, GMDRDATA.GDIV                                        !V17 ASSIGNED PERCENTAGE OF PRIZE DIVISION
                   GMDRDATA.DIVPERC(L) = DSPPER(L)
                 ENDDO
C
               ELSEIF (GNTTAB(GAMTYP,GNUM) .EQ. TTGL) THEN
                 LIQPREM = DFLOAT(LIQCONC)*CALPER(TGLSPR(GIND))/100.D0
                 MAXDIV = TGLDIV(GIND)
               ELSEIF (GNTTAB(GAMTYP,GNUM) .EQ. TKIK) THEN
                 LIQPREM = DFLOAT(LIQCONC)*CALPER(KIKSPR(GIND))/100.D0
                 MAXDIV = KIKDIV(GIND)
C
                 GMDRDATA.GNUM = GNUM                                           !V17 SAVE KICKER GAME NUMBER
                 GMDRDATA.GTYP = GNTTAB(GAMTYP,GNUM)                            !V17 SAVE KICKER GAME TYPE
                 GMDRDATA.GIND = GNTTAB(GAMIDX,GNUM)                            !V17 SAVE KICKER GAME INDEX
                 GMDRDATA.TOTPOOL = LIQPREM                                     !V17 TOTAL DRAW NET PRIZE AMOUNT FOR DISTRIBUTION (LIQPREM)
                 GMDRDATA.GDIV = KIKDIV(GIND)                                   !V17 TOTAL NUMBER OF GAME PRIZE DIVISIONS
                 GMDRDATA.PYTPERC = KIKSPR(GIND)                                !V17 PAYOUT PERCENTAGE
                 DO L = 1, GMDRDATA.GDIV                                        !V17 ASSIGNED PERCENTAGE OF PRIZE DIVISION
                   GMDRDATA.DIVPERC(L) = DKKPER(L)
                   GMDRDATA.PREVJKT = GMDRDATA.PREVJKT +                        !V17 SAVE KICKER PREVIOUS JACKPOT AMOUNT (CALCULATED THE SAME AWAY AS IN KISREP.FOR V02, BUT IS THIS CASE AS AN INTEGER)
     *                                DKKPOL(1,L)*DYN_BETUNIT + 
     *                                DKKPOL(2,L)
                 ENDDO
C
               ENDIF
C
               IF(GNUM.EQ.6.OR.GNUM.EQ.7) THEN
                 WRITE(UNIT,11201) (GLNAMES(K,GNUM),K=1,4),WEEK,YEAR
               ELSE
                 WRITE(UNIT,1120) (GLNAMES(K,GNUM),K=1,4),WEEK,YEAR
               ENDIF
               WRITE(UNIT,1125) MYCMONYI8(TOTARRECAD(GNUM),14,BETUNIT),
     *                          LIQPREM

               WRITE(UNIT,1126) 
               DO I=1,MAXDIV
                 IF(PRMDIV(I,GNUM).EQ.-1) THEN
                   WRITE(UNIT,1128) I
                   GMDRDATA.DIVNUMB(I) = I                                      !V17
                   GMDRDATA.DIVPOOL(I) = -1                                     !V17
                 ELSE
C===============================================================================
C INI V10
C===============================================================================		      
                   IF(I.EQ.1.AND.POLMIN(GNUM).GT.0) THEN
                     IF(PRMDIV(I,GNUM).LT.POLMIN(GNUM)) THEN
                       WRITE(UNIT,1129) I,
     *                        MYCMONYI8(PRMDIV(I,GNUM),13,BETUNIT),
     *                        MYCMONYI8(POLMIN(GNUM)-PRMDIV(I,GNUM),13,BETUNIT)
                       GMDRDATA.DIVNUMB(I) = I                                  !V17
                       GMDRDATA.DIVPOOL(I) = PRMDIV(I,GNUM)                     !V17
                     ELSE
                       WRITE(UNIT,1127) I,MYCMONYI8(PRMDIV(I,GNUM),13,BETUNIT)
                       GMDRDATA.DIVNUMB(I) = I                                  !V17
                       GMDRDATA.DIVPOOL(I) = PRMDIV(I,GNUM)                     !V17
                     ENDIF
                   ELSE
                     WRITE(UNIT,1127) I,MYCMONYI8(PRMDIV(I,GNUM),13,BETUNIT)
                     GMDRDATA.DIVNUMB(I) = I                                    !V17
                     GMDRDATA.DIVPOOL(I) = PRMDIV(I,GNUM)                       !V17
                   ENDIF
C===============================================================================
C FIN V10
C===============================================================================		      
                 ENDIF
               ENDDO

               WRITE(UNIT,1130)
               DO STATUS=1,3
                 TKTBYST = CNTTKTS(ONCARRY,STATUS,GNUM) + 
     *           CNTTKTS(ONDRAW,STATUS,GNUM) + 
     *           CNTTKTS(ONTMF,STATUS,GNUM)
C
C ADD SUBTOTAL + MULTIDRAW
C
                 TOTLIN = TKTBYST + CNTTKTS(ONNEXT,STATUS,GNUM)
C
                 IF(STATUS.EQ.1) GMDRDATA.TOTTCKT = TOTLIN                      !V17 TOTAL DRAW VALID TICKETS
C
                 WRITE(UNIT,1140) DESCFILE(STATUS),
     *                            (CNTTKTS(FILES,STATUS,GNUM),
     *                            FILES=ONCARRY,ONTMF),
     *                            TKTBYST, CNTTKTS(ONNEXT,STATUS,GNUM),
     *                            TOTLIN
               ENDDO
               WRITE(UNIT,1150)
C
               WRITE(UNIT,1160)
               WRITE(UNIT,1165)
C
C SALES ONLINE FOR THIS DRAW (WITH 1/5 OF 5WEEKS)
C
               DO  INDNEXT = 1, 2
                 TOTAREC = NEXTDRWTOT(1,INDNEXT,GNUM) - NEXTDRWTOT(2,INDNEXT,GNUM)
                 WRITE(UNIT,1170) WEEK,YEAR,
     *                      MYCMONYI8(NEXTDRWTOT(1,INDNEXT,GNUM),12,BETUNIT),
     *                      MYCMONYI8(NEXTDRWTOT(2,INDNEXT,GNUM),10,BETUNIT),
     *                      MYCMONYI8(TOTAREC,12,BETUNIT)
               ENDDO
C
C SALES/COMISSIONS FROM OLD DRAWS
C
               FILNAM = 'SAPDRWAMT.FIL'
               CALL FIDX_OPEN(IDX_FDB,FILNAM,SIZEOF(REGAMTDRW),'OLD',ST)
               IF  (ST.NE.0) CALL FILERR(IFILNAM,OPEN_ERROR,ST,0)
C
               I = 6				! POSITION ON OLD DRAW FOR THIS DRAW
               DO  INDBEFR = 4, 1, -1
                 CALL FIDX_READ(IDX_FDB,INDBEFR,SIZEOF(REGAMTDRW),REGAMTDRW,ST)
                 IF  (ST.NE.0) CALL FILERR(IFILNAM,READ_ERROR,ST,INDBEFR)
C
C PRINT LAS VALUES (ON + OFFLINE)
C
                 TOTAREC = REGAMTDRW.AMTDRWONL(I,GNUM) + REGAMTDRW.AMTDRWOFF(I,GNUM)
                 I8_TOTCOMAMT = IAND(REGAMTDRW.COMDRWONL(I,GNUM),'00000000FFFFFFFF'X)  +
     *                IAND(REGAMTDRW.COMDRWOFF(I,GNUM),'00000000FFFFFFFF'X)
                 TMPPOOL      = TOTAREC - I8_TOTCOMAMT
C
                 WEEKBEF      = WEEK - INDBEFR
                 YEARBEF      = YEAR
                 IF (WEEKBEF.LT.0) THEN
                   IF(GNUM.EQ.6.OR.GNUM.EQ.7) THEN
                     WOFF = 104
                     IF(MOD(YEAR,100).EQ.11) WOFF=95
                     IF(MOD(YEAR,100).EQ.14) WOFF=105
                     IF(MOD(YEAR,100).EQ.16) WOFF=105
                     IF(MOD(YEAR,100).EQ.20) WOFF=105
                     WEEKBEF = WOFF - (INDBEFR-1)
                   ELSE
                     WEEKBEF = 52 - (INDBEFR-1)
                   ENDIF
                   YEARBEF = YEARBEF - 1
                 ENDIF
C
                 WRITE(UNIT,1170) WEEKBEF, YEARBEF,
     *                            MYCMONYI8(TOTAREC,12,BETUNIT),
     *                            MYCMONYI8(I8_TOTCOMAMT,10,BETUNIT),
     *                            MYCMONYI8(TMPPOOL,12,BETUNIT)

                 CALL FIDX_READ(IDX_FDB,INDBEFR+1,SIZEOF(REGAMTAUX),REGAMTAUX,ST)
                 IF  (ST.NE.0) CALL FILERR(IFILNAM,READ_ERROR,ST,INDBEFR+1)

                 DO  INDNEXT = 1, 6
                   REGAMTAUX.AMTDRWONL(INDNEXT,GNUM) = REGAMTDRW.AMTDRWONL(INDNEXT,GNUM)
                   REGAMTAUX.COMDRWONL(INDNEXT,GNUM) = REGAMTDRW.COMDRWONL(INDNEXT,GNUM)
                   REGAMTAUX.AMTDRWOFF(INDNEXT,GNUM) = REGAMTDRW.AMTDRWOFF(INDNEXT,GNUM)
                   REGAMTAUX.COMDRWOFF(INDNEXT,GNUM) = REGAMTDRW.COMDRWOFF(INDNEXT,GNUM)
                 ENDDO

                 IF (TOWRITE) THEN
                   CALL FIDX_WRITE(IDX_FDB,INDBEFR+1,SIZEOF(REGAMTAUX),REGAMTAUX,ST)
                   IF  (ST.NE.0) CALL FILERR(IFILNAM,WRITE_ERROR,ST,INDBEFR+1)
                 ENDIF

                 I = I -  1
               ENDDO
C
C WRITE CURRENT DRAW( POS = 1 ) WITH NEXT 5 WEEK SALES
C
               CALL FIDX_READ(IDX_FDB,1,SIZEOF(REGAMTDRW),REGAMTDRW,ST)
               IF  (ST.NE.0) CALL FILERR(IFILNAM,READ_ERROR,ST,1)

               DO  INDNEXT = 1, 6
                 REGAMTDRW.AMTDRWONL(INDNEXT,GNUM) = NEXTDRWTOT(1,INDNEXT,GNUM)
                 IF (INDNEXT.EQ.1) THEN		    ! SPECIAL ADJUSTMENT TO KEEP TOTAL COMISSION FOR SAP
                   REGAMTDRW.COMDRWONL(INDNEXT,GNUM) = TOTCOMMSAP(GNUM)
                   REGAMTDRW.COMDRWONL(INDNEXT,GNUM) = ISHFT(REGAMTDRW.COMDRWONL(INDNEXT,GNUM),32) + 
     *                                                 NEXTDRWTOT(2,INDNEXT,GNUM)
                 ELSE
                   REGAMTDRW.COMDRWONL(INDNEXT,GNUM) = NEXTDRWTOT(2,INDNEXT,GNUM)
                 ENDIF
                 REGAMTDRW.AMTDRWOFF(INDNEXT,GNUM) = 0
                 REGAMTDRW.COMDRWOFF(INDNEXT,GNUM) = 0
               ENDDO

               IF (TOWRITE) THEN
                 CALL FIDX_WRITE(IDX_FDB,1,SIZEOF(REGAMTDRW),REGAMTDRW,ST)
                 IF  (ST.NE.0) CALL FILERR(IFILNAM,WRITE_ERROR,ST,1)
               ENDIF

               CALL FIDX_CLOSE(IDX_FDB,ST)
               IF  (ST.NE.0) CALL FILERR(IFILNAM,CLOSE_ERROR,ST,0)

               WRITE(UNIT,1180)
               WRITE(UNIT,1165)
C
C SALES ONLINE FOR NEXT DRAWS
C
               DO  INDNEXT = 3, 6
                 TOTAREC = NEXTDRWTOT(1,INDNEXT,GNUM) - NEXTDRWTOT(2,INDNEXT,GNUM)
                 WRITE(UNIT,1170) WEEK,YEAR,
     *                            MYCMONYI8(NEXTDRWTOT(1,INDNEXT,GNUM),12,BETUNIT),
     *                            MYCMONYI8(NEXTDRWTOT(2,INDNEXT,GNUM),10,BETUNIT),
     *                            MYCMONYI8(TOTAREC,12,BETUNIT)
               ENDDO

               CALL USRCLOS1(UNIT)
               CALL USRCLOS1(LOAD)

               IF (COPY.GT.0) CALL SPOOL(CONCRPT(GNUM),COPY,ST)
C
               CALL GMDRFIL(GMDRDATA, ST)                                       !V17 GENERATE DRAW FILE INTERFACE FILE
C
             ENDIF
           ENDIF	
         ENDIF
       ENDDO
	
c      	IF(DRWGME(6).NE.0) THEN        ! do not run INVLLOTOS for any game
c          CALL INVLLOTOS(DRWGME(6))   ! after Lotto 2 changes batch
c      	ELSEIF(DRWGME(7).NE.0) THEN
c          CALL INVLLOTOS(DRWGME(7))       
c        ELSEIF(DRWGME(1).NE.0) THEN   
c          CALL INVLLOTOS(DRWGME(1))
c        ELSEIF(DRWGME(3).NE.0) THEN
c          CALL INVLLOTOS(DRWGME(3))
c        ELSEIF(DRWGME(5).NE.0) THEN
c          CALL INVLLOTOS(DRWGME(5))
c        ELSEIF(DRWGME(10).NE.0) THEN
c          CALL INVLLOTOS(DRWGME(10))
c        ENDIF
C
       CALL GSTOP(GEXIT_SUCCESS)

1000   FORMAT('L',I1,'CONC.REP')
1001   FORMAT('L',I1,'LOAD.DAT')
1010   FORMAT('S',I1,'CONC.REP')
1011   FORMAT('S',I1,'LOAD.DAT')
1020   FORMAT('T',I1,'CONC.REP')
1021   FORMAT('T',I1,'LOAD.DAT')
1030   FORMAT('J',I1,'CONC.REP')
1031   FORMAT('J',I1,'LOAD.DAT')
1040   FORMAT(/40X,52('*'),40X,/,
     *            42X,'JOGO                      ',4A4,/,
     *            42X,'SEMANA ',30X,I3.3'/',I4.4,/,
     *            42X,'HORARIO DE ENCERRAMENTO',17X,A8,/,
     *            42X,'PERIODO DE VENDAS',4X,6A2,' A ',6A2,/,
     *            40X,52('*'),40X)
10401  FORMAT(/40X,52('*'),40X,/,
     *            42X,'JOGO                      ',4A4,/,
     *            42X,'SORTEIO ',29X,I3.3'/',I4.4,/,
     *            42X,'HORARIO DE ENCERRAMENTO',17X,A8,/,
     *            42X,'PERIODO DE VENDAS',4X,6A2,' A ',6A2,/,
     *            40X,52('*'),40X)
1041   FORMAT(1X,4A4,30X,I3.3,'/',I4.4)
1050   FORMAT(1X,131('-'),/,T56,' MEDIADORES 'T95,' 5 SEMANAS '/,
     *         6X,'DIST',8X,'  RECEITA  ',6X,'QTDE.BILHETES',6X,
     *        'PARTICIPANTES',4X,'REMUNERACAO',10X,'VALOR',9X,
     *         'QTDE',/,
     *         1X,131('-'))
1100   FORMAT(5X,I2.2,3X,6X,A13,10X,I9,13X,I5,3X,A13,1X,A13,5X,I9)
1101   FORMAT(5X,I2.2,3X,6X,I13,10X,I9,13X,I5,3X,I13,1X,I13,5X,I9)
1110   FORMAT(1X,'TOTAL ',8X,A14,10X,I9,13X,I5,3X,A13,1X,A13,5X,I9)
1111   FORMAT(1X,'TOTAL ',9X,I13,10X,I9,13X,I5,3X,I13,1X,I13,5X,I9)
C1100	FORMAT(4X,I2,2X,A2,6X,I7,9X,A13,2X,A13,2X,A13,7X,I7,1X,A13,9X,I5)
C1110	FORMAT(1X,'TOTAL ',9X,I7,9X,A13,2X,A13,2X,A13,7X,I7,1X,A13,9X,I5)
C			      3     2       X       5      7     6      4
1120   FORMAT(//34X,60('*'),40X,/,
     *           35X,'JOGO                      ',4A4,/,
     *           35X,'SEMANA ',29X,I3.3,'/',I4.4)
11201  FORMAT(//34X,60('*'),40X,/,
     *           35X,'JOGO                      ',4A4,/,
     *           35X,'SORTEIO ',29X,I3.3,'/',I4.4)

1125   FORMAT(35X,'RECEITA TOTAL    ',T69,A14/
     *        35X,'PREMIO LIQUIDO A DISTRIBUIR',T69,F14.2)

1126   FORMAT(34X,'*************** VALOR PARA PREMIOS POR CLASSE ***************',/,
     *         52X,' CLASSES         PREMIOS')
1127   FORMAT(54X,I2.2,11X,A13)
1128   FORMAT(54X,I2.2,11X,'   ********** Calc. pendente')
1129   FORMAT(54X,I2.2,11X,A13,'(',A13,')')

1130   FORMAT(21X,'********************************** BILHETES REGISTADOS ************************************',/,
     *         21X,'           5 SEMANAS   ANTER.    OUTROS DIAS     HOJE     SUBTOTAL      5 SEMANAS     TOTAL')
C***1140FORMAT(21X,A11,5X,I8,8X,I8,5X,I8,5X,I8,7X,I8,2X,I8)
1140   FORMAT(21X,A11,4X,I9,7X,I9,4X,I9,4X,I9,6X,I9,1X,I9)
1150   FORMAT(21X,91('*'),/,/,/)
1160   FORMAT(39X,'------ RECEITA DE APOSTAS DO CONCURSO  -------',40X,/)
1165   FORMAT(39X,'--- ILIQUIDA --   -REMUNERACAO-  -- LIQUIDA --',40X,/)
1170   FORMAT(29X,I3.3,'/',I4.4,5X,A12,6X,A10,3X,A12)
1180   FORMAT(//39X,'------ RECEITA DE APOSTAS / EXCEDENTES -------',40X,/)
2000   FORMAT(1X,A,1X,'Deseja gerar relatorios de encerramento de ',4A4)
2010   FORMAT(1X,A,1X,'O jogo nao fechado. Relatório encerramento não será gerado: ',4A4)
2100   FORMAT(1X,A,1X,'Gerando relatorio de cancelados para ',4A4)
9000   FORMAT(1X,A,/,1X,A,1X,'<<<<< RELCONC: Relatorio de encerramentos de Jogos ',A,' >>>>>',/,1X,A)
      END

C***************************************************
C SUBROUTINE DRAWCARRY
C***************************************************
  	SUBROUTINE DRAWCARRY( DRAW, BEGCDC, ENDCDC, ANTECIPADA, NUMCARRY, CNTTKTS,
     *                        AMTUFS, CANTAB, CANIND, ERRFLG, JKRNBR, NEXTDRWAMT )
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:NBRCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:RELCONC.DEF'
C
C LOCAL FUNCTIONS
C
	CHARACTER*8     IAGT_NO
C
C LOCAL VARIABLES
C
	LOGICAL   ERRFLG
	LOGICAL   GEN_REPORTS
C
	INTEGER*4 DRAW(MAXGAM), I, ST, SCFNAM(5), SCFFDB(7),NUMCARRY(0:NUMUF,MAXGAM)
	INTEGER*4 FTYPE(200),FILCNT,INDNEXT,DATECDC
	INTEGER*4 GIND,BEGCDC(MAXGAM),ENDCDC(MAXGAM),GTYP,FDB(7),CDC,TFDB(7)
	INTEGER*4 S,K,CLANG,BLOCK,EOF,IND,TMFBUF(8192),LENGTH
	INTEGER*4 FILES(20,200),DRWW,GNUM,TYPE,UF
	INTEGER*4 CNTTKTS(4,3,MAXGAM),MYGNUM
C
	INTEGER*4 CANTAB(6,MAXCAN,MAXGAM)                       !V04
	INTEGER*4 CANIND(MAXGAM), JKRNBR, NEXTDRWAMT(6,MAXGAM,*)
C
	INTEGER*8 AMTUFS(0:NUMUF,MAXGAM)
	INTEGER*8 ANTECIPADA(0:NUMUF,MAXGAM),TEMP

	CHARACTER*80 CFILES(200)
	EQUIVALENCE (FILES,CFILES)

	BYTE      I1TEMP(4)
	EQUIVALENCE (TEMP,I1TEMP)

	INTEGER*4   LVOL
	CHARACTER*4 CXLVOL/'DRAW'/
	EQUIVALENCE (LVOL,CXLVOL)

	DATA SCFNAM /'SCF.','FIL ',3*'    '/

	CLANG  = PORTUG
	FILCNT = 0
	DATECDC = DAYCDC
	GEN_REPORTS = .FALSE.
C
C	READ SCF
C
	CALL OPENW(1,SCFNAM,4,0,0,ST)
	CALL IOINIT(SCFFDB,1,SCFSEC*256)
	IF (ST.NE.0) CALL FILERR(SCFNAM,1,ST,0)
	CALL READW(SCFFDB,1,SCFREC,ST)
	IF (ST.NE.0) CALL FILERR(SCFNAM,2,ST,1)
	CALL CLOSEFIL(SCFFDB)
	DO I=1,MAXFIL
           IF (SCFSFN(1,I).EQ.'    ') CALL SYSVOL(SCFSFN(1,I))
	ENDDO
C
C  READ DAF FILE AND GET FILES
C
	CALL OPENW(1,SFNAMES(1,DAF),4,0,0,ST)
	CALL IOINIT(FDB,1,DAFSEC*256)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),1,ST,0)
C
	DO GNUM=1,MAXGAM
	  IF(BEGCDC(GNUM).GT.0 .AND. ENDCDC(GNUM).EQ.DATECDC) THEN
	    GEN_REPORTS = .TRUE.

	    FILCNT = 0
	    CALL FASTSET(0,FILES,20*200)

	    GTYP = GNTTAB(GAMTYP,GNUM)
	    GIND = GNTTAB(GAMIDX,GNUM)
C
C GET ALL DRAW FILES TO BE PROCESSED
C
	    DO CDC=BEGCDC(GNUM),ENDCDC(GNUM)-1
		CALL READW(FDB,CDC,DAFREC,ST)
		IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),2,ST,CDC)
		IF(DAFSTS.NE.DNOSAL) THEN 
		    FILCNT=FILCNT+1
		    FTYPE(FILCNT)=GTYP
		    IF(CDC.NE.DAYCDC) WRITE (CFILES(FILCNT),9900) 
     *				      LVOL,GSNAMES(GNUM),CDC
		ENDIF
	    ENDDO

	    DO I=1,FILCNT
		CALL OPNDRW(FILES(1,I),PTMF)
		CALL IOINIT(TFDB,PTMF,128*256)

		IF(CLANG.EQ.SPANISH) THEN
		    WRITE(5,9110) IAM(),(FILES(S,I),S=1,5)
		ELSEIF(CLANG.EQ.PORTUG) THEN
		    WRITE(5,9210) IAM(),(FILES(S,I),S=1,5)
		ELSE
		    WRITE(5,910) IAM(),(FILES(S,I),S=1,5)
		ENDIF
C
C SCAN FILE
C
		BLOCK = 0
		EOF   = 0
		IND   = 8192

		DO WHILE(EOF.LE.1000)
		  IF(IND.GE.8157) THEN
		    BLOCK = BLOCK+1
		    IND   = 1
		    CALL READW(TFDB,BLOCK,TMFBUF,ST)
		    IF(ST.NE.0) THEN
			IF(CLANG.EQ.SPANISH) THEN
			    WRITE(5,9100) IAM(),(FILES(K,I),K=1,5),ST,BLOCK
			ELSEIF(CLANG.EQ.PORTUG) THEN
			    WRITE(5,9200) IAM(),(FILES(K,I),K=1,5),ST,BLOCK
			ELSE
			    WRITE(5,900) IAM(),(FILES(K,I),K=1,5),ST,BLOCK
			ENDIF
			CALL GPAUSE
		    ENDIF
		  ENDIF

		  IF(TMFBUF(IND).EQ.0) THEN
		    EOF=EOF+1
		    IND=IND+LREC
		  ELSE
		    EOF=0
                    CALL ILBYTE(TYPE,TMFBUF(IND),LREC1-1)
		    IF(TYPE.NE.LONE.AND.TYPE.NE.LREG) THEN
			IF(CLANG.EQ.SPANISH) THEN
			    TYPE*,IAM(),
     *                      ' Error en registro, tipo > ',TYPE,' indice > ',IND
			ELSEIF(CLANG.EQ.PORTUG) THEN
			    TYPE*,IAM(),' Erro no registro, tipo > ',
     *				         TYPE,' indice > ',IND
			ELSE
			    TYPE*,IAM(),' Bad record type > ',TYPE,
     *                                  ' index > ',IND
			ENDIF
			CALL BELLS(2)
			IND=IND+LREC
		    ELSE
			LENGTH=LREC
			IF(TYPE.EQ.LONE) THEN
	                    CALL ILBYTE(TYPE,TMFBUF(IND),LREC2-1)
			    IF(TYPE.EQ.LEND) LENGTH=LREC*2
			    IF(TYPE.EQ.LTWO) LENGTH=LREC*3
			ENDIF
			CALL LOGTRA(TRABUF,TMFBUF(IND))
			IND=IND+LENGTH

			IF( TRABUF(TGAMTYP).EQ.GTYP .AND.
     *			    TRABUF(TGAMIND).EQ.GIND .AND.
     *		           (TRABUF(TSTAT).EQ.GOOD.OR.TRABUF(TSTAT).EQ.VOID.OR.
     *		            TRABUF(TSTAT).EQ.INCA).AND.
     *		           (TRABUF(TWBEG).EQ.DRAW(GNUM).OR.
     *                      (GTYP.EQ.TLTO .AND. LTOADW(GIND).EQ.1)) )
     *		            THEN

			    CALL UPD_TKT_COUNT(TRABUF,CNTTKTS,ONDRAW,JKRNBR)

     		            IF(TRABUF(TSTAT).EQ.GOOD) THEN
			      UF = AGTTAB(AGTNUM,TRABUF(TTER))/100000
			      IF (UF.LT.0.OR.UF.GT.NUMUF) THEN
				 UF = TRABUF(TAGT)/100000
				 WRITE(5,9300)IAM(),TRABUF(TGAM),
     *		                                    CMONY(TRABUF(TWAMT),12,BETUNIT),
     *			                            TRABUF(TTER),IAGT_NO(TRABUF(TAGT)),UF
			      ENDIF
C
C GET AMOUNT/UF FOR THE CURRENT DRAW
C
			      IF (TRABUF(TWBEG).EQ.DRAW(TRABUF(TGAM))) THEN
				 AMTUFS(UF,TRABUF(TGAM)) =
     *				 AMTUFS(UF,TRABUF(TGAM)) + TRABUF(TWAMT)
CC     *				 AMTUFS(UF,TRABUF(TGAM)) + (TRABUF(TWAMT) - TRABUF(TWKAMT))

				 IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0 .AND. JKRNBR.NE.TRABUF(TGAM)) 
     *                              AMTUFS(UF,JKRNBR) = AMTUFS(UF,JKRNBR) + TRABUF(TWKAMT)
C
				 IF (TRABUF(TWBEG).EQ.TRABUF(TWEND)) THEN
			            NEXTDRWAMT(1,TRABUF(TGAM),TRABUF(TTER)) =
     *                              NEXTDRWAMT(1,TRABUF(TGAM),TRABUF(TTER)) + TRABUF(TWAMT)
CC     *                              NEXTDRWAMT(1,TRABUF(TGAM),TRABUF(TTER)) + (TRABUF(TWAMT) - TRABUF(TWKAMT))

				    IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0 .AND. JKRNBR.NE.TRABUF(TGAM))
     *			               NEXTDRWAMT(1,JKRNBR,TRABUF(TTER)) =
     *                                 NEXTDRWAMT(1,JKRNBR,TRABUF(TTER)) + TRABUF(TWKAMT)
				 ELSE
				    INDNEXT = 2
				    DO	DRWW = TRABUF(TWBEG),TRABUF(TWEND)
					NEXTDRWAMT(INDNEXT,TRABUF(TGAM),TRABUF(TTER)) =
     *				        NEXTDRWAMT(INDNEXT,TRABUF(TGAM),TRABUF(TTER)) + TRABUF(TWAMT)
CC     *				        NEXTDRWAMT(INDNEXT,TRABUF(TGAM),TRABUF(TTER)) + (TRABUF(TWAMT) - TRABUF(TWKAMT))

					IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0)
     *					   NEXTDRWAMT(INDNEXT,JKRNBR,TRABUF(TTER)) =
     *					   NEXTDRWAMT(INDNEXT,JKRNBR,TRABUF(TTER)) + TRABUF(TWKAMT)
				        INDNEXT = INDNEXT + 1
				    ENDDO
				 ENDIF
CC			      ELSEIF(TRABUF(TWEND).GE.DRAW(TRABUF(TGAM)) THEN
CC				 INDBEFR = DRAW(TRABUF(TGAM)) - TRABUF(TWBEG)
CC			         BEFRDRWAMT(INDBEFR,TRABUF(TGAM),TRABUF(TTER)) =
CC     *                           BEFRDRWAMT(INDBEFR,TRABUF(TGAM),TRABUF(TTER)) + TRABUF(TWAMT)
CC
CC				 IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0)
CC     *			            BEFRDRWAMT(INDBEFR,JKRNBR,TRABUF(TTER)) =
CC     *                              BEFRDRWAMT(INDBEFR,JKRNBR,TRABUF(TTER)) + TRABUF(TWKAMT)
			      ENDIF
C
C GET AMOUNT FOR NEXT DRAWS
C
			      DO  DRWW = TRABUF(TWBEG),TRABUF(TWEND)
      			          IF (DRWW.GE.DRAW(TRABUF(TGAM))) THEN
      				     IF (DRWW.GT.DRAW(TRABUF(TGAM)))THEN
      			                ANTECIPADA(UF,TRABUF(TGAM)) =
     *				        ANTECIPADA(UF,TRABUF(TGAM)) + TRABUF(TWAMT)
CC     *				        ANTECIPADA(UF,TRABUF(TGAM)) + (TRABUF(TWAMT) - TRABUF(TWKAMT))

				        IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0)
     *      				   ANTECIPADA(UF,JKRNBR) = ANTECIPADA(UF,JKRNBR) + TRABUF(TWKAMT)

C*****				        NUMCARRY(UF,TRABUF(TGAM)) = 
C*****			                NUMCARRY(UF,TRABUF(TGAM)) + 1
      				     ENDIF
                                  ENDIF
      			      ENDDO

      			      IF ( TRABUF(TWEND) .GT. DRAW(TRABUF(TGAM)) ) THEN
				 NUMCARRY(UF,TRABUF(TGAM)) = 
     *					  NUMCARRY(UF,TRABUF(TGAM)) + 1
				 IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0) 
     *				    NUMCARRY(UF,JKRNBR) = NUMCARRY(UF,JKRNBR) + 1
      			      ENDIF
			    ELSE
			      MYGNUM = TRABUF(TGAM)
			      CANIND(MYGNUM) = CANIND(MYGNUM) + 1
		              IF (CANIND(MYGNUM) .GT. MAXCAN) THEN
			        IF(.NOT. ERRFLG)
     *				  TYPE *, 
     *                            ' Invalidacoes em excesso para relatorio ', 
     *				  CANIND(MYGNUM)
				ERRFLG = .TRUE.
			      ELSE
				CANTAB(1,CANIND(MYGNUM),MYGNUM) = TRABUF(TAGT)
				CANTAB(2,CANIND(MYGNUM),MYGNUM) = 
     *                              ISHFT(TRABUF(TCDC),16) + TRABUF(TWLMFI)
				CANTAB(3,CANIND(MYGNUM),MYGNUM) = TRABUF(TSER)
				CANTAB(4,CANIND(MYGNUM),MYGNUM) = TRABUF(TWAMT)
				CANTAB(5,CANIND(MYGNUM),MYGNUM) = 
     *				    ISHFT(TRABUF(TWBEG),16) + TRABUF(TWDUR)
      				CANTAB(6,CANIND(GNUM),GNUM) = TRABUF(TWKAMT)     !V04
			      ENDIF
			    ENDIF
			ENDIF
		    ENDIF
		  ENDIF
		ENDDO
	    ENDDO
	  ENDIF
	ENDDO

	CALL CLOSEFIL(FDB)

	IF(.NOT.GEN_REPORTS) RETURN

	IF(CLANG.EQ.SPANISH) THEN
	    TYPE*,IAM(),' Seleccion de los archivos DRAW completa'
        ELSEIF(CLANG.EQ.PORTUG) THEN
	    TYPE*,IAM(),
     *	    ' Verificacao de 5 Semanas encerrada nos arquivos de DRAW'
	ELSE
	    TYPE*,IAM(),' Multi-draw scan complete'
        ENDIF

	RETURN

900	FORMAT(1X,A,1X,5A4,' read error> ',I4,' block> ',I8)
910	FORMAT(1X,A,1X,'Scanning file ',5A4,' for winners ')
9100	FORMAT(1X,A,1X,5A4,' error de lectura > ',I4,' bloque > ',I8)
9110	FORMAT(1X,A,1X,'Leyendo archivo ',5A4,' para ganadores')
9200	FORMAT(1X,A,1X,5A4,' erro de leitura> ',I4,' bloco> ',I8)
9210	FORMAT(1X,A,1X,'Pesquisando arquivo ',5A4,' para concurso')
9300	FORMAT(1X,A,1X,' Jogo ',I2,' valor ',A12,' no terminal ',I6,
     *	      ' do agente ',A11,' sem Distrito. Colocado no ',I2)
9900	FORMAT(A4,':',A4,I4.4,'.FIL')
9901	FORMAT(5A4)
	END
C
C SUBROUTINE TO OPEN DRAW FILES FOR WINSEL.
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE OPNDRW(FILE,UNT)
	IMPLICIT NONE
C
 	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RELCONC.DEF'
C
	INTEGER*4 FILE(5), ST, UNT
	INTEGER*4   I4FILE
	INTEGER*4   VOL_EXISTS, CLANG
	CHARACTER*4 CXFILE/'DRAW'/
	EQUIVALENCE (I4FILE,CXFILE)
	CHARACTER*20 CFILE
	INTEGER*4   I4CFILE(5)
	EQUIVALENCE (I4CFILE,CFILE)
C
10	CONTINUE
	CLANG = PORTUG

	CALL USRCLOS1(     UNT)
	CALL OPENW(UNT,FILE,5,0,0,ST)
	IF(ST.EQ.0) RETURN
C
	IF(CLANG.EQ.SPANISH) THEN
	    WRITE(5,9100) IAM(),FILE,ST
	ELSE
	    WRITE(5,900) IAM(),FILE,ST
	ENDIF

	CALL FASTMOV(FILE,I4CFILE,5)
	VOL_EXISTS = INDEX(CFILE,':')
	IF(VOL_EXISTS .LE. 0) THEN
	    CFILE = '    '//':'//CFILE(1:15)
	ELSE
	    CFILE = '    '//':'//CFILE(6:20)
	ENDIF
	CALL FASTMOV(I4CFILE,FILE,5)
	FILE(1) = I4FILE
	IF(CLANG.EQ.SPANISH) THEN
	    WRITE(5,9120) IAM(),FILE(1)
	ELSE
	    WRITE(5,920) IAM(),FILE(1)
	ENDIF
	CALL GPAUSE
	GOTO 10
C
C	FORMAT MESSAGES IN ENGLISH
C
900	FORMAT(1X,A,1X,5A4,' open error> ',I4)
920	FORMAT(1X,A,'Mount ',A4,' pack and continue WINSEL')
C
C	FORMAT MESSAGES IN SPANISH
C
9100	FORMAT(1X,A,1X,5A4,' error de apertura > ',I4)
9120	FORMAT(1X,A,'Montar volumen ',A4,' y continuar WINSEL')
	END
C
C***************************************************
C SUBROUTINE TMFCARRY
C***************************************************
C
  	SUBROUTINE TMFCARRY(DRAW,ENDCDC,
     *			    ANTECIPADA,NUMCARRY,CNTTKTS,
     *                      AMTUFS,CANTAB,CANIND,ERRFLG,JKRNBR,NEXTDRWAMT)

	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:NBRCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:RELCONC.DEF'
C
C LOCAL FUNCTIONS
C
	CHARACTER*8     IAGT_NO
C
C LOCAL VARIABLES
C
	LOGICAL   ERRFLG
	LOGICAL   EOF    /.FALSE./
	LOGICAL   GEN_REPORTS
C
	INTEGER*4 DRAW(MAXGAM), I, ST, NUMCARRY(0:NUMUF,MAXGAM)
	INTEGER*4 GIND,DATECDC
	INTEGER*4 S,CLANG
	INTEGER*4 FILE(5),DRWW,GNUM,UF
	INTEGER*4 CNTTKTS(4,3,MAXGAM),PREV_SER,INDNEXT
	INTEGER*4 ATE_ONDE, CNTREC
	INTEGER*4 LOGREC(LMUREC)
	INTEGER*4 ENDCDC(MAXGAM)
C
	INTEGER*4 CANTAB(6,MAXCAN,MAXGAM)                         !V04
	INTEGER*4 CANIND(MAXGAM), JKRNBR, NEXTDRWAMT(6,MAXGAM,*)
C
	INTEGER*8 AMTUFS(0:NUMUF,MAXGAM)
	INTEGER*8 ANTECIPADA(0:NUMUF,MAXGAM),TEMP

	CHARACTER*20 CFILES
	EQUIVALENCE (FILE,CFILES)

	BYTE      I1TEMP(4)
	EQUIVALENCE (TEMP,I1TEMP)


	DATECDC = DAYCDC
	GEN_REPORTS = .FALSE.
	DO GNUM=1,MAXGAM
	    IF(ENDCDC(GNUM).EQ.DATECDC) GEN_REPORTS = .TRUE.
	ENDDO

	IF(.NOT.GEN_REPORTS) RETURN

	CLANG = PORTUG

	ATE_ONDE  = NXTSER
	TYPE *,IAM(),' LEITURA INICIAL DO TMF ATE O NRO. DE SERIE ',ATE_ONDE-3

	WRITE(CFILES,9901) (SFNAMES(I,PTMF),I=1,5)
C
C OPEN THE TMF FILE
C
	CALL OPENW(PTMF,SFNAMES(1,PTMF),4,0,0,ST)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,PTMF),1,ST,0)
	CALL TOPEN(PTMF)

	IF(CLANG.EQ.SPANISH) THEN
	    WRITE(5,9110) IAM(),(FILE(S),S=1,5)
	ELSEIF(CLANG.EQ.PORTUG) THEN
	    WRITE(5,9210) IAM(),(FILE(S),S=1,5)
	ELSE
	    WRITE(5,910) IAM(),(FILE(S),S=1,5)
	ENDIF
C
C SCAN FILE
C
	PREV_SER = 0
	CNTREC   = 1
C
	DO  WHILE( .NOT. EOF )
C
	    CALL READTMF(LOGREC,CNTREC,EOF)
C
C WAIT FOR ALL SELECTED GAMES TO BE CLOSED
C
CC		    IF(PREV_SER + (LENGTH/LREC) .GE. (ATE_ONDE - 3) )
CC     *		       CALL WAIT_GCLOSE(BEGCDC, CLOSTIM, ATE_ONDE)
C
	    CALL LOGTRA(TRABUF,LOGREC)
C
		    PREV_SER = MOD(TRABUF(TSER), SYSOFF)
C
		    IF (MOD(CNTREC, 1000000) .EQ. 0) THEN
		      TYPE*,IAM(),'Regs lidos ', CNTREC,
     *				  ' Serial ', MOD(TRABUF(TSER),SYSOFF)
		    ENDIF

		    GNUM = TRABUF(TGAM)
		    GIND = TRABUF(TGAMIND)
		    IF(GNUM.LE.0 .OR. GIND.LE.0) GOTO 1000  !V16

		    IF( TRABUF(TTYP).EQ.TWAG .AND.
     *		       (TRABUF(TSTAT).EQ.GOOD.OR.TRABUF(TSTAT).EQ.VOID.OR.
     *		        TRABUF(TSTAT).EQ.INCA) .AND.
     *                  ENDCDC(GNUM).EQ.DATECDC .AND.
     *		       (TRABUF(TWBEG).EQ.DRAW(GNUM) .OR. 
     *			(TRABUF(TGAMTYP).EQ.TLTO .AND. 
     *			 LTOADW(GIND).EQ.1 .AND. TRABUF(TTIM).LE.LTOCTM(GIND)))
     *		      ) THEN

			CALL UPD_TKT_TMF(TRABUF,CNTTKTS,JKRNBR)
		
			UF = AGTTAB(AGTNUM,TRABUF(TTER))/100000
			IF (UF.LT.0.OR.UF.GT.NUMUF) THEN
		           UF = TRABUF(TAGT)/100000
		           WRITE(5,9300)IAM(),TRABUF(TGAM),
     *		                              CMONY(TRABUF(TWAMT),12,BETUNIT),
     *			                      TRABUF(TTER),IAGT_NO(TRABUF(TAGT)),UF
			ENDIF
C
C GET AMOUNT/UF FOR THE CURRENT DRAW
C
			IF (TRABUF(TWBEG).EQ.DRAW(TRABUF(TGAM))) THEN
			   AMTUFS(UF,TRABUF(TGAM)) =
     *		           AMTUFS(UF,TRABUF(TGAM)) + TRABUF(TWAMT)
CC     *		           AMTUFS(UF,TRABUF(TGAM)) + (TRABUF(TWAMT) - TRABUF(TWKAMT))

			   IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0 .AND. JKRNBR.NE.TRABUF(TGAM)) 
     *                        AMTUFS(UF,JKRNBR) = AMTUFS(UF,JKRNBR) + TRABUF(TWKAMT)
C
		           IF (TRABUF(TWBEG).EQ.TRABUF(TWEND)) THEN
			      NEXTDRWAMT(1,TRABUF(TGAM),TRABUF(TTER)) =
     *                        NEXTDRWAMT(1,TRABUF(TGAM),TRABUF(TTER)) + TRABUF(TWAMT)
CC     *                        NEXTDRWAMT(1,TRABUF(TGAM),TRABUF(TTER)) + (TRABUF(TWAMT) - TRABUF(TWKAMT))

			      IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0 .AND. JKRNBR.NE.TRABUF(TGAM))
     *			         NEXTDRWAMT(1,JKRNBR,TRABUF(TTER)) =
     *                           NEXTDRWAMT(1,JKRNBR,TRABUF(TTER)) + TRABUF(TWKAMT)
			   ELSE
			      INDNEXT = 2
			      DO  DRWW = TRABUF(TWBEG),TRABUF(TWEND)
				     NEXTDRWAMT(INDNEXT,TRABUF(TGAM),TRABUF(TTER)) =
     *				     NEXTDRWAMT(INDNEXT,TRABUF(TGAM),TRABUF(TTER)) + TRABUF(TWAMT)
CC     *				     NEXTDRWAMT(INDNEXT,TRABUF(TGAM),TRABUF(TTER)) + (TRABUF(TWAMT) - TRABUF(TWKAMT))

				     IF  (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0)
     *					 NEXTDRWAMT(INDNEXT,JKRNBR,TRABUF(TTER)) =
     *					 NEXTDRWAMT(INDNEXT,JKRNBR,TRABUF(TTER)) + TRABUF(TWKAMT)
				     INDNEXT = INDNEXT + 1
			      ENDDO
			   ENDIF
CC			ELSEIF(TRABUF(TWEND).GE.DRAW(TRABUF(TGAM)) THEN
CC			      INDBEFR = DRAW(TRABUF(TGAM)) - TRABUF(TWBEG)
CC			      BEFRDRWAMT(INDBEFR,TRABUF(TGAM),TRABUF(TTER)) =
CC     *                        BEFRDRWAMT(INDBEFR,TRABUF(TGAM),TRABUF(TTER)) + TRABUF(TWAMT)
CC
CC			      IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0)
CC     *			         BEFRDRWAMT(INDBEFR,JKRNBR,TRABUF(TTER)) =
CC     *                           BEFRDRWAMT(INDBEFR,JKRNBR,TRABUF(TTER)) + TRABUF(TWKAMT)
			ENDIF

			DO DRWW = TRABUF(TWBEG),TRABUF(TWEND)
      			   IF (DRWW.GE.DRAW(TRABUF(TGAM))) THEN
      			      IF  (DRWW.GT.DRAW(TRABUF(TGAM))) THEN  
      				  ANTECIPADA(UF,TRABUF(TGAM)) =
     *				  ANTECIPADA(UF,TRABUF(TGAM)) + TRABUF(TWAMT)
CC     *				  ANTECIPADA(UF,TRABUF(TGAM)) + (TRABUF(TWAMT) - TRABUF(TWKAMT))

			          IF  (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0)
     *                                ANTECIPADA(UF,JKRNBR) = ANTECIPADA(UF,JKRNBR) + TRABUF(TWKAMT)
C*****				  NUMCARRY(UF,TRABUF(TGAM)) = 
C*****				  NUMCARRY(UF,TRABUF(TGAM)) + 1
      		              ENDIF
      			   ENDIF
      			ENDDO

      			IF ( TRABUF(TWEND) .GT. DRAW(TRABUF(TGAM)) ) THEN
			   NUMCARRY(UF,TRABUF(TGAM)) = 
     *					  NUMCARRY(UF,TRABUF(TGAM)) + 1
			   IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0)
     *                        NUMCARRY(UF,JKRNBR) = NUMCARRY(UF,JKRNBR) + 1
      			ENDIF

		    ENDIF

C**************************************************************************
C
C UPDATE OUR VARIABLES IF WE HAVE A (INTERNAL) CANCELLATION
C AFTER WE'VE STARTED READING TM FILE
C
		    IF( (TRABUF(TTYP).EQ.TCAN .OR. TRABUF(TTYP).EQ.TINC) .AND.
     *		        (TRABUF(TSTAT).EQ.GOOD) .AND.
     *                   ENDCDC(GNUM).EQ.DATECDC .AND.
     *		        (TRABUF(TWBEG).EQ.DRAW(GNUM) .OR. 
     *			 (TRABUF(TGAMTYP).EQ.TLTO .AND. 
     *			  LTOADW(GIND).EQ.1 .AND. TRABUF(TTIM).LE.LTOCTM(GIND)))
     *		        ) THEN

			CANIND(GNUM) = CANIND(GNUM) + 1
		        IF (CANIND(GNUM) .GT. MAXCAN) THEN
			    IF(.NOT. ERRFLG)
     *			      TYPE *, 
     *                            ' Invalidacoes em excesso para relatorio ', 
     *				  CANIND(GNUM)
			    ERRFLG = .TRUE.
			ELSE
			    CANTAB(1,CANIND(GNUM),GNUM) = TRABUF(TAGT)
			    CANTAB(2,CANIND(GNUM),GNUM) =
     *                              ISHFT(TRABUF(TCDC),16) + TRABUF(TWLMFI)
			    CANTAB(3,CANIND(GNUM),GNUM) = TRABUF(TWCSER)
			    CANTAB(4,CANIND(GNUM),GNUM) = TRABUF(TWAMT)
			    CANTAB(5,CANIND(GNUM),GNUM) = 
     *				    ISHFT(TRABUF(TWBEG),16) + TRABUF(TWDUR)
      			    CANTAB(6,CANIND(GNUM),GNUM) = TRABUF(TWKAMT)     !V04
	
			ENDIF

			CALL UPD_TKT_TMF(TRABUF,CNTTKTS,JKRNBR)

			UF = AGTTAB(AGTNUM,TRABUF(TTER))/100000
			IF (UF.LT.0.OR.UF.GT.NUMUF) THEN
		           UF = TRABUF(TAGT)/100000
			   WRITE(5,9300)IAM(),TRABUF(TGAM),
     *		                              CMONY(TRABUF(TWAMT),12,BETUNIT),
     *			                      TRABUF(TTER),IAGT_NO(TRABUF(TAGT)),UF
			ENDIF
C
C GET AMOUNT/UF FOR THE CURRENT DRAW
C
			IF (TRABUF(TWBEG).EQ.DRAW(TRABUF(TGAM))) THEN
			   AMTUFS(UF,TRABUF(TGAM)) =
     *		           AMTUFS(UF,TRABUF(TGAM)) - TRABUF(TWAMT)
CC     *		           AMTUFS(UF,TRABUF(TGAM)) - (TRABUF(TWAMT) - TRABUF(TWKAMT))

			   IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0) 
     *                        AMTUFS(UF,JKRNBR) = AMTUFS(UF,JKRNBR) - TRABUF(TWKAMT)
C
		           IF (TRABUF(TWBEG).EQ.TRABUF(TWEND)) THEN
			      NEXTDRWAMT(1,TRABUF(TGAM),TRABUF(TTER)) =
     *                        NEXTDRWAMT(1,TRABUF(TGAM),TRABUF(TTER)) - TRABUF(TWAMT)
CC     *                        NEXTDRWAMT(1,TRABUF(TGAM),TRABUF(TTER)) - (TRABUF(TWAMT) - TRABUF(TWKAMT))

			      IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0 .AND. JKRNBR.NE.TRABUF(TGAM))
     *			         NEXTDRWAMT(1,JKRNBR,TRABUF(TTER)) =
     *                           NEXTDRWAMT(1,JKRNBR,TRABUF(TTER)) - TRABUF(TWKAMT)
			   ELSE
			      INDNEXT = 2
			      DO  DRWW = TRABUF(TWBEG),TRABUF(TWEND)
				     NEXTDRWAMT(INDNEXT,TRABUF(TGAM),TRABUF(TTER)) =
     *				     NEXTDRWAMT(INDNEXT,TRABUF(TGAM),TRABUF(TTER)) - TRABUF(TWAMT)
CC     *				     NEXTDRWAMT(INDNEXT,TRABUF(TGAM),TRABUF(TTER)) - (TRABUF(TWAMT) - TRABUF(TWKAMT))

				     IF  (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0)
     *					 NEXTDRWAMT(INDNEXT,JKRNBR,TRABUF(TTER)) =
     *					 NEXTDRWAMT(INDNEXT,JKRNBR,TRABUF(TTER)) - TRABUF(TWKAMT)
				     INDNEXT = INDNEXT + 1
			      ENDDO
			   ENDIF
CC			ELSEIF(TRABUF(TWEND).GE.DRAW(TRABUF(TGAM)) THEN
CC			      INDBEFR = DRAW(TRABUF(TGAM)) - TRABUF(TWBEG)
CC			      BEFRDRWAMT(INDBEFR,TRABUF(TGAM),TRABUF(TTER)) =
CC     *                        BEFRDRWAMT(INDBEFR,TRABUF(TGAM),TRABUF(TTER)) - TRABUF(TWAMT)
CC
CC			      IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0)
CC     *			         BEFRDRWAMT(INDBEFR,JKRNBR,TRABUF(TTER)) =
CC     *                           BEFRDRWAMT(INDBEFR,JKRNBR,TRABUF(TTER)) - TRABUF(TWKAMT)
			ENDIF

			DO DRWW = TRABUF(TWBEG),TRABUF(TWEND)
     			   IF (DRWW.GE.DRAW(TRABUF(TGAM))) THEN
      			      IF  (DRWW.GT.DRAW(TRABUF(TGAM))) THEN  
      				  ANTECIPADA(UF,TRABUF(TGAM))=
     *				  ANTECIPADA(UF,TRABUF(TGAM)) - TRABUF(TWAMT)
CC     *				  ANTECIPADA(UF,TRABUF(TGAM)) - (TRABUF(TWAMT) - TRABUF(TWKAMT))

			          IF  (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0)
     *                                ANTECIPADA(UF,JKRNBR) = ANTECIPADA(UF,JKRNBR) - TRABUF(TWKAMT)

C*****				  NUMCARRY(UF,TRABUF(TGAM)) = 
C*****				  NUMCARRY(UF,TRABUF(TGAM)) - 1
      			      ENDIF
      			   ENDIF
      			ENDDO

      			IF ( TRABUF(TWEND) .GT. DRAW(TRABUF(TGAM)) ) THEN
			   NUMCARRY(UF,TRABUF(TGAM)) = 
     *					  NUMCARRY(UF,TRABUF(TGAM)) - 1
			IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0)
     *                     NUMCARRY(UF,JKRNBR) = NUMCARRY(UF,JKRNBR) - 1
      			ENDIF

C****		        ENDIF
		    ENDIF
C
1000		    CONTINUE
	ENDDO
	CALL USRCLOS1 (     PTMF)

	TYPE*,IAM(),' Verificacao de 5 Semanas encerrada no TMF'
	RETURN

900	FORMAT(1X,A,1X,5A4,' read error> ',I4,' block> ',I8)
910	FORMAT(1X,A,1X,'Scanning file ',5A4,' for winners ')
9100	FORMAT(1X,A,1X,5A4,' error de lectura > ',I4,' bloque > ',I8)
9110	FORMAT(1X,A,1X,'Leyendo archivo ',5A4,' para ganadores')
9200	FORMAT(1X,A,1X,5A4,' erro de leitura> ',I4,' bloco> ',I8)
9210	FORMAT(1X,A,1X,'Pesquisando arquivo ',5A4,' para concurso')
9300	FORMAT(1X,A,1X,' Jogo ',I2,' valor ',A12,' no terminal ',I6,
     *	      ' do agente ',A11,' sem Distrito. Colocado no ',I2)
9900	FORMAT(A4,':',A4,I4.4,'.FIL')
9901	FORMAT(5A4)
	END
C
C***************************************************
C SUBROUTINE MOUSECARRY
C***************************************************
  	SUBROUTINE MOUSECARRY(DRAW,ENDCDC,ANTECIPADA,NUMCARRY,MULTI,
     *                        CNTTKTS,AMTUFS,CANTAB,CANIND,ERRFLG,JKRNBR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:NBRCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:RELCONC.DEF'
C
C LOCAL FUNCTIONS
C
	CHARACTER*8     IAGT_NO
C
C LOCAL VARIABLES
C
	LOGICAL   ERRFLG
	LOGICAL   GEN_REPORTS
C
	INTEGER*4 DRAW(MAXGAM), I, ST, DUMMY, NUMCARRY(0:NUMUF,MAXGAM)
	INTEGER*4 MULTI(MAXGAM),LOGREC(LREC*3),ENDCDC(MAXGAM)
	INTEGER*4 S,CLANG,DATECDC
	INTEGER*4 FILE(5),GNUM,UF
	INTEGER*4 CNTTKTS(4,3,MAXGAM), JKRNBR
C
	INTEGER*4 CANTAB(6,MAXCAN,MAXGAM)                     !V04
	INTEGER*4 CANIND(MAXGAM)
C
	INTEGER*8 AMTUFS(0:NUMUF,MAXGAM)
	INTEGER*8 ANTECIPADA(0:NUMUF,MAXGAM),TEMP

	LOGICAL   EOFTCF,READIT
	CHARACTER*20 CFILES
	EQUIVALENCE (FILE,CFILES)

	BYTE      I1TEMP(4)
	EQUIVALENCE (TEMP,I1TEMP)


	CLANG  = PORTUG
	READIT = .FALSE.
	EOFTCF = .FALSE.
	GEN_REPORTS = .FALSE.
	DATECDC = DAYCDC
C
C 	OPEN CARRYOVER FILE, IF NECESSARY
C
	DO GNUM=1,MAXGAM
	    IF(MULTI(GNUM).NE.0 .AND. ENDCDC(GNUM).EQ.DATECDC) READIT=.TRUE.
	ENDDO

	IF (READIT) THEN
	    GEN_REPORTS = .TRUE.

	    CALL IOPEN(SFNAMES(1,TCF),1,LREC*2,LCDC,LSER*2-1,ST)
	    IF(ST.NE.0)  CALL FILERR(SFNAMES(1,TCF),1,ST,0)
C
C LOOP THRU READING ALL TRANSACTIONS IN TCF
C
	    IF(CLANG.EQ.SPANISH) THEN
		WRITE(5,9110) IAM(),(SFNAMES(S,TCF),S=1,5)
	    ELSEIF(CLANG.EQ.PORTUG) THEN
		WRITE(5,9210) IAM(),(SFNAMES(S,TCF),S=1,5)
	    ELSE
		WRITE(5,910) IAM(),(SFNAMES(S,TCF),S=1,5)
	    ENDIF

	    DO WHILE(.NOT.EOFTCF)
		CALL READTCF(LOGREC,1,EOFTCF)
		IF(.NOT.EOFTCF) THEN

		    CALL LOGTRA(TRABUF,LOGREC)
		    GNUM = TRABUF(TGAM)

		    IF(TRABUF(TSTAT).EQ.VOID .OR. TRABUF(TSTAT).EQ.INCA) THEN
			CALL UPD_TKT_COUNT(TRABUF,CNTTKTS,ONCARRY,JKRNBR)

			CANIND(GNUM) = CANIND(GNUM) + 1
		        IF (CANIND(GNUM) .GT. MAXCAN) THEN
			    IF(.NOT. ERRFLG)
     *			      TYPE *, 
     *                            ' Invalidacoes em excesso para relatorio ', 
     *				  CANIND(GNUM)
			    ERRFLG = .TRUE.
			ELSE
			    CANTAB(1,CANIND(GNUM),GNUM) = TRABUF(TAGT)
			    CANTAB(2,CANIND(GNUM),GNUM) =
     *                              ISHFT(TRABUF(TCDC),16) + TRABUF(TWLMFI)
			    CANTAB(3,CANIND(GNUM),GNUM) = TRABUF(TSER)
			    CANTAB(4,CANIND(GNUM),GNUM) = TRABUF(TWAMT)
			    CANTAB(5,CANIND(GNUM),GNUM) = 
     *				    ISHFT(TRABUF(TWBEG),16) + TRABUF(TWDUR)
      			    CANTAB(6,CANIND(GNUM),GNUM) = TRABUF(TWKAMT)     !V04
			ENDIF
		    ENDIF
C
C ADD TRANSACTION TO APPROPIATE SLOT IN LCF TABLE
C
		    IF ((TRABUF(TSTAT) .EQ. GOOD) .OR.
CC     *			(TRABUF(TSTAT) .EQ. XCHD) .OR.
     *		        (TRABUF(TSTAT) .EQ. EXCH))   THEN

     			CALL UPD_TKT_COUNT(TRABUF,CNTTKTS,ONCARRY,JKRNBR)

			IF ( (TRABUF(TGAMTYP) .EQ. TLTO) .OR.
     *			     (TRABUF(TGAMTYP) .EQ. TSPT) .OR.
     *                       (TRABUF(TGAMTYP) .EQ. TTGL)     )THEN

			    UF = AGTTAB(AGTNUM,TRABUF(TTER))/100000
			    IF  (UF.LT.0.OR.UF.GT.NUMUF) THEN
				UF = TRABUF(TAGT)/100000
				WRITE(5,9300)IAM(),TRABUF(TGAM),
     *		                                    CMONY(TRABUF(TWAMT),12,BETUNIT),
     *			                            TRABUF(TTER),IAGT_NO(TRABUF(TAGT)),UF
			    ENDIF
C
C GET AMOUNT/UF FOR THE CURRENT DRAW
C
			    IF(TRABUF(TWBEG).LE.DRAW(TRABUF(TGAM)).AND.
     *			       TRABUF(TWEND).GE.DRAW(TRABUF(TGAM)))THEN
 				AMTUFS(UF,TRABUF(TGAM)) =
     *				AMTUFS(UF,TRABUF(TGAM)) + TRABUF(TWAMT)
CC     *				AMTUFS(UF,TRABUF(TGAM)) + (TRABUF(TWAMT) - TRABUF(TWKAMT))

				IF  (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0 .AND. JKRNBR.NE.TRABUF(TGAM))
     *                              AMTUFS(UF,JKRNBR) = AMTUFS(UF,JKRNBR) + TRABUF(TWKAMT)

CC				INDBEFR = DRAW(TRABUF(TGAM)) - TRABUF(TWBEG)
CC			        BEFRDRWAMT(INDBEFR,TRABUF(TGAM),TRABUF(TTER)) =
CC     *                          BEFRDRWAMT(INDBEFR,TRABUF(TGAM),TRABUF(TTER)) + TRABUF(TWAMT)
CC
CC				IF  (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0)
CC     *			            BEFRDRWAMT(INDBEFR,JKRNBR,TRABUF(TTER)) =
CC     *                              BEFRDRWAMT(INDBEFR,JKRNBR,TRABUF(TTER)) + TRABUF(TWKAMT)
			    ENDIF

			    DO I = TRABUF(TWBEG),TRABUF(TWEND)
      			      IF  (I.GE.DRAW(TRABUF(TGAM))) THEN  !I WANT IT
      				  IF  (I.GT.DRAW(TRABUF(TGAM))) THEN  
      				      ANTECIPADA(UF,TRABUF(TGAM))=
     *				      ANTECIPADA(UF,TRABUF(TGAM))+ TRABUF(TWAMT)
CC     *				      ANTECIPADA(UF,TRABUF(TGAM))+ (TRABUF(TWAMT) - TRABUF(TWKAMT))

				      IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0)
     *                                   ANTECIPADA(UF,JKRNBR) = ANTECIPADA(UF,JKRNBR) + TRABUF(TWKAMT)

C*****				      NUMCARRY(UF, TRABUF(TGAM)) = 
C*****				      NUMCARRY(UF,TRABUF(TGAM)) + 1
      				  ENDIF
      			      ENDIF
      			    ENDDO

      			    IF ( TRABUF(TWEND) .GT. DRAW(TRABUF(TGAM)) ) THEN
			       NUMCARRY(UF,TRABUF(TGAM)) = 
     *					  NUMCARRY(UF,TRABUF(TGAM)) + 1
			       IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0)
     *                            NUMCARRY(UF,JKRNBR) = NUMCARRY(UF,JKRNBR) + 1
      			    ENDIF

			ENDIF
		    ENDIF
		ENDIF
	    ENDDO

	    CALL ICLOSE(1,DUMMY,ST)

	ENDIF

	IF(.NOT.GEN_REPORTS) RETURN

	TYPE*,IAM(),' Verificacao de 5 Semanas encerrada no TCF'
	RETURN

900	FORMAT(1X,A,1X,5A4,' read error> ',I4,' block> ',I8)
910	FORMAT(1X,A,1X,'Scanning file ',5A4,' for winners ')
9100	FORMAT(1X,A,1X,5A4,' error de lectura > ',I4,' bloque > ',I8)
9110	FORMAT(1X,A,1X,'Leyendo archivo ',5A4,' para ganadores')
9200	FORMAT(1X,A,1X,5A4,' erro de leitura> ',I4,' bloco> ',I8)
9210	FORMAT(1X,A,1X,'Pesquisando arquivo ',5A4,' para 5 Semanas')
9300	FORMAT(1X,A,1X,' Jogo ',I2,' valor ',A12,' no terminal ',I6,
     *	      ' do agente ',A11,' sem UF. Colocado na UF ',I2)
9900	FORMAT(A4,':',A4,I4.4,'.FIL')
9901	FORMAT(5A4)
	END

C
C***************************************************
C SUBROUTINE UPD_TKT_COUNT
C***************************************************
C
  	SUBROUTINE UPD_TKT_COUNT(TRABUF,CNTTKTS,ONWHO,JKRNBR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
C PARAMETERS
C
	INTEGER*4 ONNEXT
	PARAMETER (ONNEXT=4)
C
C LOCAL VARIABLES
C
	INTEGER*4 CNTTKTS(4,3,MAXGAM),ONWHO,STATUS,GNUM,GIND,DRAW,GTYP,JKRNBR

	STATUS = TRABUF(TSTAT)
CC	IF(STATUS.EQ.XCHD .OR. STATUS.EQ.EXCH) STATUS = GOOD
	IF(STATUS.EQ.EXCH) STATUS = GOOD

	GTYP = TRABUF(TGAMTYP)
	GNUM = TRABUF(TGAM)
	GIND = TRABUF(TGAMIND)

	IF(GTYP.EQ.TLTO) THEN
	    DRAW = LTODRW(GIND)
	    IF(LTOADW(GIND).EQ.0) THEN
C
C INCREMENT TICKET COUNTS:
C
		CNTTKTS(ONWHO,STATUS,GNUM) = CNTTKTS(ONWHO,STATUS,GNUM) + 1
C
C MULTIDRAW FOR NEXT DRAWS
C
	        IF  ( TRABUF(TWEND) .GT. DRAW )
     *		    CNTTKTS(ONNEXT,STATUS,GNUM) = CNTTKTS(ONNEXT,STATUS,GNUM) + 1
C
C THE SAME FOR JOKER
C
		IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0) THEN
		   CNTTKTS(ONWHO,STATUS,JKRNBR)  = CNTTKTS(ONWHO,STATUS,JKRNBR)  + 1
	           IF  ( TRABUF(TWEND) .GT. DRAW )
     *                 CNTTKTS(ONNEXT,STATUS,JKRNBR) = CNTTKTS(ONNEXT,STATUS,JKRNBR) + 1
		ENDIF
	    ELSE
		IF(TRABUF(TWBEG).EQ.DRAW) THEN
C
C ONLY FOR ACTUAL DRAW
C
		    CNTTKTS(ONWHO,STATUS,GNUM) = CNTTKTS(ONWHO,STATUS,GNUM) + 1
		    IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0)
     *		       CNTTKTS(ONWHO,STATUS,JKRNBR) = CNTTKTS(ONWHO,STATUS,JKRNBR) + 1
		ELSE
C
C MULTIDRAW FOR NEXT DRAWS
C
		    CNTTKTS(ONNEXT,STATUS,GNUM) = 
     *		    CNTTKTS(ONNEXT,STATUS,GNUM) + 1
		    IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0)
     *		       CNTTKTS(ONNEXT,STATUS,JKRNBR) = CNTTKTS(ONNEXT,STATUS,JKRNBR) + 1

		ENDIF
	    ENDIF
	ELSEIF(GTYP.EQ.TSPT .OR. GTYP.EQ.TTGL .OR. GTYP.EQ.TKIK) THEN
C
C INCREMENT TICKET COUNTS:
C Kicker as independent game is already counted, so we add ".AND. JKRNBR.NE.TRABUF(TGAM)" condition
C
	    CNTTKTS(ONWHO,STATUS,GNUM) = CNTTKTS(ONWHO,STATUS,GNUM) + 1
	    IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0 .AND. JKRNBR.NE.TRABUF(TGAM))
     *	       CNTTKTS(ONWHO,STATUS,JKRNBR) = CNTTKTS(ONWHO,STATUS,JKRNBR) + 1

	ENDIF

	RETURN
	END
C
C***************************************************
C SUBROUTINE UPD_TKT_TMF
C***************************************************
C
  	SUBROUTINE UPD_TKT_TMF(TRABUF,CNTTKTS,JKRNBR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:RELCONC.DEF'
C
C LOCAL VARIABLES
C
	INTEGER*4 CNTTKTS(4,3,MAXGAM),STATUS,GNUM,GIND,DRAW,GTYP,JKRNBR

	STATUS = TRABUF(TSTAT)
CC	IF(STATUS.EQ.XCHD .OR. STATUS.EQ.EXCH) STATUS = GOOD
	IF(STATUS.EQ.EXCH) STATUS = GOOD

	GTYP = TRABUF(TGAMTYP)
	GNUM = TRABUF(TGAM)
	GIND = TRABUF(TGAMIND)

	IF  (GTYP.EQ.TLTO) THEN
	    DRAW = LTODRW(GIND)
	    IF(TRABUF(TTYP).EQ.TWAG) THEN
		CNTTKTS(ONTMF,GOOD,GNUM)  = CNTTKTS(ONTMF,GOOD,GNUM) + 1
	        IF  ( TRABUF(TWEND) .GT. DRAW )
     *		    CNTTKTS(ONNEXT,GOOD,GNUM) = CNTTKTS(ONNEXT,GOOD,GNUM) + 1
C
	        IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0) THEN
		   CNTTKTS(ONTMF,GOOD,JKRNBR)  = CNTTKTS(ONTMF,GOOD,JKRNBR) + 1
	           IF  ( TRABUF(TWEND) .GT. DRAW )
     *	               CNTTKTS(ONNEXT,GOOD,JKRNBR) = CNTTKTS(ONNEXT,GOOD,JKRNBR) + 1
	        ENDIF
	    ELSE
		CNTTKTS(ONTMF,GOOD,GNUM)  = CNTTKTS(ONTMF,GOOD,GNUM) - 1
	        IF  ( TRABUF(TWEND) .GT. DRAW )
     *		    CNTTKTS(ONNEXT,GOOD,GNUM) = CNTTKTS(ONNEXT,GOOD,GNUM) - 1
C
	        IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0) THEN
		   CNTTKTS(ONTMF,GOOD,JKRNBR)  = CNTTKTS(ONTMF,GOOD,JKRNBR) - 1
	           IF  ( TRABUF(TWEND) .GT. DRAW )
     *		       CNTTKTS(ONNEXT,GOOD,JKRNBR) = CNTTKTS(ONNEXT,GOOD,JKRNBR) - 1
		ENDIF

		IF(TRABUF(TTYP).EQ.TCAN) THEN
		    CNTTKTS(ONTMF,VOID,GNUM)  = CNTTKTS(ONTMF,VOID,GNUM) + 1
	           IF  ( TRABUF(TWEND) .GT. DRAW )
     *		       CNTTKTS(ONNEXT,VOID,GNUM) = CNTTKTS(ONNEXT,VOID,GNUM) + 1
C
	            IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0) THEN
		       CNTTKTS(ONTMF,VOID,JKRNBR)  = CNTTKTS(ONTMF,VOID,JKRNBR) + 1
	               IF  ( TRABUF(TWEND) .GT. DRAW )
     *		           CNTTKTS(ONNEXT,VOID,JKRNBR) = CNTTKTS(ONNEXT,VOID,JKRNBR) + 1
		    ENDIF
		ELSE
		    CNTTKTS(ONTMF,INCA,GNUM)  = CNTTKTS(ONTMF,INCA,GNUM) + 1
		    IF  ( TRABUF(TWEND) .GT. DRAW )
     *                  CNTTKTS(ONNEXT,INCA,GNUM) = CNTTKTS(ONNEXT,INCA,GNUM) + 1
C
	            IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0) THEN
		       CNTTKTS(ONTMF,INCA,JKRNBR)  = CNTTKTS(ONTMF,INCA,JKRNBR) + 1
	               IF  ( TRABUF(TWEND) .GT. DRAW )
     *		           CNTTKTS(ONNEXT,INCA,JKRNBR) = CNTTKTS(ONNEXT,INCA,JKRNBR) + 1
		    ENDIF
		ENDIF							 
	    ENDIF
	ELSEIF(GTYP.EQ.TSPT .OR. GTYP.EQ.TTGL .OR. GTYP.EQ.TKIK) THEN
C
C INCREMENT TICKET COUNTS:
C Kicker as independent game is already counted, so we add ".AND. JKRNBR.NE.TRABUF(TGAM)" condition
C
	    IF(TRABUF(TTYP).EQ.TWAG) THEN
		CNTTKTS(ONTMF,GOOD,GNUM) = CNTTKTS(ONTMF,GOOD,GNUM) + 1
	        IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0 .AND. JKRNBR.NE.TRABUF(TGAM))
     *		   CNTTKTS(ONTMF,GOOD,JKRNBR) = CNTTKTS(ONTMF,GOOD,JKRNBR) + 1
	    ELSE
		CNTTKTS(ONTMF,GOOD,GNUM) = CNTTKTS(ONTMF,GOOD,GNUM) - 1
	        IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0 .AND. JKRNBR.NE.TRABUF(TGAM))
     *		   CNTTKTS(ONTMF,GOOD,JKRNBR) = CNTTKTS(ONTMF,GOOD,JKRNBR) - 1

		IF(TRABUF(TTYP).EQ.TCAN) THEN
		    CNTTKTS(ONTMF,VOID,GNUM) = CNTTKTS(ONTMF,VOID,GNUM) + 1
	            IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0 .AND. JKRNBR.NE.TRABUF(TGAM))
     *		       CNTTKTS(ONTMF,VOID,JKRNBR) = CNTTKTS(ONTMF,VOID,JKRNBR) + 1
		ELSE
		    CNTTKTS(ONTMF,INCA,GNUM) = CNTTKTS(ONTMF,INCA,GNUM) + 1
	            IF (TRABUF(TWKAMT).GT.0 .AND. JKRNBR.GT.0 .AND. JKRNBR.NE.TRABUF(TGAM))
     *		       CNTTKTS(ONTMF,INCA,JKRNBR) = CNTTKTS(ONTMF,INCA,JKRNBR) + 1
		ENDIF
	    ENDIF
	ENDIF

	RETURN
	END
C
C***************************************************
C SUBROUTINE WAIT_GCLOSE
C***************************************************
C
C=======OPTIONS    /CHECK=NOOVERFLOW
  	SUBROUTINE WAIT_GCLOSE(BEGCDC, CLOSTIM, ATE_ONDE)
	IMPLICIT   NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:TGLCOM.DEF'
C
C ROUTINE PARAMETERS
C
	INTEGER*4 BEGCDC(MAXGAM), CLOSTIM(MAXGAM), ATE_ONDE
C
C LOCAL VARIABLES
C
	LOGICAL NAOPODE
	INTEGER*4 MYGNUM, MYGTYP, MYGIND
	INTEGER*4 ST
C
C
	NAOPODE=.TRUE.
	DO WHILE(NAOPODE)
	    TYPE*,IAM(),'Aguardando fechamento dos jogos selecionados'
	    NAOPODE=.FALSE.
	    DO MYGNUM=1,MAXGAM      
		MYGTYP = GNTTAB(GAMTYP,MYGNUM)
		MYGIND = GNTTAB(GAMIDX,MYGNUM)
C
		IF(BEGCDC(MYGNUM).NE.0)THEN
    		    IF( (MYGTYP.EQ.TLTO .AND. 
     *		         LTOSTS(MYGIND).LT.GAMBFD) .OR.
     *		        (MYGTYP.EQ.TSPT .AND. 
     *			 SPTSTS(MYGIND).LT.GAMBFD) .OR.
     *		        (MYGTYP.EQ.TTGL .AND. 
     *			 TGLSTS(MYGIND).LT.GAMBFD)     )THEN
			NAOPODE=.TRUE.
		    ELSE
			IF(MYGTYP.EQ.TLTO) THEN
			    CLOSTIM(MYGNUM) = LTOCTM(MYGIND)
			ELSEIF(MYGTYP.EQ.TSPT) THEN
			    CLOSTIM(MYGNUM) = SPTCTM(MYGIND)
			ELSEIF(MYGTYP.EQ.TTGL) THEN
			    CLOSTIM(MYGNUM) = TGLCTM(MYGIND)
			ENDIF
		    ENDIF
		ENDIF
	    ENDDO

	    IF(NAOPODE) THEN
		CALL XWAIT(5,3,ST)
	    ELSE
		ATE_ONDE = 999999999
	    ENDIF
	ENDDO

	TYPE*,IAM(),'Jogos selecionados encerrados. Proseguindo...'
	RETURN
	END
C
C SUBROUTINE TO READ TRANSACTION FROM CARRYOVER FILE
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE READTCF(LOGREC,LU,EOT)
	IMPLICIT   NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:HSHCOM.DEF'
	INTEGER*4 TCFBUF(I4BUCSIZ),LOGREC(*), ST, LU
	LOGICAL EOT
C
C
	CALL ISREAD(LOGREC,LU,TCFBUF,ST)
	IF(ST.EQ.ERREND) THEN
	  EOT=.TRUE.
	  RETURN
	ENDIF
C
C
	IF(ST.NE.0) THEN
	  TYPE*,'Carryover read error ',ST
	  CALL GPAUSE
	  EOT=.TRUE.
	  RETURN
	ENDIF
	RETURN
	END
C
C FUNCTION TO CONVERT AN INTEGER*8
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	CHARACTER*32 FUNCTION MYCMONYI8(AMOUNT,LEN,FACTOR)
	IMPLICIT     NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
C FUNCTION PARAMETERS
C
	INTEGER*8 AMOUNT
	INTEGER*4 LEN
	INTEGER*4 FACTOR
C
C LOCAL VARIABLES
C
	INTEGER*4 AMTAUX(2)
C
	AMTAUX(1) = AMOUNT/DYN_BETUNIT
	AMTAUX(2) = MOD(AMOUNT,DYN_BETUNIT)
C
	MYCMONYI8 = CSMONYI8(AMTAUX,LEN,FACTOR)
C
	RETURN
	END
