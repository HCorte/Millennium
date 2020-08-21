C CANSORT.FOR
C
C V08 24-JUN-2011 FJG Cancellations amount is counted twice for JOKER
C V07 18-MAR-2010 HXK remove Lotto1, Lotto2, add Lotto3, Lotto4
C V06 08-FEB-11 RXK LOTOSLUN INITIALIZED
C v05 15-10-2009 CMB totobola extra dump
C V04 09-FEB-04 CMB INCLUDE KICK AMOUNT
C V03 29-MAY-03 CMB NEW FILE  LOT/LOT2
C V02 16-APR-03 CMB INCLUDE SUBTOTALS PER AGENT AND WEEK/YEAR ON THE HEADER
C V01 16-APR-01 CS  INITIAL RELEASE FOR PORTUGAL
C
C PRINT REPORT WITH ALL CANCELLED WAGERS GOING INTO A DRAW.
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE CANSORT(GNUM,DRAW,CANTAB,COUNT)
	IMPLICIT   NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

C
C LOCAL PARAMETERS
C
	INTEGER*4 MAXPRTLN
	PARAMETER (MAXPRTLN=59)
C
C LOCAL FUNCTIONS
C
	CHARACTER*8     IAGT_NO
C
	INTEGER*4	ST, GNUM
	INTEGER*4       GTYP
	INTEGER*4	CANTAB(6,50000)   !V04 
	INTEGER*4       KAMT              !V04
	INTEGER*4	COUNT
	INTEGER*4	DRAW
	INTEGER*4	START
	INTEGER*4	SCNT
	INTEGER*4	I
	INTEGER*4	PAGE
	INTEGER*4	LINCNT
	INTEGER*2	DAT(12)
	INTEGER*4	SER
	INTEGER*4	AMT
	INTEGER*4	JUL
	INTEGER*4	FRSDRW
	INTEGER*4	DUR
	INTEGER*4	LSTDRW
	INTEGER*4	AGT
	INTEGER*4	SERIAL
	INTEGER*4	CHK
	INTEGER*4	COPY
	INTEGER*4	CDC
	INTEGER*4	WEEK,YEAR
	INTEGER*4       FLGLTO
	CHARACTER*2     CFLGLTO
	CHARACTER*53	REPHED
	CHARACTER*12	REPNAM
	INTEGER*4	REPLU/7/
	INTEGER*4	WEEK1,YEAR1          !V02
	INTEGER*4	TOTCAN, TOTJOK       !V02 E v04
	INTEGER*4       TOTVALAMT            !V02
	INTEGER*4       VALAMT               !V02 VALUE OF CANCEL (AMOUNT*DUR+KAMT) 
	INTEGER*4       TOTVAL               !V02 VALUE OF CANCEL PER AGENT(AMOUNT)
	INTEGER*4       TOTAMT, TOTKAMT      !V02 TOTAL AMOUNT PER AGT
	INTEGER*4       TOTCNT               !V02 TOTAL COUNT PER AGT 
	INTEGER*4       PRVAGT               !V02 PREV AGENT
C2345#=======================================================================
	

C	CALL PRMNUM ('Relatorio de INVALIDADOS - Copias:',
C     *		      COPY, 0, 20, EXT)
C	IF (EXT.LT.0) RETURN
C
C CHECK GAME NUMBER
C
	IF (GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
	   TYPE *, IAM(), ' Jogo ', GNUM, ' invalido '
	   RETURN
	ENDIF
	GTYP = GNTTAB(GAMTYP,GNUM)
C
C SORT CANCELLATION TABLE
C
	TYPE *, IAM(), ' Ordenando por revendedor e data'
	TYPE *, IAM(), ' Processando ', COUNT, ' registros'
	CALL ISORTB (CANTAB,COUNT,1,6)                          !V04
	TYPE *, IAM(), ' Fase 1 completa '
	START = 1
	SCNT = 1
	DO I = 2,COUNT+1
	  IF (CANTAB(1,I).NE.CANTAB(1,I-1)) THEN
	    CALL ISORTB (CANTAB(1,START),SCNT,2,6)              !V04
	    SCNT = 0
	    START = I
	  ENDIF
	  SCNT = SCNT + 1
	ENDDO
	TYPE *, IAM(), ' Fase 2 completa'
C
C+++++++++++++++++++++++++++++++++++++++++++++
C
C OPEN REPORT FILE
C
	WRITE (REPNAM,9200) GSNAMES(GNUM)
	CALL ROPEN (REPNAM,REPLU,ST)
	IF (ST.NE.0) THEN
	   TYPE *, IAM(), 'CANSORT-Erro ',ST, ' na abertura do arquivo ', REPNAM
	   CALL GPAUSE
	ENDIF
	
	CALL GETWEK(DRAW,GNUM,WEEK1,YEAR1,ST)                     !V02
	IF (ST.NE.0) THEN
	    TYPE *,IAM(),'CANSORT-Erro ao procurar o concurso',WEEK1,'/',YEAR1
	ENDIF
	    
	WRITE (REPHED,9201) DRAW, WEEK1,YEAR1                    !V02
	
	LINCNT = MAXPRTLN + 2
	CALL TITLE (REPHED,REPNAM(1:8), 1, REPLU, PAGE, DAYCDC)
	WRITE (REPLU,9202)
	LINCNT = 8
C
C LOOP THRU CANCELS AND PRINT
C
	TOTCAN = 0
	TOTJOK = 0
	TOTVALAMT = 0

	PRVAGT  = 0            ! V02
	TOTCNT  = 0            ! V02
	TOTAMT  = 0            ! V02
	TOTVAL  = 0            ! V02
	TOTKAMT  = 0           ! V04
	
	DO 1000 I = 1,COUNT
C
	IF (LINCNT.GT.MAXPRTLN) THEN
	  CALL TITLE (REPHED,REPNAM(1:8), 1, REPLU, PAGE, DAYCDC)
	  WRITE (REPLU,9202)
	  LINCNT = 8
	ENDIF
C
C DECODE CANTAB
C
	AGT    = CANTAB(1,I)
	CDC    = ISHFT (CANTAB(2,I), -16)
	FLGLTO = IAND (CANTAB(2,I), '0000000F'X)
	SER    = CANTAB(3,I)
C-------V08---------------------------------------------------------------------
	IF(GTYP.EQ.TKIK) THEN
	  AMT    = CANTAB(4,I) - CANTAB(6,I)
	ELSE
	  AMT    = CANTAB(4,I)	
	ENDIF
C-------V08---------------------------------------------------------------------
	FRSDRW = ISHFT (CANTAB(5,I), -16)
	DUR    = IAND (CANTAB(5,I), 'FFFF'X)
	KAMT   = CANTAB(6,I)                       !v04
C
	CFLGLTO = '  '
	IF  (FLGLTO.GT.0) CFLGLTO = '**'
C
	IF(GNUM.EQ.6.OR.GNUM.EQ.7) THEN
	  CALL GETWEK(FRSDRW,GNUM,WEEK,YEAR,ST)
	  IF(ST.NE.0) THEN
	    WEEK=0
	    YEAR=YEAR1
	  ENDIF
	ELSE
	  CALL FIGWEK(CDC, WEEK, YEAR)
	ENDIF

	VALAMT = DUR*(AMT + KAMT)                 !v02 e V04
	TOTCAN = TOTCAN + AMT 
	TOTJOK = TOTJOK + KAMT                     !V04
	TOTVALAMT = TOTVALAMT + VALAMT             !v02

	LSTDRW = FRSDRW + DUR - 1

	DAT(VCDC) = CDC
	CALL CDATE(DAT)
	JUL = DAT(VJUL)
	CALL OUTGEN (CDC,SER,SERIAL,CHK)
C           
C AGENT
C
	IF (AGT .EQ. PRVAGT) THEN               !AGT  V02
	 
	  WRITE (REPLU, 9003) IAGT_NO(AGT), 
     *             DAT(VDAY), DAT(VMON),
     *             DAT(VYEAR), CDC, SER, JUL, SERIAL, 
     *             CHK, CFLGLTO, WEEK, YEAR, DUR,
     *             CMONY(AMT,10,BETUNIT), CMONY(KAMT,10,BETUNIT), 
     *             CMONY(VALAMT,10,BETUNIT)
	 

          TOTCNT = TOTCNT + 1          !V02
          TOTAMT = TOTAMT + AMT        !V02
	  TOTVAL = TOTVAL + VALAMT     !V02
	  TOTKAMT = TOTKAMT + KAMT     !V04 
	  LINCNT = LINCNT + 1
	
	ELSE
	
C WRITE TOTAL'S AND BEGIN VARIABLES	
	
	   IF (TOTCNT .NE. 0) THEN	
	      WRITE(REPLU,102) TOTCNT, CMONY(TOTAMT,12,BETUNIT),
     *                         CMONY(TOTKAMT,12,BETUNIT),
     *                         CMONY(TOTVAL, 12, BETUNIT)                 !V02
	   ENDIF

	   WRITE (REPLU,9003) IAGT_NO(AGT), 
     *	           DAT(VDAY),DAT(VMON),DAT(VYEAR),
     *             CDC, SER, JUL, SERIAL, CHK, 
     *             CFLGLTO, WEEK, YEAR, DUR,
     *             CMONY(AMT,10,BETUNIT),CMONY(KAMT,10,BETUNIT),
     *             CMONY(VALAMT,10,BETUNIT)

	   TOTCNT  = 1                              !V02
	   TOTAMT  = AMT                            !V02
	   TOTKAMT = KAMT
	   PRVAGT  = AGT                             !V02
           TOTVAL  = VALAMT                          !V02

	   LINCNT = LINCNT + 4                    ! TOTALS AND NEW LINE AGT

	 ENDIF               ! AGT
	
1000	 CONTINUE

	 IF (TOTCNT .NE. 0) THEN 

           WRITE(REPLU,102) TOTCNT, CMONY(TOTAMT, 12, BETUNIT),
     *           CMONY(TOTKAMT, 12, BETUNIT),
     *           CMONY(TOTVAL, 12, BETUNIT)                        !v02

	 ENDIF
C
C OVER AND OUT
C
	WRITE (REPLU,9204) COUNT, CMONY (TOTCAN, 12, BETUNIT),GSNAMES(GNUM),
     *         CMONY (TOTJOK, 12, BETUNIT), CMONY (TOTVALAMT, 12, BETUNIT)
	CLOSE (UNIT=REPLU)
	CALL SPOOL (REPNAM, COPY, ST)
			
	RETURN

C2345#===================================================================

102     FORMAT(1X,131('-')/46X,'TOTAL', 18X, I4, 12X, A12,2X,A12,2X,A12/)
901     FORMAT(I7.7,I2.2,I4.4,I8.8,I2.2,I2.2,I10.10, I10.10)

9003	FORMAT(2X,A8,3X,I2.2,'-',I2.2,'-',I2.2,3X,I4,3X,I10,
     *         5X,I3.3,'-',I8.8,'-',I3.3,A2,4X,I3,'/',I4,4X,I4,3X,A10,4X,A10,4X,A10)
     
9004    FORMAT(13X,I2.2,'-',I2.2,'-',I2.2,3X,I4,3X,I10,
     *         5X,I3.3,'-',I8.8,'-',I3.3,A2,5X,I2,'/',I4,4X,I4,3X,A10,4X,A10,4X,A10)
     
9200	FORMAT('INVL', A4, '.REP')

9201	FORMAT('     CANCELADOS DO DRAW ',I6, '   CONCURSO ',I3.3,I4.4 )

9202	FORMAT(1X,131('-'),/,
     *         1X, 'REVENDEDOR', 4X, 'DATA', 6X, ' CDC', 4X, ' SERIAL ',
     *         6X,'    RECIBO      ',5X, 'CONCURSO',5X,
     *         ' QTDE ',5X,' VALOR ',7X,' JOKER',7X,'VALOR'/,
     *         69X,'INICIAL    CONCURSOS',29X,'BILHETE',/,
     *         1X,131('-'))

9204	FORMAT(1X,//,20X,I8,' CANCELADOS TOTALIZAM ',A12, ' EUROS EM APOSTAS DE ',A4,/,
     *               28X,'            TOTALIZAM ',A12, ' EUROS COM JOKER',/,
     *               28X,'            TOTALIZAM ',A12, ' EUROS')

	END
