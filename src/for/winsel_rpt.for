C WINSEL_RPT.FOR
C
C V02 07-MAR-2011 FJG  Sorteio to CCC/YY
C V01 17-MAY-2001 ANG  INITIAL RELEASE FOR PORTUGAL
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

C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE WINSEL_RPT()
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'

	INTEGER*4 ST,GNUM,GTYP,GIND,DRW,REPLUN

	CHARACTER*12 REPNAM/'PREVISAO.REP'/

	CALL FIND_AVAILABLE_LUN(REPLUN,ST)

	IF (ST.NE.0) THEN
           TYPE*,IAM(),'>>> ERROR GETTING LOGICAL UNIT FOR REPORT: ',REPNAM
	   CALL GPAUSE()
	   RETURN
	ENDIF

	OPEN(REPLUN,
     *       FILE   = REPNAM,
     *       STATUS = 'NEW',
     *       IOSTAT = ST)

	IF (ST.NE.0) THEN
	    TYPE*,IAM(),'>>> ERROR OPPENING REPORT: ',REPNAM,' ST = ',ST
	    CALL GPAUSE()
	    RETURN
	ENDIF

	DO GNUM=1,MAXGAM
           GTYP = GNTTAB(GAMTYP,GNUM)
           GIND = GNTTAB(GAMIDX,GNUM)
	   IF (GIND.GT.0.AND.GIND.LE.MAXIND) THEN
	      IF (GTYP.EQ.TLTO) DRW = LLTDRW(GIND)
	      IF (GTYP.EQ.TSPT) DRW = LSPDRW(GIND)
	      IF (GTYP.EQ.TTGL) DRW = LTGDRW(GIND)
	      IF (GTYP.EQ.TKIK) DRW = LKKDRW(GIND)

              IF (DRW.GT.0) CALL WRT_RPT(REPLUN,GTYP,GNUM,GIND,DRW)

	   ENDIF
	ENDDO	

	CLOSE(REPLUN)

	RETURN
	END

C********************************************************
C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE WRT_RPT(REPLUN,GTYP,GNUM,GIND,DRW)
        IMPLICIT NONE
C********************************************************

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'

	INTEGER*4 REPLUN,GTYP,GNUM,GIND,DRW
	INTEGER*4 WEEK,YEAR,ST,I
	INTEGER*4 NUMWIN,NUMDIV,DIV
	INTEGER*4 LINCNT/0/,PAGE
	
	IF (LINCNT.GT.42.OR.LINCNT.EQ.0) THEN
	    CALL TITLE('RELATORIO DE PREMIOS ONLINE','PREVISAO',1,REPLUN,PAGE,DAYCDC)
	    WRITE(REPLUN,10) 
	    LINCNT = 4
	ENDIF

	IF (GTYP.EQ.TLTO)  THEN
	    NUMDIV = LLTDIV(GIND)
        ELSEIF (GTYP.EQ.TSPT) THEN
	    NUMDIV = LSPDIV(GIND)
	ELSEIF (GTYP.EQ.TTGL) THEN
	    NUMDIV = LTTDIV(GIND)
	ELSEIF (GTYP.EQ.TKIK) THEN
	    NUMDIV = LKKDIV(GIND)
	ELSE
	    RETURN
	ENDIF

	CALL GETWEK(DRW,GNUM,WEEK,YEAR,ST)
	WRITE(REPLUN,11) (GLNAMES(I,GNUM),I=1,4),WEEK,YEAR	
	LINCNT = LINCNT + 6

	DO DIV=1,NUMDIV
	   NUMWIN = 0
           IF (GTYP.EQ.TLTO)  THEN
               NUMWIN = LLTSHR(DIV,1,GIND)
           ELSEIF (GTYP.EQ.TSPT) THEN
               NUMWIN = LSPSHR(DIV,GIND)
           ELSEIF (GTYP.EQ.TTGL) THEN
               NUMWIN = LTGSHR(DIV,GIND)
           ELSEIF (GTYP.EQ.TKIK) THEN
               NUMWIN = LKKSHR(DIV,GIND)
	   ENDIF

	   WRITE(REPLUN,12) DIV,NUMWIN
	   LINCNT = LINCNT + 1
	ENDDO

	RETURN
10	FORMAT(132('='))
11	FORMAT(//,T40,'Jogo: ',4A4,T63,'Sorteio: ',I3,'/',I4,//,
     *         T37,'CLASSE DE PREMIO',T59,'QUANTIDADE DE GANHADORES',/
     *         T35,'-------------------',T58,'--------------------------')
12	FORMAT(T44,I2,T65,I8)
	END
