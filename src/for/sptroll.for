C SPTROLL.FOR
C
C V03 09-JUN-2000 UXN GOTO 20 changed to GOTO 10.
C V02 20-MAR-2000 OXK Subroutine, not separate program, called from ROLPOL
C V01 09-MAR-2000 OXK Initial release
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
	SUBROUTINE SPTROLL(RDFREC2)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:RECRDF.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 RDFREC2(RDFLEN+RDFFRESIZ)

	INTEGER*4   FR_GNUM, FR_GIND, FR_DIV, FR_DRW
	INTEGER*4            TO_GIND, TO_DIV, TO_DRW
	INTEGER*4   ST

	INTEGER*4  CBUF(CDLEN)       !

	INTEGER*4   NUMDIV	! SHOULD BE SPGDIV, BUT 6 IS ENOUGH
	PARAMETER(NUMDIV=6)	! ...AND FITS TO THE SCREEN BETTER
C
C
	IF (DAYSTS.NE.DSOPEN) THEN
	   TYPE*,IAM(),'==============================================='
	   TYPE*,IAM(),' Vakio-rollpol can be run only when game is up '
	   TYPE*,IAM(),'==============================================='
	   CALL XWAIT(2,2,ST)
	   RETURN
	ENDIF
C
C
	TYPE*,IAM(),'======================================================='
	TYPE*,IAM(),' TRANSFER ROLLOVER MONEY BETWEEN VAKIO DRAWS & INDEXES '
	TYPE*,IAM(),'======================================================='

	CALL FASTMOV(RDFREC2,RDFREC,RDFLEN)

10	CONTINUE
	WRITE(6,900)
	DO 20, FR_GIND=1,NUMSPT
	    FR_GNUM=GTNTAB(TSPT,FR_GIND)
	    IF (FR_GNUM.LT.1 .OR. FR_GNUM.GT.MAXGAM) GOTO 20
	    WRITE(6,910)FR_GIND,RDFADW(FR_GNUM),
     *		   (CSMONY(RDF_SPTPOLDIV(FR_DIV,FR_GIND),10,BETUNIT),FR_DIV=1,6)
20	CONTINUE

	CALL PRMNUM('Enter game index to take money FROM:',FR_GIND,1,NUMSPT,ST)
	IF (ST.NE.0) GOTO 800
	FR_GNUM=GTNTAB(TSPT,FR_GIND)
	IF (FR_GNUM.LT.1 .OR. FR_GNUM.GT.MAXGAM) GOTO 10
	CALL PRMNUM('Enter division   to take money FROM:',FR_DIV ,1,NUMDIV,ST)
	IF (ST.NE.0) GOTO 10
C
	CALL PRMNUM('Enter game index to move money TO  :',TO_GIND,1,NUMSPT,ST)
	IF (ST.NE.0) GOTO 10
	CALL PRMNUM('Enter division   to move money TO  :',TO_DIV ,1,NUMDIV,ST)
	IF (ST.NE.0) GOTO 10
C
	IF (TO_GIND.EQ.1 .AND. TO_DIV.NE.1) THEN
	    WRITE(6,*)IAM(),'You are transferring to OLD Vakio; div',TO_DIV
	    CALL PRMYESNO('Is this correct [Y/N]?', ST)
	    IF (ST.NE.1) GOTO 10
	ENDIF
C
	FR_DRW = RDFADW(FR_GNUM)
	TO_DRW = SPTDRW(TO_GIND)

	WRITE(6,920)IAM(),CSMONY(RDF_SPTPOLDIV(FR_DIV,FR_GIND),10,BETUNIT),
     *		      FR_GIND,FR_DRW,FR_DIV, TO_GIND,TO_DRW,TO_DIV

	CALL PRMYESNO('Is this correct [Y/N]?', ST)
	IF (ST.NE.1) GOTO 10
C
C GENERATE THE COMMAND TO DO THE STUFF...
C
      	CALL FASTSET(0,CBUF,CDLEN)
      	CBUF( 1)=4
      	CBUF( 2)=RDF_SPTPOLDIV(FR_DIV,FR_GIND)
      	CBUF( 3)=TCSPT
      	CBUF( 6)='SYS '
      	CBUF( 8)=TO_GIND
      	CBUF( 9)=TO_DIV
      	CBUF(10)=FR_GIND
      	CBUF(11)=FR_DRW

      	CALL RESCMD(CBUF)

	RDF_SPTPOLDIV(FR_DIV,FR_GIND) = 0

	GOTO 10

800	CONTINUE
	CALL FASTMOV(RDFREC,RDFREC2,RDFLEN)
	RETURN
C
900	FORMAT(1X,'Gind Draw       Div1       Div2       Div3       Div4',
     *		  '       Div5       Div6')
910	FORMAT(1X,I4,1X,I4,6(1X,A10))
920	FORMAT(1X,A,'Transferring ',A10,' FROM',I2,'/',I4,'; div',I2,
     *                                    ' TO',I2,'/',I4,'; div',I2)
	END
