C V01 21-MAY-2001 ANG INITIAL RELEASE FOR PORTUGAL
C
C
C THIS PROGRAM SET ROLOVER TO ALL GAMES. THIS ROLOVER COME FROM SCML OFFLINE SYSTEM
C THIS PROGRAM SHOULD RUN, ONLY ONE TIME
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM SETPOOL
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
        INCLUDE 'INCLIB:DTGREC.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'


	CHARACTER*46 STR
        INTEGER*4 GTYP,GIND,GNUM

        INTEGER*4 DRW,WEEK,YEAR,ST,I,GLUN,MAXDIV
        INTEGER*4 OFF_POOLS(20),FLAG
        INTEGER*4 FDB(7),DIV

	TYPE*,IAM(),'>>          ATENCAO !!!'
	TYPE*,IAM(),'>> ESTE PROGRAMA ATUALIZA OS POOLS '
        TYPE*,IAM(),'>> DOS JOGOS COM OS DADOS DO OFFLINE'
	CALL GPAUSE()

	CALL INPNUM('Entre o numero do jogo: ',GNUM,1,MAXGAM,ST)
	IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)

	CALL INPNUM('Entre o concurso: ',DRW,1,99999,ST)
	IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)

	GTYP=GNTTAB(GAMTYP,GNUM)
	GIND=GNTTAB(GAMIDX,GNUM)

        IF (GIND.LE.0.OR.GIND.GT.MAXIND .OR.
     *      GTYP.LE.0.OR.GTYP.GT.MAXTYP) THEN
	      TYPE*,IAM(),'Invalid Game TYPE or Game INDEX ',GTYP, ' ',GIND
	      CALL GSTOP(GEXIT_FATAL)
	ENDIF

        CALL FIND_AVAILABLE_LUN(GLUN,ST)
        IF (ST.NE.0) THEN
            TYPE*,IAM(),'ERROR GETTING LOGICAL UNIT FOR GAME: ',GNUM
            CALL GSTOP(GEXIT_FATAL)
        ENDIF 
        CALL OPENW(GLUN,GFNAMES(1,GNUM),4,0,0,ST)

        IF (GTYP.EQ.TLTO) THEN
            CALL IOINIT(FDB,GLUN,DLTSEC*256)
        ELSEIF (GTYP.EQ.TSPT) THEN
            CALL IOINIT(FDB,GLUN,DSPSEC*256)
        ELSEIF (GTYP.EQ.TKIK) THEN
            CALL IOINIT(FDB,GLUN,DKKSEC*256)
        ELSEIF (GTYP.EQ.TTGL) THEN
            CALL IOINIT(FDB,GLUN,DTGSEC*256)
        ELSE
	    TYPE*,IAM(),'Invalid Game TYPE ',GTYP
	    CALL GSTOP(GEXIT_FATAL)
        ENDIF

        IF(ST.NE.0) THEN
           CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
	   CALL GSTOP(GEXIT_FATAL)
        ENDIF

        FLAG = 0
        CALL FASTSET(0,OFF_POOLS,20)

        IF (GTYP.EQ.TLTO) THEN
            CALL READW(FDB,DRW,DLTREC,ST)
            MAXDIV = DLTDIV
        ELSEIF (GTYP.EQ.TSPT) THEN
	    CALL READW(FDB,DRW,DSPREC,ST)
            MAXDIV = DSPDIV
        ELSEIF (GTYP.EQ.TKIK) THEN
	    CALL READW(FDB,DRW,DKKREC,ST)
            MAXDIV = DKKDIV
        ELSEIF (GTYP.EQ.TTGL) THEN
	    CALL READW(FDB,DRW,DTGREC,ST)
            MAXDIV = DTGDIV
        ENDIF

        CALL CLRSCR(6)
        CALL GETWEK(DRW,GNUM,WEEK,YEAR,ST)

        WRITE(6,10) IAM(),(GLNAMES(I,GNUM),I=1,4),DRW,YEAR,WEEK
        DO DIV=1,MAXDIV
	   WRITE(STR,9) DIV
	   CALL INPMONY(STR,OFF_POOLS(DIV),BETUNIT,ST)
	   IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
        ENDDO

        WRITE(6,11)
        DO DIV=1,MAXDIV
           WRITE(6,12) DIV,CMONY(OFF_POOLS(DIV),13,BETUNIT)
        ENDDO
	CALL INPYESNO('Esta correto ? [Y/N]',FLAG)
	IF (FLAG.EQ.3..OR.FLAG.EQ.2) CALL GSTOP(GEXIT_OPABORT)       !OPERADOR ABORTOU

        IF (GTYP.EQ.TLTO) THEN
            DO DIV=1,DLTDIV
               DLTPOL(DIV) = DLTPOL(DIV) + OFF_POOLS(DIV) 
            ENDDO
	    CALL WRITEW(FDB,DRW,DLTREC,ST)
        ELSEIF (GTYP.EQ.TSPT) THEN
            DO DIV=1,DSPDIV
               DSPPOL(DIV) = DSPPOL(DIV) + OFF_POOLS(DIV)
            ENDDO
            CALL WRITEW(FDB,DRW,DSPREC,ST)
        ELSEIF (GTYP.EQ.TKIK) THEN
            DO DIV=1,DKKDIV
               DKKPOL(1,DIV) = DKKPOL(1,DIV) + OFF_POOLS(DIV)
            ENDDO
            CALL WRITEW(FDB,DRW,DKKREC,ST)
        ELSEIF (GTYP.EQ.TTGL) THEN
            DO DIV=1,DTGDIV
               DTGPOL(DIV) = DTGPOL(DIV) + OFF_POOLS(DIV)
            ENDDO
            CALL WRITEW(FDB,DRW,DTGREC,ST)
        ENDIF

        IF(ST.NE.0) THEN
           CALL FILERR(GFNAMES(1,GNUM),2,ST,DRW)
        ENDIF

        CALL CLOSEFIL(FDB)
	CALL GSTOP(GEXIT_SUCCESS)

9	FORMAT('Entre com o valor a ser somado para divisao ',I2.2)
10      FORMAT(1X,A,'Jogo: ',4A4,' concurso interno: ',I4,1X,I4,'/',I2.2)
11      FORMAT(/,5X,'Divisao',8X,'Pool Off-line')
12      FORMAT(8X,I2.2,11X,A13)
        END

