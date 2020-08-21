C V01 02-MAY-2001 ANG  INITIAL RELEASE FOR PORTUGAL
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

C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM GET_OFF_WINNERS
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'

	INTEGER*4 GNUM,GTYP,GIND
	INTEGER*4 LOGLUN,ST

        CALL COPYRITX(6)	
	
        CALL CLRSCR(5)

	TYPE*,IAM(),'>>>>>>     A T E N C A O       <<<<<<'
	TYPE*,IAM(),' Este programa atualiza os ganhadores'
        TYPE*,IAM(),'off-line nos arquivos de jogos.      ' 
        TYPE*,IAM(),' Deseja continuar?                   ' 
	CALL GPAUSE()
C
C ABRE ARQUIVO DE LOG
C
	CALL FIND_AVAILABLE_LUN(LOGLUN,ST)
	IF (ST.NE.0) THEN
	    TYPE*,IAM(),'ERRO ALOCANDO LOGICAL UNIT PARA ARQUIVO DE LOG'
	    CALL GSTOP(GEXIT_FATAL)
	ENDIF

	OPEN(LOGLUN,
     *       FILE   = 'GETOFFWIN.LOG',
     *       STATUS = 'UNKNOWN',
     *       IOSTAT = ST)

	IF (ST.NE.0) THEN
	    TYPE*,IAM(),'ERRO ABRINDO ARQUIVO DE LOG STATUS ',ST
	    CALL GSTOP(GEXIT_FATAL)
	ENDIF


	DO GNUM=1,MAXGAM
	   GTYP=GNTTAB(GAMTYP,GNUM)
           GIND=GNTTAB(GAMIDX,GNUM)

	   CALL UPD_WIN(GNUM,GIND,GTYP,LOGLUN)

	ENDDO

	CLOSE (LOGLUN)	
	CALL GSTOP(GEXIT_SUCCESS)
	END

C*****************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE UPD_WIN(GNUM,GIND,GTYP,LOGLUN)
        IMPLICIT NONE
C*****************************************

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
        INCLUDE 'INCLIB:DTGREC.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'

	INTEGER*4 GTYP,GIND,GNUM,LOGLUN

	INTEGER*4 DRW,WEEK,YEAR,ST,I,GLUN,MAXDIV
	INTEGER*4 WINNERS(20),GSTAT,FLAG,ON_WINNERS(20)
	INTEGER*4 FDB(7),DIV

	INTEGER*4 GET_DRW   !FUNCTION

	IF (GIND.LE.0.OR.GIND.GT.MAXIND .OR.
     *      GTYP.LE.0.OR.GTYP.GT.MAXTYP) RETURN

	IF (DAYDRW(GNUM).LE.0) RETURN

	DRW = DAYDRW(GNUM) - 1
	IF (DRW.LE.0) DRW = DAYDRW(GNUM)

	CALL FIND_AVAILABLE_LUN(GLUN,ST)
	IF (ST.NE.0) THEN
	    TYPE*,IAM(),'ERROR GETTING LOGICAL UNIT FOR GAME: ',GNUM
	    CALL GPAUSE()
	    RETURN
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
            RETURN
        ENDIF

        IF(ST.NE.0) THEN
           CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
           RETURN
        ENDIF

	FLAG = 0
	DO WHILE(FLAG.NE.1)
	   CALL FASTSET(0,ON_WINNERS,20)
	   CALL FASTSET(0,WINNERS,20)

           IF (GTYP.EQ.TLTO) THEN
               CALL READW(FDB,DRW,DLTREC,ST)
	       MAXDIV = DLTDIV
	       GSTAT  = DLTSTS
	       DO DIV=1,DLTDIV
		  ON_WINNERS(DIV) = DLTSHR(DIV,1)
	       ENDDO
           ELSEIF (GTYP.EQ.TSPT) THEN
               CALL READW(FDB,DRW,DSPREC,ST)
	       MAXDIV = DSPDIV
	       GSTAT  = DSPSTS  
	       DO DIV=1,DSPDIV
		  ON_WINNERS(DIV) = DSPSHR(DIV)
	       ENDDO
           ELSEIF (GTYP.EQ.TKIK) THEN
               CALL READW(FDB,DRW,DKKREC,ST)
	       MAXDIV = DKKDIV
	       GSTAT  = DKKSTS  
	       DO DIV=1,DKKDIV
		  ON_WINNERS(DIV) = DKKSHR(DIV)
	       ENDDO
           ELSEIF (GTYP.EQ.TTGL) THEN
               CALL READW(FDB,DRW,DTGREC,ST)
	       MAXDIV = DTGDIV
	       GSTAT  = DTGSTS  
	       DO DIV=1,DTGDIV
		  ON_WINNERS(DIV) = DTGSHR(DIV)
	       ENDDO
           ENDIF

           IF(ST.NE.0) THEN
             CALL FILERR(GFNAMES(1,GNUM),2,ST,DRW)
             CALL CLOSEFIL(FDB)
             RETURN
           ENDIF

	   CALL CLRSCR(6)
	   CALL GETWEK(DRW,GNUM,WEEK,YEAR,ST)
	   WRITE(6,10) IAM(),(GLNAMES(I,GNUM),I=1,4),DRW,YEAR,WEEK

	   IF (GSTAT.NE.GAMENV) THEN 
	      TYPE*,IAM(),'STATUS DO CONCURSO INVALIDO PARA A ATUALIZACAO'
	      DRW = GET_DRW()
              IF (DRW.EQ.0)THEN
                 CALL CLOSEFIL(FDB)
                 RETURN                        !NAO ATUALIZA ESTE JOGO 
              ENDIF
           ELSE
	      CALL INPYESNO('Esta correto ? [Y/N]',FLAG)
	      IF (FLAG.EQ.3) CALL GSTOP(GEXIT_OPABORT)       !OPERADOR ABORTOU
	      IF (FLAG.EQ.2) THEN
	         DRW = GET_DRW()
		 IF (DRW.EQ.0) THEN
		    CALL CLOSEFIL(FDB)
                    RETURN
                 ENDIF
	      ENDIF
	      IF (FLAG.EQ.1) THEN
		 DO WHILE (.TRUE.)
		    CALL GET_WIN(MAXDIV,WINNERS)
	            CALL INPYESNO('Atualizar arquivo de jogo? [Y/N]',FLAG)
	            IF (FLAG.EQ.3) CALL GSTOP(GEXIT_OPABORT)       !OPERADOR ABORTOU
	            IF (FLAG.EQ.1) EXIT
	         ENDDO
C
C FINALMENTE ATUALIZA GAME FILE
C
	         TYPE*,IAM(),'Atualizando arquivo com vencedores off-line'	         
		 CALL WRT_LOG(LOGLUN,GNUM,MAXDIV,WINNERS,ON_WINNERS,DRW,WEEK,YEAR)

                 IF (GTYP.EQ.TLTO) THEN
                    DO DIV=1,DLTDIV
                       DLTSHR(DIV,1) = DLTSHR(DIV,1) + WINNERS(DIV) 
                    ENDDO
                    CALL WRITEW(FDB,DRW,DLTREC,ST)
                 ELSEIF (GTYP.EQ.TSPT) THEN
                    DO DIV=1,DSPDIV
                       DSPOSH(DIV) =  DSPOSH(DIV) + WINNERS(DIV)
                    ENDDO
                    CALL WRITEW(FDB,DRW,DSPREC,ST)
                 ELSEIF (GTYP.EQ.TKIK) THEN
                    DO DIV=1,DKKDIV
                       DKKSHR(DIV) = DKKSHR(DIV) + WINNERS(DIV)
                    ENDDO
                    CALL WRITEW(FDB,DRW,DKKREC,ST)
                 ELSEIF (GTYP.EQ.TTGL) THEN
                    DO DIV=1,DTGDIV
                       DTGOSH(DIV) = DTGOSH(DIV) + WINNERS(DIV)
                    ENDDO
                    CALL WRITEW(FDB,DRW,DTGREC,ST)
                 ENDIF

                 IF(ST.NE.0) THEN
                   CALL FILERR(GFNAMES(1,GNUM),2,ST,DRW)
                   CALL CLOSEFIL(FDB)
                   RETURN
                 ENDIF

	      ENDIF 
	   ENDIF
	   
	ENDDO

	CALL CLOSEFIL(FDB)
	RETURN
10	FORMAT(1X,A,'Jogo: ',4A4,' concurso interno: ',I4,1X,I4,'/',I2.2)
	END

C*****************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        INTEGER*4 FUNCTION GET_DRW()
        IMPLICIT NONE
C*****************************************

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INTEGER*4 DRW,ST,OPT

	GET_DRW = 0

	TYPE*,IAM()
	TYPE*,IAM(),' 1 - Mudar o concurso'
	TYPE*,IAM(),' 2 - Nao alterar este jogo'
	TYPE*,IAM()
	CALL INPNUM('Entre com a opcao: ',OPT,1,2,ST)

	IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
	IF (OPT.EQ.1) THEN
           CALL INPNUM('Entre com o concurso: ',DRW,1,99999,ST)
	   GET_DRW = DRW
           IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
	ELSE
	   GET_DRW = 0
	ENDIF

        RETURN  
	END


C*****************************************
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GET_WIN(MAXDIV,WINNERS)
        IMPLICIT NONE
C*****************************************

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'

	INTEGER*4    MAXDIV,WINNERS(20)
	INTEGER*4    QTD,AUXQTD,ST,DIV
	CHARACTER*50 STR

	DO DIV=1,MAXDIV
	    DO WHILE(.TRUE.)
	       WRITE(STR,10) DIV
	       CALL INPNUM(STR,QTD,0,999999,ST) 
	       IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)

	       CALL INPNUM('Digite novamente para confirmar ',AUXQTD,0,999999,ST) 
	       IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)

	       IF (AUXQTD.EQ.QTD) THEN
                  EXIT		
	       ELSE
		  TYPE*,IAM(),'Valor invalido. Digite novamente.'
		  CALL XWAIT(3,2,ST)
		  TYPE*,IAM()
	       ENDIF
	    ENDDO

	    WINNERS(DIV) = QTD
	ENDDO	
	
	WRITE(6,11)
	DO DIV=1,MAXDIV
	    WRITE(6,12) DIV,WINNERS(DIV)
	ENDDO

	RETURN
10	FORMAT(' Digite a quantidade de ganhadores para divisao ',I2.2)
11	FORMAT(/,5X,'Divisao',8X,'Ganhadores Off-line')
12	FORMAT(8X,I2.2,11X,I8)
	END


C*****************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE WRT_LOG(LOGLUN,GNUM,MAXDIV,WINNERS,ON_WINNERS,DRW,WEEK,YEAR)
        IMPLICIT NONE
C*****************************************

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'

	INTEGER*4 LOGLUN,GNUM,MAXDIV,WINNERS(20),ON_WINNERS(20),DRW,WEEK,YEAR
	INTEGER*4 PAGE,DIV,I

	LOGICAL FIRST/.TRUE./

	IF (FIRST) THEN
	    FIRST = .FALSE.
            CALL TITLE('ATUALIZACAO DE GANHADORES OFF-LINE','GET_OFF_WIN',1,LOGLUN,PAGE,DAYCDC)
	ENDIF

	WRITE(LOGLUN,10) (GLNAMES(I,GNUM),I=1,4),DRW,YEAR,WEEK

	DO DIV=1,MAXDIV
	    WRITE(LOGLUN,11) DIV,ON_WINNERS(DIV),WINNERS(DIV),ON_WINNERS(DIV)+WINNERS(DIV)
	ENDDO

	WRITE(LOGLUN,12)

	RETURN
10	FORMAT(/,1X,'Jogo: ',4A4,2X,'Concurso interno: ',I4,2X,'Ano/semana: ',I4,'/',I2.2,//
     *         '                   Ganhadores          Ganhadores',/
     *         '   Divisao          On-line             Off-line          Total')
11	FORMAT(5X,I2.2,12X,I8,12X,I8,8X,I8)
12	FORMAT(80('='))
	END
