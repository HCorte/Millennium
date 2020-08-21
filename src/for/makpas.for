	PROGRAM MAKPAS
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 OPT
	CHARACTER*12 USRPASS
        CHARACTER*1  CRLN
C
        DATA CRLN /'0D'X/
C
        CALL CLRSCR(5)
        TYPE *, ' '
        TYPE *, '* * * * * * * * * * * * * * * * * * * * * * *' 
        TYPE *, '* Makpas Task Needs Password Autoritation.  *'
        TYPE *, '* * * * * * * * * * * * * * * * * * * * * * *' 
        TYPE *, ' '
        CALL PASSWORD(12, USRPASS)
        IF (USRPASS .NE. 'MAKPASBARNA'//CRLN) THEN
            TYPE *, ' '
	    TYPE *, '* * * INVALID PASSWORD ENTERED * * *'
            TYPE *, ' '
            CALL GSTOP(GEXIT_SUCCESS)
        ENDIF

	DO WHILE (.TRUE.)
	   TYPE*,' 1 - ARQUIVO DE TICKETS'
	   TYPE*,' 2 - ARQUIVO DE VENDA DE TICKETS'
	   TYPE*,' 3 - SAIR'

	   WRITE(6,FMT='(A28,$)') ' INFORME A OPCAO        :   '
	   READ(6,FMT='(I1)') OPT

	   IF (OPT.EQ.1) THEN
              CALL MAKTCK()
	   ELSEIF (OPT.EQ.2) THEN
              CALL MAKTCKSAL()
	   ELSE
	      STOP 'FIM DE EXECUCAO'
	   ENDIF
	ENDDO
	END


	SUBROUTINE MAKTCK()

	INTEGER*4 NUMTCK, NUMFRAC,ST
	INTEGER*4 FRA,TIC,ALG
	CHARACTER*20 FILNAM

	WRITE(6,FMT='(A28,$)') ' INFORME O ULTIMO TICKET:   '
	READ(6,FMT='(I5)') NUMTCK

	WRITE(6,FMT='(A28,$)') ' INFORME QUANTAS FRACOES:   '
	READ(6,FMT='(I2)') NUMFRAC
	
	WRITE(6,FMT='(A28,$)') ' INFORME O NOME DO ARQUIVO: '
	READ(6,FMT='(A15)') FILNAM
	
	OPEN (
     *	       UNIT=35,
     *	       ACCESS = 'SEQUENTIAL',       
     *         ORGANIZATION = 'SEQUENTIAL',
     *	       FILE = FILNAM,  
     *         RECL = 14,
     *	       FORM = 'FORMATTED',
     *         STATUS = 'NEW',
     *	       RECORDTYPE   = 'VARIABLE',
     *	       IOSTAT = ST)

	IF (ST.NE.0) THEN
            TYPE*,' ERRO ABRINDO ARQUIVO, ST ',ST
	    STOP
	ENDIF

	ALG = 12345
	DO TIC=0,NUMTCK
	    DO FRA=0,NUMFRAC-1
		WRITE(35,FMT='(I5.5,A1,I2.2,A1,I5.5)') TIC,' ',FRA,' ',ALG
	    ENDDO
	ENDDO

	CLOSE (35)
	RETURN
	END



	SUBROUTINE MAKTCKSAL()

	INTEGER*4 ST,CNTREC,EXTR
	INTEGER*4 TPPAS,EMIS,INITCK,FINTCK
	CHARACTER*1 CONT
	CHARACTER*15 FILNAM

	FILNAM = 'ORCPAS.ASC'
	CNTREC = 0
	
	OPEN (
     *	       UNIT=35,
     *	       ACCESS = 'SEQUENTIAL',       
     *         ORGANIZATION = 'SEQUENTIAL',
     *	       FILE = FILNAM,  
     *         RECL = 20,
     *	       FORM = 'FORMATTED',
     *         STATUS = 'NEW',
     *	       RECORDTYPE   = 'FIXED',
     *	       IOSTAT = ST)

	IF (ST.NE.0) THEN
            TYPE*,' ERRO ABRINDO ARQUIVO, ST ',ST
	    STOP
	ENDIF

	WRITE(35,100) 'HP',19760919,' '
	CNTREC = CNTREC + 1

	CONT = 'N'
	DO WHILE(CONT.NE.'S'.AND.CONT.NE.'s')

	   IF (CONT.EQ.'N') THEN
	      WRITE(6,FMT='(A31,$)') ' INFORME A EMISSAO           : '
	      READ(6,FMT='(I2)') EMIS

	      WRITE(6,FMT='(A31,$)') ' INFORME A EXTRACAO(SSAAAA)  : '
	      READ(6,FMT='(I6)') EXTR

	      WRITE(6,FMT='(A31,$)') ' INFORME(0-CLA,2-NATAL,5-POP): '
	      READ(6,FMT='(I2)') TPPAS
	   ENDIF

	   WRITE(6,FMT='(A31,$)') ' INFORME O PRIMEIRO TICKET   : '
	   READ(6,FMT='(I5)') INITCK

	   WRITE(6,FMT='(A31,$)') ' INFORME O ULTIMO TICKET     : '
	   READ(6,FMT='(I5)') FINTCK

	   WRITE(35,110) '01',TPPAS,EXTR,EMIS,INITCK,FINTCK
	   CNTREC = CNTREC + 1
	        
	   WRITE(6,FMT='(A31)') ' <N> PARA NOVA EXTRACAO '
	   WRITE(6,FMT='(A31)') ' <C> PARA MESMA EXTRACAO'
	   WRITE(6,FMT='(A31)') ' <S> PARA SAIR          '
	   WRITE(6,FMT='(A31,$)') ' ENTRE UMA OPCAO: '
	   READ(6,FMT='(A1)') CONT
	ENDDO	   

	CNTREC = CNTREC + 1
	WRITE(35,120) 'TP',CNTREC,' '

	CLOSE (35)
	RETURN

100	FORMAT(A2,I8,10(A1))
110	FORMAT(A2,I1,I6,I1,I5.5,I5.5)
120	FORMAT(A2,I8.8,10(A1))

	END

