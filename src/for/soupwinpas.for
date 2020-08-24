CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SOUPWINPAS.FOR
C CHANGE LOG:
C ---+-------------+----+----------------------------------------------
C VER|DATE         |USER|DESCRIPTION       
C ---+-------------+----+----------------------------------------------
C V01 26-JAN-2014   SCML  INITIAL RELEASE OF SOUPWINPAS SUBROUTINE 
C
C
C THIS SUBROUTINE PROCESS SETTLED PRIZES FROM SCML VPF FILE AND 
C GENERATE OUTPUT FILE TO SOUP PLATFORM. THIS IS A SUBROUTINE OF 
C THE PROGRAM SOUPWIN (THAT PROCESS THE SCML VLF FILE).
C
C    INPUT:  VPFPIDDII.FIL
C       ( PI=PASSIVE INDICE(01 or 02) / DDII=DRAW ID )
C
C    OUTPUT: SOUP_LN_PA_GGEEAAAA.ASC
C       ( GG=GAME(08 or 09) / EE=EXTRACÇÃO / AAAA=ANO )
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Copyright 2003 SCML-DJ. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE SOUPWINPAS (GNUM,ST)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:STANDARD.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:HSHCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:VALPASFIL.DEF'
        INCLUDE 'INCLIB:TAXCONFIG.DEF'
        INCLUDE 'INCLIB:M_ISLN_PJMC.DEF'
        INCLUDE 'INCLIB:DPAREC.DEF'
        INCLUDE '(LIB$ROUTINES)'                ! LIB FOR COMMAND LINE
        INCLUDE 'INCLIB:PASIOSUBS.DEF'

C PARAMETERS
C************
        INTEGER*4     TUBSIZ
        PARAMETER (TUBSIZ=I4BUCSIZ*7)
        INTEGER*4     VPFBUF(TUBSIZ)

        CHARACTER*20   CFILNAM
        CHARACTER*29   PFILENAM
        INTEGER*4      IFILNAM(5)
        INTEGER*4      VLUN
        INTEGER*4      PUNIT
        CHARACTER*6    EEAAAA,EXTRACCAOANO
        CHARACTER*1    TIPO
        INTEGER*4      EXTRACCAO, ANO
        INTEGER*4      EMIOFF,INDEMIS
        INTEGER*4      IPAS
        INTEGER*4      GNUM, DRAW
        INTEGER*4      GETDRW              !FUNCTION
        INTEGER*4      FLUN, GAMSEC, I
        INTEGER*4      YEAR1,MONTH1,DAY1
        INTEGER*4      YEAR2,MONTH2,DAY2
        INTEGER*4      YEAR3,MONTH3,DAY3
        CHARACTER*2    EGNUM
        INTEGER*4      PAS_ROUND_VALUE
        INTEGER*4      POPWSER     ! WINNING SERIE (POPULAR ONLY)
        CHARACTER*256  LIBCMD      ! DIR GEN FILES COMMAND
        INTEGER*4      STO, ST,STATUS, SZ
        INTEGER*4      YESNO
        INTEGER*4      SERIE
        INTEGER*4      SDAT(8) ! SYSDATE FROM MACHINE
        INTEGER*2      DAT(12) ! GENERIC DATE STRUCTURE FOR ALL OTHER DATES
        CHARACTER*2    TPREC
        INTEGER*4      CNTREC, CTIM(2)
        INTEGER*4      RECNUM, NUMESHR
        INTEGER*4      CNT_PRM, MGDIVNUM
        CHARACTER*50   C50DIV

        EQUIVALENCE (IFILNAM,CFILNAM)
        RECORD /STPASFDB/  PASFDB
        RECORD /STPASREC/  PASREC

        CHARACTER*11   CKEY
        INTEGER*4      CVCXL, CUCSH, CPPAY, CCASH, CBANK
        LOGICAL*4      ERRORFLAG/.FALSE./   ! ERROR FLAG CONTROL

        STO = 0
        RECNUM = 0

C      CHOOSE DRAW  
       CALL WIMG(5,'DESEJA PROCESSAR QUAL EXTRACCAO (EEAAAA) : ')
       READ(5,901) EXTRACCAOANO

       IF (EXTRACCAOANO .EQ. 'e' .OR. EXTRACCAOANO .EQ. 'E' 
     *                           .OR. EXTRACCAOANO .EQ. '  ') RETURN

       ANO = MOD(CTOI(EXTRACCAOANO,SZ),10000)
       EXTRACCAO =INT(CTOI(EXTRACCAOANO,SZ)/10000) 

       IF(ANO.LT.2000 .OR. EXTRACCAO.GT.53 .OR. EXTRACCAO.LE.0) THEN
          TYPE*,IAM(),'SOUPWIN-Ano/Extraccao Invalidos'
          ST=-1
          RETURN
       ENDIF

       WRITE(EEAAAA,FMT='(I2.2,I4.4)') EXTRACCAO,ANO
       CALL PRMYESNO('Confirma a EXTRACCAO '//EEAAAA//',  (Y/N) ? ', YESNO)
       IF (YESNO.NE.1) THEN
          ST=-1
          RETURN
       ENDIF

       IF (GNUM .EQ. 8)THEN
          TIPO = 'C'
          IPAS=1
          EGNUM='08'
       ELSEIF (GNUM .EQ. 9) THEN
          TIPO = 'P'
          IPAS=2
          EGNUM='09'
       ENDIF

C PROGRAM HEADER -START
       TYPE*,IAM(),' '
       TYPE*,IAM(),'***********************************************************'
       TYPE*,IAM(),'                      SOUPWIN STARTED                      '
       TYPE*,IAM(),'***********************************************************'


C ESCREVE NOME FICHEIROS
************************
       WRITE (PFILENAM, FMT='(A16,I2.2,A6,A4)')
     *     'FILE:SOUP_LN_PA_',GNUM,EEAAAA,'.ASC'


C     TRY TO DELETE INTERFACE FILE FIRST
C******************************
       CALL DFILX(PFILENAM,0,0,ST)
       IF (ST.NE.0) CALL GSTOP (GEXIT_FATAL)


C VAI BUSCAR O Nº DRAW
C*********************
       DRAW = GETDRW(ANO,EXTRACCAO,GNUM)


C READ GAME FILE
Cthis subroutine open, read and close the file
C************************************************
       CALL FIND_AVAILABLE_LUN (FLUN,ST)
       IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'SOUPWIN - Erro a obter uma LUN para o ficheiro:'
          TYPE*, IAM(), '       '
          TYPE*, IAM(), '       '
          CALL GSTOP (GEXIT_FATAL)
       ENDIF

       CALL READGFL(FLUN,GFNAMES(1,GNUM),DPASEC,DRAW,DPAREC)


C VERIFICA SE ESTÁ EM MEMÓRIA
C***************************+
       INDEMIS = -1
       DO EMIOFF=1,PAGEMI
          IF (PASEMIS(EMIOFF,IPAS).EQ.DRAW) THEN
             INDEMIS = EMIOFF
             EXIT
          ENDIF
       ENDDO
       
       IF (INDEMIS.LE.0) THEN
          TYPE*, IAM(),'SOUPWIN-Extraccao nao esta em memoria'
          ST=-1
          RETURN
       ENDIF

       IF (DRAW.LE.0) THEN
          TYPE*,IAM(), 'SOUPWIN-Nº do DRAW invalido'
          ST=-100
          RETURN
       ENDIF


C GET FROM MEM THE SERIE SORTEADA
C*********************************
       IF (GNUM .EQ. 9)THEN 
          POPWSER = PASWSER(INDEMIS,IPAS)
          IF (POPWSER .EQ. 0) THEN
            TYPE*, IAM(),'       '
            TYPE*, IAM(),'SOUPWIN - Serie sorteada igual a zero!'
            TYPE*, IAM(),'       '
            CALL GSTOP(GEXIT_FATAL)
          ENDIF
       ENDIF


C VERIFICA STATUS
C****************
       IF(PASSTS(INDEMIS,IPAS).NE.GFINAL) THEN
          TYPE*,IAM(),'SOUPWIN-Resultados deverão estar finalizados '
          TYPE*,IAM(),'   '
          ST=-1
          RETURN
       ENDIF

C Count Extra Share Categories
          NUMESHR=0
          I=1
          DO WHILE (PAS_ROUND_VALUE(DPAEXSHV(I)).NE.0) 
             NUMESHR=NUMESHR+1
             I=I+1
          ENDDO

C DISPLAY SHARES VALUES
       MGDIVNUM = DPADIV
       IF(GNUM .EQ. 9) THEN
          TYPE*,IAM(),' '
          TYPE*,IAM(),'A obter o valor das shares da extracao' //' '//EEAAAA
          TYPE*,IAM(),'da LOTARIA POPULAR.'
          TYPE*,IAM(),''
          WRITE(C50DIV,'(A,I2,A)') 'Valores das shares para a serie sorteada(',POPWSER,' )'
          TYPE*,IAM(),TRIM(C50DIV)
          I=1
          DO 222 I=1,MGDIVNUM
             WRITE(C50DIV,'(A,I2,A)') ' Divisao ',I,': '
             TYPE*,IAM(),TRIM(C50DIV), CMONY(PAS_ROUND_VALUE(DPASHV(I)),13,VALUNIT)
222       CONTINUE
          TYPE*,IAM(),''
          TYPE*,IAM(),'Valores das shares para as series nao sorteadas.'
          I=1
          DO 223 I=1,MGDIVNUM
             IF(I .GT. NUMESHR) THEN
                WRITE(C50DIV,'(A,I2,A)') ' Divisao ',I,': '
                TYPE*,IAM(),TRIM(C50DIV), CMONY(PAS_ROUND_VALUE(DPASHV(I)),13,VALUNIT)
             ELSE
                WRITE(C50DIV,'(A,I2,A)') ' Divisao ',I,': '
                TYPE*,IAM(),TRIM(C50DIV), CMONY(PAS_ROUND_VALUE(DPAEXSHV(I)),13,VALUNIT)
             ENDIF
223       CONTINUE
       ELSE
          TYPE*,IAM(),' '
          TYPE*,IAM(),'A obter o valor das shares da extracao' //' '//EEAAAA
          TYPE*,IAM(),'da LOTARIA CLASSICA.'
          I=1
          DO 224 I=1,MGDIVNUM
             WRITE(C50DIV,'(A,I2,A)') ' Divisao ',I,': '
             TYPE*,IAM(),TRIM(C50DIV),CMONY(PAS_ROUND_VALUE(DPASHV(I)),13,VALUNIT)
224       CONTINUE
       ENDIF


C OPEN AND READ TPF FILE
C***********************  
C=======================================================================
C       OPEN TPF FILE
C=======================================================================
C
          CALL PASIO_INIT(PASFDB,IPAS,PASEMIS(INDEMIS,IPAS),
     *                    PASNUMTCK(INDEMIS,IPAS)-1,PASNUMSER(INDEMIS,IPAS),
     *                    PASNOFFRA(INDEMIS,IPAS),CPASTPFFIL(INDEMIS,IPAS))
          CALL PASIO_OPENRO(PASFDB)
          TYPE*,IAM(),' '
          TYPE*,IAM(),'***********************************************************'
          TYPE*,IAM(),'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *'
          TYPE*,IAM(),' '
          TYPE*,IAM(),'   A processar o ficheiro '//PASFDB.FILNAM//' ...'
          TYPE*,IAM(),' '
          
          IF(PASFDB.ERR .NE. IOE_NOERR) THEN
            TYPE*, IAM(), '       '
            TYPE*, IAM(), 'SOUPWIN - Erro: ', PASFDB.ERR, ' a abrir o ficheiro: ', PASFDB.FILNAM
            TYPE*, IAM(), '       '
            TYPE*, IAM(), 'DUMP:'
            CALL PASIO_DUMP(PASFDB)
            CALL GSTOP(GEXIT_FATAL)
          ENDIF


C ABRE FICHEIRO DE INTERFACE SOUP_PA
C************************************  
       CALL OPEN_FILASC (PFILENAM,PUNIT,ST)
       IF (ST.NE.0) THEN
          TYPE*,IAM(),'SOUPWIN-Erro a abrir o ficheiro '//PFILENAM
          TYPE*, IAM(),'   '
          ST=-1
          RETURN
       ENDIF


C WRITE HEADER
C******************
       TPREC='HP'

C MIGRATION OF THE GENERATION DATE - MEMORIE FREE SOLUTION - START
        CALL ICLOCK(1,CTIM)
        CALL GDATE(SDAT(2),SDAT(3),SDAT(1))
        IF(SDAT(1).LT.77) THEN
          SDAT(1) = SDAT(1) + 2000
        ELSE
          SDAT(1) = SDAT(1) + 1900
        ENDIF
C MIGRATION OF THE GENERATION DATE - MEMORIE FREE SOLUTION - END

C    Calculate draw date
       DAT(VCDC) = DPAESD
       CALL CDATE(DAT)
       YEAR2=2000+DAT(VYEAR)
       MONTH2=DAT(VMON)
       DAY2=DAT(VDAY)
C    Calculate sales start
       DAT(VCDC) = DPABSD
       CALL CDATE(DAT)
       YEAR3=2000+DAT(VYEAR)
       MONTH3=DAT(VMON)
       DAY3=DAT(VDAY)
C    Sales end is equal to draw date

       WRITE(PUNIT,910) SDAT(1),SDAT(2),SDAT(3),YEAR2,MONTH2,DAY2,
     *                  YEAR3,MONTH3,DAY3,YEAR2,MONTH2,DAY2
       CNTREC = 1


C OPEN VPF FILE
C*************
       WRITE(CFILNAM,FMT='(A8,I2.2,I4.4,A4)')
     *    'VALX:VPF',IPAS,DRAW,'.FIL'

       CALL FIND_AVAILABLE_LUN(VLUN,ST)
       IF (ST.NE.0) THEN
          CALL FILERR(IFILNAM,0,ST,0)
          TYPE*, IAM(),'   '
          ST=-1
          RETURN
       ENDIF

       CALL IOPEN(IFILNAM,VLUN,VPFLEN*2,VFSCDC,VFSSER*2-1,ST)
       IF(ST.NE.0) THEN 
           CALL FILERR(IFILNAM,1,ST,0)
       ENDIF

       TYPE*,IAM(),'   A processar o ficheiro '//CFILNAM//' ...'
       TYPE*,IAM(),' '

       CALL FASTSET(0, V4BUF_PAS, VPFLEN * VPFMAXREC)


C SET STATISTICS COUNTERS
       CPPAY = 0
       CUCSH = 0
       CVCXL = 0
       CCASH = 0
       CBANK = 0
       CNT_PRM=0

       DO WHILE (STO.NE.EOF)
          IF (STO.NE.EOF) THEN
C  READ FILE
C ************
             CALL ISREAD(V4BUF_PAS, VLUN, VPFBUF, STATUS)
             IF (STATUS.EQ.ERREND) THEN
               STO=EOF
               GOTO 301
             ENDIF
             IF (STATUS.EQ.0) THEN
                RECNUM=1 + RECNUM
                CALL LOGPAS(VALREC,V4BUF_PAS)
                CALL DLOGPAS(VALREC,VDETAIL)  
C CHECK IF STATUS ARE PPAY, UCSH,VCXL CASH OR BANK
                IF (VALREC(VSTAT).EQ.VPRPAY.OR.
     *                  VALREC(VSTAT).EQ.VUNCSH.OR.
     *                      VALREC(VSTAT).EQ.VCXL.OR.
     *                          VALREC(VSTAT).EQ.VCASH.OR.
     *                              VALREC(VSTAT).EQ.VBANK) THEN
C INCREASE STATISTICS COUNTERS
                   IF(VALREC(VSTAT).EQ.VPRPAY) CPPAY = CPPAY + 1
                   IF(VALREC(VSTAT).EQ.VUNCSH) CUCSH = CUCSH + 1
                   IF(VALREC(VSTAT).EQ.VCXL) CVCXL = CVCXL + 1
                   IF(VALREC(VSTAT).EQ.VCASH) CCASH = CCASH + 1
                   IF(VALREC(VSTAT).EQ.VBANK) CBANK = CBANK + 1
C       CHECK AGENT OF PASREC FROM TPF FILE AND PASS IT TROUGH 
C       WRITEWINNERS_LN SUBROUTINE
                   CALL PASIO_READ(PASFDB,VALREC(VTCKT),VALREC(VSERN),
     *                             VALREC(VPFRAC),PASREC)
                   IF (PASFDB.ERR .NE. IOE_NOERR) THEN
                     TYPE*, IAM(), '       '
                     WRITE(CKEY,9000) VALREC(VTCKT),VALREC(VSERN),VALREC(VPFRAC)
                     TYPE*, IAM(), 'SOUPWIN - Erro: ', PASFDB.ERR, 
     *                             ' a ler o registo: ', CKEY
                     TYPE*, IAM(), '       '
                     TYPE*, IAM(), 'DUMP:'
                     CALL PASIO_DUMP(PASFDB)
                     TYPE*, IAM(), '       '
                     CALL GSTOP(GEXIT_FATAL)
                   ENDIF

C CALL WRITEWINNERS_LN ROUTINE
                   CALL WRITEWINNERS_LN(EGNUM,VALREC,DPASHV,
     *                       DPAEXSHV,POPWSER,DPANOFFRA,PUNIT,
     *                       VDETAIL,CNTREC,PASREC.AGT,NUMESHR)
                   CNT_PRM=1 + CNT_PRM
                ENDIF
             ELSE
                CALL GPAUSE
             ENDIF
          ENDIF 
       ENDDO

301    CONTINUE


C WRITE TRAILER
C******************
       TPREC='TP'
       CNTREC =CNTREC+1
       WRITE(PUNIT,912),CNTREC


C PRINT STATISTICS PER FILE
        TYPE*,IAM(),'Total de registos processados:  ',CNT_PRM
        TYPE*,IAM(),'   UCSH:                        ',CUCSH
        TYPE*,IAM(),'   PPAY:                        ',CPPAY
        TYPE*,IAM(),'   VCXL:                        ',CVCXL
        TYPE*,IAM(),'   CASH:                        ',CCASH
        TYPE*,IAM(),'   BANK:                        ',CBANK
        TYPE*,IAM(),' '

        IF(CNT_PRM.NE.(CUCSH+CPPAY+CVCXL+CCASH+CBANK)) THEN
           TYPE*,IAM(),'* * * * * * * * * * * * * * * * * * * * * * * *'
           TYPE*,IAM(),'  O TOTAL DE REGISTOS CONTABILIZADOS NAO ESTA  '
           TYPE*,IAM(),'  COERENTE COM OS SUBTOTAIS CONTABILIZADOS!!!  '
           TYPE*,IAM(),'* * * * * * * * * * * * * * * * * * * * * * * *'
           ERRORFLAG = .TRUE.
           CALL GPAUSE
        ENDIF


C CHECK IF ERRORS OCURRED DURING FILE PROCESSING
       IF(ERRORFLAG) THEN
          TYPE*,IAM(),'   Ficheiro gerado com ERROS:                      '
       ELSE
          TYPE*,IAM(),'   Ficheiro gerado com SUCESSO:                    '
       ENDIF
C
C=======================================================================
C       LIST THE LATEST VERSION OF THE GENERATED FILES
C=======================================================================
C
       WRITE(LIBCMD,'(A,A,A)') '$ DIR ',TRIM(PFILENAM(6:)),';0 /DATE/SIZE=ALL'

       ST = LIB$SPAWN(TRIM(LIBCMD))
       IF(.NOT. ST) CALL LIB$SIGNAL(%VAL(ST))
       TYPE*,IAM(),'***********************************************************'
       TYPE*,IAM(),' '
       TYPE*,IAM(),'***********************************************************'
       TYPE*,IAM(),'                       SOUPWIN ENDED                       '
       TYPE*,IAM(),'***********************************************************'
       TYPE*,IAM(),' '

       CALL ICLOSE(VLUN,VPFBUF,ST)
       CLOSE (PUNIT)
       CALL PASIO_CLOSE(PASFDB)
       RETURN

C   HEADER FORMAT FOR SOUP_PA FILE
910    FORMAT('HP',I4.4,I2.2,I2.2,I4.4,I2.2,I2.2I4.4,I2.2,I2.2I4.4,I2.2,I2.2,21(' '))

C   TRAILER FORMAT FOR SOUP_PA FILE
912    FORMAT('TP',I8.8,45(' '))

901    FORMAT(A6)
902    FORMAT(A1)

5      FORMAT(A2,I4.4,I2.2,I2.2,25(' '))

C 20     FORMAT(A2,I8.8,25(' '))

9000   FORMAT(I5.5,'S',I2.2,'F',I2.2)

      END


C**********************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE WRITEWINNERS_LN(EGNUM, VALREC, EDPASHV, EDPAEXSHV,
     *               EPOPWSER, ENOFFRAC, PUNIT, VDETAIL,CNTREC,AGTNUMBER,NUMESHARE)
        IMPLICIT NONE
C**********************************************************************

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
        INCLUDE 'INCLIB:NAMCMD.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:PRMHSH.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:M_ISLN_PJMC.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'

C  LOCAL VARIABLES
        INTEGER*2       AUX
        INTEGER*4       I
        INTEGER*4       CNTREC
        INTEGER*4       PUNIT
        INTEGER*4       DIV
        INTEGER*4       SHR
        CHARACTER*55    CPRIZES(VMAX)
        CHARACTER*2     EGNUM
        CHARACTER*4     BILSTAT
        INTEGER*4       CHANNEL, TEMPNETPRIZE
        INTEGER*2       DAT(12), AGTNUMBER
        INTEGER*4       PORTALSAP, NUMESHARE
        PARAMETER       (PORTALSAP=007456)
        INTEGER*4       PAS_ROUND_VALUE, SHRVAMT, TOTNUMPRIZES
        INTEGER*8       EDPASHV(*),EDPAEXSHV(*)
        INTEGER*4       EPOPWSER      ! WINNING SERIE (POPULAR ONLY)
        INTEGER*4       ENOFFRAC      ! NUMBER OF FRACTIONS ON CLASSICA

C ********************************************************************
        AUX = 1


C FOR TESTS ONLY - Start
C ********************************************************************************* REMOVE ON FINAL VERSION
C        IF(AGTNUMBER.GE.1040.AND.AGTNUMBER.LE.1060) THEN
C ********************************************************************************* REMOVE ON FINAL VERSION

C ********************************************************************************* UNCOMMENT ON FINAL VERSION
        IF ( AGTSAP(AGTNUMBER).EQ. PORTALSAP ) THEN
C ********************************************************************************* UNCOMMENT ON FINAL VERSION
           CHANNEL = 2
        ELSE
           CHANNEL = 1
        ENDIF

C Calculate Prize Expritation Date
        DAT(VCDC) = VALREC(VPRGCDC)
        CALL CDATE(DAT)

C Check if Netprize exists
        IF (VALREC(VOPSAMT).EQ.0) THEN
           TEMPNETPRIZE = VALREC(VPAMT)
        ELSE
           TEMPNETPRIZE = VALREC(VOPSAMT)
        ENDIF

C Check and convert VSTAT
        IF (VALREC(VSTAT).EQ.VPRPAY) THEN
           BILSTAT='PPAY'
        ELSEIF (VALREC(VSTAT).EQ.VUNCSH) THEN
           BILSTAT='UCSH'
        ELSEIF (VALREC(VSTAT).EQ.VCXL) THEN
           BILSTAT='VCXL'
        ELSEIF (VALREC(VSTAT).EQ.VCASH) THEN
           BILSTAT='CASH'
        ELSEIF (VALREC(VSTAT).EQ.VBANK) THEN
           BILSTAT='BANK'
        ENDIF


C PRINT VPF INFORMATION (TEMP 02 TYPE REC) 
C*************************************************
C Reset TOTAL NUM PRIZES PER VALREC
        TOTNUMPRIZES=0
        I=1
        DO 100 I=1,VALREC(VPZOFF)
           DIV = VDETAIL(VDIV,I)
C If Popular or Extraordinaria
           IF (EGNUM.EQ.'09') THEN
C If it is a Serie Premiada
              IF (VALREC(VPFRAC) .EQ. EPOPWSER) THEN
                 SHRVAMT=PAS_ROUND_VALUE(EDPASHV(DIV))
              ELSE
C If Extra Share is 0 and then use Normal Share Value
                 IF (DIV.GT.NUMESHARE) THEN
C Check if share amount is equal to zero. Not expected!
                    IF(EDPASHV(DIV).EQ.0) THEN
                       TYPE*,IAM(),'**********************************'
                       TYPE*,IAM(),'O MONTANTE DA SHARE PARA A DIVISAO'
                       TYPE*,IAM(),'      ',DIV,'  E IGUAL A ZERO!    '
                       TYPE*,IAM(),'**********************************'
                       TYPE*,IAM(),'BILHETE:           ',VALREC(VTCKT)
                       TYPE*,IAM(),'SERIE:             ',VALREC(VSERN)
                       TYPE*,IAM(),'FRACAO:            ',VALREC(VPFRAC)
                       TYPE*,IAM(),'**********************************'
                       CALL GSTOP (GEXIT_FATAL)
                    ELSE
                       SHRVAMT=PAS_ROUND_VALUE(EDPASHV(DIV))
                    ENDIF
                 ELSE
                    SHRVAMT=PAS_ROUND_VALUE(EDPAEXSHV(DIV))
                 ENDIF
              ENDIF
C If classica use Normal Share value
           ELSEIF (EGNUM.EQ.'08') THEN
C Check if share amount is equal to zero. Not expected!
              IF(EDPASHV(DIV).EQ.0) THEN
                 TYPE*,IAM(),'**********************************'
                 TYPE*,IAM(),'O MONTANTE DA SHARE PARA A DIVISAO'
                 TYPE*,IAM(),'      ',DIV,'  E IGUAL A ZERO!    '
                 TYPE*,IAM(),'**********************************'
                 TYPE*,IAM(),'BILHETE:           ',VALREC(VTCKT)
                 TYPE*,IAM(),'SERIE:             ',VALREC(VSERN)
                 TYPE*,IAM(),'FRACAO:            ',VALREC(VPFRAC)
                 TYPE*,IAM(),'**********************************'
                 CALL GSTOP (GEXIT_FATAL)
              ELSE
C Divide Amount by number of fractions
                 SHRVAMT=PAS_ROUND_VALUE(EDPASHV(DIV)) / ENOFFRAC
              ENDIF
           ENDIF
C Number of prizes per division
           SHR = VDETAIL(VSHR,I)
           WRITE (CPRIZES(AUX),804) DIV,SHR,SHRVAMT
           AUX = AUX + 1
C Count Total Number of Prizes per Rec
           TOTNUMPRIZES=TOTNUMPRIZES+SHR

100     CONTINUE

C----|--!--------------------------------------------------------------
C V02   ! adding new field with effective prize
C----|--!--------------------------------------------------------------
          WRITE(PUNIT,906)
     *                     CHANNEL           ! CHANNEL ID
     *                   , VALREC(VTCKT)     ! TICKET NUMBER
     *                   , VALREC(VSERN)     ! SERIE NUMBER
     *                   , VALREC(VPFRAC)    ! FRACTION NUMBER
     *                   , VALREC(VSCDC)     ! BET REFERENCE (CDC)
     *                   , VALREC(VSSER)     ! BET REFERENCE (SERIAL NUMBER)
     *                   , BILSTAT           ! TICKET STATUS
     *                   , VALREC(VPAMT)     ! PAY AMOUNT
     *                   , TEMPNETPRIZE      ! NET PAY AMOUNT
     *                   , TOTNUMPRIZES      ! TOTAL NUMBER OF PRIZES


C  PRINT VPF INFORMATION (02 TYPE REC)
        CNTREC = CNTREC + 1
            DO 1000 I=1,AUX-1
              WRITE(PUNIT,810) CPRIZES(I)
              CNTREC = CNTREC + 1

1000    CONTINUE


C  PRINT DIVISIONS 
903     FORMAT(A4)

906     FORMAT('01',I1.1,I5.5,I2.2,I2.2,I4.4,I9.9,A4,I11.11,I11.11,I4.4)

804     FORMAT('02',I2.2,I5.5,I11.11,35(' '))

810     FORMAT(A55) 

       END

