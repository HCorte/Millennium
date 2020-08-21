C PASPURGE.FOR
C
C
C V03 15-OCT-2010 FJG Manual invocation
C V02 01-JAN-2010 FJG ePassive
C V01 12-JAN-01 CS  INITIAL RELEASE FOR PORTUGAL
C
C 1) STOPSYS PROGRAM THAT GENERATES PURGE PASSIVE PRIZES
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
C=======OPTIONS  /CHECK=NOOVERFLOW
	PROGRAM  PASPURGE
	IMPLICIT NONE

        include 'inclib:SYSPARAM.DEF'
        include 'inclib:SYSEXTRN.DEF'
	include 'inclib:GLOBAL.DEF'
	include 'inclib:CONCOM.DEF'
	include 'inclib:PASCOM.DEF'
	include 'inclib:STANDARD.DEF'
        include 'inclib:PASPURGE.DEF'
        include '($ssdef)'
C
        integer*4 emis
        integer*4 draw
        integer*4 gnum
        integer*4 gind
        integer*4 erro
C
        character*255 osvarval
        character*255 osvarnam/'paspurge'/
        integer*4     oslen
        integer*4     lib$get_symbol    
C
C INITIALIZE SOME VARIABLES
C

        type*,iam()
        call copyrite
        type*,iam()
        type*,iam(),'<<< PASPURGE - Purge VPF files for Passive draws >>>'
        type*,iam()
        erro = lib$get_symbol(osvarnam,osvarval,oslen)
        if(erro.EQ.SS$_NORMAL) then
          type*,iam(),'<<< THIS PROCESS WILL BE EXECUTED IN MANUAL MODE >>>'
          type*,iam()
          call inpnum('Enter game index:    ',gind,1,NUMPAS,erro)
          if(erro.eq.0) then
            gnum = gtntab(tpas,gind)
            if(gnum.gt.0) then
              call inpnum('Enter draw to purge: ',draw,1,9999,erro)              
              if(erro.eq.0) then
                do emis = 1, pagemi
                  if(pasemis(emis,gind).eq.draw) then
                    if(pasprgcdc(emis,gind).gt.daycdc) then
                      type*,iam()
                      type*,iam(),'It is not purge date yet. Today: ',daycdc
                      type*,iam(),'Estimated purge CDC:             ',pasprgcdc(emis,gind)
                    endif               
                    type*,iam()
                    call prmyesno('Do you want to proceed with draw purge? ',erro)     
                    type*,iam()
                    if(erro.eq.1) call purgedraw(emis,gnum,gind)                       
                    exit
                  else
                    if(emis.eq.pagemi) then
                      type*,iam()                     
                      type*,iam(),'Draw ',draw,' not found in memory'
                    endif
                  endif
                enddo
              endif
            endif
          endif
        else
          do gind = 1, numpas
            gnum = gtntab(tpas,gind)
            if (gnum.gt.0) then
              do emis = 1, pagemi
                if( pasprgcdc(emis,gind).le.daycdc.and.pasprgcdc(emis,gind).gt.0.and.
     *              passubsts(emis,gind).eq.pdrwpur ) then 
                  call purgedraw(emis,gnum,gind)
                endif
              enddo
            endif  
	  enddo
        endif
C
        type*,iam()          
	CALL GSTOP(GEXIT_SUCCESS)
	END
C*************************************************************
C SUBROUTINE PURGEDRAW
C*************************************************************
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE PURGEDRAW(EMIS,GNUM,GIND)
	IMPLICIT   NONE
C
        include 'inclib:SYSPARAM.DEF'
        include 'inclib:SYSEXTRN.DEF'
	include 'inclib:GLOBAL.DEF'
	include 'inclib:DESVAL.DEF'
	include 'inclib:VALPASFIL.DEF'
	include 'inclib:VDETAIL.DEF'	
	include 'inclib:HSHCOM.DEF'
	include 'inclib:GTNAMES.DEF'
	include 'inclib:DPAREC.DEF'
	include 'inclib:CONCOM.DEF'
	include 'inclib:PASCOM.DEF'
	include 'inclib:STANDARD.DEF'
	include 'inclib:DATBUF.DEF'
        include 'inclib:PASPURGE.DEF'
        include '($ssdef)'
C
C PARAMETERS
C
	integer*4 TUBSIZ
	PARAMETER (TUBSIZ=I4BUCSIZ*7)
C
C LOCAL VARIABLES
C
        integer*4 VPFBUF(TUBSIZ)
        integer*4 erro
        integer*4 emis
        integer*4 gnum
        integer*4 gind
        integer*4 vlun
        integer*4 crec
        integer*4 przs
        integer*4 div
        integer*4 shr
C
        integer*4 PRGWIN(PAGDIV,PAGEMI,NUMPAS)
        integer*4 PRGCAN(PAGDIV,PAGEMI,NUMPAS)
C
	CALL FASTSET(0, PRGWIN, SIZEOF(PRGWIN)/4)
	CALL FASTSET(0, PRGCAN, SIZEOF(PRGCAN)/4)        
C	
        WRITE (5,1000) IAM(),GIND,PASEMIS(EMIS,GIND),PASDRAW(EMIS,GIND),CPASVPFFIL(EMIS,GIND)
        CALL OPENREPS(GIND, PASEMIS(EMIS,GIND), ERRO)
        IF(ERRO.NE.0) THEN
          TYPE*,IAM(),' Reports open error, JOGO: ', GNUM
          CALL GPAUSE
        ENDIF
C
C OPEN VPF FILES
C
        CALL FIND_AVAILABLE_LUN (VLUN,ERRO)
        IF (ERRO.NE.0) CALL FILERR(pasvpffil(1,emis,gind),HANDLE_ERROR,ERRO,0)
        CALL IOPEN(pasvpffil(1,emis,gind),VLUN,VPFLEN*2,VFSCDC,VFSSER*2-1,ERRO)
        IF(ERRO.NE.0) CALL FILERR(pasvpffil(1,emis,gind),OPEN_ERROR,ERRO,0)
        CALL ITUBSIZE(VLUN,TUBSIZ)
C
C LOOP READING FILE VPF FILES
C
        crec = 0
        DO  WHILE(ERRO.NE.ERREND)
          crec = crec + 1
          CALL ISREAD(V4BUF_PAS,VLUN,VPFBUF,ERRO)
          IF (ERRO.EQ.0) THEN
            CALL LOGPAS(VALREC,V4BUF_PAS)
            IF(VALREC(VSTAT).NE.VCASH.AND.VALREC(VSTAT).NE.VBANK) THEN
              CALL DLOGPAS(VALREC,VDETAIL)
              DO PRZS = 1, VALREC(VPZOFF)
                DIV = VDETAIL(VDIV,PRZS)
                SHR = VDETAIL(VSHR,PRZS)
                IF(VALREC(VSTAT).EQ.VCXL) THEN
                  PRGCAN(DIV,EMIS,GIND) = PRGCAN(DIV,EMIS,GIND) + SHR
                ELSE
                  PRGWIN(DIV,EMIS,GIND) = PRGWIN(DIV,EMIS,GIND) + SHR
                ENDIF
              ENDDO		      
              CALL PPURWIN(VALREC,VDETAIL)
            ENDIF
          ELSEIF(ERRO.NE.ERREND) THEN
            CALL FILERR(pasvpffil(1,emis,gind),READ_ERROR,ERRO,crec)
          ENDIF
        ENDDO
C
C CLOSE PASSIVE VALIDATION FILE
C
        CALL ICLOSE(VLUN,VPFBUF,ERRO)
        IF(ERRO.NE.0) CALL FILERR(pasvpffil(1,emis,gind),CLOSE_ERROR,ERRO,0)
C
C CLOSE REPORT FILES
C
        CALL CLOSEREPS(emis, GIND)
        CALL CHECK_TICKETS_PURGED(GIND,EMIS,PRGWIN,PRGCAN)
        CALL UPD_GAME_FILE(GIND,EMIS,PRGWIN,PRGCAN)
C
        RETURN
C
C FORMAT STATEMENTS
C
1000    FORMAT(1X,A,'Purging Game ',I1,' draw: ',I5,1X,I6.6,1X,' File: ',A20)        
        END        	
C*************************************************************
C SUBROUTINE CLOSEREPS
C*************************************************************
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE CLOSEREPS(EMISIND, GIND)
	IMPLICIT   NONE
C
C INCLUDES DEFINITION
C
	include 'inclib:SYSDEFINE.DEF'
	include 'inclib:SYSEXTRN.DEF'
	include 'inclib:GLOBAL.DEF'
	include 'inclib:CONCOM.DEF'
        include 'inclib:PASPURGE.DEF'
	include 'inclib:AGTCOM.DEF'
        include 'inclib:PASCOM.DEF'
C
C PARAMETERS DEFINITION 
C
        integer * 4 EMISIND                   ! EMISSION INDEX
        integer * 4 GIND                      ! GAME INDEX
C
C LOCAL VARIABLES
C
	integer * 4 VLSTS                     ! VALIDATION STATUS COUNTER
        integer * 4 IDFIL                     ! IDENTIFICATION FILE
        integer * 4 VERS                      ! REPORT VERSION
        integer * 4 CNTPAG                    ! REPORT PAGE COUNTER
        integer * 4 TOT_PRG_CNT               ! TOTAL PURGED COUNTER
C
	integer * 8 TOT_PRG_AMT               ! TOTAL PURGED AMOUNT
C
        CHARACTER * 08 NAMEREPORT             ! NAME REPORTS
        CHARACTER * 42 HEADER                 ! REPORT HEADER
        CHARACTER * 14 REP_AMT                ! REPORT AMOUNT
C
C INITIATE LOCAL VARIABLES ( VARIABLES TO WORK WITH THE REPORT )
C
        IDFIL  = RLUN(REP_PRG)
        VERS   = VERSION(REP_PRG)
        CNTPAG = PAGE(REP_PRG)
C
        HEADER     = HEAD(REP_PRG)
        NAMEREPORT = REPNAM(REP_PRG)(1:8)
C
C INITIATE LOCAL VARIABLES ( CONUNTER TO TOTAL PURGED INFORMATION )
C
        TOT_PRG_CNT = 0
        TOT_PRG_AMT = 0
C
C PRINT LAST PAGE AND PURGE REPORT ( IF REPORT IS NOT OPEN EXIT )
C
	IF(IDFIL .LE. 0) RETURN
C
C CHECK IF NEW PAGE IS NEEDED
C
	IF(LINCNT(REP_PRG) .GT. MAXPRTLN) THEN
          IF(CNTPAG .GE. 9999) CNTPAG = 1
          CALL TITLE(HEADER, NAMEREPORT, VERS, IDFIL, CNTPAG, DAYCDC, 0, 0)
	ENDIF
C
C CALCULATE GRAN TOTALS FOR PURGED TICKETS
C
        DO VLSTS = PRG_CASH, MAX_PRG_VAL_STS
          TOT_PRG_CNT = TOT_PRG_CNT + TOTPRG(VLSTS, PRG_CNT)
          TOT_PRG_AMT = TOT_PRG_AMT + TOTPRG(VLSTS, PRG_AMT)
        ENDDO
C
C WRITE ALL INFORMATION IN THE REPORT ( PURGE CASHED CONTERS AND AMOUNT )
C
        REP_AMT = CMONY(TOTPRG(PRG_UNCH, PRG_AMT), 14, VALUNIT)        
        WRITE(IDFIL, 100) TOTPRG(PRG_UNCH, PRG_CNT), REP_AMT
C
C WRITE ALL INFORMATION IN THE REPORT ( PURGE UNSOLD CONTERS AND AMOUNT ) 
C
        REP_AMT = CMONY(TOTPRG(PRG_VDEL, PRG_AMT), 14, VALUNIT)
        WRITE(IDFIL, 200) TOTPRG(PRG_VDEL, PRG_CNT), REP_AMT
C
C WRITE ALL INFORMATION IN THE REPORT ( PURGE RETURN CONTERS AND AMOUNT )
C
        REP_AMT = CMONY(TOTPRG(PRG_VCXL, PRG_AMT), 14, VALUNIT)
        WRITE(IDFIL, 300) TOTPRG(PRG_VCXL, PRG_CNT), REP_AMT
C
C WRITE ALL INFORMATION IN THE REPORT ( PURGE GENERAL INFORMATION )
C
        WRITE(IDFIL, 400) TOT_PRG_CNT, CMONY(TOT_PRG_AMT, 14, VALUNIT)
C
C CLOSE REPORT FILE
C
	CALL USRCLOS1(IDFIL)
C
C SET TOTAL PURGED AMOUNT TO PASSAP FILE ( REGISTER 34 )
C
        PASTOPAYAMT(EMISIND, GIND) = TOTPRG(PRG_UNCH, PRG_AMT)
C
C EXIT FUNCTION
C
	RETURN
C
C FORMATS DEFINITION
C
100	FORMAT(/,20X,'TOTAL DE PREMIADOS PRESCRITOS            :',I10,4X,A14)
200     FORMAT(/,20X,'TOTAL DE PREMIADOS NAO VENDIDOS          :',I10,4X,A14)
300	FORMAT(/,20X,'TOTAL DE PREMIADOS PRESCRITOS RETORNADOS :',I10,4X,A14)
400	FORMAT(/,20X,'TOTAL GERAL DE PREMIADOS PRESCRITOS      :',I10,4X,A14)
500     FORMAT(X, A, 'ERROR, PASSIVE PRIZE FOR ', A7, ' DRAW: ', I6.6)
600     FORMAT(X, A, 'IS NOT ZERO ', I)
	END
C
C*************************************************************
C SUBROUTINE OPENREPS
C*************************************************************
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE OPENREPS(GIND, DRAW, ST)
	IMPLICIT   NONE

	include 'inclib:SYSDEFINE.DEF'
	include 'inclib:SYSEXTRN.DEF'
	include 'inclib:GLOBAL.DEF'
	include 'inclib:CONCOM.DEF'
	include 'inclib:PASPURGE.DEF'
	include 'inclib:DATBUF.DEF'
C
C ROUTINE PARAMETER
C
	integer*4    GIND, DRAW, ST
C
C LOCAL VARIABLES
C
	integer*2    DATE(DATLEN)
C
	integer*4    I
C
C INIT REPORT VARIABLES
C
        CALL FASTSET(0, TOTPRG, MAX_PRG_VAL_STS * MAX_PRG_CTLS)
C
C INIT ALL REP. VARIABLES
C
	CALL FASTSET(1, VERSION,  SIZEOF(VERSION)/4)
	CALL FASTSET(0, PAGE,     SIZEOF(PAGE)/4)
C
C GET CORRECT DATE
C
	DATE(VCDC) = DAYCDC
	CALL CDATE(DATE)
C
C GET PURGE REPORT NAME AND LOGICAL UNIT
C
        WRITE(REPNAM(REP_PRG), 300) STRTYP(GIND), DRAW
	CALL FIND_AVAILABLE_LUN (RLUN(REP_PRG),ST)
	IF  (ST.NE.0) THEN
	    TYPE *,IAM(),'ERRO OBTENDO HANDLE P/ RELATORIO ', REPNAM(REP_PRG)
	    TYPE *,IAM(),'STATUS ',ST
	    RETURN
	ENDIF
C
C OPEN REPORT FILE
C
	CALL ROPEN(REPNAM(REP_PRG),RLUN(REP_PRG),ST)
	IF  (ST.NE.0) THEN
	    TYPE *,IAM(),'ERRO ABRINDO RELATORIO ',REPNAM(REP_PRG)
	    TYPE *,IAM(),'STATUS ',ST
	    RETURN
	ENDIF
C
C FILL REPORT HEADER AND PRINT TITLE
C
	WRITE(HEAD(REP_PRG), 100) (DATE(I),I=7,12)
        IF(PAGE(REP_PRG) .GE. 9999) PAGE(REP_PRG) = 1
	CALL TITLE(HEAD(REP_PRG),REPNAM(REP_PRG),VERSION(REP_PRG),
     *		       RLUN(REP_PRG),PAGE(REP_PRG),DAYCDC,0,0)
	WRITE(RLUN(REP_PRG), 200)
	LINCNT(REP_PRG) = 9

	RETURN
C
C FORMAT STATEMENTS
C
100	FORMAT('RELATORIO DE PRESCRITOS PARA ',6A2)
200	FORMAT(1X,131('-'),/,' BILHETE  SERIE FRACAO',
     *         8X,'JOGO',7X,'STATUS',8X,'VALOR',3X,'DIVISOE(S)'/,
     *         1X,131('-'),/)
300     FORMAT('PPURWIN', A1, '_', I4.4, '.REP')
C
	END
C*************************************************************
C SUBROUTINE PPURWIN
C*************************************************************
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE PPURWIN(VALREC,VDETAIL)
	IMPLICIT   NONE

	include 'inclib:SYSDEFINE.DEF'
	include 'inclib:SYSEXTRN.DEF'
	include 'inclib:GLOBAL.DEF'
	include 'inclib:CONCOM.DEF'
	include 'inclib:AGTCOM.DEF'
	include 'inclib:DESVAL.DEF'
	include 'inclib:VDETAIL.DEF'
        include 'inclib:DATBUF.DEF'
        include 'inclib:PASPURGE.DEF'
	include 'inclib:TNAMES.DEF'
C
C LOCAL VARIABLES
C
	integer*4 K,VSTS,GAM
	integer*8 AMOUNT
C
C CHECK IF NEW PAGE IS NEEDED
C
	IF(LINCNT(REP_PRG).GT.MAXPRTLN) THEN
            IF(PAGE(REP_PRG) .GE. 9999) PAGE(REP_PRG) = 1
	    CALL TITLE(HEAD(REP_PRG),REPNAM(REP_PRG),VERSION(REP_PRG),
     *		       RLUN(REP_PRG),PAGE(REP_PRG),DAYCDC,0,0)
	    WRITE(RLUN(REP_PRG), 100)
	    LINCNT(REP_PRG) = 9
	ENDIF
C
C PRINT REPORT LINES
C
	VSTS   = VALST(VALREC(VSTAT))
	GAM    = VALREC(VGAM)
	AMOUNT = VALREC(VPAMT) 

	WRITE(RLUN(REP_PRG), 200) 
     *                   VALREC(VTCKT),
     *                   VALREC(VSERN),
     *			 VALREC(VPFRAC),
     *			 (GLNAMES(K,GAM),K=1,4),VSTS,
     *			 CMONYI8(AMOUNT,12,VALUNIT),
     *			 (VDETAIL(VDIV,K),K=1,VALREC(VPZOFF))
C
	LINCNT(REP_PRG) = LINCNT(REP_PRG) + 1
C
C ACCUMULATE TOTALS ( FOR CASH VALIDATION STATUS )
C
        IF(VALREC(VSTAT) .EQ. VCASH) THEN
          TOTPRG(PRG_CASH, PRG_CNT) = TOTPRG(PRG_CASH, PRG_CNT) + 1
          TOTPRG(PRG_CASH, PRG_AMT) = TOTPRG(PRG_CASH, PRG_AMT) + AMOUNT
        ENDIF
C
C ACCUMULATE TOTALS ( FOR PURGED UNSOLD STATUS )
C
        IF(VALREC(VSTAT) .EQ. VDEL) THEN
          TOTPRG(PRG_VDEL, PRG_CNT) = TOTPRG(PRG_VDEL, PRG_CNT) + 1
          TOTPRG(PRG_VDEL, PRG_AMT) = TOTPRG(PRG_VDEL, PRG_AMT) + AMOUNT
        ENDIF
C
C ACCUMULATE TOTALS ( FOR PURGED RETURN AFTHER DRAW STATUS )
C
        IF(VALREC(VSTAT) .EQ. VCXL) THEN
          TOTPRG(PRG_VCXL, PRG_CNT) = TOTPRG(PRG_VCXL, PRG_CNT) + 1
          TOTPRG(PRG_VCXL, PRG_AMT) = TOTPRG(PRG_VCXL, PRG_AMT) + AMOUNT
        ENDIF
C
C ACCUMULATE TOTALS ( FOR PURGED UNCASH STATUS )
C
        IF(VALREC(VSTAT) .EQ. VUNCSH .OR. VALREC(VSTAT) .EQ. VPRPAY) THEN
          TOTPRG(PRG_UNCH, PRG_CNT) = TOTPRG(PRG_UNCH, PRG_CNT) + 1
          TOTPRG(PRG_UNCH, PRG_AMT) = TOTPRG(PRG_UNCH, PRG_AMT) + AMOUNT
        ENDIF
C
C CHECK VALIDATION STATUS
C
        VSTS = VALREC(VSTAT)
        IF(VSTS .NE. VCASH   .AND. 
     *     VSTS .NE. VDEL    .AND. 
     *     VSTS .NE. VCXL    .AND.
     *     VSTS .NE. VUNCSH  .AND.
     *     VSTS .NE. VNOPAY  .AND.
     *     VSTS .NE. VPRPAY )
     * 
     *   THEN
           TYPE *, IAM(), 'VALIDATION STATUS DIFFERENT OF EXPECTED ONE: ', VSTS
         ENDIF 
C
C EXIT SUBROUTINE 
C
	RETURN
C
C FORMATS DEFINITON
C
100	FORMAT(1X,131('-'),/,' BILHETE  SERIE FRACAO',
     *         8X,'JOGO',7X,'STATUS',8X,'VALOR',3X,'DIVISOE(S)'/,
     *         1X,131('-'),/)
200	FORMAT(2X,I6.6,4X,I2,4X,I2.2,4X,4A4,2X,A4,2X,
     *	       A12,<VALREC(VPZOFF)>(1X,I3))
	END
C

C*************************************************************
C SUBROUTINE CHECK_TICKETS_PURGED
C*************************************************************
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE CHECK_TICKETS_PURGED(GIND,INDEMI,PRGWIN,PRGCAN)
	IMPLICIT   NONE

	include 'inclib:SYSDEFINE.DEF'
	include 'inclib:SYSEXTRN.DEF'
	include 'inclib:GLOBAL.DEF'
	include 'inclib:CONCOM.DEF'
	include 'inclib:PASCOM.DEF'
        include 'inclib:PASPURGE.DEF'
C
C ROUTINE PARAMETERS
C
	integer*4 GIND, INDEMI, ST
C
C LOCAL VARIABLES
C
	integer*4 DIV

        integer*4 PRGWIN(PAGDIV,PAGEMI,NUMPAS)
        integer*4 PRGCAN(PAGDIV,PAGEMI,NUMPAS)

	integer*4 QTDFRA, NUMSER, EXSHR, EXPAD


        integer*4 SHRTCK, SHRPRG, SHRPAY

	ST = 0

        IF (GIND.EQ.PSBPOP) THEN
          QTDFRA = 1
          NUMSER = 1
        ELSE
          QTDFRA =  PASNOFFRA(INDEMI,GIND)
          NUMSER =  PASNUMSER(INDEMI,GIND)
         ENDIF
        
        DIV = 1
	DO WHILE( DIV.LE.PASDIV(INDEMI,GIND) )
C
C CHECK IF WE HAVE EXTRA DIVISION
C
	  EXSHR = 0
	  EXPAD = 0
	  IF (DIV.LE.PAGEDV) THEN
	    IF (PASEXSHR(DIV,INDEMI,GIND).GT.0) THEN
	      EXSHR = PASEXSHR(DIV,INDEMI,GIND)
	      EXPAD = PASEXPAD(DIV,INDEMI,GIND)
	    ENDIF
	  ENDIF
C
	  SHRTCK = (PASSHR(DIV,INDEMI,GIND) + EXSHR) * QTDFRA * NUMSER
          SHRPRG = PRGWIN(DIV,INDEMI,GIND) + PRGCAN(DIV,INDEMI,GIND)
          SHRPAY = PASPAD(DIV,INDEMI,GIND) + EXPAD
C
          IF(SHRTCK .NE. SHRPRG + SHRPAY) THEN
	    TYPE *,IAM()
	    TYPE *,IAM(),'INCONSISTENCIA NA DIVISAO ', DIV,' EXTRACAO ',PASEMIS(INDEMI,GIND)
	    TYPE *,IAM(),'TOTAL DE PREMIOS: ', SHRTCK
	    TYPE *,IAM(),'PREMIOS PAGOS   :', SHRPAY
	    TYPE *,IAM(),'PRESCRITOS      : ',PRGWIN(DIV,INDEMI,GIND) 
	    TYPE *,IAM(),'RETORNADOS      : ',PRGCAN(DIV,INDEMI,GIND)
            TYPE *,IAM(),'PRESCRICAO COM ERRO'
	    TYPE *,IAM()
          ENDIF
	  DIV = DIV + 1
        ENDDO
C
	RETURN
	END
C
C*************************************************************
C SUBROUTINE UPD_GAME_FILE
C*************************************************************
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE UPD_GAME_FILE(GIND,EMIS,PRGWIN,PRGCAN)
	IMPLICIT   NONE

	include 'inclib:SYSDEFINE.DEF'
	include 'inclib:SYSEXTRN.DEF'
	include 'inclib:GLOBAL.DEF'
	include 'inclib:CONCOM.DEF'
	include 'inclib:PASCOM.DEF'
	include 'inclib:DPAREC.DEF'
        include 'inclib:PASPURGE.DEF'
        include 'inclib:STANDARD.DEF'
C
C ROUTINE PARAMETERS
C
	integer*4 GIND, EMIS
C
C LOCAL VARIABLES
C
	integer*4 GNUM, OFF, DIV, UNIT, FDB(7), ST
	integer*4 DRAW

        integer*4 PRGWIN(PAGDIV,PAGEMI,NUMPAS)
        integer*4 PRGCAN(PAGDIV,PAGEMI,NUMPAS)

        DRAW = pasemis(emis,gind)
	GNUM = GTNTAB(TPAS,GIND)

	CALL FIND_AVAILABLE_LUN(UNIT,ST)
	IF (ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),HANDLE_ERROR,ST,0)

	CALL OPENW(UNIT,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,UNIT,DPASEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),OPEN_ERROR,ST,0)

	WRITE(5,1000) IAM(),(GLNAMES(OFF,GNUM),OFF=1,4),DRAW

	CALL READW(FDB,DRAW-PAS_DRW_OFFSET,DPAREC,ST)
	IF (ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),READ_ERROR,ST,DRAW)

	DIV = 1
	DO WHILE( DIV .LE. DPADIV )
	    DPAPRG(DIV)    = PRGWIN(DIV,EMIS,GIND) + PRGCAN(DIV,EMIS,GIND)
            DIV = DIV + 1
        ENDDO

	DPAPUP = DAYCDC
	CALL WRITEW(FDB,DRAW-PAS_DRW_OFFSET,DPAREC,ST)
	IF (ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),WRITE_ERROR,ST,DRAW)

	CALL CLOSEFIL(FDB)

	RETURN
1000	FORMAT(1X,A,'Updating game file for ',4A4,' extraction ',I6)
	END
