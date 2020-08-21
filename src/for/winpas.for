C WINPAS.FOR
C
C V07 06-JUN-2014 FRP Display total prize amount as 8 bytes in PLANOREAL
C V06 05-NOV-2013 SCML Set 96 bytes/rec in Classica VPF size calculation
C                      Set 46 bytes/rec in Popular  VPF size calculation
C V05 27-SEP-2013 SCML Added net prize amount
C                      Set 84 bytes/rec in Classica VPF size calculation
C                      Set 34 bytes/rec in Popular  VPF size calculation
C V04 26-JUL-2013 FRP Set 80 bytes/rec in Classica VPF size calculation to avoid "hashing errors"
C V03 14-MAR-2013 FRP Make serXfra as even in VPF size calculation to avoid "hashing errors"
C V02 01-JAN-2010 FJG ePassive
C                 FJG Added PLREAL per ticket type
C V01 22-DEC-2000 ANG INITIAL RELEASE FOR PORTUGAL
C
C PROGRAM TO SELECT AND WRITE ON A FILE THE PASSIVE LOTTERY WINNERS.
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
C PROJECT LOTARIA CLASSICA EM SERIES (PLCS)
C ADD NEW FIELD ON FILE
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM WINPASS
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SYSDEFINE.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:STCEMIS.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'

        ! LOCAL VARIABLES
        INTEGER*4   TOTPRZ(PAGDIV), TOTEXPRZ(PAGEDV)
        INTEGER*4   INDPAS, EMINUM, INDEMI
        INTEGER*4   GNUM, I, WEEK, YEAR, RWINPAS

        INTEGER*4   BASAMT, RFNAMT, TAXPER !LN TAX CONFIGURATION !V05
        INTEGER*4   PLREAL(6,50)
        
        ! STRUCT WITH DATA EMISSION INFORMATION
        RECORD /STCEMIS/ RECEMI

        COMMON /PLCOM/ PLREAL
C
C VERIFY IF WE HAVE WINNER SELECTION TODAY
C
        TYPE *, IAM()
        TYPE *, IAM(),'<<<<<< WINPAS: PASSIVE WINNER SELECTION >>>>>>'
        TYPE *, IAM()
        
        DO INDPAS=1, NUMPAS
          eminum = 0          

          DO INDEMI=1, PAGEMI
            IF (PASSTS(INDEMI, INDPAS) .EQ. GAMDON) THEN
              eminum = pasemis(indemi,indpas)
              EXIT
            ENDIF
          ENDDO

C         CALL HAS_PAS_WINSEL(EMINUM, INDPAS)

          IF (EMINUM .NE. 0) THEN

            GNUM = GTNTAB(TPAS,INDPAS)
        
            ! GET DATA EMISSION INFORMATION
            CALL GET_EMISSION(INDEMI, INDPAS, RECEMI, WEEK, YEAR)

            WRITE(6,1001) IAM(),(GLNAMES(I,GNUM),I=1,4),EMINUM,WEEK,YEAR

            CALL INPYESNO('Is this correct ? [Y/N]',RWINPAS)
            IF (RWINPAS.EQ.3) CALL GSTOP(GEXIT_OPABORT)
            IF (RWINPAS.EQ.1) THEN

               ! GET LN TAX CONFIGURATION (TAXCONF.FIL)
               CALL GET_LN_TAXCONF(BASAMT, RFNAMT, TAXPER) !V05
              
               ! CREATE AND CLEAR WORK FILE (VPW.FIL)
               CALL INIC_FILES(EMINUM, INDPAS)

               ! READ THE PASSIVE LOTTERY TICKETS FILE AND WRITE THE WINNERS
               CALL FASTSET(0, TOTPRZ,   PAGDIV)
               CALL FASTSET(0, TOTEXPRZ, PAGEDV)
               CALL FASTSET(0, PLREAL,   50*6)         

!               CALL SEL_PAS_WIN(EMINUM, INDPAS, INDEMI, TOTPRZ, TOTEXPRZ, RECEMI)
               CALL SEL_PAS_WIN(EMINUM, INDPAS, INDEMI, TOTPRZ, TOTEXPRZ, RECEMI, BASAMT, RFNAMT, TAXPER) !V05

               ! EXECUTE MERGE OF FILES
               CALL EXEC_MERGE(EMINUM, INDPAS)

               ! SET STATUS GAME TO FINAL AND UPDATE NUMBER OF WINNERS IN P?F FILE
               CALL SET_STAT_GAM(INDPAS, TOTPRZ, TOTEXPRZ, EMINUM, INDEMI, RECEMI)
            
               ! GENERATE PLANO REAL REPORT
               CALL PRT_PLREAL(GNUM,WEEK,YEAR)

               WRITE(6,1002) IAM(),(GLNAMES(I,GNUM),I=1,4)
            ENDIF
          ELSE

            TYPE *, IAM(), '*'
            TYPE *, IAM(), '* THERE IS NO WINNER SELECTION FOR PASSIVE ',INDPAS,' TODAY'
            TYPE *, IAM(), '*'
          ENDIF

        ENDDO

        CALL GSTOP(GEXIT_SUCCESS)

1001    FORMAT(1X,A,'WINNER SELECTION FOR ',4A4,' EXTRACTION NUMBER ',I6,' SEMANA ',I2.2,' ANO ',I4.4)
1002    FORMAT(1X,A,'WINNER SELECTION FOR ',4A4,' FINISHED.')
        END

C******************************************************************************
C**** SUBROUTINE TO READ THE PASSIVE LOTTERY TICKETS FILE AND WRITE THE WINNERS
C******************************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE SEL_PAS_WIN(EXTNUM, INDPAS, INDEMI, TOTPRZ, TOTEXPRZ, RECEMI,
     *                         BASAMT, RFNAMT, TAXPER)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SYSDEFINE.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'
        INCLUDE 'INCLIB:pascom.DEF'
        INCLUDE 'INCLIB:STCEMIS.DEF'
        include 'inclib:pasiosubs.def'          

        LOGICAL     WSER,FPOP,FEXT

        COMMON /WINPAS/ WSER,FPOP,FEXT
!=======INPUT VARIABLES
        integer*4 extnum         ! Draw number
        integer*4 indpas         ! Index of Passive NUMPAS
        integer*4 indemi         ! Entry index of PAGEMI
        record /stcemis/ recemi  ! Info about the emission (already in memory!!)
!=======OUTPUT VARIABLES
        INTEGER*4   TOTPRZ(PAGDIV), TOTEXPRZ(PAGEDV)
!=======CONSTANT DEFINITIONS
        INTEGER*4   OPEN_FILE, WRITE_FILE, CLOSE_FILE
        PARAMETER   (OPEN_FILE  = 1)
        PARAMETER   (WRITE_FILE = 2)
        PARAMETER   (CLOSE_FILE = 3)
!=======LOCAL VARIABLES
        INTEGER*4 DIVWIN(PAGDIV)
        INTEGER*4 TICNUM
        INTEGER*4 SERNUM
        INTEGER*4 FRANUM
        INTEGER*4 TOTBIL
        INTEGER*4 INDDIV
        INTEGER*4 DUMMY
        INTEGER*4 CNTREC
        INTEGER*4 IMAXSER
        INTEGER*4 TOTPAYAMT
        INTEGER*4 CBUF(CDLEN)
C
        INTEGER*4 BASAMT, RFNAMT, TAXPER !LN TAX CONFIG VALUES !V05
C
        INTEGER*8 TOTAMT
        LOGICAL   TCKWIN   !INDIVIDUAL FLAG TO VERIFY IF THE TICKET IS WINNER. IF TRUE, CHANGE TICKET STATUS.

        record /stpasfdb/  pasfdb        
        record /stpasrec/  pasrec  
C=======OPEN VPW FILE
        CALL PAS_WINLOD(OPEN_FILE, DUMMY)

        call pasio_init(pasfdb,indpas,pasemis(indemi,indpas),pasnumtck(indemi,indpas)-1,pasnumser(indemi,indpas),
     *                  pasnoffra(indemi,indpas),cpastpffil(indemi,indpas))     
        call pasio_open(pasfdb) 
        if (pasfdb.err.ne.ioe_noerr) then
          type*,iam(),'Error: ',pasfdb.err,' opening file: ',pasfdb.filnam
        ELSE
          TOTAMT = 0
          TOTBIL = 0
          CNTREC = 1
          TOTPAYAMT = 0
          TICNUM = 0 
          IMAXSER = pasnumser(indemi,indpas)
         
          do ticnum = 0,pasnumtck(indemi,indpas)-1
            do sernum = 1,pasnumser(indemi,indpas)
              do franum = 1,pasnoffra(indemi,indpas)   
                call pasio_read(pasfdb,ticnum,sernum,franum,pasrec)
                if(pasfdb.err.ne.ioe_noerr) then
                  type*,iam(),'Error: ',pasfdb.err,' reading record: ', pasrec.key 
                  call gpause()
                else
                  call fastset(0, divwin, pagdiv)
!-----------------CHECK IF IT IS A WINNER TICKET                   
                  if (fpop) then
                    call chk_pas_reg_win(ticnum, franum, divwin, inddiv, recemi)   
                  else
                    call chk_pas_reg_win(ticnum, sernum, divwin, inddiv, recemi)
                  endif
!-----------------SUBROUTINE TO ACCUMULATE THE NUMBER OF PRIZES FOUND
                  call acc_prz_found(divwin, totprz, totexprz, totamt, totbil, recemi)  
!-----------------WRITE IF IT IS A PRIZED TICKET
                  tckwin = .false.
!                  call wrt_ticket(extnum,ticnum,sernum,franum,pasrec,divwin,indpas,recemi,tckwin,totpayamt)
                  call wrt_ticket(extnum,ticnum,sernum,franum,pasrec,divwin,indpas,recemi,tckwin,totpayamt, !V05
     *                            basamt,rfnamt,taxper)
!-----------------!UPDATE TICKET STATUS ON INDEXED FILE
                  if(tckwin) then                      ! PASREC SHOULD BE UPDATED IN WRT_TICKET
                    call pasio_write(pasfdb,ticnum,sernum,franum,pasrec)                    
                    if(pasfdb.err.ne.ioe_noerr) then
                      type*,iam(),'Error: ',pasfdb.err,' writing record: ',pasrec.key
                      call gpause()
                    endif                       
                  endif                  
                  if(mod(cntrec, 50000).eq.0) type*,iam(),'QUANTIDADE DE REGISTROS JA APURADOS    ',cntrec
                  cntrec = cntrec + 1
                endif   
              enddo
            enddo
          enddo             
! 
          ! CLOSE PASSIVE LOTTERY TICKETS FILE
          TYPE *, IAM(),'QUANTIDADE TOTAL DE REGISTROS APURADOS ', (CNTREC-1)
          call pasio_close(pasfdb)

          ! CLOSE VPW FILE
          CALL PAS_WINLOD(CLOSE_FILE, DUMMY)

          ! SHOWS NUMBER OF WINNERS PER DIVISION
          CALL SHOW_WIN_DIV(TOTPRZ, TOTEXPRZ, TOTAMT, IMAXSER, TOTBIL, RECEMI)

          IF (P(SYSTYP).EQ.LIVSYS) THEN
             ! COMMAND TO UPDATE TOPAYAMT
             CALL FASTSET(0,CBUF,CDLEN)

             CBUF(1) = 8 
             CBUF(2) = TOTPAYAMT
             CBUF(3) = TCPAS
             CBUF(6) = 'SYS '
             CBUF(8) = INDPAS
             CBUF(9) = EXTNUM                      
             CALL RESCMD(CBUF)
          ENDIF
        ENDIF                   ! TEST OPEN ERROR

        RETURN
400     FORMAT('FILE:PAS',I2.2,I4.4,'_',A1,'.DAT')
        END
C***********************************************************************
C**** SUBROUTINE TO VERIFY IF THE TICKET IS A WINNER (REGULAR DIVISIONS)
C***********************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CHK_PAS_REG_WIN(TICNUM, SERNUM, DIVWIN, INDREG,RECEMI)
        IMPLICIT NONE

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'

C       INCLUDE 'INCLIB:PRMPAS.DEF'
        INCLUDE 'INCLIB:STCEMIS.DEF'

        ! PARAMETERS
        INTEGER*4   DIVWIN(PAGDIV)
        INTEGER*4   TICNUM, SERNUM, INDREG

        ! STRUCT WITH DATA EMISSION
        RECORD /STCEMIS/ RECEMI

        ! LOCAL VARIABELS
        LOGICAL     WSER,FPOP,FEXT
        LOGICAL     PPRZ,WINID(PAGNBR,4)

        COMMON /WINPAS/ WSER,FPOP,FEXT

        INTEGER*4   AUXWIN, AUXNUM, PPRZID
        INTEGER*4   BEFNUM, AFTNUM, AUXBEF, AUXAFT, AUXCENT, I, J

        INTEGER*4   IDNUM, DIVTYP, FATOR

        WSER = .FALSE.
        PPRZ   = .FALSE.
        PPRZID = 0
        DO I=1,PAGNBR
            DO J=1,4
              WINID(I,J) = .FALSE.
            ENDDO
        ENDDO

        IF (FPOP.OR.FEXT) THEN   !POPULAR OR EXTRAORDINARIA
            IF (SERNUM.EQ.RECEMI.STCWSER) THEN 
                WSER = .TRUE.
            ELSE
                WSER = .FALSE.
            ENDIF
        ENDIF

        DO INDREG=1,RECEMI.STCDIV
            IDNUM  = RECEMI.STCIDNUM(INDREG)                           !RELATIVO A QUAL NUMERO SORTEADO
            FATOR  = RECEMI.STCDIG(INDREG)                             !DIVISOR PARA OBTER OS DIGITOS 
            DIVTYP = RECEMI.STCTYP(INDREG)                             !TIPO: APROXIMACAO, CENTENAS, DIGITOS

            IF (DIVTYP.EQ.PR_APRX .AND. IDNUM.NE.0 ) THEN                !APROXIMACAO

               IF (RECEMI.STCWIN(1,IDNUM) .EQ. 0) THEN
                  BEFNUM = RECEMI.STCNUMTCK-1
               ELSE
                  BEFNUM = RECEMI.STCWIN(1,IDNUM)-1
               ENDIF

               IF (RECEMI.STCWIN(1,IDNUM) .EQ. RECEMI.STCNUMTCK-1) THEN
                  AFTNUM = 0
               ELSE
                  AFTNUM = RECEMI.STCWIN(1,IDNUM)+1
               ENDIF

               AUXNUM = MOD(TICNUM,FATOR)
               AUXBEF = MOD(BEFNUM,FATOR)
               AUXAFT = MOD(AFTNUM,FATOR)
        
               IF (.NOT.WINID(IDNUM,DIVTYP)) THEN
                  IF (AUXNUM.EQ.AUXBEF .OR. AUXNUM.EQ.AUXAFT) THEN
                      DIVWIN(INDREG) = DIVWIN(INDREG) + 1
                  ENDIF
               ENDIF                

            ELSEIF (DIVTYP.EQ.PR_CENT .AND. IDNUM.NE.0 ) THEN              !CENTENA

               AUXCENT = INT(RECEMI.STCWIN(1,IDNUM)/100)
               AUXNUM  = INT(TICNUM/100)

               IF (.NOT.PPRZ .OR. (.NOT.WINID(IDNUM,DIVTYP).AND.IDNUM.NE.PPRZID) ) THEN
                  IF (AUXCENT.EQ.AUXNUM) THEN
                      DIVWIN(INDREG) = DIVWIN(INDREG) + 1
                  ENDIF  
               ENDIF

            ELSEIF ( DIVTYP.EQ.PR_DIG .OR. DIVTYP.EQ.PR_SEQ ) THEN   !DIGITOS

               IF (RECEMI.STCWNUM(INDREG).GE.1.AND.DIVTYP.EQ.PR_SEQ) THEN !(RECEMI.STCWNUM(INDREG).GT.1)
                  DO I=1,RECEMI.STCWNUM(INDREG)
                     AUXWIN = MOD(RECEMI.STCWIN(I,INDREG),FATOR)               
                     AUXNUM = MOD(TICNUM,FATOR)

                     IF (AUXWIN.EQ.AUXNUM) THEN
                         DIVWIN(INDREG) = DIVWIN(INDREG) + 1
                     ENDIF
                  ENDDO
               ELSEIF (IDNUM.NE.0 .AND. .NOT.WINID(IDNUM,DIVTYP) ) THEN
                  AUXWIN = MOD(RECEMI.STCWIN(1,IDNUM),FATOR)               
                  AUXNUM = MOD(TICNUM,FATOR)

                  IF (.NOT.PPRZ .OR. .NOT. WINID(IDNUM,DIVTYP)) THEN
                      IF (AUXWIN.EQ.AUXNUM) THEN
                          DIVWIN(INDREG) = DIVWIN(INDREG) + 1
                      ENDIF
                  ENDIF
               ENDIF         

            ENDIF

            IF (RECEMI.STCWNUM(INDREG).EQ.1.AND.DIVWIN(INDREG).GT.0) THEN
                PPRZ = .TRUE.
                PPRZID = IDNUM
            ENDIF

            IF (DIVWIN(INDREG).GT.0.AND.IDNUM.GT.0) THEN
                WINID(IDNUM,DIVTYP) = .TRUE.
            ENDIF

        ENDDO
        
        RETURN

        END

C****************************************
C**** SUBROUTINE TO WRITE A WINNER TICKET
C****************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE WRT_TICKET(EXTNUM,TICNUM,SERNUM,FRANUM,PASREC,DIVWIN,
     *                        INDPAS,RECEMI,TCKWIN,TOTPAYAMT,BASAMT,
     *                        RFNAMT,TAXPER)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:VALPASFIL.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:STCEMIS.DEF'
        include 'inclib:pasiosubs.def'

        ! CONSTANT DEFINITIONS
        INTEGER*4   WRITE_FILE
        PARAMETER   (WRITE_FILE = 2)

        ! PROTOTYPES
        LOGICAL     GET_PAY_TYPE

        ! PARAMETERS
        INTEGER*4   DIVWIN(PAGDIV)
        INTEGER*4   EXTNUM
        INTEGER*4   TICNUM
        INTEGER*4   SERNUM
        INTEGER*4   FRANUM
        INTEGER*4   INDPAS
        INTEGER*4   PLENTN
        INTEGER*4   TOTPAYAMT
        
        ! LN TAX CONFIG VALUES
        INTEGER*4   BASAMT,RFNAMT,TAXPER !V05

        !FUNCTIONS
        INTEGER*4   PAS_ROUND_VALUE

        ! STRUCT WITH DATA EMISSION
        RECORD /STCEMIS/ RECEMI

        ! LOCAL VARIABELS
        INTEGER*4   INDDIV, IDTAIL
        INTEGER*4   QTDFRA

        INTEGER*8   PRZAMT,TOTPRZ
        
        INTEGER*8   SHRAMT,TOTNETPRZ,TAXAMT !V05
        LOGICAL     HASTAX /.FALSE./        !V05

        LOGICAL     WSER,FPOP,FEXT
        LOGICAL     TCKWIN
        
        record /stpasrec/ pasrec         

        COMMON /WINPAS/ WSER,FPOP,FEXT

        CALL FASTSET(0,  VALREC,     VALLEN)
        CALL FASTSET(0, VDETAIL, VPLEN*VMAX)

        ! LOOP TO VERIFY IF THIS TICKET IS PRIZED
        IDTAIL = 0
        TOTPRZ = 0
        TOTNETPRZ = 0    !V05
        TAXAMT = 0       !V05
        DO INDDIV = 1, PAGDIV
!---------IF DIVWIN IS TRUE, IT IS A PRIZE
          IF (DIVWIN(INDDIV) .GT. 0) THEN
            IF (VALREC(VSTAT) .EQ. VNOWIN) THEN
              VALREC(VEXP)    = EXTNUM
              VALREC(VGAM)    = GTNTAB(TPAS, INDPAS)
              VALREC(VGTYP)   = TPAS
              VALREC(VGIND)   = INDPAS
              VALREC(VWCDC)   = DAYCDC
              VALREC(VPRGCDC) = RECEMI.STCPRGCDC
              VALREC(VEXTR)   = EXTNUM
              VALREC(VTCKT)   = TICNUM
              VALREC(VSERN)   = SERNUM
              VALREC(VPFRAC)  = FRANUM
              VALREC(VSTER)   = PASREC.AGT              
              if(pasrec.stat.gt.pbilnot) then   ! Online
                VALREC(VSCDC)   = PASREC.CDC
                VALREC(VSSER)   = PASREC.SERIAL
                VALREC(VPASTYP) = VPASONL
              else
                VALREC(VVALN)   = PASREC.CONTROL
                VALREC(VPASTYP) = VPASOFF
              endif
            ENDIF

            PRZAMT = RECEMI.STCSHV(INDDIV)                      !REGULAR PRIZE
            QTDFRA = RECEMI.STCNOFFRA

            IF(FPOP) QTDFRA = 1                    

            IF(FPOP.OR.FEXT) THEN                              !POPULAR OR EXTRAORDINARIA
              IF(.NOT.WSER.AND.INDDIV.LE.PAGEDV) THEN
                IF(RECEMI.STCEXSHV(INDDIV).GT.0) THEN
                  PRZAMT = RECEMI.STCEXSHV(INDDIV)        !EXTRA PRIZE
                ENDIF
              ENDIF
            ENDIF
!-------->>V05 ------ NET PRIZE AMOUNT CALCULATION -----------------------------
            SHRAMT = PAS_ROUND_VALUE(PRZAMT) / QTDFRA !SHARE AMOUNT
            TAXAMT = 0       !RESET TAX AMOUNT
            HASTAX = .FALSE. !RESET FLAG
            IF (SHRAMT .GT. BASAMT) THEN
            	HASTAX = .TRUE.
              ! TAX AMOUNT (ADD 0.50 TO GET THE ROUNDED VALUE; REMOVE IT IF YOU WANT THE TRUNCATED VALUE)
              ! TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10) + 0.50D0))
              TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10)))
            ENDIF
            IF (HASTAX) THEN
              TOTNETPRZ = TOTNETPRZ + SHRAMT * DIVWIN(INDDIV) - TAXAMT * DIVWIN(INDDIV)
            ELSE
              TOTNETPRZ = TOTNETPRZ + SHRAMT * DIVWIN(INDDIV)
            ENDIF
!-------- V05<<-----------------------------------------------------------------
            TOTPRZ = TOTPRZ + ( (PRZAMT/QTDFRA) * DIVWIN(INDDIV) )
            IDTAIL = IDTAIL + 1
            VDETAIL(VDRW, IDTAIL) = EXTNUM
            VDETAIL(VDIV, IDTAIL) = INDDIV
            VDETAIL(VSHR, IDTAIL) = VDETAIL(VSHR, IDTAIL) + DIVWIN(INDDIV)
            VALREC(VPZOFF) = IDTAIL
          ENDIF                         ! IF DIVWIN IS GT ZERO, IT IS A PRIZE
        ENDDO
!=======IF IT IS A PRIZE, SO WE MUST WRITE IT
        IF (IDTAIL .GT. 0) THEN
          VALREC(VPAMT) = PAS_ROUND_VALUE(TOTPRZ)
          VALREC(VOPSAMT) = TOTNETPRZ !TOTAL NET PRIZE AMOUNT !V05
          PLENTN = 6          
!---------UPDATE TPF STATUS-----------------------------------------------------
          IF(pasrec.stat.eq.pbilnot) then
            VALREC(VSTAT) = VDEL
          ELSEIF(pasrec.stat.eq.pbilonl) then            
            VALREC(VSTAT) = VDEL                      
          ELSEIF(pasrec.stat.eq.pbilson) then
            IF (GET_PAY_TYPE(VDETAIL,VALREC(VPZOFF),RECEMI,VALREC(VGAM))) THEN
              VALREC(VSTAT) = VPRPAY
            ELSE
              VALREC(VSTAT) = VUNCSH
            ENDIF
            TOTPAYAMT = TOTPAYAMT + VALREC(VPAMT)  
            pasrec.stat = pbilwon                                        
            PLENTN = 3
          ELSEIF(pasrec.stat.eq.pbilcon) then
            VALREC(VSTAT) = VCXL
            pasrec.stat = pbilxon
!---------ELSEIF(pasrec.stat.eq.pbilwon) then-----------------------------------  
!---------ELSEIF(pasrec.stat.eq.pbilxon) then-----------------------------------
          ELSEIF(pasrec.stat.eq.pbiloff) then
            VALREC(VSTAT) = VDEL             
          ELSEIF(pasrec.stat.eq.pbilsof) then
            IF (GET_PAY_TYPE(VDETAIL,VALREC(VPZOFF),RECEMI,VALREC(VGAM))) THEN
              VALREC(VSTAT) = VPRPAY
            ELSE
              VALREC(VSTAT) = VUNCSH
            ENDIF
            TOTPAYAMT = TOTPAYAMT + VALREC(VPAMT)  
            pasrec.stat = pbilwof         
            PLENTN = 4            
          ELSEIF(pasrec.stat.eq.pbilcof) then
            VALREC(VSTAT) = VCXL            
            pasrec.stat = pbilxof            
            PLENTN = 5
!---------ELSEIF(pasrec.stat.eq.pbilwof) then-----------------------------------
!---------ELSEIF(pasrec.stat.eq.pbilxof) then-----------------------------------
          ELSEIF(pasrec.stat.eq.pbilrof) then
            VALREC(VSTAT) = VCXL            
            pasrec.stat = pbilkof               
!---------ELSEIF(pasrec.stat.eq.pbilkof) then-----------------------------------            
          ELSE
            VALREC(VSTAT) = VDEL ! ERROR            
            TYPE*,IAM(),'Unexpected status in ticket #',TICNUM,SERNUM,FRANUM,' ST> ',pasrec.stat
          ENDIF
          TCKWIN = .TRUE.   
!---------ACCUMULATE BY VALUE, FOR REPORT PLANO REAL----------------------------
          CALL ACC_PLREAL(VALREC(VPAMT),PLENTN)
          CALL DPASLOG(VALREC, VDETAIL)
          CALL PASLOG(VALREC, V4BUF_PAS)
          CALL PAS_WINLOD(WRITE_FILE, V4BUF_PAS)
        ENDIF
        RETURN
        END
C**************************************************
C SUBROUTINE ACCUMULATE THE PASSIVE LOTTERY WINNERS
C**************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE ACC_PRZ_FOUND(DIVWIN, TOTPRZ, TOTEXPRZ, TOTAMT, TOTBIL, RECEMI)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:STCEMIS.DEF'

        ! PARAMETERS
        INTEGER*4   DIVWIN(PAGDIV),TOTPRZ(PAGDIV), TOTEXPRZ(PAGEDV)
        INTEGER*4   TOTBIL

        INTEGER*4   QTDFRA
        INTEGER*8   PRZAMT, TOTAMT

        LOGICAL     WSER,FPOP,FEXT

        COMMON /WINPAS/ WSER,FPOP,FEXT

        ! STRUCT WITH DATA EMISSION
        RECORD /STCEMIS/ RECEMI

        ! LOCAL VARIABLES
        LOGICAL     FLGBIL

        INTEGER*4   INDBUF

        FLGBIL = .FALSE.
        DO INDBUF=1, PAGDIV

          ! IF THIS TICKET WON IN THIS DIVISION
          IF (DIVWIN(INDBUF) .GT. 0) THEN

            PRZAMT = RECEMI.STCSHV(INDBUF)                                        !REGULAR PRIZE
            QTDFRA = RECEMI.STCNOFFRA

            IF (FPOP) QTDFRA = 1

            IF ((FPOP.OR.FEXT) .AND. .NOT. WSER .AND. INDBUF. LE. PAGEDV) THEN           
                IF (RECEMI.STCEXSHV(INDBUF).GT.0) THEN
                     PRZAMT = RECEMI.STCEXSHV(INDBUF)                            !EXTRA PRIZE
                     TOTEXPRZ(INDBUF) = TOTEXPRZ(INDBUF) + DIVWIN(INDBUF)         
                ELSE
                     TOTPRZ(INDBUF) = TOTPRZ(INDBUF) + DIVWIN(INDBUF) 
                ENDIF
            ELSE
                TOTPRZ(INDBUF) = TOTPRZ(INDBUF) + DIVWIN(INDBUF) 
            ENDIF

            ! ADD IN AMOUNT OF PRIZES THAT WILL BE PAID
            TOTAMT = TOTAMT +
     *                  ((PRZAMT/QTDFRA) *
     *                    DIVWIN(INDBUF))
             

            FLGBIL = .TRUE.

          ENDIF

        ENDDO

        ! IF EXIST PRIZE FOR THIS TICKET, ADD IN TOTALS OF TICKETS THAT
        ! WILL BE WRITED
        IF (FLGBIL) TOTBIL = TOTBIL + 1

        RETURN

        END

C********************************************************
C SUBROUTINE TO SHOW THE PASSIVE LOTTERY WINNERS/DIVISION
C********************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE SHOW_WIN_DIV(TOTPRZ, TOTEXPRZ, TOTAMT, IMAXSER, TOTBIL, RECEMI)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:STCEMIS.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'

        ! PARAMETERS
        INTEGER*4   TOTPRZ(PAGDIV), IMAXSER, TOTBIL, TOTEXPRZ(PAGEDV)

        INTEGER*4   QTDFRA, NUMSER
        INTEGER*8   TOTAMT

        ! STRUCT WITH DATA EMISSION
        RECORD /STCEMIS/ RECEMI

        !FUNCTIONS
        INTEGER*4   PAS_ROUND_VALUE

        ! LOCAL VARIABLES
        INTEGER*4   INDBUF, BIGTOT, MEMPRZ

        LOGICAL     WSER,FPOP,FEXT

        COMMON /WINPAS/ WSER,FPOP,FEXT

        TYPE*,IAM(),'*'
        TYPE*,IAM(),'* CONFERE DISTRIBUICAO DE PREMIOS POR DIVISAO'
        TYPE*,IAM(),'* COMPARA QUANTIDADE CADASTRADA COM QUANTIDADE APURADA'
        TYPE*,IAM(),'*'

        TYPE*,IAM(), 'NUMERO DE SERIES APURADAS   -> ', IMAXSER
        TYPE*,IAM()

        BIGTOT = 0
        DO INDBUF=1, RECEMI.STCDIV
 

          QTDFRA =  RECEMI.STCNOFFRA
          NUMSER =  IMAXSER

          ! ACCOUNT FOR TEN
          IF (FPOP) THEN
             QTDFRA = 1
             NUMSER = 1
          ENDIF
        
          IF (FEXT) THEN
              IF (INDBUF.LE.PAGEDV) THEN
                  IF (RECEMI.STCEXSHR(INDBUF).NE.0)  THEN
                      NUMSER = NUMSER - 1
                   ENDIF
              ENDIF
          ENDIF

          MEMPRZ = RECEMI.STCSHR(INDBUF) * QTDFRA * NUMSER

          ! CHECK IF WE HAVE TO VERIFY THE TOTALS
          IF ( MOD(RECEMI.STCNUMTCK, 10000) .EQ. 0 ) THEN

            ! TEST IF WINNERS IN MEMORY ARE EQUAL THE SELECTED WINNERS IN FILE
            IF (MEMPRZ .EQ. TOTPRZ(INDBUF)) THEN

              IF (MEMPRZ .GT. 0) THEN
                TYPE*,IAM(),
     *          'PREMIOS LOTARIA NACIONAL DIVISAO ',INDBUF,' -> ', TOTPRZ(INDBUF)

              ENDIF
            ELSE

              TYPE *, IAM(),
     *         'DIFERENCA ENTRE QUANTIDADE CADASTRADA E APURADA DIVISAO ', INDBUF
              TYPE *, IAM(), ' CADASTRADA -> ', RECEMI.STCSHR(INDBUF), 
     *                     ' APURADA -> '   , TOTPRZ(INDBUF)
              CALL GPAUSE
            ENDIF

          ELSE

            IF (MEMPRZ .GT. 0) THEN
              TYPE*,IAM(),
     *          'PREMIOS LOTARIA NACIONAL DIVISAO ',INDBUF,' -> ', TOTPRZ(INDBUF)

            ENDIF
          ENDIF

          BIGTOT = BIGTOT + TOTPRZ(INDBUF)

        ENDDO


        IF (FPOP.OR.FEXT .AND. RECEMI.STCEXSHR(1).GT.0 ) THEN   !IF POPULAR OR EXTRAORDINARIA, VERIFY THE EXTRA DIVISONS
            TYPE*,IAM(),' '
            TYPE*,' ************ SERIE NAO SORTEADA ************'
            QTDFRA = RECEMI.STCNOFFRA
            NUMSER = IMAXSER

            IF (FPOP) THEN
               QTDFRA = 1
               NUMSER = 1
            ENDIF
            IF (FEXT) THEN
               NUMSER = NUMSER - 1
            ENDIF

            DO INDBUF=1,PAGEDV

               ! ACCOUNT FOR TEN
               MEMPRZ = RECEMI.STCEXSHR(INDBUF) * QTDFRA * NUMSER

               IF (MEMPRZ .EQ. TOTEXPRZ(INDBUF)) THEN

                   IF (MEMPRZ .GT. 0) THEN
                       TYPE*,IAM(),
     *                   'PREMIOS LOTARIA NACIONAL DIVISAO ',INDBUF,' -> ', TOTEXPRZ(INDBUF)

                   ENDIF
               ELSE

                   TYPE *, IAM(),
     *                            'DIFERENCA ENTRE QUANTIDADE CADASTRADA E APURADA DIVISAO ', INDBUF
                   TYPE *, IAM(), ' CADASTRADA -> ', RECEMI.STCEXSHR(INDBUF), 
     *                            ' APURADA -> '   , TOTEXPRZ(INDBUF)

                   CALL GPAUSE
               ENDIF
            
               BIGTOT = BIGTOT + TOTEXPRZ(INDBUF)
            ENDDO
        ENDIF
        


        TYPE*,IAM()
        TYPE*,IAM(),'TOTAL DE DECIMOS PREMIADOS                   -> ', BIGTOT
        TYPE*,IAM(),'TOTAL DE BILHETES PREMIADOS                  -> ', TOTBIL
        TYPE*,IAM(),
     *    'VALOR DOS PREMIOS  -> ',CMONY (PAS_ROUND_VALUE(TOTAMT), 14, BETUNIT)

        RETURN
        END

C*************************************************
C SUBROUTINE TO CREATE AND CLEAR WORK FILE VPW.FIL
C*************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE INIC_FILES(EMINUM, INDPAS)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'
        INCLUDE '(LIB$ROUTINES)'
C
C ROUTINE PARAMETERS
C
        INTEGER*4   EMINUM, INDPAS
C
C LOCAL VARIABLES
C
        INTEGER*4    STATUS
        CHARACTER*20 WORKFILE
        LOGICAL      ISTHERE

        EQUIVALENCE (SFNAMES(1,VPW),WORKFILE)
C
C VERIFY IF WORK FILE EXISTS....IF TRUE, DELETE IT.
C
        INQUIRE (FILE=WORKFILE,EXIST=ISTHERE)

        IF (ISTHERE) THEN
           STATUS = LIB$DELETE_FILE(WORKFILE,';*',,,,,,,)
           IF (.NOT.STATUS) THEN
              TYPE*,IAM(),' *** ERROR DELETING FILE ',WORKFILE
              CALL GPAUSE()
           ENDIF
        ENDIF
C
C CREATE VALIDATION PASSIVE LOTTERY WORK FILE
C
        CALL PAS_CRTFIL_VPF(SFNAMES(1,VPW), EMINUM, INDPAS, STATUS)

        IF (STATUS.NE.0) THEN
            CALL FILERR(SFNAMES(1,VPW), 1, STATUS, 0)
        ENDIF

        RETURN
        END


C**********************************
C SUBROUTINE TO EXECUTE MERGE FILES
C**********************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE EXEC_MERGE(EMINUM, INDPAS)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSDEFINE.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'

C
C ROUTINE PARAMETERS
C
        INTEGER*4   EMINUM, INDPAS
C
C LOCAL VARIABLES
C
        CHARACTER*20    FILCHAR

        INTEGER*4       FILNAM(5)
        INTEGER*4       STATUS

C
C CREATE VALIDATION PASSIVE LOTTERY FILE
C
        CALL BUILD_FILENAME(SFNAMES(1,VPF), INDPAS, EMINUM, FILCHAR, FILNAM)

        CALL PAS_CRTFIL_VPF(FILNAM, EMINUM, INDPAS, STATUS)

        ! SUBROUTINE TO MERGE VPF.FIL/VPW.FIL
        CALL MRGVPF(EMINUM,INDPAS)

        RETURN

        END

C************************************
C SUBROUTINE TO MERGE VPF.FIL/VPW.FIL
C************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE MRGVPF(EMINUM,INDPAS)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:PRMVPF.DEF'
        INCLUDE 'INCLIB:RECVLW.DEF'
        INCLUDE 'INCLIB:HSHCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'
C
C ROUTINE PARAMETERS
C
        INTEGER*4 EMINUM,INDPAS
C
C LOCAL VARIABLES
C
        CHARACTER*20 FILCHAR

        INTEGER*4    VSIZE, TUBSIZ
        PARAMETER    (VSIZE    = 4000000)
        PARAMETER    (TUBSIZ   = I4BUCSIZ*7)

        INTEGER*4    FILNAM(5)
        INTEGER*4    FDB(7),NFDB(7),BIGVLF(VSIZE)
        INTEGER*4    NEWBUF(TUBSIZ)
        INTEGER*4    COUNT, BLOCK, CURBLK,LOWBKT,WINERCNT
        INTEGER*4    SECTORS, TOTBLK, TOP, BOT,BUCKET,HIGHBKT
        INTEGER*4    ST, MEMFULL,S,SCANS,MAXBKT
ccc     DATA SCFNAM/'SCF.','FIL '/

        INTEGER*4 LODREC(I4VLWSIZ)
        INTEGER*4 XOFF,NUMCHUNK,LENGTH
        INTEGER*4  HSH_GET_RECORD_LEN(0:15)
        DATA  HSH_GET_RECORD_LEN/1,2,3,4,0,0,0,0,0,0,0,0,0,0,0,0/

        CALL COPYRITE
        CALL SNIF_AND_WRKSET
C
C MERGE VPW, VPF
C
        CALL BUILD_FILENAME(SFNAMES(1,VPF), INDPAS, EMINUM, FILCHAR, FILNAM)

        CALL IOPEN(FILNAM, VPF, VPFLEN*2, VFSCDC, VFSSER*2-1, ST)

        IF (ST.NE.0) THEN
           CALL FILERR(FILNAM, 1, ST, 0)
        ENDIF

        CALL IINIB(VPF,BIGVLF,VSIZE)
        CALL ITUBSIZE(VPF,TUBSIZ)

        CALL OPENW(VPW,SFNAMES(1,VPW),4,0,0,ST)
        CALL IOINIT(NFDB,VPW,I4VLWSIZ*4)

        IF (ST.NE.0) THEN
           CALL FILERR(SCFSFN(1,VPW), 1, ST, 0)
        ENDIF
C
C ESTIMATE NUMBER OF NEW WINNERS
C
        CALL GETSIZ_USED(VPW,SECTORS)
        TOTBLK=(SECTORS*64)/I4VLWSIZ
        BOT=0
        TOP=TOTBLK+1
20      CONTINUE
        CURBLK=BOT+(TOP-BOT)/2
        IF((TOP-BOT)/2.EQ.0) GOTO 30
        CALL READW(NFDB,CURBLK,LODREC,ST)

        IF (ST.NE.0) THEN
           CALL FILERR(SCFSFN(1,VPW), 2, ST, CURBLK)
        ENDIF

        IF(LODREC(VFSSER).NE.0) THEN
          BOT=CURBLK
        ELSE
          TOP=CURBLK
        ENDIF
        GOTO 20
30      CONTINUE
        WINERCNT=CURBLK*(I4VLWSIZ/VPFLEN)
C
C ESTIMATE NUMBER OF RECORDS THAT WILL FIT IN MEMORY BUFFER
C
        MEMFULL=(VSIZE*100)/120
        MEMFULL=MEMFULL-BIGVLF(BUFFRE)
        MEMFULL=MEMFULL/(VPFLEN+1)
C
C ESTIMATE NUMBER OF SCANS REQUIRED TO LOAD ALL WINNERS
C
        COUNT=0
        LOWBKT=0
        HIGHBKT=0
        SCANS=WINERCNT/MEMFULL+1
        CALL IMXBLK(VPF,MAXBKT)
        TYPE*,IAM(),SCANS,' scans required to load winners file'
C
C START FILE SCANS
C AND DETERMINE LOW AND HIGH BUCKET FOR THIS SCAN.
C
        DO 1000 S=1,SCANS

          TYPE*,IAM(),' performing scan ',S

          BLOCK=1
          LOWBKT=HIGHBKT+1
          HIGHBKT=((MAXBKT+SCANS-1)/SCANS)*S
C
C
40        CONTINUE
          CALL READW(NFDB,BLOCK,LODREC,ST)

          IF (ST.NE.0) THEN
             CALL FILERR(SCFSFN(1,VPW), 2, ST, BLOCK)
          ENDIF

          BLOCK=BLOCK+1

          XOFF = 1
100       CONTINUE
          IF(XOFF.GT.I4VLWSIZ-(VPFLEN*VPFMAXREC)+1)GOTO 40
          NUMCHUNK = ISHFT( LODREC(XOFF+VFSSER-1),-30)
          NUMCHUNK = HSH_GET_RECORD_LEN(NUMCHUNK)
          LENGTH = NUMCHUNK*VPFLEN
          IF(LODREC(XOFF+VFSSER-1).EQ.0) GOTO 200
          CALL IGTBLK(LODREC(XOFF+VFSSER-1),VPF,BUCKET,NUMCHUNK)
          IF(BUCKET.LT.LOWBKT) GOTO 50
          IF(BUCKET.GT.HIGHBKT) GOTO 50
C
C
          COUNT=COUNT+1
          CALL IWRIBF(LODREC(XOFF),VPF,BIGVLF,NEWBUF,ST)

          IF (ST.NE.0) THEN
             CALL FILERR(SCFSFN(1,VPW), 3, ST, 0)
          ENDIF

50        CONTINUE
          XOFF = XOFF + LENGTH
          GOTO 100
C
C FLUSH MEMORY BUFFER
C
200       CONTINUE
          CALL IFLUSH(VPF,BIGVLF,NEWBUF,ST)

          IF (ST.NE.0) THEN
             CALL FILERR(%REF(FILNAM), 3, ST, 0)
          ENDIF


1000    CONTINUE
C
C
        CALL ICLOSB(VPF,BIGVLF,NEWBUF,ST)

        IF (ST.NE.0) THEN
           CALL FILERR(%REF(FILNAM), 4, ST, 0)
        ENDIF

        CALL CLOSEFIL(FDB)
        TYPE*,IAM(),'VPF merge complete'

        RETURN

900     FORMAT(1X,5A4,' open error > ',I4)
901     FORMAT(1X,'Copying ',5A4,' to ',5A4)
902     FORMAT(1X,5A4,' read error > ',I4,' Block > ',I5)
903     FORMAT(1X,5A4,' write error > ',I4,' Block > ',I5)
904     FORMAT(1X,5A4,' Iopen error > ',I4)
905     FORMAT(1X,5A4,' Iwrite error > ',I4)
906     FORMAT(1X,I6,' records copied from ',5A4)
907     FORMAT(1X,5A4,' Iclosb error > ',I4)

        END

C************************************
C SUBROUTINE TO MERGE VPF.FIL/VPW.FIL
C************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE SET_STAT_GAM(INDPAS, TOTPRZ, TOTEXPRZ, EMINUM, INDEMI, RECEMI)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SYSDEFINE.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DPAREC.DEF'
        INCLUDE 'INCLIB:STCEMIS.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        include 'inclib:pascom.def'

        ! PARAMETERS
        INTEGER*4 TOTPRZ(PAGDIV), TOTEXPRZ(PAGEDV)
        INTEGER*4 INDPAS, EMINUM, INDEMI

        ! CONSTANT DEFINITIONS
        INTEGER*4 UNIT
        PARAMETER (UNIT = 21)
        
        INTEGER*4 AMT,CNT
        PARAMETER (AMT = 1)
        PARAMETER (CNT = 2)        

        ! LOCAL VARIABLES
        INTEGER*4 FDB(7)
        INTEGER*4 GAMNUM, STATUS, INDDIV, QTDFRA, NUMSER
        INTEGER*4 CBUF(CDLEN),BITMAP(2)
        logical   boob
        integer*4 xval(6)
        integer*4 xind

        LOGICAL     WSER,FPOP,FEXT

        COMMON /WINPAS/ WSER,FPOP,FEXT
        
        INTEGER*4   PLREAL(6,50)
        COMMON /PLCOM/ PLREAL        

        ! STRUCT WITH DATA EMISSION INFORMATION
        RECORD /STCEMIS/ RECEMI

        ! OPEN GAME FILE
        GAMNUM = GTNTAB(TPAS, INDPAS)
        CALL OPENW(UNIT,GFNAMES(1,GAMNUM),4,0,0,STATUS)
        IF(STATUS .NE. 0) THEN
             CALL FILERR(GFNAMES(1,GAMNUM),1,STATUS,0)
        ELSE    
          
          ! READ GAME FILE
          CALL IOINIT(FDB,UNIT,DPASEC*256)
          CALL READW(FDB,(EMINUM-PAS_DRW_OFFSET),DPAREC,STATUS)
          IF(STATUS .NE. 0) THEN
            CALL FILERR(GFNAMES(1,GAMNUM),2,STATUS,(EMINUM-PAS_DRW_OFFSET))

            CALL CLOSEFIL(FDB)
          ELSE

CC          ! SET GAME STATUS TO FINAL
CC          DPASTS = GFINAL

            ! WRITE NEW NUMBERS OF WINNERS PER DIVISION
            DO INDDIV = 1,  PAGDIV

               QTDFRA = RECEMI.STCNOFFRA
               NUMSER = DPANUMSER

               IF (FPOP) THEN
                  QTDFRA = 1
                  NUMSER = 1
               ENDIF

               IF (FEXT) THEN
                   IF (INDDIV.LE.PAGEDV) THEN
                      IF (DPAEXSHR(INDDIV).NE.0)  THEN
                         NUMSER = NUMSER - 1
                      ENDIF
                   ENDIF
               ENDIF

               DPASHR(INDDIV) = (TOTPRZ(INDDIV)/QTDFRA) / NUMSER 
               PASSHR(INDDIV,INDEMI,INDPAS) = DPASHR(INDDIV)

            ENDDO

            IF (FPOP) THEN
                DO INDDIV=1,PAGEDV
                   DPAEXSHR(INDDIV) = TOTEXPRZ(INDDIV) 
                   PASEXSHR(INDDIV,INDEMI,INDPAS) = DPAEXSHR(INDDIV)
                ENDDO
            ENDIF

            IF (FEXT) THEN
                NUMSER = DPANUMSER
                IF (NUMSER.GT.1) THEN
                    NUMSER = NUMSER - 1
                ENDIF
                DO INDDIV=1,PAGEDV
                   DPAEXSHR(INDDIV) = (TOTEXPRZ(INDDIV)/RECEMI.STCNOFFRA) / NUMSER 
                   PASEXSHR(INDDIV,INDEMI,INDPAS) = DPAEXSHR(INDDIV)
                ENDDO
            ENDIF

            ! UPDATE CHANGE CDC
            DPACHGCDC = DAYCDC
            PASCHGCDC(INDEMI,INDPAS) = DPACHGCDC
            
!+++++++++++LETS DO A BUBLE TO ORDER THE REAL PRIZES
            boob = .true.
            xind = 1
            do while(boob)
              boob = .false.
              do inddiv = 50,2,-1
                if(plreal(AMT,inddiv).gt.plreal(AMT,inddiv-1)) then
                  xval(1) = plreal(1,inddiv)
                  xval(2) = plreal(2,inddiv)
                  xval(3) = plreal(3,inddiv)
                  xval(4) = plreal(4,inddiv)
                  xval(5) = plreal(5,inddiv)
                  xval(6) = plreal(6,inddiv)                  
                  
                  plreal(1,inddiv) = plreal(1,inddiv-1)
                  plreal(2,inddiv) = plreal(2,inddiv-1)
                  plreal(3,inddiv) = plreal(3,inddiv-1)
                  plreal(4,inddiv) = plreal(4,inddiv-1)
                  plreal(5,inddiv) = plreal(5,inddiv-1)
                  plreal(6,inddiv) = plreal(6,inddiv-1)
                  
                  plreal(1,inddiv-1)= xval(1)
                  plreal(2,inddiv-1)= xval(2)
                  plreal(3,inddiv-1)= xval(3)
                  plreal(4,inddiv-1)= xval(4)
                  plreal(5,inddiv-1)= xval(5)
                  plreal(6,inddiv-1)= xval(6)                  
                  boob = .true.       
                endif
              enddo   
              xind = xind + 1                           
            enddo            
!+++++++++++
            DO INDDIV = 1,PAGDIV+PAGEDV
              DPADIVPLN(1,INDDIV) = PLREAL(1,INDDIV)
              PASDIVPLN(1,INDDIV,INDEMI,INDPAS) = PLREAL(1,INDDIV)
            ENDDO
            
            ! WRITE NEW RECORD
            CALL WRITEW(FDB,(EMINUM-PAS_DRW_OFFSET),DPAREC,STATUS)

            IF (STATUS.NE.0)  THEN
              CALL FILERR(GFNAMES(1,GAMNUM),3,STATUS,0)
            ENDIF

            CALL CLOSEFIL(FDB)
          ENDIF
        ENDIF

C
C SEND COMMAND TO UPDATE MEMORY TO PASVAL STARTS VALIDATIONS
C
        IF (P(SYSTYP).EQ.LIVSYS) THEN
C
C SUPRESS VALIDATION FOR THIS PASSIVE...WHEN WINPAS FINISH ON BACK-UP, OPERATOR ENABLE VALIDATIONS AGAIN
C
           CALL FASTSET(0,CBUF,CDLEN)

           BITMAP(1) = P(SUPGVA)
           BITMAP(2) = P(SUPGVA1)
           CALL BSET(BITMAP,GAMNUM)

           CBUF(1) = SUPGVA
           CBUF(2) = BITMAP(1)
           CBUF(3) = TCPAR
           CBUF(6) = 'SYS '
           CBUF(9) = BITMAP(2)
           CALL RESCMD(CBUF)
C
C SET GAME STATUS TO FINAL
C
           CALL FASTSET(0,CBUF,CDLEN)

           CBUF(1) = 1
           CBUF(2) = GFINAL
           CBUF(3) = TCPAS
           CBUF(6) = 'SYS '
           CBUF(8) = INDPAS
           CBUF(9) = EMINUM                        ! EMISSION NUMBER
           CALL RESCMD(CBUF)
        ENDIF

        RETURN
        END

C********************************************
C SUBROUTINE TO GET EMISSION DATA INFORMATION
C********************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE GET_EMISSION(INDEMI, INDPAS, RECEMI, WEEK, YEAR)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SYSDEFINE.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DPAREC.DEF'
        INCLUDE 'INCLIB:STCEMIS.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
C
C PARAMETERS
C
        INTEGER*4   INDEMI, INDPAS
C
C STRUCT WITH DATA EMISSION
C
        RECORD /STCEMIS/ RECEMI

        LOGICAL     WSER,FPOP,FEXT

        COMMON /WINPAS/ WSER,FPOP,FEXT
C
C LOCAL VARIABLES
C
        INTEGER*4   INDARR, DIV, WEEK, YEAR


C
C CLEAR STRUCT OF EMISSION DATA INFORMATION
C
        CALL FASTSET(0, RECEMI, SIZEOF(RECEMI)/4)

        CALL GAMLOGPAS(INDEMI, INDPAS, DPAREC, PASSTS)

        CALL GETPASDRW(DPADRAW,WEEK,YEAR)
C
        ! EMISSION NUMBER
        RECEMI.STCEMIS = DPAEMIS

        ! EMISSION TYPE
        RECEMI.STCEMT = DPAEMT

        ! NUMBER OF TICKETS/EMISSION
        RECEMI.STCNUMTCK = DPANUMTCK

        ! # OF SERIES
        RECEMI.STCNUMSER = DPANUMSER

        ! WINNING SERIE
        RECEMI.STCWSER = DPAWSER

        ! NUMBER OF DIVISIONS
        RECEMI.STCDIV = DPADIV

        ! BEGIN SALES DATE
        RECEMI.STCBSD = DPABSD

        ! END SALES DATE
        RECEMI.STCESD = DPAESD

        ! NUMBER OF FRACTIONS
        RECEMI.STCNOFFRA = DPANOFFRA

        ! PURGING CDC OF THIS EMISSION
        RECEMI.STCPRGCDC = DPAPRGCDC

        ! REGULAR WINNING NUMBERS
        DO DIV=1, DPADIV
           DO INDARR=1,PAGNBR
              RECEMI.STCWIN(INDARR,DIV) = DPAWIN(INDARR,DIV)
           ENDDO
        ENDDO

        ! SHARE VALUE
        DO INDARR=1, DPADIV
          RECEMI.STCSHV(INDARR) = DPASHV(INDARR)
        ENDDO

        ! EXTRA SHARE VALUE
        DO INDARR=1, PAGEDV
          RECEMI.STCEXSHV(INDARR) = DPAEXSHV(INDARR)
        ENDDO

        ! SHARES
        DO INDARR=1, DPADIV
          RECEMI.STCSHR(INDARR) = DPASHR(INDARR)
        ENDDO

        ! ERXTRA SHARES
        DO INDARR=1, PAGEDV
          RECEMI.STCEXSHR(INDARR) = DPAEXSHR(INDARR)
        ENDDO

        ! NUMBER OF WINNING NUMBERS
        DO INDARR=1, DPADIV
          RECEMI.STCWNUM(INDARR) = DPAWNUM(INDARR)
        ENDDO

        ! PRIZE TYPE
        DO INDARR=1,DPADIV
          RECEMI.STCTYP(INDARR) = DPATYP(INDARR)  
        ENDDO

        ! # OF DIGITS
        DO INDARR=1, DPADIV
          RECEMI.STCDIG(INDARR) = DPADIG(INDARR)
        ENDDO

        ! CROSS REFERENCE
        DO INDARR=1, DPADIV
          RECEMI.STCIDNUM(INDARR) = DPAIDNUM(INDARR)
        ENDDO

        FPOP = .FALSE.
        FEXT = .FALSE.
        IF (INDPAS.EQ.PSBPOP) THEN              !POPULAR
            FPOP = .TRUE.
        ELSEIF (RECEMI.STCEMT.EQ.EM_EXT.AND.
     *          RECEMI.STCWSER.GT.0          ) THEN   !EXTRAORDINARIA
            FEXT = .TRUE.
        ENDIF

        RETURN
        END

C**********************************************************************
C**** FUNCTION TO RETURN CHECK THE PRIZE CAN OR NOT BE PAY FOR AN AGENT
C**********************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        LOGICAL FUNCTION GET_PAY_TYPE(VDETAIL, TOTDIV, RECEMI, GNUM)
        IMPLICIT NONE

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMVAL.DEF'
        INCLUDE 'INCLIB:STCEMIS.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'

        ! PARAMETERS
        INTEGER*4   TOTDIV, GNUM

        !FUNCTIONS
        INTEGER*4   PAS_ROUND_VALUE

        ! STRUCT WITH DATA EMISSION
        RECORD /STCEMIS/ RECEMI

        LOGICAL     WSER,FPOP,FEXT

        COMMON /WINPAS/ WSER,FPOP,FEXT

        ! LOCAL VARIABLES
        INTEGER*4   IDTAIL,DIVISION,QTDFRA,PRZAMT

        GET_PAY_TYPE = .FALSE.
        IDTAIL = 1
        DO WHILE ( (IDTAIL .LE. TOTDIV) .AND. (.NOT. GET_PAY_TYPE) )

          DIVISION = VDETAIL(VDIV,IDTAIL)
          PRZAMT   = PAS_ROUND_VALUE(RECEMI.STCSHV(DIVISION))
          QTDFRA   = RECEMI.STCNOFFRA
          IF (FPOP) QTDFRA = 1

          IF (FPOP.OR.FEXT) THEN
             IF (DIVISION.LE.PAGEDV) THEN
                 IF (RECEMI.STCEXSHR(DIVISION).GT.0.AND..NOT.WSER) THEN
                     PRZAMT = PAS_ROUND_VALUE(RECEMI.STCEXSHV(DIVISION))
                 ENDIF
             ENDIF
          ENDIF
          
          IF(PRZAMT/QTDFRA .GT.
     *       REDMAX(GNUM) .AND.
     *       REDMAX(GNUM).GT.0) GET_PAY_TYPE = .TRUE.

          IDTAIL = IDTAIL + 1
        ENDDO

        RETURN
        END

C**************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE ACC_PLREAL(AMOUNT,PLENTN)
        IMPLICIT NONE
C**************************************************

        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
        INTEGER*4 AMOUNT
C
        INTEGER*4 PLREAL(6,50)
        INTEGER*4 PLENTN
C
        INTEGER*4 AMT,CNT
        PARAMETER (AMT = 1)
        PARAMETER (CNT = 2)
C
        INTEGER*4 OFF,NEXT,I

        LOGICAL ERROR /.FALSE./

        COMMON /PLCOM/ PLREAL,ERROR

        IF (ERROR) RETURN
C
C FIND THE POSITION ON MEMORY TABLE
C       
        OFF  = -1
        NEXT = -1
        DO I=1,50
            IF (PLREAL(AMT,I).EQ.AMOUNT) THEN
                OFF = I
                EXIT
            ELSEIF (PLREAL(AMT,I).EQ.0) THEN
                NEXT = I
                EXIT
            ENDIF
        ENDDO

        IF (OFF.EQ.-1) THEN
            IF (NEXT.EQ.-1) THEN
                TYPE*,IAM(),'>>> MEMORY TABLE IS FULL FOR REPORT PLANO REAL !!'
                TYPE*,IAM(),'>>> ENTER CONT, TO CONTINUE WITH WINPAS AND'
                TYPE*,IAM(),'>>> DONT GENERATE PLANO REAL REPORT'
                CALL GPAUSE()
                ERROR = .TRUE.
                RETURN
            ENDIF

            PLREAL(AMT,NEXT) = AMOUNT

            OFF = NEXT
        ENDIF

        PLREAL(CNT,OFF)    = PLREAL(CNT,OFF) + 1
        IF(PLENTN.GT.0) PLREAL(PLENTN,OFF) = PLREAL(PLENTN,OFF) + 1

        RETURN
        END



C**************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRT_PLREAL(GNUM,WEEK,YEAR)
        IMPLICIT NONE
C**************************************************

        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
!       INCLUDE '($SORDEF)'

C-----------------------------------
C     SORT STUFF
!
!       BYTE            SORTTYPE                ! Sort Process f/Record Interface
!       INTEGER*2       KEYBUFFER(5) /5*0/    ! 
!       INTEGER*2       LRL                     ! longest record length in bytes
!
!       INTEGER   SOR$BEGIN_SORT,                 SOR$RELEASE_REC,
!    *            SOR$RETURN_REC,                 SOR$SORT_MERGE,
!    *            SOR$END_SORT
!
!       EXTERNAL  SOR$K_REC_SOR,                  SOR$GK_RECORD,
!    *            SOR$K_REC_OUT,                  SOR$K_NODES,
!    *            SOR$K_REC_INP,                  DSC$K_DTYPE_Z,
!    *            SS$_NORMAL
C
C-----------------------------------
C
        INTEGER*4 PLREAL(6,50)
        LOGICAL ERROR
C
        INTEGER*4 AMT,CNT
        PARAMETER (AMT = 1)
        PARAMETER (CNT = 2)
C
        INTEGER*4 REPLUN,ST,I,TOTCNT,TOTREC,GNUM
        INTEGER*4 WEEK,YEAR,PAGE
        INTEGER*8 TOTAMT
        CHARACTER*23 REPNAM
!       CHARACTER*132 STR

        COMMON /PLCOM/ PLREAL,ERROR
C
C CODE BEGINS HERE
C
        IF (ERROR) RETURN

        WRITE(REPNAM,10) GSNAMES(GNUM)

        CALL FIND_AVAILABLE_LUN(REPLUN,ST)
        IF (ST.NE.0) THEN
            TYPE*,IAM(),'>>> ERROR ALOCATING LUN FOR REPORT ',REPNAM
            RETURN
        ENDIF

        OPEN( REPLUN,
     *        FILE   = REPNAM,
     *        STATUS = 'NEW',
     *        IOSTAT = ST)

        IF (ST.NE.0) THEN
            TYPE*,IAM(),'>>> ERROR OPPENING REPORT ',REPNAM
            RETURN 
        ENDIF
C
C PREPARE SORT
C
!       LRL = 132
!
!       KEYBUFFER(1)   = 1                       ! number of keys described
!       KEYBUFFER(2)   = %LOC(DSC$K_DTYPE_Z)     ! key type undefined
!       KEYBUFFER(3)   = 0                       ! sort type = ascending
!       KEYBUFFER(4)   = 52                      ! key offset inside record (bytes)
!       KEYBUFFER(5)   = 13                      ! key length in bytes
!
!       SORTTYPE  = %LOC(SOR$GK_RECORD)         ! record interface (sort type)
!       ST = SOR$Begin_Sort (KEYBUFFER, LRL,,,,, SORTTYPE)

        CALL TITLE('PLANO REAL PARA LOTARIA NACIONAL','WINPAS',1,REPLUN,PAGE,DAYCDC)
        WRITE(REPLUN,5) 

        ! WRITE TITLE
        WRITE(REPLUN,20) (GLNAMES(I,GNUM),I=1,4),WEEK,YEAR,'GENERAL   '

        TOTREC = 0
        DO I=1,50
            IF (PLREAL(AMT,I).GT.0) THEN
                WRITE(REPLUN,30) PLREAL(CNT,I),
     *                           CMONY(PLREAL(AMT,I),13,BETUNIT),
     *                           CMONY(PLREAL(CNT,I)*PLREAL(AMT,I),13,BETUNIT)

                TOTCNT = TOTCNT + PLREAL(CNT,I)
                TOTAMT = TOTAMT + KZEXT((PLREAL(AMT,I)*PLREAL(CNT,I)))
                TOTREC = TOTREC + 1

!               ST = SOR$Release_Rec (STR)              
            ENDIF
        ENDDO

!       ST = SOR$Sort_Merge () 
!
!       DO I=1,TOTREC
!          ST = SOR$Return_Rec (STR)
!          WRITE(REPLUN,35) STR
!       ENDDO
!
!       ST = SOR$End_Sort ()

        WRITE(REPLUN,40)
        WRITE(REPLUN,50) TOTCNT,CMONYK8(TOTAMT,13,BETUNIT)
!+++++++ONLINE++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        TOTCNT = 0
        TOTAMT = 0
        CALL TITLE('PLANO REAL PARA LOTARIA NACIONAL','WINPAS',1,REPLUN,PAGE,DAYCDC)
        WRITE(REPLUN,5) 
        WRITE(REPLUN,20) (GLNAMES(I,GNUM),I=1,4),WEEK,YEAR,'ONLINE    '
        DO I=1,50                                              
            IF (PLREAL(AMT,I).GT.0) THEN                       
                WRITE(REPLUN,30) PLREAL(3,I),                
     *                           CMONY(PLREAL(AMT,I),13,BETUNIT),
     *                           CMONY(PLREAL(3,I)*PLREAL(AMT,I),13,BETUNIT)
                                                               
                TOTCNT = TOTCNT + PLREAL(3,I)                
                TOTAMT = TOTAMT + KZEXT((PLREAL(AMT,I)*PLREAL(3,I)))
            ENDIF                                              
        ENDDO                                                  
                                                               
        WRITE(REPLUN,40)                                       
        WRITE(REPLUN,50) TOTCNT,CMONYK8(TOTAMT,13,BETUNIT)       
!+++++++OFFLINE+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        TOTCNT = 0
        TOTAMT = 0
        CALL TITLE('PLANO REAL PARA LOTARIA NACIONAL','WINPAS',1,REPLUN,PAGE,DAYCDC)
        WRITE(REPLUN,5) 
        WRITE(REPLUN,20) (GLNAMES(I,GNUM),I=1,4),WEEK,YEAR,'PREPRINTED'
        DO I=1,50                                              
            IF (PLREAL(AMT,I).GT.0) THEN                       
                WRITE(REPLUN,30) PLREAL(4,I),                
     *                           CMONY(PLREAL(AMT,I),13,BETUNIT),
     *                           CMONY(PLREAL(4,I)*PLREAL(AMT,I),13,BETUNIT)
                                                               
                TOTCNT = TOTCNT + PLREAL(4,I)                
                TOTAMT = TOTAMT + KZEXT((PLREAL(AMT,I)*PLREAL(4,I)))
            ENDIF                                              
        ENDDO                                                  
                                                               
        WRITE(REPLUN,40)                                       
        WRITE(REPLUN,50) TOTCNT,CMONYK8(TOTAMT,13,BETUNIT)       
!+++++++RETURNED++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        TOTCNT = 0
        TOTAMT = 0
        CALL TITLE('PLANO REAL PARA LOTARIA NACIONAL','WINPAS',1,REPLUN,PAGE,DAYCDC)
        WRITE(REPLUN,5) 
        WRITE(REPLUN,20) (GLNAMES(I,GNUM),I=1,4),WEEK,YEAR,'RETURNED  '
        DO I=1,50                                              
            IF (PLREAL(AMT,I).GT.0) THEN                       
                WRITE(REPLUN,30) PLREAL(5,I),                
     *                           CMONY(PLREAL(AMT,I),13,BETUNIT),
     *                           CMONY(PLREAL(5,I)*PLREAL(AMT,I),13,BETUNIT)
                                                               
                TOTCNT = TOTCNT + PLREAL(5,I)                
                TOTAMT = TOTAMT + KZEXT((PLREAL(AMT,I)*PLREAL(5,I)))
            ENDIF                                              
        ENDDO                                                  
                                                               
        WRITE(REPLUN,40)                                       
        WRITE(REPLUN,50) TOTCNT,CMONYK8(TOTAMT,13,BETUNIT)       
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        TOTCNT = 0
        TOTAMT = 0
        CALL TITLE('PLANO REAL PARA LOTARIA NACIONAL','WINPAS',1,REPLUN,PAGE,DAYCDC)
        WRITE(REPLUN,5) 
        WRITE(REPLUN,20) (GLNAMES(I,GNUM),I=1,4),WEEK,YEAR,'UNSOLD    '
        DO I=1,50                                              
            IF (PLREAL(AMT,I).GT.0) THEN                       
                WRITE(REPLUN,30) PLREAL(6,I),                
     *                           CMONY(PLREAL(AMT,I),13,BETUNIT),
     *                           CMONY(PLREAL(6,I)*PLREAL(AMT,I),13,BETUNIT)
                                                               
                TOTCNT = TOTCNT + PLREAL(6,I)                
                TOTAMT = TOTAMT + KZEXT((PLREAL(AMT,I)*PLREAL(6,I)))
            ENDIF                                              
        ENDDO                                                  
                                                               
        WRITE(REPLUN,40)                                       
        WRITE(REPLUN,50) TOTCNT,CMONYK8(TOTAMT,13,BETUNIT)       
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        CLOSE(REPLUN)

        RETURN

5       FORMAT(132('='))

10      FORMAT('FILE:PLANOREAL_',A4,'.REP')

20      FORMAT(///,T48,'PLANO REAL - ',4A4,//,T71,'Extraccao: ',I2.2,'/',I4,1X,A10,
     *         ////,T34,'Fraccoes',T59,'Premios',T88,'Total',/,
     *         T28,'--------------    --------------------    -----------------------')

30      FORMAT(T32,I10,T53,A13,T80,A13)
35      FORMAT(A132)

40      FORMAT(/,T28,'--------------                            -----------------------')

50      FORMAT(T32,I10,T80,A13)
        END



C*********************************************************************
C**** CREATE WINNER SELECTION FILES IN ACCORDING TO NUMBER OF WINNERS
C*********************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PAS_CRTFIL_VPF(FILNAM, EMINUM, INDPAS, STATUS)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
C
C ROUTINE PARAMETERS
C
        INTEGER*4   FILNAM(5)
        INTEGER*4   EMINUM, INDPAS
        INTEGER*4   MULTIP
        INTEGER*4   TOTSERFRA
C
C LOCAL VARIABLES
C
        INTEGER*4   OFFEMI, INDDIV, TOTSHR, TOTSEC, STATUS, TEMP
C
C CALCULATE NUMBER OF WINNERS
C
        TOTSHR = 0
        OFFEMI = -1
        DO TEMP=1,PAGEMI
            IF (PASEMIS(TEMP, INDPAS) .EQ. EMINUM) THEN
               OFFEMI = TEMP
            ENDIF
        ENDDO

        IF (OFFEMI.LT.0) THEN
            TYPE*,IAM(),'ERROR FINDING OFFSET ON MEMORY (PAS_CRTFIL)'
            CALL GPAUSE()
            STATUS = -1
            RETURN
        ENDIF

        IF(INDPAS.EQ.PSBCLA) THEN   ! CLASSICA
C          MULTIP = 60
C          MULTIP = 80
C          MULTIP = 84 !V05
           MULTIP = 96 !V06
        ELSE                        ! POPULAR
C          MULTIP = 30
C          MULTIP = 34 !V05
          MULTIP = 46 !V06
        ENDIF

        TOTSERFRA = PASNUMSER(OFFEMI, INDPAS) * PASNOFFRA(OFFEMI, INDPAS)
        TOTSERFRA = (TOTSERFRA+2-1)/2*2  !V03

        DO INDDIV=1, PAGDIV
C          TOTSHR = TOTSHR +
C    *             (PASSHR(INDDIV, OFFEMI, INDPAS) *
C    *              PASNUMSER(OFFEMI, INDPAS) * PASNOFFRA(OFFEMI, INDPAS))
          TOTSHR = TOTSHR +
     *             (PASSHR(INDDIV, OFFEMI, INDPAS) * TOTSERFRA)
        ENDDO
C
C CALCULATE FILE LENGTH TO SUPPORT THESE NUMBER OF WINNERS WHERE
C MULTIP - AVERAGE SIDE
C 512    - NUMBER OF BYTES PER BLOCK
C
        TOTSEC = (MULTIP * TOTSHR) / 512
C
C CREATE FILE
C
        CALL CRTFIL(FILNAM, TOTSEC, STATUS)

        RETURN
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE GET_LN_TAXCONF(BASAMT,RFNAMT,TAXPER)
C
C       THIS SUBROUTINE LOADS THE LN TAX CONFIGURATION FROM TAXCONF.FIL
C
C       INPUTS: NONE
C
C       OUTPUTS:
C        BASAMT           BASE AMOUNT
C        RFNAMT           REFUND AMOUNT
C        TAXPER           TAX PERCENTAGE
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE GET_LN_TAXCONF(BASAMT,RFNAMT,TAXPER)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:TAXCONFIG.DEF'
C
C=======================================================================
C       LOCAL VARIABLES
C=======================================================================
C
        INTEGER*4 ST, TAXLUN, FDB(7)
        LOGICAL   ISTHERE
C
C=======================================================================
C       OUTPUT VARIABLES
C=======================================================================
C
        INTEGER*4  BASAMT, RFNAMT, TAXPER
C
        BASAMT=-1
        RFNAMT=-1
        TAXPER=-1
C
C=======================================================================
C       IF FILE TAXCONF.FIL DOESN'T EXIST, PAUSE PROGRAM
C=======================================================================
C
        TYPE*, IAM()
        TYPE*, IAM(), 'A obter a configuracao do imposto de selo a aplicar ...'
C
        INQUIRE(FILE='FILE:TAXCONF.FIL', EXIST=ISTHERE)
        IF (.NOT. ISTHERE) THEN
          TYPE*, IAM()
          TYPE*, IAM(),'Nao foi encontrado o ficheiro FILE:TAXCONF.FIL'
          TYPE*, IAM()
          CALL GSTOP(GEXIT_OPABORT)
        ENDIF
C
C=======================================================================
C       OPEN TAX CONFIG FILE AND LOAD CONFIGURATION VALUES
C=======================================================================
C
        CALL FIND_AVAILABLE_LUN(TAXLUN,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM()
          TYPE*, IAM(), 'Erro a obter LUN para abrir FILE:TAXCONF.FIL'
          TYPE*, IAM(), 'LUN error > ',ST
          CALL GSTOP(GEXIT_OPABORT)
        ENDIF
C
        CALL OPENX(TAXLUN,'FILE:TAXCONF.FIL',4,0,0,ST)
        CALL IOINIT(FDB,TAXLUN,TXCF_SEC*256)
        IF (ST .NE. 0) THEN
          TYPE*, IAM()
          TYPE*, IAM(), 'Nao foi possivel abrir o ficheiro FILE:TAXCONF.FIL'
          TYPE*, IAM(), 'TAXCONF.FIL open error > ',ST
          CALL CLOSEFIL(FDB)
          CALL GSTOP(GEXIT_OPABORT)
        ENDIF
C
        CALL READW(FDB,1,TXCF_REC,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM()
          TYPE*, IAM(), 'Nao foi possivel ler o ficheiro FILE:TAXCONF.FIL'
          TYPE*, IAM(), 'TAXCONF.FIL read error > ',ST
          CALL CLOSEFIL(FDB)
          CALL GSTOP(GEXIT_OPABORT)
        ENDIF
        CALL CLOSEFIL(FDB)
C
C=======================================================================
C       SET THE TAX PERCENTAGE, BASE AMOUNT AND REFUND AMOUNT
C=======================================================================
C
        TAXPER = TXCF_LNTAX
        BASAMT = TXCF_LNBSAMNT
        RFNAMT = TXCF_LNTAXRFN
C
        WRITE(5,'(1X,A,A,F11.2)') IAM(), ' Taxa de imposto:               ', DISPER(TAXPER * 10)
        WRITE(5,'(1X,A,A,A11)')   IAM(), ' Menor premio aplicavel:        ', CMONY(BASAMT,11,VALUNIT)
        WRITE(5,'(1X,A,A,A11)')   IAM(), ' Valor a amortizar no imposto:  ', CMONY(RFNAMT,11,VALUNIT)
C
        IF (BASAMT .LT. RFNAMT) THEN
          TYPE*, IAM()
          TYPE*, IAM(), 'Menor premio aplicavel MENOR que Valor a amortizar no imposto!'
          TYPE*, IAM()
          CALL GSTOP(GEXIT_OPABORT)
        ENDIF
C
        RETURN
        END
