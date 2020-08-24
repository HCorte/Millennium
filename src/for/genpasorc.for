C
C GENPASORC.FOR
C
C V03 10-JUN-2010 FRP Use VPNBNK in validation transactions
C V02 19-MAY-2010 FRP Use VOID as sale and add cancellation transactions
C V01 20-APR-2010 FRP Initial Release for Portugal ePassive
C
C EXTRACTS RETURNS, SALES/CANCELS AND CASHES INFORMATION FROM MTM
C AND GENERATES PASORC_RET, PASORC_VEN AND PASORC_PRM INTERFACES
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      OPTIONS /CHECK=NOOVERFLOW/EXT
      PROGRAM GENPASORC
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:DESTRA.DEF'
      INCLUDE 'INCLIB:PRMLOG.DEF'
C
      INTEGER*4 RET/1/,VEN/2/,PRM/3/
      INTEGER*4 RETLUN,VENLUN,PRMLUN
      INTEGER*4 ST,SER,SERCNT
      INTEGER*4 LOGREC(LMUREC)
      LOGICAL*1 EOF
C
      TYPE*,IAM()
      TYPE*,IAM(),'**************************************************'
      TYPE*,IAM(),'GENERATE PASSIVE INTERFACES PASORC_RET, _VEN, _PRM'
      TYPE*,IAM(),'**************************************************'
      TYPE*,IAM()
C
C Check if System Is Up
      CALL CHCKDIS(ST)
      IF(ST .EQ. 0) THEN
        TYPE*,IAM()
        TYPE*,IAM(),'System is ACTIVE!'
        TYPE*,IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
C Open PASORC Files
      CALL OPEN_PASORC(RET,RETLUN)
      CALL OPEN_PASORC(VEN,VENLUN)
      CALL OPEN_PASORC(PRM,PRMLUN)
C
C Write PASORC Headers
      CALL WRITE_PASORC('HP',TRABUF,RET,RETLUN)
      CALL WRITE_PASORC('HP',TRABUF,VEN,VENLUN)
      CALL WRITE_PASORC('HP',TRABUF,PRM,PRMLUN)
C
C Open VPF Files
      CALL FILL_FILEPAS_PRIZE_PAID(ST)
      IF(ST .NE. 0) THEN
        TYPE*,IAM(),'WARNING: THERE ARE NO PAID EXTRACTIONS TODAY FOR PASSIVE GAMES'
      ENDIF
C
      CALL OPENPAS(ST)
      IF(ST .NE. 0) THEN
        TYPE*,IAM(),'ERROR OPENING VPF FILES FOR PASSIVE GAMES'
        CALL GPAUSE()
      ENDIF
C
C Open MTM File
      CALL OPENW(PTMF,SFNAMES(1,PTMF),4,0,0,ST)
      IF(ST .NE. 0) CALL FILERR(SFNAMES(1,PTMF),1,ST,0)
      CALL TOPEN(PTMF)
C
C Read MTM File
      SER = 1
      SERCNT = 0
      EOF = .FALSE.
C
1000  CONTINUE
      CALL READTMF(LOGREC,SER,EOF)
      IF(EOF) GOTO 2000
C
      IF(MOD(SERCNT,1000000) .EQ. 0) TYPE*,IAM(),SERCNT,' records read from MTM'
      SERCNT = SERCNT+1
C
      CALL LOGTRA(TRABUF,LOGREC)  
C
      IF(TRABUF(TCDC) .NE. DAYCDC)  GOTO 1000
C
      IF(TRABUF(TGAMTYP) .NE. TPAS)  GOTO 1000
C
      IF(TRABUF(TTYP) .NE. TRET .AND.
     *   TRABUF(TTYP) .NE. TWAG .AND.
     *   TRABUF(TTYP) .NE. TCAN .AND.
     *   TRABUF(TTYP) .NE. TVAL)  GOTO 1000
C
      IF(TRABUF(TTYP) .EQ. TRET .AND. TRABUF(TSTAT) .EQ. GOOD)
     *  CALL WRITE_PASORC('02',TRABUF,RET,RETLUN)  !pTicket Returns
C
      IF(TRABUF(TTYP) .EQ. TWAG .AND. TRABUF(TWEPOP) .EQ. EPASSAL) THEN
        IF(TRABUF(TSTAT) .EQ. GOOD .OR.
     *     TRABUF(TSTAT) .EQ. VOID .OR.
     *     TRABUF(TSTAT) .EQ. INCA)
     *    CALL WRITE_PASORC('05',TRABUF,VEN,VENLUN)  !eTicket Sales
C
      ENDIF
C
      IF(TRABUF(TTYP) .EQ. TCAN .AND. TRABUF(TWEPOP) .EQ. EPASSAL .AND.
     *   TRABUF(TSTAT) .EQ. GOOD)
     *  CALL WRITE_PASORC('06',TRABUF,VEN,VENLUN)  !eTicket Cancels
C
      IF(TRABUF(TTYP) .EQ. TVAL .AND. TRABUF(TSTAT) .EQ. GOOD .AND.
     *   TRABUF(TERR) .EQ. NOER) THEN
        IF(TRABUF(TVTYPE) .EQ. VPNBNK) THEN
          CALL WRITE_PASORC('04',TRABUF,PRM,PRMLUN)  !xTicket Bank Transfers
        ELSE
          CALL WRITE_PASORC('03',TRABUF,PRM,PRMLUN)  !xTicket Payments
        ENDIF
      ENDIF
C
      GOTO 1000
C
2000  CONTINUE
C
C Close MTM File
      CALL USRCLOS1(PTMF)
C
C Close VPF Files
      CALL CLOSEPAS(ST)
C
C Write PASORC Trailers
      CALL WRITE_PASORC('TP',TRABUF,RET,RETLUN)
      CALL WRITE_PASORC('TP',TRABUF,VEN,VENLUN)
      CALL WRITE_PASORC('TP',TRABUF,PRM,PRMLUN)
C
C Close PASORC Files
      CLOSE(RETLUN)
      CLOSE(VENLUN)
      CLOSE(PRMLUN)
C
C End
      CALL GSTOP(GEXIT_SUCCESS)
      END
C
C***********************
C SUBROUTINE OPEN_PASORC
C***********************
C OPEN PASORC INTERFACE FILE
C
      OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE OPEN_PASORC(ORCIND,ORCLUN)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
      INTEGER*4 ORCIND
      INTEGER*4 ST,ORCLUN
      CHARACTER ORCNAM*19
      CHARACTER ORCSTR(3)*3/'RET','VEN','PRM'/
C
      WRITE(ORCNAM,100) ORCSTR(ORCIND)
100   FORMAT('SYSX:PASORC_',A3,'.ASC')
C
      CALL FIND_AVAILABLE_LUN(ORCLUN,ST)
      IF(ST .NE. 0) THEN
        TYPE*,IAM(),'ERROR GETTING LOGICAL UNIT FOR ',ORCNAM
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      OPEN(UNIT            = ORCLUN,
     *     FILE            = ORCNAM,
     *     STATUS          = 'NEW',
     *     CARRIAGECONTROL = 'LIST',
     *     ACCESS          = 'SEQUENTIAL',
     *     IOSTAT          = ST)
C
      IF(ST .NE. 0) THEN
        TYPE*,IAM(),'ERROR OPENING ',ORCNAM,' STATUS ',ST
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      RETURN
      END
C
C************************
C SUBROUTINE WRITE_PASORC
C************************
C WRITE PASORC INTERFACE FILE
C
      OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE WRITE_PASORC(RECTYP,TRABUF,ORCIND,ORCLUN)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:DESTRA.DEF'
      INCLUDE 'INCLIB:DATBUF.DEF'
C
      INTEGER*4 ORCIND,ORCLUN,RECCNT(3)
      INTEGER*2 DATE(DATLEN)
      CHARACTER RECTYP*2
C
      COMMON /GENPASORCCOM/ RECCNT
C
C Header
      IF(RECTYP .EQ. 'HP') THEN
        DATE(VCDC) = DAYCDC
        CALL CDATE(DATE)
C
        RECCNT(ORCIND) = RECCNT(ORCIND) + 1
        WRITE(ORCLUN,100) RECTYP,2000+DATE(VYEAR),DATE(VMON),DATE(VDAY)
C
C Trailer
      ELSEIF(RECTYP .EQ. 'TP') THEN
        RECCNT(ORCIND) = RECCNT(ORCIND) + 1
        WRITE(ORCLUN,110) RECTYP,RECCNT(ORCIND)
C
C pTicket Returns
      ELSEIF(RECTYP .EQ. '02') THEN
        CALL WRITE_PASORC_02(RECTYP,TRABUF,ORCIND,ORCLUN)
C
C xTicket Payments and Bank Transfers
      ELSEIF(RECTYP .EQ. '03' .OR. RECTYP .EQ. '04') THEN
        CALL WRITE_PASORC_03_04(RECTYP,TRABUF,ORCIND,ORCLUN)
C
C eTicket Sales and Cancels
      ELSEIF(RECTYP .EQ. '05' .OR. RECTYP .EQ. '06') THEN
        CALL WRITE_PASORC_05_06(RECTYP,TRABUF,ORCIND,ORCLUN)
      ENDIF
C
100   FORMAT(A2,I4.4,I2.2,I2.2)
110   FORMAT(A2,I8.8)
C
      RETURN
      END
C
C***************************
C SUBROUTINE WRITE_PASORC_02
C***************************
C WRITE PASORC_RET INTERFACE FILE
C
      OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE WRITE_PASORC_02(RECTYP,TRABUF,ORCIND,ORCLUN)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:AGTCOM.DEF'
      INCLUDE 'INCLIB:PASCOM.DEF'
      INCLUDE 'INCLIB:DESTRA.DEF'
      INCLUDE 'INCLIB:DATBUF.DEF'
C
      INTEGER*4 ORCIND,ORCLUN,RECCNT(3)
      INTEGER*4 TERN,AGTN,SAPN,GNUM,GIND,PSVTYP
      INTEGER*4 ST,TCKS,TCKN,NUMFRACS,HALF
      INTEGER*4 EMIS,EIND,ECNT,WEEK,YEAR
      INTEGER*4 EXTSER,CHKDIG
      INTEGER*2 DATE(DATLEN)
      CHARACTER RECTYP*2
      CHARACTER HHMMSS*6,GET_HHMMSS_TIME*6
      LOGICAL*1 SECTIC
C
      COMMON /GENPASORCCOM/ RECCNT
C
      DATE(VCDC) = TRABUF(TCDC)
      CALL CDATE(DATE)
      HHMMSS = GET_HHMMSS_TIME(TRABUF(TTIM))
C
      TERN = TRABUF(TTER)
      IF(TRABUF(TPOFFTER) .NE. 0) TERN = TRABUF(TPOFFTER)
      AGTN = AGTTAB(AGTNUM,TERN)
      SAPN = AGTSAP(TERN)
      GNUM = TRABUF(TGAM) 	       
      GIND = TRABUF(TGAMIND)
C
      IF(GIND .EQ. PSBCLA) THEN
        PSVTYP = 0
      ELSEIF(GIND .EQ. PSBPOP) THEN
        PSVTYP = 5
      ELSE
        TYPE*,IAM(),'ERROR GETTING PASSIVE TYPE, SERIAL ',TRABUF(TSER)
        CALL GPAUSE()
      ENDIF
C
      CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),EXTSER,CHKDIG)
C
      DO TCKS=1,TRABUF(TPTCK)
        IF(TRABUF(TPSTS1 + OFFTRA*(TCKS-1)) .EQ. RETURND .OR.
     *     TRABUF(TPSTS1 + OFFTRA*(TCKS-1)) .EQ. RETAFDR) THEN
C
          EMIS = TRABUF(TPEMIS1 + OFFTRA*(TCKS-1))
          CALL GETWEK(EMIS,GNUM,WEEK,YEAR,ST)
          IF(ST .NE. 0) THEN
            TYPE*,IAM(),'ERROR GETTING SCML EMISSION NUMBER, SERIAL ',TRABUF(TSER)
            CALL GPAUSE()
          ENDIF
C
          EIND = -1
          DO ECNT = 1,PAGEMI
            IF(PASEMIS(ECNT,GIND) .EQ. EMIS) THEN
              EIND = ECNT
              EXIT
            ENDIF
          ENDDO
          IF(EIND .LT. 0) THEN
            TYPE*,IAM(),'ERROR GETTING MEMORY OFFSET, SERIAL ',TRABUF(TSER)
            CALL GPAUSE()
          ENDIF
C
          SECTIC = .FALSE.
          IF(TRABUF(TPRETYP) .EQ. ALLTCK) THEN
            NUMFRACS = PASNOFFRA(EIND,GIND)
            IF(GIND .EQ. PSBPOP) THEN
              SECTIC = .TRUE.
              HALF = PASNUMTCK(EIND,GIND)/2
              IF(TRABUF(TPNUM1+OFFTRA*(TCKS-1)) .GE. HALF) THEN
                TCKN = TRABUF(TPNUM1+OFFTRA*(TCKS-1)) - HALF
              ELSE
                TCKN = TRABUF(TPNUM1+OFFTRA*(TCKS-1)) + HALF
              ENDIF
            ENDIF
          ELSEIF(TRABUF(TPRETYP) .EQ. BYFRAC) THEN
            NUMFRACS = 1
          ELSEIF(TRABUF(TPRETYP) .EQ. HALFTCK) THEN
            IF(GIND .EQ. PSBPOP) THEN
              NUMFRACS = PASNOFFRA(EIND,GIND)
            ELSE
              NUMFRACS = PASNOFFRA(EIND,GIND)/2
            ENDIF
          ELSEIF(TRABUF(TPRETYP) .EQ. QUARTCK) THEN
            NUMFRACS = PASNOFFRA(EIND,GIND)/4
          ENDIF
C
          RECCNT(ORCIND) = RECCNT(ORCIND) + 1
	  WRITE(ORCLUN,100) RECTYP,
     *                      PSVTYP,
     *                      WEEK,
     *                      YEAR,
     *                      TRABUF(TPNUM1 + OFFTRA*(TCKS-1)),
     *                      TRABUF(TPSER1 + OFFTRA*(TCKS-1)),
     *			    TRABUF(TPTEN1 + OFFTRA*(TCKS-1)),
     *                      SAPN,
     *                      AGTN,
     *                      DATE(VJUL),
     *                      EXTSER,
     *                      CHKDIG,
     *                      2000+DATE(VYEAR),DATE(VMON),DATE(VDAY),
     *                      HHMMSS,
     *                      TRABUF(TPEMIS1 + OFFTRA*(TCKS-1))
C
          IF(SECTIC) THEN
            RECCNT(ORCIND) = RECCNT(ORCIND) + 1
            WRITE(ORCLUN,100) RECTYP,
     *                        PSVTYP,
     *                        WEEK,
     *                        YEAR,
     *                        TCKN,
     *                        TRABUF(TPSER1 + OFFTRA*(TCKS-1)),
     *                        TRABUF(TPTEN1 + OFFTRA*(TCKS-1)),
     *                        SAPN,
     *                        AGTN,
     *                        DATE(VJUL),
     *                        EXTSER,
     *                        CHKDIG,
     *                        2000+DATE(VYEAR),DATE(VMON),DATE(VDAY),
     *                        HHMMSS,
     *                        TRABUF(TPEMIS1 + OFFTRA*(TCKS-1))
          ENDIF
        ENDIF
      ENDDO
C
100   FORMAT(A2,I1,I2.2,I4.4,I5.5,I2.2,I2.2,I6.6,I7.7,I3.3,I8.8,I3.3,
     *       I4.4,I2.2,I2.2,A6,I4.4)
C
      RETURN
      END
C
C******************************
C SUBROUTINE WRITE_PASORC_05_06
C******************************
C WRITE PASORC_VEN INTERFACE FILE
C
      OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE WRITE_PASORC_05_06(RECTYP,TRABUF,ORCIND,ORCLUN)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:AGTCOM.DEF'
      INCLUDE 'INCLIB:DESTRA.DEF'
      INCLUDE 'INCLIB:DATBUF.DEF'
C
      INTEGER*4 ORCIND,ORCLUN,RECCNT(3)
      INTEGER*4 TERN,AGTN,SAPN,GNUM,GIND,PSVTYP
      INTEGER*4 EXTSER,CHKDIG
      INTEGER*2 DATE(DATLEN)
      CHARACTER RECTYP*2
      CHARACTER HHMMSS*6,GET_HHMMSS_TIME*6
C
      COMMON /GENPASORCCOM/ RECCNT
C
      DATE(VCDC) = TRABUF(TCDC)
      CALL CDATE(DATE)
      HHMMSS = GET_HHMMSS_TIME(TRABUF(TTIM))
C
      TERN = TRABUF(TTER)
      AGTN = AGTTAB(AGTNUM,TERN)
      SAPN = AGTSAP(TERN)
      GNUM = TRABUF(TGAM) 	       
      GIND = TRABUF(TGAMIND) 
C
      IF(GIND .EQ. PSBCLA) THEN
        PSVTYP = 0
      ELSEIF(GIND .EQ. PSBPOP) THEN
        PSVTYP = 5
      ELSE
        TYPE*,IAM(),'ERROR GETTING PASSIVE TYPE, SERIAL ',TRABUF(TSER)
        CALL GPAUSE()
      ENDIF
C
      CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),EXTSER,CHKDIG)
C
      RECCNT(ORCIND) = RECCNT(ORCIND) + 1
      WRITE(ORCLUN,100) RECTYP,
     *                  PSVTYP,
     *                  TRABUF(TWEPWK),
     *                  2000+TRABUF(TWEPYR),
     *                  TRABUF(TWEPSN),
     *                  TRABUF(TWEPSS),
     *                  TRABUF(TWEPSF),
     *                  SAPN,
     *                  AGTN,
     *                  DATE(VJUL),
     *                  EXTSER,
     *                  CHKDIG,
     *                  2000+DATE(VYEAR),DATE(VMON),DATE(VDAY),
     *                  HHMMSS,
     *                  TRABUF(TWBEG)
C
100   FORMAT(A2,I1,I2.2,I4.4,I5.5,I2.2,I2.2,I6.6,I7.7,I3.3,I8.8,I3.3,
     *       I4.4,I2.2,I2.2,A6,I4.4)
C
      RETURN
      END
C
C******************************
C SUBROUTINE WRITE_PASORC_03_04
C******************************
C WRITE PASORC_PRM INTERFACE FILE
C
      OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE WRITE_PASORC_03_04(RECTYP,TRABUF,ORCIND,ORCLUN)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:AGTCOM.DEF'
      INCLUDE 'INCLIB:DESTRA.DEF'
      INCLUDE 'INCLIB:DESVAL.DEF'
      INCLUDE 'INCLIB:VDETAIL.DEF'
      INCLUDE 'INCLIB:VALPASFIL.DEF'
      INCLUDE 'INCLIB:DATBUF.DEF'
C
      INTEGER*4 ORCIND,ORCLUN,RECCNT(3)
      INTEGER*4 TERN,AGTN,SAPN,GNUM,GIND
      INTEGER*4 PSVTYP,TCKTYP,APRTYP
      INTEGER*4 ST,TCKS,EMIS,WEEK,YEAR
      INTEGER*4 EXTSER,CHKDIG
      INTEGER*4 PZIND,PZDIV(6)
      INTEGER*2 DATE(DATLEN)
      CHARACTER RECTYP*2
      CHARACTER HHMMSS*6,GET_HHMMSS_TIME*6
C
      COMMON /GENPASORCCOM/ RECCNT
C
      DATE(VCDC) = TRABUF(TCDC)
      CALL CDATE(DATE)
      HHMMSS = GET_HHMMSS_TIME(TRABUF(TTIM))
C
      IF(TRABUF(TPOFFTER) .EQ. 0) THEN
        TERN = TRABUF(TTER)
        IF(TSBIT(AGTTAB(AGTTYP,TERN),AGTPRV)) THEN
          IF(RECTYP .EQ. '03') APRTYP = 1  !Payments to players from privileged terminals
          IF(RECTYP .EQ. '04') APRTYP = 6  !Payments by Bank Transfer directly to the Player from privileged terminals
        ELSE
          IF(RECTYP .EQ. '03') APRTYP = 3  !Payments to players from agents (non-privileged terminals)
          IF(RECTYP .EQ. '04') APRTYP = 5  !Payments by Bank Transfer directly to the Player
        ENDIF
      ELSE
        TERN = TRABUF(TPOFFTER)
        IF(TSBIT(AGTTAB(AGTTYP,TERN),AGTBNK)) THEN
          APRTYP = 2  !Payments done by banks from privileged terminals
        ELSE
          APRTYP = 4  !Payments done to agents from privileged terminals
        ENDIF
      ENDIF
C
      AGTN = AGTTAB(AGTNUM,TERN)
      SAPN = AGTSAP(TERN)
      GNUM = TRABUF(TGAM) 	       
      GIND = TRABUF(TGAMIND)
C
      IF(GIND .EQ. PSBCLA) THEN
        PSVTYP = 0
      ELSEIF(GIND .EQ. PSBPOP) THEN
        PSVTYP = 5
      ELSE
        TYPE*,IAM(),'ERROR GETTING PASSIVE TYPE, SERIAL ',TRABUF(TSER)
        CALL GPAUSE()
      ENDIF
C
      CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),EXTSER,CHKDIG)
C
      IF(TRABUF(TVEPTYP) .EQ. 0) THEN
        TCKTYP = 1  !pTicket
      ELSEIF(TRABUF(TVEPTYP) .EQ. 1) THEN
        TCKTYP = 2  !eTicket
      ELSE
        TYPE*,IAM(),'ERROR GETTING TICKET TYPE, SERIAL ',TRABUF(TSER)
        CALL GPAUSE()
      ENDIF
C
      DO TCKS=1,TRABUF(TPTCK)
        IF(TRABUF(TPSTS1 + OFFTRA*(TCKS-1)) .EQ. VWINNER) THEN
C
          EMIS = TRABUF(TPEMIS1 + OFFTRA*(TCKS-1))
          CALL GETWEK(EMIS,GNUM,WEEK,YEAR,ST)
          IF(ST .NE. 0) THEN
            TYPE*,IAM(),'ERROR GETTING SCML EMISSION NUMBER, SERIAL ',TRABUF(TSER)
            CALL GPAUSE()
          ENDIF
C
          CALL FASTSET(0,VALREC,SIZEOF(VALREC)/4)
          CALL READPAS(TRABUF(TGAMIND),
     *                 TRABUF(TPEMIS1 + OFFTRA*(TCKS-1)),
     *                 TRABUF(TPNUM1 + OFFTRA*(TCKS-1)),
     *                 TRABUF(TPSER1 + OFFTRA*(TCKS-1)),
     *                 TRABUF(TPTEN1 + OFFTRA*(TCKS-1)),
     *                 V4BUF_PAS,ST)
          IF(ST .NE. 0) CALL GPAUSE()
C
          CALL LOGPAS(VALREC,V4BUF_PAS)
C
          IF(VALREC(VGAM) .NE. GNUM) THEN
            TYPE*,IAM(),'ERROR IN VPF TICKET GAME NUMBER, SERIAL ',TRABUF(TSER)
            CALL GPAUSE()
          ENDIF
C
          IF(VALREC(VCCDC) .NE. DAYCDC) THEN
            TYPE*,IAM(),'ERROR IN VPF TICKET CASH CDC, SERIAL ',TRABUF(TSER)
            CALL GPAUSE()
          ENDIF
C
          IF((RECTYP .EQ. '03' .AND. VALREC(VSTAT) .NE. VCASH) .OR.
     *       (RECTYP .EQ. '04' .AND. VALREC(VSTAT) .NE. VBANK)) THEN
            TYPE*,IAM(),'ERROR IN VPF TICKET STATUS, SERIAL ',TRABUF(TSER)
            CALL GPAUSE()
          ENDIF
C
          IF((TCKTYP .EQ. 1 .AND. VALREC(VPASTYP) .NE. VPASOFF) .OR.
     *       (TCKTYP .EQ. 2 .AND. VALREC(VPASTYP) .NE. VPASONL)) THEN
            TYPE*,IAM(),'ERROR IN VPF TICKET PASSIVE TYPE, SERIAL ',TRABUF(TSER)
            CALL GPAUSE()
          ENDIF
C
          CALL DLOGPAS(VALREC,VDETAIL)
C
          CALL FASTSET(0,PZDIV,6)
          DO PZIND = 1,MIN(6,VALREC(VPZOFF))
            PZDIV(PZIND) = VDETAIL(VDIV,PZIND)
          ENDDO
C
          RECCNT(ORCIND) = RECCNT(ORCIND) + 1
	  WRITE(ORCLUN,100) RECTYP,
     *                      PSVTYP,
     *                      WEEK,
     *                      YEAR,
     *                      TCKTYP,
     *                      APRTYP,
     *                      TRABUF(TPNUM1 + OFFTRA*(TCKS-1)),
     *                      TRABUF(TPSER1 + OFFTRA*(TCKS-1)),
     *                      TRABUF(TPTEN1 + OFFTRA*(TCKS-1)),
     *                      SAPN,
     *                      AGTN,
     *                      DATE(VJUL),
     *                      EXTSER,
     *                      CHKDIG,
     *                      2000+DATE(VYEAR),DATE(VMON),DATE(VDAY),
     *                      HHMMSS,
     *                      TRABUF(TPEMIS1 + OFFTRA*(TCKS-1)),
     *                      TRABUF(TPPAY1 + OFFTRA*(TCKS-1)),
     *                      (PZDIV(PZIND), PZIND = 1,6),
     *                      TRABUF(TVNIBBB),
     *                      TRABUF(TVNIBBO),
     *                      TRABUF(TVNIBBA1),
     *                      TRABUF(TVNIBBA2),
     *                      TRABUF(TVNIBCD),
     *                      TRABUF(TVPLCARD)
        ENDIF
      ENDDO
C
100   FORMAT(A2,I1,I2.2,I4.4,I1,I1,I5.5,I2.2,I2.2,I6.6,I7.7,I3.3,I8.8,I3.3,
     *       I4.4,I2.2,I2.2,A6,I4.4,I13.13,6(I2.2),I4.4,I4.4,I9.9,I2.2,I2.2,I9.9)
C
      RETURN
      END
C
C*************************
C FUNCTION GET_HHMMSS_TIME
C*************************
C GET TIME IN HHMMSS FORMAT STARTING FROM TIME IN SECONDS
C
      OPTIONS /CHECK = NOOVERFLOW /EXT
      CHARACTER * 6 FUNCTION GET_HHMMSS_TIME(TIME)
      IMPLICIT NONE
C
      INTEGER * 4 TIME             ! IMPUT TIME IN SECONDS
      CHARACTER * 6 OUTTIME        ! OUTPUT TIME IN HHMMSS FORMAT
C
      INTEGER * 4 SECONDS           ! SECONDS TIME
      INTEGER * 4 MINUTES           ! MINUTES TIME
      INTEGER * 4 HOURS             ! HOURS TIME
C
      HOURS = TIME / 3600
      MINUTES = MOD(TIME, 3600) / 60
      SECONDS = MOD(MOD(TIME, 3600), 60)
C
      WRITE(OUTTIME, 100) HOURS, MINUTES, SECONDS
C
      GET_HHMMSS_TIME = OUTTIME
C
100   FORMAT(I2.2, I2.2, I2.2)
C
      END
C
C***********************************
C SUBROUTINE FILL_FILEPAS_PRIZE_PAID
C***********************************
C FILLS "EMISION" IN FILEPAS (FILESTRUCS.DEF) WITH CLOSED EMISSIONS WITH PRIZES PAID TODAY
C
      OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE FILL_FILEPAS_PRIZE_PAID(ST)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:PASCOM.DEF'
      INCLUDE 'INCLIB:FILESTRUCS.DEF'
C
      INTEGER*4 INDEMI, ST, POS, INDPAS, QTD
C
      ST  = 0
      QTD = 0
      CALL FASTSET(0,FILEPAS,SIZEOF(FILEPAS)/4)
C
      DO INDPAS = 1,NUMPAS
        POS = 0
        DO INDEMI = 1,PAGEMI
          IF(PASTODPAY(INDEMI,INDPAS) .GT. 0) THEN
            POS = POS+1
            FILEPAS(POS,INDPAS).EMISION = PASEMIS(INDEMI,INDPAS)
          ENDIF
        ENDDO
        QTD = QTD+POS
      ENDDO
C
      IF(QTD .EQ. 0) ST = -1
C
      RETURN
      END
