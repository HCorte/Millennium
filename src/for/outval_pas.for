C OUTVAL_PAS.FOR
C
C V07 07-FEB-14 SCML Batch validation fix: sum up also the prize value 
C                    of tickets with status equal to NPAYBAT.
C V06 31-DEC-13 SCML Fix of the ticket status of the second ticket in
C                    a batch validation: the ticket status was being 
C                    overwritten with zero.
C V05 23-SET-13 SCML New validation subtype added
C V04 11-NOV-10 FJG TPOFFTER terminal gets all privileges
C               FJG Add addtional controls for OutOfBounds
C V03 03-NOV-10 FJG NUMAGT Out of bounds bug
C V02 01-Jan-10 FJG ePassive
C     06-Aug-10 FJG Last minute SCML change of mind with INQUIRIES
C V01 14-DEC-00 CS  INITIAL RELEASE FOR PORTUGAL
C
C
C SUBROUTINE TO BUILD PASSIVE VALIDATION OUTPUT MESSAGES.
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE OUTVAL_PAS(TRABUF,OUTTAB,OUTLEN)
	IMPLICIT NONE

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
C LOCAL VARIABLES
C
	BYTE      OUTTAB(*),I1TEMP(4)

	INTEGER*2 OUTLEN

	INTEGER*4 TEMP
	INTEGER*4 ERRTYP,CONTRL
	INTEGER*4 CHKLEN,MYCHKSUM

	EQUIVALENCE (TEMP,I1TEMP)

	DATA ERRTYP/Z90/
	DATA CONTRL/Z20/

	TEMP      = CONTRL + TRABUF(TTRN)
	OUTTAB(1) = I1TEMP(1)
C
C GENERAL ERROR
C
	IF(TRABUF(TSTAT).NE.GOOD .AND. TRABUF(TERR).NE.VINQ) THEN
	    OUTTAB(2) = ERRTYP
	    OUTTAB(5) = TRABUF(TERR)
	    OUTLEN    = 5
	ELSE
C
C BUILD VALIDATION/INQUIRY MESSAGE
C
	    CALL GETPAS_OUTVAL(TRABUF,OUTTAB,OUTLEN)
	ENDIF
C
C CALCULATE CHECKSUM
C
	I4CCITT = TRABUF(TCHK)
	OUTTAB(3)=I1CCITT(2)
	OUTTAB(4)=I1CCITT(1)
	CHKLEN=OUTLEN-1
	CALL GETCCITT(OUTTAB,1,CHKLEN,MYCHKSUM)
	I4CCITT=MYCHKSUM
	OUTTAB(3)=I1CCITT(2)
	OUTTAB(4)=I1CCITT(1)

	RETURN
	END

C************************************
C***** GET OUTVAL FOR PASSIVE LOTTERY
C************************************
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE GETPAS_OUTVAL(TRABUF,OUTTAB,OUTLEN)
	IMPLICIT   NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'		
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
C
C LOCAL VARIABLES
C
	BYTE      OUTTAB(*),I1TEMP(4)

	INTEGER*2 OUTLEN

	INTEGER*4 TEMP,OFFSET,OFFSET_TRA,TICKETS,CHECK
	INTEGER*4 WEEK,YEAR,OFF_AUXEMIS,GIND
	INTEGER*4 TOTAMT
	INTEGER*4 TOTNETAMT !V05
        INTEGER*4 GAMOPT               ! GAME OPTION
        integer*4 getpagemi
        logical   ispurged
        BYTE OPTFLAGS !V05

	EQUIVALENCE (TEMP,I1TEMP)

	TOTAMT    = 0
	TOTNETAMT = 0 !V05
	OPTFLAGS  = 0 !V05
	ispurged  = .false.
C
C
C BUILD VALIDATION/INQUIRY MESSAGE
C
        TEMP = 5
        CALL PUTIME(TRABUF(TTIM), OUTTAB, TEMP)   ! TIME IN HHMMSS FORMAT
C
      	CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),TEMP,CHECK)
      	OUTTAB(8)  = I1TEMP(3)
      	OUTTAB(9)  = I1TEMP(2)
      	OUTTAB(10) = I1TEMP(1)			  ! VALIDATION SERIAL NUMBER

      	OUTTAB(11) = CHECK			  ! VALIDATION CHECK DIGITS
C
C OFFLINE AGENT #
C
	TEMP       = 0
        IF (TRABUF(TPOFFTER).GT.0) THEN
          IF (TRABUF(TPOFFTER).LE.NUMAGT) THEN
            TEMP = AGTTAB(AGTNUM,TRABUF(TPOFFTER))
          ELSE
            TYPE*,'========== OUTVAL_PAS =========='
            TYPE*,'STATUS:   ',TRABUF(TSTAT)
            TYPE*,'ERROR:    ',TRABUF(TERR) 
            TYPE*,'CDC:      ',TRABUF(TCDC)
            TYPE*,'SERIAL:   ',TRABUF(TSER)
            TYPE*,'TERMINAL: ',TRABUF(TTER)
            TYPE*,'AGENT:    ',TRABUF(TAGT)
            TYPE*,'TRNTYPE:  ',TRABUF(TTYP)
            TYPE*,'GAME:     ',TRABUF(TGAM)
            TYPE*,'GTYPE:    ',TRABUF(TGAMTYP)
            TYPE*,'GINDEX:   ',TRABUF(TGAMIND)
            TYPE*,'TPOFFTER: ',TRABUF(TPOFFTER)             
          ENDIF
        ENDIF
	OUTTAB(17) = I1TEMP(4)
	OUTTAB(18) = I1TEMP(3)
	OUTTAB(19) = I1TEMP(2)
	OUTTAB(20) = I1TEMP(1)
C
	OUTTAB(21) = TRABUF(TPTCK)  !NUMBER OF TICKETS
C
	GIND       = TRABUF(TGAMIND)
C
C SEND ALL POSSIBLE VALIDATION MESSAGES
C
	DO TICKETS = 1, TRABUF(TPTCK)

	  OFFSET_TRA = (TICKETS-1)*OFFTRA
	  OFFSET     = (TICKETS-1)*13
C
C VALIDATION STATUS
C
	  OUTTAB(22+OFFSET) = TRABUF(TPSTS1+OFFSET_TRA)	  !STATUS

	  TEMP              = TRABUF(TPEMIS1+OFFSET_TRA)
C
C CHECK EMISSION #
C
	  WEEK = 0
	  YEAR = 0
	  IF (TEMP.GT.0) THEN
C
C CREATE AUXILIARY VARIABLES (OFFSET TO PASCOM)
C
	     OFF_AUXEMIS = getpagemi(temp,gind)
	     if(off_auxemis.gt.0.and.off_auxemis.le.pagemi) then
               TEMP = PASDRAW(OFF_AUXEMIS,GIND)
               if(passubsts(off_auxemis,gind).eq.pdrwpur) ispurged = .true.
               CALL GETPASDRW(TEMP,WEEK,YEAR)
             endif
	  ENDIF

      	  OUTTAB(23+OFFSET) = WEEK		  !WEEK (WW)
      	  OUTTAB(24+OFFSET) = MOD(YEAR,100)	  !YEAR (YY)

	  TEMP              = TRABUF(TPNUM1+OFFSET_TRA)
      	  OUTTAB(25+OFFSET) = I1TEMP(4)
      	  OUTTAB(26+OFFSET) = I1TEMP(3)
      	  OUTTAB(27+OFFSET) = I1TEMP(2)
      	  OUTTAB(28+OFFSET) = I1TEMP(1)	  !TICKET NUMBER

	  OUTTAB(29+OFFSET) = TRABUF(TPSER1+OFFSET_TRA)   !SERIE #
C
	  OUTTAB(30+OFFSET) = TRABUF(TPTEN1+OFFSET_TRA)   !FRACTION # PAID

	  TEMP              = TRABUF(TPPAY1+OFFSET_TRA)
      	  OUTTAB(31+OFFSET) = I1TEMP(4)
      	  OUTTAB(32+OFFSET) = I1TEMP(3)
      	  OUTTAB(33+OFFSET) = I1TEMP(2)
      	  OUTTAB(34+OFFSET) = I1TEMP(1)	  !CASH AMOUNT

C	  IF(TRABUF(TPSTS1+OFFSET_TRA).EQ.VWINNER) THEN
	  IF(TRABUF(TPSTS1+OFFSET_TRA).EQ.VWINNER .OR. TRABUF(TPSTS1+OFFSET_TRA).EQ.NPAYBAT) THEN	!V07
	    TOTAMT          = TOTAMT + TRABUF(TPPAY1+OFFSET_TRA)
	  ENDIF

	ENDDO
C
C BUILD VALIDATION/INQUIRY MESSAGE AND CHECK IF WE NEED THE SECOND TRANSACTION
C
!-------Depending on layouts
        if(trabuf(TVEPVAL).eq.0) then
          IF(TRABUF(TERR).EQ.VINQ .AND. TOTAMT.GT.0) THEN
            TEMP = '49'X
          ELSE
            TEMP = '48'X
          ENDIF
        else
          if(trabuf(TERR).eq.VINQ) then
!-------->>V05 -------------------------------------------------------------------
!            if(totamt.ge.P(VALORDER)) then
!              TEMP = '40'X + VPNINQ
!            else
!              TEMP = '40'X + VPNREG  ! All values are INQUIRY 
!            endif
            if(TSBIT(agttab(AGTTYP,trabuf(TTER)),AGTPRV)) then
              !PRIVILEGED
              if(totamt.LT.P(VALPRZHI)) then
                !TOTAMT < € 5000,00
                TEMP = '40'X + VPNINQ !CASH/BANK TRANSFER
              else
                !TOTAMT >= € 5000,00
                TEMP = '40'X + VPNREG !CASH ONLY
              endif
            else
              !NOT PRIVILEGED
              if(totamt.LT.P(VALORDER)) then
                !TOTAMT < € 150,01
                TEMP = '40'X + VPNREG !CASH ONLY
              else
                !TOTAMT >= € 150,01
                TEMP = '40'X + VPNIBO !BANK TRANSFER ONLY
              endif
            endif
!-------- V05<<-------------------------------------------------------------------
          else
            TEMP = '40'X + trabuf(TVTYPE)            
          endif
        endif

	OUTTAB(2)  = TEMP			  ! TYPE/SUBTYPE
C
C FILL TOTAL PRIZE AMOUNT
C
	TEMP       = TOTAMT
      	OUTTAB(12) = I1TEMP(4)
      	OUTTAB(13) = I1TEMP(3)
        OUTTAB(14) = I1TEMP(2)
      	OUTTAB(15) = I1TEMP(1)    !TOTAL CASH AMOUNT
C
!-------->>V05 -----------------------------------------------------------------
C
C FILL OPTION FLAGS
C
        OUTTAB(35+OFFSET) = OPTFLAGS !V06 - OFFSET added
C
C FILL OPTION DATA WITH TOTAL NET CASH AMOUNT !V05
C
        IF(TRABUF(TVEPVAL).EQ.1) THEN ! NEW LAYOUT
          IF(TRABUF(TPTCK).EQ.1 .AND. TRABUF(TVOPPAY).GT.P(VALPRZHI)) THEN
C Only send net value for prizes greater than P(VALPRZHI)
          OUTTAB(35+OFFSET) = IOR('80'X, OPTFLAGS) ! Z10000000 - Net Cash Amount (validation units) – 4 bytes !V06 - OFFSET added
          TEMP = TRABUF(TVOPPAY)
          OUTTAB(36+OFFSET) = I1TEMP(4) !V06 - OFFSET added
          OUTTAB(37+OFFSET) = I1TEMP(3) !V06 - OFFSET added
          OUTTAB(38+OFFSET) = I1TEMP(2) !V06 - OFFSET added
          OUTTAB(39+OFFSET) = I1TEMP(1) !V06 - OFFSET added
          OFFSET = OFFSET + 4
         ENDIF
        ENDIF
!-------- V05<<-----------------------------------------------------------------
C
C SEND GAME INDEX / GAME OPTIONS
C
        if(ispurged) then
          gamopt = 1      ! Z00000001 - EXPIRED VALIDATION
        else
          gamopt = 0          
        endif
        TEMP       = ISHFT(GIND, 4)
        OUTTAB(16) = IOR(TEMP, GAMOPT)
C
C DESCRIBE MESSAGE SIZE
C
!	    OUTLEN = 34 + OFFSET
        OUTLEN = 35 + OFFSET !V05
C
C UPDATE CLERK FLAG FOR BANK AGENTS
C
        CALL UPD_AGT_BANK_WITH_CLERK_FLAG(TRABUF)
C 
C END OF PROCEDURE
C
	RETURN
	END


C ******************************************************************************
C
C     SUBROUTINE: UPD_AGT_BANK_WITH_CLERK_FLAG
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 30 / 11 / 2001
C
C ******************************************************************************
C
C FUNCION TO UPDATE UPDATE BANKS AGENTS ( SAID TO FACTUARTION PROCEDURES THAT
C WE HAVE TRANSACCTION FROM THIS TERMINAL, THIS IS USUALLY DONE IN SIGN ON
C BUT BANK AGENT ARE LOGICAL ONES, WE DON'T HAVE THE TERMINAL )
C
C=======OPTIONS /CHECK = NOOVERFLOW /EXT
      SUBROUTINE UPD_AGT_BANK_WITH_CLERK_FLAG(TRABUF)
      IMPLICIT NONE
C
C INCLUDES DEFINITION TO UPDATE BANKS AGENTS WITH CLERK FLAG
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:AGTCOM.DEF'
      INCLUDE 'INCLIB:DESTRA.DEF'
C
C VARIABLES DEFINITION TO UPDATE BANKS AGENTS WITH CLERK FLAG
C
      INTEGER * 4 TERM            ! TERMINAL TO UPDATE WITH CLERK FLAG
C
      LOGICAL AGT_ONL             ! AGENT ON LINE ( YES / NO )
      LOGICAL AGT_BNK             ! AGENT BANK ( YES / NO )
C
C SET AGENT BANKS WITH CLERK FLAG ONLY FOR NORMAL VALIDATION NOT FOR INQUIRY
C
      IF(TRABUF(TSTAT) .NE. GOOD) RETURN
      IF(TRABUF(TERR)  .NE. NOER) RETURN
C
C CHECK IF TERMINAL NUMBER IS A GOOD NUMBER
C
      TERM = TRABUF(TPOFFTER)
      IF(TERM .LT. 1 .OR. TERM .GT. NUMAGT) RETURN
C
C CHECK IF TERMINAL IS AND ON LINE ANGENT AND A BANK AGENT
C
      AGT_ONL = TSBIT(AGTTAB(AGTTYP, TERM), AGTTON)
      AGT_BNK = TSBIT(AGTTAB(AGTTYP, TERM), AGTBNK)
C
C IF TERMINAL IS NOT ON LINE OR TERMINAL IS NOT BANK DON'T UPDATE
C
      IF(AGT_ONL .EQ. .FALSE. .OR. AGT_BNK .EQ. .FALSE.) RETURN
C
C UPDATE TERMINAL WITH CLERK FLAG
C
      AGTTAB(AGTNCL, TERM) = 1
C
C THIS IS THE END TO UPDATE BANKS AGENTS WITH CLERK FLAG
C
      END
