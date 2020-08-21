C  GXSRC:OIVAL.FOR
C
C V14 08-NOV-2013 SCML Set bank validation mode and net cash amount only
C                      if ticket status is 89 (inquiry) or 99 (INOER).
C                      Send net cash amount only if the agent is privileged.
C V13 17-OCT-2013 SCML Added new bank validation mode.
C V12 19-FEB-2001 UXN Portugal changes.
C V11 06-OCT-2000 UXN AlphaIPS release.
C V10 25-APR-2000 LAH IF IPS SENDS RESULT CODE 16, SEND 16 TO 
C                     TERMINAL [RFC 2774]
C V09 17-AUG-1998 NA  ADDED CLAIM LEVEL 3 RESULT CODE 16 [RFC 2021]
C V08 21-MAR-1997 DXA ADDED PLAY-AT-HOME IPS RESULT CODE 38 (GAME OVER)
C V07 14-JAN-1997 DXA ADDED ACTIVATION CUTOFF (IPS RESULT CODES 36 & 37)
C V06 29-NOV-1996 DXA ADDED SHOW DATE & NUMBER FOR PLAY-AT-HOME
C V05 01-JUL-1992 NJA ADDED NEW CODE FOR VALIDATION STATUS
C V04 10-FEB-1992 JPJ ADDED (GVT)
C V03 05-FEB-1992 NJA ADDED (GVT REVBYT)
C V02 04-FEB-1992 NJA ADDED (2 BYTE CHECKSUM)
C V01 13-NOV-1991 JPJ RELEASED FOR VAX (INSTANTS)
C
C SUBROUTINE TO BUILD INSTANT VALIDATION OUTPUT MESSAGES.
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE OIVAL(TRABUF,OUTTAB,OUTLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
        INCLUDE 'INCLIB:NON_CASH_PRIZE.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF' !V14
C
        INTEGER*4 MYCHKSUM, CHKLEN, IND
        INTEGER*4 ERRTYP, ERRCOD
        BYTE      OPTFLAG     !V13
        INTEGER*4 OPTFLAG_IDX !V13
	INTEGER*4 CHKDIG, I
C
	BYTE      OUTTAB(*)
	INTEGER*2 OUTLEN
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
	DATA ERRTYP/Z90/
C
C CONTROL AND SEQUENCE NUMBER
C
	OUTTAB(1) = '20'X+TRABUF(TTRN)
C
C IF TRANSACTION STATUS IS NOT GOOD
C BUILD ERROR MESSAGE.
C
	IF(TRABUF(TSTAT).NE.GOOD.AND.TRABUF(TERR).NE.BCRS) THEN
	  OUTTAB(2) = ERRTYP
	  OUTTAB(5) = TRABUF(TERR)
	  OUTLEN=5
	  GOTO 1000
	ENDIF
C
C TYPE AND SUBTYPE
C
	OUTTAB(2) = 'DD'X
C
C TIME
C
	IND=5
	CALL HOST_TO_TERM(OUTTAB(IND),TRABUF(TTIM),3)
	IND=IND+3
C
C JULIAN DATE
C
	CALL HOST_TO_TERM(OUTTAB(IND), DAYJUL, 2)
	IND=IND+2
C
C SERIAL NUMBER AND CHECK DIGITS
C
	CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),I4TEMP,CHKDIG)
	CALL HOST_TO_TERM(OUTTAB(IND), I4TEMP, 3)
	OUTTAB(IND+3) = CHKDIG
	IND=IND+4
C
C INSTANT RESULT CODE ONE
C
	IF(TRABUF(TIERR).EQ.INOER) THEN
	   ERRCOD=0
	ELSE
	   ERRCOD=1
	ENDIF
	OUTTAB(IND+0) = ERRCOD
	IND=IND+1
C
C
C INSTANT RESULT CODE TWO
C
        OUTTAB(IND) = TRABUF(TIBCH)
        IF(TRABUF(TIERR) .NE. INOER) OUTTAB(IND) = TRABUF(TIERR)
        IF(OUTTAB(IND) .EQ. INOER .AND. TRABUF(TIERR) .NE. INOER) THEN
          OUTTAB(IND) = INCNS
        ENDIF
	      IND = IND + 1
C
        IF (TRABUF(TIVMT) .EQ. IBVMT) THEN ! NEW BANK VALIDATION MODE LAYOUT !V13
          IF (TRABUF(TIVTYP) .EQ. IVBM_NOPRZ) THEN
            
            ! NON-CASH PRIZE: set up option flag '08'X
            OUTTAB(IND+0) = '08'X
            IND = IND + 1
            
            ! TICKET RESULT CODE
            ERRCOD = TRABUF(TISTS1)
            IF(TRABUF(TISTS1).EQ.INOER) ERRCOD = 0
            OUTTAB(IND) = ERRCOD
            IND = IND + 1
            
            ! PACK STATUS FOR PRIV. TERMINAL
            OUTTAB(IND) = TRABUF(TIPCKSTS1)
            IND = IND + 1
            
            ! PRIZE DESCRIPTION
            CALL MOVBYT(TRABUF(TIVDESCR),1,OUTTAB(IND),1,20)
            IND = IND + 20
            
          ELSE
            OPTFLAG = '00'X !V14
            OPTFLAG_IDX = IND ! SAVE OPTION FLAG INDEX
            IND = IND + 1

            ! IF TICKET STATUS IS 89 (INQUIRY) OR 99 (INOER) THEN
            !   - SET BANK VALIDATION MODE BIT FLAG AND OPTION DATA
            !   - SET NET CASH AMOUNT BIT FLAG AND OPTION DATA IF AGENT IS PRIVILEGED
            IF(TRABUF(TISTS1).EQ.89 .OR. TRABUF(TISTS1).EQ.INOER) THEN !V14
              OPTFLAG = '01'X
              OUTTAB(IND+0) = TRABUF(TIVTYP) ! VALUE OF BANK VALIDATION MODE
              IND = IND + 1
              IF(TSBIT(AGTTAB(AGTTYP,TRABUF(TTER)),AGTPRV)) THEN
                IF(TRABUF(TIVTYP) .EQ. IVBM_CSH .OR. TRABUF(TIVTYP) .EQ. IVBM_BNK) THEN
                  IF(TRABUF(TINETPRZ).GT.0 .AND. TRABUF(TINETPRZ).GT.P(VALPRZHI)) THEN
                    OPTFLAG = IOR('02'X, OPTFLAG) ! SET NET CASH AMOUNT BIT FLAG
                    CALL HOST_TO_TERM(OUTTAB(IND+0), TRABUF(TINETPRZ), 4)
                    IND = IND + 4
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          
            ! OPTION FLAG
            OUTTAB(OPTFLAG_IDX) = OPTFLAG

            ! VALIDATION STATUS
            OUTTAB(IND+0) = TRABUF(TISTS1)
            IF(TRABUF(TISTS1) .EQ. INOER) OUTTAB(IND+0) = 0
            IND = IND + 1
            
            ! PRIZE AMOUNT OF VALIDATED TICKET (IN CENTS)
            CALL HOST_TO_TERM(OUTTAB(IND+0), TRABUF(TIPRZ1), 4)
            IND = IND + 4
            
            ! PACK STATUS
            OUTTAB(IND) = TRABUF(TIPCKSTS1)
            IND = IND + 1
          ENDIF
          
        ELSEIF(TRABUF(TIVMT) .EQ. IRVMT) THEN ! OLD VALIDATION MODE LAYOUT
        
          IF ( TRABUF(TIVTYP) .EQ. IVTP_NCP ) THEN
C
C Low-Tier Non-Cash Prize.  Set up Option flag '08'X
C
            OUTTAB(IND+0) = '08'X
            IND = IND + 1

            ERRCOD = TRABUF(TISTS1)
            IF(TRABUF(TISTS1).EQ.INOER) ERRCOD = 0
            OUTTAB(IND) = ERRCOD
            IND = IND + 1
C
C Pack status for priv. terminal
C
            OUTTAB(IND) = TRABUF(TIPCKSTS1)
            IND = IND + 1
C
C Prize Description
C
            CALL MOVBYT(TRABUF(TIVDESCR),1,OUTTAB(IND),1,20)
            IND = IND + 20
          ELSE
C
C "Normal" Instant validation.  Set up Option flag '00'X
C
            OUTTAB(IND+0) = '00'X
            IND = IND + 1
C
C Validation Status
C
            DO 100 I=0,TRABUF(TIBCH)-1
              OUTTAB(IND+0) = TRABUF(TISTS1+I)
              IF(TRABUF(TISTS1+I).EQ.INOER) OUTTAB(IND+0) = 0
              IND = IND + 1
C
C Amount Won (in cents)
C
              CALL HOST_TO_TERM(OUTTAB(IND), TRABUF(TIPRZ1+I), 4)
              IND = IND + 4
C
C Pack Status
C
              OUTTAB(IND) = TRABUF(TIPCKSTS1+I)
              IND = IND + 1
C
100         CONTINUE
          ENDIF
        ENDIF
C
        OUTLEN=IND-1
C
C CALCULATE CHECKSUM
C
1000	CONTINUE
C
	I4CCITT = TRABUF(TCHK)
	CALL HOST_TO_TERM(OUTTAB(3), I4CCITT, 2)
	CHKLEN=OUTLEN-1
	CALL GETCCITT(OUTTAB,1,CHKLEN,MYCHKSUM)
	I4CCITT = MYCHKSUM

	CALL HOST_TO_TERM(OUTTAB(3), I4CCITT, 2)
C
	RETURN
	END
