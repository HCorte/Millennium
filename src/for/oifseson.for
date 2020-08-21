C  GXSRC:OIFSESON.FOR
C
C V03 10-JUN-2005 FRP Modify for IPS Distribution.
C V02 06-OCT-2000 UXN AlphaIPS release.
C V01 11-NOV-1997 DXA INITIAL RELEASE FOR UK NATIONAL LOTTERY
C
C SUBROUTINE TO BUILD FSE SIGN-ON/OFF OUTPUT MESSAGES.
C
C NOTE : THERE ARE THREE TYPES :-
C
C 1. TRABUF(TIFSETYP) = 0 > FSE SIGN-ON  WITH    PASSWORD CHECKING (SEND TO IPS)
C 2. TRABUF(TIFSETYP) = 1 > FSE SIGN-ON  WITHOUT PASSWORD CHECKING (DO NOT SEND TO IPS)
C 3. TRABUF(TIFSETYP) = 2 > FSE SIGN-OFF WITHOUT PASSWORD CHECKING (DO NOT SEND TO IPS)
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
C=======OPTIONS/CHECK=NOOVERFLOW/EXTEND_SOURCE
C
      SUBROUTINE OIFSESON(TRABUF,OUTTAB,OUTLEN)
C
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:AGTCOM.DEF'
      INCLUDE 'INCLIB:DESTRA.DEF'
      INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
      BYTE OUTTAB(*),I1TEMP(4)
C
      INTEGER*2 OUTLEN,
     +          I2TEMP(2)
C
      INTEGER*4 ERRTYP,
     +          SUCCESS,
     +          FAILURE
C
      PARAMETER (ERRTYP  = '90'X,
     +           SUCCESS = 0,
     +           FAILURE = 1)
C
      INTEGER*4 MYCHKSUM,CHKLEN,I4TEMP,CHKDIG,IND
C
      EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C     
C CONTROL AND SEQUENCE NUMBER
C
      OUTTAB(1) = '20'X + TRABUF(TTRN)
C
C IF TRANSACTION STATUS IS NOT GOOD BUILD ERROR MESSAGE
C
      IF ((TRABUF(TSTAT).NE.GOOD).AND.(TRABUF(TERR).NE.BCRS)) THEN
C
        OUTTAB(2) = ERRTYP
        OUTTAB(5) = TRABUF(TERR)
        OUTLEN    = 5
C
      ELSE
C
C TYPE AND SUBTYPE
C
         OUTTAB(2) = 'DB'X
C
C TIME
C
	 IND = 5
	 CALL HOST_TO_TERM(OUTTAB(IND),TRABUF(TTIM), 3)
	 IND = IND + 3
C
C JULIAN DATE
C
	 CALL HOST_TO_TERM(OUTTAB(IND), DAYJUL, 2)
	 IND = IND + 2
C
C SERIAL NUMBER AND CHECK DIGITS
C
         CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),I4TEMP,CHKDIG)
	 CALL HOST_TO_TERM(OUTTAB(IND), I4TEMP, 3)
	 IND = IND + 3
         OUTTAB(IND) = CHKDIG
	 IND = IND + 1
C
C RESULT CODE ONE
C
         IF (TRABUF(TIFSETYP).EQ.1) THEN      ! FSE SIGN-ON WITHOUT PASSWORD CHECKING
C
            OUTTAB(IND) = SUCCESS
C
         ELSEIF (TRABUF(TIFSETYP).NE.0) THEN  ! FSE SIGN-OFF WITHOUT PASSWORD CHECKING
C
            OUTTAB(IND) = SUCCESS
C
         ELSEIF (TRABUF(TIERR).NE.INOER) THEN
C
            OUTTAB(IND) = FAILURE
C
         ELSEIF (TRABUF(TIFSERSLT).EQ.INOER) THEN
C
            OUTTAB(IND) = SUCCESS
C
         ELSE
C
            OUTTAB(IND) = FAILURE
C
         ENDIF
	 IND = IND + 1
C
C RESULT CODE TWO
C
         OUTTAB(IND) = 0
	 IND = IND + 1
C
C FLAG
C
         IF ((TRABUF(TIFSETYP).EQ.0).OR.
     +       (TRABUF(TIFSETYP).EQ.1)) THEN  ! FSE SIGN-ON
           OUTTAB(IND) = 0
         ELSE
           OUTTAB(IND) = 1                   ! FSE SIGN-OFF
         ENDIF
	 IND = IND + 1
C
C FSE name
C
	 CALL MOVBYT(TRABUF(TIFSENAMS),1,OUTTAB,IND,TIFSEBNAM)
	 IND = IND + TIFSEBNAM

         OUTLEN = IND - 1
C
      ENDIF
C
C CALCULATE CHECKSUM
C
      I4CCITT   = TRABUF(TCHK)
      CALL HOST_TO_TERM(OUTTAB(3),I4CCITT,2)
      CHKLEN    = OUTLEN - 1
      CALL GETCCITT(OUTTAB,1,CHKLEN,MYCHKSUM)
      I4CCITT   = MYCHKSUM
      CALL HOST_TO_TERM(OUTTAB(3),I4CCITT,2)
C
      RETURN
      END
