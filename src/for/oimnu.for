C  GXSRC:OIMNU.FOR
C
C V10 14-JUN-2005 FRP Modify for IPS Distribution.
C V09 10-OCT-2000 UXN AlphaIPS release.
C V08 19-NOV-1999 UXN Fix for error messages.
C V07 22-MAY-1997 WPW Fix from Rita for retries.
C V06 18-DEC-1996 HXK Reorganised for inventory report
C V05 01-FEB-1995 JJOLY UPDATED TO MATCH NEW ERROR MESSAGE MAPPING
C V04 10-FEB-1992 JPJ ADDED (GVT)
C V03 05-FEB-1992 NJA ADDED (GVT REVBYT)
C V02 04-FEB-1992 NJA ADDED (2 BYTE CHECKSUM)
C V01 13-NOV-1991 JPJ RELEASED FOR VAX (INSTANTS)
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
        SUBROUTINE OIMNU(TRABUF,OUTTAB,OUTLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
        INTEGER*4 MYCHKSUM, CHKLEN, IND, I
        INTEGER*4 ERRTYP 
        INTEGER*4 CHKDIG
C
        BYTE      OUTTAB(*)
        INTEGER*2 OUTLEN
C
        INTEGER*4   I4TEMP
        INTEGER*2   I2TEMP(2)
        BYTE        I1TEMP(4)
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
        OUTTAB(2) = 'D6'X
C
C TIME
C
        IND=5
	CALL HOST_TO_TERM(OUTTAB(IND),TRABUF(TTIM),3)
        IND=IND+3
C
C JULIAN DATE
C
	CALL HOST_TO_TERM(OUTTAB(IND),DAYJUL,2)
        IND=IND+2
C
C SERIAL NUMBER AND CHECK DIGITS
C
        CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),I4TEMP,CHKDIG)
        CALL HOST_TO_TERM(OUTTAB(IND),I4TEMP,3)
        OUTTAB(IND+3) = CHKDIG
        IND=IND+4
C
C INSTANT RESULT CODE ONE
C
        OUTTAB(IND+0) = 0
        IF(TRABUF(TIERR).NE.INOER) OUTTAB(IND+0) = 1
        IND=IND+1
C
C INSTANT RESULT CODE TWO
C
        OUTTAB(IND+0) = 0
        IF(TRABUF(TIERR).NE.INOER) OUTTAB(IND+0) = TRABUF(TIERR)
        IND=IND+1
C
C RESULT INFO
C
	CALL HOST_TO_TERM(OUTTAB(IND),TRABUF(TSINF),4)
        IND=IND+4
C
C ORDER NUMBER
C
	CALL HOST_TO_TERM(OUTTAB(IND),TRABUF(TSORD),4)
        IND=IND+4
C
C NUMBER OF ORDERS IN BATCH
C
        CALL HOST_TO_TERM(OUTTAB(IND),TRABUF(TIBCH),2)
        IND=IND+2
C
C OUT OF STOCK FLAGS
C
        DO 100 I=0,TRABUF(TIBCH)-1
          OUTTAB(IND+0) = 0
          IF(TSBIT(TRABUF(TSSTK1),I)) OUTTAB(IND+0) = 1
          IND=IND+1
100     CONTINUE
C
C
        OUTLEN=IND-1
C
C CALCULATE CHECKSUM
C
1000    CONTINUE
        I4CCITT = TRABUF(TCHK)
	CALL HOST_TO_TERM(OUTTAB(3),I4CCITT,2)
        CHKLEN=OUTLEN-1
        CALL GETCCITT(OUTTAB,1,CHKLEN,MYCHKSUM)
        I4CCITT = MYCHKSUM
	CALL HOST_TO_TERM(OUTTAB(3),I4CCITT,2)
        RETURN
        END
