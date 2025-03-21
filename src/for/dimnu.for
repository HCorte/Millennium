C  GXSRC:DIMNU.FOR
C
C V10 11-DEZ-2020 SCML New Terminals Project - Olimpo
C V09 14-JUN-2005 FRP Modify for IPS Distribution.
C V08 10-OCT-2000 UXN AlphaIPS release.
C V07 14-FEB-1997 RXK Things revbyted (to get correct value for trabuf) 
C V06 11-FEB-1997 RXK Fix for trabuf(TSIZE)
C V05 11-FEB-1997 RXK IMNU=instant supply message, IORD=instant games names 
C                     request message
C V04 28-JAN-1997 HXK IPS LOTGEN release
C V03 17-DEC-1996 HXK reorganised for order
C V02 10-FEB-1992 JPJ ADDED (GVT)
C V01 13-NOV-1991 JPJ RELEASED FOR VAX (INSTANTS)
C
C
C SUBROUTINE TO DECODE INSTANT SUPPLY ORDER MESSAGE FROM TERMINAL
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
	SUBROUTINE DIMNU(TERMES,TRABUF,MESLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
	INTEGER*2 MESLEN
	BYTE	  TERMES(*)
C
	INTEGER*4   TEMP
	INTEGER*4   CHKLEN, MYCHKSUM, ENCMES, ENCACT
	INTEGER*4   OPTIONS, IND, I

	INTEGER*4   I4TEMP
	BYTE	    I1TEMP(4)
	EQUIVALENCE(I4TEMP,I1TEMP(1))
C
C GET SEQUENCE NUMBER
C
        TEMP = ZEXT(TERMES(1))
        TRABUF(TTRN)=IAND(TEMP,15)
C
C GET CHECKSUM
C
	CALL TERM_TO_HOST(TERMES(3),TRABUF(TCHK),2)
C
C GET STATISTICS
C
        TRABUF(TTSTCS)=ZEXT(TERMES(5))
C
C GET OPTION FLAGS
C
	CALL TERM_TO_HOST(TERMES(6),OPTIONS,2)
        IND=8
C
C CHECK FOR NODE NUMBER (NOT USED)
C
        IF(IAND(OPTIONS,'0001'X).NE.0) THEN
           IND=IND+4
        ENDIF
C
C CHECK FOR RETAILER NUMBER (NOT USED)
C
        IF(IAND(OPTIONS,'0002'X).NE.0) THEN
           IND=IND+4
        ENDIF
C
C CHECK FOR PASSWORD (NOT USED)
C
        IF(IAND(OPTIONS,'0004'X).NE.0) THEN
           IND=IND+2
        ENDIF
C
C CHECK FOR ORIGINATOR   (NOT USED)
C
        IF(IAND(OPTIONS,'0008'X).NE.0) THEN
           IND=IND+2
        ENDIF
C
C CHECK FOR LOCATION NUMBER (NOT USED)
C
        IF(IAND(OPTIONS,'0010'X).NE.0) THEN
           IND=IND+4
        ENDIF
C
C CHECK FOR USER ID (NOT USED)
C
        IF(IAND(OPTIONS,'0020'X).NE.0) THEN
           IND=IND+4
        ENDIF
C
C CHECK FOR OPERATOR ID (NOT USED)
C
        IF(IAND(OPTIONS,'0040'X).NE.0) THEN
           IND=IND+1
        ENDIF
C
C CHECK FOR PAYMENT TYPE (NOT USED HERE)
C
        IF(IAND(OPTIONS,'0080'X).NE.0) THEN
           IND=IND+2
        ENDIF
C
C CHECK FOR VALIDATION MODE (NOT USED HERE)
C
        IF(IAND(OPTIONS,'0100'X).NE.0) THEN
           IND=IND+2
        ENDIF
C
C CHECK FOR BANK (NOT USED HERE)
C
        IF(IAND(OPTIONS,'0200'X).NE.0) THEN
           IND=IND+8
        ENDIF
C
C GET RETAILER NUMBER
C
        CALL TERM_TO_HOST(TERMES(IND),TRABUF(TAGT),4)
        IND=IND+4
C
C GET NUMBER OF ORDERS IN BATCH
C
	CALL TERM_TO_HOST(TERMES(IND),TRABUF(TIBCH),2)
        IND=IND+2
        IF(TRABUF(TIBCH).LE.0.OR.TRABUF(TIBCH).GT.TSMAX) THEN
          TRABUF(TIBCH)=1
          TRABUF(TERR)=SYNT
          SYNTERRCOD=30
          GOTO 8000
        ENDIF
C
C GET INSTANT GAME (Last nibble: game type; First 3 nibbles: game number)
C
        DO 100 I=0,TRABUF(TIBCH)-1
C
	   CALL TERM_TO_HOST(TERMES(IND),TRABUF(TSGAM+I),2)
           IND=IND+2
C
C GET ORDER QUANTITY
C
	   CALL TERM_TO_HOST(TERMES(IND),TRABUF(TSQTY+I),2)
           IND=IND+2
C
100     CONTINUE
C
C CHECK MESSAGE CHECKSUM
C
	IF(P(SUPSUM).EQ.0) THEN
          IF(.NOT.BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTSUM)) THEN
	    I4CCITT=IAND(BASECHKSUM+TRABUF(TTER),'FFFF'X)
            CALL HOST_TO_TERM(TERMES(3),I4CCITT,2)
	    CHKLEN=MESLEN-1
	    CALL GETCCITT(TERMES,1,CHKLEN,MYCHKSUM)
	    IF(MYCHKSUM.NE.TRABUF(TCHK)) TRABUF(TERR)=CBAD
	  ENDIF
	ENDIF
C
C CHECK FOR DES ERROR
C
	IF(P(DESACT).EQ.0) THEN
	  ENCMES = ZEXT(TERMES(1))
	  ENCMES = IAND(ENCMES,'08'X)
	  IF(P(DESFLG).EQ.0.AND.
     *	     BTEST(AGTTAB(AGTTYP,
     *            TRABUF(TTER)),AGTDES)) THEN
	    ENCACT='08'X
	  ELSE
	    ENCACT=0
	  ENDIF
	  IF(ENCMES.NE.ENCACT) TRABUF(TERR) = DESMOD
	ENDIF
C
C
C
8000	CONTINUE
C----+------------------------------------------------------------------
C V10| New Terminals Project - Olimpo
C----+------------------------------------------------------------------ 
        IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
           IF(TRABUF(TIBCH).GT.3 .AND. TRABUF(TIBCH).LE.24) THEN  
              TRABUF(TSIZE) = 2
           ELSE IF (TRABUF(TIBCH).GT.24) THEN 
              TRABUF(TSIZE) = 3
           ENDIF
        ELSE
           IF(TRABUF(TIBCH).GE.9.AND.TRABUF(TIBCH).LE.28) THEN
              TRABUF(TSIZE) = 2
           ELSE IF (TRABUF(TIBCH).GE.29) THEN
              TRABUF(TSIZE) = 3
           ENDIF
        ENDIF
C        IF(TRABUF(TIBCH).GE.9.AND.TRABUF(TIBCH).LE.28) THEN
C           TRABUF(TSIZE) = 2
C        ELSE IF (TRABUF(TIBCH).GE.29) THEN
C           TRABUF(TSIZE) = 3
C        ENDIF
C----+------------------------------------------------------------------
C V10| New Terminals Project - Olimpo
C----+------------------------------------------------------------------ 
	IF(TRABUF(TERR).NE.NOER) TRABUF(TSTAT)=REJT
	RETURN
	END
