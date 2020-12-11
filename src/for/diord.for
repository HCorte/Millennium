C  GXSRC:DIORD.FOR
C  
C V13 11-DEZ-2020 SCML New Terminals Project - Olimpo
C V12 10-JUN-2005 FRP Modify for IPS Distribution.
C V11 10-OCT-2000 UXN AlphaIPS release.
C V10 11-FEB-1997 RXK IMNU=instant supply message, IORD=instant games names 
C                     request message
C V09 07-FEB-1997 HXK Removed type statements
C V08 28-JAN-1997 HXK IPS LOTGEN release
C V07 19-JAN-1997 HXK Added agent number
C V06 18-DEC-1996 HXK Modifified for IPS release, Finland
C V05 05-DEC-1996 HXK Updated for Finland IPS pre-release
C V04 10-FEB-1992 JPJ ADDED (GVT)
C V03 04-FEB-1992 JPJ ADDED (GVT REVBYT)
C V02 04-FEB-1992 JPJ ADDED (2 BYTE CHECKSUM)
C V01 13-NOV-1991 JPJ RELEASED FOR VAX (INSTANTS)
C
C SUBROUTINE TO DECODE INSTANT GAME PARAMETERS REQUEST MESSAGE FROM TERMINAL
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
	SUBROUTINE DIORD(TERMES,TRABUF,MESLEN)
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
	INTEGER*4   OPTIONS, IND

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
C CHECK FOR RETAILER NUMBER
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
C GAME NUMBER
C
	CALL TERM_TO_HOST(TERMES(IND),TRABUF(TGPGAM),2)
        IND=IND+2
C
C CONTINUATION GAME NUMBER
C
	CALL TERM_TO_HOST(TERMES(IND),TRABUF(TGPNXT),2)
        IND=IND+2
C
C RETAILER NUMBER OF CREDIT LIMIT
C
	CALL TERM_TO_HOST(TERMES(IND),TRABUF(TGPAGT),4)
        IND=IND+4
C
C
C CHECK MESSAGE CHECKSUM
C
	IF(P(SUPSUM).EQ.0) THEN
          IF(.NOT.BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTSUM)) THEN
	    I4CCITT=IAND(BASECHKSUM+TRABUF(TTER),'FFFF'X)
	    CALL HOST_TO_TERM(TERMES(3),I4CCITT,2)
	    CHKLEN=MESLEN-1
	    CALL GETCCITT(TERMES,1,CHKLEN,MYCHKSUM)
	    IF(MYCHKSUM.NE.TRABUF(TCHK)) THEN
	       TRABUF(TERR)=CBAD
	       GOTO 8000
	    ENDIF
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
	  IF(ENCMES.NE.ENCACT) THEN
	     TRABUF(TERR) = DESMOD
	     GOTO 8000
	  ENDIF
	ENDIF
C
C
C
8000	CONTINUE
C----+------------------------------------------------------------------
C V13| New Terminals Project - Olimpo
C----+------------------------------------------------------------------ 
	IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
		TRABUF(TSIZE) = 2
	ENDIF	
C----+------------------------------------------------------------------
C V13| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
	IF(TRABUF(TERR).NE.NOER) TRABUF(TSTAT)=REJT
	RETURN
	END
