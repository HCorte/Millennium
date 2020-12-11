C  GXSRC:DILOT.FOR
C
C V13 11-DEZ-2020 SCML New Terminals Project - Olimpo
C V12 13-JUN-2005 FRP Modify for IPS Distribution.
C V11 09-OCT-2000 UXN AlphaIPS release.
C V10 26-FEB-1997 RXK Starting and ending sequence numbers revbyted
C V09 18-FEB-1997 HXK Cleaned up hack for AGTXFR
C V08 12-FEB-1997 WPW Fix for TLREP.
C V07 07-FEB-1997 HXK Hack for AGTXFR (temporarily using AGTHCH)
C V06 06-FEB-1997 RXK Check if lot transfer is enabled  
C V05 28-JAn-1997 HXK IPS LOTGEN release
C V04 10-DEC-1996 HXK Revbyte pack number
C V03 05-DEC-1996 HXK Updated for Finland IPS pre-release
C V02 10-FEB-1992 JPJ ADDED (GVT)
C V01 13-NOV-1991 JPJ RELEASED FOR VAX (INSTANTS)
C
C SUBROUTINE TO DECODE INSTANT LOT MESSAGE FROM TERMINAL
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DILOT(TERMES,TRABUF,MESLEN,AGTPAS,SALPAS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
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
	INTEGER*4   OPTIONS, SALPAS, AGTPAS, IND
C
C GET SEQUENCE NUMBER
C
	TEMP = ZEXT(TERMES(1))
	TRABUF(TTRN)=IAND(TEMP,15)
C
C GET CHECKSUM
C
	CALL TERM_TO_HOST(TERMES(3), TRABUF(TCHK), 2)
C
C GET STATISTICS
C
 	TRABUF(TTSTCS)=ZEXT(TERMES(5))
C
C GET OPTION FLAGS
C
	CALL TERM_TO_HOST(TERMES(6), OPTIONS, 2)
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
C GET AGENT PASS CODE
C
	CALL TERM_TO_HOST(TERMES(IND), AGTPAS, 2)
	IND=IND+2
C
C GET SALES REP NUMBER
C
	CALL TERM_TO_HOST(TERMES(IND), TRABUF(TLREP), 4)
	IND=IND+4
C
C GET SALES REP PASS CODE
C
	CALL TERM_TO_HOST(TERMES(IND), SALPAS, 2)
	IND=IND+2
C
C GET CLASS
C
	TRABUF(TLCLS) = ZEXT(TERMES(IND))
	IND=IND+1
C

        IF(TRABUF(TLCLS).EQ.7) THEN           ! pack transfer
           IF(TSBIT(AGTTAB(AGTTYP,TRABUF(TTER)),AGTXFR).EQ.0) THEN 
              TRABUF(TERR) = SUPR
              GOTO 8000
           ENDIF
        ENDIF
C
C GET GAME NUMBER
C

        CALL TERM_TO_HOST(TERMES(IND), TRABUF(TLGAM), 2)
	IND=IND+2
C
C GET PACK NUMBER
C
	CALL TERM_TO_HOST(TERMES(IND), TRABUF(TLPCK), 4)
	IND=IND+4
C
C GET START END END SEQUENCE IF NEEDED
C
	IF(TRABUF(TLCLS).EQ.2.OR.TRABUF(TLCLS).EQ.5) THEN
	  CALL TERM_TO_HOST(TERMES(IND), TRABUF(TLSTR), 2)
	  IND=IND+2
C
	  CALL TERM_TO_HOST(TERMES(IND), TRABUF(TLEND), 2)
	  IND=IND+2
C
          IF(AGTPAS.EQ.0.OR.TRABUF(TLREP).EQ.0.OR.SALPAS.EQ.0) THEN
             TRABUF(TERR)=SYNT
             SYNTERRCOD=80    !!??
             GOTO 8000
          ENDIF
C
	ENDIF
C
C CHECK MESSAGE CHECKSUM
C
	IF(P(SUPSUM).EQ.0) THEN
          IF(.NOT.BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTSUM)) THEN
	    I4CCITT=IAND(BASECHKSUM+TRABUF(TTER),'FFFF'X)
	    CALL HOST_TO_TERM(TERMES(3), I4CCITT, 2)
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
