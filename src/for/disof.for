C  GXSRC:DISOF.FOR
C  
C  $Log:   GXAFIP:[GOLS]DISOF.FOV  $
C  
C     Rev 1.2   28 Jan 1997 19:50:40   HXK
C  IPS LOTGEN release
C  
C     Rev 1.1   05 Dec 1996 20:32:52   HXK
C  Updated for Finland IPS pre-release
C  
C     Rev 1.0   17 Apr 1996 12:54:18   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   08 Jun 1994 13:26:24   MCM
C  CHANGED OPERATIONAL STATUS FROM A HALFWORD TO A BYTE
C  
C     Rev 1.1   03 Jan 1994 20:14:38   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 17:38:00   SYSTEM
C  Initial revision.
C
C
C
C V02 10-FEB-92 JPJ ADDED (GVT)
C V01 13-NOV-91 JPJ RELEASED FOR VAX (INSTANTS)
C
C
C SUBROUTINE TO DECODE INSTANT SIGN OFF MESSAGE FROM TERMINAL
C
C
C
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
	SUBROUTINE DISOF(TERMES,TRABUF,MESLEN)
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
	INTEGER*4 MESS(EDLEN)
	BYTE	  TERMES(*)
C
	INTEGER*4   TER, TEMP, TEMP1, TEMP2, TEMP3, TEMP4
	INTEGER*4   CHKLEN, MYCHKSUM, ENCMES, ENCACT, SUBTYP
	INTEGER*4   IND
	INTEGER*4   OPTIONS
C
C GET SEQUENCE NUMBER
C
	TEMP = ZEXT(TERMES(1))
	TRABUF(TTRN)=IAND(TEMP,15)
C
C GET CHECKSUM
C
	TEMP1 = ZEXT(TERMES(3))
        TEMP2 = ZEXT(TERMES(4))
	TRABUF(TCHK) = ISHFT(TEMP1,8) + TEMP2
C
C GET STATISTICS
C
 	TRABUF(TTSTCS)=ZEXT(TERMES(5))
C
C GET OPTION FLAGS
C
        TEMP1 = ZEXT(TERMES(7))
        TEMP2 = ZEXT(TERMES(6))
        OPTIONS = ISHFT(TEMP1,8) + TEMP2
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
C SIGN OF TERMINAL
C
	TER=TRABUF(TTER)
	TRABUF(TIOLD)=AGTHTB(AOPSTS,TER)
	TRABUF(TINEW)=SIGNOF
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
	IF(TRABUF(TERR).NE.NOER) TRABUF(TSTAT)=REJT
	RETURN
	END
