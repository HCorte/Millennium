C
C SUBROUTINE DCAN
C $Log:   GXAFXT:[GOLS]DCAN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:48:42   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   09 Feb 1993 13:22:56   EBD
C  Assigned DUMYEAR to DAYYER prior to call of julcdc
C  
C     Rev 1.0   21 Jan 1993 16:02:26   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - dcan.for **
C
C DCAN.FOR
C
C V02 05-JAN-93 TD  CHANGED CALL TO JULCDC
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V02 01-NOV-91 MTK INITAL RELEASE FOR NETHERLANDS
C
C SUBROUTINE TO DECODE CANCEL MESSAGE FROM WAGERING TERMINAL
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DCAN(TERMES,TRABUF,MESLEN)
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
	INTEGER*4 MESS(EDLEN)
        INTEGER*4 DUMYEAR
	INTEGER*4 ENCMES, ENCACT, MYCHKSUM, CHKLEN, CERR, INPVER
	INTEGER*4 CDC, CDIG, TEMP, TER, SER, JUL, TEMP1, TEMP2
	BYTE TERMES(*)
C
	JUL=0
	SER=0
	TER=0
C
C
	TEMP=ZEXT(TERMES(1))
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
C GET JULIAN DATE
C
        TEMP1 = ZEXT(TERMES(6))
        TEMP2 = ZEXT(TERMES(7))
        JUL   = ISHFT(TEMP1,8) + TEMP2
C
C GET SERIAL NUMBER AND CHECK DIGITS
C
	CALL MOVBYTN(TERMES,8,SER,2,3)
	CDIG=ZEXT(TERMES(11))
C
C
	DUMYEAR = DAYYER
	CALL JULCDC(JUL,CDC,DUMYEAR)
	CERR=INPVER(CDC,SER,TRABUF(TWCSER),CDIG)
	IF(CERR.NE.0.OR.CDC.NE.DAYCDC) TRABUF(TERR)=INVL
C
C
C CHECK MESSAGE CHECKSUM
C
        IF(P(SUPSUM).EQ.0) THEN
	  IF(.NOT.BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTSUM)) THEN
            I4CCITT   = IAND(BASECHKSUM+TRABUF(TTER),'FFFF'X)
            TERMES(3) = I1CCITT(2)
            TERMES(4) = I1CCITT(1)
            CHKLEN=MESLEN-1
            CALL GETCCITT(TERMES,1,CHKLEN,MYCHKSUM)
	    IF(MYCHKSUM.NE.TRABUF(TCHK)) TRABUF(TERR)=CBAD
	  ENDIF
	ENDIF
C
C CHECK FOR DES ERROR
C
	IF(P(DESACT).EQ.0) THEN
	  ENCACT=0
	  CALL ILBYTE(ENCMES,TERMES,0)
	  ENCMES=IAND(ENCMES,'08'X)
	  IF(P(DESFLG).EQ.0.AND.
     *	     TSBIT(AGTTAB(AGTTYP,TRABUF(TTER)),AGTDES)) ENCACT=1
	  IF(ENCMES.EQ.0.AND.ENCACT.NE.0.OR.
     *	     ENCMES.NE.0.AND.ENCACT.EQ.0) TRABUF(TERR)=DESMOD
	ENDIF
C
C
	IF(TRABUF(TERR).NE.NOER) TRABUF(TSTAT)=REJT
	RETURN
	END
