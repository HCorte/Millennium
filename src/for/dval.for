C
C SUBROUTINE DVAL
C $Log:   GXAFXT:[GOLS]DVAL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:02:12   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   10 Sep 1993 18:36:18   GXA
C  Changed TWBNK to TVBNK in decoding of bank info.
C  
C     Rev 1.1   22 Aug 1993 20:21:32   GXA
C  Released for Finland Dec Conversion / Oddset.
C  
C     Rev 1.0   21 Jan 1993 16:11:00   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - dval.for **
C
C DVAL.FOR
C
C V03 08-OCT-2013 SCML New validation messages
C V02 01-NOV-1991 MTK  INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C
C SUBROUTINE TO DECODE VALIDATION MESSAGE FROM WAGERING TERMINAL
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DVAL(TERMES,TRABUF,MESLEN)
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
	INTEGER*4   JUL, SER, TER, TEMP, TEMP1, TEMP2, TEMP3, TEMP4
	INTEGER*4   VTYP1, VTYP2, CDIG, YEAR, CERR, INPVER, CDC, IND
	INTEGER*4   CHKLEN, MYCHKSUM, ENCMES, ENCACT, SUBTYP

C----+------------------------------------------------------------------
C V03| New validation messages
C----+------------------------------------------------------------------
        INTEGER*8   NIB
        BYTE        I1NIB(8)
        EQUIVALENCE (NIB,I1NIB)    
C----+------------------------------------------------------------------
C V03| New validation messages
C----+------------------------------------------------------------------
C
C
	JUL=0
	SER=0
	TER=0
	IND=1
C
C
	TEMP = ZEXT(TERMES(IND+0))
	TRABUF(TTRN)=IAND(TEMP,15)
        TEMP=ZEXT(TERMES(IND+1))
        SUBTYP=IAND(TEMP,'0F'X)
	IND = IND + 2
C
C GET CHECKSUM AND STATISTICS
C
        TEMP1 = ZEXT(TERMES(IND+0))
        TEMP2 = ZEXT(TERMES(IND+1))
        TRABUF(TCHK) = ISHFT(TEMP1,8) + TEMP2
 	TRABUF(TTSTCS)=ZEXT(TERMES(IND+2))
	IND = IND + 3
C
C GET JULIAN DATE
C
	TEMP1 = ZEXT(TERMES(IND+0))
	TEMP2 = ZEXT(TERMES(IND+1))
	JUL   = ISHFT(TEMP1,8) + TEMP2
	IND = IND + 2
C
C GET SERIAL NUMBER AND CHECK DIGITS
C
	TEMP1 = ZEXT(TERMES(IND+0))
	TEMP2 = ZEXT(TERMES(IND+1))
	TEMP3 = ZEXT(TERMES(IND+2))
	SER   = ISHFT(TEMP1,16) + ISHFT(TEMP2,8) + TEMP3
	CDIG  = ZEXT(TERMES(IND+3))
	IND = IND + 4
C
C GET BANK ID# AND ACCOUNT # IF 'PAY TO BANK'
C
	IF(SUBTYP.EQ.VPTB) THEN
           TEMP1 = ZEXT(TERMES(IND+0))
           TEMP2 = ZEXT(TERMES(IND+1))
           TEMP3 = ZEXT(TERMES(IND+2))
           TEMP4 = ZEXT(TERMES(IND+3))
           TRABUF(TVBNKID) =  ISHFT(TEMP1,24) + ISHFT(TEMP2,16) +
     *                        ISHFT(TEMP3,8) + TEMP4
           IND=IND+4
C
           TEMP1 = ZEXT(TERMES(IND+0))
           TEMP2 = ZEXT(TERMES(IND+1))
           TEMP3 = ZEXT(TERMES(IND+2))
           TEMP4 = ZEXT(TERMES(IND+3))
           TRABUF(TVBNKNUM) = ISHFT(TEMP1,24) + ISHFT(TEMP2,16) +
     *                        ISHFT(TEMP3,8) + TEMP4
	   IND = IND + 4
C
	   IF(TRABUF(TVBNKID).LT.0.OR.TRABUF(TVBNKID).GT.999999.OR.
     *        TRABUF(TVBNKNUM).LT.0.OR.TRABUF(TVBNKNUM).GT.99999999)THEN
	      TRABUF(TERR) = INVL
	   ENDIF
	ENDIF
C----+------------------------------------------------------------------
C V03| New validation messages
C----+------------------------------------------------------------------
        ! Only set values if subtype is:
        IF(SUBTYP.EQ.VNBNK) THEN ! New validation bank transfer accepted

           ! Setting Id Type: telephone number = 0, player card = 1
           TRABUF(TVPLIDTYP) = ZEXT(TERMES(IND))
           IND = IND + 1
           
           ! Setting Player Card / Telephone Number
           TEMP1 = ZEXT(TERMES(IND+0))
           TEMP2 = ZEXT(TERMES(IND+1))
           TEMP3 = ZEXT(TERMES(IND+2))
           TEMP4 = ZEXT(TERMES(IND+3))
           TRABUF(TVPLCARD) = ISHFT(TEMP1,24) + ISHFT(TEMP2,16) +
     *                        ISHFT(TEMP3,8) + TEMP4
           IND = IND + 4

           ! Setting NIB: Bank Branch - 2 bytes
           TEMP1 = ZEXT(TERMES(IND+0))
           TEMP2 = ZEXT(TERMES(IND+1))
           TRABUF(TVNIBBB) = ISHFT(TEMP1,8) + TEMP2
           IND = IND + 2

           ! Setting NIB: Bank Office - 2 bytes
           TEMP1 = ZEXT(TERMES(IND+0))
           TEMP2 = ZEXT(TERMES(IND+1))
           TRABUF(TVNIBBO) = ISHFT(TEMP1,8) + TEMP2
           IND = IND + 2

           ! Setting NIB: Account Number - 5 bytes
           I1NIB(5) = ZEXT(TERMES(IND+0))
           I1NIB(4) = ZEXT(TERMES(IND+1))
           I1NIB(3) = ZEXT(TERMES(IND+2))
           I1NIB(2) = ZEXT(TERMES(IND+3))
           I1NIB(1) = ZEXT(TERMES(IND+4))
           IND = IND + 5
           TRABUF(TVNIBBA1) = NIB/100
           TRABUF(TVNIBBA2) = MOD(NIB,100)

           ! Setting NIB: Check digits - 1 byte
           TRABUF(TVNIBCD) = ZEXT(TERMES(IND))
           IND = IND + 1
           
        ENDIF
C----+------------------------------------------------------------------
C V03| New validation messages
C----+------------------------------------------------------------------
	YEAR=DAYYER
	CALL JULCDC(JUL,TRABUF(TVCDC),YEAR)
	CERR=INPVER(TRABUF(TVCDC),SER,TRABUF(TVSER),CDIG)
	IF(CERR.NE.0) THEN
	   YEAR=DAYYER-1
	   IF(YEAR.LT.0) YEAR=99
	   CALL JULCDC(JUL,CDC,YEAR)
	   IF(CDC.GT.0) CERR=INPVER(CDC,SER,TRABUF(TVSER),CDIG)
	   TRABUF(TVCDC) = CDC
	ENDIF
C
	IF(CERR.NE.0) THEN
	   TRABUF(TERR)=INVL
	   TRABUF(TVCODE)=BSER
	ENDIF
C
C
	TRABUF(TVTYPE) = SUBTYP
C----+------------------------------------------------------------------
C V03| New validation messages
C----+------------------------------------------------------------------
C        IF(SUBTYP.LT.0.OR.SUBTYP.GT.VPTB.OR.SUBTYP.EQ.VINS) 
        IF(      SUBTYP .LT. 0 
     *      .OR. SUBTYP .GT. VNIBO
     *      .OR. SUBTYP .EQ. VINS) THEN
C----+------------------------------------------------------------------
C V03| New validation messages
C----+------------------------------------------------------------------
           TRABUF(TERR) = INVL
        ENDIF
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
	   ENCMES = ZEXT(TERMES(1))
	   ENCMES = IAND(ENCMES,'08'X)
	   IF(P(DESFLG).EQ.0.AND.
     *	      BTEST(AGTTAB(AGTTYP,
     *        TRABUF(TTER)),AGTDES)) THEN
	      ENCACT='08'X
	   ELSE
	      ENCACT=0
	   ENDIF
	   IF(ENCMES.NE.ENCACT) TRABUF(TERR) = DESMOD
	ENDIF
C
C
	IF(TRABUF(TERR).NE.NOER) TRABUF(TSTAT)=REJT
	RETURN
	END
