C  GXSRC:DIEST.FOR
C  
C  $Log:   GXAFIP:[GOLS]DIEST.FOV  $
C  
C     Rev 1.4   11 Feb 1997 18:48:42   RXK
C  Fix for trabuf(TSIZE)
C  
C     Rev 1.3   05 Feb 1997 17:04:32   WPW
C  Change saving RomRev.
C  
C     Rev 1.2   19 Jan 1997 17:13:46   HXK
C  Added two bytes (empty) to message length for consistency with terminal
C  
C     Rev 1.1   05 Dec 1996 20:32:30   HXK
C  Updated for Finland IPS pre-release
C  
C     Rev 1.0   17 Apr 1996 12:52:30   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   21 Jan 1995 17:16:02   JJOLY
C  DES IS NOT NEEDED ON IEST
C  
C     Rev 1.1   03 Jan 1994 20:12:34   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 17:37:22   SYSTEM
C  Initial revision.
C
C
C
C V03 21-Jan-93 ceb Added tracking of the APP. Rom Rev. RFSS TX1115-68
C                   Removed GVT FMT for better tracking of
C                   ESTABLISHMENT messages.    RFSS TX1115-69
C V02 10-FEB-92 JPJ ADDED (GVT)
C V01 13-NOV-91 JPJ RELEASED FOR VAX (INSTANTS)
C
C SUBROUTINE TO DECODE INSTANT FINANCIAL REPORT MESSAGE FROM TERMINAL
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
C Copyright 1992 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DIEST(TERMES,TRABUF,MESLEN)
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
	INTEGER*4   OPTIONS, IND, X
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
	IND=5

C
C CHECK FOR DES ERROR
C
c...    IF(P(DESACT).EQ.0) THEN
            ENCMES = TRABUF(TTRN)
            ENCMES = IAND(ENCMES,'08'X)

c...        IF(P(DESFLG).EQ.0.AND.
c... *         BTEST(AGTTAB(AGTTYP,TER),AGTDES)) THEN
c...           ENCACT = '08'X
c...        ELSE

               ENCACT = 0

c...        ENDIF

            IF(ENCMES.NE.ENCACT) TRABUF(TERR)=DESMOD

c...    ENDIF

C
C GET SOFTWARE REVISION
C
	TEMP1 = ZEXT(TERMES(IND+3))
	TEMP2 = ZEXT(TERMES(IND+2))
        TEMP3 = ZEXT(TERMES(IND+1))
        TEMP4 = ZEXT(TERMES(IND+0))
	TRABUF(TISFT) = ISHFT(TEMP1,24) + ISHFT(TEMP2,16) + 
     *                  ISHFT(TEMP3,8)  + TEMP4
	IND=IND+4
	AGTTAB(AGTROM,TRABUF(TTER)) = TRABUF(TISFT)
C
C GET FORMAT REVISION (Not Used)
C
	IND=IND+1
C
C GET FREESPACE  (Not Used)
C
	IND=IND+2
C
C CHECK MESSAGE CHECKSUM
C
	IF(P(SUPSUM).EQ.0) THEN
          IF(.NOT.BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTSUM)) THEN
	    I4CCITT=0
	    TERMES(3) = I1CCITT(2)
            TERMES(4) = I1CCITT(1)
	    CHKLEN=MESLEN-1
	    CALL GETCCITT(TERMES,1,CHKLEN,MYCHKSUM)
	    IF(MYCHKSUM.NE.TRABUF(TCHK)) TRABUF(TERR)=CBAD
	  ENDIF
	ENDIF
C
8000	CONTINUE
        TRABUF(TSIZE)=2
	IF(TRABUF(TERR).NE.NOER) TRABUF(TSTAT)=REJT
	RETURN
	END
