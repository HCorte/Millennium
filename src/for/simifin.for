C  GXSRC:SIMIFIN.FOR
C  
C V03 08-JUL-2005 FRP Modify for IPS Distribution.
C
C  $Log:   GXAFXT:[GOLS]SIMIFIN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:06:32   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.8   30 Sep 1994  2:14:32   MCM
C  CORRECT THE FORMATS OF THE INVOICE MESSAGE
C  
C     Rev 1.7   22 Sep 1994  8:00:22   MCM
C  START INDEX AT 2 (BYTE 1 IS RESERVED)
C  
C     Rev 1.6   13 Sep 1994  3:36:38   MCM
C  SET INVOICE REPORT VALUES ALL TO ZERO
C  
C     Rev 1.5   08 Sep 1994 14:07:20   MCM
C  SWAPPING BYTES IS NO LONGER NECESSARY ON THE DEC LMS
C  
C     Rev 1.4   30 Aug 1994 16:41:06   MCM
C  MODIFIED THE ADJUSTMENT REPORT PER LMS REQUEST
C  
C     Rev 1.3   22 Aug 1994  9:03:10   MCM
C  MODIFIED THE ADJUSTMENT TEXT 
C  
C     Rev 1.2   28 Jul 1994 13:32:06   MCM
C  ADDED PACK SETTTLED, ADJUSTMENT TEXT, REPORT AND COMBINED INVOICE
C  
C     Rev 1.1   03 Jan 1994 22:45:24   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 18:30:30   SYSTEM
C  Initial revision.
C
C
C
C V01 13-MAY-93 MCM RELEASED FOR GEORGIA
C
C V02 28-MAY-92 TKO UPDATED FOR NEW MESSAGE FORMATS
C V01 13-NOV-91 JPJ RELEASED FOR VAX (INSTANTS)
C
C SUBROUTINE TO FILL IN CROSS SYSTEM ORDER MESSAGE.
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
C Copyright 1994 GTECH Corporation. OUTTAB(IND+0)=I1TEMP(4) rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SIMIFIN(OUTTAB,TRABUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
        INTEGER*4 IND, I, ADJTEXT(3,10)
	BYTE      OUTTAB(*)
        DATA      ADJTEXT/'PAYM','ENT ','    ',
     *                    'NSF ','    ','    ',
     *                    'DEBI','T   ','    ',
     *                    'CRED','IT  ','    ',
     *                    'GVT ','2ND ','PAY ',
     *                 15*'    '/ 
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
C FILL IN LENGTH
C
	IND=2
        IF(TRABUF(TRTYP).EQ.IPSET) THEN
	  I4TEMP=60
        ELSEIF(TRABUF(TRTYP).EQ.IADJR) THEN
	  I4TEMP=80
        ELSEIF(TRABUF(TRTYP).EQ.IADJT) THEN
	  I4TEMP=136
        ELSE
          I4TEMP=76
        ENDIF
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
C
C FILL IN NUMBER OF ENTRIES (DAYS)
C
        IND=12
        IF(TRABUF(TRTYP).EQ.IPSET) THEN
	  I4TEMP=1
        ELSEIF(TRABUF(TRTYP).EQ.IADJR) THEN
	  I4TEMP=3
        ELSE
          I4TEMP=0
        ENDIF
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
C
C FILL IN ERROR CODE
C
	IND=14
	I4TEMP=99
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	IND=IND+2
C
C GET PACK SETTLED REPORT INFORMATION
C
        IF(TRABUF(TRTYP).EQ.IPSET) THEN
          IND=16
C
C GET CONTINUATION GAME NUMBER
C
          I4TEMP=0
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          IND=IND+2
C
C GET CONTINUATION PACK NUMBER
C
          I4TEMP=0
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          OUTTAB(IND+2)=I1TEMP(3)
          OUTTAB(IND+3)=I1TEMP(4)
          IND=IND+4
C
C RETAILER NUMBER
C
          I4TEMP=TRABUF(TAGT)
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          OUTTAB(IND+2)=I1TEMP(3)
          OUTTAB(IND+3)=I1TEMP(4)
          IND=IND+4
C
C GET BEGIN INVOICE DATE MONTH
C
          I4TEMP=12
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          IND=IND+2
C
C GET BEGIN INVOICE DATE DAY
C
          I4TEMP=31
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          IND=IND+2
C
C GET BEGIN DATE YEAR
C
          I4TEMP=94
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          IND=IND+2
C
C GET END INVOICE DATE MONTH
C
          I4TEMP=4
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          IND=IND+2
C
C GET END INVOICE DATE DAY
C
          I4TEMP=24
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          IND=IND+2
C
C GET END DATE YEAR
C
          I4TEMP=94
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          IND=IND+2
C
C GET PACK SETTLED AMOUNT
C
          I4TEMP=12345
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          OUTTAB(IND+2)=I1TEMP(3)
          OUTTAB(IND+3)=I1TEMP(4)
          IND=IND+4
C
C GET PACK NUMBER
C
             I4TEMP=1
             OUTTAB(IND+0)=I1TEMP(1)
             OUTTAB(IND+1)=I1TEMP(2)
             OUTTAB(IND+2)=I1TEMP(3)
             OUTTAB(IND+3)=I1TEMP(4)
             IND=IND+4
C
C GET ORDER NUMBER (FIRST 9 DIGITS)
C
             I4TEMP=123456789
             OUTTAB(IND+0)=I1TEMP(1)
             OUTTAB(IND+1)=I1TEMP(2)
             OUTTAB(IND+2)=I1TEMP(3)
             OUTTAB(IND+3)=I1TEMP(4)
             IND=IND+4
C
C GET ORDER NUMBER (LAST 4 DIGITS)
C
             I4TEMP=1234
             OUTTAB(IND+0)=I1TEMP(1)
             OUTTAB(IND+1)=I1TEMP(2)
             IND=IND+2
C
C GET GAME NUMBER
C
             I4TEMP=1
             OUTTAB(IND+0)=I1TEMP(1)
             OUTTAB(IND+1)=I1TEMP(2)
             IND=IND+2
C
C GET DATE MONTH
C
             I4TEMP=12
             OUTTAB(IND+0)=I1TEMP(1)
             OUTTAB(IND+1)=I1TEMP(2)
             IND=IND+2
C
C GET DATE DAY
C
             I4TEMP=31
             OUTTAB(IND+0)=I1TEMP(1)
             OUTTAB(IND+1)=I1TEMP(2)
             IND=IND+2
C
C GET DATE YEAR
C
             I4TEMP=94
             OUTTAB(IND+0)=I1TEMP(1)
             OUTTAB(IND+1)=I1TEMP(2)
             IND=IND+2
C
C GET AUTO SETTLED FLAG
C
             I4TEMP=3
             OUTTAB(IND+0)=I1TEMP(1)
             OUTTAB(IND+1)=I1TEMP(2)
             IND=IND+2
C
C GET ADJUSTMENT REPORT INFORMATION
C
        ELSEIF(TRABUF(TRTYP).EQ.IADJR) THEN
          IND=16
C
C GET CONTINUATION DATE
C
          I4TEMP=0
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          OUTTAB(IND+2)=I1TEMP(3)
          OUTTAB(IND+3)=I1TEMP(4)
          IND=IND+4
C
C GET CONTINUATION TIME
C
          I4TEMP=0
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          OUTTAB(IND+2)=I1TEMP(3)
          OUTTAB(IND+3)=I1TEMP(4)
          IND=IND+4
C
C GET CONTINUATION TYPE
C
          I4TEMP=0
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          IND=IND+2
C
C RETAILER NUMBER
C
          I4TEMP=TRABUF(TAGT)
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          OUTTAB(IND+2)=I1TEMP(3)
          OUTTAB(IND+3)=I1TEMP(4)
          IND=IND+4
C
C GET BEGIN INVOICE DATE YEAR
C
          I4TEMP=94
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          IND=IND+2
C
C GET BEGIN INVOICE DATE MONTH
C
          I4TEMP=12
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          IND=IND+2
C
C GET BEGIN DATE DAY
C
          I4TEMP=31
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          IND=IND+2
C
C GET END INVOICE DATE YEAR
C
          I4TEMP=95
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          IND=IND+2
C
C GET END INVOICE DATE MONTH
C
          I4TEMP=1
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          IND=IND+2
C
C GET END DATE DAY
C
          I4TEMP=7
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          IND=IND+2
C
C GET TOTAL ADJUSTMENT AMOUNT
C
          I4TEMP=5000
          OUTTAB(IND+0)=I1TEMP(1)
          OUTTAB(IND+1)=I1TEMP(2)
          OUTTAB(IND+2)=I1TEMP(3)
          OUTTAB(IND+3)=I1TEMP(4)
          IND=IND+4
C
          DO 200 I=0,2
C
C GET ADJUSTMENT DATE YEAR
C
             I4TEMP=94
             OUTTAB(IND+0)=I1TEMP(1)
             OUTTAB(IND+1)=I1TEMP(2)
             IND=IND+2
C
C GET ADJUSTMENT DATE MONTH
C
             I4TEMP=12
             OUTTAB(IND+0)=I1TEMP(1)
             OUTTAB(IND+1)=I1TEMP(2)
             IND=IND+2
C
C GET ADJUSTMENT DATE DAY
C
             I4TEMP=31
             OUTTAB(IND+0)=I1TEMP(1)
             OUTTAB(IND+1)=I1TEMP(2)
             IND=IND+2
C
C GET ADJUSTMENT TYPE
C
             I4TEMP=1+I
             OUTTAB(IND+0)=I1TEMP(1)
             OUTTAB(IND+1)=I1TEMP(2)
             IND=IND+2
C
C GET ADJUSTMENT AMOUNT
C
             I4TEMP=10000+I
             OUTTAB(IND+0)=I1TEMP(1)
             OUTTAB(IND+1)=I1TEMP(2)
             OUTTAB(IND+2)=I1TEMP(3)
             OUTTAB(IND+3)=I1TEMP(4)
             IND=IND+4
200       CONTINUE
C
C GET ADJUSTMENT REPORT TEXT INFORMATION
C
        ELSEIF(TRABUF(TRTYP).EQ.IADJT) THEN
          IND=18
          DO 300 I=0,9
C
C GET ADJUSTMENT TYPE
C
             I4TEMP=I+1
             OUTTAB(IND+0)=I1TEMP(1)
             OUTTAB(IND+1)=I1TEMP(2)
             IND=IND+2
C
C GET ADJUSTMENT TEXT
C
             I4TEMP=ADJTEXT(1,I+1)
             OUTTAB(IND+0)=I1TEMP(1)
             OUTTAB(IND+1)=I1TEMP(2)
             OUTTAB(IND+2)=I1TEMP(3)
             OUTTAB(IND+3)=I1TEMP(4)
C
             I4TEMP=ADJTEXT(2,I+1)
             OUTTAB(IND+4)=I1TEMP(1)
             OUTTAB(IND+5)=I1TEMP(2)
             OUTTAB(IND+6)=I1TEMP(3)
             OUTTAB(IND+7)=I1TEMP(4)
C
             I4TEMP=ADJTEXT(3,I+1)
             OUTTAB(IND+8)=I1TEMP(1)
             OUTTAB(IND+9)=I1TEMP(2)
             IND=IND+10
300       CONTINUE
        ELSE
          IND=18
C
C TOTAL PACKS SOLD COUNT
C
	  I4TEMP=0
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          OUTTAB(IND+2) = I1TEMP(3)
          OUTTAB(IND+3) = I1TEMP(4)
          IND=IND+4
C
C TOTAL PACKS SOLD AMOUNT
C
	  I4TEMP=0
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          OUTTAB(IND+2) = I1TEMP(3)
          OUTTAB(IND+3) = I1TEMP(4)
          IND=IND+4
C
C TOTAL CASHES COUNT
C
	  I4TEMP=0
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          OUTTAB(IND+2) = I1TEMP(3)
          OUTTAB(IND+3) = I1TEMP(4)
          IND=IND+4
C
C TOTAL CASHES AMOUNT
C
	  I4TEMP=0
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          OUTTAB(IND+2) = I1TEMP(3)
          OUTTAB(IND+3) = I1TEMP(4)
          IND=IND+4
C
C RETURN AMOUNT
C
	  I4TEMP=0
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          OUTTAB(IND+2) = I1TEMP(3)
          OUTTAB(IND+3) = I1TEMP(4)
          IND=IND+4
C
C SALES COMMISION AMOUNT
C
	  I4TEMP=0
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          OUTTAB(IND+2) = I1TEMP(3)
          OUTTAB(IND+3) = I1TEMP(4)
          IND=IND+4
C
C PRIZE COMMISION AMOUNT
C
	  I4TEMP=0
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          OUTTAB(IND+2) = I1TEMP(3)
          OUTTAB(IND+3) = I1TEMP(4)
          IND=IND+4
C
C ADJUSTMENT AMOUNT
C
	  I4TEMP=0
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          OUTTAB(IND+2) = I1TEMP(3)
          OUTTAB(IND+3) = I1TEMP(4)
          IND=IND+4
C
C GET TOTAL AMOUNT DUE
C
	  I4TEMP=0
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          OUTTAB(IND+2) = I1TEMP(3)
          OUTTAB(IND+3) = I1TEMP(4)
          IND=IND+4
C
C DATE MONTH
C
          I4TEMP=5
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          IND=IND+2
C
C DATE DAY
C
	  I4TEMP=13
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          IND=IND+2
C
C DATE YEAR
C
	  I4TEMP=93
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          IND=IND+2
C
C DATE MONTH
C
          I4TEMP=6
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          IND=IND+2
C
C DATE DAY
C
	  I4TEMP=13
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          IND=IND+2
C
C DATE YEAR
C
	  I4TEMP=93
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          IND=IND+2
C
C DATE MONTH
C
          I4TEMP=7
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          IND=IND+2
C
C DATE DAY
C
	  I4TEMP=13
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          IND=IND+2
C
C DATE YEAR
C
	  I4TEMP=93
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          IND=IND+2
C
C FILLER
C 
          IND=IND+2
C
C DOCUMENT NUMBER
C
	  I4TEMP=1000000+TRABUF(TAGT)
          OUTTAB(IND+0) = I1TEMP(1)
          OUTTAB(IND+1) = I1TEMP(2)
          OUTTAB(IND+2) = I1TEMP(3)
          OUTTAB(IND+3) = I1TEMP(4)
          IND=IND+4
        ENDIF
C
C
	RETURN
	END
