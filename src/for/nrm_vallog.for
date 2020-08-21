C
C SUBROUTINE VALLOG
C
C V30 18-MAR-1999 RXK  Gtyp+gind in 5+3 bits change.
C
C     Rev 1.0   17 Apr 1996 15:49:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.4   02 Sep 1994 18:16:02   HXK
C  Merge of May,June RFSS batch 
C  
C     Rev 1.5   17 Apr 1994 14:28:56   HXK
C  NUMREC default set to 1 instead of 2
C  
C     Rev 1.4   07 Apr 1994 19:36:56   HXK
C  FOLLOWING FIELDS ARE NO LONGER KEPT ON PHYSICAL DISK
C  1. TAX AMOUNT
C  2. CLAIM SERIAL
C  3. KIKTAX AMOUNT
C  4. CLAIM CDC
C  5. TER
C  
C     Rev 1.3   18 May 1993 15:08:44   GXA
C  Released for Finland Dec Conversion / Oddset.
C  
C     Rev 1.2   17 May 1993 17:41:32   GXA
C  Restored Baseline version 1.0.
C  
C     Rev 1.0   21 Jan 1993 18:00:08   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_vallog.for **
C
C VALLOG.FOR
C
C V03 09-JAN-01 EPH VFFCOPAMT AND VOPSCNT AND VFKOPSAMT CHANGED
C V02 07-OCT-91 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO CONVERT INTERNAL VALIDATION RECORD TO
C VALIDATION FILE RECORD.
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE VALLOG(VALREC,V4BUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VALFIL.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INTEGER*4 NUMREC, I4TEMP
	INTEGER*2 I2TEMP(2)
        BYTE      I1TEMP(4)
	EQUIVALENCE (I2TEMP,I4TEMP,I1TEMP)
	INTEGER*4 INDLEN(4)
	DATA INDLEN/Z00000000,Z40000000,Z80000000,ZC0000000/
C
C
	NUMREC=1
	CALL FASTSET(0,V4BUF,VFLEN*4)
	IF(VALREC(VPZOFF).GT. 1) NUMREC=2
	IF(VALREC(VPZOFF).GT.10) NUMREC=3
        IF(VALREC(VPZOFF).GT.19) NUMREC=4
C
	V4BUF(1)   = IOR(VALREC(VSSER),INDLEN(NUMREC))
	I2TEMP(1)  = VALREC(VSCDC)
	I2TEMP(2)  = VALREC(VWCDC)
	V4BUF(2)   = I4TEMP
C
	I2TEMP(1)  = VALREC(VEXP)
	I2TEMP(2)  = VALREC(VKEXP)
	V4BUF(3)   = I4TEMP
C 
	V4BUF(VFPAMT)   = VALREC(VPAMT)
	V4BUF(VFKPAMT)  = VALREC(VKPAMT)
C	V4BUF(VFK2PAMT) = VALREC(VK2PAMT)   !V03
	V4BUF(VFKOPSAMT)= VALREC(VKOPSAMT)  !V03
        V4BUF(VFRAMT)   = VALREC(VRAMT)
C***	V4BUF(VFTAMT)  	= VALREC(VTAMT)
C	V4BUF(VFFCOPAMT)= VALREC(VFCOPAMT) !V03
	V4BUF(VFOPSAMT) = VALREC(VOPSAMT)  !V03
	V4BUF(VFCSER)   = VALREC(VCSER)
C***	V4BUF(VFLSER)   = VALREC(VLSER)
C***	V4BUF(VFKTAMT)  = VALREC(VKTAMT)
C 
C***	I2TEMP(1)  = VALREC(VLCDC)
C***	I2TEMP(2)  = VALREC(VLTER)
C***	V4BUF(??)  = I4TEMP
 
	I2TEMP(1)  = VALREC(VSTER)
	I2TEMP(2)  = VALREC(VCTER)
	V4BUF(10)  = I4TEMP
 
	I2TEMP(1)  = VALREC(VCCDC)
        I1TEMP(3)  = VALREC(VGAM) 
        I1TEMP(4)  = ISHFT(VALREC(VGTYP),3) +
     *               IAND(VALREC(VGIND),'07'X)
	V4BUF(11)  = I4TEMP
 
	I1TEMP(1)  = VALREC(VPZOFF)
	I1TEMP(2)  = VALREC(VSTAT)
	I1TEMP(3)  = VALREC(VKGME)
	I1TEMP(4)  = VALREC(VFRAC)
	V4BUF(12)  = I4TEMP
C
C	I4TEMP = ISHFT(VALREC(VFTBSF),24)+VALREC(VBNKID)   !V03
	I4TEMP = ISHFT(VALREC(VOPSCNT),24)+VALREC(VBNKID)  !V03
	V4BUF(VFBNKID) = I4TEMP
	V4BUF(VFBNKNUM)= VALREC(VBNKNUM)
C
	CALL FASTMOV(VALREC(VPDATA),V4BUF(VFPDATA),VFPLEN)
	RETURN
	END
