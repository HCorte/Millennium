C
C PROGRAM TSTVAL
C $Log:   GXAFXT:[GOLS]TSTVAL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:39:38   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   02 Sep 1994 18:15:26   HXK
C  Merge of May,June RFSS batch 
C  
C     Rev 1.2   17 Apr 1994 14:27:02   HXK
C  zeroed claims and taxes
C  
C     Rev 1.1   18 May 1993 15:11:04   GXA
C  Released for Finland Dec Conversion / Oddset.
C  
C     Rev 1.0   21 Jan 1993 17:55:28   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - tstval.for **
C
C TSTVAL.FOR
C
C V02 07-OCT-91 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
	PROGRAM TSTVAL
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VDETAIL.DEF'
	INCLUDE 'INCLIB:VALFIL.DEF'
	INTEGER*4 J, I, WINS
	INTEGER*4 BUF(VALLEN),DBUF(VPLEN,VMAX)
C
	CALL COPYRITE
C
	TYPE*,'Enter number of wins'
	ACCEPT*,WINS
	IF(WINS.GT.VMAX) THEN
	   TYPE*,'Number of wins is greater than max allowed: ',VMAX
	   TYPE*,'Wins is reset to ',VMAX
	   WINS = VMAX
	ENDIF
C
C
	VALREC(VSTAT)=VUNCSH
	VALREC(VCCDC)=100
	VALREC(VCSER)=98762
	VALREC(VCTER)=354
	VALREC(VLCDC)=0                   !345
	VALREC(VLTER)=0                   !678
	VALREC(VLSER)=0                   !6433563
        VALREC(VKTAMT)=0                  !564323
	VALREC(VPAMT)=9887743
	VALREC(VKPAMT)=934433
C	VALREC(VK2PAMT)=536870911
C	VALREC(VFCOPAMT)=520093695
	VALREC(VRAMT)=56432
        VALREC(VTAMT)=0                   !564326
	VALREC(VSCDC)=345
	VALREC(VSSER)=5665
	VALREC(VSTER)=45
	VALREC(VEXP)=70
	VALREC(VKEXP)=71
	VALREC(VGAM)=25
	VALREC(VGTYP)=TINS
	VALREC(VGIND)=MAXIND
	VALREC(VKGME)=25
	VALREC(VWCDC)=9999
	VALREC(VBNKID)=987654
	VALREC(VBNKNUM)=99887766
	VALREC(VFRAC)=10
	VALREC(VPZOFF)=WINS
	DO 10 I=1,WINS
	VDETAIL(VKIK,I)=MOD(I+1,2)
	VDETAIL(VBDR,I)=MOD(I+3,2)
	VDETAIL(VUPD,I)=MOD(I+4,2)
	VDETAIL(VREF,I)=MOD(I+5,2)
	VDETAIL(VDRW,I)=40+I
	VDETAIL(VDIV,I)=MOD(I,8)+1
	VDETAIL(VSHR,I)=10000+I
	VDETAIL(VKI2,I)=MOD(I+6,2)
	VDETAIL(VPRG,I)=MOD(I+7,2)
10	CONTINUE
	CALL DVALLOG(VALREC,VDETAIL)
	CALL VALLOG(VALREC,V4BUF)
	CALL LOGVAL(BUF,V4BUF)
	CALL DLOGVAL(BUF,DBUF)
	TYPE 123,'VFSSER ',V4BUF(VFSSER)
123     FORMAT(1X,A,Z10.8)
	DO 20 I=1,VPZOFF
	IF(BUF(I).NE.VALREC(I)) TYPE*,I,BUF(I),VALREC(I)
20	CONTINUE
	DO 30 J=1,VMAX
	DO 30 I=1,VPLEN
	IF(VDETAIL(I,J).NE.DBUF(I,J))
     *	 TYPE*,'DETAIL ',I,J,VDETAIL(I,J),DBUF(I,J)
30	CONTINUE
	CALL GSTOP(GEXIT_SUCCESS)
	END
