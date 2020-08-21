C SUBROUTINE BUFSNP
C  
C V06 14-FEB-1997 WPW Changed to display the time stamp on the buffer.
C V05 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting,
C                     Instant Pass Thru Phase 1
C V04 11-JUN-1993 HXK added AGTINF.DEF, PRMAGT.DEF
C V03 21-JAN-1993 DAB Initial Release Based on Netherlands Bible, 12/92,
C                     and Comm 1/93 update DEC Baseline
C V02 01-AUG-1990 XXX RELEASED FOR VAX
C V01 01-MAR-1990 XXX INITIAL RELEASE FOR DENMARK
C
C PROCESSING BUFFER SNAPSHOT
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE BUFSNP(BUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
C
C
	INTEGER*4  OFFSET, NEXT, BUF
	INTEGER*4 HDRENDI2, BUFENDI1, XBEG, XEND, K
C
C
	IF (BUF.LE.0.OR.BUF.GT.NUMPRO) BUF=1
	WRITE(CLIN2,902) BUF,HPRO(TERNUM,BUF),PRO(SERIAL,BUF),
     *                   HPRO(INPLEN,BUF),HPRO(TRCODE,BUF),
     *                   DISTIM(PRO(TSTAMP,BUF))
 
	NEXT=4
	HDRENDI2 = (INPTAB*2)-2
	DO 120 OFFSET=1,HDRENDI2,16
	  XBEG = OFFSET
	  XEND = MIN(OFFSET+15,HDRENDI2)
	  WRITE(XNEW(NEXT),101) (HPRO(K,BUF),K=XBEG,XEND)
101	  FORMAT(8(X,Z4,'/',Z4.4))
	  NEXT=NEXT+1
120	CONTINUE
	NEXT =  NEXT + 1
 
	BUFENDI1 = (128*4)
	DO 220 OFFSET=BINPTAB,BUFENDI1,32
	  XBEG = OFFSET
	  XEND = MIN(OFFSET+31,BUFENDI1)
	  WRITE(XNEW(NEXT),201) (BPRO(K,BUF),K=XBEG,XEND)
201	  FORMAT(8(2X,4Z2.2))
	  NEXT=NEXT+1
220	CONTINUE
C
902     FORMAT(1H ,'Buffer ',I4,' terminal ',I5,' serial',I9,
     *             ' length ',I3,' trcode ',I3,' time ',A8)
C
	RETURN
	END
