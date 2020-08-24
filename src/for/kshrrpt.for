C
C SUBROUTINE KSHRRPT
C $Log:   GXAFXT:[GOLS]KSHRRPT.FOV  $
C
C  V15 07-AUG-1998 RXK Changed to dispaly 2 kickers
C  
C     Rev 1.0   17 Apr 1996 13:43:58   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   18 Oct 1993 14:06:30   HXK
C  CHANGE REPORT NAME
C  
C     Rev 1.1   14 Jul 1993 10:55:20   SXH
C  Released for Finland
C  
C     Rev 1.0   21 Jan 1993 16:45:06   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - kshrrpt.for **
C
C KSHRRPT.FOR
C
C V02 12-NOV-91 MTK INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
	SUBROUTINE KSHRRPT(GNUM,GIND,DRAW,COPY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DKKREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

        ! arguments
        INTEGER*4  GNUM
        INTEGER*4  GIND
        INTEGER*4  DRAW
        INTEGER*4  COPY

        ! variables
	INTEGER*4  J 
	INTEGER*4  ST
	INTEGER*4  PAGE
	INTEGER*4  I
	INTEGER*4  FDB(7) 
	INTEGER*4  OFFSHR(LTGDIV) 
	INTEGER*4  TOTSHR(LTGDIV)

	INTEGER*2  DATE(LDATE_LEN)

	CHARACTER*36  HEAD
	CHARACTER*12  REPNAM

C
C
C
	PAGE = 0
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,3,DKKSEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	CALL READW(FDB,DRAW,DKKREC,ST)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
	DATE(5)=DKKDAT(CURDRW)
	CALL LCDATE(DATE)
C
	WRITE(REPNAM,800) GIND
	WRITE(HEAD,801) GSNAMES(GNUM),(DATE(I),I=7,13)
	CALL ROPEN(REPNAM,6,ST)
	IF(ST.NE.0) THEN
	    TYPE*,IAM(),REPNAM,' Open error  st - ',ST
	    CALL USRCLOS1(6)
	    RETURN
	ENDIF
C
	CALL TITLE(HEAD,'KWINRP  ',GIND,6,PAGE,DAYCDC)
	DO J=1,DKKDIV
	    OFFSHR(J)=DKKTSR(J)-DKKSHR(J)
	    IF(OFFSHR(J).LT.0) OFFSHR(J)=0
	    TOTSHR(J)=DKKSHR(J)+OFFSHR(J)
        END DO

	WRITE(6,908) DRAW,(J,J=1,DKKDIV)
	WRITE(6,904) (DKKSHR(J),J=1,DKKDIV)
	WRITE(6,907) (CMONY(DKKSHV(J),15,VALUNIT),J=1,DKKDIV)


	CALL USRCLOS1(     6)
	CALL SPOOL(REPNAM,COPY,ST)
	CALL CLOSEFIL(FDB)

	RETURN
C
C
800	FORMAT('JO',I1,'WINRP.REP')
801	FORMAT(A4,' SHARE REPORT FOR ',7A2)
904	FORMAT(2X,'Online  Shares ',2X,<DKKDIV>(5X,I10))
905	FORMAT(2X,'Offline Shares ',2X,<DKKDIV>(5X,I10))
906	FORMAT(2X,'Total   Shares ',2X,<DKKDIV>(5X,I10))
907	FORMAT(2X,'Share   Values ',2X,<DKKDIV>(A15))
908	FORMAT(//,' DRAW ',I4,9X,<DKKDIV>(4X,'DIVISION ',I2),/)
909     FORMAT(/,1X,'Old Jokeri, multiweek tickets',/)

	END
