C
C SUBROUTINE LSHRRPT
C
C V06 27-NOV-97 UXN Changes for LOTTO extra draw.
C
C $Log:   GXAFXT:[GOLS]LSHRRPT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:56:22   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.3   18 Aug 1995 14:48:48   HXK
C  Fix for displaying bonus draw values, strange
C  
C     Rev 1.2   18 Oct 1993 14:17:04   HXK
C  CHANGE REPORT NAME
C  
C     Rev 1.1   19 May 1993 10:09:42   SXH
C  Released for Finland VAX
C  
C     Rev 1.0   21 Jan 1993 16:56:18   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - lshrrpt.for **
C
C LSHRRPT.FOR
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE LSHRRPT(GNUM,GIND,DRAW,COPY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DLTREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

        ! arguments
	INTEGER*4  GNUM               ! game number
	INTEGER*4  GIND               ! game index
	INTEGER*4  DRAW               ! draw number
	INTEGER*4  COPY               ! number of report copies

        ! variables
	INTEGER*4  J                  ! counter
	INTEGER*4  ST                 ! status
	INTEGER*4  PAGE               ! 
	INTEGER*4  BNS                ! bonus shares flag
	INTEGER*4  I                  ! counter
	INTEGER*4  FDB(7)             ! file description block
	INTEGER*4  ONLSHR(LTGDIV)     !
	INTEGER*4  ONLSHV(LTGDIV)     !
	INTEGER*4  TOTSHR(LTGDIV)     !

	INTEGER*2  DATE(LDATE_LEN)    !

	CHARACTER*36 HEAD             !
	CHARACTER*12 REPNAM           !
	INTEGER*4   LUN
C
C
C
	PAGE = 0
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,3,DLTSEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	CALL READW(FDB,DRAW,DLTREC,ST)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
	CALL CLOSEFIL(FDB)

	DATE(5) = DLTDAT(CURDRW)
	CALL LCDATE(DATE)
C
	WRITE(REPNAM,800) GIND
	WRITE(HEAD,801) GSNAMES(GNUM),(DATE(I),I=7,13)
	LUN=7
	CALL ROPEN(REPNAM,LUN,ST)
	IF(ST.NE.0) THEN
	    TYPE*,IAM(),REPNAM,' Open error  st - ',ST
	    CALL USRCLOS1(LUN)
	    RETURN
	ENDIF
C
	CALL TITLE(HEAD,'LWINRP  ',GIND,LUN,PAGE,DAYCDC)

        ! set bonus shares flag
	BNS=1
	IF(DLTBDR.NE.0) BNS=2

	DO 200 I = 1, BNS
	    IF(I.EQ.2) WRITE(LUN,909)
	    DO J = 1, DLTDIV
	        TOTSHR(J)=DLTSHR(J,I)
	        ONLSHR(J)=DLTSHR(J,I)
	        ONLSHV(J)=DLTSHV(J,I)
	    END DO

	    WRITE(LUN,908) DRAW,(J,J=1,DLTDIV)
	    WRITE(LUN,904) (ONLSHR(J),J=1,DLTDIV)
	    WRITE(LUN,906) (TOTSHR(J),J=1,DLTDIV)
	    WRITE(LUN,907) (CMONY(ONLSHV(J),15,VALUNIT),J=1,DLTDIV)
200	CONTINUE
	CALL USRCLOS1(LUN)

	CALL SPOOL(REPNAM,COPY,ST)


	RETURN
C
C
800	FORMAT('LO',I1,'WINRP.REP')
801	FORMAT(A4,' SHARE REPORT FOR ',7A2)
904	FORMAT(2X,'Online  Shares ',2X,<DLTDIV>(5X,I10))
905	FORMAT(2X,'Offline Shares ',2X,<DLTDIV>(5X,I10))
906	FORMAT(2X,'Total   Shares ',2X,<DLTDIV>(5X,I10))
907	FORMAT(2X,'Share   Values ',2X,<DLTDIV>(A15))
908	FORMAT(//,' DRAW ',I4,9X,<DLTDIV>(4X,'DIVISION ',I2),/)
909     FORMAT(/////,'<<< BONUS DRAW>>>')

	END
