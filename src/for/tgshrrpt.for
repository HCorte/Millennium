C
C SUBROUTINE TGSHRRPT
C
C V01 02-DEC-2000 UXN INITIAL RELEASE.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TGSHRRPT(GNUM,GIND,DRAW,COPY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DTGREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:REPPRF.DEF'

        ! arguments
        INTEGER*4  GNUM                  !
        INTEGER*4  GIND                  !
        INTEGER*4  DRAW                  !
        INTEGER*4  COPY                  !

        ! variables
	INTEGER*4  J                     !
	INTEGER*4  ST                    !
	INTEGER*4  PAGE                  !
	INTEGER*4  I                     !
	INTEGER*4  FDB(7)                !
	INTEGER*4  OFFSHR(TGGDIV)        !
	INTEGER*4  TOTSHR(TGGDIV)        !

	INTEGER*2  DATE(LDATE_LEN)       !

	CHARACTER*36  HEAD               !
	CHARACTER*12  REPNAM             !
C
C
C
	PAGE = 0
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,3,DTGSEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	CALL READW(FDB,DRAW,DTGREC,ST)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

	DATE(5)=DTGDAT(CURDRW)
	CALL LCDATE(DATE)
C
C
	WRITE(REPNAM,800) PREFIX(TTGL),GIND
	WRITE(HEAD,801) GSNAMES(GNUM),(DATE(I),I=7,13)
	CALL ROPEN(REPNAM,6,ST)
	IF(ST.NE.0) THEN
	    TYPE*,IAM(),REPNAM,' Open error  st - ',ST
	    CALL USRCLOS1(6)
	    RETURN
	ENDIF
C
	CALL TITLE(HEAD,'TGWINRP  ',GIND,6,PAGE,DAYCDC)
	DO J = 1, DTGDIV
	    OFFSHR(J)=DTGTSR(J)-DTGSHR(J)
	    IF(OFFSHR(J).LT.0) OFFSHR(J)=0
	    TOTSHR(J)=DTGSHR(J)+OFFSHR(J)
        END DO

	WRITE(6,908) DRAW,(J,J=1,DTGDIV)
	WRITE(6,904) (DTGSHR(J),J=1,DTGDIV)
C	WRITE(6,905) (OFFSHR(J),J=1,DTGDIV)
	WRITE(6,906) (TOTSHR(J),J=1,DTGDIV)
	WRITE(6,907) (CMONY(DTGSHV(J),15,VALUNIT),J=1,DTGDIV)

	CALL USRCLOS1(     6)
	CALL SPOOL(REPNAM,COPY,ST)
	CALL CLOSEFIL(FDB)

	RETURN
C
C
800	FORMAT(A2,I1,'WINRP.REP')
801	FORMAT(A4,' SHARE REPORT FOR ',7A2)
904	FORMAT(2X,'Online  Shares ',2X,<DTGDIV>(5X,I10))
905	FORMAT(2X,'Offline Shares ',2X,<DTGDIV>(5X,I10))
906	FORMAT(2X,'Total   Shares ',2X,<DTGDIV>(5X,I10))
907	FORMAT(2X,'Share   Values ',2X,<DTGDIV>(A15))
908	FORMAT(//,' DRAW ',I4,9X,<DTGDIV>(4X,'DIVISION ',I2),/)

	END
