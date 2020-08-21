C
C SUBROUTINE BNSHRRPT
C $Log:   GXAFXT:[GOLS]BNSHRRPT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:21:48   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   12 Dec 1994 19:43:02   HXK
C  Fixed report name bug
C  
C     Rev 1.0   24 Nov 1994 21:30:10   HXK
C  Initial revision.
C  
C
C BNSHRRPT.FOR
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE BNSHRRPT(GNUM,GIND,DRAW,COPY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DBNREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

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
	INTEGER*4  OFFSHR(BGODIV,BGOFHS) !
	INTEGER*4  TOTSHR(BGODIV,BGOFHS) !
	INTEGER*4  START,END

	INTEGER*2  DATE(LDATE_LEN)       !

        REAL*8     BINGO_NAME(BGOFHS)    !

	CHARACTER*36  HEAD               !
	CHARACTER*12  REPNAM             !
C
C
        DATA BINGO_NAME/'Bingo AB','FullHse '/
C
	PAGE = 0
	CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,3,DBNSEC*256)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)

	CALL READW(FDB,DRAW,DBNREC,ST)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

	DATE(5)=DBNDAT(CURDRW)
	CALL LCDATE(DATE)
C
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
	CALL TITLE(HEAD,'BWINRP  ',GIND,6,PAGE,DAYCDC)
        DO I = 1,BGOFHS
	DO J = 1, DBNDIV(I)
	    OFFSHR(J,I)=DBNTSR(J,I)-DBNSHR(J,I)
	    IF(OFFSHR(J,I).LT.0) OFFSHR(J,I)=0
	    TOTSHR(J,I)=DBNSHR(J,I)+OFFSHR(J,I)
        END DO
        ENDDO

        IF(DRAW.LE.DBNLOB) THEN
           WRITE(6,900) BINGO_NAME(BGOBAB)
	   WRITE(6,908) DRAW,(J,J=1,DBNDIV(BGOBAB))
	   WRITE(6,904) (DBNSHR(J,BGOBAB),J=1,DBNDIV(BGOBAB))
	   WRITE(6,906) (TOTSHR(J,BGOBAB),J=1,DBNDIV(BGOBAB))
	   WRITE(6,907) (CMONY(DBNSHV(J,BGOBAB),15,VALUNIT),J=1,DBNDIV(BGOBAB))

           WRITE(6,900)  BINGO_NAME(BGOFHS)
           WRITE(6,9081) DRAW,(J,J=1,DBNDIV(BGOFHS))
           WRITE(6,9041) (DBNSHR(J,BGOFHS),J=1,DBNDIV(BGOFHS))
           WRITE(6,9061) (TOTSHR(J,BGOFHS),J=1,DBNDIV(BGOFHS))
           WRITE(6,9071) (CMONY(DBNSHV(J,BGOFHS),15,VALUNIT),J=1,DBNDIV(BGOFHS))

        ELSE
           START = 1
           END=8
           WRITE(6,900)  BINGO_NAME(BGOFHS)
           WRITE(6,9181) DRAW
           WRITE(6,9191) (J,J=START,END)
           WRITE(6,9141) (DBNSHR(J,BGOFHS),J=START,END)
           WRITE(6,9161) (TOTSHR(J,BGOFHS),J=START,END)
           WRITE(6,9171) (CMONY(DBNSHV(J,BGOFHS),14,VALUNIT),J=START,END)
 
           START=END+1
           END=DBNDIV(BGOFHS)
           WRITE(6,9191) (J,J=START,END)
           WRITE(6,9141) (DBNSHR(J,BGOFHS),J=START,END)
           WRITE(6,9161) (TOTSHR(J,BGOFHS),J=START,END)
           WRITE(6,9171) (CMONY(DBNSHV(J,BGOFHS),14,VALUNIT),J=START,END)

        ENDIF

	CALL USRCLOS1(     6)
	CALL SPOOL(REPNAM,COPY,ST)
	CALL CLOSEFIL(FDB)

	RETURN
C
C
800	FORMAT('BI',I1,'WINRP.REP')
801	FORMAT(A4,' SHARE REPORT FOR ',7A2)

900     FORMAT(//,'Bingo Subgame: ',A8)

904	FORMAT(2X,'Online  Shares ',2X,<DBNDIV(BGOBAB)>(5X,I10))
905	FORMAT(2X,'Offline Shares ',2X,<DBNDIV(BGOBAB)>(5X,I10))
906	FORMAT(2X,'Total   Shares ',2X,<DBNDIV(BGOBAB)>(5X,I10))
907	FORMAT(2X,'Share   Values ',2X,<DBNDIV(BGOBAB)>(A15))
908	FORMAT(' DRAW ',I4,9X,<DBNDIV(BGOBAB)>(4X,'DIVISION ',I2),/)

9041    FORMAT(2X,'Online  Shares ',2X,<DBNDIV(BGOFHS)>(5X,I10))
9051    FORMAT(2X,'Offline Shares ',2X,<DBNDIV(BGOFHS)>(5X,I10))
9061    FORMAT(2X,'Total   Shares ',2X,<DBNDIV(BGOFHS)>(5X,I10))
9071    FORMAT(2X,'Share   Values ',2X,<DBNDIV(BGOFHS)>(A15))
9081    FORMAT(' DRAW ',I4,9X,<DBNDIV(BGOFHS)>(4X,'DIVISION ',I2),/)

9141    FORMAT(2X,'Online  Shares ',2X,<END-START+1>(4X,I10))
9161    FORMAT(2X,'Total   Shares ',2X,<END-START+1>(4X,I10))
9171    FORMAT(2X,'Share   Values ',2X,<END-START+1>(A14))
9181    FORMAT(' DRAW ',I4)
9191    FORMAT(/19X,<END-START+1>(3X,'DIVISION ',I2),/)
	END
