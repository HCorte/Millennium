C
C SUBROUTINE TAPHDR
C $Log:   GXAFXT:[GOLS]TAPHDR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:27:38   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:48:24   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - taphdr.for **
C
C TAPHDR.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C SUBROUTINE TO GENERATE A HEADER RECORD FOR LOG TAPES.
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TAPHDR(BUF,TAPTYP)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LOGCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INTEGER*4 I, IND, I2TEMP, TEMP, TAPTYP
	INTEGER*4 BUF(16,128)
	INTEGER*2 DATE(12)
	EQUIVALENCE (TEMP,I2TEMP)
C
C
	CALL FASTSET(0,BUF,2048)
	DATE(VCDC)=DAYCDC
	CALL CDATE(DATE)
C
	BUF(1,1)=2                       !# OF RECORDS IN BLOCK
	BUF(2,1)=-1                      !TAPE HEADER FLAG
	BUF(3,1)=TAPTYP                  !TAPE TYPE
	CALL ICLOCK(2,BUF(4,1))          !CURRENT TIME
C
C
	BUF(1,2)=DAYCDC                  !CDC DATE
	BUF(2,2)=DATE(VYEAR)             !YEAR
	BUF(3,2)=DATE(VJUL)              !JULIAN DATE
	BUF(4,2)=DATE(VDOW)              !DAY OF WEEK NUMBER
C
C
C
	IND=3
	DO 100 I=1,MAXGAM
	IF(DAYDRW(I).LE.0) GOTO 100
	BUF(1,1)=BUF(1,1)+1                !INCREMENT COUNT
	BUF(1,IND)=I                       !GAME #
	BUF(2,IND)=DAYDRW(I)               !DRAW NUMBER
	BUF(3,IND)=GNTTAB(GAMTYP,I)        !GAME TYPE
	BUF(4,IND)=GNTTAB(GAMIDX,I)        !GAME INDEX
	BUF(5,IND)=KGNTAB(I)               !KICKER GAME NUMBER
	IND=IND+1
100	CONTINUE
	RETURN
	END
