C
C SUBROUTINE FTPHDR
C $Log:   GXAFXT:[GOLS]FTPHDR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:14:06   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   02 Sep 1994 18:03:42   HXK
C  Merge of May,June RFSS batch 
C  
C     Rev 1.0   26 Jun 1994 14:03:52   HXK
C  Initial revision.
C  
C
C ** Source - FTPHDR.for **
C
C FTPHDR.FOR
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE FTPHDR(BUF,TAPTYP)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LOGCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INTEGER*4 I, IND, WEKNO, I2TEMP, TEMP, TAPTYP
	INTEGER*4 BUF(16,128)
	INTEGER*2 DATE(LDATE_LEN)
	INTEGER*4 YEAR2
	EQUIVALENCE (TEMP,I2TEMP)
C
C
	CALL FASTSET(0,BUF,2048)
	DATE(VCDC)=DAYCDC
	CALL LCDATE(DATE)
C
	BUF(1,1)=2                       !# OF RECORDS IN BLOCK
	BUF(2,1)=-1                      !TAPE HEADER FLAG
	BUF(3,1)=TAPTYP                  !TAPE TYPE
	CALL ICLOCK(2,BUF(4,1))          !CURRENT TIME
C
        YEAR2 = DATE(VYEAR)
        CALL FIGWEK(DAYCDC,WEKNO,YEAR2)
C
	BUF(1,2)=DAYCDC                  !CDC DATE
	BUF(2,2)=DATE(VYEAR)             !YEAR
	BUF(3,2)=DATE(VJUL)              !JULIAN DATE
	BUF(4,2)=DATE(VDOW)              !DAY OF WEEK NUMBER
        BUF(5,2)=WEKNO                   !WEEK NUMBER CREATED
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
