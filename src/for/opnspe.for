C
C SUBROUTINE OPNSPE
C
C V09 03-SEP-2010 MAC RFSS0145 - ASFIV FILE ADDED
C V08 13-OCT-1999 RXK World Tour added. 
C V07 13-JUL-1999 UXN Super Triple added.
C V06 23-NOV-1994 HXK Added Bingo
C V05 13-JUL-1993 CXK Check bounds on GTYPE.
C V04 13-JUL-1993 CXK Fixed IVF open error messag display.
C V03 25-JUN-1993 HXK ADDED INSTANT VALIDATION FILE
C V02 10-JUN-1993 HXK sorted out agtinf,agtcom. 
C                     also GAMSEC redimensioned.
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
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
C Copyright 2010 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C SUBROUTINE TO OPEN FILES FOR SPECIAL SERVICES
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE OPNSPE
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:SPECOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'

	INTEGER*4 MESS(EDLEN),GAMSEC(MAXTYP)
	INTEGER*4 SECTOR, GTYPE, I, UNIT, ST, NOCHECK0
	COMMON /NOCHECK0/ NOCHECK0
C
	DATA GAMSEC / DLTSEC,         ! LOTTO GAME TYPE
     *                DSPSEC,         ! SPORTS ( 1X2 ) GAME TYPE
     *                DNBSEC,         ! NUMBERS GAME TYPE
     *                DKKSEC,         ! KICKER GAME TYPE
     *                DBNSEC,         ! BINGO GAME TYPE
     *                DWISEC,         ! WINNERS TIP GAME TYPE
     *                DCPSEC,         ! TODAYS COUPLE GAME TYPE
     *                DTRSEC,         ! TODAYS TRIO GAME TYPE
     *                DDBSEC,         ! SUPER DOUBLE GAME TYPE
     *                DSTSEC,         ! SUPER TRIPLE GAME TYPE
     *                DTSSEC,         ! TOTO SELECT GAME TYPE
     *                DSCSEC,         ! SCORE GAME TYPE
     *		      DSSSEC,         ! SUPERSCORE GAME TYPE
     *                0,              ! INSTANT TICKETS ( NOT USED )
     *                DTGSEC,         ! RESULTS GAME TYPE ( 012M - TOTOGOLO )
     *                DPASEC,         ! PASSIVE GAME
     *                0,              ! NOT USED ( TYPE GAME NOT DEFINED )
     *                0  /            ! NOT USED ( TYPE GAME NOT DEFINED )
C
	NOCHECK0=-1
C
C OPEN ASF FILE
C
	MESS(1)=SPE
	CALL OPENW(1,SFNAMES(1,ASF),4,0,0,ST)
	CALL IOINIT(ASFFDB,1,ASFSEC*256)
	IF(ST.NE.0) THEN
	  MESS(2)=TEGEN
	  MESS(3)=3
	  CALL FASTMOV(SFNAMES(1,ASF),MESS(4),5)
	  MESS(9)=ST
	  CALL QUEMES(MESS)
	ENDIF
C
C OPEN DAF FILE
C
	CALL OPENW(2,SFNAMES(1,DAF),4,0,0,ST)
	CALL IOINIT(DAFFDB,2,DAFSEC*256)
	IF(ST.NE.0) THEN
	  MESS(2)=TEGEN
	  MESS(3)=3
	  CALL FASTMOV(SFNAMES(1,DAF),MESS(4),5)
	  MESS(9)=ST
	  CALL QUEMES(MESS)
	ENDIF
C
C OPEN CLERK ACCOUNTING FILE
C
	IF(P(CLRKACT).EQ.0) THEN
	  CALL OPENW(3,SFNAMES(1,CLK),4,0,0,ST)
	  CALL IOINIT(CLRKFDB,3,CLRKSEC*256)
	  IF(ST.NE.0) THEN
	    MESS(2)=TEGEN
	    MESS(3)=3
	    CALL FASTMOV(SFNAMES(1,CLK),MESS(4),5)
	    MESS(9)=ST
	    CALL QUEMES(MESS)
	  ENDIF
	ENDIF
C
C OPEN ASFIV FILE                                          !V09...
C
	MESS(1)=SPE
	CALL OPENW(4,SFNAMES(1,ASFIV),4,0,0,ST)
	CALL IOINIT(ASFIVFDB,4,ASFIVSEC*256)
	IF(ST.NE.0) THEN
	  MESS(2)=TEGEN
	  MESS(3)=3
	  CALL FASTMOV(SFNAMES(1,ASFIV),MESS(4),5)
	  MESS(9)=ST
	  CALL QUEMES(MESS)
	ENDIF                                              !...V09
C
C OPEN GAME FILES
C
	UNIT=10
	DO 10 I=1,MAXGAM
	IF(DAYHDR(I).LT.1) GOTO 10
	GTYPE=GNTTAB(GAMTYP,I)
	IF(GTYPE.LT.1.OR.GTYPE.GT.MAXTYP) GOTO 10
	SECTOR=GAMSEC(GTYPE)
	IF(SECTOR.LT.1) GOTO 10                !@@@@@ TESTING
	CALL OPENW(UNIT,GFNAMES(1,I),4,0,0,ST)
	CALL IOINIT(GAMFDB(1,I),UNIT,SECTOR*256)
	IF(ST.NE.0) THEN
	  MESS(2)=TEGEN
	  MESS(3)=3
	  CALL FASTMOV(GFNAMES(1,I),MESS(4),5)
	  MESS(9)=ST
	  CALL QUEMES(MESS)
	ENDIF
	UNIT=UNIT+1
10	CONTINUE
	RETURN
	END
