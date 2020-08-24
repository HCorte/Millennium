C
C SUBROUTINE LSTPOOL
C
C LSTPOOL.FOR
C
C V01 14-MAY-1999 UXN INITIAL RELEASE. Produced from LTRPOOL.FOR 
C
C SUBROUTINE TO LOAD SUPER TRIPLE POOL TABLES INTO STROCOM
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE LSTPOOL(GAM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:STRCOM.DEF'
	INCLUDE 'INCLIB:STROCOM.DEF'
	INCLUDE 'INCLIB:STRFREC.DEF'
	INTEGER*4 FDB(7)
	INTEGER*4 GIND,DRW,BSAL,ST,I,J,GAM
C
C
C GET GAME INDEX AND DRAW NUMBER
C
	GIND=GNTTAB(GAMIDX,GAM)
	DRW=DAYDRW(GAM)
C
C GET BEGINING SALES DAY
C
	BSAL=STRBSD(GIND)
C
C DETERMINE IF THIS IS FIRST DAY OF SALES
C
	IF(DAYCDC.EQ.BSAL) GOTO 30          ! INIT FILES AND MEMORY
C
C OPEN AND READ SUPER TRIPLE POOL FILE 
C
10	CONTINUE
	CALL OPENQW(4,STRPFN(1,GIND),4,0,0,ST)
	IF(ST.NE.0) THEN
	   WRITE(5,900) STRPFN(1,GIND),ST
	   CALL GPAUSE
	   GOTO 10
	ENDIF
	CALL IOQINIT(FDB,4,STRFSEC*256)
	CALL READQW(FDB,1,STRFREC,ST)
C
C LOAD POOL ARRAY INTO TROCOM
C
        STROLAMT(GIND) = STRFLAMT
        STROFEL(GIND)  = STRFFEL
	STROLEL(GIND)  = STRFLEL
	STROTNUM(GIND) = STRFTNUM
        CALL FASTMOV(STRFODDS,STRODDS(1,1,GIND),2*STRGPOL)

	CALL CLOSEQFIL(FDB)
        RETURN
C
C CREATE FILE AND INTIAILIZE MEMORY
C
30      CONTINUE
C
	CALL CRTFIL(STRPFN(1,GIND),STRFSEC,ST)
	IF(ST.NE.0) THEN
	   WRITE(5,901) STRPFN(1,GIND),ST
	   CALL GPAUSE
	   GOTO 30
	ENDIF
        STROLAMT(GIND)=999999999
        STROTNUM(GIND)=0
        STROFEL(GIND)=0
        STROLEL(GIND)=0
        CALL FASTSET(0,STRODDS(1,1,GIND),2*STRGPOL)
C
	RETURN
C
C FORMAT STATEMENTS
C
900	FORMAT(1X,5A4,' - Open error   status - ',I2)
901	FORMAT(1X,5A4,' - Allocation error    status - ',I2)
	END

