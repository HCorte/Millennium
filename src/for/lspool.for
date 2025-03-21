C
C SUBROUTINE LSPOOL
C $Log:   GXAFXT:[GOLS]LSPOOL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:56:36   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:56:32   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - lspool.for **
C
C LSPOOL.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO LOAD SCORE POOL INFO INTO POOL ARRAY IN SCRCOM
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE LSPOOL(GAMN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:SCRCOM.DEF'
	INCLUDE 'INCLIB:RECSPF.DEF'
	INTEGER*4 HOME(MAXD),AWAY(MAXD),FDB(7)
	INTEGER*4 GIND,DRW,BSAL,ST,I,J,GAMN
	DATA HOME /1,2,2,3,3,3,4,4,4,4,5,5,5,5,5,
     *	           0,1,2,3,4,5,0,0,0,0,0,0,0,0,0,
     *	           0,0,1,0,1,2,0,1,2,3,0,1,2,3,4/
	DATA AWAY /0,0,1,0,1,2,0,1,2,3,0,1,2,3,4,
     *	           0,1,2,3,4,5,0,0,0,0,0,0,0,0,0,
     *	           1,2,2,3,3,3,4,4,4,4,5,5,5,5,5/
	INTEGER*4 OVER1(MAXO),OVER2(MAXO)
	DATA OVER1 /6,6,0/
	DATA OVER2 /0,6,6/
C
C GET GAME INDEX AND DRAW NUMBER
C
	GIND=GNTTAB(GAMIDX,GAMN)
	DRW=DAYDRW(GAMN)
C
C GET BEGINING SALES DAY
C
	BSAL=SCRBSD(GIND)
C
C DETERMINE IF THIS IS FIRST DAY OF SALES
C
	IF(DAYCDC.EQ.BSAL) GOTO 1000          ! INIT FILE AND MEMORY
C
C OPEN SCORE POOL FILE
C
10	CONTINUE
	CALL OPENW(4,SCRPFN(1,GIND),4,0,0,ST)
	IF(ST.NE.0) THEN
	   WRITE(5,900) SCRPFN(1,GIND),ST
	   CALL GPAUSE
	   GOTO 10
	ENDIF
C
	CALL IOINIT(FDB,4,SPFSEC*256)
	CALL READW(FDB,1,SPFREC,ST)
	GOTO 2000                           ! LOAD POOL FILE INTO MEMORY
C
C CREATE FILE AND INTIAILIZE MEMORY
C
1000	CONTINUE
	CALL CRTFIL(SCRPFN(1,GIND),SPFSEC+1,ST)
	IF(ST.NE.0) THEN
	   WRITE(5,901) SCRPFN(1,GIND),ST
	   CALL GPAUSE
	   GOTO 1000
	ENDIF
	CALL OPENW(4,SCRPFN(1,GIND),4,0,0,ST)
	IF(ST.NE.0) THEN
	   WRITE(5,900) SCRPFN(1,GIND),ST
	   CALL GPAUSE
	   GOTO 1000
	ENDIF
C
	CALL IOINIT(FDB,4,SPFSEC*256)
	SPFGAM=GIND
	SPFCDC=0
	SPFDRW=DRW
	DO 1010 I=1,SLEN
	   SPFPOL(I,SPODDS)=99999
	   SPFPOL(I,SPAMNT)=0
1010	CONTINUE
C
C STUFF SCORES INTO DISPLAY AREA OF FILE
C
	DO 1020 I=1,MAXD
	   CALL POLIND(HOME(I),AWAY(I),SPFDPL(I))
1020	CONTINUE
C
	DO 1030 I=1,MAXO
	   CALL POLIND(OVER1(I),OVER2(I),SPFOPL(I))
1030	CONTINUE
C
C WRITE DATA TO SCORE POOL FILE
C
	CALL WRITEW(FDB,1,SPFREC,ST)
C
C LOAD POOL ARRAY IN SCRCOM
C
2000	CONTINUE
C
	DO 2010 I=1,SLEN
	   DO 2020 J=1,2
	      SCPOOL(I,J,SPDYNM,GIND)=SPFPOL(I,J)
2020	   CONTINUE
2010	CONTINUE
C
C LOAD DISPLAY POOL ARRAY
C
	DO 2030 I=1,MAXD
	   IF(SPFDPL(I).NE.0) THEN
	      DPOOL(I,SPSCOR,GIND)=SPFDPL(I)
	      DPOOL(I,SPDODS,GIND)=SPFPOL(SPFDPL(I),SPODDS)
	      DPOOL(I,SPDAMT,GIND)=SPFPOL(SPFDPL(I),SPAMNT)
	   ENDIF
2030	CONTINUE
C
	DO 2040 I=1,MAXO
	   IF(SPFOPL(I).NE.0) THEN
	      OPOOL(I,SPSCOR,GIND)=SPFOPL(I)
	      OPOOL(I,SPDODS,GIND)=SPFPOL(SPFOPL(I),SPODDS)
	      OPOOL(I,SPDAMT,GIND)=SPFPOL(SPFOPL(I),SPAMNT)
	   ENDIF
2040	CONTINUE
	CALL CLOSEFIL(FDB)
	RETURN
C
C FORMAT STATEMENTS
C
900	FORMAT(1X,5A4,' - Open error   status - ',I2)
901	FORMAT(1X,5A4,' - Allocation error    status - ',I2)
	END
