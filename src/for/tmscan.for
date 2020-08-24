C
C PROGRAM TMSCAN
C $Log:   GXAFXT:[GOLS]TMSCAN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:35:48   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.4   05 Oct 1993 17:22:16   GXA
C  Added call to generate TMORDER report after accumulation is done.
C  
C     Rev 1.3   03 Sep 1993 11:52:24   SXH
C  Fixed DCL errors, added IAM()
C  
C     Rev 1.2   17 Aug 1993 16:44:28   HXN
C  Used OPENQW instead of OPENW.
C  
C     Rev 1.1   07 Jun 1993 16:05:40   HXN
C  Added TMREPORT.
C  
C     Rev 1.0   21 Jan 1993 17:52:06   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - tmscan.for **
C
C TMSCAN.FOR
C
C V03 28-MAY-93 HHN INITIAL RELEASE FOR FINLAND
C V02 12-NOV-91 MTK INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C EXTRACTS SALES INFORMATION FROM THE TM FILE AND
C UPDATES OFFLINE DATA BASE FOR REPORT GENERATION PURPOSES
C
C Files produced : REP.FIL       (by TMREPORT)
C                  WHALE.REP     (by WHALE)
C                  ORDER.REP     (by TMORDER)
C                  GUTS.FIL      (by GUTSW)
C                  SWAP.FIL      (by GUTSW)
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
	PROGRAM TMSCAN
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'     
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:TMFREP.DEF'

	INTEGER*4 LOGREC(LMUREC)
	INTEGER*4 SER, ST

        INTEGER*4 COUNT
 
C BEGIN CODE -----------------------------------------

	CALL COPYRITE
	TYPE *, IAM()
	TYPE *, IAM(),
     *         ' <<<<< TMSCAN Transaction Master File Scanning V03>>>>>'
	TYPE *, IAM()

	EOF = .FALSE.



C INITIALIZE STATISTICS TABLE AND LINE/DROP SORT TABLE
C ----------------------------------------------------
C        DO 91 I=1,NUMAGT
C          DO 90 J=1,6
C            STATS(I,J) = 0
C90        CONTINUE
C91      CONTINUE




C OPEN WHALE REPORT FILE
C ----------------------
C        CALL ROPEN('WHALE.REP',WHALE_LU,ST)
C        IF(ST.NE.0)THEN
C           TYPE *,IAM(),'Error opening WHALE.REP >',ST
C           CALL GPAUSE
C        ENDIF
C
C
C OPEN TMORDER REPORT FILE  ( NOT USED FOR PORTUGAL )
C ------------------------
C        CALL ROPEN('TMORDER.REP',TMORDER_LU,ST)
C        IF(ST.NE.0)THEN
C           TYPE *,IAM(),'Error opening TMORDER.REP >',ST
C           CALL GPAUSE
C        ENDIF
C
C OPEN TAPR REPORT FILE
C ---------------------
C        CALL ROPEN('TAPR.REP',TAPR_LU,ST)
C        IF(ST.NE.0)THEN
C           TYPE *,IAM(),'Error opening TAPR.REP >',ST
C           CALL GPAUSE
C        ENDIF


C OPEN SYSSUM REPORT FILE
C -----------------------
C        CALL ROPEN('SYSSUM.REP',SYSSUM_LU,ST)
C        IF(ST.NE.0)THEN
C           TYPE *,IAM(),'Error opening SYSSUM.REP >',ST
C           CALL GPAUSE
C        ENDIF



C OPEN & READ DISTRIBUTION REPORT FILE (REP.FIL)
C ------------------------------------
	TRABUF(TTYP)=0
C	CALL DISREP   (TRABUF,EOF)
	CALL TMREPORT

C OPEN AGENT SALES FILE
C ---------------------
       CALL OPENASF(ASF)
       CALL TMLODASF






C OPEN THE TMF FILE
C -----------------
	CALL OPENW(PTMF,SFNAMES(1,PTMF),4,0,0,ST)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,PTMF),1,ST,0)
C	CALL OPENX(PTMF,TMF_NAME,4,0,0,ST)             !FOR TEST ONLY
C	IF(ST.NE.0) TYPE*,IAM(),' OPEN TMF FILE ERROR '

	CALL TOPEN(PTMF)      






C LOOP AND READ THE TM FILE
C -------------------------
	TYPE*,IAM(),' '
	TYPE*,IAM(),' Now Scanning the TM for all transactions'

	SER=1
	COUNT=0


100	CONTINUE


	CALL READTMF(LOGREC,SER,EOF)   !THIS NEEDS COMMON
	IF(EOF) GOTO 1000
	IF ( MOD(COUNT,50000) .EQ.0 ) TYPE*,'TMSCAN in progress ...',COUNT
	COUNT = COUNT + 1

	CALL LOGTRA(TRABUF,LOGREC)  


C 	UPDATE WHALE REPORT ONLY INCLUDE TYPE GOOD WAGERS
C	-------------------------------------------------
	IF ( TRABUF(TSTAT).EQ.GOOD    .AND.
     *       TRABUF(TTYP) .EQ.TWAG )
     *       CALL WHALE
C
C 	UPDATE TMORDER REPORT ( NOT USED FOR PORTUGAL )
C	---------------------
C	IF ( TRABUF(TTYP) .EQ.TSPE    .AND.
C     *       TRABUF(TSFUN).EQ.TORDR )
C     *       CALL TMORDER
C
C
C
C
C 	UPDATE GUTS SWAP FILE
C	---------------------
	IF(TRABUF(TTYP).EQ.TSPE .AND.TRABUF(TSFUN).EQ.TSON)
     *	   CALL GUTSW


C 	UPDATE GUTS TERSTAT FIL
C	-----------------------
	IF(TRABUF(TTYP).EQ.TSPE .AND.TRABUF(TSFUN).EQ.TTERSTS)
     *	   CALL GUTSTAT


C 	IF IT IS A COMMUNICATIONS TRANSACTION SAVE STATS FOR TAPR
C	---------------------------------------------------------
C        IF (TRABUF(TTER).GT.0.AND.TRABUF(TTYP).EQ.TSPE.AND.
C     *      TRABUF(TSFUN).EQ.TSLOW)
C     *      CALL SAVESTAT(TRABUF,STATS,NONI)


C 	UPDATE SYSSUM INFORMATION
C	-------------------------
C	CALL SYSSUM(TRABUF,EOF)



C       UPDATE DISTRIBUTION REPORT FILE
C	-------------------------------
	IF (TRABUF(TTYP) .NE.TWAG)  GOTO 100
C      	CALL DISREP  (TRABUF,EOF)                !FROM BASELINE

	IF (TRABUF(TSTAT).NE.GOOD  .AND.
     *	    TRABUF(TSTAT).NE.FRAC ) GOTO 100
	IF (TRABUF(TFRAC).NE.10)    GOTO 100
	CALL TMREPORT 
	
	GOTO 100

1000	CONTINUE

C FINAL UPDATE OF GUTS FILES
C --------------------------
	CALL GUTSW
	CALL GUTSTAT


C PRINT OUT TAPR REPORT
C ---------------------
C       CALL PRNTRPT(STATS,NONI,DBUF)
C	CALL USRCLOS1 (TAPR_LU)
C	CALL SPOOL('TAPR.REP',TAPR_COPY,ST)


C PRINT OUT WHALE REPORT
C ----------------------
        CALL WHALE
C	CALL USRCLOS1 (WHALE_LU)             !DONE IN WHALE.FOR
C	CALL SPOOL('WHALE.REP',WHALE_COPY,ST)


C PRINT OUT SYSSUM REPORT
C -----------------------
C	CALL SYSSUM(TRABUF,EOF)
C	CALL USRCLOS1 (SYSSUM_LU)
C	CALL SPOOL('SYSSUM.REP',SYSSUM_COPY,ST)


C UPDATE & PRINT DISTRIBUTION FILE/REPORTS
C ----------------------------------------
C	CALL DISREP   (TRABUF,EOF)
	CALL TMREPORT
C
C ORDER REPORT ( NOT USED FOR PORTUGAL )
C
C	CALL TMORDER
C
C
C CLOSE TM FILE
C -------------
	CALL USRCLOS1 (PTMF)
	CALL GSTOP(GEXIT_SUCCESS)


	END
