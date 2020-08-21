C
C SUBROUTINE SCCOMP
C $Log:   GXAFXT:[GOLS]SCCOMP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:52:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   04 Sep 1995  9:26:52   PXB
C  
C     Rev 1.1   07 Jul 1993 11:03:44   GXA
C  Released for Finland Dec Conversion / Oddset.
C  
C     Rev 1.0   21 Jan 1993 17:34:42   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - sccomp.for **
C
C
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SCCOMP(SFDB,VSFDB,GIND,DRW,GNUM,ST)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DSCREC.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:TSCREC.DEF'
C
	COMMON       SCFREC
	INTEGER*4    LUDEV(2)
	CHARACTER*6  GAMETYPE(2) /'soccer','hockey'/
	CHARACTER*10 CXREPNAM /'SCDIFF.REP'/
	CHARACTER*1  DISPLAY
	INTEGER*4    SFDB(7),VSFDB(7),BYTTAB(200)
	INTEGER*4    I,J,L,BROW,ST,DRW,ANS,COUNT,LUID
	INTEGER*4    GNUM,GIND
C
	CALL FASTSET(0,BYTTAB,200)
 100	CONTINUE
	TYPE*,' Select the output device(s) from the following.'
	CALL WIMG(5,' [ T-terminal, P-printer, B-both, E-exit ]')
	READ(5,915) DISPLAY
C
C  If the printer is required, open a report file.
C
	IF(((DISPLAY.EQ.'p').OR.(DISPLAY.EQ.'P')).OR.
     *	   ((DISPLAY.EQ.'b').OR.(DISPLAY.EQ.'B')))THEN
	   ST=0
	   LUID=6
	   CALL ROPEN(CXREPNAM,LUID,ST)
	   IF(ST.NE.0)THEN
	     TYPE*,'CANNOT OPEN ',CXREPNAM
	     TYPE*,'ST = ',ST
	     RETURN
	   ENDIF
	   WRITE(6,1000)
	ENDIF
C
C  Set up the logical units for output.
C
	IF((DISPLAY.EQ.'P').OR.(DISPLAY.EQ.'p'))THEN
	   LUDEV(1)=6
	   LUDEV(2)=6
	ELSEIF((DISPLAY.EQ.'T').OR.(DISPLAY.EQ.'t'))THEN
	   LUDEV(1)=5
	   LUDEV(2)=5
	ELSEIF((DISPLAY.EQ.'B').OR.(DISPLAY.EQ.'b'))THEN
	   LUDEV(1)=5
	   LUDEV(2)=6
	ELSEIF((DISPLAY.EQ.'E').OR.(DISPLAY.EQ.'e'))THEN
	   ST=0
	   RETURN
	ELSE
	   TYPE*,' Illegal input.  Try again...'
	   GOTO 100
	ENDIF
C
C  Read the appropriate record from the primary file.
C
20	CONTINUE
	CALL READW(SFDB,DRW,DSCREC,ST)
	IF(ST.NE.0) THEN
	  CALL CLRSCR(5)
	  CALL FILERR(SCFGFN(1,GNUM),2,ST,DRW)
	  CALL XWAIT(2,2,ST)
	  GOTO 20
	ENDIF
C
C  Read the corresponding record from the validation file.
C
	CALL READW(VSFDB,DRW,TSCREC,ST)
	IF(ST.NE.0)THEN
	  CALL CLRSCR(5)
	  CALL FILERR(SCFGVN(1,GNUM),2,ST,DRW)
	  CALL XWAIT(2,2,ST)
	  GOTO 20
	ENDIF
C
C  Initialize counters and the number of rows.
C
	COUNT=0
	BROW=1
	DO 300 LUID=LUDEV(1),LUDEV(2)
	   WRITE(LUID,990)GIND,DRW
C
C  Compare the team names.
C
	DO 110 J=1,SNMS_LEN/4
	   IF(DSCNM1(J).NE.TSCNM1(J))THEN
	      WRITE(LUID,930) (DSCNM1(L),L=1,SNMS_LEN/4)
	      WRITE(LUID,931) (TSCNM1(L),L=1,SNMS_LEN/4)
	      COUNT=COUNT+1
	      GOTO 120
	    ENDIF
110	CONTINUE
120	CONTINUE
C
	DO 130 J=1,SNMS_LEN/4
	   IF(DSCNM2(J).NE.TSCNM2(J))THEN
	      WRITE(LUID,935) (DSCNM2(L),L=1,SNMS_LEN/4)
	      WRITE(LUID,936) (TSCNM2(L),L=1,SNMS_LEN/4)
	      COUNT=COUNT+1
	      GOTO 140
	   ENDIF
130	CONTINUE
140	CONTINUE
C
C COMPARE THE TV-CHANEL NAMES
C
	DO J = 1,STVC_LEN/4
	   IF(DSCTVC(J).NE.TSCTVC(J)) THEN
	      WRITE(5,984) (DSCTVC(L),L=1,STVC_LEN/4)
	      WRITE(5,985) (TSCTVC(L),L=1,STVC_LEN/4)
	      COUNT = COUNT + 1
	      GOTO 150
	   ENDIF
	END DO
150	CONTINUE
C
C  Compare the odds.
C
	IF(DSCODS.NE.TSCODS)THEN
	   WRITE(LUID,940) DSCODS
	   WRITE(LUID,941) TSCODS
	   COUNT=COUNT+1
	ENDIF
C
C  Compare the drawing dates.
C
	IF (DSCDAT.NE.TSCDAT)THEN
	   WRITE(LUID,950) DSCDAT
	   WRITE(LUID,951) TSCDAT
	   COUNT=COUNT+1
	ENDIF
C
C  Compare the drawing time.
C
	IF(DISTIM(DSCTIM).NE.DISTIM(TSCTIM))THEN
	   WRITE(LUID,955) DISTIM(DSCTIM)
	   WRITE(LUID,956) DISTIM(TSCTIM)
	   COUNT=COUNT+1
	ENDIF
C
C  Compare the pool percentage time.
C
C***    IF(DSCSPR.NE.TSCSPR)THEN
C***       WRITE(LUID,957) DISPER(DSCSPR)
C***       WRITE(LUID,958) DISPER(TSCSPR)
C***       COUNT=COUNT+1
C***    ENDIF
C
C  Compare the base price.
C
C***    IF(DSCPRC.NE.TSCPRC)THEN
C***       WRITE(LUID,982) CMONY(DSCPRC,10,BETUNIT)
C***       WRITE(LUID,983) CMONY(TSCPRC,10,BETUNIT)
C***       COUNT=COUNT+1
C***    ENDIF
   
 
C
C  Compare the beginning and ending sales dates.
C
	IF(DSCBSD.NE.TSCBSD)THEN
	  WRITE(LUID,960)DSCBSD
	  WRITE(LUID,961)TSCBSD
	  COUNT=COUNT+1
	ENDIF
	IF(DSCESD.NE.TSCESD)THEN
	  WRITE(LUID,970)DSCESD
	  WRITE(LUID,971)TSCESD
	  COUNT=COUNT+1
	ENDIF
C
C  Check to game type.
C
	IF(DSCTYP.NE.TSCTYP)THEN
	  WRITE(LUID,995) GAMETYPE(DSCTYP)
	  WRITE(LUID,996) GAMETYPE(TSCTYP)
	  COUNT=COUNT+1
	ENDIF
C
C  Check the pool file names.
C
	DO 250 I=1,5
	   IF (DSCPFN(I).NE.TSCPFN(I))THEN
	      WRITE(LUID,980) DSCPFN
	      WRITE(LUID,981) TSCPFN
	      COUNT=COUNT+1
	      GOTO 260
	   ENDIF
250	CONTINUE
260	CONTINUE
C
C  Display the total number of differences.
C
	WRITE(LUID,910) COUNT
	IF(COUNT.EQ.0) THEN
	   WRITE(5,900)
	ENDIF
	WRITE(LUID,1001)
	IF(LUID.EQ.6)THEN
	  CALL USRCLOS1(     6)
	  IF(COUNT.GT.0)THEN
	     CALL SPOOL(CXREPNAM,1,ST)
	     IF(ST.NE.0)THEN
	       TYPE*,' An error has occurred, check printing device.'
	       RETURN
	     ELSE
	       TYPE*,' Output has been spooled to line printer.'
	       WRITE(5,1001)
	     ENDIF
	  ELSE
	     TYPE*,' Listing will not be generated.'
	  ENDIF
	ENDIF
	COUNT=0
300	CONTINUE
	TYPE*,'Hit RETURN to continue...'
	READ(5,902) ANS
	RETURN
 
C
C  Format statements.
C
900	FORMAT(' Files are identical...')
902	FORMAT(A)
910   FORMAT(/,' Comparison complete...',/,1X,I5,' differences found.')
915	FORMAT(A)
930	FORMAT(' Primary    File   Team 1 is :',<SNMS_LEN/4>A4)
931	FORMAT(' Validation File   Team 1 is :',<SNMS_LEN/4>A4,/)
935	FORMAT(' Primary    File   Team 2 is :',3A4)
936	FORMAT(' Validation File   Team 2 is :',3A4,/)
940	FORMAT(' Primary    File   Odds are:',I3.2)
941	FORMAT(' Validation File   Odds are:',I3.2,/)
950	FORMAT(' Primary    File   Draw Date (ADC) is: ',I4)
951	FORMAT(' Validation File   Draw Date (ADC) is: ',I4,/)
955	FORMAT(' Primary    File   Draw Time is: ',A8)
956	FORMAT(' Validation File   Draw Time is: ',A8/)
957     FORMAT(' Primary    File   Pool percentage: ',F8.3)
958     FORMAT(' Validation File   Pool percentage: ',F8.3,/)
960	FORMAT(' Primary    File  Beginning Sales Date (ADC) is: ',I4)
961   FORMAT(' Validation File  Beginning Sales Date (ADC) is: ',I4,/)
970	FORMAT(' Primary    File  Ending Sales Date (ADC) is: ',I4)
971	FORMAT(' Validation File  Ending Sales Date (ADC) is: ',I4,/)
980	FORMAT(' Primary    File  Score Pool Filename:',5(A4))
981	FORMAT(' Validation File  Score Pool Filename:',5(A4),/)
982     FORMAT(' Primary    File   Base price: ',A10,/)
983     FORMAT(' Validation File   base price: ',A10,/)
984	FORMAT(' Primary    File  TV-Chanel Name is: ',<STVC_LEN/4>A4)
985	FORMAT(' Validation File  TV-Chanel Name is: ',<STVC_LEN/4>A4)
990   FORMAT(/' SCORE ',I2,' EVENT ',I4,' GAME DATA FILE COMPARISON.',
     *	      / ' -----------------------------------------------',//)
995	FORMAT(' Primary    File  Game Type is: ',A6)
996	FORMAT(' Validation File  Game Type is: ',A6,/)
1000	FORMAT(///)
1001	FORMAT(/)
C
	END
