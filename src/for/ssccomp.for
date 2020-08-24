C
C SUBROUTINE SSCCOMP
C
C V01 23-DEC-97 RXK INNITIAL RELEASE.
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
	SUBROUTINE SSCCOMP(SFDB,VSFDB,GIND,DRW,GNUM,ST)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DSSREC.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:TSSREC.DEF'
C
	COMMON       SCFREC
	INTEGER*4    LUDEV(2)
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
	CALL READW(SFDB,DRW,DSSREC,ST)
	IF(ST.NE.0) THEN
	  CALL CLRSCR(5)
	  CALL FILERR(SCFGFN(1,GNUM),2,ST,DRW)
	  CALL XWAIT(2,2,ST)
	  GOTO 20
	ENDIF
C
C  Read the corresponding record from the validation file.
C
	CALL READW(VSFDB,DRW,TSSREC,ST)
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
C  Compare the event and set names.
C
	DO 110 J=1,SSNMS_LEN/4
	   IF(DSSMNM(J).NE.TSSMNM(J))THEN
	      WRITE(LUID,930) (DSSMNM(L),L=1,SSNMS_LEN/4)
	      WRITE(LUID,931) (TSSMNM(L),L=1,SSNMS_LEN/4)
	      COUNT=COUNT+1
	      GOTO 120
	    ENDIF
110	CONTINUE
120	CONTINUE
C
        DO 140 I=1,3
	  DO 130 J=1,SNMS_LEN/4
	    IF(DSSSNM(J,I).NE.TSSSNM(J,I))THEN
	      WRITE(LUID,935) I,(DSSSNM(L,I),L=1,SSNMS_LEN/4)
	      WRITE(LUID,936) I,(TSSSNM(L,I),L=1,SSNMS_LEN/4)
	      COUNT=COUNT+1
	      GOTO 140
	    ENDIF
130	  CONTINUE
140	CONTINUE
C
C  Compare the drawing dates.
C
	IF (DSSDAT.NE.TSSDAT)THEN
	   WRITE(LUID,950) DSSDAT
	   WRITE(LUID,951) TSSDAT
	   COUNT=COUNT+1
	ENDIF
C
C  Compare the drawing time.
C
	IF(DISTIM(DSSTIM).NE.DISTIM(TSSTIM))THEN
	   WRITE(LUID,955) DISTIM(DSSTIM)
	   WRITE(LUID,956) DISTIM(TSSTIM)
	   COUNT=COUNT+1
	ENDIF
C
C  Compare the beginning and ending sales dates.
C
	IF(DSSBSD.NE.TSSBSD)THEN
	  WRITE(LUID,960)DSSBSD
	  WRITE(LUID,961)TSSBSD
	  COUNT=COUNT+1
	ENDIF
	IF(DSSESD.NE.TSSESD)THEN
	  WRITE(LUID,970)DSSESD
	  WRITE(LUID,971)TSSESD
	  COUNT=COUNT+1
	ENDIF
C
C  Check the pool file names.
C
	DO 250 I=1,5
	   IF (DSSPFN(I).NE.TSSPFN(I))THEN
	      WRITE(LUID,980) DSSPFN
	      WRITE(LUID,981) TSSPFN
	      COUNT=COUNT+1
	      GOTO 260
	   ENDIF
250	CONTINUE
260	CONTINUE
	DO 270 I=1,5
	   IF (DSSPOF(I).NE.TSSPOF(I))THEN
	      WRITE(LUID,982) DSSPOF
	      WRITE(LUID,983) TSSPOF
	      COUNT=COUNT+1
	      GOTO 280
	   ENDIF
270	CONTINUE
280	CONTINUE
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
930	FORMAT(' Primary    File   Master Name is :',<SSNMS_LEN/4>A4)
931	FORMAT(' Validatio  File   Master Name is :',<SSNMS_LEN/4>A4)
935	FORMAT(' Validation File   Set',i1,' Name is :',<SSNMS_LEN/4>A4)
936	FORMAT(' Primary    File   Set',i1,' Name is :',<SSNMS_LEN/4>A4)
950	FORMAT(' Primary    File   Draw Date (ADC) is: ',I4)
951	FORMAT(' Validation File   Draw Date (ADC) is: ',I4,/)
955	FORMAT(' Primary    File   Draw Time is: ',A8)
956	FORMAT(' Validation File   Draw Time is: ',A8/)
960	FORMAT(' Primary    File  Beginning Sales Date (ADC) is: ',I4)
961   FORMAT(' Validation File  Beginning Sales Date (ADC) is: ',I4,/)
970	FORMAT(' Primary    File  Ending Sales Date (ADC) is: ',I4)
971	FORMAT(' Validation File  Ending Sales Date (ADC) is: ',I4,/)
980	FORMAT(' Primary    File  SuperScore Pool Filename:',5(A4))
981	FORMAT(' Validation File  Superscore Pool Filename:',5(A4),/)
982	FORMAT(' Primary    File  Superscore Pool Overflow Filename:',5(A4))
983	FORMAT(' Validation File  Superscore Pool Overflow Filename:',
     *           5(A4),/)
990   FORMAT(/' SCORE ',I2,' EVENT ',I4,' GAME DATA FILE COMPARISON.',
     *	      / ' -----------------------------------------------',//)
1000	FORMAT(///)
1001	FORMAT(/)
C
	END
