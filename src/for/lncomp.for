C
C SUBROUTINE LNCOMP
C $Log:   GXAFXT:[GOLS]LNCOMP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:50:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.4   04 Sep 1995  9:43:14   PXB
C  
C     Rev 1.4   10 Aug 1995 11:11:14   PXB
C  RFSS 207: - Can now handle draw numbers over 100.
C  
C     Rev 1.3   07 Sep 1993 13:29:30   HXK
C  Added longer TTSTVC, DTSTVC for individual TV channel for each row
C  
C     Rev 1.2   07 Jul 1993 17:35:54   GXA
C  Added check for TV-Chanel.
C  
C     Rev 1.1   29 Jun 1993 18:11:10   GXA
C  Released for Finland Dec Conversion / Oddset.
C  
C     Rev 1.0   21 Jan 1993 16:51:56   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - tscomp.for **
C
C TSCOMP.FOR
C
C V03 04-SEP-92 HDB  CORRETED CHECKING OF EVENT DATE, ADDED CHECK FOR
C                    BASE PRICE
C V02 02-SEP-92 GCAN ADDED CHECK FOR EVENT DRAW DATE.
C V01 01-AUG-90 XXX  RELEASED FOR VAX
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
	SUBROUTINE LNCOMP(FDB,VFDB,GIND,DRW,GNUM,ST)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DTSREC.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:TTSREC.DEF'
C
	COMMON       SCFREC
	CHARACTER*10 CXREPNAM /'TSDIFF.REP'/
	CHARACTER*1  DISPLAY
	INTEGER*4 FDB(7),VFDB(7),BYTTAB(500),LUDEV(2)
	INTEGER*4 I,J,K,L,M,BROW,EROW,LUID,COUNT
	INTEGER*4 GNUM,GIND,ST,DRW
C
	CALL FASTSET(0,BYTTAB,500)
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
	CALL READW(FDB,DRW,DTSREC,ST)
	IF(ST.NE.0) THEN
	  CALL CLRSCR(5)
	  CALL FILERR(SCFGFN(1,GNUM),2,ST,DRW)
	  CALL XWAIT(2,2,ST)
	  GOTO 20
	ENDIF
C
C  Read the corresponding record from the Verification file.
C
	CALL READW(VFDB,DRW,TTSREC,ST)
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
	EROW=DTSRWS

	TYPE *,'PAUL '
	DO 300 LUID=LUDEV(1),LUDEV(2)
	   WRITE(LUID,990)GIND,DRW
C
C  Loop through all rows.
C
	DO 200 I=BROW,EROW
C
C  Compare the team names.
C

	   DO 115 K=1,2
	      DO 110 J=1,(TNMS_LEN/4)
	         IF(DTSNMS(J,K,I).NE.TTSNMS(J,K,I))THEN
	            WRITE(LUID,930) I,
     *		    ((DTSNMS(L,M,I),L=1,TNMS_LEN/4),M=1,2)
	            WRITE(LUID,931) I,
     *              ((TTSNMS(L,M,I),L=1,TNMS_LEN/4),M=1,2)
	            COUNT=COUNT+1
	            GOTO 120
	          ENDIF
110	      CONTINUE
115	   CONTINUE
120	   CONTINUE
C
C  Compare the odds.
C
	   DO 125 J=1,3
	      IF(DTSODS(J,I).NE.TTSODS(J,I))THEN
	         WRITE(LUID,940) I,(DTSODS(L,I),L=1,3)
	         WRITE(LUID,941) I,(TTSODS(L,I),L=1,3)
	         COUNT=COUNT+1
	         GOTO 130
	      ENDIF
125	   CONTINUE
130	   CONTINUE
C
C COMPARE THE TV-CHANEL NAMES
C
           DO J = 1,TTVC_LEN/4
              IF(DTSTVC(J,I).NE.TTSTVC(J,I)) THEN
                 WRITE(5,984) (DTSTVC(L,I),L=1,TTVC_LEN/4)
                 WRITE(5,985) (TTSTVC(L,I),L=1,TTVC_LEN/4)
                 COUNT = COUNT + 1
                 GOTO 150
              ENDIF
           ENDDO
150        CONTINUE
C
C  Compare the drawing dates.
C
	   IF (DTSDAT(I).NE.TTSDAT(I))THEN
	      WRITE(LUID,950) I,DTSDAT(I)
	      WRITE(LUID,951) I,TTSDAT(I)
	      COUNT=COUNT+1
	   ENDIF
C
C  Compare the drawing time.
C
	   IF(DISTIM(DTSTIM(I)).NE.DISTIM(TTSTIM(I)))THEN
	      WRITE(LUID,955) I,DISTIM(DTSTIM(I))
	      WRITE(LUID,956) I,DISTIM(TTSTIM(I))
	      COUNT=COUNT+1
	   ENDIF
 
200	CONTINUE
C
C  Compare the beginning and ending sales dates.
C
	IF(DTSBSD.NE.TTSBSD)THEN
	  WRITE(LUID,960)DTSBSD
	  WRITE(LUID,961)TTSBSD
	  COUNT=COUNT+1
	ENDIF
C
	IF(DTSESD.NE.TTSESD)THEN
	  WRITE(LUID,970)DTSESD
	  WRITE(LUID,971)TTSESD
	  COUNT=COUNT+1
	ENDIF
C
	IF(DTSDTE.NE.TTSDTE)THEN
	   WRITE(LUID,975) DTSDTE
	   WRITE(LUID,976) TTSDTE
	   COUNT = COUNT + 1
	ENDIF
C
C Check base price
C
C***	IF(DTSPRC.NE.TTSPRC)THEN
C***	   WRITE(LUID,977) CMONY(DTSPRC,12,BETUNIT)
C***	   WRITE(LUID,978) CMONY(TTSPRC,12,BETUNIT)
C***	   COUNT = COUNT + 1
C***	ENDIF	
C
	IF(DTSPRC.LE.0) THEN
	   WRITE(LUID,979) 
	   COUNT = COUNT + 1
	ENDIF
C
C  Check the pool file names.
C
	DO 250 I=1,5
	   IF (DTSPFN(I).NE.TTSPFN(I))THEN
	      WRITE(LUID,980) DTSPFN
	      WRITE(LUID,981) TTSPFN
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
	WRITE(LUID,1000)
	IF(LUID.EQ.6)THEN
	  CALL USRCLOS1(     6)
	  IF(COUNT.GT.0)THEN
	     CALL SPOOL(CXREPNAM,1,ST)
	     IF(ST.NE.0)THEN
	       TYPE*,' An error has occurred, check printing device.'
	       RETURN
	     ELSE
	       TYPE*,' Output has been spooled to line printer.'
	       WRITE(5,1000)
	     ENDIF
	  ELSE
	     TYPE*,' Listing will not be generated.'
	  ENDIF
	ENDIF
	COUNT=0
300	CONTINUE
 
C
C  Format statements.
C
900	FORMAT(' Files are identical...')
910   FORMAT(/,' Comparison complete...',/,1X,I5,' differences found.')
915	FORMAT(A)
930   FORMAT(' Primary    File  Row # ',I2,' Teams are:',
     *   <TNMS_LEN/4>A4,' .vs. ',<TNMS_LEN/4>A4)
931   FORMAT(' Verification File  Row # ',I2,' Teams are:',
     *   <TNMS_LEN/4>A4,' .vs. ',<TNMS_LEN/4>A4,/)
940	FORMAT(' Primary    File  Row # ',I2,' Odds are:',3(I3.2,2X))
941   FORMAT(' Verification File  Row # ',I2,' Odds are:',3(I3.2,2X),/)
950   FORMAT(' Primary    File  Row # ',I2,' Draw Date (ADC) is: ',I4)
951	FORMAT(' Verification File  Row # ',I2,' Draw Date (ADC) is: ',
     *	 I4,/)
955	FORMAT(' Primary    File  Row # ',I2,' Draw Time is: ',A8)
956	FORMAT(' Verification File  Row # ',I2,' Draw Time is: ',A8/)
960	FORMAT(' Primary    File  Beginning Sales Date (ADC) is: ',I4)
961   FORMAT(' Verification File  Beginning Sales Date (ADC) is: ',I4,/)
970	FORMAT(' Primary    File  Ending Sales Date (ADC) is: ',I4)
971	FORMAT(' Verification File  Ending Sales Date (ADC) is: ',I4,/)
975	FORMAT(' Primary    File  Event Draw Date (ADC) is: ',I4)
976	FORMAT(' Verification File  Event Draw Date (ADC) is: ',I4,/)
977	FORMAT(' Primary    File  Base Price (ADC) is: ',A12)
978	FORMAT(' Verification File  Base Price (ADC) is: ',A12,/)
979	FORMAT(' Primary    File  Base Price is ZERO!!')
980	FORMAT(' Primary    File  Select Pool Filename:',5(A4))
981	FORMAT(' Verification File  Select Pool Filename:',5(A4),/)
984     FORMAT(' Primary    File  TV-Chanel Name is: ',<TTVC_LEN/4>A4)
985     FORMAT(' Validation File  TV-Chanel Name is: ',<TTVC_LEN/4>A4)
990   FORMAT(/' SELECT ',I2,' EVENT ',I4,' GAME DATA FILE COMPARISON.',
     *	      / ' -----------------------------------------------',//)
1000	FORMAT(///)
C
	END
