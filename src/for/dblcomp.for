C  GXSRC:DBLCOMP.FOR
C  
C  $Log:   GXAFXT:[GOLS]DBLCOMP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:47:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   16 Jan 1996 14:19:16   HXK
C  Removed MAXWRW
C  
C     Rev 1.1   23 Nov 1995 11:56:50   PXB
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    23 Nov 1995 11:55:30   PXB
C  Initial revision.
C  
C
C SUBROUTINE DBLCOMP
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

C=======OPTIONS /CHECK=NOOVERFLOW

	SUBROUTINE DBLCOMP(FDB,VFDB,GIND,DRW,GNUM,ST)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DDBREC.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:TDBREC.DEF'

	INTEGER*4    FDB(7)	    !file discriptor block
	INTEGER*4    VFDB(7)	    !verification file discriptor block
	INTEGER*4    GIND	    !game index
	INTEGER*4    DRW	    !draw (event) number
	INTEGER*4    GNUM	    !game number
	INTEGER*4    ST		    !result state

	CHARACTER*10 CXREPNAM /'DBDIFF.REP'/
	CHARACTER*1  DISPLAY	    !
C	INTEGER*4    BYTTAB(200)    !
	INTEGER*4    LUDEV(2)	    !
	INTEGER*4    I,J,K,L
	INTEGER*4    BROW	    !beginning row
	INTEGER*4    EROW	    !ending row
	INTEGER*4    LUID	    !
	INTEGER*4    ERRCNT	    !error count
C       INTEGER*4    RECLEN	    !
	INTEGER*4    ANS	    !
C
	COMMON       SCFREC	    !
C
C
C	CALL FASTSET(0,BYTTAB,200)
 100	CONTINUE
	TYPE*,IAM(),' Select the output device(s) from the following.'
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
	     TYPE*,IAM(),'CANNOT OPEN ',CXREPNAM
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
	   TYPE*,IAM(),' Illegal input.  Try again...'
	   GOTO 100
	ENDIF
C
C  Read the appropriate record from the primary file.
C
20	CONTINUE
	CALL READW(FDB,DRW,DDBREC,ST)
	IF(ST.NE.0) THEN
	  CALL CLRSCR(5)
	  CALL FILERR(SCFGFN(1,GNUM),2,ST,DRW)
	  CALL XWAIT(2,2,ST)
	  GOTO 20
	ENDIF
C
C  Read the corresponding record from the validation file.
C
	CALL READW(VFDB,DRW,TDBREC,ST)
	IF(ST.NE.0)THEN
	  CALL CLRSCR(5)
	  CALL FILERR(SCFGVN(1,GNUM),2,ST,DRW)
	  CALL XWAIT(2,2,ST)
	  GOTO 20
	ENDIF
C
C  Initialize counters and the number of rows.
C
	ERRCNT=0
	BROW=1
	EROW=MAXDBLRW
	DO 300 LUID=LUDEV(1),LUDEV(2)
	   WRITE(LUID,990)GIND,DRW
C
C  Compare the ROW names and check for spaces.
C
	DO 200 I=BROW,EROW
	   IF(DDBNMS(1,I).EQ.'    ') THEN
	      DO J = 2,DBLNMS_LEN/4
		 IF(DDBNMS(J,I).NE.'    ') THEN
		    WRITE(LUID,940) I,(DDBNMS(L,I),L=1,DBLNMS_LEN/4)
	            WRITE(LUID,941)
	            ERRCNT = ERRCNT + 1
		    GOTO 200
		 ENDIF
	      END DO
	   ENDIF
	   DO 110 J=1,DBLNMS_LEN/4
	      IF(DDBNMS(J,I).NE.TDBNMS(J,I))THEN
	         WRITE(LUID,930) I,(DDBNMS(L,I),L=1,DBLNMS_LEN/4)
	         WRITE(LUID,931) I,(TDBNMS(L,I),L=1,DBLNMS_LEN/4)
	         ERRCNT = ERRCNT + 1
	         GOTO 120
	       ENDIF
110	   CONTINUE   
120	   CONTINUE
C
C  Compare the CLOSE TIMES for each row.
C
	   IF (DDBRTM(I).NE.TDBRTM(I))THEN
	      WRITE(LUID,950) I,DISTIM(DDBRTM)
	      WRITE(LUID,951) I,DISTIM(TDBRTM)
	      ERRCNT = ERRCNT + 1
	   ENDIF
200	CONTINUE
C
C  Compare the BASE PRICE.
C
        IF(DDBPRC.NE.TDBPRC)THEN
           WRITE(LUID,982) CMONY(DDBPRC,10,BETUNIT)
	   WRITE(LUID,983) CMONY(TDBPRC,10,BETUNIT) 
           ERRCNT = ERRCNT + 1
        ENDIF
C
C  Compare the POOL PERCENTAGE.
C
        IF(DDBSPR.NE.TDBSPR)THEN
           WRITE(LUID,984) DISPER(DDBSPR)
           WRITE(LUID,985) DISPER(TDBSPR)
           ERRCNT = ERRCNT + 1
        ENDIF
C
C  Compare the GAME CLOSING time.
C
	IF(DDBTIM.NE.TDBTIM)THEN
	   WRITE(LUID,955) DISTIM(DDBTIM)
	   WRITE(LUID,956) DISTIM(TDBTIM)
	   ERRCNT = ERRCNT + 1
	ENDIF
C
C  Compare the BEGINNING and ENDING SALES DATES.
C
	IF(DDBBSD.NE.TDBBSD)THEN
	  WRITE(LUID,960)DDBBSD
	  WRITE(LUID,961)TDBBSD
	  ERRCNT = ERRCNT + 1
	ENDIF
	IF(DDBESD.NE.TDBESD)THEN
	  WRITE(LUID,970)DDBESD
	  WRITE(LUID,971)TDBESD
	  ERRCNT = ERRCNT + 1
	ENDIF
C
C  Check the EVENT NAME.
C
	DO 210 J=1,DBLENM_LEN/4
C
	   IF(DDBENM(J).NE.TDBENM(J))THEN
	      WRITE(LUID,995) (DDBENM(L),L=1,DBLENM_LEN/4)
	      WRITE(LUID,996) (TDBENM(L),L=1,DBLENM_LEN/4)
	      ERRCNT = ERRCNT + 1
	      GOTO 220
	   ENDIF
210	CONTINUE
220	CONTINUE
C
C  Check the pool file names.
C
	DO 250 I=1,5
	   IF (DDBPFN(I).NE.TDBPFN(I))THEN
	      WRITE(LUID,980) DDBPFN
	      WRITE(LUID,981) TDBPFN
	      ERRCNT = ERRCNT + 1
	      GOTO 260
	   ENDIF
250	CONTINUE
260	CONTINUE
C
C CHECK THE TV-CHANEL NAME
C
	DO I = 1,DBLTVC_LEN/4
	   IF(DDBTVC(I).NE.TDBTVC(I)) THEN
	      WRITE(LUID,986) (DDBTVC(K),K=1,DBLTVC_LEN/4)
	      WRITE(LUID,987) (TDBTVC(K),K=1,DBLTVC_LEN/4)
	      ERRCNT = ERRCNT + 1
	      GOTO 270
	   ENDIF
	END DO
270	CONTINUE
C
C  Display the total number of differences.
C
	WRITE(LUID,910) ERRCNT
	IF(ERRCNT.EQ.0) THEN
	   WRITE(5,900)
	ENDIF
	WRITE(LUID,1001)
	IF(LUID.EQ.6)THEN
	  CALL USRCLOS1(     6)
	  IF(ERRCNT.GT.0)THEN
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
	ERRCNT = 0
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
930	FORMAT(' Primary    File   Row ',I2,' name is :',
     *         <DBLNMS_LEN/4>A4)
931	FORMAT(' Validation File   Row ',I2,' name is :',
     *         <DBLNMS_LEN/4>A4,/)
940	FORMAT(' Primary    File   Row ',I2,' contains spaces name is:',
     *         <DBLNMS_LEN/4>A4)
941	FORMAT('                   THIS LINE WILL APPEAR CLOSED !!! ')
950	FORMAT(' Primary    File   Row Close Time is: ',A8)
951	FORMAT(' Validation File   Row Close Time is: ',A8,/)
955	FORMAT(' Primary    File   Draw Close Time is: ',A8)
956	FORMAT(' Validation File   Draw Close Time is: ',A8/)
960	FORMAT(' Primary    File  Beginning Sales Date (ADC) is: ',I4)
961     FORMAT(' Validation File  Beginning Sales Date (ADC) is: ',I4,/)
970	FORMAT(' Primary    File  Ending Sales Date (ADC) is: ',I4)
971	FORMAT(' Validation File  Ending Sales Date (ADC) is: ',I4,/)
980     FORMAT(' Primary    File  SupDbl Pool Filename:',5(A4))
981     FORMAT(' Validation File  SupDbl Pool Filename:',5(A4),/)
982     FORMAT(' Primary    File  base price:',A10)
983     FORMAT(' Validation File  base price:',A10,/)
984	FORMAT(' Primary    File  pool percentage:',F8.3)
985	FORMAT(' Validation File  pool percentage:',F8.3,/)
986	FORMAT(' Primary    File  Tv-Chanel Name:',<DBLTVC_LEN/4>A4)
987	FORMAT( 'Validation File  Tv-Chanel Name:',<DBLTVC_LEN/4>A4)
990   FORMAT(/' SUPDBL ',I2,' EVENT ',I4,' GAME DATA FILE COMPARISON.',
     *	      / ' -----------------------------------------------',//)
995	FORMAT(' Primary    File  Event Name is: ',<DBLENM_LEN/4>A4)
996	FORMAT(' Validation File  Event Name is: ',<DBLENM_LEN/4>A4,/)
1000	FORMAT(///)
1001	FORMAT(/)
C
	END
