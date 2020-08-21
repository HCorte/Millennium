C  GXSRC:TRPCOMP.FOR
C
C V01 12-Jan-98 RXK Initial release.  
C
C SUBROUTINE TRPCOMP
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

	SUBROUTINE TRPCOMP(FDB,VFDB,GIND,DRW,GNUM,ST)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DTRREC.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:TTRREC.DEF'

	INTEGER*4    FDB(7)	    !file discriptor block
	INTEGER*4    VFDB(7)	    !verification file discriptor block
	INTEGER*4    GIND	    !game index
	INTEGER*4    DRW	    !draw (event) number
	INTEGER*4    GNUM	    !game number
	INTEGER*4    ST		    !result state

	CHARACTER*10 CXREPNAM /'TRDIFF.REP'/
	CHARACTER*1  DISPLAY	    !
C	INTEGER*4    BYTTAB(200)    !
	INTEGER*4    LUDEV(2)	    !
	INTEGER*4    I,J,K,L	    !misc counters
	INTEGER*4    LUID	    !
	INTEGER*4    ERRCNT	    !error count
C       INTEGER*4    RECLEN	    !
	INTEGER*4    ANS	    !
C
	COMMON       SCFREC	    !
C
C
C	CALL FASTSET(0,BYTTAB,1000)
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
	CALL READW(FDB,DRW,DTRREC,ST)
	IF(ST.NE.0) THEN
	  CALL CLRSCR(5)
	  CALL FILERR(SCFGFN(1,GNUM),2,ST,DRW)
	  CALL XWAIT(2,2,ST)
	  GOTO 20
	ENDIF
C
C  Read the corresponding record from the validation file.
C
	CALL READW(VFDB,DRW,TTRREC,ST)
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
	DO 300 LUID=LUDEV(1),LUDEV(2)
	   WRITE(LUID,990)GIND,DRW
C
C  Compare the ROW names and check for spaces.
C
   	DO 200 K=1,3
	DO 200 I=1,MAXTRPRW
	   IF(DTRNMS(1,I,K).EQ.'    ') THEN
	      DO J = 2,TRPNMS_LEN/4
		 IF(DTRNMS(J,I,K).NE.'    ') THEN
		    WRITE(LUID,940) I,(DTRNMS(L,I,K),L=1,TRPNMS_LEN/4)
	            WRITE(LUID,941)
	            ERRCNT = ERRCNT + 1
		    GOTO 200
		 ENDIF
	      END DO
	   ENDIF
	   DO 110 J=1,TRPNMS_LEN/4
	      IF(DTRNMS(J,I,K).NE.TTRNMS(J,I,K))THEN
	         WRITE(LUID,930) K,I,(DTRNMS(L,I,K),L=1,TRPNMS_LEN/4)
	         WRITE(LUID,931) K,I,(TTRNMS(L,I,K),L=1,TRPNMS_LEN/4)
	         ERRCNT = ERRCNT + 1
	         GOTO 200
	       ENDIF
110	   CONTINUE   
200     CONTINUE
C
C  Compare the BASE PRICE.
C
        IF(DTRPRC.NE.TTRPRC)THEN
           WRITE(LUID,982) CMONY(DTRPRC,10,BETUNIT)
	   WRITE(LUID,983) CMONY(TTRPRC,10,BETUNIT) 
           ERRCNT = ERRCNT + 1
        ENDIF
C
C  Compare the POOL PERCENTAGE.
C
        IF(DTRSPR.NE.TTRSPR)THEN
           WRITE(LUID,984) DISPER(DTRSPR)
           WRITE(LUID,985) DISPER(TTRSPR)
           ERRCNT = ERRCNT + 1
        ENDIF
C
C  Compare the GAME CLOSING time.
C
	IF(DTRTIM.NE.TTRTIM)THEN
	   WRITE(LUID,955) DISTIM(DTRTIM)
	   WRITE(LUID,956) DISTIM(TTRTIM)
	   ERRCNT = ERRCNT + 1
	ENDIF
C
C  Compare the BEGINNING and ENDING SALES DATES.
C
	IF(DTRBSD.NE.TTRBSD)THEN
	  WRITE(LUID,960)DTRBSD
	  WRITE(LUID,961)TTRBSD
	  ERRCNT = ERRCNT + 1
	ENDIF
	IF(DTRESD.NE.TTRESD)THEN
	  WRITE(LUID,970)DTRESD
	  WRITE(LUID,971)TTRESD
	  ERRCNT = ERRCNT + 1
	ENDIF
C
C  Check the EVENT NAME.
C
        DO 220 K=1,3
	DO 210 J=1,TRPENM_LEN/4
C
	   IF(DTRENM(J,K).NE.TTRENM(J,K))THEN
	      WRITE(LUID,995) (DTRENM(L,K),L=1,TRPENM_LEN/4)
	      WRITE(LUID,996) (TTRENM(L,K),L=1,TRPENM_LEN/4)
	      ERRCNT = ERRCNT + 1
	      GOTO 220
	   ENDIF

210	CONTINUE
220	CONTINUE
C
C  Check the pool file names.
C
	DO 250 I=1,5
	   IF (DTRPFN(I).NE.TTRPFN(I))THEN
	      WRITE(LUID,980) DTRPFN
	      WRITE(LUID,981) TTRPFN
	      ERRCNT = ERRCNT + 1
	      GOTO 260
	   ENDIF
250	CONTINUE
260	CONTINUE
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
930	FORMAT(' Primary    File   Event ',I1,' Row ',I2,' name is :',
     *         <TRPNMS_LEN/4>A4)
931	FORMAT(' Validation File   Event ',I1,' Row ',I2,' name is :',
     *         <TRPNMS_LEN/4>A4,/)
940	FORMAT(' Primary    File   Event ',I1,' Row ',I2,
     *        ' contains spaces in name :',<TRPNMS_LEN/4>A4)
941	FORMAT('                   THIS LINE WILL APPEAR CLOSED !!! ')
950	FORMAT(' Primary    File   Row Close Time is: ',A8)
951	FORMAT(' Validation File   Row Close Time is: ',A8,/)
955	FORMAT(' Primary    File   Draw Close Time is: ',A8)
956	FORMAT(' Validation File   Draw Close Time is: ',A8/)
960	FORMAT(' Primary    File  Beginning Sales Date (ADC) is: ',I4)
961     FORMAT(' Validation File  Beginning Sales Date (ADC) is: ',I4,/)
970	FORMAT(' Primary    File  Ending Sales Date (ADC) is: ',I4)
971	FORMAT(' Validation File  Ending Sales Date (ADC) is: ',I4,/)
980     FORMAT(' Primary    File  TODTRP Pool Filename:',5(A4))
981     FORMAT(' Validation File  TODTRP Pool Filename:',5(A4),/)
982     FORMAT(' Primary    File  base price:',A10)
983     FORMAT(' Validation File  base price:',A10,/)
984	FORMAT(' Primary    File  pool percentage:',F8.3)
985	FORMAT(' Validation File  pool percentage:',F8.3,/)
986	FORMAT(' Primary    File  Tv-Chanel Name:',<TRPTVC_LEN/4>A4)
987	FORMAT( 'Validation File  Tv-Chanel Name:',<TRPTVC_LEN/4>A4)
990   FORMAT(/' TODTRP ',I2,' EVENT ',I4,' GAME DATA FILE COMPARISON.',
     *	      / ' -----------------------------------------------',//)
995	FORMAT(' Primary    File  Event Name is: ',<TRPENM_LEN/4>A4)
996	FORMAT(' Validation File  Event Name is: ',<TRPENM_LEN/4>A4,/)
1000	FORMAT(///)
1001	FORMAT(/)
C
	END

