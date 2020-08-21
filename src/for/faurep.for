C
C SUBROUTINE FAUREP
C $Log:   GXAFXT:[GOLS]FAUREP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:07:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:15:34   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - faurep.for **
C
C FAUREP.FOR
C
C V02 16-MAR-11 GPW NUMAGT=12288
C V01 06-SEP-92 HDB  INITIAL RELEASE BASED ON FUALTANL.FOR
C
C SUBROUTINE TO REPORT FAULTS FROM TERMINAL
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
	SUBROUTINE FAUREP(TRABUF,REPLU,CDCDATE,EOF,ERROR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*2   DATE(LDATE_LEN)         !
	INTEGER*4   LOGREC(LMUREC)	    !
	INTEGER*4   XX			    !
	INTEGER*4   EXT			    !
	INTEGER*4   COPY		    !
	INTEGER*4   ST			    !
	INTEGER*4   SER			    !
	INTEGER*4   PAGE		    !
	INTEGER*4   LINCNT		    !
	INTEGER*4   REPLU		    !
	INTEGER*4   STATUS		    !error after call
C
	INTEGER*4   TER			    !Terminal Number.
	INTEGER*4   STACK		    !Stcak Pointer.
	INTEGER*4   LNENO		    !Line Number.
	INTEGER*4   FAULT		    !Fault Code.
	INTEGER*4   PCPNT		    !PC Pointer.
	INTEGER*4   PRCID		    !Process ID.
	INTEGER*4   ERROR		    !problems ?
	INTEGER*4   CDCDATE		    !reports cdc date
	INTEGER*4   FLTS(NUMAGT)	    !number of faults per agent
C
	CHARACTER HEAD*40
	CHARACTER*20	REPNAME		    !report name
	CHARACTER   REPORTNAME*5	    !equivalenced report name
	EQUIVALENCE (REPNAME,REPORTNAME)    !,,	    ,,	    ,,	  ,,,
C
	LOGICAL EOF			    !Are we at the end of file
C
	DATA LINCNT/70/			    !
	LOGICAL FIRST  /.TRUE./		    !
C
C
C CLEAR/SET VARIABLES
C
	ERROR = 0
	IF(FIRST) THEN
	    WRITE (5,9010) CDCDATE
	    WRITE (REPNAME,9005) CDCDATE
	    CALL ROPEN(REPNAME,REPLU,STATUS)
	    IF(STATUS.NE.0) THEN
	      TYPE*,IAM(),REPNAME,' Open error  st - ',STATUS
	      CALL USRCLOS1(REPLU)
	      ERROR= -1
	      RETURN
	    ENDIF
	    FIRST=.FALSE.
	    PAGE=0
	    SER=1
C
C GET TODAYS DATE FROM MEMORY
C
	    DATE(VCDC)=CDCDATE
	    CALL LCDATE(DATE)
C
C ENCODE REPORT HEADER
C
	    WRITE (HEAD,8001) DATE(VMON),DATE(VDAY),DATE(VYEAR2)
	ENDIF
C
	IF(EOF) THEN
	    
	    CALL USRCLOS1(REPLU)
	ENDIF
C
	IF(TRABUF(TSTAT).NE.GOOD)   RETURN
	IF(TRABUF(TTYP).NE.TSPE)    RETURN
	IF(TRABUF(TSFUN).NE.TFAULT) RETURN
	TER=TRABUF(TTER)
C
C REPORT DETAILS
C
	IF(LINCNT.GT.55) THEN
	  CALL TITLE(HEAD,'FAULTANL',1,REPLU,PAGE,DAYCDC)
	  WRITE(REPLU,9000)
	  LINCNT=5
	ENDIF
C
	STACK=0     !STACK POINTER 2 BYTES
	CALL MOVBYTN(TRABUF(TSDT1),1,STACK,3,2)
	LNENO=0     !CODE LINE NUMBER
	CALL MOVBYTN(TRABUF(TSDT1),3,LNENO,3,2)
	FAULT=0	    !FAULT CODE
	CALL MOVBYTN(TRABUF(TSOLD),1,FAULT,1,4)
	PCPNT=0	    !PC POINTER
	CALL MOVBYTN(TRABUF(TSNEW),1,PCPNT,1,4)
	PRCID=0	    !PROCESS ID
C
	CALL MOVBYTN(TRABUF(TSSGN),1,PRCID,1,4)
	WRITE(REPLU,9001)TER,FAULT,PCPNT,PRCID,
     *	   STACK,LNENO,(TRABUF(XX),XX=TSDT2,TSDT6)
	LINCNT=LINCNT+1
	RETURN
C
C1000	CONTINUE
C	CALL USRCLOS1(REPLU)
C	CALL SPOOL('FAULTANL.REP',COPY,ST)
C	CALL GSTOP(GEXIT_SUCCESS)
C
C     ======================= FORMAT STATEMENTS ====================
C
8001	FORMAT('SPECTRA FAULT TRACKING ----> ',I2.2,'/',I2.2,'/',I4.4)
C
9000	FORMAT(/,1X,' TERM',3X,'FAULT',3X,'PC POINTER',4X,'PROCESS ID',
     *	       4X,'S. R.',5X,'LINE',4X,'16 BYTES OF SOURCE DATA',//)
9001	FORMAT(1X,I5,1X,Z8.8,2X,Z8.8,10X,A4,3X,Z8.8,3X,I6.6,3X,4(4A4))
9005	FORMAT('FAUREP',I4.4,'.REP')
9010	FORMAT(' CREATING    FAUREP',I4.4,'.REP')
C
	END
