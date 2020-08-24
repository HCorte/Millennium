C HDRRPT.FOR
C
C V01 01-MAR-2011 HXK INITIAL RELEASE FOR PORTUGAL
C
C PROGRAM TO EXTRACT DATE AND DAY STATUS FROM THE DAF.FIL
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C======	OPTIONS /CHECK=NOOVERFLOW
        PROGRAM HDRRPT
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:RECDAF.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
C
	INTEGER*4 FDB(7)
	INTEGER*4 I, ST, DAT, CDC, CDCLST, CDCFST, EXT
	INTEGER*4 REPCDC,PAGE/0/,CNT,IUNIT/7/
	INTEGER*2 DBUF(LDATE_LEN)
C
	INTEGER*4 FROM,UPTO,HOWMANY
C
	INTEGER*4 MAXINROW
	PARAMETER(MAXINROW=20)
C
        REPCDC=DAYCDC

C OPEN SCF.FIL
C
        CALL GETSCONF(SCFREC,ST)
        IF (ST.NE.0) CALL GSTOP(GEXIT_FATAL)

	DO I=1,MAXGAM					!V03
	  IF(SCFSGN(I).EQ.0 .OR. SCFSGN(I).EQ.'    ') SCFSGN(I)=' -- '
	ENDDO
C
C OPEN DAILY ACTIVITY FILE
C
        CALL OPENW(1,SCFSFN(1,DAF),4,0,0,ST)
        CALL IOINIT(FDB,1,DAFSEC*256)
        IF(ST.NE.0) THEN
          CALL CLOSEFIL(FDB)
          CALL FILERR(SCFSFN(1,DAF),1,ST,0)
        ENDIF
C
C ENTER START CDC DATE OF REPORT
C	
	WRITE(6,910), IAM()
	CALL PRMDAT(CDCFST,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
C ENTER LAST CDC DATE OF REPORT
C	
	WRITE(6,920), IAM()
	CALL PRMDAT(CDCLST,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
CCC        IF(CDCLST.GT.2000) CDCLST=2000
C
C OPEN THE REPORT FILE
C
	CALL ROPEN('HDRRPT.REP',IUNIT,ST)
	IF(ST.NE.0) THEN
	   TYPE *,IAM(),'HDRRPT.REP Open error  st - ',ST
	   CALL USRCLOS1(IUNIT)
	   CALL GSTOP(GEXIT_FATAL)
	ENDIF
	FROM = 1    
	UPTO = MIN(MAXGAM,MAXINROW)
C
10      CONTINUE
C
        CALL TITLE('HDRRPT REPORT','HDRRPT  ',1,IUNIT,PAGE,REPCDC)
	HOWMANY = UPTO-FROM+1
C
	WRITE(IUNIT,930)(SCFSGN(I),I=FROM,UPTO)
        WRITE(IUNIT,950)
        CNT=7
C
C READ DAF.FIL LOOP
C
      DO 100 CDC=CDCFST,CDCLST
        CALL READW(FDB,CDC,DAFREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFSFN(1,DAF),2,ST,CDC)
	DAT=CDC
	DBUF(5)=DAT
	CALL LCDATE(DBUF)
        WRITE(IUNIT,940)CDC,DBUF(VJUL),DBUF(VYEAR2),(DBUF(I),I=VDNAM,13),
CCC  *                  DAFSTS,(DAFDRW(I),I=FROM,UPTO)
     *                  DAFSTS,(DAFHDR(I),I=FROM,UPTO)
        CNT=CNT+1
        IF(CNT.GT.LINSPP) THEN
          CALL TITLE('HDRRPT REPORT','HDRRPT  ',1,IUNIT,PAGE,REPCDC)
          WRITE(IUNIT,930)(SCFSGN(I),I=FROM,UPTO)
          WRITE(IUNIT,950)
          CNT=7
        ENDIF
100   CONTINUE
C
	FROM = UPTO + 1
	UPTO = MIN(FROM+MAXINROW-1,MAXGAM)
        IF(FROM.LE.MAXGAM) GOTO 10
C
        CALL CLOSEFIL(FDB)
        CALL USRCLOS1(IUNIT)
	CALL GSTOP(GEXIT_SUCCESS)
C
910	FORMAT(//3X,A,' Enter the start CDC date of the report')
920	FORMAT(//3X,A,' Enter the last  CDC date of the report')
930     FORMAT(/1X,'  CDC',' JULIAN','        DATE  ',' STS ',
     *             <HOWMANY>(A4,1X))
940     FORMAT(I5,I4,'/',I4,7A2,I2,<HOWMANY>I5)
950	FORMAT(1X,131('='))
	END
