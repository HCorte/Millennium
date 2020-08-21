C
C SUBROUTINE X2CHKRCL
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CHKRCL.FOV                                 $
C  $Date::   17 Apr 1996 16:12:48                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2chkrcl.for;1 **
C
C X2CHKRCL.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will perform edit checks on the
C Report Class file.  Any errors encountered
C will be displayed to the screen, and if the input print
C flag is set, will also print the errors to the printer.
C
C Calling sequence:
C
C     CALL X2CHKRCL(PRTFLG,FAST,ERRCNT)
C
C Input parameters:
C
C     PRTFLG      Logical     Display errors to printer
C     FAST	  Logical     Check bitmap editcheck flag
C
C Output parameters:
C
C     ERRCNT      Int*4       Count of errors detected
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2CHKRCL(PRTFLG,FAST,ERRCNT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XRCL.DEF'
	INCLUDE 'INCLIB:X2XRCD.DEF'
C
	INTEGER*4   ST                          !Read status
	INTEGER*4   REC                         !Record pointer
	INTEGER*4   I                           !Array index
	INTEGER*4   ERRCNT                      !Number of errors
	INTEGER*4   EOFCNT                      !End of file count
	INTEGER*4   REPCNT                      !Number of reports
	CHARACTER   X2FILNAM*20                 !File name function
	LOGICAL     PRTFLG                      !Print error flag
	LOGICAL     EMPSLOT                     !Blank report code
	LOGICAL	    FAST			!Fast editcheck flag
C
	WRITE(5,9020)
	IF(PRTFLG) WRITE(6,9020)
	ERRCNT=0
	EOFCNT=0
	REPCNT=0
C
C OPEN THE REPORT CLASS CONFIGURATION FILE. (BUFFERED I/O)
C
	CALL OPENX2X(X2FILNAM(XRCL),1)
C
C OPEN THE REPORT CODE FILE.
C
	CALL OPENX2X(X2FILNAM(XRCD),2)
C
C READ THROUGH ENTIRE FILE SKIPPING EMPTY SLOTS. (BUFFERED I/O)
C
	REC=0
100	CONTINUE
	  REC=REC+1
	  CALL READX2X(1,REC,X2XRCL_REC,ST)
	  IF(ST.EQ.-99) THEN
	    ERRCNT=ERRCNT+1
	    WRITE(5,9080)
	    GOTO 8000
	  ENDIF
	  IF(ST.EQ.144) GOTO 8000
C
C SKIP EMPTY RECORDS.
C
	  IF(X2XRCL_REC(1).LE.0) THEN
	    EOFCNT=EOFCNT+1
	    GOTO 100
C         IF(EOFCNT.LT.100) GOTO 100
C         GOTO 8000
	  ELSE
	    EOFCNT=0
	  ENDIF
C
C IF FAST CHECK OF DATABASE, CHECK THE BITMAP TO
C DETERMINE IF THE RECORD HAS BEEN MODIFIED.
C
	  IF(FAST .AND. 
     *       X2XRCL_BITMAP.EQ.0 .AND. 
     *       X2XRCL_BITMAP2.EQ.0 .AND.
     *       X2XRCL_BITMAP3.EQ.0 .AND.
     *       X2XRCL_BITMAP4.EQ.0) GOTO 100
C
C CHECK TO MAKE SURE THAT ALL DEFINED REPORT CODE EXISTS.
C
	  EMPSLOT=.FALSE.
	  REPCNT=0
	  DO 120 I=1,16
	    IF(X2XRCL_RPTCDE(I).NE.0) THEN
	      CALL READX2X(2,X2XRCL_RPTCDE(I),X2XRCD_REC,ST)
	      IF(ST.EQ.-99) THEN
	        ERRCNT=ERRCNT+1
	        WRITE(5,9080)
	        GOTO 8000
	      ENDIF
	      IF(ST.EQ.144 .OR. X2XRCD_REC(1).LE.0) THEN
	        WRITE(5,9000) REC,X2XRCL_RPTCDE(I)
	        IF(PRTFLG) WRITE(5,9000) REC,X2XRCL_RPTCDE(I)
	        ERRCNT=ERRCNT+1
	      ELSE
	        REPCNT=REPCNT+1
	      ENDIF
	      IF(EMPSLOT) THEN
	        WRITE(5,9010) REC,I-1
	        IF(PRTFLG) WRITE(5,9010) REC,I-1
	        ERRCNT=ERRCNT+1
	        EMPSLOT=.FALSE.
	      ENDIF
	    ELSE
	      EMPSLOT=.TRUE.
	    ENDIF
120	  CONTINUE
C
C VERIFY THE INPUT NUMBER OF REPORTS AGAINST THE ACTUAL.
C
	  IF(REPCNT.NE.X2XRCL_COUNT) THEN
	    WRITE(5,9040) REC,REPCNT,X2XRCL_COUNT
	    IF(PRTFLG) WRITE(5,9040) REC,REPCNT,X2XRCL_COUNT
	    ERRCNT=ERRCNT+1
	  ENDIF
	  GOTO 100
C
C PROGRAM EXIT.
C
8000	CONTINUE
	CALL CLOSX2X(1)
	CALL CLOSX2X(2)
	WRITE(5,9030) ERRCNT
	IF(PRTFLG) WRITE(6,9030) ERRCNT
	RETURN
C
C     =================== Format Statements ==================
C
9000	FORMAT(3X,'Report Class ',I3,' Invalid report code ',I4)
9010	FORMAT(3X,'Report Class ',I3,' Blank code in slot ',I4)
9020	FORMAT(1X,'Begin Edit Check of Report Class File')
9030	FORMAT(1X,'End Edit Check of Report Class File - 'I3,
     *	          ' error(s) encountered')
9040	FORMAT(3X,'Report Class ',I3,' invalid report count ',
     *	          'actual/input ',I3,'/',I3)
9080	FORMAT(3X,'Locked record encounted - check aborted ')
	END
