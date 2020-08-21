C
C SUBROUTINE X2CHKSPC
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CHKSPC.FOV                                 $
C  $Date::   17 Apr 1996 16:13:00                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2chkspc.for;1 **
C
C X2CHKSPC.FOR
C
C V03 19-AUG-94 GPR MODIFIED TO SPEED UP SLIGHTLY - Integrate UK changes 
C		    into X2X Baseline
C V02 09-JUL-92 NJA FIXED PROBLEM WITH FIELD OVERFLOWS.
C V01 01-DEC-91 DAS REMOVED TEST OF TERCNT .EQ. 1
C V01 01-DEC-91 XXX RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will perform edit checks on the
C Station Port Configuration.  Any errors encountered
C will be displayed to the screen, and if the input print
C flag is set, will also print the errors to the printer.
C
C Calling sequence:
C
C     CALL X2CHKSPC(PRTFLG,FAST,ERRCNT)
C
C Input parameters:
C
C     PRTFLG      Logical     Display errors to printer
C     FAST	  Logical     Fast editcheck flag
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
	SUBROUTINE X2CHKSPC(PRTFLG,FAST,ERRCNT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XSPC.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
	INCLUDE 'INCLIB:X2XLIS.DEF'
	INCLUDE 'INCLIB:X2XCHK.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INTEGER*4   ST                          !Read status
	INTEGER*4   REC                         !Record pointer
	INTEGER*4   I,J                         !Array index
	INTEGER*4   TERCNT                      !Count of terminals
	INTEGER*4   ERRCNT                      !Number of errors
	CHARACTER   X2FILNAM*20                 !File name function
	LOGICAL     PRTFLG                      !Print error flag
	LOGICAL	    FAST			!Fast editcheck flag
	LOGICAL	    DONE			!Done reading		!V03
C
	WRITE(5,9020)
	IF(PRTFLG) WRITE(6,9020)
	ERRCNT=0
C
C LOAD THE TERMINAL AND DROP ARRAYS FROM THE TERMINAL
C FILE.
C
	CALL X2GETTER
C
C OPEN THE PORT CONFIGURATION FILE.
C
	CALL OPENX2X(X2FILNAM(XSPC),1)
C
C       ***** Start V03 changes *****
C
C OPEN THE STATION FILE.
C
	CALL OPENX2X(X2FILNAM(XSTN),4)
C
C       Read the entire Station file up front
C
        REC=1
        DONE=.FALSE.
        DO WHILE ((.NOT.DONE).AND.(REC.LE.X2X_STATIONS))
          CALL READX2X(4,REC,X2XSTN_REC,ST)
          IF(ST.EQ.-99) THEN
            WRITE(5,9080) IAM()
            ERRCNT=ERRCNT+1
            DONE=.TRUE.
          ELSEIF (ST.EQ.144) THEN
            DONE=.TRUE.
          ELSE
	    X2XCHK_STNREC(X2XCHK_STN,REC)=X2XSTN_STN
            REC=REC+1
          ENDIF
        ENDDO
	CALL CLOSX2X(4)

        IF(ST.NE.0) GOTO 8000
C
C       ***** End V03 changes *****
C
C
C READ THROUGH PORT CONFIGURATION FILE SKIPPING EMPTY SLOTS.
C
	REC=0
100	CONTINUE
	  REC=REC+1
	  CALL READX2X(1,REC,X2XSPC_REC,ST)
	  IF(ST.EQ.-99) THEN
	    ERRCNT=ERRCNT+1
	    WRITE(5,9080)
	    GOTO 8000
	  ENDIF
	  IF(ST.EQ.144) GOTO 8000
	  IF(X2XSPC_REC(1).LE.0) GOTO 100
C
C IF FAST CHECK OF DATABASE, CHECK THE BITMAP TO
C DETERMINE IF THE RECORD HAS BEEN MODIFIED.
C
	  IF(FAST .AND. 
     *       X2XSPC_BITMAP.EQ.0 .AND. 
     *       X2XSPC_BITMAP2.EQ.0 .AND.
     *       X2XSPC_BITMAP3.EQ.0 .AND.
     *       X2XSPC_BITMAP4.EQ.0) GOTO 100
C
C COUNT THE NUMBER OF DEFINED DROPS.
C
	  TERCNT=0
	  DO 120 I=1,X2X_MAXTERMS
	    IF(X2XSPC_DROPS(I).GT.'  ') THEN
	      TERCNT=TERCNT+1
	    ENDIF
120	  CONTINUE
C
C COMPARE THE DEFINED COUNT AGAINST THE INPUT COUNT.
C
	  IF(TERCNT.NE.X2XSPC_TERCNT) THEN
	    WRITE(5,9000) X2XSPC_STN, X2XSPC_PORT,
     *	                  TERCNT, X2XSPC_TERCNT
	    IF(PRTFLG) WRITE(6,9000) X2XSPC_STN, X2XSPC_PORT,
     *	                             TERCNT, X2XSPC_TERCNT
	    ERRCNT=ERRCNT+1
	  ENDIF
C
C CHECK THE STATION RECORD TO ENSURE THAT IF X25 TERMINAL,
C NO MORE THAN ONE TERMINAL IS DEFINED.
C
	  IF(X2XCHK_STNREC(X2XCHK_STN,X2XSPC_STN).EQ.0) THEN	    !V03
	    WRITE(5,9060) X2XSPC_STN, X2XSPC_PORT		    !V03
	    IF(PRTFLG) WRITE(6,9060) X2XSPC_STN, X2XSPC_PORT
	    ERRCNT=ERRCNT+1
	  ENDIF
C
C CHECK FOR X25 STATION TYPE.
C
C....	  IF(X2XSTN_TYPE.EQ.X2XSCT_X25SVC .AND. TERCNT.NE.1) THEN
C.....	    WRITE(5,9070) X2XSPC_STN, X2XSPC_PORT
C....	    IF(PRTFLG) WRITE(6,9070) X2XSPC_STN, X2XSPC_PORT
C....	    ERRCNT=ERRCNT+1
C....	  ENDIF
C
C COMPARE THE TERMINALS DEFINED IN THE TERMINAL CONFIGURATION
C FILE WITH THE DEFINED TERMINALS.
C
    	  IF(X2XCHK_SPC(X2XSPC_STN,X2XSPC_PORT).NE.TERCNT) THEN
    	    WRITE(5,9010) X2XSPC_STN, X2XSPC_PORT, TERCNT,
     *	                  X2XCHK_SPC(X2XSPC_STN,X2XSPC_PORT)
    	    IF(PRTFLG) WRITE(6,9010) X2XSPC_STN, X2XSPC_PORT, TERCNT,
     *	                  X2XCHK_SPC(X2XSPC_STN,X2XSPC_PORT)
   	    ERRCNT=ERRCNT+1
   	  ENDIF
C
C CHECK FOR TOO MANY TERMINALS CONFIGURED ON THE PORT.
C
	  IF(TERCNT.GT.X2X_MAXTERMS) THEN
	    WRITE(5,9050) X2XSPC_STN, X2XSPC_PORT
	    IF(PRTFLG) WRITE(5,9050) X2XSPC_STN, X2XSPC_PORT
	    ERRCNT=ERRCNT+1
	  ENDIF
C
C CHECK FOR DUPLICATE DEFINED DROPS.
C
	  DO 130 I=1,X2XLIS_TERMS(0,X2XSPC_PORT,X2XSPC_STN)
	    DO 132 J=I+1,X2XLIS_TERMS(0,X2XSPC_PORT,X2XSPC_STN)
	      IF(X2XLIS_DROPS(I,X2XSPC_PORT,X2XSPC_STN) .EQ.
     *	         X2XLIS_DROPS(J,X2XSPC_PORT,X2XSPC_STN)) THEN
	        WRITE(5,9040) X2XSPC_STN, X2XSPC_PORT,
     *	                      X2XLIS_TERMS(I,X2XSPC_PORT,X2XSPC_STN),
     *	                      X2XLIS_TERMS(J,X2XSPC_PORT,X2XSPC_STN),
     *	                      X2XLIS_DROPS(J,X2XSPC_PORT,X2XSPC_STN)
	        IF(PRTFLG) WRITE(6,9040)
     *	                      X2XSPC_STN, X2XSPC_PORT,
     *	                      X2XLIS_TERMS(I,X2XSPC_PORT,X2XSPC_STN),
     *	                      X2XLIS_TERMS(J,X2XSPC_PORT,X2XSPC_STN),
     *	                      X2XLIS_DROPS(J,X2XSPC_PORT,X2XSPC_STN)
 
	        ERRCNT=ERRCNT+1
	      ENDIF
132	    CONTINUE
130	  CONTINUE
C
C READ THE NEXT RECORD.
C
	  GOTO 100
C
C PROGRAM EXIT.
C
8000	CONTINUE
	CALL CLOSX2X(1)
	WRITE(5,9030) ERRCNT
	IF(PRTFLG) WRITE(6,9030) ERRCNT
	RETURN
C
C     =================== Format Statements ==================
C
9000	FORMAT(3X,'Stn/Port ',I5,'/',I4,' Input Term cnt not ',
     *	          'match defined drops - 'I4,'/',I4)
9010	FORMAT(3X,'Stn/Port ',I5,'/',I4,' Defined drops not match ',
     *	          'defined terminals - ',I4,'/',I4)
9020	FORMAT(1X,'Begin Edit Check of Station Port Configuration')
9030	FORMAT(1X,'End Edit Check of Station Port Configuration - 'I4,
     *	          ' error(s) encountered')
9040	FORMAT(3X,'Stn/Port ',I5,'/',I4,' Terms ',I5,' and ',I5,
     *	          ' have duplicate drop ',A2)
9050	FORMAT(3X,'Stn/Port ',I5,'/',I4,' Exceeded max terminals ')
9060	FORMAT(3X,'Stn/Port ',I5,'/',I4,' Unknown station number ')
9070	FORMAT(3X,'Stn/Port ',I5,'/',I4,' Exceeded terminals for ',
     *	          'X25 station ')
9080	FORMAT(3X,'Locked record encounted - check aborted ')
	END
