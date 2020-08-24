C
C SUBROUTINE X2CHKLPC
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CHKLPC.FOV                                 $
C  $Date::   17 Apr 1996 16:12:22                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2chklpc.for;1 **
C
C X2CHKLPC.FOR
C
C V02 29-NOV-94 SCD CHANGED I3 TO I6 IN 9030 FORMAT STATEMENT - Integrate
C		    UK changes into X2X Baseline
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will perform edit checks on the
C Local Port Configuration file.  Any errors encountered
C will be displayed to the screen, and if the input print
C flag is set, will also print the errors to the printer.
C
C Calling sequence:
C
C     CALL X2CHKLPC(PRTFLG,FAST,ERRCNT)
C
C Input parameters:
C
C     PRTFLG      Logical     Display errors to printer
C     FAST        Logical     Fast edit check flag
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
	SUBROUTINE X2CHKLPC(PRTFLG,FAST,ERRCNT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XLPC.DEF'
	INCLUDE 'INCLIB:X2XNPC.DEF'
	INCLUDE 'INCLIB:X2XGBL.DEF'
C
	INTEGER*4   ST                          !Read status
	INTEGER*4   REC                         !Record pointer
	INTEGER*4   I                           !Array index
	INTEGER*4   ERRCNT                      !Number of errors
	INTEGER*4   PORTCNT                     !Number of local ports
	INTEGER*4   PRTARY(X2X_LOCAL_PORTS)     !Array of SAP ports
	INTEGER*4   SAPARY(X2X_LOCAL_PORTS)     !Array of SAPs
	INTEGER*4   NETARY(X2X_LOCAL_PORTS)     !Array of network ports
	INTEGER*4   LPCARY(X2X_LOCAL_PORTS)     !Array of local ports
	INTEGER*4   SAPTYP(X2X_SAP)             !Type of SAP ports
	CHARACTER   X2FILNAM*20                 !File name function
	LOGICAL     PRTFLG                      !Print error flag
	LOGICAL	    FAST			!Bitmap edit check flag
C
	WRITE(5,9020)
	IF(PRTFLG) WRITE(6,9020)
	ERRCNT=0
	PORTCNT=0
	DO 50 I=1,X2X_LOCAL_PORTS
	  SAPARY(I)=0
	  PRTARY(I)=0
	  NETARY(I)=0
	  LPCARY(I)=0
50	CONTINUE
	CALL FASTSET(0,SAPTYP,X2X_SAP)
C
C OPEN THE LOCAL PORT CONFIGURATION FILE. (BUFFERED I/O)
C
	CALL OPENX2X(X2FILNAM(XLPC),1)
C
C OPEN THE NETWORK PORT CONFIGURATION FILE. (BUFFERED I/O)
C
	CALL OPENX2X(X2FILNAM(XNPC),2)
C
C OPEN THE GLOBAL CONFIGURATION FILE AND GET THE GAME
C SAP.
C
	CALL OPENX2X(X2FILNAM(XGBL),3)
	CALL READX2X(3,1,X2XGBL_REC,ST)
	IF(ST.EQ.-99) THEN
	  ERRCNT=ERRCNT+1
	  WRITE(5,9080)
	  GOTO 8000
	ENDIF
C
C READ THROUGH ENTIRE FILE SKIPPING EMPTY SLOTS.
C
	REC=0
100	CONTINUE
	  REC=REC+1
	  CALL READX2X(1,REC,X2XLPC_REC,ST)
	  IF(ST.EQ.-99) THEN
	    ERRCNT=ERRCNT+1
	    WRITE(5,9080)
	    GOTO 8000
	  ENDIF
	  IF(ST.EQ.144) GOTO 8000
	  IF(X2XLPC_REC(1).LE.0) GOTO 100
C
C IF FAST CHECK OF DATABASE, CHECK THE BITMAP TO
C DETERMINE IF THE RECORD HAS BEEN MODIFIED.
C
	  IF(FAST .AND. 
     *       X2XLPC_BITMAP.EQ.0 .AND. 
     *       X2XLPC_BITMAP2.EQ.0 .AND.
     *       X2XLPC_BITMAP3.EQ.0 .AND.
     *       X2XLPC_BITMAP4.EQ.0) GOTO 100
C
C IF ASSIGNED, CHECK TO MAKE SURE THAT THE NETWORK PORT EXISTS.
C
	  IF(X2XLPC_NETPORT.GE.1) THEN
	     CALL READX2X(2,X2XLPC_NETPORT,X2XNPC_REC,ST)
	     IF(ST.EQ.-99) THEN
	       ERRCNT=ERRCNT+1
	       WRITE(5,9080)
	       GOTO 8000
	     ENDIF
	     IF(ST.EQ.144 .OR. X2XNPC_REC(1).LE.0) THEN
	       WRITE(5,9000) REC,X2XLPC_NETPORT
	       IF(PRTFLG) WRITE(5,9000) REC,X2XLPC_NETPORT
	       ERRCNT=ERRCNT+1
	     ENDIF
	  ENDIF
C
C CHECK TO ENSURE THE SAP IS NOT THE SAME AS THE GAME
C SAP.
C
	  IF(X2XLPC_SAP.EQ.X2XGBL_SAP) THEN
	    WRITE(5,9040) REC
	    IF(PRTFLG) WRITE(5,9040) REC
	    ERRCNT=ERRCNT+1
	  ENDIF
C
C CHECK THE TYPE OF THE SAP.
C
	  IF(SAPTYP(X2XLPC_SAP).EQ.0) SAPTYP(X2XLPC_SAP)=X2XNPC_TYPE
	  IF(SAPTYP(X2XLPC_SAP).NE.X2XNPC_TYPE) THEN
	    WRITE(5,9070) REC
	    IF(PRTFLG) WRITE(5,9070) REC
	    ERRCNT=ERRCNT+1
	  ENDIF
C
C CHECK TO ENSURE DUPLICATE PORTS HAVE NOT BEEN DEFINED.
C (NETWORK PORT MAY BE ZERO FOR IF THIS LOCAL PORT IS AVAILABLE AS SPARE)
C
	  DO 200 I=1,PORTCNT
	    IF(X2XLPC_SAP.EQ.SAPARY(I) .AND.
     *	       X2XLPC_SAP_PORT.EQ.PRTARY(I)) THEN
	      WRITE(5,9050) REC, LPCARY(I)
	      IF(PRTFLG) WRITE(5,9050) REC, LPCARY(I)
	      ERRCNT=ERRCNT+1
	    ENDIF
	    IF(X2XLPC_NETPORT.NE.0.AND.X2XLPC_NETPORT.EQ.NETARY(I)) THEN
	      WRITE(5,9060) REC, LPCARY(I), X2XLPC_NETPORT
	      IF(PRTFLG) WRITE(5,9060) REC, LPCARY(I), X2XLPC_NETPORT
	      ERRCNT=ERRCNT+1
	    ENDIF
200	  CONTINUE
C
C STORE THE PORT AND SAP.
C
	  PORTCNT=PORTCNT+1
	  SAPARY(PORTCNT)=X2XLPC_SAP
	  PRTARY(PORTCNT)=X2XLPC_SAP_PORT
	  NETARY(PORTCNT)=X2XLPC_NETPORT
	  LPCARY(PORTCNT)=X2XLPC_PORT
C
	GOTO 100
C
C PROGRAM EXIT.
C
8000	CONTINUE
	CALL CLOSX2X(1)
	CALL CLOSX2X(2)
	CALL CLOSX2X(3)
	WRITE(5,9030) ERRCNT
	IF(PRTFLG) WRITE(6,9030) ERRCNT
	RETURN
C
C     =================== Format Statements ==================
C
9000	FORMAT(3X,'Local port ',I3,' invalid network port ',I4)
9020	FORMAT(1X,'Begin Edit Check of Local Port Configuration')
9030	FORMAT(1X,'End Edit Check of Local Port Configuration - 'I6,	!V02
     *	          ' error(s) encountered')
9040	FORMAT(3X,'Local port ',I3,' has duplicate SAP with GAME')
9050	FORMAT(3X,'Local ports ',I3,' and ',I3,' have '
     *	          'duplicate SAP and Port')
9060	FORMAT(3X,'Local ports ',I3,' and ',I3,' have '
     *	          'duplicate network port ',I3)
9070	FORMAT(3X,'Inconsistant network port type for port ',I3)
9080	FORMAT(3X,'Locked record encounted - check aborted ')
	END
