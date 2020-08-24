C
C SUBROUTINE X2CHKNPC
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CHKNPC.FOV                                 $
C  $Date::   17 Apr 1996 16:12:44                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2chknpc.for;1 **
C
C X2CHKNPC.FOR
C
C V02 29-NOV-94 SCD CHANGED I2 TO I3 IN 9010 FORMAT STATEMENT - Integrate 
C		    UK changes into X2X Baseline
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will perform edit checks on the
C Network Port Configuration file.  Any errors encountered
C will be displayed to the screen, and if the input print
C flag is set, will also print the errors to the printer.
C
C Calling sequence:
C
C     CALL X2CHKNPC(PRTFLG,ERRCNT,FAST)
C
C Input parameters:
C
C     PRTFLG      Logical     Display errors to printer
C     FAST        Logical     TRUE if check bitmap 
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
	SUBROUTINE X2CHKNPC(PRTFLG,FAST,ERRCNT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XNPC.DEF'
	INCLUDE 'INCLIB:X2XCHK.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INTEGER*4   ST                          !Read status
	INTEGER*4   REC                         !Record pointer
	INTEGER*4   I                           !Array index
	INTEGER*4   ERRCNT                      !Number of errors
	CHARACTER   X2FILNAM*20                 !File name function
	LOGICAL     PRTFLG                      !Print error flag
	LOGICAL     FAST                        !Print error flag
C
	WRITE(5,9020)
	IF(PRTFLG) WRITE(6,9020)
	ERRCNT=0
C
C OPEN THE NETWORK PORT CONFIGURATION FILE.
C
	CALL OPENX2X(X2FILNAM(XNPC),1)
C
C READ THROUGH ENTIRE FILE SKIPPING EMPTY SLOTS.
C
	REC=0
	X2XCHK_NPCCNT=0
100	CONTINUE
	  REC=REC+1
	  CALL READX2X(1,REC,X2XNPC_REC,ST)
	  IF(ST.EQ.-99) THEN
	    ERRCNT=ERRCNT+1
	    WRITE(5,9034)
	    GOTO 8000
	  ENDIF
	  IF(ST.EQ.144) GOTO 8000
	  IF(X2XNPC_REC(1).LE.0) GOTO 100
C
C IF FAST CHECK OF DATABASE, CHECK THE BITMAP TO
C DETERMINE IF THE RECORD HAS BEEN MODIFIED.
C
	  IF(FAST .AND. 
     *       X2XNPC_BITMAP.EQ.0 .AND. 
     *       X2XNPC_BITMAP2.EQ.0 .AND.
     *       X2XNPC_BITMAP3.EQ.0 .AND.
     *       X2XNPC_BITMAP4.EQ.0) GOTO 130
C
C CHECK FOR DUPLICATE ADDRESSES.
C
	  DO 120 I=1,X2XCHK_NPCCNT
	    IF(X2XNPC_ADDRES(1).EQ.X2XCHK_NPCADR(1,I) .AND.
     *	       X2XNPC_ADDRES(2).EQ.X2XCHK_NPCADR(2,I)) THEN
	      WRITE(5,9010) REC, X2XCHK_NPCREC(I)
	      IF(PRTFLG) WRITE(6,9010) REC, X2XCHK_NPCREC(I)
	      ERRCNT=ERRCNT+1
	    ENDIF
120	  CONTINUE
C
C STORE THE HOST ADDRESS.
C
130	  CONTINUE
	  X2XCHK_NPCCNT=X2XCHK_NPCCNT+1
	  X2XCHK_NPCADR(1,X2XCHK_NPCCNT)=X2XNPC_ADDRES(1)
	  X2XCHK_NPCADR(2,X2XCHK_NPCCNT)=X2XNPC_ADDRES(2)
	  X2XCHK_NPCREC(X2XCHK_NPCCNT)=REC
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
9010	FORMAT(3X,'Network port ',I,' has duplicate address',
     *	          ' with code ',I)					!V02
9020	FORMAT(1X,'Begin Edit Check of Network Port Configuration')
9030	FORMAT(1X,'End Edit Check of Network Port Configuration - 'I3,
     *	          ' error(s) encountered')
9034	FORMAT(3X,'Locked record encountered - check aborted.')
	END
