C
C SUBROUTINE X2CHKTER
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CHKTER.FOV                                 $
C  $Date::   17 Apr 1996 16:13:18                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2chkter.for;1 **
C
C X2CHKTER.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Added PRMAGT.DEF, rearranged AGTINF.DEF for Finland
C
C V04 19-DEC-94 GPR Integrate UK changes into X2X Baseline
C V03 15-AUG-94 GPR Speed up check of terminal file
C V02 09-DEC-91 RRB REMOVE CHECK OF COMM TYPE (NOW STATION CLASS)
C V01 01-AUG-90 XXX RELEASED FOR VAX
C V01 30-OCT-89 MRM INITIAL RELEASE
C
C This subroutine will perform edit checks on the
C Terminal and Station Port Configuration.  Any errors encountered
C will be displayed to the screen, and if the input print
C flag is set, will also print the errors to the printer.
C
C Calling sequence:
C
C     CALL X2CHKTER(PRTFLG,FAST,ERRCNT)
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
	SUBROUTINE X2CHKTER(PRTFLG,FAST,ERRCNT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XTER.DEF'
	INCLUDE 'INCLIB:X2XSPC.DEF'
	INCLUDE 'INCLIB:X2XCHK.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:prmAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*4   ST                          !Read status
	INTEGER*4   REC				!Record pointer
	INTEGER*4   J                           !Array index
	INTEGER*4   ERRCNT                      !Number of errors
	INTEGER*4   EOFCNT                      !End of file count
	INTEGER*4   CHK_INDEX			!Index for chk spc array    !V04
	CHARACTER   X2FILNAM*20                 !File name function
	LOGICAL     PRTFLG                      !Print error flag
	LOGICAL	    FAST			!Fast editcheck flag
	LOGICAL	    DONE			!Done reading		    !V03
C
	WRITE(5,9020) IAM()
	IF(PRTFLG) WRITE(6,9020) IAM()
	ERRCNT=0
	EOFCNT=0
	CALL FASTSET(0,X2XCHK_SPC,X2X_STATIONS*X2X_MAXPORT)
C
C OPEN THE TERMINAL CONFIGURATION FILE FOR BUFFERED I/O.
C
	CALL OPENX2X(X2FILNAM(XTER),1)
C
C OPEN THE STATION PORT CONFIGURATION FILE. (BUFFERED I/O)
C
	CALL OPENX2X(X2FILNAM(XSPC),4)
C
C	***** Start V03 changes *****
C	
C	Read the entire Station Port file up front
C
	REC=1
	DONE=.FALSE.
	DO WHILE ((.NOT.DONE).AND.(REC.LE.X2XCHK_SPC_MAX))		    !V04
	  CALL READX2X(4,REC,X2XSPC_REC,ST)
	  IF(ST.EQ.-99) THEN
	    WRITE(5,9080) IAM()
	    ERRCNT=ERRCNT+1
	    DONE=.TRUE.
	  ELSEIF (ST.EQ.144) THEN
	    DONE=.TRUE.
	  ELSE
	    X2XCHK_SPC_PORT(REC)=X2XSPC_PORT
	    DO J=1,X2X_MAXTERMS
	      X2XCHK_SPC_DROPS(REC,J)=X2XSPC_DROPS(J)
	    ENDDO
	    REC=REC+1
	  ENDIF
	ENDDO
	CALL CLOSX2X(4)

	IF(ST.NE.0) GOTO 8000
C
C	***** End V03 changes *****
C	
C
C READ THROUGH TERMINAL FILE SKIPPING EMPTY SLOTS.
C
	CALL FASTSET(0,X2XCHK_TER,X2X_TERMS)		!V03
	REC=0
100	CONTINUE
	  REC=REC+1
	  CALL READX2X(1,REC,X2XTER_REC,ST)
	  IF(ST.EQ.-99) THEN
	    ERRCNT=ERRCNT+1
	    WRITE(5,9080) IAM()
	    GOTO 8000
	  ENDIF
	  IF(ST.EQ.144) GOTO 8000
C
C SKIP EMPTY RECORDS.
C
	  IF(X2XTER_REC(1).LE.0) THEN
	    EOFCNT=EOFCNT+1
	    GOTO 100
C         IF(EOFCNT.LT.100) GOTO 100
C         GOTO 8000
	  ELSE
	    EOFCNT=0
	  ENDIF
C
C CHECK TO MAKE SURE THAT THE TERMINAL EXISTS IN THE
C STATION PORT CONFIGURATION FILE.
C
	  CHK_INDEX=X2XTER_STN*X2X_MAXPORT+(X2XTER_PORT-1)		    !V04
	  IF(X2XCHK_SPC_PORT(CHK_INDEX).EQ.X2XTER_PORT) THEN		    !V04
	    X2XCHK_SPC(X2XTER_STN,X2XTER_PORT) =
     *	      X2XCHK_SPC(X2XTER_STN,X2XTER_PORT) + 1
	  ELSE
	    WRITE(5,9000) IAM(), REC,X2XTER_STN, X2XTER_PORT
	    IF(PRTFLG) WRITE(5,9000) IAM(), REC, X2XTER_STN,
     *         X2XTER_PORT
	    ERRCNT=ERRCNT+1
	  ENDIF
C
C IF FAST CHECK OF DATABASE, CHECK THE BITMAP TO
C DETERMINE IF THE RECORD HAS BEEN MODIFIED.
C
	  IF(FAST .AND. 
     *       X2XTER_BITMAP.EQ.0 .AND. 
     *       X2XTER_BITMAP2.EQ.0 .AND.
     *       X2XTER_BITMAP3.EQ.0 .AND.
     *       X2XTER_BITMAP4.EQ.0) GOTO 100

	  X2XCHK_TER(REC)=.TRUE.			!V03
C
C CHECK TO MAKE SURE THE DROP IS DEFINED.
C
110	  CONTINUE
	  CHK_INDEX=X2XTER_STN*X2X_MAXPORT+(X2XTER_PORT-1)		    !V04
	  DO 200 J=1,X2X_MAXTERMS
	    IF(X2XCHK_SPC_DROPS(CHK_INDEX,J).EQ.
     *	       X2XTER_DROP) GOTO 210
200	  CONTINUE
	  WRITE(5,9040) IAM(), REC, X2XTER_DROP
	  IF(PRTFLG) WRITE(5,9040) IAM(), REC,X2XTER_DROP
	  ERRCNT=ERRCNT+1
C
C READ THE NEXT RECORD.
C
210	  CONTINUE
	  GOTO 100
C
C PROGRAM EXIT.
C
8000	CONTINUE

	CALL CLOSX2X(1)
	WRITE(5,9030) IAM(), ERRCNT
	IF(PRTFLG) WRITE(6,9030) IAM(), ERRCNT
	RETURN
C
C     =================== Format Statements ==================
C
9000	FORMAT(1X,A,'Term # ',I7,' not configured on station ',I5,
     *	          ' and port ',I3)
9020	FORMAT(1X,A,'Begin Edit Check of Terminal Configuration')
9030	FORMAT(1X,A,'End Edit Check of Terminal Configuration - 'I4,
     *	          ' error(s) encountered')
9040	FORMAT(1X,A,'Term # ',I7,' invalid drop address ',A2)
9080	FORMAT(1X,A,'Locked record encounted - check aborted ')
	END
