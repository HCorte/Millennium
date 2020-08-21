C
C SUBROUTINE X2SPCLIS
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2SPCLIS.FOV                                 $
C  $Date::   17 Apr 1996 16:35:10                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2spclis.for;1 **
C
C X2SPCLIS.FOR
C
C V03 07-FEB-94 GPR USE I5 FORMAT FOR STATION AND TERMINAL TYPE-OUTS
C V02 07-FEB-92 DAS DISPLAY OF DROPS/TER # DID NOT MATCH TERMINAL COUNT
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This subroutine will display defined station ports
C stored in the X2XSPC file to the screen.  The user
C will have the option of displaying a specific code,
C or to page forward or back.
C
C Calling sequence:
C
C     CALL X2SPCLIS
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
	SUBROUTINE X2SPCLIS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XSPC.DEF'
	INCLUDE 'INCLIB:X2XLIS.DEF'
C
	INTEGER*4   SPC,SPCCNT          !Rec #/station count
	INTEGER*4   LASTSPC             !Last station displayed
	INTEGER*4   EOFCNT              !End of file count
	INTEGER*4   I,J,ST              !Work variables
	INTEGER*4   STN,PORT            !Station/port
	INTEGER*4   CNT,OFFSET          !Term/drop display
	INTEGER*4   PREV, LAST_PREV
	CHARACTER   PROMPT*65           !Output prompt
	CHARACTER   OPT*4               !Input option
	LOGICAL     MORE                !OK to display more
C
C LOAD THE X2XCHK ARRAYS WITH ALL DEFINED TERMINALS.
C
	CALL CLRSCR(5)
	WRITE(5,9080)
	WRITE(5,9110)
	CALL X2GETTER
C
C DISPLAY THE NEXT SCREEN OF PORTS.
C
	SPC=0
	LASTSPC=0
	LAST_PREV=0
	PREV=0
100	CONTINUE
	IF(SPC.LT.0) SPC=0
	EOFCNT=0
	SPCCNT=0
	CALL CLRSCR(5)
	WRITE(5,9080)
	WRITE(5,9090)
C
C READ A STATION FROM THE FILE AND DISPLAY
C IT TO THE SCREEN.
C
200	CONTINUE
	IF(SPCCNT.LT.10 .AND. EOFCNT.LE.100) THEN
250	  SPC=SPC+1
	  CALL READX2X(4,SPC,X2XSPC_REC,ST)
	  IF(ST.EQ.144) GOTO 300
C
	  IF(X2XSPC_REC(1).LE.0) THEN
	    GOTO 250
	  ENDIF
C
C DISPLAY THE INFORMATION TO THE SCREEN.
C
	  SPCCNT=SPCCNT+1
	  IF(SPCCNT.EQ.1) THEN
	    LAST_PREV=PREV
	    PREV=SPC
	  ENDIF
	  LASTSPC=SPC
	  STN=X2XSPC_STN
	  PORT=X2XSPC_PORT
	  CNT=X2XLIS_TERMS(0,PORT,STN)
	  IF(CNT.GT.4) CNT=4
	  WRITE(5,9100) X2XSPC_STN,    X2XSPC_PORT,
     *	                X2XSPC_TERCNT,
     *	               (X2XLIS_DROPS(I,PORT,STN),
     *	                X2XLIS_TERMS(I,PORT,STN),I=1,CNT)
C
	  DO 260 J=1,X2X_MAXTERMS/4+1
	    IF(X2XLIS_TERMS(0,PORT,STN).GT.J*4) THEN
	      OFFSET=J*4+1
	      CNT=X2XLIS_TERMS(0,PORT,STN)-OFFSET+1
	      IF(CNT.GT.4) CNT=4
	      WRITE(5,9105) (X2XLIS_DROPS(I,PORT,STN),
     *	                     X2XLIS_TERMS(I,PORT,STN),
     *	                     I=OFFSET,OFFSET+CNT-1)
	      SPCCNT=SPCCNT+1
	    ENDIF
260	  CONTINUE
	  GOTO 200
	ENDIF
C
C IF NO CODE EXIST, DISPLAY AN APPROPRIATE MESSAGE.
C
300	CONTINUE
	IF(SPCCNT.EQ.0) THEN
	  WRITE(5,9050)
	ENDIF
C
C BUILD THE APPROPRIATE OUTPUT PROMPT.
C
	IF(EOFCNT.LE.100) THEN
	  WRITE (PROMPT,9000)
	  MORE=.TRUE.
	ELSE
	  WRITE (PROMPT,9010)
	  MORE=.FALSE.
	ENDIF
C
C ASK USER WHAT THEY WANT TO DO.
C
	WRITE(5,*)
350	CONTINUE
	CALL WIMG(5,PROMPT)
	READ(5,9020) OPT
	IF(OPT(1:1).EQ.'E'.OR.OPT(1:1).EQ.'e') THEN
	  GOTO 8000
	ELSE IF(MORE.AND.(OPT(1:1).EQ.'N'.OR.OPT(1:1).EQ.'n')) THEN
	  SPC=LASTSPC
	  GOTO 100
	ELSE IF(OPT(1:1).EQ.'P'.OR.OPT(1:1).EQ.'p') THEN
	  SPC=LAST_PREV-1
	  GOTO 100
	ELSE
	  WRITE(5,9040)
	  GOTO 350
	ENDIF
C
C PROGRAM EXIT
C
8000	CONTINUE
	RETURN
C
C     ==================== Format Statements ===================
C
9000	FORMAT(15(' '),'Enter N for next, P for Previous ')
9010	FORMAT(15(' '),'Enter P for previous             ')
9020	FORMAT(A4)
9040	FORMAT(15(' '),'Invalid option input ')
9050	FORMAT(15(' '),'Sorry, no more stations exist ')
9080	FORMAT(//,T26,'GTECH Distributed Network',/,
     *	          T28,'Station Port Listing',/)
9090	FORMAT(T3,'Station',T12,'Port',T19,'# Terms',
     *	       T29,4('Drop  Term',2X),/,T3,72('='))			! V03
9100	FORMAT(T2,I5,T13,I2,T20,I5,T31,4(A2,1X,I5,4X))			! V03
9105	FORMAT(T31,4(A2,1X,I5,4X))
9110	FORMAT(/////,15(' '),'Loading Terminal Information ... ')
	END
