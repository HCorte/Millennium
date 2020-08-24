C
C SUBROUTINE X2GETTER
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2GETTER.FOV                                 $
C  $Date::   17 Apr 1996 16:19:36                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2getter.for;1 **
C
C X2GETTER.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This routine will read the X2XTER file to obtain all terminals
C and drops defined.  The information is stored in arrays contained
C in X2XLIS.DEF.
C
C Calling sequence:
C
C     CALL X2GETTER
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
	SUBROUTINE X2GETTER
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XTER.DEF'
	INCLUDE 'INCLIB:X2XLIS.DEF'
C
	INTEGER*4   REC         !Record pointer
	INTEGER*4   ST          !Status
	INTEGER*4   INDX        !Array index
	CHARACTER   X2FILNAM*20 !File name function
C
C CLEAR THE TERMS AND DROPS ARRAYS.
C
	CALL FASTSET(0,X2XLIS_TERMS,(X2X_MAXTERMS+1)*X2X_MAXPORT*
     *	                             X2X_STATIONS)
	CALL FASTSET(0,IX2XLIS_DROPS,X2X_MAXTERMS*X2X_MAXPORT*
     *	                             X2X_STATIONS/4*2)
	REC=0
C
C OPEN THE TERMINAL CONFIGURATION FILE.
C
	CALL OPENX2X(X2FILNAM(XTER),3)
C
C READ ALL RECORDS IN THE TERMINAL CONFIGURATION FILE.
C
100	CONTINUE
	REC=REC+1
	CALL READX2X(3,REC,X2XTER_REC,ST)
	IF(ST.EQ.144) GOTO 8000
C
C CHECK FOR UNUSED SLOTS.
C
	IF(X2XTER_REC(1).LE.0) GOTO 100
C
C STORE INFORMATION INTO ARRAYS.
C
        IF(X2XTER_STN.LE.0.OR.X2XTER_STN.GT.X2X_STATIONS)GOTO 100
        IF(X2XTER_PORT.LE.0) GOTO 100
C
	X2XLIS_TERMS(0,X2XTER_PORT,X2XTER_STN)=
     *	  X2XLIS_TERMS(0,X2XTER_PORT,X2XTER_STN)+1
	INDX=X2XLIS_TERMS(0,X2XTER_PORT,X2XTER_STN)
	X2XLIS_TERMS(INDX,X2XTER_PORT,X2XTER_STN)=REC
	X2XLIS_DROPS(INDX,X2XTER_PORT,X2XTER_STN)=X2XTER_DROP
	GOTO 100
C
C PROGRAM EXIT
C
8000	CONTINUE
	CALL CLOSX2X(3)
	RETURN
	END
