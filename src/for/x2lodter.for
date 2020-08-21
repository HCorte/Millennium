C
C SUBROUTINE X2LODTER
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2LODTER.FOV                                 $
C  $Date::   17 Apr 1996 16:22:16                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2lodter.for;1 **
C
C X2LODTER.FOR
C
C V02 31-Jul-95 DAS Added call to X2CNVDRP
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will load the GTECH Distributed Network
C common from the Terminal Configuration files.
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
	SUBROUTINE X2LODTER
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XTER.DEF'
C
	INTEGER*4   TER             !Station/terminal pointers
	INTEGER*4   ST              !Status
	INTEGER*4   TERCNT          !Terminal count
	INTEGER*4   INDX
	CHARACTER   X2FILNAM*20     !File name function
C
C OPEN THE TERMINAL CONFIGURATION FILE.
C
	TERCNT=0
	CALL OPENLOD(X2FILNAM(XTER),1)
C
C PROCESS ALL TERMINALS CONFIGURED FOR EACH STATION.
C
	TER=0
C***  WRITE(5,9000)
100	CONTINUE
	  TER=TER+1
	  IF(TER.GT.X2X_TERMS) GOTO 8000
	  CALL READLOD(1,TER,X2XTER_REC,ST)
	  IF(ST.EQ.144) GOTO 8000
	  IF(X2XTER_REC(1).LE.0) GOTO 100
          IF(X2XTER_STN.LE.0.OR.X2XTER_STN.GT.X2X_STATIONS)GOTO 100
          IF(X2XTER_PORT.LE.0) GOTO 100
C
C UPDATE COMMON WITH THE TERMINAL INFORMATION.
C
	  X2XS_NUM_TERMS(X2XTER_PORT,X2XTER_STN) =
     *	    X2XS_NUM_TERMS(X2XTER_PORT,X2XTER_STN) + 1
          CALL X2CNVDRP(X2XTER_DROP,INDX)                       !...v02
	  IF(INDX.LE.0) THEN                                    !...V02
	    TYPE *,'*** X2LODTER: INVALID DROP  - TERMINAL ',TER
	    GOTO 100
	  ENDIF
	  X2XS_TERMS(INDX,X2XTER_PORT,X2XTER_STN)=X2XTER_TER
C
	  X2XT_STATION_NO(TER)=X2XTER_STN
	  X2XT_DROP_AD(TER)=X2XTER_DROP
	  CALL ISBYTE(X2XTER_STATE,IX2XT_STATE,TER-1)
	  CALL ISBYTE(X2XTER_PORT,IX2XT_STATION_PORT,TER-1)
	  TERCNT=TERCNT+1
	  GOTO 100
C
C CLOSE THE FILE AND RETURN.
C
8000	CONTINUE
	CALL CLOSLOD(1)
	RETURN
C
C     ==================== Format Statements =================
C
	END
