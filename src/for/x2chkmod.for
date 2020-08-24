C
C SUBROUTINE X2CHKMOD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CHKMOD.FOV                                 $
C  $Date::   17 Apr 1996 16:12:38                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2chkmod.for;1 **
C
C X2CHKMOD.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This module contains the subroutines which are utilized
C to determine whether a file has been modified, and whether
C an edit check is required.
C
C Modules contained:
C
C     CALL X2CHKMOD(FILE,FLAG)
C     CALL X2CHKEXT(FLAG)
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
	SUBROUTINE X2CHKMOD(FILE,FLAG)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INTEGER*4   FILE                !Modified file
	INTEGER*4   FLAG                !Modification flag
	INTEGER*4   X2XFILES(128)       !List of X2X files
	INTEGER*4   OSERR               !Error code
	INTEGER*4   INDX                !File index
	INTEGER*4   FDB(7)		!File decriptor block
	CHARACTER   X2FILNAM*20		!File name functions
	DATA	    FDB /7*0/
C
C ATTEMPT TO OPEN THE EXISTING FILE.
C
	CALL OPENX(8,X2FILNAM(XGBL),4,0,0,OSERR)
	CALL IOINIT(FDB,8,2*256)
	IF(OSERR.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XGBL),'OPEN',OSERR,0)
	  CALL GPAUSE
	  GOTO 8000
	ENDIF
C
C READ THE RECORD.
C
	CALL READW(FDB,50,X2XFILES,OSERR)
	IF(OSERR.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XGBL),'READ',OSERR,1)
	  CALL GPAUSE
	  GOTO 8000
	ENDIF
C
C UPDATE THE MODIFIED FILES FLAG AND REWRITE THE RECORD.
C
	INDX=FILE-XGBL+1
	IF(INDX.LT.1 .OR.INDX.GT.20) THEN
	  WRITE(5,*) 'INVALID FILE INDEX:  ',FILE
	  CALL GPAUSE
	  GOTO 8000
	ENDIF
	X2XFILES(INDX)=FLAG
C
	CALL WRITEW(FDB,50,X2XFILES,OSERR)
	IF(OSERR.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XGBL),'WRITE',OSERR,1)
	  CALL GPAUSE
	  GOTO 8000
	ENDIF
C
C PROGRAM EXIT
C
8000	CONTINUE
C	CALL USRCLOS1(8)
	RETURN
	END
