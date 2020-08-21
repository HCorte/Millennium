C
C SUBROUTINE X2CHKEXT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CHKEXT.FOV                                 $
C  $Date::   17 Apr 1996 16:12:08                                         $
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
C
C
C =====================================================
C X2CHKEXIT
C
C This routine will read the X2XCHK.FIL, and determine if
C any of the files have been modified.  If they have,
C the return flag is set to -1.
C
C Output parameters:
C
C     FLAG    Int*4       if=0 no modification, if.NE.0 file modified
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
	SUBROUTINE X2CHKEXT(FLAG)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INTEGER*4   FLAG            !Exit flag
	INTEGER*4   OSERR           !Error code
	INTEGER*4   X2XFILES(128)   !Modified files array
	INTEGER*4   I               !Array index
	INTEGER*4   FDB(7) /7*0/    !File descriptor block
	CHARACTER   X2FILNAM*20	    !File name functions
C
C ATTEMPT TO OPEN THE EXISTING FILE.
C
	FLAG=0
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
C CHECK ALL INDICES TO SEE IF THEY HAVE BEEN SET.
C
	DO 100 I=1,20
	  IF(X2XFILES(I).NE.0) FLAG=I+XGBL-1
100	CONTINUE
C
C PROGRAM EXIT.
C
8000	CONTINUE
C	CALL USRCLOS1(8)
	RETURN
	END
