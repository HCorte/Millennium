C
C FUNCTION X2FILNAM
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2FILNAM.FOV                                 $
C  $Date::   17 Apr 1996 16:16:58                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2filnam.for;1 **
C
C X2FILNAM.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This function will return the character name of the
C input index (based on GLOBAL.DEF).
C
C Input parameters:
C
C     INDEX       Int*4       File index number
C
C Output parameters:
C
C     X2FILNAM    Char*20     Char file name
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
	CHARACTER*20 FUNCTION X2FILNAM(INDEX)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
C
	INTEGER*4   I, ST, INDEX
	INTEGER*4   FDB(7)              !File descriptor block
	INTEGER*4   SCFNAM(5)           !System control file name
	LOGICAL     LOADED /.FALSE./    !Record loaded flag
	DATA        SCFNAM/'SCF.','FIL ',3*'    '/
C
C CHECK FOR A VALID INDEX.
C
	IF(INDEX.LE.0 .OR. INDEX.GT.MAXFIL) THEN
	  TYPE *,'INVALID FILE INDEX:  ',INDEX
	  CALL GPAUSE
	  GOTO 8000
	ENDIF
C
C OPEN THE SYSTEM CONTROL FILE NAME.
C
	IF(.NOT.LOADED) THEN
	  CALL OPENW(9,SCFNAM,4,0,0,ST)
	  CALL IOINIT(FDB,9,SCFSEC*256)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,'SCF.FIL','OPENX',ST,0)
	    CALL GPAUSE
	  ENDIF
C
C READ THE CONTROL RECORD.
C
	  CALL READW(FDB,1,SCFREC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,'SCF.FIL','READW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  LOADED=.TRUE.
	ENDIF
C
C STORE THE FILE NAME.
C
	WRITE (X2FILNAM,'(5(A4))') (SCFSFN(I,INDEX),I=1,5)
	IF(SCFSFN(1,INDEX).EQ.'    ') THEN
	  CALL SYSVOL(SCFSFN(1,INDEX))
	  WRITE (X2FILNAM,'(5(A4))') (SCFSFN(I,INDEX),I=1,5)
	ENDIF
C
C PROGRAM EXIT.
C
8000	CONTINUE
	CALL CLOSEFIL(FDB)
	RETURN
	END
