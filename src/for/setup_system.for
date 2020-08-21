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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	PROGRAM SETUP_SYSTEM
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'

	COMMON/SCF/ SCFREC

	INTEGER*4 FDB(7), ST
	INTEGER*4 SCF_FILE(5)/'SCF.','FIL ',3*'    '/

	CALL CRTFIL(SCF_FILE, SCFSEC/2, ST)
	IF(ST.NE.0) CALL GPAUSE

	CALL OPENW(11,SCF_FILE,4,0,0,ST)
	CALL IOINIT(FDB,11,SCFSEC*256) 
	IF(ST.NE.0) CALL FILERR(SCF_FILE,1,ST,0)

	CALL READW(FDB,1,SCFREC,ST)
	IF(ST.NE.0) CALL FILERR(SCF_FILE,2,ST,1)
C
C SETUP P() PARAMETERS
C
	CALL SETPARAMS()
C
C SETUP SYSTEM FILE NAMES.
C
	CALL SETFILES()
	
	CALL WRITEW(FDB,1,SCFREC,ST)
	IF(ST.NE.0) CALL FILERR(SCF_FILE,3,ST,1)

	CALL CLOSEFIL(FDB)
	END
