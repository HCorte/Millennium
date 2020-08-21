C
C V01 18-OCT-98 UXN INITIAL RELEASE.
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	PROGRAM LODSPTSYS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:RECSSF.DEF'
C
	EQUIVALENCE (SSFREC,SPSATR)
	INTEGER*4   ST,FDB(7)
C
        TYPE*,IAM(),'Loading Sports system file '
        CALL OPENQW(3,SFNAMES(1,SSF),4,0,0,ST)
        CALL IOQINIT(FDB,3,1*256)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,SSF),1,ST,0)
        CALL READQIO(FDB,1,SSFREC,SSFSEC*256,ST)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,SSF),2,ST,1)
        CALL CLOSEQFIL(FDB)
	TYPE*,IAM(),'Done.'
	CALL GSTOP(GEXIT_SUCCESS)
	END
