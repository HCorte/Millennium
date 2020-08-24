C
C V01 08-JUN-1999 UXN INITIAL RELEASE.
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	SUBROUTINE CREATE_FILE(NAME,SIZE,STATUS)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 NAME(5),SIZE,STATUS
C
C TRY TO CREATE CONTIGUOUS FILE
C
	CALL CRTFIL(NAME,SIZE,STATUS)
	IF(STATUS.NE.0) THEN
	   TYPE*,IAM()
	   TYPE*,IAM()
	   TYPE*,IAM(),'*************************************'
	   TYPE*,IAM()
	   TYPE*,IAM(),'Not enough contiguous space !!!!'
	   TYPE*,IAM()
	   TYPE*,IAM(),'Trying to create non-contiguous file....'
	   TYPE*,IAM()
	   TYPE*,IAM(),'*************************************'
	   TYPE*,IAM()
	   TYPE*,IAM()
	   CALL CRTFIL_NCNTG(NAME,SIZE,STATUS)
	ENDIF
	END
