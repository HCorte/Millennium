C
C V01 14-NOV-2000 UXN Initial release.
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
	PROGRAM GUI_RELOAD
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
C
	INTEGER*4 YESNO
C
	CALL COPYRITE
C
	CALL INPYESNO('Are you sure you want to realod GUIWORKers [Y/N]',YESNO)
	IF(YESNO.EQ.1) THEN
	   GUI_WORKER_RELOAD = 1
	ENDIF
C
	CALL GSTOP(GEXIT_SUCCESS)
C
	END
