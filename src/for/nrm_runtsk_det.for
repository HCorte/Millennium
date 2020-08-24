C
C SUBROUTINE RUNTSK_DET
C
C V01 24-JUL-2000 UXN Initial release (produced from NRM_RUNTSK.FOR)
C
C RUNTSK_DET(NAME) -
C SUBROUTINE TO LOAD, START AND WAIT FOR TASK COMPLETION
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RUNTSK,RUNTASK -
C	ACTIVATE THE DETACHED PROCESS UNDER THE NAME OF THE PROGRAM
C	WITH THE PROJECT PREFIX AND WAIT FOR COMPLETEION
C--------------------------------------------------------------------
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE RUNTSK_DET(NAME)
	IMPLICIT NONE
C
	BYTE	NAME(8)
	LOGICAL WFLG
C
	ENTRY RUNTASK_DET(NAME)
	WFLG=.TRUE.
C
	CALL XRUNTSK_DET(NAME,NAME,WFLG)
	RETURN
	END
