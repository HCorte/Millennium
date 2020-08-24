C
C SUBROUTINE VMS_ERROR_OPS
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]VMS_ERROR_OPS.FOV                            $
C  $Date::   17 Apr 1996 15:55:30                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vms_error_ops.for;1 **
C
C VMS_ERROR_OPS.FOR
C
C V01 11-MAY-92 JWE Call ops with a text string describing a VMS return
C		    code.
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
	SUBROUTINE VMS_ERROR_OPS(STATUS)
	IMPLICIT NONE
C
	INTEGER*4   STATUS
C
	INTEGER*2   ERROR_LENGTH
	CHARACTER   ERROR_TEXT*256
C
	CALL SYS$GETMSG(%VAL(STATUS), ERROR_LENGTH, ERROR_TEXT,,)
	CALL OPS(ERROR_TEXT, STATUS, STATUS)
C
	RETURN
	END
