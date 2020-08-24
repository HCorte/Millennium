C
C SUBROUTINE X2RGETBF
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RGETBF.FOV                                 $
C  $Date::   17 Apr 1996 16:31:26                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2xrel.for **
C
C 
C X2RGETBF.FOR 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C 
C     SUBROUTINE:
C        X2RGETBF(BUF,STATUS) 
C
C     PURPOSE:
C        RETRIEVE A BUFFER FROM THE RELAY INPUT QUEUE
C
C     INPUT:
C       BUF      -     STATION TO BE STARTED
C
C     OUTPUT:
C       STATUS       -     0 EVERY THING OKAY
C              
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
	SUBROUTINE X2RGETBF(BUF,STATUS)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
C 
	INTEGER*4 STATUS, BUF
C
	STATUS = 0
	CALL RTL(BUF,X2XR_INPUT_QUEUE,STATUS)
	IF (STATUS.NE.2) STATUS = 0
	RETURN
	END
