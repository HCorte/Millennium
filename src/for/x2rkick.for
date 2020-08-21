C
C SUBROUTINE X2RKICK
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RKICK.FOV                                  $
C  $Date::   17 Apr 1996 16:31:38                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2xrel.for **
C
C 
C X2RKICK.FOR
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C     X2RKICK
C       PURPOSE:
C         KICKS RELAY APPLICATION TASK IF SOMETHING IS ON THE QUEUE
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
C
	SUBROUTINE X2RKICK
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
        INTEGER*4 QUECNT             
	INTEGER*4 ST, PROCESS
C
C       CHECK ALL RELAY APPLICATIONS IF SOMETHING ON THE QUEUE KICKIT 
C 
	DO 10 PROCESS = 1,X2X_RELAY_APPS
	   IF (QUECNT(X2XR_APP_QUEUE(1,PROCESS)).NE.0)
     *          CALL RELSE(X2XR_APP_TASK(1,PROCESS),ST)
10	CONTINUE
	RETURN
	END
