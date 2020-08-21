C
C *** PROGRAM MSCTEST ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSCTEST.FOV                                  $
C  $Date::   17 Apr 1996 14:06:58                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - msctest.for ***
C
C V01 10-DEC-93 RRB VAX INITIAL RELEASE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	SET DIAGNOSTIC FLAG FOR ON-LINE DEBUGGING OF MSCMGR.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	PROGRAM MSCTEST
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:MSCCOM.DEF'
C
	INTEGER*4 EXT
C
	CALL COPYRITE
C
	CALL INPNUM('Enter 0=DISABLE, 1=ENABLE ',
     *	             MSC_TEST,0,1,EXT)
	STOP
	END
