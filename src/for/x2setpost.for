C
C  X2SETPOST.FOR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2SETPOST.FOV                                $
C  $Date::   17 Apr 1996 16:34:14                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C  GXSRC:X2SETPOST.FOR
C
C V01 13-DEC-94 GPR RELEASED FOR UK
C
C Subroutine to to initialize x2post queue 
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
	SUBROUTINE X2SETPOST
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GLIST.DEF'
	INCLUDE 'INCLIB:X2XPOST.DEF'
C
	INTEGER*4 FILE		!FILE NO TO BE UPDATED

	DO 100, FILE=1,X2X_MAX_X2X_FILES
	    CALL DEFLST(X2POST_QUE(1,FILE),X2POST_QUEUE_ENTRIES)
100	CONTINUE

	X2POST_COMMON_STATUS=X2POST_COMMONS_ONLINE
C
C
	RETURN
	END
