C
C DLLREAD.FOR
C
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DLLREAD.FOV                                  $
C  $Date::   17 Apr 1996 12:55:38                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C
C V01 15-sep-96 xxx Initial release
C
C This program will act as the main menu for updating the
C X.2X parameters for the GTECH Distributed Network.
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C	READ DOWNLOAD INTO MESSAGE COMMON
C
	PROGRAM DLLREAD
	IMPLICIT NONE
C
C
	CALL COPYRITE

	CALL LOADSMF(3)
	TYPE *,'Message common updated with download from SMF file'
C
	STOP 
	END
