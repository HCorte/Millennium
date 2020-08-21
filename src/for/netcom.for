C
C NETCOM.FOR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NETCOM.FOV                                   $
C  $Date::   17 Apr 1996 14:09:48                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C V01 19-APR-91 JWE ADD DECNET COMMON
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX RELEASED FOR SWEDEN
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
	BLOCK DATA  NETCOMM
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
C
C NAME: NETCOM.FTN
C
C FUNC: NETWORK COMMON
C
C
C
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DN_LINK.DEF'
	INCLUDE 'INCLIB:DN_BLOCK.DEF'
	END
