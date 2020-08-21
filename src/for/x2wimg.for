C
C  GXSRC:X2WIMG.FOR
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2WIMG.FOV                                   $
C  $Date::   17 Apr 1996 16:41:34                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C V01 23-MAR-93 XXX RELEASED FOR VAX
C
C This will output a string without a carriage return and no call to IAM()
C
C The internal name of this routine is still WIMG. The reason for this
C is that other routines call WIMG and we want them to behave similiarly.
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
	SUBROUTINE WIMG(LUN, STRING)
	IMPLICIT NONE
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4   LUN
	CHARACTER   STRING*(*)
C
C
	WRITE(LUN, 1001)STRING
1001	FORMAT(' ',A,' >',$)
C
	RETURN
	END
