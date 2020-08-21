C
C SUBROUTINE DN_GETCOMMAND
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_GETCOMMAND.FOV                            $
C  $Date::   17 Apr 1996 12:58:00                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - dn_dcnpro.for **
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
	SUBROUTINE DN_GETCOMMAND(BUFFER, COMMAND)
	IMPLICIT NONE

C This routine will get the command field from the buffer passed as the first
C argument and return it in the second arguement as a longword.

C Include files

        INCLUDE 'INCLIB:DN_LINK.DEF' 	!DECnet Structures

C Constant Declarations

C Structure Declarations

C Variable Declarations

	INTEGER*4	COMMAND		!Command holder..
	RECORD /DN_BUFFER_STRUCT/ BUFFER!Buffer passed as parameter

C Begin Code

	COMMAND = BUFFER.COMMAND        !Copy the field...
	RETURN
	END
