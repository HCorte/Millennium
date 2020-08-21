C
C SUBROUTINE DN_AP_STATUS_AST
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_AP_STATUS_AST.FOV                         $
C  $Date::   17 Apr 1996 12:56:38                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - dn_ap_status.for;1 **
C
C
C DN_AP_STATUS_AST.FOR
C
C V01 17-APR-91 JWE Initial release
C
C This routine stuffs the STATUS into the DECNET buffer header so it can be
C returned
C This version is for ASTs.
C
C INPUT
C	BUFFER DCNPRO buffer as declared in DN_LINK
C	STATUS a status to place in header of buffer
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
	SUBROUTINE DN_AP_STATUS_AST(BUFFER,STATUS)
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DN_LINK.DEF'
	INCLUDE 'INCLIB:DN_BLOCK.DEF'
C
	RECORD/DN_BUFFER_STRUCT/BUFFER
	INTEGER*4 STATUS
C
	BUFFER.AP_STATUS=STATUS	
C
	RETURN
	END
