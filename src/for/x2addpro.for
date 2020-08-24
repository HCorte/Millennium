C
C SUBROUTINE X2ADDPRO
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2ADDPRO.FOV                                 $
C  $Date::   17 Apr 1996 16:07:10                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2quemgr.for;1 **
C
C X2QUEMGR.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C     X2QUEMGR.FTN
C
C     ADD AND REMOVE TO/FROM X2X SUBSYSTEM "OUTPUT" QUEUE
C
C     ENTRIES:
C
C     X2ADDPRO(PROBUF)         ;ADD PROCOM BUFFER TO BOTTO OF Q
C     X2GRBPRO(PROBUF)         ;REMOVE FROM TOP OF Q
C
C     X2ADDPRO(PROBUF)
C     IN:
C     PROBUF   - PROCOM BUFFER
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
	SUBROUTINE X2ADDPRO(PROBUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:X2XQUE.DEF'
C
	INTEGER*4 ST, PROBUF
C
	CALL ABL(PROBUF,X2X_OUTPUT,ST)
	RETURN
	END
