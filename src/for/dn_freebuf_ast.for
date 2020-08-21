C
C SUBROUTINE DN_FREEBUF_AST
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_FREEBUF_AST.FOV                           $
C  $Date::   17 Apr 1996 12:57:56                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C  
C *** Pre-Baseline Source - chkq.for ***
C
C V01 11-SEP-90 MRM RELEASED FOR VAX
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose: Will check any queue if data is on the list
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE DN_FREEBUF_AST(BUF, TIMES)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
        INCLUDE 'INCLIB:DESNET.DEF'
        INCLUDE 'INCLIB:DN_LINK.DEF'			! DECNET STRUCTURES
        INCLUDE 'INCLIB:DN_BLOCK.DEF'			! DECNET DATA BLOCKS
C
C LOCAL DECLARATIONS
C
	INTEGER*4	BUF,
     *			ST,
     *			TIMES
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Check queue corruption
C
	CALL CHKQCOR(DCN_FREE, BUF, TIMES)
	CALL ABL_AST(BUF, DCN_FREE, ST)
C
	END
