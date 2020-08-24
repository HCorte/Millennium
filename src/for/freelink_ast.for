C
C SUBROUTINE FREELINK_AST
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]FREELINK_AST.FOV                             $
C  $Date::   17 Apr 1996 13:13:12                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - dn_mailbox_ast.for ***
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
C	ROUTINE TO CLEAR OUT AND DEALLOCATE A LINK BLOCK SO IT CAN BE USED FOR
C	ANOTHER LINK. WE SET THE STATE TO STATE_DOWN AND THE CHANNEL TO ZERO.
C	THE PASSED PARAMETER IS THE INDEX INTO THE LINK STRUCTURE ARRAY.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE FREELINK_AST(IDX)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
        INCLUDE 'INCLIB:DESNET.DEF'
        INCLUDE 'INCLIB:DN_LINK.DEF'			! DECNET STRUCTURES.
        INCLUDE 'INCLIB:DN_BLOCK.DEF'			! DECNET DATA BLOCKS
C
        INCLUDE '($SYSSRVNAM)'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	IDX,				! LINK BLOCK INDEX.
     *			STATUS				! STATUS HOLDER.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CHANGE STATE TO "DOWN".
C
	DN_LINK(IDX).STATE = STATE_DOWN
C
C INCREMENT THE CHANGE COUNTER.
C
	DN_LINK(IDX).STATE_COUNT(STATE_DOWN) =
     *  DN_LINK(IDX).STATE_COUNT(STATE_DOWN) + 1
C
C SAVE TIME OF CHANGE.
C
	STATUS = SYS$GETTIM(DN_LINK(IDX).STATE_TIME(STATE_DOWN))
C
	CALL DN_CLOSE_LINK(IDX)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
	RETURN
	END
