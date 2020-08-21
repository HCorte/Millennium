C
C SUBROUTINE BUFINI
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]BUFINI.FOV                                   $
C  $Date::   17 Apr 1996 12:22:58                                         $
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
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE BUFINI(BUFFER,		! BUFFER TO INITIALIZE.
     *                    CHANNEL,		! CHANNEL WE ARE USING.
     *                    SYS_NETLOG_TASK,	! SOURCE TASK.
     *                    LNK,			! DN_LINK STRUCTURE INDEX.
     *                    DBUFFER,		! DATA BUFFER ADDRESS.
     *                    IOSB_ADR)		! IOSB TO USE (ADDRESS).
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DN_LINK.DEF'		! DECNET STRUCTURES.
	INCLUDE 'INCLIB:DN_BLOCK.DEF'		! DECNET DATA BLOCKS.
C
	INCLUDE '($SSDEF)'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	DBUFFER,		! NO DATA BUFFER SUPPLIED.
     *			IDX,			! LINK BLOCK INDEX.
     *			IOSB_ADR,		! IOSB TO USE (ADDRESS).
     *			LNK,			! DN_LINK STRUCTURE INDEX.
     *			STATUS,			! STATUS HOLDER FOR SYSTEM CALLS
     *			SYS_NETLOG_TASK		! SOURCE TASK.
C
	INTEGER*2	CHANNEL			! CHANNEL WE ARE USING.
C
	RECORD /DN_BUFFER_STRUCT/ BUFFER	! OUR BUFFER STRUCTURE.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C BEGIN CODE.
C
	BUFFER.AP_STATUS  = 0			! CLEAR THE APPLICATION STATUS.
	BUFFER.CHANNEL    = CHANNEL       	! CHANNEL WE ARE USING.
	BUFFER.COMMAND    = 'C0'X		! INCOMING READ.
	BUFFER.DBUFFER    = DBUFFER		! DATA BUFFER ADDRESS.
	BUFFER.IOSB.STAT  = SS$_NORMAL		! INITIALIZE DEFAULT STATUS.
	BUFFER.IOSB.XSIZE = 0			! INITIALIZE DEFAULT SENT SIZE.
	BUFFER.LINK       = LNK              	! DN_LINK STRUCTURE INDEX.
	BUFFER.SOURCE     = SYS_NETLOG_TASK	! SOURCE TASK.
C
	IOSB_ADR          = %LOC(BUFFER.IOSB)	! ADDRESS OF IOSB TO USE.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
	RETURN
	END
