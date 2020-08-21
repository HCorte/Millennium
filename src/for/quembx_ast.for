C
C SUBROUTINE QUEMBX_AST
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]QUEMBX_AST.FOV                               $
C  $Date::   17 Apr 1996 14:36:32                                         $
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
C	ROUTINE TO QUEUE A READ WITH AST TO THE DECNET MAILBOX.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE QUEMBX_AST(MBXBUF)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DN_LINK.DEF'			! DECNET STRUCTURES.
	INCLUDE 'INCLIB:DN_BLOCK.DEF'			! DECNET DATA BLOCKS.
C
	INCLUDE '($IODEF)'
	INCLUDE '($SYSSRVNAM)'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	STATUS				! STATUS HOLDER.
C
	RECORD /DN_MBX_STRUCT/ MBXBUF			! MAILBOX ARGUMENT.
C
C EXTERNAL DECLARATIONS
C
	EXTERNAL	DN_MAILBOX_AST
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C QUEUE A MAILBOX READ REQUEST.
C
	STATUS = SYS$QIO(,				! EVENT FLAG.
     *                   %VAL(DN_SYS.MBXCHAN),		! CHANNEL.
     *                   %VAL(IO$_READVBLK),		! FUNCTION.
     *                   %REF(DN_SYS.IOSB),		! STATUS BLOCK.
     *                   DN_MAILBOX_AST,		! AST ADDRESS.
     *                   %REF(MBXBUF),			! AST PARAMETER.
     *                   %REF(MBXBUF),			! P1.
     *                   %VAL(104),			! P2.
     *                   ,				! P3.
     *                   ,				! P4.
     *                   ,				! P5.
     *                   )				! P6.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C *** HERE WE NEED TO DO SOMETHING, BUT WHAT ? ***
C
C THIS IS LIKELY A FATAL ERROR AND WE WILL CEASE TO OPERATE WITHOUT A NETWORK
C MAILBOX WE CAN USE.
C
C IN THE CASE IT IS TRANSIENT, A RETRY IS PROBABLY APPROPRIATE,
C BUT HOW DO WE GO ABOUT IT ? WE ARE AT AST LEVEL AND SHOULD NOT WAIT HERE.
C
C MAYBE WE SHOULD QUEUE A COMMAND TO OURSELVES.
C
C ANYWAY, FOR THE IMMEDIATE TERM WE WILL JUST TELL THE OPERATOR ABOUT THIS AND
C HOPE THE AST GETS QUEUED AND TAKES CARE OF THINGS TO KEEP US ALIVE.
C
	IF (.NOT. STATUS) THEN
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
	RETURN
	END
