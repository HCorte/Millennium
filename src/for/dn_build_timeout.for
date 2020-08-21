C
C SUBROUTINE DN_BUILD_TIMEOUT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_BUILD_TIMEOUT.FOV                         $
C  $Date::   17 Apr 1996 12:57:06                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - dn_build_timeout.for ***
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
C Purpose: Build a timeout control block (for DCN style control block)
C
C Input:   BUFFER - DCN TYPE STRUCTURE
C          BUF_NO - NETLOG BUFFER NO TO PRESET
C          SYSTEM - SYSTEM TIMEOUT IS GENERATED FOR
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
c
	SUBROUTINE DN_BUILD_TIMEOUT(BUFFER, BUF_NO, SYSTEM)
c
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INCLUDE 'INCLIB:DESNET.DEF'
        INCLUDE 'INCLIB:DN_LINK.DEF'			! DECNET STRUCTURES
        INCLUDE 'INCLIB:DN_BLOCK.DEF'			! DECNET DATA BLOCKS
C
	INCLUDE '($SSDEF)'
	INCLUDE '($SYSSRVNAM)'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	BUF_NO,
     *			STATUS,				! RETURN VALUE
     *			SYSTEM				! SYSTEM TO OPEN
C
	RECORD /DN_BUFFER_STRUCT/ BUFFER		! BUFFER HEADER
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	BUFFER.COMMAND         = '13'X
C
	BUFFER.SOURCE          = '13'X
C
	BUFFER.LINK            = SYSTEM
C
	BUFFER.LOCAL_PBLOCK(1) = NET_TIMEOUT_ERROR
	BUFFER.LOCAL_PBLOCK(2) = %LOC(NETBUF(1, BUF_NO))! BUFFER START ADDRESS
	BUFFER.LOCAL_PBLOCK(3) = BUF_NO			! BUFFER NO
	BUFFER.LOCAL_PBLOCK(4) = SYSTEM			! UNIT/NODE
	BUFFER.LOCAL_PBLOCK(5) = 0			! NOT A READ
	BUFFER.LOCAL_PBLOCK(7) = 1			! WAY
C
        BUFFER.IOSB.STAT       = SS$_NORMAL
C
        BUFFER.AP_STATUS       = DNE_SUCCESS
C
	STATUS = SYS$GETTIM(BUFFER.TIME)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
	RETURN
	END	   
