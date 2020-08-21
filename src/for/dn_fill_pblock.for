C
C SUBROUTINE DN_FILL_PBLOCK
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_FILL_PBLOCK.FOV                           $
C  $Date::   17 Apr 1996 12:57:48                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - dn_fill_pblock.for ***
C  
C V01 17-APR-91 JWE Initial release
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
C Purpose:
C	Put the error returned from DCNPRO into the PBLOCK for NETLOG.
C
C Input:
C	PBLOCK		OS/#@ parameter block
C	IOSB_STATUS	Status from IO
C	AP_STAT		Error encontered other than DECNET.  See DN_LINK.DEF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE DN_FILL_PBLOCK(PBLK, IOSB_STATUS, AP_STAT)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INCLUDE 'INCLIB:DESNET.DEF'
        INCLUDE 'INCLIB:DN_LINK.DEF'			! DECNET STRUCTURES
        INCLUDE 'INCLIB:DN_BLOCK.DEF'			! DECNET DATA BLOCKS
C
	INCLUDE '($SSDEF)'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	AP_STAT,
     *			PBLK(7),
     *			PBLOCK(7, NETNUM + 1, NETSYS)	! I/O PROCEED PARAM BLK
C
	INTEGER*2	IOSB_STATUS
C
C COMMON AREA DECLARATIONS
C
	COMMON	/PBLOCKS/	PBLOCK	
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	IF (IOSB_STATUS .NE. SS$_NORMAL) THEN
	  PBLOCK(1, PBLK(3), PBLK(4)) = INT(IOSB_STATUS)
C
	ELSEIF(AP_STAT.NE.DNE_SUCCESS)THEN
	  PBLOCK(1, PBLK(3), PBLK(4)) = AP_STAT
C
	ELSE
	  PBLOCK(1, PBLK(3), PBLK(4)) = 0
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
	RETURN
	END
