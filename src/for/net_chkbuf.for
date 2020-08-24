C
C SUBROUTINE NET_CHKBUF
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NET_CHKBUF.FOV                               $
C  $Date::   17 Apr 1996 14:10:46                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - net_chkbuf.for ***
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
C	SUBROUTINE TO VERIFY IF BUFFER ON THE STARTED QUEUE IS OK.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE NET_CHKBUF(BUFFER, NODE, NODE_USEID, RETURN_STATUS)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:DESNET.DEF'
        INCLUDE 'INCLIB:DN_LINK.DEF'			! DECNET STRUCTURES
        INCLUDE 'INCLIB:DN_BLOCK.DEF'			! DECNET DATA BLOCKS
C
C LOCAL DECLARATIONS
C
	INTEGER*4	NODE,				! NODE NUMBER
     *			NODE_USEID(NETSYS),
     *			RETURN_STATUS			! STATUS HOLDER
C
	RECORD /DN_BUFFER_STRUCT/ BUFFER		! BUFFER ARGUMENT
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	RETURN_STATUS = 0
C
	IF (BUFFER.LINK .NE. NODE) THEN
	  RETURN_STATUS = -1
	  GOTO 100
	ENDIF
C
	IF (NODE .LE. 0) THEN
	  RETURN_STATUS = -2
	  GOTO 100
	ENDIF
C
	IF (NODE .GT. NETSYS) THEN
	  RETURN_STATUS = -3
	  GOTO 100
	ENDIF
C
100	CONTINUE
	IF (RETURN_STATUS .NE. 0) 
     *    CALL OPS('*** NET_CHKBUF - ERROR ***', RETURN_STATUS, NODE)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
	RETURN
	END
